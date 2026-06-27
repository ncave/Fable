#[cfg(feature = "threaded")]
pub mod Async_ {
    use std::cell::RefCell;
    use std::future::{self, Future, ready};
    use std::pin::Pin;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::task::Context;
    use std::thread;
    use std::time::Duration;

    use futures::FutureExt;
    use futures::executor::{self, LocalPool};
    use futures::lock::Mutex;
    use futures::task::{ArcWake, waker_ref};
    use futures_timer::Delay;

    use crate::Choice_::Choice_2;
    use crate::Native_::{Func1, LrcPtr};
    use crate::NativeArray_::Array;
    use crate::System::Collections::Generic::IEnumerable_1;
    use crate::System::{Exception, IDisposable, OperationCanceledException, TimeoutException};
    use crate::System::Threading::{CancellationToken, CancellationTokenRegistration, CancellationTokenSource};
    use super::Task_::Task;

    #[derive(Clone)]
    enum ContinuationResult<T: Clone + Send + Sync> {
        Success(T),
        Error(LrcPtr<Exception>),
        Cancel(LrcPtr<OperationCanceledException>),
    }

    pub struct Async<T: Sized + Send + Sync> {
        pub future: Arc<Mutex<Pin<Box<dyn Future<Output = T> + Send + Sync>>>>,
    }

    thread_local! {
        static CURRENT_CANCELLATION_TOKEN: RefCell<Option<CancellationToken>> = RefCell::new(None);
    }

    struct CancellationTokenScope;

    impl Drop for CancellationTokenScope {
        fn drop(&mut self) {
            CURRENT_CANCELLATION_TOKEN.with(|cell| {
                *cell.borrow_mut() = None;
            });
        }
    }

    fn enter_cancellation_scope(token: Option<CancellationToken>) -> CancellationTokenScope {
        CURRENT_CANCELLATION_TOKEN.with(|cell| {
            *cell.borrow_mut() = token;
        });
        CancellationTokenScope
    }

    fn current_cancellation_requested() -> bool {
        CURRENT_CANCELLATION_TOKEN.with(|cell| {
            cell.borrow()
                .as_ref()
                .map(|t| t.get_IsCancellationRequested())
                .unwrap_or(false)
        })
    }

    pub(crate) fn throw_if_canceled() {
        if current_cancellation_requested() {
            std::panic::panic_any(OperationCanceledException::_ctor());
        }
    }

    impl<T: Clone + Send + Sync> Future for &Async<T> {
        type Output = T;

        fn poll(
            self: Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            let p = self
                .future
                .try_lock()
                .map(|mut f| f.poll_unpin(cx))
                .unwrap_or_else(|| {
                    // Avoid blocking the polling thread on lock contention.
                    cx.waker().wake_by_ref();

                    std::task::Poll::Pending
                });
            p
        }
    }

    struct StartImmediateWake {
        woke: AtomicBool,
    }

    impl ArcWake for StartImmediateWake {
        fn wake_by_ref(arc_self: &Arc<Self>) {
            arc_self.woke.store(true, Ordering::SeqCst);
        }
    }

    pub fn sleep(milliseconds: i32) -> Arc<Async<()>> {
        let fut = async move {
            let mut remaining = milliseconds.max(0);
            while remaining > 0 {
                throw_if_canceled();
                let step = remaining.min(20);
                Delay::new(Duration::from_millis(step as u64)).await;
                remaining -= step;
            }
            throw_if_canceled();
        };
        let a: Pin<Box<dyn Future<Output = ()> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn startAsTask<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        taskCreationOptions: Option<i32>,
        cancellationToken: Option<CancellationToken>,
    ) -> Arc<Task<T>> {
        let token = cancellationToken.clone();
        let unitFut = async move {
            let _scope = enter_cancellation_scope(token);
            throw_if_canceled();
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            throw_if_canceled();
            res
        };
        let task = Arc::from(Task::new(unitFut));
        Task::start(task.clone());
        task
    }

    pub fn startChild<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        millisecondsTimeout: Option<i32>,
    ) -> Arc<Async<Arc<Async<T>>>> {
        let task = startAsTask(a, None::<i32>, None);

        let child = {
            let task = task.clone();
            let fut = async move {
                if let Some(timeout) = millisecondsTimeout {
                    use futures::future::Either;

                    let task_fut = async move { (&*task).await };
                    let delay_fut = Delay::new(Duration::from_millis(timeout as u64));
                    futures::pin_mut!(task_fut);
                    futures::pin_mut!(delay_fut);

                    match futures::future::select(task_fut, delay_fut).await {
                        Either::Left((value, _)) => value,
                        Either::Right(_) => std::panic::panic_any(TimeoutException::_ctor()),
                    }
                } else {
                    (&*task).await
                }
            };

            let fut: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(fut);
            Arc::from(Async {
                future: Arc::from(Mutex::from(fut)),
            })
        };

        let outer = future::ready(child);
        let outer: Pin<Box<dyn Future<Output = Arc<Async<T>>> + Send + Sync + 'static>> = Box::pin(outer);
        Arc::from(Async {
            future: Arc::from(Mutex::from(outer)),
        })
    }

    pub fn startImmediate(a: Arc<Async<()>>, cancellationToken: Option<CancellationToken>) {
        if cancellationToken
            .as_ref()
            .map(|t| t.get_IsCancellationRequested())
            .unwrap_or(false)
        {
            return;
        }

        let polled = a.future.try_lock().map(|mut future| {
            let _scope = enter_cancellation_scope(cancellationToken.clone());
            throw_if_canceled();
            let wake = Arc::new(StartImmediateWake {
                woke: AtomicBool::new(true),
            });
            let waker = waker_ref(&wake);
            let mut cx = Context::from_waker(&waker);

            loop {
                wake.woke.store(false, Ordering::SeqCst);

                match future.poll_unpin(&mut cx) {
                    std::task::Poll::Ready(()) => return true,
                    std::task::Poll::Pending if wake.woke.load(Ordering::SeqCst) => continue,
                    std::task::Poll::Pending => return false,
                }
            }
        });

        match polled {
            Some(true) => {}
            _ => {
                startAsTask(a, None::<i32>, cancellationToken);
            }
        }
    }

    pub fn createCancellationToken() -> LrcPtr<CancellationTokenSource> {
        CancellationTokenSource::_ctor()
    }

    pub fn createCancellationTokenWithDelay(milliseconds: i32) -> LrcPtr<CancellationTokenSource> {
        let source = CancellationTokenSource::_ctor();
        source.CancelAfter_Z524259A4(milliseconds);
        source
    }

    pub fn cancel(source: LrcPtr<CancellationTokenSource>) {
        source.Cancel();
    }

    pub fn cancelAfter(source: LrcPtr<CancellationTokenSource>, milliseconds: i32) {
        source.CancelAfter_Z524259A4(milliseconds);
    }

    pub fn isCancellationRequested(source: LrcPtr<CancellationTokenSource>) -> bool {
        source.get_IsCancellationRequested()
    }

    pub fn throwIfCancellationRequested(source: LrcPtr<CancellationTokenSource>) {
        source.ThrowIfCancellationRequested();
    }

    pub fn getToken(source: LrcPtr<CancellationTokenSource>) -> CancellationToken {
        (*source.get_Token()).clone()
    }

    pub fn register(token: CancellationToken, callback: crate::Native_::Func0<()>) -> CancellationTokenRegistration {
        (*token.Register_3A5B6456(callback)).clone()
    }

    pub fn onCancel(callback: crate::Native_::Func0<()>) -> Arc<Async<CancellationTokenRegistration>> {
        let token = CURRENT_CANCELLATION_TOKEN.with(|cell| cell.borrow().clone());
        let fut = async move {
            let registration = match token {
                Some(token) => register(token, callback),
                None => {
                    let source = createCancellationToken();
                    let token = getToken(source);
                    register(token, callback)
                }
            };
            registration
        };
        let a: Pin<Box<dyn Future<Output = CancellationTokenRegistration> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn runSynchronously<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        timeout: Option<i32>,
        cancellationToken: Option<CancellationToken>,
    ) -> T {
        let token = cancellationToken.clone();
        let unitFut = async move {
            let _scope = enter_cancellation_scope(token);
            throw_if_canceled();
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            throw_if_canceled();
            res
        };
        executor::block_on(unitFut)
    }

    pub fn awaitTask<T: Clone + Send + Sync + 'static>(a: Arc<Task<T>>) -> Arc<Async<T>> {
        let fut = async move { (&*a).await };
        let a: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn ignore<T: Clone + Send + Sync + 'static>(a: Arc<Async<T>>) -> Arc<Async<()>> {
        let fut = async move {
            let mut res = a.future.lock().await;
            let _ = res.as_mut().await;
        };
        let a: Pin<Box<dyn Future<Output = ()> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn fromContinuations<T: Clone + Send + Sync + 'static>(
        continuationGenerator: Func1<
            LrcPtr<(
                Func1<T, ()>,
                Func1<LrcPtr<Exception>, ()>,
                Func1<LrcPtr<OperationCanceledException>, ()>,
            )>,
            (),
        >,
    ) -> Arc<Async<T>> {
        use std::sync::Mutex as StdMutex;

        let result = std::sync::Arc::new(StdMutex::new(None::<ContinuationResult<T>>));

        let on_success = Func1::new({
            let result = result.clone();
            move |value: T| {
                *result.lock().unwrap() = Some(ContinuationResult::Success(value));
            }
        });

        let on_error = Func1::new({
            let result = result.clone();
            move |ex: LrcPtr<Exception>| {
                *result.lock().unwrap() = Some(ContinuationResult::Error(ex));
            }
        });

        let on_cancel = Func1::new({
            let result = result.clone();
            move |ex: LrcPtr<OperationCanceledException>| {
                *result.lock().unwrap() = Some(ContinuationResult::Cancel(ex));
            }
        });

        continuationGenerator(LrcPtr::new((on_success, on_error, on_cancel)));

        let outcome = result
            .lock()
            .unwrap()
            .clone()
            .unwrap_or_else(|| {
                std::panic::panic_any(crate::Util_::new_Exception(crate::String_::string(
                    "Async.FromContinuations: no continuation called",
                )))
            });

        // `poll_fn` recomputes output from a cloned stored outcome, so this
        // async value can be safely awaited more than once when shared.
        let fut = futures::future::poll_fn(move |_| {
            std::task::Poll::Ready(match outcome.clone() {
                ContinuationResult::Success(value) => value,
                ContinuationResult::Error(ex) => std::panic::panic_any(ex),
                ContinuationResult::Cancel(ex) => std::panic::panic_any(ex),
            })
        });
        let a: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn startWithContinuations<T: Clone + Send + Sync + 'static>(
        computation: Arc<Async<T>>,
        continuation: Func1<T, ()>,
        exceptionContinuation: Func1<LrcPtr<Exception>, ()>,
        cancellationContinuation: Func1<LrcPtr<OperationCanceledException>, ()>,
        cancellationToken: Option<CancellationToken>,
    ) {
        fn get_ex(err: Box<dyn std::any::Any + Send>) -> LrcPtr<Exception> {
            use crate::String_::{fromSlice, string};
            use crate::Util_::new_Exception;
            match err.downcast_ref::<&'static str>() {
                Some(s) => new_Exception(string(*s)),
                None => match err.downcast_ref::<String>() {
                    Some(s) => new_Exception(fromSlice(s)),
                    None => match err.downcast_ref::<LrcPtr<Exception>>() {
                        Some(ex) => ex.clone(),
                        None => new_Exception(string("Unknown error")),
                    },
                },
            }
        }

        use std::panic::AssertUnwindSafe;
        let result = std::panic::catch_unwind(AssertUnwindSafe(move || {
            runSynchronously(computation, None::<i32>, cancellationToken)
        }));

        match result {
            Ok(value) => continuation(value),
            Err(err) => {
                if let Some(cancel) = err.downcast_ref::<LrcPtr<OperationCanceledException>>() {
                    cancellationContinuation(cancel.clone())
                } else {
                    exceptionContinuation(get_ex(err))
                }
            }
        }
    }

    pub fn parallelAsync<T: Clone + Send + Sync + 'static>(
        computations: LrcPtr<dyn IEnumerable_1<Arc<Async<T>>>>,
    ) -> Arc<Async<Array<T>>> {
        use crate::NativeArray_::array_from;
        // Collect all computations eagerly (before the async block)
        let items: Vec<Arc<Async<T>>> = {
            let enumerator = computations.GetEnumerator();
            let mut v = Vec::new();
            while enumerator.MoveNext() {
                v.push(enumerator.get_Current());
            }
            v
        };
        let fut = async move {
            // Start all as tasks in parallel, then await them sequentially
            let tasks: Vec<Arc<Task<T>>> = items
                .into_iter()
                .map(|a| startAsTask(a, None::<i32>, None))
                .collect();
            let mut results = Vec::with_capacity(tasks.len());
            for task in tasks {
                results.push((&*task).await);
            }
            array_from(results)
        };
        let a: Pin<Box<dyn Future<Output = Array<T>> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn sequentialAsync<T: Clone + Send + Sync + 'static>(
        computations: LrcPtr<dyn IEnumerable_1<Arc<Async<T>>>>,
    ) -> Arc<Async<Array<T>>> {
        use crate::NativeArray_::array_from;
        let items: Vec<Arc<Async<T>>> = {
            let enumerator = computations.GetEnumerator();
            let mut v = Vec::new();
            while enumerator.MoveNext() {
                v.push(enumerator.get_Current());
            }
            v
        };
        let fut = async move {
            let mut results = Vec::with_capacity(items.len());
            for item in items {
                let mut res = item.future.lock().await;
                results.push(res.as_mut().await);
            }
            array_from(results)
        };
        let a: Pin<Box<dyn Future<Output = Array<T>> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn catchAsync<T: Clone + Send + Sync + 'static>(
        work: Arc<Async<T>>,
    ) -> Arc<Async<LrcPtr<Choice_2<T, LrcPtr<Exception>>>>> {
        fn get_ex(err: Box<dyn std::any::Any + Send>) -> LrcPtr<Exception> {
            use crate::String_::{fromSlice, string};
            use crate::Util_::new_Exception;
            match err.downcast_ref::<&'static str>() {
                Some(s) => new_Exception(string(*s)),
                None => match err.downcast_ref::<String>() {
                    Some(s) => new_Exception(fromSlice(s)),
                    None => match err.downcast_ref::<LrcPtr<Exception>>() {
                        Some(ex) => ex.clone(),
                        None => new_Exception(string("Unknown error")),
                    },
                },
            }
        }

        let fut = async move {
            use std::panic::AssertUnwindSafe;
            use futures::FutureExt;
            let result = AssertUnwindSafe(async {
                let mut res = work.future.lock().await;
                res.as_mut().await
            })
            .catch_unwind()
            .await
            .map_err(get_ex);
            match result {
                Ok(value) => LrcPtr::new(Choice_2::Choice1Of2(value)),
                Err(ex) => LrcPtr::new(Choice_2::Choice2Of2(ex)),
            }
        };
        let a: Pin<Box<dyn Future<Output = LrcPtr<Choice_2<T, LrcPtr<Exception>>>> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }
}

#[cfg(feature = "threaded")]
pub mod AsyncBuilder_ {
    use std::panic::panic_any;
    use std::task::Poll;
    use std::future::{Future, ready};
    use std::pin::Pin;
    use std::sync::Arc;

    use futures::lock::Mutex;

    use super::Async_::{Async, startAsTask, throw_if_canceled};
    // crate::Exception_ not needed in AsyncBuilder_ anymore
    use crate::Native_::{Func0, Func1, LrcPtr, NullableRef};
    use crate::System::Collections::Generic::IEnumerable_1;
    use crate::System::Exception;
    use crate::System::IDisposable;

    // Thread-local depth counter to detect deeply recursive return! chains
    // and break them by spawning a new task, preventing stack overflow.
    thread_local! {
        static ASYNC_POLL_DEPTH: std::cell::Cell<usize> = std::cell::Cell::new(0);
    }

    pub fn delay<T: Send + Sync + 'static>(binder: Func0<Arc<Async<T>>>) -> Arc<Async<T>> {
        let next = async move {
            let depth = ASYNC_POLL_DEPTH.with(|d| {
                let v = d.get();
                d.set(v + 1);
                v
            });

            if depth >= 200 {
                // Deep recursion: yield once to the executor so nested poll frames unwind.
                // This preserves semantics while preventing stack overflow in long return! chains.
                ASYNC_POLL_DEPTH.with(|d| d.set(0));
                let mut yielded = false;
                futures::future::poll_fn(move |cx| {
                    if yielded {
                        Poll::Ready(())
                    } else {
                        yielded = true;
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                }).await;
            }

            let next_async = binder();
            let mut next = next_async.future.lock().await;
            let result = next.as_mut().await;

            ASYNC_POLL_DEPTH.with(|d| {
                let v = d.get();
                if v > 0 { d.set(v - 1); }
            });

            result
        };

        let pr: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(pr)),
        })
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: Arc<Async<T>>,
        binder: Func1<T, Arc<Async<U>>>,
    ) -> Arc<Async<U>> {
        let next = async move {
            throw_if_canceled();
            let mut m = opt.future.lock().await;
            let m = m.as_mut().await;
            throw_if_canceled();
            let nextAsync = binder(m);
            let nextTask = startAsTask(nextAsync, None::<i32>, None);
            (&*nextTask).await
        };

        let b: Pin<Box<dyn Future<Output = U> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn combine<T: Clone + Send + Sync + 'static>(
        computation1: Arc<Async<()>>,
        computation2: Arc<Async<T>>,
    ) -> Arc<Async<T>> {
        bind(
            computation1,
            Func1::new({
                let computation2 = computation2.clone();
                move |_| computation2.clone()
            }),
        )
    }

    pub fn while_loop(guard: Func0<bool>, computation: Func0<Arc<Async<()>>>) -> Arc<Async<()>> {
        let next = async move {
            while guard() {
                throw_if_canceled();
                let body = computation();
                let mut res = body.future.lock().await;
                res.as_mut().await;
            }
            throw_if_canceled();
        };

        let b: Pin<Box<dyn Future<Output = ()> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn for_loop<T: Clone + Send + Sync + 'static>(
        sequence: LrcPtr<dyn IEnumerable_1<T>>,
        body: Func1<T, Arc<Async<()>>>,
    ) -> Arc<Async<()>> {
        let values: Vec<T> = {
            let enumerator = sequence.GetEnumerator();
            let mut v = Vec::new();
            while enumerator.MoveNext() {
                v.push(enumerator.get_Current());
            }
            v
        };

        let next = async move {
            for value in values {
                throw_if_canceled();
                let computation = body(value);
                let mut res = computation.future.lock().await;
                res.as_mut().await;
            }
            throw_if_canceled();
        };

        let b: Pin<Box<dyn Future<Output = ()> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn try_with<T: Clone + Send + Sync + 'static>(
        computation: Arc<Async<T>>,
        catch: Func1<LrcPtr<Exception>, Arc<Async<T>>>,
    ) -> Arc<Async<T>> {
        fn get_ex(err: Box<dyn std::any::Any + Send>) -> LrcPtr<Exception> {
            use crate::String_::{fromSlice, string};
            use crate::Util_::new_Exception;
            match err.downcast_ref::<&'static str>() {
                Some(s) => new_Exception(string(*s)),
                None => match err.downcast_ref::<String>() {
                    Some(s) => new_Exception(fromSlice(s)),
                    None => match err.downcast_ref::<LrcPtr<Exception>>() {
                        Some(ex) => ex.clone(),
                        None => new_Exception(string("Unknown error")),
                    },
                },
            }
        }

        let next = async move {
            use std::panic::AssertUnwindSafe;
            use futures::FutureExt;
            let result = AssertUnwindSafe(async {
                let mut res = computation.future.lock().await;
                res.as_mut().await
            })
            .catch_unwind()
            .await;
            // Convert Box<dyn Any> to LrcPtr<Exception> before any .await point
            // so the non-Sync type does not cross an await boundary.
            let mapped: Result<T, LrcPtr<Exception>> = result.map_err(get_ex);
            match mapped {
                Ok(value) => value,
                Err(ex) => {
                    let catch_async = catch(ex);
                    let mut res = catch_async.future.lock().await;
                    res.as_mut().await
                }
            }
        };

        let b: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn try_finally<T: Clone + Send + Sync + 'static>(
        computation: Arc<Async<T>>,
        compensation: Func0<()>,
    ) -> Arc<Async<T>> {
        let next = async move {
            use std::panic::AssertUnwindSafe;
            use futures::FutureExt;
            let result = AssertUnwindSafe(async {
                let mut res = computation.future.lock().await;
                res.as_mut().await
            })
            .catch_unwind()
            .await;

            compensation();

            match result {
                Ok(value) => value,
                Err(err) => std::panic::resume_unwind(err),
            }
        };

        let b: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn using<T: IDisposable + NullableRef + Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        resource: T,
        binder: Func1<T, Arc<Async<U>>>,
    ) -> Arc<Async<U>> {
        let next = async move {
            use std::panic::AssertUnwindSafe;
            use futures::FutureExt;
            let computation = binder(resource.clone());
            let result = AssertUnwindSafe(async {
                let mut res = computation.future.lock().await;
                res.as_mut().await
            })
            .catch_unwind()
            .await;
            if resource.is_null() {
                ()
            } else {
                resource.Dispose()
            }
            match result {
                Ok(value) => value,
                Err(err) => std::panic::resume_unwind(err),
            }
        };

        let b: Pin<Box<dyn Future<Output = U> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn r_return<T: Send + Sync + 'static>(item: T) -> Arc<Async<T>> {
        let r = ready(item);
        let b: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(r);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn return_from<T: Send + Sync + 'static>(computation: Arc<Async<T>>) -> Arc<Async<T>> {
        computation
    }

    pub fn zero<T: Send + Sync + 'static>() -> Arc<Async<()>> {
        r_return(())
    }
}

#[cfg(feature = "threaded")]
pub mod ThreadPool {
    use std::sync::{OnceLock, RwLock};

    use futures::executor::ThreadPool;

    static POOL: OnceLock<RwLock<ThreadPool>> = OnceLock::new();
    pub fn try_init_and_get_pool() -> &'static RwLock<ThreadPool> {
        POOL.get_or_init(|| RwLock::new(ThreadPool::new().unwrap()))
    }
}

#[cfg(feature = "threaded")]
pub mod Monitor_ {
    use std::any::Any;
    use std::collections::HashSet;
    use std::sync::{Arc, Mutex, OnceLock, RwLock, Weak};
    use std::thread;
    use std::time::Duration;

    use crate::Native_::{Func0, LrcPtr};

    static LOCKS: OnceLock<RwLock<HashSet<usize>>> = OnceLock::new();
    fn try_init_and_get_locks() -> &'static RwLock<HashSet<usize>> {
        LOCKS.get_or_init(|| RwLock::new(HashSet::new()))
    }

    pub fn enter<T>(o: LrcPtr<T>) {
        let p = Arc::<T>::as_ptr(&o) as usize;
        loop {
            let otherHasLock = try_init_and_get_locks().read().unwrap().get(&p).is_some();
            if otherHasLock {
                thread::sleep(Duration::from_millis(10));
            } else {
                try_init_and_get_locks().write().unwrap().insert(p);
                return;
            }
        }
    }

    pub fn exit<T>(o: LrcPtr<T>) {
        let p = Arc::<T>::as_ptr(&o) as usize;
        let hasRemoved = try_init_and_get_locks().write().unwrap().remove(&p);
        if (!hasRemoved) {
            panic!("Not removed {}", p)
        }
    }

    // Not technically part of monitor, but it needs to be behind a feature switch, so cannot just dump this in Native
    pub fn lock<T: Clone + Send + Sync, U: 'static>(toLock: LrcPtr<T>, f: Func0<U>) -> U {
        enter(toLock.clone());
        let returnVal = f();
        // panics will bypass this - need some finally mechanism
        exit(toLock.clone());
        returnVal
    }
}

#[cfg(feature = "threaded")]
pub mod Task_ {
    use std::pin::Pin;
    use std::sync::{Arc, RwLock};
    use std::task::Poll;
    use std::thread::{self, JoinHandle};
    use std::time::Duration;

    use futures::{Future, FutureExt};

    use super::ThreadPool::try_init_and_get_pool;
    use crate::Native_::{Func0, Func1, LrcPtr, NullableRef};
    use crate::System::Collections::Generic::IEnumerable_1;
    use crate::System::{Exception, IDisposable};

    pub enum TaskState<T: Sized + Clone + Send> {
        New(Pin<Box<dyn Future<Output = T> + Send + Sync>>),
        Running,
        Complete(T),
    }

    impl<T: Sized + Clone + Send + 'static> TaskState<T> {
        pub fn is_new(&self) -> bool {
            match self {
                TaskState::New(_) => true,
                _ => false,
            }
        }

        pub fn is_running(&self) -> bool {
            match self {
                TaskState::Running => true,
                _ => false,
            }
        }

        pub fn is_complete(&self) -> bool {
            match self {
                TaskState::Complete(_) => true,
                _ => false,
            }
        }

        pub fn unwrap(&self) -> T {
            match self {
                TaskState::Complete(t) => t.clone(),
                _ => panic!("Task not yet complete"),
            }
        }

        pub fn replace(&mut self, next: TaskState<T>) -> TaskState<T> {
            core::mem::replace(self, next)
        }
    }

    #[derive(Clone)]
    pub struct Task<T: Sized + Clone + Send> {
        result: Arc<RwLock<TaskState<T>>>,
    }

    impl<T: Clone + Send + Sync> Future for &Task<T> {
        type Output = T;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            //eprintln!("{:?} Polling task for result", thread::current().id());
            let m = self.result.read().unwrap();

            match &*m {
                TaskState::New(_) => {
                    //schedule?
                    //eprintln!("{:?} poll: task is new, waking up", thread::current().id());
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                TaskState::Running => {
                    //eprintln!("{:?} poll: pending, nothing to do", thread::current().id());

                    // Keep polling cooperative and low-latency without sleeping the thread.
                    thread::yield_now();
                    cx.waker().wake_by_ref();

                    Poll::Pending
                }
                TaskState::Complete(res) => {
                    //eprintln!("{:?} Poll succeeded", thread::current().id());
                    Poll::Ready(res.clone())
                }
            }
        }
    }

    impl<T: Clone + Send + Sync + 'static> Task<T> {
        pub fn new(fut: impl Future<Output = T> + Send + Sync + 'static) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::New(Box::pin(fut)))),
            }
        }

        pub fn from_result(value: T) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::Complete(value))),
            }
        }

        pub fn set_result(&self, value: T) {
            let mut m = self.result.write().unwrap();
            //eprintln!("{:?} set task result", thread::current().id());
            (*m) = TaskState::Complete(value);
            //eprintln!("{:?} set task result completed", thread::current().id());
        }

        pub fn is_new(&self) -> bool {
            self.result.read().unwrap().is_new()
        }

        fn is_complete(&self) -> bool {
            self.result.read().unwrap().is_complete()
        }

        pub fn get_result(&self) -> T {
            while !self.is_complete() {
                //eprintln!("{:?} try get result", thread::current().id());
                thread::yield_now();
            }
            //eprintln!("{:?} has result", thread::current().id());
            let t = self.result.read().unwrap().unwrap();
            t
        }

        pub fn start(t: Arc<Task<T>>) {
            if !t.result.read().unwrap().is_new() {
                return;
            }
            let ts = t.result.write().unwrap().replace(TaskState::Running);
            match ts {
                TaskState::New(mut fut) => {
                    let f2 = async move {
                        let res = fut.as_mut().await;
                        t.set_result(res);
                    };
                    let pool = super::ThreadPool::try_init_and_get_pool();
                    //eprintln!("{:?} new task added to queue", thread::current().id());
                    pool.write().unwrap().spawn_ok(f2);
                }
                _ => {}
            }
        }
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: Arc<Task<T>>,
        binder: Func1<T, Arc<Task<U>>>,
    ) -> Arc<Task<U>> {
        let next = async move {
            //eprintln!("{:?} begin await source fut", thread::current().id());
            let m = opt.as_ref().await;
            //eprintln!("{:?} awaiting source future success", thread::current().id());
            let nextAsync = binder(m);
            if nextAsync.is_new() {
                Task::start(nextAsync.clone());
            }
            let next = nextAsync.as_ref().await;
            //eprintln!("{:?} setting result", thread::current().id());
            next
        };

        let task = Task::new(next);
        Arc::from(task)
    }

    pub fn combine<T: Clone + Send + Sync + 'static>(
        task1: Arc<Task<()>>,
        task2: Arc<Task<T>>,
    ) -> Arc<Task<T>> {
        bind(
            task1,
            Func1::new({
                let task2 = task2.clone();
                move |_| task2.clone()
            }),
        )
    }

    pub fn while_loop(guard: Func0<bool>, computation: Func0<Arc<Task<()>>>) -> Arc<Task<()>> {
        let fut = async move {
            while guard() {
                let task = computation();
                if task.is_new() {
                    Task::start(task.clone());
                }
                let _ = (&*task).await;
            }
        };
        Arc::from(Task::new(fut))
    }

    pub fn for_loop<T: Clone + Send + Sync + 'static>(
        sequence: LrcPtr<dyn IEnumerable_1<T>>,
        body: Func1<T, Arc<Task<()>>>,
    ) -> Arc<Task<()>> {
        let values: Vec<T> = {
            let enumerator = sequence.GetEnumerator();
            let mut v = Vec::new();
            while enumerator.MoveNext() {
                v.push(enumerator.get_Current());
            }
            v
        };

        let fut = async move {
            for value in values {
                let task = body(value);
                if task.is_new() {
                    Task::start(task.clone());
                }
                let _ = (&*task).await;
            }
        };
        Arc::from(Task::new(fut))
    }

    pub fn try_with<T: Clone + Send + Sync + 'static>(
        computation: Arc<Task<T>>,
        catch: Func1<LrcPtr<Exception>, Arc<Task<T>>>,
    ) -> Arc<Task<T>> {
        fn get_ex(err: Box<dyn std::any::Any + Send>) -> LrcPtr<Exception> {
            use crate::String_::{fromSlice, string};
            use crate::Util_::new_Exception;
            match err.downcast_ref::<&'static str>() {
                Some(s) => new_Exception(string(*s)),
                None => match err.downcast_ref::<String>() {
                    Some(s) => new_Exception(fromSlice(s)),
                    None => match err.downcast_ref::<LrcPtr<Exception>>() {
                        Some(ex) => ex.clone(),
                        None => new_Exception(string("Unknown error")),
                    },
                },
            }
        }

        let fut = async move {
            use futures::FutureExt;
            use std::panic::AssertUnwindSafe;

            let ex = {
                let result = AssertUnwindSafe(async {
                    if computation.is_new() {
                        Task::start(computation.clone());
                    }
                    (&*computation).await
                })
                .catch_unwind()
                .await;

                match result {
                    Ok(v) => return v,
                    Err(err) => get_ex(err),
                }
            };

            let task = catch(ex);
            if task.is_new() {
                Task::start(task.clone());
            }
            (&*task).await
        };
        Arc::from(Task::new(fut))
    }

    pub fn using<T: IDisposable + NullableRef + Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        resource: T,
        binder: Func1<T, Arc<Task<U>>>,
    ) -> Arc<Task<U>> {
        let fut = async move {
            use futures::FutureExt;
            use std::panic::AssertUnwindSafe;

            let task = binder(resource.clone());
            let result = AssertUnwindSafe(async {
                if task.is_new() {
                    Task::start(task.clone());
                }
                (&*task).await
            })
            .catch_unwind()
            .await;

            if !resource.is_null() {
                resource.Dispose();
            }

            match result {
                Ok(v) => v,
                Err(err) => std::panic::resume_unwind(err),
            }
        };
        Arc::from(Task::new(fut))
    }

    pub fn delay<T: Clone + Send + Sync + 'static>(binder: Func0<Arc<Task<T>>>) -> Arc<Task<T>> {
        let pr = binder();
        pr
    }

    pub fn r_return<T: Clone + Send + Sync + 'static>(item: T) -> Arc<Task<T>> {
        let t = Task::from_result(item);
        Arc::from(t)
    }

    pub fn zero<T: Clone + Send + Sync + 'static>() -> Arc<Task<()>> {
        r_return(())
    }

    pub fn from_result<T: Clone + Send>(t: T) -> Arc<Task<T>> {
        let t = Task {
            result: Arc::from(RwLock::from(TaskState::Complete(t))),
        };
        Arc::from(t)
    }
}

#[cfg(feature = "threaded")]
pub mod TaskBuilder_ {
    use super::super::Native_::LrcPtr;
    use super::Task_::Task;
    use std::sync::Arc;

    pub struct TaskBuilder {}

    impl TaskBuilder {
        pub fn run<T: Clone + Send + Sync + 'static>(&self, task: Arc<Task<T>>) -> Arc<Task<T>> {
            Task::start(task.clone());
            task
        }
    }

    pub fn new() -> LrcPtr<TaskBuilder> {
        LrcPtr::new(TaskBuilder {})
    }
}

#[cfg(feature = "threaded")]
pub mod Thread_ {
    use std::thread;
    use std::time::Duration;

    use crate::Native_::{Func0, LrcPtr, MutCell};

    enum ThreadInt {
        New(Func0<()>),
        //Building(thread::Builder),
        Running(thread::JoinHandle<()>),
        Empty,
    }
    impl Default for ThreadInt {
        fn default() -> Self {
            ThreadInt::Empty
        }
    }
    pub struct Thread(MutCell<ThreadInt>);

    pub fn new(f: Func0<()>) -> LrcPtr<Thread> {
        LrcPtr::new(Thread(MutCell::from(ThreadInt::New(f))))
    }

    impl Thread {
        pub fn start(&self) {
            match self.0.take() {
                ThreadInt::New(f) => {
                    let t = std::thread::spawn(move || f());
                    self.0.set(ThreadInt::Running(t));
                }
                _ => {}
            }
        }

        pub fn join(&self) {
            match self.0.take() {
                ThreadInt::Running(jh) => {
                    jh.join().expect("Couldn't join on the associated thread");
                }
                _ => {}
            }
        }
    }

    pub fn sleep(millis: i32) {
        thread::sleep(Duration::from_millis(millis as u64));
    }
}
