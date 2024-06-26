import './src/ArithmeticTests.dart' as arithmetic;
import './src/ArrayTests.dart' as array;
import './src/ComparisonTests.dart' as comparison;
import './src/CustomOperatorTests.dart' as custom_operator;
import './src/DateTimeTests.dart' as date;
import './src/DictionaryTests.dart' as dictionary;
import './src/EnumTests.dart' as enum_;
import './src/EnumerableTests.dart' as enumerable;
import './src/HashSetTests.dart' as hash_set;
import './src/ListTests.dart' as list;
import './src/MapTests.dart' as map;
import './src/MiscTests.dart' as misc;
import './src/OptionTests.dart' as option;
import './src/RecordTests.dart' as record;
import './src/RegexTests.dart' as regex;
import './src/ResizeArrayTests.dart' as resize_array;
import './src/ResultTests.dart' as result;
import './src/SeqTests.dart' as seq;
import './src/SeqExpressionTests.dart' as seq_expression;
import './src/SetTests.dart' as set_;
import './src/StringTests.dart' as string;
import './src/SudokuTests.dart' as sudoku;
import './src/TailCallTests.dart' as tailcall;
import './src/TimeSpanTests.dart' as timespan;
import './src/TupleTests.dart' as tuple;
import './src/TypeTests.dart' as type_;
import './src/UnionTests.dart' as union;

void main() {
  arithmetic.tests();
  array.tests();
  comparison.tests();
  custom_operator.tests();
  date.tests();
  dictionary.tests();
  enum_.tests();
  enumerable.tests();
  hash_set.tests();
  list.tests();
  map.tests();
  misc.tests();
  option.tests();
  record.tests();
  regex.tests();
  resize_array.tests();
  result.tests();
  seq.tests();
  seq_expression.tests();
  set_.tests();
  string.tests();
  sudoku.tests();
  tailcall.tests();
  timespan.tests();
  tuple.tests();
  type_.tests();
  union.tests();
}