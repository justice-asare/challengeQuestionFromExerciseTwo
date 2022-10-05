import munit.FunSuite

class MergeSortTest extends FunSuite {

  test("Asc sorting") {
    assertEquals(MergeSort.msort[Int](_<_)(List(1,3,9,0)), List(0,1,3,9))
  }

  test("Desc sorting with Int") {
    assertEquals(MergeSort.msort[Int](_>_)(List(1,3,9,0)), List(9,3,1,0))
  }

  test("Desc sorting with Double") {
    assertEquals(MergeSort.msort[Double](_>_)(List(1.5,3.2,9.1,0.6)), List(9.1,3.2,1.5,0.6))
  }

  test("Asc sorting") {
    val intSort: List[Int] => List[Int] = MergeSort.msort(_ < _)
    assertEquals(intSort(List(9,3,5,1)), List(1, 3, 5, 9))
  }

  test("Desc sorting") {
    val intSort: List[Int] => List[Int] = MergeSort.msort(_ > _)
    assertEquals(intSort(List(9,3,5,1)), List(9, 5, 3, 1))
  }
5
}
