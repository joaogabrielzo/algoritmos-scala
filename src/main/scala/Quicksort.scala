object Quicksort extends App {

  def quicksort(l: List[Int]): List[Any] = {
    if (l.isEmpty) Nil
    else {
      val ponto: Int = l.head

      val menores: List[Int] = l.filter(x => x < ponto)
      val maiores: List[Int] = l.filter(x => x > ponto)

      quicksort(menores) :: ponto :: quicksort(maiores)
    }
  }

  println(quicksort(List(654, 32, 78, 9, 1, 8648, 5, 33, 489, 67, 167, 3)))
}
