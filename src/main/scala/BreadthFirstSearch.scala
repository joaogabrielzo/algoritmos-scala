import scala.collection.mutable

case class Pessoa(nome: String, trab: String)

object BreadthFirstSearch extends App {

  val Jessy = Pessoa("Jessy", "Gerente")
  val Tom = Pessoa("Tom", "Mecânico")
  val Bob = Pessoa("Bob", "Chef")
  val Thais = Pessoa("Thaís", "Engenheira")
  val Vini = Pessoa("Vini", "QA")
  val Zo = Pessoa("Zó", "Vendedor de Manga")

  val fb: Map[Pessoa, List[Pessoa]] =
    Map(Jessy -> List(Tom, Thais), Tom -> List(Jessy), Vini -> List(Thais, Zo, Bob), Bob -> List(Zo, Vini))

  val Q: mutable.Queue[Pessoa] = mutable.Queue(Jessy, Tom, Vini, Bob)

  @scala.annotation.tailrec
  def findManga(mapa: Map[Pessoa, List[Pessoa]],
                queue: mutable.Queue[Pessoa],
                jafoi: List[Pessoa] = List()): Pessoa = {
    println(queue + "\n")
    val q1: Pessoa = queue.dequeue

    if (q1.trab == "Vendedor de Manga") q1
    else {
      val findAmigos: Option[List[Pessoa]] = mapa.get(q1)

      findAmigos match {
        case Some(amigos) =>
          for (
            amigo <- amigos
          ) if (jafoi.contains(amigo) || queue.contains(amigo)) null else queue.enqueue(amigo)
          findManga(mapa, queue, q1 :: jafoi)
        case None =>
          findManga(mapa, queue, q1 :: jafoi)
      }

    }

  }

  println(findManga(fb, Q))

}
