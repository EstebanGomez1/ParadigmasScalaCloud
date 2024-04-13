package aux{
  class metodos {
    def llenarLista(dimension: Int): List[String] = dimension match {
      case 0 => Nil
      case _ => " "::llenarLista(dimension-1)
    }

}