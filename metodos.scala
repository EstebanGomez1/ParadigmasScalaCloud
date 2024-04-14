package auxiliar {
  class metodos {
    def llenarLista(dimension: Int): List[String] = dimension match {
      case 0 => Nil
      case _ => " " :: llenarLista(dimension - 1)
    }
    def suma(a: Int, b: Int): Int = {
      return a + b
    }
    def resta(a: Int, b: Int): Int = {
      return a - b
    }
  }
}