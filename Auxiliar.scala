package Auxiliar {

  class Metodos {

    def InicializarLista(dimension: Int): List[Int] = dimension match {
      case 0 => Nil
      case _ => 0 :: InicializarLista(dimension - 1)
    }

    def suma(a: Int, b: Int): Int = {
      return a + b
    }

    def resta(a: Int, b: Int): Int = {
      return a - b
    }

  }

}