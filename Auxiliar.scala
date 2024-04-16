package Auxiliar {

  import java.util.Random

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

    def randomFunction(): Int = {
      val random = new Random()
      val randomNumber = random.nextDouble() // Genera un número aleatorio entre 0.0 (inclusive) y 1.0 (exclusivo)
      if (randomNumber < 0.5) 2 else 0
    }

    def randomAlien(): Int ={
      val random = new Random()
      val randN = random.nextDouble() // Genera un número aleatorio entre 0.0 (inclusive) y 1.0 (exclusivo)
      if (randN <= 0.4) 3
      else if (randN>0.4 && randN<=0.65) 4
      else if (randN>0.65 && randN<=0.8) 5
      else if (randN>0.8 && randN<=0.85) 6
      else if (randN>0.85 && randN<=0.98) 7
      else if (randN>0.98 && randN<=1) 8
    }


    def insertarPosicion(e: Int, pos: Int, lista: List[Int]): List[Int] = {
      lista match {
        case Nil => e::Nil
        case _  => pos match {
          case 0 => e::lista.tail
          case _ => lista.head::insertarPosicion(e,(pos-1),lista.tail)
        }
      }
    }

    def insertar(x: Int, lista: List[Int]): List[Int] = {
      lista match {
        case Nil => x :: Nil
        case head :: tail => {
          head match {
            case _ if (x <= head) => x :: lista
            case _ => head :: insertar(x, tail)
          }
        }
      }
    }

    def longitudLista(lista:List[Int], long:Int):Int =
      lista.match{
        case Nil => long
        case _ => longitud(lista.tail, long+1)
      }

  }

}