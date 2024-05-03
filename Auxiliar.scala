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

    def numeroAleatorio(): Double = {
      val random = new Random()
      random.nextDouble()
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

    def obtenerValorPosicion(pos :Int, lista :List[Int]): Int = {
      lista match {
        case Nil => 0
        case head :: tail =>
          if (pos == 0) {
            head
          } else {
            obtenerValorPosicion(pos - 1, tail)
          }
      }
    }

    def longitudLista(lista:List[Int], long:Int):Int =
      lista match{
        case Nil => long
        case _ => longitudLista(lista.tail, long+1)
      }

    def eliminarUltimo(lista: List[Int]): List[Int] = {
      lista match {
        case Nil          => Nil // Si la lista es vacía, retornamos una lista vacía
        case _ :: Nil     => Nil // Si la lista tiene un solo elemento, retornamos una lista vacía
        case cabeza :: cola => cabeza :: eliminarUltimo(cola) // Caso recursivo: mantenemos la cabeza y eliminamos el último elemento de la cola
      }
    }


  }

}