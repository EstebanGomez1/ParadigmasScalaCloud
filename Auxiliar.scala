package Auxiliar {

  import java.util.Random

  class Metodos {

    def InicializarLista(dimension: Int): List[Int] = dimension match {
      /*
      @descripcion InicializarLista: inicializa una lista de tamaño finito con valores a 0
      @param dimension: tamaño de la lista.
      */
      case 0 => Nil
      case _ => 0 :: InicializarLista(dimension - 1)
    }

    def suma(a: Int, b: Int): Int = {
      /*
      @descripcion suma: suma de dos enteros
      @param a: primer operando
      @param b: segundo operando
      */
      return a + b
    }

    def resta(a: Int, b: Int): Int = {
      /*
      @descripcion resta: diferencia de dos enteros
      @param a: primer operando
      @param b: segundo operando
      */
      return a - b
    }

    def randomFunction(): Int = {
      /*
      @descripcion randomFunction: devuelve 2 0 aleatoriamente
      */
      val random = new Random()
      val randomNumber = random.nextDouble() // Genera un número aleatorio entre 0.0 (inclusive) y 1.0 (exclusivo)
      if (randomNumber < 0.5) 2 else 0
    }

    def numeroAleatorio(): Double = {
      /*
      @descripcion numeroAleatorio: devuelve un numero aleatorio entre 0 y 1
      */
      val random = new Random()
      random.nextDouble()
    }


    def insertarPosicion(e: Int, pos: Int, lista: List[Int]): List[Int] = {
      /*
      @descripcion insertarPosicion: inserta un valor en una posicion determinada de una lista de enteros
      @param e: valor a insertar
      @param pos: posicion en la que insertar
      @param lista: lista donde insertar
      */
      lista match {
        case Nil => e::Nil
        case _  => pos match {
          case 0 => e::lista.tail
          case _ => lista.head::insertarPosicion(e,(pos-1),lista.tail)
        }
      }
    }

    def insertar(x: Int, lista: List[Int]): List[Int] = {
      /*
      @descripcion insertar: inserta un elemento al principio de una lista
      @param x: elemento
      @param lista: lista donde insertar
      */
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
      /*
      @descripcion obtenerValorPosicion: devuelve el valor de una posicion concreta de la lista
      @param pos: posicion donde obtener el valor
      @param lista: lista donde buscar
      */
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
      /*
      @descripcion longitudLista: devuelve la longitud de una lista
      @param lista: lista
      */
      lista match{
        case Nil => long
        case _ => longitudLista(lista.tail, long+1)
      }

    def eliminarUltimo(lista: List[Int]): List[Int] = {
      /*
      @descripcion eliminarUltimo: elimina el ultimo elemento de una lista
      @param lista: lista
      */
      lista match {
        case Nil          => Nil // Si la lista es vacía, retornamos una lista vacía
        case _ :: Nil     => Nil // Si la lista tiene un solo elemento, retornamos una lista vacía
        case cabeza :: cola => cabeza :: eliminarUltimo(cola) // Caso recursivo: mantenemos la cabeza y eliminamos el último elemento de la cola
      }
    }


  }

}