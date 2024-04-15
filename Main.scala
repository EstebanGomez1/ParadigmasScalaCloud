
object Constants {
  val vacio: Int = 0
  val jugador: Int = 1
  val muro: Int = 2
  val alien: Int = 3
  val nube: Int = 4
  val cefalopodo: Int = 5
  val destructor: Int = 6
  val crucero: Int = 7
  val comandante: Int = 8
}

object Main {
  import Auxiliar.Metodos
  import scala.io.StdIn
  import Constants._

  // Pedir datos al usuario
  def obtenerDimensionesMatriz(): (Int, Int) = {
    try {
      // Pedir al usuario que introduzca el valor de la fila
      print("Introduce el numero de filas: ")
      val fila = StdIn.readInt()

      // Pedir al usuario que introduzca el valor de la columna
      print("Introduce el numero de columnas: ")
      val columna = StdIn.readInt()

      // Return dimensiones
      (fila, columna)
    } catch {
      case e: NumberFormatException => {
        println("Error: Debes introducir un número válido.")
        // Llamar a la función de nuevo para permitir al usuario volver a intentarlo
        obtenerDimensionesMatriz()
      }
    }
  }

  // Imprimir por pantalla
  /*
    @descripcion printEscenario: imprime una version con formato del escenario de juego.
    @param matriz: puntero a matriz escenario.
    @param n: numero establecido de filas.
    @param m: numero establecido de columnas.
    @param puntuacion: contiene el valor de la puntuacion del usuario.
    @param vidas: contiene el valor de las vidas del usuario.
    */
  def imprimirFilaEscenario(matrix: List[Int], i: Int, n: Int, m: Int, vidas: Int, j: Int = 0): Unit = {
    if (i < n) {


      if (j < matrix.length) {
        val valor = matrix(j)
        val symbol = valor match {
          case `vacio` => "   "
          case `jugador` => if (vidas == 0) " . " else " W "
          case `muro` => " B "
          case `alien` => " A "
          case `nube` => " N "
          case `cefalopodo` => " C "
          case `destructor` => " D "
          case `crucero` => " R "
          case `comandante` => " X "
          case _ => " " + valor + " "
        }
        print(symbol)
        if (j == matrix.length - 1){
          print("|")
          println()
        }
        if ((j + 1) % m == 0 && j != matrix.length - 1) {
          print("|")
          println()
          print("  |")
          imprimirFilaEscenario(matrix, i + 1, n, m, vidas, j + 1)
        } else {
          imprimirFilaEscenario(matrix, i, n, m, vidas, j + 1)
        }
      }
    }
  }

  def imprimirLineaSuperior(m: Int, current: Int = 0): Unit = {
    if (current < m) {
      print("---")
      imprimirLineaSuperior(m, current + 1)
    } else {
      print("+")
      println()
    }
  }

  def imprimirEscenario(matrix: List[Int], n: Int, m: Int, puntuacion: Int, vidas: Int): Unit = {
    println("\n     -- Escenario -- ")
    // parte superior del escenario
    print("  +")
    imprimirLineaSuperior(m)
    // contenido del escenario
    print("  |")
    imprimirFilaEscenario(matrix, 0, n, m, vidas)
    // parte inferior del escenario
    print("  +")
    imprimirLineaSuperior(m)
    // imprimimos la puntuación y vidas del usuario
    println(s"\n  < Puntuacion: $puntuacion > < vidas: $vidas >")
  }

  // Reconversion de naves

  // Descenso de naves


  def main(args: Array[String]): Unit = {
    // Paquete de metodos auxiliares
    val metodos:  Metodos = new Metodos
    println(" - Dimensiones - ")

    // Obtener las dimensiones del escenario
    val (numFilas, numColumnas) = obtenerDimensionesMatriz()
    println(s"Dimensiones del Escenario: $numFilas x $numColumnas")
    val escenario = metodos.InicializarLista(numFilas*numColumnas)

    // Puntuación y vidas del usuario de prueba
    val puntuacion: Int = 100
    val vidas: Int = 3

    // imprimir el escenario
    val matrizPrueba: List[Int] = List(
      0, 1, 0, 2, 0,
      3, 0, 0, 0, 4,
      0, 0, 5, 0, 0,
      6, 0, 0, 0, 7,
      0, 8, 0, 9, 0
    )
    imprimirEscenario(matrizPrueba, numFilas, numColumnas, puntuacion, vidas)
  }

  /*def tablero(fila:Int, col:Int, dimension:Int):List[Int] =
    dimension match{
      case 0 => Nil
      case d if d<=2 *col && d>1*col => randomFunction()::tablero(fila, col, dimension-1)
      case d if d==((col/2)+1) => 1::tablero(fila, col, dimension-1)
      case _ =>  0::tablero(fila, col, dimension-1)
    }


def randomFunction(): Int = {
  val random = new Random()
  val randomNumber = random.nextDouble() // Genera un número aleatorio entre 0.0 (inclusive) y 1.0 (exclusivo)
  if (randomNumber < 0.5) 5 else 0
}

def modificacion(lista:List[Int], fila:Int, col:Int, dimension:Int):List[Int] =
    dimension match{
      case 0 => Nil
      case d if d<=3*col && d>2*col => lista.head::modificacion(lista.tail, fila, col, dimension-1)
      case _ =>  lista.head::modificacion(lista.tail, fila, col, dimension-1)
}
val tablanueva =modificacion(tablero(5, 5, dimension), fila, col, dimension)
println(tablanueva)*/ //FALTA PROBAR MEDIANAMENTE FUNCIONA
}
