import java.util.Random
import scala.math._

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
  import Constants._
  import Auxiliar.Metodos
  import scala.io.StdIn

  val metodos:  Metodos = new Metodos

  // Pedir datos al usuario
  def obtenerDimensionesMatriz(): (Int, Int) = {
    try {
      // Pedir al usuario que introduzca el valor de la fila
      print("Introduce el numero de filas: ")
      val fila = StdIn.readInt()
      if ( fila < 15){
        println("Dimension no valida")
        obtenerDimensionesMatriz()
      }
      else
        {
          // Pedir al usuario que introduzca el valor de la columna
          print("Introduce el numero de columnas: ")
          val col = StdIn.readInt()
          if ( col < 10){
            println("Dimension no valida")
            obtenerDimensionesMatriz()
          }else{
            // Return dimensiones
            (fila, col)
          }
        }
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

  //Muros
  def listaBloques(col:Int):List[Int] =
    col match{
      case 0 => Nil
      case _ => metodos.randomFunction()::listaBloques(col-1)
    }

  def revisar(col:Int, lista:List[Int], long:Int):List[Int] =
    lista match{
      case Nil => Nil
      case l if long>=3 => 0::revisar(col-1,lista.tail, long-3)
      case l if lista.head==2 => lista.head::revisar(col-1, lista.tail, long+1)
      case _ => lista.head::revisar(col-1, lista.tail, long=0)
    }

  def modificacion(lista:List[Int], lBlq:List[Int], fila:Int, col:Int, dimension:Int):List[Int] =
    dimension match{
      case 0 => Nil
      case d if d<=5*col && d>4*col => lBlq.head::modificacion(lista.tail, lBlq.tail, fila, col, dimension-1)
      case _ =>  lista.head::modificacion(lista.tail, lBlq, fila, col, dimension-1)
    }

  def generarMuros(escenario: List[Int], numFilas: Int, numColumnas: Int, dimension: Int): List[Int]= {
    val listanueva = listaBloques(numColumnas)
    val revisada = revisar(numColumnas, listanueva, 0)
    val tablanueva = modificacion(escenario, revisada, numFilas, numColumnas, dimension)
    // añadir usuario al escenario
    return  metodos.insertarPosicion(1,((numColumnas/2)+(numFilas-1)*numColumnas), tablanueva)
  }

  // Reconversion de naves

  // Descenso de naves
  def filaAliens(col:Int):List[Int] = col match{
    case 0 => Nil
    case _ => metodos.randomAlien()
  }
  def descensoNaves(filaAliens:List[Int], matriz:List[Int], long:Int):List[Int] =
    matriz match{
      case Nil => Nil
      case m if m.head==2 => 2::descensoNaves(filaAliens.tail, matriz.tail, long-1)
      case m if long>0 => filaAliens.head::descensoNaves(filaAliens.tail, matriz.tail, long-1)
      case _ => matriz.head::descensoNaves(filaAliens, matriz.tail, long)
    }
  //descensoNaves(naves, matriz, longitudLista(naves,0))
  // desintegracion de naves

  // generacion de naves


  def main(args: Array[String]): Unit = {
    // Paquete de metodos auxiliares
    println(" - Dimensiones - ")

    // Obtener las dimensiones del escenario
    val numFilas = 15
    val numColumnas = 11
    /*val matrizPrueba: List[Int] = List(
      0, 1, 0, 2, 0,
      3, 0, 0, 0, 4,
      0, 0, 5, 0, 0,
      6, 0, 0, 0, 7,
      0, 8, 0, 9, 0
    )*/

    //val (numFilas, numColumnas) = obtenerDimensionesMatriz()
    val dimension = numFilas*numColumnas
    println(s"Dimensiones del Escenario: $numFilas x $numColumnas")
    val escenarioVacio = metodos.InicializarLista(numFilas*numColumnas)

    // Puntuación y vidas del usuario de prueba
    val puntuacion: Int = 100
    val vidas: Int = 3

    // Generar muros y añadir usuario
    val escenarioInicial = generarMuros(escenarioVacio, numFilas, numColumnas, dimension)

    // Imprimir escenario
    imprimirEscenario(escenarioInicial, numFilas, numColumnas, puntuacion, vidas)

    // Movimiento y ejecucion del juego
    def movimiento(posicion: Int, escenario: List[Int]): Unit = {
        println(" mover jugador: ")
        val entrada = StdIn.readChar() // entrada de teclado para el movimiento
        entrada match {
          case 'a' => {
            if (posicion-1 > (numColumnas*numFilas - 1) || posicion-1 < numColumnas*(numFilas-1)) {
              println("Movimiento no válido")
              movimiento(posicion, escenario)
            } else {
              // actualizar posicion usuario
              val escenarioAux = metodos.insertarPosicion(0,posicion,escenario) // la posicion actual se vuelve vacia
              val escenarioNuevo = metodos.insertarPosicion(1,posicion-1, escenarioAux) // la nueva posicion contiene al jugador
              // actualizarEscenario
              //reconversionNaves
              //descensoNaves
              //desintegracionNaves
              //generacionNaves
              imprimirEscenario(escenarioNuevo, numFilas, numColumnas, puntuacion, vidas)
              movimiento(posicion-1, escenarioNuevo)
            }
          }
          case 'd' => {
            if (posicion+1 > (numColumnas*numFilas - 1) || posicion+1 < numColumnas*(numFilas-1)) {
              println("Movimiento no válido")
              movimiento(posicion, escenario)
            } else {
              // actualizar posicion usuario
              val escenarioAux = metodos.insertarPosicion(0,posicion,escenario) // la posicion actual se vuelve vacia
              val escenarioNuevo = metodos.insertarPosicion(1,posicion+1, escenarioAux) // la nueva posicion contiene al jugador
              // actualizarEscenario
              //reconversionNaves
              //descensoNaves
              //desintegracionNaves
              //generacionNaves
              imprimirEscenario(escenarioNuevo, numFilas, numColumnas, puntuacion, vidas)
              movimiento(posicion+1, escenarioNuevo)
            }
          }
          case _   => {
            println("Movimiento no válido")
            movimiento(posicion, escenario)
          }
        }
    }
    // ejecutar el movimiento
    val pos = ((numColumnas/2)+(numFilas-1)*numColumnas)
    movimiento(pos, escenarioInicial)

  }
}