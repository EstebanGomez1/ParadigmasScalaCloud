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
  val destruccion = -1
  val destruccionCruceroHorizontal = -2
  val destruccionCruceroVertical = -6
  val destruccionComandante = -3
  val destruccionDestructor = -4
  val destruccionJugador = -5
}

object Main {
  import Auxiliar.Metodos
  import Constants._

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
  def reconversionNaves( escenario: List[Int], numFilas :Int, numColumnas :Int):List[Int] = {
    /*
    @descripcion reconversionNaves: esta funcion modifica naves o conjuntos de naves en base a unas restricciones y condiciones del entorno
    @param escenario: lista que contiene el escenario
    @param numFilas: numero de filas del escenario
    @param numColumnas: numero de columnas del escenario
     */
    val filaInicial = 0
    val colInicial = 0
    val posInicial = 0
    val dimension = numFilas*numColumnas

    def buscarReconversion(escenarioAux :List[Int], pos :Int, fila :Int, col: Int):List[Int] ={

      def aplicarReconversion():List[Int] = {
        if( fila > 0 && fila < (numFilas-1) && col > 0 && col < (numColumnas-1)){
          // buscar condiciones para la reconversion
          if( metodos.obtenerValorPosicion(pos, escenarioAux) == alien || metodos.obtenerValorPosicion(pos, escenarioAux) == nube){
            if (metodos.obtenerValorPosicion(pos-numColumnas, escenarioAux) == alien && metodos.obtenerValorPosicion(pos+numColumnas, escenarioAux) == alien && metodos.obtenerValorPosicion(pos-1, escenarioAux) == alien && metodos.obtenerValorPosicion(pos+1, escenarioAux) == alien) {
              println("hay una reconversion")
              val escenarioAux1 = metodos.insertarPosicion(0, pos - numColumnas, escenarioAux)
              val escenarioAux2 = metodos.insertarPosicion(0, pos + numColumnas, escenarioAux1)
              val escenarioAux3 = metodos.insertarPosicion(0, pos - 1, escenarioAux2)
              val escenarioAux4 = metodos.insertarPosicion(0, pos + 1, escenarioAux3)
              if (metodos.obtenerValorPosicion(pos, escenarioAux4) == alien) {
                // alienigena rodeado de alienigenas genera nube
                return metodos.insertarPosicion(nube, pos, escenarioAux4)
              } else {
                // nube rodeada de alienigenas genera cefalopodo
                return metodos.insertarPosicion(cefalopodo, pos, escenarioAux4)
              }
            }
          }
          // comandante genera nubes alrededor
          if (metodos.obtenerValorPosicion(pos, escenarioAux) == comandante){
            val prob = metodos.numeroAleatorio()
            prob match {
              case prob if prob <= 0.1 => {
                def generarNubes( pos :Int, escenarioNubes :List[Int]):List[Int]={
                  if(metodos.obtenerValorPosicion(pos, escenarioNubes) == vacio){
                    return metodos.insertarPosicion(nube, pos, escenarioNubes)
                  }else{
                    return escenarioNubes
                  }
                }
                val escenarioAux1 = generarNubes(pos-numColumnas,escenarioAux)
                val escenarioAux2 = generarNubes(pos+numColumnas,escenarioAux1)
                val escenarioAux3 = generarNubes(pos-1, escenarioAux2)
                return generarNubes(pos+1, escenarioAux3)
              }
              case _ => escenarioAux
            }
          }
        }
        return escenarioAux
      }
      val escenarioNuevo = aplicarReconversion()
      val posAux = pos+1
      def actualizarFilas( posAux :Int, fila :Int):  Int={
        if(posAux%numColumnas == 0){
          return fila+1
        }else{
          return fila
        }
      }
      val filaAux = actualizarFilas(posAux,fila)
      def actualizarCol():Int = {
        if(col+1 >numColumnas-1){
          return 0
        }else{
          return col+1
        }
      }
      val colAux = actualizarCol()
      if(posAux >= dimension){
        return escenarioNuevo
      }else {
        buscarReconversion(escenarioNuevo, posAux, filaAux, colAux)
      }
    }
    return buscarReconversion(escenario, posInicial, filaInicial, colInicial)

  }
  // Descenso de naves
  def desciendeNaves(matriz:List[Int], fila:Int, col:Int):List[Int] = {

    val dimension = fila*col

    def elemento(e:Int):Int ={
      if (e==7) {
        if (metodos.numeroAleatorio()<=0.5) return -2
        else return -6
      }else if(e==8) return -3
      else if(e==6) return -4
      else if(e==1)return -5
      else return 2
    }
    def descensoNaves(posicion: Int, matriz: List[Int], col: Int, fila: Int): List[Int] = {
      if (matriz==Nil || posicion >= metodos.longitudLista(matriz,0)) {
        matriz
      } else if (posicion < (fila * col) - col*2 && matriz(posicion+col)!=2) {
        val nuevoElemento = matriz(posicion)
        if(nuevoElemento==2)
          metodos.insertarPosicion(nuevoElemento, posicion, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      }else if (posicion < (fila * col) - col*2 && matriz(posicion+col)==2){
        val nuevoElemento = elemento(matriz(posicion))
        if(nuevoElemento==2)
          metodos.insertarPosicion(nuevoElemento, posicion, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      } else if(posicion < (fila * col)-col && matriz(posicion+col)!=1){
        val nuevoElemento = elemento(matriz(posicion))
        if(matriz(posicion)==3 || matriz(posicion)==4 || matriz(posicion)==5)
          metodos.insertarPosicion(-1, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        else if(matriz(posicion)==0)
          metodos.insertarPosicion(0, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      } else if(posicion < (fila * col)-col && matriz(posicion)==1){
        val nuevoElemento = elemento(matriz(posicion))
        if(matriz(posicion)!=0)
          metodos.insertarPosicion(nuevoElemento, posicion+col, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(1, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      }else if(posicion < (fila * col) && posicion>(fila*col)-col){
        metodos.insertarPosicion(matriz(posicion), posicion, descensoNaves(posicion+1, matriz, col, fila))
      }else{
        metodos.insertarPosicion(matriz(posicion), posicion, descensoNaves(posicion+1, matriz, col, fila))
      }
    }

    val escenarioAux = descensoNaves(0, matriz, fila, col)

    return escenarioAux
  }

  // desintegracion de naves
  def destruccionNaveDestructor(escenario :List[Int], pos :Int):List[Int] = {
    return List(0)
  }

  def destruccionHorizontal(escenario: List[Int], col: Int, fila: Int, pos: Int):List[Int] = {
    return List(0)
  }

  def destruccionVertical(escenario: List[Int], col: Int, fila: Int, pos: Int):List[Int] = {
    return List(0)
  }

  def desintegracionNaves(escenario :List[Int], numFilas :Int, numColumnas :Int, puntuacion :Int, vidas :Int):(List[Int], Int, Int) = {
    val filaInicial = 0
    val colInicial = 0
    val posInicial = 0
    val dimension = numFilas*numColumnas
    def recorrerEscenario(escenarioAux :List[Int], pos :Int, fila :Int, col :Int):List[Int] = {
      if (pos < dimension) {
        // desintegracion en los muros
        val valor = metodos.obtenerValorPosicion(pos,escenarioAux)
        def aplicarDestrucciones():List[Int] = {
          if(fila == numFilas-5 ){
            def destruccionMuros():List[Int] = {
              valor match {
                case `destruccionCruceroHorizontal` => destruccionHorizontal(escenarioAux, col, fila, pos)
                case `destruccionCruceroVertical` => destruccionVertical(escenarioAux, col, fila, pos)
                case  `destruccionDestructor` => destruccionNaveDestructor(escenarioAux, pos)
                case _ => escenarioAux
              }
            }
            return destruccionMuros()
          }
          // desintegracion en la tierra
          else if (fila == numFilas-1 && valor == `destruccionDestructor`){
            return destruccionNaveDestructor(escenarioAux, pos)
          }else{
            return escenarioAux
          }
        }

        // actualizar datos para la siguiente iteracion
        def actualizarFilas(posAux: Int, fila: Int): Int = {
          if (posAux % numColumnas == 0) {
            return fila + 1
          } else {
            return fila
          }
        }
        val posAux = pos + 1
        val filaNueva = actualizarFilas(posAux, fila)
        recorrerEscenario(aplicarDestrucciones(), posAux, filaNueva, col)
      }else{
        return escenarioAux
      }
    }
    val escenarioNuevo = recorrerEscenario(escenario, posInicial, filaInicial, colInicial)
    return (escenarioNuevo, puntuacion, vidas)
  }
  // generacion de naves


  def main(args: Array[String]): Unit = {
    // Paquete de metodos auxiliares
    println(" - Dimensiones - ")

    // Obtener las dimensiones del escenario
    val numFilas = 15
    val numColumnas = 11
    /*
    val matrizPrueba: List[Int] = List(
      0, 3, 0, 2, 0,
      3, 3, 3, 0, 4,
      0, 3, 6, 3, 0,
      6, 0, 0, 8, 3,
      0, 8, 0, 3, 0
    )
    */

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

    def modo():Char = {
      println("---MODO DE EJECUCION---")
      println("1 - modo automatico")
      println("2 - modo manual")
      val m = StdIn.readChar()
      /*if(m.'1' || m!='2'){
        println("No se ha introducido un modo correcto, introduzca de nuevo")
        return modo()
      }*/
      return m
    }

    val modoejecucion = modo()
    // Movimiento y ejecucion del juego
    def movimiento(posicion: Int, escenario: List[Int]): Unit = {
      println(" mover jugador: ")
      def valentrada():Char = {
        if (modoejecucion == 2) {
          return StdIn.readChar() // entrada de teclado para el movimiento
        } else {
          if (metodos.randomFunction > 0.5) {
            if (posicion-1 > (numColumnas*numFilas - 1) || posicion-1 < numColumnas*(numFilas-1)) {
              return valentrada()
            }else {
              Thread.sleep(1500)
              return 'a'
            }

          } else {
          if (posicion+1 > (numColumnas*numFilas - 1) || posicion+1 < numColumnas*(numFilas-1)) {
            return valentrada()
          }else {
            Thread.sleep(1500)
            return 'd'
          }
          }
        }
      }
      val entrada = valentrada()
      entrada match {
        case 'a' => {
          if (posicion-1 > (numColumnas*numFilas - 1) || posicion-1 < numColumnas*(numFilas-1)) {
            println("Movimiento no válido")
            movimiento(posicion, escenario)
          } else {
            // actualizar posicion usuario
            val escenarioAux = metodos.insertarPosicion(0,posicion,escenario) // la posicion actual se vuelve vacia
            val escenarioAux1 = metodos.insertarPosicion(1,posicion-1, escenarioAux) // la nueva posicion contiene al jugador
            // actualizarEscenario
            // reconversionNaves
            val escenarioNuevo = reconversionNaves(escenarioAux1, numFilas, numColumnas)
            //descensoNaves
            val escenarioNuevo1 = desciendeNaves(escenarioNuevo, numColumnas, numFilas)
            //desintegracionNaves
            //generacionNaves
            val tablero = metodos.rellenar(escenarioNuevo1, numColumnas, numFilas, numFilas*numColumnas)
            // Imprimir el escenario actualizado
            imprimirEscenario(tablero, numFilas, numColumnas, puntuacion, vidas)
            movimiento(posicion-1, tablero)
          }
        }
        case 'd' => {
          if (posicion+1 > (numColumnas*numFilas - 1) || posicion+1 < numColumnas*(numFilas-1)) {
            println("Movimiento no válido")
            movimiento(posicion, escenario)
          } else {
            // actualizar posicion usuario
            val escenarioAux = metodos.insertarPosicion(0,posicion,escenario) // la posicion actual se vuelve vacia
            val escenarioAux1 = metodos.insertarPosicion(1,posicion+1, escenarioAux) // la nueva posicion contiene al jugador
            // actualizarEscenario
            // reconversionNaves
            val escenarioNuevo = reconversionNaves(escenarioAux1, numFilas, numColumnas)
            //descensoNaves
            val escenarioNuevo1 = desciendeNaves(escenarioNuevo, numColumnas, numFilas)
            //desintegracionNaves
            //generacionNaves
            val tablero = metodos.rellenar(escenarioNuevo1, numColumnas, numFilas, numFilas*numColumnas)
            // Imprimir el escenario actualizado
            imprimirEscenario(tablero, numFilas, numColumnas, puntuacion, vidas)
            movimiento(posicion+1, tablero)
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