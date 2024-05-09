
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
  val destruccionCruceroHorizontal = -2
  val destruccionCruceroVertical = -6
  val destruccionComandante = -3
  val destruccionDestructor = -4
  val destruccionJugador = -5
  val destruccionAlien = -7
  val destruccionNube = -8
  val destruccionCefalopodo = -9
}

object Main {
  import Auxiliar.Metodos
  import Constants._
  import scala.io.StdIn
  import java.util.Random

  val metodos:  Metodos = new Metodos // funciones auxiliares para el manejo de listas, numeros aleatorios y operaciones.

  // Pedir datos al usuario
  def obtenerDimensionesMatriz(): (Int, Int) = {
    /*
    @descripcion obtenerDimensionesMatriz: Pide al usuario las dimensiones del escenario de juego.
    */
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
    @descripcion
    @param
    @param
    @param
    */
  def imprimirFilaEscenario(matrix: List[Int], i: Int, n: Int, m: Int, vidas: Int, j: Int = 0): Unit = {
    /*
    @descripcion imprimirFilaEscenario: imprime una version con formato del escenario de juego.
    @param matrix: matriz escenario.
    @param i: contador con el fin de parar la recursion.
    @param n: numero establecido de filas.
    @param m: numero establecido de columnas.
    @param puntuacion: contiene el valor de la puntuacion del usuario.
    @param vidas: contiene el valor de las vidas del usuario.
    */
    // colores variados
    val color_RESET = "\u001B[0m"
    val color_RED = "\u001B[31m"
    val color_GREEN = "\u001B[32m"
    val color_YELLOW = "\u001B[33m"
    val color_brillante_GREEN = "\u001B[92m"
    val color_BLUE = "\u001B[34m"
    val color_PURPLE = "\u001B[35m"
    val color_CYAN = "\u001B[36m"
    val color_WHITE = "\u001B[37m"
    // si el contador es menor que el numero de filas
    if (i < n) {
      // si el contador es menor que la longitud del escenario
      if (j < metodos.longitudLista(matrix,0)) {
        // asignacion de los valores de las naves
        val valor = metodos.obtenerValorPosicion(j, matrix)
        val symbol = valor match {
          case `vacio` => "   "
          case `jugador` => if (vidas == 0) color_RED + " . " + color_RESET else color_BLUE + " W " + color_RESET
          case `muro` => color_CYAN +" B "+ color_RESET
          case `alien` => color_GREEN +" A " + color_RESET
          case `nube` => color_brillante_GREEN + " N " + color_RESET
          case `cefalopodo` => color_PURPLE +" C " + color_RESET
          case `destructor` => color_YELLOW + " D " + color_RESET
          case `crucero` => color_RED + " R " + color_RESET
          case `comandante` => color_WHITE + " X " + color_RESET
          //case _ => " " + valor + " "
          case _ => "   "
        }
        print(symbol)
        //bordes y recursividad
        if (j == metodos.longitudLista(matrix,0) - 1){
          print("|")
          println()
        }
        if ((j + 1) % m == 0 && j != metodos.longitudLista(matrix,0) - 1) {
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

  def imprimirBorde(m: Int, current: Int = 0): Unit = {
    /*
    @descripcion imprimirLineaSuperior: imprime los bordes superior e inferior del tablero.
    @param m: numero de columnas.
    */
    if (current < m) {
      print("---")
      imprimirBorde(m, current + 1)
    } else {
      print("+")
      println()
    }
  }

  def imprimirEscenario(matrix: List[Int], n: Int, m: Int, puntuacion: Int, vidas: Int): Unit = {
    /*
    @descripcion imprimirEscenario: imprime una version con formato del escenario.
    @param matrix: matriz escenario.
    @param n: numero establecido de filas.
    @param m: numero establecido de columnas.
    @param puntuacion: contiene el valor de la puntuacion del usuario.
    @param vidas: contiene el valor de las vidas del usuario.
    */
    println("\n     -- Escenario -- ")
    // parte superior del escenario
    print("  +")
    imprimirBorde(m)
    // contenido del escenario
    print("  |")
    imprimirFilaEscenario(matrix, 0, n, m, vidas)
    // parte inferior del escenario
    print("  +")
    imprimirBorde(m)
    // imprimimos la puntuación y vidas del usuario
    println(s"\n  < Puntuacion: $puntuacion > < vidas: $vidas >")
  }

  //Muros
  def listaBloques(col:Int):List[Int] = {
    // lista auxiliar que contien los valores de la fila de muros con los posibles muros
    col match{
      case 0 => Nil
      case _ => metodos.randomFunction()::listaBloques(col-1)
    }
  }

  def revisar(col:Int, lista:List[Int], long:Int):List[Int] = {
    // comprobacion y rectificacion si los muros cumplen su condicion de no mas de 3 juntos
    lista match{
      case Nil => Nil
      case l if long>=3 => 0::revisar(col-1,lista.tail, long-3)
      case l if lista.head==2 => lista.head::revisar(col-1, lista.tail, long+1)
      case _ => lista.head::revisar(col-1, lista.tail, long=0)
    }
  }

  def modificacion(lista:List[Int], lBlq:List[Int], fila:Int, col:Int, dimension:Int):List[Int] = {
    // en el caso de que haya mas de 3 juntos hacemos los cambios pertinentes
    dimension match{
      case 0 => Nil
      case d if d<=5*col && d>4*col => lBlq.head::modificacion(lista.tail, lBlq.tail, fila, col, dimension-1)
      case _ =>  lista.head::modificacion(lista.tail, lBlq, fila, col, dimension-1)
    }
  }

  def generarMuros(escenario: List[Int], numFilas: Int, numColumnas: Int, dimension: Int): List[Int]= {
    // funcion principal para generar muros
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
    // buscamos las posibles reconversiones usando funciones locales a modo de bucles
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
      // obtenemos un nuevo escenario despues de aplicar las reconversiones
      val escenarioNuevo = aplicarReconversion()
      // avanzamos una posicion en la lista para seguir buscamos, actualizando la fila y columna donde nos encontramos
      // siempre y cuando no nos salgamos de los limites del escenario
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
      // si la posicion esta fuera de los limites ya hemos revisado todo el escenario
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
    /*
    @descripcion desciendeNaves: funcion que desciende una posicion todas la naves del escenario
    @param matriz: el escenario
    @param fila: el numero de filas
    @param col: el numero de columnas
    */
    val dimension = fila*col
    // valores de las naves alienigenas
    def elemento(e:Int):Int ={
      if (e==7) {
        if (metodos.numeroAleatorio()<=0.5) return -2
        else return -6
      }else if(e==8) return -3
      else if(e==6) return -4
      else if(e==1) return -5
      else if(e==3) return -7
      else if(e==4) return -8
      else if(e==5) return -9
      else return -1
    }
    // procedemos a descender las naves
    def descensoNaves(posicion: Int, matriz: List[Int], col: Int, fila: Int): List[Int] = {
      //Caso base, comprueba que la posicion sea menor que la longitud
      if (matriz==Nil || posicion >= metodos.longitudLista(matriz,0)) {
        matriz
      }
      //Caso posición menos las dos ultimas filas, sin que posicion+col sea muro
      else if (posicion < (fila * col) - col*2 && metodos.obtenerValorPosicion(posicion+col, matriz)!=2) {
        val nuevoElemento = metodos.obtenerValorPosicion(posicion, matriz)
        if(nuevoElemento==2)
          metodos.insertarPosicion(nuevoElemento, posicion, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      }else if (posicion < (fila * col) - col*2 && metodos.obtenerValorPosicion(posicion+col, matriz)==2){
        val nuevoElemento = elemento(metodos.obtenerValorPosicion(posicion, matriz))
        if(nuevoElemento == -7 || nuevoElemento == -8 || nuevoElemento == -9  || nuevoElemento == -1 )
          metodos.insertarPosicion(2, posicion, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      }
      //Comprueba que la posición de abajo sea el jugador par asignarle el valor de destrucción
      else if(posicion < (fila * col)-col && metodos.obtenerValorPosicion(posicion+col, matriz)==1) {
        //Si posiciónn es 0 añade el jugador, si no, valor de destrucción
        if (metodos.obtenerValorPosicion(posicion, matriz) == 0) {
          metodos.insertarPosicion(1, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        } else {
          metodos.insertarPosicion(-5, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        }
        //mientras que posición + col sea distinto del jugador, añadimos valores de destrucción de las naves o 0 si era 0 el valor de la posicion
      }else if(posicion < (fila * col)-col && metodos.obtenerValorPosicion(posicion+col, matriz)!=1){
        val nuevoElemento = elemento(metodos.obtenerValorPosicion(posicion, matriz))
        if(metodos.obtenerValorPosicion(posicion, matriz)==3 || metodos.obtenerValorPosicion(posicion, matriz)==4 || metodos.obtenerValorPosicion(posicion, matriz)==5)
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        else if(metodos.obtenerValorPosicion(posicion, matriz)==0)
          metodos.insertarPosicion(0, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
        else
          metodos.insertarPosicion(nuevoElemento, posicion + col, descensoNaves(posicion + 1, matriz, col, fila))
      }else{
        return matriz
      }
    }

    val escenarioAux = descensoNaves(0, matriz, fila, col)

    return escenarioAux
  }

  // desintegracion de naves
  def destruccionNaveDestructor(escenario :List[Int], numFilas :Int, numColumnas :Int, pos :Int):List[Int] = {
    /*
    @descripcion destruccionNaveDestructor: esta funcion genera la explosion de radio 5 casillas del destructor cuando impacta contra la tierra o con un muro.
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    @param pos: posicion del jugador.
    */
    println("destruccion de destructor")
    // el radio de explision
    val radio = 5
    val dimension = numFilas*numColumnas
    // buscamos la posicion en la que empezar a destruir
    // el algoritmo situa la posicion en la esquina superior izquierda del cuadrado de elementos vacios que simularan la destruccion
    def buscarPosIni(posAux :Int, contador :Int):(Int,Int) = {
      if(posAux%numColumnas !=0 && contador <5){
        buscarPosIni(posAux-1,contador+1)
      }else{
        return (posAux-numColumnas*5,contador)
      }
    }
    val (posInicial,cont) = buscarPosIni(pos,0)
    // aplicamos la destruccion evitando desintegran los muros y destruyendo al jugador si esta en el radio de explosion
    def aplicarDestruccion(escenarioAux :List[Int], posAux :Int, posInicialAux :Int, contadorH :Int, contadorV :Int): List[Int] ={
      if(posAux < dimension && contadorV < radio*2){
        val valor = metodos.obtenerValorPosicion(posAux, escenarioAux)
        // en el caso de encontrarnos al jugador, muros o el jugador ya destruido
        def restricciones(escenarioRes :List[Int]):List[Int] = {
          if( valor == 1 || valor == 2 || valor == -5){
            if (valor == 1){
              return metodos.insertarPosicion(-5, posAux, escenarioRes)
            }else{
              return escenarioRes
            }
          }else{
            return metodos.insertarPosicion(0, posAux, escenarioRes)
          }
        }
        val escenarioNuevo = restricciones(escenarioAux)
        // para el cuadrado de destruccion generado utilizamos un contado horizontal y otro vertical que usan el radio como medida
        if((posAux+1)%numColumnas == 0 || contadorH >= radio*2-5+cont){
          val posInicialAuxNueva = posInicialAux+numColumnas
          val posAuxNueva = posInicialAuxNueva
          aplicarDestruccion(escenarioNuevo, posAuxNueva, posInicialAuxNueva, 0, contadorV+1)
        }else{
          val posAuxNueva = posAux+1
          aplicarDestruccion(escenarioNuevo, posAuxNueva, posInicialAux, contadorH+1, contadorV)
        }
      }else{
        return escenarioAux
      }
    }
    return aplicarDestruccion(escenario, posInicial, posInicial, 0, 0)
  }

  def destruccionHorizontal(escenario: List[Int], numColumnas: Int, numFilas: Int, pos: Int):List[Int] = {
    /*
    @descripcion destruccionHorizontal: genera la desruccion del crucero en su formato horizontal, arrasando con las casillas de esa fila.
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    @param pos: posicion del jugador.
    */
    println("destruccion de crucero")
    val dimension = numFilas*numColumnas

    def aplicarDestruccion(escenarioAux :List[Int], posAux :Int, mov :Int):List[Int] = {
      // funcion local que devuelve el escenario actualizado segun el valor encontrado, si encuentra un jugador este sera destruido, al igual que los muros
      def nuevoValor():List[Int] = {
        val valor = metodos.obtenerValorPosicion(posAux, escenarioAux)
        if( valor == 1 || valor ==`destruccionJugador` ){ // el jugador es afectado
          return metodos.insertarPosicion(`destruccionJugador`, posAux, escenarioAux)
        }else {
          return metodos.insertarPosicion(0, posAux, escenarioAux)
        }
      }
      // evitamos salir de la fila que estamos destruyendo y aplicamos recursividad
      if( mov < 0 && posAux%numColumnas == 0){
        return nuevoValor()
      }else if (posAux%numColumnas != 0) {

        val posAuxNueva = posAux + mov
        aplicarDestruccion(nuevoValor(), posAuxNueva, mov)
      }
      else
      {
        return escenarioAux
      }
    }
    // aplicamos la destruccion hacia la izquierda de la posicion del impacto de la nave
    val escenarioDestruido1 = aplicarDestruccion(escenario, pos, -1)
    // aplicamos la destruccion hacia la derecha de la posicion del impacto de la nave
    val escenarioDestruido2 = aplicarDestruccion(escenarioDestruido1, pos, 1)
    return escenarioDestruido2
  }

  def destruccionVertical(escenario: List[Int], numColumnas: Int, numFilas: Int, pos: Int):List[Int] = {
    /*
    @descripcion destruccionHorizontal: genera la desruccion del crucero en su formato vertical, arrasando con las casillas de esa columna.
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    @param pos: posicion del jugador.
    */
    println("destruccion de crucero")
    val dimension = numFilas*numColumnas
    // funcion local que devuelve el escenario actualizado segun el valor encontrado, si encuentra un jugador este sera destruido, al igual que los muros
    def aplicarDestruccion(escenarioAux :List[Int], posAux :Int, mov :Int):List[Int] = {
      if( posAux >0 && posAux < dimension){
        def nuevoValor():List[Int] = {
          val valor = metodos.obtenerValorPosicion(posAux, escenarioAux)
          if( valor == 1 || valor ==`destruccionJugador` ){ // el jugador es afectado
            return metodos.insertarPosicion(`destruccionJugador`, posAux, escenarioAux)
          }else {
            return metodos.insertarPosicion(0, posAux, escenarioAux)
          }
        }
        val posAuxNueva = posAux + mov * numColumnas
        aplicarDestruccion(nuevoValor(), posAuxNueva, mov)
      }else{
        return escenarioAux
      }
    }
    // aplicamos la destruccion hacia abajo de la posicion del impacto de la nave
    val escenarioDestruido1 = aplicarDestruccion(escenario, pos, -1)
    // aplicamos la destruccion hacia la arriba de la posicion del impacto de la nave
    val escenarioDestruido2 = aplicarDestruccion(escenarioDestruido1, pos, 1)
    return escenarioDestruido2
  }

  def desintegracionNaves(escenario :List[Int], numFilas :Int, numColumnas :Int, puntuacion :Int, vidas :Int):(List[Int], Int, Int) = {
    /*
    @descripcion desintegracionNaves: modifica el escenario de acuerdo a las naves que explotan o se estrellan contra la tierra
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    @param puntuacion: puntuacion del juego que tiene el jugador en el instante de llamada a esta funcion.
    @param vidas: cantidad de vidas que tiene el usuario en el momento de llamada a esta funcion.
    */
    val filaInicial = 0
    val colInicial = 0
    val posInicial = 0
    val dimension = numFilas*numColumnas
    // recursividad para recorrer el escenario
    def recorrerEscenario(escenarioAux :List[Int], pos :Int, fila :Int, col :Int):List[Int] = {
      if (pos < dimension) {

        val valor = metodos.obtenerValorPosicion(pos,escenarioAux)
        def aplicarDestrucciones():List[Int] = {
          // desintegracion en los muros
          if(fila == numFilas-5 ){
            // actualizamos el escenario si hay destrucciones
            def destruccionMuros():List[Int] = {
              valor match {
                case `destruccionCruceroHorizontal` => destruccionHorizontal(escenarioAux, numColumnas, numFilas, pos)
                case `destruccionCruceroVertical` => destruccionVertical(escenarioAux, numColumnas, numFilas, pos)
                case  `destruccionDestructor` => metodos.insertarPosicion(`muro`,pos,destruccionNaveDestructor(escenarioAux, numFilas, numColumnas, pos))
                case _ => escenarioAux
              }
            }
            return destruccionMuros()
          }
          // desintegracion en la tierra
          else if (fila == numFilas-1){
            valor match{
              case `destruccionDestructor` => destruccionNaveDestructor(escenarioAux, numFilas, numColumnas, pos)
              case `destruccionCruceroHorizontal` => destruccionHorizontal(escenarioAux, numColumnas, numFilas, pos)
              case `destruccionCruceroVertical` => destruccionVertical(escenarioAux, numColumnas, numFilas, pos)
              case _ => escenarioAux
            }

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
    // vidas
    def buscarJugador(escenarioAux :List[Int], posIn :Int):(List[Int],Int) = {
      // buscamos el jugador en la ultima fila y si esta destruido quitamos una vida e insertamos su valor normal de jugador
      if(posIn < dimension){
        val valor = metodos.obtenerValorPosicion(posIn, escenarioAux)
        if( valor == `destruccionJugador`){
          return (metodos.insertarPosicion(`jugador`,posIn,escenarioAux), vidas-1 )
        }else{
          buscarJugador(escenarioAux, posIn+1)
        }
      }else{
        return (escenarioAux, vidas)
      }
    }
    val (escenarioNuevo1,vidasNuevas) = buscarJugador(escenarioNuevo,numColumnas*numFilas-numColumnas)
    // Puntuaciones
    def incrementarPuntuacion(escenarioAux1 :List[Int], posIn1 :Int, contador :Int, contadorVidas :Int):(Int,Int) = {
      // las naves que hayan llegado a la ultima fila tendran su valor de destruccion, de este modo dependiendo de la nave destruida se sumara una puntuacion
      if(posIn1 < dimension) {
        val valor = metodos.obtenerValorPosicion(posIn1, escenarioAux1)
        valor match {
          case `destruccionAlien` =>  incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 5, contadorVidas )
          case `destruccionNube` =>  incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 25 , contadorVidas)
          case `destruccionCefalopodo` =>incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 15, contadorVidas )
          case `destruccionDestructor` =>incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 5, contadorVidas )
          case `destruccionCruceroVertical` => incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 13, contadorVidas )
          case `destruccionCruceroHorizontal` => incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 13, contadorVidas )
          case `destruccionComandante` => incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador + 100, contadorVidas+1 )
          case _ => incrementarPuntuacion(escenarioAux1 , posIn1+1 , contador, contadorVidas)
        }
      }else{
        return (contador, contadorVidas)
      }
    }
    val (puntos, vidasNuevas1) = incrementarPuntuacion(escenarioNuevo1,numColumnas*numFilas-numColumnas, puntuacion, vidasNuevas)
    return (escenarioNuevo1, puntos, vidasNuevas1)
  }
  // generacion de naves

  def randomAlien(): Int ={
    // esta funcion devuelve una nave alienigena distinta dependiendo de probabilidades
    val random = new Random()
    val randN = random.nextDouble() // Genera un número aleatorio entre 0.0 (inclusive) y 1.0 (exclusivo)
    if (randN <= 0.4) 3
    else if (randN>0.4 && randN<=0.65) 4
    else if (randN>0.65 && randN<=0.8) 5
    else if (randN>0.8 && randN<=0.85) 6
    else if (randN>0.85 && randN<=0.98) 7
    else 8
  }

  def generarNaves(matriz:List[Int], col:Int, fila:Int, dimension:Int):List[Int]= {
    // funcion para generar las naves alienigenas en la primera fila
    dimension match{
      case 0 => Nil
      case d if d>(fila*col)-col => randomAlien()::generarNaves(matriz.tail, col, fila, dimension-1)
      case _ => matriz.head::generarNaves(matriz.tail, col, fila, dimension-1)
    }
  }

  // pilotoAutomatico

  def eleccionCamino( izq :Int, der :Int, pos :Int, numFilas :Int, numColumnas :Int):Int = {
    /*
    @descripcion eleccionCamino: permite elegir el siguiente paso que dara el jugador en el modo piloto automatico.
                                El mejor camino siempre sera por el que reciba menos impactos, estos impactos esta contabilizados
                                en der e izq.
                                Por defecto, si hay el mismo numero de impactos a cada lado, se buscara el centro.
    @param izq: valor numerico de impactos si damos un paso a la izquierda.
    @param der: valor numerico de impactos si damos un paso a la derecha.
    @param pos: posicion del jugador en el escenario.
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    */
    // a continuacion devolveremos 1 para ir a la izquierda y 2 para ir a la derecha
    if (izq == der){
      // buscamos el centro
      if(numFilas*numColumnas-numColumnas/2-1<pos){
        return 1
      }else{
        return 2
      }
    }

    else if( izq < der){
      if(pos-1 < numFilas*numColumnas-numColumnas){
        //fuera de linea
        return 2
      }else{
        return 1
      }
    }else{
      if( pos+1 > numFilas*numColumnas-1){
        //fuera de linea
        return 1
      }else{
        return 2
      }
    }

  }

  def descensoAuxiliar(matriz: List[Int], fila: Int, col: Int): List[Int] = {
    /*
    @descripcion descensoAuxiliar: simulacion del descenso de naves en el escenario.
    @param fila: numero de filas.
    @param col: numero de columnas.
    */
    val nuevaMatriz = metodos.InicializarLista(fila * col)
    // recorremos el escenario recursivamente descendiendo las naves.
    // al ser una simulacion basica, no se aplican efectos colaterales ni reconversiones.
    def recorrerMatriz(posicion :Int, m :List[Int]):List[Int] = {
      if( posicion < fila*col-col){
        if(metodos.obtenerValorPosicion(posicion,matriz) != 2 ){
          return recorrerMatriz(posicion+1,metodos.insertarPosicion(metodos.obtenerValorPosicion(posicion,matriz), posicion+col, m))
        }else{
          val mNueva = metodos.insertarPosicion(2, posicion, m)
          return recorrerMatriz(posicion+1,mNueva)
        }
      }else{
        return m
      }
    }
    return recorrerMatriz(0, nuevaMatriz)
  }

  def pilotoAutomatico( escenario :List[Int], numColumnas :Int, numFilas :Int, posJugador :Int):Char = {
    /*
    @descripcion pilotoAutomatico: generacion del mejor camino posible en base a la simulacion del descenso de naves.
                                   Mediante la busqueda en el arbol de aridad 2 generado erecursivamente ( cada nodo tiene dos hijos izquierda y derecha)
                                   Minimiza el numero de impactos por naves alienigenas durante la simulacion del descenso de naves.
                                   EL resultado es un camino que contiene los pasos (izq o der) que debe dar el jugador para evitar ser impactado.
                                   Finalmente se tendra en cuenta solo el primer paso, pero la eleccion de este primer paso es realizada en base a los demas pasos dados.
                                   La profundidad del arbol es de 5, que comprende los suficientes movimientos para evitar naves a la altura de los muros.
    @param pos: posicion del jugador en el escenario.
    @param numFilas: numero de filas.
    @param numColumnas: numero de columnas.
    */
    def movimiento(escenarioAux :List[Int], pos :Int, contador :Int):(Int,List[Int]) = {
      // establecemos la profundidad de las soluciones (camino generado) en 5 ( profundidad del arbol)
      if ( contador < 5 && pos < numColumnas*numFilas && pos > numColumnas*numFilas-numColumnas-1){
        val valor = metodos.obtenerValorPosicion(pos, escenarioAux)
        val escenarioNuevo = descensoAuxiliar(escenarioAux, numFilas, numColumnas)
        // funcion que devuelve 1 si el jugador se encuentra con cualquier elemento que no sea el vacio
        def impactado():Int = {
          if( valor != 0){
            return 1
          }else{
            return 0
          }
        }
        // ramificacion hacia la izquierda
        val (izq, elec1) = movimiento(escenarioNuevo, pos-1, contador+1)
        // ramificacion hacia la derecha
        val (der, elec2) = movimiento(escenarioNuevo, pos+1, contador+1)
        // sumamos los impactos obtenidos a cada lado mas el de la posicion actual
        val izqAux = izq+impactado()
        val derAux = der+impactado()
        // elegimos el mejor camino en base a los resultados de la ramificacion izquierda/derecha
        val eleccion = eleccionCamino(izqAux,derAux, pos, numFilas, numColumnas)
        // dependiendo de la eleccion devolvemos el camino generado en la ramificacion derecha o el de izquierda
        def funcionAux():List[Int] = {
          if (eleccion <2){
            return elec1
          }else{
            return elec2
          }
        }

        def impactosAnteriores():Int = {
          if(eleccion <2){
            return izqAux
          }else{
            return derAux
          }
        }
        return (impactosAnteriores(),eleccion::funcionAux())
      }else{
        return (0,List())
      }
    }
    // obtenemos el camino generado y nImpactos (variable residual usada para el numero de impactos en al recurscion)
    val (nImpactos, eleccion) = movimiento(escenario, posJugador,0 )
    val elecciones = metodos.eliminarUltimo(eleccion)
    // finalmente nos quedamos con el primer movimiento que debe hacer.
    def entradaFinal():Char = {
      if (elecciones.head == 1){
        return 'a'
      }else{
        return 'd'
      }
    }
    return entradaFinal()
  }



  def main(args: Array[String]): Unit = {
    // Paquete de metodos auxiliares
    println(" - Dimensiones - ")

    // Obtener las dimensiones del escenario


    val (numFilas, numColumnas) = obtenerDimensionesMatriz()
    val dimension = numFilas*numColumnas
    println(s"Dimensiones del Escenario: $numFilas x $numColumnas")
    val escenarioVacio = metodos.InicializarLista(numFilas*numColumnas)

    // Puntuación y vidas
    val puntuacion = 0
    val vidas = 5

    // Generar muros y añadir usuario
    val escenarioInicial = generarMuros(escenarioVacio, numFilas, numColumnas, dimension)

    // Imprimir escenario
    imprimirEscenario(escenarioInicial, numFilas, numColumnas, puntuacion, vidas)

    def modo():Int = {
      val m = StdIn.readInt()
      m match {
        case 1 | 2 => m
        case _ => {
          println("No se ha introducido un modo correcto, introduzca de nuevo")
          modo()
        }
      }
    }
    println("---MODO DE EJECUCION---")
    println("1 - modo manual")
    println("2 - modo automatico")
    val modoejecucion = modo()
    // Movimiento y ejecucion del juego
    def movimiento(posicion: Int, escenario: List[Int], vidasJugador :Int, puntuacionJugador :Int): Unit = {
      if(vidasJugador >0) {
        if (modoejecucion == 1) {
          println(" mover jugador: ")
        }
        def valentrada(): Char = {
          if (modoejecucion == 1) {
            return StdIn.readChar() // entrada de teclado para el movimiento
          } else {
            Thread.sleep(1500)
            return pilotoAutomatico(escenario, numColumnas, numFilas, posicion)
          }


        }

        val entrada = valentrada()
        entrada match {
          case 'a' => {
            if (posicion - 1 > (numColumnas * numFilas - 1) || posicion - 1 < numColumnas * (numFilas - 1)) {
              println("Movimiento no válido")
              movimiento(posicion, escenario, vidasJugador, puntuacionJugador)
            } else {
              // actualizar posicion usuario
              val escenarioAux = metodos.insertarPosicion(0, posicion, escenario) // la posicion actual se vuelve vacia
              val escenarioAux1 = metodos.insertarPosicion(1, posicion - 1, escenarioAux) // la nueva posicion contiene al jugador
              // actualizarEscenario
              // reconversionNaves
              val escenarioNuevo = reconversionNaves(escenarioAux1, numFilas, numColumnas)
              //descensoNaves
              val escenarioNuevo1 = desciendeNaves(escenarioNuevo, numColumnas, numFilas)
              //desintegracionNaves
              val (escenarioNuevo2, puntuacionNueva, vidasNueva) = desintegracionNaves(escenarioNuevo1, numFilas, numColumnas, puntuacionJugador, vidasJugador)
              //generacionNaves
              val tablero = generarNaves(escenarioNuevo2, numColumnas, numFilas, numFilas * numColumnas)
              // Imprimir el escenario actualizado
              imprimirEscenario(tablero, numFilas, numColumnas, puntuacionNueva, vidasNueva)
              movimiento(posicion - 1, tablero, vidasNueva, puntuacionNueva)
            }
          }
          case 'd' => {
            if (posicion + 1 > (numColumnas * numFilas - 1) || posicion + 1 < numColumnas * (numFilas - 1)) {
              println("Movimiento no válido")
              movimiento(posicion, escenario, vidasJugador, puntuacionJugador)
            } else {
              // actualizar posicion usuario
              val escenarioAux = metodos.insertarPosicion(0, posicion, escenario) // la posicion actual se vuelve vacia
              val escenarioAux1 = metodos.insertarPosicion(1, posicion + 1, escenarioAux) // la nueva posicion contiene al jugador
              // actualizarEscenario
              // reconversionNaves
              val escenarioNuevo = reconversionNaves(escenarioAux1, numFilas, numColumnas)
              //descensoNaves
              val escenarioNuevo1 = desciendeNaves(escenarioNuevo, numColumnas, numFilas)
              //desintegracionNaves
              val (escenarioNuevo2, puntuacionNueva, vidasNueva) = desintegracionNaves(escenarioNuevo1, numFilas, numColumnas, puntuacionJugador, vidasJugador)
              //generacionNaves
              val tablero = generarNaves(escenarioNuevo2, numColumnas, numFilas, numFilas * numColumnas)
              // Imprimir el escenario actualizado
              imprimirEscenario(tablero, numFilas, numColumnas, puntuacionNueva, vidasNueva)
              movimiento(posicion + 1, tablero, vidasNueva, puntuacionNueva)
            }
          }
          case _ => {
            println("Movimiento no válido")
            movimiento(posicion, escenario, vidasJugador, puntuacionJugador)
          }
        }
      }else{
        println("--FIN DEL JUEGO--")
      }

    }
    // ejecutar el movimiento
    val pos = ((numColumnas/2)+(numFilas-1)*numColumnas)
    movimiento(pos, escenarioInicial, vidas, puntuacion)
  }
}