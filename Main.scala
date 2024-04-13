object Main{
  import aux.metodos
  import scala.io.StdIn
  def main(args: Array[String]): Unit = {
    println("Hello world!")

    var fila = 0
    var columna = 0
    val metodos = new Metodos()

    def obtenerDimensiones(): (Int, Int) = {
      try {
        // Pedir al usuario que introduzca el valor de la fila
        print("Introduce el valor de la fila: ")
        fila = StdIn.readInt()

        // Pedir al usuario que introduzca el valor de la columna
        print("Introduce el valor de la columna: ")
        columna = StdIn.readInt()
      } catch {
        case e: NumberFormatException => {
          println("Error: Debes introducir un número válido.")
          // Llamar a la función de nuevo para permitir al usuario volver a intentarlo
          return obtenerDimensiones()
        }
      }
      val dimension = fila * columna
      println(metodos.llenarLista(dimension))
      // Devolver las dimensiones como una tupla
      (fila, columna)
    }

    obtenerDimensiones()
    println(s"Fila introducida: $fila, Columna introducida: $columna")
  }
}