object Main {
  import auxiliar.metodos
  import scala.io.StdIn

  def obtenerDimensiones(): (Int, Int) = {
    try {
      // Pedir al usuario que introduzca el valor de la fila
      print("Introduce el valor de la fila: ")
      val fila = StdIn.readInt()

      // Pedir al usuario que introduzca el valor de la columna
      print("Introduce el valor de la columna: ")
      val columna = StdIn.readInt()

      // Return dimensiones
      (fila, columna)
    } catch {
      case e: NumberFormatException => {
        println("Error: Debes introducir un número válido.")
        // Llamar a la función de nuevo para permitir al usuario volver a intentarlo
        obtenerDimensiones()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Hello world!")

    // Obtener las dimensiones del usuario
    val (fila, columna) = obtenerDimensiones()
    println(s"Fila introducida: $fila, Columna introducida: $columna")
  }
}
