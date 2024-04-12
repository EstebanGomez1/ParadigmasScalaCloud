object Main{
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    import ops.Operaciones
    val x: Operaciones = new Operaciones(1,3)
    println(s"Resultado de 1 + 3: ${x.suma()}")
    println(s"Resultado de 1 - 3: ${x.resta()}")
  }
}