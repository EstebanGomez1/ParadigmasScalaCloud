object Main{
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    import aux.metodos
    val x: metodos = new metodos(1,3)
    println(s"Resultado de 1 + 3: ${x.suma()}")
    println(s"Resultado de 1 - 3: ${x.resta()}")
  }
}