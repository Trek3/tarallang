object Tarallang {

  def main(args : Array[String]) = {

    val p = new TarallangParser()

    args.foreach {
      filename =>
        val src = scala.io.Source.fromFile(filename)

        val lines = src.mkString

        p.parseAll(p.program, lines) match {
          case p.Success(r, _) => {
            val interpreter = new Interpreter(r)

            try{
              interpreter.run
            }
            catch {
              case e : Exception => println(e.getMessage)
            }
          }
          case x => println(x.toString)
        }
    }
  }
}
