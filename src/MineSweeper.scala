import Main.get_neighbors

class MineSweeper {
  var m: Array[Array[Case]] = Array[Array[Case]]()
  private var screen: Array[Array[String]] = Array[Array[String]]()
  var end: Boolean = false
  private var nb_mines = 0
  var nb_valid_case = 0

  def this(b: Array[Array[Case]], n: Int) {
    this()
    m = b
    screen = Array.ofDim[String](m.length, m(0).length)
    nb_valid_case = m.length * m(0).length - n
    println(nb_valid_case)
    nb_mines = n
  }

  def display(): Unit = {
    print("|   ")
    for (i <- 0 until Main.get_dimension(m)._1) {
      print("| " + i + " ")
    }
    println("|")
    println("---------------------------------------------")

    for (i <- 0 until Main.get_dimension(m)._1) {
      print("| " + i + " ")
      for (j <- 0 until Main.get_dimension(m)._2) {
        print("|")
        if (screen(i)(j) == null)
          print("███")
        else
          print(screen(i)(j))
      }
      print("| ")
      println()
    }
  }

  def interact(i: Int, j: Int): Unit = {
    if (screen(i)(j) == null) {
      m(i)(j).value match {
        case -1 =>
          screen(i)(j) = " \uD83D\uDCA3"
          end = true
        case 0 =>
          screen(i)(j) = "   "
          nb_valid_case -= 1
          get_neighbors(m, i, j).foreach { case (i, j) => interact(i, j) }
        case _ =>
          screen(i)(j) = " " + m(i)(j).value.toString + " "
          nb_valid_case -= 1
      }
    }
    if (nb_valid_case == 0) {
      end = true
    }
  }

  def reveal(): Unit = {
    for (i <- 0 until Main.get_dimension(m)._1) {
      for (j <- 0 until Main.get_dimension(m)._2) {
        screen(i)(j) = get_case(m(i)(j).value)
      }
    }
  }

  private def get_case(k: Int): String = {
    k match {
      case -1 => " \uD83D\uDCA3"
      case 0 => " 0 "
      case _ => " " + k.toString + " "
    }
  }
}
