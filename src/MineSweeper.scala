class MineSweeper {
  var m: Array[Array[Case]] = Array[Array[Case]]()
  private var screen: Array[Array[Case]] = Array[Array[Case]]()
  var end: Boolean = false
  private var nb_mines = 0
  private var nb_valid_case = 0

  def this(b: Array[Array[Case]], n: Int) {
    this()
    m = b
    screen = Array.ofDim[Case](m.length, m(0).length)
    nb_valid_case = m.length * m(0).length - n
    nb_mines = n
  }

  def display():Unit = {
    print("| ")
    for (i <- 0 until Main.get_dimension(screen)._1) {
      print("|" + i)
    }
    println("|")

    for (i <- 0 until Main.get_dimension(screen)._1) {
      print("|" + i)
      for (j <- 0 until Main.get_dimension(screen)._2) {
        print("|")
        if (screen(i)(j) == null)
          print("_")
        else
          print(screen(i)(j).value)
      }
      print("|")
      println()
    }
  }

  def interact(i: Int, j: Int): Unit = {
    if (screen(i)(j) == null) {
      screen(i)(j) = m(i)(j)
      if (m(i)(j).value == -1) {
        end = true
      } else if (m(i)(j).value == 0) {
        Main.get_neighbors(m, i, j).foreach { case (i, j) => interact(i, j) }
      }
    }
  }
}
