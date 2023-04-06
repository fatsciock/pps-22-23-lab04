package u04lab.polyglot.a01b
import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List
import u04lab.code.List.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private var remainingFreeTiles = (size * size) - mines
  private var minesPosition = List.empty[Tuple]

  while(length(minesPosition) < mines)
    var newPos = (Random.nextInt(size - 1), Random.nextInt(size - 1))
    while contains(minesPosition, newPos) do
      newPos = (Random.nextInt(size - 1), Random.nextInt(size - 1))
    minesPosition = cons(newPos, minesPosition)

  private def neighbours(x: Int, y: Int): Int =
    var mines = 0
    for i <- x - 1 to x + 1 do
      for j <- y - 1 to y + 1 do
        if contains(minesPosition, (i, j)) then mines = mines + 1
    mines

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if contains(minesPosition, (x, y)) then
      return OptionToOptional(None()) // Option => Optional converter
    remainingFreeTiles = remainingFreeTiles - 1
    OptionToOptional(Some(neighbours(x, y)))

  def won: Boolean = remainingFreeTiles == 0
