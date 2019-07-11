package de.dheinrich

sealed trait TileColor {
	def one: String

	def apply(n : Int): NumTile
}

object Bamboo extends TileColor {
	override val one = "🀐"
	override def apply(n: Int) = NumTile(Bamboo, n)
}

object Money extends TileColor {
	override val one = "🀇"
	override def apply(n: Int) = NumTile(Money, n)
}

object Circle extends TileColor {
	override val one = "🀙"
	override def apply(n: Int) = NumTile(Circle, n)
}

sealed trait Tile {
	def symbol: String

	override def toString: String = symbol
}

case class NumTile(color: TileColor, n: Int) extends Tile {
	override def symbol = {
		val cp = color.one.codePointAt(0) + (n - 1)
		"" + Character.highSurrogate(cp) + Character.lowSurrogate(cp)
	}

	def prev = color(n - 1)
	def next = color(n + 1)
}

object Tile {
	val colors = Seq(Bamboo, Money, Circle)
	val numTiles = for {
		color <- colors
		n <- 0 to 8
	} yield color(n)

	val special = Seq(North, South, West, East, RedMiddle, Fortune, Door)

	val fullDeck = List.fill(4)(special ++ numTiles).flatten

	implicit val tileOrdering = new Ordering[Tile] {
		override def compare(x: Tile, y: Tile): Int = (x, y) match {
			case (NumTile(ac, an), NumTile(bc, bn)) if ac == bc => an - bn
			case (NumTile(ac, _), NumTile(bc, _)) => colors.indexOf(ac) - colors.indexOf(bc)
			case (_: NumTile, _) => 1
			case (_, _: NumTile) => -1
			case (a, b) => special.indexOf(a) - special.indexOf(b)
		}
	}
}

object North extends Tile {
	override val symbol = "🀃"
}

object South extends Tile {
	override val symbol = "🀁"
}

object West extends Tile {
	override val symbol = "🀂"
}

object East extends Tile {
	override val symbol = "🀀"
}

object RedMiddle extends Tile {
	override val symbol = "🀄"
}

object Fortune extends Tile {
	override val symbol = "🀅"
}

object Door extends Tile {
	override val symbol = "🀆"
}





