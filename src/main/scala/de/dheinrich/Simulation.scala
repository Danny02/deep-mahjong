package de.dheinrich

import matryoshka._
import matryoshka.implicits._
import scalaz.Functor
import scalaz.syntax.functor._

import scala.util.Random

object Simulation extends App {
	val hand = Random.shuffle(Tile.fullDeck).take(14)

	println(Tile.fullDeck.size)
	println(hand.sorted)
	println(bestHand(hand))

	println(hasWon(List(
		North, North, North,
		Bamboo(1), Bamboo(2), Bamboo(3),
		Bamboo(5), Bamboo(6), Bamboo(7),
		Money(5), Money(5),
		Circle(2), Circle(3), Circle(4),
	)))

	def combinations(tiles: Seq[Tile]) = {
		val pairs = tiles.groupBy(t => t).filter(t => t._2.size > 1).values.toList

		val streets = tiles.filter {
			case t@(_: NumTile) => tiles.contains(t.prev) && tiles.contains(t.next)
			case _ => false
		}.map {
			case t@(_: NumTile) => Seq(t.prev, t, t.next)
		}

		pairs ++ streets
	}

	def hasWon(hTiles: List[Tile]) = {
		object Pair {
			def unapply(arg: List[Tile]): Option[Tile] = arg match {
				case List(a, b) if a == b => Some(a)
				case _ => None
			}
		}

		val h = bestHand(hTiles)
		h.other.size == 0 && h.combinations.exists {
			case Pair(_) => true
			case _ => false
		}
	}

	case class Hand(combinations: Seq[Seq[Tile]], other: Seq[Tile])

	def bestHand(hTiles: List[Tile]): Hand = {
		trait Combi[A]

		sealed case class Step[A](c: Seq[Tile], o: A) extends Combi[A]

		sealed case class Rest[A](rest: Seq[Tile]) extends Combi[A]

		implicit val combFunctor = new Functor[Combi] {
			override def map[A, B](fa: Combi[A])(f: A => B): Combi[B] = fa match {
				case Step(c, a) => Step(c, f(a))
				case Rest(a) => Rest(a)
			}
		}

		type MultiCombi[A] = List[Combi[A]]
		implicit val multiCombFunctor = new Functor[MultiCombi] {
			override def map[A, B](fa: MultiCombi[A])(f: A => B): MultiCombi[B] = fa.map(_ map f)
		}

		val expand: Coalgebra[MultiCombi, List[Tile]] = tiles => {
			val cs = combinations(tiles)
			if (cs.isEmpty)
				List(Rest(tiles))
			else
				cs.map(c => Step(c, tiles.diff(c)))
		}

		val bestRest: Algebra[MultiCombi, Hand] = mc => {
			mc.map {
				case Rest(a) => Hand(Seq(), a)
				case Step(nc, Hand(cc, r)) => Hand(cc :+ nc, r)
			}.sortBy(_.other.size).head
		}

		hTiles.hylo[MultiCombi, Hand](bestRest, expand)
	}
}
