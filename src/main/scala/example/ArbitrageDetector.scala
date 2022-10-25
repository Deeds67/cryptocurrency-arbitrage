package example

import example.ArbitrageDetector.{Currency, CurrencyPricePair, Edge}

import scala.annotation.tailrec

trait ArbitrageDetector {
  def detectArbitrage(currencyPricePairs: List[CurrencyPricePair]): List[List[Currency]]
}

object ArbitrageDetector {
  type Currency = String

  final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)

  final case class Edge(from: Currency, to: Currency, weight: Double)
}

class ArbitrageDetectorImpl() extends ArbitrageDetector {
  override def detectArbitrage(currencyPricePairs: List[CurrencyPricePair]): List[List[Currency]] = {
    val graph = currencyPricePairs.foldLeft(Map[Currency, List[Edge]]()) { (acc, pair) =>
      acc.updatedWith(pair.from) {
        case Some(f) => Some(f :+ Edge(pair.from, pair.to, -scala.math.log(pair.rate))) // todo: log?
        case None => Some(List(Edge(pair.from, pair.to, -scala.math.log(pair.rate))))
      }
    }
    val edges = graph.values.flatten

    println(s"graph $graph")
    println(s"edges $edges")

    // Using each currency as a potential source
    graph.keys.flatMap { source =>
      println(s"source: $source")
      val distance = graph.keys.map { k => (k, if (k == source) 0 else Double.PositiveInfinity) }.toMap
      val predecessor: Map[Currency, Option[Currency]] = graph.keys.map { k => (k, None) }.toMap

      @tailrec
      def relaxEdges(iteration: Int, distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]): (Map[Currency, Double], Map[Currency, Option[Currency]]) = {
        // repeat |V| - 1 times, where |V| is the number of vertices
        if (iteration < graph.size) {
          val relaxResult = edges.foldLeft((distance, predecessor)) { (acc, edge) =>
            val distance = acc._1
            val predecessor = acc._2

            (for {
              fromDistance <- distance.get(edge.from)
              toDistance <- distance.get(edge.to)
              shouldRelax = (fromDistance + edge.weight) < toDistance

              result <- if (shouldRelax) {
                Some(distance.updated(edge.to, fromDistance + edge.weight),
                  predecessor.updated(edge.to, Some(edge.from)))
              } else None
            } yield result).getOrElse(distance, predecessor)
          }

          val updatedDistance = relaxResult._1
          val updatedPredecessor = relaxResult._2
          relaxEdges(iteration + 1, updatedDistance, updatedPredecessor)
        } else {
          (distance, predecessor)
        }
      }

      val relaxEdgesResult = relaxEdges(1, distance, predecessor)

      val relaxedDistance = relaxEdgesResult._1
      val relaxedPredecessor = relaxEdgesResult._2
//      println(s"relaxedDistance  $relaxedDistance")
//      println(s"relaxedPredecessor  $relaxedPredecessor")

      final case class CycleResult(cycle: List[List[Currency]], seen: Set[Currency])

      def findNonNegativeWeightCycles(distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]): List[List[Currency]] = {
        edges.foldLeft[(List[List[Currency]], Set[String])]((List(), Set())) { (acc, edge) =>
          val cycles = acc._1
          val seen = acc._2
          if (seen.contains(edge.to)) {
            println(s"${edge.to} already seen. acc so far ${acc}")
            acc
          } else {
            println(s"${edge.to} not yet seen. acc so far ${acc}")
            @tailrec
            def findCycle(current: Currency, to: Currency, seen: Set[Currency], cycles: List[Currency] = List()): (List[Currency], Set[Currency]) = {
              println(s"finding cycle for $current")
              if (current == to || cycles.contains(current)) {
                println(s"found cycle current ${current} to $to cycles: $cycles")
                val pivot = cycles.indexOf(current)
                val inOrderCycles = (cycles :+ current).reverse
                (inOrderCycles, seen)
              } else {
                val next = predecessor.get(current).flatten.getOrElse(throw new Exception(s"Predecessor should exist for $current"))
                findCycle(next, to, seen + current, cycles :+ current)
              }
            }

            (for {
              fromDistance <- distance.get(edge.from)
              toDistance <- distance.get(edge.to)
              canRelax = (fromDistance + edge.weight) < toDistance
              cyclesResult = if (canRelax) {
                val (updatedCycles, updatedSeen) = findCycle(edge.from, edge.to, seen)
                (updatedCycles +: cycles, updatedSeen)
              } else {
                acc
              }
            } yield (cyclesResult._1, cyclesResult._2)).getOrElse(acc)
          }
        }
      }._1

      findNonNegativeWeightCycles(relaxedDistance, relaxedPredecessor)
    }
  }.toList
}