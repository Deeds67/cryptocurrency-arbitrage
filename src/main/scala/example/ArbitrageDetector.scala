package example

import example.ArbitrageDetector.{BellmanFordOutput, Currency, CurrencyPricePair, Edge, Graph}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait ArbitrageDetector {
  def detectArbitrage(graph: Graph): Set[List[Currency]]
}

object ArbitrageDetector {
  type Currency = String

  final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)

  final case class Edge(from: Currency, to: Currency, weight: Double) {
    def canRelax(distance: Map[Currency, Double]): Boolean = (distance(from) + weight) < distance(to)
  }

  final case class Graph(graph: Map[Currency, List[Edge]]) {
    def addEdge(edge: Edge): Graph = {
      this.copy(graph = this.graph.updatedWith(edge.from) {
        case Some(f) => Some(f :+ edge)
        case None => Some(List(edge))
      })
    }
  }

  object Graph {
    def apply(currencyPricePairs: List[CurrencyPricePair]): Graph = {
      currencyPricePairs.foldLeft(Graph(Map[Currency, List[Edge]]())) { (acc, pair) =>
        if (pair.from == pair.to) {
          acc
        } else {
          val edge = Edge(pair.from, pair.to, -scala.math.log(pair.rate))
          acc.addEdge(edge)
        }
      }
    }
  }

  final case class BellmanFordOutput(distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]) {
    def relaxEdge(edge: Edge): BellmanFordOutput = {
      this.copy(
        distance = distance.updated(edge.to, distance(edge.from) + edge.weight),
        predecessor = predecessor.updated(edge.to, Some(edge.from))
      )
    }
  }
}

class ArbitrageDetectorImpl() extends ArbitrageDetector {
  override def detectArbitrage(g: Graph): Set[List[Currency]] = {
    val graph = g.graph
    val allEdges = graph.values.flatten

    println(s"graph $graph")
    println(s"edges $allEdges")

      graph.keys.foldLeft[Set[List[Currency]]](Set()) { (acc, currency) =>
        println(currency)
        val negativeWeightCycles = bellmanFordAlgorithm(currency, g)
        println(s"negativeWeightCycles: $negativeWeightCycles")
        negativeWeightCycles
      }
//    println("BTC")
//    val negativeWeightCycles = bellmanFordAlgorithm("BTC", g)
//    println(s"negativeWeightCycles: $negativeWeightCycles")
//    negativeWeightCycles
  }

  def bellmanFordAlgorithm(sourceCurrency: Currency, g: Graph): Set[List[Currency]] = {
    val graph = g.graph
    val edges = graph.values.flatten
    val initialOutput = BellmanFordOutput(
      distance = graph.keys.map { c => (c, if (c == sourceCurrency) 0.0 else Double.PositiveInfinity) }.toMap,
      predecessor = graph.keys.map { k => (k, None) }.toMap
    )

    val outputAfterRelaxing = (1 until graph.size).foldLeft(initialOutput) { (acc, _) =>
      edges.foldLeft(acc) { (edgeAcc, edge) =>
        val canRelax = edge.canRelax(edgeAcc.distance)
        if (canRelax) {
          edgeAcc.relaxEdge(edge)
        } else {
          edgeAcc
        }
      }
    }

    println(s"Relaxing output: $outputAfterRelaxing")
    findNegativeWeightCycles(g, outputAfterRelaxing, sourceCurrency)
  }


  def findNegativeWeightCycles(g: Graph, output: BellmanFordOutput, source: Currency): Set[List[Currency]] = {
    val edges = g.graph.values.flatten
    edges.flatMap { e =>
      // If it's possible to relax further, it means that there must be a negative weight cycle,
      // because it takes at maximum |V| - 1 relaxations to find the optimal value.
      if (e.canRelax(output.distance)) {
        var cycles = scala.collection.mutable.ArrayBuffer[Currency](source)
        var next = source
        var found = false
        while (!found) {
          next = output.predecessor(next).getOrElse(throw new Exception("Expected node to have a predecessor"))
          if (!cycles.contains(next)) {
            cycles.append(next)
          } else {
            cycles.append(next)
            cycles = cycles.slice(cycles.indexOf(next), cycles.size)
            found = true
          }
        }
        Some(cycles.toList)
      } else None
    }.toSet
  }

  //  def calculateArb(cycles: Set[List[Currency]]): Unit = {
  //    var total = 0
  //    for ()
  //  }
}