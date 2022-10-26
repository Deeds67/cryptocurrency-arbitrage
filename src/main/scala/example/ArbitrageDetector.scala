package example

import example.ArbitrageDetector.{BellmanFordOutput, Currency, CurrencyPricePair, Edge, Graph}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait ArbitrageDetector {
  def detectArbitrage(graph: Graph): List[List[Currency]]
}

object ArbitrageDetector {
  type Currency = String

  final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)

  final case class Edge(from: Currency, to: Currency, weight: Double)

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
  override def detectArbitrage(g: Graph): List[List[Currency]] = {
    val graph = g.graph
    val allEdges = graph.values.flatten

    println(s"graph $graph")
    println(s"edges $allEdges")

    graph.keys.foldLeft[List[List[Currency]]](List()) { (acc, currency) =>
      println(currency)
      val bellmanFordOutput = bellmanFordAlgorithm(currency, g)
      println(bellmanFordOutput)
      acc
    }
  }

  def bellmanFordAlgorithm(sourceCurrency: Currency, g: Graph): BellmanFordOutput = {
    val graph = g.graph
    val edges = graph.values.flatten
    val initialOutput = BellmanFordOutput(
      distance = graph.keys.map { c => (c, if (c == sourceCurrency) 0.0 else Double.PositiveInfinity) }.toMap,
      predecessor = graph.keys.map { k => (k, None) }.toMap
    )

    (1 until graph.size).foldLeft(initialOutput) { (acc, _) =>
      edges.foldLeft(acc) { (edgeAcc, edge) =>
        val distance = edgeAcc.distance
        val shouldRelax = (distance(edge.from) + edge.weight) < distance(edge.to)
        if (shouldRelax) {
          edgeAcc.relaxEdge(edge)
        } else {
          edgeAcc
        }
      }
    }
  }
}