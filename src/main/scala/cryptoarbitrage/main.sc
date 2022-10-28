#!/usr/bin/env -S scala -cli shebang

import $ivy.`com.softwaremill.sttp.client3::core:3.8.3`
import $ivy.`com.softwaremill.sttp.client3::upickle::3.8.3`
import sttp.client3.quick._
import sttp.client3.upicklejson._
import sttp.client3.{SimpleHttpClient, basicRequest}

/* Borger, feel free to let your imagination shine but do not change this snippet :> */


@main def main(args: String*): Unit = {
  val url: String = args.length match {
    case 0 => "https://api.swissborg.io/v1/challenge/rates"
    case _ => args(0)
  }

  /* Add your stuff, be Awesome! */

  /*
    SwissBorg tech challenge solution by Pierre Marais:

    Algorithm complexity analysis:

      The Bellman-Ford algorithm was used to solve this problem. It is used to find a shortest path in a directed graph.
      Since the Bellman-Ford algorithm works using addition, but exchange rates are multiplicative, we can use this mathematical formula
      to make our exchanges additive:

      `log (x * z * y) = log(x) + log(y) + log(z)`
      Therefore, if `log(x) + log(y) + log(z) > 0`, arbitrage is possible.

      Since Bellman-Ford allows us to find *negative* weight cycles, we can simply negate the weights of the edges.
      Therefore, if `-log(x) + (-log(y)) + (-log(z)) < 0`, arbitrage is possible.

      The time complexity is O(V * N), where V is the number of vertexes (currencies) and E is the number of edges (currency price pairs).

      The standard Bellman-Ford algorithm does not return negative weight cycles (it simply identifies that they are present),
      so we have an additional linear effort to iterate through the predecessors to find the negative weight cycle.
      This does not affect the overall time complexity, as it is linear.

      Side note: Double was used, which could lead to minor inconsistencies due to floating point rounding errors. This can be avoided
      by using BigDecimal or representing the dollars/cents as integers.

    A note about CHSB and its key features:

       - It's an ERC-20 token based on the Ethereum platform.
       - It has a circulating supply of 1 billion tokens.
       - By "Proof of meritocracy", all token holders can vote on the direction which the network will be developed.
       - There exists a CHSB Buyback program, which periodically burns some of the circulating supply.
   */

  object Solution {
    type Currency = String

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

    final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)

    final case class BellmanFordOutput(distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]) {
      def relaxEdge(edge: Edge): BellmanFordOutput = {
        this.copy(
          distance = distance.updated(edge.to, distance(edge.from) + edge.weight),
          predecessor = predecessor.updated(edge.to, Some(edge.from))
        )
      }
    }

    trait ArbitrageDetector {
      def detectArbitrage(graph: Graph): Set[List[Currency]]
      def printArbitrageResult(result: Set[List[Currency]], graph: Graph): Unit
    }

    class ArbitrageDetectorImpl() extends ArbitrageDetector {
      override def detectArbitrage(g: Graph): Set[List[Currency]] = {
        val graph = g.graph

        graph.keys.foldLeft[Set[List[Currency]]](Set()) { (acc, currency) =>
          acc ++ bellmanFordAlgorithm(currency, g)
        }
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

        findNegativeWeightCycles(g, outputAfterRelaxing, sourceCurrency)
      }

      def findNegativeWeightCycles(g: Graph, output: BellmanFordOutput, source: Currency): Set[List[Currency]] = {
        val edges = g.graph.values.flatten
        val seen = scala.collection.mutable.Set[Currency]()
        edges.flatMap { e =>
          // If it's possible to relax further, it means that there must be a negative weight cycle,
          // because it takes at maximum |V| - 1 relaxations to find the optimal value.
          var next = source

          if (!seen.contains(next) && e.canRelax(output.distance)) {
            var cycles = scala.collection.mutable.ArrayBuffer[Currency](source)
            var found = false
            while (!found) {
              next = output.predecessor(next).getOrElse(throw new Exception("Expected node to have a predecessor"))
              seen.add(next)
              if (!cycles.contains(next)) {
                cycles.append(next)
              } else {
                cycles.append(next)
                cycles = cycles.slice(cycles.indexOf(next), cycles.size).reverse
                found = true
              }
            }
            Some(cycles.toList)
          } else None
        }.toSet
      }

      override def printArbitrageResult(result: Set[List[Currency]], graph: Solution.Graph): Unit = {
        if (result.isEmpty) {
          println("No arbitrage could be found.")
        } else {
          result.foreach { r =>
            val initialCurrency = r.head
            val resultingAmount = r.slice(1, r.size).foldLeft((1.0, initialCurrency)) { (acc, curr) =>
              val sum = acc._1
              val prev = acc._2
              val loggedRate = graph.graph(prev).find(_.to == curr).getOrElse(throw new Exception(s"Could not find exchange between $prev and $curr"))
              val rate = scala.math.exp(-loggedRate.weight)
              (sum * rate, curr)
            }._1
            println(s"Arbitrage found with starting currency: $initialCurrency")
            println(s"Path: ${r.mkString(" -> ")}")
            println(s"When following this path, 1 $initialCurrency would be turned into $resultingAmount $initialCurrency \n\n")
          }
        }
      }
    }
  }

  val client = SimpleHttpClient()
  val encodedUrl = uri"$url"
  val request = basicRequest.get(encodedUrl).response(asJson[Map[String,String]])
  val responseBody: Map[String, String] = client.send(request).body.right.get

  val currencyPricePairs = responseBody.map { (k: String, v: String) =>
    val split = k.split("-")
    val from = split(0)
    val to = split(1)
    val rate = v.toDouble
    Solution.CurrencyPricePair(from, to, rate)
  }.toList

  val arbitrageDetector = new Solution.ArbitrageDetectorImpl()
  val graph = Solution.Graph(currencyPricePairs)

  val arbitrageResult = arbitrageDetector.detectArbitrage(graph)
  arbitrageDetector.printArbitrageResult(arbitrageResult, graph)

}
