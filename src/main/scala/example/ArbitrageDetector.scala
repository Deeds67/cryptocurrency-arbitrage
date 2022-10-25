package example

import example.ArbitrageDetector.{Currency, CurrencyPricePair, Edge}

import scala.annotation.tailrec

trait ArbitrageDetector {
  def detectArbitrage(currencyPricePairs: List[CurrencyPricePair])
}

object ArbitrageDetector {
  type Currency = String

  final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)

  final case class Edge(from: Currency, to: Currency, weight: Double)
}

class ArbitrageDetectorImpl() extends ArbitrageDetector {
  override def detectArbitrage(currencyPricePairs: List[CurrencyPricePair]): Unit = {
    val graph = currencyPricePairs.foldLeft(Map[Currency, List[Edge]]()) { (acc, pair) =>
      acc.updatedWith(pair.from) {
        case Some(f) => Some(f :+ Edge(pair.from, pair.to, pair.rate)) // todo: log?
        case None => Some(List(Edge(pair.from, pair.to, pair.rate)))
      }
    }
    val edges = graph.values.flatten

    // Using each currency as a potential source
    graph.keys.map { source =>
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

      final case class CycleResult(cycle: List[List[Currency]], seen: Set[Currency])

      def findNonNegativeWeightCycles(distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]): List[List[Currency]] = {
        edges.foldLeft[(List[List[Currency]], Set[String])]((List(), Set())) { (acc, edge) =>
          if (acc._2.contains(edge.to)) {
            acc
          } else {

            @tailrec
            def findCycle(current: Currency, to: Currency, seen: Set[Currency], cycles: List[Currency] = List()): (List[Currency], Set[Currency]) = {
              if (current == to || cycles.contains(current)) {
                val pivot = cycles.indexOf(current)
                val inOrderCycles = cycles.drop(pivot).reverse
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
                findCycle(edge.from, edge.to, acc._2)
              } else {
                (List(), acc._2)
              }
              updatedCycles = cyclesResult._1 +: acc._1
              updatedSeen = cyclesResult._2
            } yield (updatedCycles, updatedSeen)).getOrElse(acc)
          }
        }
      }._1

      findNonNegativeWeightCycles(relaxedDistance, relaxedPredecessor)
    }


  }
}


//function BellmanFord(list vertices, list edges, vertex source) is
//    distance := list of size n
//    predecessor := list of size n
//
//    // Step 1: initialize graph
//    for each vertex v in vertices do
//
//        distance[v] := inf             // Initialize the distance to all vertices to infinity
//        predecessor[v] := null         // And having a null predecessor
//
//    distance[source] := 0              // The distance from the source to itself is, of course, zero
//
//    // Step 2: relax edges repeatedly
//
//    repeat |V|−1 times:
//         for each edge (u, v) with weight w in edges do
//             if distance[u] + w < distance[v] then
//                 distance[v] := distance[u] + w
//                 predecessor[v] := u
//
//    // Step 3: check for negative-weight cycles
//    for each edge (u, v) with weight w in edges do
//        if distance[u] + w < distance[v] then
//            // Step 4: find a negative-weight cycle
//            negativeloop := [v, u]
//            repeat |V|−1 times:
//                u := negativeloop[0]
//                for each edge (u, v) with weight w in edges do
//                    if distance[u] + w < distance[v] then
//                        negativeloop := concatenate([v], negativeloop)
//            find a cycle in negativeloop, let it be ncycle
//            // use any cycle detection algorithm here
//            error "Graph contains a negative-weight cycle", ncycle
//
//    return distance, predecessor