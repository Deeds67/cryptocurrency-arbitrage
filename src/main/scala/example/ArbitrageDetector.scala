package example

import example.ArbitrageDetector.{Currency, CurrencyPricePair, Edge}

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
        case Some(f) => Some(f :+ Edge(pair.from, pair.to, pair.rate))
        case None => Some(List(Edge(pair.from, pair.to, pair.rate)))
      }
    }
    val edges = graph.values.flatten

    graph.keys.map { source =>
      val distance = graph.keys.map { k => (k, if (k == source) 0 else Double.PositiveInfinity) }.toMap
      val predecessor: Map[Currency, Option[Currency]] = graph.keys.map { k => (k, None) }.toMap


      def relaxEdges(iteration: Int, distance: Map[Currency, Double], predecessor: Map[Currency, Option[Currency]]): (Map[Currency, Double], Map[Currency, Option[Currency]]) = {
        // repeat |V| - 1 times, where |V| is the number of vertices
        if (iteration < graph.size) {
          val iterationResult = edges.foldLeft((distance, predecessor)) { (acc, edge) =>
            val currentDistance = acc._1
            val currentPredecessor = acc._2

            (for {
              fromDistance <- currentDistance.get(edge.from)
              toDistance <- currentDistance.get(edge.to)
              shouldRelax = (fromDistance + edge.weight) < toDistance

              result <- if (shouldRelax) {
                Some(currentDistance.updated(edge.to, fromDistance + edge.weight),
                  currentPredecessor.updated(edge.to, Some(edge.from)))
              } else None
            } yield result).getOrElse(currentDistance, currentPredecessor)
          }

          val updatedDistance = iterationResult._1
          val updatedPredecessor = iterationResult._2
          relaxEdges(iteration + 1, updatedDistance, updatedPredecessor)
        } else {
          (distance, predecessor)
        }
      }


      0
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