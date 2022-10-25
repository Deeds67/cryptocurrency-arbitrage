package example

import example.ArbitrageDetector.{Currency, CurrencyPricePair, Edge}

trait ArbitrageDetector {
  def detectArbitrage(currencyPricePairs: List[CurrencyPricePair])
}
object ArbitrageDetector {
  type Currency = String
  final case class CurrencyPricePair(from: Currency, to: Currency, rate: Double)
  final case class Edge(from: Currency, to: Currency, value: Double)
}

class ArbitrageDetectorImpl() extends ArbitrageDetector {
  override def detectArbitrage(currencyPricePairs: List[CurrencyPricePair]): Unit = {
    val graph = currencyPricePairs.foldLeft(Map[Currency, List[Edge]]()) { (acc, pair) =>
      acc.updatedWith(pair.from) {
        case Some(f) => Some(f :+ Edge(pair.from, pair.to, pair.rate))
        case None => Some(List(Edge(pair.from, pair.to, pair.rate)))
      }
    }
    println(graph)
  }
}
