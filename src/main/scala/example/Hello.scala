package example

import example.ArbitrageDetector
import example.ArbitrageDetector.{CurrencyPricePair, Graph}

//{
//"BTC-BTC": "1.0000000000",
//"BTC-CHSB": "116352.2654440156",
//"BTC-DAI": "23524.1391553039",
//"BTC-EUR": "23258.8865583847",
//"CHSB-BTC": "0.0000086866",
//"CHSB-CHSB": "1.0000000000",
//"CHSB-DAI": "0.2053990550",
//"CHSB-EUR": "0.2017539914",
//"DAI-BTC": "0.0000429088",
//"DAI-CHSB": "4.9320433378",
//"DAI-DAI": "1.0000000000",
//"DAI-EUR": "0.9907652193",
//"EUR-BTC": "0.0000435564",
//"EUR-CHSB": "5.0427577751",
//"EUR-DAI": "1.0211378960",
//"EUR-EUR": "1.0000000000"
//}

object Hello extends App {
  val at = new ArbitrageDetectorImpl()
  val result = at.detectArbitrage(Graph(
    List(
      CurrencyPricePair("BTC", "BTC", 1.0),
      CurrencyPricePair("BTC", "CHSB", 116352.2654440156),
      CurrencyPricePair("BTC", "DAI", 23524.1391553039),
      CurrencyPricePair("BTC", "EUR", 23258.8865583847),
      CurrencyPricePair("CHSB", "BTC", 0.0000086866),
      CurrencyPricePair("CHSB", "CHSB", 1.0),
      CurrencyPricePair("CHSB", "DAI", 0.2053990550),
      CurrencyPricePair("CHSB", "EUR", 0.2017539914),
      CurrencyPricePair("DAI", "BTC", 0.0000429088),
      CurrencyPricePair("DAI", "CHSB", 4.9320433378),
      CurrencyPricePair("DAI", "DAI", 1.0),
      CurrencyPricePair("DAI", "EUR", 0.9907652193),
      CurrencyPricePair("EUR", "BTC", 0.0000435564),
      CurrencyPricePair("EUR", "CHSB", 5.0427577751),
      CurrencyPricePair("EUR", "DAI", 1.0211378960),
      CurrencyPricePair("EUR", "EUR", 1.0),
    )))
  println(result)
}

