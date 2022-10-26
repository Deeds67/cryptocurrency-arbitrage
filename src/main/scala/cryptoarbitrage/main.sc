#!/usr/bin/env -S scala -cli shebang

/* Borger, feel free to let your imagination shine but do not change this snippet :> */

import solution.ArbitrageDetector

@main def run(args: String*): Unit = {
  val url: String = args.length match {
    case 0 => "https://api.swissborg.io/v1/challenge/rates"
    case _ => args(0)
  }
  /* Add your stuff, be Awesome! */

  val at = new solution.ArbitrageDetectorImpl()
  val currencyPricePairs = List(
    solution.ArbitrageDetector.CurrencyPricePair("BTC", "BTC", 1.0),
    solution.ArbitrageDetector.CurrencyPricePair("BTC", "CHSB", 116352.2654440156),
    solution.ArbitrageDetector.CurrencyPricePair("BTC", "DAI", 23524.1391553039),
    solution.ArbitrageDetector.CurrencyPricePair("BTC", "EUR", 23258.8865583847),
    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "BTC", 0.0000086866),
    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "CHSB", 1.0),
    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "DAI", 0.2053990550),
    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "EUR", 0.2017539914),
    solution.ArbitrageDetector.CurrencyPricePair("DAI", "BTC", 0.0000429088),
    solution.ArbitrageDetector.CurrencyPricePair("DAI", "CHSB", 4.9320433378),
    solution.ArbitrageDetector.CurrencyPricePair("DAI", "DAI", 1.0),
    solution.ArbitrageDetector.CurrencyPricePair("DAI", "EUR", 0.9907652193),
    solution.ArbitrageDetector.CurrencyPricePair("EUR", "BTC", 0.0000435564),
    solution.ArbitrageDetector.CurrencyPricePair("EUR", "CHSB", 5.0427577751),
    solution.ArbitrageDetector.CurrencyPricePair("EUR", "DAI", 1.0211378960),
    solution.ArbitrageDetector.CurrencyPricePair("EUR", "EUR", 1.0),
  )
  val graph = solution.ArbitrageDetector.Graph(currencyPricePairs)
  val result = at.detectArbitrage(graph)

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
      println(s"Arbitrage found: $initialCurrency")
      println(s"Path: ${r.mkString(" -> ")}")
      println(s"When following this path, 1 $initialCurrency would be turned into $resultingAmount $initialCurrency \n\n")
    }
  }
}