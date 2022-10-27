#!/usr/bin/env -S scala -cli shebang

import $ivy.`com.softwaremill.sttp.client3::core:3.8.3`
import $ivy.`com.softwaremill.sttp.client3::upickle::3.8.3`
import sttp.client3.quick._
import sttp.client3.upicklejson._
import upickle.default._
import sttp.client3.{SimpleHttpClient, basicRequest}


/* Borger, feel free to let your imagination shine but do not change this snippet :> */

import solution.ArbitrageDetector


@main def run(args: String*): Unit = {
  val url: String = args.length match {
    case 0 => "https://api.swissborg.io/v1/challenge/rates"
    case _ => args(0)
  }


  /* Add your stuff, be Awesome! */
  val client = SimpleHttpClient()
  val encodedUrl = uri"${url}"
  val request = basicRequest.get(encodedUrl).response(asJson[Map[String,String]])
  val responseBody: Map[String, String] = client.send(request).body.right.get

//  val responseBody = read[Map[String,String]]("""{
//    |    "BTC-BTC": "1.0000000000",
//    |        "BTC-CHS": "0.00000973",
//    |        "BTC-DAI": "0.0000475",
//    |        "BTC-EUR": "0.00004784",
//    |        "CHS-BTC": "102127.62265611",
//    |        "CHS-CHS": "1.0000000000",
//    |        "CHS-DAI": "5.04446637",
//    |        "CHS-EUR": "4.9593934",
//    |        "DAI-BTC": "20420.5893644",
//    |        "DAI-CHS": "0.19695241",
//    |        "DAI-DAI": "1.0000000000",
//    |        "DAI-EUR": "1.01544794",
//    |        "EUR-BTC": "20770.88227025",
//    |        "EUR-CHS": "0.19562445",
//    |        "EUR-DAI": "0.97840543",
//    |        "EUR-EUR": "1.0000000000"
//    |}""".stripMargin)

//  println(responseBody)

  val currencyPricePairs = responseBody.map { (k: String, v: String) =>
    val split = k.split("-")
    val from = split(0)
    val to = split(1)
    val rate = v.toDouble
    solution.ArbitrageDetector.CurrencyPricePair(from, to, rate)
  }.toList


//  println(currencyPricePairs)
  val at = new solution.ArbitrageDetectorImpl()
//  val currencyPricePairs = List(
//    solution.ArbitrageDetector.CurrencyPricePair("BTC", "BTC", 1.0),
//    solution.ArbitrageDetector.CurrencyPricePair("BTC", "CHSB", 116352.2654440156),
//    solution.ArbitrageDetector.CurrencyPricePair("BTC", "DAI", 23524.1391553039),
//    solution.ArbitrageDetector.CurrencyPricePair("BTC", "EUR", 23258.8865583847),
//    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "BTC", 0.0000086866),
//    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "CHSB", 1.0),
//    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "DAI", 0.2053990550),
//    solution.ArbitrageDetector.CurrencyPricePair("CHSB", "EUR", 0.2017539914),
//    solution.ArbitrageDetector.CurrencyPricePair("DAI", "BTC", 0.0000429088),
//    solution.ArbitrageDetector.CurrencyPricePair("DAI", "CHSB", 4.9320433378),
//    solution.ArbitrageDetector.CurrencyPricePair("DAI", "DAI", 1.0),
//    solution.ArbitrageDetector.CurrencyPricePair("DAI", "EUR", 0.9907652193),
//    solution.ArbitrageDetector.CurrencyPricePair("EUR", "BTC", 0.0000435564),
//    solution.ArbitrageDetector.CurrencyPricePair("EUR", "CHSB", 5.0427577751),
//    solution.ArbitrageDetector.CurrencyPricePair("EUR", "DAI", 1.0211378960),
//    solution.ArbitrageDetector.CurrencyPricePair("EUR", "EUR", 1.0),
//  )
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