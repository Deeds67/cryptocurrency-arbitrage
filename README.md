# Cryptocurrency arbitrage challenge

In this puzzle, you’ll be working in a market where prices are set independently of supply and demand. Luckily, the currency exchange broker is a close friend of ours, so all trading costs are waived. Your mission is to write a program that efficiently finds the best arbitrage opportunities.
The output will look like the JSON below, where BTC-CHSB is the quantity of CHSB that you can purchase for 1 BTC:

```json
{
    "BTC-BTC": "1.0000000000",
    "BTC-CHSB": "116352.2654440156",
    "BTC-DAI": "23524.1391553039",
    "BTC-EUR": "23258.8865583847",
    "CHSB-BTC": "0.0000086866",
    "CHSB-CHSB": "1.0000000000",
    "CHSB-DAI": "0.2053990550",
    "CHSB-EUR": "0.2017539914",
    "DAI-BTC": "0.0000429088",
    "DAI-CHSB": "4.9320433378",
    "DAI-DAI": "1.0000000000",
    "DAI-EUR": "0.9907652193",
    "EUR-BTC": "0.0000435564",
    "EUR-CHSB": "5.0427577751",
    "EUR-DAI": "1.0211378960",
    "EUR-EUR": "1.0000000000"
}
```

Given these exchange rates and the promise of riches, write a program that discovers arbitrage opportunities.

## Solution

The Bellman-Ford algorithm was used to solve this problem. It is used to find a shortest path in a directed graph.
Since the Bellman-Ford algorithm works using addition, but exchange rates are multiplicative, we can use this mathematical formula
to make our exchanges additive:

```
log (x * z * y) = log(x) + log(y) + log(z)
````
Therefore, if `log(x) + log(y) + log(z) > 0`, arbitrage is possible.

Since Bellman-Ford allows us to find *negative* weight cycles, we can simply negate the weights of the edges.
Therefore, if `-log(x) + (-log(y)) + (-log(z)) < 0`, arbitrage is possible.

The time complexity is `O(V * N)`, where `V` is the number of vertexes (currencies) and `E` is the number of edges (currency price pairs).

The standard Bellman-Ford algorithm does not return negative weight cycles (it simply identifies that they are present),
so we have an additional linear effort to iterate through the predecessors to find the negative weight cycle.
This does not affect the overall time complexity, as it is linear.

Side note: Double was used, which could lead to minor inconsistencies due to floating point rounding errors. This can be avoided
by using BigDecimal or representing the dollars/cents as integers.
