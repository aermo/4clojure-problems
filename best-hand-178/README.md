# Best Hand

This application is a solution for the 4clojure.com problem Best Hand.

## Problem definition

Difficulty:	Hard
Topics:	strings game

Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards. The hand rankings are listed below for your convenience.

    Straight flush: All cards in the same suit, and in sequence
    Four of a kind: Four of the cards have the same rank
    Full House: Three cards of one rank, the other two of another rank
    Flush: All cards in the same suit
    Straight: All cards in sequence (aces can be high or low, but not both at once)
    Three of a kind: Three of the cards have the same rank
    Two pair: Two pairs of cards have the same rank
    Pair: Two cards have the same rank
    High card: None of the above conditions are met


## Usage

    $ java -jar best-hand-0.1.0-standalone.jar c1 c2 c3 c4 c5

where `ci` are distinct cards with suit and rank
 - suit: `H`, `C`, `D` or `S`, hearts, clubs, diamonds or spades respectively.
 - rank: `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `T`, `J`, `Q`, `K` or `A`

## Examples

    $ java -jar best-hand-0.1.0-standalone.jar HA D2 H3 C9 DJ
     Your best hand is high-card


    $ java -jar best-hand-0.1.0-standalone.jar HA HQ SJ DA HT
     Your best hand is pair


    $ java -jar best-hand-0.1.0-standalone.jar HA DA HQ SQ HT
     Your best hand is two-pair


    $ java -jar best-hand-0.1.0-standalone.jar HA DA CA HJ HT
     Your best hand is three-of-a-kind


    $ java -jar best-hand-0.1.0-standalone.jar HA DK HQ HJ HT
     Your best hand is straight


    $ java -jar best-hand-0.1.0-standalone.jar HA H2 S3 D4 C5
     Your best hand is straight


    $ java -jar best-hand-0.1.0-standalone.jar HA DA CA HJ DJ
     Your best hand is full-house


    $ java -jar best-hand-0.1.0-standalone.jar HA DA CA SA DJ
     Your best hand is four-of-a-kind


    $ java -jar best-hand-0.1.0-standalone.jar HA HK HQ HJ HT
     Your best hand is straight-flush


## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
