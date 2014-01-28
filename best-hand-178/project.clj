(defproject best-hand "0.1.0-SNAPSHOT"
  :description "4clojure.com, problem 178 - Best Hand
 
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
    High card: None of the above conditions are met"
  :url "http://www.4clojure.com/problem/178"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot best-hand.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
