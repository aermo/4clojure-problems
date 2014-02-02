(defproject squares-squared "0.1.0-SNAPSHOT"
  :description "4clojure.com, Problem #138, Squares Squared
 
Difficulty:	Hard
Topics:	data-analysis

Create a function of two integer arguments: the start and end, respectively. You must create a vector of strings which renders a 45° rotated square of integers which are successive squares from the start point up to and including the end point. If a number comprises multiple digits, wrap them around the shape individually. If there are not enough digits to complete the shape, fill in the rest with asterisk characters. The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, with the initial direction being down and to the right."
  :url "http://www.4clojure.com/problem/138"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot squares-squared.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
