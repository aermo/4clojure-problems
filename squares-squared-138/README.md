# Squares Squared
 
Difficulty:	Hard
Topics:	data-analysis

Create a function of two integer arguments: the start and end, respectively. You must create a vector of strings which renders a 45° rotated square of integers which are successive squares from the start point up to and including the end point. If a number comprises multiple digits, wrap them around the shape individually. If there are not enough digits to complete the shape, fill in the rest with asterisk characters. The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, with the initial direction being down and to the right.

## Test examples

(= (squares-squared 2 2) ["2"])


(= (squares-squared 2 4) [" 2 "
                          "* 4"
                          " * "])


(= (squares-squared 3 81) [" 3 "
                           "1 9"
                           " 8 "])


(= (squares-squared 4 20) [" 4 "
                           "* 1"
                           " 6 "])

(= (squares-squared 2 256) ["  6  "
                            " 5 * "
                            "2 2 *"
                            " 6 4 "
                            "  1  "])


(= (squares-squared 10 10000) ["   0   "
                               "  1 0  "
                               " 0 1 0 "
                               "* 0 0 0"
                               " * 1 * "
                               "  * *  "
                               "   *   "])

## Installation

Download sources from http://github.com/aermo/4clojure-problems

## Usage

    $ java -jar squares-squared-0.1.0-standalone.jar k n

where k and n are integers: the start and the end.

## License

Copyright © 2014 Antti Rämö

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
