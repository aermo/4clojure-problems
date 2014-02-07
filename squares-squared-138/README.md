# Squares Squared

This application is a solution for the 4clojure.com problem Squares Squared.

## Problem definition:

Difficulty:	Hard
Topics:	data-analysis

Create a function of two integer arguments: the start and end, respectively. You must create a vector of strings which renders a 45° rotated square of integers which are successive squares from the start point up to and including the end point. If a number comprises multiple digits, wrap them around the shape individually. If there are not enough digits to complete the shape, fill in the rest with asterisk characters. The direction of the drawing should be clockwise, starting from the center of the shape and working outwards, with the initial direction being down and to the right.


## Usage

    $ java -jar squares-squared-0.1.0-standalone.jar k n

where k and n are integers: the start and the end.

## Test examples

java -jar squares-squared-0.1.0-standalone.jar 2 2
> "2"


java -jar squares-squared-0.1.0-standalone.jar 2 4
> " 2 "
> "* 4"
> " * "


java -jar squares-squared-0.1.0-standalone.jar 3 81
> " 3 "
> "1 9"
> " 8 "


java -jar squares-squared-0.1.0-standalone.jar 4 20
> " 4 "
> "* 1"
> " 6 "

java -jar squares-squared-0.1.0-standalone.jar 2 256
> "  6  "
> " 5 * "
> "2 2 *"
> " 6 4 "
> "  1  "


java -jar squares-squared-0.1.0-standalone.jar 10 10000
> "   0   "
> "  1 0  "
> " 0 1 0 "
> "* 0 0 0"
> " * 1 * "
> "  * *  "
> "   *   "

## License

Copyright © 2014 Antti Rämö

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
