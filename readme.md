# Conway's Game of Life in Guile

This is [Conway's Game of Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life), written in [Guile](http://www.gnu.org/software/guile/), an implementation of the Scheme programming language. To augment the more idiomatic Scheme implementation on [Rosetta Code](http://rosettacode.org/wiki/Conway%27s_Game_of_Life), this version explores how one deals with the more day-to-day aspects of software development in Guile, such as:

* Code organisation with modules
* Encapsulation with closures and message passing
* Unit testing
* IO

## The Game

Load it up into the interpreter with `guile -l gameoflife.scm`. Functions exposed are:

* `game-of-life`: function that creates the game of life closure, which can receive the following messages:
    * `(setgrid! grid)`: stores `grid` (a list of list of 0s and 1s) into the game
    * `(getgrid)`: gets the current pattern from the game
    * `(step!)`: increments the generation
* `pretty-print-grid`: converts a grid to a more readable string
* `grid-from-file`: loads up a grid from file
* `send`: sugar function for sending a message
* `new-instance`: sugar function for instantiating a new closure

See tests (below) for usage.

## Tests

Uses `srfi-64`, an API for writing test suites. The Guile 2.0 port is by [Sunjong Lee](https://lists.gnu.org/archive/html/guile-user/2012-04/msg00006.html), also included in this repository for convenience. Copy it into the site packages directory, where [third party Scheme files](https://www.gnu.org/software/guile/manual/html_node/Installing-Site-Packages.html) should be placed:

    sudo cp ./srfi-64-guile/srfi/srfi-64.scm /usr/share/guile/site/2.0/srfi/srfi-64.scm

Run tests with

    guile -l gameoflife.scm gameoflife-test.scm

and output should be written to `game_logic.log` and `utility_functions.log`

## Contributing

Scheme newbie here, so contributions on better ways of doing things are welcome!

## Sources

* [Using Guile modules](https://www.gnu.org/software/guile/manual/html_node/Using-Guile-Modules.html)
* [Unit testing module](http://srfi.schemers.org/srfi-64/srfi-64.html)
* [Installing site packages](https://www.gnu.org/software/guile/manual/html_node/Installing-Site-Packages.html#Installing-Site-Packages)
* [Classes and objects in Scheme](http://people.cs.aau.dk/~normark/prog3-03/html/notes/oop-scheme_themes-classes-objects-sec.html)
* [Scheme style guide](http://community.schemewiki.org/?scheme-style)
