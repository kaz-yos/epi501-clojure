# EPI501 Clojure project

This is a project created for a network-based SEIR infectious disease modeling for the EPI501 Dynamics of Infectious Disease class at Harvard School of Public Health.

## Installation

Clone the Leiningen project directory

## Usage

For testing, do the following in the project directory.

    $ lein test

or

    $ lein midje

Equivalent tests are implemented in the default test environment and Midje.


For running the default model and plotting, do the following.

    $ lein run

You can alternatively create an executable .jar file in the target directory by 

    $ lein uberjar


## Example figure

![Graph 2](./resources/graph2.png)


## License

Copyright © 2014-2015 Kazuki Yoshida

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
