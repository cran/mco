##
## mco.R - General MCO utilities
##
## Authors:
##  Heike Trautmann  <trautmann@statistik.uni-dortmund.de>
##  Detlef Steuer    <detlef.steuer@hsu-hamburg.de>
##  Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

## Extract pareto set:
paretoSet <- function(x, ...) 
  UseMethod("paretoSet", x)

## Extract pareto front:
paretoFront <- function(x, ...)
  UseMethod("paretoFront", x)

## Short helper functions to return matricies as is...
paretoSet.matrix <- function(x, ...)   { return(x) }
paretoFront.matrix <- function(x, ...) { return(x) }

