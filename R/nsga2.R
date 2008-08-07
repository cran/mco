##
## nsga2.R - Interface to nsga2.c
##
## Authors:
##  Heike Trautmann  <trautmann@statistik.uni-dortmund.de>
##  Detlef Steuer    <detlef.steuer@hsu-hamburg.de>
##  Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

nsga2 <- function(fn, idim, odim, ...,
                  constraints=NULL, cdim=0,
                  lower.bounds=rep(-Inf, idim),
                  upper.bounds=rep(Inf, idim),
                  popsize=100, generations=100,
                  cprob=0.7, cdist=5,
                  mprob=0.2, mdist=10) {
  ff <- function(x)
    fn(x, ...)
  cf <- function(x)
    constraints(x, ...)

  ## Make sure popsize is a multiple of 4
  if (popsize %% 4 != 0)
    stop("Population size must be a multiple of 4")
  
  ## Set cdim = 0 if no cfn was given:
  if (is.null(constraints)) cdim <- 0
  
  res <- .Call("do_nsga2",
               ff, cf, sys.frame(),
               as.integer(odim),
               as.integer(cdim),
               as.integer(idim),
               lower.bounds, upper.bounds,
               as.integer(popsize), as.integer(generations),
               cprob, as.integer(cdist),
               mprob, as.integer(mdist))
  names(res) <- c("par", "value", "pareto.optimal")
  class(res) <- c("nsga2", "mco")
  return (res)
}

plot.nsga2 <- function(x, ...) {  
  v <- x$value
  o <- x$pareto.optimal
  d <- ncol(v)
  col <- ifelse(o, "red", "blue")
  pch <- ifelse(o, 4, 19)
  if (d <= 2) {
    plot(v, col=col, pch=pch, ...)
    ov <- v[o,]
    ov <- ov[order(ov[,1]),]
    lines (ov, col="red", type="s")
  } else if (d == 3) {
    if (require(scatterplot3d)) {
      scatterplot3d(v, color=ifelse(o, "red", "blue"))
    } else {
      pairs(v, col=col, pch=pch, ...)
    }
  } else {
    pairs(v, col=col, pch=pch, ...)
  }
}
