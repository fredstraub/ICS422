######################################################################
# Adjusting weights in Network Science: comparing methods
# (For Analysis-5)
# Mar 10 2022 DS elaborated a bit on today's class demo version
######################################################################

library(igraph)
setwd("/Users/fred/Github/ICS422/R-Files")
source("Utility/topnv.R")

NS <- read.graph("Networks/netscience.graphml", format="graphml")
summary(NS)

# Existing weights 
ew <- E(NS)$weight
# Negated weights 
nw <- (max(ew) + 1) - ew 
# Inverted weights 
iw <- 1/ew 

# Compare (head were too similar) 
tail(ew, 10)
tail(nw, 10)
tail(iw, 10)

hist(ew, breaks=100, main="Existing Weights")
hist(nw, breaks=100, main="Negated Weights")
hist(iw, breaks=100, main="Inverted Weights")

# Notice how inversion spreads out the low values more, while negation
# keeps a similar distribution.

# The large weights on the right of the IW distribution come from: 

min(ew)
1/min(ew) # x axis 
(20*19)/2 # y axis: guessing these are the weights on the clique of 20

# Comparing the effect of weights on rankings: 
library(tibble)
tibble(
  unweighted = topnv(NS, betweenness(NS, weights=NA, normalized=TRUE))$label,
  incorrect  = topnv(NS, betweenness(NS, weights=ew, normalized=TRUE))$label,
  negated    = topnv(NS, betweenness(NS, weights=nw, normalized=TRUE))$label, 
  inverse    = topnv(NS, betweenness(NS, weights=iw, normalized=TRUE))$label
)

# Although the winners are largely the same, we do get different
# answers even from the two corrections. Need to think through the
# justification of each.

######################################################################
# Pau 