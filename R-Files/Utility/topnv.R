######################################################################
# topnv 
# Returns the top N vertices ("top n v") in a graph under a set of
# values, which can be anything. Typical use: ranking nodes by
# centrality metrics.
# Mar  8 2022 Dan Suthers created from old code. 
######################################################################

require('igraph')

topnv <- function(graph, values, n=10) {
  return(V(graph)[order(values, decreasing=TRUE)[1:n]])
}

######################################################################


