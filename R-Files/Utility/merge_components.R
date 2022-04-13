######################################################################
# INCOMPLETE -- not working in functional environment. 
# Posted here in case someone wants to work on it. 
library(igraph)

# Find disconnected components and link them in to the giant component
# Written for undirected graphs but might do something on directed. 

# Note: one must add the edges in one pass. If one iterates over 
# components to add edges then the graph is changed and one must
# recompute components to get current vertex IDs. 

make_connected <- function(g) {
  comp <- components(g)
  while (comp$no > 1) {
    g <- merge_2_components(g)
    comp <- components(g)
  }
  return(g)
}

merge_2_components <- function(g) {
  comp <- components(g)
  V(g)$membership <- comp$membership
  if (comp$no > 1){
    # Randomly connect vertices from two different components 
    # relying on the fact that there will always be #1 and #2 
    v1 <- sample(which(V(g)$membership == 1), 1)
    cat(v1, " ")
    v2 <- sample(which(V(g)$membership == 2), 1)
    cat(v2, "\n")
    g <- add_edges(g, c(v1, v2))
  }
  return(g)
}