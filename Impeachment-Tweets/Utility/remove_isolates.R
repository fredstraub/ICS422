######################################################################
# remove_isolates
# Removes isolates from an igraph 
# Sun Oct  7 04:46:15 2018 Dan Suthers extracted from another script 
######################################################################
library(igraph)

remove_isolates <- function(g) {
  g.degree <- degree(g)
  g.isolates <- V(g)[which(g.degree < 1)]
  return(delete_vertices(g, g.isolates))
}

######################################################################
# Pau 