# Extracts the giant component of a graph 
# Apr 15 2021 Dan Suthers extracted this from a class demo 

giant_component <- function(g) {
  c <- components(g)
  induced_subgraph(g, which(c$membership == which.max(c$csize)))
}

# Pau 