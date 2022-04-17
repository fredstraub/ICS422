# Extracts the giant component of a graph 
# Apr 15 2021 Dan Suthers extracted this from a class demo 
# Apr 14 2022 DS added mode parameter 

giant_component <- function(g, mode="weak") {
  c <- components(g, mode=mode)
  induced_subgraph(g, which(c$membership == which.max(c$csize)))
}

# Pau 