######################################################################
# Class exercise: Community detection in Tapped In Chats Sociogram
# Thu Apr  1 2021 DS: Created this template for 422/622 spring 2021
######################################################################
# Setup if starting fresh 

library(igraph)
library(tibble)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 
TI <- read.graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")

######################################################################
# 1. Do community analysis for the TI Chats sociogram 
# Try cluster_louvain and cluster_infomap (you may also try others) 
#   --> There will be a problem you should solve
is_directed(TI)
# Louvain 
TI.louvain <- cluster_louvain(TI)
TI.louvain <- cluster_louvain(as.undirected(TI, mode="collapse"))
modularity(TI.louvain)
# Infomap 
TI.infomap <- cluster_infomap(TI)

# ----------
# Show modularity, number of communities, and table of community sizes 

modularity(TI.infomap)
# Interpret this in terms of known limitations of Louvain and Infomap

## The graph is directed and these algorithms don't work.

# -----
# Assign community membership as vertex attribute V(TI)$... 
TIM <- membership <-

# -----
# Write out this graph for reading into Gephi
summary(TI) # Make sure both Louvain and Infomap results are there
write_graph(TI, "TI-Chats-Communities.graphml", format="graphml")

# ------------------------------------------------------------
# 2. In Gephi: Compare igraph to Gephi results 
# * Compute "Modularity" (Louvain method) and Connected Components 
# * Compare the results to igraph by coloring and in Data Laboratory
#   - What kinds of components do they differ on?  
#   - Do you see the resolution limit? The horizon limit? 
#   - How can you get Louvain's results to be more similar to Infomap's? 

######################################################################
# Pau 
