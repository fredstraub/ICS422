######################################################################
# Centralities: Class Activity Template 
# Dan Suthers, March 1, 2018
######################################################################
library(igraph)

######################################################################
# Activity: Analyze centralities in the Les Miserables network: 
# 
# Les Miserables: coappearance weighted network of characters in the
# novel Les Miserables. D. E. Knuth, The Stanford GraphBase: A
# Platform for Combinatorial Computing, Addison-Wesley, Reading, MA
# (1993).
# 
######################################################################

# Load in the network
LM <- read_graph("Networks/Les-Miserables.graphml", format="graphml")
summary(LM) # it is undirected weighted 

# Compute the following metrics: 
# Degree: 

# Weighted degree: 

# Eigenvector: 

# Page_rank: 

# Hubs and authorities do not apply to undirected graphs. 

# Betweenness: 

# Closeness: 

# Write out: 
write_graph(NS, "LesMis_with_centralities.graphml", format="graphml")

# Load into Gephi, and visualize. 
# Identify important actors
# Compute the same metrics in Gephi and compare
# Share in the google doc 

######################################################################
# Pau 