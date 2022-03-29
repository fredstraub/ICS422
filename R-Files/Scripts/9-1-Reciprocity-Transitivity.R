######################################################################
# Reciprocity and Transitivity 
# 
# Dan Suthers, October 10, 2016. 
# March 7, 2018 (after class) DS
# * Includes use of new make_ego_graph. 
# * Added comparison to powerlaw transitivity
# * betweenness in data frame
# August 25, 2018 DS: Minor updates for current script style and clarity
# October 22, 2019 DS: Updates for 2019 class. 
# Feb 29 2020 DS: Minor updates for self study students. 
# Mar  9 2021 DS: Updates for ICS 422/622 Spring 2021
# Mar  8 2022 DS: Updates for ICS 422/622 Spring 2022
# * Split off Ego-Centric Subgraphs into a new script for Thursday
######################################################################

library(igraph)
setwd("/Users/fred/Github/ICS422/R-Files") # Set to yours 
source("Utility/topnv.R")

######################################################################
# Graphs we will work with

# Undirected example

NS <- read.graph("Networks/netscience.graphml", format="graphml")
summary(NS)

# Directed example 

TI <- read.graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")
summary(TI)

######################################################################
# Reciprocity 
######################################################################

?reciprocity

# Artificial Example (from the lecture)

rg <- graph_from_literal(v1+-+v2, v1-+v3, v3-+v2)
plot(rg, main="Reciprocity Example", vertex.size=30)

# ***** Do a mental computation of the reciprocity of this graph! 

reciprocity(rg)

# Natural Example 

reciprocity(TI)

# How does that compare to a random graph of the same size? 

TI.gnm <- sample_gnm(vcount(TI), ecount(TI), directed=TRUE)
reciprocity(TI.gnm)

# What about a degree sequence constrained random model? 
# (Note we give degree sequence in both directions.)

TI.conf <- sample_degseq(degree(TI, mode="out"), degree(TI, mode="in"))
reciprocity(TI.conf)

# ***** What does the reciprocity, in comparison to the null models, 
#       tell you about TI Chats? What would explain its reciprocity? 
#       Consider where users chat and how I made the graph ... 

# Gephi: no reciprocity! 

######################################################################
# Transitivity
######################################################################
# Artificial Examples

?transitivity 

# This merits a little study ... 

# Notice that localaverage, localundirected, localaverageundirected are 
# defined but not documented. They are all the Watts-Strogatz version.

# ***** In which versions or when are these used? 
#       - directed arcs 
#       - weights 
#       - isolates 

# ***** Does giving vids speed it up if you just want a few? 

# Example from lecture 

tg <- graph_from_literal(1-2, 1-3, 1-4, 2-3, 4-3)
plot(tg, main="Transitivity Example", vertex.size=30)

# Global transitivity is number of two-paths that are closed. 
# It is a measure of graph-wide edge clustering 

# ***** Do the mental arithmetic to predict the transitivity 
#       of the plotted graph before running this:
#       - how many two-paths? 
#       - how many of them are closed? 

transitivity(tg, type="global")

# Local transitivity is the proportion of neighbors of a node
# that are connected. It is a property of vertices. 

transitivity(tg, type="local")

# ***** Interpret the vector

# Watts-Strogatz is average of local, and not the same as global: 

sum(transitivity(tg, type="local"))/4  # take average of above .. 
transitivity(tg, type="localaverage")

# ***** Conceptually, why is localaverage higher than the global
#       measure? Consider nodes 2 and 4. 

# Weighted transitivity (elaborated from igraph documentation)

gw <- graph_from_literal(A-B:C:D:E, B-C:D, C-D) # RStudio incorrectly flags
E(gw)$weight <- 1

# Give the A-E edge weight of 5: interesting igraph notation here! 

E(gw)[ V(gw)[name == "A"] %--% V(gw)[name == "E" ] ]$weight <- 5
plot(gw, vertex.size=30, edge.width=2*E(gw)$weight, 
     main="Weighted Transitivity Example")

# Local clustering coefficient with all edges weighted equally 

transitivity(gw, vids="A", type="local") # this method ignores weights

# ***** Why is that the correct value for A? Explain the computation. 
#       There are 4 neighbors ... 

# With weights: A--E has more weight 

transitivity(gw, vids="A", type="weighted")

# ***** Why is local transitivity of A lower when weighted? 

# If you want values for all of them: 

transitivity(gw, type="local")
transitivity(gw, type="weighted") 
transitivity(gw, type="weighted", isolates="zero") # if you prefer 

######################################################################
# Transitivity in a natural graph

# Global: Property of the graph 

transitivity(NS, type="global")
transitivity(sample_gnm(vcount(NS), ecount(NS)), type="global")

# Local: property of vertices 

transitivity(NS, type="local")[1:20]
transitivity(NS, type="weighted")[1:20]

# ***** Why NaN? Find the degrees of BOTH vertices and tell me!

# We can specify that these vertices have 0 transitivity ... 

transitivity(NS, type="localaverage")
transitivity(NS, type="localaverage", isolates="zero")

# ***** Why do the results differ in the direction shown? 

# Let's store the results in NS 

V(NS)$unweighted_cc <- transitivity(NS, type="local")
V(NS)$weighted_cc <- transitivity(NS, type="weighted")

# Who has the top values? 

topnv(NS, page_rank(NS)$vector)$label # The major players, but ... 
topnv(NS, transitivity(NS, type="local"))$label

# These are not the same people as in our centrality results! 

# ***** How would you interpret that result in this domain?
#       Clue: 

topnv(NS, transitivity(NS, type="local"))$unweighted_cc

# Weighting changes the ordering but the top values are still 1 

topnv(NS, transitivity(NS, type="weighted"))$label
topnv(NS, transitivity(NS, type="local"))$weighted_cc

######################################################################
# Gephi 

# We can look at the above nodes in Gephi to find their setting. 
# Then show how to compute local and global transitivity in Gephi. 

write_graph(NS, "Network-Science-Transitivity.graphml", format="graphml")

######################################################################
# Pau 