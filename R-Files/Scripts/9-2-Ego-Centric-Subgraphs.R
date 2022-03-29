######################################################################
# Extracting Ego-Centric Subgraphs 
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
# Mar 10 2022 DS: Updates for ICS 422/622 Spring 2022
# * Split off Ego-Centric Subgraphs into a new script for Thursday
# * Reordering to plot first, then look at metrics. 
######################################################################
# Setup 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 

source("Utility/topnv.R")

NS <- read.graph("Networks/netscience.graphml", format="graphml")
summary(NS)

######################################################################
# Making Ego-Centric Subgraphs
######################################################################
# These let us query the neighborhood of important nodes. 

# It may be worth bringing up Network Science in Gephi to follow this.
# Look up the IDs of Barabasi and Newman, add Abramson from our prior
# demo, and Yang who we saw in Gephi is a member of an isolated
# cluster:

(B <- V(NS)[which(V(NS)$label=="BARABASI, A")])  # Important actor 
(N <- V(NS)[which(V(NS)$label=="NEWMAN, M")])    # Important actor
(A <- V(NS)[which(V(NS)$label=="ABRAMSON, G")])  # High transitivity 
(Y <- V(NS)[which(V(NS)$label=="YANG, M")])      # In isolated cluster

# This is how I used to do it ... 
# Get the nodes 1 step away from ego 

?ego # note you can control order and mode 
ego(NS, 1, B)[[1]]

# Then make an induced subgraph from these vertices 

?induced_subgraph 
summary(induced_subgraph(NS, ego(NS, 1, B)[[1]]))

# This was worth noting as ego and induced_subgraph are useful
# elsewhere, but there is a new addition that does it in one step:

?make_ego_graph

# It returns a vector of graphs so we have to strip out the first
# element with [[1]]

NS.B <- make_ego_graph(NS, 1, B)[[1]]
NS.B$name <- paste("Ego graph for", B$label) 
summary(NS.B)

# Notice it is the same graph and attributes are preserved. 
# Let's plot it and compare to some others ... 
# new_window("Egos in Network Science", 12, 12) # if desired 

par(mfrow=c(2,2))
plot(NS.B, main=paste0(B$label, " Ego 1, |V|=", vcount(NS.B)),
     vertex.color=V(NS.B)$label == B$label, vertex.label=NA, vertex.size=10) 

# Do the same for Newman, Abramson, and Yang 

NS.N <- make_ego_graph(NS, 1, N)[[1]] # Newman
plot(NS.N, main=paste0(N$label, " Ego 1, |V|=", vcount(NS.N)), 
     vertex.color=V(NS.N)$label == N$label, vertex.label=NA, vertex.size=10) 
NS.A <- make_ego_graph(NS, 1, A)[[1]] # Abramson
plot(NS.A, main=paste(A$label, " Ego 1, |V|=", vcount(NS.A)),
     vertex.color=V(NS.A)$label == A$label, vertex.label=NA, vertex.size=10) 
NS.Y <- make_ego_graph(NS, 1, Y)[[1]] # Yang
plot(NS.Y, main=paste(Y$label, " Ego 1, |V|=", vcount(NS.Y)),
     vertex.color=V(NS.Y)$label == Y$label, vertex.label=NA, vertex.size=10) 

# ***** Initial observations of the differences between ego networks? 

# Increasing ego distance can be instructive about the relationship
# between local transitivity and betweenness. Here's a function to 
# repeat the above for different orders: 

make_and_plot_ego_graph <- function(g, e, order=1){
	eg <- make_ego_graph(g, order, e)[[1]]
	plot(eg, main=paste0(e$label, " Ego ", order, ", |V|=", vcount(eg)), 
	     vertex.color=V(eg)$label == e$label, 
	     vertex.label=NA, vertex.size=10)
	return(eg)
}

NS.B2 <- make_and_plot_ego_graph(NS, B, 2)
NS.N2 <- make_and_plot_ego_graph(NS, N, 2)
NS.A2 <- make_and_plot_ego_graph(NS, A, 2)
NS.Y2 <- make_and_plot_ego_graph(NS, Y, 2)

NS.B3 <- make_and_plot_ego_graph(NS, B, 3)
NS.N3 <- make_and_plot_ego_graph(NS, N, 3)
NS.A3 <- make_and_plot_ego_graph(NS, A, 3)
NS.Y3 <- make_and_plot_ego_graph(NS, Y, 3)

NS.B4 <- make_and_plot_ego_graph(NS, B, 4)
NS.N4 <- make_and_plot_ego_graph(NS, N, 4)
NS.A4 <- make_and_plot_ego_graph(NS, A, 4)
NS.Y4 <- make_and_plot_ego_graph(NS, Y, 4)

# Notice that some of these are isolated clusters. 

# Compare transitivities and betweenness in a data frame (which in
# this case is cleaner than a tibble).

rnames <- c(B$label, N$label, A$label, Y$label)
cnames <- c("Ego", "LocalCC", "EgoCC", "Betweenness") 
NS.Trans <- data.frame(rnames, matrix(data=0, nrow=4, ncol=3))
colnames(NS.Trans) <- cnames
NS.Trans # empty

# Local transitivity within the larger graph. Turning off weights to
# simplify interpretation.

NS.Trans$LocalCC <- transitivity(NS, type="local", weights=NA, 
                                 vids=c(B, N, A, Y))

# Global transitivity of their ego graphs 

NS.Trans$EgoCC <- c(transitivity(NS.B, type="global", weights=NA), 
                    transitivity(NS.N, type="global", weights=NA), 
                    transitivity(NS.A, type="global", weights=NA), 
                    transitivity(NS.Y, type="global", weights=NA))

# Betweenness is related (rounding to avoid e notation). Also turning
# off weights because they are not distances (will discuss later).

V(NS)$betweenness <- betweenness(NS, normalized=TRUE, weights=NA)
NS.Trans$Betweenness <- c(round(V(NS)$betweenness[[B]], 8),
                          round(V(NS)$betweenness[[N]], 8), 
                          round(V(NS)$betweenness[[A]], 8), 
                          round(V(NS)$betweenness[[Y]], 8))

# Results 

NS.Trans

# ****** Discuss these results in comparison to the visualizations  
#        Start with Abramson and Yang 

# Cleanup 
# if you were using external window 
# dev.set(2) 
# otherwise 

par(mfrow=c(1,1))

######################################################################
# Activities (edit your copy of this script below)
######################################################################

# 1a. Find the highest ranked node for betweenness centrality in
# Network Science (NS) and save its vertex ID. (Hint: topnv)
# Then print the label on this vertex, and also the betweenness value. 

id <- .... 

# 1b. Do you expect the local clustering coefficient of that node to
# be high or low compared to the average local CC of all vertices?
# Why?


# 1c. Compute it and compare it to the average local CC. Are you
# right?


# 1d. Make the ego network for the node you found. and plot that
# network. Discuss results in relation to its high betweenness.


# 1e. Compute the global transitivity for that ego network. 
# Compare to local transitivity of the node in the full graph. 
# Explain why the results are as expected.


# 2. Write out the annotated NS graph and compare igraph weighted 
# and unweighted clustering coefficient to those computed in Gephi 
# by Avg. Clustering Coefficient. Which is Gephi using? 
# Do you see other differences in the results, and what explains it? 


# Solution in 9-3-Local-Structure-Exercise-Solution.R
######################################################################
# Points to not miss 
# * Gephi does not include NaN nodes in its avg. clustering coefficient
#   average, but displays them as having value 0 so you can't tell 
#   them from actual 0! 
######################################################################
# Pau for now