######################################################################
# Activity: Try to model Netscience and EuroSiS WebAtlas as above 
######################################################################

# If you have not been running the above demo run this now: 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

# Read in the natural graphs to be modeled, and make random graphs 
# with the same |V| and |E| 

NS <- read_graph("Networks/netscience.graphml", format="graphml")
WA <- read_graph("Networks/EuroSiS-WebAtlas-Simplified.graphml", 
                 format="graphml")

NS_gnm <- sample_gnm(vcount(NS), ecount(NS)) # just pass N, M
summary(NS)
summary(NS_gnm)

# ***** You do WA 
WA_gnm <- sample_gnm(vcount(WA), ecount(WA))
summary(WA_gnm)

# Predict regime (subcritical, critical, supercritical, connected)
# by comparing mean degree to 1 and natural log of |V| = N 

mean(degree(NS)) # will be same as NS_gnm
log(vcount(NS))

mean(degree(WA))
log(vcount(WA))

# ***** Which regime is its random model in? Answer in Google doc 

# NS model is supercritical; WA connected 

# Compare component distributions of natural to random model and see
# whether this fits the above prediction. If it does not, EXPLAIN in
# terms of the domain.

table(sapply(decompose(NS), vcount))
table(sapply(decompose(NS_gnm), vcount))

table(sapply(decompose(WA), vcount))
table(sapply(decompose(WA_gnm), vcount))

# Compare the two on distance, transitivity and degree distribution. 
# (Also compare transitivity to Gephi result and explain.)

mean_distance(NS)
mean_distance(NS_gnm)

transitivity(NS, type="global")
transitivity(NS_gnm, type="global")

plot(degree_domain(NS), 
     nonzero_degree_distribution(NS)) 
plot(degree_domain(NS_gnm), 
     nonzero_degree_distribution(NS_gnm)) 

# Then do the same for WA. Just Find and Replace NS with WA! 

mean_distance(WA)
mean_distance(WA_gnm)

transitivity(WA, type="global")
transitivity(WA_gnm, type="global")

plot(degree_domain(WA), 
     nonzero_degree_distribution(WA)) 
plot(degree_domain(WA_gnm), 
     nonzero_degree_distribution(WA_gnm)) 

######################################################################
# Pau 
