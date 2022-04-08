######################################################################
# Demonstration of reify_link_communities using Les Miserables
# 
# Demonstration 12-5-Link-Communities-to-Gephi-LesMis.R shows how
# reify_link_communities.R was constructed, step by step. It creates a
# node for every link community and makes every node in the original
# graph point to its respective link community. This short demo shows
# how to use that utility.
#
# Dan Suthers, April 2017
# Apr  4 2018 DS: Les Miserables version this year
# Sep 19 2018 DS: Minor updates for current script style and clarity
# Nov 14 2019 DS: Checking and cleaning up commments for topic 11 
# Mar 28 2020 DS: Minor updates for self-study students. 
# Apr  6 2021 DS: Updates for ICS 422/622 Spring 2021
# Apr  7 2022 DS: Updates for ICS 422/622 Spring 2022
#
######################################################################
# This section needed only if you have not run the 12-1 or 12-5 demo. 

library(igraph)
library(linkcomm)

setwd("~/Desktop/Network-Science-Demos") # Set to yours 
LM <- read_graph("Networks/Les-Miserables.graphml", format="graphml")

# Compute the link communities.
LM_edges <- as_edgelist(LM)
LM_wedges <- cbind(LM_edges, E(LM)$weight)
LM_wlc <- getLinkCommunities(LM_wedges, hcmethod="average", plot=FALSE)

######################################################################
# Compute any metrics we want on the graph.
# Reason for this: we will be adding nodes to the graph, so should 
# compute metrics that may be affected before the nodes are added. 

V(LM)$degree       <- degree(LM)
V(LM)$wdegree      <- strength(LM, mode="all")
V(LM)$pagerank     <- page_rank(LM)$vector
V(LM)$betweenness  <- betweenness(LM, normalized=TRUE, 
                                  weights=1/E(LM)$weight)
V(LM)$comm_louvain <- membership(cluster_louvain(LM))
V(LM)$comm_infomap <- membership(cluster_infomap(LM))
summary(LM)

######################################################################
# It's a four line demo! 

# Load the utility 
source("Utility/reify_link_communities.R")

# Reify link communities in a copy of the graph. 
LM_comm <- reify_link_communities(LM, LM_wlc)
summary(LM_comm) 

# Write it out for inspection. 
write_graph(LM_comm, "Les-Mis-Reified-Communities-v2.graphml", 
            format="graphml")

######################################################################
# 
# We can now take a quick look at what we have accomplished in Gephi. 
# * Size nodes by degree and Color nodes by comm_p (I usually make
#   community nodes yellow)
# * Give it a good layout (Force Atlas2, nonoverlap, linlog, gravity 5) 
# * Point at community nodes to see membership.
# * Use ego filter to see only the community 
# * Change the node coloring between Louvain and InfoMap to see how
#   these compare to link communities ("null" nodes are community 
#   nodes: make them yellow)
# * Size nodes by community centrality and interpret. 
#
######################################################################
# Activity 12-7: 
# 
# Do the same with Tapped In Chats or Network Science (HW)
# * Generate link communities 
# * Compute metrics 
# * reify_link_communities
# * Write out and examine communities in Gephi 
# 
######################################################################
# Pau. 