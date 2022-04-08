######################################################################
# Activity: Link Community Analysis of TI-Chats-Week-of-060401.graphml
# Mar 28 2020 DS: Minor updates for self-study students.
# Apr  6 2021 DS: Updates for ICS 422/622 Spring 2021
#  (Split off Centrality metrics into separate demo.)
# Apr  5 2022 DS: Minor updates for ICS 422/622 Spring 2022 
######################################################################
# 0. Setup 
 
library(igraph)
library(linkcomm)
setwd("~/Desktop/Network-Science-Demos")

# Function to get the communities a node is in 
# (repeating in case we are in a new session)

getNodeCommunities <- function(lc, id) {
  as.numeric(lc$nodeclusters$cluster[which(lc$nodeclusters$node == id)])
}

########################################################
# 1. Prepare an Edge List Representation of the Network

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", 
                format="graphml")
summary(TI)

# Make edgelist representation 

TI_edges <- as_edgelist(TI)
TI_edges <- cbind(TI_edges, E(TI)$weight)
head(TI_edges)

#############################
# 2. Compute Link Communities 
# Ensure you can write to getwd()!!!

?getLinkCommunities # for reference 
TI_lc <- getLinkCommunities(TI_edges, hcmethod="average", 
                           directed=TRUE, plot=FALSE)

########################################
# 3. Inspecting Link Communities

TI_lc # summary of results 

# (a) How many link communities did you find? 

# (b) Plot the link communities. Would you like a better approach to
# plotting? :-)

plot(TI_lc, type = "graph") # for regular layout, or use: 
plot(TI_lc, type = "graph", layout="spencer.circle")

# This motivates an alternative method of visualization I will show you! 

########################################
# 4. Inspecting Induced Overlapping Node Communities 

# (a) What are the induced node communities with the most
# nodes (top 5)?

head(TI_lc$clustsizes, 5)

# (b) Can you plot the largest node community and identify the node
# with the largest page_rank in this community?

largest <- induced_subgraph(TI, getNodesIn(TI_lc, clusterids = 121))
V(largest)$page_rank <- page_rank(largest)$vector 
V(largest)[which(V(largest)$page_rank == max(V(largest)$page_rank))]$label
plot(largest, main="Cluster 121", 
     layout=layout_with_fr, # with_fr or in_circle
     vertex.size=5, vertex.label.cex=0.7,
     vertex.color=V(largest)$page_rank == max(V(largest)$page_rank))

# (c) What are the names of the actors who are in the most induced
# node communities (top 5)?

head(TI_lc$numclusters, 18) # It is sorted by number
V(TI)[as.numeric(names(head(TI_lc$numclusters, 5)))]$label

# (d) Which communities are both JeffC and BjB in?

(J <- which(V(TI)$label == "JeffC"))
(B <- which(V(TI)$label == "BjB"))
intersect(getNodeCommunities(TI_lc, J), getNodeCommunities(TI_lc, B))

# (e) Is anyone in all of the communities that JeffC is in? BjB?

get.shared.nodes(TI_lc, getNodeCommunities(TI_lc, J)) # JeffC = 34
get.shared.nodes(TI_lc, getNodeCommunities(TI_lc, B)) # BjB = 545

# (f) Plot community membership for the top nodes (the matrix plot): 

plot(TI_lc, type = "members")     # Community membership for top nodes

# (g) Which nodes have similar community membership in top
# communities?

V(TI)[c(359, 636, 471)]$label

# (h) Which communities have similar membership among the top nodes?
#     --> Read this off the membership matrix. 

########################################
# 5. Nested Communities 

# (a) Find and plot all nested communities, saving the returned
# results. (Hang on to your hat!)

nestings <- getAllNestedComm(TI_lc, plot=TRUE)

# (b) How many communities are nested in other communities?

length(nestings)

# See Rathnayake-Suthers-HICSS-2021.pdf for example application. 

######################################################################
# Pau 
