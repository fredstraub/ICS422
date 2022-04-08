######################################################################
# Overlapping Community Detection: Preparing to Visualize in Gephi
# Les Miserables Version
# 
# This is an optional demo: if time is short in class we may just skip
# to 12-6-reify_link_communities-Demo.R)
# 
# Continues 12-1-Link-Communities-basics.R. Purpose is to enable easy
# visualization of overlapping community membership of nodes induced
# by link communities, using Gephi. This script steps you through the
# reasoning and computations needed to do so. The computations will be
# encapsulated in reify_link_communities.R, which you can use without
# this script. To be followed by 12-6-reify_link_communities-Demo,
# which demonstrates the use of the consolidated code.
#
# Dan Suthers, April 6, 2017 (NetScience version)
# April 3, 2018 DS: Updated for Les Miserables
# Sep 19, 2018 DS: Minor updates for current script style and clarity
# Nov 14 2019 DS: I will not have time to show this in class, but 
#  cleaned it up a bit for those who want to study it out of class.
# Mar 28 2020 DS: Minor updates for self-study students. 
# Apr  6 2021 DS: Updates for ICS 422/622 Spring 2021
# Apr  7 2022 DS: Updates for ICS 422/622 Spring 2022: 
#   LM_wlc$nodeclusters$cluster is no longer a factor (possibly as
#   part of move to tidyverse); found it simpler to use unique. 
#   Also now using add_edges to add all edges at once. 
# 
######################################################################
# This section needed only if you have not just run the 12-1 demo. 

library(igraph)
library(linkcomm)

setwd("~/Desktop/Network-Science-Demos") # Set to yours 
LM <- read.graph("Networks/Les-Miserables.graphml", format="graphml")

# Compute the link communities. The weighted version may be more
# accurate, as it reflects number of times characters co-appear,
# and the Louvain and InfoMap methods will also be using weights. 

LM_edges <- as_edgelist(LM)
LM_wedges <- cbind(LM_edges, E(LM)$weight)
LM_wlc <- getLinkCommunities(LM_wedges, hcmethod="average", plot=FALSE)

######################################################################
# The approach below will add vertices representing communities to 
# the graph, so compute any metrics we want on the graph BEFORE we
# add them! We will compare to other community detection methods. 

V(LM)$degree       <- degree(LM)
V(LM)$wdegree      <- strength(LM, mode="all")
V(LM)$pagerank     <- page_rank(LM)$vector
# our weights are not distances 
V(LM)$betweenness  <- betweenness(LM, normalized=TRUE,
                                  weights=1/E(LM)$weight)
V(LM)$comm_louvain <- membership(cluster_louvain(LM))
V(LM)$comm_infomap <- membership(cluster_infomap(LM))

# This won't work: see 12-3
# V(LM)$commweight   <- getCommunityCentrality(LM_wlc, type="commweight")
# V(LM)$commconn     <- getCommunityCentrality(LM_wlc, type="commconn") 

summary(LM)

######################################################################
# Visualization is easier in Gephi, but how to visualize multiple 
# membership in communities? Is there something better than pie charts?
#
# Let's use a method I developed for Tapped In: make a vertex for 
# each community, and make an edge from each original vertex to the 
# community vertex of the community to which it belongs. (Edges will
# be directed only if the original graph is directed. Direction is 
# chosen so in-degree of community vertex is size.)
#
# Below I step you through an explanation of the code. You do not
# need to follow this demo to use reify_link_communities.R, which
# compiles this code into a single function. 
#
######################################################################
# Copy the Graph and mark existing nodes as not community nodes
# to aid filtering in Gephi. 

LM_comm <- LM              # make a copy we will modify 
V(LM_comm)$comm_p <- FALSE # to tell these from new nodes we will make 

######################################################################
# Making vertices representing communities
######################################################################
# How do we get a list of cluster (community) names as strings? 
# The cluster IDs are available in this dataframe: 

?getLinkCommunities
head(LM_wlc$nodeclusters) # They are in the column called $cluster
unique(LM_wlc$nodeclusters$cluster) # This will do it 

# ***** Note: I believe that this can be replaced with 
1:LM_wlc$numbers[[3]]
# but need to test before making the change. 

# Function to make a label for each cluster with a type prefix and ID. 

comm_label <- function (id) {return(paste0("COMM_", id))}

# For example: 

comm_label(1)

# These will be the names of "community" vertices for each cluster.

comm_names <- as.character(lapply(unique(LM_wlc$nodeclusters$cluster),  
                                  comm_label))
comm_names

# Add these vertices all at once, copying the graph just once. 

?add_vertices # lets you add other attributes; we will do so

LM_comm <- add_vertices(LM_comm, length(comm_names), 
                        label = comm_names, comm_p = TRUE)

vcount(LM)
summary(LM_comm)         # notice change in vertex count for 13 comms
V(LM_comm)$label[75:85]  # at boundary of actors and communities 
V(LM_comm)$comm_p[75:85]  

######################################################################
# Making the original vertices point to their community vertices
######################################################################
# We need a link for each row in this matrix: 

head(LM_wlc$nodeclusters)

# Get list of vertices from the original graph that are in link
# communities (some may not be, so we use $node, not V(g)). Note that
# node_ids <- lc$nodeclusters$node did not work.

node_ids <- as.numeric(LM_wlc$nodeclusters$node)
head(node_ids)

# However, since we are now identifying communities by string 
# labels generated from cluster IDs, we need to map from the IDs 
# via these strings to actual vertex IDs, for example: 

which(V(LM_comm)$label == comm_label(1)) # vertex ID is not 1! 

# vapply lets us specify that we want a character vector result,
# and may also be faster than sapply.

?vapply

# We use it here to make a list of the names of the communities
# that each node is in 

comm_labels <- as.vector(vapply(LM_wlc$nodeclusters$cluster, 
                                comm_label, 
                                character(1)))
head(comm_labels, 8)

# Add edges from nodes to communities all at once. add_edges wants a
# list of alternating source and target vertices, that is, pairs of
# node + community vertices.

?add_edges

# This will make the pairs: 

node_and_comm <- function(i) { 
  c(node_ids[i], 
    which(V(LM_comm)$label == comm_labels[i]))
}

# Apply this to sequence of node positions in the lists. 
pairs <- lapply(1:length(node_ids), node_and_comm)
head(pairs)
head(unlist(pairs))

LM_comm <- add_edges(LM_comm, 
                     unlist(lapply(1:length(node_ids), node_and_comm)))

ecount(LM)
summary(LM_comm) # edge count increases

######################################################################
# Write it out for inspection in Gephi. 

write_graph(LM_comm, "Les-Mis-Reified-Communities.graphml", 
            format="graphml")

# We can now take a quick look at what we have accomplished in Gephi. 
# * Size nodes by degree and Color nodes by comm_p (I usually make
#   community nodes yellow)
# * Give it a good layout (Force Atlas2, nonoverlap, linlog, gravity 5) 
# * Point at community nodes to see membership.
# * Use ego filter to see only the community 
# * Change the node coloring between Louvain and InfoMap to see how
#   these compare to link communities ("null" nodes are community 
#   nodes: make them yellow)
#
# Next, this is packaged into function reify_link_communities.R, and
# demonstrated in 12-6-reify_link_communities-LeMis-Demo.R
# 
######################################################################
# Pau 

