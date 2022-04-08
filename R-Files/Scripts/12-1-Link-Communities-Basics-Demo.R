######################################################################
# Overlapping Community Detection Demonstration: Link Communities 
# A step by step demonstration of linkcomm package basics. 
# 
# Dan Suthers, April 6, 2017 (NetScience version)
# Apr  3 2018 DS: Updated for Les Miserables
# Sep 19 2018 DS: Minor updates for current script style and clarity
# Nov 12 2019 DS: Removing unnecessary steps to shorten it a bit. 
# Nov 14 2019 DS: Further updates for Thursday continuation
# Mar 28 2020 DS: Minor updates for self-study students. 
# Apr  6 2021 DS: Split off Centrality metrics into separate demo.
#                 Other minor updates for ICS 422/622 Spring 2021
# Apr  6 2022 DS: Minor updates for ICS 422/622 Spring 2021 
# Apr  7 2022 DS: Removed the time consuming fill in the blank. 
#                 Converted . to _ names for current style. 
#
######################################################################
# Overview of the linkcomm package 
# 0. Setup 
# 1. Preparing the Network 
# 2. Computing Link Communities 
# 3. Inspecting Link Communities 
# 4. Inspecting Induced Overlapping Node Communities 
# 5. Nested Communities 
# 6. Community-based Measures of Centrality (separate demo) 
######################################################################
# 0. Setup 

library(igraph)

# If the following does not work, install.packages("linkcomm")

library(linkcomm)

# ***** Notice the warning, discussed below 

setwd("~/Desktop/Network-Science-Demos") # Set to yours

# Network we will use 

LM <- read_graph("Networks/Les-Miserables.graphml", format="graphml")
summary(LM)
table(E(LM)$weight) # Number of scenes two characters appear together 

# ***** NOTE: To use linkcomm, you require read and write permissions 
#       in the current directory! If needed, change it after reading 
#       the graph from Google Drive.

######################################################################
# Overview of the linkcomm package 
######################################################################

# Provides citations and related functions, but directs us to the 
# next items for documentation. getLinkCommunities is the entry. 

?linkcomm

# Brings up a useful paper on the package (on which this demonstration
# is based). Includes further information and visualization methods. 

vignette(topic = "linkcomm", package = "linkcomm")

# You can do this, but it is not explained very well, and the graph
# window clears unexpectedly. My demo is better. 
#   demo(topic = "linkcomm", package = "linkcomm")

# The main function for computing link communities. Read the 
# Arguments and we'll take a little time to discuss:

?getLinkCommunities 

# ***** What do we need to do to prepare the graph we read in for
#       getLinkCommunities? 
        
# ***** What parameters might we want to adjust for this or other
#       networks? (consider hcmethod, use.all.edges (see figure), 
#       network 3rd column, directed, bipartite) 

# Why we probably don't want to increase edgelim much: a function 
# to compute how many gigabytes of memory are needed.

gigs <- function(edgelim){(edgelim^2)*8/1024/1024/1024}
gigs(10000) # default
gigs(20000)
gigs(30000)
gigs(100000)

######################################################################
# 1. Preparing the Network 
######################################################################
# We saw we need edgelists. igraph has a conversion utility. 

?as_edgelist

LM_edges <- as_edgelist(LM)

# Demonstrating the conversion: 

head(E(LM))
head(LM_edges)

# Notice that we no longer have edge weights. But linkcomm can use 
# edge weights, and in this domain we have edge weights encoding the 
# number of times two characters appear together in a scene, which 
# seems important: 

head(E(LM)$weight, 20)

# Add the third column to make weighted edges

LM_wedges <- cbind(LM_edges, E(LM)$weight)
head(LM_wedges)

# Should look like: 
# [,1] [,2] [,3]
# [1,]   18   61    1
# [2,]   18   45    8
# [3,]   18   46   10
# [4,]   45   46    6
# [5,]   18   62    1
# [6,]   18   63    1

######################################################################
# 2. Computing Link Communities 
######################################################################

# Unweighted and weighted versions 

LM_lc  <- getLinkCommunities(LM_edges,  hcmethod="average") 
LM_wlc <- getLinkCommunities(LM_wedges, hcmethod="average")

# ***** How do we interpret these plots? 

# They get useless with large graphs: turn off with plot=FALSE. If
# needed you can regenerate with plot(LM_lc, type = "summary")

# Quick overview of results ("summary" not as useful): 

LM_lc
LM_wlc

# What's in the object? 

names(LM_wlc) 

# Let's return to the Help window to read the Value section explaining
# these names.

?getLinkCommunities 

# numbers is |E|, |V| and number of clusters (link communities)

LM_wlc$numbers

# We investigate some of the others below.

######################################################################
# 3. Inspecting Link Communities 
######################################################################
# We will generally be concerned with induced node communities (next
# section), but let's take a quick look at the link communities. From
# now on we work mostly with the weighted version, LM_wlc. Refer to
# ?getLinkCommunities as needed ...

# List of edges that are in each link community

length(LM_wlc$clusters) # same result as above 
head(LM_wlc$clusters, 3)

# Link community membership of edges 

head(LM_wlc$edges, 20)

# ***** Check your understanding: what is the link ID of the link 
#       represented by row 16 of LM_wlc$edges? 

# ***** Can a link ID appear on more than one list in LM_wlc$clusters? 

# ***** Can a pair of nodes appear on more than one row in LM_wlc$edges? 

# ***** Can a node appear on more than one row in LM_wlc$nodeclusters?
#       See documentation and sample below.

head(LM_wlc$nodeclusters, 15)

# linkcomm objects have their own plotting methods, dispatched from: 

?plot.linkcomm

# See plotLinkCommGraph and others at bottom. 
?plotLinkCommGraph 

# Display the network with edges coloured according to community
# membership, and optionally nodes as membership pie charts. Note:
# layout names have been updated; documentation is behind. Also may
# need to run first one twice; previous plot did not clean up par.

plot(LM_wlc, type = "graph", layout=layout_with_kk, node.pies=FALSE)
plot(LM_wlc, type = "graph", layout=layout_with_kk, node.pies=TRUE)

# By default only classified edges are shown. We can show all,
# with the unclassified edges being black:

plot(LM_wlc, type = "graph", layout=layout_with_kk, showall=TRUE)

# A Spencer Circle arranges the nodes by membership in a circle

plot(LM_wlc, type = "graph", layout="spencer.circle")

# ***** Lets interpret that 

# Open question: How to make their plotter label vertices with $label?

# ***** DS: Start a new video here 

######################################################################
# 4. Inspecting Induced Overlapping Node Communities 
######################################################################
# Now we take a node-centric look. The whole point is that nodes can
# participate in many link communities. As we saw above: 

head(LM_wlc$nodeclusters, 20)

########################################
# Node membership of communities. 

?getNodesIn

getNodesIn(LM_wlc, clusterids = 1)
getNodesIn(LM_wlc, clusterids = 2) # ... etc. 

# Note correspondence to above.

# Those were names. We can also get indices: 

getNodesIn(LM_wlc, clusterids = 1, type="indices")

?getLinkCommunities # look at clustsizes and numclusters

# clustsizes: Number of nodes in each community 

head(LM_wlc$clustsizes, 15)

# ***** How do you interpret the numbers in that table in general? 

# ***** Give an example of correspondence to previous output. 
#       (Hint: cluster 1)

########################################
# Communities to which each node belongs 

# Number of communities

head(LM_wlc$numclusters, 15)

# ***** How do you interpret the numbers in that table in general? 

# Print the $labels of the persons who are in the most clusters.

head(LM_wlc$numclusters)                    # Table is already sorted 
head(as.integer(names(LM_wlc$numclusters))) # We want the first row
V(LM)[head(as.integer(names(LM_wlc$numclusters)))]$label # Convert to $label 

# ***** If you are familiar with Les Mis, do these seem correct? 

# We can get the nodes in a community with getNodesIn, but how to 
# get the communities a node is in? Try this: 

head(LM_wlc$nodeclusters) # suggests that we can do:
which(LM_wlc$nodeclusters$node == 6)

# ***** From previous results we expected that node 6 was in 
#       cluster 2 and 3 (among others). What went wrong? 

LM_wlc$nodeclusters[13:15,]

# It returned indices of positions in nodeclusters, not clusters! 
# We need to use those indices to select the $cluster values: 

LM_wlc$nodeclusters$cluster[ which(LM_wlc$nodeclusters$node == 6) ]

# I'm going to make a function for that

getNodeCommunities <- function(lc, id) {
	as.numeric(lc$nodeclusters$cluster[which(lc$nodeclusters$node == id)])
}
getNodeCommunities(LM_wlc, 6)

# Let's take a look at the overlapping node communities Valjean is in. 
# His id: 

which(V(LM)$label == "Valjean")

# IDs of communities he is in: 

(Valjean_comm <- getNodeCommunities(LM_wlc, 1))

# The labels of nodes that are in these communities: 

V(LM)[as.numeric(getNodesIn(LM_wlc, clusterids = 4))]$label
V(LM)[as.numeric(getNodesIn(LM_wlc, clusterids = 9))]$label
V(LM)[as.numeric(getNodesIn(LM_wlc, clusterids = 13))]$label 

# ***** Les Mis fans, why does this make sense? 

########################################
# Community Overlap 

# Who overlaps between all of Valjean's communities? 
# Here's how to find overlap between communities 

?get.shared.nodes

Valjean_comm # computed above
get.shared.nodes(LM_wlc, Valjean_comm)

# Who is the other person? 

V(LM)[as.numeric(get.shared.nodes(LM_wlc, Valjean_comm))]$label

# ***** Does that mean Marius is in every scene Valjean is in?
#       That Valjean is in every scene Marius is in? Neither? 
#       Explain the following result. 

getNodeCommunities(LM_wlc,1) # Valjean's 
getNodeCommunities(LM_wlc,3) # Marius' 

# Display community membership for the top nodes that belong to
# the most communities.

plot(LM_wlc, type = "members") 

# ***** How do you interpret the rows and columns? Where are
#       Valjean and Marius? The communities that they share? 

# See for how to control this plot

?plotLinkCommMembers

# **** New Video 

############################################################
# 5. Nested Communities 
############################################################
# Finding (and optionally displaying) subnetworks where the
# nodes of one link community are entirely nested within
# nodes of another link community. 

?getNestedHierarchies
?getAllNestedComm

# We could check each cluster one at a time, for example starting with
# cluster 1:

getNestedHierarchies(LM_wlc, clusid = 1, plot = TRUE, ids=TRUE)

# The plot shows a larger community centered on node 1, Valjean. 
# Marius (3) is also in there. 

# ***** Why did we find Valjean with clustid = 1, when the above
#       computations showed him in clusters 4, 9 and 13? Look 
#       carefully at the console and documentation.

# Hint: 

getNodesIn(LM_wlc, 1) # membership of cluster 1
getNodesIn(LM_wlc, 4) # membership of the cluster Valjean is in 

# and look at the plot! Cluster 1 is nested in Valjean's cluster 4. 

V(LM)$label[as.integer(getNodesIn(LM_wlc, 1))] # What's the scene? 

# ... we can continue checking one at a time ... 

getNestedHierarchies(LM_wlc, clusid = 2, plot = TRUE, ids=TRUE)
getNestedHierarchies(LM_wlc, clusid = 3, plot = TRUE, ids=TRUE)
getNestedHierarchies(LM_wlc, clusid = 4, plot = TRUE, ids=TRUE) 

# ... or rather than guessing, use getAllNestedCommm, which 
# iterates over all nested communities: 

LM.nestedcomm <- getAllNestedComm(LM_wlc, plot=TRUE)

# We get a named list mapping clusters to superclusters. 

LM.nestedcomm # 1 and 7 are nested in Valjean's 4. 
getNodesIn(LM_wlc, clusterids = 1)
getNodesIn(LM_wlc, clusterids = 7) 
getNodesIn(LM_wlc, clusterids = 4)

# ***** Compare these membership lists to the plots. Where is cluster
#       7? Where is Marius?

# Hence linkcomm is not just computing overlapping communities, it is
# also computing hierarchical nesting.

# Let's try extracting and plotting subgraphs ourselves 

cluster_1 <- induced_subgraph(LM, getNodesIn(LM_wlc, clusterids = 1))
cluster_1$name <- paste(cluster_1$name, "Cluster 1")
cluster_4 <- induced_subgraph(LM, getNodesIn(LM_wlc, clusterids = 4))
cluster_4$name <- paste(cluster_4$name, "Cluster 4")
cluster_7 <- induced_subgraph(LM, getNodesIn(LM_wlc, clusterids = 7))
cluster_7$name <- paste(cluster_7$name, "Cluster 7")

par(mar=c(2, 2, 2, 2)) # A prior method set this to 0 
plot(cluster_1, vertex.color=0, vertex.size=0, main="Cluster 1")
plot(cluster_7, vertex.color=0, vertex.size=0, main="Cluster 7")
plot(cluster_4, vertex.color=0, vertex.size=0, main="Cluster 4")

# ***** Compare the plots. Do they make sense? 

# 12-5-Link-Communities-to-Gephis.R will provide a better way to
# visualize link communities in Gephi that I have invented.
 
######################################################################
# Summary: This is a template for the essential steps from above. It
# does not include pedagogical "building up" or the interpretive
# follow-up. Use this as a guide when doing linkcomm analyses. 
######################################################################

# 0. Setup 
library(igraph)
library(linkcomm)
setwd("yourpath")

# 1. Preparing an Edge List Representation of the Network 
g <- read.graph("filename.type", format="type")
g_edges <- as_edgelist(g)
# if weighted 
g_edges <- cbind(g_edges, E(g)$weight)

# 2. Computing Link Communities 
# You may want to vary hcmethod, or plot for small graphs
g_lc <- getLinkCommunities(g_edges, hcmethod="average", plot=FALSE)

# 3. Inspecting Link Communities 
g_lc                       # for summary of results 
# We normally don't print these out: too big
head(g_lc$clusters)        # Edges in each link community
head(g_lc$edges)           # Link community membership of edges 
plot(g_lc, type = "graph") # for regular layout, or use: 
plot(g_lc, type = "graph", layout="spencer.circle")

# 4. Inspecting Induced Overlapping Node Communities 
head(g_lc$nodeclusters)          # Clusters each node is in 
getNodesIn(g_lc, clusterids = C) # Node membership of community C
head(g_lc$clustsizes, 20)        # # nodes in each community (sorted)
head(g_lc$numclusters, 20)       # # communities each node is in (sorted)
# Function to get the communities a node is in
getNodeCommunities <- function(lc, id) {
	as.numeric(lc$nodeclusters$cluster[which(lc$nodeclusters$node == id)])
}
getNodeCommunities(g_lc, N)      # Communities that node N is in
get.shared.nodes(g_lc, commlist) # Overlap between clusters in commlist
plot(g_lc, type = "members")     # Community membership for top nodes

# 5. Nested Communities 
# clusters/communities C is nested in
getNestedHierarchies(g_lc, clusid = C, plot = TRUE) 
# Find all nested communities 
getAllNestedComm(g_lc, plot=TRUE)

# 6. Community-based Measures of Centrality -- Future Demo 

######################################################################
# Activity: Do the above with TI-Chats-Week-of-060401.graphml! 
# Use 12-2-Link-Communities-Activity.R
######################################################################
# Pau 