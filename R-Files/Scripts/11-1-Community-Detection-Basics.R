######################################################################
# Community Detection Demo: Basics 
# Dan Suthers, March 21, 2017
# 
# Edit March 21, 2018 after class: moved exploration of edge weights
# and rewiring to test dependence on degree sequence to 11-3, 
# as we were short on time. Also cleaned up comments. 
#
# Edit March 23, 2018: Made a plotting function and clearer variables. 
#
# Sep 17, 2018 DS: 
# * Minor updates for current script style and clarity
# * Also removed cluster_optimal, as GLPK is no longer available. 
# 
# Tue Nov  5 2019 DS: Updating for 2019 class. (Last edit before class)
# Mon Mar 16 2020 DS: Minor updates for spring 2020 self study students
# Tue Mar 30 2021 DS: Minor updates for 422/622 spring 2021. 
# Tue Mar 29 2022 DS: Minor updates for 422/622 spring 2022. 
#
######################################################################
# Setup

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 

###########
# Networks 

# Undirected example

NS <- read_graph("Networks/netscience.graphml", format="graphml")
summary(NS)

# Directed example (will be activity)

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")
summary(TI)

######################################################################
# Communities objects 

# Community object holds membership info. 
# Scroll to end for methods of finding communities. 

?communities

######################################################################
# Quick introduction on small graph (Zachary)
######################################################################

K <- make_graph("Zachary")
summary(K)

# -------------------------------
# Optimal Modularity Partitioning 
# Gives the best possible partitioning under modularity but is NP-Hard
# This is the only network we will attempt this on!

?cluster_optimal
K.opt <- cluster_optimal(K)
names(K.opt) # to see what information it returns

# Every community object will have a membership vector

membership(K.opt)

# We can ask for modularity of the community object, and also can
# ask for modularity of a partition we provide (here, the same one). 

modularity(K.opt)
modularity(K, membership(K.opt)) # same result 

# -------------------------------
# Plotting community partitions 

# Will used a fixed layout for comparisons 

K.layout <- layout_with_fr(K)

# We could do this ... 
# plot(K, layout=K.layout, vertex.color=membership(K.opt), ...) 
# ... but communities has its own specialized layout procedure
# polymorphic on plot. Let's make that a "throwaway" function just for
# today using the above layout and showing modularity

plot_karate <- function(comm, title){
	plot(comm, K, layout=K.layout, 
	     main=paste0(title, 
	                 ": M = ", round(modularity(comm), 4),
	                 ", # = ", length(comm)))
}

plot_karate(K.opt, "Optimal")

# The colors indicate community membership, but be careful: the 
# background sometimes encloses a node of a different color. 

# ------------------------------------------------
# Why do we need other methods if we have optimal?
#
# Finding an optimal modularity partition is NP-Hard, so cannot be run
# on large graphs unless you have a really big computer and a lot of
# time. You may need to move to a different star system while it's
# running as the sun may swallow the Earth before it's done! An
# estimate of runtime:

#   "The number of ways a set of n elements can be partitioned into
#    nonempty subsets is called a Bell number and is denoted Bn"
#    (http://mathworld.wolfram.com/BellNumber.html)
#    B_n = Sum_(k=0,n)[Bk*choose(n-1,k)]

# Suppose we want to do it on Network Science: 

vcount(NS)

# so B_1589 = Sum_(k=0,1589)[Bk*choose(1588,k)]
# By comparison, a MUCH SMALLER number is the number of subsets of N

2^vcount(NS)

# Even the smaller TI won't finish: 

2^vcount(TI)

# Optimal is impossible for nontrivial networks, so we look at methods
# that approximate optimal

# ----------------------------------
# Edge Betweenness (Newman & Girvan)

# A classic divisive algorithm that uses betweenness to decide what
# edge to remove. Uses modularity to decide where to cut the dendrogram.

?cluster_edge_betweenness
K.eb <- cluster_edge_betweenness(K)

# This is a hierarchical method that provides more information

is_hierarchical(K.eb)
names(K.eb)

plot_karate(K.eb, "Edge Betweenness")

# Compare to optimal plot ... 

# -----------------------------------
# Fast Greedy Modularity Optimization

# Hierarchical agglomerative method that uses modularity to decide 
# what to merge and where to cut the dendrogram. 

?cluster_fast_greedy
K.fg <- cluster_fast_greedy(K)
names(K.fg)
plot_karate(K.fg, "Fast Greedy")

# -------------------
# Louvain and InfoMap 
# Two top performers on benchmarks we will discuss further below 

# Louvain: Hierarchical clustering based on modularity 

?cluster_louvain
K.louvain <- cluster_louvain(K)
names(K.louvain)

# The modularity function returns the best partition achieved ...

modularity(K.louvain)

# ... but because it is hierarchical there are other partitions: 

K.louvain$modularity

# Membership of chosen partition

K.louvain$membership

# memberships at all hierarchical levels

K.louvain$memberships

# Compare ... almost optimal: nodes 10, 31 and 9 differ

plot_karate(K.louvain, "Louvain")
plot_karate(K.opt, "Optimal") # To compare 

# InfoMap: Hierarchical clustering based on information theory 

?cluster_infomap
K.infomap <- cluster_infomap(K)
names(K.infomap)
K.infomap$codelength # what it is optimizing
plot_karate(K.infomap, "InfoMap")

# You might notice that on Karate, Louvain and Infomap differ in ways
# opposite of the resolution and horizon limit predictions.

# -------------
# Other Methods 
          
# Spinlass: Based on simulated annealing; can handle negative weights 

?cluster_spinglass
K.sg <- cluster_spinglass(K)
plot_karate(K.sg, "Spinglass") # ! 

# Walktrap: actually does random walk

?cluster_walktrap
K.wt <- cluster_walktrap(K)
plot_karate(K.wt, "Walktrap") 

######################################################################
# Network Science (a more realistic application)
######################################################################
# Review of stricter definitions (From 9-3-Clique-Census-Demo)

# Max Cliques: 
# Proposal: a community is a group of nodes all of whom are connected
# to each other. 

NS.maxcliques <- max_cliques(NS)
table(sapply(NS.maxcliques, length))

# ***** Gets the clique of 20, but what community structure is missing? 

# Components:
# Proposal: a community is a group of nodes where each pair is connected
# by some path, possibly via intermediate nodes. 

NS.components <- components(NS)
attributes(NS.components)
table(NS.components$csize) 

# ***** Did it get the missing structure? 

# Remember we can tell modularity what partition to use

modularity(NS, NS.components$membership) 

# ***** High modularity, and includes the giant component and the 
#       group of 21, but why is this insufficient? 

# We need something in between these two ... 

######################################################################
# Louvain

# Developed at the University of Louvain in Belgium; hence the name. 
# Multilevel modularity partitioning; handles large graphs well. 
# A strong contender on benchmarks and in my experience. 

?cluster_louvain

NS.louvain <- cluster_louvain(NS)

# This shows us the multilevel partitioning but fills your screen
# NS.louvain$memberships
# Membership of the maximum modularity level 
# NS.louvain$membership

# Modularities for each of the 4 levels of partitioning

NS.louvain$modularity

# Modularity of the maximum level (approximating optimal).

modularity(NS, NS.components$membership) # for comparison
modularity(NS.louvain)

# So Louvain performs better than finding connected components 

# Number of communities:  

length(NS.louvain) 

# Table of sizes of distribution: Compare to components

table(sizes(NS.louvain))
table(NS.components$csize) 

# ***** Many lower values are the same. What happened to the group of
#       21? To the giant component?

# Let's assign the membership to the graph to inspect later.

V(NS)$comm_louvain <- membership(NS.louvain)
summary(NS)

# Note: we will explore edge weights in the next class. 

######################################################################
# InfoMap clustering of Network Science 

# Using Information theoretic metric L, treats the community partition
# as a Map (hence InfoMap), and heuristically optimizes the map to 
# minimize the length of a hypothetical encoding of a random walk. 
# Otherwise uses the same hierarchical algorithm as Louvain. A strong
# contender on some benchmarks. 
# We can give VERTEX weights for probability of random jump as well as 
# edge weights for probability of transition. 

?cluster_infomap

NS.infomap <- cluster_infomap(NS)

# It optimizes L, not modularity, so modularity will be a little lower 

modularity(NS.infomap) # InfoMap
modularity(NS.louvain) # Louvain

length(NS.infomap) # InfoMap
length(NS.louvain) # Louvain 

# ***** Why is the second one lower? (Hint: limitations of algorithms
#       discussed in readings.)

V(NS)$comm_infomap <- membership(NS.infomap)
summary(NS)

########################################
# Try a few others and save them.

NS.spinglass <- cluster_spinglass(NS) # this did great on karate! 

NS.between <- cluster_edge_betweenness(NS) # recall that this is slower

# Nice warning! See 

?cluster_edge_betweenness

# Note NULL not NA to get rid of weights. 

NS.between <- cluster_edge_betweenness(NS, weights=NULL)
modularity(NS.between) 
modularity(NS.louvain) # to compare 
V(NS)$comm_between <- membership(NS.between)

NS.walk <- cluster_walktrap(NS)
modularity(NS.walk) 
V(NS)$comm_walk <- membership(NS.walk)

######################################################################
# Examine in Gephi 

write_graph(NS, "Network-Science-Communities.graphml", format="graphml")

# ***** Go to Gephi and load this graph ***** 
# * Compute "Modularity" (Louvain method) and Connected Components 
# * Compare the results to igraph by coloring and in Data Laboratory
#   - What kinds of components do they differ on?  
#   - Do you see the resolution limit? 

######################################################################
# Activity: Community Analysis of TI Chat Sociogram 
######################################################################
# Please go to 11-2-TI-Chat-Activity.R 

######################################################################
# Pau