######################################################################
# Centralities based on Degree 
# A brief introduction to how to compute them in igraph. 
# Dan Suthers, Oct 3, 2016
# 
# Updates: 
# Aug 20 2018 DS: Minor updates for current script style and clarity
# Oct  8 2019 DS: Reducing nonessentials for 2019 class. 
# Feb 22 2020 DS: Cleaning up and additional comments for self 
#                 study students.
# Mar  2 2021 DS: Minor updates for ICS 422/622 Spring 2021
#  - using nonzero_degree_distribution.R instead of Herman's function
# Mar  1 2022 DS: Minor updates for ICS 422/622 Spring 2022

# REMINDER: The best way for you to learn is to experiment with R
# and igraph yourself. If you just execute this script without 
# understanding it, you won't learn as much. 

# This script contains many steps that are present for expository 
# purposes only, and are not needed in an actual analysis. You
# should try to understand this script well enough to tell expository 
# and essential steps apart. When you do your homework, show me only
# the essential parts!  (You can probably get what you need in about
# 8 lines of code! Can you figure out what they are?) 

######################################################################
# Setup 

library(igraph)
library(tibble) 

setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 

source("Utility/new_window.R")
source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

######################################################################
# Load the graph we will work with (I'll explain the context)

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", 
                 format="graphml")
summary(TI)

######################################################################
# Degree 
######################################################################

# We are already familiar with degree when working with degree 
# distributions, but now we consider it from the standpont of 
# individual nodes. 

?degree

# We can specify direction. 

head(degree(TI, mode="in"))
head(degree(TI, mode="out"))

# We can normalize, though I don't use this ... 

head(degree(TI, mode="in", normalized=TRUE))

# Degree as an integer count has a specific interpretation in my
# domain: number of others whom have taken up ego's chats (in-degree)
# or whose chats ego has taken up (out-degree).  Therefore I'll leave
# it un-normalized.

# ***** What would normalization enable you to do? Read the
#       documentation.

# It is often useful to store degree info on vertices. 

V(TI)$degree    <- degree(TI, mode="all")
V(TI)$indegree  <- degree(TI, mode="in")
V(TI)$outdegree <- degree(TI, mode="out")
summary(TI)

##############################
# How do we inspect this data?

# Plot in vs out - moderate difference 

plot(degree_domain(TI, mode="in"), 
     nonzero_degree_distribution(TI, mode="in"), 
     main="TI Chat Actors In-DD", xlab="k", ylab="p(k)")
plot(degree_domain(TI, mode="out"), 
     nonzero_degree_distribution(TI, mode="out"), 
     main="TI Chat Actors Out-DD", xlab="k", ylab="p(k)")

# ***** What *individual* (node level) centrality phenomena do
#       these plots suggest that you look for?

##############################
# Looking at individual nodes. 
# While igraph is good for operating on the whole graph, Gephi is better
# for diving in and looking at individual nodes. We'll do that later, 
# but here are ways to get the top nodes in igraph. 

# Find the maximum

max(V(TI)$indegree)
max(V(TI)$outdegree)

# Who has these values: 

V(TI)$label[V(TI)$indegree == max(V(TI)$indegree)]
V(TI)$label[V(TI)$outdegree == max(V(TI)$outdegree)]



# Finding Top N on some metric 
# Sort the vertices in decreasing order by indegree and return the
# top 10 (indegree indicates actors who others take up):

?order
V(TI)[order(V(TI)$indegree, decreasing=TRUE)[1:10]]

# But we want to see the labels, not just the IDs 

V(TI)[order(V(TI)$indegree, decreasing=TRUE)[1:10]]$label

# Let's make a function for the top N vertices: "top n v". This will
# let us compare the top n under other centrality metrics.

topnv <- function(graph, values, n=10) {
	return(V(graph)[order(values, decreasing=TRUE)[1:n]])
}

topnv(TI, V(TI)$indegree)$label
topnv(TI, V(TI)$outdegree)$label

######################################################################
# Weighted Degree 
######################################################################
# We are familiar with this too ... 

?strength

# Our graph has a 'weight' edge attribute, so we don't need 
# to give it as an argument. 

summary(TI)

# Again we could normalize, but in TI Chats the integer counts have
# specific domain interpretations: number of uptakes per interlocutor. 

V(TI)$strength  <- strength(TI, mode="all")
V(TI)$windegree <- strength(TI, mode="in")
V(TI)$woutdegree <- strength(TI, mode="out")
summary(TI)
head(V(TI)$windegree)
head(V(TI)$woutdegree)

# Compare unweighted to weighted top N vertices 

tibble(
  indegree = topnv(TI, V(TI)$indegree)$label,
  windegree = topnv(TI, V(TI)$windegree)$label,
  outdegree = topnv(TI, V(TI)$outdegree)$label,
  woutdegree = topnv(TI, V(TI)$woutdegree)$label
)

# ***** What does this say about the activity of BjB and DavidWe in 
#       comparison to each other? 

######################################################################
# Eigenvector Centrality 
######################################################################

?eigen_centrality

# Note that weights can be used. "This function interprets weights as
# connection strength. Higher weights spread the centrality better."

# ***** How does that compare to distance? 

# Can compute directed and undirected. 

dec <- eigen_centrality(TI, directed=TRUE)

# What was that warning? (Gephi doesn't have it!)
# See "WARNING" in the documentation: a directed matrix is not symmetric. 

uec <- eigen_centrality(TI, directed=FALSE)

# The returned object has various parts

names(uec)

# The eigenvalue 

uec$value

# The eigenvector, which contains the centralities 

head(uec$vector, 20)

# These options are from the ARPACK library for calculating
# eigenvectors of sparse matrices

names(uec$options)

?arpack # igraph uses this; Gephi uses the power method 

# We use the vector portion. Save both for later use. 

V(TI)$dir_eigen_cent <- dec$vector
V(TI)$und_eigen_cent <- uec$vector
head(V(TI)$dir_eigen_cent)
head(V(TI)$und_eigen_cent)

# It is scaled to 1 max by default (see 'scale' in ?eigen_centrality)

max(V(TI)$dir_eigen_cent) 
max(V(TI)$und_eigen_cent) 

# Let's compare the distributions of degree, strength and eigenvector
# centrality (which uses weights by default), focusing on undirected
# for comparison.

hist(V(TI)$degree, breaks=64) 
hist(V(TI)$strength, breaks=64) 
hist(V(TI)$und_eigen_cent, breaks=64) 

# ***** Which will discriminate a broader range of nodes? 

# What about the top 10? Edge weights are used by default, so let's 
# compare the results to strength as well as degree: 

tibble(
  degree = topnv(TI, V(TI)$degree)$label, 
  strength = topnv(TI, V(TI)$strength)$label, 
  und_eigen = topnv(TI, V(TI)$und_eigen_cent)$label
)

# ***** Which actors seem to benefit from proximity to central actors? 

#################
# Katz centrality 
# igraph and Gephi do not have Katz centrality, but we can see 
# the problem with eigenvector centrality that motivates Katz
# as follows. Test with a star graph of 9 sources and 1 sink 

?make_star
S <- make_star(10, mode="in") 

# Let's plot this in a separate window, leaving the current untouched 

w <- dev.cur()
new_window("Star", 6, 6)
plot(S, vertex.size=25)
dev.set(which=w)

(V(S)$eigen_directed <- eigen_centrality(S, directed=TRUE)$vector)

# Yes that is what Newman predicted

(V(S)$eigen_undirected <- eigen_centrality(S, directed=FALSE)$vector)

# The top value is scaled to 1. The rest are the same. 
# Write this out for Gephi; we'll inspect later. 

write_graph(S, "Star-Network-10.graphml", format="graphml")

######################################################################
# Page Rank 
######################################################################
# Page rank is Katz centrality with adjustment for outdegree. 

?page_rank 

# Discuss algo (ARPACK / PRPACK and power method), and damping. 
# Follow link to http://infolab.stanford.edu/~backrub/google.html

# Like eigenvector, this gives various parts and we want the vector. 

V(TI)$page_rank <- page_rank(TI, directed=TRUE)$vector

# PageRank is scaled differently than Eigenvector 

max(V(TI)$page_rank) # Not 1, like Eigenvector  
sum(V(TI)$page_rank) # PageRank is like a probability (of random walk)

# Compare to other histograms ... 

hist(V(TI)$page_rank, breaks=64)

# ***** Does this let us discriminate more or less nodes than eigenvector?

# The pagerank adjustments give different results: 

tibble(
  strength = topnv(TI, V(TI)$strength)$label, 
  und_eigen = topnv(TI, V(TI)$und_eigen_cent)$label, 
  page_rank = topnv(TI, V(TI)$page_rank)$label
)

# ***** Can you explain Heather's demotion mathematically?

# ***** Comment on how PageRank might differ from weighted degree 

######################################################################
# HITS (Hubs and Authorities)
# Authority is based on in-degree and Hub on out-degree. 

?authority_score
?hub_score

# You can run this on undirected graphs, but the distinction is only
# useful for directed graphs (scores are the same for undirected). 

V(TI)$authority <- authority_score(TI)$vector
V(TI)$hub       <- hub_score(TI)$vector
summary(TI)

# Distributions 
hist(V(TI)$authority, breaks=64)
hist(V(TI)$hub, breaks=64)

# Tight distribution like eigenvector, but the value is in
# discriminating the two roles

# Comparing all of the eigenvector-based centralities: 

tibble(
  dir_eigen = topnv(TI, V(TI)$dir_eigen_cent)$label,
  page_rank = topnv(TI, V(TI)$page_rank)$label, 
  authority = topnv(TI, V(TI)$authority)$label, 
  hub = topnv(TI, V(TI)$hub)$label
)

# ***** How would you describe Heather's role given the above? 

######################################################################
# Viewing the results and Comparing to Gephi
######################################################################

# We could try to work up a visualization in igraph ... 

plot(TI, vertex.size=V(TI)$page_rank*1000, vertex.label=NA)

# ...but the best way is to write it out and view in Gephi. 

write_graph(TI, "TI-Chats-With-Metrics.graphml", format="graphml")

# We will explore this in Gephi, comparing to Gephi-computed versions. 
# Gephi has equivalents of degree, strength, eigenvector, pagerank and HITS. 

# ***** Compare Gephi's default eigenvector centrality to igraph. 
#      Can you explain the discrepancy? 

# ***** Does Gephi give the directed star network all 0's? 

######################################################################
# Summary: What are the ~8 lines of code? 
# 
# This is just a hint; you'll give the answers in 8-3-Centralities-Activity.
#
# 1. Read in the graph 
# 2. Compute and save as attribute on vertices: degree 
# 3. Compute and save as attribute on vertices: strength 
# 4. Compute and save as attribute on vertices: eigenvector centrality
# 5. Compute and save as attribute on vertices: page rank
# 6. Compute and save as attribute on vertices: hubs
# 7. Compute and save as attribute on vertices: authorities
# 8. Write out the graph so you can examine results in Gephi 
# 
# Of course, you can extend this with unweighted and weighted versions. 
# Also you can do both undirected and directed, but usually you are
# interested in only one option for a given domain.
#
######################################################################
# Pau 