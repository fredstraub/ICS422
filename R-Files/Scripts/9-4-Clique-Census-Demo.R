######################################################################
# Clique Census
# Class demonstration
# Dan Suthers, Oct 16, 2016
# Revised March 25, 2018 (minor comment change)
# Note: A portion of this takes ~4 hours to run; run it in advance. 
#       See comments in Clique Census section 
#       Need to do library(igraph) after loading saved session. 
# Aug 26, 2018 DS: Minor updates for current script style and clarity
# October 24, 2019 DS: Updates for 2019 class. 
# Feb 29 2020 DS: Minor updates for self study students. 
# Mar 11 2021 DS: Updates for ICS 422/622 Spring 2021 
# Mar 10 2022 DS: Minor updates for ICS 422/622 Spring 2022
######################################################################
# If you have not done so already .. 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
 
# Undirected example

NS <- read_graph("Networks/netscience.graphml", format="graphml")
summary(NS)

# Directed example 

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")
summary(TI)

######################################################################
# Dyad and Triad Census for DIRECTED graphs (briefy)
######################################################################
# Dyad Census 

# Mutual, Asymetric or Null: applies only to directed graphs 

?dyad_census 
dyad_census(TI)

# To contextualize the numbers, how many pairs of vertices are
# possible? 

?choose # and lots of other useful functions too! 
n <- vcount(TI)
choose(n,2)

# That should sum to: 

dyad_census(TI)$mut + dyad_census(TI)$asym + dyad_census(TI)$null

###############
# Triad Census

?triad_census
triad_census(TI)

# How many triads of vertices are possible? 

choose(n, 3)
sum(triad_census(TI))

# But what about other structures, such as larger cliques? 

######################################################################
# Clique Census for UNDIRECTED graphs 
######################################################################

# Methods we will use (all are on the same documentation page)

?cliques         # finds all complete subgraphs in the graph 
?max_cliques     # finds maximal cliques (cannot be extended) 
?largest_cliques # finds all largest maximal cliques (not on list)

# Finding all cliques is NP-Hard, meaning it takes exponential time
# (there are potentially an exponential number of cliques in a graph). 
# Doing so with Network Science takes 2-4 hours. If you want to have 
# these results do it in advance of class:
# system.time(NS.cliques <- cliques(NS))[[3]]
# NS.clique.counts <- table(sapply(NS.cliques, length))

# I have already computed the cliques. To get them, load NS-Cliques.RData 

load("Networks/NS-Cliques.RData")
NS.clique.counts 

# Here is the result, broken up across two lines. 
#
#  1      2      3      4     5      6      7     8      9      10
#  1589   2742   3764   7159  17314  39906  78055 126140 167993 184759 
#
#  11     12      13     14     15      16     17      18     19     20 
#  167960 125970  77520  38760  15504   4845   1140    190    20     1

# Knowing |V| = 1589, |E| = 2742 helps interpret the results ... 
# The column labels are the sizes of cliques and the numbers below the
#  counts. So we see that NS has: 
# * 1589 cliques of size 1, which is the same as the number of vertices. 
# * 2742 cliques of size 2, which is the number of edges. 
# * 3764 cliques of size 3, the number of triangles. 
# * Then we have the counts for cliques of size 4, 5, etc. 
# The numbers in the midrange are rather surprising: are there really 
# 184759 cliques of size 10?  

# Realize that these are subsets of the larger cliques in the graph. 
# We can see that there is a clique of size 20 authors who have all
# published with all of each others. *** At this point, we should go
# into Gephi and find that group. There are actually 21 in the group, 
# but one of the group has only published with 3 others. You probably 
# noticed that these 3 get the top eigenvector centrality using Gephi's
# method: this is no accident.

# Within this clique we must have 20 cliques of size 19, since we get
# such a clique by leaving out any of the 20. Then, the number of
# sub-cliques of size 18 is the number of ways we can choose 2 items
# out of 20 (the two vertices to leave out). In general, from group of
# 20 we get this many cliques of smaller sizes:

for (k in 19:10) {
  print(paste("20 choose", k, ":", choose(20, k)), quote=FALSE)
}

NS.clique.counts # ***** Compare the results to this

# Notice that the higher values from 20 down match the number of
# cliques found exactly. It is not until we get to cliques of size 10
# that the number of cliques found exceeds those expected simply as
# subsets of the group of 20: there are 184759 - 184756 = 3 more
# cliques of size 10 unaccounted for. These must be independent of the
# 20.

# Let's plot the distribution of cliques of each size.

plot(NS.clique.counts, lwd=4, 
     main="All Complete Subgraphs", ylab="Clique Counts")

# Why does data from a natural network look like the binomial expansion? 

# Consider the contribution of the group of 20 to this distribution.
# We have choose(20,20) cliques of size 20. Then choose(20,19) cliques
# of size 19 (focusing on how many we include), but this is the same
# as choose(20,1) (focusing on how many we leave out), which is the
# same as the number of cliques of size 1. Similarly, choose(20,18) ==
# choose(20,2) cliques of size 18, which is also the number of cliques
# of size two.

# It is a fact of combinatorics that there is a symmetry: 

choose(20,0)
choose(20,20)

choose(20,1)
choose(20,19) 

choose(20,2)
choose(20,18)

choose(20,3)
choose(20,17)

# This binomial expansion from the clique of 20 dominates the
# distribution of (non-maximal) cliques. The counts are inflated by
# having non-maximal cliques.

# It is probably more informative to find only the max-cliques, ones
# that are not parts of larger cliques:

NS.maxcliques <- max_cliques(NS)
table(sapply(NS.maxcliques, length))

#   1   2   3   4    5   6    7   8   9  10  20 
#   128 221 195 108  52  19   3   8   3   3   1 

# Now the pattern is much clearer. There is one clique of size 20, and
# all the cliques of sizes 11-19 reported above were just subsets of
# this. It is not until we reach size 10 that 3 more are found that
# are independent of the group of 20, as we predicted.

# This census of max-cliques also tells us that there are 128 isolated
# vertices, 221 pairs where one only publishes with the other
# (isolated pairs or pendants), 195 triads that don't all share a 4th
# author, etc.

# Compare the distributions (attend to the y axis scale): 

plot(table(sapply(NS.maxcliques, length)), lwd=4, 
     main="Maximal Cliques", ylab="Clique Counts")

# The max-clique distribution is more informative, as it is not
# dominated by mathematically necessary binomial distribution. (Notice
# the small uptick on the left side of the previous non-maximal plot:
# that is the contribution of the maximal cliques in this plot.)

######################################################################
# Extracting Subgraphs 
# Suppose we want to extract these cliques for examination ... 
######################################################################
# Largest Maximum Clique 

# We can ask for only the largest of the maximum cliques (though
# largest_cliques is not in the list at the top of the documentation).
# As expected from the above census, there will be only one.

NS.largestcliques <- largest_cliques(NS)

# It's a list of one ..  
NS.largestcliques
# so we use [[...]] to get the actual list of vertices  
NS.largestcliques[[1]]

# We can turn that list of vertices into a subgraph with 

?induced_subgraph  # ('subgraph' is deprecated)

NS.lc <- induced_subgraph(NS, NS.largestcliques[[1]])
NS.lc$name <- "Largest Clique in NS"
summary(NS.lc)
plot(NS.lc, main="Largest Clique in NS")

###########################
# Giant Connected Component 

# Suppose we wanted to focus on the giant connected component. 
# Here is how to extract the largest component of a graph.

?components 
?which.max 
?which

# Piecing it together: 
c <- components(NS)
# Right hand returns ID of largest cluster; then test membership 
max_c <- which(c$membership == which.max(c$csize))
length(max_c)

# Let's make a function that will extract the giant component as a
# graph.

giant_component <- function(g) {
  c <- components(g)
  induced_subgraph(g, which(c$membership == which.max(c$csize)))
}

# Note: which.max returns the first maximum. If there are two 
#       of identical size, we miss the other one. 

# Make a graph of the largest component 

NS.giant <- giant_component(NS)
NS.giant$name <- "Giant Component of NS"
summary(NS.giant)

# Try to visualize it with our authors highlighted. 

plot(NS.giant, main="Giant Component of NS", 
     layout=layout_with_kk,
     vertex.label=NA, vertex.size=5, 
     vertex.color=(V(NS)$label=="BARABASI, A" 
                   | V(NS)$label=="NEWMAN, M"))

# This is easy in Gephi: 
# The general approach: use filter Attribute/Partition/Component ID 
# and choose the ID of the largest component. 
# To get the giant component: filter Topology/Giant Component 
# ***** Let's do it.

# Note in Data Laboratory we can then inspect high centrality nodes
# within the giant component only. 

######################################################################
# Activity: Clique Census 
# Use NS.giant, which was computed in the class demo (not the full NS!): 

# 1. Do the census of all cliques in this graph

# 2. Interpret that: how many vertices, dyads, triangles, quads ...
#    and what is the size of the largest clique? 

# 3. Do the max_cliques census

# 4. Interpret that: How many Isolates are there? Isolated pairs or
#    pendants? Isolated or pendant Triangles? ...

# 5. Extract the largest maximal clique from the giant component as 
#    a separate graph and plot

# 6. What is the significance of this largest clique in the giant 
#    component compared to the largest clique in the graph as a whole? 

######################################################################
# Pau 

