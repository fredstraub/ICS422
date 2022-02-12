######################################################################
# 5-2 Degree Sequence based random models: 
# * Configuration Model 
# * Degree Preserving Rewiring 
# Modified from Statistical Analysis of Network Data with R, with
# additional example using Network Science collaboration graph. 
# 
# Dan Suthers, September 14, 2016
# Feb 1, 2018 DS: small explanatory changes; scaling to ecount. 
# Jul 24 2018 DS: Minor updates for current script style and clarity. 
# Sep 19 2019 DS: Cleaning up for 2019 class. 
# Feb  1 2020 DS: Small revisions for 2020 self study students. 
# Feb 11 2021 Revisions for ICS 422/622 Spring 2021
# Feb 10 2022 Minor revisions for ICS 422/622 Spring 2022. 
######################################################################
# If you have not done so already today ... 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours

# For clean plots 

source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

# Today's example

NS <- read_graph("Networks/netscience.graphml", format="graphml")

######################################################################
# Simple Introduction to Configuration Model 
######################################################################
# Elaboration on the SAND example of the configuration model, formerly 
# degree.sequence.game, but now renamed to sample_degseq

?sample_degseq # Read the Details: use "vl" when you can

# ****** Why doesn't the first work but the second does? 

sample_degseq(c(1,1,1,2))
sample_degseq(c(  1,1,2))

# Two configuration models for a single legal degree sequence ...

degs <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,5,6,9)
g1 <- sample_degseq(degs, method="vl")
g2 <- sample_degseq(degs, method="vl")

# Are they the same? 

# Same number of vertices and same number of edges, but not isomorphic 

c(vcount(g1), vcount(g2))
c(ecount(g1), ecount(g2))
degree(g1)
degree(g2)
all(degree(g1) == degree(g2)) # if too big for visual check 
isomorphic(g1, g2)

par(mfrow=c(1,2))
plot(g1, vertex.label=NA, main="sample g1")
plot(g2, vertex.label=NA, main="sample g2")
par(mfrow=c(1,1))

# Now for a more complex example with Network Science ...

######################################################################
# Configuration Model (Degree Sequence Model)
######################################################################
# This makes a graph 'from scratch' with a specified degree sequence. 

# There are 3 methods. We'll demonstrate all three methods with the
# Network Science graph so let's get the degree sequence. 

NS.degrees <- degree(NS)

# "Simple" method: it is the *method* that is simple ... 

NS.degseq1 <- sample_degseq(NS.degrees, method="simple")
summary(NS)
summary(NS.degseq1)

# ... but the *graph* is not simple in this case 

is_simple(NS.degseq1)

# We can simplify it (if you loaded tidyverse use igraph::simplify)

NS.simp <- simplify(NS.degseq1) 

# ... but now it has fewer edges

c(ecount(NS), ecount(NS.simp))

# The Viger-Latapy method is supposed to avoid this: 

NS.degseq2 <- sample_degseq(NS.degrees, method="vl")

# The reason for the error is the presence of 0 degree nodes:

length(V(NS)[NS.degrees == 0])

# The bogus error was reported to Csardi in Summer 2018 

# "vl" is intended to generate connected graphs, so won't model
# NS well, even if we delete the isolated nodes: 

components(NS)$no

# We'd have to run it on each component and then reassemble them.
# (We'd have to filter isolates as their degree is 0, and the
# resulting graph would not be random with respect to components.)

# ***** What are 0 degree nodes in the Network Science domain?
#       What are components in this domain? 

# This works but is not guaranteed to sample uniformly from the model 

NS.degseq3 <- sample_degseq(NS.degrees, method="simple.no.multiple")
is.simple(NS.degseq3)
c(ecount(NS), ecount(NS.degseq3))
NS.degseq <- NS.degseq3 # pick this one 
summary(NS.degseq)
NS.degseq$method # records its method 

# We will evaluate this against the real graph after making the
# degree-preserving randomization model below. 

######################################################################
# Degree-Preserving Random Rewiring
######################################################################
# This makes a copy of the original graph including the original node
# names, but rewires randomly, preserving the degree of each node. 

?rewire           # Requires we give a rewiring method
?keeping_degseq   # We will use this method for 'with'

# Want to iterate enough to be sure the result is random.
# Documentation is not clear on what an iteration is, so I did an
# experiment: I specified one iteration and examined the graph,
# verifying that only one pair of edges had been swapped.

# We should scale to size of the network. SAND scaled according to
# vcount, but since it is swapping edges I suggest scaling to ecount.
# Let's do 100 rewirings of each edge on average (but increase this
# factor, perhaps to 1000, for real work if you have time.)

ecount(NS) 
NS.rewired <- rewire(NS, with=keeping_degseq(niter=ecount(NS)*100))
NS.rewired$name <- paste(NS.rewired$name, "Rewired")
summary(NS)
summary(NS.rewired)

# Notice that the attributes are still there. This is a significant
# advantage of rewiring for some applications. 

######################################################################
# Comparing the results 
# This part of the demonstration is important for understanding how
# degree-sequence constrained random graphs are similar to natural 
# ones, and how they differ. 
######################################################################

# Are the degree distributions identical? 
all(degree_distribution(NS) == degree_distribution(NS.degseq))
all(degree_distribution(NS) == degree_distribution(NS.rewired))
all(degree_distribution(NS) == degree_distribution(NS.simp))
# Because when edges were deleted the maximum was reduced
max(degree(NS))
max(degree(NS.simp))
# so the distributions have different lengths 
length(degree_distribution(NS)) 
length(degree_distribution(NS.simp))

# Visually ... 

par(mfrow=c(2,2))
dd <- degree_domain(NS)
plot(dd, nonzero_degree_distribution(NS), 
     main="NetSci Original", xlab="degree", ylab="proportion")
plot(dd, nonzero_degree_distribution(NS.degseq), 
     main="NetSci Config", xlab="degree", ylab="proportion")
plot(dd, nonzero_degree_distribution(NS.rewired), 
     main="NetSci Rewired", xlab="degree", ylab="proportion")
plot(degree_domain(NS.simp), # Need to recompute due to different max 
     nonzero_degree_distribution(NS.simp), 
     main="NetSci Config Simp", xlab="degree", ylab="proportion")
par(mfrow=c(1,1))

# We will leave NS.simp out from now on 

# It will help to first look at connectivity, as it will help explain
# subsequent results. First let's predict connectivity. Revisiting
# Tuesday's activity, compute critical points for random graph regimes.

mean(degree(NS))
log(vcount(NS))

# ***** In what regime is a random graph for netscience? What
#       connectivity do you expect? Refer to Figure 3.7 of Barabasi.

# Distribution of clusters differs (and will explain subsequent results)

table(sapply(decompose(NS), vcount))
table(sapply(decompose(NS.degseq), vcount))
table(sapply(decompose(NS.rewired), vcount))

# ***** What domain process explanation can you give for the 
#       natural network having more components than random? 

# ***** Why does random rewiring reduce the number of components?

# Distance is very close 

mean_distance(NS)
mean_distance(NS.degseq)
mean_distance(NS.rewired)

# Diameter a little off but not bad 

diameter(NS)
diameter(NS.degseq)
diameter(NS.rewired)

# This suggests that the distances in NetSci are explained largely by
# the degree distribution, as when we hold DD constant but randomize
# the distances don't change much.

# Transitivity is much lower in random models

transitivity(NS, type="global")
transitivity(NS.degseq, type="global")
transitivity(NS.rewired, type="global")

# ***** What domain process explanation can you give for the
#       natural network having higher transitivity than random? 

# Transitivity is not modeled well. That motivates Watts-Strogatz, 
# next in 5-3-Small-World-Models.R. 

######################################################################
# Pau
