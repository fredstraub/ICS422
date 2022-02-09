######################################################################
# 5-1 Random Graph Evolution
# Plotting the evolution of random networks with respect to "regimes"
# and phase transitions. See Barabasi section 3.6 and Figure 3.7.
# Dan Suthers, September 12, 2016; updated January 28, 2018
# Jul 24 2018 DS Minor updates for current script style and clarity. 
# Sep 17 2019 DS Minor revisions for class
# Feb  1 2020 DS Minor revisions for 2020 self study students. 
# Feb  9 2021 Revisions for ICS 422/622 Spring 2021
# Feb  8 2022 Minor revisions for ICS 422/622 Spring 2022. 
<<<<<<< HEAD
#   This demo is stable and in good shape. Can do in about 20 min 
#   excluding Gephi part. 
=======
# (This demo is stable and in good shape. Can do in less than 30 min.)
>>>>>>> ce55f0201bcf2fdf8a3e208aa1a19833611874ef
######################################################################

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 

# For clean plots 

source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

# No longer using in class demo 
# source("Utility/new_window.R")

######################################################################
# Basics
######################################################################
# Random graph models G(n,m) and G(n,p). Understand the difference:

# G(n,m): Given fixed |V| = n, randomly insert |E| = m edges 
# This is Barabasi's G(N,L). Note the default directed and loops. 

?sample_gnm 

# G(n,p); Given fixed |V| = n, for each pair {v1, v2} in V add an 
# edge between them with probability p. (We can choose p to have an 
# expected |E| = m from the G(n,m) model, but it may vary.)

?sample_gnp 

# Let's make a couple of random graphs to show they differ

gnp1 <- sample_gnp(50, 0.05)
gnp2 <- sample_gnp(50, 0.05)
summary(gnp1)
summary(gnp2)

# On average we expect 5% of n(n-1)/2 = 50*49/2 possible edges

0.05*(50*49)/2

plot(gnp1, main=(paste(gnp1$name, 1)), layout=layout.circle)
plot(gnp2, main=(paste(gnp2$name, 2)), layout=layout.circle)

# Even if we specify the number of edges the graphs may be different

gnm1 <- sample_gnm(50, 61)
gnm2 <- sample_gnm(50, 61)
summary(gnm1)
summary(gnm2)

# The summary looks the same but the connectivity is different: 

isomorphic(gnm1, gnm2)
plot(gnm1, main=(paste(gnm1$name, 1)), layout=layout.circle)
plot(gnm2, main=(paste(gnm2$name, 2)), layout=layout.circle)

######################################################################
# More systematic exploration of properties, following Figure 3.7
######################################################################
# Random Graph Regimes
# (Display and understand figure 3.7 from Barabasi before going on.)

# New window to plot comparisons. You may need to change size. 
# new_window("Evolution of Random Graph", 12, 12) 

# Make a 2x2 grid

par(mfrow=c(2,2))

# Sub-critical Regime: 0 < <k> < 1, so 2|E| < |V| 
# We will use |V| = 100.
# For <k> = 0.5, choose 25 edges, giving 50 endpoints for 100 vertices
# Should see only small trees and no giant component

gnm_100_25 <- sample_gnm(100, 25)
plot(gnm_100_25, vertex.label=NA, vertex.size=3, 
     main="Subcritical G(100,25)")

# Critical point: <k> = 1 so |E| = 1/2|V| = 50 
# Larger components begin to emerge

gnm_100_50 <- sample_gnm(100, 50)
plot(gnm_100_50, vertex.label=NA, vertex.size=3, 
     main="Critical G(100,50)")

# Super-critical regime: 1 < <k> < ln|V| = 4.605

log(100) # ln|V| 

# Setting |E| = 100 gives us <k> = 2, in the middle of the regime
# There should be a giant component and may be cycles 

gnm_100_100 <- sample_gnm(100, 100)
plot(gnm_100_100, vertex.label=NA, vertex.size=3, 
     main="Supercritical G(100,100)")

# Connected Regime: <k> >> ln N = 4.605. 
# Compute number of edges for <k> == ln N to see where it starts 

(100*log(100))/2 # Avg degree times # vertices divided by 2

# With 100 vertices (100*4.605)/2 = about 230 edges. 
# Let's set it to 400 for >>
# Expect a fully connected graph (though with small N it may not be)

(400*2)/100 # Average degree will be this 
gnm_100_400 <- sample_gnm(100, 400)
plot(gnm_100_400, vertex.label=NA, vertex.size=3, 
     main="Connected G(100,400)")

##############################
# Component distributions 
# Verify that the giant component appears as predicted by Barabasi's analysis. 

# Review of R and igraph used here as needed (decompose is new)

?igraph::decompose
<<<<<<< HEAD
head(decompose(gnm_100_50), 2) # It's a list of graphs 
?vcount  # for how many vertices in each graph 
?sapply  # to map the above across the list 
?table   # to make frequency table of sizes 
=======
?vcount
?sapply
?table
>>>>>>> ce55f0201bcf2fdf8a3e208aa1a19833611874ef

# Doing it: 

table(sapply(decompose(gnm_100_25), vcount))   # only tiny components?
table(sapply(decompose(gnm_100_50), vcount))   # larger components emerge?
table(sapply(decompose(gnm_100_100), vcount))  # has a giant component? 
table(sapply(decompose(gnm_100_400), vcount))  # fully connected?

##############################
# Mean distance 
# Random graphs model the small world phenomenon well 

mean_distance(gnm_100_25)
mean_distance(gnm_100_50)
mean_distance(gnm_100_100)
mean_distance(gnm_100_400)

diameter(gnm_100_25)
diameter(gnm_100_50)
diameter(gnm_100_100)
diameter(gnm_100_400)

# ***** Why did distances get LONGER as we add edges and then shorter?
#       Look at plot for hints, and if needed the documentation: 

?mean_distance

##############################
# Transitivity 
# However, random graphs have very low transitivity (global clustering), 
# and Newman Table 8.1 shows us that natural graphs have much higher
# transitivity (column labeled C). 

transitivity(gnm_100_25, type="global")
transitivity(gnm_100_50, type="global")
transitivity(gnm_100_100, type="global")
transitivity(gnm_100_400, type="global")

##############################
# Degree Distributions 
# Their degree distributions are Poisson-like (Since it's a small graph
# it is hard to see until we get to denser graphs.) 

# new_window("Degree Distributions", 9, 9) # Could run in same window 
<<<<<<< HEAD
# par(mfrow=c(2,2)) # still active if in same window 
=======
# par(mfrow=c(2,2))
>>>>>>> ce55f0201bcf2fdf8a3e208aa1a19833611874ef

plot(degree_domain(gnm_100_25), 
     nonzero_degree_distribution(gnm_100_25), 
     main="G(100,25)", xlab="degree", ylab="proportion")
plot(degree_domain(gnm_100_50), 
     nonzero_degree_distribution(gnm_100_50), 
     main="G(100,50)", xlab="degree", ylab="proportion")
plot(degree_domain(gnm_100_100), 
     nonzero_degree_distribution(gnm_100_100), 
     main="G(100,100)", xlab="degree", ylab="proportion")
plot(degree_domain(gnm_100_400), 
     nonzero_degree_distribution(gnm_100_400), 
     main="G(100,400)", xlab="degree", ylab="proportion")

# Can also plot histograms if you prefer

hist(degree(gnm_100_25), col="lightblue", 
     xlab="Degree", ylab="Frequency", main="G(100,25)")
hist(degree(gnm_100_50), col="lightblue", 
     xlab="Degree", ylab="Frequency", main="G(100,50)")
hist(degree(gnm_100_100), col="lightblue", 
     xlab="Degree", ylab="Frequency", main="G(100,100)")
hist(degree(gnm_100_400), col="lightblue", 
     xlab="Degree", ylab="Frequency", main="G(100,400)")

# Turn off the 2x2 grid 

par(mfrow=c(1,1))

######################################################################
# Summary of methods in this script 
######################################################################

# making G(n,p) and G(n,m) 

<<<<<<< HEAD
gnm <- sample_gnm(100, 500)  # If modeling known number of edges
gnp <- sample_gnp(100, 0.01) # We'll examine this one 

=======
gnp <- sample_gnp(100, 0.01)
gnm <- sample_gnm(100, 500)
>>>>>>> ce55f0201bcf2fdf8a3e208aa1a19833611874ef

# Initial inspection 

summary(gnp)

# new_window("Summary of G(n,p)", 12, 6)

plot(gnp, vertex.label=NA, vertex.size=3, main=gnp$name, 
     layout=layout_with_fr)
mean(degree(gnp))  # <k> so we can compare to 1 and next item 
log(vcount(gnp))   # ln N, the boundary for connected regime 

# Standard metrics 

table(sapply(decompose(gnp), vcount))  # components 
mean_distance(gnp)                     # mean geodesic
transitivity(gnp, type="global")       # clustering coefficient 
plot(degree_domain(gnp), 
     nonzero_degree_distribution(gnp))         # degree distribution 

######################################################################
# How to return to the R Studio plot window if you used new_window: 
# dev.list()
# dev.set(2)
# Rerun a plot to show it worked. 

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

# Predict regime (subcritical, critical, supercritical, connected)
# by comparing mean degree to 1 and natural log of |V| = N 

mean(degree(NS))
log(vcount(NS))

# ***** Which regime is its random model in? Answer in Google doc 

# Compare component distributions of natural to random model and see
# whether this fits the above prediction. If it does not, EXPLAIN in
# terms of the domain.

# Compare the two on distance, transitivity and degree distribution. 
# (Also compare transitivity to Gephi result and explain.)

# Then do the same for WA. Just Find and Replace NS with WA! 

######################################################################
# Pau 


