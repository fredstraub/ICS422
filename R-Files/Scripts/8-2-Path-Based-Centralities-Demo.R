######################################################################
# Centralities based on Paths 
# An introduction to how to compute them in igraph, and issues in
# choosing parameters. 
# Dan Suthers, Oct 5, 2016 
# 
# Aug 20, 2018 DS: 
# * Minor updates for current script style and clarity
# * Moved estimate_betweenness here from 9-1
# Oct  10 2019 DS: Updated for 2019. 
# Feb 22 2020 DS: Cleaning up and additional comments for self 
#                 study students.
# Mar  4 2021 DS: Updates for ICS 422/622 Spring 2021
# * Handled fact that TI Chat weights are not distances. 
# Mar  3 2022 DS: Minor updates for ICS 422/622 Spring 2022
# 
######################################################################
# Setup and establishing demo 8-1 context that we reuse 
# Do this section only if you have not run 8-1 in this session. 

library(igraph)
library(tibble)

setwd("/Users/fred/Github/ICS422/R-Files") # Set to yours 

source("Utility/new_window.R")
source("Utility/degree_domain.R")

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")
summary(TI)

# Utility function defined in the previous class 
topnv <- function(graph, values, n=10) {
	return(V(graph)[sort.list(values, decreasing=TRUE)[1:n]])
}

# Recompute the degree-based centralities from the previous class.
V(TI)$degree      <- degree(TI, mode="all")
V(TI)$indegree    <- degree(TI, mode="in")
V(TI)$outdegree   <- degree(TI, mode="out")
V(TI)$strength    <- strength(TI, mode="all")
V(TI)$windegree   <- strength(TI, mode="in")
V(TI)$woutdegree  <- strength(TI, mode="out")
V(TI)$und_eigen   <- eigen_centrality(TI, directed=FALSE)$vector
V(TI)$dir_eigen   <- eigen_centrality(TI, directed=TRUE)$vector
V(TI)$page_rank   <- page_rank(TI)$vector
V(TI)$authority   <- authority_score(TI)$vector
V(TI)$hub         <- hub_score(TI)$vector

######################################################################
# Betweenness
######################################################################

?betweenness

# We can specify directed(default)/undirected, weights (it uses them
# by default), and whether normalized. The normalization denominator
# shows us how big unnormalized betweenness can be: 

n <- vcount(TI)
choose(n-1, 2)     # undirected normalizing denominator 
((n-1) * (n-2))/2  # Version in ?betweenness documentation is equivalent
2 * choose(n-1, 2) # directed normalizing denominator 

# ***** Why do we expect betweenness to be a slow computation? 

# ***** How much would v (or vids) help speed it up? 

# estimate_betweenness: We can set cutoff for large graphs: path
# length searched is limited. (Demonstrated below.) 

# edge betweenness: used for one method of community detection.

# ***** How would you define edge betweenness *conceptually*?

#################
# Weights, Part 1

# ***** What do weights mean in the TI Chat sociogram? Reminder: 

tibble (
        weight = head(E(TI)$weight), 
        LEX    = head(E(TI)$LEX), 
        ADR    = head(E(TI)$ADR), 
        RPLY   = head(E(TI)$RPLY), 
        SA     = head(E(TI)$SA),  
        PE     = head(E(TI)$PE)
        )

# ***** How does the betweenness metric interpret weights? 

# "Weights are used to calculate weighted shortest paths, so they are
# interpreted as distances"

# ***** Is this appropriate? 

########## 
# We'll simplify things, avoiding weights to give a purely structural
# metric, but compute both unnormalized and normalized. Keep the
# default of directed as we are concerned with information flow. (This
# is an example of how one must think carefully about what one is
# modeling with a graph before choosing appropriate metrics.)

V(TI)$betweenness   <- betweenness(TI, weights=NA)
V(TI)$betweenness_n <- betweenness(TI, weights=NA, normalized=TRUE)

# Let's look at its characteristics. 
# Unnormalized values can be very large; normalized values are small. 

max(V(TI)$betweenness)     # compare to 2 * choose(n-1, 2) value above
max(V(TI)$betweenness_n)   # interpretation? 

# Compare ranking to some of our prior metrics, removing weights for
# apples-to-apples comparison. (Normalization does not change rank, so
# we leave that out.)

tibble(
        indegree  = topnv(TI, V(TI)$indegree)$label,
        page_rank = topnv(TI, page_rank(TI, weights=NA)$vector)$label,
        between   = topnv(TI, V(TI)$betweenness)$label
)

# After the top several, a new set of actors shows up! 
# ***** What do you think those new actors are doing that makes them
#        prominent under betweenness even though they don't rank high 
#        under degree based centralities?

# Comparing the dynamic range (with weights off for now)

hist(V(TI)$indegree, breaks=64)
hist(eigen_centrality(TI, weights=NA)$vector, breaks=64)
hist(page_rank(TI, weights=NA)$vector, breaks=64)
hist(V(TI)$betweenness, breaks=64) # only changes x axis 
hist(V(TI)$betweenness_n, breaks=64) # only changes x axis 

# Betweenness does not discriminate the 'little guys' well. It does
# pick out a few exceptional individuals who play an important role! 

######################################################################
# Estimated betweenness 

# ?estimate_betweenness is on the same documentation page 
# ****** Cannot normalize estimated_betweenness: Why?

# The TI graph is too small for this to be useful. Estimated
# betweenness is only needed for very large graphs in which the
# computation of all-pairs shortest paths would be slow.

# Networks/internet_routers-22july06.gml is big enough to show the
# effect (although still not that large), but too slow for a class
# demo. I will run 8-3-IR-Estimate-Betweenness.R in the background to
# show how the estimates and runtimes compare to actual for different
# cutoffs, including plots and correlations: 

?system.time # to track execution time 
?cor.test    # to find correlations between estimate and actual 

# Using Kendall tau as histogram shows values are definitely not a
# bivariate normal distribution

# Meanwhile, let's look at how the estimate improves with distance
# using a graph with familiar node names:

NS <- read_graph("Networks/netscience.graphml", format="graphml")

# Diameter gives us maximum cutoff 

diameter(NS, weights=NA) # ignoring weights 

# We can see the rank ordering progress towards the actual, with some
# winners emerging even with low cutoff ...

view(tibble(
        actual = topnv(NS, betweenness(NS, weights=NA))$label,
        cutoff10 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=10))$label,
        cutoff9 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=9))$label,
        cutoff8 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=8))$label,
        cutoff7 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=7))$label,
        cutoff6 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=6))$label,
        cutoff5 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=5))$label,
        cutoff4 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=4))$label,
        cutoff3 = topnv(NS, estimate_betweenness(NS, weights=NA, cutoff=3))$label
        ))

# **** What can you conclude about Newman based on this progression? 
#      Barabasi? And Moreno? 
# **** How does this relate to the concepts of "local bridges" and
#      "structural holes"?

# If it was running in the background take a look at the results of
# 8-3-IR-Estimate-Betweenness.R now.

# However, a challenge is choosing the cutoff. We can't base it on
# experiments like the above, or even diameter or mean_distance, as
# these require the all-pairs shortest-paths computations we are
# trying to avoid! estimate_betweenness may be most useful in
# applications requiring repeated computation as the graph is
# modified.

######################################################################
# Closeness
######################################################################

?closeness

# Similar parameters, except that for directed graphs we can specify
# "mode" for in-directed, out-directed or all to treat as undirected.
# Weights are included by default (see below). There is no edge
# closeness. We can estimate_closeness.

# We'll leave weights out at first. 

V(TI)$closeness <- closeness(TI, weights=NA)
V(TI)$closeness_n <- closeness(TI, weights=NA, normalized=TRUE)

# If you read Newman, there is a reasonable way to handle disconnected
# graphs.

max(V(TI)$closeness) # Small values because inverse of large ones
max(V(TI)$closeness_n)

# Newman points out that closeness has limited range because distances
# grow only as the log of the number of vertices. We will see this in
# Gephi when sizing nodes by their centralities. Also it is biased to
# members of small components, as everyone is close. We will see this
# shortly.  The histogram is strikingly different:

hist(V(TI)$closeness, breaks=64)   # You can guess the tiny ones
hist(V(TI)$closeness_n, breaks=64) # Same histogram 

# Compare ranking to some of our prior metrics 

tibble(
        indegree = topnv(TI, V(TI)$indegree)$label,
        page_rank = topnv(TI, V(TI)$page_rank)$label,
        between   = topnv(TI, V(TI)$betweenness)$label,
        closeness = topnv(TI, V(TI)$closeness)$label
)

# ***** Quite different! What would you predict their structural
#       setting to be?

######################################################################
# Weights Part 2
######################################################################

# If we want to consider weights we need to adjust for the
# interpretation as distance. Inversion is one option. You could also
# subtract edge weights from the maximum edge weight + 1 to retain
# linearity. The choice should be justified theoretically.

head(E(TI)$weight)
head(1/E(TI)$weight)

V(TI)$betweenness_wn <- betweenness(TI, normalized=TRUE, 
                                    weights=1/E(TI)$weight)
topnv(TI, V(TI)$betweenness)$label
topnv(TI, V(TI)$betweenness_wn)$label

# After the top three it is bringing in new people: we'd need to
# interpret in terms of frequency of interaction.

V(TI)$closeness_wn <- closeness(TI, normalized=TRUE, 
                                weights=1/E(TI)$weight)
topnv(TI, V(TI)$closeness)$label
topnv(TI, V(TI)$closeness_wn)$label

# It is just reordering the same people. 

# You can try computing with weights not inverted and comparing: the
# winners will be different and it will simply be wrong.

######################################################################
# Zafarani puzzle from the lecture (optional for class)
######################################################################

# Create a test graph g and verify it is the same as Zafarani Fig. 3.6
g <- graph_from_literal(v1-v2, v2-v3, v3-v4, v3-v5, v3-v6, v4-v5, v4-v6, v5-v6, v6-v7, v7-v8, v7-v9, v8-v9, v8-v10, v9-v10)
plot(g)

# Which do you think will be highest and second highest under these metrics? 
# Record your predictions here! 
#   degree: 
#   eigenvector: 
#   page_rank: 
#   betweenness: 
#   closeness:  

# compute the metrics
V(g)$degree <- degree(g)
V(g)$eigen_centrality <- eigen_centrality(g)$vector
V(g)$page_rank <- page_rank(g, directed=FALSE)$vector
V(g)$betweenness <- betweenness(g, directed=FALSE)
V(g)$closeness <- closeness(g)

# Make a table of rankings 
tibble(rank_d = topnv(g, V(g)$degree)$name,
       degree = topnv(g, V(g)$degree)$degree,
       rank_e = topnv(g, V(g)$eigen_centrality)$name,
       eigen  = topnv(g, V(g)$eigen_centrality)$eigen_centrality, 
       rank_p = topnv(g, V(g)$page_rank)$name, 
       p_rank = topnv(g, V(g)$page_rank)$page_rank, 
       rank_b = topnv(g, V(g)$betweenness)$name, 
       betwee = topnv(g, V(g)$betweenness)$betweenness, 
       rank_c = topnv(g, V(g)$closeness)$name, 
       close  = topnv(g, V(g)$closeness)$closeness
       )

# Optional: Write to compare in Gephi 
write_graph(g, "Centrality-Guessing-Game.graphml", format="graphml")

######################################################################
# Comparing to Gephi
# Let's see what Gephi can compute and compare results to igraph. 
######################################################################

write_graph(TI, "TI-Chats-With-Metrics.graphml", format="graphml")

# Go to Gephi and load this graph. Compute Eigenvector, PageRank, HITS
# (hubs and authorities), and Betweenness and Closeness (using
# Diameter: normalize). Compare the results.

######################################################################
# Activity: 
# Compute undirected centrality metrics on Les-Miserables.graphml 
# Write out, load into Gephi, and visualize (LinLog, Gravity 7)
# Compute Gephi versions of the metrics. Any discrepancies? 
# Compare the metrics to each other. Identify important actors 
# (besides Valjean) by roles suggested by these metrics. 
######################################################################
# Pau 

