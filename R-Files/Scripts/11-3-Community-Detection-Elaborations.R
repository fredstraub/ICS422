######################################################################
# Community Detection Demo: Elaborations 
# Dan Suthers, March 21, 2017
# March 23, 2018 (after class)
#   The demo has been reordered to flow better. More explanatory
#   comments were added, and variable names made clearer. Also 
#   fixed some errors. 
# Sep 17, 2018 DS: Minor updates for current script style and clarity
# Thu Nov  7 2019 DS: Modifying for 2019 class 
# Sat Nov  9 2019 DS: Added a check that flattening ends was indeed
#  preserving vertices at both ends of the edges. 
# Mon Mar 16 2020 DS: Minor updates for spring 2020 self study students
# Thu Apr  1 2021 DS: Minor updates for 422/622 spring 2021
# Thu Mar 31 2022 Updates for 422/622 spring 2022 (more systematic). 
######################################################################
# Setup  

library(igraph)
library(tibble)

setwd("/Users/fred/Github/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")

NS <- read_graph("Networks/netscience.graphml", format="graphml")
TI <- read.graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")

######################################################################
# What difference does weights make?
######################################################################

# Default uses weights 

?cluster_louvain
?cluster_infomap # has v.weights too, but we don't use it 

# How much difference is there in NS weights? 

summary(E(NS)$weight) 
hist(E(NS)$weight, breaks=50)

# Compute with weights and save membership 

NS_louvain_w <- cluster_louvain(NS)
V(NS)$comm_louvain_w <- membership(NS_louvain_w)
NS_infomap_w <- cluster_infomap(NS)
V(NS)$comm_infomap_w <- membership(NS_infomap_w)

# Compute without weights and save membership

NS_louvain_u <- cluster_louvain(NS, weights=NA)
V(NS)$comm_louvain_u <- membership(NS_louvain_u)
NS_infomap_u <- cluster_infomap(NS, e.weights=NA)
V(NS)$comm_infomap_u <- membership(NS_infomap_u)

# Compare 

clist <- list(NS_louvain_w, NS_louvain_u, NS_infomap_w, NS_infomap_u)
tibble( 
  method = c("Louvain W", "Louvain U", "InfoMap W", "InfoMap U"), 
  modularity = sapply(clist, modularity),
  commcount  = sapply(clist, length)
  )

# Little difference on summary metrics but we can see some variation
# in the larger partitions where the highly weighted nodes live: 

table(sizes(NS_louvain_u))
table(sizes(NS_louvain_w))

# Hypothesis: high weights break up the larger component, possibly
# centered around high weight nodes.

# ***** Quick look in Gephi to confirm. (Could also extract components.)

# For a given application, you need to decide whether community 
# membership should be based on weighted ties. In the case of NS, 
# it makes sense to use weights, even if the difference is small.

######################################################################
# Community Structure of Random Models 
######################################################################

# G(n,m) (we'll look at result below). 

NS_gnm <- sample_gnm(vcount(NS), ecount(NS))
NS_gnm_louvain <- cluster_louvain(NS_gnm)

# Is community structure dependent on degree sequence? The
# configuration model is actually the null model in Newman's
# modularity formula (the ki*kj/2m term). We don't need attributes,
# so we'll use the configuration model here.

NS_config         <- sample_degseq(degree(NS))
NS_config_louvain <- cluster_louvain(NS_config)

# The random models don't have edge weights, so compare to partition
# found without weights

tibble( 
  graph = c("NS Original", "NS Config", "NS G(n,m)"), 
  modularity = c(modularity(NS_louvain_u),
                 modularity(NS_config_louvain),
                 modularity(NS_gnm_louvain)),

  commcount  = c(length(NS_louvain_u),
                 length(NS_config_louvain), 
                 length(NS_gnm_louvain))
)

# Expect components to merge (make console wider before running)

table(sizes(NS_louvain_u))
table(sizes(NS_config_louvain))
table(sizes(NS_gnm_louvain))

# ***** Is community structure due to the degree sequence? 

# ***** What community structures go away and which develop more
#       strongly in the random model?

###################################
# Tapped In Chat Sociogram
# Another example using rewiring.

# As we saw in the activity, need to go undirected. 

TI_undirected <- as.undirected(TI, mode="collapse")

# G(n,m) 

TI_gnm <- sample_gnm(vcount(TI_undirected), ecount(TI_undirected))
TI_gnm_louvain <- cluster_louvain(TI_gnm)


# Let's try rewiring this time (it is equivalent to the configuration
# model and fast on smaller graphs) We'll use undirected for comparison. 

TI_rewired <- rewire(TI_undirected, 
                     with = keeping_degseq(niter = ecount(TI) * 1000))

# Does the rewired graph keep weights? 

head(E(TI_undirected)$weight, 15)
head(E(TI_rewired)$weight, 15) # why does this make sense? 

# Find communities, making both unweighted 

TI_cl <- cluster_louvain(TI_undirected, weights=NA) 
TI_rewired_cl <- cluster_louvain(TI_rewired)
V(TI_undirected)$comm_louvain <- membership(TI_cl)
V(TI_rewired)$comm_louvain <- membership(TI_rewired_cl)

# Compare 

tibble( 
  graph = c("TI Original", "TI Rewire", "TI G(n,m)"), 
  modularity = c(modularity(TI_cl),
                 modularity(TI_rewired_cl),
                 modularity(TI_gnm_louvain)), 
  commcount  = c(length(TI_cl),
                 length(TI_rewired_cl), 
                 length(TI_gnm_louvain))
)

# Again, there is some community structure due to the density of the
# graph, but more that must be attributed to other factors.

######################################################################
# Quantitative Comparison of Partitions
######################################################################
# Above we were comparing partitions on modularity and count. Is there
# a more precise way to measure how similar two partitions are? 

?compare # Yes. We will study the three methods below. 

# A small example first: 
K <- make_graph("Zachary")
K_louvain <- cluster_louvain(K)
K_infomap <- cluster_infomap(K)

# Repeating our plotting from last class 
K.layout <- layout_with_fr(K)
plot_karate <- function(comm, title){
  plot(comm, K, layout=K.layout, 
       main=paste0(title, 
                   ": M = ", round(modularity(comm), 4),
                   ", # = ", length(comm)))
}
plot_karate(K_louvain, "Louvain")
plot_karate(K_infomap, "InfoMap")

# ----------
# Split-Join
# A distance metric based on the number of pairs you have to swap to
# make the partitions the same. It does not seem to be used much in
# the literature, but is simple to understand. This documentation is
# from a different software package:
# https://micans.org/mcl/man/clmdist.html#_section_5

compare(K_louvain, K_infomap, method="split.join") # Find 6 nodes in plots
compare(K_louvain, K_louvain, method="split.join") # Perfect agreement

# ----------
# Rand Index
# The proportion of pairs of items on which the two classifiers agree,
# where agreement means classified in the same cluster or classified
# in different clusters by both classifiers. It is in [0, 1], where 1
# is most similar. It does not take into account what is expected at
# random. The Adjusted Rand index does, but may not fall in [0, 1].
# https://en.wikipedia.org/wiki/Rand_index#Adjusted_Rand_index

compare(K_louvain, K_infomap, method="rand")
compare(K_louvain, K_infomap, method="adjusted.rand")

# Perfect agreement is 1 rather than 0:

compare(K_louvain, K_louvain, method="rand")
compare(K_louvain, K_louvain, method="adjusted.rand")

# -----------------------------
# Variation of Information (VI)
# An information theoretic measure of the information "distance"
# between two clusterings. There is no upper bound on the value, but
# smaller results mean more similar. Obeys the triangle inequality,
# which may be useful in some applications.
# https://en.wikipedia.org/wiki/Variation_of_information
# https://link.springer.com/chapter/10.1007/978-3-540-45167-9_14

compare(K_louvain, K_infomap, method="vi")
compare(K_louvain, K_louvain, method="vi") # Perfect agreement

# -----------------------------------
# Normalized Mutual Information (NMI) 
# Measures how much information one variable gives you about the other,
# so larger results mean more similar. This version is normalized to
# [0,1], making it like a correlation coefficient. 
# https://en.wikipedia.org/wiki/Mutual_information 
# http://deim.urv.cat/~alexandre.arenas/publicacions/pdf/jstat05.pdf

compare(K_louvain, K_infomap, method="nmi")
compare(K_louvain, K_louvain, method="nmi") # Perfect agreement

# I see normalized mutual information used a lot in the literature.

##########################
# Try with Network Science 

# Expect Louvain and InfoMap to be similar but not identical

compare(NS_louvain_u, NS_infomap_u, method="split.join")    # swaps
compare(NS_louvain_u, NS_infomap_u, method="rand")          # % pairs agreeing
compare(NS_louvain_u, NS_infomap_u, method="adjusted.rand") # adjusted random
compare(NS_louvain_u, NS_infomap_u, method="vi")            # info distance 
compare(NS_louvain_u, NS_infomap_u, method="nmi")           # info similarity 

# Compared to Configuration Model 
# Is there probability based test of whether a community structure
# departs from random? The literature does not focus on this because
# any natural network departs from random. However, we can compare to
# the configuration model to determine the extent to which degree
# distribution constrains community structure. We expect the original
# and configuration model to differ.

# How many swaps would be needed to bring them into alignment? 

compare(NS_louvain_u, NS_config_louvain, method="split.join")

# What % pairs agree? I use both original and adjusted Rand to show
# how much the adjustment matters. They appear to agree a lot until we
# factor out what is expected at random.

compare(NS_louvain_u, NS_config_louvain, method="rand")
compare(NS_louvain_u, NS_config_louvain, method="adjusted.rand") # wow! 

# Distance is > 1, though I am not sure how to interpret the number 

compare(NS_louvain_u, NS_config_louvain, method="vi") 

# This might be the best one. A nontrivial portion of the community
# structure is constrained by the degree distribution.

compare(NS_louvain_u, NS_config_louvain, method="nmi")

######################################################################
# Let's quantify the similarity between Gephi's and igraph's Louvain

summary(NS) # has our igraph memberships 
write_graph(NS, "Network-Science-Communities.graphml", format="graphml")

# *** Read the above Network-Science-Communities.graphml into Gephi and 
# *** use Gephi's implementation of Louvain, "Modularity", to compute 
# *** the "Modularity Class" of each vertex. Write it out as graphml. 

# *** Attribute names with spaces in them are hard to work with in igraph.
# *** Edit the graphml and replace string "Modularity Class" with 
# *** "gephi_louvain". Then read that back into igraph and continue ... 

NS2 <- read_graph("Network-Science-Communities2.graphml", format="graphml")
summary(NS2)

# We don't have a community object from Gephi, so we need to give it a
# membership vector instead. But values must start with 1.

?compare

# Gephi starts modularity class IDs at 0, but 'compare' needs IDs
# that start with 1. Easy to fix: 

head(sort(unique(V(NS2)$gephi_louvain))) # starts with 0
V(NS2)$gephi_louvain <- V(NS2)$gephi_louvain + 1
head(sort(unique(V(NS2)$gephi_louvain))) # now starts with 1 

# Now we can compare, using membership vectors (Gephi was weighted):

compare(membership(NS_louvain_w), V(NS2)$gephi_louvain, method="nmi")
compare(membership(NS_louvain_w), V(NS2)$gephi_louvain, method="vi")
compare(membership(NS_louvain_w), V(NS2)$gephi_louvain, method="adjusted.rand")

# Quite good agreement! 

# **** What's a quick way to find where the partitions differ, given
#      that they use different numbers? 

#      I have not done this, but we must rely on the fact that
#      we have vertex-vertex correspondence across the graphs;
#      otherwise it's an NP-Hard problem!

######################################################################
# Edges Crossing Partitions 
######################################################################
# We would want to minimize the number of edges crossing partitions, 
# all other things being equal (but they are not) ... 

?crossing

crossing(NS_louvain_w, NS)[1:100]
length(which(crossing(NS_louvain_w, NS)))
length(which(crossing(NS_infomap_w, NS)))

# ***** Why does that make sense in in terms of what we know about
#       the number of communities each algorithm produced AND
#       the formulas they are optimizing? 

# People who connect to edges that cross partitions may be playing
# a brokering role between communities, so we want to identify them. 
# Here is how I figured out how to make a table of such actors.
# (All but the last line are for exposition only: you only need the
# last line. Until then, I show only 'head' to not fill your screen.) 

# Select the crossing edges from E(NS)

head(E(NS)[which(crossing(NS_louvain_w, NS))])

# Who's at the ends? 'end' gives a list of pairs of IDs

?ends 

# Note I am dropping E(NS)[...] and it still works 

head(ends(NS, which(crossing(NS_louvain_w, NS))))[1:1]
tail(ends(NS, which(crossing(NS_louvain_w, NS))))
length(ends(NS, which(crossing(NS_louvain_w, NS))))

# Note the total length is the two columns stacked up

length(ends(NS, which(crossing(NS_louvain_w, NS)))[,1])

# Convert that into vertex objects with V(NS) 
# It will also flatten out the 2xE matrix
# Check the tail and length to confirm that we got both columns. 

head(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]) # Has start of [,1]
tail(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]) # Has end of [,2]
length(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))])

# Now that we are sure we did not lose data, convert to labels 

head(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]$label)

# That had duplicates: we can make a list of unique members and sort
# on the label. These are the actors who connect to other clusters: 

sort(unique(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]$label))

# Are they all equal crossers? Make a table of frequency counts.

head(table(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]$label))

# For the final result, sort the table in decreasing order. R knows
# to sort the table on the frequency, not the label like above! 

sort(table(V(NS)[ends(NS, which(crossing(NS_louvain_w, NS)))]$label), 
     decreasing=TRUE)
     
# ***** Who is brokering between communities? 
# ***** Is this consistent with your prior studies of this network? 
# ***** How do you think these results relate to betweenness? 

# I recommend that we (or you) look at the visualization of the
# network in Gephi to help you think about this. It turns out that
# while Jeong has many more crossing edges than Newman, they are to a
# similar number of partitions as Newman's crossing edges, so there is
# some redundancy. You will also notice that the partitions reached by
# Jeong's crossing edges are more closely related to each other than
# the partitions reached by Newman's crossing edges. So, be cautious
# about interpreting number of crossing edges as a node centrality
# metric. we will see a better metric in topic
# 12-3-Community-Centrality-Demo.R

######################################################################
# Extracting Partitions as Graphs  
######################################################################
# I want to get the partition Newman is in under Louvain. 

# Get Newman's vertex index 

(N_index <- which(V(NS)$label=="NEWMAN, M"))

# Get the partition this vertex is in

(N_part_id <- membership(NS_louvain_w)[N_index])

# Make an induced subgraph of all vertices in this partition 

N_part <- induced_subgraph(NS, V(NS)$comm_louvain_w == N_part_id)
N_part$name <- "Newman's Partition"
summary(N_part)

plot(N_part, main = paste("Partition", N_part_id),
     vertex.color=V(N_part)$label == "NEWMAN, M")

######################################################################
# Community Size Distributions
######################################################################

# Barabasi notes that some literature shows community size distribution
# follows a power law. Here we plot on log-log plot for an initial look. 

# Utility: Given community object, plot community sizes 

plot_community_sizes <- function(c_obj, log="xy", main="Community Sizes") {
  table <- table(sizes(c_obj))
	plot(as.numeric(names(table)), as.vector(table), log=log,
	     main=main, xlab="community size", ylab="frequency",
	     sub=paste("M =", round(modularity(c_obj), 4), 
	                "# =", length(c_obj)))
}

new_window("Community Size Distributions", 12, 6)
par(mfrow=c(1,2))

# Plot both linear and log-log (default)
plot_community_sizes(NS_louvain_w, main="NS Louvain", log="")
plot_community_sizes(NS_infomap_w, main="NS InfoMap", log="")
plot_community_sizes(NS_louvain_w, main="NS Louvain")
plot_community_sizes(NS_infomap_w, main="NS InfoMap")

# ***** Notice the log-log shape w.r.t. Barabasi's claim. 

# Compare to configuration models

plot_community_sizes(NS_louvain_u, main="NS Louvain UW")
plot_community_sizes(NS_config_louvain, main="Config Louvain")

plot_community_sizes(NS_infomap_u, main="NS InfoMap UW", log="xy")
NS_config_infomap <- cluster_infomap(NS_config)
plot_community_sizes(NS_config_infomap, main="Config Infomap", log="xy")

# ***** Any idea what is going on in InfoMap's analysis of Rewired? 
#       (Compare Config Louvain to Config Infomap)

######################################################################
# Performance on Large Graphs -- If time, or do at home 
######################################################################
# Due to the NP-Hard nature of this problem, we need approximation
# algorithms. There are many of them, some of which we learned about
# in the prior demo. How fast are they? We have reviewed the
# theoretical big-O results, but let's get a feel in practice with the
# larger HEP citation network.

HEP <- read_graph("Networks/cit-HepTh.gml", format="gml")
summary(HEP)

# It is directed and unweighted. Some methods don't use direction. 
# To simplify the demo we'll make it undirected.

HU <- as.undirected(HEP, mode="collapse")

# How fast is "fast greedy"? 

system.time(HU.fastgreedy <- cluster_fast_greedy(HU))
modularity(HU.fastgreedy)

# Louvain's hierarchical method is designed for speed: 

system.time(HU.louvain  <- cluster_louvain(HU))
modularity(HU.louvain)

# How do its competitors compare? 

system.time(HU.infomap <- cluster_infomap(HU))

# HU.walktrap <- cluster_walktrap(HU) # too slow for class 
# date()

modularity(HU.fastgreedy)
modularity(HU.louvain)
modularity(HU.infomap)

# ***** Now you can see one reason to prefer one of these! 
#       Given that Louvain and InfoMap use the same underlying
#       algorithm, why does InfoMap take so much longer?

length(HU.fastgreedy)
length(HU.louvain)
length(HU.infomap)

# ***** How can you explain the Louvain and InfoMap results in terms
#       of the algorithms?

######################################################################
# Pau
