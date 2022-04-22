######################################################################
# A quick look at statnet's sna package 
# Meant to run after Statnet-1-Converting-igraph-to-Network.R 
# as it uses the PS_network
# Dan Suthers, November 20, 2016
# Apr  9 2020 DS Updated for class
# Apr 22 2021 DS Updating for ICS 422/622 spring 2021
# Apr 21 2022 DS Major updates for ICS 422/622 spring 2022
#  * Merged Statnet-1-Networks and Statnet-2-SNA into this one script
#  * Removed some uninteresting extra stuff 
#  * Better sequence for the structural equivalence section.
######################################################################
# install.packages("statnet")    # if you have not done this 
# install.packages('intergraph') # ditto 

library(igraph)  # just using this to read in our familiar examples 
library(tibble)  # for some output 

# library(statnet) # Won't do this, it loads a lot.  
# I prefer to control which packages are loaded (notice masking): 

library(network)    # the graph representation used in statnet 
library(intergraph) # conversion utility between igraph and network
library(sna)        # analysis methods: notice masking of igraph 

# Set the directory you are working in 
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") 

######################################################################
# Making Networks 
######################################################################
# How to make random graphs. 

# This is like sample_gnp, where p is tprob. You can make a "stack" of
# m of them, useful for averaging across a sample, and tprob can vary
# across the stack. However, it makes matrix representations that then
# must be convered to network.

?rgraph
g <- rgraph(20)
class(g)
?as.network
gn <- as.network(g)
gn
plot(gn) 

# This is like sample_gnm. inconsistency: "m" has a different meaning!
# We'll skip it.

?rgnm

######################################################################
# Making a graph from an adjacency matrix and vertex attribute matrix

# To demonstrate this we need the matrices. Let's convert a familiar
# igraph: the 5 minute contact one from our Cascades demonstration.

PS <- read_graph("Networks/Primary-School-5-Minute-Contacts.graphml", 
                 format="graphml")
summary(PS)

PS_adj <- as_adjacency_matrix(PS)
PS_adj[1:20,1:20]

?as_data_frame # use the igraph version
PS_attrs <- igraph::as_data_frame(PS, what="vertices")
head(PS_attrs)

# --------------------------------
# Now we move to the statnet world.

# Make a network object out of the adjacency matrix. Turns out PS_adj
# is a sparse matrix but as.network needs a plain matrix.
class(PS_adj)
class(as.matrix(PS_adj))

?as.network
PSN <- as.network(as.matrix(PS_adj), directed=FALSE)
PSN 

# Add the attributes using this pattern for each attribute desired 

?set.vertex.attribute # Ambiguous without the package name
?network::set.vertex.attribute # scroll way down 
network::set.vertex.attribute(PSN, "classname", PS_attrs$classname)
network::set.vertex.attribute(PSN, "gender", PS_attrs$gender)

# Notice the addition of Vertex attribute names 

PSN

######################################################################
# Converting from igraph to network with intergraph 

# The above method obviously can also be used to convert igraphs to
# statnet networks, but there is a more direct approach.

# install.packages('intergraph')
library(intergraph)

# Main methods of interest: see also Index 

?asNetwork 
?asIgraph 

# Converting the igraph PS to a network 

PS_network <- asNetwork(PS)

# It appears identical to what we made "by hand" 

PS_network
PSN

# Converting the network to igraph, in the other direction

PS_igraph <- asIgraph(PS_network)

# Comparing them: notice that some extra attributes were added by
# network

summary(PS)
summary(PS_igraph)

# These should be the same 

all(V(PS)$label == V(PS_igraph)$label)
all(V(PS)$gender == V(PS_igraph)$gender)
all(V(PS)$classname == V(PS_igraph)$classname)
all(V(PS)$count == V(PS_igraph)$count)
all(V(PS)$duration == V(PS_igraph)$duration)

# These are new from the translation 

head(V(PS)$na)
head(V(PS)$vertex.names)

# This is where they came from 

list.vertex.attributes(PS_network)
list.edge.attributes(PS_network)

# I thought I could delete them but this broke the next demonstration,
# so they are essential. Leaving this code here just to show how to
# edit a graph. Note that unlike igraph modifiers, these modify the
# network IN PLACE: no <- needed
# delete.vertex.attribute(PS_network, 'na')
# delete.vertex.attribute(PS_network, 'vertex.names')
# delete.edge.attribute(PS_network, 'na')
# delete.edge.attribute(PS_network, 'Edge.Label')

# We'll save the version we just made for the next demo

save(PS_network, file="Primary-School-5M-Contacts-Network.rdata")

######################################################################
# Converting TI Chats Sociogram and Uptake with intergraph for our
# demo

# The one week sociogram 

TI <- igraph::read_graph("Networks/TI-Chats-Week-of-060401.graphml", 
                         format="graphml")
TI_network <- asNetwork(TI)
TI_network # correctly makes it directed 
save(TI_network, file="TI-Chats-Week-of-06040.rdata")

# The uptake graph for a short chat segment 

TIC <- igraph::read_graph("Networks/TappedInChatSample.graphml", 
                          format="graphml")
TIC_network <- asNetwork(TIC)
TIC_network
save(TIC_network, file="TappedInChatSample.rdata")

######################################################################
# Basics of describing and querying a network 
######################################################################

?sna 
# look at index 

# Basic queries

network.size(PS_network)
network.edgecount(PS_network)

list.vertex.attributes(PS_network)
list.edge.attributes(PS_network)
list.vertex.attributes(TIC_network) 
list.edge.attributes(TI_network)

head(get.vertex.attribute(PS_network, "classname"), 10)
head(get.edge.attribute(PS_network, "duration"), 10)

######################################################################
# Familiar Metrics 

?sna::degree # SNA verson: the igraph one is masked or not loaded
head(degree(TI_network), 20)
?sna::betweenness
head(betweenness(TI_network), 20)
head(betweenness(TI_network, rescale=TRUE), 20) # normalized

# This handles various centrality metrics chosen by cmode, but since
# they are prestige metrics there is no out-degree!

?prestige 

# Default is in-degree 

head(prestige(TIC_network), 20) 
head(prestige(TIC_network, cmode="indegree"), 20) 

# Will just note the presence of this and move on 

head(prestige(PS_network, gmode="graph", cmode="eigenvector"), 20)

######################################################################
# Metrics not found in igraph

# Domain is a transitive prestige, counting not just indegree but how
# many other nodes transitively point to a node that point to the ego
# node. It is very useful in analyzing sequentially related events,
# such as TI Chats: how many other chats did a given chat influence
# transitively? Notice how degree does not give all the information:

l <- 20 
tibble(
  indegree = head(degree(TIC_network, cmode="indegree"), l),
  indomain = head(prestige(TIC_network, cmode="domain"), l),
  actor = head(get.vertex.attribute(TIC_network, "actor"), l), 
  contribution = head(get.vertex.attribute(TIC_network, "contribution"), l)  
)

# A numeric measure of Krackhardt's connectedness 

?connectedness
connectedness(PS_network)
connectedness(TI_network)
connectedness(TIC_network)

# There is also efficiency, hierarchy and "lubness" but I'm not clear
# on their meaning so will skip.

# We have this in igraph but it is popular with social scientists 

?sna::triad.census
triad.census(PS_network)
# triad.census(TI_network)

# Brokerage is an important sociological concept 

?brokerage # various kinds of mediating roles (bridges, etc)

# You need to give it "class memberships". This could be any attribute
# by which we classify nodes, including results of community detection
# algorithms, but here we literally have class membership!

PS_b <- brokerage(PS_network, 
                  get.vertex.attribute(PS_network, "classname"))

# Results are complex but perhaps we can interpret z scores 

names(PS_b)
head(PS_b$z.nli) # normalized z scores for each role

# Can you find students who are moderate "brokers" between distinct
# classes? Need to know the range (wide screen for this):

summary(PS_b$z.nli)

######################################################################
# Structural Equivalence of vertices means having similar
# connectivity, and is popular in sociological network analysis.

?sedist # distance metric based on structural equivalence 
PS_sed <- sedist(PS_network)
plot(cmdscale(as.dist(PS_sed))) 

# We don't interpret that; instead we use it as a distance metric for
# cluster analysis: 

?equiv.clust # note sedist is the default 
PS_eq <- equiv.clust(PS_network)
plot(PS_eq) # groups nodes by their structural equivalence 

# Somewhere in there are 10 classes and 5 grades!
# That in term is used to construct a block model. 

?blockmodel # a way of summarizing cluster analysis 
PS_bm <- blockmodel(PS_network, PS_eq, 10) # for the classes

# Make plot wide. This is likely to crash R so I have it last! 
plot(PS_bm) 

# There is much more; that is just a sample. 

######################################################################
# Pau 