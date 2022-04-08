######################################################################
# Overlapping Community Detection Demonstration: 
# Link Community Centralities 
# 
# Dan Suthers, April 6, 2017 (Original NetScience version)
# Apr  6 2021 DS: Split off Centrality metrics from 12-1 Basics 
# Apr  7 2022 DS: Minor updates for ICS 422/622 Spring 2022
#
######################################################################
# Setup 

library(igraph)
library(linkcomm)
library(tibble)
setwd("~/Desktop/Network-Science-Demos") # Set to yours

# Network we will use - small, familiar, interpretable 

LM <- read_graph("Networks/Les-Miserables.graphml", format="graphml")
summary(LM)

# Reminder: To use linkcomm, you require read and write permissions in
# the current directory! If needed, change it after reading the graph
# from Google Drive.

######################################################################
# Re-establishing linkcomm from 12-1 demo 
######################################################################

LM_wedges <- cbind(as_edgelist(LM), E(LM)$weight)
LM_wlc <- getLinkCommunities(LM_wedges, hcmethod="average", plot=FALSE)

######################################################################
# NODE community centrality: 
# How important is each node in the overall community structure? 
######################################################################

# We could just ask which nodes are in the most communities ... 

?getLinkCommunities # reminder of numclusters 

(nc <- tibble(
  names = V(LM)[as.numeric(names(head(LM_wlc$numclusters, 10)))]$label,
  numclusters = head(LM_wlc$numclusters, 10)
))

# However, a node may belong to communities that highly overlap. 
# Perhaps a node with the same number of communities is more 
# important if it belongs to communities that are different. See: 

?getCommunityCentrality

############
# commweight - values being in distinct communities
# Node centralities based on weighted count of community membership.
# From section 3.3.3 of Kalinka & Tomancak (2011) linkcomm.pdf: 
# "In link communities, nodes may belong to several communities, and
# so it is possible to measure the importance of a node in a network
# based on the number of communities to which it belongs. To do this,
# we weight the membership of a node in a community by how distinct
# that community is from the other communities to which the same node
# belongs." This is the default if type is not given. 

LM_cw <- getCommunityCentrality(LM_wlc, type="commweight")

# Let's look at the top nodes.

top_cw <- head(sort(LM_cw, decreasing = TRUE), 10)
cw <- tibble(
  names = V(LM)[as.numeric(names(top_cw))]$label,
  commweight = as.numeric(top_cw)
)
cbind(cw, nc) # Adding previous result for comparison 

# Notice that the ordering is consistent with numclusters but refines
# within each value (the new names were on the '3' list). 

# ***** What does this say about Marius' role? 

###########
# commconn - values being in connected communities 
# Node centrality based on connectedness of communities. 
# From ?getCommunityCentrality: 
# "... weights each community that a node belongs to by how many
# connections the community forms outside of itself relative to how 
# many connections the community has within itself (the inverse of
# modularity), so that nodes that belong to more highly connecting
# communities will receive a higher community centrality score." 

LM_cc <- getCommunityCentrality(LM_wlc, type="commconn") 
(top_cc <- head(sort(LM_cc, decreasing = TRUE), 10))
tibble(
  names = V(LM)[as.numeric(names(top_cc))]$label, 
  commconn = as.numeric(top_cc)
)

# Or putting it all together, we see that this is very different: 

tibble(
  numclusters = head(LM_wlc$numclusters, 10), 
  clustnames = V(LM)[as.numeric(names(head(LM_wlc$numclusters, 10)))]$label,
  commweight = as.numeric(top_cw), 
  weightnames = V(LM)[as.numeric(names(top_cw))]$label,
  commconn = as.numeric(top_cc), 
  connnames = V(LM)[as.numeric(names(top_cc))]$label
)

# ***** How do you explain the change in who is top ranked? 

# ***** How might you use these centralities? Which traditional
#       centralities are they similar to, if any? 

######################
# A WARNING 

# As we usually do with other centralities, I tried to assign these
# values to vertices and visualize them in Gephi by node size. The
# wrong one was large. Why?

V(LM)$commconn <- as.numeric(LM_cc) # what we normally do
V(LM)[V(LM)$commconn == max(V(LM)$commconn)]$label # who has the max  
V(LM)[V(LM)$label == "Valjean"]$commconn           # expected Valjean

# Documentation says value is: "A named numerical vector where the
# names are node names and the numbers are community centrality
# measures." However, in what order??

head(LM_cc, 10)

# The one with top cc is in the 8th position, but Valjean has index 1,
# so Courfeyrac got Valjean's number:

c(V(LM)[1]$label, V(LM)[1]$commconn)
c(V(LM)[8]$label, V(LM)[8]$commconn)

# This means we cannot assign the vector to vertices as we do for
# other centralities, unless we sort the values by vertex ID (table
# names). Can be done but I'll spare us this detail. 

delete_vertex_attr(LM, "commconn")

######################################################################
# CLUSTER Community Connectedness: 
# How important is each cluster in the overall community structure? 
######################################################################

# This gives cluster (not node) metrics of connectivity: 

?getCommunityConnectedness

# Documentation of "conn" vs "mod" is sketchy, but with some 
# experimentation we can see that 
#   comm = "mod" returns an unnormalized modularity (can be greater
#          than 1)
#   comm = "conn" (the default) returns the inverse of the above
# Therefore, a community with very low modularity will have very
# high community connectivity because it is connected to many other
# communities.  Let's test this: 

# Community Connectedness

head(LM_comcon <- getCommunityConnectedness(LM_wlc, conn = "conn"))

# Community Modularity

head(LM_commod <- getCommunityConnectedness(LM_wlc, conn = "mod"))

# They are inverses of each other

head(1/LM_commod)

# What are the top ones under each measure, and what does this mean? 

head(sort(LM_comcon, decreasing=TRUE)) # Community connectedness
head(sort(LM_commod, decreasing=TRUE)) # Community modularity

# Notice that the lists are completely different, unlike for the node
# centralities. This is expected since the measures are inverses. 

# ***** What is the relationship between Cluster 4 and other clusters? 
# ***** What is the relationship between Cluster 2 and other clusters? 

# Who is in them? 

(labels_in_4 <- V(LM)[as.numeric(getNodesIn(LM_wlc, clusterids = 4))]$label)
(labels_in_2 <- V(LM)[as.numeric(getNodesIn(LM_wlc, clusterids = 2))]$label)

# Plot the two clusters together

g24 <- induced_subgraph(LM, getNodesIn(LM_wlc, clusterids = c(2, 4)))

plot(g24,
     vertex.label.cex=0.8,
     # vertex IDs different for the two graphs so map by label
     vertex.color=(V(g24)$label %in% labels_in_2))

# Or in the full graph 
plot(LM, 
     vertex.size = 10,
     vertex.label.cex=0.7,
     vertex.color=ifelse(V(LM)$label %in% labels_in_4, 
                         "lightblue", 
                         ifelse(V(LM)$label %in% labels_in_2,
                                "pink","white")))

# Keep in mind that these are not necessarily single scenes, 
# but possibly clusters of scenes involving the same persons.
# May be useful to look at in Gephi. 

# ***** How might you use these centrality metrics?

######################################################################
# Summary: This is a template for the essential steps from above. 
######################################################################

# Assuming we have done 
g <- read.graph("filename.type", format="type")
g_edges <- as_edgelist(g)
# if weighted 
g_edges <- cbind(g_edges, E(g)$weight)

# Node centralities based on weighted count of community membership.
getCommunityCentrality(g_lc, type="commweight") 

# Node centrality based on connectedness of communities. 
getCommunityCentrality(g_lc, type="commconn") 

# Community connectedness: isolation vs connectedness 
getCommunityConnectedness(g_lc, conn = "conn") # modularity (isolation)
getCommunityConnectedness(g_lc, conn = "mod")  # inverse of above 

######################################################################
# Activity: Do the above with TI-Chats-Week-of-060401.graphml! 
# Use 12-4-Community-Centrality-Activity.R
######################################################################
# Pau 