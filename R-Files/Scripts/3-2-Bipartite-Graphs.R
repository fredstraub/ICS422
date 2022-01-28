######################################################################
# 3-2 Bipartite Graphs: Represenation and Visualization. 
# Dan Suthers, January 27, 2017
# July 23, 2018: Split this off from 2-3-Plotting-in-iGraph. 
# Sep 10 2019 DS: minor changes for 2019 
# Jan 28 2021 DS: Removed nonessential material for 422/622
# - Not using new_window 
# Jan 27 2022 DS: Updating for spring 2022 class: formatting only. 
#                 ~10 minutes
######################################################################
# *** NOTE: Good idea to restart R session after 3-1 demo 
# Standard preamble 
# We'll almost always do these at the beginning of every script, but
# it is not needed if you already did it in this session: 
getwd()

library(igraph)
setwd("/Users/frederickstraub/R-Files") # Set to your location

######################################################################
# Introduction 
#
# Bipartite graphs are often useful for capturing the relationships
# between two kinds of entities, such as actors and venues (company
# boards, meetings), or actors and media through which they interact.
# But sometimes when we have a bipartite graph we want to do
# projection to make direct links between nodes of one type. Here we
# introduce visualization of bipartite graphs and how to project them
# in igraph.

######################################################################
# Data 
######################################################################
# Here is a trivial bipartite graph (converted with upgrade_graph from
# the SAND demo). It connects imaginary actors to movies they appear in.

BPG <- read_graph("Networks/actor-movie.graphml", format="graphml")

BPG
# Note the "B" and 'type' attribute, and the links 

# The type attribute must be logical and differentiates the partitions. 
V(BPG)$type
V(BPG)$name

######################################################################
# Plotting 
######################################################################

# Let's compare some plots in a two-frame window

par(mfrow=c(1, 2))

# Default plot does not show bipartite organization well 

plot(BPG, main = "Bipartite v1")

# We can set default shape and color conditional on V(BPG)$type
# to indicate partitions, for example: 

V(BPG)$type
ifelse(V(BPG)$type, "rectangle", "circle")

# Let's set shape and color options for the next several plots 

igraph_options(
  vertex.shape = ifelse(V(BPG)$type, "rectangle", "circle"),
  vertex.color = ifelse(V(BPG)$type, "red", "cyan"))

plot(BPG, main = "Bipartite v2")

# Now it is easier to tell the node types apart, but the layout is not
# good. There is a layout method that works for small graphs

?layout_as_bipartite

plot(BPG, main = "Bipartite v3", 
     layout = layout_as_bipartite(BPG))

# (Notice it is cycling through the two frames, starting over again)

# If you want it to make vertical columns add the expression [,2:1] to  
# flip the columns. (I got this from SAND, not the R documentation.)  
# Let's examine how the layouts are represented: 

layout_as_bipartite(BPG)
layout_as_bipartite(BPG)[,2:1] # Select from column 2 to 1: reverses 

# See how the columns were flipped? Now let's draw it: 

plot(BPG, main = "Bipartite v4", 
     layout = layout_as_bipartite(BPG)[,2:1])

# If you want the actors on the left, put in a - before the 
# layout (I'm not sure why but it works!)

-layout_as_bipartite(BPG)[,2:1]
plot(BPG, main = "Bipartite v4", 
     layout = layout_as_bipartite(BPG)[,2:1])  # old
plot(BPG, main = "Bipartite v5", 
     layout = -layout_as_bipartite(BPG)[,2:1]) # new
 
######################################################################
# Projecting 
######################################################################

# Bipartite graphs can be projected onto one of the partitions
# for example connecting two actors if they are in the same movie: 

?bipartite_projection
BPG.proj <- bipartite_projection(BPG)
BPG.proj

# So the result is a named list of igraphs, one for each projection. 
# Use the $ attributes to access parts: 

summary(BPG.proj$proj1)
summary(BPG.proj$proj2)

# Notice that 'type' is gone, as it is implied by the partitions. 
# ***** How would you tell which has the actors and which has the
#        movies?

# We can plot the projections and get the expected results, but the 
# type attribute is gone so we need to specify the visuals 

plot(BPG.proj$proj1, main = "Projection 1", 
          vertex.shape = "circle", 
          vertex.color = "cyan")
plot(BPG.proj$proj2, main = "Projection 2",
     vertex.shape = "rectangle", 
     vertex.color = "red")

# Again we should reset our defaults.

igraph_options(vertex.shape=NULL, vertex.color=NULL)
par(mfrow=c(1, 1))

# You will do something like this on a homework. 
#################################################################
# Pau 