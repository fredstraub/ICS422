######################################################################
# Overlapping Community Detection 
# Class Activity 
# Dan Suthers, April 6, 2017
# Nov 14, 2019 DS: cleaned up for fall 2019 class
# Apr  6 2021 DS: Updates for ICS 422/622 Spring 2021
# 
######################################################################
# Setup 
library(igraph)
library(linkcomm)

setwd("~/Desktop/Network-Science-Demos") # Set to yours 
TI <- read.graph("Networks/TI-Chats-Week-of-060401.graphml", format="graphml")
summary(TI)

######################################################################
# Make edgelist representation 
TI.edges <- 
TI.edges <- 
head(TI.edges)

# Compute and check link communities 
TI.lc <- 
print(TI.lc)

######################################################################
# reify_link_communities

# Compute centralities befor we mess with the graph

# Make the reified link community graph 

# Go to Gephi ... 

# This one turns out to be hard to interpret: lots of small communities. 
# A good question for students: why aren't the three major actors BjB, 
# JeffC and DavidWe in a link community together, given that they all
# have reciprocal links to each other? (Consider who they connect to.)

######################################################################
# Pau 