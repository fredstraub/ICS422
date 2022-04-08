######################################################################
# Activity: Link Community Analysis of TI-Chats-Week-of-060401.graphml
# Mar 28 2020 DS: Minor updates for self-study students.
# Apr  6 2021 DS: Updates for ICS 422/622 Spring 2021
#  (Split off Centrality metrics into separate demo.)
######################################################################
# 0. Setup 
 
library(igraph)
library(linkcomm)
setwd("~/Desktop/Network-Science-Demos")

# Function to get the communities a node is in 
# (repeating in case we are in a new session)
getNodeCommunities <- function(lc, id) {
  as.numeric(lc$nodeclusters$cluster[which(lc$nodeclusters$node == id)])
}

########################################################
# 1. Prepare an Edge List Representation of the Network

TI <- read.graph("Networks/TI-Chats-Week-of-060401.graphml", 
                format="graphml")
summary(TI)

# Make edgelist representation 


#############################
# 2. Compute Link Communities 
# Ensure you can write to getwd()!!!


########################################
# 3. Inspecting Link Communities

# (a) How many link communities did you find? 


# (b) Plot the link communities. Would you like a better approach to
# plotting? :-)


########################################
# 4. Inspecting Induced Overlapping Node Communities 

# (a) What are the induced node communities communities with the most
# nodes (top 5)?


# (b) Can you plot the largest node community and identify the node
# with the largest page_rank in this community?


# (c) What are the names of the actors who are in the most induced
# node communities (top 5)?


# (d) Which communities are both JeffC and BjB in? 


# (e) Is anyone in all of the communities that JeffC is in? BjB? 


# (f) Plot community membership for the top nodes (the matrix plot): 



# (g) Which nodes have similar community membership in top
# communities?


# (h) Which communities have similar membership amongst the top nodes


########################################
# 5. Nested Communities 

# (a) Find and plot all nested communities, saving the returned
# results. (Hang on to your hat!)


# (b) How many communities are nested in other communities?


######################################################################
# Pau 
