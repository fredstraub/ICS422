######################################################################
# Activity: Link Community Analysis of TI-Chats-Week-of-060401.graphml
# Apr  6 2021 DS: Separated from basic demo, and other updates for 
#   ICS 422/622 Spring 2021
# Apr  7 2022 DS: Minor updates for ICS 422/622 Spring 2022
######################################################################
# Setup 
 
library(igraph)
library(linkcomm)
setwd("~/Desktop/Network-Science-Demos")

TI <- read_graph("Networks/TI-Chats-Week-of-060401.graphml", 
                format="graphml")
summary(TI)
TI_edges <- cbind(as_edgelist(TI), E(TI)$weight)
TI_lc <- getLinkCommunities(TI_edges, hcmethod="average", 
                            directed=TRUE, plot=FALSE)

######################################################################
# Community-based Measures of Centrality 

# (a) Who has the top 5 commweight node centralities, and what does
# this mean for their behavior?


# (b) Who has the top 5 commconn node centralities, and what does this
# mean for their behavior?


# (c) Extract and plot the communities with the greatest com.con and
# comm.mod


# (d) Explain what these metrics mean in terms of the communities you
# plotted, and why this makes sense given the names you see plotted.


######################################################################
# Pau 
