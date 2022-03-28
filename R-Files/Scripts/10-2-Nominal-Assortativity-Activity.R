######################################################################
# Topic 10 Nominal Assortativity Activity Solutions
# Meant to be run in the context of 10-1-Nominal-Assortativity-Demo.R
# Aug 28 2018 DS: Minor updates for current script style and clarity
# Oct 29 2019 DS: Minor updates for fall 2019 class
# Mar 23 2021 DS: Minor updates for spring 2021 422/622
# Mar 22 2022 DS: Minor updates for spring 2022 422/622
######################################################################

library(igraph)
library(tibble)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/degree_domain.R")

######################################################################
# Political Blogs 
# Try nominal assortativity by the 'value' attribute, which represents
# left vs. right. 

PB <- read_graph("Networks/political-blogs.graphml", format="graphml")
summary(PB)
unique(V(PB)$value)

# value is string but can be converted directly to numbers 1, 2

leftright <- as.integer(V(PB)$value)
assortativity_nominal(PB, leftright)

# Easy to interpret. Left links to left and right links to right. 

####################
# Yeast Proteins 
# Try nominal assortativity by Class. We may not have the biological 
# knowledge to interpret fully, but we'll speculate. 

YP <- read_graph("Networks/Yeast-Proteins.graphml", format="graphml")
summary(YP)
unique(V(YP)$Class)

# Class is arbitrary string labels: must convert to factor first

classnum <- as.integer(as.factor(V(YP)$Class))
assortativity_nominal(YP, classnum)

# Additional item: A shortcut for computing assortativity of one 
# class vs all the others: 

assortativity_nominal(YP, (V(YP)$Class=="P")+1)

# This works because logicals are represented as 0 and 1, so we
# can add 1, coercing the logical to numeric. 

# ***** What does it mean that this value is higher for P? 

###################################
# Is the assortativity structural? 

# If you have a slow machine change the factor to 100 

f <- 1000
PB.rewired <- rewire(PB, with = keeping_degseq(niter = ecount(PB)*f))
assortativity_nominal(PB, leftright)
assortativity_nominal(PB.rewired, leftright)

YP.rewired <- rewire(YP, with = keeping_degseq(niter = ecount(YP)*f))
assortativity_nominal(YP, classnum)
assortativity_nominal(YP.rewired, classnum)

######################################################################
# Pau 


