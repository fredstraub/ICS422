######################################################################
# R Scratch for ICS 422/622 Spring 2022
######################################################################
# Common startup 

library(igraph)
setwd("~/Desktop/Network-Science-Demos")

######################################################################
# First Demo 

# In R 
library('igraph')
?sample_gnm
gnm <- sample_gnm(50,235) # random graph
gpa <- sample_pa(50,m=5)  # preferential attachment
plot(gnm)
plot(gpa)
# quartz("Degree Distributions")
plot(degree_distribution(gpa))
plot(degree_distribution(gpa, mode="in")) # also try out: what’s wrong with x axis? 

# In R Studio 
library('igraph')
NS <- read_graph("Networks/netscience.graphml", format="graphml")
NS # good start; TMI on the edges 
str(NS) # good for data tables, not good for igraph
summary(NS) # best way 
plot(NS) # why we use Gephi 
plot(degree_distribution(NS)) # … and more 

######################################################################
# Pau 
