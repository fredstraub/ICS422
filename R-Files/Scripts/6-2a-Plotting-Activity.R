######################################################################
# Class activity for 6-1-Plotting-Degree-Distributions.R (Spring 2022)
#
# Activity: plot degree distribution for another network 
# * Read in cit-HepTh.gml and compute degree distribution 
# * Make linear plot, and see how much degree_domain matters
# * Make log-log plot, and see how much degree_domain matters
# * Make cumulative and binned_histogram plots 
# * How would you handle a directed graph? 
# 
# You really should try all of this before looking at the answer!
######################################################################
# Only needed if you have not already run the class script: 

library(igraph)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")
source("Utility/binned_histogram.R")
source("Utility/nonzero_degrees.R")
source("Utility/nonzero_degree_distribution.R")

######################################################################
# Data Network
######################################################################
# Arxiv HEP-TH (high energy physics theory) citation graph is from the
# e-print arXiv and covers all the citations within a dataset of
# 27,770 papers with 352,807 edges. If a paper i cites paper j, the
# graph contains a directed edge from i to j. If a paper cites, or is
# cited by, a paper outside the dataset, the graph does not contain
# any information about this.
# The data covers papers in the period from January 1993 to April 2003
# (124 months). It begins within a few months of the inception of the
# arXiv, and thus represents essentially the complete history of its
# HEP-TH section.
# The data was originally released as a part of 2003 KDD Cup.

# Read in cit-HepTh.gml
HEP <- read_graph("Networks/cit-HepTh.gml", format="gml")
summary(HEP)

######################################################################
# Compute degree distribution and nonzero degree domain 
head(degree_distribution(HEP))

head(sort(unique(nonzero_degree_distribution(HEP))), 3)
######################################################################
# Make a linear plot with and without degree_domain to see how much
# degree_domain matters

plot(degree_distribution(HEP), 
     main="Arxiv HEP-TH Degree Dist Lin Lin", 
     xlab="k", ylab="p(k)")

plot(nonzero_degree_distribution(HEP), 
     main="Arxiv HEP-TH Nonzero Degree Dist Lin Lin", 
     xlab="k", ylab="p(k)")

# Is the adjustment needed in this case? Why or why not? 

######################################################################
# Make log-log plots with and without degree_domain 


# Is the adjustment needed in this case? Why or why not? 

######################################################################
# Make Cumulative Degree Distribution plot


######################################################################
# Make Binned Degree Distribution plot 


######################################################################
# How would you handle a directed graph? 
# Hint: compute and plot In-degree and Out-degree separately 

# Compute the distributions we'll need 


# Log Log In and Out 


# Cumulative In and Out 


# Binned Degree Distribution 



######################################################################
# Pau 