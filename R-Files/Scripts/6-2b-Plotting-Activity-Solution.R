######################################################################
# Solution to class activity 6-2a-Plotting-Activity (Spring 2022)
#
# Activity: plot degree distribution for another network 
# * Read in cit-HepTh.gml and compute degree distribution 
# * Make linear plot, and see how much degree_domain matters
# * Make log-log plot, and see how much degree_domain matters
# * Make cumulative and binned_histogram plots 
# * How would you handle a directed graph? 
# 
# September 26, 2019 DS: Updates for Fall 2019
# Feb 08 2020 DS: Cleaning up for self study use. 
# Feb 16 2021 DS: Adjustments for Spring 2021
# Feb 15 2022 DS: Updates for Spring 2022 422/622
######################################################################
# Only needed if you have not already run the class script: 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
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

HEP_dis <- nonzero_degree_distribution(HEP) 
HEP_dom <- degree_domain(HEP)

######################################################################
# Make linear plot, and see how much degree_domain matters

# Skip this: use one pane and flip between to animate differences
# new_window("HEP Plots", 12, 12)
# par(mfrow=c(1,1)) # if needed 

plot(         HEP_dis, main="HEP Lin Lin",  
     xlab="k", ylab="p(k)")
plot(HEP_dom, HEP_dis, main="HEP Lin Lin Adjusted",  
     xlab="k", ylab="p(k)")

# ***** Is the adjustment needed in this case? Why or why not? 

######################################################################
# Make log-log plots. 

plot(         HEP_dis, main="HEP Log Log",  
     xlab="k", ylab="p(k)", log="xy")
plot(HEP_dom, HEP_dis, main="HEP Log Log Adjusted",  
     xlab="k", ylab="p(k)", log="xy")

# ***** Is the adjustment needed in this case? Why or why not? 
#       Look at the RStudio Global Environment Values, or: 

head(HEP_dis)

# In the unadjusted case, the value for k=1 has been incorrectly
# placed above x axis 2. This was also true in the first example, but
# the scale was too large to see the tiny shift. When we plot log-log,
# the lower end of the x axis is stretched out, so we can see the
# shift!

######################################################################
# Make Cumulative Degree Distribution plot

HEP_cdd <- degree_distribution(HEP, cumulative=TRUE)
plot(HEP_dom, HEP_cdd, main="HEP Log Log Cumulative Adjusted",  
     xlab="k", ylab="CDF p(k)", log="xy")

# **** Notice that the first two values in the cumulative distribution
#      shown below are 1.0: why is this the case? 

head(HEP_cdd)

# ***** But why are they not both plotted? Here is a clue

head(HEP_dom) # and look at the warning 

######################################################################
# Make Binned Degree Distribution plot 

HEP.bh <- binned_histogram(degree(HEP))
plot(HEP.bh$mids, HEP.bh$density, main="HEP Log Log Binned", 
     xlab="k", ylab="p(k)", log="xy", type="b")

# Compare the shape to the previous log log cumulative plot. 

######################################################################
# How would you handle a directed graph? 
# Compute and plot In-degree and Out-degree separately 

new_window("HEP In/Out Degree Distribution 1", 12, 12)
par(mfrow=c(2,2))
par(cex.axis=1.2, cex.lab=1.2)

# Compute the distributions we'll need 

HEP_in_dom  <- degree_domain(HEP, mode="in")
HEP_out_dom <- degree_domain(HEP, mode="out")
HEP_in_dis  <- nonzero_degree_distribution(HEP, mode="in")
HEP_out_dis <- nonzero_degree_distribution(HEP, mode="out")
HEP.in.cdd  <- degree_distribution(HEP, mode="in", cumulative=TRUE)
HEP.out.cdd <- degree_distribution(HEP, mode="out", cumulative=TRUE)

# Lin Lin In and Out

plot(HEP_in_dom,  HEP_in_dis,  main="HEP In-Degree Lin Lin",  
     xlab="k", ylab="p(k)")
plot(HEP_out_dom, HEP_out_dis, main="HEP Out-Degree Lin Lin",  
     xlab="k", ylab="p(k)")

# Log Log In and Out

plot(HEP_in_dom,  HEP_in_dis,  main="HEP In-Degree Log Log",  
     xlab="k", ylab="p(k)", log="xy")
plot(HEP_out_dom, HEP_out_dis, main="HEP Out-Degree Log Log",  
     xlab="k", ylab="p(k)", log="xy")

# ***** Why is the x axis larger on one of them? You can discuss in
#       Analysis 4

max(degree(HEP, mode="in"))
max(degree(HEP, mode="out"))

# **** And why do they curve down on the left? We'll find out next week!!!

# Without linear binning: 

new_window("HEP In/Out Degree Distribution 2", 12, 12)
par(mfrow=c(2,2))
par(cex.axis=1.2, cex.lab=1.2)

# Cumulative In and Out

plot(HEP_in_dom, HEP.in.cdd, main="HEP In-Degree Cumulative", 
     xlab="k", ylab="CDF p(k)", log="xy")
plot(HEP_out_dom, HEP.out.cdd, main="HEP Out-Degree Cumulative",  
     xlab="k", ylab="CDF p(k)", log="xy")

# Binned 

HEP.bh.in <- binned_histogram(nonzero_degrees(HEP, mode="in"))
plot(HEP.bh.in$mids, HEP.bh.in$density, main="HEP In-Degree Binned", 
     xlab="k", ylab="p(k)", log="xy", type="b")

HEP.bh.out <- binned_histogram(nonzero_degrees(HEP, mode="out"))
plot(HEP.bh.out$mids, HEP.bh.out$density, main="HEP Out-Degree Binned", 
     xlab="k", ylab="p(k)", log="xy", type="b")

# Line the windows up side by side to compare. 

# How we return to R Studio plotting 

dev.list()
dev.set(2)
par(mfrow=c(1,1))

######################################################################
# Pau 