######################################################################
# Comparing IR Models: Class Demonstration and HW Analysis 3 solution
#   How well do the different graph models model the natural network? 
#   This is Analysis 3 solution with BA preferential attachment model
#   added for comparison
# 
# Note: rewiring IR and computing mean distances take a few minutes
# 
# Dan Suthers, February 21, 2017
# Feb 15, 2018 to match revisions to HW 4 (now 5) 
# August 19, 2018 DS: 
# * Minor updates for current script style and clarity
# * Finished what I think were the intended revisions for HW 4. 
# * Added zero.appeal = 0 now that the bug was fixed. 
# October  1 2019 DS: Updates for class 
# February 15 2020 DS: Minor improvements for self-study students 
# Feb 23 2021 DS: Updates for Spring 2021 ICS 422/622. 
# * Removed zero.appeal as the new implementation changes behavior; 
#   need to investigate further. 
# Feb 22 2022 DS: Updates for Spring 2022 ICS 422/622
# * Restored zero.appeal: now understand it is for undirected only
# * using quickplot and quicklegend 
#
######################################################################
# If continuing previous demos you only need the tibble below 

library(igraph)
library(tibble) # load this

setwd("/Users/fred/Github/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")
summary(IR)

######################################################################
# Some plotting utilities adapted from 7-2 (definitions differ)

quickplot <- function(g, main=g$name, cumulative=TRUE) {
  plot(degree_domain(g), 
       degree_distribution(g, cumulative=cumulative), 
       log="xy", xlab="k", ylab="p(k)", 
       main=main) 
}

# Combines some elements of 7-2 legends; works with any graph 

quicklegend <- function(g) {
  legend("topright", bty="n", cex=1.2, 
         c(paste("|V| =", vcount(g)), 
           paste("|E| =", ecount(g)), 
           paste("kmin =", min(degree(g))), 
           paste("kmax =", max(degree(g)))))
}

plfit_legend <- function(g) {
  g.plfit <- fit_power_law(degree(g), implementation="plfit")
  legend("bottomleft", bty="n", cex=1.2, 
         c(paste("plfit xmin =", g.plfit$xmin),
           paste("plfit gamma = ", round(g.plfit$alpha, digits=2)),
           paste("plfit KS stat = ", round(g.plfit$KS.stat, digits=3)),
           paste("plfit KS p = ", round(g.plfit$KS.p, digits=3))
         ))
}

######################################################################
# Assignment 3 solution with the sample_pa model added 
######################################################################
# Construct the models 

IR.gnm <- sample_gnm(vcount(IR), ecount(IR))
IR.config <- sample_degseq(degree(IR), method="vl")

# Reduce to 100 for class: use 1000 or more outside class
# (1000 is only 30 seconds on my machine)

IR.rewired <- rewire(IR, keeping_degseq(niter=ecount(IR)*100)) 

IR.sw <- sample_smallworld(1, 22963, 2, 0.4) # the best fit I had

# We just did nonlinear attachment power=1.2 but it seemed a bit strong: 

IR.ba <- sample_pa(vcount(IR), directed=FALSE, zero.appeal=0, 
                   power=1.1, m=round(ecount(IR)/vcount(IR)))

# Collect the natural graph and models in a list, and make row names 

models <- list(IR,      IR.gnm, IR.config, IR.rewired, IR.sw, IR.ba)
mnames <- c("Routers", "G(n,m)", "Config", "Rewire", "SmallW", "BA/PA")

# Compute metrics and put in tibble.
# Mean Distance is commented out for class: about 2 minutes on my
# 2018 MBP. See table of results below. 

date()
IR.metrics <- tibble(
  Model         = mnames, 
  V             = sapply(models, vcount),
  E             = sapply(models, ecount),
  # meandist      = sapply(models, mean_distance),
  components    = sapply(models, count_components), 
  transitivity  = sapply(models, transitivity, type="global"), 
  degreeassort  = sapply(models, assortativity_degree)
)
date()

##############################
# Metrics Results

view(IR.metrics) # one option
IR.metrics 

# Results from previous run with mean distances 
# Model        V     E meandist components transitivity degreeassort
# Routers  22963 48436     3.84          1     0.0111       -0.198  
# G(n,m)   22963 48436     7.12        345     0.000162      0.00289
# Config   22963 48436     3.65          1     0.0129       -0.187  
# Rewire   22963 48436     3.63        299     0.0137       -0.187  
# SmallW   22963 45926     7.79        122     0.0201        0.00607
# BA/PA    22963 45923     4.70          1     0.000616     -0.0510 

##############################
# Degree Distributions: We know the Configuration Model and Rewiring 
# will have the same degree distribution, so don't bother plotting them. 

new_window("Internet Routers Model Degree Distributions", 12, 12)
par(mfrow=c(2,2), cex.main=1.5, cex.axis=1.5, cex.lab=1.5)

quickplot(IR,     main="Internet Routers")
quicklegend(IR)
plfit_legend(IR)
quickplot(IR.ba,  main="Routers BA Model")
quicklegend(IR.ba)
plfit_legend(IR.ba)
quickplot(IR.gnm, main="Routers G(n,m)")
quicklegend(IR.gnm)
quickplot(IR.sw,  main="Routers Small World")
quicklegend(IR.sw)

# ***** Compare BA to Routers and other models: which matches best? 

# ***** Does the BA model have any advantage over the Configuration 
#       model? If so, what is it? 

######################################################################
# Pau
