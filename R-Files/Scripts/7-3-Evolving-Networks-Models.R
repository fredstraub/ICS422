######################################################################
# Evolving Networks Experiments 
# Template for experimenting with sample_pa and sample_pa_age
# Dan Suthers, February 23, 2017
# August 19, 2018 DS: 
# * Minor updates for current script style and clarity. 
# * Set zero.appeal to 0 now that the bug we found is fixed. 
# * The bug is still present for zero.deg.appeal in sample_pa_age 
# October  3, 2019 DS: 
# * Changed from poweRlaw to regular plotting and fit_power_law
# * Other minor changes for fall class
# October  4, 2019 DS: 
# * Added parameters for window size and cex. 
# February 15 2020 DS: Minor improvements for self-study students 
# Feb 25 2021 DS: Updates for Spring 2021 ICS 422/622
# Feb 24 2022 DS: Updates for Spring 2022 ICS 422/622 (minor)
######################################################################
library(igraph)

setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

# Network we will work with. It does not need many of the adjustments,
# but provides us with a reference point to see what they do. 

IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")

######################################################################
# Utility Plotter 
# We will want to plot and compare to power law a lot, and compute
# metrics. This does two plots: regular with metrics, and CDF with 
# fit_power_law results. 
######################################################################
# g: an igraph 
# title: prefix for main label on the two plots 
# mode: "total", "in", "out"
# geodesic: mean_distance is slow. Set to FALSE to leave it out. 
# cex: affects size of text labels. 

plot_pl_distribution <- function(g, title, mode="all", 
                                 geodesic=FALSE, cex=1.0) {
  # 
  # Distributions and metrics we'll need 
  g.deg   <- degree(g, mode=mode)
  g.dom   <- degree_domain(g, mode=mode)
  g.dis   <- degree_distribution(g, mode=mode)
  g.cum   <- degree_distribution(g, mode=mode, cumulative=TRUE)
  g.plfit <- fit_power_law(g.deg, implementation = "plfit")
  g.trans <- transitivity(g, type="global")
  g.wcc   <- components(g, mode="weak")$no 
  g.scc   <- components(g, mode="strong")$no
  g.dass  <- assortativity_degree(g)
  # The slow one. Expect delay before plot starts. 
  if (geodesic) {g.dist <- mean_distance(g)}
  #
  # Regular distribution with metrics 
  plot(g.dom, g.dis, main=paste(title, "Log"), 
       log="xy", xlab="k", ylab="p(k)", 
       cex.axis=cex, cex.lab=cex)
  legend("topright", bty="n", cex=cex,
         c(paste("|V|=", vcount(g), " |E|=", ecount(g), sep=""),
           paste("Transitivity:", round(g.trans, digits=4)),
           if (geodesic) {
             paste("Mean Geodesic: ", round(g.dist, digits=2))},
           paste("Assortativity:", round(g.dass, digits=4)),
           paste("WCC:", g.wcc, "SCC:", g.scc)
         ))
  #
  # CDF with power law fit.
  plot(g.dom, g.cum, main=paste(title, "CDF"), 
       log="xy", xlab="k", ylab="p(k)",
       cex.axis=cex, cex.lab=cex)
  legend("bottomleft", bty="n", cex=cex,
         c(paste("plfit gamma =", round(g.plfit$alpha, 2)),
           paste("plfit xmin =", g.plfit$xmin),
           paste("plfit KS.stat =", round(g.plfit$KS.stat, 2)),
           paste("plfit KS.p", round(g.plfit$KS.p, 2))))
  legend("topright", bty="n", cex=cex,
         c(paste("k min: ", min(g.deg)),
           paste("k max: ", max(g.deg))))
}

# False for slow computer or in class demo; True to see geodesics. 

geo=FALSE
# geo=TRUE

# Plotting parameters: adjust for your screen. 
# 8, 4 for Sakamaki D101 class demo; 12, 5 for cinema monitor

window.w <- 13
window.h <- 5
cex <- 1.2

######################################################################
# Original network: keep this up for reference
######################################################################

new_window("Internet Routers", window.w, window.h)
par(mfrow=c(1,2))

# This is slow if geo=TRUE.

plot_pl_distribution(IR, "IR", geodesic=geo, cex=cex)

######################################################################
# Fitness 
# I wanted to show the effect of fitness models, but it turns out that
# these are not the same as the Bianconi-Barabasi Model, nor are they
# even process models, so it's hard to compare. They are just ways of
# generating specified graphs. We will skip them today. 
######################################################################

?sample_fitness 
# Problem: this is a non-growing random graph, and does not have an
# independent preferential attachment term, so is not the same as
# the Bianconi-Barabasi Model. We will skip it for now. 

?sample_fitness_pl
# This also generates non-growing random graphs, and we specify gamma. 
# It generate graphs with power law degree distributions, but does NOT
# include growth or preferential attachment. It is not a process model.
# Skip for now 

######################################################################
# Linearity of Attachment 
######################################################################
# We did this in 7-1-Preferential-Attachment-Demo.R, but let's revisit
# it with the new plotter.

?sample_pa 

new_window("Preferential Attachment Models", window.w, window.h)
par(mfrow=c(1,2))

# Sublinear preferential attachment: Barabasi predicts stretched 
# exponential with reduced kmax 

IR.alpha0.8 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=0, 
                         power=0.8)
plot_pl_distribution(IR.alpha0.8, "sample_pa, a=0.8, za=0", 
                     geodesic=geo, cex=cex)

# Linear preferential attachment. Barabasi predicts power law with
# gamma of 3.0.  

IR.alpha1.0 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=0,
                         power=1.0)
plot_pl_distribution(IR.alpha1.0, "sample_pa, a=1.0, za=0", 
                     geodesic=geo, cex=cex)

# Superlinear preferential attachment: Barabasi predicts increased 
# kmax and hub-and-spoke topology. 

IR.alpha1.2 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=0,
                         power=1.2)
plot_pl_distribution(IR.alpha1.2, "sample_pa, a=1.2, za=0", 
                     geodesic=geo, cex=cex)

# After running these three pairs of plots, "animate" the window by 
# flipping back and forth as you compare how values change with alpha. 
# (Command arrow keys on OS X to go left and right.)
# Compare to Internet Routers 

#####################################################################
# Can we find an intermediary? How consistent are multiple runs? 
# Let's try with power (alpha) 1.1 

for (i in 1:5) {
  IR.alpha1.1 <- sample_pa(vcount(IR), directed=FALSE,  
                           m=2, zero.appeal=0,
                           power=1.1)
  plot_pl_distribution(IR.alpha1.1, 
                       paste0("sample_pa #", i, " a=1.1, za=0"), 
                       geodesic=geo, cex=cex)
}

# Clearly we should average many runs before making parameter claims! 

######################################################################
# Exploring effect of Initial Attractiveness
# Try nonzero values of zero.appeal. 
######################################################################

new_window("Initial Attractiveness", window.w, window.h)
par(mfrow=c(1,2))

# Starting with zero.appeal=0 from above for comparison ... 

IR.appeal0.0 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=0)
plot_pl_distribution(IR.appeal0.0, "sample_pa, a=1.0, z.a=0.0", 
                     geodesic=geo, cex=cex)

IR.appeal0.5 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=0.5)
plot_pl_distribution(IR.appeal0.5, "sample_pa, a=1.0, za=0.5", 
                     geodesic=geo, cex=cex)

IR.appeal1.0 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=1.0)
plot_pl_distribution(IR.appeal1.0, "sample_pa, a=1.0, za=1.0", 
                     geodesic=geo, cex=cex)

IR.appeal1.5 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=1.5)
plot_pl_distribution(IR.appeal1.5, "sample_pa, a=1.0, za=1.5", 
                     geodesic=geo, cex=cex)

# Push to extreme to see the effect 

IR.appeal10 <- sample_pa(vcount(IR), directed=FALSE, m=2, zero.appeal=10)
plot_pl_distribution(IR.appeal10, "sample_pa, a=1.0, za=10", 
                     geodesic=geo, cex=cex)

# Again, "animate" by flipping through the plots to compare. 

# ***** Does Initial Attractiveness (zero.appeal) seem to be operating
#       in IR? What explains the low degree saturation for degree 1?

######################################################################
# Aging with sample_pa_age 
# Try changing aging.exp to see how this affects the distribution. 
# We'll set the other parameters to the defaults. 
######################################################################

# This model also includes parameters from the above, but some are
# renamed ("power" is now "pa.exp"). 

?sample_pa_age

new_window("Aging Models", window.w, window.h)
par(mfrow=c(1,2))

# Let's run the range from very negative to very positive. 

# aging.exp = -10
IR.age.n10 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                            aging.exp=-10)
plot_pl_distribution(IR.age.n10, "sample_pa_age, age -10", 
                     geodesic=geo, cex=cex)

# aging.exp = -2
IR.age.n2 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                           aging.exp=-2)
plot_pl_distribution(IR.age.n2, "sample_pa_age, age -2", 
                     geodesic=geo, cex=cex)

# aging.exp = -1
IR.age.n1 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                           aging.exp=-1)
plot_pl_distribution(IR.age.n1, "sample_pa_age, age -1", 
                     geodesic=geo, cex=cex)

# aging.exp = 0
IR.age.p0 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                           aging.exp=0)
plot_pl_distribution(IR.age.p0, "sample_pa_age, age 0", 
                     geodesic=geo, cex=cex)

# aging.exp = 1
IR.age.p1 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                           aging.exp=1)
plot_pl_distribution(IR.age.p1, "sample_pa_age, age +1", 
                     geodesic=geo, cex=cex)

# aging.exp = 2
IR.age.p2 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                           aging.exp=2)
plot_pl_distribution(IR.age.p2, "sample_pa_age, age +2", 
                     geodesic=geo, cex=cex)

# aging.exp = 10
IR.age.p10 <- sample_pa_age(vcount(IR), directed=FALSE, pa.exp=1, m=2, 
                            aging.exp=10)
plot_pl_distribution(IR.age.p10, "sample_pa_age, age +10", 
                     geodesic=geo, cex=cex)

# ***** How does this compare to Barabasi's prediction for v? 
#       Can you explain any discrepancies? 

# sample_pa_age has many parameters and can be used to compare the
# results of simultaneous effects of nonlinear preference, aging, and
# initial appeal based on age as well as degree. It's up to you to
# experiment and see what matches your network. Keep in mind that
# these are random samples and runs may differ: we really should be
# averaging across multiple samples of each.

######################################################################
# Look back through the last three experiments, comparing to the 
# natural IR graph. Is there evidence that IR has
# * Nonlinear preference of attachment? 
# * Initial Appeal? 
# * Positive or negative preference for routers set up earlier (age)?

######################################################################
# Activity: Modeling Wiki Votes in-degree 
# Find settings for sample_pa or sample_pa_age that match the 
# WikiVotes in-degree distribution. 
######################################################################
# To get you started: 

WV <- read_graph("Networks/Wiki-Vote.gml", format="gml")
summary(WV)
new_window("Wiki Votes", window.w, window.h)
par(mfrow=c(1,2))
plot_pl_distribution(WV, "WikiVotes", mode="in", 
                     geodesic=geo, cex=cex)

# What phenomena do you see in the degree distribution that you need
# to model? Which adjustments to the BA model will model them? 

# Try them! 


######################################################################
# Another Activity: 
# Choose two of the parameters (power, zero.appeal, aging.exp) and do
# an experiment where you cross values of each. How do they interact? 
# Which combinations seem to match natural networks? 
######################################################################
# Pau
