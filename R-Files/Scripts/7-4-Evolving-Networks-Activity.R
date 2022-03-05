######################################################################
# Evolving Networks Class Activity
#
# Overview: Identify features of interest in the WikiVotes in-degree
# distribution; hypothesize about domain processes that might lead to
# these features; and find settings for sample_pa or sample_pa_age
# that match the distribution, thus testing our hypotheses. 
# 
# Feb 25 2021 DS: Split from other script for Spring 2021 ICS 422/622
#
######################################################################
# This is intended to be run after 7-3-Evolving-Networks-Models.R but
# this section re-runs the necessary parts. (Skip if you just ran 7-3)

library(igraph)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

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

# Plotting parameters: adjust for your screen. 
# 8, 4 for Sakamaki D101 class demo; 12, 5 for cinema monitor. 

window.w <- 12
window.h <- 5
cex <- 1.2

######################################################################
# Plot Wiki Votes In-degree and paste results in Google Doc 

# The only difference from 7-3 setup: we'll add distances, as its fast
# for this network:

geo=TRUE

WV <- read_graph("Networks/Wiki-Vote.gml", format="gml")
summary(WV)
new_window("Wiki Votes Outdegree", window.w, window.h)
par(mfrow=c(1,2))

# Outdegree is how many elections one participates in. 

plot_pl_distribution(WV, "Wiki Votes Out", mode="out", 
                     geodesic=geo, cex=cex)

# Indegree is how many people voted in your election (for or against)
new_window("Wiki Votes Indegree", window.w, window.h)
par(mfrow=c(1,2))
plot_pl_distribution(WV, "Wiki Votes In", mode="in", 
                     geodesic=geo, cex=cex)

######################################################################
# Try to model with sample_pa_age and compare to the empirical 
# distribution and metrics. 
######################################################################


######################################################################
# Pau 