######################################################################
# Robustness Simulation Continued: Supercritical Networks 
#
# Change Log
# Nov 21, 2019 DS: Split from 12-1-Robustness-Simulation-Demo.R 
# Nov 30, 2019 DS: Added numcomp_ylim, bigcomp_ylim, and meandist_ylim
#   to show improved plotting. 
# Apr  5 2020 DS Removed sample_fitness as sample_pa gives
#  clearer results. Introduce y axis scaling after attack, where
#  the difference is clearer.
# Apr 15 2021 DS Revising for Spring 2021 ICS 422/622
#  Normalizing ylim; trials parameter. Renaming this as continuation. 
# Apr 12 2022 DS: Minor revisions for ICS 422/622 Spring 2022.
# 
######################################################################
# Set up again 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Or yours 
source("Utility/degree_domain.R")
source("Utility/new_window.R")
source("Utility/robustness_simulation_utility.R")
source("Utility/remove_isolates.R") # This is new 

######################################################################
# Parameters for Synthetic Networks 
# You are encouraged to try different values. 
######################################################################

# |V| should be large to approximate predictions, but small enough to 
# run in class. 100 is fast but too small; 1000 doable. 
# *** NOTE: Small networks may not match theoretical predictions *** 

numV <- 1000 

# Boundary between supercritical and connected regimes for random
# graphs is <k> ~ log(numV). 

log(numV)

# We'll make graphs in the the Supercritical regime (below connected)
# # Make average degree half way between <k> = 1 and <k> ~ log(numV): 

(targetK <- ceiling(log(numV)/2))
numV*targetK # is number of endpoints needed; each edge contributes 2
numE <- ceiling(numV*targetK/2)

# Summary of what we got: 

numV 
numE
log(numV) # boundary.
targetK   # what we wanted 
# Actual <k> we got: 
(kAvg <- 2*numE / numV)

######################################################################
# Make the graphs and check parameters.
######################################################################

# Random network model. 
# ?sample_gnm
g.nm <- sample_gnm(numV, numE)
components(g.nm)$no  
# Try iterating until you get 1 component. 

# It isn't happening! Since we are below Connected regime it is not
# connected. Can we hack this to make it connected? Should we?

# One approach: Add the number of vertices that are isolates; then remove them
summary(g.nm)
(numIso <- length(which(degree(g.nm)==0)))
g.nm <- sample_gnm(numV+numIso, numE) # Can iterate from here if needed
summary(g.nm)
components(g.nm)$no  
g.nm <- remove_isolates(g.nm)
summary(g.nm)
components(g.nm)$no
# (I have code for a more principled approach but it has a bug.)

summary(g.nm)
mean(degree(g.nm))
plot(degree_domain(g.nm), degree_distribution(g.nm), main=g.nm$name)

# sample_pa  model. Always connected, but harder to get numE close
# m is edges added per iteration: each adds 2 endpoints. 
# ?sample_pa

g.pa <- sample_pa(numV, power=1.1, m=round(numE/numV))
g.pa <- as.undirected(g.pa)
components(g.pa)$no
summary(g.pa)
mean(degree(g.pa))
plot(degree_domain(g.pa), degree_distribution(g.pa), main=g.pa$name, log="xy")

# Hub and randomly connected spokes, with spokes meeting kmin. 
# We can construct this via the configuration model, specifying a
# degree sequence where one node has degree numV-1 and the rest have
# kAvg-1 (which we round to integer). If you get an error "sum of
# degrees should be even" just change the hub size by one

g.hs.degrees <- replicate(numV, round(kAvg) - 1) # take off 1 for the hub 
head(g.hs.degrees)
g.hs.degrees[1] <- numV - 1    # -1 or -2 depending on error below
head(g.hs.degrees)

# ?sample_degseq
g.hs <- sample_degseq(g.hs.degrees, method="vl") # Error may be here
summary(g.hs)
mean(degree(g.hs))

# # *** This section only needed for targetK <- log(numV)/2 *** 
# # It's harder to hit the edges value, and hence mean degree. It's
# # possible to fix this by making alternating sequence of ceiling and
# # floor values of kAvg:
# 
# g.hs.degrees <- as.vector(replicate(500, c(floor(kAvg)-1, ceiling(kAvg)-1)))
# g.hs.degrees[1] <- numV - 2 # -1 or -2 depending on error below
# head(g.hs.degrees)
# g.hs <- sample_degseq(g.hs.degrees, method="vl") 
# g.hs$name <- "Robust hub & kmin spoke graph"
# summary(g.hs)
# mean(degree(g.hs))
# components(g.hs)$no # by design
# plot(degree_domain(g.hs), degree_distribution(g.hs), main=g.hs$name, log="xy")

######################################################################
# Run and plot *iterated* simulations for random failure 
######################################################################

# Speed it up by looking at first 10% of nodes; we can do more trials

cutoff <- 0.10 
trials <- 5

# If you prefer you can do this in multiple windows. 
new_window(paste0("Iterated Random Failure: |V|=", numV, " |E|=", numE,
              " <k>=", kAvg), 12, 12)
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)
par(mfrow=c(3, 3))

# Run simulations without iteration plots; plot final results. 

g.nm.failure <- 
  iterated_robustness_simulation(g.nm, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.nm.failure, 
                        paste("G(n,m) Failure,", trials, "trials"),
                        meandist = TRUE)

g.pa.failure <- 
  iterated_robustness_simulation(g.pa, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.pa.failure, 
                        paste("BA Model Failure,", trials, "trials"), 
                        meandist = TRUE)

g.hs.failure <- 
  iterated_robustness_simulation(g.hs, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.hs.failure, 
                        paste("Hub Spoke Failure,", trials, "trials"), 
                        meandist = TRUE)

# Save as image file before running another, with same ylim. 
numcomp_range <- range(g.nm.failure$numcomp, 
                       g.pa.failure$numcomp, 
                       g.hs.failure$numcomp)
bigcomp_range <- range(g.nm.failure$bigcomp, 
                       g.pa.failure$bigcomp, 
                       g.hs.failure$bigcomp)
meandist_range <- range(na.omit(g.nm.failure$meandist), 
                        na.omit(g.pa.failure$meandist), 
                        na.omit(g.hs.failure$meandist))
png(paste0("RandomFailureIterated-", numV, "-", numE, ".png"), 700, 700)
par(mfrow=c(3, 3))
plot_robustness_results(g.nm.failure, g.nm$name, meandist = TRUE, 
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)          
plot_robustness_results(g.pa.failure, g.pa$name, meandist = TRUE, 
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)
plot_robustness_results(g.hs.failure, g.hs$name, meandist = TRUE, 
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)
dev.off()

# ***** Discuss results! 

######################################################################
# Run and plot simulations for ATTACK
######################################################################

# Staying in R Studio .. 
new_window(paste0("Iterated Attack: |V|=", numV, " |E|=", numE,
             " <k>=", kAvg), 12, 12)
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)
par(mfrow=c(3, 3))

# Run and plot the simulations

g.nm.attack <- 
  iterated_robustness_simulation(g.nm, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.nm.attack, 
                        paste("G(n,m) Attack,", trials, "trials"), 
                        meandist = TRUE)

g.pa.attack <- 
  iterated_robustness_simulation(g.pa, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.pa.attack, 
                        paste("BA Model Attack,", trials, "trials"), 
                        meandist = TRUE)

g.hs.attack <- 
  iterated_robustness_simulation(g.hs, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.hs.attack, 
                        paste("Hub Spoke Attack,", trials, "trials"), 
                        meandist = TRUE)

# Save as image file before running another, with same ylim.
numcomp_range <- range(g.nm.attack$numcomp, 
                       g.pa.attack$numcomp, 
                       g.hs.attack$numcomp)
bigcomp_range <- range(g.nm.attack$bigcomp, 
                       g.pa.attack$bigcomp, 
                       g.hs.attack$bigcomp)
meandist_range <- range(na.omit(g.nm.attack$meandist), 
                        na.omit(g.pa.attack$meandist), 
                        na.omit(g.hs.attack$meandist))
png(paste0("AttackIterated-", numV, "-", numE, ".png"), 700, 700)
par(mfrow=c(3, 3))
plot_robustness_results(g.nm.attack, g.nm$name, meandist = TRUE, 
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)
plot_robustness_results(g.pa.attack, g.pa$name, meandist = TRUE, 
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)
plot_robustness_results(g.hs.attack, g.hs$name, meandist = TRUE,
                        numcomp_ylim=numcomp_range, 
                        bigcomp_ylim = bigcomp_range, 
                        meandist_ylim = meandist_range)
dev.off()

######################################################################
# Star graph 
######################################################################
# Something I added later out of curiosity. 

# Star graph: cannot simultaneously have the desired number of edges and vertices. 

?make_star
g.star <- make_star(numV, mode="undirected")
components(g.star)$no
summary(g.star)
mean(degree(g.star))
# plot(degree_distribution(g.star), main=g.star$name, log="xy")

new_window("Star Robustness", 12, 4)
par(mfrow=c(1,3))
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)

# Here we see it matters when the hub happens to be hit ... 

star.failure <- 
  iterated_robustness_simulation(g.star, meandist=TRUE, attack=FALSE, 
                                 trials=3, plottrials=TRUE)

# ... and the "average" results don't make much sense! 

plot_robustness_results(star.failure, "Star Failure 3 Trials", meandist = TRUE)

# But here the hub gets hit first every time: result always the same 
star.attack <- 
  iterated_robustness_simulation(g.star, meandist=TRUE, attack=TRUE, 
                                 trials=3, plottrials=TRUE)

######################################################################
# Pau 
