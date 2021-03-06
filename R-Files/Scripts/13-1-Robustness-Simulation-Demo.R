######################################################################
# Robustness Simulation Demonstration Part 1: Connected Regime 
# Dan Suthers, November 9, 2016
# 
# A demonstration of robustness_simulation_utility.R, which should
# be reviewed and loaded first. This uses graphs in the Connected
# regime for random graphs: 13-2 will use Supercritical graphs. 
# 
# First we demonstrate the simulation on the power grid network. 
# 
# Then we construct three kinds of networks and compare how they 
# deteriorate under random deletion and attack. The types are: 
# * Random G(n,m) (from sample_gnm)
# * Scale Free (from sample_fitness_pl)
# * kmin + hub (the recommended robust design)
# One can set network parameters: |V|, |E|
# 
# Change Log
# April 10, 2018 DS: various Updates 
# Sep 23, 2018 DS: 
# * Minor updates for current script style and clarity
# Nov 21, 2019 DS: Updated for class 2019
# Nov 30, 2019 DS: Added numcomp_ylim, bigcomp_ylim, and meandist_ylim
#   to show improved plotting. 
# Apr  5 2020 DS: Removed sample_fitness as sample_pa gives
#  clearer results. Introduce y axis scaling after attack, where
#  the difference is clearer. 
# Apr 15 2021 DS: Revising for Spring 2021 ICS 422/622
#  Added initial Power Grid example. Removed gamma, no longer used.
# Apr 12 2022 DS: Minor revisions for ICS 422/622 Spring 2022. 
# 
######################################################################

library(igraph)

# Load simulation functions 
setwd("~/Desktop/Network-Science-Demos") # Or yours 
source("Utility/degree_domain.R")        # Optional 
source("Utility/new_window.R")
source("Utility/robustness_simulation_utility.R")

######################################################################
# Demonstration on natural network: Read robustness_simulation_utilit
# source code for each function before demonstrating.
######################################################################

# What would it take to bring down the power grid, which as we 
# saw in a previous assignment trades off redundancy with cost? 

PG <- read_graph("Networks/power_grid.gml", format="gml")
summary(PG)

# Take a look at visualization in Gephi: Western-States-Power-Grid.pdf

# Single simulation of random failure or attack on a network
# (No mean distance since it would be VERY slow.)

PG.sim <- robustness_simulation(PG) 
head(PG.sim)
tail(PG.sim)

# Visualizing results 

plot_robustness_results(PG.sim, name="Power Grid")

# ***** Why does the number of components increase and then decrease? 

# ***** Explain the gradual drop and sudden spike in the second plot. 

# Multiple trials of robustness_simulation, returning averaged metrics

PG.itsim <- iterated_robustness_simulation(PG, trials=5)

# Visualizing results 

plot_robustness_results(PG.itsim, name="Power Grid Iterated")

# Generally it smooths out the results. 

######################################################################
# Simulations on Synthetic Networks       *** New Video *** 
######################################################################
# Parameters: You are encouraged to try different values. 

# |V| should be large to approximate predictions, but small enough to 
# run in class. 100 is fast but too small; 1000 doable. 
# *** NOTE: Small networks may not match theoretical predictions *** 

numV <- 1000 

# Boundary between supercritical and connected regimes for random
# graphs is <k> ~ log(numV). Let's try below and above this boundary.

log(numV)

# Above the boundary, in the Connected Regime: <k> ~ 2log(numV)
# Suppose we want twice as many edges as needed for the boundary of
# supercritical to connected. 

numE <- ceiling(log(numV)) * numV

# Summary of what we got: 

numV 
numE
log(numV) # boundary. Check that <k> > log(numV): 
(kAvg <- round(2*numE / numV))

######################################################################
# Make the graphs and check parameters. 
######################################################################
# new_window("Degree Distributions of Test Graphs")

####################
# Random networks. Let's sample until we get a single component. 

g.nm <- sample_gnm(numV, numE)
components(g.nm)$no  # Repeat above until this is 1. 
summary(g.nm)
mean(degree(g.nm))
plot(degree_domain(g.nm), degree_distribution(g.nm), main=g.nm$name)

####################
# Scale free networks. Generated by Preferential attachment model 
# with superlinear attachment (hubs more likely)

g.pa <- sample_pa(numV, power=1.2, m=ceiling(kAvg/2))
components(g.pa)$no # by design 
summary(g.pa)
mean(degree(g.pa))
plot(degree_domain(g.pa), degree_distribution(g.pa), 
     main=g.pa$name, log="xy")

####################
# Hub and spoke, with spokes meeting kmin. We can construct this via 
# the configuration model, specifying a degree sequence where one node
# has degree numV-1 and the rest have kAvg-1.

# First I'll plot a small one so we can see what these look like. 
hs.degrees <- replicate(100, 5) # You can try other odd values 
head(hs.degrees)
hs.degrees[1] <- 99 # define the hub
head(hs.degrees)
# ?sample_degseq # reminder
hs <- sample_degseq(hs.degrees, method="vl")
summary(hs)
mean(degree(hs))
plot(hs, vertex.size=sqrt(degree(hs)), vertex.label=NA)

# Now here is the real graph. If you get an error "sum of degrees
# should be even" just change the hub size by one
g.hs.degrees <- replicate(numV, kAvg-1) # take off 1 for the hub 
head(g.hs.degrees)
g.hs.degrees[1] <- numV - 1    # -1 or -2 depending on error below
head(g.hs.degrees)
g.hs <- sample_degseq(g.hs.degrees, method="vl") # Error may be here
g.hs$name <- "Robust hub & kmin spoke graph"
summary(g.hs)
components(g.hs)$no # by design
mean(degree(g.hs)) # close enough! 
plot(degree_domain(g.hs), degree_distribution(g.hs), 
     main=g.hs$name, log="xy")

# ***** Is something wrong with this plot? 

######################################################################
# Run and plot *iterated* simulations for random failure
######################################################################

# If you prefer you can do this in your R Studio window. 
new_window(paste0("Random Failure: |V|=", numV, " |E|=", numE, 
              " <k>=", kAvg), 12, 12) 
par(mfrow=c(3, 3))
# This is so we can see the labels 
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)

# Run simulations without iteration plots; plot final results. 
cutoff <- 1.0
trials <- 5 

g.nm.failure <- 
  iterated_robustness_simulation(g.nm, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.nm.failure, 
                        paste("G(n,m) Failure,", trials, "trials"),
                        meandist = TRUE)

# ***** Interpret. Are the results consistent with Barabasi? 

g.pa.failure <- 
  iterated_robustness_simulation(g.pa, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.pa.failure, 
                        paste("BA Model Failure,", trials, "trials"), 
                        meandist = TRUE)

# ***** How do these results differ? (Start next while discussing)

g.hs.failure <- 
  iterated_robustness_simulation(g.hs, meandist=TRUE, attack=FALSE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.hs.failure, 
                        paste("Hub Spoke Failure,", trials, "trials"), 
                        meandist = TRUE)

# ***** How do the three networks compare? (Don't ignore y axis)

######################################################################
# Run and plot simulations for ATTACK
######################################################################

new_window(paste0("Iterated Attack: |V|=", numV, " |E|=", numE, 
                  " <k>=", kAvg), 12, 12)
par(mfrow=c(3, 3))
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)

# Run and plot the simulations. 
# ***** As they run: what do you expect? 

g.nm.attack <- 
  iterated_robustness_simulation(g.nm, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.nm.attack, 
                        paste("G(n,m) Attack,", trials, "trials"), 
                        meandist = TRUE)

# ***** Compare those to Random Failure: any difference? 

g.pa.attack <- 
  iterated_robustness_simulation(g.pa, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.pa.attack, 
                        paste("BA Model Attack,", trials, "trials"), 
                        meandist = TRUE)

# ***** (Start the next and discuss:) How about here: any difference? 

g.hs.attack <- 
  iterated_robustness_simulation(g.hs, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=FALSE, 
                                 cutoff=cutoff)
plot_robustness_results(g.hs.attack, 
                        paste("Hub Spoke Attack,", trials, "trials"), 
                        meandist = TRUE)

# ***** Why do some runs take so much longer than others? 

# If we don't see the dramatic dropoff Barabasi predicts for scale
# free networks, look at the largest node sizes:

head(sort(degree(g.pa), decreasing=TRUE), 20) 

# These are small networks: hubs would be more accentuated in larger. 

######################################################################
# Plotting on comparable scales 
######################################################################
# Plotting the different network results on different y axis scales 
# makes it misleading to visually compare the results. We should 
# normalize the y axis for a given variable to include the minimum 
# and maximum values for that variable across all runs, all graphs. 

# Find the minimum and maximum values for each variable. 
?range
numcomp_range <- range(g.nm.attack$numcomp, 
                       g.pa.attack$numcomp, 
                       g.hs.attack$numcomp)
numcomp_range
bigcomp_range <- range(g.nm.attack$bigcomp, 
                       g.pa.attack$bigcomp, 
                       g.hs.attack$bigcomp)
bigcomp_range
meandist_range <- range(na.omit(g.nm.attack$meandist), 
                        na.omit(g.pa.attack$meandist), 
                        na.omit(g.hs.attack$meandist))
meandist_range

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

# ***** Compare to previous set: what looked similar but turns out
#       to be different? 

######################################################################
# Plotting Multiple Trials 
######################################################################
# This section is optional, but shows how to plot each trial and
# also how to use cutoff to zoom in on early behavior. 

cutoff <- 0.10 # speed it up by looking at first 10% of nodes 
trials <- 5 

# ===== Random network =====
# We'll plot each trial, so start a new window for each 

new_window("G(n,m) Attack", 12, 4)
par(mfrow=c(1,3))
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)

# Run the attack, plotting each trial. 

nm.attack <- 
  iterated_robustness_simulation(g.nm, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=TRUE, 
                                 cutoff=cutoff)

# Plot final results 

plot_robustness_results(nm.attack, 
                        paste("G(n,m) Attack", trials, "trials"), 
                        meandist = TRUE)

# How to "Zoom In" on early behavior 

plot_robustness_results(nm.attack[1:20,], 
                        paste("Early G(n,m) Attack,", trials, "trials"), 
                        meandist = TRUE)

# ===== Scale Free (Preferential Attachment) =====

new_window("Scale Free Attack", 12, 4)
par(mfrow=c(1,3))
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)
pa.attack <- 
  iterated_robustness_simulation(g.pa, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=TRUE, 
                                 cutoff=cutoff)
plot_robustness_results(pa.attack, 
                        paste("Scale Free PA Attack,", trials, "trials"),
                        meandist = TRUE)
plot_robustness_results(pa.attack[1:20,], 
                        paste("Early Scale Free PA Attack,", trials, "trials"),
                        meandist = TRUE)

# We can probably see that there are about 5-6 hubs, as shown above. 

# ===== Optimized hub and spoke ======

new_window("Hub and Spoke Attack", 12, 4)
par(mfrow=c(1,3))
par(cex.main=1.5, cex.sub=2.0, cex.axis=1.5, cex.lab=1.5)
hs.attack <- 
  iterated_robustness_simulation(g.hs, meandist=TRUE, attack=TRUE, 
                                 trials=trials, plottrials=TRUE, 
                                 cutoff=cutoff)
plot_robustness_results(hs.attack, 
                        paste("Hub and Spoke Attack,", trials, "trials"),
                        meandist = TRUE)
plot_robustness_results(hs.attack[1:20,], 
                        paste("Early Hub and Spoke Attack,", trials, "trials"), 
                        meandist = TRUE)

# ***** Why is it always the same?

# With more time, we'd run this across many more trials.

######################################################################
# Activity: see 13-2-Robustness-Simulation-Activity.R
######################################################################
# Pau 











# Answer: 
numE <- ceiling(log(numV)) * numV
