######################################################################
# Sampling the Power Law Distribution 
# Using sample_fitness_pl to generate power law distributions.  
# (This model is not as useful as generative models to come later, 
# but does serve to illustrate the effect of gamma.)
# Dan Suthers,  September 2016
# February 6, 2018 DS: modified after class for spring 2018 course. 
# August 13, 2018 DS: 
#  * Minor updates for current script style and clarity
#  * Also added question about anomalous plot
# Sep 24, 2019 DS: Updates for fall 2019
# Feb 08, 2020 DS: Cleaning up for self study use. 
# Feb 17, 2021 DS: Adjustments for Spring 2021 ICS 422/622. 
#  * Removed unnecessary details about the correction. 
#  * Added data.frame to collect metric results
######################################################################
# If you haven't done it yet today ... 

library(igraph)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")
IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")

######################################################################
# Basics: sample_fitness_pl 
######################################################################
# The sample_fitness_pl method generates power law distributions. It
# is based on sample_fitness, a generalization of preferential
# attachment where the preference can be based on any specified
# "fitness". Growth is not modeled. We will discuss these concepts in
# a future class demo, and for now just use sample_fitness_pl to
# generate distributions that approximate the power law with different
# exponents but having the same number of vertices and edges as IR. We
# will turn off finite.size.correction to produce results that better
# illustrate the points I am making here.

?sample_fitness_pl 

# Try gamma for different regimes to see which fits IR the best:

# Boundary of Anomalous Regime gamma = 2.0 

PL.2.0 <- sample_fitness_pl(vcount(IR), ecount(IR), 2, 
                            finite.size.correction = FALSE)  

# Ultra-Small World gamma = 2.5

PL.2.5 <- sample_fitness_pl(vcount(IR), ecount(IR), 2.5, 
                            finite.size.correction = FALSE)

# Critical Point gamma = 3.0 

PL.3.0 <- sample_fitness_pl(vcount(IR), ecount(IR), 3.0, 
                            finite.size.correction = FALSE)

# Plots: you can stay in RStudio window, but you will lose detail. 
# I'll do it in a new window to show the detail: 

new_window("Internet Routers Power Law Models", 12, 12)

# Run this even if plotting in RStudio: 

par(mfrow=c(2,2))
par(cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

# Log log plots (zero values will be filtered)

plot(degree_domain(IR),
     degree_distribution(IR),     main="Internet Routers Log Log", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(PL.2.0),
     degree_distribution(PL.2.0), main="sample_fitness_pl, 2.0 Log Log",   
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(PL.2.5),
     degree_distribution(PL.2.5), main="sample_fitness_pl, 2.5 Log Log",
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(PL.3.0),
     degree_distribution(PL.3.0), main="sample_fitness_pl, 3.0 Log Log",
     log="xy", xlab="k", ylab="p(k)")
     
# ***** At first glance, which seems to fit IR the best? 

# Cumulative Log-Log

plot(degree_domain(IR),
     degree_distribution(IR,     cumulative=TRUE), 
     main="Routers Cumulative", log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(IR))), 
       bty="n", cex=1.5)

plot(degree_domain(PL.2.0),
     degree_distribution(PL.2.0, cumulative=TRUE), 
     main="sample_fitness_pl, 2.0 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.2.0))), 
       bty="n", cex=1.5)

plot(degree_domain(PL.2.5),
     degree_distribution(PL.2.5, cumulative=TRUE), 
     main="sample_fitness_pl, 2.5 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.2.5))), 
       bty="n", cex=1.5)

plot(degree_domain(PL.3.0),
     degree_distribution(PL.3.0, cumulative=TRUE), 
     main="sample_fitness_pl, 3.0 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.3.0))), 
       bty="n", cex=1.5)

# ***** Why does PL.2.0 have a larger x axis than the others? 

# Notice that PL.2.5 and PL.3.0 curve down on the left. This will be
# explained next week (it has to do with the lack of growth), but for
# now we can assume we match the power law above some k-min.

# 2.0 appears to have the closest degree distribution match. 
# fit_power_law or poweRlaw will give us more precise fit tests. 

###############
# Other metrics 
# How well are the other two metrics matched? (Caveat: these are 
# single random samples: should average across multiple samples.) 

# First compare distances. 
# ***** Don't run this in class: these are slow. Why are they slow? 

# data.frame(
#         graph = "<d>",
#         IR = mean_distance(IR),          # 3.842426
#         PL.2.0 = mean_distance(PL.2.0),  # 3.22189,  or 3.210103 uncorrected
#         PL.2.5 = mean_distance(PL.2.5),  # 5.52108,  or 4.488724 uncorrected
#         PL.3.0 = mean_distance(PL.3.0)   # 5.472358, or 5.495619
# )

# graph       IR  PL.2.0   PL.2.5   PL.3.0
# 1   <d> 3.842426 3.22802 4.486981 5.533849

# ***** Can you explain why PL.2.0 has the shortest distance? 

# Compare to expected small world distance ln(N)/ln(<k>) for random
# graphs and ultra-small world distances ln(ln(N)) for 2 < gamma 3

log(vcount(IR)) / log(mean(degree(IR))) 
log(log(vcount(IR)))

# Transitivity or global clustering coefficient. 

data.frame(graph  = "CC",
           IR     = transitivity(IR, type = "global"),
           PL.2.0 = transitivity(PL.2.0, type = "global"),
           PL.2.5 = transitivity(PL.2.5, type = "global"),
           PL.3.0 = transitivity(PL.3.0, type = "global"))

# Again 2.0 has the best match.  

# Degree Assortativity 

data.frame(graph = "r", 
           IR    = assortativity_degree(IR),
           PL2.0 = assortativity_degree(PL.2.0),
           PL2.5 = assortativity_degree(PL.2.5), 
           PL3.0 = assortativity_degree(PL.3.0))

# ***** Why does negative assortativity make sense for low gamma? 

# Punchline: we have kept vcount and ecount the same: these
# differences are solely due to changes in degree distribution
# controlled by the exponent (Barabasi's gamma). This shows the
# importance of degree distribution.

################################################################
# Entering the Random Realm 
################################################################
# We'll try gamma higher than can be verified in natural systems 
# just to see the similarity to random. 
# We do not need finite.size.correction = FALSE since this 
# only applies for gamma <= 3. 

PL.3.5 <- sample_fitness_pl(vcount(IR), ecount(IR), 3.5)
PL.4.0 <- sample_fitness_pl(vcount(IR), ecount(IR), 4.0)
PL.5.0 <- sample_fitness_pl(vcount(IR), ecount(IR), 5.0)
Gnm    <- sample_gnm(vcount(IR), ecount(IR))

new_window("High Gamma and Random Models", 12, 12)
par(mfrow=c(2,2), cex.main=1.2, cex.axis=1.2, cex.lab=1.2)

# Linear: Notice the progression towards G(n,m) in both shape of curve
# and size of largest hub

plot(degree_domain(PL.3.5), 
     degree_distribution(PL.3.5), main="sample_fitness_pl, 3.5", 
     xlab="k", ylab="p(k)")
plot(degree_domain(PL.4.0), 
     degree_distribution(PL.4.0), main="sample_fitness_pl, 4.0", 
     xlab="k", ylab="p(k)")
plot(degree_domain(PL.5.0), 
     degree_distribution(PL.5.0), main="sample_fitness_pl, 5.0", 
     xlab="k", ylab="p(k)")
plot(degree_domain(Gnm), 
     degree_distribution(Gnm),    main="G(n,m)",         xlab="k", 
     ylab="p(k)")

# Log log (quick look)

plot(degree_domain(PL.3.5), 
     degree_distribution(PL.3.5), main="sample_fitness_pl, 3.5 Log Log", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(PL.4.0), 
     degree_distribution(PL.4.0), main="sample_fitness_pl, 4.0 Log Log", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(PL.5.0), 
     degree_distribution(PL.5.0), main="sample_fitness_pl, 5.0 Log Log", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(Gnm), 
     degree_distribution(Gnm),    main="G(n,m) Log Log",
     log="xy", xlab="k", ylab="p(k)")

# Cumulative Log-Log (we'll compare this one)

plot(degree_domain(PL.3.5), 
     degree_distribution(PL.3.5, cumulative=TRUE), 
     main="sample_fitness_pl, 3.5 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.3.5))), 
       bty="n", cex=1.5)

plot(degree_domain(PL.4.0), 
     degree_distribution(PL.4.0, cumulative=TRUE), 
     main="sample_fitness_pl, 4.0 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.4.0))), 
       bty="n", cex=1.5)

plot(degree_domain(PL.5.0), 
     degree_distribution(PL.5.0, cumulative=TRUE), 
     main="sample_fitness_pl, 5.0 Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(PL.5.0))), 
       bty="n", cex=1.5)

plot(degree_domain(Gnm), 
     degree_distribution(Gnm,    cumulative=TRUE), 
     main="G(n,m) Cumulative", 
     log="xy", xlab="k", ylab="CDF(p(k))")
legend("topright", paste("k-max =", max(degree(Gnm))), 
       bty="n", cex=1.5)

# Now bring up the window for the lower gamma plots, put it side
# by side with the ones we just did, and compare. 
# ***** Where in the sequence does Internet Routers fall? 
# ***** Can you explain the changes in the plots? 

###############
# Other metrics 
# Mean distance trends towards random graph. Compare to ln(N) and
# ln(N)/ln(<k>), the expected small world distances for gamma > 3 and
# for random graphs respectively.

log(vcount(IR))
log(vcount(IR)) / log(mean(degree(IR)))

# These are slow; don't run them in class demo 
# data.frame(graph  = "<d>", 
#            PL.3.5 = mean_distance(PL.3.5), # 6.12392
#            PL.4.0 = mean_distance(PL.4.0), # 6.49623
#            PL.5.0 = mean_distance(PL.5.0), # 6.828254
#            "G(n,m)" = mean_distance(Gnm))  # 7.109501
#  graph  PL.3.5   PL.4.0   PL.5.0   G.n.m.
#    <d>  6.143134 6.465618 6.823471 7.107662

# Clustering coefficient is very low, as expected for random graphs. 

data.frame(graph = "CC", 
           PL.3.5 = transitivity(PL.3.5, type="global"), 
           PL.4.0 = transitivity(PL.4.0, type="global"), 
           PL.4.5 = transitivity(PL.5.0, type="global"), 
           G.n.m  = transitivity(Gnm,    type="global"))

# Assortativity becomes more neutral: the graph lacks variation in
# degree.

data.frame(graph = "r", 
           PL.3.5 = assortativity_degree(PL.3.5), 
           PL.4.0 = assortativity_degree(PL.4.0), 
           PL.4.5 = assortativity_degree(PL.5.0),
           G.n.m  = assortativity_degree(Gnm))

# Keep in mind that random samples may vary, and we should be taking
# many samples of each model and averaging the metrics across the
# samples. It would be interesting to write some code that plots these
# more systematically as gamma varies, using multiple random samples.

# How we return to R Studio plotting 

dev.list()
dev.set(2)
par(mfrow=c(1,1))

#################################################################
# Analysis HW: Find a good visual fit for another large graph  
################################################################# 
# Pau 