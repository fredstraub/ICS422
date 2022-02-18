#####################################################################
# Demonstration of fit_power_law 
# 
# Igraph function fit_power_law performs a statistical test of the
# fit of a distribution to the power law. It can estimate xmin. 
# Should first present a summary of the Clauset, Shalizi and Newman 
# method. This provides a basic demonstration on Internet Routers and
# model graphs. 
# 
# September 26, 2019 Dan Suthers created this. 
# October    1, 2019 DS 
# * Increased network sizes since power laws are easier to discern 
#   in large networks 
# * Put positive example first to be less confusing 
# * Added HEP as another example 
# * Corresponding changes to comments 
# Feb 08, 2020 DS: Cleaning up for self study use. 
# Feb 13, 2020 DS: A few nuances added.
# Feb 17, 2021 DS: Adjustments for Spring 2021 ICS 422/622.
# 
######################################################################

library(igraph)
library(tibble)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files")
source("Utility/degree_domain.R")

#######################################################################
# Documentation: Read carefully, particularly for plfit and xmin
# Also how to interpret p value. This does require some statistical 
# background. 

?fit_power_law

#######################################################################
# First test on a model that claims to follow power law
#######################################################################
# ?sample_fitness_pl 

# We need fairly large graphs but low average degree to get strong 
# power law effects. (Note: your results WILL differ.)

sfp <- sample_fitness_pl(100000, 200000, 2.2, 
                         finite.size.correction = FALSE)
summary(sfp)

# We'll put it on a grid so we can compare examples. 

par(mfrow=c(2,2))

plot(degree_domain(sfp), degree_distribution(sfp), 
     log="xy", xlab="k", ylab="p(k)",
     main='Sample Fitness PL (gamma=2.2)')

plot(degree_domain(sfp), 
     degree_distribution(sfp, cumulative=TRUE), 
     log="xy", xlab="k", ylab="CDF(p(k))",
     main='Sample Fitness PL (gamma=2.2)')

# That looks like it could be a power law. Let's test, and print 
# cleanly with a tibble. 

as_tibble(sfp.plfit <- fit_power_law(degree(sfp), implementation="plfit"))

# ***** What do the xmin and alpha tell you? 

# ***** How do you interpret the p value? (Carefully: see documentation)

#######################################################################
# Now test on graphs of the same size that definitely do not follow 
# the power law to learn a few lessons of interpretation ... 
#######################################################################
# Random Graphs: G(n,m)

gnm <- sample_gnm(100000, 200000)
summary(gnm)

# First lin lin, then CDF to compare to last one 

plot(degree_domain(gnm), 
     degree_distribution(gnm), 
     xlab="k", ylab="p(k)", main='G(n,m)')
plot(degree_domain(gnm), 
     degree_distribution(gnm, cumulative=TRUE), 
     log="xy", xlab="k", ylab="CDF(p(k))", main='G(n,m)')

# Definitely not a power law, right?

as_tibble(gnm.plfit <- fit_power_law(degree(gnm), implementation="plfit"))

# If we look at KS.p alone we might say it is a power law, but look
# at what it fit! xmin is nearly the end of the data, and the alpha 
# (gamma) is WELL into the random-like regime. It fit a small tail, 
# and even then with alpha that is hard to distinguish from random. 

# Compare to results when you force it to include all the data: 

as_tibble(gnm.plfit <- fit_power_law(degree(gnm), implementation="plfit", 
                                     xmin=1))

##################################
# Complete graphs: Constant degree 

# Every vertex in a complete graph of N vertices has degree N-1

complete_deg <- replicate(100000, 100000-1)
as_tibble(complete_deg.plfit <- fit_power_law(complete_deg, 
                                              implementation="plfit"))

# This used to produce results! Apparently they have improved the
# implementation since last year: it's now power.law.fit.new. When I
# ran this last year I got:
#   xmin=1
#   KS.p=1   # looks OK but ... 
#   alpha=1
#   lokLik = 0 
#   KS.stat = 1.797693e+308 !!! 
# This distance value is close to the maximum floating point value
# possible in R: when I multiplied by 10 it returned "Inf". Nice that
# R lets us know rather than quietly doing numeric overflow!

#######################################################################
# Finally test on real world networks 
#######################################################################
# Internet Routers 

IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")
summary(IR)

plot(degree_domain(IR), degree_distribution(IR), 
     main='Internet Routers', 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(IR), degree_distribution(IR, cumulative=TRUE), 
     main='Internet Routers CDF', 
     log="xy", xlab="k", ylab="CDF(p(k))")
as_tibble(IR.plfit <- fit_power_law(degree(IR), implementation="plfit"))

# ***** What does that result tell us? 

###########
# HEP

HEP <- read_graph("Networks/cit-HepTh.gml", format="gml")
summary(HEP)
plot(degree_domain(HEP), degree_distribution(HEP), 
     main="High Energy Physics Citations", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(HEP), degree_distribution(HEP, cumulative=TRUE), 
     main="High Energy Physics Citations CDF", 
     log="xy", xlab="k", ylab="CDF(p(k))")
as_tibble(HEP.plfit <- fit_power_law(degree(HEP), implementation="plfit"))

# ***** What does that result tell us? 

# Clean up 
par(mfrow=c(1,1))

#######################################################################
# Assignment: HEP is a directed citation graph. In-degree and
# out-degree have very different meanings. The above analysis mixed
# them together, but we should not expect them to have the same
# behavior and so should analyze them separately. You will replicate
# the above for HEP in-degree and out-degree and interpret the results
# in terms of what in-degree and out-degree mean.

#######################################################################
# Final Comments: 

# We might find that the power law has a moderate to good fit to our
# distribution, but how do we know that some other statistical
# distribution doesn't have a better match? For example, log-normal
# distributions can look very similar to power laws, and exponentials
# under some conditions.

# We need to be careful about making claims unless we test them. The
# poweRlaw package provides a more sophisticated set of tools for
# comparing degree distributions to statistical distributions such as
# power law, log-normal and exponential, and testing which of these
# fits better. This is a Network Science II topic, but if you want to
# look:

# install.packages('poweRlaw')
library(poweRlaw)
?poweRlaw # and see the Index 

# However, unless we are using the degree distribution to test
# hypotheses of network formation, often we don't care so much exactly
# which matches the best, but just that the 'heavy tailed' nature of
# the distribution distinguishes many natural networks from random
# ones.

#######################################################################
#Pau