#######################################################################
# ERGM demonstration Part I: Basics of ERGM formulas and results. 
# Adapted from 
#   http://cran.r-project.org/web/packages/ergm/vignettes/ergm.pdf
# Dan Suthers, Nov 23, 2016 
# Tue Apr 14 2020 DS Updating for new class
# Apr 22 2021 DS Updating for ICS 422/622 spring 2021
# Apr 21 2021 DS Updating for ICS 422/622 spring 2022
# * Finding that ergm has changed; many examples no longer converge. 
#######################################################################
# Setup 

library(igraph) # in case we want to jump back to our familiar world 
setwd("~/Desktop/Network-Science-Demos") # Or yours 

# install.packages('statnet') # If not installed 
# update.packages('statnet') # If you have not updated for a while 

# library(statnet)
# or to install less, install and library 'network', 'ergm' and 'sna'

library(network)
library(ergm)
library(sna)

# Optional: This makes pretty plots specialized for ERGMs
# install.packages('coda')
library('coda')
# install.packages('latticeExtra')
library('latticeExtra')

#######################################################################
# Look over the documentation 

?ergm         

# read about formula, and take note of Value coef

?'ergm-terms' # this is your new friend 

# vignette('ergm-term-crossRef') # or if you prefer, this 

#######################################################################
# Networks to use 

data(package='ergm') # tells us the datasets in our packages
data(florentine)     # loads flomarriage and flobusiness data

# Network of marriage ties among Renaissance Florentine families

flomarriage # note vertex attributes 

# notice use of %v% to extract vertex attributes 

plot.network(flomarriage, label = flomarriage %v% "vertex.names")

#######################################################################
# Edges only

# ERGMs implicitly model graphs with the same number of vertices. Here
# we add to the model one explicit term, for the number of edges. This
# also introduces the ~ formula notation. The edges occur
# independently of each other. Thus this is an Erdos-Renyi G(n,m)
# model.

flomodel.e <- ergm(flomarriage ~ edges)

# The result is an ergm object ... 

class(flomodel.e)
names(flomodel.e)
flomodel.e$formula # Formula we used 
coef(flomodel.e)   # Coefficients (flomodel.e$coef depreciated)

# Summary of results. 

summary(flomodel.e) 

# Let's extract the coefficient on edges and interpret it:

(edges.coef <- coef(flomodel.e)[1])

# The log-odds of any tie occurring is edges.coef * change in number
# of ties or simply edges.coef since adding a tie changes the number
# of ties by 1

# We can use the "logistic function" p = exp(x)/(1 + exp(x)) to
# convert log odds x (or "logits" from "LOGistic unIT" ) to a
# probability. The logistic function is used for predicting binary
# outcome variables (such as link formation), as binary variables
# violate assumptions of linear models.

exp(edges.coef)/(1 + exp(edges.coef))

# This probability should be the same as the fraction of ties. Let's
# check:

network.size(flomarriage)
network.edgecount(flomarriage)

# There are 20 edges and 16 vertices, so the probability of a
# given tie is |E| / |V|*(|V|-1)/2 

20/((16*15)/2)

# which is what we expect. 

# Conversion of logit to probability will be a common computation, so
# let's make a utility function:

l_to_p <- function(l) {exp(l)/(1 + exp(l))}
l_to_p(edges.coef)

#######################################################################
# Edges and Triangles 
# Triangles are the most obvious way to model transitivity or clustering. 
# (Be warned that this term leads to degenerate problems in some graphs.)

# This model will use the MCMC or Markov Chain Monte Carlo method. By
# default we assume that ties are independent of each other, but if
# they share a node they are dependent: this is "Markov Dependence."
# It is our first example of departure from logistic regression.

# Modeling such dependency is more complex, not amenable to analytic
# solutions. We solve this by repeated random sampling of the
# distribution (here, of possible graphs): this is a "Monte Carlo
# method", perhaps because it is like gambling. Your results may
# differ.

flomodel.et <- ergm(flomarriage ~ edges + triangle)

# (We will discuss mcmc.diagnostics later, but here's a quick look.) 

mcmc.diagnostics(flomodel.et)

summary(flomodel.et)

# ***** Were triangles helpful in modeling this data? 

# ********** FOLLOWING IS TMI FOR ONE DAY INTRO **********
# As a reminder we can get the formula this way: 

flomodel.et$formula 

# Let's get the coefficients ... 

coef(flomodel.et)
(et.edges.coef = coef(flomodel.et)[1])
(et.triangle.coef = coef(flomodel.et)[2])

# Conditional log-odds of two actors forming a tie is: 
#   (et.edges.coef x change in number of ties) 
#    + (et.triangle.coef) x change in number of triangles
# If you add one edge you could create 0 or more triangles (if two
# nodes share n neighbors and then you connect the two nodes you 
# get n triangles.)
# Let's compute this the R way for 0, 1, 2 and 3 triangles added: 

(et.logodds = et.edges.coef + c(0,1,2,3) * et.triangle.coef)

# or in probabilities (notice how sapply not needed)

l_to_p(et.logodds)

# ***** How to interpret that? 

#######################################################################
# Exogenous effects: Wealth 
# Recall that we can bring in node attributes. Here is how. 

# In statnet, %v% extracts vertex attributes from a network

(wealth <- flomarriage %v% 'wealth')

# Visualize the network with wealth as node size 

plot(flomarriage, vertex.cex=wealth/25, 
     label= flomarriage %v% "vertex.names")

# Let's test whether edge probabilities are a function of wealth. We
# use 'nodecov' for numeric attributes treated as covariates.

?nodecov # and search within page 
flomodel.ew <- ergm(flomarriage ~ edges + nodecov('wealth'))

# ***** Why no MCMC this time? 

summary(flomodel.ew)

# ***** Are edge probabilities a function of wealth? 

#######################################################################
# Modeling Reciprocity: Dyadic Dependence 
# Does the likelihood of a tie from A to B depend on whether there is
# a tie from B to A? That is, is reciprocity a factor?

# For this we need directed graphs: Sampson's Monks ("liking" relations
# among a set of 18 monk-novitiates preparing to enter a monastery.
# See for example http://www.analytictech.com/ucinet/help/hs5121.htm 

data(samplk)
ls()          # There are different ones 
samplk3       # We will use this one
plot(samplk3, label = samplk3 %v% "vertex.names")

# We use the "mutual" term for reciprocity 

?mutual
sampmodel.em <- ergm(samplk3 ~ edges + mutual) 

# Just to show that the model fits well (I'll discuss briefly): 

mcmc.diagnostics(sampmodel.em) 

# GOF is a future topic. 

summary(sampmodel.em)

# Is there a significant tendency for ties to be reciprocated? 

########################################################################
# Departing from the 'official' demo ... 
# Let's experiment a bit. First, the monks have been classified into 
# factions or "groups" by the researcher, under attribute group. 
# This is categorical data, so we use a categorical term rather than
# nodecov.  Note that there are two versions: match and factor. 
# Read the documentation and compare the plots and results. 

samplk3 %v% 'group'
sampmodel.emgm <- ergm(samplk3 ~ edges + mutual + nodematch('group'))
mcmc.diagnostics(sampmodel.emgm) 
summary(sampmodel.emgm)

sampmodel.emgf <- ergm(samplk3 ~ edges + mutual + nodefactor('group'))
mcmc.diagnostics(sampmodel.emgf) 
summary(sampmodel.emgf)

# ***** Can you explain the difference in results? 

# How about throwing triangles in the first one above for transitivity? 

sampmodel.emtg <- ergm(samplk3 ~ edges + mutual + triangle + nodematch('group'))
mcmc.diagnostics(sampmodel.emtg) 
summary(sampmodel.emtg)

# ***** Do triangles make a significant contribution given the other terms? 

# What if we removed the group factor, which is kind of 'cheating'? 

# sampmodel.emt <- ergm(samplk3 ~ edges + mutual + triangle)
# 
# This will end in the following after several minutes. 
# Iteration 1 of at most 60:
# Error in ergm.MCMLE(init, nw, model, initialfit = (initialfit <- NULL),  : 
#   Unconstrained MCMC sampling did not mix at all. Optimization cannot continue.
# In addition: Warning message:
# In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
#   Unable to reach target effective size in iterations alotted.
# 
# summary(sampmodel.emt) # In the rare case that it worked 

# This is not the last time you will see problems with triangles 
# (We will need to return to the "social circuit" alternative.)

# But triangles depend not only on the number of edges but also
# the number of 2-stars. Since this is directed we can't use kstar,
# so we'll have a term for istar and ostar. 

# ?istar
# sampmodel.emi2o2t <- 
#   ergm(samplk3 ~ edges + mutual + istar(2) + ostar(2) + triangle)
# 
# # 9 hours later I killed it ... should not use these together! 
# # Iteration 20 of at most 60:
# #   Warning: Model statistics ‘ostar2’ are linear combinations of some set of preceding statistics at the current stage of the estimation. This may indicate that the model is nonidentifiable.
# # Optimizing with step length 1.0000.
# # The log-likelihood improved by < 0.0001.
# # Unable to test for convergence; increasing sample size.
# 
# mcmc.diagnostics(sampmodel.emi2o2t)
# summary(sampmodel.emi2o2t)
# date()
# 
# # At least that showed you more of the terms. 

########################################################################
# Restart here 
########################################################################
# Another example of testing whether exogenous attributes predict links. 
# Simulated data from "Mesa High" has grade, race and gender attributes.
# This is a simulation of an in-school friendship network. The school 
# commnunity on which it is based is in the rural western US, with a 
# student body that is largely Hispanic and Native American. 

data(faux.mesa.high)
mesa <- faux.mesa.high           # just for shorthand
mesa                             # to see the attribute names to use 

# We learn a little about plotting networks (not the same as igraph).

# Plot it by grade, saving the vertex coordinates (it is sometimes
# useful to preserve layout across different visualizations). 

mcoord <- plot(mesa, vertex.col='Grade')  
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

# Let's color by Race but label gender, using same coordinates 

plot(mesa, vertex.col='Race', label = mesa %v% 'Sex', coord=mcoord)
legend('bottomleft',fill=7:12,legend=paste('Race',7:12),cex=0.75)

# Let's see how grade and race jointly account for edges 
# Read the nodematch diff documentation

?nodematch
mesa.esgr <- ergm(mesa ~ edges + 
                         nodematch('Sex') + 
                         nodematch('Grade',diff=TRUE) + 
                         nodematch('Race',diff=TRUE))
summary(mesa.esgr)

# ***** Are ties more likely within same-gender? 

# ***** Are within-group ties more likely within each grade independently? 

# ***** What about race? Why are two coefficients estimated as -Inf? 

table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race") 

# The problem is that there are few students in Black and Other, so
# some entries for ties to other groups are 0, and surprisingly there
# are no within-group ties. This results in the -Inf estimates.

#######################################################################
# Missing Data 
#######################################################################
# What is the impact of missing data (and how do you represent it?)
# We'll make a simple example. 

# A network with missing edge data for two pairs. 
missnet <- network.initialize(10,directed=FALSE)
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1
missnet[4,6] <- missnet[4,9] <- NA   # missing data 
missnet    # Notice the "missing edges= 2"
miscoord <- plot(missnet, displaylabels=TRUE) # Save coordinates 

ergm(missnet ~ edges + degree(2))

# In the above, we know that one node has degree 2. There are two that
# may or may not have degree 2, and seven that do not have degree 2. 

# Add the missing data 
missnet[4,6] <- missnet[4,9] <- 0
plot(missnet, displaylabels=TRUE, coord=miscoord)  # looks the same but ...
missnet        # missing edges no longer recorded 
ergm(missnet ~ edges + degree(2))

# In the above, one node has degree 2 and nine do not. So, the 
# estimates for degree 2 coefficient differ. 

#######################################################################
# Insert Here: discussion of chapters 6 and 7, and model terms
#######################################################################
# Pau 
