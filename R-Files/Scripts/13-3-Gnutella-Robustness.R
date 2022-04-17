######################################################################
# Gnutella Robustness 
# Apr 15 2021 Dan Suthers 
# 
# See paper https://arxiv.org/abs/cs/0209028 
#   Matei Ripeanu, Ian Foster, Adriana Iamnitchi (2002). Mapping the
#   Gnutella Network: Properties of Large-Scale Peer-to-Peer Systems 
#   and Implications for System Design IEEE Internet Computing Journal
#   (special issue on peer-to-peer networking), vol. 6(1) 2002
# which claims that the Gnutella network structure is more robust to
# attach than scale free networks, but does not provide analytic or
# empirical evidence. We will provide evidence with a simulation.
# 
######################################################################

library(igraph)
setwd("~/Desktop/Network-Science-Demos")
source("Utility/degree_domain.R")
source("Utility/robustness_simulation_utility.R")

# This is new 
source("Utility/giant_component.R")

######################################################################
# The network 

Gnu <- read_graph("Networks/p2p-Gnutella31.gml", format="gml")
summary(Gnu)

# It has a weird distribution (a bit different than in the paper)

plot(degree_domain(Gnu), degree_distribution(Gnu), 
     main="Gnutella Degree Distribution", 
     log="xy", xlab="k", ylab="p(k)")

# Mostly due to out-degree, but it's not clear why this is a directed
# graph: the original paper does not mention directedness, and it may
# be an artifact of how the network was originally read.

plot(degree_domain(Gnu, mode="in"), 
     degree_distribution(Gnu, mode="in"), 
     main="Gnutella In-Degree Distribution", 
     log="xy", xlab="k", ylab="p(k)")
plot(degree_domain(Gnu, mode="out"), 
     degree_distribution(Gnu, mode="out"),
     main="Gnutella Out-Degree Distribution", 
     log="xy", xlab="k", ylab="p(k)")

# We will treat it as undirected for simplicity. Is it connected? 

components(Gnu, mode="weak")$csize

# Let's work with the giant component so it starts connected 

Gnu <- giant_component(Gnu, mode="weak")
Gnu$name <- "Gnutella Giant Component"
summary(Gnu)

# Make a preferential attachment model to compare, with more edges
# (ceiling) to give it benefit of the doubt, and superlinear
# preference of attachment for bigger hubs.

GPA <- sample_pa(vcount(Gnu), m=ceiling(ecount(Gnu)/vcount(Gnu)), 
                 power=1.1)
summary(GPA)

# We'll fail a small percentage of the nodes 

cutoff <- 0.001  # 0.01 better but is slow 
trials <- 5      # strange distribution, let's be sure
# Failure and attack for Gnutella 

Gnu.failure <- iterated_robustness_simulation(Gnu, 
                                              cutoff=cutoff, 
                                              trials=trials, 
                                              attack=FALSE) 
Gnu.attack  <- iterated_robustness_simulation(Gnu, 
                                              cutoff=cutoff,
                                              trials=trials, 
                                              attack=TRUE)

# Failure and attack for PA model 

GPA.failure <- iterated_robustness_simulation(GPA, 
                                              cutoff=cutoff, 
                                              trials=trials, 
                                              attack=FALSE) 
GPA.attack <- iterated_robustness_simulation(GPA, 
                                             cutoff=cutoff,
                                             trials=trials, 
                                             attack=TRUE)

# Take a look with comparable y axes 

par(mfrow=c(2,2))

numcomp_frange <- range(Gnu.failure$numcomp, 
                        GPA.failure$numcomp)
bigcomp_frange <- range(Gnu.failure$bigcomp, 
                        GPA.failure$bigcomp)
numcomp_arange <- range(Gnu.attack$numcomp, 
                        GPA.attack$numcomp)
bigcomp_arange <- range(Gnu.attack$bigcomp, 
                        GPA.attack$bigcomp)

plot_robustness_results(Gnu.failure, name="Gnu Failure",
                        numcomp_ylim = numcomp_frange, 
                        bigcomp_ylim = bigcomp_frange)
plot_robustness_results(GPA.failure, name="Gnu PA Model Failure",
                        numcomp_ylim = numcomp_frange, 
                        bigcomp_ylim = bigcomp_frange)

plot_robustness_results(Gnu.attack, name="Gnu Attack", 
                        numcomp_ylim = numcomp_arange, 
                        bigcomp_ylim = bigcomp_arange)
plot_robustness_results(GPA.attack, name="Gnu PA Model Attack", 
                        numcomp_ylim = numcomp_arange, 
                        bigcomp_ylim = bigcomp_arange)

par(mfrow=c(1,1))

# Illustrates robustness of scale free with respect to failure but
# vulnerability with respect to attack. Gnutella retains some failure
# robustness (note small range on y axis) while outperforming scale
# free on attack.

######################################################################
# Pau 