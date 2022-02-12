######################################################################
# 5-3 Small World Model (Watts-Strogatz)
# Introduction and example modeling Network Science collaboration. 
# Dan Suthers, September 14, 2016
# 
# Comment: This model is primarily of historical interest and is not 
# normally used as a comparative model. >>> We could skip it. <<<
# 
# Feb 1, 2018 DS: new_window and small edits. 
# Jul 24 2018 DS: Minor updates for current script style and clarity.
# Sep 19 2019 DS: Cleaning up for today's class.
# Feb  1 2020 DS: Small revisions for 2020 self study students. 
# Feb  9 2021 Revisions for ICS 422/622 Spring 2021
# Feb 10 2022 Minor revisions for ICS 422/622 Spring 2022. 
######################################################################
# If you have not done so already today ... 

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

NS <- read_graph("Networks/netscience.graphml", format="graphml")

######################################################################
# General introduction to Watt-Strogatz models 
######################################################################

# In SAND this was watts.strogatz.game. Read the Arguments 

?sample_smallworld

# Example: A circular lattice g.cl without rewiring

g.cl <- sample_smallworld(1, 25, 5, 0) # 0 means no rewire
plot(g.cl, main=paste0(g.cl$name, ", no rewiring"), 
     layout=layout.circle, vertex.label=NA, vertex.size=2)

# You multiply dim and size to get |V| and that by nei to get |E| 

summary(g.cl)

# Example: Watts-Strogatz model rewires some of the edges randomly

g.ws <- sample_smallworld(1, 25, 5, 0.05)
ecount(g.ws)

# ***** How many edges are rewired on average? 

# Here we can see the random rewires: 

plot(g.ws, main=paste0(g.cl$name, ", 5% rewiring, v1"), 
     layout=layout.circle, vertex.label=NA, vertex.size=2)

# Run again if you want to verify it changes. 

g.ws <- sample_smallworld(1, 25, 5, 0.05)
summary(g.ws)
plot(g.ws, main=paste0(g.cl$name, ", 5% rewiring, v2"), 
     layout=layout.circle, vertex.label=NA, vertex.size=2)

# It is designed to give high transitivity and short mean distance 

transitivity(g.ws, type="global")
mean_distance(g.ws)

# Let's look at a larger unmodified lattice as a starting point. 

g.lat100 <- sample_smallworld(1, 100, 5, 0) 
plot(g.lat100, main="Lattice of 100 Without Rewiring", layout=layout.circle, vertex.label=NA, vertex.size=2)

# Should have high transitivity but high distances. 

transitivity(g.lat100, type="global")
diameter(g.lat100)
mean_distance(g.lat100)

# Now lets rewire only 5% of the edges 

g.ws100 <- sample_smallworld(1, 100, 5, 0.05)
plot(g.ws100, main="Lattice of 100 With Rewiring", layout=layout.circle, vertex.label=NA, vertex.size=2)

# Transitivity should still be high but distances cut in half. 

transitivity(g.ws100, type="global")
diameter(g.ws100)
mean_distance(g.ws100)

######################################################################
# Small world / transitivity tradeoff (do at home if out of time) 
######################################################################
# It turns out that only a small amount of rewiring is needed to get 
# shorter distances while retaining high transitivity. Let's run a
# simulation to graph the tradeoff at different probabilities of rewire.  

# General principle: when evaluating metrics from a random graph model, 
# take multiple samples from the model to be sure your metrics converge
# towards the mean: the Central Limit Theorem! So we need loops: 

# Setup: The loop for rewiring probabilities is controlled by this: 

?seq
powers <- seq(-4, -0.5, 0.1) 
powers

# Will use the above to generate rewiring probabilities: 

10^powers
(len <- length(powers)) # to control iteration
(cl <- numeric(len))   # to accumulate clustering results 
(pl <- numeric(len))   # to accumulate path length results 

# The loop for taking multiple samples from the random model is 
# controlled by this. 100 would be better but class time precious. 

ntrials <- 20

# Here is the sampling simulation. Understand it before running. 

for (i in (1:len)) {       # Loop is for each rewiring probability
  cat(i, " p =", 10^powers[i], "\n") # So we know it is making progress 
  cltemp <- numeric(ntrials)
  pltemp <- numeric(ntrials)
  for (j in (1:ntrials)) { # Loop to sample with given probability
    g <- sample_smallworld(1, 1000, 10, 10^powers[i]) # latter term is probability
    cltemp[j] <- transitivity(g, type="global")
    pltemp[j] <- mean_distance(g, directed=FALSE)
  }
  cl[i] <- mean(cltemp)
  pl[i] <- mean(pltemp)
}

# Plot the results. 
# (Study of the plot, lines and legend commands will be instructive.) 

cl/max(cl) # gives x values, clustering values scaled to max 
plot(powers, cl/max(cl), 
     ylim=c(0,1), lwd=3, type="l", col="blue", 
     xlab=expression(log[10](p)), ylab="Transitivity and Average Distance")
?lines # adds lines
lines(powers, pl/max(pl), lwd=3, col="red")
?legend
legend("topright", c("transitivity", "distance"), 
       col=c("blue", "red"), lty=1, lwd=3)

# ***** What does this plot tell us? Within what range of p do we 
#       model short distances and high transitivity well? 

# In the slides I have a plot of 100 trials. Plot is smoother. 

######################################################################
# Modeling Network Science with Watts-Strogatz Small World Model 
# (Skip in class if short on time)
######################################################################

# How to get a graph of similar N=|V|, L=|E|? 

summary(NS)

# I tried 2-d models but in order to get number of edges
# close had to reduce density to where transitivity too low. 
# This is an example of the kind of explorations you might do ... 

sqrt(1589) # size of each dimension
NS.sw <- sample_smallworld(2, 40, 2, 0.05)
summary(NS.sw) # too many edges
NS.sw <- sample_smallworld(2, 40, 1, 0.05)
summary(NS.sw) # closer
transitivity(NS, type="global")
transitivity(NS.sw, type="global")

# That is too low. So let's try a circular lattice 

NS.sw <- sample_smallworld(1, 1589, 2, 0.05)
summary(NS)
summary(NS.sw)
transitivity(NS, type="global")
transitivity(NS.sw, type="global") # better but not same
mean_distance(NS)
mean_distance(NS.sw)

# ***** Can you make a better fit by modifying p? 
#       Which way would you modify p to better model distance? 
#       What would happen to transitivity if you did that? 

# Does not model components well: Watts-Strogatz is always connected

table(sapply(decompose(NS), vcount))
table(sapply(decompose(NS.sw), vcount))

# How well does it model degree distribution? 
# new_window("NS Watts-Strogatz Model", 10, 5)
# par(mfrow=c(1,2))

plot(degree_domain(NS), 
     nonzero_degree_distribution(NS), 
     main="NetSci", xlab="degree", ylab="proportion")
plot(degree_domain(NS.sw), 
     nonzero_degree_distribution(NS.sw), 
     main="NetSci Small World", xlab="degree", ylab="proportion")

# ***** Can you explain the second (Small World) distribution? 

######################################################################
# Optional Activity: Model EuroSiS WebAtlas the same way. 
######################################################################

WA <- read_graph("Networks/EuroSiS-WebAtlas-Simplified.graphml", 
                 format="graphml")
summary(WA) 

# I recommend that you recreate commands down here rather than editing
# the above. 

######################################################################
# Pau