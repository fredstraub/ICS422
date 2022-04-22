######################################################################
# SIR simulation demonstration 
# Originally based on Statistical Analysis of Social Networks (SAND)
# book, but substantially elaborated.
#   To add: 
#     Would be good to have a function wrapping some of it up and 
#     outputting tau = time required to reach fraction 1/e ~36% 
#     of individuals affected.  Then we can run simulations comparing
#     networks. Example: Ebola broke out; reactionaries want to cut 
#     all flights to Africa. Cut them out of the network: how much
#     of a delay do we see? (Barabasi defines tau early in chapter.)
# Dan Suthers, Nov 16, 2016
# Apr 17 2018 DS Last update for 2018 class. 
# Sep 23 2018 DS Minor updates for current script style and clarity
# Nov 26 2019 DS Minor changes for this year's class. 
# Apr 18 2021 DS Updating for ICS 422/622 spring 2021
# Apr 19 2021 DS Updating for ICS 422/622 spring 2022
# * Nontrivial improvements to comments and code 
# * Using same two-day Public School data as we used for Analysis 7
# Apr 21 2021 DS Updating for ICS 422/622 spring 2022
# * Added beta variation
#
######################################################################

library("igraph")
setwd("~/Desktop/Network-Science-Demos") # Or yours 
source("Utility/new_window.R")
source("Utility/nonzero_degree_distribution.R")
source("Utility/remove_isolates.R")

######################################################################
# Example Graphs (from SAND demo)
######################################################################

# Make a list of three kinds of random graphs, with |V|=10000 and 
# |E|=40000. 

N <- 10000
M <- 40000
alpha <- 2.1 # gamma means something else in SIR

gl <- list()
gl$er <- sample_gnm(N, M)
gl$sf <- sample_fitness_pl(N, M, alpha)
gl$ws <- sample_smallworld(1, N, M/N, 0.05)
summary(gl$ws) # Just to show we have the same M! 

# Suggestion: to analyze your own graph you might generate scale-free
# and random graphs with the same N, M for comparison and substitute
# yours for Watts-Strogatz small world (which is less realistic). 

# Just to remind us of what these look like

plot(nonzero_degree_distribution(gl$er), main="Erdos-Renyi Random Graph")
plot(nonzero_degree_distribution(gl$sf), main="Scale Free")
plot(nonzero_degree_distribution(gl$ws), main="Watts-Strogatz")

######################################################################
# Running SIR (from SAND demo)
######################################################################

?sir

# Parameters: infection rate, recovery rate and number of trials
# Vary the first two to experiment.

beta <- 0.5     # transmission probability 
gamma <- 1.0    # recovery rate, Barabasi's mu
ntrials <- 100  # how many trials to run (try 1000 outside class)

# Run simulation for each graph and save results as list. 

sim <- lapply(gl, sir, beta=beta, gamma=gamma, no.sim=ntrials)

# Explore what the results objects look like. 

attributes(sim)    # It has 3 named elements and each is a simulation

# sim$er DO NOT DO THIS! Many screens full 

names(sim$er[[1]]) # Each simulation has these parts ... 

# Let's look at numbers at the beginning and end of one of the
# simulation. (This is just one: it may happen to have weird results.)

# times = times of events. Note different ending times. 
head(sim$er[[1]]$times)
tail(sim$er[[1]]$times)
tail(sim$sf[[1]]$times)
tail(sim$ws[[1]]$times)

# NS = number of susceptible individuals
head(sim$er[[1]]$NS)
tail(sim$er[[1]]$NS)

# NI = number of infected individuals 
head(sim$er[[1]]$NI)
tail(sim$er[[1]]$NI)

# NR = number of recovered individuals
head(sim$er[[1]]$NR)
tail(sim$er[[1]]$NR)

# See documentation. The first marks the time bins and the second
# returns the median number of infected in each time bin. 

head(time_bins(sim$er))
head(median(sim$er)[["NI"]])

# But we don't need to plot these ourselves ... 

######################################################################
# Plotting results (from SAND demo)
######################################################################
# Plot total number infected as a function of time for each network. 
# Has own plotting method

?plot.sir 

# Following SAND, we will change the colors for each model, and plot
# individual runs (color) as well as medians and quantiles. 

# **** Plotting is a lot faster in new_window! 

new_window(paste0("SIR, |V|=", N, "; |E|=", M, 
                  "; beta=", beta, "; gamma=", gamma), 
           12, 12)
par(mfrow=c(2,2))

plot(sim$er, main="Erdos-Renyi")
plot(sim$sf, main="Scale Free", 
     color="palegoldenrod", median_color="orange", quantile_color="orange")
plot(sim$ws, main="Watts-Strogatz", 
     color="pink", median_color="red", quantile_color="red")

# ***** In which network does the disease spread the fastest? 
#       In which network does the disease take the longest to die out? 

# ----------------------------------------

# The above discussion motivates plotting a comparison of the medians.
# We'll do this with our own code. First find the max values across
# the three data sets for the plot. (1.05 for "headroom" on top.) n

(x.max <- max(sapply(sapply(sim, time_bins), max)))
(y.max <- 1.05 * max(sapply(sapply(sim, function(x) median(x)[["NI"]]), 
                            max, na.rm=TRUE)))

# Plot ER against the max coordinates

plot(time_bins(sim$er), median(sim$er)[["NI"]], 
     type="l", lwd=2, col="blue", 
     main="Comparison of SIR medians", 
     xlab="Time", ylab=expression(N[I](t)), 
     xlim=c(0,x.max), ylim=c(0,y.max))

# Add lines for SF and WS

lines(time_bins(sim$sf), median(sim$sf)[["NI"]], lwd=2, col="orange")
lines(time_bins(sim$ws), median(sim$ws)[["NI"]], lwd=2, col="red")

# Add the legend

legend("topright", c("Erdos-Renyi", "Scale Free", "Watts-Strotagz"), 
       col=c("blue", "orange", "red"), lty=1, lwd=2, bty="n", cex=1.2)

# ***** Discuss what these curves mean 

######################################################################
# Activity 
######################################################################
# You can rerun the above plot code unmodified for the following. 

# How would you change it to an SI model in theory? 
# (Turns out they no longer allow gamma of 0.)

# ***** If you are revisiting this at home, try it with other values 
#       of the simulation parameters. (Sometimes it crashes.)

######################################################################
# Utility Functions 
######################################################################
# To make experimentation easier, let's make some functions for 
# comparing a network to its G(n,m) and configuration models. 
# We'll just plot the medians, not the detail plots. 

# List of a graph and its G(n,m) and Configuration models. This
# probably needs more parameters

make_graph_list <- function (g, method="vl") {
	graphs <- list()
	  graphs$natural <- g
    graphs$gnm <- sample_gnm(vcount(g), ecount(g), directed=FALSE)
    graphs$conf <- sample_degseq(degree(g), method=method)
    return(graphs)
}

# Plot a comparison of the medians 

plot_sims <- function(sims, name="Natural") {
	# First find the max values across the three data sets for the plot
	x.max <- max(sapply(sapply(sims, time_bins), max))
	y.max <- 1.05 * max(sapply(sapply(sims, 
	                                  function(x) median(x)[["NI"]]), 
	                           max, na.rm=TRUE))
	# Plot G(n,m) and Configuration Model first
	plot(time_bins(sims$gnm), median(sims$gnm)[["NI"]], 
	     type="l", lwd=2, lty="dashed", 
	     col="orange", xlim=c(0,x.max), ylim=c(0,y.max), xlab="Time", 
	     ylab=expression(N[I](t)), main=paste("SIR medians for", name), 
	     sub=paste0("SIR beta=", beta, "; gamma=", gamma))
	     # Add line for Configuration Model 
    lines(time_bins(sims$conf), median(sims$conf)[["NI"]], 
       lty="dashed", lwd=2, col="red")
    # Lines for our source graph last so we can see it 
    lines(time_bins(sims$natural), median(sims$natural)[["NI"]], 
          lty="solid", lwd=2, col="blue")
    # Add the legend
    legend("topright", c(name, "G(n,m)", "Config"), 
           col=c("blue", "orange", "red"), lty=1, lwd=2, bty="n", cex=0.8)
	}

######################################################################
# Primary School: Infectious diseases spread in schools ... 

# new_window("SIR on Primary School Network")
# To go back to R Studio window: 
dev.list()
dev.set(2)

# Let's work with the same 2 day data as our recent analysis. 

PS <- read_graph("Networks/sp_data_school_day_1_2_multigraph.graphml", 
                 format="graphml")
PS <- simplify(PS, edge.attr.comb = list(weight="sum", 
                                         duration="sum", 
                                         count="sum", "ignore"))
summary(PS)

# This is smaller so we can run 1000 trials
ntrials <- 1000

# First try the unfiltered version 

plot(nonzero_degree_distribution(PS))
PS_graphs <- make_graph_list(PS)
beta <- 0.5   # In case these were changed during the activity
gamma <- 1.0
PS_sims <- lapply(PS_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS_sims, "Primary School: 2 Days Unfiltered")

# ***** Explain the results: a coincidence? a bug? or what we expect? 

plot(PS, vertex.size=3, vertex.label=NA, 
     main="Primary School Unfiltered")
edge_density(PS)

# Now suppose we require at least 60 seconds of contact as before. 
# Use our prior method to account for multiple contacts. 

PS60 <- delete_edges(PS, E(PS)[E(PS)$duration < (count*20) + 40])
summary(PS60)
plot(PS60, vertex.size=3, vertex.label=NA, 
     main="Primary School 60 second contacts")

PS60_graphs <- make_graph_list(PS60)
PS60_sims <- lapply(PS60_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS60_sims, "Primary School 60 second contacts")

# Not much difference but there is a trend. 

# Suppose CDC determines that 5 minutes of contact is typically needed
# for transmission. Again, we approximate this accounting for spurious
# contacts:

PS5M <- delete_edges(PS, E(PS)[E(PS)$duration < (count*20) + (5*60)-20])
summary(PS5M)
plot(PS5M, vertex.size=3, vertex.label=NA, 
     main="Primary School 5 minute contacts")

PS5M_graphs <- make_graph_list(PS5M)

# ***** Huh? 
#       Look at the plot and think about the configuration model. 
#       Fix it and then continue: 

PS5M <- remove_isolates(PS5M)
summary(PS5M)
PS5M_graphs <- make_graph_list(PS5M)

PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "Primary School 5 minute contacts")

# ***** Can you explain why it is slowing down in the natural network?
#       How does it differ from the random models? 

plot(PS5M_graphs$gnm, vertex.size=3, vertex.label=NA, 
     main="Primary School 5 minute contacts G(n,m)")
plot(PS5M_graphs$conf, vertex.size=3, vertex.label=NA, 
     main="Primary School 5 minute contacts Config")

###################################################################
# Lets see the effect of Beta (probability of infection on contact). 

beta <- 0.9
PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "PS5 Beta 0.9")

beta <- 0.7
PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "PS5 Beta 0.7")

beta <- 0.5
PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "PS5 Beta 0.5")

beta <- 0.3
PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "PS5 Beta 0.3")

beta <- 0.1
PS5M_sims <- lapply(PS5M_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(PS5M_sims, "PS5 Beta 0.1")

# ***** Interpretation?
#       (With a little work we could plot all of them on same axes.)

######################################################################
# Network Science: Suppose co-authoring means you also get "infected"
# with an idea ... 

NS <- read_graph("Networks/netscience.graphml", format="graphml")
summary(NS)
plot(nonzero_degree_distribution(NS))

# We know NS has isolates 
NS <- remove_isolates(NS)
NS_graphs <- make_graph_list(NS)

# A bit larger, speed up the plots
ntrials <- 100

NS_sims <- lapply(NS_graphs, sir, beta=beta, gamma=gamma, no.sim=ntrials)
plot_sims(NS_sims, "Network Science SIR")

# ***** WTF? Can you explain these plots? 
# ***** Try repeated runs of the last two steps above (simulate and plot)

# Once you have tried to explain it, here is a clue: 

sapply(NS_graphs, count_components)

# Or more visually: 

viz <- function(g) {plot(g, vertex.size=3, vertex.label=NA, main=g$name)}
viz(NS_graphs[[1]])
viz(NS_graphs[[2]])
viz(NS_graphs[[3]])

# And the original plotter shows the variation: 

plot(NS_sims$natural, main="NetSci Natural")
plot(NS_sims$gnm, main="NetSci G(n,m)")
plot(NS_sims$conf, main="NetSci Configuration")

# But these are on different scales. Put on one plot: 

(x.max <- max(sapply(sapply(NS_sims, time_bins), max)))
(y.max <- 1.05 * max(sapply(sapply(NS_sims, function(x) median(x)[["NI"]]), 
                            max, na.rm=TRUE)))
plot(time_bins(NS_sims$natural), median(sim$natural)[["NI"]], 
     type="l", lwd=2, col="blue", 
     main="Comparison of SIR medians", 
     xlab="Time", ylab=expression(N[I](t)), 
     xlim=c(0,x.max), ylim=c(0,y.max))
lines(time_bins(NS_sims$gnm), median(NS_sims$gnm)[["NI"]], lwd=2, col="orange")
lines(time_bins(NS_sims$conf), median(NS_sims$conf)[["NI"]], lwd=2, col="red")
legend("topright", c("Natural NetSci", "Erdos-Renyi", "Configuration"), 
       col=c("blue", "orange", "red"), lty=1, lwd=2, bty="n", cex=1.2)

# ***** Make more sense? 
#       What happens to good ideas in isolated groups? 
#       Why is Configuration spread faster than ER? 

######################################################################
# Further Activity 
######################################################################

# Apply SIR to other graphs
#   - comparing to G(n,m) and configuration model 
#   - plotting only the medians 

######################################################################
# ***** Bridging from Cascades to Temporal Analysis **** 
######################################################################
# Before we leave this topic, let's run last week's cascade algorithm
# on PS5M 

source("Utility/cascading_simulation.R")

PS5M_cs <- cascading_simulation(PS5M, 
                                ends(PS5M, sample(E(PS5M), 1)),
                                q=0.2, verbose=TRUE, plot=FALSE)

# Repeat this if needed to get a good sequence. 

# Then write out to read in Gephi. Color nodes by Raking of iteration.
# Then use Merge Column to make Intervals from iteration, and do a
# temporal animation.

write_graph(PS5M_cs, "PS5M_cs.graphml", format="graphml")

######################################################################
# Pau 

