######################################################################
# Plotting Distributions 
# (In particular, degree distributions, but works for other data too.)
# Including log-log, cumulative and binned log plots.
# See Barabasi Network Science section 4.B for why we want to do this;
# Newman Network Science section 8.4.1 for more on math of plotting.
# Dan Suthers, September 2016
# Feb  6 2018 DS: modified after class for spring 2018 course. 
# Aug  9 2018 DS: Minor updates for current script style and clarity
# Sep 24 2019 DS: Updates for Fall 2019
# Feb 08 2020 DS: Cleaning up for self study use. 
# Feb 16 2021 DS: Updates for Spring 2021 ICS 422/622
# Feb 15 2022: Updates for Spring 2022 422/622
######################################################################

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")

# Need a reasonably large graph for degree trends to be useful. 

IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")
summary(IR)

######################################################################
# Plotting Degree Distributions in Gephi 
######################################################################

# First read the above graph into Gephi, and run Average Degree to get 
# the "Degree Report" in a window called "HTML Report". There is a 
# Degree Distribution plot in this report. What are some problems with 
# this plot? Is it easy to see the shape of the degree distribution? 

######################################################################
# Plotting Degree Distributions in R (exploring types of plots)
######################################################################

# Using a new window is optional. If you stay in a single window 
# (as in RStudio), you will need to flip between plots. 
# new_window("Internet Routers Degree Distribution", 12, 15)

par(mfrow=c(2,2))

# I like the axis labels to be a little bigger 

par(cex.axis=1.5, cex.lab=1.5)

####################
# Linear Plots 

# Plain vanilla degree distribution plot: 

plot(degree_distribution(IR), 
     main="Routers Degree Dist Lin Lin", xlab="k", ylab="p(k)")

# Unlike Gephi, R is plotting a value for every possible degree, even
# those that don't exist. This creates the solid black blob. Let's use
# the function we wrote previously in 4-Basic-Graph-Metrics.R to
# remove probability zero values.

plot(nonzero_degree_distribution(IR), 
     main="Routers Nonzero Degree Dist Lin Lin", 
     xlab="k", ylab="p(k)")

# However, we still have the same problem we saw in Gephi: values
# bunched together along the axes. This is because small values have
# high probability, so stack up on the left side, while large vaues
# have small probability, so are plotted along the bottom. We need ... 

####################
# Log Log Plots

# Log log shows the distribution better. First let's try all values. 

plot(degree_distribution(IR), 
     main="Routers Degree Dist Log Log", 
     xlab="k", ylab="p(k)", 
     log="xy")

# Better, but there are two issues. 

# ***** Why the warning for y values <= 0? 

# The y values are degree_distribution probabilities, so there 
# should be 2230 values p(k)=0

length(which(degree_distribution(IR) == 0.0))

# Although the log-log plot has gotten rid of the 0 values for us, we
# might want to do it ourselves to get rid of the warning. So we will
# use nonzero_degree_distribution in our second version.

# ***** Why is there no data point for degree 1? There are many: 

length(which(degree(IR) == 1))

? degree_distribution
# Recall that degree_distribution returns a vector where the first
# position is the probability of degree 0, the second the probability
# of degree 1, etc. So the point for degree=1 is above 2 on the x
# axis! and there are no vertices of degree 0; hence no plotted point: 

length(which(degree(IR) == 0))

# This is why we wrote degree_domain in 4-Basic-Graph-Metrics.R to adjust the x axis. 
# Let's use both of the above solutions: 

plot(degree_domain(IR), 
     nonzero_degree_distribution(IR), 
     main="Routers Nonzero Degree Dist Log Log Adj", 
     xlab="k", ylab="p(k)", 
     log="xy")

# ***** We still got a warning: What is it for? 
head(degree_domain(IR))

# We might add a kmin parameter to degree_domain to allow us to
# specify kmin=1, but we would have to do the same to
# nonzero_degree_distribution so the lengths are the same. I have not
# done this yet.

# ***** Without reading ahead, why are there plateaus? (I am
#       referring to the flat structures at the bottom of the
#       plot.) What do these plateaus represent? 
#       Why is this a problem? 








# Plateaus are due to linear binning. More specifically, the
# bottom-most horizontal line is for degrees for which there is only
# one instance; the second horizontal line is likely for those with
# two instances; etc. We can see this by determining the probability
# of a value held by a single vertex and comparing to the smallest
# probabilities in the distribution:

1/vcount(IR) 
head(sort(unique(nonzero_degree_distribution(IR))), 3)

# 4.354832e-05 is slightly below 5e-05 on the y axis, so is consistent
# with where we see the plateau: That is, if a node's degree occurs
# with probability 4.354832e-05 (where we see the plateau), then there
# is just one node with that degree. So, each dot on the bottom
# plateau is a single node. Similarly, each dot on the next plateau is
# two nodes, etc.

# Why are the plateaus a problem? Barabasi: insufficient data points 
# in higher bins to fit curve. But if we are just visualizing, they
# are accurate as long as we interpret them correctly. 

# In particular, one dot does NOT stand for one vertex, so there being
# few dots on the upper left does not mean that there are few low
# degree nodes, and more dots on the lower right does not mean that
# there are more higher degree nodes! You must read the probability
# off the y axis. The probability of a low degree node is very high.

# **** Which degree k has the highest probability in this network? 
#      Can you tell from the plots alone? 

# Confirm: 
length(V(IR)[degree(IR)==1])
length(V(IR)[degree(IR)==2])
length(V(IR)[degree(IR)==3])

########################################
# Cumulative Distribution Function (CDF)

# Cumulative plots can smooth out the curve but change gamma.
# For each k, the cumulative distribution gives the probability
# that there are vertices of degree k OR GREATER. 

# Cumulative linear is not useful: it needs a log scale. 

plot(degree_domain(IR), 
     degree_distribution(IR, cumulative=TRUE), # note change 
     main="Routers Cumulative Lin Lin", 
     xlab="k", ylab="CDF p(k)")

# ***** Why use degree_distribution? Why don't we modify
#        nonzero_degree_distribution to allow cumulative=TRUE?

# Cumulative Log-Log is nicer, though we need to learn to interpret 
# the y axis. 

plot(degree_domain(IR), 
     degree_distribution(IR, cumulative=TRUE), 
     main="Routers Cumulative Log Log", 
     xlab="k", ylab="CDF p(k)", 
     log="xy")

# Cumulative Distribution Function is often used in the literature: 
# be comfortable reading them. First examine the x and y axes. Then 
# be sure you can answer these questions.

# ********** Why is the left most point always at p(k) = 1.0 
#            in cumulative plots? 

# ********** What do the y axis positions of the first three 
#            points from the left in the log-log plots mean?

# ********** Where are these points in the lin lin plots? 
#            Match values of d_dis carefully to y axes and
#            notice the difference in y axis spacing

# ********** Why are there short horizontal plateaus in the 
#            high degree region of Cumulative Log Log? What 
#            do they mean? What is the corresponding feature 
#            in the noncumulative Log Log plot? 
#            (Hint: mind the gaps!)

#################################################################
# Log Binning 
#################################################################
# It's surprising to me that log binning is not built in to R or
# igraph plots. We can try writing our own. Here is one approach. 

data <- degree(IR)

# How many bins? Since we are log binning base 2 ...

length(data)
log(length(data), base=2)

# Need an integer number of bins, and let's save it

(numbins <- ceiling(log(length(data), base=2)))

# Compute the breakpoint values for the bins (+1 for boundaries)
# This is what it would look like for linear binning: 

?seq
seq(min(data), max(data), length.out=numbins+1) 

# Each being this wide: 

(max(data)-min(data))/numbins

# For log binning, first generate a linear sequence of exponents base e:

seq(log(min(data)), log(max(data)), length.out=numbins+1)

# Each being this wide, but remember this is the exponent: 

log(max(data))/numbins

# We now use the exponents of e to generate sequence of actual 
# degree values that bound the bins: 
(breakpoints <- exp(seq(log(min(data)), 
                        log(max(data)), 
                        length.out=numbins+1)))
# Notice how the bins get wider

# ***** Question: If the number of bins was computed with log base=2, 
#       don't we need to use log base=2 in computing the exponents? 

# Compare this to the above: 

2^(seq(log(min(data), base=2), 
       log(max(data), base=2), 
       length.out=numbins+1))

# We'll make a histogram table but suppress plotting so we can
# control visualization

?hist
binhist <- hist(data, breaks=breakpoints, plot=FALSE)
summary(binhist) # Notice the numeric vectors 

# new_window("Log Binning Tests", 12, 12)
# par(mfrow=c(2,2))
# par(cex.axis=1.2, cex.lab=1.2)

# The $mids give us the x axis values and $density the y or p values

head(binhist$mids)
head(binhist$density)
plot(binhist$mids, binhist$density, main="Routers Log Log Binned v1", 
     xlab="k", ylab="p(k)", log="xy")

# Let's package that up into a function. This will be in binned_histogram.R

binned_histogram <- function(data, base=2) {
	numbins <- ceiling(log(length(data), base))
	breakpoints <- exp(seq(log(min(data)), 
	                       log(max(data)), 
	                       length.out=numbins+1))
	return(hist(data, breaks=breakpoints, plot=FALSE))
}

# Summary of use: this should give an identical plot with less work

IR.bh <- binned_histogram(degree(IR))
plot(IR.bh$mids, IR.bh$density, main="Routers Log Log Binned v2", 
     xlab="k", ylab="p(k)", log="xy")

# Unfortunately there is a problem ...

PB <- read_graph("Networks/political-blogs.graphml", format="graphml")
PB.bh <- binned_histogram(degree(PB))

# Investigating ... 

min(degree(PB))
log(min(degree(PB)))

# We need to remove isolate nodes! 

# A function to get degree sequence without isolate nodes. Important:
#   length(nonzero_degrees(g)) <= length(degrees(g)) = length(V(g))
# This will be in Utility/nonzero_degrees.R 

nonzero_degrees <- function(g, mode="total") {
        d <- degree(g, mode=mode) 
        return(d[d != 0])
}

# Now it works, but we need to be aware that isolates are left out.

PB.bh <- binned_histogram(nonzero_degrees(PB))
plot(PB.bh$mids, PB.bh$density, 
     main="Political Blogs Binned Degree Dist", 
     xlab="k", ylab="p(k)", log="xy")

# Compare to the other options for this network 

# The other way to get rid of linear binning plateus
plot(degree_domain(PB), 
     degree_distribution(PB, cumulative=TRUE),
     main="Political Blogs Cumulative Degree Dist", 
     xlab="k", ylab="p(k)", log="xy")

# If we want to see the individual values 
plot(degree_domain(PB),
     nonzero_degree_distribution(PB),
     main="Political Blogs Log-Log Degree Dist", 
     xlab="k", ylab="p(k)", log="xy")

# Least useful 
plot(degree_domain(PB), 
     nonzero_degree_distribution(PB),
     main="Political Blogs Lin-Lin Degree Dist", 
     xlab="k", ylab="p(k)")

#################################################################
# Summary: Compare these plots 

new_window("Summary of Plot Methods on IR", 12, 15)
par(mfrow=c(3, 2))
par(cex.axis=1.5, cex.lab=1.5)

# Plain vanilla plot 
plot(degree_distribution(IR), 
     main="Routers Unadjusted Lin Lin", xlab="k", ylab="p(k)")

# That was way too dense: remove values p(k)=0 
plot(nonzero_degree_distribution(IR), 
     main="Routers Nonzero Lin Lin", xlab="k", ylab="p(k)")

# Still hard to read: squashed on axes. Try log-log: 
plot(nonzero_degree_distribution(IR), 
     main="Routers Log Log Nonzero", 
     xlab="k", ylab="p(k)", log="xy")

# The axis is shifted, placing p(1) above k=2: adjust 
plot(degree_domain(IR), 
     nonzero_degree_distribution(IR), 
     main="Routers Nonzero Adjusted Domain Log Log", 
     xlab="k", ylab="p(k)", log="xy")

# Need to get rid of linear binning. Two solutions, log binning: 
IR.bh <- log_binned_histogram(degree(IR))
plot(IR.bh$mids, 
     IR.bh$density, 
     main="Routers Binned Log Log", 
     xlab="k", ylab="p(k)", log="xy")

# Or use Cumulative plot with adjusted domain 
plot(degree_domain(IR), 
     degree_distribution(IR, cumulative=TRUE), 
     main="Routers Cumulative Log Log", 
     xlab="k", ylab="CDF p(k)", log="xy")

# ***** Which do you think is easiest to understand? 
#       Different ones for different purposes? 

#################################################################
# Activity: plot degree distribution for another network 
# * Read in cit-HepTh.gml and compute degree distribution 
# * Make linear plot, and see how much degree_domain matters
# * Make log-log plot, and see how much degree_domain matters
# * Make cumulative and binned_histogram plots 
# * How would you handle a directed graph?  
# See 6-2-Plotting-Activity.R for template 
# See 6-2-Plotting-Activity-Solution.R for solution once you
#   have tried it. 
#################################################################
# Pau 