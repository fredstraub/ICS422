######################################################################
# Topic 10 Degree Correlations and knn(k)
# Class demo, Network Science 
# Dan Suthers, March 16, 2017; March 15, 2018
# Aug 28, 2018 DS: Minor updates for current script style and clarity
# Oct 31, 2019 DS: Minor updates for fall 2019 class
# Mar  7 2020 DS: Minor updates for self study students.
# Mar 25 2021 DS: Minor updates for spring 2021 422/622
# Mar 24 2022 DS: Updates for spring 2022 422/622 (knn mode)
######################################################################

library(igraph)
library(tibble)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

######################################################################
# Graphs we will work with. (May already be read in from prior demo.)

# Undirected example - fairly dense
YP <- read_graph("Networks/Yeast-Proteins.graphml", format="graphml")
summary(YP)

# Directed example - also dense  
PB <- read_graph("Networks/political-blogs.graphml", format="graphml")
summary(PB)

# A larger, undirected and connected graph 
IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")
summary(IR)

######################################################################
# Computing and interpreting degree correlations 

# Let's review the methods available 

?assortativity

# assortativity_degree is easy to request: we don't need attribute data 

assortativity_degree(PB)

# This is a directed graph: if we wanted to simplify interpretation by
# making it undirected we'd do: 

assortativity_degree(PB, directed=FALSE)

# Before we put effort into interpretation, we should see whether it
# is structural. Let's rewire all our networks and speculate while
# this runs:

f <- 1000 
YP.rewired <- rewire(YP, with = keeping_degseq(niter = ecount(YP) * f))
PB.rewired <- rewire(PB, with = keeping_degseq(niter = ecount(PB) * f))
IR.rewired <- rewire(IR, with = keeping_degseq(niter = ecount(YP) * f))

# (It may be better to run many trials, averaging assortativity 
# across trials.) 

# Results: If the original and rewired assortativities are
# indistinguishable, degree correlation is structural. If they differ,
# then degree correlation is due to exogenous process.

A <-
        tibble(
                Graph       = c("Original", "Rewired"),
                Yeast       = c(assortativity_degree(YP),
                                assortativity_degree(YP.rewired)),
                Routers     = c(assortativity_degree(IR),
                                assortativity_degree(IR.rewired)),
                
                PolyBlogUnd = c(
                        assortativity_degree(PB, directed = FALSE),
                        assortativity_degree(PB.rewired, directed = FALSE)),
                PolyBlogDir = c(assortativity_degree(PB),
                                assortativity_degree(PB.rewired))
        )
A
view(A)

# ***** How do you interpret these? For which one(s) does assortativity
#       seem to be a consequence of the degree distribution (and hence
#       whatever processes produce that distribution), and for which 
#       are some nonstructural process responsible? 

# ***** How to interpret directed assortativity? See Newman (2003): 
#       out --> in, so positive assortativity is like hubs referencing 
#       authorities, and negative is hubs referencing low degree blogs 
#       (Blogs for Bush) and small blogs referencing authorities 
#       (e.g., Instapundit being the authority).

######################################################################
# Visualizing Degree Correlations

# We can visualize degree correlations with knn(k), the average degree
# in the network neighborhood as a function of degree. The following  
# computes both knn, the vector of average neighbor degree, and knnk,
# the knn(k) distribution: 

?knn 

# The weights option is a bit confusing (read it). We won't use
# weights today: Only PB has weights in today's networks, and they are
# all 1:

table(E(PB)$weight)

# Let's start with Yeast Proteins:

YP.knn <- knn(YP)
summary(YP.knn)

# ***** Why do the two vectors have the lengths shown in the summary? 
#       Type in two expressions that show what the lengths are based on. 

##############################
# Quick aside: The Friend Paradox

mean(degree(YP)) # Average degree of a node in the graph 
mean(YP.knn$knn) # Average degree of the neighbors of a node 

# ***** Why do your friends have more friends than you do? 

# Now here is the answer to the question in the previous section. 
# Read the Value section in ?knn to understand. 

vcount(YP)
max(degree(YP))

##############################
# Degree Correlations Differ from Degree Distributions: We'll plot the
# latter just for comparison.

plot(degree_domain(YP), degree_distribution(YP), log="xy", 
     main="Yeast Protein Degree Dist", xlab="k", ylab="p(k)") 

##############################
# Plot knnk for the original and randomized graphs.
# (knnk represents knn(k) as a vector of knn for each k)

YP.rewired.knn <- knn(YP.rewired, weights=NA)
new_window("Degree Correlation Examples", 12, 6)
par(mfrow=c(1,2))

plot(YP.knn$knnk, main="YP knn(k)", 
     log="xy", xlab="k", ylab="knn(k)")
legend("topleft", bty="n", cex=1.2, 
       c(paste0("r = ", round(assortativity_degree(YP), digits=4))))

plot(YP.rewired.knn$knnk, main="YP Rewired knn(k)", 
     log="xy", xlab="k", ylab="knn(k)")
legend("bottomleft", bty="n", cex=1.2,
       c(paste0("r = ", 
                round(assortativity_degree(YP.rewired), digits=4))))

# ***** How do you interpret the change in distribution/slope? 

##############################
# What info does knn(k) hide? 

# The degree distribution indicates there are a lot of nodes of degree
# 1, but they are summarized with one point in the knn(k) plot.

# Plot knn for k of each vertex. Compare to plot of knn(k), 
# the mean of knn across all vertices of degree k. 

V(YP)$k   <- degree(YP)
V(YP)$knn <- knn(YP, weights=NA)$knn

# Repeat the previous plot so they are side by side 

plot(knn(YP, weights=NA)$knnk, main="YP knn(k)", 
     log="xy", xlab="k", ylab="knn(k)")
plot(V(YP)$k, V(YP)$knn, main="YP k vs knn",
     log="xy", xlab="k", ylab="knn")

# This shows that they smooth things out by providing one data point
# (the average) per value of k. But by plotting k vs knn we get a
# sense of the spread of values (variance) for each k, which can be
# substantial! But why is the point in knn(k) so low? 

# There are actually even more data points hiding by overlapping. 
# "Jitter" puts some random variation in values to separate them: 

plot(V(YP)$k, V(YP)$knn, main="YP k vs knn",
     log="xy", xlab="k", ylab="knn")
plot(jitter(V(YP)$k), V(YP)$knn, main="YP k jittered vs knn",
     log="xy", xlab="k", ylab="knn")

# Jitter lets us see that there are many low degree values. 

######################################################################
# Utility function: plotting source and rewired graphs 
# This will enable us to do more examples quickly. 
# For now we will stick with unweighted and undirected. 

plot_assortativity_rewiring <- function(name, source, rewired) {
	source.knn <- knn(source, weights=NA)
	rewire.knn <- knn(rewired, weights=NA)
	plot(source.knn$knnk, log="xy",
	     main=paste(name, "knn(k)"), xlab="k", ylab="knn(k)")
	legend("bottomleft", bty="n", cex=1.2,
           c(paste0("r = ",
                    round(assortativity_degree(source, directed=FALSE),
                          digits=4))))
	plot(rewire.knn$knnk, log="xy", 
	     main=paste(name, "Rewired knn(k)"), xlab="k", ylab="knn(k)")
	legend("bottomleft", bty="n", cex=1.2,
           c(paste0("r = ", 
                    round(assortativity_degree(rewired, directed=FALSE),
                          digits=4))))
}

# Show it works 

plot_assortativity_rewiring("YP", YP, YP.rewired)

# What about that synthetic example from 10-1 demo?
# f=1000 takes a minute or more to rewire; so use 100

g.simp <- sample_fitness_pl(10000, 100000, 2.1, 
                            loops=FALSE, multiple=FALSE, 
                            finite.size.correction = FALSE)
g.simp.rewired <- 
        rewire(g.simp,
               with = keeping_degseq(niter = ecount(g.simp) * 100))
plot_assortativity_rewiring("Synthetic", g.simp, g.simp.rewired)

# Clearly supports structural disassortativity. 

######################################################################
# More examples

# Internet Routers

plot_assortativity_rewiring("Internet Routers", IR, IR.rewired)

# ***** Is the assortativity structural? Your interpretation? 

# Political Blogs 

plot_assortativity_rewiring("Political Blogs", PB, PB.rewired)

# Oops!

is_simple(PB)

# Let's simplify, but will the correlation change? 

summary(PB)
PB.simp <- simplify(PB)
summary(PB.simp)
assortativity_degree(PB)
assortativity_degree(PB.simp)

# Not that different; we can use it. What about the rewired version?  

is_simple(PB.rewired)

# Let's rewire the simplified version (change to 100 if on slow computer)

f <- 1000
PB.simp.rewired <- rewire(PB.simp, 
                          with = keeping_degseq(niter = ecount(PB) * f))
assortativity_degree(PB.rewired)
assortativity_degree(PB.simp.rewired) # Also very close 

# Now we can finish plotting 

plot_assortativity_rewiring("Political Blogs", PB.simp, PB.simp.rewired)

# Is degree assortativity structural? Your interpretation? Compare to
# the 'value' left-right nominal assortativity:

leftright <- as.numeric(V(PB)$value)
assortativity_nominal(PB, leftright)
assortativity_degree(PB)

# ***** Fill in the blank: PB is __________ by political orientation,
# while being __________ by degree, but this is structural.

######################################################################
# Activity
######################################################################
# Compute and interpret degree correlations for the following
# * Compute the degree correlations using assortativity_degree 
# * Rewire the network to determine whether it is due to structural
#   cutoffs or exogeneous
# * Identify any explanatory domain processes 
# You may use the function for the second part. 

# Network Science 

NS <- read_graph("Networks/netscience.graphml", format="graphml")
summary(NS)


# High Energy Physics: Recall that this is a citation network 

HEP <- read_graph("Networks/cit-HepTh.gml", format="gml")

# This is not a simple graph, so let's simplify before proceeding.

is_simple(HEP)
HEP.simp <- simplify(HEP)

# How much changed? 

summary(HEP)
summary(HEP.simp)
ecount(HEP) - ecount(HEP.simp) # insignificant 

# Can you compute the 4 directed assortativities? 
# in-in, in-out, out-in and out-out. What about knn? 

?assortativity
?knn 

######################################################################
# Pau
