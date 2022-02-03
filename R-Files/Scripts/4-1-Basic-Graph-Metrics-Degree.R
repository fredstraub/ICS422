######################################################################
# 4-1 Basic Graph Metrics: Degree and Plotting Degree Distributions
# Dan Suthers, August 31, 2016; updated January 25, 2018
# Aug 24 2019 DS: Added table(degree(TI))
# Sep 12 2019 DS: Edited for fall 2019 class. 
# Sep 24 2019 DS: Clarification of when degree_domain is needed
# Jan 25 2020 DS: Minor edits of comments for self/online study. 
# Feb  2 2021 DS: Updates for 422/611 class. 
# Feb  4 2021 DS: Fixed issue of plotting 0.0 in degree_distribution 
# Feb  1 2022 DS: Updates for Spring 2022 422/622. 
#   Split into two parts:
#   - Degrees and Plotting (35-37 min) and Coding degree_domain (15 min) 
#   - Other metrics (distances, components, clustering) (45 min) 
######################################################################

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to your location

######################################################################
# Reading Graphs we will use 
######################################################################

# Co-authorship relationships between authors of early papers in 
# the field of Network Science. 

NS <- read_graph("Networks/netscience.graphml", format="graphml")

# A subset of the Java classes hierarchy 

JC <- read_graph("Networks/java-classes.graphml", format="graphml")

# Contributions in a "Tapped-In" chat room, and inferred relationships
# between them, annotated in our class exercise last week. 

TI <- read_graph("Networks/TappedInChatSampleAnnotated.graphml", 
                 format="graphml")

# ***** Please also load these graphs into Gephi, or use provided 
#       4-Graph-Metrics.gephi, which already has layout. 

######################################################################
# Inspecting Graphs 
######################################################################
# How many edges and vertices? Is it Directed? Weighted? Simple? 

# ========== In Gephi: ==========
# First briefly discuss the networks that these graphs model. 
# Context panel shows |V|, |E|, directed/undirected. 
# Look at edges in Data Laboratory for weights 
# No simple graph test that I know of.

# ========== In igraph: ==========
# Always check this when reading in a new graph

summary(NS)  # U-W- weighted but not directed 
summary(JC)  # D--- directed but not weighted; similar size, more edges
summary(TI)  # D-W- a small directed weighted graph with attributes 

# ***** Why is JC unweighted and TI weighted in igraph, when both
#       load with edge weights in Gephi? 
#       (Hint: see .graphml headers) 

# Basic things we want to know before computing metrics 
# Some algorithms don't work on non-simple graphs 

is_simple(NS)
is_simple(JC)
is_simple(TI)

# We can read from summary whether directed, but this lets us test in
# code.

is_directed(NS) 
is_directed(JC)
is_directed(TI)

# ***** Why do the results make sense in terms of domain modeling? 
#       Why *should* NS be undirected and the other two directed? 

# Also need to attend to whether weights are included in computations. 

is_weighted(NS) 

# Different ways to look at distribution of weights.

?table
table(E(NS)$weight)
View(table(E(NS)$weight)) # better way to look at big things 
?hist
hist(E(NS)$weight) # but we can get a finer grained histogram
hist(E(NS)$weight, breaks=length(unique(E(NS)$weight)))

# Looking at the other two 

is_weighted(JC) 
is_weighted(TI) 

# ... but ... 

table(E(TI)$weight)

# A weight attribute was defined in graphml but all values are 1.0. 
# This leads us to question whether they are meaningful. 
# ***** Let's examine the pathway of source files to find out what
#       happened.

######################################################################
# Degree and Degree Distribution
######################################################################
# We will find that degree distribution affects almost everything
# See "Degree, Average Degree and Degree Distribution" in slides. 

# ========== In Gephi: ==========
# Statistics Panel: Average Degree,  Avg. Weighted Degree
# Run on both Network Science and Java Classes 
# The Gephi plots are not easy to read: most values are along the axes
# Look at what running this added in Data Laboratory 

# ========== In igraph: ==========

?degree

# Vector of degrees. Large graphs will fill your screen.
# Normally use this only to pass it to something else (next example)

degree(TI)

# Average or mean degree
# (Next week we'll find out why this is not very useful.)

mean(degree(JC))
mean(degree(JC, mode="in"))  # Why half? 
mean(degree(JC, mode="out")) # Why the same?

# ***** Which matches Gephi? 
#       Where does Gephi tell you what it is computing? 

# What about weighted degree? It is 'strength'

?strength
head(degree(NS), 10)
head(strength(NS), 10)
mean(degree(NS))
mean(strength(NS))

# ***** What does the difference tell you about the edge weights?
#       What do these weights mean? 




#       Hint: Evaluate this; then look at the actual weights in Gephi:

mean(E(NS)$weight)



#       Hint: Evaluate 1/n for small integers in R 

1/(1:10)


#       Hint: How much credit should each author get for a paper
#       if there are two authors? If three authors? ... 


# But that is not exactly what Newman did. Relevant passage from 
# page 5 of Newman (2001) Phys. Rev. E 64, 016132): 
# "we weight collaborative ties inversely according to the number of
# coauthors as follows. Suppose a scientist collaborates on the
# writing of a paper that has n authors in total, i.e., he or she has
# n-1 coauthors on that paper. Then we assume that he or she is
# acquainted with each coauthor 1/(n-1) times as well, on average, as
# if there were only one coauthor." 

# Degree distribution gives proportion of vertices of each degree,
# including 0

?degree_distribution

# These are the possible values 

sort(unique(degree(TI))) 

# A table of the frequency of each value 

table(degree(TI))

# The proportion of nodes having each value

(TI.dd <- degree_distribution(TI))

# Now let's plot this vector of probabilities. Compare the following
# plots to the above, and to the plot Gephi shows.

plot(TI.dd, main="Tapped In Chat Sample Degrees v1") 

# ***** Do you see a discrepancy with the console results? 
# ***** Can you explain it? Reply before scrolling ... 







# Compare the headers on the table output with "Index" in the plot. 
# The x axis is not aligned properly (off by 1). 

# Take a closer look at the documentation of Value returned 

?degree_distribution

# To fix the plot, we might specify the x and y values

plot(sort(unique(degree(TI))), # x values 
     TI.dd,                    # y values 
     main="Tapped In Chat Sample Degrees v2") 
# Notice that the x and y axis labels are the expressions we gave 

# That won't work if values are nonconsecutive, as in NS ... 

sort(unique(degree(NS)))          # mind the gaps! 
length(sort(unique(degree(NS))))
length(degree_distribution(NS))   # not the same 
degree_distribution(NS)           # because some are 0! 

# If we try to do what we did for TI ... 

plot(sort(unique(degree(NS))), degree_distribution(NS)) # error 

# We need ALL the x values, for example, using 

0:34 # to generate integers from 0 to 34. 
plot(0:34,                    # x values 
     degree_distribution(NS), # y values 
     main="Network Science Degree Distribution 1")
     
# That label "0:34" is not very informative: we can give labels to
# indicate we are plotting probability of degree k against k: 

plot(0:34,                    # x values 
     degree_distribution(NS), # y values  
     main="Network Science Degree Distribution 2", 
     xlab="k", ylab="p(k)")

# Compare that to the Gephi plot for degree distribution: igraph's is
# more readable but there is one thing Gephi does better. 
# ***** What? ... 


# The 0 values make it hard to read. Turns out we can fix this by 
# replacing the 0 values we do not want plotted with NA! 

plot(0:34,
     ifelse(degree_distribution(NS)==0.0, NA, degree_distribution(NS)), 
     main="Network Science Degree Distribution 3", 
     xlab="k", ylab="p(k)")

######################################################################
# Coding Tutorial: Can we write code that does this for all graphs? 
# (Includes introduction to functions, and debugging. 15 min.) 
######################################################################

# We want a function that generates x axis values from 0 to max
# degree. Building it up part by part by example with NS:

NS.deg <- degree(NS)    # get the degrees 
max(NS.deg)             # find maximum 
0:max(NS.deg)           # Here's the domain we want on the x axis 

# Let's package this up in a helper function (beta as we'll improve it)

degree_domain_beta <- function(g) {
	return(0:max(degree(g)))
}
degree_domain_beta(NS)
degree_domain_beta(TI)

# Use that to plot. 

plot(degree_domain_beta(NS),  # the new part 
     ifelse(degree_distribution(NS)==0.0, NA, degree_distribution(NS)), 
     main="Network Science Degree Distribution 4", 
     xlab="k", ylab="p(k)")
     
# Verify it works on a larger graph 

plot(degree_domain_beta(JC), 
     ifelse(degree_distribution(JC)==0.0, NA, degree_distribution(JC)),
     main="Java Classes Degree Distribution", xlab="k", ylab="p(k)")

# Just to reinforce why we want to remove 0 values: 

plot(degree_domain_beta(JC), 
     degree_distribution(JC), 
     main="Java Classes Degree Distribution with 0.0", 
     xlab="k", ylab="p(k)")

# This includes 0 values that are obscuring the low probability nodes
# on the bottom. If you look carefully you can see them sticking up
# over the black bar. 

# We can also compare in and out degree distribution. However, 
# first we need to modify our function to add a mode parameter: 

?degree                                      # and read about mode 
degree_domain <- function(g, mode="all") {   # added mode
	return(0:max(degree(g, mode=mode)))
}
# NOTE: This function is provided in Utility/degree_domain.R 

# Let's also make a function for removal of 0 values: 

nonzero_degree_distribution <- function(g, mode="all") {
        dd <- degree_distribution(g, mode=mode) # compute once 
        ifelse(dd == 0.0, NA, dd)
}
# NOTE: This function is provided in Utility/nonzero_degree_distribution.R 

# Let's use it for Java Classes ... 

plot(degree_domain(JC, mode="in"), 
     nonzero_degree_distribution(JC, mode="in"), 
     main="Java Classes In-Degree Distribution", xlab="k", ylab="p(k)")     
plot(degree_domain(JC, mode="out"), 
     nonzero_degree_distribution(JC, mode="out"), 
     main="Java Classes Out-Degree Distribution", xlab="k", ylab="p(k)")

# ***** How are they different? Why are they different, in terms of Java?

# End of Coding Tutorial

######################################################################
# Further Comments 

# We will learn how to improve these plots in the next few weeks. 
# A preview: using log scales on x and y axes can help. Compare:

plot(degree_domain(JC), 
     nonzero_degree_distribution(JC), 
     main="Java Classes Linear Degree Distribution", 
     xlab="k", ylab="p(k)")
plot(degree_domain(JC), 
     nonzero_degree_distribution(JC), 
     log="xy",  # This is added 
     main="Java Classes Log Degree Distribution", 
     xlab="k", ylab="p(k)")

# Notice the warnings: 0 values are not included. So why did I do 
# the degree domain adjustment? Look what happens when I leave it off: 

plot(degree_distribution(JC), log="xy", 
     main="Java Classes DD: Unadjusted x Axis", 
     xlab="k", ylab="p(k)")

# It starts at 2, yet the minimum is 1: 
min(degree(JC))

# ==> The degree_domain adjustment is needed whenever plotting
#     degree_distribution and we want to label the x axis as k, 
#     as the value for p(1) will always be at position 2!

######################################################################
# Pau 