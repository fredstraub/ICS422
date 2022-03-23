######################################################################
# Topic 10 Nominal Assortativity Class Demo 
# Dan Suthers, March 16, 2017; March 13, 2018 
# Aug 28 2018 DS: Minor updates for current script style and clarity
# Oct 29 2019 DS: Minor updates for fall 2019 class
# Mar  7 2020 DS: Minor updates for self study students. 
# Mar 23 2021 DS: Minor updates for spring 2021 422/622
# Mar 22 2022 DS: Minor updates for spring 2022 422/622
######################################################################
# Setup 

library(igraph)
library(tibble)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 

source("Utility/degree_domain.R")

# A "Web Atlas" of European Science in Society websites. Edges
# are links. Node attributes indicate type of web site and country.

WA <- read_graph("Networks/EuroSiS-WebAtlas-Multigraph.graphml", 
                 format="graphml")

######################################################################
# Choosing and Preparing Variables for Nominal Assortativity 
######################################################################
# Nominal assortativity lets us answer the question: given a 
# categorization of vertices, to what extent are vertices connected
# within types (or between types) more than expected at random? 

summary(WA)

# Obviously I chose this graph because there are many attributes to 
# consider for computing nominal (categorical) assortativity! 

# ***** Take a moment to look at the graph in Gephi Data Laboratory

# The "tag_" attributes are 0,1. These are just binary: the attributes 
# with more values may be more interesting: country and actor_type. 

# ---------------------
# Nominal assortativity 

?assortativity # all are documented together 

# We need to supply a 'types' vector of vertex types, and we can
# compute directed or undirected assortativity.

# ***** We have the option of supplying a second types vector. When
#       might we want to do this?

# Note that types1 and types2 can arbitrary numeric values, but nominal
# types must be integers starting at 1. How then do we compute nominal
# assortativity with our country and actor_type data?

head(V(WA)$country)              # not numeric 
head(as.integer(V(WA)$country))  # Can't convert character vector 
head(as.factor(V(WA)$country))
# So make it a factor first: 

WA_country_nums <- as.integer(as.factor(V(WA)$country))
head(WA_country_nums) # numeric 

# Let's do this for actor_type as well: 

WA_actor_nums <- as.numeric(as.factor(V(WA)$actor_type))

######################################################################
# Interpreting Nominal Assortativity and Modularity
######################################################################
# Which of these attributes is more predictive of connectivity? 

assortativity_nominal(WA, WA_country_nums)
assortativity_nominal(WA, WA_actor_nums)

# ***** What's your conclusion? What is a plausible explanation?
#people are more likely to be associated by country
#       Remember getting that result in the visualization homework? 

# Nominal assortativity is modularity scaled to have a max of 1 

?modularity 

# 'membership' plays the same role as assortativity 'types' 

# Compare modularity to assortativity ... 

tibble(Attribute     = c("country", "actor-type"),
       Assortativity = c(assortativity_nominal(WA, WA_country_nums),
                         assortativity_nominal(WA, WA_actor_nums)),
       Modularity    = c(modularity(WA, WA_country_nums), 
                         modularity(WA, WA_actor_nums)))

# ***** Why do the relative magnitudes above make sense? Explain
#       why assortativity is larger than modularity. 

# Unlike the assortativity methods, modularity can use weights. 
# But it is NOT using weights by default! 

modularity(WA, WA_country_nums)  # Default

# Same as if we give weights of 1

modularity(WA, WA_country_nums, weights=rep.int(1, ecount(WA))) 

# Slightly different if we tell it to use the weights (weights are not
# a major factor in this data set).

modularity(WA, WA_country_nums, weights=E(WA)$weight)

# We can try a few of the binary attributes. They are character data
# "0" and "1" but we can easily convert to {1, 2}

# ***** Which do you think will have higher assortativity? 

assortativity_nominal(WA, (as.integer(V(WA)$tag_research) + 1))
assortativity_nominal(WA, (as.integer(V(WA)$tag_space) + 1))
assortativity_nominal(WA, (as.integer(V(WA)$tag_gender) + 1))

# You may want to try assortativity on other WA tag attributes. 

######################################################################
# Are the nominal assortativities structural? 
######################################################################
# Recall that some network properties can be a consequence of the 
# degree distribution. To see whether this is the case, we rewire 
# the network randomly and see whether the property changes. (Rewire
# rather than using the configuration model because we need vertex
# attributes!) 

?rewire   # just as a reminder
f <- 1000 # Change f to 100 on a slow computer.
WA.rewired <- rewire(WA, with = keeping_degseq(niter = ecount(WA)*f))

assortativity_nominal(WA, WA_country_nums)
assortativity_nominal(WA.rewired, WA_country_nums)

assortativity_nominal(WA, WA_actor_nums)
assortativity_nominal(WA.rewired, WA_actor_nums)

# ***** Is the assortativity structural, or is it independent of 
#       degree distribution? Why do these results make sense?

# Nominal assortativity, if found, is rarely structural, because 
# vertex attributes are usually independent of degree. But one 
# can think of attributes that lead to fitness differences. 

######################################################################
# Demonstration of structural cutoffs
######################################################################
# Following Barabasi's slide, make a scale free network with and without
# multiedges. We expect the simple graph to show structural cutoffs.
# ***** Review why that is the case.

g.mult <- sample_fitness_pl(10000, 100000, 2.1, 
                            loops=TRUE, multiple=TRUE, 
                            finite.size.correction = FALSE)

g.simp <- sample_fitness_pl(10000, 100000, 2.1, 
                            loops=FALSE, multiple=FALSE, 
                            finite.size.correction = FALSE)

# Plot them both on the same figure. 

plot(degree_domain(g.mult), 
     degree_distribution(g.mult, cumulative=TRUE), log="xy", 
     main="Simple and Multi Degree Dist", xlab="k", ylab="p(k)", 
     pch=18, col="blue")

points(degree_domain(g.simp), 
       degree_distribution(g.simp, cumulative = TRUE), 
       pch=20, col="red")

legend("bottomleft", bty="n",
       c("simple", "multi"), 
       col   =c("red", "blue"),
       lty=1, lwd=5)

# Clearly the requirement that a graph be simple imposes a structural
# cutoff on higher degree nodes. This affects degree assortativity,
# which we cover in the next demo:

assortativity_degree(g.mult)
assortativity_degree(g.simp)

# We are reminded that structural assortativity is always
# disassortative.

######################################################################
# Activity: Try it with Yeast Proteins and Political Blogs. 
######################################################################
# I chose these two because they have attributes. 

####################
# Political Blogs 

PB <- read_graph("Networks/political-blogs.graphml", format="graphml")
summary(PB)
unique(V(PB)$value)

# ***** The 'value' attribute represents left vs. right. Try nominal 
#       assortativity on this: 

PB_side <- as.integer(as.factor(V(PB)$value))
assortativity_nominal(PB, PB_side)

####################
# Yeast Proteins 

YP <- read_graph("Networks/Yeast-Proteins.graphml", format="graphml")
summary(YP)
unique(V(YP)$Class)

# Try nominal assortativity by Class. We may not have the biological 
# knowledge to interpret fully, but we'll speculate: 

YP_class <- as.integer(as.factor(V(YP)$Class))
assortativity_nominal(YP, YP_class)

###################################
# Is the assortativity structural? 
# Rewire to find out you have time ... 

######################################################################
# Pau 