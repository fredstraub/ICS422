######################################################################
# 4-2 Basic Graph Metrics: Distances, Components, Clustering etc. 
# Dan Suthers, August 31, 2016; updated January 25, 2018
# Aug 24 2019 DS: Added table(degree(TI))
# Sep 12 2019 DS: Edited for fall 2019 class. 
# Sep 24 2019 DS: Clarification of when degree_domain is needed
# Jan 25 2020 DS: Minor edits of comments for self/online study. 
# Feb  2 2021 DS: Updates for 422/611 class. 
# Feb  4 2021 DS: Fixed issue of plotting 0.0 in degree_distribution 
# Feb  3 2022 DS: Updates for Spring 2022 422/622. 
#   Split into two parts:
#   - Degrees and Plotting (35-37 min) and Coding degree_domain (15 min) 
#   - Other metrics (distances, components, clustering) (45 min)
#   Numerous improvements in comments, particularly with respect to
#     discrepancies between Gephi and igraph. 

######################################################################
getwd()

library(igraph)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to your location

# Feb  4 2022 DS: Resolving the discrepancy between igraph and Gephi 
#   on local clustering coefficient.
######################################################################


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
# Distances
# These give an idea of network "size" relevant to flow of entities 
# over paths (information, diseases, etc.)
######################################################################

# ========== In Gephi: ==========
# Statistics Panel: "Network Diameter" OR "Avg. Path Length" (both run)
# Ensure directed/undirected is correct and check normalization. 
# We will look at centralities later 
# Do this for Network Science and Java Classes (Chats is linear)

# ========== In igraph: ==========

?mean_distance # ICS students, note the algorithms you studied in 311! 

mean_distance(NS)
mean_distance(JC)
mean_distance(JC, directed=FALSE) 
# ***** Why is undirected distance shorter? 

# Diameter 

?diameter

diameter(NS) 
# ***** Why is this different from Gephi? 
#       Try to get igraph to give the same result. (Try NULL and NA) 
#       What does this say about what Gephi is doing? 

diameter(JC)
diameter(TI)  
# ***** TI is the smallest graph: why longer distance? 

?distances # for all of them (usually TMI)
head(distances(TI), 1) # just the first start vertex! 
# ***** Why are some values Inf? (see parameter unconnected)


######################################################################
# Connected Components
# If the graph is not connected we need to characterize the pieces 
######################################################################

# ========== In Gephi: ==========
# Statistics Panel: "Connected Components"
# Notice strongly and weakly connected results for Java Classes. 
# Can visualize with color if it helps 

# ========== In igraph: ==========

?components # look at the Value named list items 

# ?count_components is the same page, though left out of the index

count_components(NS)

count_components(JC) 

# Clearly the default is WCC. 
# Documentation does not say we can specify, but we can: 

is_connected(JC, mode="weak")
count_components(JC, mode="weak")   # WCC 
is_connected(JC, mode="strong")
count_components(JC, mode="strong") # SCC

# Component distributions 

NS.cd <- component_distribution(NS)
head(NS.cd)
# ***** Why the first entry? See the documentation under Value! 
#       Any guesses as to what the programmer was thinking? 

# Let's see how component sizes are distributed, two ways 

table(NS.cd)
plot(NS.cd, 
     main="Network Science Component Sizes v1", 
     xlab="Nc", ylab="p(Nc)")


# We need to do our NA tick to not plot 0 values.

plot(ifelse(NS.cd==0, NA, NS.cd), 
     main="Network Science Component Sizes v2", 
     xlab="Nc", ylab="p(Nc)")

# Clearly we need log-log plots

plot(ifelse(NS.cd==0, NA, NS.cd), 
     main="Network Science Component Sizes v3", 
     xlab="Nc", ylab="p(Nc)", 
     log="xy")

# Notice that the count for components of size 1 is above the "2" on
# the x axis. We also need something analogous to degree_domain:

plot(0:(length(NS.cd)-1),
     ifelse(NS.cd==0, NA, NS.cd), 
     main="Network Science Component Sizes v4", 
     xlab="Nc", ylab="p(Nc)", 
     log="xy")

# From this we see there are many small strongly-connected components
# and a very few large components, with one giant component.

# components gives us membership, csize and count (no)

NS.c <- components(NS)
names(NS.c)

NS.c$no

JC.c <- components(JC, mode="strong")

# Make tables (frequency counts) of sizes: these are sometimes easier
# to interpret than plots

table(NS.c$csize)
table(JC.c$csize)

######################################################################
# Local Clustering Coefficient and Global Transitivity
# Measures of how much links are clustered versus spread out randomly
######################################################################

# ========== In Gephi: ==========
# Node Overview Panel: "Avg. Clustering Coefficient"
# I have a plug-in in Network Overview: "Clustering Coefficient" 
# (choose triangle method)

# ========== In igraph: ==========

?transitivity

# There are really just three types with various names. 
# Which is the default? 

transitivity(TI) 
transitivity(TI, type="average")
transitivity(TI, type="global")

# This gives local clustering coefficient for *each* vertex 

head(transitivity(TI, type="local"), 10)

# ***** What are the NaN, and why are they there? What about 0.0?
#       Clue: 

head(degree(TI), 10)

# ***** Explain the first NaN, the second one, and the 0.0 value

# Notice differences between average and global:

transitivity(NS, type="average")
transitivity(NS, type="global")

# ***** Which is Gephi using? What does Newman think? (See optional reading)


# **** Is one always smaller than the other? 


# Barrat or weighted gives a vector of local values: explaining this
# and understanding this would be too much of a diversion.
# transitivity(NS, type="weighted")

transitivity(JC, type="average")
transitivity(JC, type="global")

# ***** Compare to Gephi. Why the difference? Can we get Gephi's to
#       match igraph? What does that mean igraph is doing? 

# Is clustering sensitive to degree? (This relates to the claim that
# global is usually smaller than average local.)

plot(degree(NS), transitivity(NS, type="local"), main="NS degree vs CC")
# Possibly a trend here, but it's weak. 

plot(degree(JC), transitivity(JC, type="local"), main="JC degree vs CC")
# That's a new warning. To be safe: 

JC.und <- as.undirected(JC)
plot(degree(JC.und), transitivity(JC.und, type="local"), 
     main="JC Undirected degree vs CC")

# Easier to see log log: 

plot(degree(JC.und), transitivity(JC.und, type="local"), 
     main="JC Undirected degree vs CC", log="xy")

# ***** Why would this relationship be the case, in terms of the
#       structure of typical networks?

# What about the other metrics? Are they sensitive to degree? 
# Something to explore at home ... 

# ========== Sending igraph values to Gephi: ==========
# Since we have established that there are differences in how igraph
# and Gephi compute metrics, and igraph is better documented and more
# developed as a data analysis tool, we will trust the igraph values.
# But visualization is much easier in Gephi. What if we want to use
# igraph node-level metrics while conducting visualizations in Gephi?
# We already have seen a simple solution: assign the metrics as
# attributes in igraph and write out the graph for Gephi. We'll
# compare the local clustering coefficents computed in the two
# environments.

# Give each vertex its local transitivity (clustering coefficient): 

# (We will ignore the directed graph warning to use comparables.)

V(JC)$local_cc <- transitivity(JC, type="local") 
V(NS)$local_cc <- transitivity(NS, type="local")

# Although Gephi implies it can compute directed transitivity, we
# use undirected so we know that igraph's are correct. 

V(NS)$local_cc <- transitivity(NS, type="local")
V(JC.und)$local_cc <- transitivity(JC.und, type="local")


# To see what we assigned: 

summary(NS)
head(V(NS)$local_cc)

summary(JC)
head(V(JC)$local_cc) 

summary(JC.und)
head(V(JC.und)$local_cc)


# Here is the pattern you use for other attributes: 
#   V(graph)$attribute <- expression_to_compute_attribute(graph)

# Write them out to read into Gephi ... 


write_graph(JC, "java-classes_with_cc.graphml", format="graphml")
write_graph(NS, "netscience_with_cc.graphml", format="graphml")

# Open the graphml of both JC and NS in Gephi. No need to do layout.
# Compute Gephi's Avg. Clustering Coefficient. Then the local cc
# vertex attributes in Data Laboratory. Sort by the values to see NA
# vs 0.0.

write_graph(JC.und, "java_classes_und_with_cc.graphml", format="graphml")
write_graph(NS,     "netscience_with_cc.graphml", format="graphml")

# Open the graphml of both JC and NS in Gephi. No need to do layout.
# Compute Gephi's Avg. Clustering Coefficient. Then view the results
# and compare to the igraph local_cc vertex attributes in Data
# Laboratory. Sort by the values to compare.

# When examining the results for NS, note that some values of 0.0
# computed by Gephi correspond to blanks in the local_cc column: these
# are the NaN. Therefore Gephi does not distinguish the two situations
# (having a CC of 0.0 vs. CC being undefined.)

# ------ Might leave out of class, or edit to simpler resolution ----- 
# Examining the results for JC, the values are different. Why? 
# (This was not in the class demo, and may be TMI: the take-home 
#  message is that we should trust igraph on undirected graphs.)
# * In igraph we used type="local": the documentation says this is
#   for an undirected graph. 
# * In Gephi, save the results by copying to a new column, and 
#   recompute "Avg. Clustering Coefficient" as undirected and compare. 
# * The results are closer, but not the same! Rounding error? Or a
#   different assumption? (Recommend you copy these values to 
#   another column before going on.)
# * If you have the Clustering Coefficient plug-in, run it but use
#   "Basic Methods". They are the same as "Avg. Clustering Coefficient" 
#   undirected, but different from igraph local transitivity. 
# * Take a look again at transitivity documentation. What else can 
#   we change in igraph that might make them the same? 
#   - there are no weights
#   - change isolates to "zero": this does not solve it. 
#   - but there is a warning that we should use undirected graphs 

library('tibble')
cc <- tibble (local_dir_NaN  = transitivity(JC, type="local"),
              local_dir_zero = transitivity(JC, type="local", 
                                            isolates="zero"),
              local_und_NaN  = transitivity(JC.und, type="local"),
              local_und_zero  = transitivity(JC.und, type="local", 
                                             isolates="zero"))
View(cc)

# It turns out that the only way to get the results the same is to
# convert the network to undirected in igraph, and to use undirected
# in Gephi. I don't know whether the directed transitivity results in
# Gephi are correct, but given that igraph warns that directed
# transitivity may be incorrect and that Gephi is not as well vetted,
# I do not trust the directed local transitivity values in Gephi. The
# Gephi results window references a paper, but this paper says it is
# only for undirected graphs, so we don't even know what definition
# (if any) of directed transitivity Gephi is using.


######################################################################
# Degree Assortativity
# Do high degree nodes connect to high degree nodes, or to low? etc. 
# This is covered much later in the semester, but is also important,
# as it captures structure not captured by the above, and there are 
# related process or behavioral hypotheses. 
######################################################################

# ========== In Gephi: ==========
# Nothing available as far as I know 

# ========== In igraph: ==========

?assortativity_degree
assortativity_degree(NS)
assortativity_degree(JC)

# ***** Can you explain the difference in terms of the domains? 

######################################################################
# Pau 
