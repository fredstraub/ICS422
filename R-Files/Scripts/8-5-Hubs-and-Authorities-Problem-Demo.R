######################################################################
# Brief demo of hubs and authorities, for E&K homework problem 
# Dan Suthers, March 1, 2018
# Updated Mar  4 2021 to round the results. 

######################################################################
library(igraph)

# Figure 14.15 of E&K 

g <- graph_from_literal(A-+C, B-+C, B-+D, B-+E)
plot(g, vertex.size=30)

# Do manual authorities and hubs, 2 passes (k=2). Each pass refers
# to values from the *prior* pass: 
# 1. Authority Update Rule: set auth(v) to be sum of hub scores of
#    nodes that point to it in prior pass
# 2. Hub Update Rule: set hub(v) to be sum of the authority scores of 
#    nodes that it points to in prior pass

# Authority    A 1   B 1   C 1   D 1   E 1
# Hub          A 1   B 1   C 1   D 1   E 1 

# k=1
# Authority    A _   B _   C _   D _   E _
# Hub          A _   B _   C _   D _   E _

# k=2
# Authority    A _   B _   C _   D _   E _
# Hub          A _   B _   C _   D _   E _

# Now run the full iteration and compare 
round(authority_score(g)$vector, 4)
round(hub_score(g)$vector, 4)

######################################################################
# Pau 