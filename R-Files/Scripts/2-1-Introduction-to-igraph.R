#################################################################
# 2-1 Introduction to igraph: 
# A modification of SAND Chapter 2 demo
# Dan Suthers, created January 17, 2017
# July 23, 2018: Minor edits while reviewing.
# Tue Sep  3 2019 DS: Revision for fall 2019 
# Mon Jan 18 2021 DS Revision for Spring 2021
# Tue Jan 18 2022 DS Revision for Spring 2022 

#################################################################
# If your screen looks different from mine, a reminder: 
# In RStudio: View / Panes / Pane Layout => flip vertical orders
#################################################################
library(igraph)

# We can make graphs with formulas, usually just for quick examples. 
?graph_from_literal

# We'll make a few for demonstration purposes 
# Undirected 
GU <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
# Directed (notice bidirectional links)
GD <- graph_from_literal(1-+2, 1+-+3, 2-+3, 2+-+4, 3-+5, 4+-+5, 4-+6, 4-+7, 5-+6, 6-+7)

# Summary information. 
summary(GU)
summary(GD)
# ***** Where does it say it's (un)directed? How many nodes and edges? 




# The fields: 
#   D|U: directed or undirected 
#   N|-: N if vertex name attribute is set 
#   W|-: W if it is weighted 
#   B|-: B if a bipartition has been specified (will be explained)
# Next two numbers are number of vertices and number of edges 
# This is followed by list of attributes: 
#   "v" for vertex attribute, "e" for edge attribute; 
#   data type: "c" character, "n" numeric, "l" logical, "x" other

# Evaluating the graph variable prints the summary plus edges: usually TMI 
GU 
GD 

##########
# Plotting (we'll get into visualization in the next class)
plot(GU, main = "Undirected Graph Example")
plot(GD, main = "Directed Graph Example")

?layout # Can control layout: see igraph version and scroll down
plot(GU,  main = "Undirected Graph Example Circular", layout=layout_in_circle)

##############################
# Accessing vertices and edges 
V(GU)
E(GU)
E(GD)

##############################
# Different Representations
?as_adjacency_matrix
as_adjacency_matrix(GU) # Why is is symmetric? 
as_adjacency_matrix(GU, type="upper")
as_adjacency_matrix(GU, type="lower")
as_adjacency_matrix(GD) # Notice the asymmetry

?as_edgelist
as_edgelist(GU) # Notice each edge listed only once, e.g., no 3,1
as_edgelist(GD) # whereas here we have 1,3 and 3,1 (go back to plot)

##############################
# Adding attributes to graphs 

G1 <- graph_from_literal(Sam-+Mary, Sam-+Tom, Mary++Tom)
summary(G1)

# Vertex attributes 
V(G1)$name
V(G1)$gender # not there 
V(G1)$gender <- c("M","F","M")
V(G1)$gender
V(G1)$color <- "red"  # R will take the single value and replicate as needed
V(G1)$color
plot(G1)
summary(G1)  # Notice the addition of the vertex character attributes (v/c)

# To give nodes different colors by attributes: 
?ifelse
ifelse(V(G1)$gender=="M", "lightblue", "pink") 
# That just demonstrated the results but we need to assign it: 
V(G1)$color <- ifelse(V(G1)$gender=="M", "lightblue", "pink")
V(G1)$color

# I want to keep the layout fixed as I change the graph, so save layout: 
G1.layout <- layout_in_circle(G1)
# If you are curious how a layout is represented: 
G1.layout
# Plot using this layout
plot(G1, main = "Three Friends", layout=G1.layout)

# Edge attributes work the same. Most commonly we give edges weights. 
is_weighted(G1)
E(G1)$weight <- c(4, 1, 3, 2)
is_weighted(G1)
E(G1)$weight
summary(G1)  # Notice the addition of the edge attribute weight (e/n)

# Can use edge weights and increase vertex size (more next week) 
plot(G1, main = "Three Friends", 
     layout=G1.layout, 
     edge.width = E(G1)$weight,
     vertex.size=30)

# Graphs can have attributes too. 
G1$name <- "Three Friends"
summary(G1)

####################
# Simple Graphs 

# We can test whether a graph is simple 
is_simple(G1)

# To show a "not simple" graph add a self loop and multi-edges with weights of 1
# using edge
?edge

# Notice the use of "+" to combine the graph with edges. 
# (Had to END the line with it for proper parsing of multiple lines.)
G2 <- G1 + edge("Mary","Mary", weight = 1) + 
           edge("Tom","Sam", weight = 1) + 
           edge("Sam","Tom", weight = 1)
G2
E(G2)$weight 
plot(G2, main = G2$name, 
     layout=G1.layout, 
     edge.width = E(G2)$weight,
     vertex.size=30)

is_simple(G2)

# simplify can be used to clean up graphs 
?simplify 
# edge.attr.comb is critical for some applications: how do you combine edge
# attributes?
G2.simp <- simplify(G2)
G2.simp$name <- paste(G1$name, "Simplified")
summary(G2.simp)  # notice link count
is_simple(G2.simp)
E(G2.simp)$weight # edge weights have been summed 
plot(G2.simp, main=G2.simp$name, 
     layout=G1.layout, 
     edge.width = E(G2)$weight,
     vertex.size=30)

# There is much more. You could continue the demo in Ch. 2 of SAND
# starting with 2.20 (where they load g.lazega). However, we'll learn
# methods as we need them. 

# On to file I/O in 2-2-Reading-Graphs-Into-igraph and the activity ... 

###########################################################################
# Pau