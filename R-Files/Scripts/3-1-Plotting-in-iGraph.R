######################################################################
# 3-1 Basics of Plotting and Visualization in iGraph
# Dan Suthers, January 27, 2017
# July 23, 2018 DS: Refactoring into 3 scripts 
# Sep 10 2019 DS: minor changes for 2019 
# Jan 28 2021 DS: Removed nonessential material for 422/622 
# - new_window and Trees are now in the Outtakes script
# Jan 27 2022 DS: Updating for spring 2022 class: formatting only. 
#                 ~17 minutes. 
######################################################################
getwd()
library(igraph)

# Set to your base directory, or use Session/Set Working Directory 
# There should be a subdirectory 'Networks' with the graphs. 

setwd("/Users/fred/Github/ICS422/R-Files") 

# Loading sample graph. This graph is from the SAND demo, but has 
# been upgraded for current igraph with upgrade_graph(g).  

aidsblog <- read_graph("Networks/aidsblog.graphml", format="graphml")
summary(aidsblog)

######################################################################
# Basic Plotting and Layouts 
######################################################################
# 'plot' is a generic method that takes a guess at how best to
# display different kinds of objects. The generic plot is set up for
# making line or scatter plots of x,y pairs, but we use some of its
# parameters for visualizing graphs too: 

?plot # choose the one in base 
methods(plot)

1:10
plot(1:10, 10:1, main = "Generic Plot Example")
# Notice how the axes are named by the expressions giving their data 

# Can also plot a single sequence with the x axis defaulting to index
# position. This shows how to change to a combined line and point
# graph.

plot(sin(1:20), main = "Discrete Sine", type="b")
 
# This is the version used when plot is called with an igraph. You
# probably never need to call this directly but the documentation
# covers a way of marking groups of vertices. 

?plot.igraph

# This page has most of what you need to know about graph plotting.
# Look at the parameters. Add 'vertex.' for vertex and 'edge.' for
# edge parameters.

?igraph.plotting

# Let's try it with this graph: 

plot(aidsblog, main = "Aids Blog v1")

# That's hard to read! Let's change some things, using what we saw
# in the igraph.plotting page. In R, NA means Not Available. It 
# prevents use of the vertex.label parameter (NULL would not work). 

plot(aidsblog, main = "Aids Blog v2", 
     vertex.size = 4, 
     vertex.label = NA, 
     edge.arrow.size = 0.4)

# You may often want to give the same parameters in the call to plot.
# This is how you set default attributes for the session (see
# ?igraph.options for other things you can do)

igraph_options(vertex.size=4, vertex.label=NA, edge.arrow.size=0.4)

# Now we don't have to keep giving those arguments. 
# How to specify layout, here circular 

plot(aidsblog, main = "Aids Blog Network, Circular", 
     layout = layout_in_circle)

# Fruchterman-Reingold is the classic spring-embedding layout: 

plot(aidsblog, main = "Aids Blog Network, FR Layout", 
     layout = layout_with_fr)

# Other layout methods try to minimize the "energy" in the system.
# One approach is a multi-dimensional scaling algorithm, Kamada Kawai: 

plot(aidsblog, main = "Aids Blog Network, KK Layout", 
     layout = layout_with_kk)

# DRL is the same as OpenOrd in Gephi. It's a hierarchical strategy 
# useful for showing structure in larger graphs (but not small ones):

plot(aidsblog, main = "Aids Blog Network, DRL Layout", 
     layout = layout_with_drl)

# Flip back through your plots. 
# ***** Which best shows the structure of this network, and why? 

# When you do the homework, also try multiple layouts and pick the one
# that shows something interesting about the network. Scroll down in 
# this page (later you may want to try some of the other layouts): 

?layout_

######################################################################
# Graphical Parameters and Layouts 

# We use par to control various plotting parameters (skim through): 
# More frequently used: cex, lty, lwd, mar, mfcol, mfrow. 

?par 

# Suppose we want to plot the top 2 layouts from above on one screen. 
# This is how we divide the plot area up into 2 columns using par

par(mfrow=c(1, 2))

# Here are 2 favorites: 

plot(aidsblog, main = "Aids Blog, FR Layout", 
     layout = layout_with_fr)
plot(aidsblog, main = "Aids Blog, KK Layout", 
     layout = layout_with_kk)

# Better reset our defaults so as not to screw up later graphs 

igraph_options(vertex.size=NULL, vertex.label=NULL, edge.arrow.size=NULL) 
par(mfrow=c(1, 1))


######################################################################
# Decorating Graphs 
# There is a very nice example in SAND 3.4 Decorating Graph Layouts
# using Karate, but we won't have time in class: try it at home. 
# Notice especially the notation for coloring edges selectively.    
# Details may have changed: igraph is evolving every year! ]

######################################################################
# Writing image files 
######################################################################

# Bitmap images. Size defaults to be specified in pixels. 

?png

# You can set image size, font size, etc. but we'll take defaults

png("Aids_Blog_KK.png") # This opens the file for writing 

# Put your plot code here, for example: 

plot(aidsblog, main = "Aids Blog Network, KK Layout", 
     layout=layout_with_kk, 
     vertex.size = 3, 
     vertex.label = NA, 
     edge.arrow.size = 0.4)
dev.off() # This closes the file 

# PDF images. Size is specified in inches. 

?pdf
pdf("Aids_Blog_KK.pdf")

# Put your plot code here, for example: 

plot(aidsblog, main = "Aids Blog Network, KK Layout", 
     layout = layout_with_kk, 
     vertex.size = 3, 
     vertex.label = NA, 
     edge.arrow.size = 0.4)
dev.off() 

# ***** Open these files and zoom into them. Which should you use for
#       high quality publications or detailed graphs? 
     
#################################################################
# Pau 