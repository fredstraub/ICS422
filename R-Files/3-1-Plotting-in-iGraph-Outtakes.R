# Material removed from 3-1-Plotting-in-iGraph.R 
# Assumes that some of the above has been executed. 
######################################################################
# Making windows and multiple display spaces in a window 
######################################################################
# Suppose we want to create a new window of a size suitable for 3 plots. 

# If you are in RStudio and don't make a new window the plots will be 
# "squashed". Such plots are hard to read, indicate lack of control 
# over your tools, (lack of professionalism), and should not be published. 

# Instead, we will make separate windows. You may need to move your 
# RStudio window to the right to see these when they pop up: 

# New window in MacOS: 
quartz("New MacOS Window", 9, 3) 

# On Linux or if you are using X11: 
x11(title=title, width=9, height=3)

# On other platforms (please inform me if this does not work)
dev.new(title="New Generic Window", width=9, height=3)

# 2021: The width and height are no longer working for x11 or dev.new! 

# To avoid the complexity of providing different code for different
# platforms, I've written a utility function called new_window that 
# detects and works with your platform. It is new_window.R in the 
# Utility sub-folder. We will use this in almost every class. 

dir("Utility") # Make sure you have new_window.R 
source("Utility/new_window.R")
new_window("Trees", 12, 4)

# ***** We just made more than one new window. 
#       How do you tell which window is currently active? 

# If you want more control over windows, see dev.cur(), dev.list(), 
# dev.set(), dev.next(), dev.off() etc.: 
?dev.cur

######################################################################
# Graphical Parameters and Layouts 

# We use par to control various plotting parameters (skim through): 
# More frequently used: cex, lty, lwd, mar, mfcol, mfrow. 
?par 

# This is how we divide the plot area up into 2 columns using par
par(mfrow=c(1, 2))

# See also ?screen for another approach (incompatible with par)

######################################################################
# Example: plotting 3 tree layouts 

# For this example we'll plot a tree-like graph: 
g.tree <- graph_from_literal(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,3-+8,3-+9,4-+10)

# Again normally we would give these options to the plot method, but 
# here we set them globally to avoid repeating them across 3 plots: 
igraph.options(vertex.size=30, edge.arrow.size=0.5)

# Here are three layouts for trees (see ?layout_as_tree for last two)
plot(g.tree, layout=layout_in_circle, main="Circle layout")
plot(g.tree, layout=layout_as_tree, main="Tree layout")
plot(g.tree, layout=layout_as_tree(g.tree, circular=TRUE), main="Tree circular layout")

# Again we can reset our defaults.
igraph.options(vertex.size=NULL, edge.arrow.size=NULL)
par(mfrow=c(1, 1))

######################################################################
# I'm going to go back to plotting in the RStudio window. Do this
# only if you were using new_window. 
dev.cur()        # Current graphics device 
dev.list()       # to see what devices are active 
dev.set(which=2) # to set the current device to RStudioGD, if needed

######################################################################
# Pau 