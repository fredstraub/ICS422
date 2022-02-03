######################################################################
# 2-2 Reading and Writing different file formats in igraph
# Dan Suthers, created August 29, 2016
# July 23, 2018: Minor edit to note Networks subdirectory 
# Tue Sep  3 2019 DS Revision for fall 2019 
# Thu Jan 21 2021 DS Revised for ICS 422/622: 
# * More explicit setup of setwd 
# * Les-Miserables replaces karate (has more attributes)
# Thu Jan 20 2022 DS Revision for Spring 2022 
# - Removed star, ring etc. and instead adding attributes to contacts. 
######################################################################

library('igraph')

######################################################################
# Setting up Directories 

# For this semester, you should set up a working directory you use for each
# class. The directory should have two subdirectories:
# * Networks - where the graphs are stored 
# * Utility - for utility scripts 

# An easy way to do this is to download today's Google "Current Class"
# folder to a location you prefer. Please set that up now ...

# ---------------------------------
# Setting Default Working Directory

# Let's see what your default working directory is: 
getwd()

# If this is different from your intended working directory (e.g., your
# copy of Current Class), set the working directory to it using R Studio
# menu Session / Set Working Directory. This will print an expression such
# as the following to console. Save it to use in future scripts.

setwd("~/Desktop/Network-Science-Demos") # Set to YOUR version 
getwd()

# FROM NOW ON START ALL YOUR .R SCRIPTS FOR CLASS WITH: 
# library('igraph')
# setwd("~/Desktop/Network-Science-Demos") # your directory

# Here are the contents of your new working directory. It should have a
# Networks subdirectory.
dir()

# For today's class, the Les Mis networks, the NS-Class-Contacts we made in
# Gephi and TappedInChatSample.vna should be in the Networks subdirectory:

dir("Networks") 

######################################################################
# Reading Different Formats 
# We will read in the various versions and examine them to determine 
# what kinds of information they support. 

# Read function for common formats. Notice the format options 
?read_graph # if this fails you forgot to do library('igraph')

# ----------
# Edgelist - the simplest representation with no labels or attributes 
# *** Go to Terminal, cd to Networks if needed and do: 
# head Les-Miserables.edgelist 
LM.edgelist <- read_graph("Networks/Les-Miserables.edgelist", format="edgelist")
summary(LM.edgelist)
# Notice there are NO attributes of any kind (graph, vertex or edge)
LM.layout <- layout_with_kk(LM.edgelist) # will reuse this layout 
plot(LM.edgelist, main = "Les-Miserables.edgelist", layout=LM.layout)
# It defaults to directed, which is incorrect for this data. Fix: 
LM.edgelist <- read_graph("Networks/Les-Miserables.edgelist", format="edgelist", 
                          directed=FALSE)
summary(LM.edgelist)
plot(LM.edgelist, main = "Les-Miserables.edgelist", layout=LM.layout)

# ----------
# Pajek's .net - adds labels and weights, but no other attributes 
# head Les-Miserables.net
LM.net <- read_graph("Networks/Les-Miserables.net", format="pajek")
summary(LM.net)
# Vertices have id and names and edges have weights. Notice "N" 
# If we want to know the unique values for these new attributes 
sort(unique(V(LM.net)$name)) 
sort(unique(E(LM.net)$weight)) 
# These attributes affect the plot: 
plot(LM.net, main="Les-Miserables.net", 
     layout=LM.layout, 
     edge.width = E(LM.net)$weight)

# ----------
# GML - A human readable format that includes attributes 
# head Les-Miserables.gml
LM.gml <- read_graph("Networks/Les-Miserables.gml", format="gml")
summary(LM.gml) 

# Similar to Pajek .net, but uses "label" and "value" instead of "name", and
# "weight". Summary does not include the "N", but plot knows to use "label"! 
# We have to tell it to use "value" as the edge weight.

plot(LM.gml, main="Les-Miserables.gml", 
     layout=LM.layout, 
     edge.width = E(LM.gml)$value)

# ----------
# Graphml - Standard XML format, preserves all attributes
# head Les-Miserables.graphml
LM.graphml <- read_graph("Networks/Les-Miserables.graphml", format="graphml")
summary(LM.graphml)
# Like GML, but the graph now has a name attribute and others are added. 
LM.graphml$name
plot(LM.graphml, main="Les-Miserables.graphml", 
     layout=LM.layout,
     edge.width = E(LM.net)$weight)

# The last three appear identical in plot, but summary shows differences. 

# Note: when Gephi parses a .graphml with graph attribute $name, it will
# generate a "severe error" in the Report. You can ignore this.)

###########################################################################
# From Gephi to igraph and back 
###########################################################################
# We will often need to move between Gephi and igraph when completing tasks
# (typically analysis in igraph and visualization in Gephi, but we also use
# Gephi for data conversion and Data Laboratory).

# How to read in what we wrote out from Gephi 
contacts <- read_graph("Networks/NS-Class-Contacts.graphml", format="graphml")
summary(contacts)
plot(contacts, main="ICS 422/622 Contacts in the First Week") 

# Could try adding departments
plot(contacts, main="ICS 422/622 Contacts in the First Week", 
     vertex.color = V(contacts)$department)

# Turns out that igraph plotter expects integer values for vertex color. To
# convert character names of departments we need to order them. An easy way to
# do this is to convert the values to a "factor", which orders its possible
# values. Then igraph knows how to coerce factor values to integer:
as.factor(V(contacts)$department)
as.integer(as.factor(V(contacts)$department)) # No need to do this ...
plot(contacts, main="ICS 422/622 Contacts in the First Week", 
     vertex.color = as.factor(V(contacts)$department))

# Next week we learn how to make this prettier, though it is much easier in
# Gephi.

# ------------------------
# Adding an attribute to the graph 

# An easy one that we could also do in Gephi: 
V(contacts)$degree <- degree(contacts)
summary(contacts)
V(contacts)$degree

# Let's compute something that would be hard in Gephi. 
# How many departments has a student had contact with? 
neighbors(contacts, 1)
neighbors(contacts, 1)$department
neighbors(contacts, 2)$department

# Generalizing: 
sapply(V(contacts), 
       function(v){length(unique(neighbors(contacts, v)$department))})

# Add this as a vertex attribute: 
V(contacts)$num_dep_contacts <- 
        sapply(V(contacts), 
               function(v){length(unique(neighbors(contacts, v)$department))})
V(contacts)$num_dep_contacts


# ------------------------
# How to write out a graph
?write_graph

write_graph(contacts, "Networks/NS-Class-Contacts-2.graphml", format="graphml")

# ***** Load into Gephi and visualize the number of departments with node size. 

###########################################################################
# Activity: 
# VNA is a useful format: it is like Pajek but lets you add attributes. 
# See TappedInChatSample.VNA
# How would you read a VNA file into igraph? 
# ***** See Google Docs for the problem statement. 
# ***** There is an optional template 2-3-Activity.R 
###########################################################################
# Pau