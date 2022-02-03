###########################################################################
# Topic 2 activity: Reading VNA into igraph -- STUDENT TEMPLATE 
# Thu Jan 21 2021 DS updated for 2021 class
# Tue Jan 20 2022 DS updated for 2022 class 
#
# You are given a VNA file TappedInChatSample.vna in Google Drive. The
# vertices are chat messages. Each vertex has attribute information for the
# time of the chat, the actor chatting, and the contribution (contents of
# the chat.) It is a directed graph where directed edges from a vertex
# point to vertices for prior chats that may have been related to the
# present chat. You want to read it into igraph. 
#
###########################################################################
library(igraph)
# set working directory 
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
dir("Networks/") # Check for TappedInChatSample.vna

###########################################################################
# 1. Reading into igraph

# Figure out how to read this network into igraph. Be sure that all
# attributes of the .VNA have been preserved and are available in igraph,
# and that no unnecessary attributes are added. (I anticipate that there
# will be problems. The point of this exercise is for you to notice and
# solve the problems.)

# ----------
# a. Put first attempts to read in the graph here. Modify as needed to get it
#    read in.


# ----------
# b. Show a summary of the graph in igraph. If something is wrong, do what you
#    need to do to fix it. Depending on what you did in (a) you may have to 
#    return to this step to fix a problem after you do (c).


# ----------
# c. Print out the first 6 values of the "contribution" attribute in sequence to
#    the igraph console to show you have them. If this fails, document what you
#    have to do to fix it.


# ----------
# d. Plot the graph without specifying layout, so it defaults to choosing a
#    layout for you. Is the layout what you expected? Is it a good one? 
#    Compare your visualization to others' and discuss possible fixes. 

# ----------
# e. As a group in Google Doc, summarize the problems encountered and the
#    procedures you used to solve them.


###########################################################################
# 2. Querying the graph  
# ----------
# a. Test whether the graph is a simple graph (not a multigraph).


# ----------
# b. Test whether the graph is a Directed Acyclic Graph (DAG), as we
# expected given the data definition. You’ll need to find the method.


# ----------
# c. Test whether the graph is weakly connected and strongly connected.
# (Igraph has a documentation error: you’ll need to search for
# ‘component_distribution’.)


# Then show the weakly connected components of the graph.


# Which of these (whether simple or DAG)  was already determined by the
# result in (b)?


# ----------
# d. Challenge: Compute the in-degree of each vertex and write an
# expression (not a loop!) that returns the contribution of the vertex or
# vertices with the maximum in-degree.


###########################################################################
# 3. Annotating the graph  
# ----------
# a. Add an additional attribute to each vertex called ‘charcount’ that has an
# integer representing the length of the contribution for the given vertex.
# Hint: nchar.


# ----------
# b. Challenge: Which contribution has the maximum length? Print it out. 


###########################################################################
# 4. Writing the graph  
# ----------
# a. Write a faithful representation of the graph, including all original
# attributes and those you added. Show the console for the write
# expression.


# ----------
# b. Read the file you just wrote into Gephi and demonstrate that the
# charcount attribute is present (either via visualization or in Data
# Laboratory).

# Do this in Data Laboratory. While you are there, take a look at the "Id"
# fields and compare them to the original VNA you read in and also to the
# vertex id and label in igraph. Some work may be needed to make both
# systems use the same IDs.

###########################################################################
# Pau
