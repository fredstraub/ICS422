######################################################################
# Dynamic Networks in statnet 
# 
# Annotated/Extended version of SAND demo for representing, accessing,
# visualizing, and characterizing dynamic networks. This file focuses
# on statnet, a package for network analysis.
#
# Dec  3 2019 DS Refactored from script by Dan Suthers, April 27, 2017
# Dec  5 2019 DS Inserted write-out to Gephi and improved the rest. 
# Apr 27 2021 DS spelling corrections while reviewing. 
# Apr 27 2021 DS Cleaning up for ICS 422/622 Spring 2021
# Apr 26 2022 DS Revisions for ICS 422/622 Spring 2022
# * Slightly cleaner code in places 
# * Reordered to finish statnet before going to Gephi, and do static 
#   first, to contrast ndtv with Gephi
# * Using full stergm.sim.1 due to wealth error in short.stergm.sim
# 
######################################################################
# networkDynamic 
# This is part of the 'statnet' collection of packages, which use
# a different network representation, 'network'. The statnet package
# is developed primarily by social scientists (in contrast to igraph,
# which includes physicists and others), and is hosted at U. Washington. 
# It also includes an ambitious package 'ergm' for Exponential Random
# Graph Models, a way of testing hypotheses about graph structures. 
# If you get an error invoking the library, use install.packages. 
######################################################################

setwd("~/Desktop/Network-Science-Demos") # Or yours 
source("Utility/new_window.R")

#  You may need to run these if you have not already: 
# install.packages('networkDynamic')
# install.packages('ndtv')
# install.packages('scatterplot3d')
library(network)
library(networkDynamic) 
library(ndtv)
library(scatterplot3d)

######################################################################
# Hospital Contact (HC) Data 
# Contacts among patients and health care workers in the geriatric 
# unit of a hospital in Lyon, 2010/12/6 13:00 to 2010/12/10 14:00, 
# Based on FTF RFID contact within 1-1.5m during 20 seconds intervals. 
# data(HC)

HC <- read.csv("Networks/hospital_contacts.csv")

# (Review of first look in 15-1) This is a data frame (not an igraph): 

class(HC)
names(HC)      # named fields: Time, IDs, and Status (job category)

# Nonstandard representation of edge list: the time stamps come first,
# then the IDs, then attributes that belong on vertices

head(HC)

# How many ...

nrow(HC)                          # contact events 
length(unique(c(HC$ID1, HC$ID2))) # actors 

######################################################################
# networkDynamic
######################################################################

?networkDynamic

# We will construct our network from a base.net of vertices and 
# edge.spells. 

# A "spell" is the period during which an edge in E(t) is connected. 
# Each spell has an onset and termination time. The dynamic network 
# is represented as a set of spells. 

# Our data has only single time points for each contact. Let's make
# this the termination and assume that the onset was 20 seconds 
# earlier. This way we will get continuous spells out of contacts
# that were recorded at contiguous 20 second intervals. 
# Following the documentation, we'll do it with this pattern: 
# edge_spells = [onset, terminus, tail.vertex.id, head.vertex.id]

# Make a spell for each contact in HC. Onset is 20 seconds before 
# recorded time. Convert seconds to hours (60*60 seconds/hour). 

HC_spells <- data.frame(
  onsets = (HC$Time-20)/(60*60),
  termini = HC$Time/(60*60),
  tail    = HC$ID1,
  head    = HC$ID2
  )
head(HC_spells) # Now in the required input format

# The network in the SAND demo came out directed by default, but this
# is undirected data. As far as I can tell from documentation, we need
# to give it a base network with undirected attribute, and we have to
# give the correct number of vertices.

?network.initialize
ug <- network.initialize(75, directed=FALSE)

# Make Hospital Contact Dynamic Network

HC_dn <- networkDynamic(base.net=ug, edge.spells=HC_spells)

# What's it look like? 

HC_dn

# (Don't use 'summary' on network objects: too verbose.)

# Very specific queries are possible. For example, is an edge 
# or vertex active during a specified time period? 

?is.active

# Query for edge with ID 1 in first and second hour: 

is.active(HC_dn, onset=0, terminus=1, e=1)
is.active(HC_dn, onset=1, terminus=2, e=1)

# We can get all the spells during which an edge was active. 
# For example, edge 1 is active only once: 

?get.edge.activity # notice other functions too
get.edge.activity(HC_dn,e=1)

# Here's an edge that is in seven spells, all in the first 25 hours. 

get.edge.activity(HC_dn,e=10)

# ***** What does it mean in terms of actors and time? 

######################################################################
# Conversions between Network Representations
######################################################################

# Make "periodically sampled collapsed networks", a.k.a. time slices,
# for specified sampling intervals (e.g., 12 hours)

?get.networks

# "Hospital Contacts dynamic networks 12 hour slices" ... 

HC_dn_12h <- get.networks(HC_dn, start=0, end=96, time.increment=12)

# A list of networks

length(HC_dn_12h)
HC_dn_12h[[1]]

# One can compute metrics on each time slice and plot changes:
# to be demonstrated shortly. 

#========================
# Convert to a data frame (Hospital Contact Dynamic Network Data Frame) 
# (This enables construction of a fancy visualization below.)

HC_df <- as.data.frame(HC_dn)
names(HC_df)

# Make console wide for this 

head(HC_df) 

# Rows in this data frame are spells, not contacts: notice repeated 
# edges but gaps in times 

# Getting summary statistics on duration, we see that most spells
# are short, but the longest is over an hour (the ADM meeting?)

summary(HC_df$duration)

# Those are hours. Might clarify to convert to seconds:

summary(HC_df$duration) * 60 * 60 # Yes you can do that! 

# So the minimum is a 20 second contact. 

######################################################################
# Visualization: Phase Plot 
######################################################################
# Phase Plot -- Requires networkDynamic tables 
# Plot onset and termination of edges as function of time, and 
# colored by status of endpoints. We learn more about plotting. 

# List of vertices for convenience 

vids <- sort(unique(c(HC$ID1, HC$ID2)))

# Status vector. Pair each ID with its staus, stack them up, and throw
# out duplicates.

status <- unique(rbind(data.frame(id=HC$ID1, status=HC$S1),
                       data.frame(id=HC$ID2, status=HC$S2)))
head(status)
v_status <-  as.character(status[order(status[,1]),2])
head(v_status)

# Reminder of the Hospital Contact Dynamic Network Data Frame object: 

head(HC_df)

# Establish colors for edge status, using integer representation 
# and treating X-Y same as Y-X as we did in 15-1

temp_es <- paste(v_status[HC_df$tail], "-",
                v_status[HC_df$head], sep="")
tail(temp_es, 10)
e_colors <- numeric(nrow(HC_df)) # new vector to put edge colors in 
e_colors[temp_es=="ADM-ADM"] <- 1
e_colors[temp_es=="MED-MED"] <- 2
e_colors[temp_es=="NUR-NUR"] <- 3
e_colors[temp_es=="PAT-PAT"] <- 4
e_colors[(temp_es=="ADM-MED") | (temp_es=="MED-ADM")] <- 5
e_colors[(temp_es=="ADM-NUR") | (temp_es=="NUR-ADM")] <- 6
e_colors[(temp_es=="ADM-PAT") | (temp_es=="PAT-ADM")] <- 7
e_colors[(temp_es=="MED-NUR") | (temp_es=="NUR-MED")] <- 8
e_colors[(temp_es=="MED-PAT") | (temp_es=="PAT-MED")] <- 9
e_colors[(temp_es=="NUR-PAT") | (temp_es=="PAT-NUR")] <- 10
tail(e_colors, 10)

# SAND used rainbow but it has been critiqued: see documentation
# We'll try the recommended hcl.colors. 

?rainbow 
(e_palette <- hcl.colors(10))                         # default 
# (e_palette <- hcl.colors(10, palette="YlGnBu"))     # alternative 
# (e_palette <- hcl.colors(10, palette="Blue-Red 3")) # color blind
# (e_palette <- rainbow(10))                          # pretty but ... 

# Compute x and y axis maximums 

max_y <- max(HC_df$edge.id)  # max edge id is y axis limit
max_x <- max(HC_df$terminus) # max terminus is x axis limit

# Create the plotting space but don't plot yet 
# ann = whether to annotate title and axes; type = don't plot 

new_window("Hospital Contacts Edge Phase Plot", 12, 12) # make it big
plot(c(0,max_x), c(0,max_y), ann=FALSE, type='n') 

# Put in the line segment for onset to terminus. 
# Adjust lwd upwards for greater visibility at loss of resolution

?segments 
segments(HC_df$onset,    HC_df$edge.id,
         HC_df$terminus, HC_df$edge.id,
         col=e_palette[e_colors], lwd=4) 

# Annotations

title(main="Dynamic Edges in Hospital Contact Network", 
      xlab="Time (hours)",
      ylab="Interacting Pair (Ordered by First Interaction)", 
      cex=1.5)

# Day boundaries 

?abline
abline(v=c(11,35,59,83), lty="dashed", lw=2, col="lightgray")

# Legend 

status.pairs <- c("ADM-ADM","MED-MED","NUR-NUR",
                  "PAT-PAT", "ADM-MED","ADM-NUR","ADM-PAT",
                  "MED-NUR", "MED-PAT","NUR-PAT")
legend("topleft", status.pairs, text.col=e_palette[(1:10)], 
       bg="white", bty="n", cex=1.3)

# Let's spend a little time trying to interpret that plot:
# Repeated Nurse contact 
# Initial color shows many NUR-PAT and MED-NUR contacts 
# Repeated meeting of doctors who met on first day. 
# Admin meeting on Friday (where is the hour long contact?)

# Can we sort vertically by type? "Exercise for the reader!" 

######################################################################
# Temporal Visualizations       ***** NEW VIDEO *****
# A variety of methods in statnet 
######################################################################
# Example data: marriages between families in Florence: data(florentine)

# Original demo used this temporal network but there is a bug in
# wealth data:
# data(short.stergm.sim)
# short.stergm.sim # A networkDynamic Network

# So we'll use the longer one: 

data("stergm.sim.1")
stergm.sim.1

# Will label and color by priorates: "the ruling magistrates of the
# medieval Italian republic of Florence."

######################################################################
# Static visual summary of the network changes 

new_window("Florentine Marriages: Static", 12, 18)

?filmstrip
filmstrip(stergm.sim.1,displaylabels=FALSE)

# Plot a “timePrism” projection (also in large window). May need to
# resize window to get it to redraw in limits. 

?timePrism
compute.animation(stergm.sim.1)
attimes=c(1, 25, 50, 75, 100)
timePrism(stergm.sim.1, 
          at=attimes,
          displaylabels=TRUE, 
          planes = TRUE,
          label.cex=1.2)

# Plot a timeline

?timeline
timeline(stergm.sim.1, cex=1.2)

# Plot a proximity timeline

?proximity.timeline
proximity.timeline(stergm.sim.1,
                   default.dist=6,
                   mode='sammon',
                   labels.at=17,
                   vertex.cex=4)

#######################################################################
# Animation in statnet using ndtv, from Sunbelt workshop

?ndtv

# Render as an animation. Will take a minute (pause video).

new_window("Florentine Marriages: Dynamic", 12, 12)
?render.animation
render.animation(stergm.sim.1,
                 # Found by experimentation sqrt / 2 gets a good range
                 vertex.cex=function(slice){sqrt(slice%v%'wealth')/2}, 
                 # Color 0 is white and 1 is black; I want others so +2
                 vertex.col=function(slice){slice%v%'priorates'+2}
                 )

# Play it back in the R plot window as many times as we want.
# (You can stop it once you get the point.) 

ani.replay() 

# Similar animation as interactive HTML5 animation in a web browser

?render.d3movie
render.d3movie(stergm.sim.1, displaylabels=TRUE, 
               vertex.cex=function(slice){sqrt(slice%v%'wealth')/2}, 
               vertex.col=function(slice){slice%v%'priorates'+2})

# ***** What do you see happening in the marriage coalitions? 

# ***** Discuss value of dynamic layout vs. fixing node positions. 

######################################################################
# Activity: Choose your favorite temporal animation from the above and
# re-do it with HC_dn. Can you color vertices by status? 

# This takes a while and is not my favorite. 
# render.animation(HC_dn, 
#                  vertex.col=as.numeric(as.factor(v_status)))
# My choice: 

render.d3movie(HC_dn, displaylabels=TRUE, 
               label=v_status, 
               vertex.col=as.numeric(as.factor(v_status)))

#######################################################################
# Animation in Gephi 
#######################################################################
# Gephi is a stronger platform for interactive temporal visualization.

# Write the hospital data  out as tables to be read into Gephi as a
# network, using Gephi-interpreted column names

nodelist <- data.frame(ID = vids, 
                       Label = v_status, 
                       Job_Role = v_status # to color by this partition
                       )
head(nodelist)

edgelist <- data.frame(Source = HC_df$tail, 
                      Target = HC_df$head, 
                      start  = HC_df$onset, 
                      end    = HC_df$terminus)
head(edgelist)

write.csv(edgelist, "HC_edgelist.csv", row.names=FALSE)
write.csv(nodelist, "HC_nodelist.csv", row.names=FALSE)

# Now read into Gephi, reading the Node List first. 
# See these for how to turn start and end into an interval using
# Merge Columns, but I'll demonstrate ... 
# https://seinecle.github.io/gephi-tutorials/generated-html/converting-a-network-with-dates-into-dynamic.html
# https://github.com/gephi/gephi/wiki/Import-Dynamic-Data

# If you slow down the timeline animation and run Fruchterman-Rheingold
# continuously it is pretty interesting. Is it useful? 

######################################################################
# For tools to test hypotheses about temporal patterns in networks,
# see CRAN package tergm (Temporal Exponential Random Graph Models)
# at https://CRAN.R-project.org/package=tergm. This requires first
# learning to use ERGMs (https://CRAN.R-project.org/package=ergm). 

######################################################################
# Pau 
