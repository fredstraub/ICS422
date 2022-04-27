######################################################################
# Dynamic Networks in igraph 
#
# Annotated/Extended version of SAND demo for representing, accessing,
# visualizing, and characterizing dynamic networks. This script focuses
# on igraph; the next one on statnet. Includes many small lessons in R
# as well. 
# 
# SAND IS PRE-TIDYVERSE: this would benefit from conversion
# 
# Dec  3 2019 DS Refactored from script by Dan Suthers, April 27, 2017
# Apr 27 2021 DS Cleaning up for ICS 422/622 Spring 2021
# Apr 26 2022 DS Revisions for ICS 422/622 Spring 2022
# 
######################################################################
# Introduction
# 
# Dynamic network is a time indexed graph G(t) = (V(t), E(t)): edges
# and vertices may appear and disappear over time. This is not the
# same as the analysis of dynamic processes that take place across
# static networks: cascades, epidemics, etc.
# 
# Various representations of dynamic networks are possible: 
# * Full resolution t 
# * Finite time resolution (by time slices or spans) 
# * Series of static snapshots ("panel data" or "longitudinal network")
# * Static representation: single snapshot from which we infer dynamics. 
# 
# This script shows some methods of temporal analysis in igraph,
# elaborated from the SAND book. It is primarily based on time slices.
# The next script will show methods of temporal analysis in statnet,
# which provides full resolution analysis.
# 
######################################################################

setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Or yours 
source("Utility/new_window.R")

######################################################################
# Representations and Manipulation: Hospital Contact (HC) Example 
######################################################################
# Contacts among patients and health care workers in the geriatric 
# unit of a hospital in Lyon, 2010/12/6 13:00 to 2010/12/10 14:00, 
# Based on FTF RFID contact within 1-1.5m during 20 seconds intervals. 
# data(HC)

HC <- read.csv("Networks/hospital_contacts.csv")

# This is a data frame (not an igraph): 

class(HC)
names(HC)      # named fields: Time, IDs, and Status (job category)
head(HC)       # first 6 rows. Note 20 second intervals. 
nrow(HC)       # how many contact events 

######################################################################
# Summary Data (non-temporal analysis)
######################################################################
# Start by getting a feel for the cumulative graph ("marginal sums")

# -------------------- Exercise --------------------
# P1: Count the number of unique IDs (to be vertices)
# _____________________________________

#===========================================================
# To count the numbers of each category (ADM, MED, NUR, PAT),
# need to count only once for each ID. SAND approach: 

# Make concatenated lists of IDs and Status for EACH contact event:

ID_stack <- c(HC$ID1, HC$ID2)
status_stack <- c(as.character(HC$S1), as.character(HC$S2))
length(ID_stack) # twice the number of events, not number of IDs
head(ID_stack)
head(status_stack)

# ID_status is table mapping IDs to Status. Notice structure 

ID_status <- table(ID_stack, status_stack)
head(ID_status)
nrow(ID_status) # one for each ID

# This is how we will retrieve the status of each vertex. It's
# old-school (from the SAND demo): surely tidyverse has a better way!

ID_status[1,]                  # get a row ..
which(ID_status[1,]!=0)        # get non-null column of that row
names(which(ID_status[1,]!=0)) # pull out the column name 
names(which(ID_status[2,]!=0)) # etc. 

# Let's put that in a loop to make a simple list of status ...

v_status <- character(nrow(ID_status))
for(i in (1:length(v_status))){
  v_status[i] <- names(which(ID_status[i,]!=0))
}
head(v_status)

# ... and count frequencies 

table(v_status)

#========================================================
# A 2-way table counting edges according to Status-Status 

# Make a contingency table of source and target status.
# Will examine each step so we can compare to understand. 

status_table_1 <- table(HC$S1, HC$S2)
status_table_1

# Notice it is not symmetric. For example, 1197 ADM-NUR and 
# 1399 NUR-ADM. But we want to count these as the same kinds 
# of interactions, as this is an undirected graph (S1 and S2
# are arbitrary). So, add the matrix to its transpose to get 
# totals for each X-Y or Y-X pairing, making it symmetric. 

?t # Transpose 

status_table_2 <- status_table_1 + t(status_table_1)
status_table_2

# Problem: Items on diagonal were counted twice. Cut in half. 

?diag
status_table_3 <- status_table_2
diag(status_table_3)
diag(status_table_3) <- round(diag(status_table_3)/2)
diag(status_table_3)

# Final answer. 

status_table_3

# ***** Which types interact most frequently? The least? 
#       Is it normalized by number of persons? 

######################################################################
# Temporal Analysis by Time Slices using Data Frame Representation
######################################################################
# First let's annotate the edges with the types of agents interacting 

# Temporary list of edge status labels 

temp_es <- paste(HC$S1, "-", HC$S2, sep="")
head(temp_es) # data starts with medical meetings 
tail(temp_es, 7) # various interactions at end (note NUR-PAT and PAT-NUR)

# Copy these edge status labels to a new list, removing asymmetry 

e_status <- character(dim(HC)[[1]]) 
head(e_status) # it's an empty string for each event

# These are already symmetric 

e_status[temp_es=="ADM-ADM"] <- "ADM-ADM"
e_status[temp_es=="MED-MED"] <- "MED-MED"
e_status[temp_es=="NUR-NUR"] <- "NUR-NUR"
e_status[temp_es=="PAT-PAT"] <- "PAT-PAT"

# Give these a canonical alphabetical order representation

e_status[(temp_es=="ADM-MED") | (temp_es=="MED-ADM")] <- "ADM-MED"
e_status[(temp_es=="ADM-NUR") | (temp_es=="NUR-ADM")] <- "ADM-NUR"
e_status[(temp_es=="ADM-PAT") | (temp_es=="PAT-ADM")] <- "ADM-PAT"
e_status[(temp_es=="MED-NUR") | (temp_es=="NUR-MED")] <- "MED-NUR"
e_status[(temp_es=="MED-PAT") | (temp_es=="PAT-MED")] <- "MED-PAT"
e_status[(temp_es=="NUR-PAT") | (temp_es=="PAT-NUR")] <- "NUR-PAT"

# Now the labels are "canonical": check what happened to PAT-NUR:

tail(temp_es, 7)
tail(e_status, 7)

# Make a new data frame with time converted from seconds to hours
# and edges labeled with the canonical status representation. 

my.HC <- data.frame(Time = HC$Time/(60*60),
                    ID1 = HC$ID1,
                    ID2 = HC$ID2,
                    Status = e_status)
head(my.HC)

# For nicer plots 

library(lattice)
?histogram # not the same as base system hist. Read about formula x

# Plot values with Time on the x axis, with Status-Status links
# dividing the plots up. 

histogram(~ Time | Status, data=my.HC, xlab="Hours", layout=c(5,2))

# ***** What can we conclude from this visualization? 
#       (The time axis covers slightly over 4 days) 

# -------------------- Exercise (easy) -------------------- 
# P2: Can you get the graph to plot with finer granularity histograms 
# so we can see the daily patterns in more detail?  Read the breaks
# and nint documentation in ?histogram. How many intervals do we want? 
# Note: there are 97 hours in the data. 
histogram(~ Time | Status, data=my.HC, xlab="Hours", layout=c(5,2),nint=97)
# ____________________________________________________________

# It might also be useful to plot each day separately. 

######################################################################
# Representing as Temporal Network in igraph ***** NEW VIDEO *****
######################################################################
# Now we use igraph 

library(igraph)

# Multigraph approach: 
# One edge for each contact; edges annotated with start of 20 second 
# time period. One graph for entire week (HC_week). 

# Get the vertex IDs

vids <- sort(unique(c(HC$ID1, HC$ID2)))
vids

# How to make an edge list? Recall that HC has this structure 

head(HC)

# This makes an edge list with temporal annotations

?Extract # to understand this
head(HC[, c("ID1", "ID2", "Time")], 10)

# You could have also constructed that table outside of igraph as .csv
# or obtained event data as .csv and read it in with read.csv.
# dplyr::select is a clean tidyverse way of doing it 

?graph_from_data_frame 
HC_week <- graph_from_data_frame(HC[, c("ID1", "ID2", "Time")], 
                                vertices=data.frame(vids), 
                                directed=FALSE)

HC_week # notice # edges and repeated edges on first row
is_simple(HC_week) # of course not

# Convert edge time to hours scale. (I would prefer to leave
# it and convert only for output, but we'll follow their demo.) 

head(E(HC_week)$Time)
E(HC_week)$Time <- E(HC_week)$Time  / (60 * 60)
head(E(HC_week)$Time)

# Add Status as a vertex attribute. 
# (This could have been done by constructing data.frame(vids) to have
# a Status column, or by reading in .csv table with that info, but
# we'll follow the SAND demo here.)
# Recall that ?rbind binds rows

status <- unique(rbind(data.frame(id=HC$ID1, status=HC$S1),
                       data.frame(id=HC$ID2, status=HC$S2)))
head(status)
length(status[,1]) # check for correct length

# Assign status as a vertex attribute.
# First let's unpack what this expression is doing: 

head(status[order(status[,1]),1:2], 10) # Status pairs ordered by ID
head(status[order(status[,1]),2], 10)   # We just want the status part 

V(HC_week)$Status <- status[order(status[,1]),2]
V(HC_week)$Status  

# Now we have a multigraph with node attributes and time annotated
# edges.

summary(HC_week)
(vcount(HC_week)*(vcount(HC_week)-1)) / 2 # max |E| in simple graph

# These actors ... 

head(ends(HC_week, E(HC_week)))

# ... connect at these times ... 

head(E(HC_week)$Time)

######################################################################
# Static network for reference 
######################################################################
# If we collapse into a weighted simple graph we can get overview
# metrics

# Count the number of 20-second contacts by weighting each edge 1 
# (No need to try to merge Time, which is not a duration anyway.)

E(HC_week)$weight <- 1
HC_wsimp <- simplify(HC_week) # defaults work, but see ?simplify
summary(HC_wsimp) # reduced number of edges, counted in weight
is_simple(HC_wsimp)
head(E(HC_wsimp)$weight)
summary(E(HC_wsimp)$weight)

# Will get back to this below. 

# -------------------- Exercise (after class) -------------------- 
# P3: What is the Status (job type) of the nodes with top betweenness
# centrality? (These are important for spreading both information 
# and disease.)

# _____________________________________________________________

# But that loses temporal information. 

######################################################################
# Time Slice Networks 
######################################################################

# Make eight slices of 12 hours each. This will be a list
# of graphs, each retaining all 75 vertices (no deletion)

?subgraph 

# Note: subgraph.edges will be renamed subgraph, but the latter
# currently does not allow control of delete.vertices 

HC_12h <- lapply(1:8,
                 function(i) {
                   g <- subgraph.edges(HC_week,
                                       E(HC_week)[Time > 12 * (i - 1) &
                                                  Time <= 12 * i],
                                       delete.vertices = FALSE)
                   simplify(g) # weights will be combined
                 })

# All 75 vertices are in each graph 

sapply(HC_12h, vcount)

# Number of edges per graph

sapply(HC_12h, ecount)

# -------------------- Exercise (easy)-------------------- 
# P4: Make HC_24h, a graph of 24 hour slices

HC_24h <-  lapply(1:4,
                  function(i) {
                    g <- subgraph.edges(HC_week,
                                        E(HC_week)[Time > 24 * (i - 1) &
                                                     Time <= 24 * i],
                                        delete.vertices = FALSE)
                    simplify(g) # weights will be combined
                  })
sapply(HC_24h, vcount)
sapply(HC_24h, ecount)

# -------------------- Exercise (skip)--------------------
# P5: Does the Status of top betweenness centralities change 
# over the four days? Other centralities? (apply P3 to P4)

# _____________________________________________________________


######################################################################
# Visualizations
######################################################################

# For convenience, status and color vectors 

status <- c("ADM", "MED", "NUR", "PAT")
colors <- c("red", "blue", "green", "black")

# Make a vector of colors for each vertex (notice how colors vertex is
# sampled repeatedly!)

as.numeric(as.factor(V(HC_wsimp)$Status)) # indices into color
v_cols <- colors[as.numeric(as.factor(V(HC_wsimp)$Status))]
head(v_cols, 18)

# We first plot the collapsed static network we made previously, with
# edge thickness by weight. This also lets us find a layout that takes
# into account all edges: we can then use this layout in dynamic
# slices to keep the nodes fixed. Can give this more space by reducing
# margins. Record current parameters so we can restore later.

opar <- par()
par(mar=c(1.0, 1.0, 1.0, 1.0), # margins
    oma=c(1.0, 1.0, 1.0, 1.0)) # outer margins 
lfr = layout_with_fr(HC_wsimp)

# new_window("Hospital Contacts Static Network")

plot(HC_wsimp, layout=lfr, vertex.size=5,
     edge.width=15*(E(HC_wsimp)$weight)/1000,
     vertex.color=v_cols, vertex.label=NA)
legend("topleft", status, col=colors, lty=1, lwd=5, bty="n", cex=0.8)

# We can see that there are some heavily weighted edges. The legend
# tells us who they are between, but this lacks temporal info. 

#=====================================================================
# Plotting Slices 

# HC_12h has our 12 time slices. Plot each of these using the
# same layout and colors as before. 
# new_window("Hospital Contacts 12 Hour Slices")

for(i in (1:8)) {
  plot(HC_12h[[i]], layout=lfr, vertex.size=5,
       edge.width=15*(E(HC_wsimp)$weight)/1000,
       vertex.color=v_cols, vertex.label=NA)
  title(paste(12*(i-1),"to",12*i,"hrs"))
  legend("topleft", status, col=colors, lty=1, lwd=5, bty="n", cex=0.8)
}

# Flip through for animation! 

# -------------------- Exercise --------------------
# P6: Try making 4 plots for HC_24h from P4. Reduce edge thickness to 8*
for(i in (1:4)) {
  plot(HC_12h[[i]], layout=lfr, vertex.size=5,
       edge.width=8*(E(HC_wsimp)$weight)/1000,
       vertex.color=v_cols, vertex.label=NA)
  title(paste(24*(i-1),"to",24*i,"hrs"))
  legend("topleft", status, col=colors, lty=1, lwd=5, bty="n", cex=0.8)
}
# new_window("Hospital Contacts 24 Hour Slices")

# __________ Your visualization for loop here __________

# Restore parameters. (Produces errors; have not investigated)

par(opar)

# If we went instead to finer granularities we could make a movie. 
# There are packages that facilitate this process. 

######################################################################
# Characterizing Dynamic Networks 
######################################################################
# Most analysis of dynamic networks applies static methods to time
# slices, for three reasons: 
# * In the past, it was hard to get time-indexed network data 
# * The graph theory was not as mature for time-indexed networks 
# * Introduction of time greatly increases computational complexity
# All of these are changing but dynamic analysis is still immature. 
# We examine static methods applied to time slices. 
# In the next class I'll show you my own approach. 
######################################################################
# Epidemics and Degree Distribution 
# We saw in the previous topic that degree distribution affects 
# spread of disease: hubs make disease spread faster. This is a 
# relevant question for a hospital context! Also, we saw in the 
# Easley and Kleinberg readings that time of contact can matter, 
# so we should do a temporally sensitive analysis. Does degree
# distribution change with time, and are there hubs? 
######################################################################

# Compute the degree distribution for each 12 hour time slice 

all_degrees <- sapply(HC_12h, degree)
head(all_degrees)

# It's a 75 (vertices) by 8 (slices) matrix 

length(all_degrees[,1])
length(all_degrees[1,])

# Summary statistics of degree distribution for each time slice: 
summary(all_degrees) 

# We can see there are hubs. We could plot it. 

######################################################################
# Plotting Time Slice Degree Distributions 
# **** Frankly I don't find this plot useful: may gloss over ****
######################################################################

new_window("Degree Histograms", 12, 14) # make it larger if you can 

# Make a simple list of labels for each time range: 

sl_lab <- unlist(lapply(1:8, 
                        function(i) {
                          paste(12*(i-1), "-", 12*i, " hrs", sep="")
                        }))
sl_lab

# Make dataframe of Degrees, Slice labels, and Status columns 
# To see what rep[licate] is doing: rep(sl_lab, each=75)

deg_df <- data.frame(Degree = as.vector(all_degrees),
                     Slice  = rep(sl_lab, each=75),
                     Status = rep(V(HC_week)$Status, times=8))
head(deg_df)

# A row for Degree of each of 75 vertices in 8 time slices 

length(deg_df$Degree)  # 75*8=600 
summary(deg_df)

# Popular and powerful graphics package using a graphic grammar: See
# http://ggplot2.org/. This is part of the "tidyverse".

library(ggplot2)
?ggplot2

# Make a plot object. It will plot the Degree factor of our data
# frame, as a bar plot with color filled according to Status. 

?qplot # quick plot: similar to plot 
p = qplot(factor(Degree), data=deg_df, geom="bar", fill=Status)

# Make panels with Degree histograms for each time Slice using this:  

?facet_grid

# Notice how a simple formula expression causes the plot to appear.
# The "Slice~." formula is of form row~column, and means make a row
# for each Slice but don't separate (.) the columns. 

p + facet_grid(Slice ~ .) + xlab("Degree") + ylab("Count")

# We can see a heavy tailed distribution and the category of hubs. 
# * Many individuals have 0 degree over a given time slice: 
#   this may reflect staff shifts, and patients left alone. 
# * The hubs tend to be hospital staff. 
# Hospital staff are likely risk for disease spread: let's identify
# those who are in the top five over time. 

######################################################################
# Inspecting Temporal Degree Distribution 

# Make a vector of the top degree nodes in each time slice. 
# Since there are 75 nodes, ranking above 70 means top 5. 

top_degrees <- lapply(1:8,
                      function(i) {
                        all_degrees[,i][rank(all_degrees[,i])>=70]
                        })

# Table of vertices and number of 12 hour shifts in which they
# are in the top 5

table(unlist(lapply(1:8,
                    function(i) as.numeric(names(top_degrees[[i]])))))

# Nodes 7 and 15 are in top 5 for 4 shifts. What is their status? 

V(HC_week)$Status[c(7,15)]

# -------------------- Exercise --------------------
# P7: What is the status of the nodes in the top 5 for 3 shifts? 

# _____________________________________________________

# Are these merely brief contacts, or are they for significant
# duration? We take the ratio of strength (weighted degree, which
# in our data counts number of episodes) divided by degree. 

all_strength <- sapply(HC_12h, strength)
all_ratios <- all_strength/all_degrees

# Here are the distributions of ratios across all actors: 

summary(all_ratios)    # stats for all 8 time slices 
summary(c(all_ratios)) # stats collapsing time

# Compare to the ratios for for Nurse 7 and Doctor 15

summary(c(all_ratios[c(7,15),]))       # stats collapsing time 
round(all_ratios[c(7,15),], digits=2)  # stats for all 8 time slices 

# We see that Nurse 7 and Doctor 15 not only contact more people
# (being on the high degree list), they have more contacts per
# person contacted than on average. 

######################################################################
# Distances 
# Rate of spread would be related to average shortest paths
# from these individuals to all others. How do they compare & change? 

spread_length <- lapply(1:8,
                        function(i) {
                          spl <- distances(HC_12h[[i]],
                                           v = c(7, 15),
                                           to = V(HC_12h[[i]]),
                                           weights = NA
                                           )
                          spl[spl == Inf] <- NA # drop unconnected pairs
                          spl
                        })

summary(spread_length) 

# From each of 2 nodes to all 75: 75+75 = 150 pairs, 
# for each of 8 slices 

# Get the averages (means), removing NAs 

average_sprlen <- sapply(1:8,
                         function(i) apply(spread_length[[i]], 
                                           1, mean, na.rm=T))
round(average_sprlen,digits=2)

# These seem short, but how do these distances compare to typical 
# distances in the network? They are small compared to the overall
# diameter of each of 8 slices ... 

sapply(HC_12h,diameter)

# Are they different from distances between other pairs of actors? 

round(sapply(HC_12h, mean_distance), digits=2)

# Their distances are actually typical. So the risk they pose is 
# due to the degree and time spent (c.f., Easley & Kleinberg), 
# not the distance. 

######################################################################
# Now go to 15-2-Dynamic-Networks-in-statnet for some nice tools 
######################################################################
# Pau 