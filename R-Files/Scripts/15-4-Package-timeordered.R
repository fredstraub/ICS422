######################################################################
# Package timeordered demonstration 
# Benjamin Blonder's (bblonder@email.arizona.edu) examples from igraph
# documentation, elaborated by Dan Suthers to understand what is
# happening and apply to our data.
# Feb 27 2020 DS first draft. 
# Apr 29 2021 DS Cleaning up for ICS 422/622 Spring 2021
# * For brevity, removed attempt to apply to Twitter data 
# Apr 28 2022 DS Minor revisions for ICS 422/622 Spring 2022
######################################################################

library(lubridate)
library(igraph)
library(timeordered)
# library(tidyverse) # possible conflict reported when dplyr is loaded 

setwd("~/Desktop/Network-Science-Demos")
source("Utility/new_window.R")

?timeordered # and click on index ... note randomization 

######################################################################
# Making a time-ordered network: "generate t.o. network"

?generatetonetwork

data(ants)
head(ants)

# Trying it without vertex info, it makes an igraph.

test <- generatetonetwork(ants)
summary(test) 

# But their demo adds vertex data. They do not explain the fields or
# why we are doing this, but they appear to be allowing for null
# values...

allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
g <- generatetonetwork(ants, allindivs)
summary(g) # ... there are a few extra vertices and edges
vcount(g) - vcount(test)
ecount(g) - ecount(test)

# Their example includes this plot ... 

?plottonet # Following Figure 1B of their paper

# Use a new window since they set some parameters they don't clean up. 

new_window("Timeordered ANT Plots", 12, 12)
plottonet(g)

# Not exactly clear! I will explain, but would need some work. 

######################################################################
# How to make time aggregated networks 

# First we make the list of windows, like my spans tibble of
# start_times and end_times

?generatetimedeltas

# Then we use that to make the networks, like iterative application of
# my interval_graph.R

?generatenetworkslices

# Their demo: 

td100 <- generatetimedeltas(0,1500,100)
head(td100)
ns100 <- generatenetworkslices(g, td100)
summary(ns100[[1]])
summary(ns100[[2]]) # etc. 

# A fancy plotting function 

?plotnetworkslices
plotnetworkslices(ns100, td100)

######################################################################
# Applying a metrics function to time-aggregated networks 

?applynetworkfunction

md100 <- applynetworkfunction(ns100, diameter)
head(md100, 4)

# One can use lags rather than deltas to make the slices. 

?generatetimelags
tl100 <- generatetimelags(0,1500,100)

# Deltas were contiguous; lags are cumulative: 

head(td100, 4)
head(tl100, 4)
nl100 <- generatenetworkslices(g, tl100)

# Their demo had function(x){diameter(x)}!!!

ml100 <- applynetworkfunction(nl100, diameter)

# Plotting the results. 

new_window("Timordered Ants Metrics", 12, 6) 
par(mfrow=c(1,2))
plot(midpoints(td100), # a utility function they provide 
     main="Diameter by Time Deltas",
     unlist(md100),    # because that list was ugly! 
     type="l", xlab="Time (window size = 100)", ylab="Diameter")
plot(maxpoints(tl100),
     main="Diameter by Time Lags",
     unlist(ml100),
     type="l", xlab="Aggregation time", ylab="Diameter")

# Beyond their demo, we could compute other stuff 

m_r <- applynetworkfunction(ns100, reciprocity)
plot(midpoints(td100),
     main="Reciprocity by Time Deltas",
     unlist(m_r),
     type="l", xlab="Time (window size = 100)", ylab="Reciprocity")

m_cc <- applynetworkfunction(ns100, 
                             function(x){transitivity(x, type="global")})
plot(midpoints(td100),
     main="Transitivity by Time Deltas",
     unlist(m_cc),
     type="l", xlab="Time (window size = 100)", ylab="Transitivity")

# So there is potential to do what I did with custom code.

######################################################################
# Transmission Latencies 

?generatelatencies 

new_window("Timordered Ants Latencies", 12, 12) 
l <- generatelatencies(ants, allindivs)
image(l[,,1000],axes=FALSE,frame=TRUE,col=rainbow(100))
axis(1, at = (1:ncol(l))/ncol(l), labels=colnames(l),tick=FALSE,las=2,cex.axis=0.2)
axis(2, at = (1:nrow(l))/nrow(l), labels=rownames(l),tick=FALSE,las=2,cex.axis=0.2)

# ... and apparently that is supposed to tell us something! 

######################################################################
# Pau 
