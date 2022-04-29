######################################################################
# Utility - Extracting Interval Graphs
# Apr 28 2022 Dan Suthers extracted code from Temporal Multigraph demo
# 
# Deletes any edges not in the specified half-open interval
# [start_time, end_time). Half-open is used so edges falling on
# end_time do not appear in two interval graphs. Optionally removes
# isolates. Assumes that g has 'utc_posix_datetime' attribute of the
# same data type of (comparable to) start_time and end_time.
######################################################################
library(igraph)
library(lubridate)

# Implementation nodes (avoiding previous bugs): 
# * Use < start_time so edges at start_time are in the span 
# * Use >= end_time so edges at end time don't appear in two spans
# * Must use vector-or | not short-cut || to get the full vector:
#   See ?base::Logic for use of | vs ||

interval_graph <- function(g, start_time, end_time, no_isolates=FALSE) {
  ig <- delete_edges(g, 
                     E(g)[which((E(g)$utc_posix_datetime < start_time) |
                                 E(g)$utc_posix_datetime >= end_time)])
  if (no_isolates) {ig <- remove_isolates(ig)}
  ig$name <- paste(start_time, "to", end_time)
  ig$start_time <- start_time
  ig$end_time   <- end_time
  return(ig)
}


######################################################################
# Pau