######################################################################
# Temporal Analysis of Impeachment Twitter Sample using lubridate
# Demonstration for ICS 691C Network Science II 
# 
# This code has parameters specific to the 3 days of twitter data 
# during the prosecution of Trump, but could be extended to other data. 
# 
# Tue Feb 18 2020 DS created for and modified in class.
# Thu Feb 20 2020 DS Modified for/in second class. 
# Sun Feb 23 2020 DS Automated iteration; added metrics; finished plots 
#  with abline labeling of start of proceedings each day. 
# Tue Feb 25 2020 DS Fine tuning for class: comments, and solved 
#  problem of human readable x axis, but left the solution in Scratch
#  so students can try to solve it first. 
# Wed Feb 26 2020 DS Added the additional work done in class. 
#  *** Note: some renaming of functions. 
# Thu Feb 27 2020 DS Added construction of list of span graphs and 
#  iteration over them for centrality metrics, with quick example
#  of plotting. 
# Apr 29 2021 DS Cleaning up for ICS 422/622 Spring 2021
# May  3 2021 DS major rewrite 
# * Terminology changed to be consistent with lubridate 
# * Computes list of interval graphs from the beginning and keeps 
#   them: no more throwaway graphs. 
# * Fixed the which selection of edges to delete using | not ||
# Apr 29 2022 DS Major revisions for ICS 422/622 Spring 2022
# 
######################################################################
# Setup and reading the graph

library(igraph)
library(tidyverse)
library(lubridate) 

# Set to yours (note we are in a subdirectory). 

analysis_dir <- "~/Desktop/Network-Science-Demos/Impeachment-Tweets"
setwd(analysis_dir) # for default 
dir()

# Will use this 

(utility_dir <- paste0(analysis_dir, "/Utility"))
source(paste0(utility_dir, "/remove_isolates.R"))
source(paste0(utility_dir, "/topnv.R"))
source(paste0(utility_dir, "/interval_graph.R")) # will explain below

# -------------------
# Graphml for network 
# Full network 
# mentions_graphml <- "Impeach_Mention_Graph_Multi-Jan-16-Feb-05.graphml"
# Three days and shortened tweet content (less memory but hint of content). 
# mentions_graphml <- "Impeach_Mention_Graph_Multi-Jan-22-24-Short.graphml"
# Three days and no tweet content (for faster reading)

# mentions_graphml <- "Impeach_Mention_Graph_Multi-Jan-22-24.graphml"
# date()
# IMG <- read_graph(paste0(analysis_dir, "/Networks/", mentions_graphml),
#                   format = "graphml")
# date()


# Faster to just load the rdata from the prior demo 

load("Networks/Impeach_Mention_Graph_Multi-Jan-22-24.rdata", 
     verbose=TRUE)
IMG <- mention_graph # shorter name:a Impeachment Mention Graph
summary(IMG)

# If you want centrality metrics on the full graph to be available in
# the time interval graphs, compute them now and annotate the full
# graph with them, but keep the names from colliding.
# ***** Why might we be interested in each of these?

V(IMG)$cum_in_degree <- degree(IMG, mode="in")
V(IMG)$cum_page_rank <- page_rank(IMG)$vector
# avoid purr:::simplify 
V(IMG)$cum_local_cc <-  transitivity(igraph::simplify(as.undirected(IMG)), 
                                     type="local")

# This is slow: over 9 minutes 
# date()
# V(IMG)$cum_betweenness <- betweenness(IMG, directed=FALSE, weights=NA)
# date()

######################################################################
# Extracting Time Slices or "Interval Graphs" 
#
# We want to do computations on sub-networks defined by date ranges,
# which we will call "interval graphs" as they correspond to lubridate
# intervals. These correspond to what package timeordered calls "time
# aggregated networks". This section provides the tools. 
# 
# It is handy to have a list of contiguous interval graphs so we can
# revisit them as needed. This is the approach we take. However, if
# you have a very large network and are having memory problems, you
# may want to make the interval graphs one at a time, recording
# metrics and then reclaiming the memory before making the next one.
# 
######################################################################
# Temporal Parameters and Setup 

# What is the first and last time in the data, in human readable form? 

(first_time <- min(E(IMG)$utc_datetime))
(last_time <- max(E(IMG)$utc_datetime))

# This is a parser converting strings to POSIX time objects.

?ymd_hms

# Convert utc_datetime from character (used to store in .graphml) to
# POSIX utx_posix-datetime (appropriate for time computations).

E(IMG)$utc_posix_datetime <- ymd_hms(E(IMG)$utc_datetime)
head(E(IMG)$utc_posix_datetime)

# ---------------------------------------------
# Defining intervals as start times plus period

?hours

# How much time in a time span to be analyzed. Change this to vary
# granularity of analysis.

(interval_period <- hours(1))

# Stop before this time. 

(last_time_posix <- ymd_hms(last_time))

# Setup for constructing a list of intervals: 
# Current interval being constructed 

(curr_start <- ymd_hms(first_time))
(curr_end   <- ymd_hms(first_time) + interval_period)
class(curr_start)

# Accumulators for start and end times. 

class(c())                   # Empty list will have wrong class.
start_times <- c(curr_start) # Give it an object of desired type ...
class(start_times)           # ... and list will be of class POSIX 
end_times   <- c(curr_end)

# Keep shifting start and end times until the next ending would fall
# off the end of our data. 

while((curr_end + interval_period) <= last_time_posix) {
  curr_start  <- curr_start + interval_period
  curr_end  <-   curr_end   + interval_period
  start_times <- c(start_times, curr_start)
  end_times   <- c(end_times, curr_end)
  
}
length(start_times)
class(start_times) # expect "POSIXct" "POSIXt" 

# Put them together into one object for easy handling by functions 

spans <- tibble(Start = start_times,
                End = end_times) 
spans

# ------------------------------------
# Extracting Interval Graphs

# ***** See Utility/interval_graph.R for code and explanation 

# Making list of interval graphs
# There were 40,000+ isolates in some of the intervals. I chose 
# to remove isolates for this analysis. 

# R note: Previously I initialized the list with c(), and it coerced
# the igraphs added to it to a different data type. This was NOT
# solved by initializing c(firstgraph). The solution was to use lapply
# (which does not simply or transform content) to map a graph
# generating function across the dates.

# Takes less than a minute ... 

interval_graphs <- 
  lapply(start_times, 
         function(s){interval_graph(IMG, 
                                    s, s + interval_period, 
                                    no_isolates = TRUE)})

length(interval_graphs)
summary(interval_graphs[[1]])
summary(interval_graphs[[2]])

######################################################################
# Computing Metrics on Interval Graphs 

# Given an interval graph (possibly a multigraph), returns a tibble
# with various metrics computed (some may be computed on the simple
# graph).

interval_metrics <- function(ig) {
  ig_simple     <- simplify(ig)
  ig_components <- components(ig_simple, mode="weak")
  ig_louvain    <- cluster_louvain(as.undirected(ig_simple))
  tibble(StartTime       = ig$start_time,
         EndTime         = ig$end_time,
         ActorCount      = vcount(ig), 
         TweetCount      = ecount(ig),
         MeanTweets      = mean(degree(ig)),
         MeanPartners    = mean(degree(ig_simple)),
         MeanLocalCC     = transitivity(ig_simple, type="localaverage"),
         Transitivity    = transitivity(ig_simple, type="global"),
         NumComponents   = ig_components$no,
         GiantComponent  = max(ig_components$csize)/vcount(ig_simple),
         DegreeAssortDir = assortativity_degree(ig, directed=TRUE),
         DegreeAssortUnD = assortativity_degree(ig, directed=FALSE),  
         Modularity      = modularity(ig_louvain), 
         CommCount       = length(ig_louvain),
  )
}

# Now iterate that over the time spans, accumulating in a tibble 

all_interval_metrics <- tibble()
for (ig in interval_graphs) {
  cat(ig$name, "\n")
  all_interval_metrics <-  
    rbind(all_interval_metrics, interval_metrics(ig))
  }

# with wide screen ... 

all_interval_metrics

######################################################################
# Plotting Network Metrics 
######################################################################
# Identifying significant event times. 

# Testimony started at 1pm EST each day, and certain days were
# significant. I write these days and times as character strings in
# EST and parse them to POSIX. The parser defaults to UTC but that is
# the wrong time zone for the string I wrote, so I force it to be EST,
# and then convert the result to UTC. (This is safer than writing them
# in UTC in the first place, because human error may and did happen in
# the conversion.)

?force_tz
?with_tz

# These three days were the prosecution. 

(p_day_1_start <- with_tz(force_tz(ymd_hms("2020-01-22 13:00:00"),
                                   tzone="EST"),
                          tzone="UTC"))
(p_day_2_start <- with_tz(force_tz(ymd_hms("2020-01-23 13:00:00"), 
                                   tzone="EST"),
                          tzone="UTC"))
(p_day_3_start <- with_tz(force_tz(ymd_hms("2020-01-24 13:00:00"), 
                                   tzone="EST"),
                          tzone="UTC"))

# (Remainder below are not in the 3-day data set.)
# These three days were the defense

(d_day_1_start <- with_tz(force_tz(ymd_hms("2020-01-25 13:00:00"),
                                   tzone="EST"),
                          tzone="UTC"))
(d_day_2_start <- with_tz(force_tz(ymd_hms("2020-01-27 13:00:00"),
                                   tzone="EST"),
                          tzone="UTC"))
(d_day_3_start <- with_tz(force_tz(ymd_hms("2020-01-28 13:00:00"),
                                   tzone="EST"),
                          tzone="UTC"))

# Closing Events

(witness_vote      <- with_tz(force_tz(ymd_hms("2020-01-31 18:00:00"), # *** FIND ACTUAL
                                       tzone="EST"),
                              tzone="UTC"))
(closing_arguments <- with_tz(force_tz(ymd_hms("2020-02-03 11:00:00"),
                                       tzone="EST"),
                              tzone="UTC"))
(impeachment_vote  <- with_tz(force_tz(ymd_hms("2020-02-05 16:00:00"),
                                       tzone="EST"),
                              tzone="UTC"))

######################################################################
# Plot Functions

# Example: 
# plot_metric(all_interval_metrics$ActorCount, "Active Actors")

plot_metric <- function(metric, label, lpos="topright") {
  # To get the labels positioned correctly, convert to EST 
  est_start_times <- with_tz(start_times, tzone="EST")
  plot(est_start_times, metric, main=label, type="b",
       xlab="EST",  
       ylab=label)
  mark_impeachment_events(lpos=lpos)
  }

# Mark the events in a plot.     
# This only works if numeric UTC times were used for the x axis 

mark_impeachment_events <- function(lpos = "topright") {
  # Prosecution 
  abline(v=p_day_1_start, lty="dotted", lwd=2, col="blue")
  abline(v=p_day_2_start, lty="dotted", lwd=2, col="green")
  abline(v=p_day_3_start, lty="dotted", lwd=2, col="red")
  # Defense 
  # abline(v=d_day_1_start, lty="dotted", col="blue")
  # abline(v=d_day_2_start, lty="dotted", col="green")
  # abline(v=d_day_3_start, lty="dotted", col="red")
  # Closing 
  # abline(v=witness_vote, lty="dotted", col="blue")
  # abline(v=closing_arguments, lty="dotted", col="green")
  # abline(v=impeachment_vote, lty="dotted", col="red")
  # Legend 
  legend(lpos, 
         # bty="n",  # cannot use bty with bg 
         bg="white",
         c(
           "1/22 1pm EST", "1/23 1pm EST", "1/24 1pm EST"
           # ADD COMMA ABOVE 
           # "1/25 1pm EST", "1/27 1pm EST", "1/28 1pm EST",
           # "1/31 6pm EST", "2/03 11am EST", "2/05 4pm EST" # *** FIND ACTUAL 6pm?
           ),
         col=c(# We will repeat the colors 
               # "blue", "green", "red",
               # "blue", "green", "red",
               "blue", "green", "red"
               ),
         lty="dotted", lwd=2, cex=0.8)
}

######################################################################
# Plotting 

# Now we can plot all the metrics we computed above. 

# Daily pattern is evident 
plot_metric(all_interval_metrics$ActorCount, "Active Actors")
plot_metric(all_interval_metrics$TweetCount, "Number of Tweets")

# Spike after start of first day 
plot_metric(all_interval_metrics$MeanTweets, "Mean Tweets per Actor")
plot_metric(all_interval_metrics$MeanLocalCC, "Mean Local Clustering Coefficient")
plot_metric(all_interval_metrics$Transitivity, "Global Transitivity")
plot_metric(all_interval_metrics$NumComponents, "WCC") 

plot_metric(all_interval_metrics$GiantComponent, "% in Giant Component",
            lpos="bottomright")

# Notice events middle of days
plot_metric(all_interval_metrics$DegreeAssortDir, "Directed Degree Assortativity", 
            lpos="topright") 
plot_metric(all_interval_metrics$DegreeAssortUnD, "Undirected Degree Assortativity", 
            lpos="topright")
plot_metric(all_interval_metrics$Modularity, "Louvain Modularity",
            lpos="bottomleft")
plot_metric(all_interval_metrics$CommCount, "Louvain Community Count")

######################################################################
# Other Variations 

# What happens when we don't remove isolates? Rerun with no_isolates =
# FALSE. For which metrics is this more informative?

# Do the degree distributions change? How would we show this? (For
# starters, animate the change in degree distribution with fixed xmax
# and ymax)

######################################################################
# Tracking changes in Actor Centrality 

# Find actors with high values 

id_max    <- max(V(IMG)$cum_in_degree)
(id_max_v  <- V(IMG)[which(V(IMG)$cum_in_degree == id_max)])
pr_max    <- max(V(IMG)$cum_page_rank)
(pr_max_v  <- V(IMG)[which(V(IMG)$cum_page_rank == pr_max)])

# In tight groups 
(lcc_max   <- max(V(IMG)$cum_local_cc, na.rm=TRUE)) # because it will be NA 
(lcc_max_v <- V(IMG)[which(V(IMG)$cum_local_cc == lcc_max)])

# bet_max   <- max(V(IMG)$cum_betweenness)
# (bet_max_v <- V(IMG)[which(V(IMG)$cum_betweenness == bet_max)])
# + 1/44751 vertex, named, from caa7b94:
# [1] realdonaldtrump

# Find top n vertices
topnv(IMG, V(IMG)$cum_in_degree)
topnv(IMG, V(IMG)$cum_page_rank)

# topnv(IMG, V(IMG)$cum_betweenness)

# One way to compute metrics in each span ... 
  
annotate_span_indegree <- 
  function(g) {
    V(g)$span_indegree <- degree(g, mode="in")
    return(g)
    }
interval_graphs <- lapply(interval_graphs, annotate_span_indegree)

summary(interval_graphs[[1]]) # added span_in_degree distinct from cum_

#----------
# Plotting 

# Pull out the values to be plotted. 
values <- 
  lapply(interval_graphs, 
         function(g, a) {V(g)$span_indegree[which(V(g)$name == a)]}, 
         'repadamschiff')
# For now we'll reuse the above function.  You can improve on this. 
plot_metric(values, "Adam Shiff In-Degree")

# ***** Why does this make sense? What was Adam doing on the opening
#       hours of the prosecution?

# values <- 
#   lapply(interval_graphs, 
#          function(g, a) {V(g)$span_betweenness[which(V(g)$name == a)]}, 
#          'repadamschiff')
# plot_metric(values, "Adam Shiff Betweenness")

#---------------------------------------------------
# For other users and metrics ...

annotate_span_pagerank <- 
  function(g) {
    V(g)$span_pagerank <- page_rank(g)$vector
    return(g)
  }
interval_graphs <- lapply(interval_graphs, annotate_span_pagerank)

plot_metric(lapply(interval_graphs, 
                   function(g, a) {V(g)$span_pagerank[which(V(g)$name == a)]}, 
                   'realdonaldtrump'),
            "Donald Trump Page Rank")
plot_metric(lapply(interval_graphs, 
                   function(g, a) {V(g)$span_pagerank[which(V(g)$name == a)]}, 
                   'repadamschiff'),
            "Adam Shiff Page Rank")
plot_metric(lapply(interval_graphs, 
                   function(g, a) {V(g)$span_pagerank[which(V(g)$name == a)]}, 
                   'speakerpelosi'),
            "Speaker Pelosi Page Rank")

# Perhaps Pelosi was introducing the proceedings. 

######################################################################
# Activities for the reader (these were HW for NS II): 

# Plot several top users in the same plot, with different line colors
# to differentiate them, and on the same scale of course for
# comparison.

# Consider other improvements, such as plotting the mean value across
# all actors as a comparison line. Improve on the X axis labeling. Use
# ggplot if you wish.

# Can you find actors that have high centrality in a *subset* of time
# slices and plot their centrality over time?

######################################################################
# Pau 