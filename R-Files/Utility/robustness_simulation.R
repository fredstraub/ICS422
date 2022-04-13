#!/usr/bin/env Rscript
######################################################################
# Robustness Simulation, Console version 
# (c) 2016 Dan Suthers, suthers@hawaii.edu. 
# Students may modify this code for class assignments.
# 
# Arguments: 
# - path to a graph (string)
# - type of graph, e.g., "gml" or "graphml" 
# - name to identify graph in plots and file (string)
# - cutoff indicating proportion of vertices to delete, in (0,1]
# - trials for number of trials to run (integer > 0)
# - meandist, whether to compute mean distances (0=FALSE, 1=TRUE)
#
# Examples: 
#  Deleting first 5% of vertices over 10 trials, with no meandist
#  ./robustness_simulation.R "graph.gml" "gml" "MyGraph" 0.05 10 0
#  Deleting first 10% of vertices over 3 trials with mean distances 
#  ./robustness_simulation.R "Networks/power_grid.gml" "gml" "PowerGrid" 0.10 3 1
#
# Output: 
# - file paste0(name, "RobustnessSimulation.png") 
#     visualization of number of components and proportion in giant 
#     component as a function of vertices deleted
# - file paste0(name, "RobustnessSimulation.RData") 
#     containing the igraph as "g" and values of name, cutoff, trials, 
#     meandist, and dataframes g.failure.metrics, g.attack.metrics
# 
# TODO (Future Changes): 
# - separate specification of numtrials for attack, which is often
#   deterministic 
# - allow leaving out trials and cutoff to take default
# - printout of some basic stats such as when it becomes disconnected
# - add a failure rate parameter numfail, the number of vertices 
#   to delete on each pass. Then v <- sample(numv, numfail)
# 
# Change Log: 
# Written November 11, 2016. 
# Apr 10, 2018 DS: added meandist and write out trace as RData. 
# Nov 19, 2019 DS: 
#  - Documentation of previously undocumented output
#  - Added meandist parameter to command line interface, and trace
#  - g$name assigned only if not already defined 
# Apr 15 2021 DS: just adjusting comments in command line section
######################################################################
library(igraph)

######################################################################
# robustness_simulation 
# Iterates to delete one vertex each pass until only one remains, 
# recording the values of metrics for each pass. Returns a data frame
#   g: an igraph graph 
#   attack = TRUE: vertex is chosen randomly from those remaining 
#            vertices having the highest degree on each pass. 
#   attack = FALSE (default): vertex is chosen randomly from all 
#            remaining vertices each pass. 
#   cutoff = 1.0 (default): value in (0,1] indicating proportion
#            of the simulation to run, e.g., 0.05 means stop after 
#            5% of the vertices have been deleted. This enables
#            focusing on more plausible scenarios for real systems. 
#   meandist=FALSE (default): no distance metrics are computed 
#   meandist=TRUE: mean geodesic is computed each pass, which may be
#            slow for large graphs (see mean_distance documentation). 
# Value: A data frame with up to |V|-1 rows (depending on cutoff).
#   The first data row has metrics before any vertices are deleted, 
#   and the remaining rows have metrics after each vertex is deleted. 
#   If meandist=FALSE, columns include $numvert (number of vertices), 
#   $numcomp (number of components), and $bigcomp (percent vertices 
#   in largest component). If meandist=TRUE, an additional column 
#   $meandist (mean geodesic distance) is included. 
# Does not modify the argument graph. 
######################################################################

robustness_simulation <- function(g, attack=FALSE, cutoff=1.0, meandist=FALSE) {
  # tracks current number of vertices 
  numv <- vcount(g) 
  # How many vertices to delete. Since we record metrics before deleting
  # the number of data rows recorded will be deletions+1 
  deletions <- floor((numv - 1) * cutoff)
  
  # Create a data frame with one row per graph state and columns for
  # the metrics requested. Results are returned in this data frame. 
  if (meandist) {
    metrics <- data.frame(matrix(data = 0, nrow = deletions+1, ncol = 4))
  	colnames(metrics) <- c("numvert", "numcomp", "bigcomp", "meandist")
  } else {
    metrics <- data.frame(matrix(data = 0, nrow = deletions+1, ncol = 3))
  	colnames(metrics) <- c("numvert", "numcomp", "bigcomp")
  }
  
  # Record metrics for network before first deletion 
  metrics$numvert[1] <- numv
  comp <- components(g, mode="weak")
  metrics$numcomp[1] <- comp$no
  metrics$bigcomp[1] <- max(comp$csize)/numv
  if (meandist) {metrics$meandist[1] <- mean_distance(g)} # slow
  
  # Provide progress meter for larger runs 
  if (deletions > 99) {
  	cat(paste("Deleting", deletions, "of", numv, "vertices (.=100): "))
  	}
  
  # We used row 1 already and want to write to row i, starting at 2
  for (i in 2:(deletions+1)) { 
  	
  	# Progress meter
  	if (deletions > 99) { 
        if (i %% 1000 == 0) {    
    	    cat("|")
        } else if (i %% 100 == 0) {
    	    cat(".")
        }
    }

    # Delete a vertex, randomly chosen unless this is an attack
    # in which case we randomly choose a highest degree vertex. 
    if (attack) {
	    	d <- degree(g)
	    	# Sample randomly from among those of max degree. 
            v <- sample(which(d==max(d)), 1)
            # for debugging
            # cat(paste("\n  deleting vertex", v, "of degree", max(d)))  
    } else {
    	    v <- sample(numv, 1)
    }
    g <- delete_vertices(g, v)
    numv <- numv - 1 
    
    # Record updated metrics in row for current iteration. 
    metrics$numvert[i] <- numv
    comp <- components(g, mode="weak")
    metrics$numcomp[i] <- comp$no
    metrics$bigcomp[i] <- max(comp$csize)/numv
    if (meandist) {metrics$meandist[i] <- mean_distance(g)}
  }
  
  if (deletions > 99) {cat("\n")}
  return(metrics)
} 

######################################################################
# iterated_robustness_simulation
# Performs multiple trials of robustness_simulation, returning a data
# frame with the mean values for each trial. 
#   g, attack, cutoff, meandist: see robustness_simulation documentation 
#   trials=3 (default): number of trials of robustness_simulation
#   plottrials=FALSE(default): no plotting is done 
#   plottrials=TRUE: plot_robustness_results is called to plot the 
#     results of each trial, but not of the final result. This is 
#     useful for illustrating variation. (Attribute g$name is used 
#     to identify the graph in the plot.) 
# Value: Returns a data frame as described for robustness_simulation,
#   but values are the mean for each trial. 
# Does not modify the argument graph. 
######################################################################

iterated_robustness_simulation <- function(g, attack=FALSE, 
                                           cutoff=1.0, meandist=FALSE,
                                           trials=3, plottrials=FALSE) {

    # Tell the user what is about to happen; it could be slow.
    deletions <- floor((vcount(g) - 1) * cutoff)  # Just for their info
    name <- if (is.null(g$name)) {"Anonymous Graph"} else {g$name} 
    cat(paste0(
 	  if(attack) {"\nAttack on "} else {"\nRandom Failure of "}, deletions,
	  " vertices (", cutoff*100, "%) in ", name, ", ", trials, 
      if (meandist) {" Trials, with meandist\n"} else {" Trials\n"}))
    
    # First trial provides the data frame into which we accumlate results 
    cat("Trial 1: ")
    results <- robustness_simulation(g, attack=attack, cutoff=cutoff, 
                                        meandist=meandist)
    if (plottrials) {
      	plot_robustness_results(results, meandist=meandist,         
      	                        name=paste(name, "Trial 1"))}
    
    # Remaining trials are accumulated into this data frame. 
    for (i in 2:trials) {
    	  cat(paste0("Trial ", i, ": "))
      metrics <- robustness_simulation(g, attack=attack, cutoff=cutoff, 
                                          meandist=meandist) 
      if (plottrials) {
      	plot_robustness_results(metrics, meandist=meandist,         
      	                        name=paste(name, "Trial", i))}
      results$numcomp <- results$numcomp + metrics$numcomp 
      results$bigcomp <- results$bigcomp + metrics$bigcomp
      if (meandist) {results$meandist <- results$meandist + metrics$meandist}
    }

    # Now take the average and return 
    results$numcomp <- results$numcomp / trials
    results$bigcomp <- results$bigcomp / trials
    if (meandist) {results$meandist <- results$meandist / trials}
    return(results)
}

######################################################################
# plot_robustness_results 
# Plots the results of a robustness simulation to the current device, 
# including number of components and fraction of vertices in largest 
# component, with optional plotting of geodesic distances.
#   metrics: data frame in format provided by robustness_simulation
#   name: string name used to identify the graph in the plots 
#   meandist=FALSE (default): geodesic means are not plotted (two plots)
#   meandist=TRUE: geodesic means are plotted (three plots)
#   yzero=FALSE (default): if TRUE, forces y axis to go to 0, which
#     may be useful for comparison across runs with different ranges, 
#     but for most simulations will leave a lot of whitespace. 
# Usage Note: No 'cutoff' parameter is available here because it is 
# not necessary. If your similation metrics include more iterations 
# than you want to plot, simply take a 'row slice', e.g., 
#   plot(metrics[1:100,] ...) 
# (the comma is needed) to plot metrics for the first 100 deletions. 
######################################################################

plot_robustness_results <- function(metrics, name = "", meandist = FALSE,
                                    yzero=FALSE) {
	
	plot(metrics$numvert, metrics$numcomp, 
  	   main = "Number of Components", 
	     sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
	     xlab = "Vertices", ylab = "Components", 
	     # Reverse the x axis to start with all vertices and see decline
		   xlim = rev(range(metrics$numvert)),
		   # if requested scale y axis to 0
		   if (yzero) {ylim = c(0, max(range(metrics$numcomp)))}
		)
		
	plot(metrics$numvert, metrics$bigcomp, 
	     main = "Proportion in Giant Component", 
		   sub = name, cex.sub = 1.2, type = "l", lwd = "2", xlab = "Vertices", 
		   ylab = "Proportion", xlim = rev(range(metrics$numvert)),
		   if (yzero) {ylim = c(0, max(range(metrics$bigcomp)))}
		)
		
	if (meandist) {
		plot(metrics$numvert, metrics$meandist, 
		     main = "Mean Distance", 
		     sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
		     xlab = "Vertices", ylab = "Mean Distance", 
			   xlim = rev(range(metrics$numvert)),
         if (yzero) {ylim = c(0, max(range(metrics$meandist)))}
			 )
	}
}

######################################################################
# Read, Run and Plot 
# Arguments: 
# 1 - path to a graph (string)
# 2 - type of graph, e.g., "gml" or "graphml" 
# 3 - name to identify graph in plots and file (string)
# 4 - cutoff indicating proportion of vertices to delete, in (0,1]
# 5 - trials for number of trials to run (integer > 0)
# 6 - meandist, whether to compute mean distances (0=FALSE, 1=TRUE)
######################################################################

args <- commandArgs(trailingOnly=TRUE)
g <- read_graph(args[1], format=args[2])
name <- args[3] 
cutoff <- as.numeric(args[4])
trials <- as.numeric(args[5])
meandist <- as.logical(as.numeric(args[6]))
if(is.null(g$name)) g$name <- name 

cat("Network to analyze:\n")
summary(g) 
if (meandist)  cat("\nMean Distances will be computed (slower)\n") 
if (!meandist) cat("\nMean Distances will not be computed\n")

if (meandist) date()
g.failure.metrics <-
  iterated_robustness_simulation(g, cutoff=cutoff, trials=trials, 
                                    attack = FALSE, meandist=meandist) 
if (meandist) date()
g.attack.metrics <- 
  iterated_robustness_simulation(g, cutoff=cutoff, trials=trials, 
                                    attack = TRUE, meandist=meandist)
if (meandist) date()

# Save plot 
filename <- paste0(name, "RobustnessSimulation.png")
if (meandist) png(filename, 900, 600) else png(filename, 600, 600)
if (meandist) par(mfrow=c(2, 3)) else par(mfrow=c(2, 2))
plot_robustness_results(g.failure.metrics, paste0(name, " Random Failure"),
                        meandist = meandist)
plot_robustness_results(g.attack.metrics, paste0(name, " Attack"),
                        meandist = meandist)
dev.off()
cat(filename, "written\n")

filename <- paste0(name, "RobustnessSimulation.RData")
# Save data 
save(g, name, cutoff, trials, g.failure.metrics, g.attack.metrics, file=filename)
cat(filename, "written\n")

######################################################################
# Pau 