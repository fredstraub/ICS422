######################################################################
# Robustness Simulation Functions
# (c) 2016 Dan Suthers, suthers@hawaii.edu. Written November 27, 2016. 
# Students may modify this code for class assignments. 
# Available as console version in robustness_simulation.R
# 
#   robustness_simulation - simulates random failure or attack on a 
#     network; returns component metrics and optionally geodesic
#     metrics for each step.
#
#   iterated_robustness_simulation - performs multiple trials of 
#     robustness_simulation, returning averaged metrics for each step. 
#   
#   plot_robustness_results - plots either of the above metrics as 
#     a function of number of vertices in the network.
# 
#   Limitations and future work: 
#   * Allow separate specification of numtrials for attack, which 
#     is often deterministic 
#   * Does not return the modified (degraded) graph. This could be
#     packaged up in a named list that also includes the dataframe. 
#   * Currently intended for undirected graphs. It will run on 
#     directed graphs, but component metrics are based on WCC not
#     SCC. Distance metrics will be directed. 
#   * Allow user to give integer to cutoff in robustness_simulation: 
#     if value is greater than 1, this will be numv to delete. 
#   * Consider adding a failure rate parameter numfail, the number
#     of vertices to delete on each pass. This would speed things
#     up when computing mean distances each pass.
#   * Uses old data frames: consider whether tibbles would be 
#     advantageous, but don't fix what ain't broken! 
# 
# Change Log: 
# Apr 11, 2017 DS: various improvements 
# Sep 23, 2018 DS: Minor updates for current script style and clarity
# Nov 19, 2019 DS: Minor improvements for class today. 
# Nov 29, 2019 DS: Added numcomp_ylim, bigcomp_ylim, and meandist_ylim
#  parameters to enable plotting multiple results at same y scale 
#  for comparison. 
# Apr  5 2020 DS: Additional comments for clarification. 
# Apr 12 2022 DS: Tiny changes to comments only. 
# 
######################################################################

library(igraph)

######################################################################
# robustness_simulation (g, attack, cutoff, meandist)
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

robustness_simulation <- function(g, 
                                  attack=FALSE, 
                                  cutoff=1.0, 
                                  meandist=FALSE) {
  # tracks current number of vertices 
  numv <- vcount(g) 
  
  # How many vertices to delete. Can go up to numv - 1 since there is 
  # no point in deleting the last vertex. Cutoff says what percentage
  # of this to actually run. Since we record metrics before deleting
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
  	
  	# Progress meter: . = 100, | = 1000
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
    	    # for debugging
    	    # cat(paste("\n  deleting vertex", v, "of degree", degree(g)[v]))
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
 	             if(attack) {"\nAttack on "} else {"\nRandom Failure of "}, 
 	             deletions,
	             " vertices (", cutoff*100, "%) in ", name, ", ", trials, 
               if (meandist) {" Trials, with meandist\n"} else {" Trials\n"}))
    
    # First trial provides the data frame into which we accumulate results 
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
      # I'm assuming these won't get large enough to overflow.
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
#
#   metrics: data frame in format provided by robustness_simulation
#   name: string name used to identify the graph in the plots 
#   meandist=FALSE (default): geodesic means are not plotted (two plots)
#   meandist=TRUE: geodesic means are plotted (three plots)
#   yzero=FALSE (default): if TRUE, forces y axis to go to 0, which
#     may be useful for comparison across runs with different ranges, 
#     but for most simulations will leave a lot of whitespace. 
#   numcomp_ylim, bigcomp_ylim, and meandist_ylim: passed as ylim to
#     the plotter, each of these is a pair of numbers representing 
#     the minimum and maximum value of the y axis. If not supplied, 
#     will default to range(metrics$numcomp) etc. If yzero is true, 
#     the minimum will be set to 0 regardless of what is given.
#
# Usage Note: No 'cutoff' parameter is available here because it is 
# not necessary. If your simulation metrics include more iterations 
# than you want to plot, simply take a 'row slice', e.g., 
#   plot(metrics[1:100,] ...) 
# (the comma is needed) to plot metrics for the first 100 deletions. 
######################################################################

plot_robustness_results <- 
  function(metrics, name = "", 
           meandist = FALSE, yzero = FALSE,
           numcomp_ylim =  c(if (yzero) 0 else min(range(metrics$numcomp)), 
                             max(range(metrics$numcomp))),
           bigcomp_ylim =  c(if (yzero) 0 else min(range(metrics$bigcomp)), 
                             max(range(metrics$bigcomp))),
           # May not be available, and may include NaN
           meandist_ylim = if(meandist) {
             c(if (yzero) 0 else min(range(na.omit(metrics$meandist))), 
               max(range(na.omit(metrics$meandist)))) 
             } 
           ) {
	
	plot(metrics$numvert, metrics$numcomp, 
	     main = "Number of Components", 
	     sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
	     xlab = "Vertices", ylab = "Components", 
	     # Reverse the x axis to start with all vertices and see decline
		   xlim = rev(range(metrics$numvert)),
		   ylim = numcomp_ylim
		)
		
	plot(metrics$numvert, metrics$bigcomp, 
	     main = "Proportion in Giant Component", 
	     sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
	     xlab = "Vertices", ylab = "Proportion", 
	     xlim = rev(range(metrics$numvert)),
	     ylim = bigcomp_ylim 
		)
		
	if (meandist) {
		plot(metrics$numvert, metrics$meandist, 
		     main = "Mean Distance", 
		     sub = name, cex.sub = 1.2, type = "l", lwd = "2", 
		     xlab = "Vertices", ylab = "Mean Distance", 
			   xlim = rev(range(metrics$numvert)),
         ylim = meandist_ylim
			 )
	}
}

######################################################################
# Pau 