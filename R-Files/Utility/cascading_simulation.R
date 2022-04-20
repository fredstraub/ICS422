######################################################################
# Cascading_Simulation 
# Roderick Tabalba, Jr <tabalbar@hawaii.edu>

# Description
#   Implements model of diffusion in networks from Easley, D., &
#   Kleinberg, J. (2010). Networks, Crowds and Markets: Reasoning
#   about a Highly Connected World, Chapter 19.
#
#   Initializes graph by setting every node's behavior to FALSE, and
#   specified vertices v to TRUE. Iterates checking whether the number
#   of each vertex's affected neighbors meets or exceeds a threshold
#   value q. Prints the number of new nodes that were affected and
#   plots the graph after each iteration.

# Usage
#   cascading_simulation(
#      g,
#      v,
#      q = 0.3,
#      plot = TRUE
#      verbose = FALSE
#   )

# Arguments
#   g:      an igraph graph 
#   v:      Vertices to initialize as affected behavior
#   q:      Threshold value to check if the ratio of neighbors
#           affected meet or exceeds this value
#   plot:   TRUE/FALSE to plot after each iteration
#   verbose:  TRUE/FALSE to print # of nodes affected after each iteration

# Value: 
#   A modified graph (copy of original: does not modify the argument 
#   graph) with these graph and vertex attributes: 
#     iterations (g/n): total number of iterations run 
#     behavior (v/l): TRUE if the behavior was adopted 
#     iteration (v/n): Step of the simulation on which the current value
#      of the behavior was assigned. 0 for initial adopters and nodes
#      that never changed their behavior 
#     color (v/c): Colors for TRUE and FALSE behavior. 

# Apr 15 2021 First debugged version by RJ 
# Apr 16 2021 Formatted documentation, simplified function 
#             (added new parameter method=c("vertex", "edge)),
#             I don't understand vapply & vector assignment
# Apr 18 2021 Worked more on lapply and got vector assignment working!
# Apr 21 2021 New parameter v and removed method. Utility to select
#             random edges. Return {graph, affected, unaffected}.
# Apr 26 2021 Add Comments, Example, and parameters(plot, text) for debug
# Apr 30 2021 debug -> {plot, verbose} and changed return result to graph
# Apr 14 2022 Dan Suthers: 
#             Minor improvements to code and comments for clarity.
#             Improved plot titles and vertex size. Plots the start
#             state. Changed test against q from > to >= for "meets or
#             exceeds" as this is Easley & Kleinberg's specification.
#             Debugged examples and added sample_pa example.
# Apr 15 2022 DS: Rewrote adopt_behavior to only return TRUE for those
#             changing in the current iteration. Then recording the
#             iteration in which nodes got their current value as a
#             vertex attribute $iteration. This enables us to replay
#             the simulation. 
# 
######################################################################
library(igraph)

cascading_simulation <- function(g, v, q = .3, 
                                 plot=TRUE, 
                                 verbose=FALSE, 
                                 layout="nicely") {
  
  V(g)$behavior = FALSE     # Initialize behavior
  V(g)[v]$behavior <- TRUE  # Initialize vertices with behavior
  V(g)$iteration <- 0       # When current value was assigned 
  # Initialize colors for visualization
  V(g)$color <- ifelse(V(g)$behavior == TRUE, "pink", "white")
  
  iteration <- 0 # Iteration counter
  # How many nodes changed behavior this iteration 
  iteration_change_count <- length(v) 
  
  # Default if a specific layout has not been specified
  if(layout == "nicely") {
    layout <- layout_nicely(g)
  } 
  
  # Plot start state 
  if(plot){
    plot(g,
         main = paste("Start State:", 
                       iteration_change_count, 
                      "initial adopters"),
         layout=layout, vertex.size=10, vertex.label=NA)
  }
  
  # Run while loop till no new nodes were affected
  # If no change, stop the loop and return the result
  while(iteration_change_count != 0){
    
    iteration <- iteration + 1
    
    # Run adopt_behavior function on all vertices to get nodes to
    # change. Record new behavior and iteration on which changed.
    changed_nodes <- sapply(V(g), adopt_behavior, g=g, q=q) 
    iteration_change_count <- length(which(changed_nodes == TRUE))
    V(g)[changed_nodes]$behavior  <- TRUE 
    V(g)[changed_nodes]$iteration <- iteration 
    V(g)[changed_nodes]$color     <- "pink"
    
    # Plot current iteration of affected nodes
    if(plot){
      plot(g,
           main = paste0("Iteration ", iteration, ": ",
                         iteration_change_count, 
                        " nodes changed"), 
           layout=layout, vertex.size=10, vertex.label=NA
      )
    }

    # Printing number of nodes affected and full convergence
    if (verbose) {
      cat(paste("----",
                  iteration_change_count,
                  "nodes changed\n"))
    }
  }
  
  if (verbose && all(V(g)$behavior)) {
      cat("**** All nodes were affected ****\n")
    }

  # Return the modified graph, which has a full record. 
  g$iterations <- iteration
  return(g)
}

# Helper function to test whether a vertex v should adopt a behavior.
# If v has not yet adopted, checks whether the ratio of neighbors
# affected meets or exceeds threshold value q. Returns TRUE only for
# those nodes needing to change this iteration.

adopt_behavior <- function(v, g, q){
  if(V(g)[v]$behavior == TRUE) {
    return(FALSE) # because it need not change
  } else {        # check whether a non-adoptor now changes 
    # Get neighbors of node
    neighbors = neighbors(g, v)
  
    # Count of neighbors having the behavior 
    A <- length(which(V(g)[neighbors]$behavior == TRUE))
    # Total neighbors, with min of 1 to prevent divide by 0
    N <- max(length(neighbors), 1) 

    # If ratio meets or exceeds threshold q, return TRUE to change the
    # node's behavior; otherwise FALSE
    if(A/N >= q) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }
}

# # Examples
# # (Recommend running each example several times.)
#
# gnm <- sample_gnm(100, 300) # 100, 100 for less dense network
# summary(gnm)
#
# # starting with random vertices
# gnm.sim1 <- cascading_simulation(gnm, sample(V(gnm), 4), q=0.2, verbose=TRUE)
#
# # inspect results 
# gnm.sim1$iterations
# V(gnm.sim1)$iteration
#
# # increasing q limits the cascade 
# gnm.sim1 <- cascading_simulation(gnm, sample(V(gnm), 4), q=0.3)
#
# # starting with random edges
# gnm.sim2 <- cascading_simulation(gnm, ends(gnm, sample(E(gnm), 1)), q=0.2)
# 
# # Illustrates how cascades stop at cluster boundaries
## # Examples
# # (Recommend running each example several times.)
#
# gnm <- sample_gnm(100, 300) # 100, 100 for less dense network
# summary(gnm)
#
# # starting with random vertices
# gnm.sim1 <- cascading_simulation(gnm, sample(V(gnm), 4), q=0.2, verbose=TRUE)
#
# # inspect results 
# gnm.sim1$iterations
# V(gnm.sim1)$iteration
#
# # increasing q limits the cascade 
# gnm.sim1 <- cascading_simulation(gnm, sample(V(gnm), 4), q=0.3)
#
# # starting with random edges
# gnm.sim2 <- cascading_simulation(gnm, ends(gnm, sample(E(gnm), 1)), q=0.2)
# 
# # Illustrates how cascades stop at cluster boundaries
# pa <- sample_pa(100, directed=FALSE)
# summary(pa)
# pa.sim1 <- cascading_simulation(pa, sample(V(pa), 4), q=0.3)
# pa.sim2 <- cascading_simulation(pa, ends(pa, sample(E(pa), 1)), q=0.3)

# summary(pa)
# pa.sim1 <- cascading_simulation(pa, sample(V(pa), 4), q=0.3)
# pa.sim2 <- cascading_simulation(pa, ends(pa, sample(E(pa), 1)), q=0.3)
