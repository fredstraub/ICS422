######################################################################
# Cascading_Simulation 
# Roderick Tabalba, Jr <tabalbar@hawaii.edu>
# Description
#   Initializes graph by setting every node's behavior to FALSE
#   Initialize n number of vertices to be affected
#   Iterates on graph checking every n vertex's neighbors if the number
#   of affected neighbors exceed a threshold value q
#   Prints the number of new nodes that were affected and plots the graph 
#   After each iteration

# Usage
#   cascading_simulation(
#      g,
#      v,
#      q = 0.3,
#      plot = FALSE
#      verbose = FALSE
#   )

# Arguments
#   g:      an igraph graph 
#   v:      Vertices to initialize as affected behavior
#   q:      Threshold value to check if the ratio of neighbors
#           affected exceeds this value
#   plot:   TRUE/FALSE to plot after each iteration
#   verbose:  TRUE/FALSE to print # of nodes affected after each iteration

#   Does not modify the argument graph. 
#
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
#             Improved plot label and vertex size 
#             Debugged examples and added sample_pa 
#
# Value: A named list:
#   graph:      The final outcome of the graph.
#   affected:   List of vertices adopting new behavior 
#   unaffected: List of vertices retaining original behavior
# 

######################################################################
library(igraph)

cascading_simulation <- function(g, v, q = .3, 
                                 plot=TRUE, 
                                 verbose=TRUE, 
                                 layout="nicely") {
  
  V(g)$behavior = FALSE  # Initialize behavior
  
  V(g)[v]$behavior <- TRUE  # Initialize vertices with behavior
  
  # Initialize to check for convergence
  # Total Number of vertices that were affected in previous iteration
  prev_num_affected <- length(v) 
  # Total Number of nodes that were affected in current iteration
  new_num_affected <- 0
  iteration <- 0 # Keep track of iteration
  V(g)$color <- ifelse(V(g)$behavior == TRUE, "pink", "white")
  
  # If a specific layout is preferred
  if(layout == "nicely") {
    layout <- layout_nicely(g)
  } 
  
  # Plot start state 
  if(plot){
    plot(g,
         main = paste("Start State:", 
                        prev_num_affected, 
                      "initial adopters"),
         layout=layout, vertex.size=10)
  }
  
  # Run while loop till no new nodes were affected
  # If no change, stop the loop and return the result
  while(prev_num_affected != new_num_affected){
    
    # Before Update
    prev_num_affected <- length(which(V(g)$behavior == TRUE)) 
    iteration <- iteration + 1
    
    # Update: Run adopt_behavior function on all vertices to get nodes
    # to change
    changed_nodes <- sapply(V(g), adopt_behavior, g=g, q=q) 
    V(g)$behavior <- changed_nodes # Apply result of adopt_behavior
    V(g)$color <- ifelse(V(g)$behavior == TRUE, "pink", "white")
    
    # After Update
    new_num_affected <- length(which(changed_nodes == TRUE))
    
    # Plot current iteration of affected nodes
    if(plot){
      plot(g,
           main = paste0("Iteration ", iteration, ": ",
                        new_num_affected - prev_num_affected, 
                        " vertices affected"), 
           layout=layout, vertex.size=10
      )
    }

    # Check for full convergence
    if(new_num_affected == vcount(g)){
      if(verbose){
        print(paste("----", 
                    new_num_affected - prev_num_affected, 
                    "number of nodes affected ----"))
        print(paste("**** All nodes were affected ****"))
      }
      break
    }
    # Text output of new nodes that were affected
    if(verbose){
      print(paste("----", 
                  new_num_affected - prev_num_affected, 
                  "number of nodes affected ----"))
    }
  }
  result <- NULL
  result$graph <- g
  result$affected <- which(changed_nodes == TRUE)
  result$unaffected <- which(changed_nodes == FALSE)
  return(result)
}


# Helper function to adopt a behavior.
# Computes every node's neighbors to check if
# the ratio of neighbors affected exceed threshold value q

adopt_behavior <- function(V, g, q){
  # Get neighbors of node
  neighbors = neighbors(g, V)
  
  # Then check the neighbors of the affected node's neighbors to get 
  # total number of neighbors that are affected
  A <- length(which(V(g)[neighbors]$behavior == TRUE))
  # Total number of neighbors
  B <- length(neighbors)
  
  #If no neighbors, set to 1 in case divide by 0
  if(B == 0) {
    B <- 1
  }
  # If ratio exceeds threshold q, return TRUE to change the node's behavior
  # Or if node already has property behavior == TRUE
  if(A/B > q || V(g)[V]$behavior == TRUE){
    return(TRUE)
  } else {
    # If no change, return FALSE for that node
    return(FALSE)
  }
  
}

# Examples
#
# gnm <- sample_gnm(100, 300)
# gnm <- sample_gnm(100, 100) # Less dense network
# starting with random vertices
# gnm.sim1 <- cascading_simulation(gnm, sample(V(gnm), 4), q=0.2, verbose=TRUE)
# starting with random edges 
# gnm.sim2 <- cascading_simulation(gnm, ends(gnm, sample(E(gnm), 1)), q=0.2)
#
# Illustrates how cascades stop at cluster boundaries 
# pa <- sample_pa(100, directed=FALSE)
# pa.sim1 <- cascading_simulation(pa, sample(V(pa), 4), q=0.2)
# pa.sim2 <- cascading_simulation(pa, ends(pa, sample(E(pa), 1)), q=0.2)
# pa.sim3 <- cascading_simulation(pa, ends(pa, sample(E(pa), 1)), q=0.1)
