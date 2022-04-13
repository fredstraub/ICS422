######################################################################
# reify_link_communities 
# Adds vertices representing link communities to a graph, with the
# intention that they be used for visualization in Gephi. 
# Dan Suthers, Nov 16, 2016
# April 7, 2018 Comment added concerning computation of node.ids
# Nov 14 2019 DS: Minor reordering for clarity; no functional change 
# Apr 11 2021 DS: Replaced use of factor with 1:lc$numbers[[3]] to 
#   generate community IDs, as nodeclusters$cluster is no longer 
#   returning a factor
# Apr  7 2022 DS: Using unique on $nodes instead of numbers. Also,
#   using add_edges to add all edges at once so graph is copied once. 

######################################################################
library(igraph)
library(linkcomm)

# Community labels will be constructed from IDs to indicate 
# that they are communities. 
#
comm_label <- function (id) {return(paste0("COMM_", id))}

# Given a graph g and a legal link community object lc for
# that graph, returns a copy of the graph with communities
# added as vertices. We don't compute the link community
# within this function as we want the user to retain full
# control of that computation through its various parameters. 
# 
reify_link_communities <- function(g, lc) {
	
  #  Mark existing vertices as not being community nodes. 
  V(g)$comm_p <- FALSE 

  # Names of community vertices for each cluster.
  comm_names <- as.character(lapply(unique(lc$nodeclusters$cluster),  
                                    comm_label))
  
  # Create a community vertex for each cluster, using the above
  # labels. Add these vertices all at once for one graph copy. 
  g <- add_vertices(g, 
                    length(comm_names), 
                    label = comm_names, 
                    comm_p = TRUE)
  
  # Get list of vertices from the original graph that are in link
  # communities (some may not be, so we use $node, not V(g)).
  node_ids <- as.numeric(lc$nodeclusters$node)
  
  # Make corresponding list of communities identified by label. 
  comm_labels <- as.vector(vapply(lc$nodeclusters$cluster, 
                                  comm_label, 
                                  character(1)))

  # Add edges from original nodes to community vertices all at once.
  # add_edges wants a list of alternating source and target vertices,
  # that is, pairs of node + community vertices. We use an anonymous
  # function to make pairs, and then flatten the list. Need to use
  # 'which' to map from string comm_labels to actual node IDs in V(g).
  
  g <- add_edges(g, 
                 unlist(lapply(1:length(node_ids), 
                               function(i) { 
                                 c(node_ids[i], 
                                   which(V(g)$label == comm_labels[i]))
                                 })))
  
  return(g)
}

######################################################################
# Pau 