######################################################################
# nonzero_degrees
# 
# Given a graph, returns the degrees of vertices with nonzero degrees.
# If there are nonzero degrees this will have fewer values than V(g)
# 
# September 23 2019 Dan Suthers Created from old code. 
# 
######################################################################

nonzero_degrees <- function(g, mode="total") {
  d <- degree(g, mode=mode) 
  return(d[d != 0])
}

######################################################################
# Pau