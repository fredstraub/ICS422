######################################################################
# Binned Histogram 
# 
# Given a set of nonzero numeric values, returns a histogram with bins
# increasing in size as powers of the base, defaulting to 2. 
# Useful for making smooth plots of power law like distributions. 
# 
# September 24 2019 Dan Suthers Created from old code, but updated
#   for new seq and hist parameters. 
# 
# TODO: test base parameter further
# 
######################################################################

binned_histogram <- function(data, base=2) {
  numbins <- ceiling(log(length(data), base))
  breakpoints <- exp(seq(log(min(data)), log(max(data)), length.out=numbins+1))
  return(hist(data, breaks=breakpoints, plot=FALSE))
}

######################################################################
# Pau