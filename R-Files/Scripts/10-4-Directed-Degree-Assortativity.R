######################################################################
# Topic 10 Degree Assortativity: Directed knn 
# Meant to be run in the context of 10-3-Degree-Correlation-Demo
# Aug 28, 2018 DS: Minor updates for current script style and clarity
# Oct 31, 2019 DS: Minor updates for fall 2019 class
# Mar 23 2021 DS: Minor updates for spring 2021 422/622
# Mar 24 2022 DS: Updates for spring 2022 422/622 (knn mode)
# * New material for knn mode and neighbor.degree.mode
######################################################################
# If starting fresh 

library(igraph)
library(tibble)
setwd("/Users/frederickstraub/GitHub/ICS422/R-Files") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

plot_assortativity_rewiring <- function(name, source, rewired) {
  source.knn <- knn(source, weights=NA)
  rewire.knn <- knn(rewired, weights=NA)
  plot(source.knn$knnk, log="xy",
       main=paste(name, "knn(k)"), xlab="k", ylab="knn(k)")
  legend("bottomleft", bty="n", cex=1.2,
         c(paste0("r = ",
                  round(assortativity_degree(source, directed=FALSE),
                        digits=4))))
  plot(rewire.knn$knnk, log="xy", 
       main=paste(name, "Rewired knn(k)"), xlab="k", ylab="knn(k)")
  legend("bottomleft", bty="n", cex=1.2,
         c(paste0("r = ", 
                  round(assortativity_degree(rewired, directed=FALSE),
                        digits=4))))
}

######################################################################
# Wrapping up the activity ... 
######################################################################
# Compute and interpret degree correlations for the following
# * Compute the degree correlations using assortativity_degree 
# * Rewire the network to determine whether it is due to structural
#   cutoffs or exogeneous
# * Identify any explanatory domain processes 
# You may use the function for the second part. 

# Network Science 

NS <- read_graph("Networks/netscience.graphml", format="graphml")
summary(NS)

assortativity_degree(NS)

f <- 1000
NS.rewired <- rewire(NS, with = keeping_degseq(niter = ecount(NS) * f))
assortativity_degree(NS.rewired)

plot_assortativity_rewiring("Network Science", NS, NS.rewired)

# Interpret ... 

######################################################################
# Directed graphs
######################################################################
# Which lets us handle directed graphs? 

?assortativity 
?knn 

# A good example: 

HEP <- read_graph("Networks/cit-HepTh.gml", format="gml")

# This is not a simple graph, so let's simplify before proceeding. 

is_simple(HEP)
HEP.simp <- simplify(HEP)

# How much changed? 

summary(HEP)
summary(HEP.simp)
ecount(HEP) - ecount(HEP.simp) # insignificant 

# assortativity lets us specify whether directed or not. Compare
# assortativities for the 4 versions (didn't use tibble as it rounds)

data.frame(
  Directed = c(assortativity_degree(HEP, directed=TRUE), 
               assortativity_degree(HEP.simp, directed=TRUE)), 
  Undirected = c(assortativity_degree(HEP, directed=FALSE), 
                 assortativity_degree(HEP.simp, directed=FALSE)),  
  row.names = c("Original", "Simplified")
)

# So, simplification does not matter and all values are close to 0. 

# Since assortativity is neutral, one might think there is not much
# point in further investigation, but it turns out the single number
# hides something structurally interesting that we will see with knn. 

######################################################################
# Directed knn
# Read this while running the rewiring below. 
# Look at ?knn mode and neighbor.degree.mode 

?knn 

# Rewire the simplified graph. (Takes a minute; don't do 1000 in class)

HEP.simp.rewired <- rewire(HEP.simp, 
                           with = keeping_degseq(niter = ecount(HEP)*100)) 


##############################
# Directed Plot Function

plot_assortativity_rewiring <- function(name, source, rewired, 
                                        mode="all", nmode="all",
                                        directed=FALSE, weights=NA) {
  
  source.knn <- knn(source, weights=weights, mode=mode, 
                    neighbor.degree.mode=nmode)
  rewire.knn <- knn(rewired, weights=weights, mode=mode, 
                    neighbor.degree.mode=nmode)
  
  plot(source.knn$knnk, log="xy",
       main=paste(name, "knn(k)"), xlab="k",
       ylab=paste0("knn(k), mode=", mode))
  # Linear correlation between degree k and knn
  legend("bottomleft", bty="n", cex=1.2, 
         c(as.character(round(cor.test(degree(source,mode=mode),
                                       source.knn$knn)$estimate, 
                              digits=4))))
  plot(rewire.knn$knnk, log="xy", 
       main=paste(name, "Rewired knn(k)"), xlab="k", 
       ylab=paste0("knn(k), mode=", mode))
  legend("bottomleft", bty="n", cex=1.2, 
         c(as.character(round(cor.test(degree(rewired,mode=mode),
                                       rewire.knn$knn)$estimate, 
                              digits=4))))
}

# Assortativity does not let us specify direction so I am taking the
# correlation between degree and knn of each node. This is untested. 

?cor.test 

#################################
# Degree Distribution and ALL-ALL

new_window(title="HEP Deg Dist", width=12, height=6)
par(mfrow=c(1,2))
plot(degree_domain(HEP, mode="in"), 
     degree_distribution(HEP, mode="in"),
     log="xy", main="HEP IN degree distribution")
plot(degree_domain(HEP, mode="out"),
     degree_distribution(HEP, mode="out"),
     log="xy", main="HEP OUT degree distribution")


new_window(title="HEP knn(k) ALL-ALL", width=12, height=6)
par(mfrow=c(1,2))
plot_assortativity_rewiring("HEP Citations ALL-ALL", HEP.simp, 
                            HEP.simp.rewired, directed=TRUE)

# Looking at the distribution, this is an odd one. It is assortative
# for part of the distribution and disassortative for another part.
# There is structural disassortativity but it is weak and does not
# explain the assortative part. Can you?

#################################
# Let's break it down in the four combinations: 

new_window(title="HEP knn(k) IN-IN", width=12, height=6)
par(mfrow=c(1,2))
plot_assortativity_rewiring("HEP Citations IN-IN", HEP.simp, 
                            HEP.simp.rewired, directed=TRUE, 
                            mode="in", nmode="in")

new_window(title="HEP knn(k) OUT-OUT", width=12, height=6)
par(mfrow=c(1,2))
plot_assortativity_rewiring("HEP Citations OUT-OUT", HEP.simp, 
                            HEP.simp.rewired, directed=TRUE, 
                            mode="out", nmode="out")

new_window(title="HEP knn(k) IN-OUT", width=12, height=6)
par(mfrow=c(1,2))
plot_assortativity_rewiring("HEP Citations IN-OUT", HEP.simp, 
                            HEP.simp.rewired, directed=TRUE, 
                            mode="in", nmode="out")

new_window(title="HEP knn(k) OUT-IN", width=12, height=6)
par(mfrow=c(1,2))
plot_assortativity_rewiring("HEP Citations OUT-IN", HEP.simp, 
                            HEP.simp.rewired, directed=TRUE, 
                            mode="out", nmode="in")

# We can see that some directions are structural. 
# There appears to be a structural component to OUT-IN and IN-OUT
# IN-IN and OUT-OUT are not structural. 


######################################################################
# Pau 


