######################################################################
# Using a Data Frame to Gather Results (NetScience Example)
#
# Often we work with structured sets of data and results. For example,
# we want to compare several different graph models to the natural 
# network on several different metrics. It would be nice to record 
# the results in a tabular format, indexed by model and metric names. 
# We'll do this with one of the most fundamental data structures in R,
# the Data Frame. (It is usually used for data, not results, but is 
# equally useful for results as we will see here.)
#
# Dan Suthers created February 1, 2018 from older material 
# Jul 24 2018 DS: Minor updates for current script style and clarity.
# Aug 9  2018 DS: Added the alternative method section. 
# Sep 19 2019 DS: Checks for class, and comment on tibbles. 
# Feb  1 2020 DS: Small revisions for 2020 self study students. 
# Feb  9 2021 Revisions for ICS 422/622 Spring 2021
# Feb 10 2022 Minor revisions for ICS 422/622 Spring 2022. 
######################################################################
# If you have not done so already today ... 

library(igraph)
setwd("/Users/fred/Github/ICS422/R-Files") # Set to yours 
source("Utility/degree_domain.R")
source("Utility/nonzero_degree_distribution.R")
NS <- read_graph("Networks/netscience.graphml", format="graphml")

######################################################################
# Making a Data Frame 

# Details here, but let's just dive in 
?data.frame 

# We will make a data frame to record the various graph metrics by
# which we compare the natural graph to its random models. 

# Row names are the natural graph and the different models 

rnames <- c("NetScience", "G(n,m)", "Config", "Rewire", "SmallW")

# Column names are the model and metrics by which we compare the models

cnames <- c("Model", "V", "E", "meandist", "components", "transitivity", "degreeassort")

# We will wrap the above labels around a matrix, initialized to 0: 

matrix(data=0, nrow=5, ncol=6)

# Make a data frame to store the results. It has the above row and
# column names wrapped around a matrix of 0's. ncol=6 because the 
# first column is just the index into rows. 

NS.metrics <- data.frame(rnames, matrix(data=0, nrow=5, ncol=6))
colnames(NS.metrics) <- cnames
NS.metrics

# Now we fill it in cell by cell. There are ways to rewrite this
# code to fill in an entire column at once, or an entire row. 

# Source graph metrics 

NS.metrics$V[1] <- vcount(NS)
NS.metrics$E[1] <- ecount(NS)
NS.metrics$meandist[1] <- mean_distance(NS)
NS.metrics$transitivity[1] <- transitivity(NS, type="global")
NS.metrics$components[1] <- count_components(NS)
NS.metrics$degreeassort[1] <- assortativity_degree(NS)

NS.metrics

# G(n,m) 
# (For more accurate evaluation of the model we would take multiple 
# samples of each model and compute the means of metrics.) 

NS.gnm <- sample_gnm(vcount(NS), ecount(NS))
NS.metrics$V[2] <- vcount(NS.gnm)
NS.metrics$E[2] <- ecount(NS.gnm)
NS.metrics$meandist[2] <- mean_distance(NS.gnm)
NS.metrics$transitivity[2] <- transitivity(NS.gnm, type="global")
NS.metrics$components[2] <- count_components(NS.gnm)
NS.metrics$degreeassort[2] <- assortativity_degree(NS.gnm)

NS.metrics

# Configuration Model 

NS.config <- sample_degseq(degree(NS), method="simple.no.multiple")
NS.metrics$V[3] <- vcount(NS.config)
NS.metrics$E[3] <- ecount(NS.config)
NS.metrics$meandist[3] <- mean_distance(NS.config)
NS.metrics$transitivity[3] <- transitivity(NS.config, type="global")
NS.metrics$components[3] <- count_components(NS.config)
NS.metrics$degreeassort[3] <- assortativity_degree(NS.config)

NS.metrics

# Degree Preserving Randomization (Rewiring) model

NS.rewired <- rewire(NS, keeping_degseq(niter=ecount(NS)*100))
NS.metrics$V[4] <- vcount(NS.rewired)
NS.metrics$E[4] <- ecount(NS.rewired)
NS.metrics$meandist[4] <- mean_distance(NS.rewired)
NS.metrics$transitivity[4] <- transitivity(NS.rewired, type="global")
NS.metrics$components[4] <- count_components(NS.rewired)
NS.metrics$degreeassort[4] <- assortativity_degree(NS.rewired)

NS.metrics 

# Small World model (not on HW; here just for illustration)

summary(NS)
NS.sw <- sample_smallworld(1, 1589, 2, 0.05) 
summary(NS.sw) 
NS.metrics$V[5] <- vcount(NS.sw)
NS.metrics$E[5] <- ecount(NS.sw)
NS.metrics$meandist[5] <- mean_distance(NS.sw)
NS.metrics$transitivity[5] <- transitivity(NS.sw, type="global")
NS.metrics$components[5] <- count_components(NS.sw)
NS.metrics$degreeassort[5] <- assortativity_degree(NS.sw)

# This gives your results to interpret. When you do the homework,
# display the corresponding table for the homework network, and then
# discuss how the models compare on each metric. 

NS.metrics 

# Plotting 

par(mfrow=c(2,2))

plot(degree_domain(NS), 
     nonzero_degree_distribution(NS), 
     main="Natural Network", xlab="k", ylab="P(k)")
plot(degree_domain(NS.gnm), 
     nonzero_degree_distribution(NS.gnm), 
     main="G(n,m)", xlab="k", ylab="P(k)")
# Always the same as natural network ... 
plot(degree_domain(NS.rewired), 
     nonzero_degree_distribution(NS.rewired), 
     main="Config and Rewired", xlab="k", ylab="P(k)")
plot(degree_domain(NS.sw), 
     nonzero_degree_distribution(NS.sw), 
     main="Small World", xlab="k", ylab="P(k)")

# ***** Can you explain the degree distribution differences in terms 
#       of how the graphs are constructed? 

######################################################################
# Column-wise construction 
######################################################################
# This section illustrates an alternative method of construction that 
# may be more convenient in some situations. We construct the data 
# frame by pasting on columns of metrics with cbind. We don't need to
# anticipate the number of columns in advance; only the number of rows. 

# We assume that the graphs have already been created, and that 
# rnames and cnames are defined as above. We need a list of graphs: 

models <- list(NS, NS.gnm, NS.config, NS.rewired, NS.sw)

# Initialize the data frame to the row names

NS.metrics2 <- data.frame(rnames)
NS.metrics2 

# Sapply each metric to the list of graphs and bind results as columns

NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, vcount)))
NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, ecount)))
NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, mean_distance)))
NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, count_components)))
# Sapply will pass on additional arguments such as type
NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, transitivity, 
                                                    type="global")))
NS.metrics2 <- cbind(NS.metrics2, data.frame(sapply(models, assortativity_degree)))

# Column names default to the expressions used to make them, ... 

NS.metrics2 

# ... which is ugly, so let's assign our list of column names

colnames(NS.metrics2) <- cnames
NS.metrics2
NS.metrics # compare: outcomes are identical 

######################################################################
# Tibbles 
######################################################################
# The 'Tidyverse' approach to data analytics uses enhanced data frames
# called 'tibbles'. They avoid some quirks of data frames. They don't
# convert strings to factors, and have enhanced operators.

# The book: "R for Data Science" https://r4ds.had.co.nz is an
# excellent introduction to data science with the Tidyverse. 

# install.packages('tibble') # if needed 

library('tibble')
?tibble

# Tibbles make it easy to make named columns. Each row below will
# become a column. Assuming we have already created the models 
# above and models <- list(NS, NS.gnm, NS.config, NS.rewired, NS.sw)

NS.metrics3 <- tibble(
  Model         = c("NetScience", "G(n,m)", "Config", "Rewire", "SmallW"), 
  V             = sapply(models, vcount), 
  E             = sapply(models, ecount),
  meandist      = sapply(models, mean_distance),
  components    = sapply(models, count_components), 
  transitivity  = sapply(models, transitivity, type="global"), 
  degreeassort  = sapply(models, assortativity_degree)
)

# Notice the differences in how the values are displayed. Be cautious 
# as tibble printing may round (for example) .000000001 to 0.0000

NS.metrics3
NS.metrics

# Tibbles have specialized print functions that avoid filling the screen. 
# The values appear to be different, but accessing the rows directly: 

NS.metrics$degreeassort
NS.metrics3$degreeassort

######################################################################
# Pau