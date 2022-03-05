######################################################################
# Preferential Attachment:
# * Demonstration of sample_pa and comparison to powerlaw distribution. 
# * Demonstration of nonlinear preference of attachment. 
# Meant to be run just after reviewing Barabasi Ch. 5 with presentation. 
# Dan Suthers, Sept. 26, 2016
# August 19, 2018 DS: 
# * Minor updates for current script style and clarity. 
# * Set zero.appeal to 0 now that the bug we found next week is fixed. 
# September 30, 2019 DS: 
# * Replaced use of poweRlaw with fit_power_law to simplify course 
# February 15 2020 DS: Minor improvements for self-study students 
# Feb 23 2021 DS: Updates for Spring 2021 ICS 422/622. 
# * Removed zero.appeal as the new implementation changes behavior; 
#   need to investigate further. 
# Feb 22 2022 DS: Updates for Spring 2022 ICS 422/622
# * Restored zero.appeal: now understand it is for undirected only 
# * Shorthand functions quickplot, pa_legend, and plfit_legend
# 
######################################################################
library(igraph)
library(tibble)

setwd("~/Desktop/Network-Science-Demos") # Set to yours 
source("Utility/new_window.R")
source("Utility/degree_domain.R")

# Graph we will work with 

IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")

######################################################################
# Barabasi-Albert Model: a Preferential Attachment Generator 
######################################################################
# Let's read the documentation, considering all the parameters ... 

?sample_pa

# Make a quick example and fit to power law (your results may differ)

spa0 <- sample_pa(100000, m=2)
spa0$name <- paste(spa0$name, "default, m=2") 
summary(spa0)

# Notice that various parameters are recorded. We know what m is but
# what is power?

spa0$power # what power is this? 

plot(degree_domain(spa0), degree_distribution(spa0), 
     log="xy", xlab="k", ylab="p(k)", 
     main=spa0$name)

# Notice that even though we specified degree_domain the plot does not
# start at 1: why? A hint: 

spa0$m

# Does it fit the power law as expected? 

as_tibble(spa0.plfit <- fit_power_law(degree(spa0), 
                                      implementation="plfit"))

# ***** What do xmin and alpha (gamma) tell you? 

# ***** How do you interpret the p value? 

# ***** How does this compare to Barabasi's prediction? 

# Turns out Barabasi's model is for undirected graphs with zero
# appeal. You can't set zero appeal to 0 in a directed graph: 

spa1 <- sample_pa(100000, m=2, zero.appeal=0)

# In fact, we are mixing two very different distributions!!! In-degree: 

plot(degree_domain(spa0, mode="in"), 
     degree_distribution(spa0, mode="in"), 
     log="xy", xlab="k", ylab="p(k)", 
     main=spa0$name)

# How different do you think the outdegree distribution will be? 

plot(degree_domain(spa0, mode="out"), 
     degree_distribution(spa0, mode="out"),  
     log="xy", xlab="k", ylab="p(k)", 
     main=spa0$name)

# Can you explain that??? Hint: same explanation as above. 

# But you can set zero appeal to 0 in an undirected graph. Let's redo
# the first undirected for apples to apples comparison and name them.

spa1 <- sample_pa(100000, m=2, directed=FALSE)
spa2 <- sample_pa(100000, m=2, zero.appeal=0, directed=FALSE)

spa1$name <- paste(spa1$name, "1 with zero.appeal") 
spa2$name <- paste(spa2$name, "2 without zero appeal") 

# Before we compare, some code to make it easier: 

######################################################################
# Utility functions for plotting and annotating with legends 

quickplot <- function(g, main=g$name, cumulative=TRUE) {
        plot(degree_domain(g), 
             degree_distribution(g, cumulative=cumulative), 
             log="xy", xlab="k", ylab="p(k)", 
             main=main) 
}

# Legend for sample_pa characteristics: 

pa_legend <- function(g) {
        legend("topright", bty="n", cex=1.2, 
               c(paste("|V| =", vcount(g)), 
                 paste("|E| =", ecount(g)), 
                 paste("m =", g$m), 
                 paste("z.a =", g$zero.appeal), 
                 paste("power =", g$power), # or alpha 
                 if(is_directed(g)) "directed" else "undirected"
               ))
}

# Legend for fitting any graph to the power law: 

plfit_legend <- function(g) {
        g.plfit <- fit_power_law(degree(g), implementation="plfit")
        legend("bottomleft", bty="n", cex=1.2, 
               c(paste("kmin =", min(degree(g))), 
                 paste("kmax =", max(degree(g))), 
                 paste("plfit xmin =", g.plfit$xmin),
                 paste("plfit gamma = ", round(g.plfit$alpha, digits=2)),
                 paste("plfit KS stat = ", round(g.plfit$KS.stat, digits=3)),
                 paste("plfit KS p = ", round(g.plfit$KS.p, digits=3))
               ))
        
}


######################################################################
# Now compare for the effect of zero.appeal.

# Put the legend on the existing plot 
# RERUN IN DEGREE PLOT HERE! 
pa_legend(spa0)
plfit_legend(spa0)

quickplot(spa1)
pa_legend(spa1)
plfit_legend(spa1)

quickplot(spa2)
pa_legend(spa2)
plfit_legend(spa2)

# The second model is much closer to what Barabasi predicted for the
# BA model.

# How does m (edge density) or graph size affect the fit? 
# Barabasi predicted the distribution is independent of both: 

spa3 <- sample_pa(100000, m=4, directed=FALSE, zero.appeal=0)
spa3$name <- paste(spa3$name, "3") 
quickplot(spa3)
pa_legend(spa3)
plfit_legend(spa3)

spa4 <- sample_pa(1000000, m=2, directed=FALSE, zero.appeal=0)
spa4$name <- paste(spa4$name, "4") 
quickplot(spa4)
pa_legend(spa4)
plfit_legend(spa4)

# Consistent across changes in density and graph size. 

######################################################################
# Try to make a sample_pa match Internet Routers 
######################################################################

IR.pa <- sample_pa(vcount(IR), directed=FALSE, zero.appeal=0)
summary(IR)
summary(IR.pa)   # not enough edges.

IR.pa$m 

# Just one edge was added per node: let's increase it. 
# Each node must add this many edges, which we round to integer: 

ecount(IR)/vcount(IR)

IR.pa <- sample_pa(vcount(IR), directed=FALSE, zero.appeal=0,
                   m=round(ecount(IR)/vcount(IR)))

# Edge match is as close as we will get with integer m:
ecount(IR.pa)
ecount(IR)

# So that is a good starting model for size parameters |V| and |E|.
# Let's fit them both to the power law distribution to get more info. 

IR$name <- "Internet Routers"
IR.pa$name <- "BA Model 1 for IR"

new_window("Internet Routers and BA Model", 12, 6)
par(mfrow=c(1,2))

# Plot as above 
quickplot(IR)
pa_legend(IR) # some are undefined but to get |V| and |E| 
plfit_legend(IR)

# Same for IR.pa 
quickplot(IR.pa)
pa_legend(IR.pa)
plfit_legend(IR.pa)

# The gammas are not the same. BA is close to 3 as expected, but IR is
# much lower. Also IR has higher kmax.

# ***** Why do you expect kmax to be higher when gamma is lower?

# ***** What variation of the BA model leads to larger kmax? 

# ***** What domain process might this correspond to? 


######################################################################
# Nonlinear Preferential Attachment
# Following up on Barabasi's discussion that we expect sublinear 
# preference to lead to exponential and superlinear preference to
# large hubs, let's tweak his alpha parameter (called 'power' here). 
######################################################################

# Make non-linear versions SUB and SUPer preference
IR.pa.sub <- sample_pa(vcount(IR), directed=FALSE, 
                       power=0.8, # sublinear
                       m=2, zero.appeal=0) # to match 
IR.pa.sub$name <- paste(IR.pa.sub$name, "Sublinear")
IR.pa.sup <- sample_pa(vcount(IR), directed=FALSE, 
                       power=1.2, # superlinear 
                       m=2, zero.appeal=0)
IR.pa.sup$name <- paste(IR.pa.sup$name, "Superlinear")

# Plot the nonlinear cases in order to compare
new_window("Nonlinear Preferential Attachment", 12, 6)
par(mfrow=c(1,2))

quickplot(IR.pa.sub)
pa_legend(IR.pa.sub)
plfit_legend(IR.pa.sub)

quickplot(IR.pa.sup)
pa_legend(IR.pa.sup)
plfit_legend(IR.pa.sup)

# ***** Notice differences in kmax and gamma and discuss. 

# The fit is not satisfactory. We'll need to try other methods for
# adjusting gamma on Thursday.

# That completes the basic demonstration, but take some time to 
# experiment. 
                     
######################################################################
# Activity (will do after next demo or at home):
# 1. Try to improve fit of the BA nonlinear attachment model to the 
#    Internet Routers data by adjusting the alpha power. Plot them 
#    side by side with legends. 

new_window("IR PA Model Experiments", 12, 6)
par(mfrow=c(1,2))

IR.pa.sup2 <- sample_pa(vcount(IR), directed=FALSE, 
                       power=1.5, # superlinear 
                       m=2, zero.appeal=0)
IR.pa.sup2$name <- paste(IR.pa.sup2$name, "SUPERlinear")

quickplot(IR.pa.sup2)
pa_legend(IR.pa.sup2)
plfit_legend(IR.pa.sup2)

# After class/on your own: 
# 2. Try more extreme alpha.
# 3. Explore other parameters of sample_pa
######################################################################
# Pau. 