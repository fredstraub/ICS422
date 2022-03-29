library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to yours 
IR <- read_graph("Networks/internet_routers-22july06.gml", format="gml")
diameter(IR)

system.time(IR.bvalues <- betweenness(IR))
# Run estimates and collect times it took 
times <- c(
  system.time(IR.evalues1 <- estimate_betweenness(IR, cutoff=1))[[3]],
  system.time(IR.evalues2 <- estimate_betweenness(IR, cutoff=2))[[3]],
  system.time(IR.evalues3 <- estimate_betweenness(IR, cutoff=3))[[3]],
  system.time(IR.evalues4 <- estimate_betweenness(IR, cutoff=4))[[3]],
  system.time(IR.evalues5 <- estimate_betweenness(IR, cutoff=5))[[3]],
  system.time(IR.evalues6 <- estimate_betweenness(IR, cutoff=6))[[3]],
  system.time(IR.evalues7 <- estimate_betweenness(IR, cutoff=7))[[3]],
  system.time(IR.evalues8 <- estimate_betweenness(IR, cutoff=8))[[3]],
  system.time(IR.evalues9 <- estimate_betweenness(IR, cutoff=9))[[3]]
)

# Plot the "animation" 
plot(IR.bvalues, IR.evalues1, main="Estimated Betweenness, Cutoff 1")
plot(IR.bvalues, IR.evalues2, main="Estimated Betweenness, Cutoff 2")
plot(IR.bvalues, IR.evalues3, main="Estimated Betweenness, Cutoff 3")
plot(IR.bvalues, IR.evalues4, main="Estimated Betweenness, Cutoff 4")
plot(IR.bvalues, IR.evalues5, main="Estimated Betweenness, Cutoff 5")
plot(IR.bvalues, IR.evalues6, main="Estimated Betweenness, Cutoff 6")
plot(IR.bvalues, IR.evalues7, main="Estimated Betweenness, Cutoff 7")
plot(IR.bvalues, IR.evalues8, main="Estimated Betweenness, Cutoff 8")
plot(IR.bvalues, IR.evalues9, main="Estimated Betweenness, Cutoff 9")

# Compute correlations 
correlations <- c( 
  cor(IR.bvalues, IR.evalues1, method="kendall"), 
  cor(IR.bvalues, IR.evalues2, method="kendall"), 
  cor(IR.bvalues, IR.evalues3, method="kendall"), 
  cor(IR.bvalues, IR.evalues4, method="kendall"), 
  cor(IR.bvalues, IR.evalues5, method="kendall"), 
  cor(IR.bvalues, IR.evalues6, method="kendall"), 
  cor(IR.bvalues, IR.evalues7, method="kendall"), 
  cor(IR.bvalues, IR.evalues8, method="kendall"), 
  cor(IR.bvalues, IR.evalues9, method="kendall")
)

# Plot the time and correlation as a function of cutoff.
# To get them on the same axis, use time / maximum time. 
plot(times/max(times), col="blue", type="b", lwd=3,
     main="Estimate Betweenness", 
     xlab="cutoff", ylab="correlations and time")
lines(correlations, col="red", type="b", lwd=3)
legend("bottomright", bty="n",
       c("time / max(time)", "correlation"), 
       col=c("blue", "red"), 
       lty=1, lwd=3)