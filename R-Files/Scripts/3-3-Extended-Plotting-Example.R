######################################################################
# 2-5 Extended Visualization Example: French Political Blogs
# Selections from SAND visualization demo with elaborations. Shows how
# to transform a network and using basic igraph visualization
# parameters. We learn a little R along the way.
#
# Dan Suthers, January 27, 2017
# July 24, 2018: Split this off from script 2-3
# Sep 10 2019 DS: minor changes for 2019 
# Jan 28 2021 DS: minor updates for 2021 422/622 
# Jan 27 2022 DS: Updating for spring 2022 class: formatting only. 
#                 ~21 minutes + 3 minutes = 24 minutes 
######################################################################

library(igraph)
setwd("~/Desktop/Network-Science-Demos") # Set to your location

######################################################################
# French Political Blog 

# In this network, nodes are blogs and edges are references between
# blogs. Attribute PolParty indicates the party the blog promotes.
# Suppose we want to see references between political parties as a
# whole (rather than individual blogs). The blog graph is too
# detailed: we need to collapse all nodes of a given party into a
# single node.

# This example is from SAND 3.5 Visualizing Large Graphs, but has been
# upgraded for current igraph. There will be similar homework.
######################################################################
fblog <- read_graph("Networks/French-Political-Blogs.graphml", 
                    format="graphml")
summary(fblog)

# Collapse all the party names into a set of unique names. 

unique(V(fblog)$PolParty)

# Let's sort and save that (() around the expression prints the result)

(party_name <- sort(unique(V(fblog)$PolParty)))

# We need to code the parties as numbers to give as color argument. 

# Here is a brief demonstration of factors and numeric. If we try to
# convert to as.numeric without as.factor we get all NA, because
# strings are nominal data and have no ordering.

head(V(fblog)$PolParty, 20)             # The values to be coded
head(as.numeric(V(fblog)$PolParty), 20) # Can't code strings as numeric 

# R uses "factors" to record possible values and give them an ordering.

head(as.factor(V(fblog)$PolParty), 20)  # Convert to factor 

# Notice the Levels at the bottom of the above output. These are
# used to generate the numeric coding, e.g., "Les Verts" is #3: 

(party_numbers <- as.numeric(as.factor(V(fblog)$PolParty)))

# Now we can plot using party_numbers for color coding.

plot(fblog, main = "French Political Blogs",
     vertex.label = NA, 
     vertex.color = party_numbers, 
     vertex.size = 5)

# That is rather dense: what if we want to see what is happening by
# party? Use 'contract' to collapse vertices into one node for each
# party ...

?contract 
party <- contract(fblog, party_numbers)
summary(party) 

# There are 9 parties but all the original edges are still there!  
# It is not simple because there are multiple edges between any pair
# of party nodes, since we collapsed all blogs under each party into a
# single node, taking the edges along with them.

is_simple(party)

# We can set a layout and then re-use it. This is useful if we want to
# compare different visualizations without the nodes jumping all over
# the place, and to save and recreate a layout we like.

party.lfr <- layout_with_fr(party)

# Apply that layout to the collapsed party graph. The plot will have
# many redundant edges (slow)

plot(party, main = "Party Graph v1", 
     layout = party.lfr,
     vertex.label = NA, 
     vertex.color = party_numbers, 
     vertex.size = 5)

# We want to combine the multiple edges between parties into a single
# link between each pair, weighting each combined link by the number
# of links that went between parties in the former graph. Here's how:

summary(party)
E(party)$weight             # there are no weights yet 
E(party)$weight <- 1        # count each link once when we simplify
?simplify                   # see attribute.combination for default
party <- simplify(party)    # combines multiedges between parties 
summary(party)              # now has 25 edges and a weight attribute
is_simple(party)            # and it's simple
E(party)$weight             # now we have nontrivial weights

# Now the edges are collapsed. Use same layout: 

plot(party, main = "Party Graph v2", 
     layout = party.lfr,
     vertex.label = NA, 
     vertex.color = party_numbers, 
     vertex.size = 5)

# ***** Look at the last 3 plots. What information have we lost in
#       this visualization? Can we restore some of it?

# We will use party sizes for vertex size, so count the number of 
# nodes in each party in the original graph using table and 
# change this into an ordinary vector rather than table:

table(V(fblog)$PolParty)             # just to illustrate 'table' 
(party_size <- as.vector(table(V(fblog)$PolParty)))

# Edge weight is number of cross-party connections, which we can
# visualize with thicker edges for more connections.

# Plot, with attributes as commented below. Note use of sqrt to keep
# extreme values from generating large graphics. I've added some other
# controls as well to illustrate them.

plot(party, main = "Party Graph v3 with FR layout", 
   layout = party.lfr,                  # keep same layout as before
   vertex.size = 5*sqrt(party_size),    # scale vertex size to party size 
   vertex.label = party_name,           # label with their names 
   vertex.label.cex = 0.7,              # a bit smaller 
   vertex.label.color="black",          # was blue
   vertex.label.family=NA,              # was "serif", this turns it off
   vertex.label.font=2,                 # bold
   vertex.label.dist = 0.0,             # sets offset of label
   vertex.color = V(party),             # each party gets its own color 
   edge.width = sqrt(E(party)$weight),  # line thickness shows edge weight
   edge.arrow.size = 0                  # no longer directed 
   )

# Flip back through the screens to see what we did! 

# One can tweak this by adjusting the label distance and size. 
# Since you specified the layout it should stay the same. 

# Or try other layouts, such as 

party.lkk <- layout_with_kk(party)
plot(party, main = "Party Graph v4 with KK layout", 
   layout = party.lkk, # I changed the layout 
   vertex.size = 5*sqrt(party_size),    # scale vertex size to party size 
   vertex.label = party_name,           # label with their names 
   vertex.label.cex = 0.7,              # a bit smaller 
   vertex.label.color="black",          # was blue
   vertex.label.family=NA,              # was "serif", this turns it off
   vertex.label.font=2,                 # bold
   vertex.label.dist = 0.0,             # sets offset of label
   vertex.color = V(party),             # each party gets its own color 
   edge.width = sqrt(E(party)$weight),  # line thickness shows edge weight
   edge.arrow.size = 0                  # no longer directed 
   )

# ***** Home Activity: experiment with the parameters! 

# Then try writing this graph out to visualize in Gephi. Since some of
# the data is in separate vectors we need to add them to the graph.

V(party)$party_name <- party_name
V(party)$party_size <- party_size
summary(party)
write_graph(party, "French-Party-Blogs.graphml", format="graphml")

# ***** Activity: Check that in Gephi 

######################################################################
# If time (3 minutes): Draw colored backgrounds around the parties
######################################################################
# This is an alternative method of grouping. 

# Recall we had previously plotted the uncollapsed graph. Let's do it
# again with a fixed layout: 

fblog.lfr <- layout_with_fr(fblog)
plot(fblog, main = "French Political Blogs Ungrouped", 
     layout = fblog.lfr,
     vertex.label = NA, 
     vertex.color = party_numbers, 
     vertex.size = 3)

# See mark.groups for what I am trying to do here

?plot.igraph 

# Assign each vertex a party number 

V(fblog)$party_num <- party_numbers

# Make a list of lists for party membership (One can do this more 
# elegantly with apply, but this code makes intent explicit.) 

groups <- list(V(fblog)[V(fblog)$party_num==1],
               V(fblog)[V(fblog)$party_num==2], 
               V(fblog)[V(fblog)$party_num==3],
               V(fblog)[V(fblog)$party_num==4],
               V(fblog)[V(fblog)$party_num==5],
               V(fblog)[V(fblog)$party_num==6], 
               V(fblog)[V(fblog)$party_num==7], 
               V(fblog)[V(fblog)$party_num==8],
               V(fblog)[V(fblog)$party_num==9])
               
# Should be a list of lists, of length 9 

length(groups)
summary(groups)

# Give this as argument to mark.groups, and voila! 

plot(fblog, main = "French Political Blogs Grouped", 
     layout = fblog.lfr,
     mark.groups = groups, # This is new 
     vertex.label = NA, 
     vertex.color = party_numbers, 
     vertex.size = 3)
     
# Obviously we need a cleaner separation of nodes in the layout for
# this to work visually. However, you get the idea. With a little 
# work it can be used to make professional looking visualizations. 
     
#################################################################
# Pau 