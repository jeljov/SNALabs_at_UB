############################################################################
# This R script is partially based on the 'LAB 2: Methodological beginnings'
# from the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=2)
############################################################################


##############################################################
# 
# LAB 3
#
# The objective of this lab is to introduce measures of 
# connectivity and cohesion, including:
# - reachability, shortest path, diameter
# - density, reciprocity, transitivity (clustering coef.)
# - k-cores
# - triadic analyses
# It also covers basic measures of homophily and heterogenity.
#
##############################################################


### 
# 1. SET UP A SESSION
###

# Load the required libraries
library(igraph)
library(ggplot2)
library(tidyr)

# We will also use visNetwork R package to create interactive
# graph plots. For a brief introduction to visNetwork, see Section 6.2 
# of the 'Network visualization with R' tutorial, available at:
# http://kateto.net/network-visualization
# We will cover this visualization package in more detail in Lab 5.

# install.packages('visNetwork')
library(visNetwork)


# Load the data, that is, the networks we have created in Lab 1
data_dir = "output/lab1/"

# Start with the network with all kinds of ties
krack_full <- readRDS(paste0(data_dir, "krack_full.RData"))
summary(krack_full)

# Then, load sub-graphs, one for each tie type
krack_advice <- readRDS(paste0(data_dir, "krack_advice.RData"))
summary(krack_advice)

krack_friendship <- readRDS(paste0(data_dir, "krack_friendship.RData"))
summary(krack_friendship)

krack_reports_to <- readRDS(paste0(data_dir, "krack_reports_to.RData"))
summary(krack_reports_to)


### 
# 2. MEASURES OF CONNECTIVITY AND COHESION
###
  
#
# 2.1 Reachability
#

# The subcomponent function allows us to compute reachability for 
# an individual node, for example:
subcomponent(krack_advice, 1, mode = 'out')
subcomponent(krack_reports_to, 1, mode = 'out')
subcomponent(krack_reports_to, 1, mode = 'in')
# Note that the subcomponent function returns a vector of ids
# of vertices that are in the same component as the given vertex;
# so, the result includes the vertex itself.

# To get graph-wide statistics, we need a loop that will produce
# a matrix with reachability indicators (0/1) for each pair of 
# nodes in the graph

reachability <- function(g, m) {
	reach_mat = matrix(nrow = vcount(g), ncol = vcount(g))
	for (i in 1:vcount(g)) {
		reach_mat[i,] = 0
		this_node_reach <- subcomponent(g, i, mode = m)  
		for (alter in this_node_reach) 
		 	reach_mat[i, alter] = 1
	}
	return(reach_mat)
}

# Let's compare the advice and reports-to networks with
# respect to reachability

# Compute reachability matrices for the advice network,
# first for incoming links, than for outgoing edges:
reach_advice_in <- reachability(krack_advice, 'in')
View(reach_advice_in)
# It seems that in the advice network we have a perfect reachability.
# Let's check:
all(reach_advice_in == 1)
reach_advice_out <- reachability(krack_advice, 'out')
View(reach_advice_out)
all(reach_advice_out == 1)
# So, in advice network, each node is reachable from any other node

# If our graph was larger, it would be difficult to determine the level of 
# reachability by observing the reachability matrix. 
# Instead, we can compute, for each node in a graph, the number of other 
# nodes reachable by the given node:
apply(reach_advice_in, 1, function(x) sum(x==1))
apply(reach_advice_out, 1, function(x) sum(x==1))
# As we saw above, every vertex is reachable from any other vertex

# Now, the same but for the reports-to network
reach_reports_to_in <- reachability(krack_reports_to, 'in')
View(reach_reports_to_in)
reach_reports_to_out <- reachability(krack_reports_to, 'out')
View(reach_reports_to_out)
# Far from the level of reachability observed in the advice network.
apply(reach_reports_to_in, 1, function(x) sum(x==1))
apply(reach_reports_to_out, 1, function(x) sum(x==1))
# In general, very low reachability; only one - the central node - is 
# reachable by all other nodes

# To better understand these results, we can visualize the network.
# In particular, we will use an interactive visualization that will
# allow us to focus on those vertices we are interested in.

# First, prepare the graph data in the format required by visNetwork.
# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)
reports_to_nodes <- data.frame(id=V(krack_reports_to)$name, stringsAsFactors = FALSE)
reports_to_edges <- data.frame(as_edgelist(krack_reports_to), stringsAsFactors = FALSE)
colnames(reports_to_edges) <- c('from', 'to')
# add arrows to display edge direction:
reports_to_edges$arrows <- "to"
# add labels to nodes
reports_to_nodes$label <- reports_to_nodes$id
# Finally, display the network
visNetwork(nodes = reports_to_nodes, edges = reports_to_edges, 
           main="Reports-to network")


# An average of reach for a network reveals what percent of the network 
# is connected in some way.
# For the advice network, we saw that there is a connection between 
# each pair of nodes in the network. However, in the reports-to network
# the level of reach is far lower:
sum(reach_reports_to_in)/(vcount(krack_reports_to)^2)
sum(reach_reports_to_out)/(vcount(krack_reports_to)^2)
# Only ~13% of the potential pairwise connections were realized


#
# 2.2 Shortest path (geodesic) and diameter
#

# Often we want to know path distances between actors in a network. 
# This is typically done by calculating geodesics, or shortest paths between
# each actor (node) pair. 
# By averaging geodesics for the entire network we get average distance in
# the network, which is a sort of cohesiveness score. 

# For example, we can compute shortest paths between each pair of nodes in
# the friendship network.
# First, let's compute shortest paths to each vertex
sp_friendship_in <- distances(krack_friendship, mode='in')
View(sp_friendship_in)
# Next, shortest paths from each vertex
sp_friendship_out <- distances(krack_friendship, mode='out')
View(sp_friendship_out)

# Inf values mean no connection between the corresponding two verices.
# Replace Inf values with NA
sp_friendship_in[sp_friendship_in==Inf] <- NA
sp_friendship_out[sp_friendship_out==Inf] <- NA

# Then, we can compute average shortest path for each node in the 
# friendship network
mean_sp_friendship_in = vector()
mean_sp_friendship_out = vector()
for(i in 1:vcount(krack_friendship)) {
  mean_sp_friendship_in[i] <- mean(sp_friendship_in[i,], na.rm = TRUE)
  mean_sp_friendship_out[i] <- mean(sp_friendship_out[i,], na.rm = TRUE)
}
summary(mean_sp_friendship_in)
summary(mean_sp_friendship_out)


# Let's visualize these values to better understand them
mean_sp_friendship_df <- data.frame(node_id=as.integer(V(krack_friendship)$name),
                                    sp_in = mean_sp_friendship_in,
                                    sp_out = mean_sp_friendship_out)
mean_sp_friendship_df_long <- gather(mean_sp_friendship_df, 
                                     key = "Mode", value = "SP",
                                     ... = sp_in:sp_out, factor_key = TRUE)
head(mean_sp_friendship_df_long)
ggplot(data = mean_sp_friendship_df_long,
       mapping = aes(x = node_id, y = SP, fill=Mode)) +
  geom_col(position = 'dodge') +
  scale_fill_discrete(name='Kind of shortest path (SP)',
                      breaks=c('sp_in', 'sp_out'),
                      labels=c('SP to the vertex', 'SP from the vertex')) +
  labs(x = 'Node ID', y = "Average Shortest Path") +
  scale_x_continuous(breaks = seq(1,21,1)) +
  theme_bw() +
  theme(legend.position = 'bottom') 


# Another way to examine the computed average shortest paths (SP) to and from nodes
# is to plot the friendship network as a graph, using node and label size to 
# represent mean SP from a node, and node color to represent mean SP towards 
# the node:
source('SNA_custom_functions.R')
sp_col_palette = attr_based_color_gradient(mean_sp_friendship_in, 
                                           c('gold','red')) 
plot(krack_friendship, 
     layout=layout_with_kk(krack_friendship), 
     vertex.color=sp_col_palette, 
     edge.arrow.size=.30,
     vertex.size=10*mean_sp_friendship_out,
     vertex.label.cex = 0.5*mean_sp_friendship_out,
     main="Friendship network\n (node color denotes mean SP towards a vertex, 
     size denotes mean SP from a vertex)")


# Let's now compute the diameter for the friendship network, that is, the 
# longest shortest path in the network:
diameter(krack_friendship)

# The notable difference between the value of the diameter (=5) and the average 
# shortest paths, suggest that we have some peripheral nodes in the network. 
# To get the nodes that are most apart, we can use the farthest_vertices() f.
farthest_vertices(krack_friendship)
# Take the vertices that are most apart:
most_apart <- farthest_vertices(krack_friendship)$vertices

# Get the shortest path (sequence of edges) that connect the two vertices
# that are most apart:
most_apart_path <- shortest_paths(krack_friendship, 
                                  from = most_apart[1], 
                                  to = most_apart[2], 
                                  output = 'epath')
most_apart_path
most_apart_path <- most_apart_path$epath[[1]]

# Let's plot the two most apart nodes and the shortest path that 
# connects them 
# Define node color so that all the nodes are gold except the 
# two most distant ones which will be painted in red 
node_colors <- rep('gold', times=vcount(krack_friendship))
node_colors[most_apart_path] <- 'red'
# Define edge color so that all edges are grey except those that
# connect the two most distant nodes - these will be red
edge_colors <- rep('grey', times=ecount(krack_friendship))
edge_colors[most_apart_path] <- 'red'
# Define edge weight (thickness) so that the weight of all edges 
# is 1 except those that connect the two most distant nodes - their
# weight will be 3.5
edge_width <- rep(1, times=ecount(krack_friendship))
edge_width[most_apart_path] <- 3.5
# Now, plot the network
plot(krack_friendship, 
     layout=layout_with_kk(krack_friendship), 
     vertex.color=node_colors, 
     edge.color=edge_colors,
     edge.width=edge_width,
     edge.arrow.size=.30,
     main="The longest geodesic in the friendship network")


##
# TASK: do the same kind of analysis for the advice network
##


#
# 2.3 Density 
#

# The density of a graph is the ratio of the number of 
# edges in the graph and the number of possible edges
edge_density(krack_full)
edge_density(krack_advice)
edge_density(krack_friendship)
edge_density(krack_reports_to)

# Observe the difference in the density of the four
# examined networks

#
# 2.4 Reciprocity
#

# Reciprocity represents the proportion of mutual connections
# in a directed graph. To compute it, we can use the function that
# computes dyad census, that is, the number of dyads (node pairs) with:
# - mutual connections ($mut)
# - non-mutual connections ($asym)
# - no connection at all ($null)
dc_friendship <- dyad_census(krack_friendship)
# To get reciprocity:
(2*dc_friendship$mut)/ecount(krack_friendship)

# Note: since dc_friendship$mut is the number of node pairs with
# mutual connections, we multiply it by 2 to get the number of 
# connections.

# We can also use the reciprocity function:
reciprocity(krack_friendship)

# Reciprocity is also defined as the probability 
# that if A is connected to B, then B will be connected to A.
# In this interpretation, reciprocity is the following ratio:
dc_friendship$mut/(dc_friendship$mut + dc_friendship$asym)
# It can be also computed using the reciprocity f. with the
# mode parameter set to 'ratio':
reciprocity(krack_friendship, mode = 'ratio')

# Compare the four networks with respect to reciprocity:
reciprocity(krack_full)
reciprocity(krack_advice)
reciprocity(krack_friendship)
reciprocity(krack_reports_to)


#
# 2.5 Transitivity
#

# Transitivity implies that, if A is connected to B, and B is connected to C, 
# then A is connected to C, as well. In real networks, perfect transitivity is
# rarely present, partial transitivity is more typical: the fact that A is 
# connected to B and B is connected to C does not guarantee that A will be 
# related to C, but makes it much more likely; for example, the friend of my 
# friend is not necessarily my friend, but is far more likely to be my friend 
# than a randomly chosen member of the population.

# Transitivity is also known as the clustering coefficient. It can be:
# - global - refers to the graph as a whole
# - local - refers to an individual vertex

transitivity(krack_full)
transitivity(krack_advice)
mean(transitivity(krack_friendship, type = 'local'))
transitivity(krack_friendship, type = 'global')
transitivity(krack_reports_to)
# Note: the transitivity function treats the input graph as undirected.
# To determine transitiviy (clustering coefficient) of a directed graph,
# one has to compute triad census, which we'll do next.

#
# 2.6 Triad census
#

# Triad census is the number of different configurations of 3 vertices
# (triples) in a graph.
# See the figures in the 'resources' folder illustrating those different
# configurations and their labels - those are standard labels that are used in
# SNA analysis to refer to particular triple configurations.

# We'll first build a vector of labels for different triad types. 
# Then we'll combine this vector with the triad censuses for the different 
# networks, which we'll export as a CSV.

census_labels = c('003',
                  '012',
                  '102',
                  '021D',
                  '021U',
                  '021C',
                  '111D',
                  '111U',
                  '030T',
                  '030C',
                  '201',
                  '120D',
                  '120U',
                  '120C',
                  '210',
                  '300')

triad_df <- data.frame(labels=census_labels,
                       full=triad.census(krack_full), 
                       advice=triad.census(krack_advice), 
                       friendship=triad.census(krack_friendship),
                       reports_to=triad.census(krack_reports_to))
triad_df

# To export any of these vectors to a CSV for use in another program, simply
# use the write.csv() command:
write.csv(triad_df, 'output/lab3/krack_triad_census.csv')

##
# TASK: 
# (a) Identify the most dominant triad forms in each of the 4 examined networks. 
# (b) What do the identified triads tell us (for each network)? In other words, 
#     try to interpret the identified triad forms in the context of each network.
# (c) How do the networks differ with respect to their triad census?
##


#
# 2.7 K-cores
#

# The k-core is the maximal subgraph in which every node has degree of at least k. 
# A node has coreness M if it belongs to a M-core but not to (M+1)-core.

# The coreness function computes the coreness of each vertex in the network:
friend_core_in <- coreness(krack_friendship, mode = 'in')
friend_core_in
table(friend_core_in)
friend_core_out <- coreness(krack_friendship, mode = 'out')
friend_core_out
table(friend_core_out)

# To better understand the coreness results, let's visualize them

# We will use a palette of colors from the *RColorBrewer* R package
library(RColorBrewer)
# We will need as many colors in the palette as there are different 
# in-coreness values
display.brewer.pal(n = length(unique(friend_core_in)), name = 'Blues')
core_colors <- brewer.pal(n = length(unique(friend_core_in)), name = 'Blues')
# Now, use the created palette to color nodes based on their in-coreness, 
# that is, their coreness based on their incoming edges; we will use out-coreness
# (coreness based on outgoing edges to determine the size of nodes and their labels)
plot(krack_friendship,
     layout = layout_with_kk(krack_friendship),
     vertex.color = core_colors[friend_core_in],
     vertex.size = friend_core_out * 10,
     vertex.label.cex = friend_core_out * 0.75,
     edge.arrow.size = 0.2,
     main="Friendship network\n (node color denotes in-coreness, size denotes out-coreness)")


##
# TASK: 
# Compute in- and out-coreness for the advice and reports_to networks,
# and compare the three networks based on the level of actors' grouping 
# that is reflected through the corness scores.
##


### 
# 3. HOMOPHILY AND HETEROGENEITY 
###

#
# 3.1 Homophily / Assortativity
#

# Homophily - also known as Assortativity or Assortative Mixing - can be defined 
# as the tendency of actors in a ntwork to connect to other actors that are similar 
# to them in some way ("birds of feather flock together"). 
# Assortativity can be based on any attribute of a node, but often it is assortativity
# based on nodes' degree (number of direct connections) that is examined.

# The function computes assortativity in the given network based on 
# age, department, tenure, and degree
compute_assortitivity <- function(g) {
  result = vector(mode = 'numeric', length = 4)
  result[1] <- assortativity(g, types1 = V(g)$age)
  result[2] <- assortativity(g, types1 = V(g)$dept)
  result[3] <- assortativity(g, types1 = V(g)$tenure)
  result[4] <- assortativity_degree(g)
  names(result) <- c('age', 'department', 'tenure', 'degree')
  result
}

# Examine assortativity for the three kinds of networks 
compute_assortitivity(krack_advice)
compute_assortitivity(krack_friendship)
compute_assortitivity(krack_reports_to)
# Observe the differences in the attributes that result in 
# higher assoratativity values


#
# 3.2 Index of Qualitative Variation (IQV)
# 

# Another way to examine the presence of homophily in a network is
# to estimate the extent to which an actor's "associates" 
# (friend, advisor, manager) are heterogenous.
# To that end, we'll use a statistic called Index of Qualitative
# Variation (IQV). This is an implementation of Blau's Index of
# Heterogeneity, normalized so that perfect heterogeneity equals 1.

# We will use a function, get_iqvs, that takes a graph (the 1st input
# parameter) and computes the IQV statistic for the categorical variable 
# given as the function's 2nd input parameter. 
# NOTE: this function assumes that categorical variables have been 
# numerically coded to integer values that ascend sequentially from 0.
# The function is defined in the SNA_custom_functions script.
source('SNA_custom_functions.R')

# For this data set, we'll look at homophily across departments, 
# which is already coded 0-4, so no recoding is needed. 

advice_iqvs <- get_iqvs(krack_advice, 'dept')
summary(advice_iqvs)
# Mostly high values for IQV suggesting that employees are often connected, 
# through advice relation, to colleagues from other departments

friendship_iqvs <- get_iqvs(krack_friendship, 'dept')
summary(friendship_iqvs)
# The diversity is present but on a lower level than in the case of the 
# advice network

# Note: NA values in the results are due to the nodes not having
# alters (direct ties); this can be verified by examining a graph's 
# adjecency matrix, e.g.:
as_adjacency_matrix(krack_friendship)
# note that nodes 7 and 9 have empty rows, that is, no recorded 
# connection

reports_to_iqvs <- get_iqvs(krack_reports_to, 'dept')
summary(reports_to_iqvs)
# According to the IQV values, there is a perfect homogeneity of
# reports-to connection with respect to the department attribute,
# that is, connections tend to be almost always within the department.

# Now let's color-code vertices by department and compare the
# obtained IQV values with the department-colored plots 

# First, choose a qualitative palette so that we have one color for each department 
display.brewer.pal(n = length(unique(V(krack_advice)$dept)), name = 'Set1')
dept_colors <- brewer.pal(n = length(unique(V(krack_advice)$dept)), name = 'Set1')

# Then, use the palette to create plots
plot(krack_advice, 
     layout=layout_with_kk(krack_advice), 
     vertex.color=dept_colors[V(krack_advice)$dept + 1], 
     edge.arrow.size=.25,
     main="Advice network\n(node color denotes department)")

plot(krack_reports_to, 
     layout=layout_nicely(krack_reports_to), 
     vertex.color=dept_colors[V(krack_reports_to)$dept + 1], 
     edge.arrow.size=.25,
     main="Reports-to network\n(node color denotes department)")


##
# TASK: 
# Numeric attributes can be discretized and thus turned into factors (that is,
# qualitative variables), so that the get_iqvs() function can be applied. 
# Try to compute and interpret IQV for the (discretized) 'age' attribute.
# To discretize values of a numeric attribute, you can use the cut() f.
?cut
# for an example, see this StackOverflow answer:
# https://stackoverflow.com/questions/22075592/creating-category-variables-from-numerical-variable-in-r

