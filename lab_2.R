####################################################################
# This R script is partially based on the 'LAB 4: Centrality' from  
# the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=4)
####################################################################


##############################################################
# 
# LAB 2
#
# The objective of this lab is to introduce graph centrality 
# measures, to examine how they are interrelated, and to learn 
# how to interpret their meaning
#
##############################################################


### 
# 1. SET UP THE SESSION
###

# Install and load the required libraries
library(igraph)
# install.packages('ggplot2')
library(ggplot2)
# install.packages('tidyr')
library(tidyr)
# Note: tidyr is a new package; we will need it for some tasks 
# associated with manipulating data frames

# Load the data, that is, the networks we created in Lab 1
data_dir = "output/lab1/"

# Start by loading the network with all kinds of ties
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
# 2. NODE-LEVEL STATISTICS: CENTRALITY MEASURES
###

#
# 2.1 Degree centrality
#

# Compute indegree and outdegree for each node, first in the 
# full graph (accounting for all tie types) and then in each 
# tie-specific sub-graph
deg_full_in <- degree(krack_full, mode="in") 
deg_full_in
table(deg_full_in)

deg_full_out <- degree(krack_full, mode="out") 
deg_full_out
table(deg_full_out)

# To better appreciate the computed values, we will visualise them

# First, create a data frame that integrates the computed 
# in and out degree measures for all the nodes
deg_full_df <- data.frame(node_id=as.integer(V(krack_full)$name), 
                          in_degree=deg_full_in,
                          out_degree=deg_full_out)
head(deg_full_df)

# Then, transform the data frame from wide to long format suitable
# for plotting; to that end, we'll use the gather() f. from the
# tidyr package
?gather
deg_full_df_long <- gather(data = deg_full_df, 
                           key = 'degree_type', 
                           value = 'degree_value', 
                           in_degree:out_degree,
                           factor_key = TRUE)
head(deg_full_df_long)
tail(deg_full_df_long)

# Finally, create a plot 
ggplot(data = deg_full_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  labs(x = "Node ID", y = "In- and Out-Degrees") +
  scale_x_continuous(breaks = seq(0,21,1)) +
  theme_bw()

# We can also use plots to examine degree distribution
ggplot(data = deg_full_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  # geom_histogram(bins = 15, position = 'dodge') +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  theme_bw()


# Now, compute the indegree and outdegree for each node in each 
# tie-specific sub-graph and store computed values in corresponding
# data frames

deg_advice_df = data.frame(node_id = as.integer(V(krack_advice)$name),
                           in_degree = degree(krack_advice, mode="in"),
                           out_degree = degree(krack_advice, mode="out"))

deg_friendship_df = data.frame(node_id = as.integer(V(krack_friendship)$name),
                               in_degree = degree(krack_friendship, mode="in"),
                               out_degree = degree(krack_friendship, mode="out"))

deg_reports_to_df = data.frame(node_id = as.integer(V(krack_reports_to)$name),
                               in_degree = degree(krack_reports_to, mode="in"),
                               out_degree = degree(krack_reports_to, mode="out"))

# Let's examine more closely in and out degrees in the reports-to network
deg_reports_to_df_long = gather(data = deg_reports_to_df, 
                                key = 'degree_type', 
                                value = 'degree_value', 
                                in_degree:out_degree, 
                                factor_key = TRUE)

ggplot(deg_reports_to_df_long, 
       mapping = aes(x=node_id, y=degree_value, fill=degree_type)) +
  geom_col(position = 'dodge') +
  scale_x_continuous(breaks = seq(0,21,1)) +
  labs(x = "Node ID", title = "In and out degree in the reports-to network") +
  theme_bw()

# Degree distribution in the reports-to network 
ggplot(data = deg_reports_to_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  labs(x = "Degree", title = "Degree distribution in the reports-to network") +
  theme_bw()


##
# TASK:
#
# Create visualizations for the degree distribution for the other two networks 
# (advice and friendship) and compare them to the degree distribution of the 
# reports-to network. Note down your observations.
##  


#
# 2.2 Closeness centrality 
#

# We'll start with an undirected network, as it is somewhat easier to deal with.
# Let's assume that all relations in the friendship network are reciprocal
# so that we can transform the friendship network into an undirected
# network; we will keep the friendship_tie edge attribute and drop the other two
# attributes
summary(krack_friendship)
krack_friendship_undirect <- as.undirected(krack_friendship, mode = "collapse",
                                           edge.attr.comb = list(friendship_tie='sum', 'ignore'))
summary(krack_friendship_undirect)
table(E(krack_friendship_undirect)$friendship_tie)

# Before computing closeness, let's check if the network is connected.
# A network is connected if there is a path between any pair of nodes
# in the network.
is_connected(krack_friendship_undirect)

closeness_friend_undirect <- closeness(krack_friendship_undirect, normalized = TRUE)
summary(closeness_friend_undirect)

# We can also include edge attributes (weights) in the calculation of closeness.
# It is important to note that edge "weights are used for calculating weighted 
# shortest paths, so they are interpreted as distances".
# In our case higher values for the friendship_tie attribute mean closer relations,
# that is, lower distance. So, to appropriately calculate weighted closeness, it is 
# better to take reciprocal value of the friendship_tie attribute:
cl_weighted_friend_undirect <- closeness(krack_friendship_undirect, normalized = TRUE,
                                         weights = 1/E(krack_friendship_undirect)$friendship_tie)
summary(cl_weighted_friend_undirect)

# To better appreciate the computed metrics, let's visualise them.
# In particular, let's plot a graph with closeness determining the node color 
# and degree representing through the node size.

# To create a color palette based on the computed closeness values, we will use 
# the attr_based_color_gradient() function from the SNA_custom_functions R script
source('SNA_custom_functions.R')
# The function creates a gradient color vector with as many color gradients as 
# there are different values in the given attribute 
closeness_colors = attr_based_color_gradient(g_attr = closeness_friend_undirect, 
                                             pal_end_points = c('steelblue2', 'gold'))
cl_weighted_colors = attr_based_color_gradient(g_attr = cl_weighted_friend_undirect, 
                                             pal_end_points = c('steelblue2','red'))

# Note: 
# to select colors and find their names in R color pallets, you can use the
# following R Colors cheatsheet:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

# Now, we can make a plot as we did in Lab 1
plot(krack_friendship_undirect, 
     layout=layout_with_kk(krack_friendship_undirect), 
     vertex.color = closeness_colors, #cl_weighted_colors,
     vertex.size = 1.5 * degree(krack_friendship_undirect),
     vertex.label.cex	= 0.15 * degree(krack_friendship_undirect),
     main="Uniderected friendship network\n
     (node color denotes closeness, size denotes degree)")



# Now, we move (go back) to directed network and compute in-closeness and 
# out-closeness centrality.
# You can think of in-closeness centrality as the average number of steps 
# one would have to go through to get TO a given node FROM all other 
# reachable nodes in the network. Out-closeness centrality, not surprisingly, 
# measures the same thing with the directionality reversed: the average
# number of steps FROM the given node TO any other reachable node in the 
# network.

# First, check if the network is connected.
# Note: in directed networks, we need to differentiate between two modes
# of connectedness: 'weak' and 'strong'. The 'weak' form does not consider
# edge direction, whereas the 'strong' mode does consider the direction of
# edges 
is.connected(krack_friendship, mode='strong')
# not connected -> cannot compute closeness; so, we need to find the 
# giant component (= the largest connected component in the graph)
fr_components <- components(krack_friendship, mode = 'strong')
str(fr_components)
fr_components$membership
# two nodes outside the giant component; get ids of those nodes
not_in_gc <- names(fr_components$membership[fr_components$membership!=1])
# create the giant component by removing these two nodes
friendship_gc <- delete.vertices(krack_friendship, not_in_gc)
summary(friendship_gc)
is.connected(friendship_gc, mode='strong')
# Now that we have a connected friendship (sub)graph, we can compute in- and
# out-closeness
friendship_closeness = data.frame(node_id=as.integer(V(friendship_gc)$name),
                                  in_cl=closeness(friendship_gc, mode = 'in', normalized = TRUE),
                                  out_cl=closeness(friendship_gc, mode = 'out', normalized = TRUE))
str(friendship_closeness)

# Let's visualise these measures using node and label size to represent in-closeness,
# and node color to represent out-closeness
out_closeness_colors = attr_based_color_gradient(friendship_closeness$out_cl, c('steelblue3','gold'))
plot(friendship_gc, 
     layout=layout_with_kk(friendship_gc), 
     vertex.color=out_closeness_colors, 
     vertex.size=friendship_closeness$in_cl*30,
     vertex.label.cex=friendship_closeness$in_cl*2, 
     edge.arrow.size=.3,
     main="Giant component of the Friendship network\n
            (node color denotes out-closeness, size denotes in-closeness)")

# It seems that those with high in-closeness have low out-closeness and vice versa;
# We will check that later by computing correlations of centrality measures.

###
# TASK:  
# 
# Do the same kinds of computations and visualizations for the other two networks 
# (advice and reports-to) and compare them to the results obtained for the 
# friendship network. Note down your observations.
###


#
# 2.3 Betweenness centrality
#

# Betweenness centrality measures the number of shortest paths
# going through a specific vertex.
?betweenness
summary(betweenness(krack_full))

# Compute betweeness for all the graphs and store them in a data frame
# Since we will be interested in comparing betweenness across the networks, 
# it is better to normalize the computed values
krack_betweenness_df = data.frame(node_id=as.integer(V(krack_full)$name),
                                  advice=betweenness(krack_advice, normalized = TRUE),
                                  friendship=betweenness(krack_friendship, normalized = TRUE),
                                  reports_to=betweenness(krack_reports_to, normalized = TRUE))
krack_betweenness_df

# Identify the node with the highest betweenness in the advice network:
max(krack_betweenness_df$advice)
which(krack_betweenness_df$advice == max(krack_betweenness_df$advice))

# Identify nodes with the highest betweenness in each network:
apply(krack_betweenness_df[,-1], 2, function(x) which(x==max(x)))
# Interestingly, no overlap across the networks

# Let's visualise one of the networks using node color to represent betweeness
# and node size to represent in-degree 
betweenness_colors = attr_based_color_gradient(krack_betweenness_df$advice, c('steelblue3','gold'))
plot(krack_advice, 
     layout=layout_with_kk(krack_advice), 
     vertex.color=betweenness_colors, 
     vertex.size=1.5 * deg_advice_df$in_degree,
     edge.arrow.size=.30,
     main="Advice network\n
            (node color denotes betweenness, size denotes in-degree)")


###
# TASK:  
# 
# Create similar kind of visualization for the other two networks 
# (friendship and reports-to) and compare them to the advice 
# network. Consider combining betweenness with some other centrality 
# metrics to examine their mutual relation.
###


#
# 2.4 Eigenvector centrality
# 

# Eigenvector centrality gives higher scores to nodes the more they 
# are connected to other highly connected nodes. 
# It is often interpreted as a measure of a node's network importance.

?eigen_centrality

# We'll examine first undirected network of friendship ties
eigen_friend_undirect <- eigen_centrality(krack_friendship_undirect)
str(eigen_friend_undirect)
# Note that we are only interested in the $vector element of the object
# returned by the eigen_centrality() function
eigen_friend_undirect <- eigen_centrality(krack_friendship_undirect)$vector
summary(eigen_friend_undirect)

# We can also compute weighted Eigenvector centrality.
# Note that in this case, weights are interpreted as the reflection of the 
# connection strength: "higher weights spread the centrality better"
w_eigen_friend_undirect <- eigen_centrality(krack_friendship_undirect, 
                                    weights = E(krack_friendship_undirect)$friendship_tie)
w_eigen_friend_undirect <- w_eigen_friend_undirect$vector
summary(w_eigen_friend_undirect)

# Plot the undirected friendship graph with eigenvector centrality represented 
# through the nodes' color, and degree centrality through the nodes' size
eigen_col = attr_based_color_gradient(eigen_friend_undirect, c('steelblue3', 'red'))
# eigen_col = attr_based_color_gradient(w_eigen_friend_undirect, c('gold', 'red'))
plot(krack_friendship_undirect, 
     layout=layout_with_kk(krack_friendship_undirect), 
     vertex.color=eigen_col, 
     vertex.size=degree(krack_friendship_undirect)*1.5,
     vertex.label.cex=degree(krack_friendship_undirect)*0.15,
     main="Uniderected friendship network\n
     (node color denotes eigenvector centrality, while size reflects degree)")


# Let's now compute also Eigenvector centrality using the (true) directed version
# of the friendship network
friendship_eigen <- eigen_centrality(krack_friendship, directed = TRUE)$vector
summary(friendship_eigen)

# Plot the original (directed) friendship graph using Eigenvector centrality 
# for the nodes' color, and In-degree centrality for the nodes' size
eigen_col = attr_based_color_gradient(friendship_eigen, c('steelblue3', 'gold'))
plot(krack_friendship, 
     layout=layout_with_kk(krack_friendship), 
     vertex.color=eigen_col, 
     vertex.size=deg_friendship_df$in_degree * 2.5,
     vertex.label.cex=deg_friendship_df$in_degree * 0.2,
     edge.arrow.size = 0.25,
     main="Friendship network\n
     (node color denotes Eigenvector centrality, while size reflects in-degree)")



##
# TASK:  
# 
# Do the same kinds of computations and visualizations for the other two networks 
# (advice and reports-to) and compare them to the results obtained for the 
# friendship network. Note down your observations.
###


#
# 2.5 Summary of centrality metrics
#

# To get a summary of centrality measures for the friendship network, we'll construct 
# a data frame with the vertices as rows and the centrality scores as columns:
friendship_centrality_all <- data.frame(node_id=as.integer(V(krack_friendship)$name),
                                        in_degree=deg_friendship_df$in_degree,
                                        out_degree=deg_friendship_df$out_degree,
                                        betweenness=krack_betweenness_df$friendship,
                                        eigen=friendship_eigen)

# Note that to compute in- and out-closeness we needed to remove a few vertices,
# so, we have to add these scores in a separate step, using the merge() function
?merge
friendship_centrality_all <- merge(x = friendship_centrality_all, 
                                   y = friendship_closeness,
                                   by = 'node_id', all = TRUE)
str(friendship_centrality_all)
View(friendship_centrality_all)

# Now we can sort the data frame to find the most central actors 
# according to different centrality measures we have computed.

# Sort by betwenness
friendship_centrality_all[order(friendship_centrality_all$betweenness, decreasing = TRUE),]
# Note that betweenness seems to be correlated with out-degree and out-closeness

# Sort by eigen
friendship_centrality_all[order(friendship_centrality_all$eigen, decreasing = TRUE),]
# Note that Eigenvector c. seems to be correlated with in-degree and in-closeness

##
# TASK: try to interpret these results - how can we explain the observed correlations?
##


###
# 3. CORRELATIONS BETWEEN CENTRALITY MEASURES
###

# Now we'll more thoroughly examine correlations between the centrality metrics 
# to determine how closely these measures are interrelated. We'll use the data 
# frame with centrality metrics for the friendship network.

# We need to compute pairwise correlations, that is, to generate a table
# with correlation values for each centrality measures pair.
# To determine how to compute these correlations, we need to check if the 
# assumption of normal distribution applies to our centrality measures:
apply(friendship_centrality_all[,-1], 2, shapiro.test)
apply(friendship_centrality_all[,-1], 2, qqnorm)
# Not all metrics are normally distributed (betweenness and out_degree 
# have non-normal distribution). So, better compute Spearman correlation coefficient
centrality_corr <- cor(friendship_centrality_all[,-1], 
                       use='complete.obs', # has to be set as we have few NAs
                       method = 'spearman')
centrality_corr
# Not that easy to read and follow...
# We will use the corrpolot() function from the *corrplot* R package 
# to visually represent the computed correlations table
# install.packages('corrplot')
library(corrplot)
corrplot(corr = centrality_corr, type = "upper", 
         diag = FALSE,
         addCoef.col = "black")

# Note:
# To examine the plotting options of the corrplot function
# check, for example, these examples:
# https://rpubs.com/melike/corrplot


# Interpretation:
# In-degree and out-degree have low positive correlation (rho = 0.24),
# indicating that naming someone a friend is not that often reciprocated
# (i.e., if A names B as a friend, it is not very likely that B would 
# also name A as a friend)
#
# In-degree is highly positively correlated with in-closeness, eigenvector 
# centrality (EC), and betweenness. This suggests that those who have been
# named by many others as friends have good network position that is often
# associated with having power and influence in the network. 
#
# Out-degree is highly correlated with out-closeness, and also with 
# betweeness. This implies that those who have named many other people
# as friends have higher chances to act as intermediaries in the network
# (betweenness) and can reach many others through outbound edges.
#
# There is almost perfect positive correlation between in-closeness and
# eigenvector centrality. It suggests that those who are close to many others 
# by being named as a friend are also those who have well connected friends 
# (eigenvector). Both metrics are also highly correlated with in-degree. 

##
# TASK: 
# Do the same kind of analysis - computation of centrality measures 
# and examining correlations of those measures for the advice network.
##