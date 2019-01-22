##############################################################
# 
# LAB 5
#
# The primary objective of this lab is to show how to use the
# *rtweet* R package to collect data about tweets and Twitter 
# users, and how to use the collected data to create graphs 
# (social networks) of Twitter users in igraph.
# The secondary objective is to demonstrate the use of graph 
# visualization R packages such as *visNetwork*.
#
##############################################################


##
# 1. SETUP
##

# To be able to collect data from Twitter through the Twitter API
# (https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets)
# you'll need to do the following three things:
# 1) Set up a Twitter account, if you don’t already have one
# 2) Using your account, setup an application that you will use to 
# access Twitter from R
# 3) Install and load the following R packages: rtweet, httpuv, and tidyr

# To setup your Twitter app, follow the tutorial from rtweet:
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# or this 'recipe' from "21 Recipes for Mining Twitter Data with rtweet":
# https://rud.is/books/21-recipes/using-oauth-to-access-twitter-apis.html

# Then, install and load *rtweet* and *tidyr* R packages
# install.packages('rtweet')
# install.packages('httpuv')
# install.packages('tidyr')
library(rtweet)
library(httpuv)
library(tidyr)

# We'll also install and load some additional 
# R packages that we'll need for this lab:
# - dplyr - for various data manipulation tasks (not specific to SNA)
# - visNetwork - for creating interactive graph visualizations
#
# install.packages('visNetwork')
# install.packages('dplyr')
library(dplyr)
library(visNetwork)
library(igraph)


# Before we can proceed, make sure that you have created a *twitter_token*.
# Note that the creation of twitter_token is done just once. After the token is 
# created, stored in a file, and the path to the file saved in the .Renviron file, 
# (as will be shown below) the stored token will be directly accessed and used 
# by the rtweet's functions whenever you request some data from Twitter.

# The following code (lines 61-67) will create a twitter token for your Twitter app.
# Note that before executing these lines, you have to substitute the placeholders 
# (e.g. "your_app_name") with your own values for app, consumer_key, consumer_secret, 
# access_token, and access_secret variables. You can find these values at the web page
# for your Twitter app (https://developer.twitter.com/en/apps)
twitter_token <- create_token(
  app = "your_app_name", # replace this string with the actual name of your Twitter app
  consumer_key = "your_consumer_key", # replace this string with the actual consumer_key for your Twitter app 
  consumer_secret = "your_consumer_secret", # replace this string with the actual consumer_secret for your Twitter app 
  access_token = "your_access_key", # replace this string with the actual access_key for your Twitter app 
  access_secret = "your_access_secret" # replace this string with the actual access_secret for your Twitter app 
)  

# Save the token in a file:
saveRDS(twitter_token, "rtweet.rds")
# And store the name of the file ("rtweet.rds") in the .Renviron file:
# open the local .Renviron file for editing...
file.edit(".Renviron")
# ... and add the path to the file where the token is saved as the value
# of the TWITTER_PAT environment variable, that is, add the following line: 
# TWITTER_PAT=rtweet.rds 
# That's all; now you can use functions from the rtweet package without worrying
# about the token - rtweet will automatically discover the token in your .Renviron file. 

# Note: the creation of twitter token is covered in the above suggested tutorials for 
# setting up a Twitter app. So, if the above lines are not fully clear to you,
# you can create the needed token by following this tutorial:
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html
# If you are having problems with setting up / using the .Renviron file, 
# check the following section from the Efficient R Programming book:
# https://csgillespie.github.io/efficientR/3-3-r-startup.html#r-startup


###
# 2. SEARCH TWITTER AND COLLECT DATA FOR SETTING UP A NETWORK
###

?search_tweets
brexit_tweets <- search_tweets(q = "#brexit", 
                               type = 'mixed', 
                               lang="en", 
                               # geocode = "51.509865,-0.118092,5mi",
                               n = 5000, 
                               include_rts = FALSE)

# Note: to more easily create complex queries, that is queries enabled by 
# Twitter advanced search (https://twitter.com/search-advanced),
# you can follow these guidelines:
# https://help.twitter.com/en/using-twitter/twitter-advanced-search

# (optionally) save the results
saveRDS(brexit_tweets, 'data/brexit_tweets_22-01-2019.RData')

# Examine the variables (columns) in the obtained dataset
colnames(brexit_tweets)

# Select only those columns (variables) that are relevant for creating
# a network. We will create "mentions" network, that is a network with
# Twitter users as nodes and connections (edges) established based on 
# one user mentioning another in his/her tweet. Mentioning can take the
# form of direct replies or 'simple' mentions anywhere in the tweet's text.
# We will also rename the selected columns, for the sake of having 
# shorter variable names
brexit_tweets_users <- brexit_tweets %>%
  select(screen_name, reply_to_screen_name, mentions_screen_name) %>%
  rename(sender=screen_name, replied_to=reply_to_screen_name, mentioned=mentions_screen_name)

glimpse(brexit_tweets_users) 
# Note that the mentioned column is a list; we will deal with it later.

# Note that many entries have missing values (NA) for the 'replied_to' and 'mentioned'
# variables. Check how many such cases we have:
length(which(is.na(brexit_tweets_users$replied_to)))
length(which(is.na(brexit_tweets_users$mentioned)))
# Keep only those tweets where the sender mentioned or replied to at least 
# one other Twitter user
brexit_tweets_users <- brexit_tweets_users %>%
  filter( !is.na(replied_to) | !is.na(mentioned) )

View(brexit_tweets_users[1:10,])

# Next, we will unnest the 'mentioned' column. 
# Unnesting is the transformation of a list-column, so that each 
# element of the list gets its own row.
?unnest
brexit_tweets_users <- unnest(brexit_tweets_users, mentioned) 
View(brexit_tweets_users[1:20,])

# To create graphs, we need to transform our brexit_tweets_users dataframe
# into the format required by igraph's functions for graph construction.
# In particular, we will transform it into 2 edge lists: one for the  
# 'reply_to' relation, and the other for the 'mentioned' relation.

# First, create edge list for the 'reply_to' relation:
replied_to_edgelist <- brexit_tweets_users %>%
  select(-mentioned) %>%              # remove the 'mentioned' column
  filter(complete.cases(.)) %>%       # keep only complete rows, that is, rows without NAs
  group_by(sender, replied_to) %>%    # group the rows based on the sender, replied_to combination
  summarise(n = n()) %>%              # compute the size of each group and assign the value to variable 'n'
  ungroup()

head(replied_to_edgelist, n=10)
# check the pairs with the most intensive communication: 
replied_to_edgelist %>%
  arrange(desc(n)) %>%
  head(n=10)

# In a similar way, create an edge list based on 'mentioned' relation
mentioned_edgelist <- brexit_tweets_users %>%
  select(-replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, mentioned) %>%
  summarise(n = n()) %>%
  ungroup()

head(mentioned_edgelist, n=10)
# examine most frequent mentions:
mentioned_edgelist %>%
  arrange(desc(n)) %>%
  head(n=10)


# Get the unique users in both edge lists.
# Those will be nodes in the corresponding networks.

# Unique users in replied_to edgelist
reply_to_unique <- with(replied_to_edgelist, union(sender, replied_to))
length(reply_to_unique)

# Unique users in mentioned edgelist
mention_unique <- with(mentioned_edgelist, union(sender, mentioned))
length(mention_unique)

# Considering the large number of unique users, that is, the number of
# nodes in the prospective networks, it is worth examining options for 
# filtering out some of them, in order to:
# - reduce the 'noise' in the data,
# - make the graphs easier to manipulate and analyse.

# To decide on how to do the filtering, we'll examine the frequency 
# of connections
summary(replied_to_edgelist$n)
summary(mentioned_edgelist$n)

# As there are too many users with loose connections (n<=2),
# remove those connections that occurred just once.
# First for the mentioned edgelist:
mentioned_edgelist_reduced <- mentioned_edgelist %>%
  filter(n > 1)
# Note a huge reduction in the size of the edgelist

# Check again the number of unique users 
mention_unique <- with(mentioned_edgelist_reduced, union(sender, mentioned))
# It is also significantly reduced

# Now, for the reply_to edge list:
replied_to_edgelist_reduced <- replied_to_edgelist %>%
  filter(n > 1)
# Again, a notable reduction

# Check again the number of unique users 
reply_to_unique <- with(replied_to_edgelist_reduced, union(sender, replied_to))
# It is reduced but not as much as in the case of the 
# mentioned relation


# Next, we'll collect users' data. These data can be used to describe nodes 
# in networks, that is, to associate attributes to nodes.

# First, we will collect data about the senders, since these data are available 
# in the dataset we have already collected from Twiter (brexit_tweets)
glimpse(brexit_tweets)
# Get the data for all the senders, regardless of the type of relation
# they established with alters (mentioned, replied_to). 
all_senders <- union(mentioned_edgelist_reduced$sender, 
                     replied_to_edgelist_reduced$sender)
senders_data <- brexit_tweets %>%
  filter(screen_name %in% all_senders) %>%  
  users_data() %>%                          # rtweet's function that pulls user data from a dataset of tweets
  distinct(user_id, .keep_all = TRUE)

# check the kind of user data that is available
glimpse(senders_data)


# Now, collect data for alters (mentioned / replied_to) - these are not necessarily
# available in the collected tweets data, but have to be obtained separately.

# First, identify the alters (users) for whom the data are not available
no_data_alters <- setdiff(union(mention_unique, reply_to_unique), all_senders)
length(no_data_alters)
# Collect data for these users (no_data_alters) from Twitter. 
# To that end, we will use the lookup_users function from rtweet package
?lookup_users
alters_data <- lookup_users(no_data_alters)

# save the data
saveRDS(alters_data, "data/brexit_alters_data_22-01-2019.RData")

glimpse(alters_data)
# In addition to user data, the lookup_users() f. also returned users' tweets.
# However, we need only user data:
alters_data <- users_data(alters_data)

# We should also check if we managed to retrieve data for all the users that 
# we were interested (the service may not return all the requested data)
missing_alter <- setdiff(no_data_alters, alters_data$screen_name)
# We didn't get the data for 1 user; 
# we'll drop him/her from the edge lists before saving them.

# Since we have done a lot of relevant processing steps, it may be wise to save 
# the created edge lists and user attributes, so that we do not ahve to repeat the
# processing steps.
# Before saving edge lists, rename columns to the typical names used in edge lists
mentioned_edgelist_reduced %>%
  filter(!mentioned %in% missing_alter) %>%
  rename(ego=sender, alter=mentioned, mention_tie=n) %>%
  saveRDS(file = "data/mentions_edgelist_#brexit_20-01-2019.RData")

replied_to_edgelist_reduced %>%
  filter(!replied_to %in% missing_alter) %>%
  rename(ego=sender, alter=replied_to, reply_to_tie=n) %>%
  saveRDS(file = "data/replied_to_edgelist_#brexit_20-01-2019.RData")

saveRDS(senders_data, file = "data/ego_data_#brexit_20-01-2019.RData")
saveRDS(alters_data, file = "data/alter_data_#brexit_20-01-2019.RData")

# Do the clean up, that is, remove all objects from the environment, 
# we won't need them any more
remove(list = ls())


##
# 3. CREATE NETWORKS OF TWITTER USERS
##

# Load data (edge list) for creating a network based on the 'mentioned' relation
mention_edgelist <- readRDS("data/mentions_edgelist_#brexit_20-01-2019.RData")
summary(mention_edgelist$mention_tie)

# Create a directed network
mention_net <- graph_from_data_frame(mention_edgelist)
summary(mention_net)
# Obviously very sparse network; let's check its density
edge_density(mention_net)

# We can try to plot the graph, but it is overly large 
# and the plot will be messy
plot(mention_net, 
     layout = layout_with_lgl(mention_net),
     edge.arrow.size=0.3,
     vertex.size = 5,
     vertex.label = NA)
# We will get back to the plotting task a bit later.

# Let's add attributes to nodes, and use these to better understand the network.
# For example, we can include the number of followers, friends, and tweets.
# To that end, we will create a function that receives: 
# 1) Twitter user data as returned by the rtweet's users_data() function, 
# 2) a vector of screen names of those users we are interested in.
# The function returns a data frame with four variables (columns):
# screen_name, followers_count, friends_count, and statuses_count
get_user_attrs <- function(twitter_user_data, users_screen_names) {
  twitter_user_data %>%
    filter(screen_name %in% users_screen_names) %>%
    select(screen_name, followers_count, friends_count, statuses_count)
}

# Load senders data; it will be used to add attributes to ego /source nodes
# (i.e. nodes that edges originate from) 
senders_data <- readRDS("data/ego_data_#brexit_20-01-2019.RData")
# Extract the set of attributes we are interested in 
ego_attrs <- get_user_attrs(senders_data, V(mention_net)$name)
glimpse(ego_attrs)

# Load data and extract attributes about users mentioned in tweets
alters_data <- readRDS("data/alter_data_#brexit_20-01-2019.RData")
alter_attrs <- get_user_attrs(alters_data, V(mention_net)$name)  

# Merge attributes for all the actors in the 'mentioned' network 
node_attrs <- rbind(ego_attrs, alter_attrs) %>% # merge the two data frames
  distinct(screen_name, .keep_all = TRUE) %>%   # keep only distinct rows (= remove duplicates)
  arrange(screen_name)                          # sort based on the username

head(node_attrs, n=10)
summary(node_attrs[,-1])

# Add attributes to nodes
# Initialize all vertex attributes to zero
V(mention_net)$followers_cnt <- rep(0, vcount(mention_net))
V(mention_net)$friends_cnt <- rep(0, vcount(mention_net))
V(mention_net)$posts_cnt <- rep(0, vcount(mention_net))
# For each vertex...
for (i in 1:vcount(mention_net)) {
  # find the corresponding row in the node_attrs df
  index <- which(node_attrs$screen_name == V(mention_net)$name[i])
  # assign to the vertex attributes values of the corresponding columns in the selected row
  V(mention_net)$followers_cnt[i] <- node_attrs$followers_count[index]
  V(mention_net)$friends_cnt[i] <- node_attrs$friends_count[index]
  V(mention_net)$posts_cnt[i] <- node_attrs$statuses_count[index]
}
# Let's check if the attributes have been added
summary(mention_net)
summary(V(mention_net)$followers_cnt)


# We will now make use of the vertex attribute to better understand the graph.

# We will make the size of the nodes proportional to the number of followers and 
# use color to reflect the number of friends. 
# Create gradient color vector based on the number of friends:
source('SNA_custom_functions.R')
friends_for_color <- attr_based_color_gradient(log1p(V(mention_net)$friends_cnt), 
                                               c('gold','steelblue3'))
# Create a vector for node sizes, based on the number of followers
followers_for_size <- log1p(V(mention_net)$followers_cnt)
# Note that we used logged values of both the friend_cnt and followers_cnt vectors
# due to the very uneven distribution of values of these two attributes 
# (plot the density functions of these attribues to see the distribution)

# Now, draw a plot
plot(mention_net, 
     layout = layout_with_lgl(mention_net),
     edge.arrow.size=0.3, 
     vertex.label = NA,
     vertex.size = followers_for_size,
     vertex.color = friends_for_color,
     main = "Twitter-based mention network\n 
              node color denotes the number of friends, node size reflects the number of followers")

###
# TASK: Follow the above procedure to create and visualise a graph 
# based on the reply_to connection
###


##
# 4. VISUALISE NETWORKS OF TWITTER USERS USING VISNETWORK
##

# Since the overall graph is overly large for meaningful visualization
# let's take the giant component and create its visualization.

# We will start by identifying components (subgraphs) in the graph. 
# Due to the sparsity of edges, we won't be able to identify 'strong' components, 
# so, we will opt for 'weak' components (reminder: for 'strong' components, the 
# directionality of edges is considered; for detection of 'weak' components, the 
# direction of edges is disregarded)
m_net_comp <- components(mention_net, mode = 'weak')
str(m_net_comp)
# Identify the largest component:
giant_comp_size <- max(m_net_comp$csize)
giant_comp_index <- which(m_net_comp$csize == giant_comp_size)
# Next, extract the giant component from the mention_net graph. To that end, we will use
# induced_subgraph function
?induced_subgraph
giant_comp <- induced_subgraph(mention_net,
                               vids = V(mention_net)[m_net_comp$membership==giant_comp_index])

summary(giant_comp)

# Now, plot the giant component using igraph's plotting features. 
# Use the same kind of mapping between nodes' attributes (friends_cnt and followers_cnt) and their
# visual representation (color and size, respectively)
gc_colors <- attr_based_color_gradient(log1p(V(giant_comp)$friends_cnt), c('gold','steelblue3'))
gc_size <- log1p(V(giant_comp)$followers_cnt)
plot(giant_comp, 
     layout = layout_with_dh(giant_comp),
     edge.arrow.size=0.3, 
     vertex.label = NA,
     vertex.size = gc_size,
     vertex.color = gc_colors,
     main = "Giant component of the Twitter-based mention network\n 
     node color denotes the number of friends, node size reflects the number of followers")

# Better than the previous graph, but still not sufficiently clear. 
# To try to get a better insight into the network, we will use interactive plots of the
# *visNetwork* R package.

# Note: for a tutorial on visNetwork and examples of use, see:
# - Introduction to visNetwork, at:
#   https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html 
# - visNetwork documentation:
#   http://datastorm-open.github.io/visNetwork/ 
# - Section 6.2 of the 'Network visualization with R' tutorial, available at:
#   http://kateto.net/network-visualization

# As a minimum for graph visualization, visNetwork requires:
# - nodes data frame, with 'id' column (plus any additional columns with node attributes)
# - edges data.frame, with 'from' and 'to' columns (plus any additional columns with edge attributes)

# Let's create those two (minimal) data frames:
nodes_df <- data.frame(id=V(giant_comp)$name, stringsAsFactors = FALSE)
head(nodes_df)
edges_df <- data.frame(as_edgelist(giant_comp), stringsAsFactors = FALSE)
colnames(edges_df) <- c('from', 'to')
head(edges_df)
# Now, we can create the simplest visualisation with visNetwork
visNetwork(nodes = nodes_df, edges = edges_df, 
           main="Giant component of the Twitter-based mention network")


# Extend the nodes data frame with columns defining node color and size
# Note: see visNodes for available options
nodes_df$color <- gc_colors
nodes_df$size <- 12 + gc_size
head(nodes_df)

# Extend the edges data frame with columns defining edge width, color, 
# if it is curvy or not, and if / how arrows are displayed.
# Note: see visEdges for available options
edges_df$width <- 1 + (E(giant_comp)$mention_tie / 3)
edges_df$color <- 'slategray3'
edges_df$smooth <- TRUE # should the edges be curved?
edges_df$arrows <- 'to'
head(edges_df)

visNetwork(nodes = nodes_df, 
           edges = edges_df, 
           main="Giant component of the Twitter-based mention network",
           footer = "Node color denotes the number of friends, node size reflects the number of followers")


# Let's add a few additional details for node display:
nodes_df$shadow <- FALSE        # nodes will have shadow?
nodes_df$title  <- nodes_df$id  # text to be displayed on mouse over 
nodes_df$borderWidth <- 1.5     # node border width

# Instead of one color for a node, we can specify different colors 
# for different parts of a node (main body and border) and different interaction states.
# First, remove the existing color attribute
nodes_df <- nodes_df %>% select(-color)
# Then, add new color-related attributes
nodes_df$color.background <- gc_colors
nodes_df$color.border <- "black"
nodes_df$color.highlight.background <- "orange" # color of the main body of a node when clicked
nodes_df$color.highlight.border <- "darkred"    # color of the border when a node is clicked

# Run the visualization with the new parameters set
visnet <- visNetwork(nodes = nodes_df, edges = edges_df, 
                     main="Giant component of the Twitter-based mention network",
                     footer = "Node color denotes the number of friends, 
                                node size reflects the number of followers")
visnet

# visNetwork offers a number of other options available through 
# the visOptions() function. For instance, we can highlight all 
# direct neighbors and those 2 hops away:
visnet2 <- visOptions(visnet, highlightNearest = list(enabled = TRUE, degree = 2))
visnet2

# explore other visOptions
?visOptions

# To examine the available layout options
?visLayout

visLayout(visnet, randomSeed = 2001, improvedLayout = TRUE)

###
# TASK: Follow the above procedure to create and visualise - using visNetwork - 
# giant component of the graph based on the reply_to connection
###