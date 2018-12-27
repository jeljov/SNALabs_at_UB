#################################################################
# This R script is partially based on the 'Introductory Lab' 
# from the Social Network Analysis course at Stanford University
# (available at: https://sna.stanford.edu/lab.php?l=1)
#################################################################
 

####################################################
# LAB 1 - Introductory Lab                                              
# The objectives of this lab are to:
# - cover some basic R commands
# - introduce the *igraph* R package          
# - load and manage network data
# - generate network  visualizations
# - export the network (graph) data for use in  
#   subsequent analysis
####################################################
 
###
# 0. R BASICS 
###
 
# If (when) you get confused, a good place to start is with R's
# built-in help functionality. R offers detailed help files for
# each function and each package. To access help, type: 
# ?[function or package name] 
# For example, for help on the str() function, type:
?str
 

# For this and all subsequent labs, we will use the *igraph* 
# R package, one of the most popular packages for SNA.
# The official manual is available at: 
# http://cran.r-project.org/web/packages/igraph/igraph.pdf
# also at: http://igraph.org/r/
# There is also an excellent tutorial for R and igraph beginners
# available at: http://kateto.net/networks-r-igraph
# It starts with introduction (reminder) of basic R constructs 
# and then proceeds to cover igraph; it offers a detailed 
# coverage of igraphs functionalities in an easy to follow, 
# step-by-step manner. 


# If you haven't used the *igraph* package before, you'll have  
# to instal it first:
# install.packages('igraph')

# The installation is done only once, but you'll have to load 
# the package each time you need to use it:
library(igraph) 
 
# Sometimes, different packages overlap in functionality and 
# cause unexpected behavior when both are loaded simultaneously.
# If you ever want to remove an existing library, use the 
# "detach" command:
#
# detach(package:igraph)
 
 
###
# 1. LOADING DATA
###

# We will use the Krackhardt's High-tech Managers Networks dataset.
# The data were collected from 21 management personnel in a high-tech, 
# machine manufacturing firm to assess the effects of a recent 
# management intervention program.
# The dataset and its description are available at: 
# http://networkdata.ics.uci.edu/netdata/html/krackHighTech.html

# We will load edge lists for 3 networks with the same actors (nodes),
# but different kinds of ties among them:
# - advice ties - one actor (ego) tends to ask the other (alter) 
#   for advice
# - friendship ties - one actor (ego) has named the other actor
#   (alter) as a friend
# - reports-to ties - one actor (ego) reports to the other one
#   (alter)
# All three networks are directed.

# These edge lists are stored in tabular format in .txt files.
# To read data fro those files, we will use the read.table() function.
# read.table() is a common R function for loading data from
# files in which values are in tabular format. The function loads
# the table into a data frame object, which is the basic data type
# for most operations in R. By default, R assumes that the table
# has no header and is delimited by any white space; these
# settings are fine for our purposes here.
#
# One handy aspect of R is that you can read in data from a URL 
# directly by referencing the URL in the read.table() function,
# as follows: 
# advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
# friendship_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Friendship.txt')
# reports_to_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-ReportsTo.txt')
 
# If the files you want to work with are on your local machine, 
# you can reference them by name:
# your_data_frame <- read.table('your_file_name')
advice_data_frame <- read.table('data/Krack-High-Tec-edgelist-Advice.txt')
friendship_data_frame <- read.table('data/Krack-High-Tec-edgelist-Friendship.txt')
reports_to_data_frame <- read.table('data/Krack-High-Tec-edgelist-ReportsTo.txt')

# Note that when you set a variable equal to something, if all 
# goes well R will not provide any feedback. To see the data we
# just loaded, it's necessary to call the variables directly.
advice_data_frame

# Since this is a bit too long, we can see just the top ten rows
head(friendship_data_frame, n=10)
# ... or the bottom ten rows:
tail(reports_to_data_frame, n=10)

# We can also examine the strucure of the data frame:
str(advice_data_frame)

# And look at the unique values of a data column
unique(advice_data_frame$V1) 
unique(advice_data_frame$V2) 

# To view your data in a spreadsheet-like window, use the command 'View()'. 
View(reports_to_data_frame)

# The attribute data for this lab are in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame
# object. In this case, we do have a header row, so we set
# header=T, which tells R that the first row of data contains
# column names.
# We can load csv file from a URLČ
# attributes <- read.csv('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-Attributes.csv', header=T)
# Or we can read from a local file:
attributes <- read.csv('data/Krack-High-Tec-Attributes.csv', header=T)
attributes
 
# Other commands may be used to load data from files in different 
# formats. read.delim() is a general function for loading any
# delimited text file. The default is tab-delimited, but this can 
# be overridden by setting the "sep" parameter. For example:
#
#     f <- read.delim("tab_delimited_file.txt")
#     f <- read.delim("colon_delimited_file.txt", sep=':')
#
# The 'foreign' package will allow you to read a few other 
# custom data types, such as SPSS files via read.spss().
 
# When data files are part of an R package you can read them as 
# follows:
#
# data(kracknets, package = "NetData")
# 
# This is good to know as R packages often come with a number 
# of datasets that can be used for practicing. 
 
 
###
# 2. MERGING DATA
###

# Since the three loaded edge lists relate to the same actors,
# connected through 3 different kinds of relationships, for easier
# data management, we can merge the data from the 3 data frames 
# (corresponding to the 3 edge lists) into one data frame.
 
# For convenience, we can assign some more meaningful column names 
# to our newly imported data frames:
colnames(advice_data_frame) <- c('ego', 'alter', 'advice_tie')
head(advice_data_frame)
 
colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')
head(friendship_data_frame)
 
colnames(reports_to_data_frame) <- c('ego', 'alter', 'reports_to_tie')
head(reports_to_data_frame)
 
# Before we merge these three data frames, we need to make sure 'ego' 
# and 'alter' are the same across data sets. 
# The command below should return TRUE for every row if all ego rows
# are the same for advice and friendship data frames:
advice_data_frame$ego == friendship_data_frame$ego
 
# That's a lot of output to sort through. Instead, we can use all() f.
# which returns TRUE if all vector elements are equal to TRUE
all(advice_data_frame$ego == friendship_data_frame$ego)
 
# Repeat for the other variables
all(advice_data_frame$alter == friendship_data_frame$alter)
all(friendship_data_frame$ego == reports_to_data_frame$ego)
all(friendship_data_frame$alter == reports_to_data_frame$alter)
 
# Now that we've verified they are all the same, we can combine them into 
# a single data frame:
krack_full_data_frame <- cbind(advice_data_frame, 
                               friendship_tie=friendship_data_frame$friendship_tie, 
                               reports_to_tie=reports_to_data_frame$reports_to_tie)
head(krack_full_data_frame)
nrow(krack_full_data_frame)
 
# Now let's move on to some data processing.

# Some actor pairs may not be connected at all. Let's check if / how many
# such cases we have:
nrow(subset(krack_full_data_frame, 
            (advice_tie == 0 & friendship_tie == 0 & reports_to_tie == 0)))

# Reduce to non-zero edges so that the data frame contains only
# actual ties of some type
krack_full_nonzero_edges <- subset(krack_full_data_frame, 
	(advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
head(krack_full_nonzero_edges)
nrow(krack_full_nonzero_edges)


###
# 3. CREATING YOUR FIRST SOCIAL GRAPH
###

# Now we can import our data into a graph object using igraph. 
# Coercing the data into a graph object is what allows us to 
# perform network-analysis techniques
krack_full <- graph_from_data_frame(krack_full_nonzero_edges) 
summary(krack_full)
 
# By default, graph_from_data_frame() treats the first two columns 
# of a data frame as an edge list and any remaining columns as 
# edge attributes. Thus, the 232 edges appearing in the summary()
# output refer to the 232 pairs of vertices that are joined by 
# *any type* of tie. The tie types themselves are listed as edge 
# attributes.
 
# To get a vector of edges for a specific type of tie, use the 
# get.edge.attribute() function.
get.edge.attribute(krack_full, 'advice_tie')
# Alternatively, it can be done as follows:
E(krack_full)$advice_tie
# where 'E' represents the set of edges of the graph
# whose name is given in the brackets

# We can examine the proporition of advice connections
#  in the overall set of ties:
prop.table(table(E(krack_full)$advice_tie))

# Access the friendship ties
get.edge.attribute(krack_full, 'friendship_tie')
# alternative:
E(krack_full)$friendship_tie
# The proportion of friendship relations:
prop.table(table(E(krack_full)$friendship_tie))

# And, to reports-to ties
get.edge.attribute(krack_full, 'reports_to_tie')
# alternative:
E(krack_full)$reports_to_tie
# The proportion of reports-to relations:
prop.table(table(E(krack_full)$reports_to_tie))

 
# If you would like to symmetrize the network, making all asymmetric
# (directed) ties symmetric (undirected), use the as.undirected() function.
# For example, if we assume that the friendship relation is symmetric, we
# can transfrom the friendship network into undirected one as follows
# (check the documentation for the 'mode' parameter): 
krack_friend_undirect <- as.undirected(krack_friendship, mode='collapse')
summary(krack_friend_undirect)
# Note that the resulting, undirected, network has no edge attributes.
# If we want to collapse the directed network and keep edge weights,
# we should set the "edge.attr.comb". For example, to sum the values
# of the friendship_tie attribute and ignore the other two tie attributes
# we set "edge.attr.comb" as follows:
krack_friend_undir_weighted <- as.undirected(krack_friendship, mode='collapse', 
                                             edge.attr.comb = list(friendship_tie='sum', 'ignore'))

# Note: the list given as the value of the 'edge.attr.comb' parameter specifies that 
# the 'friendship_tie' attribute of the new (undirected) edge should be the sum of the 
# 'friendship_tie' attr. of the corresponding edges in the directed graph; and that the 
# rest of the attributes should be ignored (=dropped). 
# To learn more about setting these options, check the documentation for attribute.combination
?attribute.combination

summary(krack_friend_undir_weighted) 
E(krack_friend_undir_weighted)$friendship_tie

 
###
# 4. ADDING VERTEX ATTRIBUTES TO A GRAPH OBJECT
###

# Now, we want to use the 'attributes' data frame (df) to assign
# attributes to the nodes in the 'krack_full' graph. 
# To do that, we first need to add a column to the 'attributes' df to 
# represent nodes' IDs. This is straightforward since, in this case, node IDs 
# are simply the nodes' ordinal numbers: 
attributes$node_id = 1:nrow(attributes)
# Next, we can use node IDs to match rows from the 'attributes' df to the 
# appropriate nodes in the graph. Considering that nodes's IDs are ordinal numbers
# from 1 to 21 (21 = number of nodes), we can expect that the order of nodes in the 
# graph is the same as the order of attributes in the 'attributes' df. 
# Let's check that:
all(attributes$node_id == V(krack_full)$name)
# 'V' represents the set of nodes (vertices) of the graph
# whose name is given in the brackets

# Since our assumption proved correct, we can simply add attributes as follows:
V(krack_full)$age = attributes$AGE
V(krack_full)$tenure = attributes$TENURE
V(krack_full)$level = attributes$LEVEL
V(krack_full)$dept = attributes$DEPT

summary(krack_full) 
# Note that we now have 'age,' 'tenure,' 'level', and 'dept'
# listed alongside 'name' as vertex attributes.


# Another way to add node attributes to a graph is to include them 
# when creating the graph object.
 
# First, reorder the columns, so that node id is the first one 
str(attributes)
attributes <- attributes[,c(5,1:4)]
# Now, create the graph. 
# Check the documentatio of the graph_from_data_frame() f. to better
# understand the 'vertices' parameter. 
krack_full_v2 <- graph_from_data_frame(d = krack_full_nonzero_edges, 
                                       vertices = attributes) 
summary(krack_full_v2)


# We can get values for a given attribute for all of
# the actors in the network - for example:
get.vertex.attribute(krack_full, 'age')
# Alternatively:
V(krack_full)$age

get.vertex.attribute(krack_full, 'tenure')
# or
V(krack_full)$tenure

# We can also get attribute values for a subset of nodes.
# For example, to get the tenure of only those who are 
# above 40 years of age:
get.vertex.attribute(krack_full, 'tenure', V(krack_full)[V(krack_full)$age > 40])
# or
V(krack_full)$tenure[V(krack_full)$age > 40]

# Or, we can the age of employees in the largest department:
table(V(krack_full)$dept)
V(krack_full)$age[V(krack_full)[V(krack_full)$dept == 2]]



###
# 4. CREATING MORE GRAPHS
###

# Now, starting from the krack_full graph, we'll create a separate network 
# for each tie type

# advice network
krack_advice <- delete_edges(krack_full, 
                             E(krack_full)[E(krack_full)$advice_tie == 0])
summary(krack_advice)
# Note that this will not remove the other kinds of ties (friendship, reports_to).
# If two employees are connected not only through the advice tie, but also through
# other kinds of ties, those other kinds will be preserved. If you want to remove 
# those other kinds of ties, you need to remove the corresponding edge attributes
# using the delete_edge_attr() f.

# friendship network
krack_friendship <- delete_edges(krack_full, 
                                 E(krack_full)[E(krack_full)$friendship_tie == 0])
summary(krack_friendship)

# reports-to network
krack_reports_to <- delete_edges(krack_full, 
                                 E(krack_full)[E(krack_full)$reports_to_tie == 0])
summary(krack_reports_to)

 
###
# 5. VISUALIZING NETWORKS
###
 
# The igraph package allows us to use R's plot() function to generate 
# custom visualizations of our networks.

# R only lets us look at one plot at a time. To be able to compare
# multiple networks, that is, their plots, you may want to save 
# each plot in a separate PDF or image file. The code below shows 
# how to do that. Alternatively, to just create a plot, execute 
# the code between the pdf() / jpeg() function and dev.off().

# First, let's plot the network with all possible ties
jpeg("output/1.1_Krackhardt_Full.jpg")
plot(krack_full,
     edge.arrow.size=.3,  # reduce the size of arrows
     main="High-tech Managers Networks")
dev.off()
# Check the documentation of the jpeg() function to see
# how you can customise the image dimension, quality, ..
 
# This is a bit of a jumble, so let's look at the networks for
# single edge types
 
pdf("output/1.2_Krackhardt_Advice.pdf")
plot(krack_advice,
     edge.arrow.size=.3,
     main="High-tech Managers Advice Network")
dev.off()
# Check the documentation of the pdf() function to see
# how you can customise the generated PDF document
# (e.g. by setting font, paper size, title, etc)
 
jpeg("output/1.3_Krackhardt_Friendship.jpg")
plot(krack_friendship,
     edge.arrow.size=.3,
     main="High-tech Managers Friendship Network")
dev.off()

jpeg("output/1.4_Krackhardt_Reports.jpg")
plot(krack_reports_to,
     edge.arrow.size=.3,
     main="High-tech Managers Reports-to Network")
dev.off()
 
# We can optimize the layout by applying a layout 
# algorithm to the specific set of ties we care about. 
# The above graphs were plotted using the Fruchterman-Rheingold
# algorithm (the default one). Other options are described in 
# the igraph help page for "layout_" which can be accessed by 
# entering ?layout_
?layout_

reports_to_layout <- layout_nicely(krack_reports_to)
jpeg("output/1.5_Krackhardt_Reports_layout_nicely.jpg")
plot(krack_reports_to, 
     layout=reports_to_layout,
     edge.arrow.size=.3,
     main="High-tech Managers Advice Network")
dev.off()
 
# Now let's color-code vertices by department 
# Check the possible values for the 'dept' attribute
unique(V(krack_full)$dept)
# Initiate the vector of node colors
dept_vertex_colors = V(krack_full)$dept
# Select a color palette using the ColorBrewer (http://colorbrewer2.org) 
colors = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e')
for(i in 0:max(dept_vertex_colors))
  dept_vertex_colors[dept_vertex_colors == i] = colors[i+1]

jpeg("output/1.6_Krackhardt_Reports_Color.jpg") 
plot(krack_reports_to, 
    layout=reports_to_layout, 
    vertex.color=dept_vertex_colors, # setting node color
    vertex.label=NA,     # removing vertex labels
    edge.arrow.size=.3,
    main="Reports-to network\n(node color denotes department)")
dev.off() 

# Now let's set the vertex size by tenure:
tenure_vertex_sizes = V(krack_full)$tenure
tenure_vertex_sizes

jpeg("output/1.7_Krackhardt_Reports_Vertex_Size.jpg") 
plot(krack_reports_to, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3,
     vertex.size=tenure_vertex_sizes, # setting the vertex size
     main="Reports-to network\n(node color denotes department, size denotes tenure)")
dev.off() 


# Now let's try to incorporate additional tie types. We'll use the 
# layout generated for the graph with the reports-to ties only
# but overlay the friendship ties (in another color). The idea is
# to try to (visually) explore how well friendship relations go
# along the official organisational structure (reflected in the 
# reports_to relation).

# We are about to add several visualization related attributes 
# to the graph. Since this is just temporarily, for the sake of 
# demonstrating visualization options, we'll replicate the graph, 
# so that the 'main' one is not 'burdened' with visual details.
krack_full_viz <- krack_full

# Since we are not interested in the advice tie, we will remove that edge
# attribute from the graph we want to visualize
krack_full_viz <- delete_edge_attr(krack_full_viz, 'advice_tie')
summary(krack_full_viz)
# Add colors to edges based on their type (friendship, reports_to)
E(krack_full_viz)$color[ E(krack_full_viz)$friendship_tie==1 ] <- '#377eb8' # blue
E(krack_full_viz)$color[ E(krack_full_viz)$reports_to_tie==1 ] <- '#984ea3' # purple
E(krack_full_viz)$arrow.size=.3 
V(krack_full_viz)$label = NA
V(krack_full_viz)$size = tenure_vertex_sizes
V(krack_full_viz)$color = '#fed98e' # yellow as the node color
V(krack_full_viz)$frame = '#000000' # black as the color of the nodes' edge/frame

jpeg("output/1.8_Krackhardt_Overlayed_Ties.jpg")
plot(krack_full_viz, 
     layout=reports_to_layout)
# Add a legend. Note that the plot window must be open for this to 
# work.
legend(x = 0.95, 
       y = 1.4,
       legend = c('Friendship',
                  'Reports To'), 
       col = c('#377eb8', '#984ea3'), 
       lty=1,
       cex = .7)
dev.off() 
 

# Another option for visualizing different network ties relative 
# to one another is to overlay the edges from one tie type on the 
# structure generated by another tie type. Here we can use the
# reports-to layout but show the friendship ties:

jpeg("output/1.9_Krackhardt_Overlayed_Structure.jpg")
plot(krack_friendship, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3,
     vertex.size=tenure_vertex_sizes, 
     main="Friendship network\noverlayed on the reports-to network structure")
dev.off() 


plot(krack_advice, 
     layout=reports_to_layout, 
     vertex.color=dept_vertex_colors, 
     vertex.label=NA, 
     edge.arrow.size=.3,
     vertex.size=tenure_vertex_sizes, 
     main="Advice network\noverlayed on the reports-to network structure")

 
###
# 5. EXPORT THE NETWORK
###
 
# The write.graph() function exports a graph object in various
# formats readable by other programs. For example, a graph can be
# exported in the 'graphml" format and imported in Gephi for 
# visualisation.
write.graph(krack_full, file='output/lab1/krack_full.graphml', format="graphml")
 
# For a more general file type (e.g., importable to Excel),
# use the "edgelist" format. Note that this file format will
# write the attributes; only the ties will be stored.
write.graph(krack_full, file='output/lab1/krack_full.txt', format="edgelist")

# We can also save graphs as RData files. This is the best option if we intend
# to further process graphs in R, since this format will keep all the specific
# characteristics of the graph object that we have created.
saveRDS(krack_full, "output/lab1/krack_full.RData")
saveRDS(krack_advice, "output/lab1/krack_advice.RData")
saveRDS(krack_friendship, "output/lab1/krack_friendship.RData")
saveRDS(krack_reports_to, "output/lab1/krack_reports_to.RData")
