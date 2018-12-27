# The function creates a vector of color values for the given graph (node / edge)
# attribute (1st argument). The color vector consists of gradients of the given
# color palette: the 2nd argument is a vector of two elements defining the palette's
# end points (e.g.: c('yellow','red')). The resulting color vector has as many color
# gradients as there are different values in the given node / edge attribute.
# The function is based on the procedure described in:
# http://natpoor.blogspot.com/2016/07/making-spectrumgradient-color-palette.html

attr_based_color_gradient <- function(g_attr, pal_end_points) {
  require(dplyr)
  # 1) Set the resolution, that is, how many color nuances are to be created
  col_resolution = n_distinct(g_attr)
  # 2) Set palette end points 
  col_palette = colorRampPalette(pal_end_points)
  # 3) Get the max value of the attribute to make the ratio: 
  max_val = max(g_attr, na.rm = TRUE)
  # 4) Create a vector of values which will determine the color values 
  # for each node: 
  value_vector = g_attr / max_val
  # 5) Create the vector of color values, based on the value_vector, the 
  # palette end points and the resolution. This will produce a vector of 
  # color values with the correct color value in the correct location for 
  # the chosen graph attribute: 
  g_colors = col_palette(col_resolution)[as.numeric(cut(value_vector, breaks=col_resolution))]
  return(g_colors)
}


# The function computes homophily as the proportion of all edges in a network 
# which bridge two actors with matching characteristics on some attribute. 
# For example, gender homophily could be calculated for both males and females. 
# Male homophily would be calculated as the total number of edges that exist  
# between two males, expressed as a proportion of the total number of edges  
# involving at least one male.
# Based on the function presented at:
# https://dappls.umasscreate.net/networks/calculating-network-homophily-part-1/
homophily<-function(graph, vertex.attr, attr.val=NULL, prop=T){
  
  # Create a copy of the input graph, so that the input graph is not altered
  g <- graph_from_data_frame(get.data.frame(graph))
  
  # Assign values of the chosen vertex attribute (vertex.attr) as the
  # 'name' attribute of the vertices; this way, we can create 
  # attribute-specific edgelist, that is, edgelist with values of 
  # vertex.attr as representations of the nodes
  V(g)$name<-vertex_attr(graph, vertex.attr)
  
  # Get the edgelist with values of vertex.attr as nodes
  ee<-get.data.frame(g)
  
  # If no particular attribute value was specified, get percentage (prop=T)
  # or count (prop=F) of all nodes tied with matching attribute
  if(is.null(attr.val)){
    ifelse(prop==T, sum(ee[,1]==ee[,2])/nrow(ee), sum(ee[,1]==ee[,2]))
  
  # If attr.val is not null, get proportion (prop=T) or count (prop=F) of
  # edges among nodes with that particular node attribute value
  } else {
    ifelse(prop==T,sum(ee[,1]==attr.val & ee[,2]==attr.val)/nrow(ee[ee[,1]==attr.val|ee[,2]==attr.val,]),
           sum(ee[,1]==attr.val & ee[,2]==attr.val))
  }
}



# The function takes a graph (the 1st input parameter) and finds 
# the iqv statistic for the categorical variable given as the 2nd 
# input parameter. 
# NOTE: the function assumes that categorical variables have been 
# numerically coded to integer values that ascend sequentially from 0.
get_iqvs <- function(graph, attribute) {
  
  mat <- get.adjacency(graph)
  
  # Find out how many coded levels (unique responses) exist for
  # the attribute variable 
  attr_levels = get.vertex.attribute(graph,
                                     attribute,
                                     V(graph))
  num_levels = length(unique(attr_levels))
  
  iqvs = rep(0, nrow(mat))
  
  # Now, loop through all the actors in the network
  
  for (ego in 1:nrow(mat)) {
    
    # initialize actor-specific variables
    alter_attr_counts = rep(0, num_levels)
    num_alters_this_ego = 0
    sq_fraction_sum = 0
    
    # For each ego we want to check each tied alter for the same
    # level on the variable attribute as the ego.
    
    for (alter in 1:ncol(mat)) {
      
      # only examine alters that are actually tied to ego
      if (mat[ego, alter] == 1) {
        
        num_alters_this_ego = num_alters_this_ego + 1
        
        # get the alter's level on the attribute 
        alter_attr = get.vertex.attribute(graph, 
                                          attribute, alter)
        
        # increment the count of alters with this level of the attribute
        alter_attr_counts[alter_attr + 1] =
          alter_attr_counts[alter_attr + 1] + 1
      }
    }
    
    # now that we're done looping through all of the alters,
    # get the squared fraction for each level of the attribute
    # out of the total number of tied alters
    for (i in 1:num_levels) {
      attr_fraction = alter_attr_counts[i] / num_alters_this_ego
      sq_fraction_sum = sq_fraction_sum + attr_fraction ^ 2
    }
    
    # now we can compute the ego's blau index...
    blau_index = 1 - sq_fraction_sum
    
    # and the ego's IQV, which is just a normalized blau index
    iqvs[ego] = blau_index / (1 - (1 / num_levels))
  }
  
  return(iqvs)
}


# The following function produces an animation of the edge-betweeness
# community detection process applied to the given graph (g). 
# The result is a .gif file that will be saved under the name specified
# as the 2nd argument. 
animate_edge_betweenness <- function(g, out_fpath) {
  require(animation)
  saveGIF(plot_eb_steps(20, g), interval = 0.5, movie.name = out_fpath)
}

# The function creates plots required for animating the edge-betweeness
# community detection process
plot_eb_steps <-function(x, g){
  
  l <- layout_with_kk(g, maxiter = 1000)
  ebc <- cluster_edge_betweenness(g)
  
  colbar <- rainbow(6)
  colbar2 <- c(rainbow(5), rep("black",15))
  
  for (i in 1:x) {
    g2 <- delete.edges(g, ebc$removed.edges[seq(length=i-1)])
    eb <- edge.betweenness(g2)
    cl <- clusters(g2)$membership
    q <- modularity(g, cl)
    E(g2)$color <- "grey"
    E(g2)[ order(eb, decreasing=TRUE)[1:5] ]$color <- colbar2[1:5]
    
    E(g2)$width <- 1
    E(g2)[ color != "grey" ]$width <- 2
    
    plot(g2, layout=l, vertex.size=12,
         edge.label.color="red", vertex.color=colbar[cl+2],
         edge.label.font=2)
    title(main=paste("Q=", round(q,3)), font=2)
    ty <- seq(1,by=-strheight("1")*1.5, length=20)
    text(-1.3, ty, adj=c(0,0.5), round(sort(eb, dec=TRUE)[1:20],2),
         col=colbar2, font=2)
  }
}
