#############################################################################################
#				          Network Basics
#
#             functions for basic manipulation with vertices and edges
##############################################################################################

library(igraph)

g=dolphin
g

# What is the shortest path between vertices A and E.

shortest_paths(g, "Feather", "Fish", output="epath")$epath[1]


# Plot the graph. The size of a vertex should depend on the number of incoming edges.

plot(g, layout=layout_nicely, vertex.size=degree(g, V(g),
"in")*15+15, vertex.label.dist=0.5, edge.arrow.size=0.5)


# Plot the distribution of vertices degrees.

plot(degree_distribution(g), main="Degree distribution",
xlab="Degree", ylab="Frequency")


# Create a heatmap from the data.

pal <- colorRampPalette(c("lightblue", "blue"))
a <- as.matrix(get.adjacency(g))
heatmap(a, Rowv=NA, Colv="Rowv", col=pal(100))


# Create graph from subset of vertices with 1 or more incoming edges and plot it. 
# Set the vertice shape to green box. The size of a vertice should depend on the number of outcoming edges.

sg <- induced_subgraph(g, V(g)[degree(g, V(g), "in") >= 1])
plot(sg, layout=layout_nicely, vertex.size=degree(sg, V(sg),
"out")*10+15, vertex.color="green", vertex.shape="square",
vertex.label.dist=0.5, edge.arrow.size=0.5)

###########################################################################################
#					                             Network Statistics
#
#                                 functions for network statistics
###########################################################################################

# A number of employees in a factory was interview on question:
# ?Do you like to work with your co-worker??. 
# Possible answers are 1 for yes and 0 for no. Each employee gave answer for each
# other employee thus creating adjacency matrix.

# Load the dataset(sociogram-employees-un) and create an unweighted directed graph 
# from the adjecancy matrix. Name the nodes as letters A to Y. 
# Set node color to yellow and shape to sphere. 
# Set the edge?s color to gray and arrow size to 0.2.

library(igraph)

# read sociogram-employees-un.csv as d (note: header must be false)
d=read.csv(file.choose(), header=FALSE)
g <- graph.adjacency(as.matrix(d), mode="directed")
V(g)$name <- LETTERS[1:NCOL(d)]
V(g)$color <- "yellow"
V(g)$shape <- "sphere"
E(g)$color <- "gray"
E(g)$arrow.size <- 0.2

# Plot the graph.
plot(g)

# Calculate network diameter, average closeness and average network betweenness
diameter(g)  # network diameter
mean(closeness(g))  # average closeness
mean(betweenness(g)) # average network betweenness


# Calculate network density and average degree.
graph.density(g)
mean(degree(g, mode="all"))

# Calculate network reciprocity and average transitivity.
reciprocity(g)
mean(transitivity(g))

# Calculate average eccentricity of the vertices (average distance between two nodes)
mean(eccentricity(g))
mean_distance(g)


# Find the hubs and plot graph with node?s size according to their hubs index.
# locate the employee with the biggest hub
hs <- hub.score(g)$vector
hs

which.max(hs) #employee with the biggest hub


# Find the authorities and plot graph with node?s size according to their authority index. 
# locate the employee is the biggest authority?
as <- authority.score(g)$vector
as

plot(g, layout=layout_nicely, vertex.size=as*20)
which.max(as) #employee is the biggest authority

# Show the nodes that make diameter. Plot these nodes larger and in red. 
# Plot edges on this path thicker in red.
diameter.nodes <- get.diameter(g)
diameter.nodes


V(g)$size <- 20
V(g)[diameter.nodes]$color <- "red"
V(g)[diameter.nodes]$size <- V(g)[diameter.nodes]$size+10
E(g)$width <- 1
E(g, path=diameter.nodes)$color <- "red"
E(g, path=diameter.nodes)$width <- 2
plot.igraph(g, layout=layout.fruchterman.reingold)

#######################################################################################
#				                  Graph Structure of Networks
#
#	                        functions for graph structure
########################################################################################

# Load the data and create un-directed graph from adjacency matrix. Name nodes as letters A to Y. 
# Set node color to orange and shape to square. 
# Set edge?s color to blue and arrow size to 0.2. Plot the graph.

library(igraph)

d <- read.csv((file.choose()), header=FALSE)
g <- graph.adjacency(as.matrix(d), mode="undirected")
V(g)$name <- LETTERS[1:NCOL(d)]
V(g)$color <- "orange"
V(g)$shape <- "square"
E(g)$color <- "blue"
E(g)$arrow.size <- 0.2
plot(g)


# Find the largest cliques in the group.
largest_cliques(g)

# How many maximal cliques are there?
maximal.cliques.count(g)


# Calculate the network cohesion.
cohesion(g)

# Find the clusters based on betweenness.
cluster_edge_betweenness(g)


# Find the components of a graph.
components(g)


# Find the loop edges.
E(g)[which_loop(g)==TRUE]


# Identify triangles in the graph? Locate triangles where vertex S included.
sum(count_triangles(g))
count_triangles(g, V(g)['S'])


# Find the global clustering coefficient of this network? Is the clustering is statistically significant for this network.
transitivity(g, type="global")
transitivity(sample_gnm(vcount(g), ecount(g)), type="global")


# Ans: No, the clustering is not significant,since clastering coefficient of network



