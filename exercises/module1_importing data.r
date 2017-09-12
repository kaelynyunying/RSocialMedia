################### creating iGraph objects ########################
library(igraph)

# creating graph manunally
g=make_empty_graph(n=0, directed=T)
g=g + vertices(c("A","B","C"))
g=g + edges(c("A","C", "B","C"))   # an edge from A to C and B to C
g

# if we want to delete and edge
g=g - V(g)["A"]
g


# creating graph manually
g=graph_from_literal(A--B, B-+C, C-+A)
g



# we create an empty directed graph with 5 nodes. Set color of all
# nodes to yellow and shape to sphere.

g <- make_empty_graph(n=5, directed=TRUE)
V(g)$color = "yellow"
V(g)$shape = "sphere"
plot.igraph(g)

# Add the following edges to the graph: 1->2, 1->3, 2->4, 3->4, 4->5

g <- add.edges(g, c(1,2, 1,3, 2,4, 3,4, 4,5))
plot.igraph(g)

# Add a vertex to the graph, color it to red and add edges:3->6, 6->5. Set vertex shape to sphere.

g <- add.vertices(g, 1, color="red", shape="sphere")
g <- add.edges(g, c(3,6, 6,5))
plot.igraph(g)

# Replace edge 1->3 with the edge 3->1.

g <- delete.edges(g, c(2))
g <- add.edges(g, c(3,1))
plot.igraph(g)

# Name vertices with letters A-F. List all vertices and edges

V(g)$name <- LETTERS[1:6]
V(g)
plot.igraph(g)


###################  Properties ###############################

##### create a graph

g=make_ring(10, directed=T, mutual=T)
V(g)$name=LETTERS[1:10]
g=g + edges(9,5, 7,1, 1,5)
lo=layout_in_circle(g)
E(g)$arrow.size<-0.2
plot(g, layout=lo)

# vertex shape
plot(g, layout=lo, vertex.shape=c("circle","square"))

# vertex color
plot(g, layout=lo, vertex.color=c("tomato2","royalblue"))

# vertex size
plot(g, layout=lo, vertex.size=c(15,30))


# properties using attributes
V(g)$shape="circle"
V(g)$size=15
V(g)$color="orange"
plot(g, layout=lo)

# change the vowels
vowel=V(g)$name %in% c("A","E","I","O","U")
V(g)[vowel]$shape="square"
V(g)[vowel]$color="royalblue"
V(g)[vowel]$size=25
plot(g, layout=lo)


##### edge properties

E(g)$width=1
E(g)[V(g)[vowel] %--% V(g)[vowel]]$width=4
plot(g, layout=lo)

plot(g, layout=lo, edge.curved=0.3*which_mutual(g))

################### importing objects ###########################

# import edgelist as object
edgelist=read.csv(file.choose())   # choose edgelist in folder
el=as.matrix(edgelist)
g=graph_from_edgelist(el, directed=T)
g

# import agency matrix > uncheck first row as names whem importing
adjmatrix=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)   # choose adjmatrix in folder
adj=as.matrix(adjmatrix)
g=graph_from_adjacency_matrix(adj)
g


# import from data frame as df
df=as.data.frame(el)
g=graph_from_data_frame(df, directed=T)
g


################ Sample Network Structures ###################################


# 1> Random with 10 nodes and probability of an edge of 0.4 (Erdos-Renyi random graph)
g.random=sample_gnp(10, 0.4)
E(g.random)$arrow.size<-0.2
plot(g.random)


# 2> Full un-directed graph with 15 nodes
g.full=make_full_graph(15)
E(g.full)$arrow.size<-0.2
plot(g.full)


# 3> Star network with 20 nodes
g.star=graph.star(20)
E(g.star)$arrow.size<-0.2
plot(g.star)


# 4> Tree graph with 27 nodes
g.tree=make_tree(27, children=3)
E(g.tree)$arrow.size<-0.2
plot(g.tree)


# 5> Full graph
g.full=make_full_graph(n=6)
E(g.full)$arrow.size<-0.2
plot(g.full)

# 6> Directed ring network with 15 nodes and mutual edges
g.ring <- make_ring(15, directed=TRUE, mutual=TRUE)
E(g.ring)$arrow.size <- 0.2
plot(g.ring)

############################### importing and exporting data #################################

library(igraph)

dolphinEdges=read.csv(file.choose())           # import dolphin edges
dolphinVertices=read.csv(file.choose())        # import dolphin vertices

# read the dolphin edges and dolphin vertices
head(dolphinEdges)
head(dolphinVertices)

# we can load both edges and vertices into a dataframe structure
dolphin=graph_from_data_frame(dolphinEdges, vertices=dolphinVertices,
                              directed=F)


# force layouts

plot(dolphin, layout=layout_with_fr(dolphin))

#############################################
# layout_with_kk > kamada-kawai layout algorithm
#                  edge weights correspond to length of springs
#
# layout_with_fr > Fruchterman-Reingold layout algorithm
#                   edge weights correspond to strength of springs 
#
# layout_with_drl > Distributed Rcursive Layout algorithm
#                   based on VxOrd package
#
##############################################


# we can export the data frame
dolphinDFs=as_data_frame(dolphin, what="both")
str(dolphinDFs)

# export to other formats
write_graph(dolphin, "dolphin.graphml", format="graphml")


################ common formats ###############
# edgelist > simple text file with one edge per line
# pajek > popular windows program for network analysis
# gml > graph modelling language is a common text based open format
# graphml > graph markup language is an XML based open format
# dot > format used by GraphViz

# Gephi > to export to GEXF fformat use the rgexf package
#################################################