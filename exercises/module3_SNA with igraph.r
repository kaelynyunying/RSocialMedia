#######################################################################################
#                  Social Network Analysis using igraph
#######################################################################################


library(igraph)

sample2Edges=read.csv(file.choose())           # import sample edges
sample2Vertices=read.csv(file.choose())        # import sample vertices

# read the sample2 edges and sample2 vertices
head(sample2Edges)
head(sample2Vertices)

# we can load both edges and vertices into a dataframe structure
sample2=graph_from_data_frame(sample2Edges, vertices=sample2Vertices,
                              directed=F)

 g=sample2

g   # basic information about the igraph object

V(g)                             # list of vertices
E(g)                             # list of edges
V(g)[3]                          # accessing a particular vertex
E(g)[2]                          # accessing a particula edge
V(g)$id                          # a list of "id" vertex attributes
E(g)#weight                      # a list of "weight" edge attributes
vcount(g)                        # number of nodes in network
length(V(g))                     # as above
ecount(g)                        # number of edges
length(E(g))                     # as above
list.vertex.attributes(g)        # a list of vertex arttributes
list.edge.attributes(g)          # a list of edge attributes


# basic plotting
plot(g, vertex.shape="none", edge.width=1.5, edge.curved=.5, edge.arrow.size=0.5, asp=9/16, margin=-0.1)

#if we notice loops in the network
g2=simplify(g)
g2
is.simple(g2)   # checking if it is simple
plot(g2, vertex.shape="none", edge.width=1.5, edge.curved=.5, edge.arrow.size=0.5, asp=9/16, margin=-0.1)

########### looking at connectivity of graphs
neighbors(g2,3)   # for vertex id 3

is.connected(g2, mode="weak")   # checking for weak connections

cc=clusters(g2)                  # info about connected components
head(cc$membership)		 # whcih component each node assigned to
cc$csize                          # size of each component
cc$no                            # number of components


# subnetwork - giant components
g3=induced_subgraph(g2, which(cc$membership==which.max(cc$csize)))
head(degree(g3, mode="in"))     # node indegree
head(degree(g3, mode="out"))    # node outdegree

ind=strength(g3, mode="in")    # node indegree, using edge weights
V(g3)[order(ind, decreasing=T)[1:3]]   # top 5 nodes, based on (weighted) indegree

head(closeness(g3))		# closeness centrality
head(betweenness(g3))		# betweeness centrality
head(evcent(g3)$vector)		# eigenvector centrality


####### Network cohesion
graph.density(g3)                 # density
transitivity(g3)                  # (global) clustering coefficient - rel. frequency connected triples
reciprocity(g3, mode="default")   # number of dyads with (reciprocated) edges/number of dyads
reciprocity(g3, mode="ratio")     #total number of reciprocated edges/total number of edges


###### Community detection
wt=walktrap.community(g3)
wt
length(wt)
sizes(wt)

plot(wt, g3, vertex.shape="none", edge.width=1.5, edge.curved=.5, edge.arrow.size=0.5, asp=9/16, margin=-0.1)
