library(sna)

# read sociogram-employees-un.csv as d (note: header must be false)
d=read.csv(file.choose(), header=FALSE)
m=as.matrix(d)

############## Degree
# the degree of a vertex is a count of the number of edges attached to it

degDol=degree(m)
hist(degree(m))

############# Density
# number of exisiting edges divided by the number of possible ones
# higher density means more connected

gden(m, mode="graph")

########### connectedness
# fraction of all dygraphs(a group of 2 nodes) such that there exists an undirected path
# higher connectedness more resistant to linkfailures

connectedness(m)

########### betweeeness
# measure of the degree to which the given node lies on the shortest path between other nodes in graph

betweenness(m)
geodist(m)   # path lengths/geodisc distances


######## egocentric
# subgraph induced by a node and it's neighbours
# used to compute metrics over a local neighbourhood, especially usefull when
# dealing with large networks

ego.extract(m,6)   # returns a list containing egocentric networks centered on named vertices
                   # in this case we examine node 6

###### closeness
# rates the centrality of node by it's distance to other nodes

closeness(m)

closeness(ego.extract(m,6))


######### reciprocity
# proportion of dyads which are symmetric

grecip(m, measure="dyadic")
grecip(m, measure="edgewise")


####### mutuality
# the number of complete dyads

mutuality(m)


####### transtivity
# the total number of transitive triads

gtrans(m)







# import edgelist (use the dolphin dataset)

g=dolphin

############## Clustering
#local motif :: triangle > tell us whether your friends are likely to be friends,
#                        > tell us how often you hear the same info compared to fresh
#                          info outside the group
# > the frequency of closed triangles is known as transitvity of a network

transitivity(g, type="global")
transitivity(g, type="local")   # fraction of connected triplets through each
				# vertex that are closed

############### Paths
# sequence of vertices each connected to the next with an edge
V(g)$name

sp=shortest_paths(g, from="Cross", to="Double")
sp$vpath

############# Diameter
# largest geodesic > gives good idea of size of network
diameter(g) 

######## eigenvector and betweenness

# use the sociogram-employees
d

# we assume all relationships are reciprocal
cent=data.frame(bet=betweenness(d), eig=evcent(d))
# evcent returns lots of data associated with the EC, but we need only
# leading eigenvector
res=lm(eig~bet, data=cent)$residuals
cent=transform(cent,res=res)
# we will use residuals in the next steps

library(ggplot2)
p=ggplot(cent, aes(x=bet, y=eig, label=rownames(cent), colour=res, size=abs(res)))+ xlab("Betweenness Centrality") + ylab("Eigenvector Centrality")
# we use residuals to color and shape the points of our plot, easier to spot outliers
p + geom_text() + ggtitle("Centrality Measure")
# use the geom_text function to plot the ID rather than points

################# Calculate centrality measures ######################

library(igraph)
library(sna)

metrics=data.frame(
        deg = degree(d),    #degree
        bet=betweenness(d),  # between
        clo=closeness(d),   # closeness
        eig= evcent(d)   # eig.cent
)

metrics


################ Calculate network measures ##########################

library(igraph)
library(sna)
plot.igraph(g)


# TK plot
tkplot(g, layout=layout.fruchterman.reingold)

############# clusters
clusters(g)
no.clusters(g)

# plot the cluster   # in this case we only have one cluster
strongclusters <- clusters(g, mode="strong")$membership
plot(g, vertex.color = strongclusters)


############# cliques
cliques(g)
largest.cliques(g)
maximal.cliques(g)

# plot the cliques
a2=largest.cliques(g)
clique1 <- a2[[1]]
g2 <- induced.subgraph(graph=g,vids=clique1)
plot(g2)



############ community

library(igraph)

sample2Edges=read.csv(file.choose())           # import sample2 edges
sample2Vertices=read.csv(file.choose())        # import sample2 vertices

# read the sample2 edges and sample2 vertices
head(sample2Edges)
head(sample2Vertices)

# we can load both edges and vertices into a dataframe structure
sample2=graph_from_data_frame(sample2Edges, vertices=sample2Vertices,
                              directed=F)

g=sample2



# type 1
edge.betweenness.community(g)

# plot the edge betweeness community
a5=edge.betweenness.community(g)
length(a5)
sizes(a5)
V(g)$community=a5$membership
rain <- rainbow(14, alpha=.5)
V(g)$color <- rain[V(g)$community]

E(g)$color <- apply(as.data.frame(get.edgelist(g)), 1, 
                    function(x) ifelse(V(g)$community[x[1]] == V(g)$community[x[2]], 
                                       rain[V(g)$community[x[1]]], '#00000000'))
plot(g, vertex.size=4, vertex.label=NA, edge.color=E(g)$color)

eb=edge.betweenness.community(g, modularity=T)
dend3=as.dendrogram(eb, use.modularity=T)
plot(dend3, nodePar=list(pch=c(NA, 20)))

### see notes on community types algorithms


################### challenge ####################

set.seed(247)
library(igraph)


g1 <- erdos.renyi.game(8, 15/100)
plot.igraph(g1)

g2 <- watts.strogatz.game(1, 100, 5, 0.05)
plot.igraph(g2)


# find the node metrics and network metrics of g1 and g2
