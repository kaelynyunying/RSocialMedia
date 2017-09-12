# import csv as data

library(igraph)
#import an edge list in igraph 
dataA=read.csv(file.choose())
#use the graph.data.frame() function to import into igraph format:
g=graph.data.frame(dataA,directed=FALSE)
# igraph inputs edgelist data as directed graph
plot.igraph(g)

#The first thing to note is that in igraph, verted attributes have the notation
#V(graphname)$attributename
V(g)$name

##import the attributes
dataB=read.csv(file.choose())
colnames(dataB)
V(g)$Sex=as.character(dataB$Sex[match(V(g)$name,dataB$ID)])
# This code says to create a vertex attribute called "Sex" by extracting the value of the column "Sex" in the attributes file when the ID number matches the vertex name.
V(g)$Sex


#using this, you will then assign colors to the "F" and "M" categories, and move this to a vertex #attribute named "color". 
#This is a pre-assigned vertex attribute number. When you fill this, the plot #will automatically refer to these values for setting node colors.
V(g)$color=V(g)$Sex #assign the "Sex" attribute as the vertex color
V(g)$color=gsub("F","red",V(g)$color) #Females will be red
V(g)$color=gsub("M","blue",V(g)$color) #Males will be blue
plot.igraph(g,vertex.label=V(g)$name,layout=layout.fruchterman.reingold)

#Now, let's play with the node size. Node size is set through a vertex attribute named "size". I'm going #to set up the node size as the degree centrality of the nodes.
V(g)$size=degree(g)*2 #because 1 is a small size for a node, I'm just multiplying it by 2 (can be by 5 or more)
plot.igraph(g,vertex.label=V(g)$name,layout=layout.fruchterman.reingold)


#Now, let's add the names by maching the IDs
V(g)$Label=as.character(dataB$Label[match(V(g)$name,dataB$ID)])
# This code says to create a vertex attribute called "Names" by extracting the value of the column "Label" in the attributes file when the ID number matches the vertex name.
V(g)$Label

V(g)$label=V(g)$Label
plot.igraph(g,vertex.label=V(g)$label,layout=layout.fruchterman.reingold)


###################### import weighted matrix ##########################

#import an adjmatrix in igraph
dataC=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)
m=as.matrix(dataC)

# this matrix contains weights
ig <- graph.adjacency(m, mode="undirected", weighted=TRUE)
plot(ig, edge.label=round(E(ig)$weight, 3))



##################### challenge ###########################

# import the camp file; it is an admatrix file
# plot a weighted network
# import the campttr file ; assign different colors for gender



###################### manipulating edges & vertices ################

library(igraphdata)
data(USairports)
graph_attr(USairports)

## vertex attributes
vertex_attr_names(USairports)

## edge attributes
edge_attr_names(USairports)

# sequences
V(USairports)
V(USairports)[1:5]
V(USairports)["JFK"]   # subset by name
V(USairports)[["JFK"]] # all attributes

E(USairports)["JFK" %--% "BOS"]   # edges in both directions
# all carrierd from JKF to Boston
unique(E(USairports)["JFK"%->% "BOS"]$Carrier)  # Directed edges

# returns all flights from Calafornia to New York
# we grep the state code then extract edges from CA to NY
inCal=grepl("CA$", V(USairports)$City)
inNY=grep("NY$", V(USairports)$City)
E(USairports)[V(USairports)[inCal] %->% V(USairports)[inNY]]

# the indiced subgraph creates a new graph containing selected vertices
calAirports=induced_subgraph(USairports, inCal)
calAirports

# neighbourhood > if we want to return all airports within distance d
d2Vertices=ego(USairports, nodes=c("JFK","LAX"), order=2)
JFKNet=make_ego_graph(USairports, nodes="JFK", order=2)[[1]]
