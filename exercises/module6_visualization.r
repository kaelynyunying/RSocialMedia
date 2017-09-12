
#######################################################################################
#				Network Visualization
#
#	                    visualizing using statnet and gplot(sna)
########################################################################################

dolphin=read.csv(file.choose(), header=T)          # dolphin edges
dolphinInfo=read.csv(file.choose(),header=T)       # dolphin vertices

dolphin=as.matrix(dolphin)
rownames(dolphin)=dolphinInfo$name
colnames(dolphin)=dolphinInfo$name

dolphin             # sna package want it in matrix form

##### creating network objects
library(statnet)
library(network)

ndolphin=network(dolphin, directed=F)
summary(ndolphin)
network.dyadcount(ndolphin)   # how many dyad in nflo
network.edgecount(ndolphin)   # how many degrees are present
network.size(ndolphin)        # how large is the network
list.edge.attributes(ndolphin)
list.vertex.attributes(ndolphin)
list.network.attributes(ndolphin) 

###### plotting
plot(ndolphin, displaylabels=T)
plot(ndolphin, displaylabels=T, mode="circle")


######### gplot
library(sna)

gplot(ndolphin)
gplot(ndolphin, displaylabels=T)


nodeColors=ifelse(dolphinInfo$Gender=="Female", "hotpink", "dodgerblue")
gplot(ndolphin, gmode="graph", displaylabels=T, label.cex=0.5, vertex.col=nodeColors, label.col="blue")


gplot(ndolphin, gmode="graph", label.cex=0.5, label.col="blue", displaylabels=T, displayisolates=F, mode="circle")


######### 3d gplot
library(rgl)

gplot3d(ndolphin, displaylabels=T)



####### plotting network statistics
degree(ndolphin) # finding degree
ideg=degree(ndolphin, cmode="indegree")
odeg=degree(ndolphin, cmode="outdegree")
all(degree(ndolphin)==ideg+odeg)


gplot(ndolphin, vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50, label.cex=0.4,
                  vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)), displaylabels=T)



bet=betweenness(ndolphin, gmode="graph")
gplot(ndolphin, vertex.cex=sqrt(bet)/25, gmode="graph")


clo=closeness(ndolphin)
gplot(ndolphin, vertex.cex=sqrt(clo)/25, gmode="graph")

###### subgraphs

dyad.census(ndolphin)
triad.census(ndolphin)
kpath.census(ndolphin, maxlen=6)
kcycle.census(ndolphin, maxlen=6)
clique.census(ndolphin)

# plot tabulation/co-membership for paths/cliques/cycles
indirect1=kpath.census(ndolphin, maxlen=6, dyadic.tabulation="sum")$paths.bydyad
gplot(indirect1, label.cex=0.4, vertex.cex=0.75, displaylabels=T, edge.col=rgb(0,0,0,0,25))

# components information
cl=component.largest(ndolphin, connected="weak") # can change to strong
gplot(ndolphin[cl,cl], boxed.lab=F, label.cex=0.5, label.col=4, label=network.vertex.names(ndolphin)[cl])


# cohesion info
gplot(ndolphin, vertex.col=2+cutpoints(ndolphin, mode="graph", return.indicator=T))

# nesting of cores
kc=kcores(ndolphin, cmode="indegree")
gplot(ndolphin, vertex.col=rainbow(max(kc)+1))

######## positional analysis

gplot(ndolphin, gmode="graph", vertex.cex=0.5)
ec=equiv.clust(ndolphin, mode="graph", plabels=network.vertex.names(ndolphin))
ec # the clustering
plot(ec)
rect.hclust(ec$cluster, h=20)


#######################################################################################
#				                 Community Visualization
#
#	                    visualizing using linkcomm and network
########################################################################################

library(linkcomm)

# import edgelist

lc=getLinkCommunities(dolphin, hcmethod="single")
print(lc)

### for directed networks
# lc=getLinkCommunities(data, directed=T, dirweight=0.8) # default is 0.5 for links that share nodes
                                                         # yet in opposite direction

# lc=getLinkCommunities(data, edglim=10) # for large networks >edlim allows to limit size of network in terms of links

################################ visualizing communities ###########################

# visualizing communities
plot(lc, type="graph", layout=layout.fruchterman.reingold)
plot(lc, type="graph", layout="spencer.circle")

# visualize key nodes
plot(lc, type="graph", layout="spencer.circle", shownodesin=3)

# visualize node membership
plot(lc, type="graph", shownodesin=3, node.pies=T)

# visualize community membership
plot(lc, type="membership")

# display summary of results
plot(lc, type="summary")

# dendogram of cummunity colored clusters
plot(lc, type="dend")


################################ analyzing link communities ###############################

cr=getClusterRelatedness(lc, hcmethod="ward")

cc=getCommunityCentrality(lc)
cm=getCommunityConnectedness(lc, conn="modularity")
plot(lc, type="commsumm", summary="modularity")

getNodesIn(lc, clusterids=c(4,5))
get.shared.nodes(lc, comms=c(3,4))

################################ OCG clusters ################################

library(network)
lesmiserables  # in built dataset

oc=getOCG.clusters(lesmiserables)
print(oc)
plot(oc, type="graph", shownodesin=7, scale.vertices=0.1)


### try got karate dataset
karate



