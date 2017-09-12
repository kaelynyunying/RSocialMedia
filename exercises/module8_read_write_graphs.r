# export igraph to other formats
write_graph(dolphin, "dolphin.graphml", format="graphml")


################ common formats ###############
# edgelist > simple text file with one edge per line
# pajek > popular windows program for network analysis
# gml > graph modelling language is a common text based open format
# graphml > graph markup language is an XML based open format
# dot > format used by GraphViz

# Gephi > to export to GEXF fformat use the rgexf package
#################################################


# reading in a graph object
dolphin2=read_graph("dolphin.graphml", format="graphml")



# read the graphs in the graphml_files folder and visualize it
# one example is done below

library(igraph)

g=read_graph(file.choose(), format="graphml")
g
plot.igraph(g)   # hard to see anything


E(g)$arrow.size<-0.2
lo=layout_in_circle(g)
plot.igraph(g, layout=lo)


lo1=layout_as_star(g)
plot.igraph(g, layout=lo1)


lo2=layout_as_tree(g)
plot.igraph(g, layout=lo2)

