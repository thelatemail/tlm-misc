library(igraph)

el <- read.table(text="
A B
B C
C B
C D
A C
E B
F A
X Z")


g <- graph_from_data_frame(el)
plot(g)

### define a selection of vertices, or paired from/to vertices defining edges
sel <- c("B","C")
self <- c("B","C")
selt <- c("C","D")


##############################
######## SELECT EDGES ########
##############################

## select the edges which have one of the listed vertices at either end
sg <- subgraph.edges(g, E(g)[ .inc(sel) ])
sg
plot(sg)

## select the edges which have have a start point in the selection
sg <- subgraph.edges(g, E(g)[ .from(sel) ])
sg
plot(sg)

## select the edges which have have an end point in the selection
sg <- subgraph.edges(g, E(g)[ .to(sel) ])
sg
plot(sg)

## select edges between points, in the specific direction (directed graph)
sg <- subgraph.edges(g, E(g)[ self %->% selt ])
sg
plot(sg)

## select edges between points, regardless of direction
sg <- subgraph.edges(g, E(g)[ self %--% selt ])
sg
plot(sg)


## can also just get the ends/vertices at each end of edges doing something like
ends(g, E(g)[.inc(sel)])
   ## which you can regenerate a graph from
graph_from_edgelist(ends(g, E(g)[.inc(sel)]))


#################################
######## SELECT VERTICES ########
#################################

sel <- c("A","C","E")

## Straight select to only keep the listed vertices 
## Appears to not include 'E' as it is not connected
## but it is there if you plot it.
sg <- induced_subgraph(g, V(g)[sel] )
sg
plot(sg)


## select neighbours of the selection (both in and out neighbours for directed graphs)
sg <- induced_subgraph(g, V(g)[ .nei(sel) ])
sg
plot(sg)
## A has neighbours - B C F, set is B C F
## C has neighbours - A B D, set is A B C D F
## E has neighbours - B,     set is A B C D F 
## so just E will get dropped from the main cluster, along with X and Z

## just 'in' neighbours
sg <- induced_subgraph(g, V(g)[ .innei(sel) ])
sg
plot(sg)

## just 'out' neighbours
sg <-induced_subgraph(g, V(g)[ .outnei(sel) ])
sg
plot(sg)




### more complicated selections - not sure these are useful

## select vertices in edge list, at any point
## i.e. all vertices in all edges that include A/C/E at all
sg <- induced_subgraph(g, V(g)[ .inc(E(g)[ .inc(sel) ]) ])
sg
plot(sg)

## can also specify whether at start or end of the edge
## vertices in edges starting at A/C/E
sg <- induced_subgraph(g, V(g)[ .inc(E(g)[ .from(sel) ]) ])
sg
plot(sg)

## vertices in edges ending at A/C/E
sg <- induced_subgraph(g, V(g)[ .inc(E(g)[ .to(sel) ]) ])
sg
plot(sg)

## all vertices in edges containing A/C/E, where A/C/E has an outward 'from' connection
sg <- induced_subgraph(g, V(g)[ .from(E(g)[ .inc(sel) ]) ])
sg
par(mfrow=c(1,2))
plot(g)
plot(sg)

## all vertices in edges containing A/C/E, where A/C/E has an inward 'to' connection
sg <- induced_subgraph(g, V(g)[ .to(E(g)[ .inc(sel) ]) ])
sg
par(mfrow=c(1,2))
plot(g)
plot(sg)





###############################
### DELETING EDGES/VERTICES ###
###############################


## remove vertices
sg <- g - vertices(sel)
sg <- g - V(g)[sel]
sg <- delete_vertices(g, sel)
sg
plot(sg)


## remove edges using any of the selection methods above
## or manually specify one
sg <- g - E(g)[ .to(sel) ]
sg <- g - E(g)[ self %->% selt ]
sg <- g - edges("B|C", "C|D")
sg <- delete_edges(g, c("B|C", "C|D"))
sg
plot(sg)



#############################
### ADDING EDGES/VERTICES ###
#############################

## Add edges - have to be between vertices
## that already exist, not totally new ones.
## c("A","B","C","D") will make A--B and C--D edges
## Need to add vertices first if want to add a new edge
add_edges(g, c("A", "B"))
g + edges(c("A", "B"))

## add vertices
g + vertices(c("G","H"))
add_vertices(g, 2, attr=list(name=c("G","H")))

## add vertices + join with an edge
g + vertices(c("G","H")) + edges(c("G","H"))
## or even native pipe it
g |> add_vertices(nv=2, attr=list(name=c("G","H"))) |> add_edges(edges=c("G","H"))



##################
### COMPONENTS ###
##################

## get the components (or clusters)
cmp <- components(g)

## add them back to the main object
V(g)[names(cmp$membership)]$group <- cmp$membership

data.frame(vertex = V(g)$name, group = V(g)$group)

