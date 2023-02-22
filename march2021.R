vertices <- read.csv("gc_nodes.csv")
edges <- read.csv("gc_edges.csv")
graph_gc <- graph_from_data_frame(d = edges, vertices = vertices, directed = TRUE)
library(linkcomm)
edg_linkcomm <- get.edgelist(graph_gc, names=TRUE)
lc <- getLinkCommunities(edg_linkcomm, hcmethod = "single", directed = TRUE)
cc <- getCommunityCentrality(lc, type = "commweight")
sort(cc, decreasing = T)[1:10]




## REPLICATION
vert_linkcomm <- read.csv("finalvertices.csv")
vert_linkcomm <- vert_linkcomm[,-1]
edges_linkcomm <- read.csv("linkcomm_lastgraph_edges_date.csv", sep = ";")
cat <- read.csv("categories.csv")
names(cat)[1] <- "Id"
vertices <- vert_linkcomm %>% left_join(cat, by = "Id")

grafo <- graph_from_data_frame(d = edges_linkcomm, vertices = vertices, directed = TRUE)

s1 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date<=1970], del=T)
s3 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 1972:1991], del=T)
s5 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 1992:2001], del=T)
s7 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2002:2011], del=T)
s8 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2012:2020], del=T)

s1_diversity <- myfunction(s1, V(s1)$categ, directed = T)
s1_diver <- data.frame(node = V(s1)$name,
                       size = V(s1)$cc,
                       categ = V(s1)$categ,
                       range_grade = s1_diversity,
                       stringsAsFactors = F)
head(s1_diver[order(s1_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
