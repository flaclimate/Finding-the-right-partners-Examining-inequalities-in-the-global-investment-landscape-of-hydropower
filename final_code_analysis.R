## Network analysis
rm(list=ls(all=TRUE)) # Removes all previously created variables
gc() # frees up memory resources

######## CREATING THE GRAPH: SETTING ATTRIBUTES AND COMPUTING CENTRALITIES

setwd("C:/Users/Larosa/Dropbox/UCL/inputR/")
library(tidyverse)
library(xlsx)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)
library(Hmisc)
library(plotly)
library(formattable)
library(data.table)
library(igraph)
library(parallel)
library(checkmate)
library(influential)



#The corrected edges file is here extracted from the big "merged_original_projectsfiltered.xlsx" file
hydro_edges <- read.csv("hydro_edges_nodate.csv")
#Create graph
g <- graph.data.frame(hydro_edges, directed = F)

#Assign attributes to nodes
V(g)$type <- V(g)$name %in% hydro_edges[,2]
V(g)$color <- V(g)$type
V(g)$color=gsub("Equity","red",V(g)$color)
V(g)$color=gsub("Debt","blue",V(g)$color)
V(g)$size <- degree(g)
V(g)$label.cex <- degree(g) /20

#Compute influencers
graph_vertices <- V(g) #extracting the vertices
graph_degree <- degree(g, v = graph_vertices, normalized = FALSE)
#degree centrality
graph_betweenness <- betweenness(g, v = graph_vertices, directed = F, normalized = F)
#betweenness centrality
neigh.co <- neighborhood.connectivity(graph = g,
                                      vertices = graph_vertices,
                                      mode = "all")
ci <- collective.influence(graph = g,
                           vertices = graph_vertices,
                           mode = "all", d = 3)
vertices.ivi <- ivi(graph =g, vertices = graph_vertices,
                    weights = NULL, directed = F, mode = "all",
                    loops = F, d = 3, scaled = T)

#Set new attributes
vertex_attr(g, "ci", index = V(g)) <- ci
vertex_attr(g, "ivi", index = V(g)) <- vertices.ivi

#Include nodes categorisation
vert <- igraph::as_data_frame(g, what = "vertices")
edges <- igraph::as_data_frame(g, what = "edges")
cat <- read.csv("categories.csv")
vertices <- vert %>% left_join(cat, by = "name")
date <- read.csv("hydro_edges.csv")
date <- date[,-(1:2)]
date <- as.data.frame(date)
edges <- cbind(hydro_edges, date)
g <- graph_from_data_frame(d= edges, vertices = vertices, directed = T)
#write.graph(g, file = "finalgraph.gml", format = "gml")
g <- read.graph("g.gml", format = "gml")

# GIANT COMPONENT: importing from gephi
graph_gc <- read.graph("gc.gml", format = "gml")
#gc <- giant_component_extract(g, directed = T)
#graph_gc <- gc[[1]] #extracting the igraph object

## COMMUNITY DETECTION
library(linkcomm)
edg_linkcomm <- get.edgelist(graph_gc, names=TRUE)
lc <- getLinkCommunities(edg_linkcomm, hcmethod = "single", directed = TRUE)
cc <- getCommunityCentrality(lc, type = "commweight")
sort(cc, decreasing = T)[1:10]
## CHE COSA HO FATTO? PRENDO lc$nodeclusters E QUESTI SONO I MIEI NODI. 
# POI PRENDO lc$edges  E QUESTI SONO I MIEI LINK. LI ESPORTO IN GEPHI. DA LI FACCIO IL DOWNLOAD DEI NODI
# CALCOLO LE PARTNERSHIPS E UNISCO CON LEFT_JOIN SIA IL CC CHE IL RANGE_DIVER ALLA MIA TABELLA
# NODI ESPORTATA DA GEPHI. OTTENGO 642 NODI CON 10 VARIABILI.
# POI graphc_linkcomm <- graph_from_data_frame(d = edges_lc, vertices = vertgc_linkcomm, directed = TRUE) 
# POI write.graph(graphc_linkcomm, file = "graphgc_linkcomm.gml", format = "gml")

plot(lc, type = "graph", vsize = 0.05, vshape = "circle", shownodesin = 0, node.pies = TRUE,
     clusterids = 1:4)
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
     layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
     nodes = "International Finance Corp")
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
     layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
     nodes = "Nordic Hydropower AB")
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
           layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
           nodes = "Asian Development Bank")
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
           layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
           nodes = "Export-Import Bank of China/The")
plot(lc, type = "graph", vsize = 0.03, vshape = "circle", 
     layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set2"), shownodesin = 0, node.pies = TRUE,
     nodes = "Banco Nacional de Desenvolvimento Economico e Social")
## JAPAN
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
     layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
     nodes = "Sumitomo Mitsui Banking Corp")
plot(lc, type = "graph", vsize = 0.05, vshape = "circle", 
           layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE,
           nodes = "Kansai Electric Power Co Inc/The")

getEdgesIn(lc, clusterids = 1:length(lc$clusters), nodes = "International Finance Corp", all = TRUE)
#Find where the International Finance Corporation appears

cm <- getCommunityConnectedness(lc, conn = "modularity")
cr = getClusterRelatedness(lc)
hcd <- as.dendrogram(cr)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                                cex = 0.7, col = "blue")
plot(hcd, ylim = c(1, 34), ylab = "Height", nodePar = nodePar, horiz = TRUE)
commc <- getCommunityConnectedness(lc, conn = "conn")
commc <- data.frame(commc)
commc$c <- rownames(commc)
library(forcats)
commc %>%
  mutate(c = fct_reorder(c, desc(commc))) %>%
  ggplot(aes(x=c, y=commc)) +
  theme_minimal() +
  geom_bar(stat = "identity", fill="hotpink4", alpha=.6, width=.7) +
  labs(x = "Community", y = "Connectedness") +
  theme(axis.text.x = element_text(size=7, angle=45))

ggplot(vert_un, aes(x=cc, y=range_grade, color=categ, size=Degree)) +
  geom_point() +
  theme(legend.position="none")

plotLinkCommGraph(lc, clusterids = 1:4, nodes = NULL,
                  layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set3"),
                  random = TRUE, node.pies = TRUE, pie.local = TRUE, 
                  vertex.radius = 0.05,
                  scale.vertices = 0.05, edge.color = NULL, vshape = "circle", 
                  vsize = 0.05,
                  ewidth = 3, margin = 0, vlabel.cex = 0.6, vlabel.color = "black",
                  vlabel.family = "Helvetica", vertex.color = "palegoldenrod",
                  vlabel = TRUE, col.nonclusters = "black", jitter = 0.2, circle = TRUE,
                  printids = TRUE, cid.cex = 1, shownodesin = 0, showall = FALSE)

c_meta = cutDendrogramAt(cr, cutat = 1.5)
c_meta_i = which(sapply(c_meta,length)==4)
plot(lc, type = "graph", 
     shownodesin = 0, node.pies = TRUE, vlabel.cex = 0.6, clusterids = c_meta[[c_meta_i]])
plot(lc, type = "graph", shownodesin = 0, node.pies = TRUE, vlabel=F)
sort(cm, decreasing = T)[1:50]

plot(lc, type = "graph", layout = "spencer.circle", shownodesin = 3, node.pies = T)
plot(lc, type = "members")
plot(lc, type = "summary")

names(edges)[names(edges) == "as.numeric.as.character.date_merged.V3.."] <- "date"

## Include label debt and equity
hydrodata <- read.csv("Hydro_OrbisReady.csv", sep = ";")
hydrodata <- hydrodata[,-6]
hydrodata <- hydrodata[,-4]
names(hydrodata)[1] <- "Id"
finalvert <- read.csv("linkcomm_lastgraph_vertices.csv")
finalvertices <- finalvert %>% left_join(hydrodata, by = "Id")
edges_linkcomm <- read.csv("linkcomm_lastgraph_edges_date.csv", sep = ";")
grafo <- graph_from_data_frame(d = edges_linkcomm, vertices = vert_unique, directed = TRUE)

# Subset: create graph before 1971
s1 <- subgraph.edges(graph_gc, E(graph_gc)[E(graph_gc)$date<=1970], del=T)
s3 <- subgraph.edges(graph_gc, E(graph_gc)[E(graph_gc)$date %in% 1972:1991], del=T)
s5 <- subgraph.edges(graph_gc, E(graph_gc)[E(graph_gc)$date %in% 1992:2001], del=T)
s7 <- subgraph.edges(graph_gc, E(graph_gc)[E(graph_gc)$date %in% 2002:2011], del=T)
s8 <- subgraph.edges(graph_gc, E(graph_gc)[E(graph_gc)$date %in% 2012:2020], del=T)

## NET RANGE COMPUTATION
## Function to find network range for each node in a network
## Arguments:
##  net = adjacency matrix, igraph graph, or network object
##  attr = Vector of attributes associated with each node in net
##  directed = boolean indicated if the network is directed or not
myfunction <- function(net, attr, directed = TRUE){
  require(reshape2)
  if (class(net) == "igraph") {
    net <- as_adjacency_matrix(net, sparse = F)
  }
  else {
    if(class(net) == "network") {
      net <- as.matrix.network(net)
    }
    else {
      net <- as.matrix(net)
    }
  }
  if(nrow(net) != length(attr)) {
    stop("Number of nodes must match length of attributes vector")
  }
  else {
    if (directed == TRUE){
      ns <- colnames(net)
      el <- melt(net, varnames=c("ego", "alter"), value.name = "weight")
      df <- cbind(rownames(net), attr)
      el$ego_grp <- df[match(el[,1], df[,1]), 2]
      el$alter_grp <- df[match(el[,2], df[,1]), 2]
      
      ##FINDING p_ik, the strength of connection from person i to group k
      # x_iq = sum of strength of ties for _i_ to alters in group _k_
      # x_ij = sum of strength of ties for _i_ to all alters
      
      x_ij <- sapply(colnames(net), function(x) {
        sum(el[which(el$ego==x | el$alter==x), "weight"])
      }
      )
      x_iq <- list(NULL)
      for(i in colnames(net)) {
        x_iq[[i]] <- sapply(unique(attr), function(x) {
          sum(el[which(el$ego==i & el$alter_grp==x), "weight"],
              el[which(el$alter==i & el$ego_grp==x), "weight"])
        }
        )
      }
      x_iq <- x_iq[-c(1)] #x_iq is now a list where each elements is a vector of node _i_ summed strength of tie to group _k_
      
      p_ik <- lapply(1:length(x_iq), 
                     function(x) x_iq[[x]] / x_ij[x])
      nd_i <- sapply(1:length(p_ik), 
                     function(x) 1 - sum(p_ik[[x]]^2, na.rm = F)
      )
    }
  }
}
### FAI GIRARE DA QUI GLI STESSI COMANDI MA CON IL GRAFO OTTENUTO DA
# finalvertices e edges_linkcomm che viene da linkcomm_lastgraph_edges_date.csv
range_diversity <- myfunction(graph_gc, V(graph_gc)$categ, directed = T)
vert_diver <- data.frame(node = V(graph_gc)$name,
                         size = V(graph_gc)$ci,
                         categ = V(graph_gc)$categ,
                         range_grade = range_diversity,
                         stringsAsFactors = F)
head(vert_diver[order(vert_diver$range_grade, decreasing = T), 
                c("node", "size", "range_grade", "categ")])

vert_linkcomm <- read.csv("linkcomm_lastgraph_vertices.csv")
edges_linkcomm <- read.csv("edgesgc_linkcomm.csv")
cat <- read.csv("categories.csv")
names(cat)[1] <- "Id"
vertices <- vert_linkcomm %>% left_join(cat, by = "Id")

vert_diver_01 <- vert_diver[, -(2:3)]
names(vert_diver_01)[1] <- "Id"
vertices <- vertices %>% left_join(vert_diver_01, by = "Id")
grafo <- graph_from_data_frame(d = edges_linkcomm, vertices = finalvertices, directed = TRUE)

vertex_attr(graph_gc, "diver", index = V(graph_gc)) <- range_diversity

write.graph(graph_gc, file = "graph_dec2020.gml", format = "gml")

s1_diversity <- myfunction(s1, V(s1)$categ, directed = T)
s1_diver <- data.frame(node = V(s1)$name,
                         size = V(s1)$ci,
                         categ = V(s1)$categ,
                         range_grade = s1_diversity,
                         stringsAsFactors = F)
head(s1_diver[order(s1_diver$range_grade, decreasing = T), 
                c("node", "size", "range_grade", "categ")])
s3_diversity <- myfunction(s3, V(s3)$categ, directed = T)
s3_diver <- data.frame(node = V(s3)$name,
                       size = V(s3)$ci,
                       categ = V(s3)$categ,
                       range_grade = s3_diversity,
                       stringsAsFactors = F)
head(s3_diver[order(s3_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s5_diversity <- myfunction(s5, V(s5)$categ, directed = T)
s5_diver <- data.frame(node = V(s5)$name,
                       size = V(s5)$ci,
                       categ = V(s5)$categ,
                       range_grade = s5_diversity,
                       stringsAsFactors = F)
head(s5_diver[order(s5_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s7_diversity <- myfunction(s7, V(s7)$categ, directed = T)
s7_diver <- data.frame(node = V(s7)$name,
                       size = V(s7)$ci,
                       categ = V(s7)$categ,
                       range_grade = s7_diversity,
                       stringsAsFactors = F)
head(s7_diver[order(s7_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s8_diversity <- myfunction(s8, V(s8)$categ, directed = T)
s8_diver <- data.frame(node = V(s8)$name,
                       size = V(s8)$ci,
                       categ = V(s8)$categ,
                       range_grade = s8_diversity,
                       stringsAsFactors = F)
head(s8_diver[order(s8_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])

## COMPUTE CENTRALITY OVERTIME ON EVERY SINGLE NETWORK
edg_s5_linkcomm <- get.edgelist(s5, names=TRUE)
lc_s5 <- getLinkCommunities(edg_s5_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_s5, type = "commweight")
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
s5_finalvertices <- s5_diver %>% left_join(cc_score, by = "node")
s5_finalvertices$date <- "1992-2001"

edg_s7_linkcomm <- get.edgelist(s7, names=TRUE)
lc_s7 <- getLinkCommunities(edg_s7_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_s7, type = "commweight")
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
s7_finalvertices <- s7_diver %>% left_join(cc_score, by = "node")
s7_finalvertices$date <- "2002-2011"

edg_s8_linkcomm <- get.edgelist(s8, names=TRUE)
lc_s8 <- getLinkCommunities(edg_s8_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_s8, type = "commweight")
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
s8_finalvertices <- s8_diver %>% left_join(cc_score, by = "node")
s8_finalvertices$date <- "2012-2020"
merged_overtime <- rbind(s7_finalvertices, s8_finalvertices)

together <- rbind(s5_finalvertices, s7_finalvertices, s8_finalvertices)
together <- data.frame(lapply(together, function(x) {
  gsub("Public bank", "Public Bank", x)
}))
together <- within(together, {
  categ <- as.character(categ)
  range_grade <- as.numeric(range_grade)
  cc_score <- as.numeric(cc_score)
})
library(ggplot2)
ggplot(data = together, aes(x = categ, y = range_grade, color = date)) +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  geom_point(aes(size=cc_score)) 
ggplot(together, aes(x = date, y= range_grade, group = categ, color = categ)) +
  geom_line()
gd_s8 <- s8_finalvertices %>%
  group_by(categ) %>%
  summarise(
    range_grade = mean(range_grade),
    cc_score = mean(cc_score),
    size = mean(size)
  )
gd_s8$date <- "2012-2020"
gd_s7 <- s7_finalvertices %>%
  group_by(categ) %>%
  summarise(
    range_grade = mean(range_grade),
    cc_score = mean(cc_score),
    size = mean(size)
  )
gd_s7$date <- "2002-2011"
gd_s5 <- s5_finalvertices %>%
  group_by(categ) %>%
  summarise(
    range_grade = mean(range_grade),
    cc_score = mean(cc_score),
    size = mean(size)
  )
gd_s5$date <- "1992-2001"
gd <- rbind(gd_s5, gd_s7, gd_s8)
ggplot(data = gd, aes(x = categ, y = range_grade, color = date)) +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  geom_point(aes(size=cc_score)) 
ggplot(data = gd, aes(x = cc_score, y = range_grade, color = categ)) +
  scale_color_viridis(discrete = TRUE, option = "viridis") +
  geom_point(alpha = .4) +
  geom_point(aes(size = date), data = gd) 
ggplot(gd, aes(x = date, y= range_grade, group = categ, color = categ)) +
  geom_line()

## to work on the figure with centralities
vertices_complete <- read.csv("vertices_complete.csv")
vertices_complete <- vertices_complete[,-1]
vert_unique <- vertices_complete %>% distinct(Id, .keep_all = TRUE)
vert_unique <- vert_unique[,-(8:11)]
library(viridis)
library(hrbrthemes)
vert_unique %>%
  ggplot(aes(x=range_grade, y=Degree, size = cc, color=categ)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  scale_size(guide = FALSE) +
  ylab("Degree") +
  xlab("Partnership Index") +
  theme_bw()
grafo <- graph_from_data_frame(d = edges_linkcomm, vertices = vert_unique, directed = TRUE)
#Use Cytoscape
library(RCy3)
createNetworkFromIgraph(grafo, title = "grafofinale", collection = "Hydropower Finance")

countries <- data.frame(vertices_complete$Country....Name)
names(countries)[1] <- "name"
countrcodes <- read.csv("countries_coordinates.csv")
countries_complete <- countries %>% left_join(countrcodes, by = "name")
vertices_wcodes <- cbind(vertices_complete, countries_complete)
vertices_wcodes <- vertices_wcodes[,-14]

fig_osm <- vertices_wcodes %>%
  plot_ly(
    lat = ~latitude,
    lon = ~longitude,
    marker = list(color = "red"),
    type = 'scattermapbox') 

fig_osm <- fig_osm %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -18))) 

fig_map <- vertices_wcodes %>%
  plot_ly(
    type = 'densitymapbox',
    lat = ~latitude,
    lon = ~longitude,
    coloraxis = 'coloraxis',
    radius = vertices_wcodes$cc)

fig_map <- fig_map %>%
  layout(
    mapbox = list(
      style="stamen-terrain",
      center= list(lon=180)), coloraxis = list(colorscale = "Viridis"))

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZmxhcm9zYSIsImEiOiJja2s3Mnd3dnQwOXczMndwYjk2ZHlxeTFkIn0.OJjj2Ke-iH1RnvA_srmCUw')
plot_mapbox(vertices_wcodes) %>%
  add_markers(
    x = ~longitude, 
    y = ~latitude, 
    size = ~cc, 
    color = ~categ,
    colors = "Accent",
    text = ~paste(Id, cc),
    hoverinfo = "text"
  )
#FROM HERE: corrected categories
vertices_completenew <- vertices_complete
vertices_completenew <- data.frame(lapply(vertices_completenew, function(x) {
                  gsub("Public bank", "Public Bank", x)
               }))
vertices_completenew <- vertices_completenew[,-(1:11)]
vertices_completenew <- data.frame(vertices_completenew[,-2])
names(vertices_completenew)[1] <- "categ"
vertices_complete_categ <- cbind(vertices_complete, vertices_completenew)
vertices_complete_categ <- vertices_complete_categ[,-9]
vertices_complete_categ <- vertices_complete_categ[,-11]
names(vertices_complete_categ)[12] <- "categ"
vert_un <- vertices_complete_categ %>% distinct(Id, .keep_all = TRUE)

vert_un %>%
  ggplot(aes(x=range_grade, y=Degree, size = cc, color=categ)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  scale_size(guide = FALSE) +
  ylab("Degree") +
  xlab("Partnership Index") +
  theme_bw()
vert_un %>%
  ggplot(aes(x=range_grade, y=Degree, size = cc, color=categ)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(breaks = c("Diversified", "Energy firm",
                                  "Governmental agency", "Institutional investors",
                                  "International and multilateral actors",
                                  "Non-profit", "Private utility",
                                  "Public Bank", "Public utility",
                                  "Renewable Energy", "NA",
                                  "State-owned institution"),
                       values = c("#ecf8b6", "#d4d1cd", "#dd9da6", "#d29092",
                                  "#95b2c0", "#fdb462", "#bfd76b", 
                                  "#e4d3bc", "#cfbbd0", "#bf92be", "#ccebc5", "white")) +
  scale_size(guide = FALSE) +
  ylab("Degree") +
  xlab("Partnership Index") +
  theme_bw()


names(vertices_completenew)[1] <- "category"
vertices_wcodes <- cbind(vertices_wcodes, vertices_completenew)
plot_mapbox(vertices_wcodes) %>%
  add_markers(
    x = ~longitude, 
    y = ~latitude, 
    size = ~cc, 
    color = ~category,
    colors = "Accent",
    text = ~paste(Id, cc),
    hoverinfo = "text"
  )
#Questo è il grafo giusto con le giuste categorie
grafo <- graph_from_data_frame(d = edges_linkcomm, 
                               vertices = vert_un, directed = TRUE)

## Compute partnership intensity per country
pi_intense <- read.csv("PI_intense.csv", sep = ";")
names(pi_intense)[1] <- "name"
countrcodes <- read.csv("countries_coordinates.csv")
pi_intense_complete <- pi_intense %>% left_join(countrcodes, by = "name")
#Create a folder named "DATA" in my directory
library(maps)
require(viridis)
theme_set(
  theme_void()
)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
pi_choropl <- pi_intense_complete %>%
  select(name, PI_intensity) %>%
  rename(region = name, value = PI_intensity) %>%
  mutate(
    region = ifelse(region == "United States", "USA", region)
  ) %>%
  mutate(
    region = ifelse(region == "Cote d'Ivoire", "Ivory Coast", region)
  ) %>%
  mutate(
    region = ifelse(region == "Russian Federation", "Russia", region)
  )
pi_plot <- right_join(pi_choropl, world_map, by = "region")
ggplot(pi_plot, aes(long, lat, group = group))+
  geom_polygon(aes(fill = value), color = "white")+
  scale_fill_viridis_c(alpha = 0.5, option = "A")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat)+
  scale_fill_viridis_c(alpha = 0.5, option = "viridis")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat) + 
  scale_fill_distiller(type = "seq", palette = "BuPu", direction = 1, 
                     guide = "colourbar", aesthetics = "fill")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat) + 
  scale_fill_distiller(type = "seq", palette = "Set2", direction = 1, 
                       guide = "colourbar", aesthetics = "fill")

#Top10 partnerships activators
pi_intense10 <- read.csv("PI_top10.csv", sep = ";")
names(pi_intense10)[1] <- "name"
countrcodes <- read.csv("countries_coordinates.csv")
pi_intense_complete10 <- pi_intense10 %>% left_join(countrcodes, by = "name")
#Create a folder named "DATA" in my directory
library(maps)
require(viridis)
theme_set(
  theme_void()
)
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")
pi10_choropl <- pi_intense_complete10 %>%
  select(name, PI_intensity) %>%
  rename(region = name, value = PI_intensity) %>%
  mutate(
    region = ifelse(region == "United States", "USA", region)
  ) %>%
  mutate(
    region = ifelse(region == "Cote d'Ivoire", "Ivory Coast", region)
  ) %>%
  mutate(
    region = ifelse(region == "Russian Federation", "Russia", region)
  )
pi_plot <- right_join(pi10_choropl, world_map, by = "region")
ggplot(pi_plot, aes(long, lat, group = group))+
  geom_polygon(aes(fill = value), color = "white")+
  scale_fill_viridis_c(alpha = 0.5, option = "A")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat)+
  scale_fill_viridis_c(alpha = 0.5, option = "viridis")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat) + 
  scale_fill_distiller(type = "seq", palette = "BuPu", direction = 1, 
                       guide = "colourbar", aesthetics = "fill")

ggplot(pi_plot, aes(map_id = region, fill = value))+
  geom_map(map = pi_plot,  color = "white")+
  expand_limits(x = pi_plot$long, y = pi_plot$lat) + 
  scale_fill_distiller(type = "seq", palette = "Set2", direction = 1, 
                       guide = "colourbar", aesthetics = "fill")

library(reactable)
vert_plot <- vert_unique[, -(3:7)]
vert_plot <- vert_plot[, -1]
vert_plot <- vert_plot[, -(3:5)]
is.num <- sapply(vert_plot$range_grade, is.numeric)
vert_plot$range_grade[is.num] <- lapply(vert_plot$range_grade[is.num], round, 3)
is.num <- sapply(vert_plot$cc, is.numeric)
vert_plot$cc[is.num] <- lapply(vert_plot$cc[is.num], round, 4)

reactable(vert_plot, columns = list(
  Id = colDef(name = "Investor", align = "center", minWidth = 200),
  categ = colDef(name = "Category", align = "center", minWidth = 100),
  cc = colDef(name = "Centrality", align = "center", minWidth = 80),
  range_grade = colDef(name = "Partnership Index", align = "center", minWidth = 200)), 
  defaultSorted = list(range_grade = "desc"))
  
