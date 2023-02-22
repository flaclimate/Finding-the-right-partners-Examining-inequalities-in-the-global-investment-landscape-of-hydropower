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
library(CINNA)
library(stringr)
library(linkcomm)

graph_gc <- read.graph("gc.gml", format = "gml")

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
     nodes = "International Finance Corp", margin = -2)
plotLinkCommGraph(lc, clusterids = 1: length(lc$clusters), nodes = "International Finance Corp",vsize = 1.15, vshape = "circle", 
                  layout = layout.sphere, pal = brewer.pal(7, "Set1"), shownodesin = 0, node.pies = TRUE, vlabel.cex = 1)
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
orderCommunities(lc, clusterids = 1:length(lc$clusters), verbose = TRUE)
plotLinkCommMembers(lc, nodes = c("International Finance Corp", "Kansai Electric Power Co Inc/The"),
                                       pal = brewer.pal(11, "Spectral"), shape = "rect", total = TRUE,
                                       fontsize = 11, nspace = 3.5, maxclusters = 15)
corLinkcommCentrality(lc, centrality = "degree", type = "commweight",
                      method = "spearman", plot = TRUE, pch = 20)
plotLinkCommMembers(lc, nodes = c("International Finance Corp", "Nachtigal Hydro Power Co",
                                  "Kansai Electric Power Co Inc/The", 
                                  "Sumitomo Mitsui Banking Corp", 
                                  "Asian Development Bank", "BNP Paribas SA",
                                  "World Bank Group/The", "Bujagali Energy Ltd",
                                  "CDC Group PLC"),
                    pal = brewer.pal(12, "Set3"), shape = "rect", total = FALSE,
                    fontsize = 12, nspace = 10.5, maxclusters = 25)
plotLinkCommMembers(lc, nodes = get.shared.nodes(lc, c(1)),
                    pal = brewer.pal(11, "Spectral"), shape = "rect", total = F,
                    fontsize = 11, nspace = 3.5, maxclusters = 15)
cmod <- getCommunityConnectedness(lc, conn = "modularity")
cr = getClusterRelatedness(lc)
ld <- LinkDensities(lc)

plotLinkCommGraph(lc, clusterids = 1: length(lc$clusters), nodes = c("International Finance Corp", "Kansai Electric Power Co Inc/The"),
                  vsize = 1.05, 
                  vshape = "rectangle", 
                  layout = layout.fruchterman.reingold, pal = brewer.pal(7, "Set1"), scale.vertices = 1,
                  shownodesin = 0, node.pies = TRUE, vlabel.cex = 1, 
                  vlabel = T)

plotLinkCommGraph(lc, clusterids = 1: length(lc$clusters), 
                  vsize = 1.05, 
                  vshape = "rectangle", 
                  layout = "spencer.circle", pal = brewer.pal(7, "Set1"), scale.vertices = 0.5,
                  shownodesin = 0, node.pies = TRUE, vlabel.cex = 1, 
                  vlabel = F)

plotLinkCommGraph(lc, clusterids = c(1,2,3,5,9,12,13,14,17,23,24,32,34,43,65,68,75,78,80,84,87,88,93,95), 
                  vsize = 1.05, 
                  vshape = "rectangle", 
                  layout = "spencer.circle", pal = brewer.pal(7, "Set1"), scale.vertices = 0.5,
                  shownodesin = 0, node.pies = TRUE, vlabel.cex = 1, 
                  vlabel = F)
plotLinkCommGraph(lc, clusterids = 18, 
                  vsize = 1.05, 
                  vshape = "rectangle", 
                  layout = "spencer.circle", pal = brewer.pal(7, "Set1"), scale.vertices = 0.5,
                  shownodesin = 0, node.pies = TRUE, vlabel.cex = 1, 
                  vlabel = F)


plotLinkCommMembers(lc, nodes = get.shared.nodes(lc, c(87)),
                    pal = brewer.pal(11, "Spectral"), shape = "rect", total = F,
                    fontsize = 9, nspace = 3.5, maxclusters = 15)


## Include label debt and equity
hydrodata <- read.csv("Hydro_OrbisReady.csv", sep = ";")
hydrodata <- hydrodata[,-6]
hydrodata <- hydrodata[,-4]
names(hydrodata)[1] <- "Id"
finalvert <- read.csv("linkcomm_lastgraph_vertices.csv")
cat <- read.csv("categories.csv")
names(cat)[1] <- "Id"
vertices <- finalvert %>% left_join(cat, by = "Id")

#finalvertices <- finalvert %>% left_join(hydrodata, by = "Id")
# I worked offline to add dates
edges_linkcomm <- read.csv("linkcomm_lastgraph_edges_date.csv", sep = ";")
# Create the final graph
finalvertices <- read.csv(file = "finalvertices.csv")
finalvertices <- finalvertices[,-1]
grafo <- graph_from_data_frame(d = edges_linkcomm, vertices = finalvertices, directed = TRUE)
## WORKING ON THE FINAL GRAPH ("grafo.gml")

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

# Subset: create graph before 1971
s1 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date<=1970], del=T)
s3 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 1972:1991], del=T)
s5 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 1992:2001], del=T)
s7 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2002:2011], del=T)
s8 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2012:2020], del=T)

range_diversity <- myfunction(grafo, V(grafo)$categ, directed = T)
vert_diver <- data.frame(node = V(grafo)$name,
                         size = V(grafo)$cc,
                         categ = V(grafo)$categ,
                         range_grade = range_diversity,
                         stringsAsFactors = F)
head(vert_diver[order(vert_diver$range_grade, decreasing = T), 
                c("node", "size", "range_grade", "categ")])
s1_diversity <- myfunction(s1, V(s1)$categ, directed = T)
s1_diver <- data.frame(node = V(s1)$name,
                       size = V(s1)$cc,
                       categ = V(s1)$categ,
                       range_grade = s1_diversity,
                       stringsAsFactors = F)
head(s1_diver[order(s1_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])

s3_diversity <- myfunction(s3, V(s3)$categ, directed = T)
s3_diver <- data.frame(node = V(s3)$name,
                       size = V(s3)$cc,
                       categ = V(s3)$categ,
                       range_grade = s3_diversity,
                       stringsAsFactors = F)
head(s3_diver[order(s3_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s5_diversity <- myfunction(s5, V(s5)$categ, directed = T)
s5_diver <- data.frame(node = V(s5)$name,
                       size = V(s5)$cc,
                       categ = V(s5)$categ,
                       range_grade = s5_diversity,
                       stringsAsFactors = F)
head(s5_diver[order(s5_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s7_diversity <- myfunction(s7, V(s7)$categ, directed = T)
s7_diver <- data.frame(node = V(s7)$name,
                       size = V(s7)$cc,
                       categ = V(s7)$categ,
                       range_grade = s7_diversity,
                       stringsAsFactors = F)
head(s7_diver[order(s7_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])
s8_diversity <- myfunction(s8, V(s8)$categ, directed = T)
s8_diver <- data.frame(node = V(s8)$name,
                       size = V(s8)$cc,
                       categ = V(s8)$categ,
                       range_grade = s8_diversity,
                       stringsAsFactors = F)
head(s8_diver[order(s8_diver$range_grade, decreasing = T), 
              c("node", "size", "range_grade", "categ")])

## COMPUTE CENTRALITY OVERTIME ON EVERY SINGLE NETWORK
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

## I BREAK THE GRAPH IN SMALLER TIMESLICES
grafo1 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2000:2005], del=T)
grafo2 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2006:2011], del=T)
grafo3 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2012:2017], del=T)
grafo4 <- subgraph.edges(grafo, E(grafo)[E(grafo)$date %in% 2018:2020], del=T)
## COMPUTE CENTRALITY
edg_grafo4_linkcomm <- get.edgelist(grafo4, names=TRUE)
lc_grafo4 <- getLinkCommunities(edg_grafo4_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_grafo4, type = "commweight")
sort(cc_score, decreasing = T)[1:10]
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
grafo4_diversity <- myfunction(grafo4, V(grafo4)$categ, directed = T)
grafo4_diver <- data.frame(node = V(grafo4)$name,
                           size = V(grafo4)$cc,
                           categ = V(grafo4)$categ,
                           range_grade = grafo4_diversity,
                           stringsAsFactors = F)
head(grafo4_diver[order(grafo4_diver$range_grade, decreasing = T), 
                  c("node", "size", "range_grade", "categ")])
grafo4_finalvertices <- grafo4_diver %>% left_join(cc_score, by = "node")
grafo4_finalvertices$date <- "2018-2020"

edg_grafo3_linkcomm <- get.edgelist(grafo3, names=TRUE)
lc_grafo3 <- getLinkCommunities(edg_grafo3_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_grafo3, type = "commweight")
sort(cc_score, decreasing = T)[1:10]
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
grafo3_diversity <- myfunction(grafo3, V(grafo3)$categ, directed = T)
grafo3_diver <- data.frame(node = V(grafo3)$name,
                           size = V(grafo3)$cc,
                           categ = V(grafo3)$categ,
                           range_grade = grafo3_diversity,
                           stringsAsFactors = F)
head(grafo3_diver[order(grafo3_diver$range_grade, decreasing = T), 
                  c("node", "size", "range_grade", "categ")])
grafo3_finalvertices <- grafo3_diver %>% left_join(cc_score, by = "node")
grafo3_finalvertices$date <- "2012-2017"


edg_grafo2_linkcomm <- get.edgelist(grafo2, names=TRUE)
lc_grafo2 <- getLinkCommunities(edg_grafo2_linkcomm, hcmethod = "single", directed = TRUE)
cc_score <- getCommunityCentrality(lc_grafo2, type = "commweight")
sort(cc_score, decreasing = T)[1:10]
cc_score <- as.data.frame(cc_score)
cc_score$node <- rownames(cc_score)
grafo2_diversity <- myfunction(grafo2, V(grafo2)$categ, directed = T)
grafo2_diver <- data.frame(node = V(grafo2)$name,
                           size = V(grafo2)$cc,
                           categ = V(grafo2)$categ,
                           range_grade = grafo2_diversity,
                           stringsAsFactors = F)
head(grafo2_diver[order(grafo2_diver$range_grade, decreasing = T), 
                  c("node", "size", "range_grade", "categ")])
grafo2_finalvertices <- grafo2_diver %>% left_join(cc_score, by = "node")
grafo2_finalvertices$date <- "2006-2011"

merged_smaller_overtime <- rbind(grafo4_finalvertices, grafo3_finalvertices, grafo2_finalvertices)

ggplot(merged_smaller_overtime, aes(x=date, y=range_grade, size = cc_score)) +
  geom_point(alpha=0.7)

merged_smaller_overtime %>%
  arrange(desc(cc_score)) %>%
  ggplot(aes(x=range_grade, y=date, size = cc_score, color=categ)) +
  geom_point(alpha=0.5) 

hydrodata <- read.csv("Hydro_OrbisReady.csv", sep = ";")
hydrodata <- hydrodata[,-6]
hydrodata <- hydrodata[,-4]
names(hydrodata)[1] <- "Id"
finalvert <- read.csv("linkcomm_lastgraph_vertices.csv")
finalvertices <- finalvert %>% left_join(hydrodata, by = "Id")
vertices_complete <- read.csv("finalvertices_country.csv", sep = ";")
vertices_complete <- data.frame(vertices_complete[,-(1:12)])
names(vertices_complete)[1] <- "Region"
vertex <- read.csv("finalvertices_country.csv", sep = ";")
vertex <- vertex[,-(1:8)]
vertex <- data.frame(vertex[,-(3:5)])
vertices_complete <- cbind(finalvertices, vertices_complete)
vertices_complete <- cbind(vertices_complete, vertex)

library(viridis)
library(hrbrthemes)
vertices_complete %>%
  ggplot(aes(x=range_grade, y=Degree, size = cc, color=categ)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  scale_size(guide = FALSE) +
  ylab("Degree") +
  xlab("Partnership Index") +
  theme_bw()
lmventr <- lm(cc ~ range_grade + Equity.Provider....Name, data = vertices_complete)
summary(lmventr)
plot(cc, pch = 16, col = "blue")
plot(lmventr$residuals, pch = 16, col = "red")

overtime <- read.csv("diversity_overtime.csv")
overtime <- data.frame(lapply(overtime, function(x) {
  gsub("Public bank", "Public Bank", x)
}))
overtime <- data.frame(lapply(overtime, function(x) {
  gsub("International and multilater actors", "International and multilateral actors", x)
}))
overtime <- data.frame(lapply(overtime, function(x) {
  gsub("Renewable energy", "Renewable Energy", x)
}))
ggplot(overtime, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6))+
  scale_color_d3() +
  theme(legend.position="right", axis.line = element_line(size = 0.6, linetype = "solid")) 
library(gganimate)
ggplot(overtime, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6))+
  scale_color_d3() +
  theme(legend.position="right", axis.line = element_line(size = 0.6, linetype = "solid"))

diversity1 <- read.csv("diversity_1.csv")
diversity1 <- diversity1[,-1]
diversity1['Time'] <- '<1970'
diversity3 <- read.csv("diversity_3.csv")
diversity3 <- diversity3[,-1]
diversity3['Time'] <- '1972-1991'
diversity5 <- read.csv("diversity_5.csv")
diversity5 <- diversity5[,-1]
diversity5['Time'] <- '1992-2001'
diversity7 <- read.csv("diversity_7.csv")
diversity7 <- diversity7[,-1]
diversity7['Time'] <- '2002-2011'
diversity8 <- read.csv("diversity_8.csv")
diversity8 <- diversity8[,-1]
diversity8['Time'] <- '2012-2020'
diversity_all <- rbind(diversity1, diversity3, diversity5, diversity7, diversity8)

diversity_all <- data.frame(lapply(diversity_all, function(x) {
  gsub("Public bank", "Public Bank", x)
}))
diversity_all <- data.frame(lapply(diversity_all, function(x) {
  gsub("International and multilater actors", "International and multilateral actors", x)
}))
diversity_all <- data.frame(lapply(diversity_all, function(x) {
  gsub("Renewable energy", "Renewable Energy", x)
}))


anim <- ggplot(diversity_all, aes(x = categ, y = range_grade, group = Time)) +
  geom_point(alpha = 2.7, aes(color = Time, size = 0.6))+
  scale_color_d3() +
  theme(axis.text.x = element_text(angle = 45), legend.position="right", 
        axis.line = element_line(size = 0.6, linetype = "solid")) +
  transition_states(categ,
                    transition_length = 4,
                    state_length = 1)
animate(
  anim + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)
anim_save("overtime_diversity", animation = last_animation())

anim2 <- ggplot(diversity_all, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6))+
  scale_color_d3() +
  theme(axis.text.x = element_text(angle = 45), legend.position="right", 
        axis.line = element_line(size = 0.6, linetype = "solid")) +
  transition_states(Time,
                    transition_length = 4,
                    state_length = 1)
animate(
  anim2 + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)

diversity_round <- read.csv("diversity_all_round.csv")
anim2 <- ggplot(diversity_round, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6))+
  scale_color_d3(palette = "category20") +
  expand_limits(y = c(0.00, 1)) +
  theme(axis.text.x = element_text(angle = 45), legend.position="right", 
        axis.line = element_line(size = 0.6, linetype = "solid")) +
  guides(size = FALSE) +
  transition_states(Time,
                    transition_length = 5,
                    state_length = 1)
animate(
  anim2 + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)

anim_save("overtime_diversity.mp4", animation = last_animation())

anim3 <- ggplot(diversity_round, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6, group = seq_along(Time)))+
  scale_color_d3(palette = "category20") +
  expand_limits(y = c(0.00, 1)) +
  theme(axis.text.x = element_text(angle = 45), legend.position="right", 
        axis.line = element_line(size = 0.6, linetype = "solid")) +
  guides(size = FALSE) +
  transition_states(Time,
                    transition_length = 5,
                    state_length = 1)
animate(
  anim3 + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)
anim_save("overtime_diversity2.mp4", animation = last_animation())

anim4 <- ggplot(diversity_round, aes(x = Time, y = range_grade, group = categ)) +
  geom_point(alpha = 2.7, aes(color = categ, size = 0.6, group = seq_along(Time)))+
  scale_color_d3(palette = "category20") +
  expand_limits(y = c(0.00, 1)) +
  theme(axis.text.x = element_text(angle = 45), legend.position="right", 
        axis.line = element_line(size = 0.6, linetype = "solid")) +
  guides(size = FALSE) +
  transition_states(Time,
                    transition_length = 5,
                    state_length = 1) +
  shadow_mark()

animate(
  anim4 + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)

anim_save("overtime_diversity3.mp4", animation = last_animation())


## Change diversity overtime new figure 
library(ggsci)
median_over <- read.csv("medianValues_diversity_overtime.csv")
ggplot(data = median_over, aes(x = reorder(Type.of.actor, +Value), y = Value, fill = Time)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme(axis.title.y = element_blank()) +
  scale_fill_lancet() +
  coord_flip()

edges_date <- read.csv("edges_date.csv")
cat <- read.csv("categories.csv")
cat <- data.frame(lapply(cat, function(x) {
  gsub("Public bank", "Public Bank", x)
}))
cat <- data.frame(lapply(cat, function(x) {
  gsub("International and multilater actors", "International and multilateral actors", x)
}))
cat <- data.frame(lapply(cat, function(x) {
  gsub("Renewable energy", "Renewable Energy", x)
}))
cat <- data.frame(lapply(cat, function(x) {
  gsub("Commercial Bank", "Commercial bank", x)
}))

source_edge <- as.data.frame(edges_date[,-(2)])
names(source_edge)[1] <- "name"
target_edge <- as.data.frame(edges_date[,-1])
target_edge <- as.data.frame(target_edge[,-2])
names(target_edge)[1] <- "name"

source_ed <- source_edge %>% left_join(cat, by = "name")
target_ed <- target_edge %>% left_join(cat, by = "name")
names(target_ed)[3] <- "Categ_target"
categories_ed_year <- cbind(source_ed, target_ed)
select_cat <- categories_ed_year %>% select(2,4,7)
#select_cat <- as.matrix(select_cat)
ggplot(select_cat, aes(x=Categ, y = Categ_target, size = date)) +
  geom_point()

## Final figure
vertices_forfigure <- read.csv("vertices_forfigure.csv")
names(vertices_forfigure)[13] <- "Type of actor"
names(vertices_forfigure)[8] <- "Community Centrality"
names(vertices_forfigure)[14] <- "Partnership Index"
ggplot(vertices_forfigure, aes(x=`Partnership Index`, y = Region, size =`Community Centrality`, color =`Type of actor`)) +
  geom_point(alpha=0.5) +
  scale_color_d3("category20")

grafo <- read.graph("grafofinale_rev.graphml", format = "graphML")
# install devtools
install.packages("devtools")

# load devtools
library(devtools)

# install arcdiagram
install_github('arcdiagram', username='gastonstat')

# load arcdiagram
library(arcdiagram)
# get edgelist
edgelist = get.edgelist(grafo)
# get vertex labels
vlabels = get.vertex.attribute(grafo, "name")
# get vertex groups
vgroups = get.vertex.attribute(grafo, "categ")

# get vertex degree
degrees = get.vertex.attribute(grafo, "range_grade")
# get cc
centr = get.vertex.attribute(grafo, "cc")
# get edges date
values = get.edge.attribute(grafo, "date")
library(reshape)
x = data.frame(vgroups, degrees, vlabels, ind = 1:vcount(grafo))
y = arrange(x, desc(vgroups), desc(degrees))
new_ord = y$ind
# plot arc diagram
arcplot(edgelist, show.nodes = F)

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(igraph)
library(ggraph)
library(colormap)
options(ggrepel.max.overlaps = Inf)
vertices <- as_data_frame(grafo, what = "vertices")


ggraph(grafo) + 
  geom_edge_link(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=id), repel = TRUE, size=8, color="#69b3a2") +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(rep(2,4), "cm")
  ) 

ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  geom_node_text( aes(label=name), repel = TRUE, size=8, color="#69b3a2", nudge_y=-0.1) +
  theme_void() +
  theme(
    legend.position="none")

vgroups = get.vertex.attribute(grafo, "categ")

mycolor <- colormap(colormap=colormaps$magma, nshades=max(as.numeric(as.factor(vertices$categ))))
mycolor = scale_color_d3("category20")

c( as.character(grafo$source), as.character(grafo$target)) %>%
  as_tibble() %>%
  summarise(n=n()) -> coauth
colnames(coauth) <- c("name", "n")

ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="grey", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point(color = as.numeric(as.factor(vertex_attr(grafo, "categ")), 
                                     size=vertex_attr(grafo, "range_grade")*100)) +
  scale_size_continuous(range=c(0,1)) +
  geom_node_text( aes(label=name), repel = T, 
                  size=as.numeric(vertex_attr(grafo, "range_grade")*5), angle=65, hjust=1, color="black", nudge_y=-0.1) +
  scale_color_manual(values=mycolor) +
  theme_void() +
  theme(
    legend.position="none") +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 

#This is the ultimate right one
ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="grey", edge_alpha=0.3, edge_width=0.2) +
  geom_node_point(aes(size=vertex_attr(grafo, "range_grade")*100, color = as.factor(vertex_attr(grafo, "categ")), 
                                     fill = as.factor(vertex_attr(grafo, "categ")))) +
  geom_node_text(aes(label=name), repel = T, 
                  size=as.numeric(vertex_attr(grafo, "cc")), angle=65, hjust=1, color="black", nudge_y=-0.1) +
  scale_color_manual(values=mycolor) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position="none") +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 

ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="grey", edge_alpha=0.3, edge_width=0.2, fold = T) +
  geom_node_point(aes(size=vertex_attr(grafo, "cc")*100, color = as.factor(vertex_attr(grafo, "categ")), 
                      fill = as.factor(vertex_attr(grafo, "categ")))) +
  geom_node_text(aes(label=name), repel = T, 
                 size=as.numeric((vertex_attr(grafo, "range_grade")>0.81)*3), angle=75, hjust=1, color="black", 
                 nudge_y=-0.1, nudge_x = 0.1) +
  scale_color_manual(values=mycolor) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position="none", 
    plot.margin=unit(c(0,0,0.4,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")) +
  expand_limits(x = c(-2.2, 2.2), y = c(-7.6, 1.2))

ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="grey", edge_alpha=0.3, edge_width=0.2, fold = T) +
  geom_node_point(aes(size=vertex_attr(grafo, "cc")*100, color = as.factor(vertex_attr(grafo, "categ")), 
                      fill = as.factor(vertex_attr(grafo, "categ")))) +
  geom_node_text(aes(label=name), repel = T, 
                 size=as.numeric((vertex_attr(grafo, "range_grade")>0.81)*6), angle=75, hjust=1, color="black", 
                 nudge_y=-0.1, nudge_x = 0.1) +
  scale_color_d3("category20") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position="none", legend.text = element_text(colour="black", size=16),
    plot.margin=unit(c(0,0,1,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")) +
  expand_limits(x = c(-2.2, 2.2), y = c(-7.6, 1.2))

par(mar = c(8, 5, 5, 5))

ggraph(grafo, layout="linear") + 
  geom_edge_arc(edge_colour="grey", edge_alpha=0.3, edge_width=0.2, fold = T) +
  geom_node_point(aes(size=vertex_attr(grafo, "cc")*1000, color = as.factor(vertex_attr(grafo, "categ")), 
                      fill = as.factor(vertex_attr(grafo, "categ")))) +
  geom_node_text(aes(label=name), repel = T, check_overlap = T,
                 size=as.numeric((vertex_attr(grafo, "range_grade")>=0.81)*4), angle=75, hjust=1, color="black", 
                 nudge_y=-0.1, nudge_x = -0.1) +
  scale_color_manual(values=mycolor) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position="top", 
    plot.margin=unit(c(0,0,1,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")) +
  expand_limits(x = c(-2.2, 2.2), y = c(-9.6, 1.2))

##TRy to work with categories and circlise instead
#colnames(categories_ed_year) <- c("from", "date", "owner", "from_cat", "to", "owner_to", "to_cat")
#adj_grafo <- as_adjacency_matrix(grafo)
select_cat_1 <- select_cat[,-3]
mat <- edgelist_to_adjmat(select_cat_1)
m <- as.matrix(mat)
colnames(m) <- rownames(m)
m <- as.data.frame(m)
connect <- m %>%
  rownames_to_column() %>%
  gather(key="key", value="value", -rowname) 
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag) 

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))
mycolor <- colormap(colormap=colormaps$magma, nshades=12)
mycolor <- viridis(20, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:12)]

chordDiagram(
  x = connect,
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE)
  }
)

## Express high central actors avoiding networks
#Work with transactions
merged_final <- read.xlsx("merged_original_projectsfiltered.xlsx") #upload final dataset
extractnodes <- merged_final %>%
  select(Equity.Provider....Name, Lead.Debt.Arranger....Name)
date_merged <- str_split_fixed(merged_final$Date.of.Close.x, "/", 3)
date_merged <- as.data.frame(date_merged)
merged_final <- cbind(merged_final, date_merged)
g <- graph.data.frame(extractnodes, directed = T) #create graph
E(g)$weight <- merged_final$Capacity.total..MW.
E(g)$year <- merged_final$V3
E(g)$country <- merged_final$Country....Name


edges_new <- as_data_frame(g, what = "edges")
library(splitstackshape)
CSL <- cSplit(
  edges_new, splitCols = "from",
  sep = ",", direction = "long")
edges_df <- setDF(CSL)
CSL <- cSplit(
  edges_df, splitCols = "to",
  sep = ",", direction = "long")
edges_df <- setDF(CSL)

edges_new <- read.csv("edges_new.csv")
coord <- read.csv("tableConvert.com_t0fc9z.csv")
names(coord)[1] <- "country"
coord <- coord[,-(2:4)]
edges_coord <- edges_new %>% left_join(coord, by = "country")
#write.csv(edges_coord, file = "edges_new.csv")

vertices_new <- as_data_frame(g, what = "vertices")

total_vert <- vertices_new %>% left_join(vertices, by="name")
total_vert <- merge(vertices_new, vertices, by = "name", all = TRUE)
g <- graph_from_data_frame(d = edges_new, vertices = total_vert, directed = T)
write.graph(g, file = "revised.graph.gml", format = "gml")

