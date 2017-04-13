#edgebind test

library(igraph)
library(data.table)
library(edgebundleR)
d <- structure(list(ID = c("Event1", "GP3040", "KP1757", "GP2243",
                           "KP682", "KP1789", "KP1933", "KP1662", "KP1718", "GP3339", "GP4007",
                           "GP3398", "GP6720", "KP808", "KP1154", "KP748", "GP4263", "GP1132",
                           "GP5881", "GP6291", "KP1004", "KP1998", "GP4123", "GP5930", "KP1070",
                           "KP905", "KP579", "KP1100", "KP587", "GP913", "GP4864", "KP1513",
                           "GP5979", "KP730", "KP1412", "KP615", "KP1315", "KP993", "GP1521",
                           "KP1034", "KP651", "GP2876", "GP4715", "GP5056", "GP555", "GP408",
                           "GP4217", "GP641"),
                    Type = c("B", "A", "B", "A", "B", "B", "B",
                             "B", "B", "A", "A", "A", "A", "B", "B", "B", "A", "A", "A", "A",
                             "B", "B", "A", "A", "B", "B", "B", "B", "B", "A", "A", "B", "A",
                             "B", "B", "B", "B", "B", "A", "B", "B", "A", "A", "A", "A", "A",
                             "A", "A"),
                    Set = c(15L, 1L, 10L, 21L, 5L, 9L, 12L, 15L, 16L,
                            19L, 22L, 3L, 12L, 22L, 15L, 25L, 10L, 25L, 12L, 3L, 10L, 8L,
                            8L, 20L, 20L, 19L, 25L, 15L, 6L, 21L, 9L, 5L, 24L, 9L, 20L, 5L,
                            2L, 2L, 11L, 9L, 16L, 10L, 21L, 4L, 1L, 8L, 5L, 11L),
                    Loc = c(3L, 2L, 3L, 1L, 3L, 3L, 3L, 1L, 2L,
                            1L, 3L, 1L, 1L, 2L, 2L, 1L, 3L,
                            2L, 2L, 2L, 3L, 2L, 3L, 2L, 1L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L,
                            1L, 3L, 2L, 3L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 2L, 2L, 3L, 3L)),
               .Names = c("ID", "Type", "Set", "Loc"), class = "data.frame",
               row.names = c(NA, -48L))
# let's add Loc to our ID
d$key <- d$ID
d$ID <- paste0(d$Loc,".",d$ID)
# Get vertex relationships
sets <- unique(d$Set[duplicated(d$Set)])
rel <-  vector("list", length(sets))
for (i in 1:length(sets)) {
  rel[[i]] <- as.data.frame(t(combn(subset(d, d$Set ==sets[i])$ID, 2)))
}
rel <- rbindlist(rel)
# Get the graph
g <- graph.data.frame(rel, directed=F, vertices=d)
clr <- as.factor(V(g)$Loc)
levels(clr) <- c("salmon", "wheat", "lightskyblue")
V(g)$color <- as.character(clr)
V(g)$size = degree(g)*5
# igraph static plot
# plot(g, layout = layout.circle, vertex.label=NA)

edgebundle( g )
