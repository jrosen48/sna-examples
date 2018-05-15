## --------------------------------------------------------------
## load packages
## --------------------------------------------------------------

library(tidyverse)
library(janitor)

## --------------------------------------------------------------
## read in data
## --------------------------------------------------------------

library(readxl)

d <- read_excel("group-nominations.xlsx")
names(d) <- c("sender", "receivers")
d

students <- unique(d$sender)  # complete list of students


## --------------------------------------------------------------
## create edgelist
## see https://stackoverflow.com/posts/43409986/revisions
## --------------------------------------------------------------

library(dplyr)
library(tidyr)

d2 <- d %>% 
    mutate(receivers = strsplit(receivers, split = ", ")) %>%
    unnest(receivers) %>%
    select(sender, receivers)

el <- as.matrix(d2)

## --------------------------------------------------------------
## create a graph
## see http://igraph.org/r/doc/aaa-igraph-package.html
## --------------------------------------------------------------

library(igraph)  # for processing

g <- graph_from_edgelist(el, directed=TRUE)

# plot.igraph(g)  # ugly plot, shouldn't use


## --------------------------------------------------------------
## add plotting function
## see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## --------------------------------------------------------------
library(ggraph)  # for visualizing

ggraph(g, layout = 'kk', maxiter=1) + 
    geom_edge_link(alpha=.25) +
    geom_node_point()

ggraph(g, layout = 'linear', circular=TRUE) + 
    geom_edge_arc(alpha=.25) + 
    geom_node_point() +
    coord_fixed()

ggraph(g, layout = 'linear', circular=TRUE) + 
    geom_edge_link(alpha=.25,
        aes(start_cap = label_rect(node1.name),
        end_cap = label_rect(node2.name)),
        arrow = arrow(length = unit(2, 'mm'))) + 
    geom_node_text(aes(label = name))

## Bret thinks this plot is the best:
ggraph(g, layout = 'linear', circular=TRUE) + 
    geom_edge_arc(alpha=.25,
                   aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name)),
                   arrow = arrow(length = unit(1, 'mm'))) + 
    geom_node_text(aes(label = name))
