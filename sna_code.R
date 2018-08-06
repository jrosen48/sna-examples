## --------------------------------------------------------------
## load packages
## --------------------------------------------------------------

library(tidyverse)
library(readxl)
library(igraph)  # for processing
library(ggraph)  # for visualizing


## --------------------------------------------------------------
## read in data
## --------------------------------------------------------------

d <- read_excel("group-nominations.xlsx")
names(d) <- c("sender", "receiver")
students <- unique(d$sender)  # complete list of students


## --------------------------------------------------------------
## create edgelist
## --------------------------------------------------------------

# create edgelist with weight via rank_prop
el <- d %>%    # el = "edgelist"
    mutate(receiver = strsplit(receiver, split = ", ")) %>%
    unnest(receiver) %>%
    select(sender, receiver) %>% 
    group_by(sender) %>% 
    mutate(rank = row_number()) %>% 
    arrange(sender, desc(rank)) %>% 
    mutate(rev_rank = row_number()) %>% 
    arrange(sender, rank) %>% 
    mutate(rank_prop = rev_rank/sum(rev_rank)) %>% 
    select(sender, receiver = receiver, rank, rank_prop)


## --------------------------------------------------------------
## create a graph
## see http://igraph.org/r/doc/aaa-igraph-package.html
## --------------------------------------------------------------

# this function doesn't require you to coerce to a matrix and treats every additional column as a vertex attribute
g <- graph_from_data_frame(el, directed = TRUE)

# here's in-degree simply using the number of relations
node_size <- degree(g, mode = "in")

# plot.igraph(g)  # ugly plot, shouldn't use


## --------------------------------------------------------------
## add plotting function
## see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## --------------------------------------------------------------

# default layout (layout='nicely'), shows centrality (also consider layout='kk')
ggraph(g, layout = 'nicely') + 
    geom_edge_link(alpha=.5, # changes darkness of lines
                   aes(edge_width = rank_prop^2)) + # changes width of lines
    scale_edge_width(range = c(.5, 3)) + # possible range of line widths
    theme_graph() +
    theme(legend.position = "none") +
    geom_node_point(aes(size = node_size)) + # changes node size
    scale_size(range = c(1,10)) + # possible range of node sizes
    geom_node_label(aes(label = name),
                    repel = TRUE, # bumps labels away from nodes
                    point.padding = unit(.1, "lines"),
                    label.padding = unit(.15, "lines"),
                    label.size = .1) # only adjusts the width of label border
