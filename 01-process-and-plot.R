## --------------------------------------------------------------
## load packages
## --------------------------------------------------------------

# moved these to be together at the top this way, if folks need to see if they
# can run the script they can check all the dependencies here
library(tidyverse)
library(readxl)
library(igraph)  # for processing
library(ggraph)  # for visualizing
# library(dplyr) # loaded by tidyverse 
# library(tidyr) # same

## --------------------------------------------------------------
## read in data
## --------------------------------------------------------------

d <- read_excel("group-nominations.xlsx")
names(d) <- c("sender", "receiver")

students <- unique(d$sender)  # complete list of students

## --------------------------------------------------------------
## create edgelist
## see https://stackoverflow.com/posts/43409986/revisions
## --------------------------------------------------------------

d2 <- d %>% 
    mutate(receiver = strsplit(receiver, split = ", ")) %>%
    unnest(receiver) %>%
    select(sender, receiver)

# create edgelist with weight via rank_prop
d3 <- d %>% 
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

el <- as.matrix(d2) # el = "edgelist"

## --------------------------------------------------------------
## create a graph
## see http://igraph.org/r/doc/aaa-igraph-package.html
## --------------------------------------------------------------

g <- graph_from_edgelist(el, directed = TRUE)

# check this function out 
# doesn't require you to coerce to a matrix and treats every additional column as a vertex attribute
g2 <- graph_from_data_frame(d3, directed = TRUE)

# here's in-degree simply using the number of relations
# we'll need to (manually?) calculate our version using rank prop
node_size <- degree(g2, mode = "in")

# plot.igraph(g)  # ugly plot, shouldn't use

## --------------------------------------------------------------
## add plotting function
## see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## --------------------------------------------------------------

# ggraph(g, layout = 'kk', maxiter=1) +
#    geom_edge_link(alpha=.25) +
#    geom_node_point()
# 
# ggraph(g, layout = 'linear', circular=TRUE) + 
#     geom_edge_arc(alpha=.25) + 
#     geom_node_point() +
#     coord_fixed()
# 
# ggraph(g, layout = 'linear', circular=TRUE) + 
#     geom_edge_link(alpha=.25,
#         aes(start_cap = label_rect(node1.name),
#         end_cap = label_rect(node2.name)),
#         arrow = arrow(length = unit(2, 'mm'))) + 
#     geom_node_text(aes(label = name))

# Version 1.0 (Bret)
#ggraph(g, layout = 'linear', circular=TRUE) + 
#    geom_edge_arc(alpha=.25,
#                  aes(start_cap = label_rect(node1.name),
#                      end_cap = label_rect(node2.name)),
#                  arrow = arrow(length = unit(1, 'mm'))) + 
#    geom_node_text(aes(label = name))

# Version 2.0 (Josh)
# I think my widths aren't working...
#ggraph(g2, layout = 'linear', circular=TRUE) + 
#    geom_edge_arc(alpha = .25,
#                   aes(edge_width = rank_prop,
#                       start_cap = label_rect(node1.name),
#                       end_cap = label_rect(node2.name)),
#                   arrow = arrow(length = unit(1, 'mm'))) + 
#    geom_node_text(aes(label = name)) +
#    scale_edge_width(range = c(1, 1.5)) +
#    theme_graph() +
#    theme(legend.position = "none")

# Version 3.0 (Bret) # circular layout
ggraph(g2, layout = 'linear', circular=TRUE) + 
    geom_edge_arc(alpha=.5, # changes darkness of lines
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

# Version 4.0 (Bret) # default layout, shows centrality
ggraph(g2, layout = 'nicely') + 
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
