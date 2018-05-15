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
names(d) <- c("sender", "receivers")

students <- unique(d$sender)  # complete list of students

## --------------------------------------------------------------
## create edgelist
## see https://stackoverflow.com/posts/43409986/revisions
## --------------------------------------------------------------

d2 <- d %>% 
    mutate(receivers = strsplit(receivers, split = ", ")) %>%
    unnest(receivers) %>%
    select(sender, receivers) # minor, consider changing receivers to receiver (singular) - I didn't make yet as this will lead to some changes below and possible confusion

# create edgelist with weight via rank_prop
d3 <- d %>% 
    mutate(receivers = strsplit(receivers, split = ", ")) %>%
    unnest(receivers) %>%
    select(sender, receivers) %>% 
    group_by(sender) %>% 
    mutate(rank = row_number()) %>% 
    arrange(sender, desc(rank)) %>% 
    mutate(rev_rank = row_number()) %>% 
    arrange(sender, rank) %>% 
    mutate(rank_prop = rev_rank/sum(rev_rank)) %>% 
    select(sender, receiver = receivers, rank, rank_prop)

el <- as.matrix(d2) # being (nit-)picky, what does el stand for?

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
degree(g2, mode = "in")

# plot.igraph(g)  # ugly plot, shouldn't use

## --------------------------------------------------------------
## add plotting function
## see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## --------------------------------------------------------------

ggraph(g, layout = 'kk', maxiter=1) +
    geom_edge_link(alpha=.25) +
    geom_node_point()
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

## Bret thinks this plot is the best:
ggraph(g, layout = 'linear', circular=TRUE) + 
    geom_edge_arc(alpha=.25,
                  aes(start_cap = label_rect(node1.name),
                      end_cap = label_rect(node2.name)),
                  arrow = arrow(length = unit(1, 'mm'))) + 
    geom_node_text(aes(label = name))

# Josh version
# I think my widths aren't working...
ggraph(g2, layout = 'linear', circular=TRUE) + 
    geom_edge_arc(alpha = .25,
                   aes(edge_width = rank_prop,
                       start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name)),
                   arrow = arrow(length = unit(1, 'mm'))) + 
    geom_node_text(aes(label = name)) +
    scale_edge_width(range = c(1, 1.5)) +
    theme_graph() +
    theme(legend.position = "none")
