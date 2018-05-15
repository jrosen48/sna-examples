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
    select(sender, receivers)

el <- as.matrix(d2) # being (nit-)picky, what does el stand for?

## --------------------------------------------------------------
## create a graph
## see http://igraph.org/r/doc/aaa-igraph-package.html
## --------------------------------------------------------------

g <- graph_from_edgelist(el, directed = TRUE)

# plot.igraph(g)  # ugly plot, shouldn't use

## --------------------------------------------------------------
## add plotting function
## see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## --------------------------------------------------------------

# ggraph(g, layout = 'kk', maxiter=1) + 
#     geom_edge_link(alpha=.25) +
#     geom_node_point()
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
