library(tidyverse)
library(readxl)
library(janitor)

d <- read_excel("group-nominations.xlsx")

names(d) <- c("sender", "receivers")

d

## Bret editing to see what happens...
