library(tidyverse)
library(dm)
library(lpSolve)
x = rio::import("./data/dummy.xlsx")
x = split(x, factor(x$staff))
x = x$David
y=schedule(x)
#You can do dependencies for staff across projects.
z = y$loading_table %>% group_by(week) %>% summarise(loading = sum(loading)) %>% ungroup()
plot(z$week, z$loading, type = 's')
