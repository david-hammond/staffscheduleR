library(tidyverse)
library(dm)
library(lpSolve)
x = rio::import("./data/dummy.xlsx")
y = split(y, factor(y$staff))
y = y$Briahna
y=schedule(y)
y$loading_chart
plot(y$gantt)
#You can do dependencies for staff across projects.
z = y$loading_table %>% group_by(week) %>% summarise(loading = sum(loading)) %>% ungroup()
plot(z$week, z$loading, type = 's')
