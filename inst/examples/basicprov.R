library(histry)
set.seed(0)
x = 5L
l = 5
y = rnorm(x)
w = 10
z = y + w
k = l + mtcars$wt
provstore = histryProvDF()
library(roprov)
gr = fullprovgraph(provstore)
