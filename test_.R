# test
library(cluster)
library(ftdr)
d <- read.table("/home/olga/Dev/dispersive/data/line_xray/D_SRS.csv")
new <- read.table("/home/olga/Dev/dispersive/data/line_xray/new.csv")
cl <- c(1,1,1,1,2,3,3,4,5,5,6,7)
fris_stolps(d, cl, 7, new)

