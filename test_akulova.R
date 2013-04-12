# test
library(cluster)
library(ftdr)
new = read.table("/home/olga/Dev/fristdr/ftdr_0.1/data/D_SRS.csv")
cl = fris_class(new, 8)

