
# $Id: fristdr.R 24 2009-09-24 12:36:55Z edd $

dissiCl <- c("dissimilarity", "dist")

diss.matrix <- function(m, lareas) {
    sm <- matrix(0, lareas, lareas)
    for (i in 1:lareas) {
	    for (j in 1:lareas) {
	        sm[i,j]=m[[j]][i]
	    }
    }
    full <- sm
    full[!lower.tri(full, diag = TRUE)] <- sm
    disv <- t(full)[lower.tri(full)]
    if (any(is.na(disv)))
        attr(disv, "NA.message") <- "NA-values in the dissimilarity matrix !"
    class(disv) <- dissiCl
    attr(disv, "Labels") <- 1:lareas
    attr(disv, "Size") <- lareas
    attr(disv, "Metric") <- "euclidian" # or "manhattan"
    disv
};

comp.matrix <- function(m, lareas){
	#sm=diss.matrix(m, lareas);
	sm=m
	disc <- matrix(0, lareas, lareas) 
	for (i in 1:lareas) {
		v <- sm[i,]
	    for (j in 1:lareas) {
			vv <- v[j]
	        disc[i,j]= (max(vv)-sm[i,j])/(max(vv)+sm[i,j])
	    }
    }
	disc
};

fristdr <- function(m){
	lareas=length(m[1,])
	rez=comp.matrix(m,lareas)

	return(rez)
};



