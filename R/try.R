library(cluster)

fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
}

fris_compact_one <-function (clus, dm=NULL) {
	if (is.null(dm)) {
		dm = as.matrix(clus$diss)
	}
	
	cl = clus$clustering
	lcl = length(cl)
	s = 0.
	for (i in 1:lcl) {
		rho_self = NULL
		rho_alien = NULL
		clus_i = cl[i]
		for (j in 1:lcl) {
			if (i != j) {
				clus_j=cl[j]
				if (clus_i == clus_j) {
					if (is.null(rho_self)) {
						rho_self = dm[i,j]
					} else {
						rho = dm[i,j]
						if (rho_self > rho) {
							rho_self=rho
						}
					}
				} else {
					if (is.null(rho_alien)) {
						rho_alien = dm[i,j]
					} else {
						rho = dm[i,j]
						if (rho_alien > rho) {
							rho_alien=rho
						}
					}
				}
			}
		}
		f = fris(rho_self, rho_alien)
		s = s + f
	}
	s / lcl
}

test <- function () {
	t<-fanny(ruspini,5)
	cpt <- fris_compact_one(t)

cpt
}

