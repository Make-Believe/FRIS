# This is a cool program!!!
library(cluster)

fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
}

find_min_new <- function (i, dm_mix, l) { #Найти минимальное расстояние от элемента под номером i, до элемента под номером большем l. 
	rho_m=NULL
	for (j in (l:ncol(dm_mix))) {
		if (is.null(rho_m)) {
			rho_m = dm_mix[i,j]
		} else {
			rho = dm_mix[i,j]
			if (rho_m > rho) {
				rho_m=rho
				}
		}
	}
	rho_m
}

find_max<- function(i, dm_mix, clus_st){
	cl = clus_st$clustering
	dm = as.matrix(clus_st$diss)
	mm=NULL					 #лист минимальных расстояний от i
	for (j in length(cl)){
		m=NULL
		for (k in ncol(dm)){
			if (is.null(m)) {
				m=dm_mix[i,k]
			}else{
				rho=dm_mix[i,k]
				if (m>rho){
				m=rho
				}
			}
		}
		mm<-append(mm, m)
	}
	rez<-max(mm)
}



						

fris_compact_mix <- function (clus_st, joined,  dm=NULL) {
	if (is.null(dm)) {
	dm = as.matrix(clus_st$diss)
	}
	cl = clus_st$clustering
	dm_mix <- as.matrix(daisy(joined, metric = "euclidean"))
	l_dm_mix = ncol(dm_mix)
	s=0
	for (i in 1:l_dm_mix) {
		rho_self = NULL
		rho_alien = 3
		if (i <= length(cl)) {
				clus_i = cl[i]
				for (j in 1:length(cl)) {
					if (i != j) {			
						clus_j=cl[j]
						if (clus_i == clus_j) {
							if (is.null(rho_self)) {
								rho_self = find_min_new(i, dm_mix, length(cl))
							} else {
								rho = dm_mix[i,j]
								if (rho_self > rho) {
									rho_self=rho
								}
							}	
						} else {
							rho = dm_mix[i,j]
							if (rho_alien > rho) {
								rho_alien=rho
							}
						}
					}
				}
		f = fris(rho_self, rho_alien) # Считаем значение Fris функции для элементов верифицированной выборки
		s = s + f
		
		}else{
			for (j in 1:l_dm_mix) {
				if (i != j) {
					if (is.null(rho_self)) {
						rho_self = dm_mix[i,j]
					} else {
						rho = dm_mix[i,j]
						if (rho_self > rho) {
							rho_self=rho
						}
					}
					
					h=find_max(i, dm_mix, clus_st)
					if (rho_alien>h){
						rho_alien=h
					}
				}
		
		
			f = fris(rho_self, rho_alien) # Считаем значение Fris функции для элементов неверифицированной выборки
			s = s + f
			}	
		}
	
	}
	s / length(cl)
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
	#cpt <- fris_compact_one(t)
	n=5
	print('Hi')
	print(n)
	#joined <- read.table("/home/olga/Dev/fristdr/R/new.csv")
	#cpt_mix <- fris_compact_mix(t, joined) 
#cpt_mix

100
}
