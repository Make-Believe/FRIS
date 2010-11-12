# $Id: fristdr.R 24 2009-09-24 12:36:55Z edd $
library(cluster)
fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
}

fris_compact_mix <- function( i_class, j_sample, dm_mix, cl, n) {  #Оценка компактности, если в i-ом классе jый образец - единственный столп.
	kol_ss <- ncol(dm_mix) 		#Количество образцов 
	kol_st <- length(cl)		#Количество стандартных образцов 
	rez=0

	for (i in 1:kol_ss) {
		rho_self = NULL
		rho_alien = 3
		min_=NULL
		rho_min=NULL
		if (i<= kol_st){
			if (cl[i]==i_class){
				rho_self = dm_mix[i, j_sample]
				for (j in 1:kol_st){ 
				if (i !=j){
					if( cl[i] != cl[j]){
						rho=dm_mix[i,j]
						if (rho_alien > rho) {
							rho_alien=rho
						}
					}
				}
				}	
			}else{
				rho_alien = min(rho_alien, dm_mix[i, j_sample])
				for (j in 1:kol_ss) { 
				if(i!=j){
					if (j<=kol_st){
						if (cl[i] == cl[j]){
							if (is.null(rho_self)){
								rho_self = dm_mix[i,j]
							}else{
								rho=dm_mix[i,j]
								if (rho_self > rho) {
									rho_lelf=rho
								}
							}
						}
					}else{
						rho=dm_mix[i,j]
						if (rho_self > rho) {
							rho_lelf=rho
						}
					}
				}
				}	
			}
		}else{
			for (k in 1:n){
				for (j in 1:kol_st) {
				if (cl[j]==k){
					if (is.null(min_)){
						min_ = dm_mix[i,j] 
					}else{
						rho= dm_mix[i,j]
						if (min_ > rho) {
						min_=rho
					
						}
					} 
				}
				}
				rho_min[k]=min_
			}
			g=which.min(rho_min)
			if (g==i_class) {
				rho_self = dm_mix[i, j_sample]
				rho_min[g] <- rho_alien
				rho_alien = min(rho_min)
			}else{
				rho_alien = min(rho_alien, dm_mix[i, j_sample])
				for (j in 1:kol_ss) {
					
					
					if (j<=kol_st){
						if (cl[j] == i_class){
							if (is.null(rho_self)){
								rho_self = dm_mix[i,j]
							}else{
								rho=dm_mix[i,j]
								if (rho_self > rho) {
									rho_lelf=rho
								}
							}
						}
					}else{
						rho=dm_mix[i,j]
						if (rho_self > rho) {
							rho_lelf=rho
						}
					}
				}
			}
		}
		
	f = fris(rho_self, rho_alien)
	rez=rez+f
	}
	rez / length(cl)
}

fristdr_2 <- function (ruspini, n, clus, s, mix) {
	cl = clus$clustering	
	kol_st <- length(cl)									#Количество стандартных образцов 
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_ss <- ncol(dm_mix)  								#Количество образцов 
	for (j in 1:kol_ss){
			
			if (j <= kol_st) {
				#if (cl[j] == i) {
				f_mix <- append(f_mix,  fris_compact_mix_add (cl[j], s, j, dm_mix, cl, n))
				i_f <- append(i_f, j)
				#} 
			} else {
				f_mix <- append(f_mix, fris_compact_mix_add (NULL , s, j, dm_mix, cl, n))
				i_f <- append(i_f, j)	
			}
		}

}

fristdr <- function (data_st, n, clus, mix) {
	#clus <-fanny(data_st,n)	 								#Разбиение стандартных образцов на n классов
	cl = clus$clustering	
	kol_st <- length(cl)									#Количество стандартных образцов 
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_ss <- ncol(dm_mix)  								#Количество образцов 
	s=NULL
	for (i in 1:n){
		f_mix_max = NULL  	    # Максимальное значение F для столпа из класса i 								
		i_f = NULL				# Номер образца
		f_mix = NULL
		for (j in 1:kol_ss){
			
			if (j <= kol_st) {
				if (cl[j] == i) {
					f_mix <- append(f_mix,  fris_compact_mix (i, j, dm_mix, cl, n))
					i_f <- append(i_f, j)
				} 
			} else {
				f_mix <- append(f_mix, fris_compact_mix (i, j, dm_mix, cl, n))
				i_f <- append(i_f, j)	
			}
		}	
		num<- which.max(f_mix)
		f_mix_max<-i_f[num]
		s <- append(s,f_mix_max)
	}
	s
}

test<-function(){
	new <- read.table("/home/olga/Dev/fristdr/R/new.csv")
	mix<-rbind(ruspini, new)
	n=5
	clus <-fanny(data_st,n)	
	system_of_stolps <- fristdr_1(ruspini, n, clus, mix) 
	stolp_to_add <- fristdr_2(ruspini, n, clus, mix, system_of_slolps) 
	#system_of_stolps
	
}
