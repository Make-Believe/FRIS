# $Id: fristdr.R 24 2009-09-24 12:36:55Z edd $
library(cluster)

fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
}

checking <- function (l, smth){
	f<-TRUE
	for (i in 1:length(smth)){
		for (j in 1:length(smth[[i]])){
			if (smth[[i]][j]==l){f <- FALSE}
		}
	}
	f
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
			g=which.max(rho_min)
			if (g==i_class) {
				rho_self = dm_mix[i, j_sample]
				rho_min[g] <- rho_alien
				rho_alien = max(rho_min)
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

fris_compact_mix_ss <- function (ss, dm_mix, cl, n){
	kol_ss <- ncol(dm_mix) 		#Количество образцов 
	kol_st <- length(cl)		#Количество стандартных образцов 
	rez=0
	
	for (i in 1: kol_ss){
		rho_self = NULL
		rho_alien = 3
		if (i<=kol_st){
			for (j in 1: kol_st){
				if (i!=j){
					if (cl[i]==cl[j]){
						for (k in 1:length(ss[[cl[i]]])){
							if (is.null(rho_self)){
								rho_self <- dm_mix[ss[[cl[i]]][k], i]
							}else{
								rho <- dm_mix[ss[[cl[i]]][k], i]
								if (rho_self>rho){rho_self<-rho}
							}
						}	
					}else{
						for (l in (1:length(ss))){
							if (l != cl[i]){
								for (k in 1:length(ss[[l]])){
									rho <- dm_mix[ss[[l]][k], i]
									if (rho_alien >rho){rho_alien<-rho}
								}
							}
						}
					}
				}
			}

		}else{
			check<-checking(i, ss)
			if(check==TRUE){
				
				for (l in (1:length(ss))){
					for (k in 1:length(ss[[l]])){
						if (is.null(rho_self)){
							rho_self<-dm_mix[ss[[l]][k],i]
						}else{
							rho<-dm_mix[ss[[l]][k],i]
							if(rho_self>rho){rho_self<-rho}
						}
					}
				}	
				
				rho_s<-NULL
				for (h in 1:n){
					#rho_s[h]<-NULL
					rho_min = NULL
					for (k in (1:length(ss[[h]]))){
						if (is.null(rho_min)) {
							rho_min <- dm_mix[ss[[h]][k], i]
						}else{
							rho <- dm_mix[ss[[h]][k], i]
							if (rho_min > rho){rho_s[h]<-rho}
						}
					}
					rho_s[h]=rho_min
				}
				rho_alien <- min(rho_alien, max(rho_s))
				
				
			

			}					
				
		}
	f = fris(rho_self, rho_alien)
	rez = rez + f
	}


	rez / length(cl)
	#100
}

fristdr_1 <- function (data_st, n, clus, mix) {
	#clus <-fanny(data_st,n)	 								#Разбиение стандартных образцов на n классов
	cl = clus$clustering	
	kol_st <- length(cl)									#Количество стандартных образцов 
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_ss <- ncol(dm_mix)  								#Количество образцов 
	s <- list()
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
		num_f_mix_max<-i_f[num]
		s[[i]] <- num_f_mix_max
	}
	s
}

fristdr <- function(data_st, new, clus){
	n=ncol(clus$membership)
	mix<-rbind(data_st, new)
	first_system_of_stolps <- fristdr_1(data_st, n, clus, mix) 		#Первый шаг алгоритма

																	#Второй шаг работы алгоритма
	cl = clus$clustering	
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_st <- length(cl)											#Количество стандартных образцов 
	kol_ss <- ncol(dm_mix)  										#Количество образцов 
	actual_system_of_stolps <- first_system_of_stolps
	system_of_slolps <-	actual_system_of_stolps 			
	ff_mix =NULL				
	
	for (k in 1:kol_ss){
	if (k>40){break}													#Запасное условие остановки:)
	if (k>3){
		if (ff_mix[k] < ff_mix[k-1]){
			if (ff_mix[k-1]>ff_mix[k-2]){break}}}						#Условие остановки
			
		f_mix = NULL
		i_f = NULL
		cl_i = NULL	
		num_stolp_add=NULL
		for (j in 1:kol_ss){
		
			check <- checking(j, actual_system_of_stolps)
			if (check==TRUE){
	
					if (j <= kol_st) {
						system_of_slolps[[ cl[j] ]] <- append(system_of_slolps[[ cl[j] ]], j)
						f_mix <- append(f_mix,  fris_compact_mix_ss (system_of_slolps, dm_mix, cl, n))
						i_f <- append(i_f, j)
						cl_i <- append (cl_i, cl[j])
					}else{
						for(i in (1:n)){
							system_of_slolps[[i]] <- append(system_of_slolps[[i]], j)
							f_mix <- append(f_mix, fris_compact_mix_ss (system_of_slolps, dm_mix, cl, n))
							i_f <- append(i_f, j)
							cl_i <- append (cl_i, i)
							system_of_slolps <-	actual_system_of_stolps
						}	
					}
				system_of_slolps <-	actual_system_of_stolps	
	
			}	
	
		}	
		num<- which.max(f_mix)
		num_stolp_add <- i_f[num]
		
		ff_mix <- append(ff_mix, max(f_mix))
		resultant_system_of_stolps <- actual_system_of_stolps
		actual_system_of_stolps[[cl_i[num]]] <- append(actual_system_of_stolps[[cl_i[num]]], num_stolp_add)
	}
	#ff_mix
	resultant_system_of_stolps										#Результат работы алгоритма
}
