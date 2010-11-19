# $Id: fristdr.R 24 2009-09-24 12:36:55Z edd $
library(cluster)

# Расчет функции конкурентного сходства, где:
# rho_self - расстояние до ближайшего "своего" обзазца
# rho_alien - расстояние до ближайшего чужого образца
fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
}

# Проверка, есть ли в smth в списке l
checking <- function (l, smth){
	f<-TRUE
	for (i in 1:length(smth)){
		for (j in 1:length(smth[[i]])){
			if (smth[[i]][j]==l){f <- FALSE}
		}
	}
	f
}



# Рассчет функции конкурентного сходства для множества с системой столпов, где:
# ss - система столпов
# dm_mix - матрица сходства
# сl - разбиение
# n - количество классов
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

# Первый шаг алгоритма, определение первой системы столпов
# data_st - данные верифицированной выборки
# n - количество классов
# cl - вектор разбиение data_st на n классов
# mix - смешанные данные
fristdr_1 <- function (data_st, n, cl, mix) {
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
					storage.mode(dm_mix) <- "double"
					rez=0
					f_c_m <- .C("fris_compact", as.integer(i), as.integer(j), as.integer(kol_ss), dm_mix, cl, as.integer(kol_st), as.integer(n), as.double(rez), PACKAGE="ftdr")
					f_mix <- append(f_mix,  f_c_m[[8]])
					i_f <- append(i_f, j)
				} 
			} else {
				storage.mode(dm_mix) <- "double"
				rez=0
				f_c_m <- .C("fris_compact", as.integer(i), as.integer(j), as.integer(kol_ss), dm_mix, cl, as.integer(kol_st), as.integer(n), as.double(rez), PACKAGE="ftdr")
				f_mix <- append(f_mix, f_c_m [[8]])
				i_f <- append(i_f, j)	
			}
		}	
		num<- which.max(f_mix)
		num_f_mix_max<-i_f[num]
		s[[i]] <- num_f_mix_max
	}
	s
}

#Функция FRiS-TDR, возращает оптимальную систему столпов для смешанной выборки, где:
# data_st - данные о верифицированных образцах
# new - неизвестные образцы
# clus - разбиение верифицированных образцов
fristdr <- function(data_st, new, clus){
	n=ncol(clus$membership)
	mix<-rbind(data_st, new)
	first_system_of_stolps <- fristdr_1(data_st, n, clus, mix) 		#Первый шаг алгоритма
	print('Первая система столпов: ' )
	print (first_system_of_stolps )
	print ('=======')
	
																	#Второй шаг работы алгоритма
	cl = clus$clustering	
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_st <- length(cl)											#Количество стандартных образцов 
	kol_ss <- ncol(dm_mix)  										#Количество образцов 
	actual_system_of_stolps <- first_system_of_stolps
	system_of_slolps <-	actual_system_of_stolps 			
	ff_mix =NULL				
	
	for (k in 1:kol_ss){
	if (k>3){break}													#Запасное условие остановки:)
	if (k>3){
		#print(k)
		#print(ff_mix[k])
		#print(ff_mix[k-1])
		if (ff_mix[k-1] < ff_mix[k-2]){
			if (ff_mix[k-2]>ff_mix[k-3]){break}
		}
	}																	#Условие остановки
			
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
		print (k)
		#print(max(f_mix))
		ff_mix <- append(ff_mix, max(f_mix))
		resultant_system_of_stolps <- actual_system_of_stolps
		actual_system_of_stolps[[cl_i[num]]] <- append(actual_system_of_stolps[[cl_i[num]]], num_stolp_add)
	}
	#ff_mix
	print(actual_system_of_stolps)
	resultant_system_of_stolps										#Результат работы алгоритма
	
}

#Функция отображения результата, где:
# data_st - данные о верифицированных образцах
# new - неизвестные образцы
# clus - разбиение верифицированных образцов
plot.fristdr <- function(data_st, new, clus){
	s <- fristdr(data_st, new, clus)
	mix<-rbind(data_st, new)
	
	#n=ncol(clus$membership)
	#s<- fristdr_1(data_st, n, clus, mix)
	#print('Система столпов:')
	#print(s)
	
	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	v<-clus$clustering
	kol_ss <- ncol(dm_mix) 		#Количество образцов 
	kol_st <- length(v)		#Количество стандартных образцов 
	kol<-kol_st+1
	
	for (i in kol:kol_ss){
		min_rho = NULL
		num_cl = NULL
		#print(i)
		for (j in 1:length(s)){
			for (l in 1:length(s[[j]])){
				if (is.null(min_rho)){
					min_rho <- dm_mix[i, s[[j]][l]]
					num_cl <-j
				}else{ 
					rho <- dm_mix[i, s[[j]][l]]
					if (min_rho > rho){
						min_rho <- rho
						num_cl <- j
					}
				}

				#print(dm_mix[i, s[[j]][l]])
				#print(num_cl)
				
				
			}
		}
		#print(rho_min)
		#print('--------------')
		v<-append(v,num_cl)
	}
	print('Разбиение:')
	print(v)
	clusplot(mix, v, labels=3)
}


test<- function(data_st, cl, n){
	new<- read.table('/home/olga/Dev/fristdr/ftdr_0.1/data/new.csv')
	mix<-rbind(data_st, new)
	first_system_of_stolps <- fristdr_1(data_st, n, cl, mix) 		#Первый шаг алгоритма
	#print('Первая система столпов: ' )
	#print (first_system_of_stolps )

	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_ss <- ncol(dm_mix)
	kol_st <- length(cl)			
	rez=0
	
	actual_system_of_stolps <- first_system_of_stolps
	ff_mix =NULL	
	
#--- testing	
#	s <- first_system_of_stolps 
#	s[[1]] <- append(s[[1]], 13)
#	s[[1]] <- append(s[[1]], 14)
#	s[[1]] <- append(s[[1]], 17)
#	s[[1]] <- append(s[[1]], 18)
#	s[[1]] <- append(s[[1]], 19)
#	ss <- list.as.matrix(s)
#	print (ss)
#	storage.mode(dm_mix) <- "double"
#	storage.mode(ss) <- "integer"
#	rz <- .C("fris_compact_ss", ss, as.integer(kol_ss), as.integer(kol_st), dm_mix, cl, as.integer(n), as.integer(ncol(ss)), as.double(rez), PACKAGE="ftdr")	
#	rz[[8]]
	
#--- end of testing

	for (k in 1:kol_ss){
		#if (k>1){break}												#Запасное условие остановки:)
		if (k>3){
			if (ff_mix[k-1] < ff_mix[k-2]){
				if (ff_mix[k-2]>ff_mix[k-3]){break}
			}
		}																#Условие остановки

		f_mix = NULL
		i_f = NULL
		cl_i = NULL	
		num_stolp_add=NULL
		#print(actual_system_of_stolps)
		
		for (j in 1:kol_ss){
			check <- checking(j, actual_system_of_stolps)
			if (check==TRUE){											# Идем по всем образцам, которые не принадлежат системе столпов
					system_of_slolps <-	actual_system_of_stolps	
					storage.mode(dm_mix) <- "double"
					if (j <= kol_st) {
						system_of_slolps[[ cl[j] ]] <- append(system_of_slolps[[ cl[j] ]], j)
						ss <- list.as.matrix(system_of_slolps)
						storage.mode(ss) <- "integer"
						f_c_m_s <- .C("fris_compact_ss", ss, as.integer(kol_ss), as.integer(kol_st), dm_mix, cl, as.integer(n), as.integer(ncol(ss)), as.double(rez), PACKAGE="ftdr")	
						f_mix <- append(f_mix, f_c_m_s[[8]] )
						i_f <- append(i_f, j)
						cl_i <- append (cl_i, cl[j])
					}else{
						for(i in (1:n)){
							system_of_slolps[[i]] <- append(system_of_slolps[[i]], j)
							ss <- list.as.matrix(system_of_slolps)
							storage.mode(ss) <- "integer"
							f_c_m_s <- .C("fris_compact_ss", ss, as.integer(kol_ss), as.integer(kol_st), dm_mix, cl, as.integer(n), as.integer(ncol(ss)), as.double(rez), PACKAGE="ftdr")	
							f_mix <- append(f_mix, f_c_m_s[[8]] )
							i_f <- append(i_f, j)
							cl_i <- append (cl_i, i)
							system_of_slolps <-	actual_system_of_stolps
						}	
					}
				#system_of_slolps <-	actual_system_of_stolps	
	
			}	
	
		}	
		num<- which.max(f_mix)
		num_stolp_add <- i_f[num]
		#print (k)
		print(max(f_mix))
		ff_mix <- append(ff_mix, max(f_mix))
		resultant_system_of_stolps <- actual_system_of_stolps
		actual_system_of_stolps[[cl_i[num]]] <- append(actual_system_of_stolps[[cl_i[num]]], num_stolp_add)
	}
	
	
	resultant_system_of_stolps										#Результат работы алгоритма
	
}

fill.vector <- function (v, l) {
	n = 1:l 
	lv = length(v)
	for (i in 1:l) {
		if (i <= lv) {
			n[i] = v[i]
		} else {
			n[i] = 0;
		}
	}
	n
};

list.as.matrix <- function( l ){
	ll = max(sapply(l, length))
	nl = lapply(l, fill.vector, ll)
	do.call('rbind',nl)
}
