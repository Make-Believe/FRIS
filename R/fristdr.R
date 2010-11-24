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
					f_c_m <- .C("fris_compact", as.integer(i), as.integer(j), as.integer(kol_ss), dm_mix, cl, as.integer(kol_st), as.integer(n), as.double(rez), PACKAGE="fristdr")
					f_mix <- append(f_mix,  f_c_m[[8]])
					i_f <- append(i_f, j)
				} 
			} else {
				storage.mode(dm_mix) <- "double"
				rez=0
				f_c_m <- .C("fris_compact", as.integer(i), as.integer(j), as.integer(kol_ss), dm_mix, cl, as.integer(kol_st), as.integer(n), as.double(rez), PACKAGE="fristdr")
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


#Функция отображения результата, где:
# data_st - данные о верифицированных образцах
# new - неизвестные образцы
# clus - разбиение верифицированных образцов
plot.fristdr <- function(data_st, cl, n, new){
	s <- fristdr(data_st, cl, n, new)
	mix<-rbind(data_st, new)

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
			}
		}

		v<-append(v,num_cl)
	}
	print('Разбиение:')
	print(v)
	clusplot(mix, v, labels=3)
}

#Функция FRiS-TDR, возращает оптимальную систему столпов для смешанной выборки, где:
# data_st - данные о верифицированных образцах;
# cl - разбиение верифицированных образцов;
# n - количество классов;
# new - неизвестные образцы;
fristdr <- function(data_st, cl, n, new){
	#new<- read.table('/home/olga/Dev/fristdr/ftdr_0.1/data/new.csv')
	mix<-rbind(data_st, new)
	first_system_of_stolps <- fristdr_1(data_st, n, cl, mix) 		#Первый шаг алгоритма
	print('Первая система столпов: ' )
	print (first_system_of_stolps )

	dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
	kol_ss <- ncol(dm_mix)
	kol_st <- length(cl)			
	rez=0
	
	actual_system_of_stolps <- first_system_of_stolps
	ff_mix =NULL	
	
	for (k in 1:kol_ss){
		#if (k>1){break}												#Запасное условие остановки:)
		if (k>3){
			if (ff_mix[k-1] <= ff_mix[k-2]){
				if (ff_mix[k-2] >= ff_mix[k-3]){break}
			}
		}																#Условие остановки

		f_mix = NULL
		i_f = NULL
		cl_i = NULL	
		num_stolp_add=NULL

		
		for (j in 1:kol_ss){
			check <- checking(j, actual_system_of_stolps)
			if (check==TRUE){											# Идем по всем образцам, которые не принадлежат системе столпов
				system_of_slolps <-	actual_system_of_stolps	
				storage.mode(dm_mix) <- "double"
				if (j <= kol_st) {
					system_of_slolps[[ cl[j] ]] <- append(system_of_slolps[[ cl[j] ]], j)
					ss <- list.as.matrix(system_of_slolps)
					storage.mode(ss) <- "integer"
					f_c_m_s <- .C("fris_compact_ss", ss, as.integer(kol_ss), as.integer(kol_st), dm_mix, cl, as.integer(n), as.integer(ncol(ss)), as.double(rez), PACKAGE="fristdr")	
					f_mix <- append(f_mix, f_c_m_s[[8]] )
					i_f <- append(i_f, j)
					cl_i <- append (cl_i, cl[j])
				}else{
					for(i in (1:n)){
						system_of_slolps[[i]] <- append(system_of_slolps[[i]], j)
						ss <- list.as.matrix(system_of_slolps)
						storage.mode(ss) <- "integer"
						f_c_m_s <- .C("fris_compact_ss", ss, as.integer(kol_ss), as.integer(kol_st), dm_mix, cl, as.integer(n), as.integer(ncol(ss)), as.double(rez), PACKAGE="fristdr")	
						f_mix <- append(f_mix, f_c_m_s[[8]] )
						i_f <- append(i_f, j)
						cl_i <- append (cl_i, i)
						system_of_slolps <-	actual_system_of_stolps
					}	
				}
			}	
		}	
		num<- which.max(f_mix)
		num_stolp_add <- i_f[num]
		print(max(f_mix))
		ff_mix <- append(ff_mix, max(f_mix))
		resultant_system_of_stolps <- actual_system_of_stolps
		actual_system_of_stolps[[cl_i[num]]] <- append(actual_system_of_stolps[[cl_i[num]]], num_stolp_add)
	}
	
	
	Print("Результирующая система столпов:")
	s<-resultant_system_of_stolps										#Результирующая система столпов
	print(s)
	kol<-kol_st+1
	
	for (i in kol:kol_ss){
		min_rho = NULL
		num_cl = NULL
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
			}
		}

		v<-append(v,num_cl)												#вектор разбиения
	}
	Print("Разбиение:")
	print(v)
	clusplot(mix, v, labels=3)
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
