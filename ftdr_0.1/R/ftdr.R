# $Id: fristdr.R 24 2009-09-24 12:36:55Z edd $
library(cluster)

# Расчет функции конкурентного сходства, где:
# rho_self - расстояние до ближайшего "своего" обзазца
# rho_alien - расстояние до ближайшего чужого образца
fris <- function (rho_self, rho_alien) {
	(rho_alien - rho_self) / (rho_self + rho_alien)
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

# Проверка, есть ли в smth в списке l
chck <- function (l, s){
	f<-TRUE
	if (is.null(s)){
		f<-TRUE
	}else{
		for (i in 1:length(s)){
			if (s[i]==l) {f <- FALSE}
		}
	}
	f
}

#Определяем обучающую выборку для классифицированного множества и распределяем по классам новые образцы, где
	# data_st - образца
	# cl - вектор разбиения
	# n - количество класстеров
	# new - набор новых образцов
fris_stolps <- function(data_st, cl, n, new=NULL, r=NULL){
#Первый шаг алгоритма, определяем по одному столпу на кждый класс. 		
	kol_st <- length(cl)												#Количество стандартных образцов 
	dm <- as.matrix(daisy(data_st, metric = "euclidean"))				#Матрица сходства
	if (is.null(r)){r = (max(dm)*2)}
	s <- list()
	for (i in 1:n){							
		i_f = NULL														# Номер образца
		f_mix = NULL
		for (j in 1:kol_st){
			if (cl[j] == i) {
			 	storage.mode(dm) <- "double"
				rez=0
				f_c_m <- .C("fris_compact_verified", as.integer(i), as.integer(j), dm, cl, as.integer(kol_st), as.integer(n), as.double(r),as.double(rez), PACKAGE="ftdr")
				f_mix <- append(f_mix,  f_c_m[[8]])
				i_f <- append(i_f, j)
			} 
		}	
		num<- which.max(f_mix)
		num_f_mix_max<-i_f[num]
		s[[i]] <- num_f_mix_max
	}
	first_system_of_stolps <- s
	print (first_system_of_stolps)
#Второй шаг алгоритма, наращиваем систему столпов до нужного размера. 	
	actual_system_of_stolps <- first_system_of_stolps
	ff_mix =NULL
	
	for (k in 1:kol_st){
		if (k > (kol_st-4)){break}											#Запасное условие остановки:)
		if (k>3){
			if (ff_mix[k-1] <= ff_mix[k-2]){
				if (ff_mix[k-2] >= ff_mix[k-3]){break}					#Условие остановки
			}
		}
		f_mix = NULL
		i_f = NULL
		cl_i = NULL	
		num_stolp_add=NULL
	
		for (j in 1:kol_st){
			check <- checking(j, actual_system_of_stolps)
			if (check==TRUE){											# Идем по всем образцам, которые не принадлежат системе столпов
				system_of_slolps <-	actual_system_of_stolps	
				storage.mode(dm) <- "double"
				system_of_slolps[[ cl[j] ]] <- append(system_of_slolps[[ cl[j] ]], j)
				ss <- list.as.matrix(system_of_slolps)
				storage.mode(ss) <- "integer"
				f_c_m_s <- .C("fris_compact_ss_verified", ss, as.integer(kol_st), dm, cl, as.integer(n), as.integer(ncol(ss)), as.double(r), as.double(rez), PACKAGE="ftdr")	
				f_mix <- append(f_mix, f_c_m_s[[8]] )
				i_f <- append(i_f, j)
				cl_i <- append (cl_i, cl[j])
				
			}	
		}	
		num<- which.max(f_mix)
		num_stolp_add <- i_f[num]
		ff_mix <- append(ff_mix, max(f_mix))
		resultant_system_of_stolps <- actual_system_of_stolps
		actual_system_of_stolps[[cl_i[num]]] <- append(actual_system_of_stolps[[cl_i[num]]], num_stolp_add)
	}	
	
	if (is.null(new)){		# если new не задано, просто отображаем получившееся множество со столпами
		clustersplot(data_st, cl, n, resultant_system_of_stolps) # функция отображения результата
	}else{					# если new задано, определяем, используя систему столпов принадлежность новых к классам и отображаем результат
		mix<-rbind(data_st, new)
		dm_mix <- as.matrix(daisy(mix, metric = "euclidean"))
		cluster=NULL
		number= NULL
		for (i in (kol_st+1):nrow(mix)){
			mini=NULL
			for (j in 1:length(resultant_system_of_stolps)){
				for (k in 1:length(resultant_system_of_stolps[[j]])){
					if (is.null(mini)){
						mini = dm_mix[i,resultant_system_of_stolps[[j]][k]]
						num_class = j 
					}else{
						rho = dm_mix[i,resultant_system_of_stolps[[j]][k]]
						if(rho < mini){
							mini = rho
							num_class = j 
						}
					}
				}
			}
			cluster<-append(cluster, num_class)
			number<-append(number, i)
			
		}
		
		cl<-append(cl, cluster)
		kol = nrow(mix) - kol_st
		clustersplot(mix, cl, n, resultant_system_of_stolps, kol) # функция отображения результата
		result<-list(slolps = resultant_system_of_stolps, clustering = cl)
		result
	}
}

#Графически отображаем классифицированное множество со столпами и распределенные по классам новые образцы, где
	# data_st - выборка
	# cl - вектор разбиения
	# n - количество класстеров
	# ss - система столпов
	# kol - количество новых образцов
clustersplot <- function(data_st, cl, n, ss, kol=0){
	kol_st <- length(cl)
	kol_pr <- ncol(data_st)
	x=NULL
	y=NULL

	for (i in 1:kol_st){
		b=NULL
		xx=c(0.0)
		yy=c(0.0)
		for (j in 1:kol_pr){
			b<-append(b, data_st[[j]][i])
		}
		recalc <- .C("recalc", as.double(b), as.integer(kol_pr), as.double(xx), as.double(yy), PACKAGE="ftdr")
		x<- append(x, recalc[[3]])
		y<- append(y, recalc[[4]])
	}

	color=c("black", "red", "blue", "magenta", "dark red", "cyan", "brown", "dark green", "dark cyan", "grey", "green", "violet", "purple", "sienna", "light green", "dark blue")										# 16 цветов  "light blue", "yellow", "orange"
	
	plot(x,y)
	#plot(min(x):max(x), min(y):max(y), type='p')
	for (j in 1:n){	
		for (i in 1:kol_st){
			if (cl[i] == j){
				points(x[i], y[i], pch = j, col = color[j])
				check <- checking(i, ss)
				if (check==FALSE){
					points(x[i], y[i], sub = i, pch=16, cex=1.7, col = color[j])
				}
				if (i>(kol_st-kol)){
					points(x[i], y[i], sub = i, cex=1.7, col = color[j])
				}
			}
		}	
	}
	#cl
}

# Разбиваем выборку на наиболее удачное число кластеров, но не большее K, где: 
	# unknown_data - неклассифицированные данные
	# K - максимально возможное число кластеров
	# r - фиксированное расстояние 
fris_tax <- function(unknown_data, K, r=NULL, cls=FALSE){
	s = NULL
	fss = NULL
	cl=NULL
	systems_of_s = list()
	systems_of_s[[1]]=NULL
	dm = as.matrix(daisy(unknown_data, metric = "euclidean"))
	if (is.null(r)){r = (max(dm)*2)}
	storage.mode(dm) <- "double"
	ff_mix=NULL
	kol = nrow(unknown_data)
	for (k in 1:kol){
		cl<- append(cl, 0)
	}
	cll <-list()
	sss<- list()
	
	for (k in 1:K){
		s <- fss
		systems_of_s[[k]] <- fss
		f_mix = NULL
		num_i = NULL
		rez = c(0.0)
		for (i in 1:kol){
			if (chck(i, fss)==TRUE){ 										#надо дописать условие проверки i в системе столпов. 
				s <- append(s, i)											#получить значение редуцированной функции, при системе столпов s
				r_f <- .C("reduced_fris", as.integer(k), s, as.integer(kol), dm, as.double(r), as.double(rez), PACKAGE="ftdr")
				f_mix <- append(f_mix, r_f[[6]])
				num_i <- append(num_i, (i))
				s <- fss
			}
		}
		num<- which.max(f_mix)
		num_stolp_add <- num_i[num]
		fss <- append(fss, num_stolp_add)
		systems_of_s[[k]] <- fss
		if (k>1){
			est_k <- .C("est",  as.integer(k), fss, as.integer(kol), dm, as.double(r), as.double(rez), as.integer(cl),  PACKAGE="ftdr")
			ff_mix <- append(ff_mix, est_k[[6]])
			cll[[k-1]]<- (est_k[[7]])
			sss[[k-1]] <- est_k[[2]]
		}			
	}
	num<- which.max(ff_mix)
	if(cls==FALSE){
		clustersplot(unknown_data, cll[[num]], num+1, sss[[num]])
	}
	result<-list(stolps = sss[[num]], clustering = cll[[num]], n=num+1)
	result
} 

fris_class <- function(unknown_data, K, r=NULL, f=NULL, alpha=NULL) {
	m<-fris_tax(unknown_data, K, r, TRUE)								# первый шаг, разбили выборку на кластеры, в каждом кластере 1 столп. 
	dm = as.matrix(daisy(unknown_data, metric = "euclidean"))
	t = c()
	if (is.null(f)){f = 0.2}
	if (is.null(alpha)){alpha = 7}
	storage.mode(dm) <- "double"
	rz <- .C("fris_class", dm, as.integer(nrow(unknown_data)), as.integer(m$n), as.integer(m$stolps), as.integer(m$clustering), as.double(f), as.double(alpha), PACKAGE="ftdr") 
	cl <- rz[[5]] 
	s <- list()
	for (i in 1:m$n){
		s[[i]] <- m$stolps[i]
	}
	clustersplot(unknown_data, cl, m$n, s)	
	print (cl)
	cl
}
