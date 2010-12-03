

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>


double get_el(double * dm_mix, int num_col, int num_row, int *size) {
	return dm_mix[*size * (num_row-1)  + num_col - 1];
	}

int get_eli(int * dm_mix, int num_col, int num_row, int *size) {
	return dm_mix[*size * (num_row-1)  + num_col - 1];
	}

// Расчет функции конкурентного сходства, где:
	// r_s - расстояние до ближайшего "своего" обзазца
	// r_a - расстояние до ближайшего чужого образца
double fris(double r_s, double r_a){
	return (r_a - r_s)/(r_a + r_s);
	}

double get_min(int * ss, int * kol, int k, double * dm_mix, int * kol_ss){
	int i;
	double mn, rho;
	mn = 0;
	//printf("kol = %i\n", *kol);
	for (i=1; i<= *kol; i++){
		if (ss[i] != 0 && ss[i]!=k){
			if (mn == 0){
				mn = get_el(dm_mix, k, ss[i], kol_ss);
			}else{			
				rho = get_el(dm_mix, k, ss[i], kol_ss);
				if (mn > rho){
					mn = rho;
				}
			}
		}
	}
	i = 0;
	rho = 0; 
	return mn;
}

char * cheking(int k, int * b, int kol){
	char * d; 
	int i;
	d = "TRUE";
	for (i = 0; i < kol; i++){
		if (b[i] == k){
			d = "FALSE";
		}
	}
	return d;
}	
	
// Находим значение fris-функции, для смешанной выборки, где определён единственный столп:
	// num_cl - номер класса, для которого определен единственный столп;
	// smpl - единственный столп класса num_cl;
	// kol_ss - количество образцов в смешанной выборке;
	// dm_mix - матрица подобия;
	// cl - вектор разбиения;
	// kol_st - количество верифицированных образов;
	// n - количество классов в разбиении;
	// rez - переменная для формирования результата;
void fris_compact(int * num_cl, int * smpl, int * kol_ss, double * dm_mix, int * cl, int * kol_st, int * n, double *rez) {
	
	double rho_self, rho_alien, rho, rho_min, rho_cl[*n];
	double f=0, r=0;
	int k, i, j, num_class, el;
	
	for (k = 1; k <= *kol_ss; k++){ 			// для каждого образца находим расстояние до ближайшего своего и ближайшего чужого. 
		rho_self=0;
		rho_alien=3;
		if (k != *smpl-1){						// рассматриваем его, если только он не является передаваемым столпом
			if (k <= *kol_st){
				if(cl[k-1] == *num_cl){			// находим расстояния, если он попадает в передаваемый класс 
					rho_self = get_el(dm_mix, k, *smpl, kol_ss);
					for ( i = 1; i <= *kol_st; i++){
						if (cl[i-1] != cl[k-1]){
							rho = get_el(dm_mix, k, i, kol_ss);
							if (rho_alien > rho){
								rho_alien = rho; 
							}
						}
					}
				}else{							// находим расстояния, если он не попадает в передаваемый класс, но является верифицированным. 
					rho = get_el(dm_mix, k, *smpl, kol_ss);
					if (rho_alien > rho){
						rho_alien = rho; 
					}
					for (i = 1; i <= *kol_ss; i++){
						if(cl[i-1] == cl[k-1] || i > *kol_st){
							if (rho_self==0){
								rho_self=get_el(dm_mix, k, i, kol_ss);
							}else{
								rho = get_el(dm_mix, k, i, kol_ss);
								if (rho_self>rho){
									rho_self = rho;
								}
							}
						}
					}
				}		
			}else{								// находим расстояния, если образец является неверифицированным. 
				rho_min = 0;					// находим ближайший верифицированный элемент
				el = 0;
				for (j = 1; j <= *kol_st; j++){
					if (rho_min == 0){
						rho_min = get_el(dm_mix, k, j, kol_ss);
						el = j;
					}else{
						rho = get_el(dm_mix, k, j, kol_ss);
						if (rho_min > rho ){
							rho_min = rho;
							el = j; 
						}		
					}
				}
				num_class  = cl [el - 1];		// определяем класс, которому принадлежит ближайший элемент.
				if (num_class == *num_cl){		// считаем расстояния, если класс образца совпадает с передоваемым
					rho_self = get_el(dm_mix, k, *smpl, kol_ss);
					for ( i = 1; i <= *kol_st; i++){
						if (cl[i-1] != num_class){
							rho = get_el(dm_mix, k, i, kol_ss);
							if (rho_alien > rho){
								rho_alien = rho; 
							}
						}
					}
				}else{
					rho = get_el(dm_mix, k, *smpl, kol_ss);
					if (rho_alien > rho) {
						rho_alien = rho; 
					}
					for (i = 1; i <= *kol_ss; i++){
						if(cl[i-1] == num_class || i > *kol_st){
							if (rho_self==0){
								rho_self=get_el(dm_mix, k, i, kol_ss);
							}else{
								rho = get_el(dm_mix, k, i, kol_ss);
								if (rho_self > rho){
									rho_self = rho;
								}
							}
						}
					}
				
				}
			}
		}
		f = fris(rho_self, rho_alien);			// считаем значение fris-функции для k образца
		r=r+f;									
	}	
	*rez = r / *kol_ss; 						// Находим значение fris- функции, сумму значений fris-функций делим на количество образцов в смешанной выборке
}

// Находим значение fris-функции, для смешанной выборки, где определёна система столпов:
	// s - матрица столпов.
	// num_cl - номер класса, для которого определен единственный столп;
	// smpl - единственный столп класса num_cl;
	// kol_ss - количество образцов в смешанной выборке;
	// dm_mix - матрица подобия;
	// cl - вектор разбиения;
	// kol_st - количество верифицированных образов;
	// n - количество классов в разбиении;
	// rez - переменная для формирования результата;
void fris_compact_ss(int * s, int * kol_ss, int * kol_st, double * dm_mix, int * cl, int * n, int * kol_col, double * rez) {
	int i,j,k, count;
	int ss[*n+1][*kol_col+1];
	int b[(*n+1) * (*kol_col+1)];
	double rho_self, rho_alien, rho, max, r, f;

	for (i = 1; i <= *n; i++) {
		for (j = 1; j<= *kol_col; j++) {
			ss[i][j] = get_eli(s, i, j, n);
		}
	}
	for (k = 1; k <= *kol_ss; k++){
		rho_self = 0;
		rho_alien = 6;	
		//printf("%i\n", k);  
		if (k <= *kol_st){
			if(cheking(k, ss[cl[k-1]], *kol_col) == "TRUE"){
				rho_self = get_min(ss[cl[k-1]], kol_col, k, dm_mix, kol_ss); 
				count = 1;
				for (i=1; i<= *n; i++){
					if (cl[k-1] != i){
						for (j=0; j< *kol_col; j++){
							b[count] = ss[i][j+1]; 
							count ++ ;
						}
					}
				}
				count = count-1;
				rho = get_min(b, &count, k, dm_mix, kol_ss);  
				if (rho_alien > rho ){
					rho_alien = rho; 
				}
			}
		}else{
			count = 1;
			for (i=1; i<= *n; i++){
				for (j=0; j< *kol_col; j++){
					b[count] = ss[i][j+1]; 
					count ++ ;
				}
			}
			count = count-1;
			if(cheking(k, b, count) == "TRUE"){
				rho_self = get_min(b, &count, k, dm_mix, kol_ss);		
				max = get_min(ss[1], kol_col, k, dm_mix, kol_ss);
				for (i = 1; i <= *n; i++){
					rho = get_min(ss[i], kol_col, k, dm_mix, kol_ss);
					if(max < rho){ 
						max = rho;  
					}
				}
				if (rho_alien > max){
					rho_alien = max;
				}
			}
		}
		if (rho_self == 0 && rho_alien ==0 ){
			printf("k = %i\n", k );
		}
		//printf("rho_self = %f\n", rho_self);
		//printf("rho_alien = %f\n", rho_alien);
		f = fris(rho_self, rho_alien);			// считаем значение fris-функции для k образца
		//printf("f = %f\n", f);
		r=r+f;									
	}	
*rez = r / *kol_ss; 						// Находим значение fris- функции, сумму значений fris-функций делим на количество образцов в смешанной выборке 
}



// Находим значение fris-функции, для верифицированной выборки, где определён единственный столп:
	// num_cl - номер класса, для которого определен единственный столп;
	// smpl - единственный столп класса num_cl;
	// dm - матрица подобия;
	// cl - вектор разбиения;
	// kol_st - количество верифицированных образов;
	// n - количество классов в разбиении;
	// rez - переменная для формирования результата;
void fris_compact_verified(int * num_cl, int * smpl, double * dm, int * cl, int * kol_st, int * n,  double * rrr, double *rez) {
	double rho_self, rho_alien, rho, rho_min, rho_cl[*n];
	double f=0, r=0;
	int k, i, j, num_class, el;
	
	for (k = 1; k <= *kol_st; k++){ 			// для каждого образца находим расстояние до ближайшего своего и ближайшего чужого. 
		rho_self=0;
		rho_alien=*rrr;
		if (k != *smpl){						// рассматриваем его, если только он не является передаваемым столпом
			if(cl[k-1] == *num_cl){				// находим расстояния, если он попадает в передаваемый класс 
				rho_self = get_el(dm, k, *smpl, kol_st);
				for ( i = 1; i <= *kol_st; i++){
					if (cl[i-1] != cl[k-1]){
						rho = get_el(dm, k, i, kol_st);
						if (rho_alien > rho){
							rho_alien = rho; 
						}
					}
				}
				}else{							// находим расстояния, если он не попадает в передаваемый класс
					rho = get_el(dm, k, *smpl, kol_st);
					if (rho_alien > rho){
						rho_alien = rho; 
					}
					for (i = 1; i <= *kol_st; i++){
						if(cl[i-1] == cl[k-1]){
							if (rho_self==0){
								rho_self=get_el(dm, k, i, kol_st);
							}else{
								rho = get_el(dm, k, i, kol_st);
								if (rho_self>rho){
									rho_self = rho;
								}
							}
						}
					}
				}		
		
		}
		f = fris(rho_self, rho_alien);			// считаем значение fris-функции для k образца
		r=r+f;									
	}	
	*rez = r / *kol_st; 						// Находим значение fris- функции, сумму значений fris-функций делим на количество образцов в смешанной выборке
}

// Находим значение fris-функции, для верифицированной выборки, где определёна система столпов:
	// 1) s - матрица столпов.
	// 2) kol_st - количество верифицированных образов;
	// 3) dm - матрица подобия;
	// 4) cl - вектор разбиения;	
	// 5) n - количество классов в разбиении;
	// 6) kol_col - количество столбцов в s;
	// 7) rez - переменная для формирования результата;
void fris_compact_ss_verified(int * s, int * kol_st, double * dm, int * cl, int * n, int * kol_col,  double * rrr, double * rez) {
	int i,j,k, count;
	int ss[*n+1][*kol_col+1];
	int b[(*n+1) * (*kol_col+1)];
	double rho_self, rho_alien, rho, max, r, f;

	for (i = 1; i <= *n; i++) {
		for (j = 1; j<= *kol_col; j++) {
			ss[i][j] = get_eli(s, i, j, n);
		}
	}
	for (k = 1; k <= *kol_st; k++){
		rho_self = 0;
		rho_alien = *rrr;	
		if(cheking(k, ss[cl[k-1]], *kol_col) == "TRUE"){
			rho_self = get_min(ss[cl[k-1]], kol_col, k, dm, kol_st); 
			count = 1;
			
			for (i=1; i<= *n; i++){
				if (cl[k-1] != i){
					for (j=0; j< *kol_col; j++){
						b[count] = ss[i][j+1]; 
						count ++ ;
					}
				}
			}
			count = count-1;
			rho = get_min(b, &count, k, dm, kol_st);  
			if (rho_alien > rho ){
				rho_alien = rho; 
			}
		}
		f = fris(rho_self, rho_alien);			// считаем значение fris-функции для k образца
		r=r+f;									
	}	
*rez = r / *kol_st; 						// Находим значение fris- функции, сумму значений fris-функций делим на количество образцов в смешанной выборке 
}

void recalc(double * b, int * n, double * xx, double *yy){
	double pi = 3.1415926535;
	double x[*n];
	double y[*n];
	double d;
	int i;
	x[0] = b[0];
	y[0] = 0;
	d = pow(b[0],2);
	for (i=1; i<*n; i++){
		x[i] = x[i-1] + b[i]*cos(pi*i/(*n));
		y[i] = y[i-1] + b[i]*sin(pi*i/(*n));
		d = d + pow(b[i],2);
		} 
		d = sqrt(d); 
	if (x[*n-1] > 0){
		*xx = d/sqrt(1+pow((y[*n-1]/x[*n-1]),2));
	}else{
		*xx = -d/sqrt(1+pow((y[*n-1]/x[*n-1]),2));
	}
	*yy = y[*n-1]/x[*n-1] * *xx;
}

void reduced_fris(int * k, int * s, int * kol, double * dm, double * rho_alien, double * rez){
	int i, j;
	double rho_self, rho, f, re;
	//printf ("kol = %i\n",*kol);
	//printf ("k = %i\n",*k);
	
	for (i = 1; i <= *kol; i++){
		rho_self = 0;
		for (j = 1; j<= *k; j++){
			if(rho_self == 0){
				rho_self = get_el(dm, s[j-1], i, kol);
			}else{
				rho= get_el(dm, s[j-1], i, kol); 	
				if (rho_self > rho){
					rho_self = rho;
				}
			}
		} 
	f = fris(rho_self, *rho_alien);			// считаем значение fris-функции для k образца
	re=re+f;		
	}
*rez = re / *kol;		
}

void est(int * kol_class, int * s, int * kol, double * dm, double * rrr,  double * rez, int * cl){
	int i, j, k,  num, ss[*kol_class];
	double rho_self, rho, f, re, max;
	const l = 1;
	
	for (i = 1; i <= *kol; i++) {
		rho_self = get_el(dm, s[0], i, kol);
		num = 1;
		for (j = 1; j<= *kol_class; j++){
			rho= get_el(dm, s[j-1], i, kol); 	
			if (rho_self > rho){
				rho_self = rho;
				num = j;
			}

		}
		cl[i-1] = num;	
	}
	for (k = 1; k<=*kol_class; k++){
		fris_compact_verified(&k, &l, dm, cl, kol, kol_class, rrr, rez);
		max = *rez;
		num = i;

		for (i = 1; i <= *kol; i++){
			if (cl[i-1] == k){
				fris_compact_verified(&k, &i, dm, cl, kol, kol_class, rrr, rez);
				if (max < *rez){
					max = *rez;
					num = i;
				}
			}
		}
		ss[k-1] = num;
		s[k-1] = num;
	}
	fris_compact_ss_verified(&ss, kol, dm, cl, kol_class, &l, rrr, rez);
	
}

void fris_class(double * dm, int *kol, int * kol_class, int * stolps, int * cl, double * ff, double * alpha ){
	int i, j, jj, k, l, area, points[*kol], count = 0, merger[* kol_class][* kol_class], a, b;
	double rho, rho_k, rho_l, f, rho_a, rho_b;
	
	for (k = 1; k<= *kol_class; k++){
		for (l = 1; l<= *kol_class; l++){
			if (k==l){
				merger[k][l] = 1;
			}else{
			merger[k][l] = 0 ;
			}
		}
	}
	
	for (k = 1; k < *kol_class; k++){
		for (l = k; l <= *kol_class; l++){
			if (l != k){
				for (i = 1; i <= *kol; i++){
					if (cl[i-1] == k || cl[i-1] == l){
						rho_k = get_el(dm, stolps[k-1], i, kol);
						rho_l = get_el(dm, stolps[l-1], i, kol);
						area = 1;
						for (j=0; j<*kol_class; j++){
							if (rho_k > get_el(dm, stolps[j], i, kol)  || rho_l > get_el(dm, stolps[j], i, kol)){
								area=0;
							}else{
								area = 1;
							}
						}
						if (area==1 & cl[i-1] == k){
							f = fris(rho_k, rho_l);
						}
						if (area==1 & cl[i-1] == l){
							f = fris(rho_l, rho_k);
						}
						if (f > *ff){
							area=0;
						}
						if (area == 1){
							points[count] = i; 
							//printf(" %i ", i);
							count ++;
						}
					}
				}
			}
		}	
	}
	count--;
	//printf("\n");
	for (i=0; i<=count; i++){											// идем по всем точкам, попавшим в зону конкуренции, проверяем надо ли объединять по ним классы. 
		k = cl[points[i]-1];
		
		rho_l = 0;
		for (j = 0; j<*kol_class; j++){
			if (rho_l == 0 && cl[stolps[j]-1] != k ){
				rho_l = get_el(dm, stolps[j], points[i], kol);
				l = cl[stolps[j]-1];
			}
			//printf("  %i  ", j);
			//printf("  %f", get_el(dm, stolps[j], points[i], kol));
			
			if(cl[stolps[j]-1] != k && rho_l > get_el(dm, stolps[j], points[i], kol)){
				l = cl[stolps[j]-1];
			}
		}
		
		//printf("k = %i ", k);
		//printf("l = %i\n ", l);
		if (merger[k][l] != 1 && merger[l][k] != 1){					//проверяем надо ли объединять k и l классы
			rho_a = 0;													//расстояние до ближайшего своего от точки а;
			rho_b = 0;													//расстояние до ближайшего своего от точки b;
			rho = 0;													//минимальное расстояние между a и b точками из классов l и k;
			for (j = 0; j <= count; j++){
				for (jj = j; jj <= count; jj++){
					if (jj!=j){
						if(cl[points[j]-1] != cl[points[jj]-1]){
							if (rho == 0) {
								rho = get_el(dm, points[j], points[jj], kol);
								a=points[j];
								b=points[jj];
							}else{
								if (rho > get_el(dm, points[j], points[jj], kol)){
									rho = get_el(dm, points[j], points[jj], kol);
									a=points[j];
									b=points[jj];
								}
							}	
						}
					}
				}
			}
			for (j = 1; j <= *kol; j++){
				if (j != a && j != b){
					if (cl[j-1] == cl[a-1]){
						if (rho_a == 0){
							rho_a = get_el(dm, j, a, kol);
						}else{
							if(rho_a > get_el(dm, j, a, kol)){
								rho_a = get_el(dm, j, a, kol);
							}  
						}
					}
					if (cl[j-1] == cl[b-1]){
						if (rho_b == 0){
							rho_b = get_el(dm, j, b, kol);
						}else{
							if(rho_b > get_el(dm, j, b, kol)){
								rho_b = get_el(dm, j, b, kol);
							}  
						}
					}
				}	
			}  
			//printf("rho_a = %f\n", rho_a);
			//printf("rho_b = %f\n", rho_b);
			//printf("rho_a = %f\n", (rho_a+rho_b)/2);
			if (rho_a < (*alpha * rho_b) && rho_b < (*alpha * rho_a) && rho < *alpha *((rho_a+rho_b)/2)){
				merger[k][l] = 1;
				merger[l][k] = 1;
				//printf("merge\n");
			}
		}
	}
	
	for (k = *kol_class; k>=1; k--){
		for (i=1; i<=*kol; i++){
			if(cl[i-1] != k && merger[cl[i-1]][k]==1){
				cl[i-1] = k;
			}
		} 
	}
	
	/*int bb[*kol];
	count=0;
	for (i = 0; i<*kol; i++){
		if (cheking(cl[i], bb, *kol)=="TRUE"){
			bb[count]= cl[i];
			//printf("%i", bb[count]);
			count ++;
		}
	}
	for (k=1; k<=count;k++){
		for (i = 0; i<*kol; i++){
			if(bb[k-1]==cl[i]){
				cl[i]=k;
			}
		}
	}
	
	//printf("\n");
	//printf("%i\n", count);*/
}
