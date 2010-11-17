
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


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
		rho_alien=2;
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
	// num_cl - номер класса, для которого определен единственный столп;
	// smpl - единственный столп класса num_cl;
	// kol_ss - количество образцов в смешанной выборке;
	// dm_mix - матрица подобия;
	// cl - вектор разбиения;
	// kol_st - количество верифицированных образов;
	// n - количество классов в разбиении;
	// rez - переменная для формирования результата;
void fris_compact_ss(int * s, int * kol_ss, int * kol_st, double * dm_mix, int * cl, int * n, int * kol_col, double * rez) {
	int r,c;
	for (r = 1; r <= *kol_col; r++) {
		for (c = 1; c<= *n; c++) {
			printf("%i ", get_eli(s, c, r, kol_col));
		}
		printf("\n");
	}
	
}
