#include <fstream>
#include "fris.h"


class PDist {
private:
	double* P;
	int Num;

public:
	PDist(double* p, int n): P(p), Num(n) {}
	double Dist(int a, int b) {
		return P[Num*(a-1)+b-1];
	}
};

typedef FRiS_Cluster< PDist > MyFRiS;
typedef FRiS_Class< PDist > MyFRiS_Cl;
typedef PrecedentSet<unsigned> MyPS;

extern "C" {
	__declspec(dllexport) void fris_cluster(
		double* dist, int* num, double* R, int* MaxStolps,
		int* outStolps, double* outQR, int* outClass, int* outTempStolps, double* Q3) {

		PDist PD(dist,*num);
		MyFRiS MF(PD, *R);
		MyPS TS, Stolps, OutTS, OutStolps;
		int k, a = 0;
		MyPS::ClassIter i;
		MyPS::PrecIter j;
		
		for(k = 1; k<=*num; ++k) {
			TS.AddPrecedent(k, 0);
		}
		for(k = 0; outTempStolps[k]!=0 && k < *MaxStolps; ++k){
			TS.Move(Stolps,outTempStolps[k],0);
		}
		*Q3=MF.NextLocalMax(TS,Stolps,OutTS,OutStolps,*outQR,*MaxStolps,*Q3);
		
		for(i = OutStolps.Classes.begin(); i!=OutStolps.Classes.end(); ++i){
			for(j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				outStolps[a]=*j; ++a;
				outClass[*j-1]=i->Class+1;
			}
		}
		for(i = OutTS.Classes.begin(); i!=OutTS.Classes.end(); ++i){
			for(j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				outClass[*j-1]=i->Class+1;
			}
		}
		a = 0;
		for(i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i){
			for(j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				outTempStolps[a]=*j; ++a;
			}
		}
	}

	__declspec(dllexport) void fris_qr(
		double* dist, int* num, int* classif, int* stolps, int* NumStolps, double* Q) {

		MyPS TS, Stolps;
		int k;

		PDist PD(dist,*num);
		MyFRiS MF(PD, 1);

		for(k = 1; k<=*num; ++k) {
			TS.AddPrecedent(k, classif[k-1]);
		}
		for(k = 0; stolps[k]!=0 && k < *NumStolps; ++k){
			TS.Move(Stolps,stolps[k],classif[stolps[k]]);
		}
		*Q = MF.QualityRating(TS,Stolps);
	}

	__declspec(dllexport) void fris_class(
		double* dist, int* num, int* classif, int* stolps, int* NumStolps,
		double* ParamF, double* ParamA, int* outClass) {

		MyPS TS, Stolps;
		MyPS::ClassIter i;
		MyPS::PrecIter j;
		int k;

		PDist PD(dist,*num);
		MyFRiS_Cl MF(PD, *ParamF, *ParamA);

		for(k = 1; k<=*num; ++k) {
			TS.AddPrecedent(k, classif[k-1]);
		}
		for(k = 0; stolps[k]!=0 && k < *NumStolps; ++k){
			TS.Move(Stolps,stolps[k],classif[stolps[k]]);
		}
		MF.UniteClusters(TS,Stolps);
		for(i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i){
			for(j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				outClass[*j-1]=i->Class;
			}
		}
		for(i = TS.Classes.begin(); i!=TS.Classes.end(); ++i){
			for(j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				outClass[*j-1]=i->Class;
			}
		}
	}
}
//dyn.load("C:\\Documents and Settings\\User\\Мои документы\\Visual Studio 2008\\Projects\\rfris\\Debug\\rfris.dll")