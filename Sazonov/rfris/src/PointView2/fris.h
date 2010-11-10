#pragma once
#include <float.h>
#include <set>
#include <vector>

template < typename TPrecedentID, typename TClass = unsigned >
class PrecedentSet {
public:
	struct ID_Array{
		TClass Class;
		std::set<TPrecedentID> Precedents;

		bool operator< (const ID_Array& a) const { return Class < a.Class; }
		bool operator== (const ID_Array& a) const { return Class == a.Class; }
		ID_Array(TClass classID):Class(classID) {}
	};
	std::set<ID_Array> Classes;
	typedef typename std::set<TPrecedentID >::iterator PrecIter;
	typedef typename std::set<ID_Array>::iterator ClassIter;
	typedef typename TPrecedentID PrecType;
	typedef typename TClass ClassType;

	std::pair<ClassIter, PrecIter> AddPrecedent (TPrecedentID ID, TClass Class) {
		std::pair<ClassIter, PrecIter> ret;
		std::pair<ClassIter, bool> a;
		std::pair<PrecIter, bool> b;
		a = Classes.insert(ID_Array(Class));
		ret.first = a.first;
		b = a.first->Precedents.insert(ID);
		ret.second = b.first;
		return ret;
	}
	std::pair<ClassIter, PrecIter> Move(PrecedentSet& PS, TPrecedentID ID, TClass Class) {
		std::pair<ClassIter, PrecIter> ret;
		ret = PS.AddPrecedent(ID,Class);
		Delete(ID,Class);
		return ret;
	}
	 std::pair<ClassIter, PrecIter> Move(PrecedentSet& PS, std::pair<ClassIter, PrecIter> P) {
		 std::pair<ClassIter, PrecIter> ret;
		 ret = AddPrecedent(*(P.second), P.first->Class);
		 P.first->Precedents.erase(P.second);
		 return ret;
	}
	 void Delete(TPrecedentID ID, TClass Class) {
		std::pair<ClassIter, ClassIter> a;
		std::pair<PrecIter, PrecIter> b;
		a = Classes.equal_range(ID_Array(Class));
		if(a.first==a.second) return;
		b = a.first->Precedents.equal_range(ID);
		if(b.first==b.second) return;
		a.first->Precedents.erase(b.first);
	 }

	 unsigned Size() {
		 unsigned ret = 0;
		 for(ClassIter i = Classes.begin(); i!=Classes.end(); ++i) {
			 ret += i->Precedents.size();
		 }
		 return ret;
	 }
};

template < typename Metric, typename IDArray = PrecedentSet <unsigned> >
class FRiS{
private:
	Metric& M;
	double R;

public:
	FRiS(Metric& m, double ParamR):M(m), R(ParamR) {}

	double MinDist(typename IDArray& Stolps, typename IDArray::PrecType Prec) {
		double a, Min = DBL_MAX;
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				a = M.Dist(*j,Prec);
				if(a < Min) Min = a;
			}
		}
		return Min;
	}

	std::pair<double, double> MinDist(typename IDArray& Stolps, typename IDArray::PrecType Prec, typename IDArray::ClassType Class) {
		double a;
		std::pair<double, double> ret;
		ret.first = DBL_MAX; ret.second = DBL_MAX;
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			if(Class == i->Class) {
				for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					a = M.Dist(*j,Prec);
					if(a < ret.first) ret.first = a;
				}
			}
			else{
				for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					a = M.Dist(*j,Prec);
					if(a < ret.second) ret.second = a;
				}
			}
		}
		return ret;
	}

	typename IDArray::ClassType MinClass(typename IDArray& Stolps, typename IDArray::PrecType Prec) {
		double a, Min = DBL_MAX;
		IDArray::ClassType CT;
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				a = M.Dist(*j,Prec);
				if(a < Min) { Min = a; CT = i->Class; }
			}
		}
		return CT;
	}

	double rQualityRating(typename IDArray& TS, typename IDArray& Stolps) {
		double Q = 0, Num = 0, Min;
		for(IDArray::ClassIter i = TS.Classes.begin(); i!=TS.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				Min = MinDist(Stolps, *j);
				Q+=(R - Min)/(R + Min);
			}
			Num += i->Precedents.size();
		}
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			double N = i->Precedents.size();
			Q += N;
			Num += N;
		}
		return Q / Num;
	}

	double QualityRating(typename IDArray& TS, typename IDArray& Stolps) {
		double Q = 0, Num = 0;
		pair<double, double> FriendFoe;
		for(IDArray::ClassIter i = TS.Classes.begin(); i!=TS.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				FriendFoe = MinDist(Stolps,*j,i->Class);
				Q+=(FriendFoe.second - FriendFoe.first)/(FriendFoe.second + FriendFoe.first);
			}
			Num += i->Precedents.size();
		}
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			double N = i->Precedents.size();
			Q += N;
			Num += N;
		}
		return Q / Num;
	}

	void AddNextCluster(typename IDArray& TS, typename IDArray& Stolps){
		std::pair<IDArray::ClassIter, IDArray::PrecIter> i, a;
		double Q, Max = -(DBL_MAX);
		typename IDArray::PrecType MaxID;
		typename IDArray::ClassType MaxClass;

		i.first = TS.Classes.begin();
		while (i.first != TS.Classes.end()) {
			i.second = i.first->Precedents.begin();
			while(i.second != i.first->Precedents.end()) {
				a = TS.Move(Stolps, *(i.second), i.first->Class);
				Q = rQualityRating(TS,Stolps);
				i = TS.Move(Stolps, a);
				if(Q > Max) { Max = Q; MaxClass = i.first->Class; MaxID = *i.second; }
				++i.second;
			}
			++i.first;
		}
		TS.Move(Stolps, MaxID, MaxClass);
	}

	void AddStolpForClass(typename IDArray& TS, typename IDArray& Stolps, typename IDArray::ClassType CT) {
		std::pair<IDArray::ClassIter, IDArray::PrecIter> i, a;
		std::pair<IDArray::ClassIter, IDArray::ClassIter> b;
		double Q, Max = -(DBL_MAX);
		typename IDArray::PrecType MaxID;

		b = TS.Classes.equal_range(IDArray::ID_Array(CT));
		if(b.first == b.second) return;
		
		i.first = b.first;
		i.second = i.first->Precedents.begin();
		while(i.second != i.first->Precedents.end()) {
			a = TS.Move(Stolps, *(i.second), i.first->Class);
			Q = QualityRating(TS,Stolps);
			i = TS.Move(Stolps, a);
			if(Q > Max) { Max = Q; MaxID = *i.second; }
			++i.second;
		}
		TS.Move(Stolps, MaxID, i.first->Class);
	}

	void SpecifyStolps(typename IDArray& TS, typename IDArray& Stolps) {
		if(Stolps.Size()< 2) return;
		IDArray copy(Stolps);
		for(IDArray::ClassIter i = copy.Classes.begin(); i!=copy.Classes.end(); ++i) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j!=i->Precedents.end(); ++j) {
				Stolps.Move(TS,*j, i->Class);
				AddStolpForClass(TS,Stolps,i->Class);
			}
		}
	}

	double NextLocalMax(typename IDArray& TS, typename IDArray& OldStolps, typename IDArray& OutTS, typename IDArray& OutStolps, double& Q2, unsigned n, double Q1 = -1) {
		std::pair<IDArray*,IDArray*> a, b;
		IDArray NTS,NS;
		double Q3 = 1;
		unsigned i, NumStolps = OldStolps.Size(), NumObj = TS.Size();

		if(NumStolps >= n || NumObj == 0) {
			NewClasses(OldStolps,OutStolps);
			Reclassify(TS,OutStolps,OutTS);
			SpecifyStolps(OutTS, OutStolps);
			Q2 = QualityRating(OutTS, OutStolps);
			return -1.0;
		}

		if(NumStolps == 0) {
			AddNextCluster(TS,OldStolps);
			++NumStolps;
			--NumObj;
		}
		
		AddNextCluster(TS,OldStolps);
		NewClasses(OldStolps,NS);
		Reclassify(TS,NS,NTS);
		a.first = new IDArray(NTS);
		a.second = new IDArray(NS);
		SpecifyStolps(*a.first, *a.second);
		Q2 = QualityRating(*a.first, *a.second);
		for(i = min((n - NumStolps),NumObj) - 1; i>0; --i){
			AddNextCluster(TS,OldStolps);
			NewClasses(OldStolps,NS);
			Reclassify(TS,NS,NTS);
			b.first = new IDArray(NTS);
			b.second = new IDArray(NS);
			SpecifyStolps(*b.first, *b.second);
			Q3 = QualityRating(*b.first, *b.second);
			if(Q1<Q2 && Q3<Q2) {
				delete b.first; delete b.second;
				break;
			}
			Q1 = Q2; Q2 = Q3;
			delete a.first;
			delete a.second;
			a = b;
		}
		OutTS = *a.first;
		OutStolps = *a.second;
		delete a.first; delete a.second;
		return Q3;
	}

	void NewClasses(typename IDArray& OldStolps, typename IDArray& NewStolps) {
		IDArray::ClassType CT = 0;
		NewStolps.Classes.clear();
		for(IDArray::ClassIter i = OldStolps.Classes.begin(); i!=OldStolps.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				NewStolps.AddPrecedent(*j,CT);
				++CT;
			}
		}
	}

	void Reclassify (typename IDArray& OldTS, typename IDArray& Stolps, typename IDArray& NewTS){
		NewTS.Classes.clear();
		for(IDArray::ClassIter i = OldTS.Classes.begin(); i!=OldTS.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				NewTS.AddPrecedent(*j,MinClass(Stolps, *j));
			}
		}
	}
};

template < typename Metric, typename IDArray = PrecedentSet <unsigned> >
class FRiS_Class{
private:
	Metric& M;
	double FStar;
	double alfa;

public:
	FRiS_Class(Metric& m, double F_Star, double Alfa):M(m), FStar(F_Star), alfa(Alfa) {}

/*	double MinFoe(typename IDArray& Stolps, typename IDArray::ClassType Class, typename IDArray::PrecType Prec) {
		double a, Min = DBL_MAX;
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			if(Class == i->Class) continue;
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				a = M.Dist(*j,Prec);
				if(a < Min) Min = a;
			}
		}
		return Min;
	}*/

	bool InArea(typename IDArray& Stolps, typename IDArray::PrecType Prec, typename IDArray::ClassIter FoeClass, typename IDArray::ClassIter FriendClass) {
		double a, Foe = DBL_MAX, Friend = DBL_MAX;
		for(IDArray::PrecIter j = FoeClass->Precedents.begin(); j != FoeClass->Precedents.end(); ++j) {
			a = M.Dist(*j,Prec);
			if(a < Foe) Foe = a;
		}
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			if(i->Class == FoeClass->Class || i->Class == FriendClass->Class ) continue;
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				if(M.Dist(*j,Prec) < Foe) return false;
			}
		}
		for(IDArray::PrecIter j = FriendClass->Precedents.begin(); j != FriendClass->Precedents.end(); ++j) {
			if(*j == Prec) continue;
			a = M.Dist(*j,Prec);
			if(a < Friend) Friend = a;
		}
		double F = (Foe - Friend)/(Foe + Friend);
		if((-FStar)>F || F>FStar) return false;
		return true;
	}

	void UniteClusters(typename IDArray& TS, typename IDArray& Stolps) {
		typedef vector<typename IDArray::PrecType> PrecVect;
		PrecVect cl1,cl2;
		IDArray::ClassIter i, j;
		double a, Dab, Da, Db;
		IDArray::PrecType A, B;

		for(i = TS.Classes.begin(); i!=TS.Classes.end(); ++i ) {
			j = i; ++j;
			if(j==TS.Classes.end()) break;
			while(j!=TS.Classes.end()) {
				cl1.clear(); cl2.clear();
				for(IDArray::PrecIter k = i->Precedents.begin(); k != i->Precedents.end(); ++k) {
					if(InArea(Stolps, *k, j, i)) cl1.push_back(*k);
				}
				if(cl1.size() == 0) { ++j; continue; }
				for(IDArray::PrecIter k = j->Precedents.begin(); k != j->Precedents.end(); ++k) {
					if(InArea(Stolps, *k, i, j)) cl2.push_back(*k);
				}
				if(cl2.size() == 0) { ++j; continue; }
				Db = Da = Dab = DBL_MAX;
				for(PrecVect::iterator l = cl1.begin(); l!=cl1.end(); ++l) {
					for(PrecVect::iterator m = cl2.begin(); m!=cl2.end(); ++m) {
						a = M.Dist(*l,*m);
						if(a < Dab) {Dab = a; A = *l; B = *m;}
					}
				}
				for(IDArray::PrecIter k = i->Precedents.begin(); k != i->Precedents.end(); ++k) {
					if(*k == A) continue;
					a = M.Dist(*k,A);
					if(a < Da) Da = a;
				}
				for(IDArray::PrecIter k = j->Precedents.begin(); k != j->Precedents.end(); ++k) {
					if(*k == B) continue;
					a = M.Dist(*k,B);
					if(a < Db) Db = a;
				}
				std::pair<IDArray::ClassIter, IDArray::ClassIter> rA,rB;
				rA = Stolps.Classes.equal_range(i->Class);
				if(rA.first!=rA.second) {
					for(IDArray::PrecIter n = rA.first->Precedents.begin(); n!=rA.first->Precedents.end(); ++n) {
						a = M.Dist(*n,A);
						if(a < Da) Da = a;
					}
				}
				rB = Stolps.Classes.equal_range(j->Class);
				if(rB.first!=rB.second) {
					for(IDArray::PrecIter n = rB.first->Precedents.begin(); n!=rB.first->Precedents.end(); ++n) {
						a = M.Dist(*n,B);
						if(a < Db) Db = a;
					}
				}
				if(Da < alfa*Db && Db < alfa*Da && Dab < alfa*(Da+Db)/2) {
					for(IDArray::PrecIter k = j->Precedents.begin(); k != j->Precedents.end(); ++k) {
						i->Precedents.insert(*k);
					}
					TS.Classes.erase(j);
					j = i; ++j;
					if(rB.first!=rB.second) {
						for(IDArray::PrecIter n = rB.first->Precedents.begin(); n!=rB.first->Precedents.end(); ++n) {
							Stolps.AddPrecedent(*n,i->Class);
						}
						Stolps.Classes.erase(rB.first);
					}
				}
				else { ++j; }
			}
		}
	}

};

template < typename Metric, typename IDArray = PrecedentSet <unsigned> >
class FRiS_TDR{
private:
	Metric& M;
	double R;
	typename IDArray::ClassType NoClassif;

public:
	FRiS_TDR(Metric& m, double ParamR, typename IDArray::ClassType No_Classif):M(m), R(ParamR), NoClassif(No_Classif) {}
	double MinFoe(typename IDArray& Stolps, typename IDArray::ClassType Class, typename IDArray::PrecType Prec) {
		double a, Min = DBL_MAX;
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			if(Class == i->Class || Class == NoClassif) continue;
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				a = M.Dist(*j,Prec);
				if(a < Min) Min = a;
			}
		}
		return Min;
	}

	double MinFriend(typename IDArray& Stolps, typename IDArray::ClassType Class, typename IDArray::PrecType Prec) {
		double a, Min = DBL_MAX;
		std::pair<IDArray::ClassIter, IDArray::ClassIter> b;
		b = Stolps.Classes.equal_range(IDArray(Class));
		for(IDArray::PrecIter j = b.first->Precedents.begin(); j != b.first->Precedents.end(); ++j) {
			a = M.Dist(*j,Prec);
			if(a < Min) Min = a;
		}
		return Min;
	}

	double QualityRating(typename IDArray& TS, typename IDArray& Stolps, typename IDArray::ClassType Class) {
		double Q = 0, Num = 0, Min, Foe, Friend;
		for(IDArray::ClassIter i = TS.Classes.begin(); i!=TS.Classes.end(); ++i ) {
			for(IDArray::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
				//if(i->Class == NoClassif == Class) {???
				if(i->Class == Class) {
					Foe = min(MinFoe(TS,Class,*j),MinFoe(Stolps,Class,*j));
					Friend = MinFriend(Stolps, Class, *j);
				}
				else {
					if(i->Class == NoClassif) {
					}
					else {
						Foe = min(MinFoe(TS,i->Class,*j),MinFoe(Stolps,i->Class,*j));
						
					}
				}
				Foe = min(R,Foe);
				Q += (Foe-Friend)/(Foe+Friend);
			}
			Num += i->Precedents.size();
		}
		for(IDArray::ClassIter i = Stolps.Classes.begin(); i!=Stolps.Classes.end(); ++i ) {
			double N = i->Precedents.size();
			Q += N;
			Num += N;
		}
		return Q / Num;
	}

};