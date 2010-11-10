#include <windows.h>
#include <vector>
#include <fstream>
#include <set>
#include <math.h>
#include <string.h>

#include "resource.h"
#include "fris.h"

#define PICSIZE 15 //Размер (пикселы) пиктограммы объекта
#define PENSIZE 3 //Ширина карандаша
#define SELSIZE 4 //Отступ (пикселы) для рамки выделения
#define PARAM_R 30
#define MAX_STOLPS 25
#define F_STAR 0.5
#define ALFA 3

using namespace std;

LRESULT CALLBACK WindowFunc(HWND, UINT, WPARAM, LPARAM);
char szWinName[] = "PointView";
HINSTANCE hInst;

class Point {
public:
	int X;
	int Y;
	Point(int x,int y): X(x),Y(y) {}
	operator RECT() {
		RECT r;
		SetRect(&r,X-SELSIZE,Y-SELSIZE,X+PICSIZE+SELSIZE,Y+PICSIZE+SELSIZE);
		return r;
	}
};

typedef vector<Point> PointArray;

class PointFile {
public:
	static bool Save(const PointArray& PA, char* FileName) {
		ofstream f(FileName);
		f << "X\tY";
		for(unsigned i = 0; i < PA.size(); ++i)
			f << "\n" << PA[i].X << '\t' << PA[i].Y;
		return false;
	}
	static bool Open(PointArray& PA, char* FileName) {
		char buf[10];
		Point A(0,0);
		ifstream f(FileName);
		if(!f) return true;
		f >> buf; if(lstrcmp(buf,"X")) return true;
		f >> buf; if(lstrcmp(buf,"Y")) return true;
		while(!f.eof()){
			f >> A.X; f >> A.Y;
			PA.push_back(A);
		}
		return false;
	}
	static char* GetFilter() { return "PoinView TrainingSample files *.pvt\0*.pvt\0All files *.*\0*.*\0"; }
	static char* GetExtension() { return "pvt"; }
	static char* GetSaveMsg () { return "Save sample?";}
};

typedef vector<unsigned> ClassArray;

class ClassFile {
public:
	static bool Save(const ClassArray& CA, char* FileName) {
		ofstream f(FileName);
		f << "Class";
		for(unsigned i = 0; i < CA.size(); ++i)
			f << "\n" << CA[i];
		return false;
	}
	static bool Open(ClassArray& CA, char* FileName) {
		char buf[10];
		unsigned A;
		ifstream f(FileName);
		if(!f) return true;
		f >> buf; if(lstrcmp(buf,"Class")) return true;
		while(!f.eof()){
			f >> A;
			CA.push_back(A);
		}
		return false;
	}
	static char* GetFilter() { return "PoinView Classification files *.pvc\0*.pvc\0All files *.*\0*.*\0"; }
	static char* GetExtension() { return "pvc"; }

	static char* GetSaveMsg () { return "Save classification?";}
};

typedef set<unsigned> IndexArray;
class StolpsFile {
public:
	static bool Save(const IndexArray& SA, char* FileName) {
		ofstream f(FileName);
		f << "Stolp";
		for(IndexArray::const_iterator i = SA.begin(); i != SA.end(); ++i)
			f << "\n" << *i;
		return false;
	}
	static bool Open(IndexArray& SA, char* FileName) {
		char buf[10];
		unsigned A;
		ifstream f(FileName);
		if(!f) return true;
		f >> buf; if(lstrcmp(buf,"Stolp")) return true;
		while(!f.eof()){
			f >> A;
			SA.insert(A);
		}
		return false;
	}
	static char* GetFilter() { return "PoinView Stolp files *.pvs\0*.pvs\0All files *.*\0*.*\0"; }
	static char* GetExtension() { return "pvs"; }
	static char* GetSaveMsg () { return "Save stolps?";}
};

template <  typename Doc, typename DocFile, typename Element >
class Document {
private:
	bool edit;
	char FileName[MAX_PATH];
	HWND hWnd;
	HINSTANCE hInst;
	bool Close() {
		if(!D) return false;
		if(edit) {
			unsigned r = MessageBox(hWnd,DocFile::GetSaveMsg(),"Warning",MB_YESNOCANCEL | MB_ICONWARNING);
			switch(r) {
				case IDCANCEL:
					return true;
				case IDNO:
					break;
				case IDYES:
					Save();
					break;
			}
		}
		delete D;
		return false;
	}

public:
	Doc* D;
	Document(HWND hwnd, HINSTANCE hinst)
		:hWnd(hwnd),hInst(hinst),D(0) { New(); }
	~Document() { Close(); }
	void SetEdit() { edit = true; }
	void New() {
		if(Close()) return;
		D = new(Doc);
		edit = false;
		FileName[0] = 0;
	}
	void New(Doc* d) {
		delete D;
		D = d;
	}
	void Save() {
		if(FileName[0]) { DocFile::Save(*D,FileName); edit = false; }
		else SaveAs();
	}
	void SaveAs() {
		OPENFILENAME of;
		ZeroMemory(&of,OPENFILENAME_SIZE_VERSION_400A);
		of.lStructSize=OPENFILENAME_SIZE_VERSION_400A;
		of.hwndOwner=hWnd;
		of.hInstance=hInst;
		of.lpstrFile=FileName;
		of.nMaxFile=MAX_PATH;
		of.Flags=OFN_PATHMUSTEXIST|OFN_HIDEREADONLY;
		of.lpstrFilter=DocFile::GetFilter();
		of.nFilterIndex = 0;
		of.lpstrDefExt=DocFile::GetExtension();
		if (!GetSaveFileName(&of)) return;
		DocFile::Save(*D,FileName);
		edit = false;
	}
	void Open() {
		New();
		OPENFILENAME of;
		ZeroMemory(&of,OPENFILENAME_SIZE_VERSION_400A);
		of.lStructSize=OPENFILENAME_SIZE_VERSION_400A;
		of.hwndOwner=hWnd;
		of.hInstance=hInst;
		of.lpstrFile=FileName;
		of.nMaxFile=MAX_PATH;
		of.Flags=OFN_PATHMUSTEXIST|OFN_FILEMUSTEXIST|OFN_HIDEREADONLY;
		of.lpstrFilter=DocFile::GetFilter();
		of.nFilterIndex = 0;
		if (!GetOpenFileName(&of)) return;
		DocFile::Open(*D,FileName);
	}
};

class PDist {
private:
	PointArray* P;
public:
	PDist(PointArray* p): P(p) {}
	void operator = (PointArray* p) { P = p; }
	double Dist(unsigned a, unsigned b) {
		return sqrt(pow((double)((*P)[b].X-(*P)[a].X),2)+pow((double)((*P)[b].Y-(*P)[a].Y),2));
	}
};

typedef FRiS< PDist > MyFRiS;

int WINAPI WinMain (HINSTANCE hThisInst,HINSTANCE hPrevInst, 
					LPSTR lpszArgs,int nWinMode){
	HWND hwnd; 
	MSG msg; 
	WNDCLASS wcl;
	hInst = hThisInst;

	wcl.hInstance = hThisInst; 
	wcl.lpszClassName = szWinName; 
	wcl.lpfnWndProc = WindowFunc; 
	wcl.style = 0; 
	wcl.hIcon = LoadIcon (NULL,IDI_APPLICATION); 
	wcl.hCursor = LoadCursor (NULL, IDC_ARROW); 
	wcl.lpszMenuName = "IDR_MENU"; 
	wcl.cbClsExtra = 0; 
	wcl.cbWndExtra = 0; 
	wcl.hbrBackground= (HBRUSH)GetStockObject (WHITE_BRUSH); 

	if ( !RegisterClass (&wcl) ) 
	return 0; 

	hwnd = CreateWindow (szWinName,	"PointView", WS_OVERLAPPEDWINDOW, 
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 
		HWND_DESKTOP, NULL, hThisInst, NULL);

	ShowWindow (hwnd,nWinMode); 
	UpdateWindow (hwnd); 

	while ( GetMessage (&msg, NULL,0,0) ) { 
		TranslateMessage (&msg); 
		DispatchMessage (&msg); 
	} 

	return msg.wParam; 
} 

LRESULT CALLBACK WindowFunc(HWND hwnd,UINT message, WPARAM wParam,LPARAM lParam) { 
	static HPEN hPen[4];
	static unsigned short CurrentClass;
	static Document<PointArray, PointFile, Point> TS(hwnd, hInst);
	static Document<ClassArray, ClassFile, unsigned> Classif(hwnd, hInst);
	static Document<IndexArray, StolpsFile, unsigned> Stolps(hwnd, hInst);
	static PDist PD(TS.D);
	static IndexArray SA;
	static MyFRiS MF(PD, PARAM_R);
	static PrecedentSet<unsigned> sPS, sStolps;
	static double sQ = -1;
	//set<int> asd;
	//asd.equal_range
	
	switch (message) { 
	case WM_CREATE: {
		hPen[0] = CreatePen(PS_SOLID,PENSIZE,RGB(0,0,0));
		hPen[1] = CreatePen(PS_SOLID,PENSIZE,RGB(255,0,0));
		hPen[2] = CreatePen(PS_SOLID,PENSIZE,RGB(0,255,0));
		hPen[3] = CreatePen(PS_SOLID,PENSIZE,RGB(0,0,255));
		CheckMenuRadioItem(GetMenu(hwnd),IDM_Select,IDM_Blue_cross,IDM_Select,MF_BYCOMMAND);
		CurrentClass = IDM_Select;
		break;
	}

	case WM_LBUTTONUP:
		if(CurrentClass!=IDM_Select) {
			TS.D->push_back(Point(LOWORD(lParam),HIWORD(lParam)));
			Classif.D->push_back(CurrentClass-IDM_Black_circle);
			TS.SetEdit();
			Classif.SetEdit();
		}
		else {
			PointArray::iterator i;
			for(i = TS.D->begin(); i != TS.D->end() ; ++i) {
				if((i->X <= LOWORD(lParam) && i->X+PICSIZE >= LOWORD(lParam))
					&& (i->Y <= HIWORD(lParam) && i->Y+PICSIZE >= HIWORD(lParam))) break;
			}
			if(i!=TS.D->end()) {
				if(wParam == MK_SHIFT) SA.insert(i-TS.D->begin());
				else {
					SA.clear();
					SA.insert(i-TS.D->begin());
				}
			}
			else SA.clear();
		}
		InvalidateRect(hwnd,NULL,TRUE);
		break;

	case WM_COMMAND:
		if((LOWORD(wParam)>=IDM_Select) && (LOWORD(wParam)<=IDM_Blue_cross)) {
			if(!(SA.size())) {
				CheckMenuRadioItem(GetMenu(hwnd),IDM_Select,IDM_Blue_cross,LOWORD(wParam),MF_BYCOMMAND);
				CurrentClass = LOWORD(wParam);
				break;
			}
			else {
				for(IndexArray::iterator i = SA.begin(); i != SA.end(); ++i)
					*(Classif.D->begin()+*i) = LOWORD(wParam)-IDM_Black_circle;
				Classif.SetEdit();
				InvalidateRect(hwnd,NULL,TRUE);
			}
		}

		switch(LOWORD(wParam)){
		case IDM_TS_NEW:
			SA.clear();
			Stolps.New();
			TS.New();
			Classif.New();
			PD = TS.D;
			InvalidateRect(hwnd,NULL,TRUE);
			break;

		case IDM_TS_OPEN:
			SA.clear();
			Stolps.New();
			Classif.New();
			TS.Open();
			Classif.D->insert(Classif.D->begin(),TS.D->size(),0);
			PD = TS.D;
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_TS_SAVE:
			TS.Save();
			break;
		case IDM_TS_SAVEAS:
			TS.SaveAs();
			break;

		case IDM_C_NEW: {
			unsigned i;
			Classif.New();
			for(i = 0; i < TS.D->size(); ++i) Classif.D->push_back(0);
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		}
		case IDM_C_OPEN:
			Classif.Open();
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_C_SAVE:
			Classif.Save();
			break;
		case IDM_C_SAVEAS:
			Classif.SaveAs();
			break;

		case IDM_S_NEW:
			Stolps.New();
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_S_OPEN:
			Stolps.Open();
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_S_SAVE:
			Stolps.Save();
			break;
		case IDM_S_SAVEAS:
			Stolps.SaveAs();
			break;
		case IDM_DELETE:
			for(IndexArray::reverse_iterator i = SA.rbegin(); i != SA.rend(); ++i) {
				TS.D->erase(TS.D->begin()+*i);
				Classif.D->erase(Classif.D->begin()+*i);
				for(IndexArray::iterator j = Stolps.D->begin(); j != Stolps.D->end(); ++j) {
					if(*j==*i) { Stolps.D->erase(j); Stolps.SetEdit(); break; }
				}
				TS.SetEdit();
				Classif.SetEdit();
			}
			SA.clear();
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_SET_STOLP:
			for(IndexArray::iterator i = SA.begin(); i != SA.end(); ++i) {
				Stolps.SetEdit();
				Stolps.D->insert(*i);
			}
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_UNSET_STOLP:
			for(IndexArray::reverse_iterator i = SA.rbegin(); i != SA.rend(); ++i) {
				for(IndexArray::iterator j = Stolps.D->begin(); j != Stolps.D->end(); ++j) {
					if(*j==*i) { Stolps.D->erase(j); Stolps.SetEdit(); break; }
				}
			}
			InvalidateRect(hwnd,NULL,TRUE);
			break;
		case IDM_AddStolp: {
			PrecedentSet<unsigned> PS,NewPS;
			PrecedentSet<unsigned> PSStolps, NewStolps;
			for(unsigned i = 0; i<TS.D->size(); ++i) {
				PS.AddPrecedent(i,(*Classif.D)[i]);
			}
			for(IndexArray::iterator i = Stolps.D->begin(); i!=Stolps.D->end(); ++i) {
				PS.Move(PSStolps, *i, (*Classif.D)[*i]);
			}
			MF.AddNextCluster(PS,PSStolps);
			MF.NewClasses(PSStolps, NewStolps);
			MF.Reclassify(PS,NewStolps,NewPS);
			Stolps.D->clear();
			for(PrecedentSet<unsigned>::ClassIter i = NewPS.Classes.begin(); i != NewPS.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
				}
			}
			for(PrecedentSet<unsigned>::ClassIter i = NewStolps.Classes.begin(); i != NewStolps.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
					Stolps.D->insert(*j);
				}
			}
			InvalidateRect(hwnd,NULL,TRUE);
			}
		break;
		case IDM_SpecifyStolps: {
			PrecedentSet<unsigned> PS;
			PrecedentSet<unsigned> PSStolps;
			for(unsigned i = 0; i<TS.D->size(); ++i) {
				PS.AddPrecedent(i,(*Classif.D)[i]);
			}
			for(IndexArray::iterator i = Stolps.D->begin(); i!=Stolps.D->end(); ++i) {
				PS.Move(PSStolps, *i, (*Classif.D)[*i]);
			}
			MF.SpecifyStolps(PS,PSStolps);
			Stolps.D->clear();
			for(PrecedentSet<unsigned>::ClassIter i = PSStolps.Classes.begin(); i != PSStolps.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					Stolps.D->insert(*j);
				}
			}
			InvalidateRect(hwnd,NULL,TRUE);
		}
		break;
		case IDM_QualityRating: {
			PrecedentSet<unsigned> PS;
			PrecedentSet<unsigned> PSStolps;
			char buf[1024];
			double Q1, Q2;

			for(unsigned i = 0; i<TS.D->size(); ++i) {
				PS.AddPrecedent(i,(*Classif.D)[i]);
			}
			for(IndexArray::iterator i = Stolps.D->begin(); i!=Stolps.D->end(); ++i) {
				PS.Move(PSStolps, *i, (*Classif.D)[*i]);
			}
			Q1 = MF.QualityRating(PS,PSStolps);
			Q2 = MF.rQualityRating(PS,PSStolps);
			sprintf_s(buf,1024,"QualityRating1 = %lf, QualityRating2 = %lf",Q1,Q2);
			MessageBox(hwnd,buf,"Info",MB_OK);
			
		}
		break;
		case IDM_FirstLocalMax: {
			double Q;
			PrecedentSet<unsigned> NewPS, NewStolps;
			sPS.Classes.clear();
			sStolps.Classes.clear();
			for(unsigned i = 0; i<TS.D->size(); ++i) {
				sPS.AddPrecedent(i,(*Classif.D)[i]);
			}
			for(IndexArray::iterator i = Stolps.D->begin(); i!=Stolps.D->end(); ++i) {
				sPS.Move(sStolps, *i, (*Classif.D)[*i]);
			}
			sQ = MF.NextLocalMax(sPS,sStolps,NewPS,NewStolps, Q, MAX_STOLPS);
			Stolps.D->clear();
			for(PrecedentSet<unsigned>::ClassIter i = NewPS.Classes.begin(); i != NewPS.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
				}
			}
			for(PrecedentSet<unsigned>::ClassIter i = NewStolps.Classes.begin(); i != NewStolps.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
					Stolps.D->insert(*j);
				}
			}
			InvalidateRect(hwnd,NULL,TRUE);
		}
		break;
		case IDM_NextLocalMax: {
			double Q;
			PrecedentSet<unsigned> NewPS;
			PrecedentSet<unsigned> NewStolps;
			sQ = MF.NextLocalMax(sPS,sStolps,NewPS,NewStolps, Q, MAX_STOLPS, sQ);
			Stolps.D->clear();
			for(PrecedentSet<unsigned>::ClassIter i = NewPS.Classes.begin(); i != NewPS.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
				}
			}
			for(PrecedentSet<unsigned>::ClassIter i = NewStolps.Classes.begin(); i != NewStolps.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = i->Class;
					Stolps.D->insert(*j);
				}
			}
			InvalidateRect(hwnd,NULL,TRUE);
		}
		break;
		case IDM_UniteClusters: {
			PrecedentSet<unsigned> PS;
			PrecedentSet<unsigned> PSStolps;
			unsigned k;
			PDist PD(TS.D);
			FRiS_Class<PDist> MF(PD, F_STAR, ALFA);
			for(unsigned i = 0; i<TS.D->size(); ++i) {
				PS.AddPrecedent(i,(*Classif.D)[i]);
			}
			for(IndexArray::iterator i = Stolps.D->begin(); i!=Stolps.D->end(); ++i) {
				PS.Move(PSStolps, *i, (*Classif.D)[*i]);
			}
			MF.UniteClusters(PS,PSStolps);
			k = 0;
			for(PrecedentSet<unsigned>::ClassIter i = PS.Classes.begin(); i != PS.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = k;
				}
				++k;
			}
			k = 0;
			for(PrecedentSet<unsigned>::ClassIter i = PSStolps.Classes.begin(); i != PSStolps.Classes.end(); ++i) {
				for(PrecedentSet<unsigned>::PrecIter j = i->Precedents.begin(); j != i->Precedents.end(); ++j) {
					(*Classif.D)[*j] = k;
					Stolps.D->insert(*j);
				}
				++k;
			}
			InvalidateRect(hwnd,NULL,TRUE);
		}
		break;
	}
	break;
	case WM_PAINT:	{
		PAINTSTRUCT ps;
		HDC hdc = BeginPaint(hwnd,&ps);
		PointArray::iterator i;
		ClassArray::iterator j = Classif.D->begin();
		for(i = TS.D->begin(); i != TS.D->end(); ++i) {
			if(*j < 12){
				SelectObject(hdc,hPen[*j/3]);
				switch(*j%3){
				case 0:
					Ellipse(hdc,i->X,i->Y,i->X+PICSIZE,i->Y+PICSIZE);
					break;
				case 1:
					Rectangle(hdc,i->X,i->Y,i->X+PICSIZE,i->Y+PICSIZE);
					break;
				case 2:
					MoveToEx(hdc,i->X,i->Y,NULL);
					LineTo(hdc,i->X+PICSIZE,i->Y+PICSIZE);
					MoveToEx(hdc,i->X,i->Y+PICSIZE,NULL);
					LineTo(hdc,i->X+PICSIZE,i->Y);
					break;
				}
			}
			else {
				SelectObject(hdc,hPen[3]);
				MoveToEx(hdc,i->X,i->Y,NULL);
				LineTo(hdc,i->X+PICSIZE,i->Y+PICSIZE);
				MoveToEx(hdc,i->X,i->Y+PICSIZE,NULL);
				LineTo(hdc,i->X+PICSIZE,i->Y);
			}
			++j;
		}
		for(IndexArray::iterator i = Stolps.D->begin(); i != Stolps.D->end(); ++i)
			InvertRect(hdc,&(RECT)(*TS.D)[*i]);

		for(IndexArray::iterator i = SA.begin(); i != SA.end(); ++i)
			DrawFocusRect(hdc,&(RECT)(*TS.D)[*i]);

		EndPaint(hwnd,&ps);
	}
	break;
	case WM_DESTROY: 
		PostQuitMessage (0); 
		break; 
	default: 
		return DefWindowProc (hwnd,message,wParam,lParam);
	}
	return 0; 
}
