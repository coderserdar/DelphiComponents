//$$---- Form CPP ----
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SXSkinUnit.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "SXSkinControl"
#pragma link "SXSkinImage"
#pragma link "SXSkinLibrary"
#pragma link "SXSkinButton"
#pragma link "SXSkinForm"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Exit1Click(TObject *Sender)
{
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SkinChange(TObject *Sender)
{
	TMenuItem *T;
	if ((T=dynamic_cast<TMenuItem *>(Sender))==0) exit;
	for(int A=0;A<T->Parent->Count;A++)
		T->Parent->Items[A]->Checked=false;
	T->Checked=true;
	SXSkinLibrary1->Active=false;
	if (T->Tag<0) exit;
	AnsiString S=SkinPaths->Strings[T->Tag];
	if ((S!="")&&(S[1]==':'))
	{
		TSXStoredSkin *Skin=GetStoredSkinByZIPName(S.SubString(2,MaxInt));
		SXSkinLibrary1->StoredSkin=Skin;
	}
	else
	{
		SXSkinLibrary1->SkinFile=S;
	}
	SXSkinLibrary1->Active=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
 SkinNames=new TStringList;
 SkinPaths=new TStringList;
 AnsiString S1;
 S1=WithLastSlash(ExtractFilePath(Application->ExeName))+"Skins\\";
 SXSkinLibrary1->SkinDir=WithLastSlash(ExtractFilePath(Application->ExeName))+"Skins";
 if (FileExists(SXSkinLibrary1->SkinDir+"\\App_Skins.zip"))
 {
	 SXSkinLibrary1->SkinFile2=SXSkinLibrary1->SkinDir+"\\App_Skins.zip";
 }
 else
 if (FileExists(SXSkinLibrary1->SkinDir+"\\App_Skins\\skin.sxs"))
 {
	 SXSkinLibrary1->SkinFile2=SXSkinLibrary1->SkinDir+"\\App_Skins\\skin.sxs";
 }
 else
 if (FileExists(SXSkinLibrary1->SkinDir+"\\App_Skins\\skin.ini"))
 {
	 SXSkinLibrary1->SkinFile2=SXSkinLibrary1->SkinDir+"\\App_Skins\\skin.ini";
 }
 else
 {
	 SXSkinLibrary1->SkinFile2="";
 }  
 TStringList *L=new TStringList;


 TStringList *SL=new TStringList;
 for(int A=0;A<StoredSkinCount();A++)
 {
	TSXStoredSkin *Skin=GetStoredSkinByIndex(A);
	LoadStringsFromZIPSkinStream(Skin->Stream,SL);
	if (SL->Values["SkinName"]!="")
	{
		SkinNames->Add(SL->Values["SkinName"]+" (internal)");
		SkinPaths->Add(":"+Skin->FileName);
	}
 }
 SL=NULL;
 

 TSearchRec SR;
 int A=0,B;
 do
 {
	A++;
	if (A==1)
	 B=FindFirst(S1+"*.*",faDirectory,SR);
	  else B=FindNext(SR);
	if ((B==0)&&(SR.Name!=".")&&(SR.Name!="..")) L->Add(S1+SR.Name);
 }
 while (B==0);
 FindClose(SR);

 SL=new TStringList;
 for(A=0;A<L->Count;A++)
 {
	if (FileExists(L->Strings[A]+"\\skin.sxs"))
	{
		LoadStringsFromSkinFile(L->Strings[A]+"\\skin.sxs",SL);
		if (SL->Values["SkinName"]!="")
		{
			SkinNames->Add(SL->Values["SkinName"]+" (SXS)");
			SkinPaths->Add(L->Strings[A]+"\\skin.sxs");
		}
	}
	else if (FileExists(L->Strings[A]+"\\skin.ini"))
	{
		LoadStringsFromSkinFile(L->Strings[A]+"\\skin.ini",SL);
		if (SL->Values["SkinName"]!="")
		{
			SkinNames->Add(SL->Values["SkinName"]+" (INI)");
			SkinPaths->Add(L->Strings[A]+"\\skin.ini");
		}
	}
 }
 SL=NULL;
 L->Clear();


 A=0;
 do
 {
	A++;
	if (A==1)
	 B=FindFirst(S1+"*.zip",faAnyFile,SR);
	  else B=FindNext(SR);
	if ((B==0)&&(SR.Name!=".")&&(SR.Name!="..")) L->Add(S1+SR.Name);
 }
 while (B==0);
 FindClose(SR);

 SL=new TStringList;
 for(A=0;A<L->Count;A++)
 {
	if (FileExists(L->Strings[A]))
	{
		LoadStringsFromSkinFile(L->Strings[A],SL);
		if (SL->Values["SkinName"]!="")
		{
			SkinNames->Add(SL->Values["SkinName"]+" (ZIP)");
			SkinPaths->Add(L->Strings[A]);
		}
	}
 }
 SL=NULL;

 for(A=0;A<SkinNames->Count;A++)
 {
	TMenuItem *T=new TMenuItem(Skins1);
	T->Caption=SkinNames->Strings[A];
	T->Tag=A;
	T->OnClick=SkinChange;
	Skins1->Add(T);
	if (A==0) SkinChange(T);
 }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
 SkinNames=NULL;
 SkinPaths=NULL;
}
//---------------------------------------------------------------------------
