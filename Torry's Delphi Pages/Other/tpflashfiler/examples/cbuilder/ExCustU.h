//---------------------------------------------------------------------------

#ifndef ExCustUH
#define ExCustUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffdb.hpp"
#include "ffdbbase.hpp"
#include "ffllbase.hpp"
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TDBGrid *CustomerGrid;
  TDBNavigator *DBNavigator1;
  TffCommsEngine *ffCommsEngine1;
  TffSession *ffSession1;
  TffTable *CustomerTable;
  TDataSource *CustomerData;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *Open1;
  TMenuItem *Close1;
  TMenuItem *N1;
  TMenuItem *Exit1;
  TMenuItem *Navigate1;
  TMenuItem *First1;
  TMenuItem *Last1;
  TMenuItem *Next1;
  TMenuItem *Prior1;
  TMenuItem *Edit1;
  TMenuItem *Append1;
  TMenuItem *Insert1;
  TMenuItem *Post1;
  TMenuItem *Refresh1;
  TMenuItem *N2;
  TMenuItem *Cancel1;
  void __fastcall Open1Click(TObject *Sender);
  void __fastcall Close1Click(TObject *Sender);
  void __fastcall Exit1Click(TObject *Sender);
  void __fastcall First1Click(TObject *Sender);
  void __fastcall Last1Click(TObject *Sender);
  void __fastcall Next1Click(TObject *Sender);
  void __fastcall Prior1Click(TObject *Sender);
  void __fastcall Append1Click(TObject *Sender);
  void __fastcall Insert1Click(TObject *Sender);
  void __fastcall Post1Click(TObject *Sender);
  void __fastcall Refresh1Click(TObject *Sender);
  void __fastcall Cancel1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
