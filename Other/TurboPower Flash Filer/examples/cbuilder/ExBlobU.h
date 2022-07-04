//---------------------------------------------------------------------------

#ifndef ExBlobUH
#define ExBlobUH
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
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TDBText *DbText1;
  TDBText *DbText2;
  TDBText *DbText3;
  TDBImage *DbImage1;
  TDBNavigator *DBNavigator1;
  TffCommsEngine *ffCommsEngine1;
  TffSession *ffSession1;
  TDataSource *BlobSource;
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
  TMenuItem *Images1;
  TMenuItem *Bmp1;
  TMenuItem *Jpeg1;
  TMenuItem *All1;
  TffTable *BlobTable;
  void __fastcall Open1Click(TObject *Sender);
  void __fastcall Close1Click(TObject *Sender);
  void __fastcall Exit1Click(TObject *Sender);
  void __fastcall First1Click(TObject *Sender);
  void __fastcall Last1Click(TObject *Sender);
  void __fastcall Next1Click(TObject *Sender);
  void __fastcall Prior1Click(TObject *Sender);
  void __fastcall All1Click(TObject *Sender);
  void __fastcall Bmp1Click(TObject *Sender);
  void __fastcall Jpeg1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
