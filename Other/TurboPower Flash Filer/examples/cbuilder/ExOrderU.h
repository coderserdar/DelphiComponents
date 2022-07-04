//---------------------------------------------------------------------------

#ifndef ExOrderUH
#define ExOrderUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffdb.hpp"
#include "ffdbbase.hpp"
#include "ffllbase.hpp"
#include <Db.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
#include "ffclreng.hpp"
#include "ffllcomm.hpp"
#include "ffllcomp.hpp"
#include "fflleng.hpp"
#include "fflllgcy.hpp"
#include "ffsrintm.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TGroupBox *GroupBox1;
  TDBGrid *DBGrid1;
  TGroupBox *GroupBox2;
  TDBGrid *DBGrid4;
  TDBGrid *DBGrid3;
  TffSession *ffSession1;
  TDataSource *CustomerData;
  TDataSource *LinesData;
  TDataSource *OrdersData;
  TffTable *CustomerTable;
  TffTable *OrdersTable;
  TffTable *LinesTable;
  TIntegerField *LinesTableLineID;
  TIntegerField *LinesTableOrderID;
  TIntegerField *LinesTableProductID;
  TIntegerField *LinesTableCount;
  TStringField *LinesTableDescription;
  TCurrencyField *LinesTableTotal;
  TCurrencyField *LinesTablePrice;
  TffTable *ProductTable;
  TMainMenu *MainMenu1;
  TMenuItem *File1;
  TMenuItem *Open1;
  TMenuItem *Close1;
  TMenuItem *N1;
  TMenuItem *Exit1;
        TffClient *FFClient;
    TffLegacyTransport *ffLegacyTransport1;
    TFFRemoteServerEngine *FFRemoteServerEngine1;
  void __fastcall LinesTableCalcFields(TDataSet *DataSet);
  void __fastcall Open1Click(TObject *Sender);
  void __fastcall Close1Click(TObject *Sender);
  void __fastcall Exit1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
