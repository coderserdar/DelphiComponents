//---------------------------------------------------------------------------

#ifndef MainUnitH
#define MainUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "FBCustomDataSet.hpp"
#include "jvuib.hpp"
#include "mydbUnit.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TStatusBar *StatusBar1;
        TPageControl *PageControl1;
        TTabSheet *TabSheet1;
        TTabSheet *tabDirectories;
        TPageControl *PageControl2;
        TTabSheet *tabSprCountry;
        TDBGrid *DBGrid1;
        TTabSheet *tabCustomer;
        TDBGrid *DBGrid2;
        TJvUIBDataBase *JvUIBDataBase;
        TJvUIBTransaction *trRead;
        TJvUIBTransaction *trWrite;
        TFBDataSet *quSprCountry;
        TDataSource *dsSprCountry;
        TDataSource *dsSprCustomer;
        TFBDataSet *quSprCustomer;
        TActionList *ActionList1;
        void __fastcall PageControl2Change(TObject *Sender);
        void __fastcall PageControl1Change(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
