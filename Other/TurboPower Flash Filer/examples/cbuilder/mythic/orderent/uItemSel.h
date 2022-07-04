//---------------------------------------------------------------------------

#ifndef uItemSelH
#define uItemSelH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TfrmItemSelection : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlBottom;
  TButton *btnSelect;
  TButton *btnCancel;
  TDBGrid *grdItems;
  TDataSource *dtsProducts;
private:	// User declarations
  TDataSet* GetProductsDataset();
  void SetProductsDataset(TDataSet* Value);
public:		// User declarations
  __fastcall TfrmItemSelection(TComponent* Owner);
  __property TDataSet* ProductsDataset =
    {read = GetProductsDataset, write = SetProductsDataset};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmItemSelection *frmItemSelection;
//---------------------------------------------------------------------------
#endif
