//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
  TToolBar *barToolbar;
  TToolButton *spdNewOrder;
  TToolButton *ToolButton2;
  TToolButton *spdNewCustomer;
  TToolButton *spdDeleteCustomer;
  TPanel *pnlBackAlClient;
  TSplitter *splCustomer_Recent;
  TPanel *pnlCustomerListTitle;
  TLabel *Label2;
  TSpeedButton *spdFindByCompany;
  TEdit *edtCompanySearch;
  TDBGrid *grdCustomer;
  TPanel *pnlOrderAlClient;
  TPanel *pnlRecentOrdersTitle;
  TLabel *Label1;
  TComboBox *cboOrderRange;
  TDBGrid *grdOrders;
  TMainMenu *mnuMain;
  TMenuItem *Orders1;
  TMenuItem *mnuCreateOrder;
  TMenuItem *Show1;
  TMenuItem *mnuOrderRangeAll;
  TMenuItem *mnuOrderRangeLast7;
  TMenuItem *mnuOrderRangeLast30;
  TMenuItem *mnuOrderRange1Year;
  TMenuItem *N2;
  TMenuItem *mnuExit;
  TMenuItem *mnuFile;
  TMenuItem *mnuNewCustomer;
  TMenuItem *mnuDeleteCustomer;
  TMenuItem *N1;
  TMenuItem *mnuConfiguration;
  TMenuItem *mnuEmployees;
  TImageList *imgToolBar;
  void __fastcall mnuExitClick(TObject *Sender);
  void __fastcall mnuEmployeesClick(TObject *Sender);
  void __fastcall mnuDeleteCustomerClick(TObject *Sender);
  void __fastcall mnuNewCustomerClick(TObject *Sender);
  void __fastcall mnuCreateOrderClick(TObject *Sender);
  void __fastcall cboOrderRangeChange(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall mnuOrderRange1YearClick(TObject *Sender);
  void __fastcall spdFindByCompanyClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
  TDataSet* GetCustDataset();
  TDataSet* GetOrderDataset();
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
  // property used to simplify access to the customer table
  __property TDataSet* CustDataset = {read=GetCustDataset};
  // property used to simplify access to the order table
  __property TDataSet* OrderDataset = {read=GetOrderDataset};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
