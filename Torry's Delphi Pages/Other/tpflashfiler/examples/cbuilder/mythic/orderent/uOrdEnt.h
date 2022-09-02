//---------------------------------------------------------------------------

#ifndef uOrdEntH
#define uOrdEntH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffdb.hpp"
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TfrmOrderEntry : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlTopLeft;
  TGroupBox *grpBillTo;
  TDBEdit *dbeBillToName;
  TDBEdit *dbeBillToID;
  TDBEdit *dbeBillToAddress1;
  TDBEdit *dbeBillToAddress2;
  TDBEdit *dbeBillToCity;
  TDBEdit *dbeBillToState;
  TDBEdit *dbeBillToZip;
  TGroupBox *grpShipTo;
  TDBEdit *dbeShipToContact;
  TDBEdit *dbeShipToAddress1;
  TDBEdit *dbeShipToAddress2;
  TDBEdit *dbeShipToCity;
  TDBEdit *dbeShipToState;
  TDBEdit *dbeShiptoZip;
  TPanel *pnlTopRight;
  TPanel *pnlTopRight_Bottom;
  TButton *btnRemoveItem;
  TDBGrid *grdOrderItems;
  TPanel *pnlItemSearch;
  TLabel *lblOEMNumber;
  TEdit *edtOEM;
  TButton *btnAddItem;
  TGroupBox *grpPaymentInformation;
  TLabel *lblSoldBy;
  TLabel *lblTerms;
  TLabel *lblPaymentMethod;
  TLabel *lblShipvia;
  TLabel *lblPO;
  TLabel *lblSubtotal;
  TLabel *lblDue;
  TLabel *lblPaid;
  TLabel *lblFreight;
  TLabel *lblTax;
  TBevel *bvlPayment;
  TLabel *lblCCNumber;
  TLabel *lblCCExpMonth;
  TLabel *lblCCExpYear;
  TDBLookupComboBox *dbeSoldBy;
  TDBComboBox *dbeTerms;
  TDBComboBox *dbePaymentMethod;
  TDBComboBox *dbeShipVIA;
  TDBEdit *dbePO;
  TDBEdit *dbePaid;
  TDBEdit *dbeSubtotal;
  TDBEdit *dbeTax;
  TDBEdit *dbeFreight;
  TDBEdit *dbeDue;
  TDBEdit *dbeTaxRate;
  TButton *btnLogOrder;
  TButton *btnCancelOrder;
  TDBEdit *dbeCCNumber;
  TDBComboBox *dbeCCExpMonth;
  TDBComboBox *dbeCCExpYear;
  TffTable *tblEmployees;
  TDataSource *dtsemployees;
  TffTable *tblProducts;
  TAutoIncField *tblProductsID;
  TStringField *tblProductsName;
  TStringField *tblProductsUOM;
  TStringField *tblProductsOEM;
  TCurrencyField *tblProductsSalePrice;
  void __fastcall btnLogOrderClick(TObject *Sender);
  void __fastcall btnCancelOrderClick(TObject *Sender);
  void __fastcall btnAddItemClick(TObject *Sender);
  void __fastcall btnRemoveItemClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
private:	// User declarations
  TDataSet* GetOrderDataset();
  TDataSet* GetOrderItemsDataset();
public:		// User declarations
  __fastcall TfrmOrderEntry(TComponent* Owner);
  __property TDataSet* OrderDataset = {read = GetOrderDataset};
  __property TDataSet* OrderItemsDataset = {read = GetOrderItemsDataset};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmOrderEntry *frmOrderEntry;
//---------------------------------------------------------------------------
#endif
