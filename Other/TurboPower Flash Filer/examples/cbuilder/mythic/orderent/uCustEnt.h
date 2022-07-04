//---------------------------------------------------------------------------

#ifndef uCustEntH
#define uCustEntH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <DBCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TfrmCustomerEntry : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TLabel *Label5;
  TLabel *Label6;
  TLabel *Label7;
  TLabel *Label8;
  TLabel *Label9;
  TLabel *Label10;
  TPanel *pnlBottom;
  TButton *btnClose;
  TDBEdit *dbeZip;
  TDBEdit *dbeCompany;
  TDBEdit *bdeContact;
  TDBEdit *bdeAddress1;
  TDBEdit *dbeAddress2;
  TDBEdit *dbeCity;
  TDBEdit *dbeState;
  TDBEdit *dbeCountry;
  TDBEdit *dbePhone;
  TDBComboBox *dbeDeliveryMethod;
  void __fastcall btnCloseClick(TObject *Sender);
private:	// User declarations
  TDataSet* GetDataset();
public:		// User declarations
  __fastcall TfrmCustomerEntry(TComponent* Owner);
  // property used to simplify access to the customer table
  __property TDataSet* Dataset = {read = GetDataset};
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCustomerEntry *frmCustomerEntry;
//---------------------------------------------------------------------------
#endif
