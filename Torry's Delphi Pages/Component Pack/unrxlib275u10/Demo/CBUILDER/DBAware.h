//---------------------------------------------------------------------------
#ifndef DBAwareH
#define DBAwareH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Placemnt.hpp"
#include "PicClip.hpp"
#include <vcl\DB.hpp>
#include "RxQuery.hpp"
#include <vcl\DBTables.hpp>
#include "DBFilter.hpp"
#include "DBQBE.hpp"
#include <vcl\Tabnotbk.hpp>
#include <vcl\ComCtrls.hpp>
#include "RXDBCtrl.hpp"
#include <vcl\DBGrids.hpp>
#include "Grids.hpp"
#include <vcl\ExtCtrls.hpp>
#include <vcl\DBCtrls.hpp>
#include <vcl\Mask.hpp>
#include "RXLookup.hpp"
#include "DBIndex.hpp"
#include "DBUtils.hpp"
#include "BdeUtils.hpp"
#include <vcl\Buttons.hpp>
#include "RXCtrls.hpp"
//---------------------------------------------------------------------------
class TDBAwareForm : public TForm
{
__published:	// IDE-managed Components
        TFormStorage *FormStorage1;
        TPicClip *PicClip;
        TDataSource *DataSource5;
        TRxQuery *rxQuery1;
        TFloatField *rxQuery1OrderNo;
        TDateTimeField *rxQuery1SaleDate;
        TStringField *rxQuery1Company;
        TFloatField *rxQuery1CustNo;
        TDateTimeField *rxQuery1ShipDate;
        TStringField *rxQuery1ShipToContact;
        TStringField *rxQuery1ShipToAddr1;
        TStringField *rxQuery1ShipToAddr2;
        TStringField *rxQuery1ShipToCity;
        TStringField *rxQuery1ShipToState;
        TStringField *rxQuery1ShipToZip;
        TStringField *rxQuery1ShipToCountry;
        TStringField *rxQuery1ShipToPhone;
        TStringField *rxQuery1ShipVIA;
        TStringField *rxQuery1PO;
        TStringField *rxQuery1Terms;
        TStringField *rxQuery1PaymentMethod;
        TCurrencyField *rxQuery1ItemsTotal;
        TFloatField *rxQuery1TaxRate;
        TCurrencyField *rxQuery1Freight;
        TCurrencyField *rxQuery1AmountPaid;
        TIntegerField *rxQuery1EmpNo;
        TStringField *rxQuery1LastName;
        TDataSource *DataSource6;
        TTable *Table4;
        TRxDBFilter *DBFilter1;
        TDataSource *DataSource4;
        TTable *Table3;
        TDataSource *DataSource2;
        TQBEQuery *QBEQuery1;
        TDataSource *DataSource1;
        TTable *Table2;
        TFloatField *Table2SpeciesNo;
        TStringField *Table2Category;
        TStringField *Table2Common_Name;
        TFloatField *Table2Lengthcm;
        TMemoField *Table2Notes;
        TGraphicField *Table2Graphic;
        TDataSource *DataSource3;
        TTable *Table1;
        TFloatField *Table1OrderNo;
        TFloatField *Table1ItemNo;
        TFloatField *Table1PartNo;
        TIntegerField *Table1Qty;
        TFloatField *Table1Discount;
        TTabbedNotebook *TabbedNotebook1;
        TGroupBox *GroupBox1;
        TRxDBGrid *rxDBGrid1;
        TPanel *Panel1;
        TCheckBox *CheckBox1;
        TCheckBox *CheckBox2;
        TGroupBox *GroupBox2;
        TDBStatusLabel *DBStatusLabel1;
        TLabel *Label2;
        TDBStatusLabel *DBStatusLabel2;
        TDBNavigator *DBNavigator1;
        TGroupBox *GroupBox3;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TLabel *Label6;
        TMaskEdit *Edit1;
        TRxDBLookupCombo *rxDBLookupCombo1;
        TGroupBox *GroupBox4;
        TLabel *Label7;
        TRxDBGrid *rxDBGrid2;
        TDBIndexCombo *DBIndexCombo1;
        TPanel *Panel2;
        TGroupBox *GroupBox6;
        TDBNavigator *DBNavigator;
        TGroupBox *GroupBox7;
        TLabel *Label15;
        TPanel *Panel3;
        TScrollBox *ScrollBox;
        TLabel *Label8;
        TLabel *Label9;
        TLabel *Label10;
        TLabel *Label11;
        TLabel *Label12;
        TLabel *Label13;
        TLabel *Label14;
        TDBEdit *EditCompany;
        TDBEdit *EditCity;
        TDBEdit *EditState;
        TDBEdit *EditZip;
        TDBEdit *EditCountry;
        TDBEdit *EditPhone;
        TGroupBox *GroupBox5;
        TSpeedButton *EnterQuery;
        TSpeedButton *ExecQuery;
        TSpeedButton *CancelQuery;
        TRadioGroup *RadioGroup1;
        TDBEdit *EditCustNo;
        TGroupBox *GroupBox8;
        TLabel *Label16;
        TLabel *Label1;
        TPanel *Panel4;
        TLabel *Label17;
        TLabel *Label19;
        TRxDBLookupCombo *rxDBLookupCombo2;
        TComboBox *ComboBox2;
        TRxDBGrid *DBGrid1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall CheckBox1Click(TObject *Sender);
        void __fastcall rxDBLookupCombo1Change(TObject *Sender);
        void __fastcall EnterQueryClick(TObject *Sender);
        void __fastcall ExecQueryClick(TObject *Sender);
        void __fastcall CancelQueryClick(TObject *Sender);
        void __fastcall RadioGroup1Click(TObject *Sender);
        void __fastcall DBFilter1Change(TObject *Sender);
        void __fastcall ComboBox2Change(TObject *Sender);
        void __fastcall rxDBLookupCombo2Change(TObject *Sender);
        
        void __fastcall rxDBLookupCombo1GetImage(TObject *Sender, bool IsEmpty,
        TGraphic *&Graphic, int &TextMargin);
        void __fastcall DBGrid1CheckButton(TObject *Sender, long ACol,
        TField *Field, bool &Enabled);
        void __fastcall DBGrid1TitleBtnClick(TObject *Sender, long ACol,
        TField *Field);
        void __fastcall CheckBox2Click(TObject *Sender);
        void __fastcall rxDBGrid1GetCellParams(TObject *Sender, TField *Field,
        TFont *AFont, TColor &Background, bool Highlight);
private:	// User declarations
        void __fastcall SetMacro(const AnsiString MacroName,
          const AnsiString AValue);
public:		// User declarations
        __fastcall TDBAwareForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TDBAwareForm *DBAwareForm;
//---------------------------------------------------------------------------
#endif
