//---------------------------------------------------------------------------
#ifndef PageDemoH
#define PageDemoH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include "RXDBCtrl.hpp"
#include <vcl\DBGrids.hpp>
#include "Grids.hpp"
#include "RXCtrls.hpp"
#include <vcl\DBCtrls.hpp>
#include <vcl\Mask.hpp>
#include "ToolEdit.hpp"
#include "RxDBComb.hpp"
#include "Placemnt.hpp"
#include "PageMngr.hpp"
#include <vcl\DBTables.hpp>
#include <vcl\DB.hpp>
#include "RxQuery.hpp"
#include "DBUtils.hpp"
#include "BdeUtils.hpp"
//---------------------------------------------------------------------------
enum TClientAssistanceMode { camNew, camEdit, camDelete, camView };
//---------------------------------------------------------------------------
class TClientAssistant : public TForm
{
__published:	// IDE-managed Components
        TPanel *BtnPanel;
        TBevel *Bevel1;
        TButton *SaveBtn;
        TButton *BackBtn;
        TButton *NextBtn;
        TButton *CloseBtn;
        TNotebook *Notebook;
        TImage *Image1;
        TLabel *Label1;
        TLabel *Label16;
        TRadioButton *NewBtn;
        TRadioButton *EditBtn;
        TRadioButton *DeleteBtn;
        TRadioButton *ViewBtn;
        TLabel *NewSearchHint;
        TImage *Image3;
        TLabel *Label24;
        TRadioButton *DoSearchBtn;
        TRadioButton *SkipSearchBtn;
        TImage *Image2;
        TLabel *Label17;
        TLabel *Label19;
        TLabel *Label20;
        TLabel *Label21;
        TLabel *Label23;
        TLabel *Label27;
        TEdit *SrchName;
        TEdit *SrchFirstName;
        TEdit *SrchCity;
        TEdit *SrchState;
        TEdit *SrchOccupation;
        TCheckBox *ExactBtn;
        TImage *Image4;
        TLabel *Label25;
        TRxDBGrid *Grid;
        TImage *Image5;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label8;
        TLabel *Label11;
        TLabel *Label12;
        TLabel *Label14;
        TLabel *Label9;
        TLabel *Label10;
        TLabel *Label13;
        TLabel *Label15;
        TLabel *Label5;
        TRxLabel *RxLabel1;
        TDBEdit *EditACCT_NBR;
        TDBEdit *EditFIRST_NAME;
        TDBEdit *EditADDRESS_;
        TDBEdit *EditCITY;
        TDBEdit *EditSTATE;
        TDBEdit *EditZIP;
        TDBEdit *EditTELEPHONE;
        TDBEdit *EditOCCUPATION;
        TDBEdit *EditLAST_NAME;
        TDBEdit *EditSS_NUMBER;
        TDBEdit *EditOBJECTIVES;
        TDBDateEdit *EditBIRTH_DATE;
        TDBEdit *EditINTERESTS;
        TDBImage *EditIMAGE;
        TDBDateEdit *EditDATE_OPEN;
        TRxDBComboBox *ComboRISK_LEVEL;
        TFormStorage *FormStorage;
        TPageManager *PageManager;
        TPageProxy *ToDo;
        TPageProxy *SearchConfirm;
        TPageProxy *SearchParams;
        TPageProxy *ClientsBrowse;
        TPageProxy *ClientEdit;
        TTable *Clients;
        TStringField *ClientsLAST_NAME;
        TStringField *ClientsFIRST_NAME;
        TFloatField *ClientsACCT_NBR;
        TStringField *ClientsADDRESS_1;
        TStringField *ClientsCITY;
        TStringField *ClientsSTATE;
        TStringField *ClientsZIP;
        TStringField *ClientsTELEPHONE;
        TDateField *ClientsDATE_OPEN;
        TFloatField *ClientsSS_NUMBER;
        TStringField *ClientsPICTURE;
        TDateField *ClientsBIRTH_DATE;
        TStringField *ClientsRISK_LEVEL;
        TStringField *ClientsOCCUPATION;
        TStringField *ClientsOBJECTIVES;
        TStringField *ClientsINTERESTS;
        TBlobField *ClientsIMAGE;
        TDataSource *ClientsDS;
        TRxQuery *SearchQuery;
        TStringField *SearchQueryLAST_NAME;
        TStringField *SearchQueryFIRST_NAME;
        TFloatField *SearchQueryACCT_NBR;
        TStringField *SearchQueryADDRESS_1;
        TStringField *SearchQueryCITY;
        TStringField *SearchQuerySTATE;
        TStringField *SearchQueryZIP;
        TStringField *SearchQueryTELEPHONE;
        TDateField *SearchQueryDATE_OPEN;
        TFloatField *SearchQuerySS_NUMBER;
        TDateField *SearchQueryBIRTH_DATE;
        TStringField *SearchQueryRISK_LEVEL;
        TStringField *SearchQueryOCCUPATION;
        TStringField *SearchQueryOBJECTIVES;
        TStringField *SearchQueryINTERESTS;
        TDataSource *SearchQueryDS;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall SaveBtnClick(TObject *Sender);
        void __fastcall ToDoLeave(bool Next);
        void __fastcall SearchParamsLeave(bool Next);
        void __fastcall ClientsBrowseLeave(bool Next);
        void __fastcall ClientEditEnter(bool Next);
        void __fastcall ClientEditLeave(bool Next);
        void __fastcall ClientsBrowseShow(bool Next);
        void __fastcall ClientEditShow(bool Next);
        void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
        void __fastcall PageManagerGetNextPage(int CurrentPage, int &NewPage);
        void __fastcall PageManagerGetPriorPage(int CurrentPage, int &NewPage);
        void __fastcall CloseBtnClick(TObject *Sender);
        
        void __fastcall EditChange(TObject *Sender);
private:	// User declarations
        void __fastcall DoSearch(void);
public:		// User declarations
        TClientAssistanceMode Mode;
        __fastcall TClientAssistant(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TClientAssistant *ClientAssistant;
//---------------------------------------------------------------------------
#endif
