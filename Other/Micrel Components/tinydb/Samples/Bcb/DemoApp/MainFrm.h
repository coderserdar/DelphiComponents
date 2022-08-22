//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <DBGrids.hpp>
#include <Grids.hpp>
#include <ExtCtrls.hpp>
#include "TinyDB.hpp"
#include <Db.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
    TPageControl *PageControl;
    TTabSheet *DatabaseTabSheet;
    TTabSheet *RecordsTabSheet;
    TTabSheet *SearchTabSheet;
    TTabSheet *FilterTabSheet;
    TGroupBox *GroupBox1;
    TLabel *Label2;
    TEdit *NameEdit;
    TLabel *Label3;
    TEdit *AgeEdit;
    TLabel *Label4;
    TComboBox *SexComboBox;
    TLabel *Label5;
    TEdit *TelEdit;
    TLabel *Label6;
    TEdit *AddrEdit;
    TLabel *Label7;
    TMemo *NoteMemo;
    TButton *AppendButton;
    TButton *PostButton;
    TButton *FirstButton;
    TButton *PrevButton;
    TButton *NextButton;
    TButton *LastButton;
    TGroupBox *GroupBox2;
    TGroupBox *GroupBox3;
    TLabel *Label8;
    TEdit *GotoKeyNameEdit;
    TButton *GotoKeyButton;
    TLabel *Label9;
    TEdit *FindNameEdit;
    TButton *FindFirstButton;
    TButton *FindNextButton;
    TLabel *Label10;
    TEdit *FilterEdit;
    TButton *FilterButton;
    TDBGrid *FilterDBGrid;
    TDBGrid *SearchDBGrid;
    TPanel *StatePanel;
    TButton *OpenDatabaseButton;
    TButton *CloseDatabaseButton;
    TTinyTable *TinyTable1;
    TGroupBox *GroupBox5;
    TCheckBox *CompCheckBox;
    TGroupBox *GroupBox6;
    TCheckBox *EncryptCheckBox;
    TComboBox *EncAlgoComboBox;
    TLabel *Label12;
    TLabel *Label13;
    TEdit *EncPwdEdit;
    TComboBox *CompAlgoComboBox;
    TLabel *Label11;
    TLabel *Label14;
    TComboBox *CompLevelComboBox;
    TLabel *Label1;
    TEdit *FileNameEdit;
    TButton *CreateDatabaseButton;
    TButton *BrowseButton;
    TDataSource *DataSource1;
    TButton *CancelFilterButton;
    TTinyDatabase *TinyDatabase1;
    void __fastcall BrowseButtonClick(TObject *Sender);
    void __fastcall CreateDatabaseButtonClick(TObject *Sender);
    void __fastcall OpenDatabaseButtonClick(TObject *Sender);
    void __fastcall CloseDatabaseButtonClick(TObject *Sender);
    void __fastcall AppendButtonClick(TObject *Sender);
    void __fastcall PostButtonClick(TObject *Sender);
    void __fastcall FirstButtonClick(TObject *Sender);
    void __fastcall PrevButtonClick(TObject *Sender);
    void __fastcall NextButtonClick(TObject *Sender);
    void __fastcall LastButtonClick(TObject *Sender);
    void __fastcall GotoKeyButtonClick(TObject *Sender);
    void __fastcall FindFirstButtonClick(TObject *Sender);
    void __fastcall FindNextButtonClick(TObject *Sender);
    void __fastcall FilterButtonClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall CompCheckBoxClick(TObject *Sender);
    void __fastcall EncryptCheckBoxClick(TObject *Sender);
    void __fastcall TinyTable1AfterOpen(TDataSet *DataSet);
    void __fastcall TinyTable1AfterClose(TDataSet *DataSet);
    void __fastcall TinyTable1AfterScroll(TDataSet *DataSet);
    void __fastcall CancelFilterButtonClick(TObject *Sender);
private:	// User declarations
    void LoadFieldData();
    void SaveFieldData();
public:		// User declarations
    __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
