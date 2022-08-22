//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TinyDB"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TMainForm::LoadFieldData()
{
    NameEdit->Text = TinyTable1->FieldByName("Name")->AsString;
    AgeEdit->Text = TinyTable1->FieldByName("Age")->AsString;
    SexComboBox->ItemIndex = (TinyTable1->FieldByName("Sex")->AsBoolean? 0 : 1);
    TelEdit->Text = TinyTable1->FieldByName("Tel")->AsString;
    AddrEdit->Text = TinyTable1->FieldByName("Addr")->AsString;
    NoteMemo->Lines->Text = TinyTable1->FieldByName("Note")->AsString;
}
//---------------------------------------------------------------------------
void TMainForm::SaveFieldData()
{
    TinyTable1->FieldByName("Name")->AsString = NameEdit->Text;
    TinyTable1->FieldByName("Age")->AsInteger = StrToInt(AgeEdit->Text);
    TinyTable1->FieldByName("Sex")->AsBoolean = ((SexComboBox->ItemIndex == 0)? true : false);
    TinyTable1->FieldByName("Tel")->AsString = TelEdit->Text;
    TinyTable1->FieldByName("Addr")->AsString = AddrEdit->Text;
    TinyTable1->FieldByName("Note")->AsString = NoteMemo->Lines->Text;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
    PageControl->ActivePage = DatabaseTabSheet;
    FileNameEdit->Text = ExtractFilePath(Application->ExeName) + "Test.tdb";
    TTinyDatabase::GetCompressAlgoNames(__classid(TTinyDatabase), CompAlgoComboBox->Items);
    TTinyDatabase::GetEncryptAlgoNames(__classid(TTinyDatabase), EncAlgoComboBox->Items);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::BrowseButtonClick(TObject *Sender)
{
    TSaveDialog *SaveDialog = new TSaveDialog(this);

    SaveDialog->Filter = "TinyDB Files(*.tdb)|*.tdb|Any Files(*.*)|*.*";
    SaveDialog->DefaultExt = ".tdb";
    SaveDialog->Options << ofOverwritePrompt;
    if (SaveDialog->Execute())
    {
        FileNameEdit->Text = SaveDialog->FileName;
    }
    delete SaveDialog;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::CreateDatabaseButtonClick(TObject *Sender)
{
    TFieldItem FieldItems[6];
    bool Ok;

    //Create database
    Ok = TinyDatabase1->CreateDatabase(
        FileNameEdit->Text,               //Database file name
        CompCheckBox->Checked,            //Compress or not
        (TCompressLevel)CompLevelComboBox->ItemIndex,   //Compression level
        CompAlgoComboBox->Text,           //Compression algorithm
        EncryptCheckBox->Checked,         //Encrypt or not
        EncAlgoComboBox->Text,            //Encryption algorithm
        EncPwdEdit->Text,                 //Password
        false                             //CRC32 or not
        );

    //Create table
    FieldItems[0] = FieldItem("Name", ftString, 32, fdDefault);
    FieldItems[1] = FieldItem("Age", ftInteger, 0, fdDefault);
    FieldItems[2] = FieldItem("Sex", ftBoolean, 0, fdDefault);
    FieldItems[3] = FieldItem("Tel", ftString, 32, fdDefault);
    FieldItems[4] = FieldItem("Addr", ftString, 100, fdDefault);
    FieldItems[5] = FieldItem("Note", ftMemo, 0, fdDefault);

    TinyDatabase1->DatabaseName = FileNameEdit->Text;
    TinyDatabase1->Password = EncPwdEdit->Text;
    TinyDatabase1->CreateTable("TestTable", FieldItems, 5);

    //Create index
    TinyDatabase1->CreateIndex("TestTable", "NameIndex", TTDIndexOptions(), OPENARRAY(AnsiString, ("Name")) );

    //Open database
    OpenDatabaseButtonClick(NULL);

    //Append some records
    TinyTable1->AppendRecord(ARRAYOFCONST(("Tom", 24, true, "", "Shanghai", "")) );
    TinyTable1->AppendRecord(ARRAYOFCONST(("John", 22, false, "", "Wuhan", "")) );
    TinyTable1->AppendRecord(ARRAYOFCONST(("Anna", 22, false, "", "Nanjing", "")) );

    if (Ok)
        ShowMessage("Create database successfully.");
    else
        ShowMessage("Fail to create database.");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::OpenDatabaseButtonClick(TObject *Sender)
{
    TinyTable1->Close();
    TinyTable1->DatabaseName = FileNameEdit->Text;
    TinyTable1->Password = EncPwdEdit->Text;
    TinyTable1->TableName = "TestTable";
    TinyTable1->IndexName = "NameIndex";
    TinyTable1->Open();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::CloseDatabaseButtonClick(TObject *Sender)
{
    TinyTable1->Close();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::AppendButtonClick(TObject *Sender)
{
    TinyTable1->Append();
    
    NameEdit->Text = "";
    AgeEdit->Text = "";
    SexComboBox->ItemIndex = -1;
    TelEdit->Text = "";
    AddrEdit->Text = "";
    NoteMemo->Lines->Clear();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::PostButtonClick(TObject *Sender)
{
    SaveFieldData();
    TinyTable1->Post();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FirstButtonClick(TObject *Sender)
{
    TinyTable1->First();
    LoadFieldData();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::PrevButtonClick(TObject *Sender)
{
    TinyTable1->Prior();
    LoadFieldData();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::NextButtonClick(TObject *Sender)
{
    TinyTable1->Next();
    LoadFieldData();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::LastButtonClick(TObject *Sender)
{
    TinyTable1->Last();
    LoadFieldData();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::GotoKeyButtonClick(TObject *Sender)
{
    bool Found;

    TinyTable1->IndexName = "NameIndex";
    TinyTable1->SetKey();
    TinyTable1->FieldByName("Name")->AsString = GotoKeyNameEdit->Text;
    Found = TinyTable1->GotoKey();
    if (!Found) ShowMessage("Record not found.");
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FindFirstButtonClick(TObject *Sender)
{
    TinyTable1->Filter = "Name='" + FindNameEdit->Text + "'";
    TinyTable1->FindFirst(); 
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FindNextButtonClick(TObject *Sender)
{
    TinyTable1->Filter = "Name='" + FindNameEdit->Text + "'";
    TinyTable1->FindNext(); 
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FilterButtonClick(TObject *Sender)
{
    TinyTable1->Filter = FilterEdit->Text;
    TinyTable1->Filtered = true;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::CancelFilterButtonClick(TObject *Sender)
{
    TinyTable1->Filtered = false;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::CompCheckBoxClick(TObject *Sender)
{
    CompAlgoComboBox->Enabled = CompCheckBox->Checked;
    CompLevelComboBox->Enabled = CompCheckBox->Checked;
    if (CompCheckBox->Checked)
    {
        CompAlgoComboBox->ItemIndex = 0;
        CompLevelComboBox->ItemIndex = (int)clNormal;
    }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::EncryptCheckBoxClick(TObject *Sender)
{
    EncAlgoComboBox->Enabled = EncryptCheckBox->Checked;
    EncPwdEdit->Enabled = EncryptCheckBox->Checked;
    if (EncryptCheckBox->Checked)
        EncAlgoComboBox->ItemIndex = 0;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TinyTable1AfterOpen(TDataSet *DataSet)
{
    StatePanel->Caption = "Database State: Opened.";
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TinyTable1AfterClose(TDataSet *DataSet)
{
    StatePanel->Caption = "Database State: Closed.";
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::TinyTable1AfterScroll(TDataSet *DataSet)
{
    LoadFieldData();
}
//---------------------------------------------------------------------------

