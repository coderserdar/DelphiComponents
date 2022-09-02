//---------------------------------------------------------------------------

#include <vcl\vcl.h>
#pragma hdrstop

#include "uEmpCfg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ffdb"
#pragma resource "*.dfm"
TfrmEmployees *frmEmployees;
//---------------------------------------------------------------------------
__fastcall TfrmEmployees::TfrmEmployees(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
TDataSet* TfrmEmployees::GetDataset()
{
  return grdEmp->DataSource->DataSet;
}


void __fastcall TfrmEmployees::btnDeleteClick(TObject *Sender)
{
  // make sure the dataset is in browse mode
  Dataset->CheckBrowseMode();

  // confirm before deleting the record
  String S = "Delete " + Dataset->FieldByName("Name")->AsString + "?";
  if (MessageDlg(S, mtConfirmation, TMsgDlgButtons() << mbOK << mbCancel, 0) == mrOk)
    Dataset->Delete();
}
//---------------------------------------------------------------------------
void __fastcall TfrmEmployees::btnNewClick(TObject *Sender)
{
  // make sure the dataset is in browse mode
  Dataset->CheckBrowseMode();
  // retrieve the new employee name, and add it to the table
  String NameStr;
  if (InputQuery("Create new employee", "Name", NameStr))
    Dataset->AppendRecord(OPENARRAY(TVarRec, (0, NameStr)));
}
//---------------------------------------------------------------------------
void __fastcall TfrmEmployees::btnClearPhotoClick(TObject *Sender)
{
  if (Dataset->State != dsEdit || Dataset->State != dsInsert)
    Dataset->Edit();
  TffBlobStream* BS = new TffBlobStream(
    (TBlobField*)Dataset->FieldByName("Picture"), bmWrite);
  try {
    BS->Position = 0;
    BS->Truncate();
    Dataset->Post();
  }
  __finally {
    delete BS;
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmEmployees::btnSelectPhotoClick(TObject *Sender)
{
  if (dlgOpenPicture->Execute()) {
    if (Dataset->State != dsEdit || Dataset->State != dsInsert)
      Dataset->Edit();
    TFileStream* FS = new TFileStream(
      dlgOpenPicture->FileName, fmOpenRead | fmShareDenyWrite);
    try {
      TffBlobStream* BS = (TffBlobStream*)Dataset->CreateBlobStream(
        Dataset->FieldByName("Picture"), bmReadWrite);
      try {
        BS->CopyFrom(FS, FS->Size);
        Dataset->Post();
      }
      __finally {
        delete BS;
      }
    }
    __finally {
      delete FS;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmEmployees::FormShow(TObject *Sender)
{
  tblEmployees->Open();  
}
//---------------------------------------------------------------------------

