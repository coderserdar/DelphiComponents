/*********************************************************/
/* XMLPartner: ExURL.CPP 2.55                            */
/* Copyright (c) TurboPower Software Co 2002             */
/* All rights reserved.                                  */
/*********************************************************/
/* XMLPartner: XML Editor URL Edit form                  */
/*********************************************************/
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ExURL.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TURLForm *URLForm;
//---------------------------------------------------------------------------
__fastcall TURLForm::TURLForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TURLForm::OkBtnClick(TObject *Sender)
{
  if (UrlEdit->Text == "")
  {
    MessageDlg("The URL cannot be blank.", mtError,
               TMsgDlgButtons() << mbOK, 0);
    ModalResult == mrNone;
  }
}
//---------------------------------------------------------------------------
void __fastcall TURLForm::FormShow(TObject *Sender)
{
  UrlEdit->SetFocus();
}
//---------------------------------------------------------------------------
String __fastcall TURLForm::GetFTPPassword(void)
{
  return(FtpPasswordEdit->Text);
}
//---------------------------------------------------------------------------
String __fastcall TURLForm::GetFTPUser(void)
{
  return(FtpUserIdEdit->Text);
}
//---------------------------------------------------------------------------
String __fastcall TURLForm::GetURL(void)
{
  return(UrlEdit->Text);
}
//---------------------------------------------------------------------------
void __fastcall TURLForm::SetFTPPassword(const String aPassword)
{
  FtpPasswordEdit->Text = aPassword;
}
//---------------------------------------------------------------------------
void __fastcall TURLForm::SetFTPUser(const String aUser)
{
  FtpUserIdEdit->Text = aUser;
}
//---------------------------------------------------------------------------
void __fastcall TURLForm::SetURL(const String aURL)
{
  UrlEdit->Text = aURL;
}
//---------------------------------------------------------------------------

