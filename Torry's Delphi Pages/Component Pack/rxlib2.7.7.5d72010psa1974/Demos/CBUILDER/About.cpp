//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "About.h"
#include "LinkUnit.h"
//---------------------------------------------------------------------------
#pragma link "RXCtrls"
#pragma link "RXConst"
#pragma resource "*.dfm"
TAboutForm *AboutForm;
//---------------------------------------------------------------------------
__fastcall TAboutForm::TAboutForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TAboutForm::FormCreate(TObject *Sender)
{
    AppIcon->Picture->Icon = Application->Icon;
    AppIcon->Cursor = (TCursor)crHand;
    WebLabel->Cursor = (TCursor)crHand;
}
//---------------------------------------------------------------------------
void __fastcall TAboutForm::AppIconDblClick(TObject *Sender)
{
    SecretPanel1->Active = true;
}
//---------------------------------------------------------------------------
void __fastcall TAboutForm::SecretPanel1DblClick(TObject *Sender)
{
    SecretPanel1->Active = false;
}
//---------------------------------------------------------------------------
void __fastcall TAboutForm::WebLabelClick(TObject *Sender)
{
    RxWebSite();
}
//---------------------------------------------------------------------------
void __fastcall TAboutForm::WebLabelActivate(TObject *Sender)
{
    if (WebLabel->MouseInControl) {
      WebLabel->Font->Color = clHighlight;
    }
    else {
      WebLabel->Font->Color = clWindowText;
    }

}
//---------------------------------------------------------------------------