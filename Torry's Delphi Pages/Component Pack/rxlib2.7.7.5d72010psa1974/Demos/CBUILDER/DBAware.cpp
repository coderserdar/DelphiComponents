//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "DBAware.h"
//---------------------------------------------------------------------------
#pragma link "Placemnt"
#pragma link "PicClip"
#pragma link "RxQuery"
#pragma link "DBFilter"
#pragma link "DBQBE"
#pragma link "RXDBCtrl"
#pragma link "Grids"
#pragma link "RXLookup"
#pragma link "DBIndex"
#pragma link "DBUtils"
#pragma link "BdeUtils"
#pragma link "RXCtrls"
#pragma resource "*.dfm"
TDBAwareForm *DBAwareForm;
//---------------------------------------------------------------------------
__fastcall TDBAwareForm::TDBAwareForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::SetMacro(const AnsiString MacroName,
          const AnsiString AValue)
{
  rxQuery1->MacroByName(MacroName)->AsString = AValue;
  rxQuery1->DisableControls();
  try {
    rxQuery1->Close();
    rxQuery1->Open();
  }
  catch(...) {
    rxQuery1->EnableControls();
    throw;
  }
  rxQuery1->EnableControls();
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::FormCreate(TObject *Sender)
{
  ComboBox2->ItemIndex = 1;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::FormClose(TObject *Sender, TCloseAction &Action)
{
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::CheckBox1Click(TObject *Sender)
{
  rxDBGrid1->ShowGlyphs = CheckBox1->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::rxDBLookupCombo1Change(TObject *Sender)
{
  Edit1->Text = rxDBLookupCombo1->Value;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::EnterQueryClick(TObject *Sender)
{
  DBFilter1->SetCapture();
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::ExecQueryClick(TObject *Sender)
{
  DBFilter1->ReadCaptureControls();
  DBFilter1->ReleaseCapture();
  DBFilter1->Activate();
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::CancelQueryClick(TObject *Sender)
{
  DBFilter1->ReleaseCapture();
  DBFilter1->Deactivate();
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::RadioGroup1Click(TObject *Sender)
{
  DBFilter1->LogicCond = (TFilterLogicCond)RadioGroup1->ItemIndex;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::DBFilter1Change(TObject *Sender)
{
  ExecQuery->Enabled = DBFilter1->Captured;
  CancelQuery->Enabled = (DBFilter1->Active || DBFilter1->Captured);
  EnterQuery->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::ComboBox2Change(TObject *Sender)
{
  AnsiString order;

  switch (ComboBox2->ItemIndex) {
    case 0: {
      order = "ORDERS.\"SaleDate\"";
      break;
    }
    case 1: {
      order = "ORDERS.\"ShipDate\"";
      break;
    }
    case 2: {
      order = "ORDERS.\"CustNo\"";
      break;
    }
    case 3: {
      order = "ORDERS.\"EmpNo\"";
      break;
    }
    default: return;
  }
  SetMacro("ORDER", order);
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::rxDBLookupCombo2Change(TObject *Sender)
{
  AnsiString s;

  s = "0=0";
  if (rxDBLookupCombo2->Value != "") {
    s = "ORDERS.\"CustNo\"=" + rxDBLookupCombo2->Value;
  }
  SetMacro("CUSTOMER", s);
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::rxDBLookupCombo1GetImage(TObject *Sender,
        bool IsEmpty, TGraphic *&Graphic, int &TextMargin)
{
  AnsiString PhoneExt;

  TextMargin = PicClip->Width + 2;
  if (!IsEmpty) {
    PhoneExt = QBEQuery1->FieldByName("PhoneExt")->AsString;
    Graphic = PicClip->GraphicCell[3];
    if (PhoneExt == "") {
      Graphic = PicClip->GraphicCell[4];
    }
  }
  else {
    Graphic = PicClip->GraphicCell[5];
  }
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::DBGrid1CheckButton(TObject *Sender, long ACol,
        TField *Field, bool &Enabled)
{
  Enabled = (Field != NULL) && (dynamic_cast<TBlobField*>(Field) == 0);
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::DBGrid1TitleBtnClick(TObject *Sender, long ACol,
        TField *Field)
{
  if (Field != NULL) {
    SetMacro("ORDER", Field->FieldName);
    ComboBox2->ItemIndex = -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::CheckBox2Click(TObject *Sender)
{
  rxDBGrid1->MultiSelect = CheckBox2->Checked;        
}
//---------------------------------------------------------------------------
void __fastcall TDBAwareForm::rxDBGrid1GetCellParams(TObject *Sender,
        TField *Field, TFont *AFont, TColor &Background, bool Highlight)
{
  if (Field->FieldName == "Category") {
    AFont->Style = AFont->Style << fsBold;
  }
  else if (Field->FieldName == "Length (cm)") {
    Background = clYellow;
  };
  int Len = ((TRxDBGrid *)Sender)->DataSource->DataSet->FieldByName("Length (cm)")->AsInteger;
  if ((Len <= 30) && (Len > 0)) {
    Background = clLime;    // shortest
  }
  else if (Len >= 150) {
    AFont->Color = clRed;  // longest
  };
  if (Highlight) {
    AFont->Color = clHighlightText;
    Background = clHighlight;
  }
}
//---------------------------------------------------------------------------


