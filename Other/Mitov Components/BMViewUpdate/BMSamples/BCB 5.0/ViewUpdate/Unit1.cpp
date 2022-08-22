//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ViewUpdate"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
  ProgressBar1->Visible = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMTreeViewUpdate1CustomCompare(TObject *Sender,
      TTreeNode *OldItem, TTreeNode *NewItem, bool &Changed)
{
  if ( OldItem != NewItem )
    if ( OldItem && NewItem )
      Changed = ( OldItem->Text.Length () != NewItem->Text.Length () );

    else
      Changed = true;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::BMTreeViewUpdate1TreeInvalidate(TObject *Sender,
      TTreeNode *Item)
{
  Label1->Font->Color = clRed;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMTreeViewUpdate1TreeValidate(TObject *Sender,
      TTreeNode *Item)
{
  Label1->Font->Color = clBlack;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMTreeViewUpdate1TreeUpdate(TObject *Sender,
      TTreeNode *OldItem, TTreeNode *NewItem)
{
  switch ( RadioGroup1->ItemIndex )
    {
    case 0:
      Label1->Caption = NewItem->Text + " - Index : " + NewItem->Index;
      break;

    case 1:
      Label1->Caption = NewItem->Text;
      break;

    case 2:
      Label1->Caption = (AnsiString)"Length : " + NewItem->Text.Length ();
      break;

    }

  ProgressBar1->Position = 0;
  ProgressBar1->Visible = true;
  for ( int i = 1; i < 1000; i ++ )
    ProgressBar1->Position = i;

  Label1->Font->Color = clBlack;
  ProgressBar1->Visible = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RadioGroup1Click(TObject *Sender)
{
  switch ( RadioGroup1->ItemIndex )
    {
    case 0:
      BMTreeViewUpdate1->CompareType = TBMTreeViewUpdate::ctItem;
      break;
      
    case 1:
      BMTreeViewUpdate1->CompareType = TBMTreeViewUpdate::ctText;
      break;

    case 2:
      BMTreeViewUpdate1->CompareType = TBMTreeViewUpdate::ctCustom;
      break;

    }

  Label1->Caption = "Unknown";
}
//---------------------------------------------------------------------------

