//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "DIBDemoUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "DIBAccess"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
  Image3->Width = Image1->Width;
  Image3->Height = Image1->Height;
}
//---------------------------------------------------------------------------
TColor __fastcall Calc ( TColor Color1, TColor Color2, int i, int k )
{
//  Color1 = DIBAccess1->Pixels [ x ][ y ];
//  Color2 = DIBAccess2->Pixels [ x ][ y ];

  int A0 = Color1 & 0xFF;
  int A1 = ( Color1 >> 8 ) & 0xFF;
  int A2 = ( Color1 >> 16 ) & 0xFF;

  int B0 = Color2 & 0xFF;
  int B1 = ( Color2 >> 8 ) & 0xFF;
  int B2 = ( Color2 >> 16 ) & 0xFF;

  int Red   = A0 * i / 100 + B0 * k / 100;
  int Green = A1 * i / 100 + B1 * k / 100;
  int Blue  = A2 * i / 100 + B2 * k / 100;

  return ( TColor )RGB ( Red, Green, Blue );
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Process ( int i )
{
  DIBAccess3->BeginUpdate ();
  ProgressBar1->Position = 0;

  int k = 100 - i;

  for ( int x = 0; x < Image1->Width; x ++ )
    {
    if ( Application->Terminated )
      break;

    for ( int y = 0; y < Image1->Height; y ++ )
      {
      DIBAccess3->Pixels [ x ][ y ] = Calc ( DIBAccess1->Pixels [ x ][ y ], DIBAccess2->Pixels [ x ][ y ], i, k );
      ProgressBar1->Position = x;
      if ( Application->Terminated )
        break;
        
      Application->ProcessMessages ();
      }
    }

  ProgressBar1->Position = 0;
  DIBAccess3->EndUpdate ();
  Image3->Repaint ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ProcessStandart ( int i )
{
  ProgressBar1->Position = 0;

  int k = 100 - i;

  for ( int x = 0; x < Image1->Width; x ++ )
    {
    if ( Application->Terminated )
      break;

    for ( int y = 0; y < Image1->Height; y ++ )
      {
      Image3->Canvas->Pixels [ x ][ y ] = Calc ( Image1->Canvas->Pixels [ x ][ y ], Image2->Canvas->Pixels [ x ][ y ], i, k );
      ProgressBar1->Position = x;
      if ( Application->Terminated )
        break;

      Application->ProcessMessages ();
      }
    }

  ProgressBar1->Position = 0;
  Image3->Repaint ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Button1->Enabled = false;
  Button2->Enabled = false;

  DIBAccess1->BeginUpdate ();
  DIBAccess2->BeginUpdate ();

  for ( int i = 0; i <= 100; i += 10 )
    {
    if ( Application->Terminated )
      break;

    Process ( i );
    Application->ProcessMessages ();
    }

  DIBAccess1->EndUpdate ();
  DIBAccess2->EndUpdate ();

  Button1->Enabled = true;
  Button2->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  Button1->Enabled = false;
  Button2->Enabled = false;
  
  for ( int i = 0; i <= 100; i += 10 )
    {
    if ( Application->Terminated )
      break;

    ProcessStandart ( i );
    Application->ProcessMessages ();
    }

  Button1->Enabled = true;
  Button2->Enabled = true;
}
//---------------------------------------------------------------------------
