//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BMThread"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SynchroEvent1 ( TObject *Sender )
{
  Label2->Caption = Value1;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SynchroEvent2 ( TBMThread * Sender, TBMExecuteThread *Thread, void *&Data )
{
  TLabel *Label = (TLabel *)Data;
  Label->Caption = Value2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread1Execute( TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Value1 = 0;
  for ( int i = 0; i < 1000; i ++ )
    {
    Thread->PercentProgress = i / 10;
    SleepEx ( 20, false );
    if ( Thread->Terminated )
      break;

    Value1 ++;
    Thread->Synchronize ( &SynchroEvent1 );
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  BMThread1->Start ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread1Terminate(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Label1->Caption = "Stop";
  Button1->Enabled = true;
  Button2->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread1Start(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Label1->Caption = "Start";
  Button1->Enabled = false;
  Button2->Enabled = true;

  Data = Label2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  BMThread1->Stop ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  BMThread2->Start ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject *Sender)
{
  BMThread2->Stop ();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  BMThreadGroup1->Stop ();
  while ( BMThreadGroup1->Runing )
    Application->ProcessMessages ();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BMThreadGroup1Start(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  ProgressBar1->Position = 0;
  Label5->Visible = true;
  ProgressBar1->Visible = true;
  ButtonStopAll->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThreadGroup1Terminate(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Label5->Visible = false;
  ProgressBar1->Visible = false;
  ProgressBar1->Position = 0;
  ButtonStopAll->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThreadGroup1Update(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data, int Percent)
{
  if ( Sender == BMThread1 )
    Label5->Caption = "Thread 1 :";
    
  else
    Label5->Caption = "Thread 2 :";

  ProgressBar1->Position = Percent;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread2Execute(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Value2 = 0;
  for ( int i = 0; i < 1000; i ++ )
    {
    Thread->PercentProgress = i / 10;
    SleepEx ( 20, false );
    if ( Thread->Terminated )
      break;

    Value2 ++;
    Thread->Synchronize ( &SynchroEvent2, Data );
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread2Start(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Label3->Caption = "Start";
  Button3->Enabled = false;
  Button4->Enabled = true;

  Data = Label4;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BMThread2Terminate(TObject *Sender,
      TBMExecuteThread *Thread, void *&Data)
{
  Label3->Caption = "Stop";
  Button3->Enabled = true;
  Button4->Enabled = false;
}
//---------------------------------------------------------------------------


void __fastcall TForm1::ButtonStopAllClick(TObject *Sender)
{
  BMThreadGroup1->Stop ();
}
//---------------------------------------------------------------------------


