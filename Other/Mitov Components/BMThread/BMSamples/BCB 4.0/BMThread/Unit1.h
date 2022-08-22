//---------------------------------------------------------------------------
#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BMThread.h"
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TBMThread *BMThread1;
    TButton *Button1;
    TLabel *Label1;
    TLabel *Label2;
    TButton *Button2;
    TBMThreadGroup *BMThreadGroup1;
    TButton *Button3;
    TButton *Button4;
    TBevel *Bevel1;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label5;
    TProgressBar *ProgressBar1;
    TBMThread *BMThread2;
    TButton *ButtonStopAll;
    void __fastcall BMThread1Execute(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall BMThread1Terminate(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall BMThread1Start(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall Button3Click(TObject *Sender);
    void __fastcall Button4Click(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall BMThreadGroup1Start(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall BMThreadGroup1Terminate(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    
    void __fastcall BMThread2Execute(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall BMThread2Start(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    void __fastcall BMThread2Terminate(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data);
    
    void __fastcall BMThreadGroup1Update(TObject *Sender,
          TBMExecuteThread *Thread, void *&Data, int Percent);
    void __fastcall ButtonStopAllClick(TObject *Sender);
    void __fastcall SynchroEvent1 (TObject *Sender);
    void __fastcall SynchroEvent2 ( TBMThread * Sender, TBMExecuteThread *Thread, void *&Data);

private:	// User declarations
    int Value1;
    int Value2;

private:	// User declarations

public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 