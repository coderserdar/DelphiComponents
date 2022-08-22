//---------------------------------------------------------------------------
#ifndef DIBDemoUnitH
#define DIBDemoUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "DIBAccess.h"
#include <ComCtrls.hpp>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TImage *Image1;
    TImage *Image2;
    TImage *Image3;
    TDIBAccess *DIBAccess1;
    TDIBAccess *DIBAccess2;
    TDIBAccess *DIBAccess3;
    TButton *Button1;
    TProgressBar *ProgressBar1;
    TButton *Button2;
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
    void __fastcall Process ( int i );
    void __fastcall ProcessStandart ( int i );

public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
