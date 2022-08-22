//---------------------------------------------------------------------------
#ifndef DemoUnitH
#define DemoUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BMShapedButton.h"
#include <ExtCtrls.hpp>
#include <Dialogs.hpp>
#include "BMWave.h"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TOpenDialog *OpenDialog1;
    TImage *Image1;
    TBMShapedButton *BMShapedButton1;
    TBMShapedButton *BMShapedButton2;
    TBMShapedButton *BMShapedButton3;
    TBMShapedButton *BMShapedButton4;
    TBMShapedButton *BMShapedButton5;
    TBMShapedButton *BMShapedButton6;
    TBMShapedButton *BMShapedButton7;
    TBMShapedButton *BMShapedButton8;
    TBMShapedButton *BMShapedButton9;
    TBMShapedButton *BMShapedButton10;
    TBMShapedButton *BMShapedButton11;
    TBMShapedButton *BMShapedButton12;
    TBMShapedButton *BMShapedButton13;
    TBMShapedButton *BMShapedButton14;
    TBMShapedButton *BMShapedButton15;
    TBMShapedButton *BMShapedButton16;
    TBMWave *BMWave1;
    TBMWave *BMWave2;
    TBMWave *BMWave3;
        TBMShapedButton *BMShapedButton17;
    void __fastcall BMShapedButton4Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
 