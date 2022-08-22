//---------------------------------------------------------------------------
#ifndef BMPageDemoUnitH
#define BMPageDemoUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BMPAGE.h"
#include <Buttons.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "BMWave.h"
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TBMPageControl *BMPageControl1;
    TBMTabSheet *BMTabSheet1;
    TBMTabSheet *BMTabSheet2;
    TBMTabSheet *BMTabSheet3;
    TPanel *Panel1;
    TBitBtn *BitBtn1;
    TBitBtn *BitBtn2;
    TButton *Button1;
    TCheckBox *CheckBox1;
    TRadioButton *RadioButton1;
    TCheckBox *SoundsCheckBox;
    TBMWave *BMWave1;
    TBMWave *BMWave2;
    TBMWave *BMWave3;
    TBMWave *BMWave4;
    TBMWave *BMWave5;
    TBMWave *BMWave6;
    TBMWave *BMWave7;
        TBMPageControl *BMPageControl2;
        TBMTabSheet *BMTabSheet4;
        TLabel *Label1;
        TBMTabSheet *BMTabSheet5;
        TEdit *Edit1;
        TBMTabSheet *BMTabSheet6;
        TPanel *Panel2;
        TBMTabSheet *BMTabSheet7;
        TImage *Image1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
    void __fastcall BitBtn1Click(TObject *Sender);
    void __fastcall BitBtn2Click(TObject *Sender);
    void __fastcall SoundsCheckBoxClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
