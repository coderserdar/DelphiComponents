//---------------------------------------------------------------------------
#ifndef ToolsH
#define ToolsH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "PicClip.hpp"
#include "RXCalc.hpp"
#include "Placemnt.hpp"
#include "DualList.hpp"
#include <vcl\Tabnotbk.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include "ClipView.hpp"
#include "ToolEdit.hpp"
#include <vcl\Mask.hpp>
#include <vcl\Clipbrd.hpp>
#include "CurrEdit.hpp"
#include "RXCtrls.hpp"
#include "ClipIcon.hpp"
#include "MaxMin.hpp"
//---------------------------------------------------------------------------
class TToolsForm : public TForm
{
__published:	// IDE-managed Components
        TPicClip *PicClip1;
        TRxCalculator *rxCalculator1;
        TFormStorage *FormStorage1;
        TDualListDialog *DualListDialog1;
        TTabbedNotebook *TabbedNotebook1;
        TGroupBox *GroupBox1;
        TLabel *Label1;
        TLabel *Label2;
        TButton *Button1;
        TGroupBox *GroupBox2;
        TLabel *Label8;
        TLabel *Label9;
        TLabel *Label13;
        TImage *Image5;
        TClipboardViewer *ClipboardViewer1;
        TComboBox *ComboBox1;
        TComboBox *ComboBox2;
        TGroupBox *GroupBox3;
        TImage *Image1;
        TLabel *Label22;
        TImage *Image2;
        TGroupBox *GroupBox5;
        TImage *Image3;
        TLabel *Label17;
        TLabel *Label21;
        TRxCheckListBox *ListBox1;
        TButton *Button2;
        TGroupBox *GroupBox4;
        TImage *Image6;
        TLabel *Label4;
        TComboEdit *ComboEdit1;
        TGroupBox *GroupBox6;
        TLabel *Label27;
        TLabel *Label30;
        TLabel *Label31;
        TLabel *Label32;
        TBevel *Bevel1;
        TImage *Image4;
        TLabel *Label3;
        TImage *PicImage;
        TCheckBox *CheckBox1;
        TCurrencyEdit *SpinEdit1;
        TGroupBox *GroupBox7;
        TSecretPanel *SecretPanel1;
        TLabel *Label34;
        TButton *Button3;
        TComboBox *ComboBox5;
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall ComboBox1Change(TObject *Sender);
        void __fastcall ComboBox2Change(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall CheckBox1Click(TObject *Sender);
        void __fastcall SpinEdit1Change(TObject *Sender);
        void __fastcall Button3Click(TObject *Sender);
        void __fastcall ComboBox5Change(TObject *Sender);
        void __fastcall ClipboardViewer1Change(TObject *Sender);
        void __fastcall ComboEdit1ButtonClick(TObject *Sender);
        void __fastcall SecretPanel1DblClick(TObject *Sender);
private:	// User declarations
        void __fastcall PopulateListBox1(void);
public:		// User declarations
        __fastcall TToolsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TToolsForm *ToolsForm;
//---------------------------------------------------------------------------
#endif
