//---------------------------------------------------------------------------
#ifndef ctrlsH
#define ctrlsH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Tabnotbk.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include "RXCtrls.hpp"
#include "RXCombos.hpp"
#include "RXSlider.hpp"
#include "RXSwitch.hpp"
#include "ToolEdit.hpp"
#include <vcl\Mask.hpp>
#include "CurrEdit.hpp"
#include "RXSpin.hpp"
#include <vcl\FileCtrl.hpp>
#include "RXSplit.hpp"
#include "Animate.hpp"
#include "RXClock.hpp"
#include "RXDice.hpp"
#include "AniFile.hpp"
#include "DateUtil.hpp"
#include "PickDate.hpp"
#include <vcl\Dialogs.hpp>
#include "Placemnt.hpp"
#include "RxNotify.hpp"
//---------------------------------------------------------------------------
class TControlsForm : public TForm
{
__published:	// IDE-managed Components
        TRxDice *rxDice1;
        TRxClock *rxClock1;
        TComboBox *ComboBox8;
        TTextListBox *TextListBox1;
        TShape *Shape1;
        TColorComboBox *ColorComboBox1;
        TFontComboBox *FontComboBox1;
        TComboBox *ComboBox1;
        TCheckBox *CheckBox1;
        TComboBox *ComboBox2;
        TComboBox *ComboBox3;
        TImage *ADHThumb;
        TImage *ADVThumb;
        TImage *ADVRuler;
        TImage *ADHRuler;
        TImage *SwOn;
        TImage *SwOff;
        TFontDialog *FontDialog1;
        TFormStorage *FormStorage1;
        TFilenameEdit *FilenameEdit1;
        TComboEdit *ComboEdit1;
        TCurrencyEdit *CurrencyEdit1;
        TEdit *Edit1;
        TDateEdit *DateEdit1;
        TLabel *Label14;
        TDirectoryEdit *DirectoryEdit1;
        TColorComboBox *ColorComboBox2;
        TComboBox *ComboBox6;
        TDriveComboBox *DriveComboBox1;
        TDirectoryListBox *DirectoryListBox1;
        TFileListBox *FileListBox1;
        TMemo *Memo1;
        TAnimatedImage *AnimatedImage1;
        TCurrencyEdit *SpinEdit3;
        TCurrencyEdit *SpinEdit4;
        TButton *Button2;
        TImage *RunnerImage;
        TImage *BookImage;
        TImage *SearchImage;
        TImage *FlagImage;
        TComboBox *ComboBox7;
        TLabel *Label1;
        TLabel *Label2;
        TRxLabel *rxLabel1;
        TCheckBox *CheckBox2;
        TCheckBox *CheckBox3;
        TButton *Button5;
        TButton *Button4;
        TCheckBox *CheckBox4;
        TCheckBox *CheckBox6;
        TCurrencyEdit *SpinEdit5;
        TCurrencyEdit *SpinEdit6;
        TCurrencyEdit *SpinEdit7;
        TCurrencyEdit *SpinEdit8;
        TCurrencyEdit *SpinEdit9;
        TLabel *Label6;
        TRxSlider *rxSlider1;
        TComboBox *ComboBox4;
        TRxSwitch *rxSwitch1;
        TCheckBox *CheckBox5;
        TComboBox *ComboBox5;
        TRxSpinEdit *rxSpinEdit1;
        TComboBox *ComboBox9;
        TCurrencyEdit *SpinEdit2;
        TCurrencyEdit *SpinEdit1;
        TTabbedNotebook *TabbedNotebook1;
        TGroupBox *GroupBox1;
        TGroupBox *GroupBox2;
        TGroupBox *GroupBox3;
        TGroupBox *GroupBox4;
        TPanel *Panel1;
        TPanel *Panel2;
        TOpenDialog *OpenDialog;
        TRxSplitter *rxSplitter1;
        TRxSplitter *rxSplitter2;
        TCheckBox *CheckBox7;
        TRxCalcEdit *RxCalcEdit1;
        TRxFolderMonitor *RxFolderMonitor;
        void __fastcall SpinEdit3Change(TObject *Sender);
        void __fastcall ComboBox7Change(TObject *Sender);
        void __fastcall SpinEdit4Change(TObject *Sender);
        void __fastcall AnimatedImage1StartStop(TObject *Sender);
        
        void __fastcall Button2Click(TObject *Sender);
        void __fastcall Button6Click(TObject *Sender);
        void __fastcall Button5Click(TObject *Sender);
        void __fastcall ComboBox8Change(TObject *Sender);
        void __fastcall rxClock1Alarm(TObject *Sender);
        void __fastcall Button3Click(TObject *Sender);
        void __fastcall CheckBox6Click(TObject *Sender);
        void __fastcall AlarmEditChange(TObject *Sender);
        void __fastcall rxDice1Change(TObject *Sender);
        void __fastcall Button4Click(TObject *Sender);
        void __fastcall SpinEdit9Change(TObject *Sender);
        void __fastcall SpinEdit8Change(TObject *Sender);
        void __fastcall CheckBox4Click(TObject *Sender);
        void __fastcall ColorComboBox1Change(TObject *Sender);
        void __fastcall FontComboBox1Change(TObject *Sender);
        void __fastcall ComboBox1Change(TObject *Sender);
        void __fastcall CheckBox1Click(TObject *Sender);
        void __fastcall rxSlider1Change(TObject *Sender);
        void __fastcall ComboBox2Change(TObject *Sender);
        void __fastcall ComboBox3Change(TObject *Sender);
        void __fastcall CheckBox3Click(TObject *Sender);
        void __fastcall CheckBox2Click(TObject *Sender);
        void __fastcall ComboBox4Change(TObject *Sender);
        void __fastcall rxSwitch1Off(TObject *Sender);
        void __fastcall rxSwitch1On(TObject *Sender);
        void __fastcall CheckBox5Click(TObject *Sender);
        void __fastcall ComboBox5Change(TObject *Sender);
        void __fastcall ComboEdit1ButtonClick(TObject *Sender);
        void __fastcall Edit1Change(TObject *Sender);
        void __fastcall ComboBox9Change(TObject *Sender);
        void __fastcall SpinEdit1Change(TObject *Sender);
        void __fastcall ColorComboBox2Change(TObject *Sender);
        void __fastcall ComboBox6Change(TObject *Sender);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall SpinEdit2Change(TObject *Sender);
        void __fastcall FileListBox1Change(TObject *Sender);
        
        void __fastcall CheckBox7Click(TObject *Sender);
        void __fastcall DirectoryListBox1Change(TObject *Sender);
        void __fastcall RxFolderMonitorChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TControlsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TControlsForm *ControlsForm;
//---------------------------------------------------------------------------
#endif
