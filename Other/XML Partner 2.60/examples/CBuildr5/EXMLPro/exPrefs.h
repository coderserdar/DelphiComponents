//---------------------------------------------------------------------------
#ifndef ExPrefsH
#define ExPrefsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <inifiles.hpp>
#include <registry.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TPrefsForm : public TForm
{
__published:	// IDE-managed Components
    TPageControl *PageControl1;
    TTabSheet *GeneralTab;
    TCheckBox *NormalizeCheckBox;
    TCheckBox *FormattedCheckBox;
    TButton *btnXMLDefault;
    TButton *btnXSLDefault;
    TCheckBox *BackupCheckBox;
    TCheckBox *TextWinCheckBox;
    TCheckBox *AppWinCheckBox;
    TButton *OkBtn;
    TButton *CancelBtn;
    void __fastcall btnXMLDefaultClick(TObject *Sender);
    void __fastcall btnXSLDefaultClick(TObject *Sender);
    void __fastcall OkBtnClick(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
    TIniFile* FINIFile;
public:		// User declarations
    __fastcall TPrefsForm(TComponent* Owner);
    
    __property TIniFile* INIFile={read=FINIFile, write=FINIFile, nodefault};
};
//---------------------------------------------------------------------------
extern PACKAGE TPrefsForm *PrefsForm;
//---------------------------------------------------------------------------
#endif
