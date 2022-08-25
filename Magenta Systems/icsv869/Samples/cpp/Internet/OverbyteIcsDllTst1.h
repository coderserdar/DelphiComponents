//---------------------------------------------------------------------------

#ifndef OverbyteIcsDllTst1H
#define OverbyteIcsDllTst1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TDllTestForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *Panel1;
        TMemo *DisplayMemo;
        TButton *CallDllButton;
        TLabel *Label1;
        TEdit *HostnameEdit;
        TLabel *Label2;
        TEdit *PortEdit;
        void __fastcall FormShow(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall CallDllButtonClick(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
    AnsiString   FIniFileName;
    BOOL         FInitialized;
    void __fastcall Display(const System::String &Msg);
public:		// User declarations
        __fastcall TDllTestForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDllTestForm *DllTestForm;
//---------------------------------------------------------------------------
#endif
