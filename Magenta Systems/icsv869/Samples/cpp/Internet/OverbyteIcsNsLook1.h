//---------------------------------------------------------------------------
#ifndef OverbyteIcsNsLook1H
#define OverbyteIcsNsLook1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsDnsQuery.hpp"
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TNsLookupForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TEdit *DnsEdit;
    TEdit *NameEdit;
    TButton *MXLookupButton;
    TBitBtn *ClearDisplayBitBtn;
    TButton *ALookupButton;
    TDnsQuery *DnsQuery1;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall DnsQuery1RequestDone(TObject *Sender, WORD Error);
    void __fastcall ClearDisplayBitBtnClick(TObject *Sender);
    void __fastcall MXLookupButtonClick(TObject *Sender);
    void __fastcall ALookupButtonClick(TObject *Sender);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL       FInitialized;
    int        FRequestID;
    void __fastcall Display(String Msg);
    void __fastcall DumpDnsResponse(void);
public:		// User declarations
    __fastcall TNsLookupForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TNsLookupForm *NsLookupForm;
//---------------------------------------------------------------------------
#endif
