//---------------------------------------------------------------------------
#ifndef OverbyteIcsPingTst1H
#define OverbyteIcsPingTst1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsPing.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TPingTstForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TEdit *HostEdit;
    TButton *PingButton;
    TMemo *DisplayMemo;
    TButton *CancelButton;
    TPing *Ping1;
    void __fastcall PingButtonClick(TObject *Sender);
    void __fastcall Ping1DnsLookupDone(TObject *Sender, WORD Error);
    void __fastcall Ping1Display(TObject *Sender, TObject *Icmp, AnsiString Msg);
    void __fastcall CancelButtonClick(TObject *Sender);
    void __fastcall Ping1EchoRequest(TObject *Sender, TObject *Icmp);
    void __fastcall Ping1EchoReply(TObject *Sender, TObject *Icmp, int Error);
private:	// User declarations
public:		// User declarations
    __fastcall TPingTstForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TPingTstForm *PingTstForm;
//---------------------------------------------------------------------------
#endif
