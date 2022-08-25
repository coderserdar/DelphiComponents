//---------------------------------------------------------------------------

#ifndef OverbyteIcsReverseDnsSync1H
#define OverbyteIcsReverseDnsSync1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWndControl.hpp"
#include "OverbyteIcsWSocket.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TEdit *Edit1;
        TButton *Button1;
        void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
        int ReverseDNSFlag;
        AnsiString __fastcall getICSReverseDNS(AnsiString Addr);
        void __fastcall WSocket1DnsLookupDone(TObject *Sender, WORD Error);
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
