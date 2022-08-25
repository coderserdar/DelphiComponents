//---------------------------------------------------------------------------
#ifndef OverbyteIcsTwsChat1H
#define OverbyteIcsTwsChat1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TTWSChatForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TLabel *Label1;
    TEdit *ServerEdit;
    TButton *ConnectButton;
    TButton *DisconnectButton;
    TRadioButton *RunningRadioButton;
    TRadioButton *StoppedRadioButton;
    TPanel *Panel2;
    TEdit *MessageEdit;
    TButton *SendButton;
    TMemo *DisplayMemo;
    TWSocket *SrvWSocket;
    TWSocket *CliWSocket;
    TWSocket *TmpWSocket;
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall CliWSocketDnsLookupDone(TObject *Sender, WORD Error);
    void __fastcall CliWSocketSessionConnected(TObject *Sender, WORD Error);
    void __fastcall CliWSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall CliWSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall SrvWSocketSessionAvailable(TObject *Sender, WORD Error);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall StoppedRadioButtonClick(TObject *Sender);
    void __fastcall RunningRadioButtonClick(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
    char RcvBuf[1024];
    int  RcvLen;
    BOOL Initialized;
    void __fastcall StartServer();
public:		// User declarations
    __fastcall TTWSChatForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TTWSChatForm *TWSChatForm;
//---------------------------------------------------------------------------
#endif
