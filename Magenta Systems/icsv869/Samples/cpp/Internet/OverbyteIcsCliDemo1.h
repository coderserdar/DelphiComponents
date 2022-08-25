//---------------------------------------------------------------------------
#ifndef OverbyteIcsCliDemo1H
#define OverbyteIcsCliDemo1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TClientForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TLabel *LineLabel;
    TLabel *Label1;
    TLabel *Label2;
    TEdit *SendEdit;
    TButton *SendButton;
    TButton *DisconnectButton;
    TEdit *PortEdit;
    TEdit *ServerEdit;
    TWSocket *CliSocket;
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall CliSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionConnected(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
    char Buffer[1024];
    int  Count;
    WORD ConnectError;
    BOOL Initialized;
    void __fastcall ProcessCommand(AnsiString Cmd);
public:		// User declarations
    __fastcall TClientForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TClientForm *ClientForm;
//---------------------------------------------------------------------------
#endif
