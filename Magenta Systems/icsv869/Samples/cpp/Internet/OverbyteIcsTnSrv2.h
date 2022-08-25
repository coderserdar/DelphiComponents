//---------------------------------------------------------------------------
#ifndef OverbyteIcsTnSrv2H
#define OverbyteIcsTnSrv2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"

#define WM_DISCONNECT      (WM_USER + 2)
#define DISCONNECT_SELF    1          // Client form ask to disconnect          
#define DISCONNECT_REMOTE  2          // Client user (remote) has disconnected
//---------------------------------------------------------------------------
class TClientForm : public TForm
{
__published:	// IDE-managed Components
    TWSocket *Socket;
    TMemo *Memo;
    TEdit *DataEdit;
    TButton *SendButton;
    TButton *DisconnectButton;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall SocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall SocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall FormResize(TObject *Sender);
private:	// User declarations
    AnsiString FCommand;
    BOOL       FRcvdCR;
    void __fastcall CommandInterpreter();
    void __fastcall ProcessChar(char Ch);
public:		// User declarations
    TForm *AcceptForm;
    void  *Reference;
    void __fastcall Display(char *Msg);
    void __fastcall Display(AnsiString *Msg);
    __fastcall TClientForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TClientForm *ClientForm;
//---------------------------------------------------------------------------
#endif
