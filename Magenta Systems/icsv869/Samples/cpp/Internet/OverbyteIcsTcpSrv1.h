//---------------------------------------------------------------------------
#ifndef OverbyteIcsTcpSrv1H
#define OverbyteIcsTcpSrv1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWSocketS.hpp"
#include <ExtCtrls.hpp>
#include "OverbyteIcsWndControl.hpp"
#define WM_APPSTARTUP (WM_USER + 1)
//---------------------------------------------------------------------------
// TTcpSrvClient is the class which will be instanciated by server component
// for each new client. N simultaneous clients means N TTcpSrvClient will be
// instanciated. Each being used to handle only a signle client.
// We can add any data that has to be private for each client, such as
// receive buffer or any other data needed for processing.
class TTcpSrvClient : public TWSocketClient
{
public:
    AnsiString RcvdLine;
    TDateTime  ConnectTime;
    __fastcall TTcpSrvClient(TComponent* Owner);
};
//---------------------------------------------------------------------------
class TTcpSrvForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *ToolPanel;
    TMemo *DisplayMemo;
    TWSocketServer *WSocketServer1;void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall WSocketServer1ClientConnect(TObject *Sender,
          TWSocketClient *Client, WORD Error);
    void __fastcall WSocketServer1ClientDisconnect(TObject *Sender,
          TWSocketClient *Client, WORD Error);
    void __fastcall WSocketServer1BgException(TObject *Sender,
          Exception *E, bool &CanClose);
private:	// User declarations
    AnsiString   FIniFileName;
    BOOL         FInitialized;
    void __fastcall Display(AnsiString Msg);
    void __fastcall ProcessData(TTcpSrvClient *Client);
    void __fastcall ClientDataAvailable(TObject *Sender, WORD Error);
    void __fastcall ClientBgException(TObject   *Sender,
                                      Exception *E,
                                      bool      &CanClose);
public:		// User declarations
    __fastcall TTcpSrvForm(TComponent* Owner);
protected:
    void __fastcall WMAppStartup(TMessage Message);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_APPSTARTUP, TMessage, WMAppStartup)
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
#endif
