//---------------------------------------------------------------------------
#ifndef OverbyteIcsTnSrv1H
#define OverbyteIcsTnSrv1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsTnSrv2.h"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TClient : public TObject
{
public:
    TClientForm *Form;
    AnsiString  Peer;
    __fastcall TClient(TComponent *AOwner);
    __fastcall ~TClient();
};
//---------------------------------------------------------------------------
class TServerForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *Memo;
    TWSocket *SrvSocket;
    TButton *AboutButton;
    TButton *QuitButton;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormActivate(TObject *Sender);
    void __fastcall FormResize(TObject *Sender);
    void __fastcall SrvSocketSessionAvailable(TObject *Sender, WORD Error);
    void __fastcall SrvSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall QuitButtonClick(TObject *Sender);
    void __fastcall AboutButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    TList *Clients;
    void __fastcall Display(AnsiString *Msg);
    void __fastcall Display(AnsiString Msg);
    void __fastcall Display(char *Msg);
    __fastcall TServerForm(TComponent* Owner);
protected:
    void __fastcall WMDisconnect(TMessage Message);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_DISCONNECT, TMessage, WMDisconnect)
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern TServerForm *ServerForm;
//---------------------------------------------------------------------------
#endif
