//---------------------------------------------------------------------------
#ifndef OverbyteIcsSrv5H
#define OverbyteIcsSrv5H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TServerForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *InfoLabel;
    TButton *DisconnectButton;
    TWSocket *SrvSocket;
    TWSocket *CliSocket;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall SrvSocketSessionAvailable(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall DisconnectButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TServerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TServerForm *ServerForm;
//---------------------------------------------------------------------------
#endif
