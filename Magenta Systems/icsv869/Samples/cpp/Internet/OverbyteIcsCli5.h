//---------------------------------------------------------------------------
#ifndef OverbyteIcscli5H
#define OverbyteIcscli5H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TClientForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *InfoLabel;
    TLabel *DataLabel;
    TButton *ConnectButton;
    TButton *DisconnectButton;
    TWSocket *CliSocket;
    TButton *IPButton;
    void __fastcall CliSocketSessionConnected(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall CliSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall IPButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TClientForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TClientForm *ClientForm;
//---------------------------------------------------------------------------
#endif
