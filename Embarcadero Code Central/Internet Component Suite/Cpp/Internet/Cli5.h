//---------------------------------------------------------------------------
#ifndef cli5H
#define cli5H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "WSocket.hpp"
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
    TButton *ReadLineButton;
    void __fastcall CliSocketSessionConnected(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall CliSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall IPButtonClick(TObject *Sender);
    void __fastcall ReadLineButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TClientForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TClientForm *ClientForm;
//---------------------------------------------------------------------------
#endif
