//---------------------------------------------------------------------------
#ifndef srv5H
#define srv5H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "WSocket.hpp"
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
