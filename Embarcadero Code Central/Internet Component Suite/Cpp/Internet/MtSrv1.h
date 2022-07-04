//---------------------------------------------------------------------------
#ifndef mtsrv1H
#define mtsrv1H
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
    TButton *DisconnectButton;
    TButton *DisconnectAllButton;
    TButton *QuitButton;
    TListBox *ClientListBox;
    TWSocket *ServerWSocket;
    void __fastcall QuitButtonClick(TObject *Sender);
    void __fastcall ServerWSocketSessionAvailable(TObject *Sender, WORD Error);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall DisconnectAllButtonClick(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:	// User declarations
    void __fastcall ClientThreadTerminate(TObject *Sender);
    void __fastcall DisconnectAll();
public:		// User declarations
    __fastcall TServerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TServerForm *ServerForm;
//---------------------------------------------------------------------------
#endif
