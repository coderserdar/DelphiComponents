//---------------------------------------------------------------------------
#ifndef tnCli1H
#define tnCli1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "WSocket.hpp"
#include "Emulvt.hpp"
#include "TnEmulvt.hpp"
//---------------------------------------------------------------------------
class TTelnetForm : public TForm
{
__published:	// IDE-managed Components
	TButton *ConnectButton;
	TLabel *StatusLabel;
	TButton *DisconnectButton;
	TTnEmulVT *TnEmulVT1;
	TLabel *Label1;
	TLabel *Label2;
	TEdit *HostNameEdit;
	TEdit *PortEdit;
    TButton *SendButton;
    TCheckBox *LocalEchoCheckBox;
    TButton *RequestLocalEchoOffButton;
    TButton *RequestLocalEchoOnButton;
    TButton *OptionsButton;
	void __fastcall ConnectButtonClick(TObject *Sender);
	
	
	void __fastcall DisconnectButtonClick(TObject *Sender);
	
	
	void __fastcall TnEmulVT1SessionConnected(TObject *Sender);
	void __fastcall TnEmulVT1SessionClosed(TObject *Sender);
	
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall RequestLocalEchoOffButtonClick(TObject *Sender);
    void __fastcall RequestLocalEchoOnButtonClick(TObject *Sender);
    void __fastcall LocalEchoCheckBoxClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormResize(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall OptionsButtonClick(TObject *Sender);
    
private:	// User declarations
public:		// User declarations
	__fastcall TTelnetForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TTelnetForm *TelnetForm;
//---------------------------------------------------------------------------
#endif
