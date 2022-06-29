//---------------------------------------------------------------------------
#ifndef udplstn1H
#define udplstn1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "WSocket.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *DataAvailableLabel;
    TLabel *InfoLabel;
    TLabel *Label1;
    TLabel *Label2;
    TButton *StartButton;
    TButton *StopButton;
    TEdit *PortEdit;
    TEdit *ServerEdit;
    TCheckBox *AnyServerCheckBox;
    TWSocket *WSocket;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall StartButtonClick(TObject *Sender);
    void __fastcall StopButtonClick(TObject *Sender);
    void __fastcall WSocketSessionConnected(TObject *Sender, WORD Error);
    void __fastcall WSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall WSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall AnyServerCheckBoxClick(TObject *Sender);
    void __fastcall ServerEditChange(TObject *Sender);
private:	// User declarations
    AnsiString   FIniFileName;
    AnsiString   FSectionName;
    AnsiString   FKeyName;
    TInAddr      FServerAddr;
public:		// User declarations
    __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
