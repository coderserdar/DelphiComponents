//---------------------------------------------------------------------------
#ifndef OverbyteIcsSrvDemo2H
#define OverbyteIcsSrvDemo2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include <ExtCtrls.hpp>
#include <DBTables.hpp>
#include <DB.hpp>
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TCliForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TEdit *SendEdit;
    TButton *SendButton;
    TPanel *Panel2;
    TLabel *LineLabel;
    TButton *DisconnectButton;
    TWSocket *CliSocket;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall CliSocketDataAvailable(TObject *Sender, WORD Error);
    void __fastcall CliSocketSessionClosed(TObject *Sender, WORD Error);
    void __fastcall SendButtonClick(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
private:	// User declarations
    BOOL Initialized;
    char Buffer[1024];
    void __fastcall ProcessCommand(AnsiString Cmd);
public:		// User declarations
    TTable *DataTable;
    __fastcall TCliForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TCliForm *CliForm;
//---------------------------------------------------------------------------
#endif
