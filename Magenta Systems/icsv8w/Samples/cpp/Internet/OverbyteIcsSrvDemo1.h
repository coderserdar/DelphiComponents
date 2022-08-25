//---------------------------------------------------------------------------
#ifndef OverbyteIcsSrvDemo1H
#define OverbyteIcsSrvDemo1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Db.hpp>
#include <DBTables.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TSrvForm : public TForm
{
__published:	// IDE-managed Components
    TListBox *ClientListBox;
    TPanel *Panel1;
    TLabel *Label1;
    TEdit *PortEdit;
    TButton *RestartButton;
    TWSocket *SrvSocket;
    TTable *DataTable;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall RestartButtonClick(TObject *Sender);
    void __fastcall SrvSocketSessionAvailable(TObject *Sender, WORD Error);
private:	// User declarations
    BOOL Initialized;
    int  ClientNumber;
    void __fastcall StartServer(void);
protected:
    void __fastcall WMUser(TMessage Message);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_USER, TMessage, WMUser)
END_MESSAGE_MAP(TForm)
public:		// User declarations
    __fastcall TSrvForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TSrvForm *SrvForm;
//---------------------------------------------------------------------------
#endif
