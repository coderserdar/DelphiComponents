//---------------------------------------------------------------------------
#ifndef OverbyteIcsRecv1H
#define OverbyteIcsRecv1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include <ExtCtrls.hpp>
#include "OverbyteIcsWndControl.hpp"
#define WM_DESTROY_SOCKET (WM_USER + 1)
//---------------------------------------------------------------------------
class TRecvForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TLabel *Label1;
    TLabel *Label2;
    TEdit *PortEdit;
    TButton *ActionButton;
    TButton *CloseAllButton;
    TCheckBox *LingerCheckBox;
    TCheckBox *BannerCheckBox;
    TButton *LineModeOnButton;
    TButton *LineOffButton;
    TMemo *DisplayMemo;
    TWSocket *WSocket1;
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall ActionButtonClick(TObject *Sender);
    void __fastcall PortEditChange(TObject *Sender);
    void __fastcall WSocket1SessionAvailable(TObject *Sender, WORD Error);
    void __fastcall CloseAllButtonClick(TObject *Sender);
    void __fastcall LineModeOnButtonClick(TObject *Sender);
    void __fastcall LineOffButtonClick(TObject *Sender);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL       FInitialized;
    TList      *FClients;
    void __fastcall ClientDataAvailable(TObject *Sender, WORD Error);
    void __fastcall ClientSessionClosed(TObject *Sender, WORD Error);
    void __fastcall Display(AnsiString Msg);
protected:
    void __fastcall WMDestroySocket(TMessage Msg);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_DESTROY_SOCKET, TMessage, WMDestroySocket)
END_MESSAGE_MAP(TForm)
public:		// User declarations
    __fastcall TRecvForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TRecvForm *RecvForm;
//---------------------------------------------------------------------------
#endif
