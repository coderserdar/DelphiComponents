//---------------------------------------------------------------------------
#ifndef OverbyteIcsSocks1H
#define OverbyteIcsSocks1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include <ExtCtrls.hpp>
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TSocksTestForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TLabel *Label1;
    TLabel *Label2;
    TButton *ConnectButton;
    TButton *DisconnectButton;
    TEdit *TargetHostEdit;
    TEdit *TargetPortEdit;
    TEdit *SocksServerEdit;
    TEdit *SocksPortEdit;
    TEdit *SocksUsercodeEdit;
    TEdit *SocksPasswordEdit;
    TButton *ClearButton;
    TWSocket *WSocket1;
    TLabel *Label6;
    TLabel *Label5;
    TLabel *Label4;
    TLabel *Label3;
    TCheckBox *SocksAuthCheckBox;
    TRadioButton *Socks5RadioButton;
    TRadioButton *Socks4RadioButton;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall WSocket1SessionConnected(TObject *Sender, WORD Error);
    void __fastcall WSocket1SocksConnected(TObject *Sender, WORD Error);
    void __fastcall WSocket1SocksAuthState(TObject *Sender,
          TSocksAuthState AuthState);
    void __fastcall WSocket1SessionClosed(TObject *Sender, WORD Error);
    void __fastcall WSocket1DataAvailable(TObject *Sender, WORD Error);
    void __fastcall WSocket1SocksError(TObject *Sender, int Error,
          AnsiString Msg);
    void __fastcall ClearButtonClick(TObject *Sender);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL       FInitialized;
    char       FRcvBuf[2048];
    int        FRcvCnt;
    void __fastcall DisplayMsg(TObject *Sender, String &Msg);
public:		// User declarations
    __fastcall TSocksTestForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TSocksTestForm *SocksTestForm;
//---------------------------------------------------------------------------
#endif
