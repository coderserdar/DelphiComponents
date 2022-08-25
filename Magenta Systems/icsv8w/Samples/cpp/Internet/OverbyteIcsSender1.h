//---------------------------------------------------------------------------
#ifndef OverbyteIcsSender1H
#define OverbyteIcsSender1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"
#define WM_AUTO_START      (WM_USER + 1)
#define WM_CLOSE_REQUEST   (WM_USER + 2)
//---------------------------------------------------------------------------
class TSenderForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label5;
    TLabel *CountLabel;
    TEdit *ServerEdit;
    TEdit *PortEdit;
    TEdit *DataEdit;
    TEdit *RepeatEdit;
    TCheckBox *ContCheckBox;
    TButton *ActionButton;
    TEdit *LengthEdit;
    TCheckBox *DisplayDataCheckBox;
    TCheckBox *UseDataSentCheckBox;
    TButton *PauseButton;
    TButton *AutoStartButton;
    TCheckBox *LingerCheckBox;
    TMemo *DisplayMemo;
    TWSocket *WSocket1;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall ContCheckBoxClick(TObject *Sender);
    void __fastcall ActionButtonClick(TObject *Sender);
    void __fastcall WSocket1DnsLookupDone(TObject *Sender, WORD Error);
    void __fastcall WSocket1SessionConnected(TObject *Sender, WORD Error);
    void __fastcall WSocket1DataSent(TObject *Sender, WORD Error);
    void __fastcall WSocket1DataAvailable(TObject *Sender, WORD Error);
    void __fastcall WSocket1SessionClosed(TObject *Sender, WORD Error);
    void __fastcall DisplayDataCheckBoxClick(TObject *Sender);
    void __fastcall UseDataSentCheckBoxClick(TObject *Sender);
    void __fastcall PauseButtonClick(TObject *Sender);
    void __fastcall AutoStartButtonClick(TObject *Sender);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL       FInitialized;
    char       *FDataBuf;
    int        FDataBufSize;
    int        FCount;
    int        FFinalCount;
    BOOL       FSending;
    BOOL       FDisplayData;
    BOOL       FUseDataSent;
    BOOL       FFinished;
    BOOL       FPaused;
    int        FAutoStart;
    void __fastcall Display(String Msg);
    void __fastcall DoSend(void);
    void __fastcall WSocket1NoDataSent(TObject *Sender, WORD Error);
protected:
    void __fastcall WMAutoStart(TMessage Msg);
    void __fastcall WMCloseRequest(TMessage Msg);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_AUTO_START, TMessage, WMAutoStart)
    MESSAGE_HANDLER(WM_CLOSE_REQUEST, TMessage, WMCloseRequest)
END_MESSAGE_MAP(TForm)
public:		// User declarations
    __fastcall TSenderForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TSenderForm *SenderForm;
//---------------------------------------------------------------------------
#endif
