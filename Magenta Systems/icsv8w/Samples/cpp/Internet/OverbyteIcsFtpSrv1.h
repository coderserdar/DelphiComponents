//---------------------------------------------------------------------------
#ifndef OverbyteIcsFtpSrv1H
#define OverbyteIcsFtpSrv1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsFtpSrv.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <Graphics.hpp>
#include "OverbyteIcsWndControl.hpp"
#include <OverbyteIcsUtils.hpp>
#define   WM_APPSTARTUP  (WM_USER + 1)
//---------------------------------------------------------------------------
class TMyClient;
class TGetProcessingThread : public TThread
{
protected:
        void __fastcall Execute();
public:
        TFtpServer *Server;
        TMyClient  *Client;
        __fastcall TGetProcessingThread(bool CreateSuspended);
};
//---------------------------------------------------------------------------
class TMyClient :public TFtpCtrlSocket
{
public:
      TGetProcessingThread *FWorkerThread;
};
//---------------------------------------------------------------------------
class TLogMsg : public TComponent
{
public:
     void __fastcall Text(char Prefix, AnsiString Msg);
    __fastcall TLogMsg(TComponent* Owner);
};
//---------------------------------------------------------------------------
class TFtpServerForm : public TForm
{
__published:	// IDE-managed Components
    TPanel *Panel1;
    TImage *GreenImage;
    TLabel *ClientCountLabel;
    TImage *RedImage;
    TCheckBox *StartMinimizedCheckBox;
    TMemo *InfoMemo;
    TMainMenu *MainMenu1;
    TMenuItem *File1;
    TMenuItem *MnuStartServer;
    TMenuItem *MnuStopServer;
    TMenuItem *N1;
    TMenuItem *MnuQuit;
    TMenuItem *Tools1;
    TMenuItem *Cleardisplay1;
    TMenuItem *About1;
    TFtpServer *FtpServer1;
        TMenuItem *Disconnectall1;
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall MnuQuitClick(TObject *Sender);
    void __fastcall MnuStopServerClick(TObject *Sender);
    void __fastcall MnuStartServerClick(TObject *Sender);
    void __fastcall ImagesDblClick(TObject *Sender);
    void __fastcall FtpServer1ClientConnect(TObject *Sender,
          TFtpCtrlSocket *Client, WORD Error);
    void __fastcall FtpServer1ClientDisconnect(TObject *Sender,
          TFtpCtrlSocket *Client, WORD Error);
    void __fastcall FtpServer1Start(TObject *Sender);
    void __fastcall FtpServer1Stop(TObject *Sender);
    void __fastcall FtpServer1StorSessionConnected(TObject *Sender,
          TFtpCtrlSocket *Client, TWSocket *Data, WORD Error);
    void __fastcall FtpServer1StorSessionClosed(TObject *Sender,
          TFtpCtrlSocket *Client, TWSocket *Data, WORD Error);
    void __fastcall FtpServer1RetrDataSent(TObject *Sender,
          TFtpCtrlSocket *Client, TWSocket *Data, WORD Error);
    void __fastcall FtpServer1RetrSessionConnected(TObject *Sender,
          TFtpCtrlSocket *Client, TWSocket *Data, WORD Error);
    void __fastcall FtpServer1RetrSessionClosed(TObject *Sender,
          TFtpCtrlSocket *Client, TWSocket *Data, WORD Error);
    void __fastcall FtpServer1BuildDirectory(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString &Directory, bool Detailed);
    void __fastcall FtpServer1AlterDirectory(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString &Directory, bool Detailed);
    void __fastcall FtpServer1ClientCommand(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString &Keyword, TFtpString &Params,
          TFtpString &Answer);
    void __fastcall FtpServer1AnswerToClient(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString &Answer);
    void __fastcall FtpServer1Authenticate(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString UserName,
          TFtpString Password, bool &Authenticated);
    void __fastcall FtpServer1ChangeDirectory(TObject *Sender,
          TFtpCtrlSocket *Client, TFtpString &Directory, bool &Allowed);
    void __fastcall Cleardisplay1Click(TObject *Sender);
        void __fastcall Disconnectall1Click(TObject *Sender);
        void __fastcall FtpServer1GetProcessing(TObject *Sender,
          TFtpCtrlSocket *Client, bool &DelayedSend);
protected:
    void __fastcall WMAppStartup(TMessage &Msg);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_APPSTARTUP, TMessage, WMAppStartup)
END_MESSAGE_MAP(TControl)
private:	// User declarations
    BOOL       FInitialized;
    AnsiString FIniFileName;
    AnsiString FPort;
    int        FXTop;
    int        FXLeft;
    int        FXWidth;
    int        FXHeight;
    void __fastcall LoadConfig(void);
    void __fastcall SaveConfig(void);
    void __fastcall StartServer(void);
    void __fastcall StopServer(void);
    void __fastcall UpdateClientCount(void);
    void __fastcall WorkerThreadTerminated(TObject *Sender);
public:		// User declarations
    __fastcall TFtpServerForm(TComponent* Owner);
};
#endif
