//---------------------------------------------------------------------------

#ifndef OverbyteIcsWebServ1H
#define OverbyteIcsWebServ1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsHttpSrv.hpp"
#include <ExtCtrls.hpp>
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
// This component is used for client connection instead of default one.
// This enables to add any data we need to handle our application.
// As this data is located in client component, each connected client has
// his own private data.
class TMyHttpConnection : public THttpConnection
{
protected:
    char *FPostedDataBuffer;     // Will hold dynamically allocated buffer
    int  FPostedDataSize;        // Databuffer size
    int  FDataLen;               // Keep track of received byte count.
public:
    virtual __fastcall ~TMyHttpConnection();
};

class TWebServForm : public TForm
{
__published:	// IDE-managed Components
        TPanel *ToolsPanel;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *ClientCountLabel;
        TLabel *Label5;
        TEdit *DocDirEdit;
        TEdit *DefaultDocEdit;
        TButton *StartButton;
        TButton *StopButton;
        TEdit *PortEdit;
        TButton *ClearButton;
        TCheckBox *DisplayHeaderCheckBox;
        TMemo *DisplayMemo;
        THttpServer *HttpServer1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormShow(TObject *Sender);
        void __fastcall StartButtonClick(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall StopButtonClick(TObject *Sender);
        void __fastcall ClearButtonClick(TObject *Sender);
        void __fastcall HttpServer1ServerStarted(TObject *Sender);
        void __fastcall HttpServer1ServerStopped(TObject *Sender);
        void __fastcall HttpServer1ClientConnect(TObject *Sender,
          TObject *Client, WORD Error);
        void __fastcall HttpServer1ClientDisconnect(TObject *Sender,
          TObject *Client, WORD Error);
        void __fastcall HttpServer1HeadDocument(TObject *Sender,
          TObject *Client, THttpGetFlag &Flags);
        void __fastcall HttpServer1GetDocument(TObject *Sender,
          TObject *Client, THttpGetFlag &Flags);
private:	// User declarations
    AnsiString   FIniFileName;
    BOOL         FInitialized;
    int          FCountRequests;
    void __fastcall Display(AnsiString Msg);
    void __fastcall DisplayHeader(TMyHttpConnection *Client);
    void __fastcall CreateVirtualDocument_time_htm(
                        TObject *Sender,
                        TObject *Client,
                        THttpGetFlag &Flags);
public:		// User declarations
        __fastcall TWebServForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebServForm *WebServForm;
//---------------------------------------------------------------------------
#endif
