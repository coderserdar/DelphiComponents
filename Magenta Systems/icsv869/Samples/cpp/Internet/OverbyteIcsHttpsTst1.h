//---------------------------------------------------------------------------
#ifndef OverbyteIcsHttpsTst1H
#define OverbyteIcsHttpsTst1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsHttpProt.hpp"
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsSslSessionCache.hpp"
#include "OverbyteIcsCliCertDlg.h"
#include "OverbyteIcsLogger.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
#define WM_SSL_NOT_TRUSTED  (WM_USER + 1)

#define SectionWindow      "HttpTstMainWindow"
#define KeyTop             "Top"
#define KeyLeft            "Left"
#define KeyWidth           "Width"
#define KeyHeight          "Height"
#define SectionData        "Data"
#define KeyUrl             "Url"
#define KeySocksServer     "SocksServer"
#define KeySocksPort       "SocksPort"
#define KeySocksLevel      "SocksLevelIndex"
#define KeyProxyHost       "ProxyHost"
#define KeyProxyPort       "ProxyPort"
#define KeyDoc             "Doc"
#define KeyCertFile        "CertFile"
#define KeyPassPhrase      "PassPhrase"
#define KeyPrivKeyFile     "PrivKeyFile"
#define KeyCAFile          "CAFile"
#define KeyCAPath          "CAPath"
#define KeyLineMode        "LineMode"
#define KeyVerifyPeer      "VerifyPeer"
#define KeyAcceptableHosts "AcceptableHosts"
#define KeyHttpVer         "HttpVer"
#define KeySessCache       "SessCache"
#define KeyDebugEvent      "DebugEvent"
#define KeyDebugOutput     "DebugOutput"
#define KeyDebugFile       "DebugFile"

class THttpTestForm : public TForm
{
__published: // IDE-managed Components
        TPanel *Panel1;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label3;
        TLabel *Label4;
        TLabel *Label5;
        TLabel *Label6;
        TButton *GetButton;
        TEdit *URLEdit;
        TEdit *ProxyHostEdit;
        TEdit *ProxyPortEdit;
        TEdit *DocEdit;
        TEdit *DateTimeEdit;
        TButton *HeadButton;
        TButton *AbortButton;
        TMemo *DisplayMemo;
        TMemo *DocumentMemo;
        TSslHttpCli *SslHttpCli1;
        TIcsLogger *IcsLogger1;
        TSslContext *SslContext1;
        TLabel *Label7;
        TEdit *CertFileEdit;
        TLabel *Label8;
        TEdit *CAFileEdit;
        TEdit *CAPathEdit;
        TLabel *Label17;
        TEdit *PrivKeyFileEdit;
        TLabel *Label9;
        TLabel *Label10;
        TEdit *PassPhraseEdit;
        TCheckBox *VerifyPeerCheckBox;
        TCheckBox *SessCacheCheckBox;
        TLabel *Label12;
        TLabel *Label13;
        TEdit *AcceptableHostsEdit;
        TButton *ButtonOSSLVersion;
        TButton *ClearButton;
        TComboBox *HttpVersionComboBox;
        TComboBox *SocksLevelComboBox;
        TEdit *SocksPortEdit;
        TEdit *SocksServerEdit;
        TCheckBox *DebugFileCheckBox;
        TCheckBox *DebugOutputCheckBox;
        TCheckBox *DebugEventCheckBox;
        TLabel *Label11;
        TLabel *Label15;
        TLabel *Label16;
        TLabel *Label14;
        TLabel *Label18;
        TButton *CloseButton;
        void __fastcall FormShow(TObject *Sender);
        void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
        void __fastcall HeadButtonClick(TObject *Sender);
        void __fastcall GetButtonClick(TObject *Sender);
        void __fastcall SslHttpCli1DocBegin(TObject *Sender);
        void __fastcall SslHttpCli1RequestDone(TObject *Sender, THttpRequest RqType,
        WORD ErrCode);
        void __fastcall AbortButtonClick(TObject *Sender);
        void __fastcall SslHttpCli1Command(TObject *Sender, String &S);
        void __fastcall SslHttpCli1Cookie(TObject *Sender,
          const String Data, bool &Accept);
        void __fastcall SslHttpCli1DocEnd(TObject *Sender);
        void __fastcall SslHttpCli1HeaderData(TObject *Sender);
        void __fastcall SslHttpCli1LocationChange(TObject *Sender);
        void __fastcall SslHttpCli1SslCliCertRequest(TObject *Sender,
          TX509Base *&Cert);
        void __fastcall SslHttpCli1SslCliGetSession(TObject *Sender,
          Pointer &SslSession, bool &FreeSession);
        void __fastcall SslHttpCli1SslCliNewSession(TObject *Sender,
          Pointer SslSession, bool WasReused, bool &IncRefCount);
        void __fastcall SslHttpCli1SslHandshakeDone(TObject *Sender,
          WORD ErrCode, TX509Base *PeerCert, bool &Disconnect);
        void __fastcall SslHttpCli1SslVerifyPeer(TObject *Sender, int &Ok,
          TX509Base *Cert);
        void __fastcall IcsLogger1IcsLogEvent(TObject *Sender,
          TLogOption LogOption, const String Msg);
        void __fastcall ButtonOSSLVersionClick(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall ClearButtonClick(TObject *Sender);
        void __fastcall CloseButtonClick(TObject *Sender);
protected:
    void __fastcall WmSslNotTrusted(TMessage Message);
BEGIN_MESSAGE_MAP
    MESSAGE_HANDLER(WM_SSL_NOT_TRUSTED, TMessage, WmSslNotTrusted)
END_MESSAGE_MAP(TForm)        
private:
	BOOL Initialized;
	String FIniFileName;
	String FDocFileName;
	TX509List* FClientCerts;
    TSslAvlSessionCache* FMyExternalSslSessionCache;
    TStringList* FTrustedList;
    void __fastcall SetButtonState(BOOL State);
    void __fastcall Display(const String Msg);
    void __fastcall BackgroundException(TObject* Sender, Exception* E, bool& CanClose);
    void __fastcall PrepareConnection(void);
public:
    __fastcall THttpTestForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern THttpTestForm *HttpTestForm;
//---------------------------------------------------------------------------
#endif
