//---------------------------------------------------------------------------
#ifndef MailSnd1H
#define MailSnd1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "SmtpProt.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TMailSndForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *MsgMemo;
    TMemo *DisplayMemo;
    TPanel *ToolsPanel;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Subject;
    TLabel *Label4;
    TLabel *Label5;
    TLabel *Label8;
    TEdit *HostEdit;
    TEdit *FromEdit;
    TEdit *ToEdit;
    TEdit *SubjectEdit;
    TEdit *SignOnEdit;
    TEdit *PortEdit;
    TButton *ClearDisplayButton;
    TButton *ConnectButton;
    TButton *HeloButton;
    TButton *MailFromButton;
    TButton *RcptToButton;
    TButton *DataButton;
    TButton *AbortButton;
    TButton *QuitButton;
    TButton *MailButton;
    TButton *OpenButton;
    TPanel *AttachPanel;
    TLabel *Label6;
    TMemo *FileAttachMemo;
    TPanel *InfoPanel;
    TLabel *Label7;
    TSmtpCli *SmtpClient;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    void __fastcall SmtpClientDisplay(TObject *Sender, AnsiString Msg);
    void __fastcall SmtpClientGetData(TObject *Sender, int LineNum,
          PChar MsgLine, int MaxLen, bool &More);
    void __fastcall SmtpClientHeaderLine(TObject *Sender, PChar Msg,
          int Size);
    void __fastcall ClearDisplayButtonClick(TObject *Sender);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall HeloButtonClick(TObject *Sender);
    void __fastcall OpenButtonClick(TObject *Sender);
    void __fastcall MailFromButtonClick(TObject *Sender);
    void __fastcall RcptToButtonClick(TObject *Sender);
    void __fastcall DataButtonClick(TObject *Sender);
    void __fastcall MailButtonClick(TObject *Sender);
    void __fastcall QuitButtonClick(TObject *Sender);
    void __fastcall AbortButtonClick(TObject *Sender);
    void __fastcall SmtpClientRequestDone(TObject *Sender,
          TSmtpRequest RqType, WORD Error);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL       FInitialized;
    void __fastcall ExceptionHandler(TObject *Sender, Exception *E);
    void __fastcall BuildRcptList(void);
public:		// User declarations
    __fastcall TMailSndForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TMailSndForm *MailSndForm;
//---------------------------------------------------------------------------
#endif
