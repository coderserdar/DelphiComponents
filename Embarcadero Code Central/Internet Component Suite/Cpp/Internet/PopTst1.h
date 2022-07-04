//---------------------------------------------------------------------------
#ifndef PopTst1H
#define PopTst1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "Wait.hpp"
#include "pop3cli.hpp"
//---------------------------------------------------------------------------
class TPOP3ExcercizerForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *InfoLabel;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label5;
    TButton *ConnectButton;
    TWait *Wait1;
    TMemo *DisplayMemo;
    TButton *DisconnectButton;
    TButton *UserButton;
    TEdit *HostEdit;
    TEdit *UserNameEdit;
    TEdit *PassWordEdit;
    TButton *PassButton;
    TEdit *MsgNumEdit;
    TButton *RetrButton;
    TButton *StatButton;
    TButton *ListAllButton;
    TButton *ListButton;
    TButton *DeleteButton;
    TButton *NoopButton;
    TButton *LastButton;
    TButton *ResetButton;
    TButton *TopButton;
    TEdit *MsgLinesEdit;
    TButton *RpopButton;
    TPop3Client *Pop3Client;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall Pop3ClientDisplay(TObject *Sender, AnsiString Msg);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall UserButtonClick(TObject *Sender);
    void __fastcall PassButtonClick(TObject *Sender);
    void __fastcall RetrButtonClick(TObject *Sender);
    void __fastcall DisconnectButtonClick(TObject *Sender);
    void __fastcall TopButtonClick(TObject *Sender);
    void __fastcall RpopButtonClick(TObject *Sender);
    void __fastcall StatButtonClick(TObject *Sender);
    void __fastcall ListAllButtonClick(TObject *Sender);
    void __fastcall ListButtonClick(TObject *Sender);
    void __fastcall DeleteButtonClick(TObject *Sender);
    void __fastcall NoopButtonClick(TObject *Sender);
    void __fastcall LastButtonClick(TObject *Sender);
    void __fastcall ResetButtonClick(TObject *Sender);
    void __fastcall Pop3ClientMessageBegin(TObject *Sender);
    void __fastcall Pop3ClientMessageEnd(TObject *Sender);
    void __fastcall Pop3ClientMessageLine(TObject *Sender);
    void __fastcall Pop3ClientListBegin(TObject *Sender);
    void __fastcall Pop3ClientListEnd(TObject *Sender);
    void __fastcall Pop3ClientListLine(TObject *Sender);
private:	// User declarations
    BOOL __fastcall TPOP3ExcercizerForm::DoTheJob(
        TPop3Method MethodPtr,
        AnsiString  MethodName);
public:		// User declarations
    __fastcall TPOP3ExcercizerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TPOP3ExcercizerForm *POP3ExcercizerForm;
//---------------------------------------------------------------------------
#endif
