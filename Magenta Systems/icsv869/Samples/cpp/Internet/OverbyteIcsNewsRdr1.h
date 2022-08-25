//---------------------------------------------------------------------------
#ifndef OverbyteIcsNewsRdr1H
#define OverbyteIcsNewsRdr1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsNntpCli.hpp"
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TNNTPForm : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TEdit *ServerEdit;
	TButton *ConnectButton;
	TButton *AbortButton;
	TButton *GroupButton;
	TEdit *GroupEdit;
	TEdit *ArticleNumEdit;
	TButton *ArticleByNumberButton;
	TButton *ArticleByIDButton;
	TButton *NextButton;
	TButton *LastButton;
	TButton *HeadByNumberButton;
	TButton *HeadByIDButton;
	TButton *BodyByNumberButton;
	TButton *BodyByIDButton;
	TButton *StatByNumberButton;
	TButton *StatByIDButton;
	TButton *ListButton;
	TEdit *ArticleIDEdit;
	TButton *PostButton;
	TButton *QuitButton;
	TEdit *FileEdit;
	TButton *NewGroupsButton;
	TButton *NewNewsButton;
	TButton *HelpButton;
	TButton *XOverButton;
	TButton *OverViewFmtButton;
	TButton *DateButton;
	TMemo *DisplayMemo;
	TNntpCli *NntpCli1;
    TLabel *Label6;
    TEdit *UserEdit;
    TLabel *Label7;
    TEdit *UserNameEdit;
    TLabel *Label8;
    TEdit *PasswordEdit;
    TButton *AuthenticateButton;
        TButton *XHdrButton;
        TButton *ModeReaderButton;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall NntpCli1SessionConnected(TObject *Sender, WORD Error);
	void __fastcall NntpCli1SessionClosed(TObject *Sender, WORD Error);
	void __fastcall NntpCli1RequestDone(TObject *Sender, TNntpRequest RqType,
	WORD Error);
	void __fastcall NntpCli1DataAvailable(TObject *Sender, WORD Error);
	void __fastcall NntpCli1MessageBegin(TObject *Sender);
	void __fastcall NntpCli1MessageLine(TObject *Sender);
	void __fastcall NntpCli1MessageEnd(TObject *Sender);
	void __fastcall ConnectButtonClick(TObject *Sender);
	void __fastcall AbortButtonClick(TObject *Sender);
	void __fastcall QuitButtonClick(TObject *Sender);
	void __fastcall GroupButtonClick(TObject *Sender);
	void __fastcall NextButtonClick(TObject *Sender);
	void __fastcall LastButtonClick(TObject *Sender);
	void __fastcall ArticleByIDButtonClick(TObject *Sender);
	void __fastcall ArticleByNumberButtonClick(TObject *Sender);
	void __fastcall HeadByIDButtonClick(TObject *Sender);
	void __fastcall HeadByNumberButtonClick(TObject *Sender);
	void __fastcall BodyByIDButtonClick(TObject *Sender);
	void __fastcall BodyByNumberButtonClick(TObject *Sender);
	void __fastcall StatByIDButtonClick(TObject *Sender);
	void __fastcall StatByNumberButtonClick(TObject *Sender);
	void __fastcall ListButtonClick(TObject *Sender);
	void __fastcall NewGroupsButtonClick(TObject *Sender);
	void __fastcall NewNewsButtonClick(TObject *Sender);
	void __fastcall HelpButtonClick(TObject *Sender);
	void __fastcall PostButtonClick(TObject *Sender);
	void __fastcall XOverButtonClick(TObject *Sender);
	void __fastcall OverViewFmtButtonClick(TObject *Sender);
	void __fastcall DateButtonClick(TObject *Sender);
    void __fastcall AuthenticateButtonClick(TObject *Sender);
        void __fastcall ModeReaderButtonClick(TObject *Sender);
        void __fastcall XHdrButtonClick(TObject *Sender);
        void __fastcall NntpCli1XHdrBegin(TObject *Sender);
        void __fastcall NntpCli1XHdrEnd(TObject *Sender);
        void __fastcall NntpCli1XHdrLine(TObject *Sender);
private:	// User declarations
    BOOL    FInitialized;
    TStream *FDataStream;
    void __fastcall Display(AnsiString Msg);
    void __fastcall LineToStream(AnsiString Buf);
    Classes::TStream* __fastcall GetStream(void);
public:		// User declarations
	__fastcall TNNTPForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TNNTPForm *NNTPForm;
//---------------------------------------------------------------------------
#endif
