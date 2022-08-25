//---------------------------------------------------------------------------
#ifndef OverbyteIcsMailRcv1H
#define OverbyteIcsMailRcv1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "OverbyteIcsPop3Prot.hpp"
#include <ExtCtrls.hpp>
#include "OverbyteIcsWndControl.hpp"
//---------------------------------------------------------------------------
class TPOP3ExcercizerForm : public TForm
{
__published:	// IDE-managed Components
    TMemo *DisplayMemo;
    TPanel *Panel1;
    TLabel *InfoLabel;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label5;
    TLabel *Label6;
    TButton *ConnectButton;
    TButton *QuittButton;
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
    TButton *UidlButton;
    TButton *ApopButton;
    TButton *NextButton;
    TButton *GetAllButton;
    TEdit *PortEdit;
    TButton *OpenButton;
    TPop3Cli *Pop3Client;
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall OpenButtonClick(TObject *Sender);
    void __fastcall UserButtonClick(TObject *Sender);
    void __fastcall PassButtonClick(TObject *Sender);
    void __fastcall QuittButtonClick(TObject *Sender);
    void __fastcall RetrButtonClick(TObject *Sender);
    void __fastcall StatButtonClick(TObject *Sender);
    void __fastcall ListAllButtonClick(TObject *Sender);
    void __fastcall ListButtonClick(TObject *Sender);
    void __fastcall DeleteButtonClick(TObject *Sender);
    void __fastcall NoopButtonClick(TObject *Sender);
    void __fastcall LastButtonClick(TObject *Sender);
    void __fastcall ResetButtonClick(TObject *Sender);
    void __fastcall TopButtonClick(TObject *Sender);
    void __fastcall RpopButtonClick(TObject *Sender);
    void __fastcall UidlButtonClick(TObject *Sender);
    void __fastcall ApopButtonClick(TObject *Sender);
    void __fastcall Pop3ClientMessageBegin(TObject *Sender);
    void __fastcall Pop3ClientMessageEnd(TObject *Sender);
    void __fastcall Pop3ClientMessageLine(TObject *Sender);
    void __fastcall Pop3ClientListBegin(TObject *Sender);
    void __fastcall Pop3ClientListLine(TObject *Sender);
    void __fastcall Pop3ClientListEnd(TObject *Sender);
    void __fastcall Pop3ClientUidlBegin(TObject *Sender);
    void __fastcall Pop3ClientUidlEnd(TObject *Sender);
    void __fastcall Pop3ClientUidlLine(TObject *Sender);
    void __fastcall NextButtonClick(TObject *Sender);
    void __fastcall GetAllButtonClick(TObject *Sender);
    void __fastcall Pop3ClientRequestDone(TObject *Sender,
          TPop3Request RqType, WORD Error);
private:	// User declarations
    FILE       *FFile;
    AnsiString FMsgPath;
    AnsiString FFileName;
    int        FGetAllState;
    BOOL       FFileOpened;
    void __fastcall Pop3ClientDisplay(TObject *Sender, String Msg);
    void __fastcall Exec(TPop3NextProc MethodPtr, String MethodName);
    void __fastcall MessageBegin(TObject *Sender);
    void __fastcall MessageLine(TObject *Sender);
    void __fastcall NextMessageRequestDone(TObject      *Sender,
                                           TPop3Request RqType,
                                            WORD         Error);
    void __fastcall GetAllMessageLine(TObject *Sender);
    void __fastcall GetAllRequestDone(TObject      *Sender,
                                      TPop3Request RqType,
                                      WORD         Error);
public:		// User declarations
    __fastcall TPOP3ExcercizerForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TPOP3ExcercizerForm *POP3ExcercizerForm;
//---------------------------------------------------------------------------
#endif
