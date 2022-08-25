//---------------------------------------------------------------------------
#ifndef OverbyteIcsFtpTst1H
#define OverbyteIcsFtpTst1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include "OverbyteIcsFtpCli.hpp"
#include "OverbyteIcsWSocket.hpp"
#include "OverbyteIcsWndControl.hpp"

typedef bool __fastcall (__closure *TSyncCmd)();
typedef void __fastcall (__closure *TASyncCmd)();

//---------------------------------------------------------------------------
class TFtpReceiveForm : public TForm
{
__published:	// IDE-managed Components
	TMemo *DisplayMemo;
	TPanel *Panel1;
	TLabel *InfoLabel;
	TLabel *StateLabel;
	TButton *ExitButton;
	TButton *OpenAsyncButton;
	TButton *QuitAsyncButton;
	TButton *CwdAsyncButton;
	TButton *UserAsyncButton;
	TButton *PassAsyncButton;
	TButton *ConnectAsyncButton;
	TButton *GetAsyncButton;
	TButton *ReceiveAsyncButton;
	TButton *AbortAsyncButton;
	TButton *DirAsyncButton;
	TButton *DirectoryAsyncButton;
	TButton *LsAsyncButton;
	TButton *ListAsyncButton;
	TButton *SystAsyncButton;
	TButton *SystemAsyncButton;
	TButton *FileSizeAsyncButton;
	TButton *SizeAsyncButton;
	TButton *MkdAsyncButton;
	TButton *MkdirAsyncButton;
	TButton *RmdAsyncButton;
	TButton *RmdirAsyncButton;
	TButton *RenAsyncButton;
	TButton *RenameAsyncButton;
	TButton *DeleAsyncButton;
	TButton *DeleteAsyncButton;
	TButton *PwdAsyncButton;
	TButton *QuoteAsyncButton;
	TButton *DoQuoteAsyncButton;
	TButton *PutAsyncButton;
	TButton *TransmitAsyncButton;
	TButton *TypeSetAsyncButton;
	TButton *RestGetAsyncButton;
	TButton *RestartGetAsyncButton;
	TButton *CDupAsyncButton;
	TButton *ClearButton;
	TPanel *Panel2;
	TLabel *Label1;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label2;
	TLabel *Label6;
	TEdit *HostNameEdit;
	TEdit *HostFileEdit;
	TEdit *UserNameEdit;
	TEdit *PassWordEdit;
	TCheckBox *cbDisplay;
	TEdit *LocalFileEdit;
	TCheckBox *cbBinary;
	TEdit *HostDirEdit;
	TEdit *PortEdit;
	TCheckBox *SyncCheckBox;
	TFtpClient *FtpClient1;
    TButton *AppendFileAsyncButton;
    TButton *AppendAsyncButton;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
    
    void __fastcall ExitButtonClick(TObject *Sender);
    // BCB1 wants to have count as long; BCB3 wants to have int.
    // You can correct code accordingly, but don't let IDE remove reference !! 
    void __fastcall FtpClient1Progress64(TObject *Sender, __int64 Count, bool &Abort);
    void __fastcall FtpClient1RequestDone(TObject *Sender, TFtpRequest RqType,
    WORD Error);
    void __fastcall FtpClient1SessionConnected(TObject *Sender, WORD Error);
    void __fastcall FtpClient1SessionClosed(TObject *Sender, WORD Error);
    void __fastcall FtpClient1StateChange(TObject *Sender);
    void __fastcall OpenAsyncButtonClick(TObject *Sender);
    void __fastcall QuitAsyncButtonClick(TObject *Sender);
    void __fastcall CwdAsyncButtonClick(TObject *Sender);
    void __fastcall UserAsyncButtonClick(TObject *Sender);
    void __fastcall PassAsyncButtonClick(TObject *Sender);
    void __fastcall ConnectAsyncButtonClick(TObject *Sender);
    void __fastcall GetAsyncButtonClick(TObject *Sender);
    void __fastcall ReceiveAsyncButtonClick(TObject *Sender);
    void __fastcall AbortAsyncButtonClick(TObject *Sender);
    void __fastcall DirAsyncButtonClick(TObject *Sender);
    void __fastcall DirectoryAsyncButtonClick(TObject *Sender);
    void __fastcall LsAsyncButtonClick(TObject *Sender);
    void __fastcall ListAsyncButtonClick(TObject *Sender);
    void __fastcall SystAsyncButtonClick(TObject *Sender);
    void __fastcall SystemAsyncButtonClick(TObject *Sender);
    void __fastcall FileSizeAsyncButtonClick(TObject *Sender);
    void __fastcall SizeAsyncButtonClick(TObject *Sender);
    void __fastcall MkdAsyncButtonClick(TObject *Sender);
    void __fastcall MkdirAsyncButtonClick(TObject *Sender);
    void __fastcall RmdAsyncButtonClick(TObject *Sender);
    void __fastcall RmdirAsyncButtonClick(TObject *Sender);
    void __fastcall RenAsyncButtonClick(TObject *Sender);
    void __fastcall RenameAsyncButtonClick(TObject *Sender);
    void __fastcall DeleAsyncButtonClick(TObject *Sender);
    void __fastcall DeleteAsyncButtonClick(TObject *Sender);
    void __fastcall PwdAsyncButtonClick(TObject *Sender);
    void __fastcall QuoteAsyncButtonClick(TObject *Sender);
    void __fastcall DoQuoteAsyncButtonClick(TObject *Sender);
    void __fastcall PutAsyncButtonClick(TObject *Sender);
    void __fastcall TransmitAsyncButtonClick(TObject *Sender);
    void __fastcall TypeSetAsyncButtonClick(TObject *Sender);
    void __fastcall RestGetAsyncButtonClick(TObject *Sender);
    void __fastcall RestartGetAsyncButtonClick(TObject *Sender);
    void __fastcall CDupAsyncButtonClick(TObject *Sender);
    void __fastcall ClearButtonClick(TObject *Sender);
    void __fastcall AppendAsyncButtonClick(TObject *Sender);
    void __fastcall AppendFileAsyncButtonClick(TObject *Sender);
private:	// User declarations
    AnsiString FIniFileName;
    BOOL	   FInitialized;
    void __fastcall Display(TObject *Sender, System::String &Msg);
    void __fastcall DisplayFile(const System::String FileName);
    void __fastcall TFtpReceiveForm::ExecuteCmd(
                                TSyncCmd SyncCmd, TASyncCmd ASyncCmd);
public:		// User declarations
	__fastcall TFtpReceiveForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TFtpReceiveForm *FtpReceiveForm;
//---------------------------------------------------------------------------
#endif
