//---------------------------------------------------------------------------
#ifndef ExClMainH
#define ExClMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ffllbase.hpp"
#include "ffllcomm.hpp"
#include "ffllcomp.hpp"
#include "fflllgcy.hpp"
#include <ExtCtrls.hpp>
#include "ExChtMsg.h"
//---------------------------------------------------------------------------
class TfrmCltMain : public TForm
{
__published:	// IDE-managed Components
  TBevel *bvMain;
  TLabel *lblUserName;
  TLabel *lblConnect;
  TButton *pbSend;
  TEdit *efMessage;
  TButton *pbConnect;
  TButton *pbDisconnect;
  TEdit *efUserName;
  TListBox *lbUsers;
  TCheckBox *chkPrivate;
  TListBox *lbOutput;
  TButton *pbExit;
  TComboBox *cmbServers;
  TButton *pbRefreshServers;
  TffLegacyTransport *tpClient;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall pbSendClick(TObject *Sender);
  void __fastcall efMessageKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall lbOutputDrawItem(TWinControl *Control, int Index,
          TRect &Rect, TOwnerDrawState State);
  void __fastcall chkPrivateClick(TObject *Sender);
  void __fastcall pbExitClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall pbConnectClick(TObject *Sender);
  void __fastcall pbDisconnectClick(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall pbRefreshServersClick(TObject *Sender);
private:	// User declarations
  TffChatClntHandler* FChatHandler;
  TffClientID FClientID;

  void RefreshServers();
  void SetCtrlStates();
public:		// User declarations
  __fastcall TfrmCltMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCltMain *frmCltMain;
//---------------------------------------------------------------------------
#endif
