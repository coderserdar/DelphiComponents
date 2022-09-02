//---------------------------------------------------------------------------
#ifndef ExSvMainH
#define ExSvMainH
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
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblServerName;
  TPanel *pnlMain;
  TLabel *lblServerLog;
  TMemo *memChat;
  TEdit *efSrvName;
  TButton *pbSrvCtrl;
  TffLegacyTransport *tpMain;
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall pbSrvCtrlClick(TObject *Sender);
private:	// User declarations
  TffChatSrvHandler* FChatHandler;

public:		// User declarations
    __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
