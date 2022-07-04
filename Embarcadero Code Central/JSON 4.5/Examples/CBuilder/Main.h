//---------------------------------------------------------------------------
#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "WinJson.hpp"
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel;
	TButton *ButtonOpenFile;
	TTreeView *TreeView;
	TOpenDialog *OpenDialog;
	void __fastcall ButtonOpenFileClick(TObject *Sender);
private:	// User declarations
	void __fastcall Show(TTreeNode *parent, const String prefix, TJson *jsonItem);
public:		// User declarations
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
