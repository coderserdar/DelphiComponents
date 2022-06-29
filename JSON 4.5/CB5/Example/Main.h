//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include "winjson.hpp"
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
        TTreeView *TreeView;
        TOpenDialog *OpenDialog;
        TPanel *Panel;
        TButton *ButtonOpenFile;
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
