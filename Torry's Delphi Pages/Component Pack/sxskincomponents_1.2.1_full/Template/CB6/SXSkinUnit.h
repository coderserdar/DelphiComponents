//$$---- Form HDR ----
//---------------------------------------------------------------------------

#ifndef SXSkinUnitH
#define SXSkinUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "SXSkinControl.hpp"
#include "SXSkinImage.hpp"
#include "SXSkinLibrary.hpp"
#include "SXSkinUtils.hpp"
#include <Menus.hpp>
#include "SXSkinButton.hpp"
#include "SXSkinForm.hpp"
#include <ComCtrls.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *Exit1;
	TMenuItem *Skins1;
	TSXSkinLibrary *SXSkinLibrary1;
        TSXSkinImage *SXSkinImage1;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label4;
        TLabel *Label6;
        TSXSkinButton *SXSkinButton1;
        TSXSkinForm *SXSkinForm1;
        TToolBar *ToolBar1;
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
public:		// User declarations
	TStringList	*SkinNames;
	TStringList *SkinPaths;
	void __fastcall SkinChange(TObject *Sender);
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
