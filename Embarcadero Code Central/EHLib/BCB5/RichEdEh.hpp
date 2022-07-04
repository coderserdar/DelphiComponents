// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'RichEdEh.pas' rev: 5.00

#ifndef RichEdEhHPP
#define RichEdEhHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <PrnDbgeh.hpp>	// Pascal unit
#include <ToolWin.hpp>	// Pascal unit
#include <Clipbrd.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <DsgnIntf.hpp>	// Pascal unit
#include <EhLibVCL.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Richedeh
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TRichStrEditDlgEh;
class PASCALIMPLEMENTATION TRichStrEditDlgEh : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Menus::TMainMenu* MainMenu;
	Menus::TMenuItem* FileNewItem;
	Menus::TMenuItem* FileOpenItem;
	Menus::TMenuItem* FileSaveItem;
	Menus::TMenuItem* FileSaveAsItem;
	Menus::TMenuItem* FilePrintItem;
	Menus::TMenuItem* FileExitItem;
	Menus::TMenuItem* EditUndoItem;
	Menus::TMenuItem* EditCutItem;
	Menus::TMenuItem* EditCopyItem;
	Menus::TMenuItem* EditPasteItem;
	Dialogs::TOpenDialog* OpenDialog;
	Dialogs::TSaveDialog* SaveDialog;
	Dialogs::TPrintDialog* PrintDialog;
	Extctrls::TPanel* Ruler;
	Dialogs::TFontDialog* FontDialog1;
	Stdctrls::TLabel* FirstInd;
	Stdctrls::TLabel* LeftInd;
	Extctrls::TBevel* RulerLine;
	Stdctrls::TLabel* RightInd;
	Menus::TMenuItem* N5;
	Menus::TMenuItem* miEditFont;
	Comctrls::TRichEdit* Editor;
	Comctrls::TStatusBar* StatusBar;
	Comctrls::TToolBar* StandardToolBar;
	Comctrls::TToolButton* OpenButton;
	Comctrls::TToolButton* SaveButton;
	Comctrls::TToolButton* PrintButton;
	Comctrls::TToolButton* ToolButton5;
	Comctrls::TToolButton* UndoButton;
	Comctrls::TToolButton* CutButton;
	Comctrls::TToolButton* CopyButton;
	Comctrls::TToolButton* PasteButton;
	Comctrls::TToolButton* ToolButton10;
	Stdctrls::TComboBox* FontName;
	Stdctrls::TEdit* FontSize;
	Comctrls::TToolButton* ToolButton11;
	Comctrls::TUpDown* UpDown1;
	Comctrls::TToolButton* BoldButton;
	Comctrls::TToolButton* ItalicButton;
	Comctrls::TToolButton* UnderlineButton;
	Comctrls::TToolButton* ToolButton16;
	Comctrls::TToolButton* LeftAlign;
	Comctrls::TToolButton* CenterAlign;
	Comctrls::TToolButton* RightAlign;
	Comctrls::TToolButton* ToolButton20;
	Comctrls::TToolButton* BulletsButton;
	Controls::TImageList* ToolbarImages;
	Comctrls::TToolButton* ToolButton2;
	Extctrls::TBevel* Bevel1;
	Extctrls::TPanel* Panel1;
	Extctrls::TPanel* Panel2;
	Stdctrls::TButton* OKButton;
	Stdctrls::TButton* CancelButton;
	Comctrls::TToolButton* ToolButton1;
	Comctrls::TToolButton* tbSelectFont;
	void __fastcall SelectionChange(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	HIDESBASE void __fastcall ShowHint(System::TObject* Sender);
	void __fastcall FileOpen(System::TObject* Sender);
	void __fastcall FileSave(System::TObject* Sender);
	void __fastcall FileSaveAs(System::TObject* Sender);
	void __fastcall FilePrint(System::TObject* Sender);
	void __fastcall FileExit(System::TObject* Sender);
	void __fastcall EditUndo(System::TObject* Sender);
	void __fastcall EditCut(System::TObject* Sender);
	void __fastcall EditCopy(System::TObject* Sender);
	void __fastcall EditPaste(System::TObject* Sender);
	void __fastcall SelectFont(System::TObject* Sender);
	void __fastcall RulerResize(System::TObject* Sender);
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall FormPaint(System::TObject* Sender);
	void __fastcall BoldButtonClick(System::TObject* Sender);
	void __fastcall ItalicButtonClick(System::TObject* Sender);
	void __fastcall FontSizeChange(System::TObject* Sender);
	void __fastcall AlignButtonClick(System::TObject* Sender);
	void __fastcall FontNameChange(System::TObject* Sender);
	void __fastcall UnderlineButtonClick(System::TObject* Sender);
	void __fastcall BulletsButtonClick(System::TObject* Sender);
	void __fastcall RulerItemMouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall RulerItemMouseMove(System::TObject* Sender, Classes::TShiftState Shift, int X, int 
		Y);
	void __fastcall FirstIndMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall LeftIndMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall RightIndMouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall RichEditChange(System::TObject* Sender);
	void __fastcall EditorKeyDown(System::TObject* Sender, Word &Key, Classes::TShiftState Shift);
	
private:
	AnsiString FFileName;
	bool FUpdating;
	int FDragOfs;
	bool FDragging;
	Comctrls::TTextAttributes* __fastcall CurrText(void);
	void __fastcall GetFontNames(void);
	void __fastcall SetupRuler(void);
	void __fastcall SetEditRect(void);
	void __fastcall UpdateCursorPos(void);
	MESSAGE void __fastcall WMDropFiles(Messages::TWMDropFiles &Msg);
	void __fastcall PerformFileOpen(const AnsiString AFileName);
	void __fastcall SetModified(bool Value);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TRichStrEditDlgEh(Classes::TComponent* AOwner) : 
		Forms::TForm(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TRichStrEditDlgEh(Classes::TComponent* AOwner
		, int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TRichStrEditDlgEh(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TRichStrEditDlgEh(HWND ParentWindow) : Forms::TForm(
		ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TRichEditStringsEh;
class PASCALIMPLEMENTATION TRichEditStringsEh : public Dsgnintf::TClassProperty 
{
	typedef Dsgnintf::TClassProperty inherited;
	
public:
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall Edit(void);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TRichEditStringsEh(const Dsgnintf::_di_IFormDesigner 
		ADesigner, int APropCount) : Dsgnintf::TClassProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TRichEditStringsEh(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TRichStrEditDlgEh* RichStrEditDlgEh;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Richedeh */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Richedeh;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// RichEdEh
