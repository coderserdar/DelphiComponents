// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'QBuilder.pas' rev: 4.00

#ifndef QBuilderHPP
#define QBuilderHPP

#pragma delphiheader begin
#pragma option push -w-
#include <ImgList.hpp>	// Pascal unit
#include <ExtDlgs.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <CheckLst.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <ToolWin.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Qbuilder
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TOQBbutton { bSelectDBDialog, bOpenDialog, bSaveDialog, bRunQuery, bSaveResultsDialog };
#pragma option pop

typedef Set<TOQBbutton, bSelectDBDialog, bSaveResultsDialog>  TOQBbuttons;

class DELPHICLASS TOQBuilderDialog;
class DELPHICLASS TOQBEngine;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBEngine : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	AnsiString FDatabaseName;
	AnsiString FUserName;
	AnsiString FPassword;
	bool FShowSystemTables;
	Classes::TStringList* FTableList;
	Classes::TStringList* FAliasList;
	Classes::TStringList* FFieldList;
	Classes::TStringList* FSQL;
	Classes::TStringList* FSQLcolumns;
	Classes::TStringList* FSQLcolumns_table;
	Classes::TStringList* FSQLcolumns_func;
	Classes::TStringList* FSQLfrom;
	Classes::TStringList* FSQLwhere;
	Classes::TStringList* FSQLgroupby;
	Classes::TStringList* FSQLorderby;
	bool FUseTableAliases;
	void __fastcall SetShowSystemTables(const bool Value);
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	virtual void __fastcall SetDatabaseName(const AnsiString Value);
	virtual void __fastcall SetUserName(const AnsiString Value);
	virtual void __fastcall SetPassword(const AnsiString Value);
	virtual void __fastcall SetQuerySQL(AnsiString Value) = 0 ;
	virtual void __fastcall GenerateAliases(void);
	virtual void __fastcall ReadTableList(void) = 0 ;
	virtual void __fastcall ReadFieldList(AnsiString ATableName) = 0 ;
	
public:
	TOQBuilderDialog* FOQBDialog;
	AnsiString TableName;
	__fastcall virtual TOQBEngine(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBEngine(void);
	virtual bool __fastcall SelectDatabase(void) = 0 ;
	virtual AnsiString __fastcall GenerateSQL();
	virtual void __fastcall ClearQuerySQL(void) = 0 ;
	virtual Db::TDataSet* __fastcall ResultQuery(void) = 0 ;
	virtual void __fastcall OpenResultQuery(void) = 0 ;
	virtual void __fastcall CloseResultQuery(void) = 0 ;
	virtual void __fastcall SaveResultQueryData(void) = 0 ;
	__property Classes::TStringList* TableList = {read=FTableList};
	__property Classes::TStringList* AliasList = {read=FAliasList};
	__property Classes::TStringList* FieldList = {read=FFieldList};
	__property Classes::TStringList* SQL = {read=FSQL};
	__property Classes::TStringList* SQLcolumns = {read=FSQLcolumns};
	__property Classes::TStringList* SQLcolumns_table = {read=FSQLcolumns_table};
	__property Classes::TStringList* SQLcolumns_func = {read=FSQLcolumns_func};
	__property Classes::TStringList* SQLfrom = {read=FSQLfrom};
	__property Classes::TStringList* SQLwhere = {read=FSQLwhere};
	__property Classes::TStringList* SQLgroupby = {read=FSQLgroupby};
	__property Classes::TStringList* SQLorderby = {read=FSQLorderby};
	__property AnsiString UserName = {read=FUserName, write=SetUserName};
	__property AnsiString Password = {read=FPassword, write=SetPassword};
	
__published:
	__property AnsiString DatabaseName = {read=FDatabaseName, write=SetDatabaseName};
	__property bool ShowSystemTables = {read=FShowSystemTables, write=SetShowSystemTables, default=0};
	__property bool UseTableAliases = {read=FUseTableAliases, write=FUseTableAliases, default=1};
};

#pragma pack(pop)

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBuilderDialog : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	AnsiString FDatabase;
	bool FSystemTables;
	Forms::TForm* FOQBForm;
	Classes::TStrings* FSQL;
	TOQBEngine* FOQBEngine;
	TOQBbuttons FShowButtons;
	void __fastcall SetOQBEngine(const TOQBEngine* Value);
	void __fastcall SetShowButtons(const TOQBbuttons Value);
	
protected:
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation
		);
	
public:
	__fastcall virtual TOQBuilderDialog(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBuilderDialog(void);
	virtual bool __fastcall Execute(void);
	__property Classes::TStrings* SQL = {read=FSQL};
	__property bool SystemTables = {read=FSystemTables, write=FSystemTables, default=0};
	__property AnsiString Database = {read=FDatabase, write=FDatabase};
	
__published:
	__property TOQBEngine* OQBEngine = {read=FOQBEngine, write=SetOQBEngine};
	__property TOQBbuttons ShowButtons = {read=FShowButtons, write=SetShowButtons, default=31};
};

#pragma pack(pop)

typedef int TArr[1];

typedef int *PArr;

class DELPHICLASS TOQBLbx;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBLbx : public Checklst::TCheckListBox 
{
	typedef Checklst::TCheckListBox inherited;
	
private:
	int *FArrBold;
	bool FLoading;
	HIDESBASE MESSAGE void __fastcall CNDrawItem(Messages::TWMDrawItem &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &Message);
	int __fastcall GetCheckW(void);
	void __fastcall AllocArrBold(void);
	void __fastcall SelectItemBold(int Item);
	void __fastcall UnSelectItemBold(int Item);
	int __fastcall GetItemY(int Item);
	
public:
	__fastcall virtual TOQBLbx(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBLbx(void);
	DYNAMIC void __fastcall ClickCheck(void);
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBLbx(HWND ParentWindow) : Checklst::TCheckListBox(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TOQBTable;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBTable : public Extctrls::TPanel 
{
	typedef Extctrls::TPanel inherited;
	
private:
	HDC ScreenDC;
	int OldX;
	int OldY;
	int OldLeft;
	int OldTop;
	HRGN ClipRgn;
	Windows::TRect ClipRect;
	Windows::TRect MoveRect;
	bool Moving;
	Buttons::TSpeedButton* FCloseBtn;
	Buttons::TSpeedButton* FUnlinkBtn;
	TOQBLbx* FLbx;
	AnsiString FTableName;
	AnsiString FTableAlias;
	Menus::TPopupMenu* PopMenu;
	HIDESBASE MESSAGE void __fastcall WMRButtonDown(Messages::TWMMouse &Message);
	bool __fastcall Activate(const AnsiString ATableName, int X, int Y);
	int __fastcall GetRowY(int FldN);
	void __fastcall _CloseBtn(System::TObject* Sender);
	void __fastcall _UnlinkBtn(System::TObject* Sender);
	void __fastcall _SelectAll(System::TObject* Sender);
	void __fastcall _UnSelectAll(System::TObject* Sender);
	void __fastcall _DragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, Controls::TDragState 
		State, bool &Accept);
	void __fastcall _DragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	
protected:
	virtual void __fastcall SetParent(Controls::TWinControl* AParent);
	DYNAMIC void __fastcall MouseDown(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, 
		int Y);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(Controls::TMouseButton Button, Classes::TShiftState Shift, int X, int 
		Y);
	__property Align ;
	
public:
	__fastcall virtual TOQBTable(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBTable(void);
	virtual void __fastcall Paint(void);
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBTable(HWND ParentWindow) : Extctrls::TPanel(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TOQBLink;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBLink : public Extctrls::TShape 
{
	typedef Extctrls::TShape inherited;
	
private:
	TOQBTable* Tbl1;
	TOQBTable* Tbl2;
	int FldN1;
	int FldN2;
	AnsiString FldNam1;
	AnsiString FldNam2;
	int FLinkOpt;
	int FLinkType;
	Byte LnkX;
	Byte LnkY;
	HRGN Rgn;
	Menus::TPopupMenu* PopMenu;
	void __fastcall _Click(int X, int Y);
	HIDESBASE MESSAGE void __fastcall CMHitTest(Messages::TWMNCHitTest &Message);
	Controls::TControl* __fastcall ControlAtPos(const Windows::TPoint &Pos);
	
public:
	__fastcall virtual TOQBLink(Classes::TComponent* AOwner);
	__fastcall virtual ~TOQBLink(void);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	virtual void __fastcall Paint(void);
};

#pragma pack(pop)

class DELPHICLASS TOQBArea;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBArea : public Forms::TScrollBox 
{
	typedef Forms::TScrollBox inherited;
	
public:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	void __fastcall SetOptions(System::TObject* Sender);
	void __fastcall InsertTable(int X, int Y);
	TOQBLink* __fastcall InsertLink(TOQBTable* _tbl1, TOQBTable* _tbl2, int _fldN1, int _fldN2);
	TOQBTable* __fastcall FindTable(AnsiString TableName);
	bool __fastcall FindLink(TOQBLink* Link);
	bool __fastcall FindOtherLink(TOQBLink* Link, TOQBTable* Tbl, int FldN);
	void __fastcall ReboundLink(TOQBLink* Link);
	void __fastcall ReboundLinks4Table(TOQBTable* ATable);
	void __fastcall Unlink(System::TObject* Sender);
	void __fastcall UnlinkTable(TOQBTable* ATable);
	void __fastcall _DragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, Controls::TDragState 
		State, bool &Accept);
	void __fastcall _DragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
public:
	#pragma option push -w-inl
	/* TScrollBox.Create */ inline __fastcall virtual TOQBArea(Classes::TComponent* AOwner) : Forms::TScrollBox(
		AOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TScrollingWinControl.Destroy */ inline __fastcall virtual ~TOQBArea(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBArea(HWND ParentWindow) : Forms::TScrollBox(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TOQBGrid;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBGrid : public Grids::TStringGrid 
{
	typedef Grids::TStringGrid inherited;
	
public:
	int CurrCol;
	bool IsEmpty;
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall WndProc(Messages::TMessage &Message);
	int __fastcall MaxSW(AnsiString s1, AnsiString s2);
	void __fastcall InsertDefault(int aCol);
	HIDESBASE void __fastcall Insert(int aCol, AnsiString aField, AnsiString aTable);
	int __fastcall FindColumn(AnsiString sCol);
	bool __fastcall FindSameColumn(int aCol);
	void __fastcall RemoveColumn(int aCol);
	void __fastcall RemoveColumn4Tbl(AnsiString Tbl);
	void __fastcall ClickCell(int X, int Y);
	virtual bool __fastcall SelectCell(int ACol, int ARow);
	void __fastcall _DragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, Controls::TDragState 
		State, bool &Accept);
	void __fastcall _DragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
public:
	#pragma option push -w-inl
	/* TStringGrid.Create */ inline __fastcall virtual TOQBGrid(Classes::TComponent* AOwner) : Grids::TStringGrid(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TStringGrid.Destroy */ inline __fastcall virtual ~TOQBGrid(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBGrid(HWND ParentWindow) : Grids::TStringGrid(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TOQBForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TOQBForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Extctrls::TPanel* ButtonsPanel;
	Extctrls::TPanel* QBPanel;
	Comctrls::TPageControl* Pages;
	Comctrls::TTabSheet* TabColumns;
	Stdctrls::TListBox* QBTables;
	Extctrls::TSplitter* VSplitter;
	Menus::TPopupMenu* mnuTbl;
	Menus::TMenuItem* Remove1;
	Menus::TPopupMenu* mnuFunc;
	Menus::TMenuItem* Nofunction1;
	Menus::TMenuItem* N1;
	Menus::TMenuItem* Average1;
	Menus::TMenuItem* Count1;
	Menus::TMenuItem* Minimum1;
	Menus::TMenuItem* Maximum1;
	Menus::TMenuItem* Sum1;
	Menus::TPopupMenu* mnuGroup;
	Menus::TMenuItem* Group1;
	Menus::TPopupMenu* mnuSort;
	Menus::TMenuItem* Sort1;
	Menus::TMenuItem* N2;
	Menus::TMenuItem* Ascending1;
	Menus::TMenuItem* Descending1;
	Menus::TPopupMenu* mnuShow;
	Menus::TMenuItem* Show1;
	Stdctrls::TButton* Button1;
	Stdctrls::TButton* Button2;
	Extctrls::TSplitter* HSplitter;
	Comctrls::TTabSheet* TabSQL;
	Stdctrls::TMemo* MemoSQL;
	Comctrls::TTabSheet* TabResults;
	Dbgrids::TDBGrid* ResDBGrid;
	Db::TDataSource* ResDataSource;
	Comctrls::TToolBar* QBBar;
	Comctrls::TToolButton* btnNew;
	Comctrls::TToolButton* btnOpen;
	Comctrls::TToolButton* btnSave;
	Comctrls::TToolButton* ToolButton1;
	Comctrls::TToolButton* btnTables;
	Controls::TImageList* ToolImages;
	Comctrls::TToolButton* btnPages;
	Comctrls::TToolButton* ToolButton2;
	Dialogs::TSaveDialog* DlgSave;
	Dialogs::TOpenDialog* DlgOpen;
	Comctrls::TToolButton* btnDB;
	Comctrls::TToolButton* btnSQL;
	Comctrls::TToolButton* btnResults;
	Comctrls::TToolButton* ToolButton3;
	Comctrls::TToolButton* btnAbout;
	Comctrls::TToolButton* btnSaveResults;
	void __fastcall mnuFunctionClick(System::TObject* Sender);
	void __fastcall mnuGroupClick(System::TObject* Sender);
	void __fastcall mnuRemoveClick(System::TObject* Sender);
	void __fastcall mnuShowClick(System::TObject* Sender);
	void __fastcall mnuSortClick(System::TObject* Sender);
	void __fastcall btnNewClick(System::TObject* Sender);
	void __fastcall btnOpenClick(System::TObject* Sender);
	void __fastcall btnSaveClick(System::TObject* Sender);
	void __fastcall btnTablesClick(System::TObject* Sender);
	void __fastcall btnPagesClick(System::TObject* Sender);
	void __fastcall btnDBClick(System::TObject* Sender);
	void __fastcall btnSQLClick(System::TObject* Sender);
	void __fastcall btnResultsClick(System::TObject* Sender);
	void __fastcall btnAboutClick(System::TObject* Sender);
	void __fastcall btnSaveResultsClick(System::TObject* Sender);
	
protected:
	TOQBuilderDialog* QBDialog;
	TOQBArea* QBArea;
	TOQBGrid* QBGrid;
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	void __fastcall ClearAll(void);
	void __fastcall OpenDatabase(void);
	void __fastcall SelectDatabase(void);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TOQBForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TOQBForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TOQBForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TOQBForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Qbuilder */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Qbuilder;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// QBuilder
