{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukwCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, SysUtils, Messages, CommCtrl, Classes, Graphics, Controls, ComCtrls,
	StdCtrls, uksyTypes, uksyUtils, uksyClasses;

type

	EKWCtrls = class( EKWinAPI );

{##FNS##}

{$IFNDEF EXCLUDED_CLASSES}

{ TKPageControl }

	EKTabSheet = class( EKWCtrls );

{$IFNDEF DELPHI4}

	TKCustomImgTabControl = class( TCustomTabControl )
	private
		FIcons: TImageList;

	protected
		procedure CreateWnd; override;
		property Icons: TImageList
						 read FIcons write FIcons;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

	TKPageControl = class;

{ TKTabSheet }

	TKTabSheet = class( TWinControl )
	private
		FBitmap: TBitmap;
		FTabVisible: Boolean;
		FTabShowing: Boolean;
		FPageControl: TKPageControl;

		function GetPageIndex: Integer;
		function GetTabIndex: Integer;
		procedure SetBitmap( Value: TBitmap );
		procedure SetImgPageControl( APageControl: TKPageControl );
		procedure SetPageIndex( Value: Integer );
		procedure SetTabShowing( Value: Boolean );
		procedure SetTabVisible( Value: Boolean );
		procedure UpdateTab;
		procedure UpdateTabShowing;
		procedure CMTextChanged( var Message: TMessage ); message CM_TEXTCHANGED;

	protected
		procedure Loaded; override;

	protected
		procedure ReadState( Reader: TReader ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property PageControl: TKPageControl read FPageControl write SeTImgPageControl;
		property TabIndex: Integer read GetTabIndex;

	published
		property Bitmap: TBitmap
						 read FBitmap write SetBitmap;
		property Caption;
		property Enabled;
		property Font;
		property Height stored False;
		property Left stored False;
		property PageIndex: Integer
						 read GetPageIndex write SetPageIndex stored False;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabVisible: Boolean
						 read FTabVisible write SetTabVisible default True;
		property Top stored False;
		property Visible stored False;
		property Width stored False;

		property OnDragDrop;
		property OnDragOver;
		property OnEnter;
		property OnExit;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;

	end;

	TKPageControl = class( TKCustomImgTabControl )
	private
		FPages: TList;
		FActivePage: TKTabSheet;

		procedure ChangeActivePage( Page: TKTabSheet );
		procedure DeleteTab( Page: TKTabSheet );
		function GetPage( Index: Integer ): TKTabSheet;
		function GetPageCount: Integer;
		procedure InsertPage( Page: TKTabSheet );
		procedure InsertTab( Page: TKTabSheet );
		procedure MoveTab( CurIndex, NewIndex: Integer );
		procedure RemovePage( Page: TKTabSheet );
		procedure SetActivePage( Page: TKTabSheet );
		procedure UpdateTab( Page: TKTabSheet );
		procedure UpdateActivePage;

		procedure CMDialogChar( var Msg: TCMDialogChar );
							message CM_DIALOGCHAR;
		procedure CMDesignHitTest( var Message: TCMDesignHitTest );
							message CM_DESIGNHITTEST;
		procedure CMDialogKey( var Message: TCMDialogKey );
							message CM_DIALOGKEY;

	protected
		procedure Change; override;
		procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;
		procedure SetChildOrder( Child: TComponent; Order: Integer ); override;
		procedure ShowControl( AControl: TControl ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function FindNextPage( CurPage: TKTabSheet; GoForward, CheckTabVisible: Boolean ): TKTabSheet;
		procedure SelectNextPage( GoForward: Boolean );

		property PageCount: Integer
						 read GetPageCount;
		property Pages[Index: Integer]: TKTabSheet
						 read GetPage;

		property Handle;

	published
		property ActivePage: TKTabSheet
						 read FActivePage write SetActivePage;
		property Align;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property HotTrack;
		property MultiLine;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabHeight;
		property TabOrder;
		property TabStop;
		property TabWidth;
		property Visible;

		property OnChange;
		property OnChanging;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	end;

{$ELSE}
	TKTabSheet = class( TTabSheet );
	TKPageControl = class( TPageControl );
{$ENDIF}

{$ENDIF}

{##FNS##}

{ TKDirTree }

	TKAddFileEvent = procedure( const FileName: string; var AddFile: Boolean ) of object;

	TKDirectoryAttribute = ( dtReadonly, dtHidden, dtSystem, dtArchive, dtNormal, dtAll );
	TKDirectoryAttributes = set of TKDirectoryAttribute;

	TKDirRec = record
		Attr: Integer;
		Name: TFilename;
		Handle: Cardinal;
		Data: TWin32FindData;
	end;

	PKDirName = ^TKDirName;
	TKDirName = record
		Name: string;
		Expanded: Boolean;
	end;

	TKDirTree = class( TCustomTreeView )
	private
	  FDirRecList: TList;
		FDirectory: string;
		FOpenIcon: Integer;
		FIList: TImageList;
		FTDirs: TStringList;
		FDefaultIcon: Integer;
		FAllowChange: Boolean;
		FAllowNetwork: Boolean;
		FOnAddFile: TKAddFileEvent;
		FGoBellowRecycleBin: Boolean;
		FDirectoryAttributes: TKDirectoryAttributes;
		FActiveDir: array[TKUpperAlphaEnum] of string;

		procedure InitActiveDirs;
		function  GetDrive: Char;
		function  GetDropDir: string;
		function  GetDirectory: string;
		procedure SetDrive ( Value: Char );
		procedure SetDirectory ( const Value: string );
		procedure SetDirectoryAttributes ( Value: TKDirectoryAttributes );
		procedure SetAllowNetwork( Value: Boolean );
		procedure SetGoBellowRecycleBin( Value: Boolean );
		function  GetActiveDir( Index: TKUpperAlphaEnum ): string;
		procedure SetActiveDir( Index: TKUpperAlphaEnum; const Value: string );
		function  GetChild( ANode: TTreeNode; const Path: string ): TTreeNode;
		procedure AddDir( ANode: TTreeNode; const AName: string; Value: Boolean );

	protected
		procedure Loaded; override;
		procedure CreateWnd; override;
		procedure SetupDrives; virtual;
		procedure GetButton( var ANode: TTreeNode ); dynamic;
		function  CheckMask( Attr: Integer ): Boolean;
		function  CanExpand( ANode: TTreeNode ): Boolean; override;

		function  Allowed( const Value: string ): Boolean;
		procedure Change( ANode: TTreeNode ); override;
		function  FindNextDir( var Dir: TKDirRec ): Integer;
		function  FindFirstDir( Path: TFileName; var Dir: TKDirRec ): Integer;

		property Items stored false;
		property ActiveDir[Index: TKUpperAlphaEnum]: string
						 read GetActiveDir write SetActiveDir;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Reload;
		procedure FullExpand;
		procedure UpdateButtons( var ANode: TTreeNode );
		procedure UpdateFolders( var ANode: TTreeNode );

		property Drive: Char
						 read GetDrive write SetDrive;
		property DropDirectory: string
						 read GetDropDir;
		property ShellIcons: TImageList
						 read FIList;

	published
		property Align;
		property BorderStyle;
    property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property Images;
		property Indent;
		// property Items;
		property ParentColor default False;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ReadOnly;
		property RightClickSelect;
		property ShowButtons;
		property ShowHint;
		property ShowLines;
		property ShowRoot;
		property SortType;
		property StateImages;
		property TabOrder;
		property TabStop default True;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property AutoExpand;
		property BiDiMode;
		property BorderWidth;
		property ChangeDelay;
		property Constraints;
		property DragKind;
		property HotTrack;
		property ParentBiDiMode;
		property RowSelect;
		property ToolTips;
	{$ENDIF}

		property OnChange;
		property OnChanging;
		property OnClick;
		property OnCollapsing;
		property OnCollapsed;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnStartDrag;
		property OnEndDrag;
		property OnEdited;
		property OnEditing;
		property OnEnter;
		property OnExit;
		property OnExpanding;
		property OnExpanded;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;

	{$IFDEF DELPHI4}
		property OnEndDock;
		property OnStartDock;
	{$ENDIF}

		property AllowNetwork: Boolean
						 read FAllowNetwork write SetAllowNetwork;
		property Directory: string
						 read GetDirectory write SetDirectory;
		property DirectoryAttributes: TKDirectoryAttributes
						 read FDirectoryAttributes write SetDirectoryAttributes;
		property GoBelowRecycleBin: Boolean
						 read FGoBellowRecycleBin write SetGoBellowRecycleBin;
		property OnAddFile: TKAddFileEvent
						 read FOnAddFile write FOnAddFile;

	end;

{ TKTreeView }

	PNMCustomDraw = ^TNMCustomDraw;
	TNMCustomDraw = Record
		hdr         : TNMHdr;
		dwDrawStage : DWORD;  // see above
		hdc         : HDC;    // drawing area for the specific item
		rc          : TRect;  // the enclosing rect for the specific item
		dwItemSpec  : DWORD;  // item number in the control
		uItemState  : UINT;   // see above
		lItemlParam : LPARAM; // item specific data
	end;

	TKPaintItemEvent = procedure( Sender: TObject; Node: TTreeNode; DC: HDC ) of object;

  TKFindNodeOption = ( fnoFirst, fnoLast );

	TKTreeView = class( TCustomTreeView )
	private
		FAlwaysEditted: Boolean;
		FOnPaintItem: TKPaintItemEvent;
		fInternalEditNode : TTreeNode;
		fEditingSelected : Boolean;

		procedure CNNotify( var Msg: TWMNotify );
							message CN_NOTIFY;

	protected
		procedure DoPaintItem( cd: TNMCustomDraw ); dynamic;
		procedure Edit(const Item: TTVItem); override;
		function CanEdit(Node: TTreeNode): Boolean; override;

	public
		constructor Create( AOwner: TComponent ); override;
		function FindNodeText( StartIndex: Integer; Text: string; Partial, Inclusive,
			Wrap: Boolean ): TTreeNode;
		function FindData( StartIndex: Integer; Value: Pointer; Inclusive,
			Wrap: Boolean ): TTreeNode;
		function FindNoteAtLevel( Level: Integer; Option: TKFindNodeOption ): TTreeNode;

		property EditingSelected: Boolean
						 read fEditingSelected;

	published
		property Align;
		property BorderStyle;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property HideSelection;
		property Images;
		property Indent;
		property Items;
		property ParentColor default False;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ReadOnly;
		property RightClickSelect;
		property ShowButtons;
		property ShowHint;
		property ShowLines;
		property ShowRoot;
		property SortType;
		property StateImages;
		property TabOrder;
		property TabStop default True;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property AutoExpand;
		property BiDiMode;
		property BorderWidth;
		property ChangeDelay;
		property Constraints;
		property DragKind;
		property HotTrack;
		property ParentBiDiMode;
		property RowSelect;
		property ToolTips;
	{$ENDIF}

		property OnChange;
		property OnChanging;
		property OnClick;
		property OnCollapsing;
		property OnCollapsed;
		property OnCompare;
		property OnDblClick;
		property OnDeletion;
		property OnDragDrop;
		property OnDragOver;
		property OnEdited;
		property OnEditing;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnExpanding;
		property OnExpanded;
		property OnGetImageIndex;
		property OnGetSelectedIndex;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnCustomDraw;
		property OnCustomDrawItem;
		property OnEndDock;
		property OnStartDock;
	{$ENDIF}

		property AlwaysEditted: Boolean
						 read FAlwaysEditted write FAlwaysEditted default False;
		property OnPaintItem: TKPaintItemEvent
						 read FOnPaintItem write FOnPaintItem;

	end;

{ TKShellComboBox }

	TKShellComboBoxStyle = ( scsDrives, scsExeTypeInfo );

	TKShellComboBox = class( TCustomComboBox )
	private
		FMinHeight: Integer;
		FListBuilt: Boolean;
		FDriveList: TStrings;
		FAddedPaths: TStrings;
		FAutoRefresh: Boolean;
		FImageList: TImageList;
		FOnAddPath: TNotifyEvent;
		FShellComboBoxStyle: TKShellComboBoxStyle;

		procedure ReadData( Reader: TReader );
		procedure WriteData( Writer: TWriter );

		function GetMinHeight: Integer;
		procedure SetAutoRefresh( Value: Boolean );
		procedure SetShellComboBoxStyle( Value: TKShellComboBoxStyle );

		procedure AdjustAddedPaths;

{$HINTS OFF}
		property Style;       { csOwnerDrawVariable }
		property Items;
		property ItemHeight;  { FImageList.Height + 4 }
		property Sorted;      { False }
		property Text;
{$HINTS ON}

	protected
		procedure Loaded; override;
		procedure CreateWnd; override;
		procedure DefineProperties( Filer: TFiler ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure MeasureItem( Index: Integer; var Height: Integer ); override;
		procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

		procedure DoAddPath; dynamic;
		procedure BuildShellDrivesList; dynamic;
		procedure BuildShellExeTypeInfoList; dynamic;

{
		property AddedPaths: TStrings
						 read FAddedPaths;
}

	{$IFDEF DELPHI4}
	public
	{$ENDIF}
		procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Clear( ClearAll: Boolean );
		procedure RefreshDrives;
		function AddPath( const Path: string ): Integer;
		function IndexOfPath( const Path: string ): Integer;
		procedure DeletePath( Index: Integer );
		procedure RemovePath( const Path: string );

		property Drives: TStrings
						 read FDriveList;

	published
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property DropDownCount;
		property Enabled;
		property Font;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property Constraints;
		property DragKind;
		property ParentBiDiMode;
	{$ENDIF}

		property OnChange;
		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnDropDown;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnEndDock;
		property OnStartDock;
	{$ENDIF}

		property AutoRefresh: Boolean
						 read FAutoRefresh write SetAutoRefresh default True;
		property ShellStyle: TKShellComboBoxStyle
						 read FShellComboBoxStyle write SetShellComboBoxStyle default scsDrives;
		property OnAddPath: TNotifyEvent
						 read FOnAddPath write FOnAddPath;

{ Only available for WinNT }
		property OnDrawItem;
		property OnMeasureItem;

	end;

{ TKShellListBox }

	TKShellListBoxStyle = ( slsDrives, slsExeTypeInfo );

	TKShellListBox = class( TCustomListBox )
	private
		FListBuilt: Boolean;
		FDriveList: TStrings;
		FAddedPaths: TStrings;
		FAutoRefresh: Boolean;
		FImageList: TImageList;
    FOnAddPath: TNotifyEvent;
		FShellListBoxStyle: TKShellListBoxStyle;

		procedure ReadData( Reader: TReader );
		procedure WriteData( Writer: TWriter );

		procedure SetAutoRefresh( Value: Boolean );
		procedure SetShellListBoxStyle( Value: TKShellListBoxStyle );

		procedure AdjustAddedPaths;

{$HINTS OFF}
		property Style;       { lbOwnerDrawVariable }
		property Items;
		property ItemHeight;  { FImageList.Height + 4 }
		property Sorted;      { False }
		property Text;
{$HINTS ON}

	protected
		procedure Loaded; override;
		procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure MeasureItem( Index: Integer; var Height: Integer ); override;
		procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

		procedure DoAddPath; dynamic;
		procedure BuildShellDrivesList; dynamic;
		procedure BuildShellExeTypeInfoList; dynamic;

{
		property AddedPaths: TStrings
						 read FAddedPaths;
}						 
						 
	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Clear( ClearAll: Boolean );
		procedure RefreshDrives;
		function AddPath( const Path: string ): Integer;
		function IndexOfPath( const Path: string ): Integer;
		procedure DeletePath( Index: Integer );
		procedure RemovePath( const Path: string );

		property Drives: TStrings
						 read FDriveList;

	published
    property Align;
		property BorderStyle;
		property Color;
		property Columns;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
		property ExtendedSelect;
		property Font;
		property ImeMode;
		property ImeName;
		property IntegralHeight;
		property MultiSelect;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property TabWidth;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property Constraints;
		property DragKind;
		property ParentBiDiMode;
	{$ENDIF}

		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnEndDock;
		property OnStartDock;
	{$ENDIF}

		property AutoRefresh: Boolean
						 read FAutoRefresh write SetAutoRefresh default True;
		property ShellStyle: TKShellListBoxStyle
						 read FShellListBoxStyle write SetShellListBoxStyle default slsDrives;
		property OnAddPath: TNotifyEvent
						 read FOnAddPath write FOnAddPath;

{ Only available for WinNT }
		property OnDrawItem;
		property OnMeasureItem;

	end;

implementation

uses
	Forms, ShellAPI, {$IFDEF DELPHI4} ImgList, {$ENDIF} uksyConsts, ukwResStr,
	ukwUtils;

{$IFNDEF EXCLUDED_CLASSES}

{$IFNDEF DELPHI4}

{-------------------------------- TKPageControl --------------------------------}

var
	ClearBitmap: TBitmap;

procedure TabControlError;
begin
	RaiseException( EKTabSheet, sErrTSUpdateError );
end;

procedure EmptyBitmap( ABitmap: TBitmap );
var
	bmp: TBitmap;
	i, j: Integer;
begin
	bmp := TBitmap.Create;
	try
		bmp.Width := 16;
		bmp.Height := 16;
		for i := 0 to 15 do
			for j := 0 to 15 do
				bmp.Canvas.Pixels[j,i] := clWhite;
		ABitmap.Assign( bmp );
	finally
		bmp.Free;
	end;
end;

{ TKCustomImgTabControl }

constructor TKCustomImgTabControl.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FIcons := TImageList.Create( Self );
end;

destructor TKCustomImgTabControl.Destroy;
begin
	FIcons.Free;
	inherited Destroy;
end;

procedure TKCustomImgTabControl.CreateWnd;
begin
	inherited CreateWnd;
	SendMessage( Handle, TCM_SETIMAGELIST, 0, FIcons.Handle );
end;

{ TKTabSheet }

constructor TKTabSheet.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	Align := alClient;
	ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
	Visible := False;
	FTabVisible := True;
	FBitmap := Graphics.TBitmap.Create;
	ClearBitmap := Graphics.TBitmap.Create;
	EmptyBitmap( FBitmap );
	EmptyBitmap( ClearBitmap );
end;

destructor TKTabSheet.Destroy;
begin
	FBitmap.Free;
	if CheckObject( FPageControl ) then
	begin
		FPageControl.Icons.Delete( PageIndex );
		FPageControl.RemovePage( Self );
	end;
	inherited Destroy;
end;

procedure TKTabSheet.Loaded;
begin
	inherited Loaded;
	SetBitmap( FBitmap );
end;

function TKTabSheet.GetPageIndex: Integer;
begin
	if CheckObject( FPageControl ) then
		Result := FPageControl.FPages.IndexOf( Self )
	else
	  Result := - 1;
end;

function TKTabSheet.GetTabIndex: Integer;
var
	i: Integer;
begin
	Result := 0;
	if ( not FTabShowing ) then
		Dec( Result )
	else
		for i := 0 to PageIndex - 1 do
			if TKTabSheet( FPageControl.FPages[I] ).FTabShowing then
				Inc( Result );
end;

procedure TKTabSheet.ReadState( Reader: TReader );
begin
	inherited ReadState( Reader );
	if CheckObjectClass( Reader.Parent, TKPageControl ) then
		PageControl := TKPageControl( Reader.Parent );
end;

procedure TKTabSheet.SetImgPageControl( APageControl: TKPageControl );
begin
	if ( FPageControl <> APageControl ) then
	begin
		if CheckObject( FPageControl ) then
			FPageControl.RemovePage( Self );
		Parent := APageControl;
    if CheckObject( APageControl ) then
		  APageControl.InsertPage( Self );
		FPageControl.Icons.InsertMasked( PageIndex, ClearBitmap, clWhite );
		UpdateTab;
	end;
end;

procedure TKTabSheet.SetPageIndex( Value: Integer );
var
	i: Integer;
begin
	if CheckObject( FPageControl ) then
	begin
		i := TabIndex;
		FPageControl.FPages.Move( PageIndex, Value );
		if ( i >= 0 ) then
		  FPageControl.MoveTab( I, TabIndex );
		UpdateTab;
	end;
end;

procedure TKTabSheet.SetBitmap( Value: TBitmap );
var
	i,
	j: Byte;
	bmp: TBitmap;
	clMask: TColor;
begin
	bmp := TBitmap.Create;
	try
		bmp.Assign( Value );
		FBitmap.Width := 16;
		FBitmap.Height := 16;
		for i := 0 to 15 do
			for j := 0 to 15 do
				FBitmap.Canvas.Pixels[i,j] := bmp.Canvas.Pixels[i,j];
		clMask := bmp.Canvas.Pixels[0, 0];
		FPageControl.Icons.ReplaceMasked( PageIndex, bmp, clMask );
		UpdateTab;
	finally
		bmp.Free;
	end;
end;

procedure TKTabSheet.UpdateTab;
var
	TCItem: TTCItem;
begin
	TCItem.mask := TCIF_TEXT or TCIF_IMAGE;
	TCItem.iImage := TabIndex;
	TCItem.pszText := PChar( FPageControl.Tabs[TabIndex] );
	if ( SendMessage( FPageControl.Handle, TCM_SETITEM, TabIndex,
		Longint( @TCItem ) ) < 0 ) then
		TabControlError;
end;

procedure TKTabSheet.SetTabShowing( Value: Boolean );
begin
	if ( FTabShowing <> Value ) then
		if Value then
		begin
			FTabShowing := True;
			FPageControl.InsertTab( Self );
		end
		else
		begin
			FPageControl.DeleteTab( Self );
			FTabShowing := False;
		end;
end;

procedure TKTabSheet.SetTabVisible( Value: Boolean );
begin
	if ( FTabVisible <> Value ) then
	begin
		FTabVisible := Value;
		UpdateTabShowing;
	end;
end;

procedure TKTabSheet.UpdateTabShowing;
begin
	SetTabShowing( CheckObject( FPageControl ) and FTabVisible );
	UpdateTab;
end;

procedure TKTabSheet.CMTextChanged( var Message: TMessage );
begin
	if FTabShowing then
	  FPageControl.UpdateTab( Self );
end;

{ TKPageControl }

constructor TKPageControl.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ControlStyle := [csDoubleClicks];
	FPages := TList.Create;
end;

destructor TKPageControl.Destroy;
var
	i: Integer;
begin
	for i := 0 to FPages.Count - 1 do
	  TKTabSheet( FPages[I] ).FPageControl := nil;
	FPages.Free;
	inherited Destroy;
end;

procedure TKPageControl.Change;
var
	Form: TCustomForm;
begin
	UpdateActivePage;
	if Designing( Self ) then
	begin
		Form := GetParentForm( Self );
		if CheckObject( Form ) and CheckDesigner( Form.Designer ) then
			Form.Designer.Modified;
	end;
	inherited Change;
end;

procedure TKPageControl.ChangeActivePage( Page: TKTabSheet );
var
	ParentForm: TCustomForm;
begin
	if ( FActivePage <> Page ) then
	begin
		ParentForm := GetParentForm( Self );
		if ( CheckObjects( [ParentForm, FActivePage] ) and
			   FActivePage.ContainsControl( ParentForm.ActiveControl ) ) then
			ParentForm.ActiveControl := FActivePage;
		if CheckObject( Page ) then
		begin
			Page.BringToFront;
			Page.Visible := True;
			if ( CheckObjects( [ParentForm, FActivePage] ) and
   				( ParentForm.ActiveControl = FActivePage ) ) then
				if Page.CanFocus then
					ParentForm.ActiveControl := Page
				else
					ParentForm.ActiveControl := Self;
		end;
		if CheckObject( FActivePage ) then
		  FActivePage.Visible := False;
		FActivePage := Page;
		if ( CheckObjects( [ParentForm, FActivePage] ) and
				( ParentForm.ActiveControl = FActivePage ) ) then
 			FActivePage.SelectFirst;
	end;
end;

procedure TKPageControl.DeleteTab( Page: TKTabSheet );
begin
	Tabs.Delete( Page.TabIndex );
	UpdateActivePage;
end;

function TKPageControl.FindNextPage( CurPage: TKTabSheet;
	GoForward, CheckTabVisible: Boolean ): TKTabSheet;
var
	i,
	StartIndex: Integer;
begin
	if CheckList( FPages ) then
	begin
		StartIndex := FPages.IndexOf( CurPage );
		if ( StartIndex = -1 ) then
			if GoForward then
				StartIndex := FPages.Count - 1
			else
			  StartIndex := 0;
		i := StartIndex;
		repeat
			if GoForward then
			begin
				Inc( i );
				if ( i = FPages.Count ) then
					i := 0;
			end
			else
			begin
				if ( i = 0 ) then
					i := FPages.Count;
				Dec( i );
			end;
			Result := FPages[i];
			if ( not CheckTabVisible or Result.TabVisible ) then
			  Exit;
		until ( i = StartIndex );
	end;
	Result := nil;
end;

procedure TKPageControl.GetChildren( Proc: TGetChildProc; Root: TComponent );
var
	i: Integer;
begin
	for i := 0 to FPages.Count - 1 do
		Proc( TComponent( FPages[i] ) );
end;

function TKPageControl.GetPage( Index: Integer ): TKTabSheet;
begin
	Result := FPages[Index];
end;

function TKPageControl.GetPageCount: Integer;
begin
	Result := FPages.Count;
end;

procedure TKPageControl.InsertPage( Page: TKTabSheet );
begin
	FPages.Add( Page );
	Page.FPageControl := Self;
	Page.UpdateTabShowing;
end;

procedure TKPageControl.InsertTab( Page: TKTabSheet );
begin
	Tabs.InsertObject( Page.TabIndex, Page.Caption, Page );
	UpdateActivePage;
end;

procedure TKPageControl.MoveTab( CurIndex, NewIndex: Integer );
begin
	Tabs.Move( CurIndex, NewIndex );
end;

procedure TKPageControl.RemovePage( Page: TKTabSheet );
begin
	if ( FActivePage = Page ) then
	  SetActivePage( nil );
	Page.SetTabShowing( False );
  Page.FPageControl := nil;
	FPages.Remove( Page );
end;

procedure TKPageControl.SelectNextPage( GoForward: Boolean );
var
	Page: TKTabSheet;
begin
	Page := FindNextPage( ActivePage, GoForward, True );
	if CheckObject( Page ) and ( Page <> ActivePage ) and CanChange then
	begin
		TabIndex := Page.TabIndex;
		Change;
	end;
end;

procedure TKPageControl.SetActivePage( Page: TKTabSheet );
begin
	if CheckObject( Page ) and ( Page.PageControl <> Self ) then
	  Exit;
	ChangeActivePage( Page );
	if CheckObject( Page ) then
		TabIndex := Page.TabIndex
	else
	  TabIndex := -1;
end;

procedure TKPageControl.SetChildOrder( Child: TComponent; Order: Integer );
begin
	TKTabSheet( Child ).PageIndex := Order;
end;

procedure TKPageControl.ShowControl( AControl: TControl );
begin
	if CheckObjectClass( AControl, TKTabSheet ) and ( TKTabSheet( AControl ).PageControl = Self ) then
		SetActivePage( TKTabSheet( AControl ) );
  inherited ShowControl( AControl );
end;

procedure TKPageControl.UpdateTab( Page: TKTabSheet );
begin
  Tabs[Page.TabIndex] := Page.Caption;
end;

procedure TKPageControl.UpdateActivePage;
begin
	if ( TabIndex >= 0 ) then
	  SetActivePage( TKTabSheet( Tabs.Objects[TabIndex] ) );
end;

procedure TKPageControl.CMDialogChar( var Msg: TCMDialogChar );
var
	i: Integer;
begin
	if Enabled then
		for i := 0 to PageCount - 1 do
			with Pages[i] do
				if ( IsAccel( Msg.CharCode, Caption ) and TabVisible ) then
				begin
					Msg.Result := 1;
					ActivePage := Pages[i];
					Change;
					Exit;
				end;
	inherited;
end;

procedure TKPageControl.CMDesignHitTest( var Message: TCMDesignHitTest );
var
	HitIndex: Integer;
	HitTestInfo: TTCHitTestInfo;
begin
	HitTestInfo.pt := SmallPointToPoint( Message.Pos );
  HitIndex := SendMessage( Handle, TCM_HITTEST, 0, Longint( @HitTestInfo ) );
	if ( HitIndex >= 0 ) and ( HitIndex <> TabIndex ) then
	  Message.Result := 1;
end;

procedure TKPageControl.CMDialogKey( var Message: TCMDialogKey );
begin
	if ( Message.CharCode = VK_TAB ) and ( GetKeyState( VK_CONTROL ) < 0 ) then
	begin
		SelectNextPage( GetKeyState( VK_SHIFT ) >= 0 );
		Message.Result := 1;
	end
	else
		inherited;
end;

{$ENDIF} // for Delphi 3 only

{$ENDIF} // for internal version only

{---------------------------------- TKDirTree ----------------------------------}

{ ********** Helper Function and Procedures  ********** }

function ValidDir( dr: TKDirRec ): Boolean;
begin
	Result := ( dr.Name <> CH_DOTMARK ) and ( dr.Name <> '..' ) and
		( ( dr.Attr and SysUtils.faDirectory ) > 0 );
end;

function DirPtr( ANode: TTreeNode ): PKDirName;
{ Returns a pointer to a TKDirName record for a given tree node }
begin
	ForceObject( ANode );
	try
		Result := PKDirName( ANode.Data );
	except
		Result := nil;
	end;
	ForcePointer( Result );
end;

function DirDepth( const APath: string ): Integer;
{ Returns the "depth" of the directory }
var
	sPath: string;
begin
	Result := 0;
	sPath := APath;
	while ( Pos( '\', sPath ) > 0 ) do
	begin
		Inc( Result );
		sPath := Copy( sPath, Pos( '\', sPath ) + 1, Length( sPath ) );
	end;
end;

function GetPart( const Path: string; N: Integer ): string;
{ Extracts the Directory name up to the Nth backslash }
var
	i,
	j: Integer;
begin
	j := 0;
	Result := Path;
	for i := 1 to Length( Path ) do
	begin
		if ( Path[i] = '\' ) then
			Inc( j );
		if ( j = N ) then
		begin
			Result := Copy( Path, 1, i );
			Exit;
		end;
	end;
end;

{------------------------- TKDirTree Implementation ----------------------------}

constructor TKDirTree.Create( AOwner: TComponent );
var
	sfi: TSHFileInfo;
begin
	inherited Create( AOwner );
	FDirectory := 'C:\';
	FDirRecList := TList.Create;
	FGoBellowRecycleBin := false;
	FTDirs := TStringList.Create;
	FIList := TImageList.Create( Self );
	InitActiveDirs;
	FIList.Handle := SHGetFileInfo( '', 0, sfi, SizeOf( TSHFileInfo ),
		SHGFI_SYSICONINDEX or SHGFI_SMALLICON );
	FIList.ShareImages := true;
	FAllowChange := false;
	FAllowNetwork := true;
	FDirectoryAttributes := [dtAll, dtNormal];
	Images := FIList;
	ReadOnly := true;
	SortType := stNone;
	HideSelection := false;
end;

destructor TKDirTree.Destroy;
begin
	FAllowChange := false;
	FTDirs.free;
	FIList.free;
{ Make sure all memory allocated for records created to deal with
	directory "cache" gets properly freed }
	while CheckList( FDirRecList ) do
	begin
		Dispose( PKDirName( FDirRecList[FDirRecList.Count - 1] ) );
		FDirRecList.Delete( FDirRecList.Count - 1 );
	end;
	FreeClean( FDirRecList );
	inherited Destroy;
end;

procedure TKDirTree.Loaded;
begin
	inherited Loaded;
	SetupDrives;
	SetDirectory( FDirectory );
end;

procedure TKDirTree.CreateWnd;
begin
	inherited CreateWnd;
	SetupDrives;
end;

procedure TKDirTree.InitActiveDirs;
var
	i: TKUpperAlphaEnum;
begin
	for i := Low( TKUpperAlphaEnum ) to High( TKUpperAlphaEnum ) do
		ActiveDir[i] := i + ':\';
	ActiveDir[GetCurrentDir[1]] := GetCurrentDir;
end;

procedure TKDirTree.AddDir( ANode: TTreeNode; const AName: string; Value: Boolean );
{ Associates a TKDirName record to a given tree node }
var
	pdn: PKDirName;
begin
	ForceObject( ANode );
	ForceTrimStr( AName );
	pdn := New( PKDirName );
	with pdn^ do
	begin
		Name := AName;
		Expanded := Value;
	end;
	ANode.Data := pdn;
	FDirRecList.Add( pdn );
end;

procedure TKDirTree.SetGoBellowRecycleBin( Value: Boolean );
begin
	if ( FGoBellowRecycleBin <> Value ) then
	begin
		FGoBellowRecycleBin := Value;
		Reload;
	end;
end;

procedure TKDirTree.SetDirectoryAttributes( Value: TKDirectoryAttributes );
begin
	if ( FDirectoryAttributes <> Value ) then
	begin
		FDirectoryAttributes := Value;
		Reload;
	end;
end;

procedure TKDirTree.SetAllowNetwork( Value: Boolean );
begin
	if ( FAllowNetwork <> Value ) then
	begin
		FAllowNetwork := Value;
		Reload;
	end;
end;

procedure TKDirTree.SetActiveDir( Index: TKUpperAlphaEnum; const Value: string );
begin
	Index := UpCase( Index );
	if ( FActiveDir[Index] <> Value ) then
		FActiveDir[Index] := Value;
end;

procedure TKDirTree.SetDrive( Value: char );
var
	sDir: string;
begin
	if ( Designing( Self ) or ( UpCase( Value ) = UpCase( GetDirectory[1] ) ) ) then
		Exit;
	sDir := ActiveDir[Value];
	if ( not CheckPath( sDir ) ) then
		sDir := ActiveDir[Value]{ [1] + ':\' };
	SetDirectory( sDir );
end;

procedure TKDirTree.SetDirectory( const Value: string );

	function GetRoot( const Path: string ): TTreeNode;
{ Returns the node representing the Path folder }
	var
		ANode: TTreeNode;
	begin
		Result := nil;
		ANode := Items.GetFirstNode;
		while CheckObject( ANode ) do
		begin
			if lstrcmpi( PChar( Path ), PChar( DirPtr( ANode )^.Name ) ) = 0 then
			begin
				if ( not DirPtr( ANode )^.Expanded ) then
					UpdateFolders( ANode );
				Result := ANode;
				Break;
			end;
			ANode := ANode.GetNextSibling;
		end;
	end;

var
	sDir,
	sValue: string;
	crs: TCursor;
	Node: TTreeNode;
	i,
	SlashCount: Integer;
begin
  ForcePath( Value );
	sValue := Value;
	FDirectory := sValue;
	if ( not ( CheckPath( sValue ) ) ) then
		sDir := GetCurrentDir[1] + ':\';
	if ( sValue[Length( sValue )] <> '\' ) then
		sValue := sValue + '\';
{
  During execution, if NetWork drives are NOT allowed and the current directory
	points to a network drive, set the directory to C:\
}
	if ( not FAllowNetwork ) and ( not Designing( Self ) ) and
		 ( GetDriveType( PChar( sValue[1] + ':\' ) ) = DRIVE_REMOTE ) then
	begin
		sValue := 'C:\';
		FDirectory := 'C:\';
	end;
	SlashCount := DirDepth( sValue );
	crs := Screen.Cursor;
	Screen.Cursor := crHourGlass;
	try
		Items.BeginUpdate;
		try
			sDir := GetPart( sValue, 1 );
			Node := GetRoot( sDir );
			if ( not Designing( Self ) ) then
				for i := 2 to SlashCount do
				begin
					sDir := GetPart( sValue, i );
					Node := GetChild( Node, sDir );
				end;
			Selected := Node;
			Topitem := Node;
		finally
			Items.EndUpdate;
		end;
	finally
		Screen.Cursor := crs;
	end;
end;

function TKDirTree.GetActiveDir( Index: TKUpperAlphaEnum ): string;
begin
	Index := UpCase( Index );
	Result := FActiveDir[Index];
end;

function TKDirTree.GetDrive: Char;
begin
	Result := UpCase( GetDirectory[1] );
end;

function TKDirTree.GetDropDir: string;
begin
	Result := '';
	if CheckObject( DropTarget ) then
		Result := DirPtr( DropTarget )^.Name;
end;

function TKDirTree.GetDirectory: string;
begin
	if Designing( Self ) then
	begin
		Result := FDirectory;
		Exit;
	end;
	if ( not ( CheckObject( Selected ) and CheckPointer( DirPtr( Selected ) ) ) ) then
	begin
		Result := GetCurrentDir;
		SetDirectory( Result );
	end
	else
		Result := DirPtr( Selected )^.Name;
end;

procedure TKDirTree.GetButton( var ANode: TTreeNode );
{ Updates the expand button displayed by ANode }
var
	dr: TKDirRec;
	sDir: string;
	iFound: Integer;
begin
	if ( Designing( Self ) or ( not CheckObject( ANode ) ) ) then
		Exit;
	sDir := DirPtr( ANode )^.Name;
	if ( DirPtr( ANode )^.Expanded ) then
	  Exit;
	iFound := FindFirstDir( sDir + '*.*', dr );
	if ( iFound = 0 ) then
		try
			while ( iFound = 0 ) do
			begin
				if ( ValidDir( dr ) and Allowed( sDir + dr.Name + '\' ) and CheckMask( dr.Attr ) ) then
				begin
					Items.AddChild( ANode, ' ' );
					Break;
				end;
				iFound := FindNextDir( dr );
			end;
		finally
			Windows.FindClose( dr.Handle );
		end;
end;

function TKDirTree.GetChild( ANode: TTreeNode; const Path: string ): TTreeNode;
{ Returns the node representing the Path folder }
var
	AChild: TTreeNode;
begin
	ForceObject( ANode );
	Result := nil;
	AChild := ANode.GetFirstChild;
	while CheckObject( AChild ) do
	begin
		if lstrcmpi( PChar( Path ), PChar( DirPtr( AChild )^.Name ) ) = 0 then
		begin
			if ( not DirPtr( AChild )^.Expanded ) then
				UpdateFolders( AChild );
			Result := AChild;
			Break;
		end;
		AChild := AChild.GetNextChild( AChild );
	end;
end;

procedure TKDirTree.UpdateButtons( var ANode: TTreeNode );
{ Updates the expand button of every folder displayed by ANode }
var
	Node: TTreeNode;
begin
	if ( Designing( Self ) or ( not CheckObject( ANode ) ) ) then
		Exit;
	Node := ANode.GetFirstChild;
	while CheckObject( Node ) do
	begin
		GetButton( Node );
		Node := ANode.GetNextChild( Node );
	end;
end;

procedure TKDirTree.UpdateFolders( var ANode: TTreeNode );
{ Updates the directory folders displayed by ANode }

	function AddFolder( const AFolder: string ): TTreeNode;
	{ Adds AFolder to ANode }
	var
		sFolder: string;
		Node: TTreeNode;
		sfi: TSHFileInfo;
	begin
		Result := nil;
		sFolder := AFolder;
		if ( not CheckObject( ANode ) ) then
			Exit;
		if ( sFolder[Length( sFolder )] <> '\' ) then
			sFolder := sFolder + '\';
		SHGetFileInfo( PChar( sFolder ), 0, sfi, SizeOf( sfi ),
			SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME );
		Node := ANode.Owner.AddChild( ANode, sfi.szDisplayName );
		AddDir( Node, sFolder, false );
		Node.ImageIndex := sfi.iIcon;
		SHGetFileInfo( PChar( sFolder ), 0, sfi, SizeOf( sfi ),
			SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_SELECTED );//SHGFI_OPENICON );
		Node.SelectedIndex := sfi.iIcon;
		Result := Node;
	end;

var
	dr: TKDirRec;
	sDir: string;
	i,
	iFound: Integer;
begin
	if ( Designing( Self ) or ( not CheckObject( ANode ) ) ) then
		Exit;
	FTDirs.Clear;
	if DirPtr( ANode )^.Expanded then
	  Exit;
	sDir := DirPtr( ANode )^.Name;
	ANode.DeleteChildren;
	DirPtr( ANode )^.Expanded := true;
	FillChar( dr, SizeOf( dr ), 0 );
	iFound := FindFirstDir( sDir + '*.*', dr );
	if ( iFound = 0 ) then
		try
			while ( iFound = 0 ) do
			begin
				if ( ValidDir( dr ) and Allowed( sDir + dr.Name + '\' ) and CheckMask( dr.Attr ) ) then
					FTDirs.Add( dr.Name );
				iFound := FindNextDir( dr );
			end;
		finally
			Windows.FindClose( dr.Handle );
		end;	
	FTDirs.Sorted := true;
	if ( FTDirs.Count > 0 ) then
		for i := 0 to FTDirs.Count - 1 do
			AddFolder( sDir + FTDirs[i] );
end;

procedure TKDirTree.Reload;
var
	ch: Char;
	sDir: string;
	Node,
	Child: TTreeNode;
begin
	SetupDrives;
	if Designing( Self ) then
		Exit;
	sDir := GetDirectory;
	SetDirectory( sDir );
	if ( Items.Count = 0 ) then
		Exit;
	if ( not CheckObject( Selected ) ) then
	begin
	  SetDirectory( Copy( sDir, 1, 3 ) );
		Node := Items.GetFirstNode;
		Child := Node.GetFirstChild;
		while ( CheckObject( Child ) and ( Child.Text = 'u**' ) and
					  CheckObject( Node.GetNextSibling ) ) do
		begin
			Node := Node.GetNextSibling;
			Child := Node.GetFirstChild;
		end;
		ch := UpCase( DirPtr( Node )^.Name[1] );
		SetDirectory( ActiveDir[ch] );
		if ( GetDirectory <> ActiveDir[ch] ) then
		begin
			Node.Selected := true;
			Change( Node );
		end;
	end;
end;

function TKDirTree.CheckMask( Attr: Integer ): Boolean;
{ Guarantees the inclusion criteria }
begin
	Result := ( dtAll in FDirectoryAttributes );
	if Result then
	  Exit;
	if ( ( ( ( Attr and SysUtils.faHidden ) = SysUtils.faHidden ) and
				 ( not ( dtHidden in FDirectoryAttributes ) ) ) or
			 ( ( ( Attr and SysUtils.faSysFile ) = SysUtils.faSysFile ) and
				 ( not ( dtSystem in FDirectoryAttributes ) ) ) or
			 ( ( ( Attr and SysUtils.faArchive ) = SysUtils.faArchive ) and
				 ( not ( dtArchive in FDirectoryAttributes ) ) ) ) then
		Exit;
	Result := true;
	if ( dtNormal in FDirectoryAttributes ) then
		Exit;
	if ( ( Attr and ( SysUtils.faHidden or SysUtils.faSysFile or SysUtils.faArchive ) ) = 0 ) then
		Result := false;
end;

function TKDirTree.Allowed( const Value: string ): Boolean;
{ Allows user to decide wheather to add Value to the DirTree or not }
var
	c: Integer;
begin
	Result := true;
{ allowed to go below rycle-bin ? }
	if ( not FGoBellowRecycleBin ) then
	begin
		c := Pos( '\recycled\', AnsiLowerCase( Value ) );
		if ( c > 0 ) then
			Result := ( c >= ( Length( Value ) - 9 ) );
	end;
	if ( Result and Assigned( FOnAddFile ) ) then
		FOnAddFile( Value, Result );
end;

procedure TKDirTree.SetupDrives;
{ Sets up the root drives available to the TKDirTree object }
var
	drv: Char;
	ct: Integer;
	crs: TCursor;
	node: TTreeNode;
	bits: set of 0..25;

	function DriveExists( db: byte ): Boolean;
	var
		ch: Char;
	begin
		Result := false;
		if ( not ( ct in bits ) ) then
		  Exit;
		ch := Char( db + ord( 'A' ) );
		if ( not FAllowNetwork ) then
			Result := ( not ( GetDriveType( PChar( ch + ':\' ) ) = DRIVE_REMOTE ) )
		else
			Result := true;
	end;

	procedure AddNode( ANode: TTreeNode; var Item: TTreeNode; const AFolder: string );
{ Adds a new node under ANode }
	var
		sFolder: string;
		sfi: TSHFileInfo;
	begin
		sFolder := AFolder;
		if ( sFolder[Length( sFolder )] <> '\' ) then
			sFolder := sFolder + '\';
		SHGetFileInfo( PChar( sFolder ), 0, sfi, SizeOf( sfi ),
			SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME );
		Item := Items.Add( ANode, sfi.szDisplayName );
		AddDir( Item, sFolder, false );
		Item.ImageIndex := sfi.iIcon;
		if ( sfi.iIcon <> FDefaultIcon ) then
		begin
			SHGetFileInfo( PChar( sFolder ), 0, sfi, SizeOf( sfi ),
				SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON );
			Item.SelectedIndex := sfi.iIcon;
			if ( Length( sFolder ) > 3 ) then
			begin
				FOpenIcon := sfi.iIcon;
				FDefaultIcon := Item.ImageIndex;
			end;
		end
		else
			Item.SelectedIndex := FOpenIcon;
	end;

begin
	FAllowChange := false;
	crs := Screen.Cursor;
	Screen.Cursor := crHourGlass;
	Items.BeginUpdate;
	Items.Clear;
	Integer( bits ) := GetLogicalDrives;
	for ct := 0 to 25 do
	begin
		Application.ProcessMessages;
		if DriveExists( ct ) then
		begin
			drv := Char( ct + Ord( 'A' ) );
			if Allowed( drv + ':\' ) then
			begin
				AddNode( nil, Node, drv + ':\' );
				if ( csDesigning in ComponentState ) then
					Items.AddChild( Node, 'u**' )
				else
					case GetDriveType( PChar( drv + ':\' ) ) of
						0, 1,
						DRIVE_CDROM,
						DRIVE_REMOVABLE: Items.AddChild( Node, 'u**' );
					else
						GetButton( Node );
					end;
			end;
		end;
	end;
	FAllowChange := true;
	Items.EndUpdate;
	TopItem := Selected;
	Screen.Cursor := crs;
end;

function TKDirTree.CanExpand( ANode: TTreeNode ): Boolean;
{ Checks wheather a given node can be expanded }
var
	crs: TCursor;
begin
	crs := Screen.Cursor;
	Screen.Cursor := crHourGlass;
	try
		UpdateFolders( ANode );
		UpdateButtons( ANode );
	finally
		Screen.Cursor := crs;
	end;	
	Result := inherited CanExpand( ANode );
end;

procedure TKDirTree.Change( ANode: TTreeNode );
{ Updates active dir for current drive }
var
	sDir: string;
begin
	if ( ( not Designing( Self ) ) and FAllowChange ) then
	begin
		if CheckObject( ANode ) then
		begin
			sDir := DirPtr( ANode )^.Name;
			ActiveDir[sDir[1]] := sDir;
		end;
		inherited Change( ANode );
	end;
end;

procedure TKDirTree.FullExpand;
{ Expands the TKDirTree object completely }
var
	crs: TCursor;
	Node: TTreeNode;
begin
	crs := Screen.Cursor;
	Screen.Cursor := crHourGlass;
	try
		Node := Items.GetFirstNode;
		while CheckObject( Node ) do
		begin
			Node.Expand( True );
			Node := Node.GetNextSibling;
		end;
	finally
		Screen.Cursor := crs;
	end;	
end;

function TKDirTree.FindFirstDir( path: TFileName; var Dir: TKDirRec ): Integer;
{ Finds first file satisfying the criteria in Data }
begin
	with Dir do
	begin
		ZeroMemory( @Dir, SizeOf( TKDirRec ) );
		Handle := FindFirstFile( PChar( path ), Data );
{ Here we must TypeCast INVALID_HANDLE_VALUE to Cardinal for Delphi4 }
		if ( Handle <> Cardinal( INVALID_HANDLE_VALUE ) ) then
		begin
			Result := 0;
			Attr := Data.dwFileAttributes;
			Name := Data.cFileName;
		end
		else
			Result := GetLastError;
	end;
end;

function TKDirTree.FindNextDir( var Dir: TKDirRec ): Integer;
{ Finds subsequent files satisfying the criteria in Data }
begin
	with Dir do
	begin
		if FindNextFile( Handle, Data ) then
		begin
			Result := 0;
			Attr := Data.dwFileAttributes;
			Name := Data.cFileName
		end
		else
			Result := GetLastError;
	end;
end;


{--------------------------------- TKTreeView ----------------------------------}

const
	NM_CUSTOMDRAW = NM_FIRST - 12;

{
	custom draw return flags
	values under = $00010000 are reserved for global
	custom draw values; above that are for specific controls
}
	CDRF_DODEFAULT   = $00000000;
	CDRF_NEWFONT     = $00000002;
	CDRF_SKIPDEFAULT = $00000004;

	CDRF_NOTIFYPOSTPAINT = $00000010;
	CDRF_NOTIFYITEMDRAW  = $00000020;
	CDRF_NOTIFYPOSTERASE = $00000040;
	CDRF_NOTIFYITEMERASE = $00000080;

{
	drawstage flags
	values under = $00010000 are reserved for global
	custom draw values;	above that are for specific controls
}
	CDDS_PREPAINT  = $00000001;
	CDDS_POSTPAINT = $00000002;
	CDDS_PREERASE  = $00000003;
	CDDS_POSTERASE = $00000004;

{ the = $000010000 bit means it's individual item specific }
	CDDS_ITEM = $00010000;

	CDDS_ITEMPREPAINT  = ( CDDS_ITEM OR CDDS_PREPAINT );
	CDDS_ITEMPOSTPAINT = ( CDDS_ITEM OR CDDS_POSTPAINT );
	CDDS_ITEMPREERASE  = ( CDDS_ITEM OR CDDS_PREERASE );
	CDDS_ITEMPOSTERASE = ( CDDS_ITEM OR CDDS_POSTERASE );

{ itemState flags ( compare with ODS_ constants ) }
	CDIS_SELECTED = $0001;
	CDIS_GRAYED   = $0002;
	CDIS_DISABLED = $0004;
	CDIS_CHECKED  = $0008;
	CDIS_FOCUS    = $0010;
	CDIS_DEFAULT  = $0020;
	CDIS_HOT      = $0040;

{ TKTreeView }

constructor TKTreeView.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAlwaysEditted := False;
	fInternalEditNode := nil;
	fEditingSelected := False;
end;

function TKTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
	Result := inherited CanEdit( Node );
	if ( Result ) then
	begin
		fInternalEditNode := Node;
		fEditingSelected := True;
	end
	else
	begin
		fInternalEditNode := nil;
		fEditingSelected := False;
	end;
end;

procedure TKTreeView.Edit(const Item: TTVItem);
var
	pc: PChar;
	Node: TTreeNode;
	AuxItem: TTVItem;
begin
	if CheckObject( fInternalEditNode ) then
	begin
    fInternalEditNode := nil;
		fEditingSelected := False;
	end;
	if AlwaysEditted and CheckPointer( Item.pszText ) then
	begin
	  {Hard coded because GetNodeItem is private!!!}
		if ( ( Item.state and TVIF_PARAM ) <> 0 ) then
			Node := TTreeNode( Pointer( Item.lParam ) )
		else
  		Node := Items.GetNode(Item.hItem);
		if CheckObject( Node ) and CheckStr( Node.Text ) then
		begin
			pc := StrNew( PChar( Node.Text ) );
			try
				System.Move( Item, AuxItem, SizeOf( TTVItem ) );
				AuxItem.pszText := pc;				
				inherited Edit( AuxItem );
			finally
				StrDispose( pc );
			end;
		end
		else
			inherited Edit( Item );
	end
	else
		inherited Edit( Item );
end;

procedure TKTreeView.CNNotify( var Msg: TWMNotify );
var
	cd: TNMCustomDraw;
begin
{ Check, if it is the custom draw message }
	if ( Msg.NMHdr^.Code = NM_CUSTOMDRAW ) then
	begin
{ lParam ( covered by NMHdr ) is a pointer to a custom draw structure-
	as defined in 'CustomDraw.pas'; it should have been in 'CommCtrl.pas'; }
		cd := PNMCustomDraw( Pointer( Msg.NMHDR ) )^;
		case cd.dwDrawStage of
			CDDS_PREPAINT:
				Msg.Result := CDRF_NOTIFYITEMDRAW;
			CDDS_ITEMPREPAINT:
				begin
					DoPaintItem( cd );
					Msg.Result := CDRF_NEWFONT;
				end
			else
				Msg.Result := 0;
		end;
	end
	else
		inherited;
end;

procedure TKTreeView.DoPaintItem(	cd: TNMCustomDraw );
begin
	if Assigned( OnPaintItem ) then
		FOnPaintItem( Self, Items.GetNode( HTreeItem( cd.dwItemSpec ) ), cd.HDC );
end;

function TKTreeView.FindNodeText( StartIndex: Integer; Text: string; Partial, Inclusive,
	Wrap: Boolean ): TTreeNode;
var
	i: Integer;
begin
	Result := nil;
	if Inclusive then
		Dec( StartIndex );
	for i := StartIndex + 1 to Items.Count - 1 do
		if ( ( Partial and CheckStrContains( Text, Items[i].Text ) ) or
			 ( ( not Partial ) and CheckStrEqual( Text, Items[i].Text ) ) ) then
			Break;
	if ( i <= Items.Count - 1 ) then
		Result := Items[i]
	else if Wrap then
	begin
		if Inclusive then
			Inc( StartIndex );
		for i := 0 to StartIndex - 1 do
			if ( ( Partial and CheckStrContains( Text, Items[i].Text ) ) or
				 ( ( not Partial ) and CheckStrEqual( Text, Items[i].Text ) ) ) then
				Break;
		if ( i <= StartIndex ) then
			Result := Items[i];
	end;
end;

function TKTreeView.FindData( StartIndex: Integer; Value: Pointer;
	Inclusive, Wrap: Boolean ): TTreeNode;
var
	I: Integer;
begin
	Result := nil;
	if Inclusive then
		Dec( StartIndex );
	for I := StartIndex + 1 to Items.Count - 1 do
		if ( Items[I].Data = Value ) then
			Break;
	if ( i <= Items.Count - 1 ) then
		Result := Items[i]
	else if Wrap then
	begin
		if Inclusive then
			Inc( StartIndex );
		for I := 0 to StartIndex - 1 do
			if ( Items[I].Data = Value ) then
				 Break;
		if ( i <= StartIndex ) then
			Result := Items[i];
	end;
end;

function TKTreeView.FindNoteAtLevel( Level: Integer; Option: TKFindNodeOption ): TTreeNode;
begin
	Result := TopItem;
	{ Found the node at the correct level and position }
	while CheckObject( Result ) and ( Result.Level <> Level ) do
		case Option of
			fnoFirst : Result := Result.GetFirstChild;
			fnoLast  : Result := Result.GetLastChild;
		end;
	{ in the case of the first level, go to the correct (last) position, if the
	  option is None/First, the TopItem are node selected }
	if ( Level = 0 ) and ( Option = fnoLast ) then
		while CheckObject( Result.GetNextSibling ) do
			Result := Result.GetNextSibling;
end;

{ TKShellComboBox }

constructor TKShellComboBox.Create( AOwner: TComponent );
var
	sfi: TSHFileInfo;
begin
	inherited Create( AOwner );
	TControlCanvas( Canvas ).Control := Self;
	FMinHeight := -1;
	FListBuilt := False;
	FAutoRefresh := True;
	FDriveList := TStringList.Create;
	FAddedPaths := TStringList.Create;
	if ( not CheckWinNT ) then
	begin
		FImageList := TImageList.Create( nil );
		FImageList.ShareImages := true;
		FImageList.DrawingStyle := dsTransparent;
		FImageList.Handle := SHGetFileInfo( '', 0, sfi, SizeOf( TSHFileInfo ),
			SHGFI_SYSICONINDEX or SHGFI_SMALLICON );
	end;		
	FShellComboBoxStyle := scsDrives;
	SetBounds( Left, Top, 200, 26 );
	FMinHeight := -1;
end;

destructor TKShellComboBox.Destroy;
begin
	FDriveList.Free;
	FImageList.Free;
	FAddedPaths.Free;
	inherited Destroy;
end;

procedure TKShellComboBox.ReadData( Reader: TReader );
begin
	Reader.ReadListBegin;
	FAddedPaths.Clear; // Clear( True ); { Will be refreshed at Loaded }
	while ( not Reader.EndOfList ) do
		FAddedPaths.Add( Reader.ReadString );
	Reader.ReadListEnd;
{ AdjustAddedPaths - Done in Loaded }
end;

procedure TKShellComboBox.WriteData( Writer: TWriter );
var
	i: Integer;
begin
	Writer.WriteListBegin;
	for i := 0 to FAddedPaths.Count - 1 do
		Writer.WriteString( FAddedPaths[i] );
	Writer.WriteListEnd;
end;

procedure TKShellComboBox.DefineProperties( Filer: TFiler );

	function HasData: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKShellComboBox ) then
      { Hard Couple!!! }
				Result := ( not FAddedPaths.Equals( TKShellComboBox( Filer.Ancestor ).FAddedPaths ) );
		end
		else
			Result := ( FAddedPaths.Count > 0 );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'AddedPaths', ReadData, WriteData, HasData );
end;

procedure TKShellComboBox.SetAutoRefresh( Value: Boolean );
begin
	if ( Value <> FAutoRefresh ) then
	begin
		FAutoRefresh := Value;
		if FAutoRefresh then
			RecreateWnd
		else
		begin
			Clear( ( not Designing( Self ) ) );
			AdjustAddedPaths;
		end;
	end;
end;

procedure TKShellComboBox.SetShellComboBoxStyle( Value: TKShellComboBoxStyle );
begin
	if ( Value <> FShellComboBoxStyle ) then
	begin
		NotYetImplemented;
		Exit;  // Actually read-only...
		if CheckObject( Parent ) then
			ItemIndex := -1;
		Clear( ( not Designing( Self ) ) );
		AdjustAddedPaths;
		FShellComboBoxStyle := Value;
		ReCreateWnd;
	end;
end;

procedure TKShellComboBox.Loaded;
begin
	inherited Loaded;
	if CheckStrings( FAddedPaths ) then
	begin
		if AutoRefresh then
			RefreshDrives;
		AdjustAddedPaths;
	end;
end;

procedure TKShellComboBox.CreateWnd;
begin
	if ( not CheckWinNT ) then
  	ItemHeight := FImageList.Height + 4;
	inherited CreateWnd;
	if ( Style <> csOwnerDrawVariable ) then
	begin
		FMinHeight := -1;
		Style := csOwnerDrawVariable;
	end;
	if ( AutoRefresh and not FListBuilt ) then
		RefreshDrives;
end;

function TKShellComboBox.GetMinHeight: Integer;
var
	i: Integer;
begin
	if ( FMinHeight = -1 ) then
	begin
		Result := Height;
		for i := 0 to Items.Count - 1 do
			Result := Max( Result, Canvas.TextHeight( Items[i] ) );
		FMinHeight := Result;
	end
	else
		Result := FMinHeight;
end;

procedure TKShellComboBox.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
	if ( not CheckObject( Parent ) ) then
	  inherited SetBounds( ALeft, ATop, AWidth, AHeight )
	else
		inherited SetBounds( ALeft, ATop, AWidth, Min( GetMinHeight, AHeight ) );
end;                                        

procedure TKShellComboBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( Key = VK_F5 ) and ( Shift = [] ) then
	begin
		RefreshDrives;
		Key := 0;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKShellComboBox.MeasureItem( Index: Integer; var Height: Integer );
begin
	if CheckWinNT then
		inherited MeasureItem( Index, Height )
	else
		{ do not call the inherited method for Win95 }
		Height := ( FImageList.Height + 4 );
end;

procedure TKShellComboBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
const
	SHELL_FLAGS: array[Boolean] of UINT =
	(
		SHGFI_SYSICONINDEX or SHGFI_SMALLICON,
		SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON // SELECTED
	);
var
	sfi: TSHFileInfo;
begin
	if CheckWinNT then
		inherited DrawItem( Index, Rect, State )
	else
	begin
		{ do not call the inherited method for Win95 }
		Canvas.FillRect( Rect );
		if ( Index < FDriveList.Count ) then
			SHGetFileInfo( PChar( FDriveList[Index] ), 0, sfi, SizeOf( TSHFileInfo ), SHELL_FLAGS[( odSelected in State )] )
		else
			SHGetFileInfo( PChar( Items[Index] ), 0, sfi, SizeOf( TSHFileInfo ), SHELL_FLAGS[( odSelected in State )] );
		FImageList.Draw( Canvas, Rect.Left + 2, Rect.Top + 1, sfi.iIcon );//Index );
		Canvas.TextOut( Rect.Left + FImageList.Width + 5, Rect.Top +
			( ( ItemHeight - Canvas.TextHeight( Items[Index] ) ) div 2 ), Items[Index] );
	end;		
end;

procedure TKShellComboBox.BuildShellDrivesList;
var
	i: Integer;
	sfi: TSHFileInfo;
begin
	if FListBuilt then
		Exit;
	Items.BeginUpdate;
	try
		Clear( False );
		uksyUtils.DriveList( FDriveList );
		if CheckWinNT then
		  Items.AddStrings( FDriveList )
		else
			for i := 0 to FDriveList.Count - 1 do
			begin
				SHGetFileInfo( PChar( FDriveList[i] ), 0, sfi, SizeOf( sfi ),
					SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME );
				Items.Add( sfi.szDisplayName );
			end;
		FListBuilt := True;
	finally
		Items.EndUpdate;
	end;
end;

procedure TKShellComboBox.BuildShellExeTypeInfoList;
begin
	FListBuilt := True;
end;

procedure TKShellComboBox.Clear( ClearAll: Boolean );
begin
	FMinHeight := -1;
	FListBuilt := False;
	FDriveList.Clear;
	if ClearAll then
  	FAddedPaths.Clear;
	Items.Clear;
end;

procedure TKShellComboBox.DoAddPath;
begin
	if Assigned( FOnAddPath ) then
		FOnAddPath( Self );
end;

function TKShellComboBox.AddPath( const Path: string ): Integer;
begin
	if ( AutoRefresh and not FListBuilt ) then
		RefreshDrives;
	if ( not CheckPath( Path ) ) then
	begin
		Result := -1;
		Exit;
	end;
	Result := Items.IndexOf( Path );
	if ( Result = -1 ) then
	begin
		Result := FDriveList.IndexOf( Path );
		if ( Result = -1 ) then
		begin
			Result := Items.Add( Path );
			if ( FAddedPaths.IndexOf( Path ) = -1 ) then
				FAddedPaths.Add( Path );
			if ( not ( Designing( Self ) or Loading( Self ) or Reading( Self ) ) ) then
  			DoAddPath;	
		end;
	end;
end;

function TKShellComboBox.IndexOfPath( const Path: string ): Integer;
begin
	Result := Items.IndexOf( Path );
	if ( Result = -1 ) then
	begin
		Result := FDriveList.IndexOf( Path );
		if ( Result = -1 ) then
			Result := FAddedPaths.IndexOf( Path );
	end;		
end;

procedure TKShellComboBox.DeletePath( Index: Integer );
var
	i: Integer;
begin
	i := FAddedPaths.IndexOf( Items[Index] );
	Items.Delete( Index );
	if ( i <> -1 ) then
		FAddedPaths.Delete( i );
end;

procedure TKShellComboBox.RemovePath( const Path: string );
var
  i: Integer;
begin
	i := IndexOfPath( Path );
	if ( i <> -1 ) then
		DeletePath( i );
end;

procedure TKShellComboBox.AdjustAddedPaths;
var
  i: Integer;
begin
	if CheckStrings( FAddedPaths ) then
	begin
		Items.BeginUpdate;
		try
			for i := 0 to FAddedPaths.Count - 1 do
				AddPath( FAddedPaths[i] );
		finally
			Items.EndUpdate;
		end;
	end;
end;

procedure TKShellComboBox.RefreshDrives;
var
	st: TStrings;
	i,
	iOldIndex: Integer;
begin
	FMinHeight := -1;
	FListBuilt := False;
	iOldIndex := ItemIndex;
	try
		st := TStringList.Create;
		try
			if ( Items.Count > FDriveList.Count ) then
				for i := FDriveList.Count to Items.Count - 1 do
					st.Add( Items[i] );
			if CheckStrings( FAddedPaths ) then
  			st.AddStrings( FAddedPaths );
			case FShellComboBoxStyle of
				scsDrives     : BuildShellDrivesList;
				scsExeTypeInfo: BuildShellExeTypeInfoList;
			end;
			if CheckStrings( st ) then
				for i := 0 to st.Count - 1 do
					AddPath( st[i] );
		finally
			st.Free;
		end;
	finally
		if ( Items.Count > iOldIndex ) then
			ItemIndex := iOldIndex;
	end;
end;

{ TKShellListBox }

constructor TKShellListBox.Create( AOwner: TComponent );
var
	sfi: TSHFileInfo;
begin
	inherited Create( AOwner );
	TControlCanvas( Canvas ).Control := Self;
	FListBuilt := False;
  FAutoRefresh := True;
	FDriveList := TStringList.Create;
	FAddedPaths := TStringList.Create;
	if ( not CheckWinNT ) then
	begin
		FImageList := TImageList.Create( nil );
		FImageList.ShareImages := true;
		FImageList.DrawingStyle := dsTransparent;
		FImageList.Handle := SHGetFileInfo( '', 0, sfi, SizeOf( TSHFileInfo ),
			SHGFI_SYSICONINDEX or SHGFI_SMALLICON );
	end;		
	FShellListBoxStyle := slsDrives;
	SetBounds( Left, Top, 200, Height );
end;

destructor TKShellListBox.Destroy;
begin
	FDriveList.Free;
	FImageList.Free;
	FAddedPaths.Free;
	inherited Destroy;
end;

procedure TKShellListBox.ReadData( Reader: TReader );
begin
	Reader.ReadListBegin;
	FAddedPaths.Clear; // Clear( True ); { Will be refreshed at Loaded }
	while ( not Reader.EndOfList ) do
		FAddedPaths.Add( Reader.ReadString );
	Reader.ReadListEnd;
{ AdjustAddedPaths - Done in Loaded }	
end;

procedure TKShellListBox.WriteData( Writer: TWriter );
var
	i: Integer;
begin
	Writer.WriteListBegin;
	for i := 0 to FAddedPaths.Count - 1 do
		Writer.WriteString( FAddedPaths[i] );
	Writer.WriteListEnd;
end;

procedure TKShellListBox.DefineProperties( Filer: TFiler );

	function HasData: Boolean;
	begin
		if CheckObject( Filer.Ancestor ) then
		begin
			Result := True;
			if CheckObjectClass( Filer.Ancestor, TKShellListBox ) then
			{ Hard Couple!!! }
				Result := ( not FAddedPaths.Equals( TKShellListBox( Filer.Ancestor ).FAddedPaths ) );
		end
		else
			Result := ( FAddedPaths.Count > 0 );
	end;

begin
	inherited DefineProperties( Filer );
	Filer.DefineProperty( 'AddedPaths', ReadData, WriteData, HasData );
end;

procedure TKShellListBox.Loaded;
begin
	inherited Loaded;
	if CheckStrings( FAddedPaths ) then
	begin
		if AutoRefresh then
			RefreshDrives;
    AdjustAddedPaths;
	end;
end;

procedure TKShellListBox.AdjustAddedPaths;
var
  i: Integer;
begin
	if CheckStrings( FAddedPaths ) then
	begin
		Items.BeginUpdate;
		try
			for i := 0 to FAddedPaths.Count - 1 do
				AddPath( FAddedPaths[i] );
		finally
			Items.EndUpdate;
		end;
	end;
end;

procedure TKShellListBox.SetAutoRefresh( Value: Boolean );
begin
	if ( Value <> FAutoRefresh ) then
	begin
		FAutoRefresh := Value;
		if FAutoRefresh then
			RecreateWnd
		else
		begin
			Clear( ( not Designing( Self ) ) );
			AdjustAddedPaths;
		end;
	end;
end;

procedure TKShellListBox.SetShellListBoxStyle( Value: TKShellListBoxStyle );
begin
	if ( Value <> FShellListBoxStyle ) then
	begin
		NotYetImplemented;
		Exit;  // Actually read-only...
		if CheckObject( Parent ) then
			ItemIndex := -1;
		Clear( ( not Designing( Self ) ) );
		AdjustAddedPaths;
		FShellListBoxStyle := Value;
		ReCreateWnd;
	end;
end;

procedure TKShellListBox.CreateWnd;
begin
	if ( not CheckWinNT ) then
  	ItemHeight := FImageList.Height + 4;
	inherited CreateWnd;
	if ( Style <> lbOwnerDrawVariable ) then
		Style := lbOwnerDrawVariable;
	if ( AutoRefresh and not FListBuilt ) then
		RefreshDrives;
end;

procedure TKShellListBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( Key = VK_F5 ) and ( Shift = [] ) then
	begin
		RefreshDrives;
		Key := 0;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKShellListBox.MeasureItem( Index: Integer; var Height: Integer );
begin
	if CheckWinNT then
		inherited MeasureItem( Index, Height )
	else
		{ do not call the inherited method for Win95 }
		Height := ( FImageList.Height + 4 );
end;

procedure TKShellListBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
const
	SHELL_FLAGS: array[Boolean] of UINT =
	(
		SHGFI_SYSICONINDEX or SHGFI_SMALLICON,
		SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON // SELECTED
	);
var
	sfi: TSHFileInfo;
begin
	if CheckWinNT then
		inherited DrawItem( Index, Rect, State )
	else
	begin
		{ do not call the inherited method for Win95 }
		Canvas.FillRect( Rect );
		if ( Index < FDriveList.Count ) then
			SHGetFileInfo( PChar( FDriveList[Index] ), 0, sfi, SizeOf( TSHFileInfo ), SHELL_FLAGS[( odSelected in State )] )
		else
			SHGetFileInfo( PChar( Items[Index] ), 0, sfi, SizeOf( TSHFileInfo ), SHELL_FLAGS[( odSelected in State )] );
		FImageList.Draw( Canvas, Rect.Left + 2, Rect.Top + 1, sfi.iIcon );
		Canvas.TextOut( Rect.Left + FImageList.Width + 5, Rect.Top +
			( ( ItemHeight - Canvas.TextHeight( Items[Index] ) ) div 2 ), Items[Index] );
	end;		
end;

procedure TKShellListBox.BuildShellDrivesList;
var
	i: Integer;
	sfi: TSHFileInfo;
begin
	if FListBuilt then
		Exit;
	Items.BeginUpdate;
	try
		Clear( False );
		uksyUtils.DriveList( FDriveList );
		if CheckWinNT then
		  Items.AddStrings( FDriveList )
		else
			for i := 0 to FDriveList.Count - 1 do
			begin
				SHGetFileInfo( PChar( FDriveList[i] ), 0, sfi, SizeOf( sfi ),
					SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME );
				Items.Add( sfi.szDisplayName );
			end;
		FListBuilt := True;
	finally
		Items.EndUpdate;
	end;
end;

procedure TKShellListBox.BuildShellExeTypeInfoList;
begin
	FListBuilt := True;
end;

procedure TKShellListBox.Clear( ClearAll: Boolean );
begin
	FListBuilt := False;
	FDriveList.Clear;
	if ClearAll then
		FAddedPaths.Clear;
	Items.Clear;
end;

procedure TKShellListBox.DoAddPath;
begin
	if Assigned( FOnAddPath ) then
	  FOnAddPath( Self );
end;

function TKShellListBox.AddPath( const Path: string ): Integer;
begin
	if ( not CheckPath( Path ) ) then
	begin
		Result := -1;
		Exit;
	end;
	Result := Items.IndexOf( Path );
	if ( Result = -1 ) then
	begin
		Result := FDriveList.IndexOf( Path );
		if ( Result = -1 ) then
		begin
			Result := Items.Add( Path );
			if ( FAddedPaths.IndexOf( Path ) = -1 ) then
				FAddedPaths.Add( Path );
			if ( not ( Designing( Self ) or Loading( Self ) or Reading( Self ) ) ) then
				DoAddPath;
		end;
	end;
end;

function TKShellListBox.IndexOfPath( const Path: string ): Integer;
begin
	Result := Items.IndexOf( Path );
	if ( Result = -1 ) then
	begin
		Result := FDriveList.IndexOf( Path );
		if ( Result = -1 ) then
			Result := FAddedPaths.IndexOf( Path );
	end;
end;

procedure TKShellListBox.DeletePath( Index: Integer );
var
	i: Integer;
begin
	i := FAddedPaths.IndexOf( Items[Index] );
	Items.Delete( Index );
	if ( i <> -1 ) then
		FAddedPaths.Delete( i );
end;

procedure TKShellListBox.RemovePath( const Path: string );
var
  i: Integer;
begin
	i := IndexOfPath( Path );
	if ( i <> -1 ) then
		DeletePath( i );
end;

procedure TKShellListBox.RefreshDrives;
var
	st: TStrings;
	i,
	iOldIndex: Integer;
begin
	FListBuilt := False;
	iOldIndex := ItemIndex;
	try
		st := TStringList.Create;
		try
			if ( Items.Count > FDriveList.Count ) then
				for i := FDriveList.Count to Items.Count - 1 do
					st.Add( Items[i] );
			if CheckStrings( FAddedPaths ) then
				st.AddStrings( FAddedPaths );
			case FShellListBoxStyle of
				slsDrives     : BuildShellDrivesList;
				slsExeTypeInfo: BuildShellExeTypeInfoList;
			end;
			if CheckStrings( st ) then
				for i := 0 to st.Count - 1 do
					AddPath( st[i] );
		finally
			st.Free;
		end;
	finally
		if ( Items.Count > iOldIndex ) then
			ItemIndex := iOldIndex;
	end;
end;

end.
