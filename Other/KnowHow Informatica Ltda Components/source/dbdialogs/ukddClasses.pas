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

unit ukddClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	{$IFDEF DELPHI4}SysUtils, {$ENDIF} Classes, DB, DBCtrls, DBTables, uksyUtils,
	ukdgClasses;

type

	EKDDClasses = class( EKDBDialogs );

{
--------------------------------------------------------------------------------
------------------------- Basic DB Dialog Architecture -------------------------
--------------------------------------------------------------------------------
}

{ TKDataDialog }

	TKDataDialogOption = ( ddoSizeable, ddoLoadVisible, ddoSaveVisible, ddoClearVisible,
		ddoRevertVisible, ddoChangeProps, ddoAllowCheckBoxes, ddoStrech, ddoCenter,
		ddoScrollVert, ddoWordWrap );
	TKDataDialogOptions = set of TKDataDialogOption;

const
	DEFAULT_DATA_DIALOG_OPT = [ddoSizeable, ddoLoadVisible, ddoSaveVisible, ddoClearVisible,
	  ddoRevertVisible, ddoChangeProps, ddoAllowCheckBoxes, ddoCenter, ddoScrollVert];

type

	TKDataDialogMode = ( dmMemo, dmRichEdit, dmGraphic );

{$HINTS OFF}

	TKDataDialog = class( TKExtendedDialog )
	private
		FField: TBlobField;
		FDetecting: Boolean;
		FAutoDetect: Boolean;
		FOnOkClick: TNotifyEvent;
		FOnCancel: TNotifyEvent;
		FOnLoad: TNotifyEvent;
		FOnSave: TNotifyEvent;
		FOnClear: TNotifyEvent;
		FOnRevert: TNotifyEvent;
		FDataLink: TFieldDataLink;
		FDialogMode: TKDataDialogMode;
		FOptions: TKDataDialogOptions;

{ From TKCustomDialog }
		property Picture;

		function GetField: TBlobField;
		procedure SetOptions( Value: TKDataDialogOptions );
		procedure SetAutoDetect( Value: Boolean );
		procedure SetDialogMode( Value: TKDataDialogMode );

		function GetDataField: string;
		function GetDataSource: TDataSource;
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

	protected
		procedure DefaultLimits; override;
		procedure DoBeforeExecute; override;
		function DoCheckParams: Boolean; override;

		procedure DoOK; dynamic;
		procedure DoCancel; dynamic;
		procedure DoLoad; dynamic;
		procedure DoSave; dynamic;
		procedure DoClear; dynamic;
		procedure DoRevert; dynamic;

		procedure DetectDialogMode; virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		function EditBlob( AField: TBlobField ): Boolean; dynamic;
		function CanModify: Boolean; dynamic;

		procedure Load; dynamic;
		procedure Save; dynamic;
		procedure Clear; dynamic;
		procedure Revert; dynamic;

		property Field: TBlobField
						 read GetField;

	published
{ From TKBaseDialog }
		property Width;
		property Height;

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

		property AfterExecute;
		property BeforeExecute;

		property AutoDetect: Boolean
						 read FAutoDetect write SetAutoDetect;
		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property DialogMode: TKDataDialogMode
						 read FDialogMode write SetDialogMode default dmMemo;
		property Options: TKDataDialogOptions
						 read FOptions write SetOptions default DEFAULT_DATA_DIALOG_OPT;

		property OnOkClick: TNotifyEvent
						 read FOnOkClick write FOnOkClick;
		property OnCancel: TNotifyEvent
						 read FOnCancel write FOnCancel;
		property OnLoad: TNotifyEvent
						 read FOnLoad write FOnLoad;
		property OnSave: TNotifyEvent
						 read FOnSave write FOnSave;
		property OnClear: TNotifyEvent
						 read FOnClear write FOnClear;
		property OnRevert: TNotifyEvent
						 read FOnRevert write FOnRevert;

	end;

function DataDialog( const ACaption: string; AField: TBlobField ): Boolean;
function DataDialogEx( const ACaption: string; Options: TKDataDialogOptions;
	AField: TBlobField ): Boolean;

{
--------------------------------------------------------------------------------
------------------------ DB Entities Dialog Architecture -----------------------
--------------------------------------------------------------------------------
}

type

{ TKCustomDBListDialog }

	TKCustomDBListDialog = class( TKCustomInputListDialog )
	private
    FAutoRefresh: Boolean;
    
{ From TKCustomInputListDialog }
		property EditText;
		property UserInput;

		function GetItemCount: Cardinal;
		function GetItems( Index: Cardinal ): string;

	protected
		procedure FillList( ss: TStrings ); virtual; abstract;

		procedure DoBeforeExecute; override;

	public
		constructor Create( AOwner: TComponent ); override;

    procedure Clear; virtual;
    procedure Refresh; virtual;
    
		property Items[Index: Cardinal]: string
						 read GetItems; default;
		property ItemCount: Cardinal
						 read GetItemCount;

	published
		property Caption;

{ From TKCustomInputListDialog }
		property ItemIndex;

    property AutoRefresh: Boolean
             read FAutoRefresh write FAutoRefresh default True;
	end;

	TKCustomDBListDialogClass = class of TKCustomDBListDialog;

{ TKDBSessionListDialog }

	TKDBSessionListDialog = class( TKCustomDBListDialog )
	private
		procedure FillList( ss: TStrings ); override;

	end;

{ TKDBDriverListDialog }

	TKDBDriverListDialog = class( TKCustomDBListDialog )
	private
		procedure FillList( ss: TStrings ); override;

	end;

{ TKAliasListDialog } 

	TKAliasListDialog = class( TKCustomDBListDialog )
	private
		FPersistent: Boolean;

		procedure FillList( ss: TStrings ); override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property Persistent: Boolean
						 read FPersistent write FPersistent default True;

	end;

{ TKCustomDBInfoDialog }

	TKCustomDBInfoDialog = class( TKCustomDBListDialog )
	private
		FSessionName: string;
		FDatabaseName: string;

	protected
		function GetSession: TSession; dynamic;
		procedure SetSessionName( const Value: string ); virtual;
		procedure SetDatabaseName( const Value: string ); virtual;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property DatabaseName: string
						 read FDatabaseName write SetDatabaseName;
		property SessionName: string
						 read FSessionName write SetSessionName;

	end;

	TKCustomDBInfoDialogClass = class of TKCustomDBInfoDialog; 

{ TKTableListDialog }

	TKTableListDialog = class( TKCustomDBInfoDialog )
	private
		FFilter: string;
		FExetensions: Boolean;
		FSystemTables: Boolean;

		procedure FillList( ss: TStrings ); override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property Filter: string
						 read FFilter write FFilter;
		property Extensions: Boolean
						 read FExetensions write FExetensions default True;
		property SystemTables: Boolean
						 read FSystemTables write FSystemTables default False;

	end;

{ TKStoredProcListDialog }

	TKStoredProcListDialog = class( TKCustomDBInfoDialog )
	private
		procedure FillList( ss: TStrings ); override;

	end;

{ TKCustomTableInfoDialog }

	TKCustomTableInfoDialog = class( TKCustomDBInfoDialog )
	private
		FTable: TTable;
		FTableName: string;

		procedure FillList( ss: TStrings ); override;
		procedure SetSessionName( const Value: string ); override;
		procedure SetDatabaseName( const Value: string ); override;

	protected
		procedure GetTableList( ss: TStrings ); virtual; abstract;

		property Table: TTable
						 read FTable;

	published
		property TableName: string
						 read FTableName write FTableName;

	end;

{ TKTableFieldsDialog }

	TKTableFieldsDialog = class( TKCustomTableInfoDialog )
	private
		procedure GetTableList( ss: TStrings ); override;

	public
		property Table;

	end;

{ TKTableIndexDialog }

	TKTableIndexDialog = class( TKCustomTableInfoDialog )
	private
		procedure GetTableList( ss: TStrings ); override;

	public
		property Table;

	end;

{ TKCustomStoredProcInfoDialog }

	TKCustomStoredProcInfoDialog = class( TKCustomDBInfoDialog )
	private
		FStoredProc: TStoredProc;
		FStoredProcName: string;

		procedure FillList( ss: TStrings ); override;
		procedure SetSessionName( const Value: string ); override;
		procedure SetDatabaseName( const Value: string ); override;

	protected
		procedure GetStoredProcList( ss: TStrings ); virtual; abstract;

		property StoredProc: TStoredProc
						 read FStoredProc;

	published
		property StoredProcName: string
						 read FStoredProcName write FStoredProcName;

	end;

{ TKStoredProcFieldsDialog }

	TKStoredProcFieldsDialog = class( TKCustomStoredProcInfoDialog )
	private
		procedure GetStoredProcList( ss: TStrings ); override;

	public
		property StoredProc;

	end;

	TKDBListKind = ( dblkSession, dblkDriver, dblkAlias );

	TKDBInfoListKind = ( dbilkTables, dbilkTableFields, dbilkTableIndexes, dbilkStoredProcs,
		dbilkStoredProcFields );

function DBListDialog( const ACaption, AText: string; DlgKind: TKDBListKind ): string;
function DBListInfoDialog( const ACaption, AText, SessionName, DataBaseName, DataSetName: string;
	DlgInfoKind: TKDBInfoListKind ): string;

{ TKDBLocateDialog }

const

	DEFAULT_LOCATE_OPTIONS = [loPartialKey, loCaseInsensitive];

type

{ TKDBLocateDialog }

	EKDBLocate = EKDDClasses;

	TKDBLocateDialog = class;

	TKDBLocateDialogEvent = procedure( Sender: TKDBLocateDialog; DataSet: TDataSet;
		const AFields: string; const Values: Variant ) of object;

	TKDBLocateDialog = class( TKCustomDualListEditMaskDialog )
	private
		FFields: TStrings;
		FDataSet: TDataSet;
		FLocateOptions: TLocateOptions;
		FAfterLocate: TKDBLocateDialogEvent;
		FBeforeLocate: TKDBLocateDialogEvent;


		procedure SetFields( Value: TStrings );
		procedure SetDateSet( Value: TDataSet );

{ from TKCustomDualListEditMaskDialog }
		property EditMask;
		property EditMaskType;

{ from TKCustomDualListDialog }
		property DestSel;
		property DestList;
		property SourceSel;
		property SourceList;

	protected
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		function DoExecute: Integer; override;
		procedure DoBeforeCheckParams; override;
		function DoCheckParams: Boolean; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		procedure DoBeforeLocate( const AFields: string; const Values: Variant ); dynamic;
		procedure DoAfterLocate( const AFields: string; const Values: Variant ); dynamic;

		procedure FieldListChange( Sender: TObject ); dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

    function Locate: Boolean; dynamic;

	published
{ From TKCustomDualListDialog }
		property AcceptDragDrop;
		property VisibleButtons;
		property ListImages;
		property Flat;
		property MultiSelect;

		property OnSourceListClick;
		property OnDestinationListClick;
		property OnDualListButtonClick;

{ from TKCustomDualListEditDialog }
		property EditShortCut;
		property ClearShortCut;

		property DataSet: TDataSet
						 read FDataSet write SetDateSet;
		property Fields: TStrings
						 read FFields write SetFields;
		property LocateOptions: TLocateOptions
						 read FLocateOptions write FLocateOptions default DEFAULT_LOCATE_OPTIONS;

		property BeforeLocate: TKDBLocateDialogEvent
						 read FBeforeLocate write FBeforeLocate;	
		property AfterLocate: TKDBLocateDialogEvent
						 read FAfterLocate write FAfterLocate;

	end;

{$HINTS ON}
	
function DBLocateDialog( const ACaption: string; ADataSet: TDataSet;
	LocateOptions: TLocateOptions ): Boolean;
function DBLocateDialogEx( const ACaption: string; ADataSet: TDataSet; AFields: TStrings;
	LocateOptions: TLocateOptions ): Boolean;

implementation

uses
	Windows, Controls, Graphics, Forms, StdCtrls, ExtCtrls, Buttons, Dialogs,
	ExtDlgs, ComCtrls, uksyConsts, uksyResStr, ukrDBUtils, ukrClasses,
	ukrCtrls, ukddResStr, ukddUtils;

{
--------------------------------------------------------------------------------
------------------------- Basic DB Dialog Architecture -------------------------
--------------------------------------------------------------------------------
}

{ TKDataDialog }

type

	TKDataDialogForm = class( TKCustomDialogForm )
	private
		FpnAll: TPanel;
		FpnButtons: TPanel;
		FpnControl: TPanel;
		FpnOKCancel: TPanel;
		FpnControlFrame: TPanel;

		FckxFirst: TCheckBox;
		FckxSecond: TCheckBox;

		FbnOK: TBitBtn;
		FbnLoad: TBitBtn;
		FbnSave: TBitBtn;
		FbnClear: TBitBtn;
		FbnCancel: TBitBtn;
		FbnRevert: TBitBtn;

		FControl: TControl;

		function GetDialog: TKDataDialog;

		procedure OKClick( Sender: TObject );
		procedure LoadClick( Sender: TObject );
		procedure SaveClick( Sender: TObject );
		procedure ClearClick( Sender: TObject );
		procedure CancelClick( Sender: TObject );
		procedure RevertClick( Sender: TObject );

		procedure ckxFirstClick( Sender: TObject );
		procedure ckxSecondClick( Sender: TObject );

		procedure FormResize( Sender: TObject );

	protected
		procedure DoOK; virtual; abstract;
		procedure DoClear; virtual; abstract;
		procedure DoCancel; virtual; abstract;
		procedure DoRevert; virtual; abstract;
		procedure DoCbxFirst; virtual; abstract;
		procedure DoCbxSecond; virtual; abstract;

		function GetFilter: string; virtual; abstract;
		function GetDefaultExt: string; virtual; abstract;
		procedure LoadData( const FileName: string ); virtual; abstract;
		procedure SaveData( const FileName: string ); virtual; abstract;

		procedure DoLoad; virtual;
		procedure DoSave; virtual;

		procedure PlaceOKCancel;
		procedure PlaceCheckBoxes;

		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		procedure PrepareForm; override;
		procedure UnprepareForm; override;

		property pnAll: TPanel
						 read FpnAll;
		property pnControlFrame: TPanel
						 read FpnControlFrame;
		property pnButtons: TPanel
						 read FpnButtons;
		property pnControl: TPanel
						 read FpnControl;
		property pnOKCancel: TPanel
						 read FpnOKCancel;

		property ckxFirst: TCheckBox
						 read FckxFirst;
		property ckxSecond: TCheckBox
						 read FckxSecond;

		property bnOK: TBitBtn
						 read FbnOK;
		property bnLoad: TBitBtn
						 read FbnLoad;
		property bnSave: TBitBtn
						 read FbnSave;
		property bnClear: TBitBtn
						 read FbnClear;
		property bnCancel: TBitBtn
						 read FbnCancel;
		property bnRevert: TBitBtn
						 read FbnRevert;

		property Dialog: TKDataDialog
						 read GetDialog;
		property Control: TControl
						 read FControl;
						 
	end;

{ TKImageDialogForm }

	TKImageDialogForm = class( TKDataDialogForm )
	private
		function GetImage: TImage;

	protected
		procedure DoOK; override;
		procedure DoClear; override;
		procedure DoCancel; override;
		procedure DoRevert; override;

		procedure DoCbxFirst; override;
		procedure DoCbxSecond; override;

		procedure PlaceControls; override;
		function GetFilter: string; override;
		function GetDefaultExt: string; override;
		procedure LoadData( const FileName: string ); override;
		procedure SaveData( const FileName: string ); override;

		property Image: TImage
						 read GetImage;

	end;

{ TKMemoDialogForm }

	TKMemoDialogForm = class( TKDataDialogForm )
	private
		function GetMemo: TMemo;

	protected
		procedure DoOK; override;
		procedure DoClear; override;
		procedure DoCancel; override;
		procedure DoRevert; override;

		procedure DoCbxFirst; override;
		procedure DoCbxSecond; override;

		procedure PlaceControls; override;
		function GetFilter: string; override;
		function GetDefaultExt: string; override;
		procedure LoadData( const FileName: string ); override;
		procedure SaveData( const FileName: string ); override;

		property Memo: TMemo
						 read GetMemo;

	end;

const
	DATA_CKX_DY = 2;
	DATA_BTN_DY = 45;
	DATA_BTN_DDY = 25;
	DATA_BTN_TOPMOST = 15;

function TKDataDialogForm.GetDialog: TKDataDialog;
begin
	Result := ( inherited Dialog as TKDataDialog );
end;

procedure TKDataDialogForm.FormResize;
begin
	PlaceOKCancel;
	PlaceCheckBoxes;
end;

procedure TKDataDialogForm.PrepareForm;
const
	DIALOG_CONTROL_CLASS: array[TKDataDialogMode] of TControlClass =
		( TMemo, TRichEdit, TImage );
var
	bEnabled: Boolean;
begin
	Width := ScaleX( 480 );
	Height := ScaleX( 340 );
	OnShow := FormResize;
	OnResize := FormResize;

	FpnAll := AddControl( 'pnAll', ED_RECT, true, true, Self, TPanel ) as TPanel;
	FpnOKCancel := AddControl( 'pnOKCancel', ED_RECT, true, true, FpnAll, TPanel ) as TPanel;
	FpnButtons:= AddControl( 'pnButtons', ED_RECT, true, true, FpnAll, TPanel ) as TPanel;
	FpnControlFrame := AddControl( 'pnControlFrame', ED_RECT, true, true, FpnAll, TPanel ) as TPanel;
	FpnControl := AddControl( 'pnControl', ED_RECT, true, true, FPnControlFrame, TPanel ) as TPanel;

	FControl := AddControl( 'dlgControl', ED_RECT, true, true, FpnControl,
		DIALOG_CONTROL_CLASS[Dialog.DialogMode] );

	bEnabled := ( ddoAllowCheckBoxes in Dialog.Options );

	FckxFirst := AddControl( 'ckxFirst', BN_RECT, bEnabled, true, pnButtons, TCheckBox ) as TCheckBox;
	FckxFirst.OnClick := ckxFirstClick;

	FckxSecond := AddControl( 'ckxSecond', BN_RECT, bEnabled, true, pnButtons, TCheckBox ) as TCheckBox;
	FckxSecond.OnClick := ckxSecondClick;

	bEnabled := Dialog.CanModify;

	FbnOK := AddButton( sCapOK, 'bnOK', BN_RECT, true, true, true, mrNone, nil,
		pnOKCancel, OKClick ) as TBitBtn;
	FbnCancel := AddButton( sCapCancel, 'bnCancel', BN_RECT, true, true, false,
		mrNone, nil, pnOKCancel, CancelClick ) as TBitBtn;

	FbnLoad := AddButton( sLoad, 'bnLoad', BN_RECT, bEnabled, ( ddoLoadVisible in Dialog.Options ),
		false, mrNone, nil, pnButtons, LoadClick ) as TBitBtn;
	FbnLoad.Tag := Integer( ddoLoadVisible );
	FbnSave := AddButton( sSave, 'bnSave', BN_RECT, bEnabled, ( ddoSaveVisible in Dialog.Options ),
		false, mrNone, nil, pnButtons, SaveClick ) as TBitBtn;
	FbnSave.Tag := Integer( ddoSaveVisible );
	FbnClear := AddButton( sClear, 'bnClear', BN_RECT, bEnabled, ( ddoClearVisible in Dialog.Options ),
		false, mrNone, nil, pnButtons, ClearClick ) as TBitBtn;
	FbnClear.Tag := Integer( ddoClearVisible );
	FbnRevert := AddButton( sRevert, 'bnRevert', BN_RECT, bEnabled, ( ddoRevertVisible in Dialog.Options ),
		false, mrNone, nil, pnButtons, RevertClick ) as TBitBtn;
	FbnRevert.Tag := Integer( ddoRevertVisible );
end;

procedure TKDataDialogForm.UnprepareForm;
begin
end;

procedure TKDataDialogForm.PlaceCheckBoxes;
begin
	with ckxFirst do
	begin
		Left := ( Parent.Width - Width ) div 2;
		Top := ( Parent.Height - 2 * Height );
	end;
	with ckxSecond do
	begin
		Left := ckxFirst.Left;
		Width := ckxFirst.Width + 4;
		Top := ckxFirst.Top + ckxFirst.Height;
	end;
end;

procedure TKDataDialogForm.PlaceOKCancel;
begin
	with bnOK do
	begin
		Top := ( Parent.Height - Height ) div 2;
		Left := ( ( Parent.Width - 2 * ScaleX( BN_DISTANCE ) ) div 2 ) - Width;
	end;
	with bnCancel do
	begin
		Top := bnOK.Top;
		Left := ( Parent.Width + 2 * ScaleX( BN_DISTANCE ) ) div 2;
	end;
end;

procedure TKDataDialogForm.PlaceControls;
begin
	FControl.Align := alClient;
	with pnAll do
	begin
		Caption := '';
		Align := alClient;
		BevelInner := bvNone;
		BevelOuter := bvNone;
	end;
	with pnOKCancel do
	begin
		Height := 52;
		Caption := '';
		Align := alBottom;
		BevelInner := bvRaised;
		BevelOuter := bvLowered;
	end;
	with pnButtons do
	begin
		Width := 114;
		Caption := '';
		Align := alRight;
		BevelInner := bvNone;
		BevelOuter := bvNone;
	end;
	with pnControlFrame do
	begin
		Caption := '';
		BorderWidth := 4;
		Align := alClient;
		BevelInner := bvNone;
		BevelOuter := bvNone;
	end;
	with pnControl do
	begin
		Caption := '';
		Color := clWhite;
		Align := alClient;
		BevelInner := bvRaised;
		BevelOuter := bvLowered;
	end;
end;

procedure TKDataDialogForm.PlaceButtons;
var
	i: Integer;
	bnLastBtn: TControl;
begin
  bnLastBtn := nil;
	for i := 0 to pnButtons.ControlCount - 1 do
		if ( CheckObjectClass( pnButtons.Controls[i], TBitBtn ) and
				 pnButtons.Controls[i].Visible ) then
		begin
			if ( ( i <> 0 ) and CheckObject( bnLastBtn ) ) then
			begin
				pnButtons.Controls[i].Top := bnLastBtn.Top + ScaleY( DATA_BTN_DY );
				pnButtons.Controls[i].Left := bnLastBtn.Left;
			end
			else
				with pnButtons.Controls[i] do
				begin
					Top := DATA_BTN_TOPMOST;
					Left := ( Parent.Width - Width ) div 2;
				end;
			bnLastBtn := pnButtons.Controls[i];
		end;
	PlaceOKCancel;
	PlaceCheckBoxes;
end;

procedure TKDataDialogForm.OKClick( Sender: TObject );
begin
	if Assigned( Dialog.OnOkClick ) then
		Dialog.DoOk
	else
		DoOK;
end;

procedure TKDataDialogForm.LoadClick( Sender: TObject );
begin
	if Assigned( Dialog.OnLoad ) then
		Dialog.DoLoad
	else
		DoLoad;
end;

procedure TKDataDialogForm.SaveClick( Sender: TObject );
begin
	if Assigned( Dialog.OnSave ) then
		Dialog.DoSave
	else
		DoSave;
end;

procedure TKDataDialogForm.ClearClick( Sender: TObject );
begin
	if Assigned( Dialog.OnClear ) then
		Dialog.DoClear
	else
		DoClear;
end;

procedure TKDataDialogForm.CancelClick( Sender: TObject );
begin
	if Assigned( Dialog.OnCancel ) then
		Dialog.DoCancel
	else
		DoCancel;
end;

procedure TKDataDialogForm.RevertClick( Sender: TObject );
begin
	if Assigned( Dialog.OnRevert ) then
		Dialog.DoRevert
	else
		DoRevert;
end;

procedure TKDataDialogForm.ckxFirstClick( Sender: TObject );
begin
	DoCbxFirst;
end;

procedure TKDataDialogForm.ckxSecondClick( Sender: TObject );
begin
	DoCbxSecond;
end;

type

	TOpenDialogClass = class of TOpenDialog;

procedure TKDataDialogForm.DoLoad;
const
	DIALOG_CLASS: array[TKDataDialogMode] of TOpenDialogClass =
		( TOpenDialog, TOpenDialog, TOpenPictureDialog );
var
	od: TOpenDialog;
begin
	od := DIALOG_CLASS[Dialog.DialogMode].Create( nil );
	try
		with od do
		begin
			Filter := GetFilter;
			DefaultExt := GetDefaultExt;
			FilterIndex := 0;
			if Execute then
				LoadData( FileName );
		end;
	finally
		od.Free;
	end;
end;

procedure TKDataDialogForm.DoSave;
const
	DIALOG_CLASS: array[TKDataDialogMode] of TOpenDialogClass =
		( TSaveDialog, TSaveDialog, TSavePictureDialog );
var
	sd: TOpenDialog;
begin
	sd := DIALOG_CLASS[Dialog.DialogMode].Create( nil );
	try
		with sd do
		begin
			Filter := GetFilter;
			DefaultExt := GetDefaultExt;
			FilterIndex := 0;
			if Execute then
				SaveData( FileName );
		end;
	finally
		sd.Free;
	end;
end;

{ TKImageDialogForm }

function TKImageDialogForm.GetImage: TImage;
begin
	Result := ( Control as TImage );
end;

procedure TKImageDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	Image.Picture.Assign( Dialog.Field );
	ckxFirst.Caption := sImageStretch;
	ckxFirst.Checked := ( ddoStrech in Dialog.Options );
	ckxSecond.Caption := sImageCenter;
	ckxSecond.Checked := ( ddoCenter in Dialog.Options );
end;

procedure TKImageDialogForm.DoOK;
begin
	if Dialog.CanModify then
	begin
		if ( not ( Dialog.Field.DataSet.State in dsEditModes ) ) then
			Dialog.Field.DataSet.Edit;
		if ( ( not CheckObject( Image.Picture.Graphic ) ) or Image.Picture.Graphic.Empty ) then
			Dialog.Field.Clear
		else
			{ SHOULD NOT SUPPORT JPeg images for Delphi Profissional/Standard compatibility

			if CheckObjectClass( Image.Picture.Graphic, TJPEGImage ) then
			begin
				bmp := TBitmap.Create;
				try
					bmp.Assign( Image.Picture.Graphic );
					Image.Picture.Assign( bmp );
				finally
					bmp.Free;
				end;
			end;

			}
			Dialog.Field.Assign( Image.Picture );
	end;
	ModalResult := mrOK;
end;

procedure TKImageDialogForm.DoClear;
begin
	Image.Picture.Assign( nil );
end;

procedure TKImageDialogForm.DoCancel;
begin
	ModalResult := mrCancel;
end;

procedure TKImageDialogForm.DoRevert;
begin
	Image.Picture.Assign( Dialog.Field );
end;

function TKImageDialogForm.GetFilter: string;
begin
	Result := sImageFilter;
end;

function TKImageDialogForm.GetDefaultExt: string;
begin
	Result := sImageDefExt;
end;

procedure TKImageDialogForm.LoadData( const FileName: string );
begin
	Image.Picture.LoadFromFile( FileName );
end;

procedure TKImageDialogForm.SaveData( const FileName: string );
begin
	Image.Picture.SaveToFile( FileName );
end;

procedure TKImageDialogForm.DoCbxFirst;
begin
	Image.Stretch := ckxFirst.Checked;
	if ( ddoChangeProps in Dialog.Options ) then
		if ckxFirst.Checked then
			Dialog.Options := Dialog.Options + [ddoStrech]
		else
			Dialog.Options := Dialog.Options - [ddoStrech];
end;

procedure TKImageDialogForm.DoCbxSecond;
begin
	Image.Center := ckxSecond.Checked;
	if ( ddoChangeProps in Dialog.Options ) then
		if ckxSecond.Checked then
			Dialog.Options := Dialog.Options + [ddoCenter]
		else
			Dialog.Options := Dialog.Options - [ddoCenter];
end;

{ TKMemoDialogForm }

function TKMemoDialogForm.GetMemo: TMemo;
begin
	Result := ( Control as TMemo );
end;

procedure TKMemoDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	Memo.ReadOnly := ( not Dialog.CanModify );
	Memo.BorderStyle := bsNone;
	Memo.Lines.Assign( Dialog.Field );
	ckxFirst.Caption := sMemoScrollVert;
	ckxSecond.Caption := sMemoWordWraps;
	ckxFirst.Checked := ( ddoScrollVert in Dialog.Options );
	ckxSecond.Checked := ( ddoWordWrap in Dialog.Options );
  ActiveControl := Memo;
end;

procedure TKMemoDialogForm.DoOK;
begin
	if Dialog.CanModify then
	begin
		if ( not ( Dialog.Field.DataSet.State in dsEditModes ) ) then
			Dialog.Field.DataSet.Edit;
		if ( not ( CheckStrings( Memo.Lines ) and
			 CheckTrimStr( StringReplace( Memo.Lines.Text, CH_CRLF, '', krfAll ) ) ) ) then
			Dialog.Field.Clear
		else
			Dialog.Field.Assign( Memo.Lines );
	end;
	ModalResult := mrOK;
end;

procedure TKMemoDialogForm.DoClear;
begin
	Memo.Lines.Clear;
end;

procedure TKMemoDialogForm.DoCancel;
begin
	ModalResult := mrCancel;
end;

procedure TKMemoDialogForm.DoRevert;
begin
	Memo.Lines.Assign( Dialog.Field );
end;

function TKMemoDialogForm.GetFilter: string;
begin
	Result := sMemoFilter;
end;

function TKMemoDialogForm.GetDefaultExt: string;
begin
	Result := sMemoDefExt;
end;

procedure TKMemoDialogForm.LoadData( const FileName: string );
begin
	Memo.Lines.LoadFromFile( FileName );
end;

procedure TKMemoDialogForm.SaveData( const FileName: string );
begin
	Memo.Lines.SaveToFile( FileName );
end;

const
	SCROLLS: array[Boolean, Boolean] of TScrollStyle =
	(
		( ssNone, ssVertical ),
		( ssHorizontal, ssBoth )
	);

procedure TKMemoDialogForm.DoCbxFirst;
begin
	Memo.ScrollBars := SCROLLS[( not ckxSecond.Checked ), ckxFirst.Checked];
	if ( ddoChangeProps in Dialog.Options ) then
		if ckxFirst.Checked then
			Dialog.Options := Dialog.Options + [ddoScrollVert]
		else
			Dialog.Options := Dialog.Options - [ddoScrollVert];
end;

procedure TKMemoDialogForm.DoCbxSecond;
begin
	Memo.WordWrap := ckxSecond.Checked; { ?? }
	Memo.ScrollBars := SCROLLS[( not ckxSecond.Checked ), ckxFirst.Checked];
	if ( ddoChangeProps in Dialog.Options ) then
		if ckxSecond.Checked then
			Dialog.Options := Dialog.Options + [ddoWordWrap]
		else
			Dialog.Options := Dialog.Options - [ddoWordWrap];
end;

{ TKDataDialog }

constructor TKDataDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDetecting := false;
	FAutoDetect := false;
	DialogUnits := false;
	DialogMode := dmMemo;
	FDataLink := TFieldDataLink.Create;
	FDataLink.Control := Self;
	Options := DEFAULT_DATA_DIALOG_OPT;
end;

destructor TKDataDialog.Destroy;
begin
	FField := nil;
	FDataLink.Free;
	inherited Destroy;
end;

function TKDataDialog.GetField: TBlobField;
begin
	if CheckObject( FField ) then
		Result := FField
	else
		Result := ( FDataLink.Field as TBlobField );
end;

procedure TKDataDialog.DetectDialogMode;
begin
	if ( FAutoDetect and CheckObject( Field ) ) then
	begin
		FDetecting := true;
		try
			if ( ( Field is TMemoField ) or ( Field.BlobType = ftMemo ) ) then
				SetDialogMode( dmMemo )
			else if ( ( Field is TGraphicField ) or ( Field.BlobType = ftGraphic ) ) then
				SetDialogMode( dmGraphic )
			else
				SetDialogMode( dmRichEdit );
		finally
			FDetecting := false;
		end;
	end;
end;

procedure TKDataDialog.SetOptions( Value: TKDataDialogOptions );
begin
	if ( Value <> FOptions ) then
	begin
		FOptions := Value;
		if ( ddoSizeable in FOptions ) then
			SizeStyle := ssLimittedSizing
		else
		  SizeStyle := ssNoSizing;
	end;
end;

procedure TKDataDialog.SetAutoDetect( Value: Boolean );
begin
	FAutoDetect := Value;
	DetectDialogMode;
end;

procedure TKDataDialog.SetDialogMode( Value: TKDataDialogMode );
const
	DIALOGFORM_CLASS: array[TKDataDialogMode] of TKCustomDialogFormClass =
	(
		TKMemoDialogForm,
		TKMemoDialogForm,
		TKImageDialogForm
	);
begin
	if ( FAutoDetect and ( not FDetecting ) ) then
		Exit;
	FDialogMode := Value;
	FormClass := DIALOGFORM_CLASS[Value];
end;

function  TKDataDialog.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

function  TKDataDialog.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDataDialog.SetDataField( const Value: string );
begin
	FDataLink.FieldName := Value;
	DetectDialogMode;
end;

procedure TKDataDialog.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

procedure TKDataDialog.DefaultLimits;
begin
	with Limits do
	begin
		MaxWidth := 480;
		MinWidth := 480;
		MaxHeight := 340;
		MinHeight := 340;
	end;
end;

function TKDataDialog.DoCheckParams: Boolean;
begin
	Result := CheckObject( Field ) and ( inherited DoCheckParams );
end;

procedure TKDataDialog.DoBeforeExecute;
begin
	inherited DoBeforeExecute;
	with Form, FormPainter do
	begin
		Width := ScaleX( Limits.MinWidth );
		Height := ScaleY( Limits.MinHeight );
		Limits.MaxWidth := MulDiv( Screen.Width, 4, 5 );
		Limits.MaxHeight := MulDiv( Screen.Height, 4, 5 );
		with WindowManager.MaxTrack do
		begin
			Width := ScaleX( Limits.MaxWidth );
			Height := ScaleY( Limits.MaxHeight );
		end;
		with WindowManager.MinTrack do
		begin
			Width := ScaleX( Limits.MinWidth );
			Height := ScaleY( Limits.MinHeight );
		end;
	end;
end;

function TKDataDialog.EditBlob( AField: TBlobField ): Boolean;
begin
	FField := AField;
	try
		DetectDialogMode;
		Result := Execute;
	finally
		FField := nil;
	end;
end;

function TKDataDialog.CanModify: Boolean;
begin
	Result := ( CheckObject( Field ) and ( not ( CheckDataSetReadOnly( Field.DataSet ) or Field.ReadOnly ) ) );
end;

procedure TKDataDialog.DoOK;
begin
	if Assigned( FOnOkClick ) then
		FOnOkClick( Self );
end;

procedure TKDataDialog.DoCancel;
begin
	if Assigned( FOnCancel ) then
		FOnCancel( Self );
end;

procedure TKDataDialog.DoLoad;
begin
	if Assigned( FOnLoad ) then
		FOnLoad( Self );
end;

procedure TKDataDialog.DoSave;
begin
	if Assigned( FOnSave ) then
		FOnSave( Self );
end;

procedure TKDataDialog.DoClear;
begin
	if Assigned( FOnClear ) then
		FOnClear( Self );
end;

procedure TKDataDialog.DoRevert;
begin
	if Assigned( FOnRevert ) then
		FOnRevert( Self );
end;

procedure TKDataDialog.Load;
begin
	ForceExecuting;
	( Form as TKDataDialogForm ).DoLoad;
end;

procedure TKDataDialog.Save;
begin
	ForceExecuting;
	( Form as TKDataDialogForm ).DoSave;
end;

procedure TKDataDialog.Clear;
begin
	ForceExecuting;
	( Form as TKDataDialogForm ).DoClear;
end;

procedure TKDataDialog.Revert;
begin
	ForceExecuting;
	( Form as TKDataDialogForm ).DoRevert;
end;

function DataDialog( const ACaption: string; AField: TBlobField ): Boolean;
begin
	Result := DataDialogEx( ACaption, DEFAULT_DATA_DIALOG_OPT, AField );
end;

function DataDialogEx( const ACaption: string; Options: TKDataDialogOptions;
	AField: TBlobField ): Boolean;
var
	dtdlg: TKDataDialog;
begin
	ForceObject( AField );
	dtdlg := TKDataDialog.Create( nil );
	try
		dtdlg.Options := Options;
		dtdlg.Caption := ACaption;
		dtdlg.AutoDetect := True;
		Result := dtdlg.EditBlob( AField );
	finally
		dtdlg.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------ DB Entities Dialog Architecture -----------------------
--------------------------------------------------------------------------------
}

{ TKCustomDBListDialog }

constructor TKCustomDBListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	EditText := '';
	UserInput := False;
  FAutoRefresh := True;
end;

function TKCustomDBListDialog.GetItemCount: Cardinal;
begin
	Result := inherited Items.Count;
end;

function TKCustomDBListDialog.GetItems( Index: Cardinal ): string;
begin
	Result := inherited Items[Index];
end;

procedure TKCustomDBListDialog.Clear;
begin
  inherited Items.Clear;
end;

procedure TKCustomDBListDialog.Refresh;
begin
  FillList( inherited Items );
end;

procedure TKCustomDBListDialog.DoBeforeExecute;
begin
  if ( ( ItemCount = 0 ) or AutoRefresh ) then
    Refresh;    
	inherited DoBeforeExecute;
end;

{ TKDBSessionListDialog }

procedure TKDBSessionListDialog.FillList( ss: TStrings );
begin
	Sessions.GetSessionNames( ss );
end;

{ TKAliasListDialog }

constructor TKAliasListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FPersistent := True;
end;

procedure TKAliasListDialog.FillList( ss: TStrings );
begin
	if FPersistent then
		Session.GetAliasNames( ss )
	else
		Session.GetDataBaseNames( ss );
end;

{ TKDBDriverListDialog }

procedure TKDBDriverListDialog.FillList( ss: TStrings );
begin
  Session.GetDriverNames( ss );
end;

{ TKCustomDBInfoDialog }

constructor TKCustomDBInfoDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSessionName := Session.SessionName;
end;

function TKCustomDBInfoDialog.GetSession: TSession;
begin
	Result := Sessions.FindSession( SessionName );
	ForceObject( Result );
	Result.Open;
end;

procedure TKCustomDBInfoDialog.SetSessionName( const Value: string );
begin
	if ( not CheckStrEqual( Value, FSessionName ) ) then
	begin
		FDatabaseName := '';
		if CheckStr( Value ) then
			ForceSessionName( Value );
		FSessionName := Value;
	end;
end;

procedure TKCustomDBInfoDialog.SetDatabaseName( const Value: string );
begin
	if ( not CheckStrEqual( Value, FDatabaseName ) ) then
	begin
		if CheckStr( Value ) then
		  ForceDatabaseName( Value );
		FDatabaseName := Value;
	end;
end;

{ TKTableListDialog }

constructor TKTableListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FExetensions := True;
	FSystemTables := False;
	FFilter := '*.*';
end;

procedure TKTableListDialog.FillList( ss: TStrings );
begin
  ForceTrimStr( Filter );
  ForceDataBaseName( DataBaseName );
	GetSession.GetTableNames( DataBaseName, Filter, Extensions, SystemTables, ss );
end;

{ TKStoredProcListDialog }

procedure TKStoredProcListDialog.FillList( ss: TStrings );
begin
	ForceTrimStr( DataBaseName );
	GetSession.GetStoredProcNames( DatabaseName, ss );
end;

{ TKCustomTableInfoDialog }

procedure TKCustomTableInfoDialog.SetSessionName( const Value: string );
begin
	if ( not CheckStrEqual( Value, SessionName ) ) then
	begin
		inherited SetSessionName( Value );
		FTableName := '';
	end;
end;

procedure TKCustomTableInfoDialog.SetDatabaseName( const Value: string );
begin
	if ( not CheckStrEqual( Value, DatabaseName ) ) then
	begin
		inherited SetDatabaseName( Value );
		FTableName := '';
	end;
end;

procedure TKCustomTableInfoDialog.FillList( ss: TStrings );
begin
	ForceTableName( DataBaseName, FTableName );
	FTable := TTable.Create( nil );
	try
		FTable.SessionName := SessionName;
		FTable.DatabaseName := DatabaseName;
		FTable.TableName := FTableName;
		FTable.IndexDefs.Update;
		FTable.FieldDefs.Update;
		GetTableList( ss );
	finally
		FreeClean( FTable );
	end;
end;

{ TKTableFieldsDialog }

procedure TKTableFieldsDialog.GetTableList( ss: TStrings );
var
	i: Integer;
begin
	for i := 0 to Table.FieldDefs.Count - 1 do
		ss.Add( Table.FieldDefs[i].Name );
end;

{ TKTableIndexDialog }

procedure TKTableIndexDialog.GetTableList( ss: TStrings );
var
	i: Integer;
begin
	for i := 0 to Table.IndexDefs.Count - 1 do
		ss.Add( Table.IndexDefs[i].Name );
end;

{ TKCustomStoredProcInfoDialog }

procedure TKCustomStoredProcInfoDialog.SetSessionName( const Value: string );
begin
	if ( not CheckStrEqual( Value, SessionName ) ) then
	begin
		inherited SetSessionName( Value );
		FStoredProcName := '';
	end;
end;

procedure TKCustomStoredProcInfoDialog.SetDatabaseName( const Value: string );
begin
	if ( not CheckStrEqual( Value, DatabaseName ) ) then
	begin
		inherited SetDatabaseName( Value );
		FStoredProcName := '';
	end;
end;

procedure TKCustomStoredProcInfoDialog.FillList( ss: TStrings );
begin
  ForceStoredProcName( DataBaseName, FStoredProcName );
	FStoredProc := TStoredProc.Create( nil );
	try
		FStoredProc.SessionName := SessionName;
		FStoredProc.DatabaseName := DatabaseName;
		FStoredProc.StoredProcName := FStoredProcName;
		FStoredProc.FieldDefs.Update;
		GetStoredProcList( ss );
	finally
		FreeClean( FStoredProc );
	end;
end;

{ TKStoredProcFieldsDialog }

procedure TKStoredProcFieldsDialog.GetStoredProcList( ss: TStrings );
var
	i: Integer;
begin
	for i := 0 to StoredProc.FieldDefs.Count - 1 do
		ss.Add( StoredProc.FieldDefs[i].Name );
end;

function DBListDialog( const ACaption, AText: string; DlgKind: TKDBListKind ): string;
const
	DIALOG_KIND_CLASS: array[TKDBListKind] of TKCustomDBListDialogClass =
		( TKDBSessionListDialog, TKDBDriverListDialog, TKAliasListDialog );
var
	dlg: TKCustomDBListDialog;
begin
	Result := '';
	dlg := DIALOG_KIND_CLASS[DlgKind].Create( nil );
	try
		dlg.Text := AText;
		dlg.Caption := ACaption;
		if ( dlg.Execute and ( dlg.ItemIndex <> -1 ) ) then
			Result := dlg.Items[dlg.ItemIndex];
	finally
		dlg.Free;
	end;
end;

function DBListInfoDialog( const ACaption, AText, SessionName, DataBaseName, DataSetName: string;
	DlgInfoKind: TKDBInfoListKind ): string;
const
	DIALOG_KIND_CLASS: array[TKDBInfoListKind] of TKCustomDBInfoDialogClass =
		( TKTableListDialog, TKTableFieldsDialog, TKTableIndexDialog, TKTableIndexDialog,
			TKStoredProcFieldsDialog );
var
	dlg: TKCustomDBInfoDialog;
begin
	case DlgInfoKind of
		dbilkTables,
		dbilkStoredProcs: ForceDataBaseName( DataBaseName );
		dbilkTableFields,
		dbilkTableIndexes: ForceTableName( DataBaseName, DataSetName );
		dbilkStoredProcFields: ForceStoredProcName( DataBaseName, DataSetName );
	end;
	Result := '';
	dlg := DIALOG_KIND_CLASS[DlgInfoKind].Create( nil );
	try
	  dlg.Text := AText;
		dlg.Caption := ACaption;
		dlg.SessionName := GetFirstString( [SessionName, Session.SessionName] );
		dlg.DatabaseName := DataBaseName;
		case DlgInfoKind of
			dbilkTableFields,
			dbilkTableIndexes: ( dlg as TKCustomTableInfoDialog ).TableName := DataSetName;
			dbilkStoredProcFields: ( dlg as TKCustomStoredProcInfoDialog ).StoredProcName := DataSetName;
		end;
		if ( dlg.Execute and ( dlg.ItemIndex <> -1 ) ) then
			Result := dlg.Items[dlg.ItemIndex];
	finally
		dlg.Free;
	end;
end;

{ TKDBLocateDialog }

{--------------------------- Internal Implementation ---------------------------}

const

	CB_GUTTER = 10;
	CB_TOP = 0;
	CB_LEFT = 0;
	CB_HEIGHT = 16;
	CB_WIDTH = 0;

	CB_RECT: TRect =
	(
		Left:   0;
		Top:    0;
		Right:  CB_WIDTH;
		Bottom: CB_HEIGHT;
	);

	LB_GUTTER = 3;

{
	SPT_GUTTER = 5;
	SPT_HEIGHT = 24;
	SPT_WIDTH = 140;

	SPT_RECT: TRect =
	(
		Left:   0;
		Top:    0;
		Right:  SPT_WIDTH;
		Bottom: SPT_HEIGHT;
	);
}

	BTN_GUTTER_Y = 15;
	BTN_GUTTER_X = BTN_GUTTER_Y;

	VALID_FIELD_TYPES = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean,
		ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime];

type

{ TKDBLocateDialogForm }

	TKDBLocateDialogForm = class( TKCustomDualListEditMaskDialogForm )
	private
		FlbPartial: TLabel;
		FlbCaseSensitive: TLabel;
		FcbPartial: TCheckBox;
		FcbCaseSensitive: TCheckBox;

		function GetDialog: TKDBLocateDialog;

	protected
{ Form Methods derived from TKCustomDialogForm }
		procedure PlaceControls; override;
		procedure	UnprepareForm; override;

	public
		property Dialog: TKDBLocateDialog
						 read GetDialog;

		property lbPartial: TLabel
						 read FlbPartial;
		property lbCaseSensitive: TLabel
						 read FlbCaseSensitive;
		property cbPartial: TCheckBox
						 read FcbPartial;
		property cbCaseSensitive: TCheckBox
						 read FcbCaseSensitive;

	end;

{ Private Methods }

function TKDBLocateDialogForm.GetDialog: TKDBLocateDialog;
begin
	Result := TKDBLocateDialog( inherited Dialog );
end;

{ Form Methods derived from TKCustomDualListDialogForm }

procedure TKDBLocateDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	FcbPartial := AddControl( 'cbPartial', CB_RECT, True, True, Self, TCheckBox ) as TCheckBox;
	FcbCaseSensitive := AddControl( 'cbCaseSensitive', CB_RECT, True, True, Self, TCheckBox ) as TCheckBox;
	FlbPartial := AddControl( 'lbPartial', CB_RECT, True, True, Self, TLabel ) as TLabel;
	FlbCaseSensitive := AddControl( 'lbPartial', CB_RECT, True, True, Self, TLabel ) as TLabel;
	with FcbPartial do
	begin
		Top := SrcList.Top + SrcList.Height + ScaleY( CB_GUTTER );
		Left := SrcList.Left;
		Height := ( GetSystemMetrics( SM_CYMENUCHECK ) - ScaleY( LB_GUTTER ) );
		Width := ( GetSystemMetrics( SM_CXMENUCHECK ) - ScaleX( LB_GUTTER ) );//( CB_GUTTER div 2 ) + 1 ) );
		Caption := '';
		Checked := ( loPartialKey in Dialog.LocateOptions );
	end;
	with FlbPartial do
	begin
		AutoSize := True;
		Transparent := True;
		FocusControl := FcbPartial;
		Top := FcbPartial.Top - ScaleX( LB_GUTTER - 1 );
		Left := ( FcbPartial.Left + FcbPartial.Width + ScaleY( LB_GUTTER ) );
		Caption := sDBPartial;
	end;
	with FcbCaseSensitive do
	begin
		Top := FcbPartial.Top + FcbPartial.Height + ScaleY( CB_GUTTER );
		Left := SrcList.Left;
		Caption := '';
		Height := ( GetSystemMetrics( SM_CYMENUCHECK ) - ScaleY( LB_GUTTER ) );
		Width := ( GetSystemMetrics( SM_CXMENUCHECK ) - ScaleX( LB_GUTTER ) );
		Checked := ( not ( loCaseInsensitive in Dialog.LocateOptions ) );
	end;
	with FlbCaseSensitive do
	begin
		AutoSize := True;
		Transparent := True;
		FocusControl := FcbCaseSensitive;
		Top := FcbCaseSensitive.Top - ScaleX( LB_GUTTER - 1 );
		Left := ( FcbCaseSensitive.Left + FcbCaseSensitive.Width + ScaleY( LB_GUTTER ) );
		Caption := sDBCaseSentivie;
	end;
	lbSourceList.Caption := sDBSourceList;
	lbDestList.Caption := sDBDestList;
	ClientHeight := FcbCaseSensitive.Top + FcbCaseSensitive.Height + ScaleY( CB_GUTTER );
end;

procedure	TKDBLocateDialogForm.UnprepareForm;
begin
	Dialog.LocateOptions := [];
	if FcbPartial.Checked  then
		Dialog.LocateOptions := [loPartialKey];
	if ( not FcbCaseSensitive.Checked ) then
		Dialog.LocateOptions := Dialog.LocateOptions + [loCaseInsensitive];
	inherited UnprepareForm;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKDBLocateDialog }

constructor TKDBLocateDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FFields := TKStrings.Create;
	TStringList( FFields ).OnChange := FieldListChange;
	FLocateOptions := DEFAULT_LOCATE_OPTIONS;
	TStringList( DestList ).OnChange := nil;
	TStringList( SourceList ).OnChange := nil;
	EditMaskType := emtCustom;
end;

destructor TKDBLocateDialog.Destroy;
begin
	FFields.Free;
	inherited Destroy;
end;

procedure TKDBLocateDialog.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and ( AComponent = FDataSet ) then
		FDataSet := nil;
end;

function TKDBLocateDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKDBLocateDialogForm;
end;

procedure TKDBLocateDialog.DoBeforeCheckParams;
var
	i: Integer;
	fld: TField;
begin
	inherited DoBeforeCheckParams;
	if ( not CheckStrings( FFields ) ) then
	begin
		ForceDataset( DataSet );
		TStringList( FFields ).OnChange := nil;
		try
			with FDataSet do
				for i := 0 to FieldCount - 1 do
					if ( Fields[i].DataType in VALID_FIELD_TYPES ) then
						FFields.Add( Fields[i].FieldName );
		finally
			TStringList( FFields ).OnChange := FieldListChange;
		end;
	end;
	for i := 0 to FFields.Count - 1 do
	begin
		fld := FDataSet.FieldByName( FFields[i] );
		SourceList.AddObject( GetFirstString( [fld.DisplayName, fld.FieldName] ), TObject( Integer( i ) ) );
	end;
{ EditMask := fld.EditMask;... for each field }
end;

function TKDBLocateDialog.DoExecute: Integer;
const
	LOCATE_RESULT: array[Boolean] of TModalResult = ( mrCancel, mrOk );
begin
	Result := inherited DoExecute;
	if ( Result = mrOk ) then
		Result := LOCATE_RESULT[Locate];
end;

function TKDBLocateDialog.DoCheckParams: Boolean;
begin
	Result := ( CheckDataSet( FDataSet ) and ( inherited DoCheckParams ) );
end;

procedure TKDBLocateDialog.FieldListChange( Sender: TObject );
begin
	if ( not CheckStrings( FFields ) ) then
		Exit;
	TStringList( Sender ).OnChange := nil;
	try
		ForceDataSet( FDataSet );
		ForceFieldsEx( FDataSet, TKStrings( FFields ), VALID_FIELD_TYPES, stStrings );
	finally
		TStringList( Sender ).OnChange := FieldListChange;
	end;
end;

procedure TKDBLocateDialog.SetFields( Value: TStrings );
begin
	ForceDataSet( FDataSet );
	FFields.Assign( Self );
end;

procedure TKDBLocateDialog.SetDateSet( Value: TDataSet );
begin
	if ( FDataSet <> Value ) then
	begin
		FDataSet := Value;
		FFields.Clear;
		if CheckObject( Value ) then
			Value.FreeNotification( Self );
	end;
end;

function TKDBLocateDialog.Locate: Boolean;
var
	i,
	vCount: Integer;
	v: Variant;
	sFields: string;
	bm: TBookMark;
begin
	ForceExecuting;
	Result := False;
	FDataSet.DisableControls;
	try
		bm := FDataSet.GetBookmark;
		try
			FDataSet.First;
			vCount := 0;
			sFields := '';
			for i := 0 to DestList.Count - 1 do
				if CheckStr( DestList.Values[DestList.Names[i]] ) then
				begin
					Inc( vCount );
					sFields := sFields + FFields[Integer( DestList.Objects[i] )] + CH_LIST_TOKEN;
				end;
			if ( vCount > 0 ) then
			begin
				ForceStr( sFields );
				Delete( sFields, Length( sFields ), 1 );
				if ( vCount = 1 ) then
					v := DestList.Values[DestList.Names[0]]
				else
				begin
					v := VarArrayCreate( [0, vCount - 1], varVariant );
					for i := 0 to vCount - 1 do
						v[i] := DestList.Values[DestList.Names[i]];
				end;
			end;
			DoBeforeLocate( sFields, v );
			Result := FDataSet.Locate( sFields, v, LocateOptions );
			DoAfterLocate( sFields, v );
		finally
			if CheckPointer( bm ) then
			begin
				if ( not Result ) then
					FDataSet.GotoBookmark( bm );
				FDataSet.FreeBookmark( bm );
			end;
		end;
	finally
		FDataSet.EnableControls;
	end;
end;

procedure TKDBLocateDialog.DoBeforeLocate( const AFields: string; const Values: Variant );
begin
	if Assigned( FBeforeLocate ) then
		FBeforeLocate( Self, DataSet, AFields, Values );
end;

procedure TKDBLocateDialog.DoAfterLocate( const AFields: string; const Values: Variant );
begin
	if Assigned( FAfterLocate ) then
		FAfterLocate( Self, DataSet, AFields, Values );
end;

function DBLocateDialog( const ACaption: string; ADataSet: TDataSet;
	LocateOptions: TLocateOptions ): Boolean;
begin
	ForceDataSet( ADataSet );
	Result := DBLocateDialogEx( ACaption, ADataSet, nil, LocateOptions );
end;

function DBLocateDialogEx( const ACaption: string; ADataSet: TDataSet; AFields: TStrings;
	LocateOptions: TLocateOptions ): Boolean;
var
	DBLocate: TKDBLocateDialog;
begin
  ForceDataSet( ADataSet );
	DBLocate := TKDBLocateDialog.Create( nil );
	try
		DBLocate.DataSet := ADataSet;
		if CheckObject( AFields ) then
			DBLocate.Fields := AFields;
		DBLocate.LocateOptions := LocateOptions;
		Result := DBLocate.Execute;
	finally
		DBLocate.Free;
	end;
end;

end.
