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

unit ukdcCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, Classes, Graphics, Controls, Menus, StdCtrls, Grids, 
	ExtCtrls, Buttons, ComCtrls, DB, DBCtrls, DBGrids, uksyTypes, uksyShortCuts,
	uksyUtils, ukrDBCtrls, ukrCtrls, ukrConsts;

type

	EKDCCtrls = class( EKDBCtrls );

{
--------------------------------------------------------------------------------
----------------------------- DB DataLink Classes ------------------------------
--------------------------------------------------------------------------------
}

type

	TDataSetStates = set of TDataSetState;

	TKDataSetState = ( kdsInactive, kdsBrowse, kdsEdit, kdsInsert, kdsSetKey,
		kdsCalcFields, kdsFilter );
	TKDataSetStates = set of TKDataSetState;

const

	DEF_ENABLED_STATES = [kdsInactive, kdsBrowse, kdsEdit, kdsInsert, kdsSetKey,
		kdsCalcFields, kdsFilter];

type

	TKSetEnabledEvent = procedure( Sender: TControl; var IsEnabled: Boolean ) of object;

{ TKEnabledStatesDataLink }

	TKEnabledStatesDataLink = class( TDataLink )
	private
		FControl: TControl;
		FEnabledStates: TKDataSetStates;
		FOnSetEnabled: TKSetEnabledEvent;

		procedure SetEnabledStates( Value: TKDataSetStates );

	protected
		procedure ActiveChanged; override;
		procedure EditingChanged; override;
		procedure DataSetChanged; override;

		procedure DataChanged; dynamic;
		procedure DoSetEnabled; dynamic;

		property Control: TControl
						 read FControl;
		property EnabledStates: TKDataSetStates
						 read FEnabledStates write SetEnabledStates;
		property OnSetEnabled: TKSetEnabledEvent
						 read FOnSetEnabled write FOnSetEnabled;

	public
		constructor Create( AControl: TControl; States: TKDataSetStates ); virtual;

	end;

{ TKEnabledStatesFieldDataLink }

	TKEnabledStatesFieldDataLink = class( TFieldDataLink )
	private
		FOnSetEnabled: TKSetEnabledEvent;
		FEnabledStates: TKDataSetStates;

		FOnDataChange: TNotifyEvent;
		FOnEditingChange: TNotifyEvent;
		FOnUpdateData: TNotifyEvent;
		FOnActiveChange: TNotifyEvent;

		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetControl: TControl;

	protected
		procedure DataChanged; dynamic;
		procedure DoSetEnabled; dynamic;

		procedure RecordChanged( Field: TField ); override;
		procedure InternalOnDataChange( Sender: TObject ); dynamic;
		procedure InternalOnEditingChange( Sender: TObject ); dynamic;
		procedure InternalOnUpdateData( Sender: TObject ); dynamic;
		procedure InternalOnActiveChange( Sender: TObject ); dynamic;

		property EnabledStates: TKDataSetStates
						 read FEnabledStates write SetEnabledStates;
		property OnSetEnabled: TKSetEnabledEvent
						 read FOnSetEnabled write FOnSetEnabled;

	public
		constructor Create( AControl: TControl; States: TKDataSetStates ); virtual;

		property Control: TControl
					   read GetControl;
		property OnDataChange: TNotifyEvent
						 read FOnDataChange write FOnDataChange;
		property OnEditingChange: TNotifyEvent
						 read FOnEditingChange write FOnEditingChange;
		property OnUpdateData: TNotifyEvent
						 read FOnUpdateData write FOnUpdateData;
		property OnActiveChange: TNotifyEvent
						 read FOnActiveChange write FOnActiveChange;

	end;

{ TKEnabledStatesDataSourceLink } 

	TKEnabledStatesDataSourceLink = class( TDataSourceLink )
	private
		FEnabledStates: TKDataSetStates;
		FOnSetEnabled: TKSetEnabledEvent;

		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetControl: TDBLookUpControl;

		property Control: TDBLookUpControl { Cool!!! }
						 read GetControl;

	protected
		procedure DataChanged; dynamic;
		procedure DoSetEnabled; dynamic;

    procedure FocusControl( Field: TFieldRef ); override;
		procedure ActiveChanged; override;
		procedure RecordChanged( Field: TField ); override;

		property EnabledStates: TKDataSetStates
						 read FEnabledStates write SetEnabledStates;
		property OnSetEnabled: TKSetEnabledEvent
						 read FOnSetEnabled write FOnSetEnabled;

	public
		constructor Create( AControl: TDBLookUpControl; States: TKDataSetStates ); virtual;

	end;

	TKEnabledStatesListSourceLink = class( TListSourceLink )
	private
		FEnabledStates: TKDataSetStates;
		FOnSetEnabled: TKSetEnabledEvent;

		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetControl: TDBLookUpControl;

		property Control: TDBLookUpControl { Cool!!! }
						 read GetControl;

	protected
		procedure DataChanged; dynamic;
		procedure DoSetEnabled; dynamic;

		procedure ActiveChanged; override;
		procedure DataSetChanged; override;

		property EnabledStates: TKDataSetStates
						 read FEnabledStates write SetEnabledStates;
		property OnSetEnabled: TKSetEnabledEvent
						 read FOnSetEnabled write FOnSetEnabled;

	public
		constructor Create( AControl: TDBLookUpControl; States: TKDataSetStates ); virtual;

	end;

{
--------------------------------------------------------------------------------
------------------------------ DB Control Classes ------------------------------
--------------------------------------------------------------------------------
}

{ TKDBEdit }

	TKDBEdit = class( TDBEdit )
	private
		FNullCommand: TShortCut;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
	published
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBText }

	TKDBText = class( TDBText )
	private

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBLabel3D }

	TKGetTextEvent = procedure ( Sender: TKCustomLabel3D; var AText: string ) of object; 

	TKDBLabel3D = class( TKCustomLabel3D )
	private
		FDataLink: TKEnabledStatesFieldDataLink;
		FOnGetText: TKGetTextEvent;

		function GetField: TField;
		function GetFieldText: string;
		function GetDataField: string;
		function GetDataSource: TDataSource;
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

		procedure CMGetDataLink( var Message: TMessage );
							message CM_GETDATALINK;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property DataLink: TKEnabledStatesFieldDataLink
						 read FDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		function  GetLabelText: string; override;
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor  Destroy; override;

		property Enabled: Boolean
						 read GetEnabled;
		property Field: TField
						 read GetField;

	published
		property Align;
		property Alignment;
		property Color default clWhite;
		property DragCursor;
		property DragMode;
		property FocusControl;
		property Font;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowAccelChar;
		property ShowHint;
		property Transparent;
		property Layout;
		property Visible;
		property WordWrap;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property Constraints;
		property DragKind;
		property ParentBiDiMode;
	{$ENDIF}

		property BorderStyle;
		property VerticalAlign;

		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnGetText: TKGetTextEvent
						 read FOnGetText write FOnGetText;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBCheckBox }

	TKDBCheckBox = class( TDBCheckBox )
	private
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBBitwiseCheckBox }

	TKDBBitwiseCheckBox = class( TCheckBox )
	private
		FDataLink: TKEnabledStatesFieldDataLink;
		FCheckValue: Integer;

		function GetDataField: string;
		function GetDataSource: TDataSource;
		procedure DataChange( Sender: TObject );
		procedure UpdateData( Sender: TObject );
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property DataLink: TKEnabledStatesFieldDataLink
						 read FDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Toggle; override;
		procedure KeyPress( var Key: Char ); override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		constructor Create( AOwner: TComponent ); override;
		destructor  Destroy; override;

		property Enabled: Boolean
						 read GetEnabled;
		property State;

	published
		property CheckValue: Integer
						 read FCheckValue write FCheckValue;
		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBComboBox }

	TKDBComboBox = class( TCustomComboBox )
	private
		FDataLink: TKEnabledStatesFieldDataLink;
		FPaintControl: TPaintControl;
		FNullCommand: TShortCut;
		FInSetValue: Boolean;
		FValues: TStrings;
		FValue: string;

		function  GetField: TField;
		function  GetValue: string;
		function  GetDataField: string;
		function  GetReadOnly: Boolean;
		function  GetDataSource: TDataSource;
		function  GetComboValue( Index: Integer ): string;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetItems( NewValue: TStrings );
		procedure SetValues( NewValue: TStrings );
		procedure SetReadOnly( NewValue: Boolean );
		procedure SetValue( const NewValue: string );
		procedure SetDataField( const NewValue: string );
		procedure SetDataSource( NewValue: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure WMPaint( var Message: TWMPaint );
							message WM_PAINT;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GETDATALINK;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Click; override;
		procedure Change; override;
		procedure Loaded; override;
		procedure DropDown; override;
		procedure KeyPress( var Key: Char ); override;
		procedure WndProc( var Message: TMessage ); override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
		procedure ComboWndProc( var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer ); override;

		property Style;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property Field: TField read GetField;
		property Text;
		property Value: string
						 read GetValue write SetValue;

	published
		property Color;
		property Ctl3D;
		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property DragMode;
		property DragCursor;
		property DropDownCount;
		property Font;
		property ItemHeight;
		property Items write SetItems;
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly default False;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Values: TStrings
						 read FValues write SetValues;
		property Visible;

		property OnChange;
		property OnClick;
		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnDrawItem;
		property OnDropDown;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnMeasureItem;
		property OnStartDrag;

		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBListBox }

	TKDBListBox = class( TDBListBox )
	private
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBRadioGroup }

	TKDBRadioGroup = class( TDBRadioGroup )
	private
		FTitleFont: TFont;
		FColorFocused: TColor;
		FColorUnfocused: TColor;
		FNewClick: TNotifyEvent;
		
		procedure SetColorUnfocused( Value: TColor );

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

		procedure CMExit( var Message: TCMExit );
							message CM_Exit;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMTitleFontChanged( var Message: TMessage );
							message CM_TITLEFONTCHANGED;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Paint; override;
		procedure Click; override;
		procedure SetTitleFont( Value: TFont );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure TitleFontChanged( Sender: TObject );

		property Enabled: Boolean
						 read GetEnabled;

	published
		property ColorFocused: TColor
						 read FColorFocused write FColorFocused default clRed;
		property ColorUnfocused: TColor
						 read FColorUnfocused write SetColorUnfocused default clWindowText;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property TitleFont: TFont
						 read FTitleFont write SetTitleFont;

		property OnClick: TNotifyEvent
						 read FNewClick write FNewClick;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBMemo }

	TKDBMemo = class( TDBMemo )
	private
		FTabStops: TStrings;
		FNullCommand: TShortCut;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );
		procedure SetTabStops( Value: TStrings );
		procedure TabStopsChange( Sender: TObject );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;

		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property TabStops: TStrings
						 read FTabStops write SetTabStops;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBRichEdit }

	TKDBRichEdit = class( TDBRichEdit )
	private
		FTabStops: TStrings;
		FNullCommand: TShortCut;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );
		procedure SetTabStops( Value: TStrings );
		procedure TabStopsChange( Sender: TObject );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;

		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property TabStops: TStrings
						 read FTabStops write SetTabStops;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBImage }

	TKDBImage = class( TDBImage )
	private
		FNullCommand: TShortCut;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataLink: TKEnabledStatesFieldDataLink;

		property DataLink: TKEnabledStatesFieldDataLink { Cool!!! }
						 read GetDataLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;

		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBDateTimePicker }

	TKDBDateTimePicker = class( TDateTimePicker )
	private
		FUpdating: Boolean;
		FNewChange: TNotifyEvent;
		FDataLink: TKEnabledStatesFieldDataLink;

		function GetDate: TDate;
		function GetTime: TTime;
		function GetDataField: string;
		function GetDataSource: TDataSource;
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		procedure DataChange( Sender: TObject );
		procedure UpdateData( Sender: TObject );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property Date: TDate
						 read GetDate;
		property Time: TTime
						 read GetTime;

		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

		property OnChange: TNotifyEvent
						 read FNewChange write FNewChange;

	end;

{
--------------------------------------------------------------------------------
------------------------------ DB Speed Controls -------------------------------
--------------------------------------------------------------------------------
}

{ TKDBSpeedInteger }

	TKDBSpeedInteger = class( TKCustomSpeedInteger )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DisplayFormat;
		property EditFormat;
		property EditorEnabled;
		property EditorStyle;
		property Increment;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKDBSpeedHexa }

	TKDBSpeedHexa = class( TKCustomSpeedHexa )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property Digits;
		property DisplayPrefix;
		property EditorEnabled;
		property EditorStyle;
		property Increment;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;

	end;

{ TKDBSpeedFloat }

	TKDBSpeedFloat = class( TKFormattedSpeedControl )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMEnter( var Message: TCMEnter );
							message CM_ENTER;
		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DisplayFormat;
		property EditFormat;
		property EditorEnabled;
		property EditorStyle;
		property Increment;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKDBSpeedDateTime }

	TKDBSpeedDateTime = class( TKCustomSpeedDateTime )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DateIncrement;
		property DisplayFormat;
		property EditFormat;
		property EditorEnabled;
		property EditorStyle;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;
		property TimeIncrement;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKDBSpeedDate }

	TKDBSpeedDate = class( TKCustomSpeedDate )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DateIncrement;
		property DisplayFormat;
		property EditFormat;
		property EditorEnabled;
		property EditorStyle;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKDBSpeedTime }

	TKDBSpeedTime = class( TKCustomSpeedTime )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DisplayFormat;
		property EditFormat;
		property EditorEnabled;
		property EditorStyle;
		property NullCommand;
		property ButtonCommand;
		property MaxValue;
		property MinValue;
		property TimeIncrement;
		
		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKDBSpeedText }

	TKDBSpeedText = class( TKCustomSpeedText )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property EditorEnabled;
		property EditorStyle;
		property NullCommand;
		property ButtonCommand;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnCheckValue;

	end;

{ TKDBSpeedFile }

	TKDBSpeedFile = class( TKCustomSpeedFile )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;
		property LastDir;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property DefExt;
		property EditorEnabled;
		property EditorStyle;
		property Filter;
		property InitialDir;
		property NullCommand;
		property ButtonCommand;
		property MustExist;
		property Title;
    property InitialDirAsLastDir;

		property OnButtonClick;
		property OnDrawButton;

	end;

{ TKDBSpeedFolder }

	TKDBSpeedFolder = class( TKCustomSpeedFolder )
	private
		FNullKey: Boolean;
		FDataLink: TKEnabledStatesFieldDataLink;

		function  GetReadOnly: Boolean;
		function  GetDataField: string;
		function  GetDataSource: TDataSource;
		procedure SetReadOnly( Value: Boolean );
		procedure UpdateData( Sender: TObject );
		procedure DataChange( Sender: TObject );
		procedure SetDataField( const sDataField: string );
		procedure SetDataSource( dsDataSource: TDataSource );

		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMGetDataLink( var Message: TMessage );
							message CM_GetDataLink;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		property AllowNull;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure SetNullValue; override;
		function DoCut: Boolean; override;
		function DoPaste: Boolean; override;
		function 	EditCanModify: Boolean; dynamic;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SyncNull( ZeroFlag: Boolean );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;
		property IsNull;
		property Value;
		property LastDir;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Font;
		property HideSelection;
		property ImeMode;
		property ImeName;
		property MaxLength;
		property OEMConvert;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PasswordChar;
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
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;
		property ReadOnly: Boolean
						 read GetReadOnly write SetReadOnly;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property EditorEnabled;
		property EditorStyle;
		property InitialDir;
		property NullCommand;
		property ButtonCommand;
		property MustExist;
		property Title;
    property InitialDirAsLastDir;

		property OnButtonClick;
		property OnDrawButton;

	end;

{
--------------------------------------------------------------------------------
----------------------------- DB Button Controls -------------------------------
--------------------------------------------------------------------------------
}

{ TKDBButton }

	TKDBButton = class( TButton )
	private
		FDataLink: TKEnabledStatesDataLink;

		function  GetDataSource: TDataSource;
		procedure SetDataSource( Value: TDataSource );
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBBitBtn }

	TKDBBitBtn = class( TBitBtn )
	private
		FDataLink: TKEnabledStatesDataLink;

		function  GetDataSource: TDataSource;
		procedure SetDataSource( Value: TDataSource );
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBSpeedButton }

	TKDBSpeedButton = class( TSpeedButton )
	private
		FDataLink: TKEnabledStatesDataLink;

		function  GetDataSource: TDataSource;
		procedure SetDataSource( Value: TDataSource );
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBLookupComboBox }

	TKDBLookupComboBox = class( TDBLookupComboBox )
	private
		FNullCommand: TShortCut;

		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataSourceLink: TKEnabledStatesDataSourceLink;
		function GetListSourceLink: TKEnabledStatesListSourceLink;

		property DataSourceLink: TKEnabledStatesDataSourceLink { Cool!!! }
						 read GetDataSourceLink;
		property ListSourceLink: TKEnabledStatesListSourceLink { Cool!!! }
						 read GetListSourceLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property NullCommand: TShortCut
						 read FNullCommand write FNullCommand default SC_CTRL_N;
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{ TKDBLookUpListBox }

	TKDBLookUpListBox = class( TDBLookUpListBox )
	private
		function GetEnabledStates: TKDataSetStates;
		procedure SetEnabledStates( Value: TKDataSetStates );
		function GetOnSetEnabled: TKSetEnabledEvent;
		procedure SetOnSetEnabled( Value: TKSetEnabledEvent );

		function GetDataSourceLink: TKEnabledStatesDataSourceLink;
		function GetListSourceLink: TKEnabledStatesListSourceLink;

		property DataSourceLink: TKEnabledStatesDataSourceLink { Cool!!! }
						 read GetDataSourceLink;
		property ListSourceLink: TKEnabledStatesListSourceLink { Cool!!! }
						 read GetListSourceLink;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		function GetEnabled: Boolean; {$IFDEF DELPHI4}override;{$ENDIF}

	protected
		procedure Loaded; override;

	public
		constructor Create( AOwner: TComponent ); override;

		property Enabled: Boolean
						 read GetEnabled;

	published
		property EnabledStates: TKDataSetStates
						 read GetEnabledStates write SetEnabledStates default DEF_ENABLED_STATES;
		property OnSetEnabled: TKSetEnabledEvent
						 read GetOnSetEnabled write SetOnSetEnabled;

	end;

{##FNS##}

{$IFNDEF EXCLUDED_CLASSES}

{ TKDBGrid }

	TKDBGrid = class;

	TKInvalidateOp = ( ioGrowing, ioShrinking, ioNone );
	TKBlobType = ( cbtMemo, cbtGraphic, cbtBinary, cbtBlob );

	TKBlobCellDblClick = procedure( Sender: TKDBGrid; BType: TKBlobType;
		BField: TField; ACol, ARow: Integer ) of object;

	TKCheckBoxClick = procedure( Sender: TKDBGrid; Field: TField ) of object;

	TKDBGrid = class( TDBGrid )
	private
		FRowResize: Boolean;
		FCheckBoxes: Boolean;
		FCheckBoxes3D: Boolean;
		FCheckMouseDown: Boolean;
		FTitleHeight: Integer;
		FInternalHeight: Integer;
		FOptions: TDBGridOptions;
		FBlobCellDblClick: TKBlobCellDblClick;
		FOnCheckBoxClick: TKCheckBoxClick;

		FUpdateRowCounter: Integer;
		FRowsChanging: Boolean;
		FPainting: Boolean;
		FInvalidateOp: TKInvalidateOp;

		function PointInCheckBoxRect( pt: TPoint ): Boolean;

		procedure SetChecks( Index: Integer; Value: Boolean );

		function  GetDefaultRowHeight: Integer;
		procedure SetRowResize( Value: Boolean );
		procedure SetDefaultRowHeight( Value: Integer );

		procedure CalcTitleHeight;

	protected
		function CanEditShow: Boolean; override;
		procedure Loaded; override;
		procedure ColExit; override;
		procedure ColEnter; override;
		procedure RowHeightsChanged; override;
		procedure CheckBoxClick( Fld: TField ); dynamic;
		function GetEditText( ACol, ARow: LongInt ): string; override;
		procedure DrawCell( ACol, ARow: LongInt; ARect: TRect;
			AState: TGridDrawState ); override;
		procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
		procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
		procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

	{$IFDEF DELPHI4}
	public
	{$ENDIF}
		procedure Invalidate; override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property CheckBoxes: Boolean
						 index 0 read FCheckBoxes write SetChecks default true;
		property CheckBoxes3D: Boolean
						 index 1 read FCheckBoxes3D write SetChecks default True;
		property DefaultRowHeight: Integer
						 read GetDefaultRowHeight write SetDefaultRowHeight;
		property RowResize: Boolean
						 read FRowResize write SetRowResize default true;
		property OnBlobCellDblClick: TKBlobCellDblClick
						 read FBlobCellDblClick write FBlobCellDblClick;
		property OnCheckBoxClick: TKCheckBoxClick
						 read FOnCheckBoxClick write FOnCheckBoxClick;

	end;

{$ENDIF}

{##FNS##}

{
--------------------------------------------------------------------------------
------------------------- Gradient Data-Aware Controls -------------------------
--------------------------------------------------------------------------------
}

{ TKDBGradientLabel }

	TKDBGradientLabel = class( TKCustomGradientText )
	private
		FAngle: TKAngle;
		FAutoSize: Boolean;
		FDataLink: TFieldDataLink;

		procedure DataChange( Sender: TObject );
		function GetDataField: string;
		function GetDataSource: TDataSource;
		function GetField: TField;
		function GetFieldText: string;
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

		procedure CMGetDataLink( var Message: TMessage );
							message CM_GETDATALINK;

	protected
		procedure Paint; override;

		procedure SetAngle( Value: TKAngle ); virtual;
		procedure SetAutoSize( Value: Boolean ); virtual;
		procedure SetCaption( Value: TCaption ); virtual;
		procedure SetAlign( Value: TAlign ); override;
		procedure SetAlignment( Value: TAlignment ); override;

		procedure Loaded; override;
		function GetLabelText: string;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Field: TField
						 read GetField;

	published
		property Align;
		property Alignment;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property PopupMenu;
		property Color;
		property FocusControl;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property ShowHint;
		property ShowAccelChar;
		property Visible;
		property WordWrap;

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
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property Gradient;
		property HighLight;
		property Shadow;
		property Angle: TKAngle
						 read FAngle write SetAngle default 0;
		property AutoSize: Boolean
						 read FAutoSize write SetAutoSize default true;

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

	end;

{ TKDBGradientText }

	TKDBGradientText = class( TKCustomGradientText )
	private
		FLines: TStrings;
		FMemoLoaded: Boolean;
		FDataLink: TFieldDataLink;

		procedure DataChange( Sender: TObject );
		function GetDataField: string;
		function GetDataSource: TDataSource;
		function GetField: TField;
		procedure SetDataField( const Value: string );
		procedure SetDataSource( Value: TDataSource );

		procedure CMGetDataLink( var Message: TMessage );
							message CM_GETDATALINK;

	protected
		procedure Paint; override;

		procedure Loaded; override;
		procedure LoadText; dynamic;
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		property Field: TField
						 read GetField;
		property Lines: TStrings
						 read FLines;

	published
		property Align;
		property Alignment;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property PopupMenu;
		property Color;
		property FocusControl;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property ShowHint;
		property ShowAccelChar;
		property Visible;
		property WordWrap;

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
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

    property Gradient;
		property HighLight;
		property Shadow;

		property DataField: string
						 read GetDataField write SetDataField;
		property DataSource: TDataSource
						 read GetDataSource write SetDataSource;

	end;

{ TKDBNavigator }

	TKDBNavigator = class( TKCustomDBNavigator )
	public
		property AvailableButtons;

	published
		// property Align;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Flat;
		property Ctl3D;
		property Hints;
		property ParentCtl3D;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property Constraints;
		property DragKind;
	{$ENDIF}

		property OnDblClick;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnResize;
		property OnStartDrag;

  {$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

		property DataSource;
		property NavButtons;
		property NavEvents;
		property NavMode;
		property Synchronization;

		property AfterAction;
		property BeforeAction;
		property OnPrint;
		property OnSearch;

	end;

implementation

uses
	SysUtils, DBConsts, DBTables, Mask, uksyConsts, ukrUtils, ukdcConsts,
	ukdcUtils;

{
--------------------------------------------------------------------------------
----------------------------- DB DataLink Classes ------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}


type

	TDataSourceLinkSHack = class( TDataLink )
	private
		FDBLookupControl: TDBLookupControl;
	end;

	TListSourceLinkSHack = class( TDataLink )
	private
		FDBLookupControl: TDBLookupControl;
	end;

{---------------------------- Public Implementation ----------------------------}

{ TKEnabledStatesDataLink }

constructor TKEnabledStatesDataLink.Create( AControl: TControl;
	States: TKDataSetStates );
begin
	ForceObject( AControl );
	inherited Create;
	FControl := AControl;
	FEnabledStates := States;
end;

procedure TKEnabledStatesDataLink.SetEnabledStates( Value: TKDataSetStates );
begin
	if ( FEnabledStates <> Value ) then
	begin
		FEnabledStates := Value;
		DataChanged;
	end;
end;

procedure TKEnabledStatesDataLink.ActiveChanged;
begin
	DataChanged;
end;

procedure TKEnabledStatesDataLink.EditingChanged;
begin
	DataChanged;
end;

procedure TKEnabledStatesDataLink.DataSetChanged;
begin
	DataChanged;
end;

procedure TKEnabledStatesDataLink.DataChanged;
var
	iSet: TDataSetStates;
begin
	if CheckObject( DataSet ) then
	begin
		iSet := [DataSet.State];
		Control.Enabled := SetContains( FEnabledStates, iSet, SizeOf( TDataSetStates ),
			SizeOf( TDataSetStates ) );
	end
	else
		Control.Enabled := false;
	DoSetEnabled;
end;

procedure TKEnabledStatesDataLink.DoSetEnabled;
var
	bEnabled: Boolean;
begin
	if Assigned( FOnSetEnabled ) then
	begin
		bEnabled := FControl.Enabled;
		FOnSetEnabled( FControl, bEnabled );
		FControl.Enabled := bEnabled;
	end;
end;

{ TKEnabledStatesFieldDataLink }

constructor TKEnabledStatesFieldDataLink.Create( AControl: TControl;
	States: TKDataSetStates );
begin
	ForceObject( AControl );
	inherited Create;
	FEnabledStates := States;
	inherited Control := AControl;
	inherited OnDataChange := InternalOnDataChange;
	inherited OnEditingChange := InternalOnEditingChange;
	inherited OnUpdateData := InternalOnUpdateData;
	inherited OnActiveChange := InternalOnActiveChange;
end;

function TKEnabledStatesFieldDataLink.GetControl: TControl;
begin
	Result := ( inherited Control as TControl );
end;

procedure TKEnabledStatesFieldDataLink.RecordChanged( Field: TField );
begin
	inherited RecordChanged( Field );
	DataChanged;
end;

procedure TKEnabledStatesFieldDataLink.SetEnabledStates( Value: TKDataSetStates );
begin
	if ( FEnabledStates <> Value ) then
	begin
		FEnabledStates := Value;
		DataChanged;
	end;
end;

procedure TKEnabledStatesFieldDataLink.InternalOnDataChange( Sender: TObject );
begin
	if Assigned( FOnDataChange ) then
		FOnDataChange( Self );
	DataChanged;
end;

procedure TKEnabledStatesFieldDataLink.InternalOnEditingChange( Sender: TObject );
begin
	if Assigned( FOnEditingChange ) then
		FOnEditingChange( Self );
	DataChanged;
end;

procedure TKEnabledStatesFieldDataLink.InternalOnUpdateData( Sender: TObject );
begin
	if Assigned( FOnUpdateData ) then
		FOnUpdateData( Self );
	DataChanged;
end;

procedure TKEnabledStatesFieldDataLink.InternalOnActiveChange( Sender: TObject );
begin
	if Assigned( FOnActiveChange ) then
		FOnActiveChange( Self );
	DataChanged;
end;

procedure TKEnabledStatesFieldDataLink.DataChanged;
var
	iSet: TDataSetStates;
begin
	if CheckObject( DataSet ) then
	begin
		iSet := [DataSet.State];
		Control.Enabled := SetContains( FEnabledStates, iSet, SizeOf( TDataSetStates ),
			SizeOf( TDataSetStates ) );
	end
	else
		Control.Enabled := false;
	DoSetEnabled;
end;

procedure TKEnabledStatesFieldDataLink.DoSetEnabled;
var
	bEnabled: Boolean;
begin
	if Assigned( FOnSetEnabled ) then
	begin
		bEnabled := Control.Enabled;
		FOnSetEnabled( ( Control as TComponent ), bEnabled );
		Control.Enabled := bEnabled;
	end;
end;

{ TKEnabledStatesDataSourceLink }

constructor TKEnabledStatesDataSourceLink.Create( AControl: TDBLookUpControl;
	States: TKDataSetStates );
begin
	ForceObject( AControl );
  inherited Create;
	TDataSourceLinkSHack( Self ).FDBLookupControl := AControl;
	FEnabledStates := States;
end;

function TKEnabledStatesDataSourceLink.GetControl: TDBLookUpControl;
begin
  Result := TDataSourceLinkSHack( Self ).FDBLookupControl; 
end;

procedure TKEnabledStatesDataSourceLink.SetEnabledStates( Value: TKDataSetStates );
begin
	if ( FEnabledStates <> Value ) then
	begin
		FEnabledStates := Value;
		DataChanged;
	end;
end;

procedure TKEnabledStatesDataSourceLink.ActiveChanged;
begin
	inherited ActiveChanged;
	DataChanged;
end;

procedure TKEnabledStatesDataSourceLink.FocusControl( Field: TFieldRef );
begin
	DataChanged;
	inherited FocusControl( Field );
end;

procedure TKEnabledStatesDataSourceLink.RecordChanged( Field: TField );
begin
	inherited RecordChanged( Field );
	DataChanged;
end;

procedure TKEnabledStatesDataSourceLink.DataChanged;
var
	iSet: TDataSetStates;
begin
	if CheckObject( DataSet ) then
	begin
		iSet := [DataSet.State];
		Control.Enabled := SetContains( FEnabledStates, iSet, SizeOf( TDataSetStates ),
			SizeOf( TDataSetStates ) );
	end
	else
		Control.Enabled := false;
	DoSetEnabled;
end;

procedure TKEnabledStatesDataSourceLink.DoSetEnabled;
var
	bEnabled: Boolean;
begin
	if Assigned( FOnSetEnabled ) then
	begin
		bEnabled := Control.Enabled;
		FOnSetEnabled( Control, bEnabled );
		Control.Enabled := bEnabled;
	end;
end;

{ TKEnabledStatesListSourceLink }

constructor TKEnabledStatesListSourceLink.Create( AControl: TDBLookUpControl;
	States: TKDataSetStates );
begin
	ForceObject( AControl );
	inherited Create;
	TListSourceLinkSHack( Self ).FDBLookupControl := AControl;
	FEnabledStates := States;
end;

function TKEnabledStatesListSourceLink.GetControl: TDBLookUpControl;
begin
	Result := TListSourceLinkSHack( Self ).FDBLookupControl;
end;

procedure TKEnabledStatesListSourceLink.SetEnabledStates( Value: TKDataSetStates );
begin
	if ( FEnabledStates <> Value ) then
	begin
		FEnabledStates := Value;
		DataChanged;
	end;
end;

procedure TKEnabledStatesListSourceLink.ActiveChanged;
begin
	inherited ActiveChanged;
	DataChanged;
end;

procedure TKEnabledStatesListSourceLink.DataSetChanged;
begin
	inherited DataSetChanged;
	DataChanged;
end;

procedure TKEnabledStatesListSourceLink.DataChanged;
var
	iSet: TDataSetStates;
begin
	if CheckObject( DataSet ) then
	begin
		iSet := [DataSet.State];
		Control.Enabled := SetContains( FEnabledStates, iSet, SizeOf( TDataSetStates ),
			SizeOf( TDataSetStates ) );
	end
	else
		Control.Enabled := false;
	DoSetEnabled;
end;

procedure TKEnabledStatesListSourceLink.DoSetEnabled;
var
	bEnabled: Boolean;
begin
	if Assigned( FOnSetEnabled ) then
	begin
		bEnabled := Control.Enabled;
		FOnSetEnabled( Control, bEnabled );
		Control.Enabled := bEnabled;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------------ DB Control Classes ------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TKDBEditSHack = class( TCustomMaskEdit )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBTextSHack = class( TCustomLabel )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBCheckBoxSHack = class( TCustomCheckBox )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBListBoxSHack = class( TCustomListBox )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBRadioGroupSHack = class( TCustomRadioGroup )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBMemoSHack = class( TCustomMemo )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBImageSHack = class( TCustomControl )
	private
		FDataLink: TFieldDataLink;

	end;

	TKDBRichEditSHack = class( TCustomRichEdit )
	private
		FDataLink: TFieldDataLink;

	end;

{$HINTS OFF}

	TKDBLookupControlSHack = class( TCustomControl )
	private
		FLookupSource: TDataSource;
		FDataLink: TDataSourceLink;
		FListLink: TListSourceLink;

	end;

	TKDBGridSHack = class( TCustomGrid )
	private
		FIndicators: TImageList;
		FTitleFont: TFont;
		FReadOnly: Boolean;
		FOriginalImeName: TImeName;
		FOriginalImeMode: TImeMode;
		FUserChange: Boolean;
{$IFDEF DELPHI4}
		FIsESCKey: Boolean;
{$ENDIF}
		FLayoutFromDataset: Boolean;
		FOptions: TDBGridOptions;
		FTitleOffset, FIndicatorOffset: Byte;
		FUpdateLock: Byte;
		FLayoutLock: Byte;
		FInColExit: Boolean;
		FDefaultDrawing: Boolean;
		FSelfChangingTitleFont: Boolean;
		FSelecting: Boolean;
		FSelRow: Integer;
		FDataLink: TGridDataLink;
		FOnColEnter: TNotifyEvent;
		FOnColExit: TNotifyEvent;
		FOnDrawDataCell: TDrawDataCellEvent;
		FOnDrawColumnCell: TDrawColumnCellEvent;
		FEditText: string;
	end;

{$HINTS ON}

	TCustomGridHack = class( TCustomGrid );

{---------------------------- Public Implementation ----------------------------}


{ TKDBEdit }


constructor TKDBEdit.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	FNullCommand := SC_CTRL_N;
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBEditSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBEditSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBEditSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBEditSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBEditSHack( Self ).FDataLink := nil;
	TKDBEditSHack( Self ).FDataLink := esfd;
end;

procedure TKDBEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
		Exit
	else
		inherited KeyDown( Key, Shift );
end;

procedure TKDBEdit.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		DataSource.DataSet.FieldByName( DataField ).Clear;
	end
	else
		inherited KeyUp( Key, Shift );
end;

function TKDBEdit.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBEditSHack( Self ).FDataLink );
end;

function TKDBEdit.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBEdit.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBEdit.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBEdit.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBEdit.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBEdit.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBText }

constructor TKDBText.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBTextSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBTextSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBTextSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBTextSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBTextSHack( Self ).FDataLink := esfd;
end;

function TKDBText.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBTextSHack( Self ).FDataLink );
end;

function TKDBText.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBText.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBText.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBText.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBText.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBText.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBLabel3D }

constructor TKDBLabel3D.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ShowAccelChar := false;
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	ControlStyle := ControlStyle + [csReplicatable];
end;

destructor TKDBLabel3D.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBLabel3D.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

function  TKDBLabel3D.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

function  TKDBLabel3D.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBLabel3D.SetDataField( const Value: string );
begin
	FDataLink.FieldName := Value;
end;

procedure TKDBLabel3D.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

procedure TKDBLabel3D.DataChange( Sender: TObject );
var
	s: string;
begin
	s := GetFieldText;
	if Assigned( FOnGetText ) then
		FOnGetText( Self, s );
	Caption := s;
end;

function TKDBLabel3D.GetField: TField;
begin
	Result := FDataLink.Field;
end;

function TKDBLabel3D.GetFieldText: string;
begin
	if CheckObject( FDataLink.Field ) then
		Result := FDataLink.Field.DisplayText
	else if Designing( Self ) then
		Result := Name
	else
		Result := '';
end;

function TKDBLabel3D.GetLabelText: string;
begin
	if ( csPaintCopy in ControlState ) then
		Result := GetFieldText
	else
		Result := Caption;
end;

procedure TKDBLabel3D.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

function TKDBLabel3D.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBLabel3D.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBLabel3D.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBLabel3D.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBLabel3D.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBLabel3D.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBCheckBox }


constructor TKDBCheckBox.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBCheckBoxSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBCheckBoxSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBCheckBoxSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBCheckBoxSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBCheckBoxSHack( Self ).FDataLink := esfd;
end;

function TKDBCheckBox.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBCheckBoxSHack( Self ).FDataLink );
end;

function TKDBCheckBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBCheckBox.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBCheckBox.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBCheckBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBCheckBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBCheckBox.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBBitwiseCheckBox }

const
	INT_MASK = LongInt( $FFFFFFFF );

constructor TKDBBitwiseCheckBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FCheckValue := 0;
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
end;

destructor TKDBBitwiseCheckBox.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

function TKDBBitwiseCheckBox.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

function TKDBBitwiseCheckBox.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBBitwiseCheckBox.SetDataField( const Value: string );
begin
	FDataLink.FieldName := Value;
end;

procedure TKDBBitwiseCheckBox.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
end;

procedure TKDBBitwiseCheckBox.UpdateData( Sender: TObject );
begin
	with FDataLink.Field do
		if ( State = cbGrayed ) then
			AsInteger := ( AsInteger and ( INT_MASK - FCheckValue ) )
		else if Checked then
			AsInteger := ( AsInteger or FCheckValue )
		else
			AsInteger := ( AsInteger and ( INT_MASK - FCheckValue ) );
end;

procedure TKDBBitwiseCheckBox.DataChange( sender: TObject );
begin
	if ( not CheckObject( FDataLink.Field ) ) then
		Checked := false
	else if ( FCheckValue < 1 ) then
		Checked := false
	else
		Checked := ( ( FDataLink.Field.AsInteger and FCheckValue ) = FCheckValue );
end;

procedure TKDBBitwiseCheckBox.CMExit( var Message: TCMExit );
begin
	try
		FDataLink.UpdateRecord;
	except
		SetFocus;
		raise;
	end;
	inherited;
end;

procedure TKDBBitwiseCheckBox.Toggle;
begin
	if FDataLink.Edit then
	begin
		inherited Toggle;
		FDataLink.Modified;
	end;
end;

procedure TKDBBitwiseCheckBox.KeyPress( Var Key: Char );
begin
	inherited KeyPress( Key );
	case Key of
		CH_BACK_SPACE,
		CH_SPACE : FDataLink.Edit;
		CH_ESCAPE: FDataLink.Reset
	end;
end;

procedure TKDBBitwiseCheckBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

function TKDBBitwiseCheckBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBBitwiseCheckBox.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBBitwiseCheckBox.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBBitwiseCheckBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBBitwiseCheckBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBBitwiseCheckBox.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBComboBox }


constructor TKDBComboBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ControlStyle := ControlStyle + [csReplicatable];
	FNullCommand := SC_CTRL_N;
	Style := csDropDownList;
	FValues := TStringList.Create;
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FPaintControl := TPaintControl.Create( Self, 'COMBOBOX' );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
end;

destructor TKDBComboBox.Destroy;
begin
	FValues.Free;
	FreeClean( FDataLink );
	FPaintControl.Free;
	inherited Destroy;
end;

procedure TKDBComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

procedure TKDBComboBox.DataChange( Sender: TObject );
begin
	if ( not CheckObject( FDataLink.Field ) ) then
		if Designing( Self ) then
			Text := Name
		else
			Text := ''
	else
	begin
		if ( FDataLink.Field is TBooleanField ) then
			Value := BOOL_NAME[FDataLink.Field.AsBoolean]
		else
			Value := FDataLink.Field.AsString;
	end;
end;

procedure TKDBComboBox.UpdateData( Sender: TObject );
begin
	if CheckObject( FDataLink ) and ( not ReadOnly ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsString := Value;
	end;
end;

procedure TKDBComboBox.Click;
begin
	if FInSetValue then
	  Exit;
	FDataLink.Modified;
	inherited Click;
	FDataLink.UpdateRecord;
end;

procedure TKDBComboBox.Change;
begin
	if ( not CheckObject( FDataLink ) ) then
	  Exit;
	FDataLink.Modified;
	inherited Change;
end;

procedure TKDBComboBox.DropDown;
begin
	if FInSetValue then
	  Exit;
	FDataLink.Modified;
	inherited DropDown;
end;

function TKDBComboBox.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBComboBox.SetDataSource( NewValue: TDataSource );
begin
	FDataLink.DataSource := NewValue;
	if CheckObject( NewValue ) then
	  NewValue.FreeNotification( Self );
end;

function TKDBComboBox.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBComboBox.SetDataField( const NewValue: string );
begin
	FDataLink.FieldName := NewValue;
end;

function TKDBComboBox.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBComboBox.SetReadOnly( NewValue: Boolean );
begin
	FDataLink.ReadOnly := NewValue;
end;

function TKDBComboBox.GetField: TField;
begin
	Result := FDataLink.Field;
end;

procedure TKDBComboBox.KeyUp( var Key: Word; Shift: TShiftState );
begin        
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( FDataLink ) and
		 ( FDataLink.Active ) and CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Editing ) then
			FDataLink.Edit;
		FDataLink.Field.Clear;
		ItemIndex := -1;
	end
	else
		inherited KeyUp( Key, Shift );
end;

procedure TKDBComboBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
	inherited KeyDown( Key, Shift );
	if ( Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] ) then
		if ( not FDataLink.Edit ) and ( Key in [VK_UP, VK_DOWN] ) then
			Key := 0;
end;

procedure TKDBComboBox.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key in [CH_SPACE..High( Char )] ) and
		 CheckObject( FDataLink.Field ) and
		 ( not FDataLink.Field.IsValidChar( Key ) ) then
	begin
		MessageBeep( 0 );
		Key := CH_NULL;
	end;
	case Key of
		^H, ^V, ^X,
		CH_SPACE..High( Char ): FDataLink.Edit;
		CH_ESCAPE:
			begin
				FDataLink.Reset;
				SelectAll;
				Key := CH_NULL;
			end;
	end;
end;

procedure TKDBComboBox.WndProc( var Message: TMessage );
begin
	if not ( csDesigning in ComponentState ) then
		case Message.Msg of
			WM_COMMAND:
				if ( TWMCommand( Message ).NotifyCode = CBN_SELCHANGE ) then
					if not FDataLink.Edit then
					begin
						if ( Style <> csSimple ) then
							PostMessage( Handle, CB_SHOWDROPDOWN, 0, 0 );
						Exit;
					end;
			CB_SHOWDROPDOWN:
				if ReadOnly then
					DataChange( Self )
				else if ( Message.WParam <> 0 ) then
					FDataLink.Edit
				else if ( not FDataLink.Editing ) then
					DataChange( Self ); {Restore text}
			WM_CREATE,
			WM_WINDOWPOSCHANGED,
			CM_FONTCHANGED:	FPaintControl.DestroyHandle;
		end;
	inherited WndProc( Message );
end;

procedure TKDBComboBox.ComboWndProc( var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer );
begin
	if not ( csDesigning in ComponentState ) then
		case Message.Msg of
			WM_LBUTTONDOWN:
				if ( Style = csSimple ) and ( ComboWnd <> EditHandle ) then
					if ( not FDataLink.Edit ) then
						Exit;
		end;
	inherited ComboWndProc( Message, ComboWnd, ComboProc );
end;

procedure TKDBComboBox.CMExit( var Message: TCMExit );
begin
	try
		FDataLink.UpdateRecord;
	except
		SelectAll;
		SetFocus;
		raise;
	end;
	inherited;
end;

procedure TKDBComboBox.WMPaint( var Message: TWMPaint );
var
	S: string;
begin
	S := '';
	if ( csPaintCopy in ControlState ) then
	begin
		if CheckObject( FDataLink.Field ) and ( ItemIndex >= 0 ) then
			S := Items[ItemIndex];
		SendMessage( FPaintControl.Handle, CB_RESETCONTENT, 0, 0 );
		if ( Items.IndexOf( S ) <> -1 ) then
		begin
			SendMessage( FPaintControl.Handle, CB_ADDSTRING, 0, Longint( PChar( S ) ) );
			SendMessage( FPaintControl.Handle, CB_SETCURSEL, 0, 0 );
		end;
		SendMessage( FPaintControl.Handle, WM_PAINT, Message.DC, 0 );
	end
	else
	  inherited;
end;

procedure TKDBComboBox.SetItems( NewValue: TStrings );
begin
	Items.Assign( NewValue );
	DataChange( Self );
end;

function TKDBComboBox.GetComboValue( Index: Integer ): string;
begin
	if ( Index < FValues.Count ) and CheckStr( FValues[Index] ) then
		Result := FValues[Index]
	else if ( Index < Items.Count ) then
		Result := Items[Index]
	else
		Result := '';
end;

function TKDBComboBox.GetValue: string;
begin
	if ( ItemIndex < 0 ) then
		FValue := ''
	else
		FValue := GetComboValue( ItemIndex );
	Result := FValue;
end;

procedure TKDBComboBox.SetValue( const NewValue: string );
var
	i,
	index: Integer;
begin
	if ( ( AnsiUpperCase( FValue ) = AnsiUpperCase( NewValue ) ) and ( not ReadOnly ) ) then
		Exit;
	FInSetValue := true;
	try
		index := - 1;
		for i := 0 to Items.Count - 1 do
			if ( AnsiUpperCase( NewValue ) = AnsiUpperCase( GetComboValue( i ) ) ) then
			begin
				index := i;
				Break;
			end;
		ItemIndex := index;
	finally
		FInSetValue := false;
	end;
	if ( ItemIndex = - 1 ) then
		FValue := ''
	else
		FValue := NewValue;
	Change;
end;

procedure TKDBComboBox.SetValues( NewValue: TStrings );
begin
	Values.Assign( NewValue );
	DataChange( Self );
end;

procedure TKDBComboBox.CMGetDatalink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

function TKDBComboBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBComboBox.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBComboBox.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBComboBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBComboBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBComboBox.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBListBox }


constructor TKDBListBox.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBListBoxSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBListBoxSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBListBoxSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBListBoxSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBListBoxSHack( Self ).FDataLink := esfd;
end;

function TKDBListBox.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBListBoxSHack( Self ).FDataLink );
end;

function TKDBListBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBListBox.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBListBox.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBListBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBListBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBListBox.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBRadioGroup }


constructor TKDBRadioGroup.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	FTitleFont := TFont.Create;
	FColorFocused := clRed;
	FColorUnfocused := clWindowText;
	with FTitleFont do
	begin
		Color := clWindowText;
		Name := 'Arial';
		Size := 8;
		Style := [fsBold];
		Pitch := fpVariable;
		OnChange := TitleFontChanged;
	end;
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBRadioGroupSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBRadioGroupSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBRadioGroupSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBRadioGroupSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBRadioGroupSHack( Self ).FDataLink := esfd;
end;

destructor TKDBRadioGroup.Destroy;
begin
	FTitleFont.Free;
	inherited Destroy;
end;

procedure TKDBRadioGroup.Click;
begin
	inherited Click;
	if Assigned( FNewClick ) then
		FNewClick( Self );
end;

procedure TKDBRadioGroup.SetTitleFont( Value: TFont );
begin
	FTitleFont.Assign( Value );
end;

procedure TKDBRadioGroup.SetColorUnfocused( Value: TColor );
begin
	FColorUnfocused := Value;
	FTitleFont.Color := Value;
end;

procedure TKDBRadioGroup.CMTitleFontChanged( var Message: TMessage );
begin
	Invalidate;
end;

procedure TKDBRadioGroup.TitleFontChanged( Sender: TObject );
begin
	Perform( CM_TITLEFONTCHANGED, 0, 0 );
end;

procedure TKDBRadioGroup.Paint;
var
	H: Integer;
	R: TRect;
begin
	with Canvas do
	begin
		Font := FTitleFont;
		H := TextHeight( '0' );
		R := Rect( 0, H div 2 - 1, Width, Height );
		if Ctl3D then
		begin
			Inc( R.Left );
			Inc( R.Top );
			Brush.Color := clBtnHighlight;
			FrameRect( R );
			OffsetRect( R, - 1, - 1 );
			Brush.Color := clBtnShadow;
		end
		else
			Brush.Color := clWindowFrame;
		FrameRect( R );
		if CheckStr( Text ) then
		begin
			R := Rect( 8, 0, 0, H );
			DrawText( Handle, PChar( Text ), Length( Text ), R, DT_LEFT or
			  DT_SINGLELINE or	DT_CALCRECT );
			Brush.Color := Color;
			DrawText( Handle, PChar( Text ), Length( Text ), R, DT_LEFT or DT_SINGLELINE );
		end;
	end;
end;

procedure TKDBRadioGroup.CMExit( var Message: TCMExit );
begin
	FTitleFont.Color := FColorUnfocused;
end;

procedure TKDBRadioGroup.CMEnter( var Message: TCMEnter );
begin
	FTitleFont.Color := FColorFocused;
end;

function TKDBRadioGroup.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBRadioGroupSHack( Self ).FDataLink );
end;

function TKDBRadioGroup.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBRadioGroup.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBRadioGroup.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBRadioGroup.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBRadioGroup.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBRadioGroup.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBMemo }


constructor TKDBMemo.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	FNullCommand := SC_CTRL_N;
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBMemoSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBMemoSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBMemoSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBMemoSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBMemoSHack( Self ).FDataLink := esfd;
	FTabStops := TStringList.Create;
	TStringList( FTabStops ).OnChange := TabStopsChange;
end;

destructor TKDBMemo.Destroy;
begin
	FTabStops.Free;
	inherited Destroy;
end;

procedure TKDBMemo.TabStopsChange( Sender: TObject );
begin
  SetControlTabStops( Self, FTabStops );
end;

procedure TKDBMemo.SetTabStops( Value: TStrings );
begin
	if ( not FTabStops.Equals( Value ) ) then
	  FTabStops.Assign( Value );
end;

procedure TKDBMemo.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
		Exit
	else
		inherited KeyDown( Key, Shift );
end;

procedure TKDBMemo.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		DataSource.DataSet.FieldByName( DataField ).Clear;
	end
	else
		inherited KeyUp( Key, Shift );
end;

function TKDBMemo.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBMemoSHack( Self ).FDataLink );
end;

function TKDBMemo.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBMemo.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBMemo.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBMemo.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBMemo.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBMemo.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBRichEdit }


constructor TKDBRichEdit.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	FNullCommand := SC_CTRL_N;
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBRichEditSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBRichEditSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBRichEditSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBRichEditSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBRichEditSHack( Self ).FDataLink := esfd;
	FTabStops := TStringList.Create;
	TStringList( FTabStops ).OnChange := TabStopsChange;
end;

destructor TKDBRichEdit.Destroy;
begin
	FTabStops.Free;
	inherited Destroy;
end;

procedure TKDBRichEdit.TabStopsChange;
begin
	SetControlTabStops( Self, FTabStops );
end;

procedure TKDBRichEdit.SetTabStops( Value: TStrings );
begin
	if ( not FTabStops.Equals( Value ) ) then
		FTabStops.Assign( Value );
end;

procedure TKDBRichEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
		Exit
	else
		inherited KeyDown( Key, Shift );
end;

procedure TKDBRichEdit.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		DataSource.DataSet.FieldByName( DataField ).Clear;
	end
	else
		inherited KeyUp( Key, Shift );
end;

function TKDBRichEdit.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBRichEditSHack( Self ).FDataLink );
end;

function TKDBRichEdit.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBRichEdit.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBRichEdit.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBRichEdit.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBRichEdit.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBRichEdit.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;


{ TKDBImage }


constructor TKDBImage.Create( AOwner: TComponent );
var
	esfd: TKEnabledStatesFieldDataLink;
begin
	inherited Create( AOwner );
	FNullCommand := SC_CTRL_N;
	esfd := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	esfd.OnDataChange := TKDBImageSHack( Self ).FDataLink.OnDataChange;
	esfd.OnEditingChange := TKDBImageSHack( Self ).FDataLink.OnEditingChange;
	esfd.OnUpdateData := TKDBImageSHack( Self ).FDataLink.OnUpdateData;
	esfd.OnActiveChange := TKDBImageSHack( Self ).FDataLink.OnActiveChange;
	DataLink.Free; { Cool!!! }
	TKDBImageSHack( Self ).FDataLink := esfd;
end;

procedure TKDBImage.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		DataSource.DataSet.FieldByName( DataField ).Clear;
	end
	else
		inherited KeyUp( Key, Shift );
end;

function TKDBImage.GetDataLink: TKEnabledStatesFieldDataLink;
begin
	Result := TKEnabledStatesFieldDataLink( TKDBImageSHack( Self ).FDataLink );
end;

function TKDBImage.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBImage.GetEnabledStates: TKDataSetStates;
begin
	Result := DataLink.EnabledStates;
end;

procedure TKDBImage.SetEnabledStates( Value: TKDataSetStates );
begin
	DataLink.EnabledStates := Value;
end;

function TKDBImage.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataLink.OnSetEnabled;
end;

procedure TKDBImage.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataLink.OnSetEnabled := Value;
end;

procedure TKDBImage.Loaded;
begin
	inherited Loaded;
	DataLink.DataChanged;
end;

{ TKDBDateTimePicker }


constructor TKDBDateTimePicker.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FUpdating := false;
	ControlStyle := ControlStyle - [csOpaque];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	inherited OnChange := UpdateData;
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
end;

destructor TKDBDateTimePicker.Destroy;
begin
	FDataLink.Free;
	inherited Destroy;
end;

procedure TKDBDateTimePicker.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

function TKDBDateTimePicker.GetDate: TDate;
begin
	Result := inherited Date;
end;

function TKDBDateTimePicker.GetTime: TTime;
begin
	Result := inherited Time;
end;

procedure TKDBDateTimePicker.DataChange( Sender: TObject );
begin
	if FUpdating then
		Exit;
	FUpdating := true;
	try
		if CheckObject( FDataLink ) and ( FDataLink.Active ) and
			 CheckObject( FDataLink.Field ) and ( not FDataLink.ReadOnly ) then
		begin
			if ( Kind = dtkDate ) then
				inherited Date := FDataLink.Field.AsDateTime
			else
				inherited Time := FDataLink.Field.AsDateTime;
		end;
	finally
		FUpdating := false;
	end;
	Invalidate;
end;

procedure TKDBDateTimePicker.UpdateData( Sender: TObject );
begin
	if FUpdating then
		Exit;
	FUpdating := true;
	try
		if CheckObject( FDataLink ) and ( FDataLink.Active ) and
			 CheckObject( FDataLink.Field ) and ( not FDataLink.ReadOnly ) then
		begin
			if ( not FDataLink.Editing ) then
				FDataLink.Edit;
			if ( Kind = dtkDate ) then
				FDataLink.Field.AsDateTime := Self.Date
			else
				FDataLink.Field.AsDateTime := Self.Time;
		end;
	finally
		FUpdating := false;
	end;
	Invalidate;
end;

function TKDBDateTimePicker.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBDateTimePicker.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
	Invalidate;
end;

function TKDBDateTimePicker.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBDateTimePicker.SetDataField( const Value: string );
begin
	FDataLink.FieldName := Value;
end;

function TKDBDateTimePicker.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBDateTimePicker.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBDateTimePicker.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBDateTimePicker.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBDateTimePicker.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBDateTimePicker.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{
--------------------------------------------------------------------------------
------------------------------ DB Speed Controls -------------------------------
--------------------------------------------------------------------------------
}

{ TKDBSpeedInteger }


constructor TKDBSpeedInteger.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedInteger.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedInteger.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

function TKDBSpeedInteger.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedInteger.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedInteger.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
	inherited ReadOnly := Value;
end;

function TKDBSpeedInteger.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedInteger.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedInteger.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedInteger.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedInteger.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsInteger <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsInteger := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedInteger.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedInteger.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsInteger
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedInteger.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedInteger.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedInteger.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedInteger.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedInteger.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedInteger.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedInteger.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedInteger.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedInteger.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedInteger.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedInteger.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedInteger.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedInteger.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedInteger.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedInteger.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedInteger.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedInteger.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedInteger.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedHexa }


constructor TKDBSpeedHexa.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedHexa.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedHexa.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

function TKDBSpeedHexa.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedHexa.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedHexa.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
	inherited ReadOnly := Value;
end;

function TKDBSpeedHexa.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedHexa.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedHexa.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedHexa.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedHexa.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsInteger <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsInteger := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedHexa.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedHexa.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsInteger
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedHexa.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedHexa.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedHexa.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedHexa.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedHexa.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedHexa.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedHexa.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedHexa.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedHexa.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedHexa.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedHexa.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedHexa.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedHexa.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedHexa.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedHexa.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedHexa.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedHexa.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedHexa.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedFloat }

constructor TKDBSpeedFloat.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedFloat.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedFloat.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedFloat.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedFloat.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedFloat.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedFloat.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedFloat.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedFloat.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedFloat.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedFloat.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsFloat <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsFloat := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedFloat.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedFloat.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsFloat
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedFloat.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedFloat.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedFloat.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedFloat.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedFloat.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedFloat.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedFloat.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedFloat.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedFloat.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;	
end;

procedure TKDBSpeedFloat.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedFloat.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedFloat.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedFloat.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedFloat.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedFloat.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedFloat.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedFloat.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedFloat.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedDateTime }

constructor TKDBSpeedDateTime.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedDateTime.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedDateTime.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedDateTime.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedDateTime.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedDateTime.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedDateTime.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedDateTime.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedDateTime.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedDateTime.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedDateTime.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsDateTime <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsDateTime := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedDateTime.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedDateTime.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsDateTime
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedDateTime.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedDateTime.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedDateTime.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedDateTime.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedDateTime.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedDateTime.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedDateTime.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedDateTime.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedDateTime.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedDateTime.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedDateTime.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedDateTime.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedDateTime.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedDateTime.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedDateTime.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedDateTime.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedDateTime.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedDateTime.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedDate }

constructor TKDBSpeedDate.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedDate.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedDate.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedDate.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedDate.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedDate.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedDate.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedDate.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedDate.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedDate.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedDate.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsDateTime <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsDateTime := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedDate.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedDate.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := Trunc( FDataLink.Field.AsDateTime )
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedDate.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedDate.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedDate.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedDate.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedDate.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedDate.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedDate.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedDate.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedDate.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedDate.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedDate.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedDate.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedDate.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedDate.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedDate.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedDate.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedDate.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedDate.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedTime }

constructor TKDBSpeedTime.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedTime.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedTime.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedTime.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedTime.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedTime.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedTime.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedTime.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedTime.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedTime.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedTime.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsDateTime <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsDateTime := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedTime.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := 0;
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedTime.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := Frac( FDataLink.Field.AsDateTime )
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedTime.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedTime.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedTime.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedTime.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

procedure TKDBSpeedTime.DoIncrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoIncrement;
			Modified;
			UpdateRecord;
		end;
end;

procedure TKDBSpeedTime.DoDecrement;
begin
	if Designing( Self ) or ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			if IsNull then
				Value := 0;
			inherited DoDecrement;
			Modified;
			UpdateRecord;
		end;
end;

function TKDBSpeedTime.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedTime.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedTime.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedTime.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedTime.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedTime.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedTime.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedTime.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedTime.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedTime.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedTime.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedTime.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedText }

constructor TKDBSpeedText.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedText.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedText.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedText.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedText.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedText.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedText.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedText.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedText.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedText.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedText.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsString <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsString := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedText.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := '';
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedText.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsString
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedText.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedText.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedText.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedText.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

function TKDBSpeedText.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedText.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedText.Change;
begin
// change deals with the TEXT property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedText.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedText.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedText.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedText.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedText.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedText.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedText.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedText.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedText.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedFile }

constructor TKDBSpeedFile.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedFile.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedFile.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedFile.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedFile.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedFile.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedFile.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedFile.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedFile.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedFile.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedFile.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsString <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsString := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedFile.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := '';
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedFile.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsString
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedFile.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedFile.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedFile.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedFile.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			MessageBeep( 0 )
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

function TKDBSpeedFile.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedFile.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedFile.Change;
begin
// change deals with the File property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedFile.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedFile.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedFile.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedFile.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedFile.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedFile.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedFile.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedFile.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedFile.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedFolder }

constructor TKDBSpeedFolder.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullKey := false;
	ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TKEnabledStatesFieldDataLink.Create( Self, DEF_ENABLED_STATES );
	FDataLink.OnDataChange := DataChange;
	FDataLink.OnUpdateData := UpdateData;
	AllowNull := true;
end;

destructor TKDBSpeedFolder.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

procedure TKDBSpeedFolder.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		( Operation = opRemove ) and
		( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBSpeedFolder.EditCanModify: Boolean;
begin
	Result := FDataLink.Edit;
end;

function TKDBSpeedFolder.GetReadOnly: Boolean;
begin
	Result := FDataLink.ReadOnly;
end;

procedure TKDBSpeedFolder.SetReadOnly( Value: Boolean );
begin
	FDataLink.ReadOnly := Value;
end;

function TKDBSpeedFolder.GetDataField: string;
begin
	Result := FDataLink.FieldName;
end;

procedure TKDBSpeedFolder.SetDatafield( const sDatafield: string );
begin
	FDataLink.FieldName := sDatafield;
end;

function TKDBSpeedFolder.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedFolder.SetDataSource( dsDataSource: TDataSource );
begin
	FDataLink.DataSource := dsDataSource;
	if CheckObject( dsDataSource ) then
		dsDataSource.FreeNotification( Self );
end;

procedure TKDBSpeedFolder.UpdateData( Sender: TObject );
begin
	if ( ( FDataLink.Field.IsNull ) and ( not IsNull ) ) or
		 ( FDataLink.Field.AsString <> Value ) then
	begin
		FDataLink.Edit;
		FDataLink.Field.AsString := Value;
	end
	else if ( ( not FDataLink.Field.IsNull ) and ( IsNull ) ) then
	begin
		FDataLink.Edit;
		FNullKey := false;
		FDataLink.Field.Clear;
	end;
end;

procedure TKDBSpeedFolder.SyncNull( ZeroFlag: Boolean );
begin
	KeepNull := true;
	try
		InternalText := '';
		if ZeroFlag then
			Value := '';
	finally
		KeepNull := false;
	end;
end;

procedure TKDBSpeedFolder.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
	begin
		if ( not FDataLink.Field.IsNull ) then
			Value := FDataLink.Field.AsString
		else
		begin
			FNullKey := false;
			DoNullCommand;
			SyncNull( Focused and ( FDataLink.DataSource.State in dsEditModes ) );
		end;
	end
	else
		SyncNull( Focused );
end;

function TKDBSpeedFolder.IsValidChar( Key: Char ): Boolean;
begin
	Result := ( inherited IsValidChar( Key ) ) or ( Key = CH_ESCAPE );
end;

procedure TKDBSpeedFolder.KeyPress( var Key: Char );
begin
	inherited KeyPress( Key );
	if ( Key <> CH_NULL ) then
		if ( Key = CH_ESCAPE ) then
			FDataLink.Reset
		else
			FDataLink.Edit;
end;

procedure TKDBSpeedFolder.KeyDown( var Key: Word; Shift: TShiftState );
begin
	FNullKey := false;
	if ( ShortCut( Key, Shift ) = NullCommand ) then
	begin
		FNullKey := true;
		with FDataLink do
			if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
			begin
				if ReadOnly then
					MessageBeep( 0 );
				Key := 0;
			end;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKDBSpeedFolder.SetNullValue;
begin
	if ( csDestroying in ComponentState ) then
		Exit;
	with FDataLink do
		if ( ReadOnly or ( not CheckObject( Field ) ) or ( not CheckObject( DataSource ) ) ) then
		begin
			if FNullKey then
				MessageBeep( 0 );
			Exit;
		end
		else if CheckObject( FDataLink ) then
		begin
			KeepNull := true;
			try
				if ( not Field.IsNull ) then
				begin
					Edit;
					Field.Clear;
					UpdateRecord;
				end;
//				InternalText := '';
//				inherited SetNullValue;
			finally
				KeepNull := false;
			end;
		end;
end;

function TKDBSpeedFolder.DoCut: Boolean;
begin
	Result := inherited DoCut;
	if Result then
		FDataLink.Edit;
end;

function TKDBSpeedFolder.DoPaste: Boolean;
begin
	Result := inherited DoPaste;
	if Result then
		FDataLink.Edit;
end;

procedure TKDBSpeedFolder.Change;
begin
// change deals with the Folder property...
	if CheckObject( FDataLink ) then
		FDataLink.Modified;
	inherited Change;
end;

procedure TKDBSpeedFolder.CMGetDataLink( var Message: TMessage );
begin
	Message.Result := Integer( FDataLink );
end;

procedure TKDBSpeedFolder.CMEnter( var Message: TCMEnter );
begin
	inherited;
	if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
		SyncNull( FDataLink.DataSource.State in dsEditModes );
end;

procedure TKDBSpeedFolder.CMExit( var Message: TCMExit );
begin
	inherited;
	try
		FDataLink.UpdateRecord;
		if ( FDataLink.Field <> nil ) and ( FDataLink.Field.IsNull ) then
			SyncNull( false );			
	except
		SetFocus;
		raise;
	end;
end;

function TKDBSpeedFolder.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedFolder.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedFolder.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedFolder.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedFolder.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedFolder.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{
--------------------------------------------------------------------------------
----------------------------- DB Button Controls -------------------------------
--------------------------------------------------------------------------------
}

{ TKDBButton }


constructor TKDBButton.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDataLink := TKEnabledStatesDataLink.Create( Self, DEF_ENABLED_STATES );
end;

destructor TKDBButton.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

function TKDBButton.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBButton.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBButton.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
	FDataLink.DataChanged;
end;

function TKDBButton.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBButton.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBButton.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBButton.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

procedure TKDBButton.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBBitBtn }


constructor TKDBBitBtn.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDataLink := TKEnabledStatesDataLink.Create( Self, DEF_ENABLED_STATES );
end;

destructor TKDBBitBtn.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

function TKDBBitBtn.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBBitBtn.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBBitBtn.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
	FDataLink.DataChanged;
end;

function TKDBBitBtn.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBBitBtn.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBBitBtn.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBBitBtn.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBBitBtn.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

procedure TKDBBitBtn.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBSpeedButton }


constructor TKDBSpeedButton.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDataLink := TKEnabledStatesDataLink.Create( Self, DEF_ENABLED_STATES );
end;

destructor TKDBSpeedButton.Destroy;
begin
	FreeClean( FDataLink );
	inherited Destroy;
end;

function TKDBSpeedButton.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBSpeedButton.GetDataSource: TDataSource;
begin
	Result := FDataLink.DataSource;
end;

procedure TKDBSpeedButton.SetDataSource( Value: TDataSource );
begin
	FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
	FDataLink.DataChanged;
end;

function TKDBSpeedButton.GetEnabledStates: TKDataSetStates;
begin
	Result := FDataLink.EnabledStates;
end;

procedure TKDBSpeedButton.SetEnabledStates( Value: TKDataSetStates );
begin
	FDataLink.EnabledStates := Value;
end;

function TKDBSpeedButton.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := FDataLink.OnSetEnabled;
end;

procedure TKDBSpeedButton.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	FDataLink.OnSetEnabled := Value;
end;

procedure TKDBSpeedButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if CheckObject( FDataLink ) and
		 ( Operation = opRemove ) and
		 ( AComponent = DataSource ) then
		 DataSource := nil;
end;

procedure TKDBSpeedButton.Loaded;
begin
	inherited Loaded;
	FDataLink.DataChanged;
end;

{ TKDBLookupComboBox }


constructor TKDBLookupComboBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FNullCommand := SC_CTRL_N;
	DataSourceLink.Free;
	ListSourceLink.Free;
	TKDBLookupControlSHack( Self ).FDataLink :=
		TKEnabledStatesDataSourceLink.Create( Self, DEF_ENABLED_STATES );
	TKDBLookupControlSHack( Self ).FListLink :=
		TKEnabledStatesListSourceLink.Create( Self, DEF_ENABLED_STATES );
end;

procedure TKDBLookupComboBox.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = FNullCommand ) and CheckObject( DataSource ) and
		 ( DataSource.DataSet.Active ) and CheckStr( DataField ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		DataSource.DataSet.FieldByName( DataField ).Clear;
	end
	else
		inherited KeyUp( Key, Shift );
end;

function TKDBLookUpComboBox.GetDataSourceLink: TKEnabledStatesDataSourceLink;
begin
	Result := TKEnabledStatesDataSourceLink( TKDBLookupControlSHack( Self ).FDataLink );
end;

function TKDBLookUpComboBox.GetListSourceLink: TKEnabledStatesListSourceLink;
begin
	Result := TKEnabledStatesListSourceLink( TKDBLookUpControlSHack( Self ).FListLink );
end;

function TKDBLookUpComboBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBLookUpComboBox.GetEnabledStates: TKDataSetStates;
begin
	Result := DataSourceLink.EnabledStates;
end;

procedure TKDBLookUpComboBox.SetEnabledStates( Value: TKDataSetStates );
begin
	DataSourceLink.EnabledStates := Value;
	ListSourceLink.EnabledStates := Value;
end;

function TKDBLookUpComboBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataSourceLink.OnSetEnabled;
end;

procedure TKDBLookUpComboBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataSourceLink.OnSetEnabled := Value;
	ListSourceLink.OnSetEnabled := Value;
end;

procedure TKDBLookUpComboBox.Loaded;
begin
	inherited Loaded;
	DataSourceLink.DataChanged;
	ListSourceLink.DataChanged;
end;

{ TKDBLookUpListBox }

constructor TKDBLookupListBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	DataSourceLink.Free;
	ListSourceLink.Free;
	TKDBLookupControlSHack( Self ).FDataLink :=
		TKEnabledStatesDataSourceLink.Create( Self, DEF_ENABLED_STATES );
	TKDBLookupControlSHack( Self ).FListLink :=
		TKEnabledStatesListSourceLink.Create( Self, DEF_ENABLED_STATES );
end;

function TKDBLookUpListBox.GetDataSourceLink: TKEnabledStatesDataSourceLink;
begin
	Result := TKEnabledStatesDataSourceLink( TKDBLookupControlSHack( Self ).FDataLink );
end;

function TKDBLookUpListBox.GetListSourceLink: TKEnabledStatesListSourceLink;
begin
	Result := TKEnabledStatesListSourceLink( TKDBLookUpControlSHack( Self ).FListLink );
end;

function TKDBLookUpListBox.GetEnabled: Boolean;
begin
	Result := inherited {$IFDEF DELPHI4}GetEnabled{$ELSE}Enabled{$ENDIF};
end;

function TKDBLookUpListBox.GetEnabledStates: TKDataSetStates;
begin
	Result := DataSourceLink.EnabledStates;
end;

procedure TKDBLookUpListBox.SetEnabledStates( Value: TKDataSetStates );
begin
	DataSourceLink.EnabledStates := Value;
	ListSourceLink.EnabledStates := Value;
end;

function TKDBLookUpListBox.GetOnSetEnabled: TKSetEnabledEvent;
begin
	Result := DataSourceLink.OnSetEnabled;
end;

procedure TKDBLookUpListBox.SetOnSetEnabled( Value: TKSetEnabledEvent );
begin
	DataSourceLink.OnSetEnabled := Value;
	ListSourceLink.OnSetEnabled := Value;
end;

procedure TKDBLookUpListBox.Loaded;
begin
	inherited Loaded;
	DataSourceLink.DataChanged;
	ListSourceLink.DataChanged;
end;

{$IFNDEF EXCLUDED_CLASSES}

{ TKDBGrid }

constructor TKDBGrid.Create( AOwner: TComponent );
begin
{$IFNDEF INTERNAL_VERSION}
	{ RaiseException( EKDBGrid, 'Could not create TKDBGrid in this version of KLIB!' ); }
{$ENDIF}
	inherited Create( AOwner );
	FOptions := Options;
	FInternalHeight := DEF_GRID_ROW_HEIGHT;
	RowResize := true;
	FCheckBoxes := true;
	FCheckBoxes3D := true;
	FUpdateRowCounter := 0;
	FTitleHeight := -1;
	FInvalidateOp := ioNone;
	FRowsChanging := False;
	FPainting := False;
end;

procedure TKDBGrid.Loaded;
begin
	inherited Loaded;
	FInternalHeight := DefaultRowHeight;
  FTitleHeight := -1;
	ColEnter;
end;

procedure TKDBGrid.SetChecks( Index: Integer; Value: Boolean );
begin
	case Index of
		0: FCheckBoxes := Value;
		1: FCheckBoxes3D := Value;
	end;
	if ( not Loading( Self ) ) then
		Invalidate;
end;

function TKDBGrid.CanEditShow: Boolean;
begin
	if ( not Loading( Self ) ) and ( CheckObject( SelectedField ) ) then
		Result := ( not ( CheckObjectClass( SelectedField, TBlobField ) or
										( CheckObjectClass( SelectedField, TBooleanField ) and FCheckBoxes ) ) ) and
							( inherited CanEditShow )
	else
		Result := ( inherited CanEditShow );
end;

function TKDBGrid.GetEditText( ACol, ARow: LongInt ): string;
begin
	Result := '';
	if Datalink.Active then
		with Columns[RawToDataColumn( ACol )] do
			if Assigned( Field ) then
			begin
{ Aqui fomos obrigados a colocar um artifício afim de contornar um bug
	em TCustomMaskEdit que não usa o MaxLength dele próprio.

	Ex: DateTimeField com o valor: 26/10/1998 10:05:07
			que possuia uma máscara correta: !99/99/0000;1;_
			porém era mostrado incorretamente em função do tamanho de .Text
			ficando : 26/10/0:07

	PS: Precisa também colocar o DisplayText do Field corretamente, senão não adianta
			nada. Ou seja, o DisplayText e o EditMask precisam estar setados, bem como
			essa hackeada se faz necessária.
			A hackeada do campo privado serve apenas para manter compatibilidade com
			a implementação anterior.

			Tais regras valem servem apenas para DateTime!
}
				if ( Field.DataType = ftDateTime ) then
					Result := Field.DisplayText
				else
				begin
					{Result := Field.Text;}
					Result := inherited GetEditText( ACol, ARow );
					Exit;
				end;
			end;
{$IFDEF DELPHI3}
	TKDBGridSHack( Self ).FEditText := Result;
{$ENDIF}
end;

function TKDBGrid.GetDefaultRowHeight: Integer;
begin
	Result := inherited DefaultRowHeight;
end;

procedure TKDBGrid.SetRowResize( Value: Boolean );
begin
	FRowResize := Value;
	with TCustomGridHack( Self ) do
		if FRowResize then
			Options := Options + [goRowSizing]
		else
			Options := Options - [goRowSizing];
end;

type
  TDataLinkHack = class( TDataLink );

procedure TKDBGrid.Invalidate;
var
	iELW,
	iRowCount: Integer;
begin
	if FPainting then
	  Exit;
	if ( RowCount = 1000 ) then
		Inc( FUpdateRowCounter );
	if ( FUpdateRowCounter > 1 ) and
		 ( HandleAllocated ) and ( CheckObject( DataSource ) ) and
		 ( CheckObject( DataSource.DataSet ) ) and ( DataSource.DataSet.Active ) and
		 ( not ( DataSource.DataSet.BOF and DataSource.DataSet.EOF ) ) then
	begin
	  FPainting := True;
		try
			case FInvalidateOp of
				ioNone:
				begin
					iELW := 0;
					if ( [goFixedHorzLine, goHorzLine] * TCustomGridHack( Self ).Options <> [] ) then
						iELW := GridLineWidth;
					if ( dgTitles in Options ) then
						iRowCount := ( ( ClientHeight - FTitleHeight - iELW ) div ( FInternalHeight + iELW ) )
					else
						iRowCount := ( ClientHeight div ( FInternalHeight + iELW ) );
{					if DataSource.DataSet.Filtered then
						iRowCount := MinInteger( [iRowCount, VisibleRowCount, DataLink.RecordCount] )
					else
						iRowCount := MaxInteger( [iRowCount, VisibleRowCount, DataLink.RecordCount] );}
				end;
				ioShrinking:
					iRowCount := DataLink.RecordCount;
				ioGrowing:
				begin
					iRowCount := VisibleRowCount;
				end;
			else
				iRowCount := RowCount - 1;
			end;
      TDataLinkHack( DataLink ).DataSetChanged;
			{
			if ( not Designing( Self ) ) then
				DebugLogMessageFmt( 'INV -> DRH %d; DLRC %d; OP %d; VRC %d; iRC %d; RC %d',
					[DefaultRowHeight, DataLink.RecordCount, Byte( FInvalidateOp ),
					 VisibleRowCount, iRowCount, RowCount] );
      }
			RowCount := iRowCount + Ord( dgTitles in Options );
		finally
			FInvalidateOp := ioNone;
			FPainting := False;
		end;
	end;
	inherited Invalidate;
end;

procedure TKDBGrid.CalcTitleHeight;
begin
	Canvas.Font := TitleFont;
	FTitleHeight := Canvas.TextHeight( 'Wg' ) + 4;
end;

procedure TKDBGrid.SetDefaultRowHeight( Value: Integer );
begin
	inherited DefaultRowHeight := Value;
	if ( dgTitles in Options ) then
	begin
		if ( FTitleHeight = -1 ) then
		  CalcTitleHeight;
		RowHeights[0] := FTitleHeight;
	end;
end;

procedure TKDBGrid.ColExit;
begin
	if ( SelectedField is TBlobField ) or
		 ( ( SelectedField is TBooleanField ) and FCheckBoxes ) then
		Options := FOptions;
	RowResize := FRowResize;
	inherited ColExit;
end;

procedure TKDBGrid.ColEnter;
begin
	if ( SelectedField is TBlobField ) or
		 ( ( SelectedField is TBooleanField ) and FCheckBoxes ) then
	begin
		FOptions := Options;
		Options := FOptions - [dgEditing];
	end;
	inherited ColEnter;
	RowResize := FRowResize;
end;

procedure TKDBGrid.RowHeightsChanged;
var
	i,
	iStart,
	iChangedRow: Integer;
begin
	iStart := 0;
	iChangedRow := -1;
	if ( dgTitles in Options ) then
		iStart := 1;
	for i := iStart to RowCount - 1 do
		if ( RowHeights[i] <> DefaultRowHeight ) then
			iChangedRow := i;
	if ( iChangedRow <> -1 ) then
	begin
		try
			FInternalHeight := RowHeights[iChangedRow];
			if ( not FRowsChanging ) then
			begin
				FRowsChanging := True;
				if ( FInternalHeight > DefaultRowHeight ) then
					FInvalidateOp := ioGrowing
				else if ( FInternalHeight < DefaultRowHeight ) then
					FInvalidateOp := ioShrinking
				else
					FInvalidateOp := ioNone;
			end;
			SetDefaultRowHeight( FInternalHeight );
		finally
			FRowsChanging := False;
		end;
	end;
	inherited RowHeightsChanged;
end;

procedure TKDBGrid.DrawCell( ACol, ARow: LongInt; ARect: TRect; AState: TGridDrawState );
var
	pc: PChar;
	rt,
	ckrt: TRect;
	fld: TField;
	pic: TPicture;
	ckState: Cardinal;
	iSize,
	iOldActive : Integer;
begin
	fld := GetColField( ACol - Ord( dgIndicator in Options ) );
	if ( gdFixed in AState ) or
		 ( not ( ( fld is TBlobField ) or ( fld is TMemoField ) or ( fld is TBooleanField ) ) ) or
		 ( ( fld is TBooleanField ) and ( not FCheckBoxes ) ) then
	begin
		inherited DrawCell( ACol, ARow, ARect, AState );
		Exit;
	end;
	iOldActive := DataLink.ActiveRecord;
	Dec( ARow, Ord( dgTitles in Options ) );
	Dec( ACol, Ord( dgIndicator in Options ) );
	try
		DataLink.ActiveRecord := ARow;
		if ( fld is TMemoField ) or ( TBlobField( fld ).BlobType = ftMemo ) then
		begin
			if HighlightCell( ACol, ARow, CH_SPACE, AState ) then
			begin
				Canvas.Brush.Color := clHighlight;
				Canvas.Font.Color := clHighlightText;
			end
			else
			begin
				Canvas.Brush.Color := Color;
				Canvas.Font.Color := Font.Color;
			end;
			Canvas.FillRect( ARect );
			rt := ARect;
			InflateRect( rt, -2, -2 );
			rt.Top := ( rt.Top + 1 );
			rt.Left := ( rt.Left + 1 );
			pc := StrAlloc( Length( fld.AsString ) + 1 );
			try
				if ( fld.DataSet is TBDEDataSet ) then
					AnsiToNative( TBDEDataSet( fld.DataSet ).Locale, fld.AsString, pc,
						Length( fld.AsString ) + 1 )
				else
					StrPCopy( pc, fld.AsString );
				DrawText( Canvas.Handle, pc, -1, rt, DT_WORDBREAK or DT_NOPREFIX or DT_VCENTER );
			finally
				StrDispose( pc );
			end;
			if ( gdFocused in AState ) and ( not ( dgRowSelect in Options ) ) then
				Canvas.DrawFocusRect( ARect );
		end
		else if ( fld is TGraphicField ) or ( TBlobField( fld ).BlobType = ftGraphic )then
		begin
			pic := TPicture.Create;
			try
				try
					pic.Assign( fld );
				except
					pic.Assign( nil );
				end;
			finally
				Canvas.StretchDraw( ARect, pic.Graphic );
				pic.Free;
			end;
			if ( gdFocused in AState ) and ( not ( dgRowSelect in Options ) ) then
				Canvas.DrawFocusRect( ARect );
		end
		else if ( fld is TBooleanField ) then
		begin
			rt := ARect;
			if CheckBoxes3D then
				DrawFrameControl( Canvas.Handle, rt, DFC_BUTTON, DFCS_BUTTONPUSH )
			else
			begin
				Canvas.Brush.Color := clWindow;
				Canvas.FillRect( rt );
			end;
			ckState := DFCS_BUTTONCHECK;
			if fld.IsNull then
				ckState := ckState or DFCS_INACTIVE
			else if fld.AsBoolean then
				ckState := ckState or DFCS_CHECKED;
			iSize := 14; { Magic Number! }
			with rt do
				ckrt := Rect( ( Left + Right - iSize ) div 2,
											( Top + Bottom - iSize ) div 2,
											( Left + Right + iSize ) div 2,
											( Top + Bottom + iSize ) div 2 );
			DrawFrameControl( Canvas.Handle, ckrt, DFC_BUTTON, ckState );
			if ( gdFocused in AState ) and ( not ( dgRowSelect in Options ) ) then
			begin
				InflateRect( ckrt, 1, 1 );
				Canvas.DrawFocusRect( ckrt );
			end;
		end;
	finally   
		DataLink.ActiveRecord := iOldActive;
	end;
end;

procedure TKDBGrid.CheckBoxClick( Fld: TField );
begin
	if ( DataSource.DataSet.CanModify ) then
	begin
		if ( not ( DataSource.DataSet.State in dsEditModes ) ) then
			DataSource.DataSet.Edit;
		if SelectedField.IsNull then
			SelectedField.AsBoolean := true
		else
			SelectedField.AsBoolean := ( not SelectedField.AsBoolean );
		if Assigned( FOnCheckBoxClick ) then
			FOnCheckBoxClick( Self, Fld );
		InvalidateCell( Col, Row );
	end;
end;

function TKDBGrid.PointInCheckBoxRect( pt: TPoint ): Boolean;
var
	rt,
	ckrt: TRect;
	iSize: Byte;
begin
	iSize := 14; { Magic Number! }
	rt := CellRect( Col, Row );
	with rt do
		ckrt := Rect( ( Left + Right - iSize ) div 2,
									( Top + Bottom - iSize ) div 2,
									( Left + Right + iSize ) div 2,
									( Top + Bottom + iSize ) div 2 );
	Result := PtInRect( ckrt, pt );
end;

procedure TKDBGrid.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
	gc: TGridCoord;
begin
	if ( not FCheckMouseDown ) then
	begin
		inherited MouseUp( Button, Shift, X, Y );
		Exit;
	end;
	FCheckMouseDown := false;
	if ( CheckObjectClass( SelectedField, TBooleanField ) and ( Button = mbLeft ) and
			 ( not Sizing( X, Y ) ) ) then
	begin
		gc := MouseCoord( X, Y );
		if ( ( gc.X = Col ) and ( gc.Y = Row ) and PointInCheckBoxRect( Point( X, Y ) ) ) then
			CheckBoxClick( SelectedField );
	end
	else if ( not ( CheckObjectClass( SelectedField, TBooleanField ) and CheckBoxes ) ) then
	  inherited MouseUp( Button, Shift, X, Y );
end;

procedure TKDBGrid.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
	gc: TGridCoord;
	IsBlobTwice: Boolean;
begin
  inherited MouseDown( Button, Shift, X, Y );
	IsBlobTwice := false;
	FCheckMouseDown := false;
	if ( CheckObjectClass( SelectedField, TBooleanField ) and ( Shift = [ssLeft] ) and
		 ( Button = mbLeft ) and ( not Sizing( X, Y ) ) ) then
	begin
		gc := MouseCoord( X, Y );
		if ( ( gc.X = Col ) and ( gc.Y = Row ) and PointInCheckBoxRect( Point( X, Y ) ) ) then
		begin
			FCheckMouseDown := true;
			Exit;
		end;
	end;
	if ( ( ssDouble in Shift ) and ( Button = mbLeft ) and ( not Sizing( X, Y ) ) ) then
		with MouseCoord( X, Y ) do
			if ( ( X = Col ) and ( Y = Row ) ) then
				if ( SelectedField is TBlobField ) or ( SelectedField is TBinaryField ) then
					IsBlobTwice := true;
	if IsBlobTwice then
	begin
		if ( CheckObjectClass( SelectedField, TMemoField ) ) or
			 ( TBlobField( SelectedField ).BlobType = ftMemo ) then
		begin
			if Assigned( FBlobCellDblClick ) then
				FBlobCellDblClick( Self, cbtMemo, SelectedField, Col, Row );
		end
		else if ( CheckObjectClass( SelectedField, TGraphicField ) ) or
			 ( TBlobField( SelectedField ).BlobType = ftGraphic ) then
		begin
			if Assigned( FBlobCellDblClick ) then
				FBlobCellDblClick( Self, cbtGraphic, SelectedField, Col, Row );
		end
		else if ( CheckObjectClass( SelectedField, TBinaryField ) ) then
		begin
			if Assigned( FBlobCellDblClick ) then
				FBlobCellDblClick( Self, cbtBinary, SelectedField, Col, Row );
		end
		else
			if Assigned( FBlobCellDblClick ) then
				FBlobCellDblClick( Self, cbtBlob, SelectedField, Col, Row );
	end;
end;

procedure TKDBGrid.KeyUp( var Key: Word; Shift: TShiftState );
begin
	if ( CheckObjectClass( SelectedField, TBooleanField ) and CheckBoxes ) and
		 ( Key = VK_SPACE ) then
		CheckBoxClick( SelectedField )
	else
		inherited KeyUp( Key, Shift );
end;

{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Gradient Data-Aware Controls -------------------------
--------------------------------------------------------------------------------
}

const
  MEMO_LARGE_FMT = '(%s)';

{----------------------------- TKDBGradientLabel -------------------------------}

constructor TKDBGradientLabel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAngle := 0;
	FAutoSize := true;
  ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
end;

destructor TKDBGradientLabel.Destroy;
begin
	FDataLink.Free;
	FDataLink := nil;
	inherited Destroy;
end;

procedure TKDBGradientLabel.SetAlign( Value: TAlign );
begin
	if ( FAutoSize or ( FAngle <> 0 ) ) then
		inherited SetAlign( alNone ) {Call inherited TControl Align property!}
	else
    inherited SetAlign( Value ); {Call TKCustomGradientText protected method}
end;

procedure TKDBGradientLabel.SetAngle( Value: TKAngle );
begin
	if ( FAngle <> Value ) then
	begin
		FAngle := Value;
		if ( FAngle <> 0 ) then
		begin
			FAutoSize := true;
			inherited Align := alNone;
			Alignment := taLeftJustify;
		end;
		Invalidate;
	end;
end;

procedure TKDBGradientLabel.SetAutoSize( Value: Boolean );
begin
	if ( FAutoSize <> Value ) then
	begin
		if ( Value and FDataLink.DataSourceFixed ) then
			DatabaseError( SDataSourceFixed );
		if ( FAngle <> 0 ) then
			FAutoSize := true
		else
			FAutoSize := Value;
		if FAutoSize then
			inherited Align := alNone;
		Invalidate;
	end
end;

procedure TKDBGradientLabel.SetCaption( Value: TCaption );
begin
	if ( inherited Caption <> Value ) then
	begin
		inherited Caption := Value; 
		Invalidate;
	end;
end;

procedure TKDBGradientLabel.SetAlignment( Value: TAlignment );
begin
	if ( FAngle = 0 ) then
		inherited SetAlignment( Value )
  else
		inherited SetAlignment( taLeftJustify );
end;

procedure TKDBGradientLabel.Paint;
begin
	inherited Paint;
	DoPaint( Caption, FAngle, FAutoSize );
end;

{ data support }

procedure TKDBGradientLabel.Loaded;
begin
	inherited Loaded;
	if ( csDesigning in ComponentState ) then
		DataChange( Self );
end;

procedure TKDBGradientLabel.Notification( AComponent: TComponent;
	Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and CheckObject( FDataLink ) and
		 ( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBGradientLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TKDBGradientLabel.SetDataSource( Value: TDataSource );
begin
  FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

function TKDBGradientLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TKDBGradientLabel.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;

function TKDBGradientLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TKDBGradientLabel.GetFieldText: string;
begin
  if CheckObject( FDataLink.Field ) then
    Result := FDataLink.Field.DisplayText
	else
		if Designing( Self ) then
			Result := Name
		else
			Result := '';
end;

procedure TKDBGradientLabel.DataChange( Sender: TObject );
begin
	Caption := GetFieldText;
end;

function TKDBGradientLabel.GetLabelText: string;
begin
	if ( csPaintCopy in ControlState ) then
		Result := GetFieldText
	else
    Result := Caption;
end;

procedure TKDBGradientLabel.CMGetDataLink( var Message: TMessage );
begin
  Message.Result := Integer( FDataLink );
end;

{----------------------------- TKDBGradientText --------------------------------}

constructor TKDBGradientText.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FMemoLoaded := false;
	FLines := TStringList.Create;
	FLines.Add( ClassName );
	Height := 37;
	Width := 210;
  ControlStyle := ControlStyle + [csReplicatable];
	FDataLink := TFieldDataLink.Create;
	FDataLink.Control := Self;
	FDataLink.OnDataChange := DataChange;
end;

destructor TKDBGradientText.Destroy;
begin
	FLines.Free;
	FDataLink.Free;
	FDataLink := nil;
	inherited Destroy;
end;

procedure TKDBGradientText.Paint;
var
	i: Integer;
	str: string;
begin
	inherited Paint;
  str := '';
	for i := 0 to FLines.Count - 2 do
		str := str + FLines[i] + CH_CRLF;
	if CheckStrings( FLines ) then
		str := str + FLines[FLines.Count - 1];
	DoPaint( str, 0, false );
end;

{ data support }

procedure TKDBGradientText.Loaded;
begin
	inherited Loaded;
	if ( csDesigning in ComponentState ) then
		DataChange( Self );
end;

procedure TKDBGradientText.Notification( AComponent: TComponent;
	Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and CheckObject( FDataLink ) and
		 ( AComponent = DataSource ) then
		DataSource := nil;
end;

function TKDBGradientText.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TKDBGradientText.SetDataSource( Value: TDataSource );
begin
  FDataLink.DataSource := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

function TKDBGradientText.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TKDBGradientText.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;

function TKDBGradientText.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TKDBGradientText.LoadText;
begin
	if ( not FMemoLoaded ) and CheckObject( FDataLink.Field ) and FDataLink.Field.IsBlob then
		try
			Lines.Text := FDataLink.Field.AsString;
			FMemoLoaded := True;
		except
			{ Memo too large }
			on E: EInvalidOperation do
				Lines.Text := Format( MEMO_LARGE_FMT, [E.Message] );
		end;
end;

procedure TKDBGradientText.DataChange( Sender: TObject );
begin
	if CheckObject( FDataLink.Field ) then
		if FDataLink.Field.IsBlob then
		begin
			if ( FDataLink.Editing and FMemoLoaded ) then
			begin
				FMemoLoaded := False;
				LoadText;
			end
			else
			begin
				Lines.Text := Format( MEMO_LARGE_FMT, [FDataLink.Field.DisplayLabel] );
				FMemoLoaded := False;
			end;
		end
		else
		begin
			if FDataLink.CanModify then
				Text := FDataLink.Field.Text
			else
        Text := FDataLink.Field.DisplayText;
      FMemoLoaded := True;
		end
  else
  begin
		if Designing( Self ) then
			Text := Name
		else
			Text := '';
		FMemoLoaded := False;
	end;
	Invalidate;
end;

procedure TKDBGradientText.CMGetDataLink( var Message: TMessage );
begin
  Message.Result := Integer( FDataLink );
end;

end.
