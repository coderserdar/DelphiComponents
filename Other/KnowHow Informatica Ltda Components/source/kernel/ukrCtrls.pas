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

unit ukrCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls,
	Buttons, Menus, uksyTypes, uksyUtils, uksyClasses, uksyShortCuts;

{##FNS##}

{
--------------------------------------------------------------------------------
--------------------------- Basic Control Classes ------------------------------
--------------------------------------------------------------------------------
}

type

	EKRCtrls = class( EKKernel );

	TKVerticalAlign = ( vaTop, vaCenter, vaBottom );
	TKBorderStyle = ( kbsNone, kbsLight, kbsMedium, kbsHeavy, kbsFrame, kbsRaisedFrame );

{ TKCustomLabel3D }

	TKCustomLabel3D = class( TCustomLabel )
	private
		FIsFolder: Boolean;
		FBorderStyle : TKBorderStyle;
		FVerticalAlign : TKVerticalAlign;

		procedure SetIsFolder( Value: Boolean );
		procedure SeTKBorderStyle( Value: TKBorderStyle );
		procedure SeTKVerticalAlign( Value: TKVerticalAlign );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DoDrawText( var Rect: TRect; Flags: Word );
		{$IFDEF DELPHI4}
		{ In delphi 4 this method are protected dynamic. In delphi 3 it is
			private. In delphi 3, this is a new static method, in delphi 4 it
			is a overrided static (reintroduced) method...}
		reintroduce;
		{$ENDIF}

	protected
		procedure Paint; override;

		property BorderStyle: TKBorderStyle
						 read FBorderStyle write SeTKBorderStyle default kbsHeavy;
		property IsFolder: Boolean
						 read FIsFolder write SetIsFolder default false;
		property VerticalAlign: TKVerticalAlign
						 read FVerticalAlign write SeTKVerticalAlign default vaTop;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomGradientText }

	TKCustomGradientText = class( TGraphicControl )
	private
    FFocusControl: TWinControl;
		FShadow: TKEffect;
		FHighLight: TKEffect;
		FGradient: TKGradient;
		FAlignment: TAlignment;
		FWordWrap: Boolean;
		FShowAccelChar: Boolean;

		bmpTEXT: TBitmap;
		bmpTEMP: TBitmap;
		bmpBKGND: TBitmap;

		procedure CMDialogChar( var Message: TCMDialogChar );
							message CM_DIALOGCHAR;
							
	protected
		procedure Paint; override;
		procedure GradientChanged( Sender: TObject );
		procedure DoPaint( const Value: string; Angle: TKAngle; AutoSize: Boolean );
		procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

		procedure SetAlign( Value: TAlign ); virtual;
		procedure SetWordWrap( Value: Boolean ); virtual;
		procedure SetAlignment( Value: TAlignment ); virtual;
		procedure SetShowAccelChar( Value: Boolean ); virtual;
		procedure SetFocusControl( Value: TWinControl ); virtual;

		property Align
						 write SetAlign;
		property Alignment: TAlignment
						 read FAlignment write SetAlignment default taLeftJustify;
		property FocusControl: TWinControl
						 read FFocusControl write SetFocusControl;
		property Gradient: TKGradient
						 read FGradient write FGradient;
		property HighLight: TKEffect
						 read FHighLight write FHighLight;
		property Shadow: TKEffect
						 read FShadow write FShadow;
		property ShowAccelChar: Boolean
						 read FShowAccelChar write SetShowAccelChar default false;
		property WordWrap: Boolean
						 read FWordWrap write SetWordWrap default false;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{
--------------------------------------------------------------------------------
----------------------------- Basic Speed Controls -----------------------------
--------------------------------------------------------------------------------
}

  EKSpeedControl = class( EKRCtrls );

	TKSpeedButtonState = ( sbsNone, sbsUp, sbsDown, sbsBtnDown );
	TKEditorStyle = ( esUpDown, esRXUpDown, esGlyph, esEllipse, esRightArrow,
		esLeftArrow, esDownArrow, esUpArrow, esOwnerDraw );

	TKDrawButtonEvent = procedure( Sender: TObject; ACanvas: TCanvas; ARect: TRect ) of
		object;

{ TKCustomSpeedButton }

	TKCustomSpeedButton = class( TGraphicControl )
	private
		FGlyph: TBitmap;
		FRepeatTimer: TTimer;
		FTimerEnabled: Boolean;
		FDown: TKSpeedButtonState;
		FOnUpClick: TNotifyEvent;
		FOnDownClick: TNotifyEvent;
		FOnDrawButton: TKDrawButtonEvent;
		FLastDown: TKSpeedButtonState;
		FButtonStyle: TKEditorStyle;
		FClickDelayed: Boolean;

		bmpUp: TBitmap;
		bmpDown: TBitmap;

		FUpRect: TRect;
		FDownRect: TRect;
		FMouseDown: Boolean;

		procedure SetGlyph( Value: TBitmap );
		procedure GlyphChanged( Sender: TObject );

		procedure SetWidth( Value: Integer );
		procedure SetHeight( Value: Integer );
		procedure SetTimerEnabled( Value: Boolean );
		procedure SetDown( Value: TKSpeedButtonState );
		procedure SetButtonStyle( Value: TKEditorStyle );

		procedure TimerExpired( Sender: TObject );

		procedure CMEnabledChanged( var Message: TMessage );
							message CM_ENABLEDCHANGED;

		procedure DrawRXButtons;

	{$IFDEF DELPHI4}
	public
	{$ELSE}
	protected
	{$ENDIF}
		procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

	protected
		procedure Click; override;
		procedure UpClick; dynamic;
		procedure DownClick; dynamic;
		procedure DoDrawButton; dynamic;

		procedure Paint; override;
		procedure Changed; dynamic;
		procedure MouseDown( Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer ); override;
		procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
		procedure MouseUp( Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer ); override;

		property ButtonStyle: TKEditorStyle
						 read FButtonStyle write SetButtonStyle default esUpDown;
		property Down: TKSpeedButtonState
						 read FDown write SetDown default sbsNone;
		property Enabled;
		property Glyph: TBitmap
						 read FGlyph write SetGlyph;
		property Height
						 write SetHeight;
		property ShowHint;
		property TimerEnabled: Boolean
						 read FTimerEnabled write SetTimerEnabled default true;
		property Visible;
		property Width
						 write SetWidth;

		property OnDownClick: TNotifyEvent
						 read FOnDownClick write FOnDownClick;
		property OnDrawButton: TKDrawButtonEvent
						 read FOnDrawButton write FOnDrawButton;
		property OnUpClick: TNotifyEvent
						 read FOnUpClick write FOnUpClick;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedControl }

	TKCustomSpeedControl = class( TCustomEdit )
	private
		FIsNull: Boolean;
		FKeepNull: Boolean;
		FAllowNull: Boolean;
		FLoadingValue: string;
		FAlignment: TAlignment;
		FNullCommand: TShortCut;
		FButtonCommand: TShortCut;
		FButton: TKCustomSpeedButton;
		FEditorEnabled: Boolean;
		FOnUpClick: TNotifyEvent;
		FOnDownClick: TNotifyEvent;
		FOnButtonClick: TNotifyEvent;
		FOnDrawButton: TKDrawButtonEvent;

		FButtonClicked: Boolean;
		FClipboardAction: Boolean;
		FNullCommandPressed: Boolean;

		procedure SetInternalText( const NewValue: string );

		procedure SetEditRect;
		function GetEditorStyle: TKEditorStyle;
		function GetMinHeight: Integer;
		function GetButtonHint: string;
		function GetButtonGlyph: TBitmap;
		function GetButtonWidth: Integer;
		procedure SetButtonWidth( Value: Integer );
		procedure SetShowHint( Value: Boolean );
		procedure SetButtonGlyph( Value: TBitmap );
		procedure SetButtonHint( const Value: string );
		procedure SetAllowNull( NewValue: Boolean );
		procedure SetNullCommand( NewValue: TShortCut );
		procedure SetButtonCommand( NewValue: TShortCut );
		procedure SetAlignment( Value: TAlignment );

		procedure UpClickEvent( Sender: TObject );
		procedure DownClickEvent( Sender: TObject );
		procedure ButtonClickEvent( Sender: TObject );
		procedure DrawButtonEvent( Sender: TObject; ACanvas: TCanvas; ARect: TRect );

		procedure WMSize( var Message: TWMSize );
							message WM_SIZE;
		procedure CMEnter( var Message: TCMGotFocus );
							message CM_ENTER;
		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;
		procedure WMPaste( var Message: TWMPaste );
							message WM_PASTE;
		procedure WMCut( var Message: TWMCut );
							message WM_CUT;

		procedure CMVisibleChanged( var Message: TMessage );
							message CM_VISIBLECHANGED;
		procedure CMEnabledChanged( var Message: TMessage );
						  message CM_ENABLEDCHANGED;

	protected
		procedure Change; override;
		procedure Loaded; override;
		procedure AdjustControl; dynamic;
		function DoCut: Boolean; virtual;
		function DoPaste: Boolean; virtual;
		procedure SetNullValue; virtual; abstract;
		procedure DisplayChanged; virtual; abstract;
		function ParseValue: Boolean; virtual; abstract;
		function GetDefaultEditorStyle: TKEditorStyle; dynamic;
		procedure SetEditorStyle( Value: TKEditorStyle ); virtual;
		function CheckTextValue( const NewValue: string ): string; virtual;
		procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;
		function GetText: string;

		procedure DoUpClick; dynamic;
		procedure DoDownClick; dynamic;
		procedure DoButtonClick; dynamic;
		procedure DoNullCommand; dynamic;
		procedure DoDrawButton( ACanvas: TCanvas; ARect: TRect ); dynamic;

		procedure KeyPress( var Key: Char ); override;
		function IsValidChar( Key: Char ): Boolean; virtual;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

		procedure CreateWnd; override;
		procedure CreateParams( var Params: TCreateParams ); override;

		procedure UpdateValue; virtual;

		function Editing: Boolean; dynamic;

{ either public or private in descendent classes }
		property Button: TKCustomSpeedButton
						 read FButton;
		property KeepNull: Boolean
						 read FKeepNull write FKeepNull default false;

{ published in descendent classes }
		property Alignment: TAlignment
						 read FAlignment write SetAlignment default taLeftJustify;
		property AllowNull: Boolean
						 read FAllowNull write SetAllowNull;
		property ButtonHint: string
						 read GetButtonHint write SetButtonHint;
		property ButtonGlyph: TBitmap
						 read GetButtonGlyph write SetButtonGlyph;
		property ButtonWidth: Integer
						 read GetButtonWidth write SetButtonWidth;
		property EditorEnabled: Boolean
						 read FEditorEnabled write FEditorEnabled default true;
		property EditorStyle: TKEditorStyle
						 read GetEditorStyle write SetEditorStyle;
		property IsNull: Boolean
						 read FIsNull;
		property InternalText: string
						 write SetInternalText;
		property NullCommand: TShortCut
						 read FNullCommand write SetNullCommand default SC_CTRL_N;
		property ButtonCommand: TShortCut
						 read FButtonCommand write SetButtonCommand default SC_CTRL_RETURN;
		property ShowHint
						 write SetShowHint;

		property OnButtonClick: TNotifyEvent
						 read FOnButtonClick write FOnButtonClick;
		property OnDownClick: TNotifyEvent
						 read FOnDownClick write FOnDownClick;
		property OnUpClick: TNotifyEvent
						 read FOnUpClick write FOnUpClick;
		property OnDrawButton: TKDrawButtonEvent
						 read FOnDrawButton write FOnDrawButton;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure ButtonClick; dynamic;
		procedure UpClick; dynamic;
		procedure DownClick; dynamic;

		property Text: string
						 read GetText;
	end;

	TKCustomSpeedControlClass = class of TKCustomSpeedControl;

{ TKCustomSpeedText }

	TKCheckTextEvent = procedure( Sender: TObject; var NewValue: string ) of object;

	TKCustomSpeedText = class( TKCustomSpeedControl )
	private
		FOnCheckValue: TKCheckTextEvent;

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DisplayChanged; override;

	protected
		procedure UpdateValue; override;
		procedure SetNullValue; override;
		function GetValue: string; virtual;
		function ParseValue: Boolean; override;
		procedure SetValue( const NewValue: string ); virtual;
		function GetDefaultEditorStyle: TKEditorStyle; override;
		function CheckTextValue( const NewValue: string ): string; override;

		property Value: string
						 read GetValue write SetValue;

		property OnCheckValue: TKCheckTextEvent
						 read FOnCheckValue write FOnCheckValue;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{##NI##}


{ TKCustomSpeedMask }

	TKCustomSpeedMask = class( TKCustomSpeedText )
	private
		FEditMask: string;

	protected
		property EditMask: string
						 read FEditMask write FEditMask;

	end;

{##NI##}

{ TKCustomSpeedFile }

	TKCustomSpeedFile = class( TKCustomSpeedText )
	private
		FTitle: string;
		FFilter: string;
		FDefExt: string;
		FInitialDir: string;
		FMustExist: Boolean;
		FMultiSelect: Boolean;
    FInitialDirAsLastDir: Boolean;
		FLastValid: string;

		procedure SetTitle( const Value: string );
		procedure SetMustExist( NewValue: Boolean );
		procedure SetMultiSelect( NewValue: Boolean );
    function GetLastDir: string;

	protected
		function CheckFiles( const s: string ): Boolean; dynamic;
		procedure DoButtonClick; override;
		procedure SetValue( const NewValue: string ); override;
		function GetDefaultEditorStyle: TKEditorStyle; override;
		procedure SetEditorStyle( Value: TKEditorStyle ); override;
		function CheckTextValue( const NewValue: string ): string; override;

    property InitialDirAsLastDir: Boolean
             read FInitialDirAsLastDir write FInitialDirAsLastDir default True;
		property DefExt: string
						 read FDefExt write FDefExt;
		property Filter: string
						 read FFilter write FFilter;
		property InitialDir: string
						 read FInitialDir write FInitialDir;
		property MustExist: Boolean
						 read FMustExist write SetMustExist;
		property MultiSelect: Boolean
						 read FMultiSelect write SetMultiSelect;
		property Title: string
						 read FTitle write SetTitle;

    property LastDir: string
             read GetLastDir;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedFolder }

	TKCustomSpeedFolder = class( TKCustomSpeedText )
	private
		FTitle: string;
		FInitialDir: string;
		FMustExist: Boolean;
		FMultiSelect: Boolean;
    FInitialDirAsLastDir: Boolean;
		FLastValid: string;

		procedure SetTitle( const Value: string );
		procedure SetMustExist( NewValue: Boolean );
		procedure SetMultiSelect( NewValue: Boolean );
    function GetLastDir: string;

	protected
		function CheckPaths( const s: string ): Boolean; dynamic;
		procedure DoButtonClick; override;
		procedure SetValue( const NewValue: string ); override;
		function GetDefaultEditorStyle: TKEditorStyle; override;
		procedure SetEditorStyle( Value: TKEditorStyle ); override;
		function CheckTextValue( const NewValue: string ): string; override;

    property InitialDirAsLastDir: Boolean
             read FInitialDirAsLastDir write FInitialDirAsLastDir default True;
		property InitialDir: string
						 read FInitialDir write FInitialDir;
		property MustExist: Boolean
						 read FMustExist write SetMustExist;
		property MultiSelect: Boolean
						 read FMultiSelect write SetMultiSelect;
		property Title: string
						 read FTitle write SetTitle;

    property LastDir: string
             read GetLastDir;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKFormattedSpeedControl }

	TKFormatFloatTextEvent = procedure( Sender: TObject; const AValue: Extended;
		const AFormat: string; var AText: string ) of object;

	TKFormattedSpeedControl = class( TKCustomSpeedControl )
	private
		FValue: Extended;
		FMinValue: Extended;
		FMaxValue: Extended;
		FIncrement: Extended;
		FEditFormat: string;
		FDisplayFormat: string;
		FOnFormatEditText: TKFormatFloatTextEvent;
		FOnFormatDisplayText: TKFormatFloatTextEvent;

		function GetText: string; 
		function GetValue: Extended;

		procedure SetValue( NewValue: Extended );
		procedure SetMaxValue( NewValue: Extended );
		procedure SetMinValue( NewValue: Extended );

	{$IFDEF DELPHI4}
	protected
	{$ENDIF}
		procedure DisplayChanged; override;

	protected
		procedure Change; override;

		procedure DoIncrement; dynamic;
		procedure DoDecrement; dynamic;
		procedure UpdateValue; override;
		procedure SetNullValue; override;
		function ParseValue: Boolean; override;
		function IsValidChar( Key: Char ): Boolean; override;
		procedure SetEditFormat( const Value: string ); virtual;
		procedure SetDisplayFormat( const Value: string ); virtual;
		function CheckValue( NewValue: Extended ): Extended; dynamic;

		procedure DoUpClick; override;
		procedure DoDownClick; override;

		function FormatEditText: string; dynamic;
		function FormatDisplayText: string; dynamic;
		function FormatText( const AFormat: string; AValue: Extended ): string; virtual;

		property DisplayFormat: string
						 read FDisplayFormat write SetDisplayFormat;
		property EditFormat: string
						 read FEditFormat write SetEditFormat;
		property Increment: Extended
						 read FIncrement write FIncrement;
		property MaxValue: Extended
						 read FMaxValue write SetMaxValue;
		property MinValue: Extended
						 read FMinValue write SetMinValue;
		property Value: Extended
						 read GetValue write SetValue;

		property OnFormatDisplayText: TKFormatFloatTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatFloatTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

{ from TKCustomSpeedControl } 
		property Alignment default taRightJustify;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedInteger }

	TKFormatIntegerTextEvent = procedure( Sender: TObject; const AValue: LongInt;
		const AFormat: string; var AText: string ) of object;

	TKCustomSpeedInteger = class( TKFormattedSpeedControl )
	private
		FOnFormatEditText: TKFormatIntegerTextEvent;
		FOnFormatDisplayText: TKFormatIntegerTextEvent;

		function GetIncrement: LongInt;
		procedure SetIncrement( NewValue: LongInt );
		function GetMaxValue: LongInt;
		procedure SetMaxValue( NewValue: LongInt );
		function GetMinValue: LongInt;
		procedure SetMinValue( NewValue: LongInt );
		function GetValue: LongInt;
		procedure SetValue( NewValue: LongInt );

		procedure FormatEdit( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );
		procedure FormatDisplay( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );

	protected
		function IsValidChar( Key: Char ): Boolean; override;
		function FormatText( const AFormat: string; AValue: Extended ): string; override;

		property Increment: LongInt
						 read GetIncrement write SetIncrement;
		property MaxValue: LongInt
						 read GetMaxValue write SetMaxValue;
		property MinValue: LongInt
						 read GetMinValue write SetMinValue;
		property Value: LongInt
						 read GetValue write SetValue;

		property OnFormatDisplayText: TKFormatIntegerTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatIntegerTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedHexa }

	TKCustomSpeedHexa = class( TKCustomSpeedInteger )
	private
		FDigits: Byte;
		FDisplayPrefix: Boolean;

{$HINTS OFF}
		property DisplayFormat;
		property EditFormat;
{$HINTS ON}

		property OnFormatDisplayText;
		property OnFormatEditText;

		procedure SetDigits( NewValue: Byte );
		procedure SetDisplayPrefix( NewValue: Boolean );

	protected
		function ParseValue: Boolean; override;
		procedure KeyPress( var Key: Char ); override;
		function IsValidChar( Key: Char ): Boolean; override;
		function FormatText( const AFormat: string; AValue: Extended ): string; override;

		property Digits: Byte
						 read FDigits write SetDigits default 8;
		property DisplayPrefix: Boolean
						 read FDisplayPrefix write SetDisplayPrefix default true;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedDateTime }

	TKFormatDateTimeTextEvent = procedure( Sender: TObject; const AValue: TDateTime;
		const AFormat: string; var AText: string ) of object;

{ TKDateIncrement }

	TKDateIncrement = class( TPersistent )
	private
		FDays: Cardinal;
		FMonths: Cardinal;
		FYears: Cardinal;
		FOwner: TObject;

	public
		constructor Create( AOwner: TObject );
		procedure Assign( Source: TPersistent ); override;

		property Owner: TObject
						 read FOwner;

	published
		property Days: Cardinal
						 read FDays write FDays default 1;
		property Months: Cardinal
						 read FMonths write FMonths default 0;
		property Years: Cardinal
						 read FYears write FYears default 0;

	end;

	TKCustomSpeedDateTime = class( TKFormattedSpeedControl )
	private
		FDateIncrement: TKDateIncrement;
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		procedure FormatEdit( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );
		procedure FormatDisplay( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );

		property Increment;

	protected
		function GetTimeIncrement: TDateTime; virtual;
		procedure SetTimeIncrement( NewValue: TDateTime ); virtual;
		procedure SetDateIncrement( NewValue: TKDateIncrement ); virtual;
		function GetMaxValue: TDateTime; virtual;
		procedure SetMaxValue( NewValue: TDateTime ); virtual;
		function GetMinValue: TDateTime; virtual;
		procedure SetMinValue( NewValue: TDateTime ); virtual;
		function GetValue: TDateTime; virtual;
		procedure SetValue( NewValue: TDateTime ); virtual;

		procedure DoIncrement; override;
		procedure DoDecrement; override;
		function ParseValue: Boolean; override;
		function IsValidChar( Key: Char ): Boolean; override;
		function FormatText( const AFormat: string; AValue: Extended ): string; override;

		property DateIncrement: TKDateIncrement
						 read FDateIncrement write SetDateIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property TimeIncrement: TDateTime
						 read GetTimeIncrement write SetTimeIncrement;
		property Value: TDateTime
						 read GetValue write SetValue;

		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedDate }

	TKCustomSpeedDate = class( TKFormattedSpeedControl )
	private
		FDateIncrement: TKDateIncrement;
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		procedure SetDateIncrement( NewValue: TKDateIncrement );
		function GetMaxValue: TDateTime;
		procedure SetMaxValue( NewValue: TDateTime );
		function GetMinValue: TDateTime;
		procedure SetMinValue( NewValue: TDateTime );
		function GetValue: TDateTime;
		procedure SetValue( NewValue: TDateTime );

		procedure FormatEdit( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );
		procedure FormatDisplay( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );

		property Increment;

	protected
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		function ParseValue: Boolean; override;
		function IsValidChar( Key: Char ): Boolean; override;
		function FormatText( const AFormat: string; AValue: Extended ): string; override;

		property DateIncrement: TKDateIncrement
						 read FDateIncrement write SetDateIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property Value: TDateTime
						 read GetValue write SetValue;

		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomSpeedTime }

	TKCustomSpeedTime = class( TKFormattedSpeedControl )
	private
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		function GetTimeIncrement: TDateTime;
		procedure SetTimeIncrement( NewValue: TDateTime );
		function GetMaxValue: TDateTime;
		procedure SetMaxValue( NewValue: TDateTime );
		function GetMinValue: TDateTime;
		procedure SetMinValue( NewValue: TDateTime );
		function GetValue: TDateTime;
		procedure SetValue( NewValue: TDateTime );

		procedure FormatEdit( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );
		procedure FormatDisplay( Sender: TObject; const AValue: Extended;
			const AFormat: string; var AText: string );

		property Increment;

	protected
		procedure DoIncrement; override;
		procedure DoDecrement; override;
		function ParseValue: Boolean; override;
		function IsValidChar( Key: Char ): Boolean; override;
		function FormatText( const AFormat: string; AValue: Extended ): string; override;

		property TimeIncrement: TDateTime
						 read GetTimeIncrement write SetTimeIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property Value: TDateTime
						 read GetValue write SetValue;

		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{##FNS##}

procedure RegisterEditGradientFunc;
procedure UnRegisterEditGradientFunc;

implementation

{$R brrSpeed.res}

{.$R u:\delphi\klib100\source\kernel100\lib\brrSpeed.res

 We try to use this form to D4 complaint but this generates a incomatible
 directory list for other delphis.... sorry...
}

uses
	SysUtils, Forms, ClipBrd, Dialogs, uksyConsts, uksyPackReg, ukrResStr,
	ukrUtils, ukrClasses, ukrfGradEdit;

{
--------------------------------------------------------------------------------
--------------------------- Basic Control Classes ------------------------------
--------------------------------------------------------------------------------
}

{------------------------------ TKCustomLabel3D --------------------------------}

constructor TKCustomLabel3D.Create( AOwner: TComponent );
begin
{ To allow Dialog to instantiate this kind of control without Standard or DBCtrls package }
	if ( not CheckObjectClass( AOwner, TKKernelBaseDialog ) ) then
		ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs, perStd, perDBCtrls] );
	inherited Create( AOwner );
	Width := 121;
	Height := 24;
	Color := clWhite;
	AutoSize := False;
	FIsFolder := False;
	FBorderStyle := kbsHeavy;
end;

procedure TKCustomLabel3D.Paint;
const
	WordWraps:
		array[Boolean] of Word = ( 0, DT_WORDBREAK );
	VerAlign:
		array[TKVerticalAlign] of Word	=
			( 0, DT_VCENTER or DT_SINGLELINE, DT_BOTTOM or DT_SINGLELINE );
var
	Rect: TRect;
begin
	with Canvas do
	begin
		if not Transparent then
		begin
			Brush.Color := Self.Color;
			Brush.Style := bsSolid;
			FillRect( ClientRect );
		end;
		Brush.Style := bsClear;
		Rect := ClientRect;
		case FBorderStyle of
			kbsNone:
				with Rect do InflateRect( Rect, -2, -2 );
			kbsLight:
			begin
				Frame3D( Canvas, Rect, clBtnShadow, clBtnHighlight, 1 );
				with Rect do InflateRect( Rect, -1, -1 );
			end;
			kbsMedium:
			begin
				Frame3D( Canvas, Rect, clBlack, clBtnHighlight, 1 );
				with Rect do InflateRect( Rect, -1, -1 );
			end;
			kbsHeavy:
			begin
				Frame3D( Canvas, Rect, clBtnShadow, clBtnHighlight, 1 );
				Frame3D( Canvas, Rect, clBlack, clBtnFace, 1 );
			end;
			kbsFrame:
			begin
				Frame3D( Canvas, Rect, clBtnShadow, clBtnHighlight, 1 );
				Frame3D( Canvas, Rect, clBtnHighlight, clBtnShadow, 1 );
			end;
			kbsRaisedFrame:
			begin
				Frame3D( Canvas, Rect, clBtnHighlight, clBtnShadow, 1 );
				Frame3D( Canvas, Rect, clBtnShadow, clBtnHighlight, 1 );
			end;
		end;
		with Rect do
			InflateRect( Rect, -1, -1 );
		DoDrawText( Rect, DT_EXPANDTABS or WordWraps[WordWrap] or
								TEXT_ALIGNMENT[Alignment] or VerAlign[FVerticalAlign] );
	end;
end;

procedure TKCustomLabel3D.DoDrawText( var Rect: TRect; Flags: Word );
var
	sText: string;
begin
	sText := GetLabelText;
	if ( Flags and DT_CALCRECT <> 0 ) and
		 ( ( not CheckStr( sText ) ) or
			 ShowAccelChar and
			 ( sText[1] = '&' ) and
			 ( sText[2] = CH_NULL ) ) then
		sText := Text + CH_SPACE;
	if ( not ShowAccelChar ) then
		Flags := Flags or DT_NOPREFIX;
	Canvas.Font := Font;
	if not Enabled then
		Canvas.Font.Color := clGrayText;
	if IsFolder then
		DrawTextEx( Canvas.Handle, PChar( sText ), -1, Rect,
			Flags or DT_PATH_ELLIPSIS, nil )
	else
		DrawText( Canvas.Handle, PChar( sText ), -1, Rect, Flags );
end;

procedure TKCustomLabel3D.SetIsFolder( Value: Boolean );
begin
	if ( FIsFolder <> Value ) then
	begin
		FIsFolder := Value;
		Invalidate;
	end;
end;

procedure TKCustomLabel3D.SeTKBorderStyle( Value: TKBorderStyle );
begin
	if ( FBorderStyle <> Value ) then
	begin
		FBorderStyle := Value;
		Invalidate;
	end;
end;

procedure TKCustomLabel3D.SeTKVerticalAlign( Value: TKVerticalAlign );
begin
	if ( FVerticalAlign <> Value ) then
	begin
		FVerticalAlign := Value;
		Invalidate;
	end;
end;

{---------------------------- TKCustomGradientText -----------------------------}

constructor TKCustomGradientText.Create( AOwner: TComponent );
begin
{ To allow Dialog to instantiate this kind of control without Standard or DBCtrls package }
	if ( not CheckObjectClass( AOwner, TKKernelBaseDialog ) ) then
		ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs, perStd, perDBCtrls] );
	inherited Create( AOwner );

	FShowAccelChar := false;
	FAlignment := taLeftJustify;

	Width := 20;
	Height := 20;
	Font.Size := 20;
	Font.Color := clBlack;
	Font.Name := 'Times New Roman';

	bmpTEMP := TBitmap.Create;
	bmpTEXT := TBitmap.Create;
	bmpBKGND := TBitmap.Create;
	FShadow := TKEffect.Create( Self );
	FHighLight := TKEffect.Create( Self );
	FGradient := TKGradient.Create( Self, bmpBKGND );
	FGradient.OnGradientChange := GradientChanged;

	with bmpTEXT do
	begin
		Canvas.Brush.Style := bsSolid;
		Canvas.Brush.Color := clWhite;
		Canvas.Font.Color := clBlack;
		Transparent := true;
		TransparentColor := clBlack;
	end;
	with bmpBKGND do
	begin
		Transparent := true;
		TransparentColor := clWhite;
	end;
	Canvas.Brush.Style := bsClear;
	FHighLight.Color := clWhite;
	with FShadow do
	begin
		Color := clGray;
		Direction := edDownRight;
	end;
end;

destructor TKCustomGradientText.Destroy;
begin
	FShadow.Free;
	bmpTEMP.Free;
	bmpTEXT.Free;
	bmpBKGND.Free;
	FGradient.Free;
	FHighLight.Free;
	inherited Destroy;
end;

procedure TKCustomGradientText.Notification( AComponent: TComponent;
	Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and ( AComponent = FFocusControl ) then
		FFocusControl := nil;
end;

procedure TKCustomGradientText.GradientChanged( Sender: TObject );
begin
	Invalidate;
end;

procedure TKCustomGradientText.SetAlign( Value: TAlign );
begin
	if ( inherited Align <> Value ) then
	begin
		inherited Align := Value;
		Invalidate;
	end;
end;

procedure TKCustomGradientText.SetWordWrap( Value: Boolean ); 
begin
	if ( FWordWrap <> Value ) then
	begin
		FWordWrap := Value;
		Invalidate;
	end;
end;

procedure TKCustomGradientText.SetAlignment( Value: TAlignment );
begin
	if ( FAlignment <> Value ) then
	begin
		FAlignment := Value;
		Invalidate;
	end;
end;

procedure TKCustomGradientText.SetShowAccelChar( Value: Boolean );
begin
	if ( FShowAccelChar <> Value ) then
	begin
		FShowAccelChar := Value;
		Invalidate;
	end;
end;

procedure TKCustomGradientText.SetFocusControl( Value: TWinControl );
begin
	FFocusControl := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

procedure TKCustomGradientText.CMDialogChar( var Message: TCMDialogChar );
begin
	if CheckObject( FFocusControl ) and ShowAccelChar and
		IsAccel( Message.CharCode, Caption ) then
		with FFocusControl do
			if CanFocus then
			begin
				SetFocus;
				Message.Result := 1;
			end;
end;

procedure TKCustomGradientText.Paint;
begin
	FGradient.OnGradientChange := FGradient.GradientChange;
end;

procedure TKCustomGradientText.DoPaint( const Value: string; Angle: TKAngle; AutoSize: Boolean );
const
	WordWraps: array[Boolean] of Word = ( 0, DT_WORDBREAK );
	ShowAccel: array[Boolean] of Word	= ( DT_NOPREFIX, 0 );
var
	iMinOffset, iH, iW, iX, iY: Integer;

	procedure SetTextAngle( Canvas: TCanvas );
	var
		FontRec: TLogFont;
	begin
		GetObject( Canvas.Font.Handle, SizeOf( FontRec ), Addr( FontRec ) );
		FontRec.lfEscapement := Angle * 10;
		FontRec.lfOrientation := Angle * 10;
		FontRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
		Canvas.Font.Handle := CreateFontIndirect( FontRec );
	end;

	procedure DrawEffect( eff: TKEffect; Canvas: TCanvas );
	var
		rt: TRect;
	begin
		Canvas.Font.Color := eff.Color;
		with eff do
			if ( Angle = 0 ) then
			begin
				rt := Rect( 
					EFFECT_OFFSETS[Direction, drX] * Depth - iMinOffset,
					EFFECT_OFFSETS[Direction, drY] * Depth - iMinOffset,
					EFFECT_OFFSETS[Direction, drX] * Depth - iMinOffset + ClientWidth,
					EFFECT_OFFSETS[Direction, drY] * Depth - iMinOffset + ClientHeight );
				DrawText( 	Canvas.Handle, PChar( Value ), -1, rt, DT_EXPANDTABS or
					WordWraps[WordWrap] or TEXT_ALIGNMENT[Alignment] or
					ShowAccel[ShowAccelChar] );
			end
			else
				Canvas.TextOut( EFFECT_OFFSETS[Direction, drX] * Depth - iMinOffset + iX,
												EFFECT_OFFSETS[Direction, drY] * Depth - iMinOffset + iY,
												Value );
	end;

var
	rtBMP: TRect;
	DCosAngle,
	DSinAngle: Double;
begin
	Canvas.Font := Font;
	bmpTEXT.Canvas.Font := Font;
	iMinOffset := Min( Min( Min( Min( 0,
									EFFECT_OFFSETS[FShadow.Direction, drX] * FShadow.Depth ),
									EFFECT_OFFSETS[FShadow.Direction, drY] * FShadow.Depth ),
									EFFECT_OFFSETS[FHighlight.Direction, drX] * FHighlight.Depth ),
									EFFECT_OFFSETS[FHighlight.Direction, drY] * FHighlight.Depth );

	if ( Angle <> 0 ) then
	begin
		SetTextAngle( Canvas );
		SetTextAngle( bmpTEXT.Canvas );
	end;
	iW := Canvas.TextWidth( Caption );
	iH := Canvas.TextHeight( Caption );
	if ( Angle = 0 ) then
	begin
		iX := 0;
		iY := 0;
		if AutoSize then
		begin
			Width := iW;
			Height := iH;
		end;
	end
	else
	begin
		DCosAngle := Cos( DEGREE_TO_RADIAN * Angle );
		DSinAngle := Sin( DEGREE_TO_RADIAN * Angle );
		Width := Trunc( Abs( iW * DCosAngle ) + Abs( iH * DSinAngle ) );
		Height := Trunc( Abs( iH * DCosAngle ) + Abs( iW * DSinAngle ) );
		case Angle of
				 1..90:
					begin
						iX := 0;
						iY := Height - Trunc( iH * DCosAngle );
					end;
			 91..180:
					begin
						iX := Width - Trunc( iH * DSinAngle );
						iY := Height;
					end;
			181..270:
					begin
						iX := Width;
						iY := -Trunc( iH * DCosAngle );
					end;
			271..360:
					begin
						iX := Width - Trunc( iW * DCosAngle );
						iY := 0;
					end;
		end;
	end;
{ Prepare the Background }
	bmpBKGND.Width := Width;
	bmpBKGND.Height := Height;
	bmpTEMP.Width := Width;
	bmpTEMP.Height := Height;
	with FGradient do
		if ( GradientStyle = gsNone ) or ( BeginColor = EndColor ) then
		begin
			rtBMP := Rect( 0, 0, bmpBKGND.Width, bmpBKGND.Height );
			if ( BeginColor = EndColor ) then
				bmpBKGND.Canvas.Brush.Color := BeginColor
			else
				bmpBKGND.Canvas.Brush.Color := Color;
			bmpBKGND.Canvas.FillRect( rtBMP );
		end;
	bmpTEXT.Width := bmpBKGND.Width;
	bmpTEXT.Height := bmpBKGND.Height;
	rtBMP := Rect( 0, 0, bmpTEXT.Width, bmpTEXT.Height );
	bmpTEXT.Canvas.FillRect( rtBMP );
	bmpTEMP.Canvas.FillRect( rtBMP );
	if ( Angle = 0 ) then
	begin
		rtBMP := Rect( -iMinOffset, -iMinOffset,
									 ClientWidth - iMinOffset, ClientHeight - iMinOffset );
		DrawText( 	bmpTEXT.Canvas.Handle, PChar( Value ), -1, rtBMP, DT_EXPANDTABS or
			WordWraps[WordWrap] or TEXT_ALIGNMENT[Alignment] or	ShowAccel[ShowAccelChar] );
	end
	else
		bmpTEXT.Canvas.TextOut( iX - iMinOffset, iY - iMinOffset, Value );
	if ( FGradient.GradientStyle <> gsNone ) then
		FGradient.Changed( false );
	bmpBKGND.Canvas.Draw( 0, 0, bmpTEXT );
{ Draw the Shadow and Highligh effects }
	if ( Shadow.Depth > 0 ) then
		DrawEffect( Shadow, Canvas );
	if ( HighLight.Depth > 0 ) then
		DrawEffect( HighLight, Canvas );
{ Draw the text on our canvas }
//	BitBlt( bmpTEMP.Canvas.Handle, iLeftOffset, 0, bmpBKGND.Width - iLeftOffset,
//		bmpBKGND.Height, bmpBKGND.Canvas.Handle, 0, 0, SRCAND );
//	if ( iLeftOffset > 0 ) then
//		BitBlt( bmpTEMP.Canvas.Handle, 0, 0, iLeftOffset,	bmpBKGND.Height,
//			bmpBKGND.Canvas.Handle, bmpBKGND.Width - iLeftOffset, 0, SRCAND );
	Canvas.Draw( 0, 0, bmpBKGND );
end;

{
--------------------------------------------------------------------------------
------------------------- Basic WinControl Classes -----------------------------
--------------------------------------------------------------------------------
}

const
	InitRepeatPause = 400;  { pause before repeat timer ( ms ) }
	RepeatPause     = 100;  { pause before hint window displays ( ms ) }

{ TKCustomSpeedButton }

constructor TKCustomSpeedButton.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FGlyph := TBitmap.Create;
	FGlyph.OnChange := GlyphChanged;
	Width := 20;
	Height := 24;
	FDown := sbsNone;
	FLastDown := sbsNone;
	FMouseDown := false;
	FTimerEnabled := true;
	FClickDelayed := false;
	FButtonStyle := esRxUpDown;
end;

destructor TKCustomSpeedButton.Destroy;
begin
	bmpUp.Free;
	bmpDown.Free;
	FGlyph.Free;
	FRepeatTimer.Free;
	inherited Destroy;
end;

procedure TKCustomSpeedButton.Changed;
begin
	Invalidate;
end;

procedure TKCustomSpeedButton.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
	AHeight := AHeight + ( AHeight mod 2 );
	FUpRect := Rect( 0, 0, AWidth - 1, ( AHeight div 2 ) );
	FDownRect := Rect( 0, ( AHeight div 2 ), AWidth - 1, AHeight );
	inherited SetBounds( ALeft, ATop, AWidth, AHeight );
end;

procedure TKCustomSpeedButton.SetGlyph( Value: TBitmap );
begin
	FGlyph.Assign( Value );
end;

procedure TKCustomSpeedButton.GlyphChanged( Sender: TObject );
begin
	Changed;
end;

procedure TKCustomSpeedButton.SetWidth( Value: Integer );
begin
	FUpRect := Rect( 0, 0, Value - 1, ( Height div 2 ) );
	FDownRect := Rect( 0, ( Height div 2 ), Value - 1, Height );
	inherited Width := Value;
end;

procedure TKCustomSpeedButton.SetHeight( Value: Integer );
begin
	Value := Value + ( Value mod 2 );
	FUpRect := Rect( 0, 0, Width - 1, ( Value div 2 ) );
	FDownRect := Rect( 0, ( Value div 2 ), Width - 1, Value );
	inherited Height := Value;
end;

procedure TKCustomSpeedButton.SetTimerEnabled( Value: Boolean );
begin
	if ( Value <> FTimerEnabled ) then
	begin
		FTimerEnabled := Value;
		if ( CheckObject( FRepeatTimer ) and ( not FTimerEnabled ) and FRepeatTimer.Enabled ) then
			FRepeatTimer.Enabled := false;
	end;
end;

procedure TKCustomSpeedButton.SetDown( Value: TKSpeedButtonState );
begin
	if ( FDown <> Value ) then
	begin
		FDown := Value;
		Changed;
	end;
end;

procedure TKCustomSpeedButton.SetButtonStyle( Value: TKEditorStyle );
begin
	if ( FButtonStyle <> Value ) then
	begin
		FButtonStyle := Value;
		Changed;
	end;
end;

procedure TKCustomSpeedButton.DoDrawButton;
begin
	if Assigned( FOnDrawButton ) then
		FOnDrawButton( Self, Canvas, ClientRect );
end;

procedure TKCustomSpeedButton.DrawRXButtons;
var
	R,
	RSrc: TRect;
	dRect: Integer;
	bmp: TBitmap;
begin
	if ( bmpUp = nil ) then
	begin
		bmpUp := TBitmap.Create;
		bmpUp.Handle := LoadBitmap( HInstance, 'KRXSPINUP' );
	end;
	if ( bmpDown = nil ) then
	begin
		bmpDown := TBitmap.Create;
		bmpDown.Handle := LoadBitmap( HInstance, 'KRXSPINDOWN' );
	end;
	bmp := TBitmap.Create;
	try
		bmp.Width := Width;
		bmp.Height := Height;
		with bmp.Canvas do
		begin
{ Fill the button background }
			R := Bounds( 0, 0, Width, Height );
			Pen.Width := 1;
			Brush.Color := GetSystemColor( deBtnFace );
			Brush.Style := bsSolid;
			FillRect( R );
{ dividing line }
			Pen.Color := GetSystemColor( deWindowFrame );
			Rectangle( 0, 0, Width, Height );
			MoveTo( -1, Height );
			LineTo( Width, -1 );
{ top button }
			if ( FDown = sbsUp ) then
				Pen.Color := GetSystemColor( deBtnShadow )
			else
				Pen.Color := GetSystemColor( deBtnHighlight );
			MoveTo( 1, Height - 4 );
			LineTo( 1, 1 );
			LineTo( Width - 3, 1 );
			if ( FDown = sbsUp ) then
				Pen.Color := GetSystemColor( deBtnHighlight )
			else
				Pen.Color := GetSystemColor( deBtnShadow );
			if ( FDown <> sbsUp ) then
			begin
				MoveTo( 1, Height - 3 );
				LineTo( Width - 2, 0 );
			end;
{ bottom button }
			if ( FDown = sbsDown ) then
				Pen.Color := GetSystemColor( deBtnHighlight )
			else
				Pen.Color := GetSystemColor( deBtnShadow );
			MoveTo( 2, Height - 2 );
			LineTo( Width - 2, Height - 2 );
			LineTo( Width - 2, 1 );
			if ( FDown = sbsDown ) then
				Pen.Color := GetSystemColor( deBtnShadow )
			else
				Pen.Color := GetSystemColor( deBtnHighlight );
			MoveTo( 2, Height - 2 );
			LineTo( Width - 1, 1 );
{ top glyph }
			dRect := 1;
			if ( FDown = sbsUp ) then
				Inc( dRect );
			R := Bounds( Round( ( Width / 4 ) - ( bmpUp.Width / 2 ) ) + dRect,
				Round( ( Height / 4 ) - ( bmpUp.Height / 2 ) ) + dRect, bmpUp.Width,
				bmpUp.Height );
			RSrc := Bounds( 0, 0, bmpUp.Width, bmpUp.Height );
			BrushCopy( R, bmpUp, RSrc, bmpUp.TransparentColor );
{ bottom glyph }
			dRect := -1;
			if ( FDown = sbsDown ) then
				Inc( dRect );
			R := Bounds( Round( ( 3 * Width / 4 ) - ( bmpDown.Width / 2 ) ) + dRect,
				Round( ( 3 * Height / 4 ) - ( bmpDown.Height / 2 ) ) + dRect,
				bmpDown.Width, bmpDown.Height );
			RSrc := Bounds( 0, 0, bmpDown.Width, bmpDown.Height );
			BrushCopy( R, bmpDown, RSrc, bmpDown.TransparentColor );
			if ( FDown = sbsDown ) then
			begin
				Pen.Color := GetSystemColor( deBtnShadow );
				MoveTo( 3, Height - 2 );
				LineTo( Width - 1, 2 );
			end;
		end;
		Canvas.Draw( 0, 0, bmp );
	finally
		bmp.Free;
	end;
end;

procedure TKCustomSpeedButton.Paint;
const
	BTN_SCROLL: array[TKEditorStyle] of UINT =
	( 0, 0, 0, 0, DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT, DFCS_SCROLLDOWN, DFCS_SCROLLUP, 0 );
	BTN_PUSHED: array[Boolean] of UINT = ( 0, DFCS_PUSHED );
	BTN_ENABLED: array[Boolean] of UINT = ( DFCS_INACTIVE, 0 );
	BTN_NAME: array[boolean] of PChar =
		( PChar( OBM_DNARROW ), PChar( OBM_RGARROW ) );
var
	Flags: UINT;
	old_f: TFont;
	iH,
	iW: Integer;
	bmp: TBitmap;
begin
	if ( FButtonStyle = esUpDown ) then
	begin
		bmp := TBitmap.Create;
		try
{ Draw uppy button }
			iW := Ord( FDown = sbsUp );
			bmp.Handle := LoadBitmap( HInstance, 'KSPINUP' );
			Flags := DFCS_BUTTONPUSH or BTN_PUSHED[FDown = sbsUp] or BTN_ENABLED[Enabled];
			DrawFrameControl( Canvas.Handle, FUpRect, DFC_BUTTON, Flags );
			BitBlt( Canvas.Handle, ( FUpRect.Right - bmp.Width ) div 2 + iW,
				( FUpRect.Bottom - bmp.Height ) div 2, bmp.Width, bmp.Height, bmp.Canvas.Handle, 0, 0,
				SRCAND );
//			Flags := DFCS_SCROLLUP or BTN_PUSHED[FDown = sbsUp] or BTN_ENABLED[Enabled];
//			DrawFrameControl( Canvas.Handle, FUpRect, DFC_SCROLL, Flags );
{ Draw downy button }
			iW := Ord( FDown = sbsDown );
			bmp.Handle := LoadBitmap( HInstance, 'KSPINDOWN' );
			Flags := DFCS_BUTTONPUSH or BTN_PUSHED[FDown = sbsDown] or BTN_ENABLED[Enabled];
			DrawFrameControl( Canvas.Handle, FDownRect, DFC_BUTTON, Flags );
			BitBlt( Canvas.Handle, ( FDownRect.Right - bmp.Width ) div 2 + iW,
				FDownRect.Top + ( FDownRect.Bottom - FDownRect.Top - bmp.Height ) div 2,
				bmp.Width, bmp.Height, bmp.Canvas.Handle, 0, 0, SRCAND );
//			Flags := DFCS_SCROLLDOWN or BTN_PUSHED[FDown = sbsDown] or BTN_ENABLED[Enabled];
//			DrawFrameControl( Canvas.Handle, FDownRect, DFC_SCROLL, Flags );
		finally
			bmp.Free;
		end;
	end
	else if ( FButtonStyle = esRXUpDown ) then
		DrawRXButtons
	else
	begin
		if ( FButtonStyle in [esRightArrow, esLeftArrow, esDownArrow, esUpArrow] ) then
		begin
{ Draw arrow button }
			Flags := BTN_SCROLL[FButtonStyle] or BTN_PUSHED[FDown = sbsBtnDown] or
				BTN_ENABLED[Enabled];
			DrawFrameControl( Canvas.Handle, ClientRect, DFC_SCROLL, Flags );
		end
		else
		begin
			Flags := DFCS_BUTTONPUSH or BTN_PUSHED[FDown = sbsBtnDown] or BTN_ENABLED[Enabled];
			DrawFrameControl( Canvas.Handle, ClientRect, DFC_BUTTON, Flags );
			if ( FButtonStyle = esGlyph ) and ( not FGlyph.Empty ) then
			begin
{ Glyph }
				iW := Ord( FDown = sbsBtnDown );
				if ( FGlyph.Width > Width - 2 - iW ) or
					 ( FGlyph.Height > Height - 2 - iW ) then
					StretchBlt( Canvas.Handle, 1 + iW, 1 + iW, Width - 2 - iW,
						Height - 2 - iW, FGlyph.Canvas.Handle, 0, 0, FGlyph.Width,
						FGlyph.Height, SRCCOPY )
				else
					BitBlt( Canvas.Handle, iW + ( Width - FGlyph.Width ) div 2,
						iW + ( Height - FGlyph.Height ) div 2, FGlyph.Width, FGlyph.Height,
						FGlyph.Canvas.Handle, 0, 0, SRCCOPY );
			end
			else if ( FButtonStyle = esEllipse ) then
			begin
{ Ellipse }
				old_f := Canvas.Font;
				try
					with Canvas do
					begin
						Brush.Style := bsClear;
						Font.Name := 'Times New Roman';
						Font.Style := [fsBold];
						iH := TextHeight( '...' );
						iW := TextWidth( '...' );
						Canvas.TextOut( ( Width - iW ) div 2 + Ord( FDown = sbsBtnDown ),
							( Height - iH ) div 3 + Ord( FDown = sbsBtnDown ), '...' );
					end;
				finally
					Canvas.Font := old_f;
				end;
			end
			else
{ OwnerDraw }
				DoDrawButton;
		end;
	end;
end;

procedure TKCustomSpeedButton.CMEnabledChanged( var Message: TMessage );
begin
	inherited;
  Changed;
end;

procedure TKCustomSpeedButton.Click;
begin
	if FMouseDown then
		FClickDelayed := true;
	if ( FClickDelayed or Designing( Self ) or ( ButtonStyle in [esRxUpDown, esUpDown] ) ) then
		Exit;
	inherited Click;
	if Assigned( OnClick ) then
		if ( not ( csLButtonDown in ControlState ) ) then
			FDown := sbsNone;
end;

procedure TKCustomSpeedButton.UpClick;
begin
	if Assigned( FOnUpClick ) then
	begin
		FOnUpClick( Self );
		if ( not ( csLButtonDown in ControlState ) ) then
			FDown := sbsNone;
	end;
end;

procedure TKCustomSpeedButton.DownClick;
begin
	if Assigned( FOnDownClick ) then
	begin
		FOnDownClick( Self );
		if ( not ( csLButtonDown in ControlState ) ) then
			FDown := sbsNone;
	end;
end;

procedure TKCustomSpeedButton.MouseDown( Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer );
begin
	inherited MouseDown( Button, Shift, X, Y );
	if Designing( Parent ) then
		Exit;
	if ( ( Enabled ) and ( Button = mbLeft ) and ( FDown = sbsNone ) ) then
	begin
		FLastDown := FDown;
		if ( FButtonStyle in [esUpDown, esRXUpDown] ) then
		begin
			if PtInRect( FDownRect, Point( X, Y ) ) then
				FDown := sbsDown
			else if PtInRect( Rect( 0, 0, FUpRect.Right, FUpRect.Bottom - 1 ), Point( X, Y ) ) then
				FDown := sbsUp
		end
		else if PtInRect( ClientRect, Point( X, Y ) ) then
			FDown := sbsBtnDown;
		if ( FLastDown <> FDown ) then
		begin
			FLastDown := FDown;
			Repaint;
		end;
		if FTimerEnabled then
		begin
			if ( not CheckObject( FRepeatTimer ) ) then
				FRepeatTimer := TTimer.Create( Self );
			FRepeatTimer.OnTimer := TimerExpired;
			FRepeatTimer.Interval := InitRepeatPause;
			FRepeatTimer.Enabled := True;
		end;
		FMouseDown := True;
	end;
end;

procedure TKCustomSpeedButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
	NewState: TKSpeedButtonState;
begin
	inherited MouseMove( Shift, X, Y );
	if Designing( Parent ) then
		Exit;
	if ( Cursor <> crHourGlass ) then
		Cursor := crArrow;
	if ( not FMouseDown ) then
		Exit;
	if ( FButtonStyle in [esUpDown, esRXUpDown] ) then
	begin
		if PtInRect( ClientRect, Point( X, Y ) ) then
		begin
			NewState := FDown;
			if PtInRect( FDownRect, Point( X, Y ) ) then
			begin
				if ( FDown <> sbsDown ) then
				begin
					if ( FLastDown = sbsDown ) then
						FDown := sbsDown
					else
						FDown := sbsNone;
					if ( NewState <> FDown ) then
						Repaint;
				end;
			end
			else
			begin
				if ( FDown <> sbsUp ) then
				begin
					if ( FLastDown = sbsUp ) then
						FDown := sbsUp
					else
						FDown := sbsNone;
					if ( NewState <> FDown ) then
						Repaint;
				end;
			end;
		end
		else if ( FDown <> sbsNone ) then
		begin
			FDown := sbsNone;
			Repaint;
		end;
		Exit;
	end;
	if PtInRect( ClientRect, Point( X, Y ) ) then
	begin
		NewState := FDown;
		if ( FDown <> sbsBtnDown ) then
		begin
			if ( FLastDown = sbsBtnDown ) then
				FDown := sbsBtnDown
			else
				FDown := sbsNone;
			if ( NewState <> FDown ) then
				Repaint;
		end;
	end
	else if ( FDown <> sbsNone ) then
	begin
		FDown := sbsNone;
		Repaint;
	end;
end;

procedure TKCustomSpeedButton.MouseUp( Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer );
begin
	inherited MouseUp( Button, Shift, X, Y );
	if Designing( Parent ) then
		Exit;
	if FMouseDown then
	begin
		FMouseDown := False;
		if ( FButtonStyle in [esUpDown, esRXUpDown] ) then
		begin
			if PtInRect( FDownRect, Point( X, Y ) ) then
				DownClick
			else if PtInRect( Rect( 0, 0, FUpRect.Right, FUpRect.Bottom - 1 ), Point( X, Y ) ) then
				UpClick;
		end;
{ TGraphicControl.Click will occur on the inherited call; it should not occur for up/down buttons... }
		if PtInRect( ClientRect, Point( X, Y ) ) then
		begin
			FDown := sbsNone;
			FLastDown := sbsNone;
			Repaint;
		end;
	end;
	if FClickDelayed then
	begin
		FClickDelayed := false;
		Click;
	end;
end;

procedure TKCustomSpeedButton.TimerExpired( Sender: TObject );
begin
	FRepeatTimer.Interval := RepeatPause;
	if ( ( FDown <> sbsNone ) and MouseCapture ) then
	begin
		try
			if ( FDown = sbsDown ) then
				DownClick
			else if ( FDown = sbsUp ) then
				UpClick
			else
				Click;
		except
			FRepeatTimer.Enabled := False;
			raise;
		end;
	end;
	if ( not FTimerEnabled ) then
		FRepeatTimer.Enabled := false;
end;

{ TKCustomSpeedControl }

constructor TKCustomSpeedControl.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAlignment := taLeftJustify;
	FButton := TKCustomSpeedButton.Create( nil );
	FButton.Visible := True;
	FButton.Parent := Self;
	FButton.OnClick := ButtonClickEvent;
	EditorStyle := GetDefaultEditorStyle;
	ControlStyle := ControlStyle - [csSetCaption];
	FEditorEnabled := True;
	FButtonClicked := false;
	FClipboardAction := false;
	FNullCommandPressed := false;
	FIsNull := false;
	FKeepNull := false;
	FAllowNull := false;
	FNullCommand := SC_CTRL_N;
	FButtonCommand := SC_CTRL_RETURN;
	Width := 141;
end;

destructor TKCustomSpeedControl.Destroy;
begin
	FButton.Free;
	inherited Destroy;
end;

function TKCustomSpeedControl.GetDefaultEditorStyle: TKEditorStyle;
begin
	Result := esUpDown;
end;

procedure TKCustomSpeedControl.Loaded;
begin
	inherited Loaded;
	InternalText := FLoadingValue;
	DisplayChanged;
	if ( not ( AllowNull or CheckStr( Text ) ) ) then
		RaiseException( EKSpeedControl, sErrSCInvAllowNull );
end;

procedure TKCustomSpeedControl.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
end;

function TKCustomSpeedControl.GetText: string;
begin
	Result := inherited Text;
end;

procedure TKCustomSpeedControl.SetInternalText( const NewValue: string );
{ This is a write-only property because Text had to be made readonly. Thus,
	to change text, we use this write-only property; to read it, we keep the
	old Text property <g> }
begin
	if Loading( Self ) then
		FLoadingValue := NewValue
	else
		inherited Text := NewValue;
end;

procedure TKCustomSpeedControl.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( EditorStyle in [esUpDown, esRxUpDown] ) then
	begin
		if ( Key = VK_UP ) then
		begin
			Key := 0;
			UpClickEvent( Self );
		end
		else if ( Key = VK_DOWN ) then
		begin
			Key := 0;
			DownClickEvent( Self );
		end;
	end
	else if ( ShortCut( Key, Shift ) = FButtonCommand ) then
	begin
		Key := 0;
		FButtonClicked := true; { Before the effective click... }
		FButton.Click;
	end;
	FClipboardAction := ( ShortCut( Key, Shift ) = SC_CTRL_C ) or
		( ShortCut( Key, Shift ) = SC_CTRL_X ) or
		( ShortCut( Key, Shift ) = SC_CTRL_V );
	if ( ShortCut( Key, Shift ) = FNullCommand ) then
	begin
		if FAllowNull then
		begin
			Key := 0;
			DoNullCommand;
			FNullCommandPressed := true;
		end
		else
			MessageBeep( 0 );
	end
	else if ( ShortCut( Key, Shift ) = SC_CTRL_C ) then
	begin
		Key := 0;
		CopyToClipboard;
	end
	else if ( ShortCut( Key, Shift ) = SC_CTRL_X ) then
	begin
		Key := 0;
		CutToClipboard;
	end
	else if ( ShortCut( Key, Shift ) = SC_CTRL_V ) then
	begin
		Key := 0;
		PasteFromClipboard;
	end;
	if ( not ( FButtonClicked or FNullCommandPressed ) ) then
		inherited KeyDown( Key, Shift );
end;

procedure TKCustomSpeedControl.KeyPress( var Key: Char );
begin
	if ( FButtonClicked or FNullCommandPressed or FClipboardAction ) then
		Key := CH_NULL
	else if ( Key <> CH_NULL ) and ( not IsValidChar( Key ) ) then
	begin
		Key := CH_NULL;
		MessageBeep( 0 );
	end;
	if ( Key <> CH_NULL ) and ( not ( FButtonClicked or FNullCommandPressed or FClipboardAction ) ) then
		inherited KeyPress( Key );
	FButtonClicked := false;
	FClipboardAction := false;
	FNullCommandPressed := false;
end;

function TKCustomSpeedControl.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and ( ( Key >= CH_SPACE ) or
		( Key = Char( VK_BACK ) ) );
end;

procedure TKCustomSpeedControl.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
	inherited MouseMove( Shift, X, Y );
	if ( Cursor <> crHourGlass ) then
		Cursor := crDefault;
end;

procedure TKCustomSpeedControl.CreateParams( var Params: TCreateParams );
const
	Alignments: array[TAlignment] of Byte = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
	inherited CreateParams( Params );
	Params.Style := Params.Style or Alignments[FAlignment] or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TKCustomSpeedControl.CreateWnd;
begin
	inherited CreateWnd;
	SetEditRect;
end;

procedure TKCustomSpeedControl.Change;
begin
	if ( FIsNull and CheckStr( Text ) and ( not FKeepNull ) ) then
		FIsNull := false;
	if ( not FIsNull ) then
		ParseValue;
	inherited Change;
end;

procedure TKCustomSpeedControl.SetAlignment( Value: TAlignment );
begin
	if FAlignment <> Value then
	begin
		FAlignment := Value;
		RecreateWnd;
	end;
end;

procedure TKCustomSpeedControl.SetButtonCommand( NewValue: TShortCut );
begin
	if ( FButtonCommand <> NewValue ) and
		 ( NewValue <> FNullCommand ) and
		 ( NewValue <> SC_CTRL_C ) and
		 ( NewValue <> SC_CTRL_X ) and
		 ( NewValue <> SC_CTRL_V ) then
		FButtonCommand := NewValue
	else if Designing( Self ) then
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.SetNullCommand( NewValue: TShortCut );
begin
	if ( FNullCommand <> NewValue ) and
		 ( NewValue <> FButtonCommand ) and
		 ( NewValue <> SC_CTRL_C ) and
		 ( NewValue <> SC_CTRL_X ) and
		 ( NewValue <> SC_CTRL_V ) then
		FNullCommand := NewValue
	else if Designing( Self ) then
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.SetAllowNull( NewValue: Boolean );
begin
	if ( FAllowNull <> NewValue ) then
	begin
		if ( not ( Loading( Self ) or NewValue or CheckStr( Text ) ) ) then
			RaiseException( EKSpeedControl, sErrSCInvAllowNull );
		FAllowNull := NewValue;
	end;
end;

procedure TKCustomSpeedControl.SetEditRect;
var
	Loc: TRect;
begin
	if ( Parent <> nil ) then
	begin
		SendMessage( Handle, EM_GETRECT, 0, LongInt( @Loc ) );
		Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
		Loc.Right := ClientWidth - FButton.Width { - 2 };
		Loc.Top := 0;
		Loc.Left := 0;
		SendMessage( Handle, EM_SETRECTNP, 0, LongInt( @Loc ) );
		SendMessage( Handle, EM_GETRECT, 0, LongInt( @Loc ) );  {debug}
	end;	
end;

procedure TKCustomSpeedControl.AdjustControl;
var
	MinHeight: Integer;
begin
	MinHeight := GetMinHeight;
{
	text edit bug: if size to less than minheight, then edit ctrl does not display the text
}
	if ( Height < MinHeight ) then
		Height := MinHeight
	else if CheckObject( FButton ) then
	begin
		if ( NewStyleControls and Ctl3D ) then
			FButton.SetBounds( Width - FButton.Width - 3 - Ord( EditorStyle <> esUpDown ), 0,
				FButton.Width, Height - 5 )
		else
			FButton.SetBounds( Width - FButton.Width, 1, FButton.Width, Height - 3 );
		SetEditRect;
	end;
end;

procedure TKCustomSpeedControl.WMSize( var Message: TWMSize );
begin
	inherited;
	AdjustControl;
end;

function TKCustomSpeedControl.GetEditorStyle: TKEditorStyle;
begin
	Result := FButton.ButtonStyle;
end;

procedure TKCustomSpeedControl.SetEditorStyle( Value: TKEditorStyle );
begin
	with FButton do
	begin
		OnClick := nil;
		OnUpClick := nil;
		OnDownClick := nil;
		OnDrawButton := nil;
		ButtonStyle := Value;
		if ( Value in [esUpDown, esRxUpDown] ) then
		begin
			if ( Value = esUpDown ) then
				Width := Max( 16, Self.ButtonWidth )
			else
				Width := Height;
			TimerEnabled := true;
			OnUpClick := Self.UpClickEvent;
			OnDownClick := Self.DownClickEvent;
		end
		else
		begin
			TimerEnabled := false;
			OnClick := Self.ButtonClickEvent;
			if ( Value = esOwnerDraw ) then
				OnDrawButton := Self.DrawButtonEvent;
		end;
		AdjustControl;
	end;
end;

function TKCustomSpeedControl.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC( 0 );
	GetTextMetrics( DC, SysMetrics );
  SaveFont := SelectObject( DC, Font.Handle );
  GetTextMetrics( DC, Metrics );
	SelectObject( DC, SaveFont );
  ReleaseDC( 0, DC );
  I := SysMetrics.tmHeight;
	if ( I > Metrics.tmHeight ) then
		I := Metrics.tmHeight;
	Result := Metrics.tmHeight + I div 4 + GetSystemMetrics( SM_CYBORDER ) * 4 {+ 2 };
end;

function TKCustomSpeedControl.GetButtonHint: string;
begin
	Result := FButton.Hint;
end;

function TKCustomSpeedControl.GetButtonGlyph: TBitmap;
begin
	Result := FButton.Glyph;
end;

function TKCustomSpeedControl.GetButtonWidth: Integer;
begin
	Result := FButton.Width;
end;

procedure TKCustomSpeedControl.SetButtonWidth( Value: Integer );
begin
	FButton.Width := Value;
	AdjustControl;
end;

procedure TKCustomSpeedControl.CMEnabledChanged( var Message: TMessage );
begin
	inherited;
	FButton.Enabled := Enabled;
end;

procedure TKCustomSpeedControl.SetShowHint( Value: Boolean );
begin
	inherited ShowHint := Value;
	FButton.ShowHint := Value;
end;

procedure TKCustomSpeedControl.SetButtonGlyph( Value: TBitmap );
begin
	FButton.Glyph := Value;
end;

procedure TKCustomSpeedControl.SetButtonHint( const Value: string );
begin
	FButton.Hint := Value;
end;

procedure TKCustomSpeedControl.DoButtonClick;
begin
	if Assigned( FOnButtonClick ) then
		FOnButtonClick( Self )
	else if ReadOnly then
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.ButtonClick;
var
	SavedClick: TNotifyEvent;
begin
	SavedClick := OnButtonClick;
	try
		OnButtonClick := nil;
		DoButtonClick;
	finally
		OnButtonClick := SavedClick;
	end;
end;

procedure TKCustomSpeedControl.ButtonClickEvent( Sender: TObject );
begin
	if Designing( Self ) then
		Exit;
	DoButtonClick;
end;

procedure TKCustomSpeedControl.DoNullCommand;
begin
	InternalText := '';
	FIsNull := true;
	SetNullValue;
end;

procedure TKCustomSpeedControl.DoUpClick;
begin
	if Assigned( FOnUpClick ) then
		FOnUpClick( Self )
	else if ReadOnly then
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.UpClick;
var
	SavedClick: TNotifyEvent;
begin
	SavedClick := OnUpClick;
	try
		OnUpClick := nil;
		DoUpClick;
	finally
		OnUpClick := SavedClick;
	end;
end;

procedure TKCustomSpeedControl.UpClickEvent( Sender: TObject );
begin
	if Designing( Parent ) then
		Exit;
	DoUpClick;
end;

procedure TKCustomSpeedControl.DoDownClick;
begin
	if Assigned( FOnDownClick ) then
		FOnDownClick( Self )
	else if ReadOnly then
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.DownClick;
var
	SavedClick: TNotifyEvent;
begin
	SavedClick := OnDownClick;
	try
		OnDownClick := nil;
		DoDownClick;
	finally
		OnDownClick := SavedClick;
	end;
end;

procedure TKCustomSpeedControl.DownClickEvent( Sender: TObject );
begin
	if Designing( Parent ) then
		Exit;
	DoDownClick;
end;

procedure TKCustomSpeedControl.DoDrawButton( ACanvas: TCanvas; ARect: TRect );
begin
	if Assigned( FOnDrawButton ) then
		FOnDrawButton( Self, ACanvas, ARect );
end;

procedure TKCustomSpeedControl.DrawButtonEvent( Sender: TObject; ACanvas: TCanvas;
	ARect: TRect );
begin
	DoDrawButton( ACanvas, ARect );
end;

function TKCustomSpeedControl.DoPaste: Boolean;
var
	sText: string;
begin
	Result := FEditorEnabled and ( not ReadOnly ) and Clipboard.HasFormat( CF_TEXT );
	if ( not Result ) then
		Exit;
	sText := Clipboard.AsText;
	while ( CheckStr( sText ) and Result ) do
	begin
		Result := IsValidChar( sText[1] );
		Delete( sText, 1, 1 );
	end;
end;

procedure TKCustomSpeedControl.WMPaste( var Message: TWMPaste );
begin
	if DoPaste then
		inherited
	else
		MessageBeep( 0 );
end;

function TKCustomSpeedControl.DoCut: Boolean;
begin
	Result := ( FEditorEnabled and ( not ReadOnly ) and ( PasswordChar = CH_NULL ) );
end;

procedure TKCustomSpeedControl.WMCut( var Message: TWMPaste );
begin
	if DoCut then
		inherited
	else
		MessageBeep( 0 );
end;

procedure TKCustomSpeedControl.CMExit( var Message: TCMExit );
begin
	inherited;
	UpdateValue;
end;

procedure TKCustomSpeedControl.CMVisibleChanged( var Message: TMessage );
begin
	inherited;
	DisplayChanged;
end;

function TKCustomSpeedControl.CheckTextValue( const NewValue: string ): string;
begin
	Result := NewValue;
	if ( not ( FAllowNull or CheckStr( Result ) ) ) then
		Result := Text;
end;

function TKCustomSpeedControl.Editing: Boolean;
begin
  Result := ( FEditorEnabled and ( SelLength > 0 ) );
end;

procedure TKCustomSpeedControl.UpdateValue;
begin
{
	Update the contents of the control according to the current value of the
	editor: « from Spin.pas »
	if ( CheckValue( Value ) <> Value ) then
		SetValue ( Value );
}
end;

procedure TKCustomSpeedControl.CMEnter( var Message: TCMGotFocus );
begin
	if ( AutoSelect and ( not ( csLButtonDown in ControlState ) ) ) then
		SelectAll;
	inherited;
	if ( not IsNull ) then
		DisplayChanged;
end;

{------------------------------ TKCustomSpeedText -------------------------------}

constructor TKCustomSpeedText.Create( AOwner: TComponent );
begin
{ To allow Dialog to instantiate this kind of control without Standard or DBCtrls package }
	if ( not CheckObjectClass( AOwner, TKKernelBaseDialog ) ) then
		ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs, perStd, perDBCtrls] );
	inherited Create( AOwner );
	AllowNull := true;
	InternalText := '';
end;

function TKCustomSpeedText.GetDefaultEditorStyle: TKEditorStyle;
begin
	Result := esGlyph;
end;

procedure TKCustomSpeedText.SetNullValue;
begin
	Value := '';
end;

function TKCustomSpeedText.ParseValue: Boolean;
begin
	Result := true;
end;

procedure TKCustomSpeedText.DisplayChanged;
begin
	{ do nothing for now }
end;

function TKCustomSpeedText.GetValue: string;
begin
	if IsNull then
		Result := ''
	else
		Result := Text;
end;

procedure TKCustomSpeedText.SetValue( const NewValue: string );
begin
	InternalText := NewValue;
end;

function TKCustomSpeedText.CheckTextValue( const NewValue: string ): string;
begin
	Result := NewValue;
	if Assigned( FOnCheckValue ) then
		FOnCheckValue( Self, Result );
	Result := inherited CheckTextValue( Result );
end;

procedure TKCustomSpeedText.UpdateValue;
var
	s: string;
begin
	if ( not IsNull ) then
	begin
		s := CheckTextValue( Value );
		if ( not CheckStrEqual( s, Value ) ) then
			SetValue( s );
	end;
end;

{------------------------------ TKCustomSpeedFile -------------------------------}

constructor TKCustomSpeedFile.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	OnCheckValue := nil;
	OnDownClick := nil;
	OnUpClick := nil;
	FFilter := '';
	FTitle := '';
	FInitialDir := '';
	FDefExt := '';
	Value := '';
	FLastValid := '';
	FMustExist := false;
	FMultiSelect := false;
  FInitialDirAsLastDir := true;
end;

function TKCustomSpeedFile.GetDefaultEditorStyle: TKEditorStyle;
begin
	Result := esEllipse;
end;

function TKCustomSpeedFile.GetLastDir: string;
begin
  Result := GetFirstString( [ExtractFilePath( FLastValid ), FInitialDir] );
end;

procedure TKCustomSpeedFile.SetEditorStyle( Value: TKEditorStyle );
begin
	if ( Value in [esUpDown, esRxUpDown] ) then
		Exit;
	inherited SetEditorStyle( Value  );
end;

procedure TKCustomSpeedFile.SetValue( const NewValue: string );
begin
	inherited SetValue( NewValue );
	FLastValid := Value;
end;

function TKCustomSpeedFile.CheckTextValue( const NewValue: string ): string;
{
	After the analisys of all posible situations (16 cases) we found 4 cases to
	consider:

	a) Ok (implicity Ok and Clear - when valid) for events:
			15, 11, 10, 7, 3, 2 - for explicity ok cases
			13, 12,  9, 8,      - for implicity ok cases

	b) Error1 (Invalid File Value, it must exists) for events:
			14,  6  					 - for Not Allowed and Allowed null values

		 Solution: FLastValid Value or Raise if FLasValid is Null (Error3)

	c) Error2 (Invalid File Value, null values are not allowed) for events:
			 5,  4

		 Solution: FLastValid Value or Raise if FLasValid is Null (Error3)

	d) Error3 (Null values are not allowed) for events:
			 1,  0

		 Solution: Raise (if does not allow null values)

	PS: After all, the Error2 and Error3 can be treated as the same (just give to
			allow null greater priority)...
}
//( ( ( $0000, $0001 ), ( $0010, $0011 ) ), ( ( $0100, $0101 ), ( $0110, $0111 ) ) ),
//(	( ( $1000, $1001 ), ( $1010, $1011 ) ), ( ( $1100, $1101 ), ( $1110, $1111 ) ) )
const
	FILTER_MATRIX: array[Boolean, Boolean, Boolean, Boolean] of Byte =
	(
		(	( ( 0, 1 ),  ( 2,  3 ) ), (  ( 4,  5 ), (  6,  7 ) ) ),
		(	( ( 8, 9 ), ( 10, 11 ) ), ( ( 12, 13 ), ( 14, 15 ) ) )
	);
begin
	case FILTER_MATRIX[AllowNull, FMustExist, CheckStr( NewValue ), CheckFiles( NewValue )] of
		0, 1, 4, 5:
			if CheckStr( FLastValid ) then
				Result := FLastValid
			else
				RaiseException( EKSpeedControl, sErrSCInvAllowNull );

		2, 3, 7, 8, 9, 10, 11, 12, 13, 15:
			Result := inherited CheckTextValue( NewValue );

		6, 19:
			if CheckFile( FLastValid ) then
				Result := FLastValid
			else
				RaiseException( EKSpeedControl, sErrSCInvFileMustExist );
	end;
end;

procedure TKCustomSpeedFile.DoButtonClick;
const
	DLG_OPTIONS: array[Boolean] of TOpenOptions =
		( DEFAULT_SELECT_LOADSAVE_FILES + [ofAllowMultiSelect],
		  DEFAULT_SELECT_LOADSAVE_FILES + [ofAllowMultiSelect, ofFileMustExist] );
var
	sFile: string;
	sl: TStrings;
begin
	if Assigned( OnButtonClick ) then
		inherited DoButtonClick
	else
	begin
		if ( not MultiSelect ) then
			sFile := SelectLoadFile( FFilter, FTitle, FInitialDir, FDefExt )
		else
		begin
			sl := TStringList.Create;
			try
				SelectLoadFiles( FFilter, FTitle, FInitialDir, FDefExt, DLG_OPTIONS[MustExist], sl );
				sFile := MakeString( sl, CH_LIST_TOKEN, stStrings );
			finally
				sl.Free;
			end;
		end;	
		if CheckTrimStr( sFile ) then
			Value := CheckTextValue( sFile );
	end;
end;

procedure TKCustomSpeedFile.SetTitle( const Value: string );
begin
	FTitle := Value;
	if ( ( not ( Designing( Self ) or CheckTrimStr( Hint ) ) ) and ShowHint ) then
		Hint := FTitle;
end;

function TKCustomSpeedFile.CheckFiles( const s: string ): Boolean;
var
	i: Integer;
	pStr: PStringArray;
begin
	i := StringToArray( s, CH_LIST_TOKEN, nil );
  Result := ( i <> -1 );
  if ( not Result ) then
    Exit;
	GetMem( pStr, i * SizeOf( string ) );
	try
		Result := ( ( StringToArray( s, CH_LIST_TOKEN, pStr ) <> -1 ) and uksyUtils.CheckFiles( Slice( pStr^, i ) ) );
	finally
	  FreeMem( pStr, i * SizeOf( string ) );
	end;
end;

procedure TKCustomSpeedFile.SetMultiSelect( NewValue: Boolean );
begin
	if ( FMultiSelect <> NewValue ) then
	begin
		if ( NewValue and CheckStr( Value ) and MustExist and ( not CheckFiles( Value ) ) ) then
			RaiseException( EKSpeedControl, sErrSCInvFileMustExist );
		FMultiSelect := NewValue;
		if FMultiSelect then
			UpdateValue;
	end;
end;

procedure TKCustomSpeedFile.SetMustExist( NewValue: Boolean );
begin
	if ( FMustExist <> NewValue ) then
	begin
		if ( NewValue and CheckStr( Value ) and ( not CheckFiles( Value ) ) ) then
			RaiseException( EKSpeedControl, sErrSCInvFileMustExist );
		FMustExist := NewValue;
		if FMustExist then
			UpdateValue;
	end;
end;

{----------------------------- TKCustomSpeedFolder -----------------------------}

constructor TKCustomSpeedFolder.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	OnCheckValue := nil;
	OnDownClick := nil;
	OnUpClick := nil;
	FTitle := '';
	FInitialDir := '';
	Value := '';
	FLastValid := '';
	FMustExist := false;
	FMultiSelect := false;
  FInitialDirAsLastDir := true;
end;

function TKCustomSpeedFolder.GetLastDir: string;
begin
  if ( not MultiSelect ) then
    Result := GetFirstString( [FLastValid, FInitialDir] )
  else if CheckStrContains( CH_LIST_TOKEN, FLastValid ) then
    Result := GetFirstString( [Copy( FLastValid, 1, Pos( CH_LIST_TOKEN,
      FLastValid ) - 1 ), FInitialDir] )
  else
    Result := FInitialDir;  
end;

function TKCustomSpeedFolder.GetDefaultEditorStyle: TKEditorStyle;
begin
	Result := esEllipse;
end;

procedure TKCustomSpeedFolder.SetEditorStyle( Value: TKEditorStyle );
begin
	if ( Value in [esUpDown, esRxUpDown] ) then
		Exit;
	inherited SetEditorStyle( Value  );
end;

procedure TKCustomSpeedFolder.SetValue( const NewValue: string );
begin
	inherited SetValue( NewValue );
	FLastValid := Value;
end;

function TKCustomSpeedFolder.CheckPaths( const s: string ): Boolean;
var
	i: Integer;
	pStr: PStringArray;
begin
	i := StringToArray( s, CH_LIST_TOKEN, nil );
	GetMem( pStr, i );
	try
		Result := ( ( StringToArray( s, CH_LIST_TOKEN, pStr ) > -1 ) and uksyUtils.CheckPaths( pStr^ ) );
	finally
		FreeMem( pStr, i );
	end;
end;

function TKCustomSpeedFolder.CheckTextValue( const NewValue: string ): string;
const
	FILTER_MATRIX: array[Boolean, Boolean, Boolean, Boolean] of Byte =
	(
		(	( ( 0, 1 ),  ( 2,  3 ) ), (  ( 4,  5 ), (  6,  7 ) ) ),
		(	( ( 8, 9 ), ( 10, 11 ) ), ( ( 12, 13 ), ( 14, 15 ) ) )
	);
begin
	case FILTER_MATRIX[AllowNull, FMustExist, CheckStr( NewValue ), CheckPath( NewValue )] of
		0, 1, 4, 5:
			if CheckStr( FLastValid ) then
				Result := FLAstValid
			else
				RaiseException( EKSpeedControl, sErrSCInvAllowNull );

		2, 3, 7, 8, 9, 10,
		11, 12, 13, 15   : Result := inherited CheckTextValue( NewValue );

		6, 19:
			if CheckFile( FLastValid ) then
				Result := FLastValid
			else
				RaiseException( EKSpeedControl, sErrSCInvFileMustExist );
	end;
end;
procedure TKCustomSpeedFolder.DoButtonClick;
var
	sFile: string;
begin
	if Assigned( OnButtonClick ) then
		inherited DoButtonClick
	else
	begin
		if ShellBrowseFolder( FTitle, FInitialDir, sFile ) then
			Value := CheckTextValue( sFile );
	end;
end;

procedure TKCustomSpeedFolder.SetTitle( const Value: string );
begin
	FTitle := Value;
	if ( ( not ( Designing( Self ) or CheckTrimStr( Hint ) ) ) and ShowHint ) then
		Hint := FTitle;
end;

procedure TKCustomSpeedFolder.SetMultiSelect( NewValue: Boolean );
begin
	if ( FMultiSelect <> NewValue ) then
	begin
		if ( NewValue and CheckStr( Value ) and MustExist and ( not CheckPaths( Value ) ) ) then
			RaiseException( EKSpeedControl, sErrSCInvFileMustExist );
		FMultiSelect := NewValue;
		if FMultiSelect then
			UpdateValue;
	end;
end;

procedure TKCustomSpeedFolder.SetMustExist( NewValue: Boolean );
begin
	if ( FMustExist <> NewValue ) then
	begin
		if ( NewValue and CheckStr( Value ) and ( not CheckPaths( Value ) ) ) then
			RaiseException( EKSpeedControl, sErrSCInvFolderMustExist );
		FMustExist := NewValue;
		if FMustExist then
			UpdateValue;
	end;
end;

{------------------------------- TKFormattedSpeedControl -------------------------------}

constructor TKFormattedSpeedControl.Create( AOwner: TComponent );
begin
{ To allow Dialog to instantiate this kind of control without Standard or DBCtrls package }
	if ( not CheckObjectClass( AOwner, TKKernelBaseDialog ) ) then
		ForceAnyPackagesRunning( ClassName, [perDialogs, perDBDialogs, perStd, perDBCtrls] );
	inherited Create( AOwner );
	FValue := 0;
	FMinValue := 0;
	FMaxValue := 0;
	FIncrement := 1;
{ Make sure descendent classes have not already defined default format strings }
	if ( not CheckTrimStr( FEditFormat ) ) then
		FEditFormat := DEFAULT_FLOAT_FORMAT;
	if ( not CheckTrimStr( FDisplayFormat ) ) then
		FDisplayFormat := DEFAULT_FLOAT_FORMAT;
	InternalText := '0';
	Alignment := taRightJustify;
	DisplayChanged;
end;


procedure TKFormattedSpeedControl.DisplayChanged;
begin
	InternalText := GetText;
	if Showing then
		Invalidate;
end;

function TKFormattedSpeedControl.GetText: string;
begin
	if Focused then
		Result := FormatEditText
	else
		Result := FormatDisplayText;
	if ( not CheckTrimStr( Result ) ) then
		Result := FloatToStr( FValue );
end;

function TKFormattedSpeedControl.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := FormatFloat( AFormat, AValue );
end;

function TKFormattedSpeedControl.FormatDisplayText: string;
begin
	Result := FormatText( FDisplayFormat, FValue );
	if Assigned( FOnFormatDisplayText ) then
		FOnFormatDisplayText( Self, FValue, FDisplayFormat, Result );
end;

function TKFormattedSpeedControl.FormatEditText: string;
begin
	Result := FormatText( FEditFormat, FValue );
	if Assigned( FOnFormatEditText ) then
		FOnFormatEditText( Self, FValue, FEditFormat, Result );
end;

procedure TKFormattedSpeedControl.SetNullValue;
begin
	Value := 0; 
end;

function TKFormattedSpeedControl.ParseValue: Boolean;
var
	e: Integer;
	x: Extended;
	s: string;
begin
	Result := false;
	s := StringReplace( Text, DecimalSeparator, CH_DOTMARK, krfAll );
	Val( s, x, e );
	if ( e = 0 ) then
	begin
		Value := x;
		Result := True;
	end;
end;

procedure TKFormattedSpeedControl.SetMaxValue( NewValue: Extended );
begin
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
	begin
		FMaxValue := NewValue;
		DisplayChanged;
	end;
end;

procedure TKFormattedSpeedControl.SetMinValue( NewValue: Extended );
begin
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) then
	begin
		FMinValue := NewValue;
		DisplayChanged;
	end;
end;

function TKFormattedSpeedControl.CheckValue( NewValue: Extended ): Extended;
begin
	Result := NewValue;
{ If ( MinValue = MaxValue ), the range is not being controlled }
	if ( FMaxValue <> FMinValue ) then
	begin
		if ( NewValue < FMinValue ) then
			Result := FMinValue
		else if ( NewValue > FMaxValue ) then
			Result := FMaxValue;
	end;
end;

function TKFormattedSpeedControl.GetValue: Extended;
begin
	if IsNull then
		Result := 0
	else
	begin
		if ( not ParseValue ) then
			DisplayChanged;
		Result := FValue;
	end;
end;

procedure TKFormattedSpeedControl.SetValue( NewValue: Extended );
begin
	FValue := CheckValue( NewValue );
	{ InternalText := GetText; already called in DisplayChanged }
	DisplayChanged;
end;

procedure TKFormattedSpeedControl.UpdateValue;
begin
{ called right after CM_EXIT }
	if ( not IsNull ) then
	begin
		if ( CheckValue( Value ) <> Value ) then
			SetValue( Value )
		else
			DisplayChanged;
	end;
end;

function TKFormattedSpeedControl.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and ( ( Key in CHARSETS[ceFloat] ) or
		( Key = Char( VK_BACK ) ) );
end;

procedure TKFormattedSpeedControl.SetEditFormat( const Value: string );
begin
	if ( not CheckStrEqual( FEditFormat, Value ) ) then
	begin
		FEditFormat := Value;
		DisplayChanged;
	end;
end;

procedure TKFormattedSpeedControl.SetDisplayFormat( const Value: string );
begin
	if ( not CheckStrEqual( FDisplayFormat, Value ) ) then
	begin
		FDisplayFormat := Value;
		DisplayChanged;
	end;
end;

procedure TKFormattedSpeedControl.DoIncrement;
begin
	SetValue( FValue + FIncrement );
end;

procedure TKFormattedSpeedControl.DoDecrement;
begin
	SetValue( FValue - FIncrement );
end;

procedure TKFormattedSpeedControl.DoUpClick;
begin
	if ( CheckTrimStr( Text ) or IsNull ) then
		DoIncrement;
	inherited DoUpClick;
end;

procedure TKFormattedSpeedControl.DoDownClick;
begin
	if ( CheckTrimStr( Text ) or IsNull ) then
		DoDecrement;
	inherited DoDownClick;
end;

procedure TKFormattedSpeedControl.Change;
var
  i: Integer;
begin
	{ To avoid editor setting to violate boundaries }
	inherited Change;
	{ this code can (should?) either reside in a KeyUp method (?) }
	if ( ( not Loading( Self ) ) and HandleAllocated { (* } and AutoSelect { *) } ) then
	{
	  For carret synchronization, set selstart to the first occurance (if one exist)
		of the DecimalSepeartor in text. If it does not exists, set the carret to the
		rightmost position (as a function of the Text or MaxLength Properties) relative
		to the previous carret selection position
	}
	begin
		i := ( Pos( DecimalSeparator, GetText ) - 1 );
		if ( i > 0 ) then
			SelStart := Max( SelStart, i )
		else
			SelStart := Min( SelStart, Max( MaxLength, Length( GetText ) ) );
	end;		
end;

{ TKCustomSpeedInteger }

constructor TKCustomSpeedInteger.Create( AOwner: TComponent );
{ This hard couple is to avoid a exception to occour at parse value because the
	Format's are the formats for floats. }
begin
{ Make sure descendent classes have not already defined default format strings }
	if ( not CheckTrimStr( FEditFormat ) ) then
		FEditFormat := '%d';     { Hard Couple! }
	if ( not CheckTrimStr( FDisplayFormat ) ) then
		FDisplayFormat := '%d';  { Hard Couple! }
	inherited Create( AOwner );
	inherited OnFormatEditText := FormatEdit;
	inherited OnFormatDisplayText := FormatDisplay;
end;

function TKCustomSpeedInteger.GetIncrement: LongInt;
begin
	Result := Trunc( inherited Increment );
end;

procedure TKCustomSpeedInteger.SetIncrement( NewValue: LongInt );
begin
	inherited Increment := NewValue;
end;

function TKCustomSpeedInteger.GetMaxValue: LongInt;
begin
	Result := Trunc( inherited MaxValue );
end;

procedure TKCustomSpeedInteger.SetMaxValue( NewValue: LongInt );
begin
	inherited MaxValue := NewValue;
end;

function TKCustomSpeedInteger.GetMinValue: LongInt;
begin
	Result := Trunc( inherited MinValue );
end;

procedure TKCustomSpeedInteger.SetMinValue( NewValue: LongInt );
begin
	inherited MinValue := NewValue;
end;

function TKCustomSpeedInteger.GetValue: LongInt;
begin
	Result := Trunc( inherited Value );
end;

procedure TKCustomSpeedInteger.SetValue( NewValue: LongInt );
begin
	inherited Value := NewValue;
end;

function TKCustomSpeedInteger.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := Format( AFormat, [Trunc( AValue )] );
end;

procedure TKCustomSpeedInteger.FormatEdit( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatEditText ) then
		FOnFormatEditText( Self, Trunc( AValue ), AFormat, AText );
end;

procedure TKCustomSpeedInteger.FormatDisplay( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatDisplayText ) then
		FOnFormatDisplayText( Self, Trunc( AValue ), AFormat, AText );
end;

function TKCustomSpeedInteger.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and ( ( Key in CHARSETS[ceNum] ) or
		( Key = Char( VK_BACK ) ) );
end;

{ TKCustomSpeedHexa }

constructor TKCustomSpeedHexa.Create( AOwner: TComponent );
{ This hard couple is to avoid a exception to occour at parse value because the
	Format's are the formats for floats. }
begin
	FEditFormat := '$%.*x';      { Hard Couple! }
	FDisplayFormat := '$%.*x';   { Hard Couple! }
	inherited Create( AOwner );
	FDigits := 8;
	FDisplayPrefix := true;
	OnFormatEditText := nil;
	OnFormatDisplayText := nil;
	DisplayChanged;
end;

function TKCustomSpeedHexa.ParseValue: Boolean;
begin
	Result := true;
	try
		FValue := HexToInt( Text );
	except
{ Generated by a invalid DisplayFormat mask before inherited Create returning }
		on EKSYUtils do
			Result := false;
	end;
end;

function TKCustomSpeedHexa.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := Format( AFormat, [FDigits, Trunc( AValue )] );
end;

procedure TKCustomSpeedHexa.SetDigits( NewValue: Byte );
begin
	if ( FDigits <> NewValue ) and ( NewValue > 0 ) then
	begin
		FDigits := NewValue;
		DisplayChanged;
	end;
end;

procedure TKCustomSpeedHexa.SetDisplayPrefix( NewValue: Boolean );
begin
	if ( FDisplayPrefix <> NewValue ) then
	begin
		FDisplayPrefix := NewValue;
		if NewValue then
			DisplayFormat := '$%.*x'
		else
			DisplayFormat := '%.*x';
		DisplayChanged;
	end;
end;

procedure TKCustomSpeedHexa.KeyPress( var Key: Char );
begin
	if ( Key in ['a'..'f'] ) then
		Key := UpCase( Key );
	inherited KeyPress( Key );
end;

function TKCustomSpeedHexa.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and ( ( Key in CHARSETS[ceHexDigits] ) or
		( Key = Char( VK_BACK ) ) );
end;

{ TKDateIncrement }

constructor TKDateIncrement.Create( AOwner: TObject );
begin
	inherited Create;
	FOwner := AOwner;
	FDays := 1;
end;

procedure TKDateIncrement.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKDateIncrement ) then
		with ( Source as TKDateIncrement ) do
		begin
			Self.Days := Days;
			Self.Months := Months;
			Self.Years := Years;
		end
	else
		inherited Assign( Source );
end;

{ TKCustomSpeedDateTime }

constructor TKCustomSpeedDateTime.Create( AOwner: TComponent );
{ This hard couple is to avoid a exception to occour at parse value because the
	Format's are the formats for floats. }
begin
	FEditFormat := ShortDateFormat + CH_SPACE + ShortTimeFormat;    { Hard Couple! }
	FDisplayFormat := ShortDateFormat + CH_SPACE + ShortTimeFormat; { Hard Couple! }
	inherited Create( AOwner );
	InternalText := FormatDateTime( FDisplayFormat, Now );
	MinValue := EncodeDate( 1980, 01, 01 ) + EncodeTime( 0, 0, 0, 0 );
	MaxValue := EncodeDate( 3000, 12, 31 ) + EncodeTime( 23, 59, 59, 999 );
	Value := Now;
	Increment := EncodeTime( 0, 1, 0, 0 );
	FDateIncrement := TKDateIncrement.Create( Self );
	inherited OnFormatEditText := FormatEdit;
	inherited OnFormatDisplayText := FormatDisplay;
end;

destructor TKCustomSpeedDateTime.Destroy;
begin
	FDateIncrement.Free;
	inherited Destroy;
end;

function TKCustomSpeedDateTime.ParseValue: Boolean;
begin
	Result := True;
	try
		if CheckTrimStr( Text ) then
			FValue := StrToDateTime( Text );
	except
		on EConvertError do
			Result := false;
	end;
end;

function TKCustomSpeedDateTime.GetTimeIncrement: TDateTime;
begin
	Result := Frac( Increment );
end;

procedure TKCustomSpeedDateTime.SetTimeIncrement( NewValue: TDateTime );
begin
	Increment := Trunc( Increment ) + Frac( NewValue );
end;

procedure TKCustomSpeedDateTime.SetDateIncrement( NewValue: TKDateIncrement );
begin
	FDateIncrement.Assign( NewValue );
end;

function TKCustomSpeedDateTime.GetMaxValue: TDateTime;
begin
	Result := inherited MaxValue;
end;

procedure TKCustomSpeedDateTime.SetMaxValue( NewValue: TDateTime );
begin
	inherited MaxValue := NewValue;
end;

function TKCustomSpeedDateTime.GetMinValue: TDateTime;
begin
	Result := inherited MinValue;
end;

procedure TKCustomSpeedDateTime.SetMinValue( NewValue: TDateTime );
begin
	inherited MinValue := NewValue;
end;

function TKCustomSpeedDateTime.GetValue: TDateTime;
begin
	Result := inherited Value;
end;

procedure TKCustomSpeedDateTime.SetValue( NewValue: TDateTime );
begin
	inherited Value := NewValue;
end;

function TKCustomSpeedDateTime.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := FormatDateTime( AFormat, AValue );
end;

procedure TKCustomSpeedDateTime.FormatEdit( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatEditText ) then
		FOnFormatEditText( Self, AValue, AFormat, AText );
end;

procedure TKCustomSpeedDateTime.FormatDisplay( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatDisplayText ) then
		FOnFormatDisplayText( Self, AValue, AFormat, AText );
end;

function TKCustomSpeedDateTime.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and
	( Key in CHARSETS[ceDigit] ) or
	( Key = Char( VK_BACK ) ) or
	( ( CheckStrContains( DateSeparator, SelText ) ) and
		( ( Key = DateSeparator ) and
			( CountTokens( Text, DateSeparator ) <
				CountTokens( EditFormat, DateSeparator ) ) ) ) or
	( ( CheckStrContains( TimeSeparator, SelText ) ) and
		( ( Key = TimeSeparator ) and
			( CountTokens( Text, TimeSeparator ) <
				CountTokens( EditFormat, TimeSeparator ) ) ) );
end;

procedure TKCustomSpeedDateTime.DoIncrement;
begin
	with FDateIncrement do
		Value := IncDate( Value, Days, Months, Years ) ;
	if ( ( Value + Increment ) >= EndOfDay( Value ) ) then
		MessageBeep( 0 )
	else
		inherited DoIncrement;
end;

procedure TKCustomSpeedDateTime.DoDecrement;
begin
	with FDateIncrement do
		Value := IncDate( Value, -Days, -Months, -Years );
	if ( ( Value - Increment ) < 0 ) then
		MessageBeep( 0 )
	else
		inherited DoDecrement;
end;

{ TKCustomSpeedDate }

constructor TKCustomSpeedDate.Create( AOwner: TComponent );
{ This hard couple is to avoid a exception to occour at parse value because the
	Format's are the formats for floats. }
begin
	FEditFormat := ShortDateFormat;    { Hard Couple! }
	FDisplayFormat := ShortDateFormat; { Hard Couple! }
	inherited Create( AOwner );
	InternalText := FormatDateTime( FDisplayFormat, Date );
	MinValue := EncodeDate( 1980, 01, 01 );
	MaxValue := EncodeDate( 3000, 12, 31 );
	Value := Date;
	Increment := 0;
	FDateIncrement := TKDateIncrement.Create( Self );
	inherited OnFormatEditText := FormatEdit;
	inherited OnFormatDisplayText := FormatDisplay;
end;

destructor TKCustomSpeedDate.Destroy;
begin
	FDateIncrement.Free;
	inherited Destroy;
end;

function TKCustomSpeedDate.ParseValue: Boolean;
begin
	Result := True;
	try
		if CheckTrimStr( Text ) then
			FValue := StrToDate( Text );
	except
		on EConvertError do
			Result := false;
	end;
end;

procedure TKCustomSpeedDate.SetDateIncrement( NewValue: TKDateIncrement );
begin
  FDateIncrement.Assign( NewValue );
end;

function TKCustomSpeedDate.GetMaxValue: TDateTime;
begin
	Result := Trunc( inherited MaxValue );
end;

procedure TKCustomSpeedDate.SetMaxValue( NewValue: TDateTime );
begin
	inherited MaxValue := Trunc( NewValue );
end;

function TKCustomSpeedDate.GetMinValue: TDateTime;
begin
	Result := Trunc( inherited MinValue );
end;

procedure TKCustomSpeedDate.SetMinValue( NewValue: TDateTime );
begin
	inherited MinValue := Trunc( NewValue );
end;

function TKCustomSpeedDate.GetValue: TDateTime;
begin
	Result := Trunc( inherited Value );
end;

procedure TKCustomSpeedDate.SetValue( NewValue: TDateTime );
begin
	inherited Value := Trunc( NewValue );
end;

function TKCustomSpeedDate.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := FormatDateTime( AFormat, Trunc( AValue ) );
end;

procedure TKCustomSpeedDate.FormatEdit( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatEditText ) then
		FOnFormatEditText( Self, Trunc( AValue ), AFormat, AText );
end;

procedure TKCustomSpeedDate.FormatDisplay( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatDisplayText ) then
		FOnFormatDisplayText( Self, Trunc( AValue ), AFormat, AText );
end;

function TKCustomSpeedDate.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and
	( Key in CHARSETS[ceDigit] ) or
	( Key = Char( VK_BACK ) ) or
	( ( CheckStrContains( DateSeparator, SelText ) ) and
		( ( Key = DateSeparator ) and
			( CountTokens( Text, DateSeparator ) < CountTokens( EditFormat, DateSeparator ) ) ) );
end;

procedure TKCustomSpeedDate.DoIncrement;
begin
	with FDateIncrement do
		Value := IncDate( Value, Days, Months, Years );
end;

procedure TKCustomSpeedDate.DoDecrement;
begin
	with FDateIncrement do
		Value := IncDate( Value, -Days, -Months, -Years );
end;

{ TKCustomSpeedTime }

constructor TKCustomSpeedTime.Create( AOwner: TComponent );
{ This hard couple is to avoid a exception to occour at parse value because the
	Format's are the formats for floats. }
begin
	FEditFormat := ShortTimeFormat;    { Hard Couple! }
	FDisplayFormat := ShortTimeFormat; { Hard Couple! }
	inherited Create( AOwner );
	Value := Time;
	MinValue := EncodeTime( 0, 0, 0, 0 );
	MaxValue := EncodeTime( 23, 59, 59, 999 );
	Increment := EncodeTime( 0, 1, 0, 0 );
	inherited OnFormatEditText := FormatEdit;
	inherited OnFormatDisplayText := FormatDisplay;
end;

function TKCustomSpeedTime.ParseValue: Boolean;
begin
	Result := True;
	try
		if CheckTrimStr( Text ) then
			FValue := StrToTime( Text );
	except
		on EConvertError do
			Result := false;
	end;
end;

function TKCustomSpeedTime.GetTimeIncrement: TDateTime;
begin
	Result := Frac( Increment );
end;

procedure TKCustomSpeedTime.SetTimeIncrement( NewValue: TDateTime );
begin
	Increment := Frac( NewValue );
end;

function TKCustomSpeedTime.GetMaxValue: TDateTime;
begin
	Result := Frac( inherited MaxValue );
end;

procedure TKCustomSpeedTime.SetMaxValue( NewValue: TDateTime );
begin
	inherited MaxValue := Frac( NewValue );
end;

function TKCustomSpeedTime.GetMinValue: TDateTime;
begin
	Result := Frac( inherited MinValue );
end;

procedure TKCustomSpeedTime.SetMinValue( NewValue: TDateTime );
begin
	inherited MinValue := Frac( NewValue );
end;

function TKCustomSpeedTime.GetValue: TDateTime;
begin
	Result := Frac( inherited Value );
end;

procedure TKCustomSpeedTime.SetValue( NewValue: TDateTime );
begin
	inherited Value := Frac( NewValue );
end;

procedure TKCustomSpeedTime.DoIncrement;
begin
	if ( ( Value + Increment ) >= EndOfDay( Value ) ) then
		MessageBeep( 0 )
	else
		inherited DoIncrement;
end;

procedure TKCustomSpeedTime.DoDecrement;
begin
	if ( ( Value - Increment ) < 0 ) then
		MessageBeep( 0 )
	else
		inherited DoDecrement;
end;

function TKCustomSpeedTime.FormatText( const AFormat: string; AValue: Extended ): string;
begin
	Result := FormatDateTime( AFormat, Frac( AValue ) );
end;

procedure TKCustomSpeedTime.FormatEdit( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatEditText ) then
		FOnFormatEditText( Self, Frac( AValue ), AFormat, AText );
end;

procedure TKCustomSpeedTime.FormatDisplay( Sender: TObject; const AValue: Extended;
	const AFormat: string; var AText: string );
begin
	if Assigned( FOnFormatDisplayText ) then
		FOnFormatDisplayText( Self, Frac( AValue ), AFormat, AText );
end;

function TKCustomSpeedTime.IsValidChar( Key: Char ): Boolean;
begin
	Result := EditorEnabled and
	( Key in CHARSETS[ceDigit] ) or
	( Key = Char( VK_BACK ) ) or
	( ( CheckStrContains( TimeSeparator, SelText ) ) and
		( ( Key = TimeSeparator ) and
			( CountTokens( Text, TimeSeparator ) < CountTokens( EditFormat, TimeSeparator ) ) ) );
end;

procedure RegisterEditGradientFunc;
begin
	uksyClasses.EditGradientFunc := ukrfGradEdit.EditGradient;
end;

procedure UnRegisterEditGradientFunc;
begin
	uksyClasses.EditGradientFunc := nil;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterEditGradientFunc;
end;

procedure Done;
begin
	UnRegisterEditGradientFunc;
end;

initialization
	Init;

finalization
	Done;

end.
