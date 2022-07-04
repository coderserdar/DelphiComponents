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

unit ukdgClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Classes, Graphics, Forms, Menus, Controls, Buttons, StdCtrls, Mask,
	ExtCtrls, uksyConsts, uksyUtils, uksyTypes, uksyClasses, uksyShortCuts, ukrClasses,
	ukrCtrls, ukdgConsts;

type

	EKDGClasses = class( EKDialogs );

{$HINTS OFF}

{
--------------------------------------------------------------------------------
-------------------------- Basic Dialog Architecture ---------------------------
--------------------------------------------------------------------------------
}

	EKCustomDialog = class( EKDGClasses );

	TKBaseDialog = class;

	TKDialogNotifyEvent = procedure( Sender: TKBaseDialog ) of object;
	TKDialogCheckEvent = procedure( Sender: TKBaseDialog; var ParamsChecked: Boolean ) of object;

{ TKBaseDialog }

	TKBaseDialog = class( TKKernelBaseDialog )
	private
		FPosTop: Integer;
		FCaption: string;
		FWidth: Cardinal;
		FHeight: Cardinal;
		FPosLeft: Integer;
		FCentered: Boolean;

		FChecking: Boolean;
		FExecuting: Boolean;

		FOnCheckParams: TKDialogCheckEvent;
		FAfterCheckParams: TKDialogNotifyEvent;
		FBeforeCheckParams: TKDialogNotifyEvent;

		function DialogExecute: Integer;
		function DialogCheckParams: Boolean;

	protected
		function InternalExecute: Integer;
		function GetExitCode( ExecuteCode: Integer ): Boolean; virtual;

		procedure DoAfterExecute; virtual; abstract;
		procedure DoBeforeExecute; virtual; abstract;
		function DoExecute: Integer; virtual; abstract;

		procedure DoAfterCheckParams; virtual;
		procedure DoBeforeCheckParams; virtual;
		function DoCheckParams: Boolean; virtual;

		procedure ForceChecking; dynamic;
		procedure ForceExecuting; dynamic;

		property Checking: Boolean
						 read FChecking;
		property Executing: Boolean
						 read FExecuting;

		property Caption: string
						 read FCaption write FCaption;
		property Centered: Boolean
						 read FCentered write FCentered default true;
		property Height: Cardinal
						 read FHeight write FHeight default 150;
		property PosLeft: Integer
						 read FPosLeft write FPosLeft default 29;
		property PosTop: Integer
						 read FPosTop write FPosTop default 25;
		property Width: Cardinal
						 read FWidth write FWidth default 228;

		property OnCheckParams: TKDialogCheckEvent
						 read FOnCheckParams write FOnCheckParams;
		property AfterCheckParams: TKDialogNotifyEvent
						 read FAfterCheckParams write FAfterCheckParams;
		property BeforeCheckParams: TKDialogNotifyEvent
						 read FBeforeCheckParams write FBeforeCheckParams;

	public
		constructor Create( AOwner: TComponent ); override;

		function Execute: Boolean;

	end;

	TKBaseDialogClass = class of TKBaseDialog;

{ TKCustomDialogForm }

	TKCustomDialog = class;
	TKCustomDialogForm = class;

	TKCloseStyle = ( csNone, csEnabled, csDisabled );
	TKSizeStyle = ( ssNoSizing, ssSizing, ssLimittedSizing );
	TKDialogKind = ( dkDefault, dkRollup, dkHelper, dkCustom );

	TKDialogExecuteEvent = procedure( Sender: TKCustomDialog; AForm: TKCustomDialogForm ) of object;

	TKCustomDialogForm = class( TKCustomInternalDialogForm  )
	private
		FTimer: TTimer;
		FFormUnits: TPoint;
		FDialog: TKCustomDialog;

		function GetInstanceCount( Index: TControlClass ): Cardinal;

	protected
		procedure PlaceButtons; virtual; abstract;
		procedure PlaceControls; virtual; abstract;
		procedure PrepareForm; virtual; abstract;
		procedure UnprepareForm; virtual; abstract;

		procedure SetControlsTabOrder; virtual;
		procedure TimerExpired( Sender: TObject ); virtual;

		function DoShowModal: Integer; virtual;
		procedure DoInternalShow; virtual;

		procedure Activate; override;
		function InverseScaleX( Value: Integer ): Integer; dynamic;
		function InverseScaleY( Value: Integer ): Integer; dynamic;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
		function CopyToClipBoard: Boolean; virtual;
		function CutToClipBoard: Boolean; virtual;
		function PasteFromClipBoard: Boolean; virtual;

		procedure SetControlBounds( AControl: TControl; Rect: TRect ); dynamic;
		function AddControl( const ctName: string; ctRect: TRect; ctEnabled,
			ctVisible: Boolean; ctParent: TWinControl; ctClass: TControlClass ): TControl; dynamic;
		function AddButton( const bnCaption, bnName: string; bnRect: TRect; bnEnabled,
			bnVisible, bnDefault: Boolean; bnResult: TModalResult; bnGlyph: TBitmap;
			bnParent: TWinControl; bnClick: TNotifyEvent ): TBitBtn; virtual;
		function AddButtonDefault( bnCaption: string;	bnResult: TModalResult;
			bnDefault: Boolean ): TBitBtn; virtual;
		function AddSpeedButton( const bnCaption, bnName: string; bnRect: TRect;
			bnEnabled, bnVisible, bnDown, bnFlat, bnAllowAllUp: Boolean; bnGlyph: TBitmap;
			bnGrpIdx: Integer; bnParent: TWinControl; bnClick: TNotifyEvent ): TSpeedButton; dynamic;

		property Dialog: TKCustomDialog
						 read FDialog;
		property Timer: TTimer
						 read FTimer;				 
		property InstanceCount[Index: TControlClass]: Cardinal
						 read GetInstanceCount;

	public
		destructor Destroy; override;
		constructor CreateDialog( AOwner: TKCustomDialog ); virtual;

		function ShowModal: Integer; {$IFDEF DELPHI4}override{$ELSE}dynamic{$ENDIF};
		procedure Show; dynamic;

		function ScaleX( Value: Integer ): Integer; dynamic;
		function ScaleY( Value: Integer ): Integer; dynamic;

	end;

	TKCustomDialogFormClass = class of TKCustomDialogForm;

{ TKDialogLimits }

	TKDialogLimits = class( TPersistent )
	private
		FMinWidth: Integer;
		FMinHeight: Integer;
		FMaxWidth: Integer;
		FMaxHeight: Integer;

		FOwner: TKCustomDialog;

		procedure SetMaxHeight( Value: Integer );
		procedure SetMaxWidth( Value: Integer );
		procedure SetMinHeight( Value: Integer );
		procedure SetMinWidth( Value: Integer );

	public
		constructor Create( AOwner: TKCustomDialog );

		property Owner: TKCustomDialog
						 read FOwner;

	published
		property MaxHeight: Integer
						 read FMaxHeight write SetMaxHeight;
		property MaxWidth: Integer
						 read FMaxWidth write SetMaxWidth;
		property MinHeight: Integer
						 read FMinHeight write SetMinHeight;
		property MinWidth: Integer
						 read FMinWidth write SetMinWidth;

	end;

{ TKCustomDialog }

	TKCustomDialog = class( TKBaseDialog )
	private
		FFont: TFont;
		FTimeOut: Boolean;
		FPicture: TPicture;
		FDialogUnits: Boolean;
		FExpiration: Cardinal;
		FCloseOnAltF4: Boolean;
		FCloseOnEscape: Boolean;
		FSmallCaption: Boolean;
		FFormPainter: TKFormPainter;
		FFormClass: TKCustomDialogFormClass;
		FFormPainterClass: TKFormPainterClass;

		FAfterExecute: TKDialogExecuteEvent;
		FBeforeExecute: TKDialogExecuteEvent;

		FSizeStyle: TKSizeStyle;
		FCloseStyle: TKCloseStyle;
		FDialogKind: TKDialogKind;

		FIcon: TIcon;
		FActiveFont: TFont;
		FInactiveFont: TFont;
		FLimits: TKDialogLimits;
		FSystemMenu: TPopupMenu;
		FBackGround: TKGradient;
		FActiveCaption: TKGradient;
		FInactiveCaption: TKGradient;

		FForm: TKCustomDialogForm;

		procedure SetFont( Value: TFont );
		procedure SetIcon( Value: TIcon );

	protected
		function GetForm: TKCustomDialogForm; virtual;
		procedure SetForm( Value: TKCustomDialogForm ); virtual;
		function GetFormPainter: TKFormPainter; virtual;
		procedure SetFormPainter( Value: TKFormPainter ); virtual;

		function DoCheckParams: Boolean; override;
		function GetExitCode( ExecuteCode: Integer ): Boolean; override;

		procedure UserCallBack; dynamic;
		procedure SetPicture( Value: TPicture ); virtual;
		function GetDefaultFormClass: TKCustomDialogFormClass; virtual;
    function GetFormPainterClass: TKFormPainterClass; dynamic;

		procedure DefaultLimits; virtual;
		procedure DefaultBackground; virtual;
		procedure DefaultActiveCaption; virtual;
		procedure DefaultInactiveCaption; virtual;

		procedure KeyboardClose; virtual;

		procedure DoAfterExecute; override;
		procedure DoBeforeExecute; override;
		function DoExecute: Integer; override;

		procedure DestroyForm; virtual;

		property FormPainter: TKFormPainter
						 read GetFormPainter write SetFormPainter;
		property ActiveCaption: TKGradient
						 read FActiveCaption write FActiveCaption;
		property ActiveFont: TFont
						 read FActiveFont write FActiveFont;
		property BackGround: TKGradient
						 read FBackGround write FBackGround;
		property CloseOnAltF4: Boolean
						 read FCloseOnAltF4 write FCloseOnAltF4 default true;
		property CloseOnEscape: Boolean
						 read FCloseOnEscape write FCloseOnEscape default true;
		property CloseStyle: TKCloseStyle
						 read FCloseStyle write FCloseStyle default csEnabled;
		property DialogKind: TKDialogKind
						 read FDialogKind write FDialogKind default dkDefault;
		property DialogUnits: Boolean
						 read FDialogUnits write FDialogUnits default true;
		property Expiration: Cardinal
						 read FExpiration write FExpiration default 120;
		property Font: TFont
						 read FFont write SetFont;
		property Form: TKCustomDialogForm
						 read GetForm write SetForm;
		property FormClass: TKCustomDialogFormClass
						 read FFormClass write FFormClass;
		property FormPainterClass: TKFormPainterClass
						 read FFormPainterClass write FFormPainterClass;
		property Icon: TIcon
						 read FIcon write SetIcon;
		property InactiveCaption: TKGradient
						 read FInactiveCaption write FInactiveCaption;
		property InactiveFont: TFont
						 read FInactiveFont write FInactiveFont;
		property Limits: TKDialogLimits
						 read FLimits write FLimits;
		property Picture: TPicture
						 read FPicture write SetPicture;
		property SizeStyle: TKSizeStyle
						 read FSizeStyle write FSizeStyle default ssNoSizing;
		property SmallCaption: Boolean
						 read FSmallCaption write FSmallCaption default false;
		property SystemMenu: TPopupMenu
						 read FSystemMenu write FSystemMenu;
		property TimeOut: Boolean
						 read FTimeOut write FTimeOut default false;

		property AfterExecute: TKDialogExecuteEvent
						 read FAfterExecute write FAfterExecute;
		property BeforeExecute: TKDialogExecuteEvent
						 read FBeforeExecute write FBeforeExecute;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	end;

	TKCustomDialogClass = class of TKCustomDialog;

{
--------------------------------------------------------------------------------
------------------------ Default Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}

{ TKDialogForm }

  TKDialog = class;

	TKDialogForm = class( TKCustomDialogForm )
	private
		function GetDialog: TKDialog;

	protected
		procedure PlaceButtons; override;
		procedure PlaceControls; override;
		procedure PrepareForm; override;
		procedure UnprepareForm; override;

	public
{ from TKCustomDialogForm }
		property Dialog: TKDialog
						 read GetDialog;
		property InstanceCount;

	end;

{ TKDialog }

  TKDialogEvent = procedure( Sender: TKDialog; Form: TKDialogForm ) of object;

	TKDialog = class( TKCustomDialog )
	private
		FOnPlaceButtons: TKDialogEvent;
		FOnPlaceControls: TKDialogEvent;
		FOnPrepareForm: TKDialogEvent;
		FOnUnPrepareForm: TKDialogEvent;

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

	public
	{ from TKBaseDialog }
		property Checking;
		property Executing;

	{ from TKCustomDialog }
		property Form;
		property FormPainter;

	published
	{ from TKBaseDialog }
		property Caption;
		property Centered;
		property Height;
		property PosLeft;
		property PosTop;
		property Width;

		property OnCheckParams;
		property AfterCheckParams;
		property BeforeCheckParams;

	{ from TKCustomDialog }
		property ActiveCaption;
		property ActiveFont;
		property BackGround;
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;
		property DialogKind;
		property DialogUnits;
		property Expiration;
		property Font;
		property Icon;
		property InactiveCaption;
		property InactiveFont;
		property Limits;
		property Picture;
		property SizeStyle;
		property SmallCaption;
		property SystemMenu;
		property TimeOut;

		property AfterExecute;
		property BeforeExecute;

		property OnPlaceButtons: TKDialogEvent
						 read FOnPlaceButtons write FOnPlaceButtons;
		property OnPlaceControls: TKDialogEvent
						 read FOnPlaceControls write FOnPlaceControls;
		property OnPrepareForm: TKDialogEvent
						 read FOnPrepareForm write FOnPrepareForm;
		property OnUnPrepareForm: TKDialogEvent
						 read FOnUnPrepareForm write FOnUnPrepareForm;

	end;

{
--------------------------------------------------------------------------------
----------------------------- System Compatible Dialogs ------------------------
--------------------------------------------------------------------------------
}

	EKSystemDialog = class( EKCustomDialog );

{ TKSystemDialog }

	TKSystemDialog = class;

	TKStayOnTopSystemDialogEvent = procedure( Sender: TKSystemDialog;
		Button: TBitBtn; var Close: Boolean ) of object;

	TKSystemDialog = class( TKCustomDialog )
	private
		FText: string;
		FStayOnTop: Boolean;
		FRightAlign: Boolean;
		FCacheDlgInstance: Boolean;
		FFormOnTopList: TList;
		FDialogStyle: TKDialogStyle;
		FBitmapOption: TKBitmapOption;
		FOnButtonClick: TKStayOnTopSystemDialogEvent;

{ From TKBaseDialog }
		property Width;
		property Height;

		property AfterCheckParams;
		property BeforeCheckParams;

{ From TKCustomDialog }
		property DialogKind;
		property DialogUnits;
		property FormClass;
		property Limits;
		property SystemMenu;
		property SizeStyle;

		property AfterExecute;
		property BeforeExecute;

		procedure SetBitmapOption( Value: TKBitmapOption );

	protected
		function GetForm: TKCustomDialogForm; override;
		procedure SetForm( Value: TKCustomDialogForm ); override;
		function GetFormPainter: TKFormPainter; override;
		procedure SetFormPainter( Value: TKFormPainter ); override;

		procedure SetPicture( Value: TPicture ); override;
		procedure DestroyForm; override;
		function DoExecute: Integer; override;

		procedure SetStayOnTop( Value: Boolean ); virtual;

		function ButtonClick( Button: TBitBtn ): Boolean; dynamic;

		procedure ClearFormOnTopList; virtual;

		property FormOnTopList: TList
						 read FFormOnTopList;

		property BitmapOption: TKBitmapOption
						 read FBitmapOption write SetBitmapOption default boInformation;
		property DialogStyle: TKDialogStyle
						 read FDialogStyle write FDialogStyle default dsOK;
		property Text: string
						 read FText write FText;
		property StayOnTop: Boolean
						 read FStayOnTop write SetStayOnTop default False;
		property CacheDlgInstance: Boolean
						 read FCacheDlgInstance write FCacheDlgInstance default True;
		property OnButtonClick: TKStayOnTopSystemDialogEvent
						 read FOnButtonClick write FOnButtonClick;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property RightAlign: Boolean
						 read FRightAlign write FRightAlign default False;

{ From TKBaseDialog }
		property Caption;
		property Centered;
		property PosLeft;
		property PosTop;

		property OnCheckParams;

{ From TKCustomDialog }
		property Picture;
		property ActiveCaption;
		property ActiveFont;
		property BackGround;
		property Font;
		property Icon;
		property InactiveCaption;
		property InactiveFont;
		property SmallCaption;

	end;

{ TKShowDialog }

	TKShowDialog = class( TKSystemDialog )
	public
		constructor Create( AOwner: TComponent ); override;

		procedure CloseDialogs; virtual;

	published
{ From TKCustomDialog }
    property CloseStyle;
		property CloseOnAltF4;
		property CloseOnEscape;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;
		property Expiration;
		property TimeOut;
		property Text;
		property StayOnTop;
		property OnButtonClick;
		property CacheDlgInstance;

	end;

{ TKInputDialog }

	TKInputDialog = class( TKSystemDialog )
	private
		FEditText: string;
		FEditMask: string;
    FPasswordChar: Char;

{ From TKCustomDialog }
		property Expiration;
		property TimeOut;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property EditText: string
						 read FEditText write FEditText;
		property EditMask: string
						 read FEditMask write FEditMask;
    property PasswordChar: Char
             read FPasswordChar write FPasswordChar default CH_NULL;

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

{ From TKSystemDialog }
		property Text;

	end;

{ TKCustomInputListDialog }

	TKCustomInputListDialog = class( TKSystemDialog )
	private
		FItems: TStrings;
		FEditText: string;
		FUserInput: Boolean;
		FItemIndex: Integer;

		procedure SetItems( Value: TStrings );

{ From TKCustomDialog }
		property Expiration;
		property TimeOut;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;

	protected
		property EditText: string
						 read FEditText write FEditText;
		property ItemIndex: Integer
						 read FItemIndex write FItemIndex;
		property Items: TStrings
						 read FItems write SetItems;
		property UserInput: Boolean
						 read FUserInput write FUserInput default false;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

{ From TKSystemDialog }
		property Text;

	end;

{ TKInputListDialog }

	TKInputListDialog = class( TKCustomInputListDialog )
	published
{ From TKCustomInputListDialog }
		property EditText;
		property ItemIndex;
		property Items;
		property UserInput;

	end;

{ TKInputCheckListDialog }

	TKInputCheckListDialog = class( TKSystemDialog )
	private
		FItems: TStrings;
		FSorted: Boolean;
		FBackGroundColor: TColor;
		FSelection: TKBitsWrapper;
		FIsToSetSelection: Boolean;

{ From TKCustomDialog }
		property Expiration;
		property TimeOut;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;
		property Text;

	protected
	  function DoCheckParams: Boolean; override;	

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		
	published
		property Selection: TKBitsWrapper
						 read FSelection write FSelection;
		property Items: TStrings
						 read FItems write FItems;
		property Sorted: Boolean
						 read FSorted write FSorted default False;
		property BackGroundColor: TColor
						 read FBackGroundColor write FBackGroundColor default clBtnFace;

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

	end;

{ TKProgressDialog }

	TKProgressDialog = class;

	TKVisualProgressEvent = procedure( Sender: TKProgressDialog; var Status: string;
		var Percent: TKPercent; var Cancel: Boolean; var ProgressInfoStyle: TKProgressInfoStyle ) of object;

	TKProgressDialog = class( TKSystemDialog )
	private
		FData: Pointer;
		FUserCanceled: Boolean;
		FCancelEnabled: Boolean;
		FProgressVisible: Boolean;
		FOnProgress: TNotifyEvent;
		FStatusAlignment : TAlignment;
		FExecutor: TKDialogExecuteProc;
		FInfoStyle: TKProgressInfoStyle;
		FCallback: TKProgressDialogCallBack;
		FOnVisualProgress: TKVisualProgressEvent;

{ From TKCustomDialog }
		property Expiration;
		property TimeOut;
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;

	protected
		function DoCheckParams: Boolean; override;

		procedure Progress; dynamic;
		procedure VisualProgress( var Status: string; var Percent: TKPercent ); dynamic;

		property Callback: TKProgressDialogCallBack
						 read FCallback write FCallback;
		property Data: Pointer
						 read FData write FData;
		property Executor: TKDialogExecuteProc
						 read FExecutor write FExecutor;

	public
		constructor Create( AOwner: TComponent ); override;

		property UserCanceled: Boolean
						 read FUserCanceled;

	published
		property CancelEnabled: Boolean
						 read FCancelEnabled write FCancelEnabled  default true;
		property InfoStyle: TKProgressInfoStyle
						 read FInfoStyle write FInfoStyle default pisEllipsis;
		property ProgressVisible: Boolean
						 read FProgressVisible write FProgressVisible default true;
		property StatusAlignment: TAlignment
						 read FStatusAlignment write FStatusAlignment default taLeftJustify;

		property OnProgress: TNotifyEvent
						 read FOnProgress write FOnProgress;
		property OnVisualProgress: TKVisualProgressEvent
						 read FOnVisualProgress write FOnVisualProgress;

{ From TKSystemDialog }
		property Text;

	end;

	TKDialogCheckParamFunc = function( Sender: TKCustomDialog; Data: Pointer ): Boolean;

	TKDialogDataEx = record
		ActiveFont: TKFontData;
		InActiveFont: TKFontData;
		BackGround: TKGradientData;
		ActiveCaption: TKGradientData;
		InactiveCaption: TKGradientData;
		swCheckParamCallBack: TKDialogCheckParamFunc;
		CheckParamData: Pointer;
	end;

const

	DialogDataEx: TKDialogDataEx =
	(
		ActiveFont:
		(
			Name: DEFAULT_DIALOG_FONT_NAME;
			Size: DEFAULT_DIALOG_FONT_SIZE;
			Style: [fsBold];
			Color: DIALOG_DEFAULT_ACTIVE_FONT_COLOR;
		);
		InActiveFont:
		(
			Name: DEFAULT_DIALOG_FONT_NAME;
			Size: DEFAULT_DIALOG_FONT_SIZE;
			Style: [fsBold];
			Color: DIALOG_DEFAULT_INACTIVE_FONT_COLOR;
		);
		BackGround:
		(
			BeginColor: clBtnFace;
			EndColor: clBtnFace;
			GradientStyle: DEFAULT_DIALOG_GRADIENT_STYLE;
			Steps: DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj: nil;
			BitmapFile: nil;
		);
		ActiveCaption:
		(
			BeginColor: clNavy;
			EndColor: clDeepBlue;
			GradientStyle: DEFAULT_DIALOG_GRADIENT_CAPTION_STYLE;
			Steps: DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj: nil;
			BitmapFile: nil;
		);
		InactiveCaption:
		(
			BeginColor: clGray;
			EndColor: clSilver;
			GradientStyle: DEFAULT_DIALOG_GRADIENT_CAPTION_STYLE;
			Steps: DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj: nil;
			BitmapFile: nil;
		);
	);

procedure ResetDialogDataEx;
procedure RefreshDialogData( d: TKCustomDialog );

{
--------------------------------------------------------------------------------
-------------------------- Speed Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}

type

	EKSpeedDialog = class( EKCustomDialog );

{ TKCustomSpeedProperties }

	TKCustomSpeedProperties = class( TPersistent )
	private
		FAllowNull: Boolean;
		FEditFormat: string;
		FDisplayFormat: string;
		FAlignment: TAlignment;
		FEditorEnabled: Boolean;

	protected
		property DisplayFormat: string
						 read FDisplayFormat write FDisplayFormat;
		property EditFormat: string
						 read FEditFormat write FEditFormat;

	public
		constructor Create; virtual;
		procedure Assign( Source: TPersistent ); override;

	published
		property Alignment: TAlignment
						 read FAlignment write FAlignment default taLeftJustify;
		property AllowNull: Boolean
						 read FAllowNull write FAllowNull;
		property EditorEnabled: Boolean
						 read FEditorEnabled write FEditorEnabled default true;

	end;

{ TKFloatSpeedProperties }

	TKFloatSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: Extended;
		FMinValue: Extended;
		FMaxValue: Extended;
		FIncrement: Extended;

		procedure SetMaxValue( NewValue: Extended );
		procedure SetMinValue( NewValue: Extended );

	public
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property Increment: Extended
						 read FIncrement write FIncrement;
		property MaxValue: Extended
						 read FMaxValue write SetMaxValue;
		property MinValue: Extended
						 read FMinValue write SetMinValue;
		property Value: Extended
						 read FValue write FValue;

{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;

	end;

{ TKCustomIntegerSpeedProperties }

	TKCustomIntegerSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: Integer;
		FMinValue: Integer;
		FMaxValue: Integer;
		FIncrement: Integer;

		procedure SetMaxValue( NewValue: Integer );
		procedure SetMinValue( NewValue: Integer );

	public
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property Increment: Integer
						 read FIncrement write FIncrement;
		property MaxValue: Integer
						 read FMaxValue write SetMaxValue;
		property MinValue: Integer
						 read FMinValue write SetMinValue;
		property Value: Integer
						 read FValue write FValue;

	end;

{ TKIntegerSpeedProperties }

	TKIntegerSpeedProperties = class( TKCustomIntegerSpeedProperties )
	published
	{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;

	end;

{ TKHexaSpeedProperties }

	TKHexaSpeedProperties = class( TKCustomIntegerSpeedProperties )
	private
		FDigits: Byte;
		FDisplayPrefix: Boolean;

	{ TKCustomSpeedProperties }
		property EditFormat;
		property DisplayFormat;

		procedure SetDisplayPrefix( NewValue: Boolean );

	public
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property Digits: Byte
						 read FDigits write FDigits default 8;
		property DisplayPrefix: Boolean
						 read FDisplayPrefix write SetDisplayPrefix default true;

	end;

{ TKDateSpeedProperties }

	TKDateSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: Extended;
		FMinValue: Extended;
		FMaxValue: Extended;
		FDateIncrement: TKDateIncrement;

		procedure SetDateIncrement( NewValue: TKDateIncrement );
		function GetMaxValue: TDateTime;
		procedure SetMaxValue( NewValue: TDateTime );
		function GetMinValue: TDateTime;
		procedure SetMinValue( NewValue: TDateTime );
		function GetValue: TDateTime;
		procedure SetValue( NewValue: TDateTime );

	public
		destructor Destroy; override;
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property DateIncrement: TKDateIncrement
						 read FDateIncrement write SetDateIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property Value: TDateTime
						 read GetValue write SetValue;

	{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;

	end;

{ TKTimeSpeedProperties }

	TKTimeSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: TDateTime;
		FMinValue: TDateTime;
		FMaxValue: TDateTime;
		FTimeIncrement: TDateTime;

		function GetTimeIncrement: TDateTime;
		procedure SetTimeIncrement( NewValue: TDateTime );
		function GetMaxValue: TDateTime;
		procedure SetMaxValue( NewValue: TDateTime );
		function GetMinValue: TDateTime;
		procedure SetMinValue( NewValue: TDateTime );
		function GetValue: TDateTime;
		procedure SetValue( NewValue: TDateTime );

	public
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property TimeIncrement: TDateTime
						 read GetTimeIncrement write SetTimeIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property Value: TDateTime
						 read GetValue write SetValue;

	{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;

	end;

{ TKDateTimeSpeedProperties }

	TKDateTimeSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: TDateTime;
		FMinValue: TDateTime;
		FMaxValue: TDateTime;
		FTimeIncrement: TDateTime;
		FDateIncrement: TKDateIncrement;

		function GetTimeIncrement: TDateTime;
		procedure SetTimeIncrement( NewValue: TDateTime );
		procedure SetDateIncrement( NewValue: TKDateIncrement );
		function GetMaxValue: TDateTime;
		procedure SetMaxValue( NewValue: TDateTime );
		function GetMinValue: TDateTime;
		procedure SetMinValue( NewValue: TDateTime );

	public
		destructor Destroy; override;
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;

	published
		property DateIncrement: TKDateIncrement
						 read FDateIncrement write SetDateIncrement;
		property TimeIncrement: TDateTime
						 read GetTimeIncrement write SetTimeIncrement;
		property MaxValue: TDateTime
						 read GetMaxValue write SetMaxValue;
		property MinValue: TDateTime
						 read GetMinValue write SetMinValue;
		property Value: TDateTime
						 read FValue write FValue;

	{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;

	end;

{ TKTextSpeedProperties }

	TKTextSpeedProperties = class( TKCustomSpeedProperties )
	private
		FValue: string;
		FButtonHint: string;
		FButtonGlyph: TBitmap;
		
		procedure SetButtonGlyph( Value: TBitmap );
		
	public
		destructor Destroy; override;
		constructor Create; override;
		procedure Assign( Source: TPersistent ); override;
    
	published
	{ from TKCustomSpeedProperties }
		property DisplayFormat;
		property EditFormat;
		property AllowNull default True;

		property ButtonGlyph: TBitmap
						 read FButtonGlyph write SetButtonGlyph;
		property ButtonHint: string
						 read FButtonHint write FButtonHint;
		property Value: string
						 read FValue write FValue;

	end;

{ TKCustomSpeedDialog }

	TKCustomSpeedDialog = class( TKSystemDialog )
	private
{ From TKCustomDialog }
		property Expiration;
		property TimeOut;

{ From TKSystemDialog }
		property BitmapOption;
		property DialogStyle;

	protected
		function GetSpeedControlClass: TKCustomSpeedControlClass; virtual; abstract;

	published
{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

{ From TKSystemDialog }
		property Text;

	end;

{ TKCustomUpDownDialog }

	TKCustomUpDownDialog = class( TKCustomSpeedDialog )
	private
		FRxUpDown: Boolean;

	published
		property RxUpDown: Boolean
						 read FRxUpDown write FRxUpDown;

{ From TKSystemDialog }
		property Text;

	end;

{ TKSpeedFloatDialog }

	TKSpeedFloatDialog = class( TKCustomUpDownDialog )
	private
		FSpeedProperties: TKFloatSpeedProperties;
		FOnFormatEditText: TKFormatFloatTextEvent;
		FOnFormatDisplayText: TKFormatFloatTextEvent;

		procedure SetSpeedProperties( Value: TKFloatSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKFloatSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;
		property OnFormatDisplayText: TKFormatFloatTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatFloatTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	end;

{ TKCustomSpeedIntegerDialog }

	TKCustomSpeedIntegerDialog = class( TKCustomUpDownDialog )
	private
		FOnFormatEditText: TKFormatIntegerTextEvent;
		FOnFormatDisplayText: TKFormatIntegerTextEvent;

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

		property OnFormatDisplayText: TKFormatIntegerTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatIntegerTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	end;

{ TKSpeedIntegerDialog }

	TKSpeedIntegerDialog = class( TKCustomSpeedIntegerDialog )
	private
		FSpeedProperties: TKIntegerSpeedProperties;

		procedure SetSpeedProperties( Value: TKIntegerSpeedProperties );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKIntegerSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;

{ From TKCustomSpeedIntegerDialog }
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKSpeedHexaDialog }

	TKSpeedHexaDialog = class( TKCustomSpeedIntegerDialog )
	private
		FSpeedProperties: TKHexaSpeedProperties;

{ From TKCustomSpeedIntegerDialog }
		property OnFormatDisplayText;
		property OnFormatEditText;

		procedure SetSpeedProperties( Value: TKHexaSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKHexaSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;

	end;

{ TKSpeedDateDialog }

	TKSpeedDateDialog = class( TKCustomUpDownDialog )
	private
		FSpeedProperties: TKDateSpeedProperties;
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		procedure SetSpeedProperties( Value: TKDateSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKDateSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;
		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	end;

{ TKSpeedTimeDialog }

	TKSpeedTimeDialog = class( TKCustomUpDownDialog )
	private
		FSpeedProperties: TKTimeSpeedProperties;
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		procedure SetSpeedProperties( Value: TKTimeSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKTimeSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;
		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	end;

{ TKSpeedDateTimeDialog }

	TKSpeedDateTimeDialog = class( TKCustomUpDownDialog )
	private
		FSpeedProperties: TKDateTimeSpeedProperties;
		FOnFormatEditText: TKFormatDateTimeTextEvent;
		FOnFormatDisplayText: TKFormatDateTimeTextEvent;

		procedure SetSpeedProperties( Value: TKDateTimeSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKDateTimeSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;
		property OnFormatDisplayText: TKFormatDateTimeTextEvent
						 read FOnFormatDisplayText write FOnFormatDisplayText;
		property OnFormatEditText: TKFormatDateTimeTextEvent
						 read FOnFormatEditText write FOnFormatEditText;

	end;

{ TKSpeedTextDialog }

	TKSpeedTextDialog = class( TKCustomSpeedDialog )
	private
		FOnButtonClick: TNotifyEvent;
		FOnCheckValue: TKCheckTextEvent;
		FSpeedProperties: TKTextSpeedProperties;

		procedure SetSpeedProperties( Value: TKTextSpeedProperties );

	protected
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		function GetSpeedControlClass: TKCustomSpeedControlClass; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property SpeedProperties: TKTextSpeedProperties
						 read FSpeedProperties write SetSpeedProperties;
		property OnCheckValue: TKCheckTextEvent
						 read FOnCheckValue write FOnCheckValue;
		property OnButtonClick: TNotifyEvent
						 read FOnButtonClick write FOnButtonClick;

{ From TKSystemDialog }
		property Text;

	end;

{
--------------------------------------------------------------------------------
------------------------------ Extended Dialogs --------------------------------
--------------------------------------------------------------------------------
}

	EKExtendedDialog = class( EKCustomDialog );

{ TKExtendedDialog }

	TKExtendedDialog = class( TKCustomDialog )
	private

{ From TKBaseDialog }
		property AfterCheckParams;
		property BeforeCheckParams;

{ From TKCustomDialog }
		property DialogUnits;
		property FormClass;
		property SystemMenu;

	public
		constructor Create( AOwner: TComponent ); override;

	published
{ From TKBaseDialog }
		property Caption;
		property Centered;
		property PosLeft;
		property PosTop;

		property OnCheckParams;

{ From TKCustomDialog }
		property ActiveCaption;
		property ActiveFont;
		property BackGround;
		property Font;
		property Icon;
		property InactiveCaption;
		property InactiveFont;

	end;

{ TKCustomExtDialog }

	TKCustomExtDialog = class( TKExtendedDialog )
	private
{ From TKBaseDialog }
		property Width;
		property Height;

{ From TKCustomDialog }
		property Picture;

		property AfterExecute;
		property BeforeExecute;

	protected
		function GetBitmapResName: string; virtual; abstract;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomLogonDialog }

	TKCustomLogonDialog = class;

	TKLogActionTries = 1..High( Byte );

	TKCustomLogonDialog = class( TKCustomExtDialog )
	private
		FLogon: string;
		FLocked: Boolean;
		FPassword: string;
		FPasswordChar: Char;
		FLogActionTries: Word;

		FLogonCharCase: TEditCharCase;
		FPasswordCharCase: TEditCharCase;
		FMaxTries: TKLogActionTries;

		FOnFail: TKDialogNotifyEvent;
		FOnCancel: TKDialogNotifyEvent;
		FOnSucceed: TKDialogNotifyEvent;
		FOnLastFail: TKDialogNotifyEvent;

		procedure KeyboardClose; override;

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

	protected
		function GetDefaultFailMessage: string; virtual; abstract;
		function GetDefaultLastFailMessage: string; virtual; abstract;

		procedure DoFail; dynamic;
		procedure DoCancel; dynamic;
		procedure DoSucceed; dynamic;
		procedure DoLastFail; dynamic;

		function DoValidate: Boolean; virtual; abstract;

	public
		constructor Create( AOwner: TComponent ); override;

		property Locked: Boolean
						 read FLocked;
		property LogActionTries: Word
						 read FLogActionTries;
		property Password: string
						 read FPassword;

	published
		property Logon: string
						 read FLogon write FLogon;
		property LogonCharCase: TEditCharCase
						 read FLogonCharCase write FLogonCharCase default ecNormal;
		property MaxTries: TKLogActionTries
						 read FMaxTries write FMaxTries default 3;
		property PasswordChar: Char
						 read FPasswordChar write FPasswordChar default '*';
		property PasswordCharCase: TEditCharCase
						 read FPasswordCharCase write FPasswordCharCase default ecNormal;

		property OnCancel: TKDialogNotifyEvent
						 read FOnCancel write FOnCancel;
		property OnFail: TKDialogNotifyEvent
						 read FOnFail write FOnFail;
		property OnLastFail: TKDialogNotifyEvent
						 read FOnLastFail write FOnLastFail;
		property OnSucceed: TKDialogNotifyEvent
						 read FOnSucceed write FOnSucceed;

	end;

{ TKLogonDialog }

	TKLogonDialog = class;

	TKValidateLogonEvent = procedure( Sender: TKLogonDialog; const Logon, Password: string;
		var IsValid: Boolean ) of object;

	TKLogonDialog = class( TKCustomLogonDialog )
	private
		FOnValidate: TKValidateLogonEvent;

	protected
		function GetBitmapResName: string; override;
		function GetDefaultFailMessage: string; override;
		function GetDefaultLastFailMessage: string; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		function DoValidate: Boolean; override;

	published
		property OnValidate: TKValidateLogonEvent
						 read FOnValidate write FOnValidate;

	end;

{ TKPasswordDialog }

	TKPasswordDialog = class;

	TKValidatePasswordEvent = procedure( Sender: TKPasswordDialog; const Logon,
		Password, NewPassword: string; var IsValid: Boolean ) of object;

	TKPasswordDialog = class( TKCustomLogonDialog )
	private
		FNewPassword: string;

		FOnValidate: TKValidatePasswordEvent;
		FOnPasswordMismatch: TKDialogNotifyEvent;

	protected
		function GetBitmapResName: string; override;
		function GetDefaultFailMessage: string; override;
		function GetDefaultLastFailMessage: string; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		function DoValidate: Boolean; override;

		procedure DoPasswordMismatch; dynamic;

	public
		property NewPassword: string
						 read FNewPassword;

	published
		property OnPasswordMismatch: TKDialogNotifyEvent
						 read FOnPasswordMismatch write FOnPasswordMismatch;
		property OnValidate: TKValidatePasswordEvent
						 read FOnValidate write FOnValidate;

	end;

{ TKPrintDialog }

	TKPrintDialogButton = ( pdbPrint, pdbPreview, pdbCancel );
	TKPrintDialogButtons = set of TKPrintDialogButton;

	TKPrintDialog = class( TKCustomExtDialog )
	private
		FText: string;
		FShowDialog: Boolean;
		FRightAlign: Boolean;
		FDefaultAction: TKPrintDialogButton;
		FPrintDialogButtons: TKPrintDialogButtons;
		FOnPrint: TNotifyEvent;
		FOnCancel: TNotifyEvent;
		FOnPreview: TNotifyEvent;

		procedure SetDefaultAction( Value: TKPrintDialogButton );
		procedure SetPrinddialogButtons( Value: TKPrintDialogButtons );

	protected
		function GetBitmapResName: string; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		procedure DoPrint; dynamic;
		procedure DoPreview; dynamic;
		procedure DoCancel; dynamic;

	public
		constructor Create( AOwner: TComponent ); override;

		procedure Print; virtual;
		procedure Preview; virtual;

	published

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

		property Text: string
						 read FText write FText;
		property ShowDialog: Boolean
						 read FShowDialog write FShowDialog default True;
		property RightAlign: Boolean
						 read FRightAlign write FRightAlign default True;
		property DefaultAction: TKPrintDialogButton
						 read FDefaultAction write SetDefaultAction default pdbPrint;
		property PrintDialogButtons: TKPrintDialogButtons
						 read FPrintDialogButtons write SetPrinddialogButtons
						 default [pdbPrint, pdbPreview, pdbCancel];

		property OnPrint: TNotifyEvent
						 read FOnPrint write FOnPrint;
		property OnPreview: TNotifyEvent
						 read FOnPreview write FOnPreview;
		property OnCancel: TNotifyEvent
						 read FOnCancel write FOnCancel;

	end;

{ TKCustomActionDialog }

	EKActionDialog = class( EKExtendedDialog );
	EKActionDlgCancel = class( EKActionDialog );

	TKCustomActionDialog = class;

	TKRefreshActionDlgProc = procedure( Percent: TKPercent; const Caption1, Caption2: string ) of object;

	TKDialogActionEvent = procedure( Sender: TKCustomActionDialog; 
		CallBack: TKRefreshActionDlgProc ) of object;

	TKCustomActionDialog = class( TKCustomExtDialog )
	private
		FText: string;
		FCanceled: Boolean;
		FBtn1Caption: string;
		FBtn2Caption: string;
		FLabel1Caption: string;
		FLabel2Caption: string;
		FRightAlign: Boolean;
		FCloseOnTerminate: Boolean;
		FOnCancel: TNotifyEvent;

	protected
		procedure DoBeforeExecute; override;
		function GetFormPainterClass: TKFormPainterClass; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		procedure DoBeforeAction; virtual; abstract;
		procedure DoAfterAction; virtual; abstract;
		procedure ActionProc( Percent: TKPercent; const Caption1, Caption2: string ); dynamic;

		function Btn1Enabled: Boolean; virtual;
		function Btn2Enabled: Boolean; virtual;
		function CancelEnabled: Boolean; virtual;

		procedure DoCancel; dynamic;

		property Btn1Caption: string
						 read FBtn1Caption write FBtn1Caption;
		property Btn2Caption: string
						 read FBtn2Caption write FBtn2Caption;
		property Label1Caption: string
						 read FLabel1Caption write FLabel1Caption;
		property Label2Caption: string
						 read FLabel2Caption write FLabel2Caption;

	public
		constructor Create( AOwner: TComponent ); override;

		property Canceled: Boolean
						 read FCanceled;

	published

{ From TKCustomDialog }
		property CloseOnAltF4;
		property CloseOnEscape;
		property CloseStyle;

		property Text: string
						 read FText write FText;
		property CloseOnTerminate: Boolean
						 read FCloseOnTerminate write FCloseOnTerminate default True;
		property RightAlign: Boolean
						 read FRightAlign write FRightAlign default True;

		property OnCancel: TNotifyEvent
						 read FOnCancel write FOnCancel;

	end;

	TKBackupButton = ( bdbBackup, bdbRestore, bdbCancel );
	TKBackupButtons = set of TKBackupButton;

	TKCryptoButton = ( cbEncipher, cbDecipher, cbCancel );
	TKCryptoButtons = set of TKCryptoButton;

const
	DEFAULT_BACKUP_BUTTONS = [bdbBackup, bdbRestore, bdbCancel];
	DEFAULT_CRYPTO_BUTTONS = [cbEncipher, cbDecipher, cbCancel];

type

{ TKBackupDialog }

	TKBackupDialog = class( TKCustomActionDialog )
	private
		FBackupButtons: TKBackUpButtons;
		FOnBackup: TKDialogActionEvent;
		FOnRestore: TKDialogActionEvent;

	protected
		function GetBitmapResName: string; override;

		procedure DoBeforeAction; override;
		procedure DoAfterAction; override;

		function Btn1Enabled: Boolean; override;
		function Btn2Enabled: Boolean; override;
		function CancelEnabled: Boolean; override;

	public
		constructor Create( AOwner: TComponent ); override;

		procedure Backup; virtual;
		procedure Restore; virtual;

	published
		property BackUpButtons: TKBackUpButtons
						 read FBackUpButtons write FBackUpButtons default DEFAULT_BACKUP_BUTTONS;
		property OnBackup: TKDialogActionEvent
						 read FOnBackup write FOnBackup;
		property OnRestore: TKDialogActionEvent
						 read FOnRestore write FOnRestore;

	end;

{ TKCryptoDialog }

	TKCryptoDialog = class( TKCustomActionDialog )
	private
		FCryptoButtons: TKCryptoButtons;
		FOnEncipher: TKDialogActionEvent;
		FOnDecipher: TKDialogActionEvent;

	protected
		function GetBitmapResName: string; override;

		procedure DoBeforeAction; override;
		procedure DoAfterAction; override;

		function Btn1Enabled: Boolean; override;
		function Btn2Enabled: Boolean; override;
		function CancelEnabled: Boolean; override;

	public
		constructor Create( AOwner: TComponent ); override;

		procedure Encipher; virtual;
		procedure Decipher; virtual;

	published
		property CryptoButtons: TKCryptoButtons
						 read FCryptoButtons write FCryptoButtons default DEFAULT_CRYPTO_BUTTONS;
		property OnEncipher: TKDialogActionEvent
						 read FOnEncipher write FOnEncipher;
		property OnDecipher: TKDialogActionEvent
						 read FOnDecipher write FOnDecipher;

	end;

{
--------------------------------------------------------------------------------
------------------------ Dual List Dialog Implementation -----------------------
--------------------------------------------------------------------------------
}

	TKDualListButton = ( dlbInclude, dlbIncludeAll, dlbExclude, dlbExcludeAll,
		dlbExchangeUp, dlbExchangeDown );
	TKDualListButtons = set of TKDualListButton;

const
	DUALLIST_NAV_BUTTONS = [dlbInclude, dlbIncludeAll, dlbExclude, dlbExcludeAll];
	DEFAULT_DUALLIST_VISIBLE_BUTTONS = DUALLIST_NAV_BUTTONS + [dlbExchangeUp, dlbExchangeDown];

type

	EKDualListDialog = class( EKCustomDialog );

{ TKCustomDualListDialog }

	TKCustomDualListDialog = class;

	TKDualListButtonClickEvent = procedure ( Sender: TKCustomDualListDialog;
		Button: TKDualListButton ) of object;

	TKCustomDualListDialog = class( TKCustomDialog )
	private
		FFlat: Boolean;
		FMultiSelect: Boolean;
		FListImages: TImageList;
		FAcceptDragDrop: Boolean;
		FVisibleButtons: TKDualListButtons;
		FOnSourceListClick: TNotifyEvent;
		FOnDestinationListClick: TNotifyEvent;
		FOnDualListButtonClick: TKDualListButtonClickEvent;

		FDestList: TStrings;
		FSourceList: TStrings;
		FDestSel: TKBitsWrapper;
		FSourceSel: TKBitsWrapper;

		FDestListHint: string;
		FSourceListHint: string;

		procedure SetDestList( Value: TStrings );
		procedure SetSourceList( Value: TStrings );
		procedure SetDestSel( Value: TKBitsWrapper );
		procedure SetSourceSel( Value: TKBitsWrapper );

{ from TKBaseDialog }
		property Height;
		property Width;

{ from TKCustomDialog }
		property FormClass;
		property SystemMenu;
		property AfterExecute;
		property BeforeExecute;
		property AfterCheckParams;
		property BeforeCheckParams;
		property TimeOut;
		property Picture;
		property SizeStyle;
		property Expiration;
		property DialogKind;
		property DialogUnits;
		property Limits;

		procedure SetListImages( Value: TImageList );
		procedure SetVisibleButtons( Value: TKDualListButtons );

	protected
		procedure Notification( AComponent: TComponent;	Operation: TOperation ); override;

		function DoCheckParams: Boolean; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		procedure DoSrcListClick; dynamic;
		procedure DoDstListClick; dynamic;
		procedure DoDualListButton( Button: TKDualListButton ); dynamic;

		procedure ListsChange( Sender: TObject ); dynamic;
		procedure NotifyListsChange( sl: TStrings ); virtual;
		procedure RetrieveSelection( IsSource: Boolean; Bits: TBits ); dynamic;

		property AcceptDragDrop: Boolean
						 read FAcceptDragDrop write FAcceptDragDrop default True;
		property VisibleButtons: TKDualListButtons
						 read FVisibleButtons write SetVisibleButtons
						 default DEFAULT_DUALLIST_VISIBLE_BUTTONS;
		property ListImages: TImageList
						 read FListImages write SetListImages;
		property Flat: Boolean
						 read FFlat write FFlat default True;
		property MultiSelect: Boolean
						 read FMultiSelect write FMultiSelect default True;

		property DestList: TStrings
						 read FDestList write SetDestList;
		property SourceList: TStrings
						 read FSourceList write SetSourceList;
		property DestSel: TKBitsWrapper
						 read FDestSel write SetDestSel;
		property SourceSel: TKBitsWrapper
						 read FSourceSel write SetSourceSel;

		property SourceListHint: string
						 read FSourceListHint	write FSourceListHint;
		property DestListHint: string
						 read FDestListHint write FDestListHint;

		property OnSourceListClick: TNotifyEvent
						 read FOnSourceListClick write FOnSourceListClick;
		property OnDestinationListClick: TNotifyEvent
						 read FOnDestinationListClick write FOnDestinationListClick;
		property OnDualListButtonClick: TKDualListButtonClickEvent
						 read FOnDualListButtonClick write FOnDualListButtonClick;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published

{ from TKBaseDialog }
		property Caption;
		property Centered;
		property PosLeft;
		property PosTop;
		property OnCheckParams;

{ from TKCustomDialog }
		property ActiveCaption;
		property ActiveFont;
		property BackGround;
		property Font;
		property Icon;
		property InactiveCaption;
		property InactiveFont;
		property SmallCaption;
		property CloseOnAltF4;
		property CloseStyle;

	end;

	TKCustomDualListDialogClass = class of TKCustomDualListDialog;

{ TKCustomDualListDialogForm }

	TKCustomDualListDialogForm = class( TKCustomDialogForm )
	private
		FSrcList: TListBox;
		FDstList: TListBox;
		FIncludeBtn: TSpeedButton;
		FIncAllBtn: TSpeedButton;
		FExcludeBtn: TSpeedButton;
		FExAllBtn: TSpeedButton;
		FExchangeUp: TSpeedButton;
		FExchangeDown: TSpeedButton;
		FbnOk: TBitBtn;
		FbnCancel: TBitBtn;
		FlbSourceList: TLabel;
		FlbDestList: TLabel;

		function GetImgList: TImageList;
		function GetDialog: TKCustomDualListDialog;
		function GetVisibleButtons: TKDualListButtons;
		procedure SetVisibleButtons( Value: TKDualListButtons );

	protected
{ Form Methods derived from TKCustomDialogForm }
		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		procedure PrepareForm; override;
		procedure UnprepareForm; override;

{ Form Methods introduced in this class }
		procedure LoadButtonBitmaps; dynamic;
		procedure SetButtons; virtual;
		procedure ClearSelection( List: TListBox ); dynamic;
		procedure FillSelection( List: TListBox; Bits: TBits ); dynamic;
		function FirstSelection( List: TCustomListBox ): Integer; dynamic;
		procedure RetrieveSelection( List: TListBox; Bits: TBits ); dynamic;
		procedure SelectItem( List: TCustomListBox; Index: Integer ); virtual;
		procedure MoveSelected( List: TCustomListBox; Items: TStrings ); dynamic;
		procedure InternalMoveSelect( List: TCustomListBox; Items: TStrings;
			Index: Integer ); virtual;
		procedure ButtonClicked( Button: TKDualListButton ); virtual;

{ Form Events }
		procedure FormShow( Sender: TObject );
		procedure ListClick( Sender: TObject );
		procedure IncludeBtnClick( Sender: TObject );
		procedure ExcludeBtnClick( Sender: TObject );
		procedure IncAllBtnClick( Sender: TObject );
		procedure ExcAllBtnClick( Sender: TObject );
		procedure ExchangeBtnClick( Sender: TObject );
		procedure ListDrawItem( Control: TWinControl; Index: Integer;
			Rect: TRect; State: TOwnerDrawState );
		procedure ListMeasureItem( Control: TWinControl; Index: Integer;
			var Height: Integer );
		procedure DstListKeyDown( Sender: TObject; var Key: Word;
			Shift: TShiftState );
		procedure ListMouseDown( Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer );
		procedure ListDragDrop( Sender, Source: TObject; X, Y: Integer );
		procedure ListDragOver( Sender, Source: TObject; X, Y: Integer; State: TDragState;
			var Accept: Boolean );
		procedure SrcListKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

		property SrcList: TListBox
						 read FSrcList;
		property DstList: TListBox
						 read FDstList;
		property IncludeBtn: TSpeedButton
						 read FIncludeBtn;
		property IncAllBtn: TSpeedButton
						 read FIncAllBtn;
		property ExcludeBtn: TSpeedButton
						 read FExcludeBtn;
		property ExAllBtn: TSpeedButton
						 read FExAllBtn;
		property ExchangeUp: TSpeedButton
						 read FExchangeUp;
		property ExchangeDown: TSpeedButton
						 read FExchangeDown;
		property bnOk: TBitBtn
						 read FbnOk;
		property bnCancel: TBitBtn
						 read FbnCancel;
		property lbSourceList: TLabel
						 read FlbSourceList;
		property lbDestList: TLabel
						 read FlbDestList;
		property ilList: TImageList
						 read GetImgList;

		property VisibleButtons: TKDualListButtons
						 read GetVisibleButtons write SetVisibleButtons;

		property Dialog: TKCustomDualListDialog
						 read GetDialog;

	end;

{ TKDualListDialog }

	TKDualListDialog = class( TKCustomDualListDialog )
	published
{ from TKCustomDialog }
		property CloseOnEscape;

{ from TKCustomDualListDialog }
		property AcceptDragDrop;
		property ListImages;
		property VisibleButtons;

		property DestList;
		property SourceList;
		property DestSel;
		property SourceSel;

		property SourceListHint;
		property DestListHint;

		property OnSourceListClick;
		property OnDestinationListClick;
		property OnDualListButtonClick;

	end;

{
	ImageList defines only two images. Image1 is for Source and Image2 is for Dest.
	The images must have the n x m dimensions (it will be truncated). If the ImageList
	is nil, the images will be cleared and not used.
	If the result was true, the Source/Dest List/Sel will be filled with the last user
	selection of the dialog.
	To simplify the function call, if the parameters of selection were nil, the list will be
	filled unselected. But using by that way the user will assume that the results will not
	be returned.
	The function can be also called without the DestList parameter created (the same fault
	will take place). At least the SourceList *MUST* be provided with at least one string
	item, otherwise an exception will be raised. If the source and destlist were provided,
	they will be differentiated (i.e. Exclude from SourceList all the DestList items - cannot
	have duplicate items into the two listboxes).
	The number of items of the Selection lists *SHOULD* be less or equal to the Strings lists.
	If the selection lists was higher than the Strings Lists it will be shrinked. At the end,
	all lists will be adjusted (in contents and size) if the result is true.
}

function DualListDialog( const ACaption: string; ASourceList, ADestList: TStrings;
	ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;

const

	BTN_GUTTER_Y = 15;
	BTN_GUTTER_X = BTN_GUTTER_Y;

{----------------------- DualListEdit Dialog Implementation --------------------}

type

	TWinControlClass = class of TWinControl;

	EKDualListEditDialog = class( EKDualListDialog );

{ TKCustomDualListEditDialog }

	TKCustomDualListEditDialog = class( TKCustomDualListDialog )
	private
		FText: string;
		FEditShortCut: TShortCut;
		FClearShortCut: TShortCut;

	protected
		function GetEditControlClass: TKCustomSpeedControlClass; virtual; abstract;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;
		procedure NotifyListsChange( sl: TStrings ); override;

		property EditShortCut: TShortCut
						 read FEditShortCut write FEditShortCut default SC_CTRL_RETURN;
		property ClearShortCut: TShortCut
						 read FClearShortCut write FClearShortCut default SC_CTRL_DELETE;
		property Text: string
						 read FText write FText;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomDualListEditDialogForm }

	TKCustomDualListEditDialogForm = class( TKCustomDualListDialogForm )
	private
		FEditControl: TWinControl;

		function GetDialog: TKCustomDualListEditDialog;

	protected
{ Form Methods derived from TKCustomDialogForm }
		procedure PlaceButtons; override;
		procedure PlaceControls; override;

{ Form Methods derived from TKCustomDualListDialogForm }
		procedure SetButtons; override;
		procedure ButtonClicked( Button: TKDualListButton ); override;
		procedure SelectItem( List: TCustomListBox; Index: Integer ); override;
		procedure InternalMoveSelect( List: TCustomListBox; Items: TStrings; Index: Integer ); override;

{ Form Methods introduced in this class }
		function GetControl: TWinControl;
		procedure UpdateEdit; dynamic;
		procedure AddFieldValue; dynamic;
		procedure SelectFirstDest; dynamic;

		procedure ClearEditControlText; virtual; abstract;
		function GetEditControlText: string; virtual; abstract;
		procedure SetEditControlText( const Value: string ); virtual; abstract;

{ Form Events }
    procedure EditControlBtnClick( Sender: TObject ); dynamic;
		procedure EditControlDblClick( Sender: TObject ); dynamic;
		procedure EditControlKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState ); dynamic;

		property EditControl: TWinControl
						 read GetControl;
		property EditControlText: string
						 read GetEditControlText write SetEditControlText;

	public
		property Dialog: TKCustomDualListEditDialog
						 read GetDialog;

	end;

{ TKDualListEditDialog }

	TKDualListEditDialog = class( TKCustomDualListEditDialog )
	private
		FOnEditButtonClick: TNotifyEvent;

	protected
		function GetEditControlClass: TKCustomSpeedControlClass; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

	published
		property OnEditButtonClick: TNotifyEvent
						 read FOnEditButtonClick write FOnEditButtonClick;

	{ from TKCustomDialog }
		property CloseOnEscape;

	{ from TKCustomDualListDialog }
		property AcceptDragDrop;
		property ListImages;
		property VisibleButtons;

		property SourceList;
		property SourceSel;

	{ from TKCustomDualListEditDialog }
		property EditShortCut;
		property ClearShortCut;
		property Text;

		property OnSourceListClick;
		property OnDestinationListClick;
		property OnDualListButtonClick;

	end;

function DualListEditDialog( const ACaption: string; ASourceList, ADestList: TStrings;
	ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;

const

	EDT_GUTTER = 5;
	EDT_HEIGHT = 24;
	EDT_WIDTH = 140;

	EDT_RECT: TRect =
	(
		Left:   0;
		Top:    0;
		Right:  EDT_WIDTH;
		Bottom: EDT_HEIGHT;
	);

type

{ TKCustomDualListEditMaskDialog }

	TKEditMaskType = ( emtFloat, emtInteger, emtHexa, emtDate, emtTime, emtText,
		emtFile, emtFolder, emtCustom );

	TKCustomDualListEditMaskDialog = class( TKCustomDualListEditDialog )
	private
		FEditMask: string;
		FEditMaskType: TKEditMaskType;

		procedure SetEditMask( const Value: string );
		procedure SetEditMaskType( Value: TKEditMaskType );

{ from TKCustomDialog }
		property CloseOnEscape;

	protected
		function GetEditControlClass: TKCustomSpeedControlClass; override;
		function GetDefaultFormClass: TKCustomDialogFormClass; override;

		property EditMask: string
						 read FEditMask write SetEditMask;
		property EditMaskType: TKEditMaskType
						 read FEditMaskType write SetEditMaskType default emtInteger;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKCustomDualListEditMaskDialogForm }

	TKCustomDualListEditMaskDialogForm = class( TKCustomDualListEditDialogForm )
	private
		FCleared: Boolean;
		FCleaning: Boolean;

		function GetDialog: TKCustomDualListEditMaskDialog;

	protected
		procedure PlaceControls; override;
		function GetControl: TKCustomSpeedMask;
		procedure ClearEditControlText; override;
		function GetEditControlText: string; override;
		procedure SetEditControlText( const Value: string ); override;

		procedure AddFieldValue; override;

		property EditControl: TKCustomSpeedMask
						 read GetControl;
		property Dialog: TKCustomDualListEditMaskDialog
						 read GetDialog;
		property Cleaning: Boolean
						 read FCleaning;
		property Cleared: Boolean
						 read FCleared;

	end;

{ TKDualListEditMaskDialog }

	TKDualListEditMaskDialog = class( TKCustomDualListEditMaskDialog )
	published
	{ from TKCustomDualListDialog }
		property AcceptDragDrop;
		property ListImages;
		property VisibleButtons;

		property SourceList;
		property SourceSel;

	{ from TKCustomDualListEditDialog }
		property EditShortCut;
		property ClearShortCut;

	{ from TKCustomDualListEditMaslDialog }
		property EditMask;
		property Text;

		property OnSourceListClick;
		property OnDestinationListClick;
		property OnDualListButtonClick;

	end;

function DualListEditMaskDialog( const ACaption, AEditMask: string; ASourceList,
	ADestList: TStrings; ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;

{
--------------------------------------------------------------------------------
------------------------- Custom Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}
{ TKLogInfoDialog }
{ TKTabStopsDialog }


{$HINTS ON}

{
--------------------------------------------------------------------------------
----------------------- Default Size/Position Constants ------------------------
--------------------------------------------------------------------------------
}

const
	DIALOG_ERROR_CHECKPARAMS_FAILURE = -1;

	FM_WIDTH = 480;
	FM_HEIGHT = 240;
	FM_GUTTER = 86;
	FM_LEFTMARGIN = 12;
	FM_RIGHTMARGIN = 38;
	FM_RECT: TRect =
	(
		Left:   0;
		Top:    0;
		Right:  FM_WIDTH;
		Bottom: FM_HEIGHT
	);

	BN_WIDTH = 82;
	BN_HEIGHT = 28;
	BN_GUTTER = 10;
	BN_DISTANCE = 14;
	BN_RIGHTMARGIN = 20;
	BN_RECT: TRect =
	(
		Left:   0;
		Top:    0;
		Right:  BN_WIDTH;
		Bottom: BN_HEIGHT
	);

	IM_TOP = 15;
	IM_LEFT = 12;
	IM_WIDTH = 32;
	IM_HEIGHT = 32;
	IM_RECT: TRect =
	(
		Left:   IM_LEFT;
		Top:    IM_TOP;
		Right:  IM_LEFT + IM_WIDTH;
		Bottom: IM_TOP + IM_HEIGHT
	);

	LB_TOP = 15;
	LB_LEFT = 68;
	LB_HEIGHT = 21;
	LB_WIDTH = FM_WIDTH - 106;
	LB_RECT: TRect =
	(
		Left:   LB_LEFT;
		Top:    LB_TOP;
		Right:  LB_LEFT + LB_WIDTH;
		Bottom: LB_TOP + LB_HEIGHT
	);

	ED_TOP = 0;
	ED_GUTTER = 10;
	ED_HEIGHT = 24;
	ED_FMWIDTH = 380;
	ED_LEFT = IM_LEFT;
	ED_WIDTH = ED_FMWIDTH - 2 * IM_LEFT;
	ED_RECT: TRect =
	(
		Left:   ED_LEFT;
		Top:    ED_TOP;
		Right:  ED_LEFT + ED_WIDTH;
		Bottom: ED_TOP + ED_HEIGHT
	);

procedure RegisterSystemDialogs;
procedure UnregisterSystemDialogs;
procedure ClearSavedSystemDlgProcs;

implementation

{$R brdgGlyphs.res}

{.$R u:\delphi\klib100\source\dialogs100\lib\brdgGlyphs.res

 We try to use this form to D4 complaint but this generates a incomatible
 directory list for other delphis.... sorry...
}

uses
	Messages, SysUtils, ComCtrls, CheckLst, ClipBrd, uksyResStr, ukrResStr,
  ukdgResStr, ukdgUtils;

{
--------------------------------------------------------------------------------
-------------------------- Basic Dialog Architecture ---------------------------
--------------------------------------------------------------------------------
}

{-------------------------------- TKBaseDialog ---------------------------------}

constructor TKBaseDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FWidth := 228;
	FPosTop := 25;
	FHeight := 150;
	FPosLeft := 29;
	FCentered := true;
	FCaption := ClassName;
end;

procedure TKBaseDialog.DoBeforeCheckParams;
begin
	if Assigned( FBeforeCheckParams ) then
		FBeforeCheckParams( Self );
end;

procedure TKBaseDialog.DoAfterCheckParams;
begin
	if Assigned( FAfterCheckParams ) then
		FAfterCheckParams( Self );
end;

function TKBaseDialog.GetExitCode( ExecuteCode: Integer ): Boolean;
begin
	Result := ( ExecuteCode <> DIALOG_ERROR_CHECKPARAMS_FAILURE );
end;

function TKBaseDialog.InternalExecute: Integer;
begin
	Result := DIALOG_ERROR_CHECKPARAMS_FAILURE;
	if DialogCheckParams then
		Result := DialogExecute;
end;

function TKBaseDialog.Execute: Boolean;
begin
	Result := GetExitCode( InternalExecute );
end;

function TKBaseDialog.DialogCheckParams: Boolean;
{ true if all parameters needed to call execute are properly setup }
begin
	FChecking := True;
	try
		DoBeforeCheckParams;
		Result := DoCheckParams;
		if Result then
			DoAfterCheckParams;
	finally
	  FChecking := False;
	end;
end;

procedure TKBaseDialog.ForceChecking;
begin
	if ( not Executing ) then
		RaiseException( EKCustomDialog, sErrInvDlgChecking );
end;

procedure TKBaseDialog.ForceExecuting;
begin
	if ( not Executing ) then
		RaiseException( EKCustomDialog, sErrInvDlgExecuting );
end;

function TKBaseDialog.DoCheckParams: Boolean;
{ This method shuold be overriden to provide custom behaviour. Make sure that
	inherited DoCheckParams is the last call in the overriden method. }
begin
	Result := true;
	if Assigned( FOnCheckParams ) then
		FOnCheckParams( Self, Result );
end;

function TKBaseDialog.DialogExecute: Integer;
begin
	DoBeforeExecute;
	try
		FExecuting := True;
		try
			Result := DoExecute;
		finally
			FExecuting := False;
		end;
	finally
		DoAfterExecute;
	end;
end;

{----------------------------- TKCustomDialogForm ------------------------------}

const

	MODAL_RESULTS: array[mrNone..mrYesToAll] of string[10] = ( 'bnNone', 'bnOk',
		'bnCancel', 'bnAbort', 'bnRetry', 'bnIgnore', 'bnYes', 'bnNo', 'bnAll',
		'bnNoToAll', 'bnYesToAll' );

constructor TKCustomDialogForm.CreateDialog( AOwner: TKCustomDialog );
var
	w, h: Integer;
begin
	ForceObject( AOwner );
	CreateNew( nil ); 
	FDialog := AOwner;
{ default dialog properties }
	Scaled := false;
	BorderIcons := [];
	KeyPreview := true;
	AutoScroll := false;
	Font := FDialog.Font;
	BorderStyle := bsDialog;
	FFormUnits := GetAveCharSize( Canvas );
{ setup width and height }
	w := FDialog.Width;
	h := FDialog.Height;
	Width := ScaleX( w );
	Height := ScaleY( h );
{ setup position }
	if FDialog.Centered then
		Position := poScreenCenter
	else
	begin
		Top := ScaleY( FDialog.PosTop );
		Left := ScaleX( FDialog.PosLeft );
	end;
end;

destructor TKCustomDialogForm.Destroy;
begin
	FTimer.Free;
	inherited Destroy;
end;

function TKCustomDialogForm.GetInstanceCount( Index: TControlClass ): Cardinal;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to ComponentCount - 1 do
		Inc( Result, Ord( CheckObjectClass( Components[i], Index ) ) );
end;

procedure TKCustomDialogForm.TimerExpired( Sender: TObject );
begin
	FTimer.Enabled := false;
	ModalResult := mrCancel;
end;

procedure TKCustomDialogForm.Activate;
begin
	inherited Activate;
	if CheckObject( FTimer ) then
		FTimer.Enabled := true;
end;

procedure TKCustomDialogForm.SetControlBounds( AControl: TControl; Rect: TRect );
begin
	with AControl do
	begin
		Top := ScaleY( Rect.Top );
		Left := ScaleX( Rect.Left );
		Width := ScaleX( Rect.Right - Rect.Left );
		Height := ScaleY( Rect.Bottom - Rect.Top );
	end;
end;

function TKCustomDialogForm.AddControl( const ctName: string; ctRect: TRect;
	ctEnabled, ctVisible: Boolean; ctParent: TWinControl; ctClass: TControlClass ): TControl;
var
	i: Integer;
	sName: string;
begin
	ForceClass( ctClass );
	Result := ctClass.Create( Self );
	with Result do
	begin
		i := 0;
		sName := FixString( ctName, ceIdentifier, CH_NULL );
		while CheckObject( Self.FindComponent( sName ) ) do
		begin
			Inc( i );
			sName := Format( '%s_%d', [sName, i] );
		end;
		Name := sName;
		Parent := ctParent;
		Enabled := ctEnabled;
		Visible := ctVisible;
	end;
	SetControlBounds( Result, ctRect );
end;

function TKCustomDialogForm.AddButton( const bnCaption, bnName: string;
	bnRect: TRect; bnEnabled, bnVisible, bnDefault: Boolean; bnResult: TModalResult;
	bnGlyph: TBitmap;	bnParent: TWinControl; bnClick: TNotifyEvent ): TBitBtn;
begin
	Result := ( AddControl( bnName, bnRect, bnEnabled, bnVisible, bnParent, TBitBtn ) as TBitBtn );
	if CheckObject( Result ) then
		with Result do
		begin
			Glyph := bnGlyph;
			OnClick := bnClick;
			Caption := bnCaption;
			Default := bnDefault;
			ModalResult := bnResult;
		end;
end;

function TKCustomDialogForm.AddButtonDefault( bnCaption: string;	bnResult: TModalResult;
	bnDefault: Boolean ): TBitBtn;
begin
	Result := AddButton( bnCaption, MODAL_RESULTS[bnResult], BN_RECT, true,
		true, bnDefault, bnResult, nil, Self, nil );
end;

function TKCustomDialogForm.AddSpeedButton( const bnCaption, bnName: string;
	bnRect: TRect; bnEnabled, bnVisible, bnDown, bnFlat, bnAllowAllUp: Boolean;
	bnGlyph: TBitmap;	bnGrpIdx: Integer; bnParent: TWinControl;
	bnClick: TNotifyEvent ): TSpeedButton;
begin
	Result := ( AddControl( bnName, bnRect, bnEnabled, bnVisible, bnParent, TSpeedButton ) as TSpeedButton );
	if CheckObject( Result ) then
		with Result do
		begin
			Flat := bnFlat;
			Glyph := bnGlyph;
			OnClick := bnClick;
			Caption := bnCaption;
			if ( bnGrpIdx <> 0 ) then
			begin
				Down := bnDown;
				AllowAllUp := bnAllowAllUp;
			end;
		end;
end;

procedure TKCustomDialogForm.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( ( Key = VK_ESCAPE ) and Dialog.CloseOnEscape ) or
		 ( ( ssAlt in Shift ) and ( Key = VK_F4 ) and Dialog.CloseOnAltF4 ) then
	begin
		Dialog.KeyboardClose;
		Exit;
	end;
	if ( ( Key = VK_ESCAPE ) or ( ( ssAlt in Shift ) and ( Key = VK_F4 ) ) ) then
	begin
		Key := 0;
		MessageBeep( 0 );
	end;
	if ( ssCtrl in Shift ) then
		case Key of
			Ord( 'c' ), Ord( 'C' ): if CopyToClipBoard then Key := 0;
			Ord( 'x' ), Ord( 'X' ): if CutToClipBoard then Key := 0;
			Ord( 'v' ), Ord( 'V' ): if PasteFromClipBoard then Key := 0;
		end;
	inherited KeyDown( Key, Shift );
end;

function TKCustomDialogForm.CopyToClipBoard: Boolean;
begin
	Result := False;
	{ Derived classes can use this method to copy anything to clipboard.... }
end;

function TKCustomDialogForm.CutToClipBoard: Boolean;
begin
	Result := False;
	{ Derived classes can use this method to cut anything to clipboard.... }
end;

function TKCustomDialogForm.PasteFromClipBoard: Boolean;
begin
	Result := False;
	{ Derived classes can use this method to paste anything from clipboard.... }
end;

function TKCustomDialogForm.ScaleX( Value: Integer ): Integer;
begin
	if Dialog.DialogUnits then
		Result := MulDiv( Value, FFormUnits.X, 4 )
	else
		Result := MulDiv( Value, Screen.Width, DEFAULT_DIALOG_FORM_SCALE.X );
end;

function TKCustomDialogForm.ScaleY( Value: Integer ): Integer;
begin
	if Dialog.DialogUnits then
		Result := MulDiv( Value, FFormUnits.Y, 8 )
	else
		Result := MulDiv( Value, Screen.Height, DEFAULT_DIALOG_FORM_SCALE.Y );
end;

function TKCustomDialogForm.InverseScaleX( Value: Integer ): Integer;
begin
	if Dialog.DialogUnits then
		Result := MulDiv( Value, 4, FFormUnits.X )
	else
		Result := MulDiv( Value, DEFAULT_DIALOG_FORM_SCALE.X, Screen.Width );
end;

function TKCustomDialogForm.InverseScaleY( Value: Integer ): Integer;
begin
	if Dialog.DialogUnits then
		Result := MulDiv( Value, 8, FFormUnits.Y )
	else
		Result := MulDiv( Value, DEFAULT_DIALOG_FORM_SCALE.Y, Screen.Height );
end;

function TKCustomDialogForm.DoShowModal: Integer;
begin
	Result := inherited ShowModal;
end;

procedure TKCustomDialogForm.DoInternalShow;
begin
  inherited Show;
end;

procedure TKCustomDialogForm.SetControlsTabOrder;
begin
	{ do nothing }
end;

function TKCustomDialogForm.ShowModal: Integer;
begin
	PrepareForm;
	try
		PlaceControls;
		PlaceButtons;
		SetControlsTabOrder;
		Dialog.UserCallBack;
		Result := DoShowModal;
	finally
		UnprepareForm;
	end;
end;

procedure TKCustomDialogForm.Show;
begin
	PrepareForm;
	try
		PlaceControls;
		PlaceButtons;
		SetControlsTabOrder;
		Dialog.UserCallBack;
		DoInternalShow;
	finally
		UnprepareForm;
	end;
end;

{------------------------------- TKDialogLimits --------------------------------}

constructor TKDialogLimits.Create( AOwner: TKCustomDialog );
begin
	ForceObject( AOwner );
	inherited Create;
	FOwner := AOwner;
end;

procedure TKDialogLimits.SetMaxHeight( Value: Integer );
begin
	if ( Value >= FMinHeight ) then
		FMaxHeight := Value;
end;

procedure TKDialogLimits.SetMaxWidth( Value: Integer );
begin
	if ( Value >= FMinWidth ) then
		FMaxWidth := Value;
end;

procedure TKDialogLimits.SetMinHeight( Value: Integer );
begin
	if ( Value <= FMaxHeight ) then
		FMinHeight := Value;
end;

procedure TKDialogLimits.SetMinWidth( Value: Integer );
begin
	if ( Value <= FMaxWidth ) then
		FMinWidth := Value;
end;

{------------------------------- TKCustomDialog --------------------------------}

constructor TKCustomDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FForm := nil;
	FPicture := nil;
	FTimeOut := false;
	FExpiration := 120;
	FSystemMenu := nil;
	FFormPainter := nil;
	FDialogUnits := true;
	FCloseOnAltF4 := true;
	FCloseOnEscape := true;
	FSmallCaption := false;
	FCloseStyle := csEnabled;
	FDialogKind := dkDefault;
	FSizeStyle := ssNoSizing;
	FFormClass := GetDefaultFormClass;
	FFormPainterClass := GetFormPainterClass;

{ Visual support - helper classes creation }
	FIcon := TIcon.Create;
	FFont := TFont.Create;
	FPicture := TPicture.Create;
	FActiveFont := TFont.Create;
	FInactiveFont := TFont.Create;
	FLimits := TKDialogLimits.Create( Self );
	FBackground := TKGradient.Create( nil, nil );
	FActiveCaption := TKGradient.Create( nil, nil );
	FInactiveCaption := TKGradient.Create( nil, nil );

{ Visual support - helper classes default settings - built with DialogData Informations }
	with FFont do
	begin
		Name := DEFAULT_DIALOG_FONT_NAME;
		Size := DEFAULT_DIALOG_FONT_SIZE;
		Style := [];
	end;
	GetSystemCaptionFont( GetDesktopWindow, ActiveFont );
	GetSystemCaptionFont( GetDesktopWindow, InactiveFont );
	FActiveFont.Color := DIALOG_DEFAULT_ACTIVE_FONT_COLOR;
	FInactiveFont.Color := DIALOG_DEFAULT_INACTIVE_FONT_COLOR; 
	DefaultBackground;
	DefaultActiveCaption;
	DefaultInactiveCaption;
	DefaultLimits;
end;

destructor TKCustomDialog.Destroy;
begin
	FIcon.Free;
	FFont.Free;
	FActiveFont.Free;
	FInactiveFont.Free;
	FBackGround.Free;
	FActiveCaption.Free;
	FInactiveCaption.Free;
	FLimits.Free;
	FPicture.Free;
	inherited Destroy;
end;

function TKCustomDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKCustomDialogForm;
end;

function TKCustomDialog.GetFormPainterClass: TKFormPainterClass;
begin
  Result := TKFormPainter;
end;

procedure TKCustomDialog.SetPicture( Value: TPicture );
begin
	FPicture.Assign( Value );
end;

procedure TKCustomDialog.SetIcon( Value: TIcon );
begin
	FIcon.Assign( Value );
end;

procedure TKCustomDialog.SetFont( Value: TFont );
begin
	FFont.Assign( Value );
end;

procedure TKCustomDialog.UserCallBack;
begin
	{ When the form are completely prepared, then call the DialogData callback }
	with uksyUtils.DialogData do
		if CheckReference( @swCallBack ) then
			swCallBack( Form, UserData );
end;

procedure TKCustomDialog.DefaultBackground;
begin
	FBackGround.AssignFromGradData( DialogDataEx.BackGround );
end;

procedure TKCustomDialog.DefaultActiveCaption;
begin
	FActiveCaption.AssignFromGradData( DialogDataEx.ActiveCaption );
end;

procedure TKCustomDialog.DefaultInactiveCaption;
begin
	FInactiveCaption.AssignFromGradData( DialogDataEx.InactiveCaption );
end;

procedure TKCustomDialog.DefaultLimits;
begin
	with FLimits do
	begin
		MaxWidth := Width;
		MinWidth := Width;
		MaxHeight := Height;
		MinHeight := Height;
	end;
end;

procedure TKCustomDialog.DoBeforeExecute;
var
	f: TKCustomDialogForm;
begin
	f := nil;
	Form := FormClass.CreateDialog( Self );
	try
		f := Form; { need this for stayontop based dialogs - there is a list and get/set }

{ Defininig the BorderStyle in RunTime forces a RecreateWnd- so, do it before
	creating the FormPainter class }
		with f do
		begin
			if ( SizeStyle <> ssNoSizing ) then
			begin
				if SmallCaption then
					BorderStyle := bsSizeToolWin
				else
					BorderStyle := bsSizeable;
			end
			else if SmallCaption then
				BorderStyle := bsToolWindow;
{ Set the form's caption }
			Caption := Self.Caption;
{ create the timer if this is a TimeOut dialog }
			if ( TimeOut and ( Expiration > 0 ) ) then
			begin
				FTimer := TTimer.Create( nil ); { Hard Couple!! Form.FTimer }
				Timer.OnTimer := TimerExpired;
				Timer.Interval :=  ( Integer( Expiration ) * SECOND_TO_MSECOND );
			end;
		end;
	except
		f.Free;
		Form := nil;
		raise;
	end;
	{ Now it is safe to create the form painter }
	if Designing( Self ) then
		FormPainter := FormPainterClass.Create( Application.Handle )
	else
{ At design-time, Form.Handle is the Proxy Handle (?) }
		FormPainter := FormPainterClass.Create( Form.Handle );
	try
		with FormPainter do
		begin
{ assign fonts and gradients }
			ActiveFont.Assign( FActiveFont );
			InactiveFont.Assign( FInactiveFont );
			BackGround.Assign( FBackGround );
			ActiveCaption.Assign( FActiveCaption );
			InactiveCaption.Assign( FInactiveCaption );
{ determine how to setup the caption close button }
			if ( CloseStyle <> csNone ) then
			begin
				Buttons.Add( cbkClose );
				if ( CloseStyle = csDisabled ) then
					Buttons[cbkClose].Enabled := false;
			end;
{ determine how to setup the remaining caption buttons }
			if ( DialogKind <> dkDefault ) then
			begin
				case DialogKind of
					dkRollup:
					begin
						Buttons.Add( cbkRollup );
						if ( SizeStyle = ssLimittedSizing ) then
							SizeStyle := ssSizing;
					end;
					dkHelper: Buttons.Add( cbkHelp );
					dkCustom: Buttons.Add( cbkCustom );
				end;
			end;
{ determine sizing characteristics of the form }
			if ( SizeStyle = ssLimittedSizing ) then
			begin
{ define maximum sizing limits of the form }
				with WindowManager.MaxTrack, FLimits do
				begin
					Ignore := false;
					Width := f.ScaleX( MaxWidth );
					Height := f.ScaleY( MaxHeight );
				end;
{ define minimum sizing limits of the form }
				with WindowManager.MinTrack, FLimits do
				begin
					Ignore := false;
					Width := f.ScaleX( MinWidth );
					Height := f.ScaleY( MinHeight );
				end;
			end;
{ assign the form system icon }
			if ( not( FIcon.Empty or SmallCaption ) ) then
				SystemIcon.Icon := FIcon;
{ assign the system menu }
			if CheckObject( FSystemMenu ) then
				SystemIcon.SystemMenu := FSystemMenu;
		end;
	except
		FormPainter.Free;
		FormPainter := nil;
		raise;
	end;
	if Assigned( FBeforeExecute ) then
		FBeforeExecute( Self, Form );
end;

procedure TKCustomDialog.KeyboardClose;
begin
	ForceExecuting;
	Form.Close;
end;

procedure TKCustomDialog.DoAfterExecute;
begin
	try
		if Assigned( FAfterExecute ) then
			FAfterExecute( Self, Form );
	finally
	  DestroyForm;
	end;
end;

procedure TKCustomDialog.DestroyForm;
begin
	FreeClean( FFormPainter );
	FreeClean( FForm );
end;

function TKCustomDialog.GetForm: TKCustomDialogForm;
begin
	Result := FForm;
end;

procedure TKCustomDialog.SetForm( Value: TKCustomDialogForm );
begin
  FForm := Value;
end;

function TKCustomDialog.GetFormPainter: TKFormPainter;
begin
	Result := FFormPainter;
end;

procedure TKCustomDialog.SetFormPainter( Value: TKFormPainter );
begin
	FFormPainter := Value;
end;

function TKCustomDialog.GetExitCode( ExecuteCode: Integer ): Boolean;
begin
	Result := ( ExecuteCode = mrOK ) and ( inherited GetExitCode( ExecuteCode ) );
end;

function TKCustomDialog.DoExecute: Integer;
{ This method should be overriden to provide custom behaviour }
begin
	Result := Form.ShowModal;
end;

function TKCustomDialog.DoCheckParams: Boolean;
begin
	with DialogDataEx do
		Result := ( ( inherited DoCheckParams ) and CheckClass( FormClass ) and
			CheckClass( FormPainterClass ) and ( ( not CheckReference( @swCheckParamCallBack ) ) or
			swCheckParamCallBack( Self, CheckParamData ) ) );
end;

{
--------------------------------------------------------------------------------
------------------------ Default Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}

{ TKDialogForm }

function TKDialogForm.GetDialog: TKDialog;
begin
	Result := ( inherited Dialog as TKDialog );
end;

procedure TKDialogForm.PlaceButtons;
begin
	if Assigned( Dialog.OnPlaceButtons ) then
		Dialog.OnPlaceButtons( Dialog, Self );
end;

procedure TKDialogForm.PlaceControls;
begin
	if Assigned( Dialog.OnPlaceControls ) then
		Dialog.OnPlaceControls( Dialog, Self );
end;

procedure TKDialogForm.PrepareForm;
begin
	if Assigned( Dialog.OnPrepareForm ) then
		Dialog.OnPrepareForm( Dialog, Self );
end;

procedure TKDialogForm.UnprepareForm;
begin
	if Assigned( Dialog.OnUnPrepareForm ) then
		Dialog.OnUnPrepareForm( Dialog, Self );
end;

{ TKDialog }

function TKDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKDialogForm;
end;

{
--------------------------------------------------------------------------------
-------------------------- Logo Dialog Implementation --------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TKLogoDialogForm = class( TKCustomDialogForm )
	private
		FImage: TImage;

	protected
		procedure PrepareForm; override;
		procedure UnprepareForm; override;

		property Image: TImage
						 read FImage;

	end;

procedure TKLogoDialogForm.PrepareForm;
begin
	FImage := AddControl( 'imLogo', IM_RECT, true, true, Self, TImage ) as TImage;
	with FImage do
	begin
		Stretch := true;
		Transparent := true;
		Picture.Assign( Dialog.Picture );
	end;
end;

procedure TKLogoDialogForm.UnprepareForm;
begin
	{ do nothing }
end;

{
--------------------------------------------------------------------------------
------------------------- System Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

var
{
  Here we will have a list of dialogs with a list of forms!
}
	ShowDlgOnTopList: TList = nil;
	SysDlgInstance: TKSystemDialog = nil;
	SysDlgInstanceOnTop: TKSystemDialog = nil;

type

{ TKSystemDialogForm }

	TKSystemDialogForm = class( TKLogoDialogForm )
	private
		FLabel: TLabel;
		FFormPainter: TKFormPainter;

		function GetDialog: TKSystemDialog;
		procedure ButtonClick( Sender: TObject );
		procedure FormClose( Sender: TObject; var Action: TCloseAction );

	protected
		function GetLabelRect: TRect; virtual;
		procedure PlaceLabel; virtual;
		procedure PlaceButtons; override;
		procedure PlaceControls; override;
		procedure PrepareForm; override;
		procedure UnprepareForm; override;
		procedure TimerExpired( Sender: TObject ); override;

		function AddButtonDefault( bnCaption: string;	bnResult: TModalResult;
			bnDefault: Boolean ): TBitBtn; override;

		property Dialog: TKSystemDialog
						 read GetDialog;
		property TextLabel: TLabel
						 read FLabel;
		property FormPainter: TKFormPainter
						 read FFormPainter write FFormPainter;				 

	public
		destructor Destroy; override;

	end;

{ TKSystemDialogForm }

destructor TKSystemDialogForm.Destroy;
var
	i: Integer;
begin
{
	If The dialog is not present in global list, so that is ok! This calling is by
	dialog.destroy (at finalization or at normal object destruction). But if the
	user, minutes later, closes the form, the dialog will not be signaled, so the
	dialog will be inside the global list and if it is the last form, we should destroy
	it! At finalization it will be destroyed in any case, but the early destruction
	avoid resource wasting
}
	if ( Dialog.StayOnTop and CheckObject( Dialog.FormOnTopList ) ) then
	begin
		for i := 0 to Dialog.FormOnTopList.Count - 1 do
			if ( TKSystemDialogForm( Dialog.FormOnTopList[i] ) = Self ) then
			begin
				Dialog.FormOnTopList.Delete( i );
				Break;
			end;
		Dialog.Form := nil;
{ for free, the list must exists and be cleared. }
		if ( not ( Dialog.CacheDlgInstance or uksyUtils.Destroying( Dialog ) or
		  CheckList( Dialog.FormOnTopList ) ) ) then
			Dialog.Free;
	end;
	FreeClean( FFormPainter );
	inherited Destroy;
end;

function TKSystemDialogForm.GetDialog: TKSystemDialog;
begin
	Result := ( inherited Dialog as TKSystemDialog );
end;

function TKSystemDialogForm.GetLabelRect: TRect;
begin
	Result := LB_RECT;
end;

procedure TKSystemDialogForm.TimerExpired( Sender: TObject );
begin
	Timer.Enabled := False;
	if ( not Dialog.StayOnTop ) then
		ModalResult := mrCancel
	else
	  Close;	  
end;

procedure TKSystemDialogForm.PlaceLabel;
var
	rt,
	lrt: TRect;
	ibnCount,
	iMinWidth: Integer;
begin
	with TextLabel do
	begin
		AutoSize := false;
		Transparent := true;
		Caption := Dialog.Text;
		rt := ClientRect;
		lrt := GetLabelRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > ( lrt.Right - lrt.Left ) ) then
		begin
			Caption := TextBlock( Caption, Canvas, ( lrt.Right - lrt.Left ) );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Width := rt.Right;
		Height := Max( rt.Bottom, 2 * Height );
		Self.Height := Top + Height + ScaleY( FM_GUTTER );
		Self.Width := Self.Width - ( ( lrt.Right - lrt.Left ) - Width );
		ibnCount := InstanceCount[TBitBtn];
		iMinWidth := ScaleX( ibnCount * BN_WIDTH + ( ibnCount - 1 ) * BN_DISTANCE +
			FM_LEFTMARGIN + FM_RIGHTMARGIN );
		if ( Self.Width < iMinWidth ) then
			Self.Width := iMinWidth;
		DialogData.RightAlign := false;
	end;
end;

procedure TKSystemDialogForm.PlaceControls;
begin
{ do shit here }
	PlaceLabel;
end;

procedure TKSystemDialogForm.PlaceButtons;
var
	i,
	ibnCount: Integer;
	LastButton: TBitBtn;
begin
	LastButton := nil;
	ibnCount := InstanceCount[TBitBtn];
	for i := 0 to ControlCount - 1 do
		if CheckObjectClass( Controls[i], TBitBtn ) then
			with TBitBtn( Controls[i] ) do
			begin
				Top := Self.ClientHeight - Height - ScaleY( BN_GUTTER );
				if ( not CheckObject( LastButton ) ) then
					if Dialog.RightAlign then
						Left :=
							( Self.ClientWidth ) - ( ibnCount * Width ) -
							( ScaleX( BN_RIGHTMARGIN ) ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
					else
						Left :=
							( Self.Width div 2 ) -
							( ibnCount * Width div 2 ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
				else if CheckObject( LastButton ) then
					Left := LastButton.Left + Width +	ScaleX( BN_DISTANCE );
				LastButton := TBitBtn( Self.Controls[i] );
			end;
end;

procedure TKSystemDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	case ( Dialog as TKSystemDialog ).DialogStyle of
		dsOK:
			AddButtonDefault( sCapOK, mrOK, true );
		dsCancel:
			AddButtonDefault( sCapCancel, mrCancel, true );
		dsOKCancel:
		begin
			AddButtonDefault( sCapOK, mrOK, true );
			AddButtonDefault( sCapCancel, mrCancel, false );
		end;
		dsYesNo:
		begin
			AddButtonDefault( sCapYes, mrYes, true );
			AddButtonDefault( sCapNo, mrNo, false );
		end;
		dsYesNoCancel:
		begin
			AddButtonDefault( sCapYes, mrYes, true );
			AddButtonDefault( sCapNo, mrNo, false );
			AddButtonDefault( sCapCancel, mrCancel, false );
		end;
		dsYesAllNoCancel:
		begin
			AddButtonDefault( sCapYes, mrYes, true );
			AddButtonDefault( sCapYesAll, mrAll, false );
			AddButtonDefault( sCapNo, mrNo, false );
			AddButtonDefault( sCapCancel, mrCancel, false );
		end;
		dsYesNoAllCancel:
		begin
			AddButtonDefault( sCapYes, mrYes, true );
			AddButtonDefault( sCapNo, mrNo, false );
			AddButtonDefault( sCapNoAll, mrAll, false );
			AddButtonDefault( sCapCancel, mrCancel, false );
		end;
		dsAbortRetryIgnore:
		begin
			AddButtonDefault( sCapAbort, mrAbort, true );
			AddButtonDefault( sCapRetry, mrRetry, false );
			AddButtonDefault( sCapIgnore, mrIgnore, false );
		end;
	end;
	FLabel := AddControl( 'lbMessage', GetLabelRect, true, true, Self, TLabel ) as TLabel;
	if Dialog.StayOnTop then
		OnClose := FormClose;
end;

function TKSystemDialogForm.AddButtonDefault( bnCaption: string;	bnResult: TModalResult;
	bnDefault: Boolean ): TBitBtn;
begin
	Result := inherited AddButtonDefault( bnCaption, bnResult, bnDefault );
	if Dialog.StayOnTop then
		Result.OnClick := ButtonClick;
end;

procedure TKSystemDialogForm.UnprepareForm;
begin
{ avoid abstract errors... no shit! }
end;

procedure TKSystemDialogForm.FormClose( Sender: TObject; var Action: TCloseAction );
begin
	if Dialog.StayOnTop then
	begin
		Hide;
		Action := caFree;
	end;	
end;

procedure TKSystemDialogForm.ButtonClick( Sender: TObject );
begin
	if ( Dialog.StayOnTop and Dialog.ButtonClick( Sender as TBitBtn ) ) then
		Close;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKSystemDialog }

constructor TKSystemDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	DialogUnits := false;
	FStayOnTop := False;
	FCacheDlgInstance := True;
	FormClass := TKSystemDialogForm;
{ New properties }
	FText := '';
	FRightAlign := false;
	FDialogStyle := dsOK;
{ force first-time loading of the bitmap }
	FBitmapOption := boCustom;
	BitmapOption := boInformation;
{ Redeclared private properties }
	SystemMenu := nil;
	AfterExecute := nil;
	BeforeExecute := nil;
	AfterCheckParams := nil;
	BeforeCheckParams := nil;
	Width := FM_WIDTH;
	Height := FM_HEIGHT;
end;

destructor TKSystemDialog.Destroy;
var
	i: Integer;
begin
	inherited Destroy;
	if StayOnTop then
	begin
		for i := 0 to ShowDlgOnTopList.Count - 1 do
			if ( TKSystemDialog( ShowDlgOnTopList[i] ) = Self ) then
			begin
				ShowDlgOnTopList.Delete( i );
				Break;
			end;
		ClearFormOnTopList;
	end;
	FreeClean( FFormOnTopList );
end;

procedure TKSystemDialog.ClearFormOnTopList;
var
	obj: TObject;
begin
	while CheckList( FFormOnTopList ) do
	begin
		obj := TObject( FFormOnTopList[0] );
		FFormOnTopList.Delete( 0 );
		obj.Free;
	end;
end;

procedure TKSystemDialog.SetBitmapOption( Value: TKBitmapOption );
begin
	if ( FBitmapOption <> Value ) then
	begin
		FBitmapOption := Value;
		if ( Value = boCustom ) then
			SetPicture( nil )
		else
			Picture.Bitmap.LoadFromResourceName( HInstance, SYSDLG_BMP_RESNAMES[TKBitmapOption( Value )] );
	end;
end;

function TKSystemDialog.GetForm: TKCustomDialogForm;
begin
{ Until here i trust that FFormOnTopList is created for StayOnTop dialogs }
	if ( not StayOnTop )then
		Result := inherited GetForm
	else
		Result := TKCustomDialogForm( FFormOnTopList[FFormOnTopList.Count - 1] );
end;

function TKSystemDialog.GetFormPainter: TKFormPainter;
begin
	if ( not StayOnTop )then
		Result := inherited GetFormPainter
	else
		Result := ( Form as TKSystemDialogForm ).FormPainter; { get the correct instance!! }
end;

procedure TKSystemDialog.SetForm( Value: TKCustomDialogForm );
begin
	if ( not StayOnTop ) then
		inherited SetForm( Value )
	else if CheckObject( Value ) then
	begin
		Value.FormStyle := fsStayOnTop;
		if ( not CheckObject( FFormOnTopList ) ) then
			FFormOnTopList := TList.Create;
		FFormOnTopList.Add( Value );
		{
			there is no need for .free or delete when value is nil!
			This is done in TKSystemDialog.Destroy
		}
	end;	
end;

procedure TKSystemDialog.SetFormPainter( Value: TKFormPainter );
begin
	if ( not StayOnTop )then
		inherited SetFormPainter( Value )
	else if CheckObject( Value ) then
		( Form as TKSystemDialogForm ).FormPainter := Value;
end;

procedure TKSystemDialog.SetStayOnTop( Value: Boolean );
begin
	if ( Value <> FStayOnTop ) then
	begin
	  FStayOnTop := Value;
		if FStayOnTop then
		begin
			if ( not CheckObject( ShowDlgOnTopList ) ) then
				ShowDlgOnTopList := TList.Create;
			ShowDlgOnTopList.Add( Self );
		end
		else
			ClearFormOnTopList;
	end;
end;

procedure TKSystemDialog.SetPicture( Value: TPicture );
begin
	if ( FBitmapOption = boCustom ) then
		inherited SetPicture( Value );
end;

function TKSystemDialog.DoExecute: Integer;
begin
	Result := mrNone;
	if ( not StayOnTop ) then
		Result := inherited DoExecute
	else
		Form.Show;
end;

procedure TKSystemDialog.DestroyForm;
begin
	if ( not StayOnTop ) then
		inherited DestroyForm;
end;

function TKSystemDialog.ButtonClick( Button: TBitBtn ): Boolean;
begin
	Result := ( not Assigned( FOnButtonClick ) );
	if ( not Result ) then
	begin
		Result := True;
		FOnButtonClick( Self, Button, Result );
	end;	
end;

{--------------------------- Internal Implementation ---------------------------}

type

	TKShowDialogForm = class( TKSystemDialogForm )
	private
		function GetDialog: TKShowDialog;

	protected
		function CopyToClipBoard: Boolean; override;

	public
		property Dialog: TKShowDialog
						 read GetDialog;
	end;

function TKShowDialogForm.GetDialog: TKShowDialog;
begin
	Result := ( inherited Dialog as TKShowDialog );
end;

function TKShowDialogForm.CopyToClipBoard: Boolean;
begin
	ClipBoard.AsText := Dialog.Text;
	Result := True;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKShowDialog }

constructor TKShowDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FormClass := TKShowDialogForm;
end;

procedure TKShowDialog.CloseDialogs;
begin
	if StayOnTop then
		ClearFormOnTopList;
end;

function InternalShowDialog( const Title, Message: string; Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption ): TModalResult;
begin
	if ( not CheckObject( SysDlgInstance ) ) then
	begin
		SysDlgInstance := TKShowDialog.Create( nil );
		SysDlgInstance.StayOnTop := False;
		SysDlgInstance.RightAlign := True;
	end;
	SysDlgInstance.TimeOut := False;
	SysDlgInstance.Expiration := 0;
	SysDlgInstance.Text := Message;
	SysDlgInstance.Caption := Title;
	SysDlgInstance.BitmapOption := BitmapOption;
	if ( BitmapOption = boCustom ) then
		SysDlgInstance.Picture.Assign( Bitmap );
	SysDlgInstance.DialogStyle := Style;
	RefreshDialogData( SysDlgInstance );
	Result := SysDlgInstance.InternalExecute;
end;

procedure InternalShowDialogOnTop( const Title, Message: string; Bitmap: TBitmap;
	Style: TKDialogStyle;	BitmapOption: TKBitmapOption );
begin
	if ( not CheckObject( SysDlgInstanceOnTop ) ) then
	begin
		SysDlgInstanceOnTop := TKShowDialog.Create( nil );
		SysDlgInstanceOnTop.StayOnTop := True;
		SysDlgInstanceOnTop.RightAlign := True;
		SysDlgInstanceOnTop.CacheDlgInstance := True;
	end;
	SysDlgInstanceOnTop.TimeOut := False;
	SysDlgInstanceOnTop.Expiration := 0;
	SysDlgInstanceOnTop.Text := Message;
	SysDlgInstanceOnTop.Caption := Title;
	SysDlgInstanceOnTop.BitmapOption := BitmapOption;
	if ( BitmapOption = boCustom ) then
		SysDlgInstanceOnTop.Picture.Assign( Bitmap );
	SysDlgInstanceOnTop.DialogStyle := Style;
	RefreshDialogData( SysDlgInstanceOnTop );
	SysDlgInstanceOnTop.Execute;
end;

function InternalExpireShowDialog( const Title, Message: string; SecTimeOut: Integer;
	Bitmap: TBitmap; Style: TKDialogStyle; BitmapOption: TKBitmapOption ): TModalResult;
begin
if ( not CheckObject( SysDlgInstance ) ) then
	begin
		SysDlgInstance := TKShowDialog.Create( nil );
		SysDlgInstance.StayOnTop := False;
		SysDlgInstance.RightAlign := True;
	end;
	SysDlgInstance.TimeOut := True;
	SysDlgInstance.Expiration := Cardinal( SecTimeOut );
	SysDlgInstance.Text := Message;
	SysDlgInstance.Caption := Title;
	SysDlgInstance.BitmapOption := BitmapOption;
	if ( BitmapOption = boCustom ) then
		SysDlgInstance.Picture.Assign( Bitmap );
	SysDlgInstance.DialogStyle := Style;
	RefreshDialogData( SysDlgInstance );
	Result := SysDlgInstance.InternalExecute;
end;

procedure InternalExpireShowDialogOnTop( const Title, Message: string; SecTimeOut: Integer;
	Bitmap: TBitmap; Style: TKDialogStyle; BitmapOption: TKBitmapOption );
begin
	if ( not CheckObject( SysDlgInstanceOnTop ) ) then
	begin
		SysDlgInstanceOnTop := TKShowDialog.Create( nil );
		SysDlgInstanceOnTop.StayOnTop := True;
		SysDlgInstanceOnTop.RightAlign := True;
		SysDlgInstanceOnTop.CacheDlgInstance := True;
	end;
	SysDlgInstanceOnTop.TimeOut := True;
	SysDlgInstanceOnTop.Expiration := Cardinal( SecTimeOut );
	SysDlgInstanceOnTop.Text := Message;
	SysDlgInstanceOnTop.Caption := Title;
	SysDlgInstanceOnTop.BitmapOption := BitmapOption;
	if ( BitmapOption = boCustom ) then
		SysDlgInstanceOnTop.Picture.Assign( Bitmap );
	SysDlgInstanceOnTop.DialogStyle := Style;
	RefreshDialogData( SysDlgInstanceOnTop );
	SysDlgInstanceOnTop.Execute;
end;

{--------------------------- Internal Implementation ---------------------------}

type

{ TKInputDialogForm }

	TKInputDialogForm = class( TKSystemDialogForm )
	private
		FEdit: TMaskEdit;

		function GetDialog: TKInputDialog;

	protected
		procedure PlaceControls; override;
		procedure UnprepareForm; override;
		function GetLabelRect: TRect; override;

		property Dialog: TKInputDialog
						 read GetDialog;

	end;

{ TKInputDialogForm }

function TKInputDialogForm.GetDialog: TKInputDialog;
begin
	Result := ( inherited Dialog as TKInputDialog );
end;

function TKInputDialogForm.GetLabelRect: TRect;
begin
	Result := ED_RECT;
	Result.Top := LB_RECT.Top;
	Result.Bottom := LB_RECT.Bottom;
end;

procedure TKInputDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	Width := ScaleX( ED_FMWIDTH );
	FEdit := AddControl( 'edMask', ED_RECT, true, true, Self, TMaskEdit ) as TMaskEdit;
	with FEdit do
	begin
		if CheckTrimStr( Dialog.EditMask ) then
			EditMask := Dialog.EditMask;
		Text := Dialog.EditText;
		Width := Self.ClientWidth - 2 * Left;
		Top := TextLabel.Top + TextLabel.Height + ScaleY( ED_GUTTER );
		Self.Height := Top + Height + ScaleY( FM_GUTTER + ED_GUTTER );
    PasswordChar := Dialog.PasswordChar;
	end;
	ActiveControl := FEdit;
end;

procedure TKInputDialogForm.UnprepareForm;
begin
	inherited UnprepareForm;
	if ( FEdit.Modified and ( ModalResult = mrOK ) ) then
		Dialog.EditText := FEdit.Text;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKInputDialog }

constructor TKInputDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	BitmapOption := boCustom;
	DialogStyle := dsOkCancel;
  FPasswordChar := CH_NULL;
	FormClass := TKInputDialogForm;
	FEditText := '';
	FEditMask := '';
end;

function InternalInputDialog( const Title, Prompt, EditMask: string;
	var Text: string ): Boolean;
var
	d: TKInputDialog;
begin
	d := TKInputDialog.Create( nil );
	try
		d.Text := Prompt;
		d.Caption := Title;
		d.RightAlign := true;
		d.EditText := Text;
		d.EditMask := EditMask;
		RefreshDialogData( d );
		Result := d.Execute;
		if Result then
		  Text := d.EditText;
	finally
		d.Free;
	end;
end;

function InternalPasswordDialog( const Title, Prompt, EditMask: string;
  PasswordChar: Char; var Text: string ): Boolean;
var
	d: TKInputDialog;
begin
	d := TKInputDialog.Create( nil );
	try
		d.Text := Prompt;
		d.Caption := Title;
		d.RightAlign := true;
		d.EditText := Text;
		d.EditMask := EditMask;
    d.PasswordChar := PasswordChar;
		RefreshDialogData( d );
		Result := d.Execute;
		if Result then
		  Text := d.EditText;
	finally
		d.Free;
	end;
end;

{--------------------------- Internal Implementation ---------------------------}

type

{ TKCustomInputListDialogForm }

	TKCustomInputListDialogForm = class( TKSystemDialogForm )
	private
		FCombo: TComboBox;

		function GetDialog: TKCustomInputListDialog;

	protected
		function GetLabelRect: TRect; override;
		procedure PlaceControls; override;
		procedure UnprepareForm; override;

		property Dialog: TKCustomInputListDialog
						 read GetDialog;

	end;

{ TKCustomInputListDialogForm }

function TKCustomInputListDialogForm.GetDialog: TKCustomInputListDialog;
begin
	Result := ( inherited Dialog as TKCustomInputListDialog );
end;

function TKCustomInputListDialogForm.GetLabelRect: TRect;
begin
	Result := ED_RECT;
	Result.Top := LB_RECT.Top;
	Result.Bottom := LB_RECT.Bottom;
end;

procedure TKCustomInputListDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	Width := ScaleX( ED_FMWIDTH );
	FCombo := AddControl( 'cbList', ED_RECT, true, true, Self, TComboBox ) as TComboBox;
	with FCombo do
	begin
		Items := Dialog.Items;
		if ( not Dialog.UserInput ) then
			Style := csDropDownList;
		if ( Items.Count > 0 ) then
		begin
			if ( Dialog.ItemIndex < Items.Count ) then
				ItemIndex := Dialog.ItemIndex
			else
				ItemIndex := 0;
		end
		else if Dialog.UserInput then
			Text := Dialog.EditText;
		Width := Self.ClientWidth - 2 * Left;
		Top := TextLabel.Top + TextLabel.Height + ScaleY( ED_GUTTER );
		Self.Height := Top + Height + ScaleY( FM_GUTTER + ED_GUTTER );
	end;
	ActiveControl := FCombo;
end;

procedure TKCustomInputListDialogForm.UnprepareForm;
begin
	inherited UnprepareForm;
	if ( ModalResult = mrOK ) then
	begin
		Dialog.EditText := FCombo.Text;
		Dialog.ItemIndex := FCombo.ItemIndex;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKCustomInputListDialog }

constructor TKCustomInputListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	BitmapOption := boCustom;
	DialogStyle := dsOkCancel;
	FormClass := TKCustomInputListDialogForm;
	FEditText := '';
	FItemIndex := -1;
	FUserInput := false;
	FItems := TStringList.Create;
end;

destructor TKCustomInputListDialog.Destroy;
begin
  FItems.Free;
	inherited Destroy;
end;

procedure TKCustomInputListDialog.SetItems( Value: TStrings );
begin
	FItems.Assign( Value );
end;

function InternalInputListDialog( const Title, Prompt: string;
	DefaultIndex: Integer; List: TStrings ): Integer;
var
	d: TKInputListDialog;
begin
	d := TKInputListDialog.Create( nil );
	try
		d.Text := Prompt;
		d.Items := List;
		d.Caption := Title;
		d.RightAlign := true;
    d.UserInput := false;
		d.ItemIndex := DefaultIndex;
		Result := -1;
		RefreshDialogData( d );
		if d.Execute then
			Result := d.ItemIndex;
	finally
		d.Free;
	end;
end;

function InternalInputListDialogEx( const Title, Prompt: string; DefaultIndex: Integer;
	AcceptUserInput: Boolean; List: TStrings ): string;
var
	d: TKInputListDialog;
begin
	d := TKInputListDialog.Create( nil );
	try
		d.Text := Prompt;
		d.Items := List;
		d.Caption := Title;
		d.RightAlign := true;
		d.ItemIndex := DefaultIndex;
		d.UserInput := AcceptUserInput;
		Result := '';
		RefreshDialogData( d );
		if d.Execute then
			Result := d.EditText;
	finally
		d.Free;
	end;
end;

{--------------------------- TKInputCheckListDialog ----------------------------}

{--------------------------- Internal Implementation ---------------------------}

{ TKInputCheckListDialogForm }

const

	FM_CBX_WIDTH = 252;
	FM_CBX_HEIGHT = 302;

	CBX_GUTTER = 8;
	CBX_TOP = 5;
	CBX_LEFT = 5;
	CBX_HEIGHT = 230;
	CBX_WIDTH = 240;

	CBX_ITEM_HEIGHT = 20;

	CBX_RECT: TRect =
	(
		Left:   CBX_LEFT;
		Top:    CBX_TOP;
		Right:  CBX_WIDTH + CBX_LEFT;
		Bottom: CBX_HEIGHT + CBX_TOP;
	);

type

	TKInputCheckListDialogForm = class( TKSystemDialogForm )
	private
		FCbLst: TCheckListBox;
    FPopUpMenu: TPopUpMenu;

		procedure SetItems;
		procedure GetSelection;
		procedure SetSelection;
		function GetDialog: TKInputCheckListDialog;

	protected
		procedure PlaceLabel; override;
		procedure PrepareForm; override;
		procedure PlaceButtons; override;
		procedure PlaceControls; override;
		procedure UnprepareForm; override;

{		procedure CbLstDblClick( Sender: TObject ); dynamic; }
  	procedure CheckUnCheckAllClick( Sender: TObject ); dynamic;

	public
		property CbLst: TCheckListBox
						 read FCbLst;
    property PopUpMenu: TPopUpMenu
             read FPopUpMenu;
		property Dialog: TKInputCheckListDialog
						 read GetDialog;

	end;

function TKInputCheckListDialogForm.GetDialog: TKInputCheckListDialog;
begin
	Result := TKInputCheckListDialog( inherited Dialog );
end;

procedure TKInputCheckListDialogForm.SetItems;
var
	i,
	iMax: Integer;
begin
	FCbLst.Items.Assign( Dialog.Items );
	iMax := 0;
	for i := 0 to FCbLst.Items.Count - 1 do
		if ( ScaleX( Canvas.TextWidth( FCbLst.Items[i] ) ) > iMax ) then
			iMax := ScaleX( Canvas.TextWidth( FCbLst.Items[i] ) );
	if ( iMax > FCbLst.Width ) then
		FCbLst.Width := iMax;
	if ( ( FCbLst.Width + ( 2 * FCbLst.Left ) ) > ClientWidth ) then
		ClientWidth := FCbLst.Width + ( 2 * FCbLst.Left );
{
	Leave this fixed for simplicity... After all can add ItemViewCount for max height

	FCbLst.Height := ( FCbLst.Items.Count + 1 ) * FCbLst.ItemHeight;
}
end;

procedure TKInputCheckListDialogForm.GetSelection;
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		Dialog.Selection.Bits[i] := FCbLst.Checked[i];
end;

procedure TKInputCheckListDialogForm.SetSelection;
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		FCbLst.Checked[i] := Dialog.Selection.Bits[i];
end;

procedure TKInputCheckListDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	TextLabel.Visible := False;
	Height := ScaleY( FM_CBX_HEIGHT );
	Width := ScaleX( FM_CBX_WIDTH );
end;

procedure TKInputCheckListDialogForm.PlaceControls;
var
  mi: TMenuItem;
begin
	inherited PlaceControls;
	FCbLst := ( AddControl( 'CbLst', CBX_RECT, True, True, Self, TCheckListBox ) as TCheckListBox );
  FPopUpMenu := TPopUpMenu.Create( Self );
  with FPopUpMenu do
  begin
    Name := 'PopUp';
    AutoPopup := True;
    mi := TMenuItem.Create( FPopUpMenu );
    mi.Caption := sCheckListUnCheckAll;
    mi.Tag := -2;
    mi.OnClick := CheckUnCheckAllClick;
    Items.Add( mi );
    mi := TMenuItem.Create( FPopUpMenu );
    mi.Caption := sCheckListCheckAll; 
    mi.Tag := -1;
    mi.OnClick := CheckUnCheckAllClick;
    Items.Add( mi );
  end;
	with FCbLst do
	begin
		Width := ( Self.ClientWidth - ( 2 * Left ) );
		Color := Dialog.BackGroundColor;
		Sorted := Dialog.Sorted;
		Style := lbOwnerDrawFixed;
		IntegralHeight := True;
		ItemHeight := ScaleY( CBX_ITEM_HEIGHT );
		TabOrder := 0;
    PopUpMenu := FPopUpMenu;
		SetItems;
		{ OnDblClick := CbLstDblClick; }
		if Dialog.FIsToSetSelection then { Hard Couple! }
			SetSelection;
	end;
	ActiveControl := FCbLst;
end;

procedure TKInputCheckListDialogForm.PlaceLabel;
begin
	{ do nothing does not need a label }
end;

procedure TKInputCheckListDialogForm.PlaceButtons;
var
	i: Integer;
	bn: TControl;
begin
	inherited PlaceButtons;
	bn := nil;
	for i := 0 to ControlCount - 1 do
		if CheckObjectClass( Controls[i], TBitBtn ) then
		begin
			Controls[i].Top := FCbLst.Top + FCbLst.Height + ScaleY( CBX_GUTTER );
			bn := Controls[i];
		end;
	ForceObject( bn );
	ClientHeight := bn.Top + bn.Height + ScaleY( CBX_GUTTER );
end;

procedure TKInputCheckListDialogForm.UnprepareForm;
begin
	if ( ModalResult = mrOk ) then
		GetSelection;
	inherited UnprepareForm;
end;

{
procedure TKInputCheckListDialogForm.CbLstDblClick( Sender: TObject );
var
	i: Integer;
begin
	for i := 0 to FCbLst.Items.Count - 1 do
		FCbLst.Checked[i] := ( not FCbLst.Checked[i] );
end;
}

procedure TKInputCheckListDialogForm.CheckUnCheckAllClick( Sender: TObject );
var
	i: Integer;
begin
  with ( Sender as TMenuItem ) do
  	for i := 0 to FCbLst.Items.Count - 1 do
      case Tag of
        -2: FCbLst.Checked[i] := False;
        -1: FCbLst.Checked[i] := True;
      end;
end;

{---------------------------- Public Implementation ----------------------------}

constructor TKInputCheckListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	BitmapOption := boCustom;
	DialogStyle := dsOkCancel;
	FormClass := TKInputCheckListDialogForm;
	FItems := TStringList.Create;
	FSelection := TKBitsWrapper.Create( Self );
	FSorted := False;
	FBackGroundColor := clBtnFace;
end;

destructor TKInputCheckListDialog.Destroy;
begin
	FItems.Free;
	FSelection.Free;
	inherited Destroy;
end;

function TKInputCheckListDialog.DoCheckParams: Boolean;
begin
	Result := ( CheckStrings( FItems ) and ( inherited DoCheckParams ) );
end;

function InputCheckListDialog( const Caption: string; BackColor: TColor; Sorted: Boolean;
	Source: TStrings; ResultList: TBits ): Boolean;
var
	i: Integer;
	dlgCbx: TKInputCheckListDialog;
begin
	ForceStrings( Source );
	dlgCbx := TKInputCheckListDialog.Create( nil );
	try
		dlgCbx.Caption := Caption;
		dlgCbx.Sorted := Sorted;
		dlgCbx.FIsToSetSelection := CheckObject( ResultList ); { Hard Couple! }
		dlgCbx.BackGroundColor := BackColor;
		dlgCbx.Items.Assign( Source );
		dlgCbx.Selection.Bits.Assign( ResultList );
		RefreshDialogData( dlgCbx );
		Result := dlgCbx.Execute;
		if ( Result and dlgCbx.FIsToSetSelection ) then
		begin
			ResultList.Size := dlgCbx.Items.Count;
			for i := 0 to ResultList.Size - 1 do
				ResultList.Bits[i] := dlgCbx.Selection.Bits[i];
		end;
	finally
		dlgCbx.Free;
	end;
end;

{--------------------------- Internal Implementation ---------------------------}

const
	KM_CLOSEMODAL = KM_USER + 01;

type

	TKProgressDialogForm = class;

{ TKProgressThread }

	TKProgressThread = class( TThread )
	private
		FForm: TKProgressDialogForm;

	protected
		procedure Execute; override;
		procedure UserExecute; virtual;
		function StartCondition: Boolean; virtual;
		function ExecuteCondition: Boolean; virtual;

	public
		destructor Destroy; override;
		constructor Create( Form: TKProgressDialogForm );

	end;

{ TKProgressDialogForm }

	TKProgressDialogForm = class( TKSystemDialogForm )
	private
		FStatus: TLabel;
		FEvent: THandle;
		FProgress: TProgressBar;
		FFormTerminated: Boolean;
		FThread: TKProgressThread;
		FProgressPosition: TKPercent;

		procedure KMCloseModal( var Message: TMessage );
							message KM_CLOSEMODAL;

		procedure BtnCloseEvent( Sender: TObject );
		procedure CloseQueryEvent( Sender: TObject; var CanClose: Boolean );
		procedure FormClose( Sender: TObject; var CloseAction: TCloseAction );

		function GetDialog: TKProgressDialog;

		procedure PrepareThread;
		procedure FormTerminated;

	protected
		procedure Progress;

		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		function GetLabelRect: TRect; override;

		property Dialog: TKProgressDialog
						 read GetDialog;
		property ProgressPosition: TKPercent
						 read FProgressPosition;

	end;

{ TKProgressThread }

constructor TKProgressThread.Create( Form: TKProgressDialogForm );
begin
	FForm := Form;
	FreeOnTerminate := True;
	inherited Create( False );
end;

destructor TKProgressThread.Destroy;
begin
	inherited Destroy;
	SetEvent( FForm.FEvent );
end;

function TKProgressThread.StartCondition: Boolean;
begin
	Result := ( CheckObject( FForm ) and CheckObject( FForm.Dialog ) and FForm.Visible and FForm.Showing );
end;

function TKProgressThread.ExecuteCondition: Boolean;
begin
	Result := ( StartCondition and ( not FForm.FFormTerminated ) and
		( ( FForm.ProgressPosition < DIALOG_PROGRESS_TERMINATED ) or
		( not FForm.Dialog.UserCanceled ) ) and ( FForm.ModalResult = mrNone ) );
end;

procedure TKProgressThread.Execute;
begin
	while ( not StartCondition ) do ; { Cool!!! Busy Wait }
	while ExecuteCondition do
		Synchronize( UserExecute );
end;

procedure TKProgressThread.UserExecute;
begin
	if ExecuteCondition then
		FForm.Progress;
end;

{ TKProgressDialogForm }

function TKProgressDialogForm.GetDialog: TKProgressDialog;
begin
	Result := ( inherited Dialog as TKProgressDialog );
end;

function TKProgressDialogForm.GetLabelRect: TRect;
begin
	Result := ED_RECT;
	Result.Top := LB_RECT.Top;
	Result.Bottom := LB_RECT.Bottom;
end;

procedure TKProgressDialogForm.FormTerminated;
begin
	FFormTerminated := True;
end;

procedure TKProgressDialogForm.Progress;
const
	STATUS_STYLE: array[TKProgressInfoStyle] of DWORD =
	(
		DT_CALCRECT or DT_MODIFYSTRING or DT_EXPANDTABS,
		DT_CALCRECT or DT_MODIFYSTRING or DT_EXPANDTABS or DT_PATH_ELLIPSIS,
		DT_CALCRECT or DT_MODIFYSTRING or DT_EXPANDTABS or DT_END_ELLIPSIS
	);
var
	rt: TRect;
	sStatus: string;
begin
	sStatus := FStatus.Caption;
	FProgressPosition := DIALOG_PROGRESS_DISABLED;
	if ( CheckObject( FProgress ) and FProgress.Visible ) then
		FProgressPosition := FProgress.Position;
	Dialog.Progress;
	Dialog.VisualProgress( sStatus, FProgressPosition );
	if Dialog.UserCanceled then
	begin
		PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
		Exit;
	end
	else if ( FProgressPosition >= DIALOG_PROGRESS_TERMINATED ) then
	begin
		PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrOK ), 0 );
		Exit;
	end;
	if ( not CheckStrEqual( FStatus.Caption, sStatus ) ) then
	begin
		rt := FStatus.BoundsRect;
		DrawTextEx( Canvas.Handle, PChar( sStatus ), -1, rt, STATUS_STYLE[Dialog.InfoStyle], nil );
		FStatus.Caption := sStatus;
	end;
	if ( CheckObject( FProgress ) and FProgress.Visible and
		 ( FProgressPosition > FProgress.Position ) ) then
		FProgress.Position := FProgressPosition;
end;

procedure TKProgressDialogForm.PrepareThread;
begin
	FFormTerminated := False;
	FEvent := CreateEvent( nil, false, false, '' );
	FThread := TKProgressThread.Create( Self );
end;

procedure TKProgressDialogForm.PlaceControls;
var
	iTop: Integer;
begin
	inherited PlaceControls;
	Width := ScaleX( ED_FMWIDTH );
	FProgress := nil;
	Dialog.FUserCanceled := false; { Hard Couple! }
	iTop := TextLabel.Top + TextLabel.Height;
	if Dialog.ProgressVisible then
	begin
		FProgress := ( AddControl( 'pbProgress', ED_RECT, True, True, Self, TProgressBar ) as TProgressBar );
		with FProgress do
		begin
			Width := Self.ClientWidth - 2 * Left;
			Top := iTop + ScaleY( ED_GUTTER );
			iTop := Top + Height;
			Self.Height := Top + Height + ScaleY( ED_GUTTER );
		end;
	end;
	iTop := iTop + ScaleY( ED_GUTTER );
	FStatus := ( AddControl( 'lbProgressStatus', ED_RECT, True, True, Self, TLabel ) as TLabel );
	with FStatus do
	begin
		Caption := '';
		AutoSize := false;
		Transparent := true;
		Top := iTop;
		Alignment := Dialog.StatusAlignment;
		Height := ScaleY( LB_HEIGHT );
		Width := Self.ClientWidth - 2 * Left;
		Self.Height := Top + Height + ScaleY( FM_GUTTER + ED_GUTTER );
	end;
	OnClose := FormClose;
	Dialog.FormPainter.OnCloseQuery := CloseQueryEvent;
	PrepareThread;
end;

procedure TKProgressDialogForm.KMCloseModal( var Message: TMessage );
begin
	Dialog.FormPainter.OnCloseQuery := nil;
	ModalResult := TModalResult( Message.WParam );
end;

procedure TKProgressDialogForm.CloseQueryEvent( Sender: TObject; var CanClose: Boolean );
begin
{ This will be invoked only for the Formpainter's close button! }
	CanClose := false;
	PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
end;

procedure TKProgressDialogForm.BtnCloseEvent( Sender: TObject );
begin
	PostMessage( Handle, KM_CLOSEMODAL, LongInt( mrCancel ), 0 );
end;

procedure TKProgressDialogForm.FormClose( Sender: TObject; var CloseAction: TCloseAction );
begin
	FormTerminated;
	WaitForSingleObject( FEvent, INFINITE );
	CloseHandle( FEvent );
	FEvent := NULL_HANDLE_VALUE;
end;

procedure TKProgressDialogForm.PlaceButtons;
var
	bn: TBitBtn;
begin
	inherited PlaceButtons;
	bn := TBitBtn( FindComponent( 'bnCancel' ) );
	if CheckObject( bn ) then
	begin
		bn.ModalResult := mrNone;
		bn.OnClick := BtnCloseEvent;
		bn.Enabled := Dialog.CancelEnabled;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKProgressDialog }

constructor TKProgressDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	CloseOnAltF4 := False;
	CloseOnEscape := False;
	DialogStyle := dsCancel;
	BitmapOption := boCustom;
	FormClass := TKProgressDialogForm;
	FData := nil;
	FCallback := nil;
	FExecutor := nil;
	FCancelEnabled := true;
	FInfoStyle := pisEllipsis;
	FProgressVisible := true;
	FStatusAlignment := taLeftJustify;
end;

function TKProgressDialog.DoCheckParams: Boolean;
begin
	Result := ( CheckPointer( @FCallback ) or Assigned( FOnVisualProgress ) ) and
		( inherited DoCheckParams );
end;

procedure TKProgressDialog.VisualProgress( var Status: string; var Percent: TKPercent );
begin
	if CheckPointer( @FCallback ) then
		FCallback( Data, Status, Percent, FUserCanceled, FInfoStyle )
	else if Assigned( FOnVisualProgress ) then
		FOnVisualProgress( Self, Status, Percent, FUserCanceled, FInfoStyle );
end;

procedure TKProgressDialog.Progress;
begin
	if CheckPointer( @FExecutor ) then
		FExecutor( Data )
	else if Assigned( FOnProgress ) then
		FOnProgress( Self );
end;

function InternalProgressDialog( const Title, Message: string;
	StatusAlignment: TAlignment; AllowCancel, ProgressVisible: Boolean;
	Callback: TKProgressDialogCallBack; ExecutorProc: TKDialogExecuteProc;
	Data: Pointer ): Boolean;
var
	d: TKProgressDialog;
begin
	ForceReference( @Callback );
	d := TKProgressDialog.Create( nil );
	try
		d.Data := Data;
		d.Callback := Callback;
		d.Executor := ExecutorProc;
		d.Text := Message;
		d.Caption := Title;
		d.CancelEnabled := AllowCancel;
		d.StatusAlignment := StatusAlignment;
		d.ProgressVisible := ProgressVisible;
		RefreshDialogData( d );
		Result := ( d.InternalExecute = mrOK ) and ( not d.UserCanceled );
	finally
		d.Free;
	end;
end;

procedure ResetDialogDataEx;
begin
	with DialogDataEx do
	begin
		with ActiveFont do
		begin
			Name := DEFAULT_DIALOG_FONT_NAME;
			Size := DEFAULT_DIALOG_FONT_SIZE;
			Style := [fsBold];
			Color := DIALOG_DEFAULT_ACTIVE_FONT_COLOR;
		end;
		with InActiveFont do
		begin
			Name := DEFAULT_DIALOG_FONT_NAME;
			Size := DEFAULT_DIALOG_FONT_SIZE;
			Style := [fsBold];
			Color := DIALOG_DEFAULT_INACTIVE_FONT_COLOR;
		end;
		with ActiveCaption do
		begin
			BeginColor := clNavy;
			EndColor := clDeepBlue;
			GradientStyle := DEFAULT_DIALOG_GRADIENT_STYLE;
			Steps := DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj := nil;
			BitmapFile := nil;
		end;
		with InactiveCaption do
		begin
			BeginColor := clGray;
			EndColor := clSilver;
			GradientStyle := DEFAULT_DIALOG_GRADIENT_STYLE;
			Steps := DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj := nil;
			BitmapFile := nil;
		end;
		with BackGround do
		begin
			BeginColor := clBtnFace;
			EndColor := clBtnFace;
			GradientStyle := DEFAULT_DIALOG_GRADIENT_STYLE;
			Steps := DEFAULT_DIALOG_GRADIENT_STEPS;
			BitmapObj := nil;
			BitmapFile := nil;
		end;
	end;
end;

procedure RefreshDialogData( d: TKCustomDialog );
begin
{ Custom Dialog Data Values }
	d.PosTop := DialogData.Top;
	d.PosLeft := DialogData.Left;
	d.Centered := DialogData.Centered;
	d.SmallCaption := DialogData.ToolWindow;

	AssignFontFromFontData( d.Font, DialogData.Font );

{ System Dialog Data Values }
	if CheckObjectClass( d, TKSystemDialog ) then
		( d as TKSystemDialog ).RightAlign := DialogData.RightAlign;

{ Dialogs Dialog Data Extendend Values }
	AssignFontFromFontData( d.ActiveFont, DialogDataEx.ActiveFont );
	AssignFontFromFontData( d.InactiveFont, DialogDataEx.InactiveFont );

	d.BackGround.AssignFromGradData( DialogDataEx.BackGround );
	d.ActiveCaption.AssignFromGradData( DialogDataEx.ActiveCaption );
	d.InactiveCaption.AssignFromGradData( DialogDataEx.InactiveCaption );
end;

{
--------------------------------------------------------------------------------
-------------------------- Speed Dialog Implementation -------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

{ TKCustomSpeedDialogForm }

	TKCustomSpeedDialogForm = class( TKSystemDialogForm )
	private
		FSpeedControl: TKCustomSpeedControl;

		function GetDialog: TKCustomSpeedDialog;

	protected
		function GetLabelRect: TRect; override;
		procedure PlaceControls; override;
		procedure UnprepareForm; override;

		procedure SetSpeedControlDefaults; virtual; abstract;
		procedure SetDialogResult; virtual; abstract;

		property Dialog: TKCustomSpeedDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedControl
						 read FSpeedControl;

	end;

{ TKCustomUpDownDialogForm }

	TKCustomUpDownDialogForm = class( TKCustomSpeedDialogForm )
	private
		function GetDialog: TKCustomUpDownDialog;

	protected
		procedure SetSpeedControlDefaults; override;

		property Dialog: TKCustomUpDownDialog
						 read GetDialog;

	end;

{ TKSpeedFloatDialogForm }

	TKSpeedFloatDialogForm = class( TKCustomUpDownDialogForm )
	private
		function GetDialog: TKSpeedFloatDialog;
		function GetSpeedControl: TKFormattedSpeedControl;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKSpeedFloatDialog
						 read GetDialog;
		property SpeedControl: TKFormattedSpeedControl
						 read GetSpeedControl;

	end;

{ TKSpeedIntegerDialogForm }

	TKSpeedIntegerDialogForm = class( TKCustomUpDownDialogForm )
	private
		function GetDialog: TKCustomSpeedIntegerDialog;
		function GetSpeedControl: TKCustomSpeedInteger;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKCustomSpeedIntegerDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedInteger
						 read GetSpeedControl;

	end;

{ TKSpeedHexaDialogForm }

	TKSpeedHexaDialogForm = class( TKSpeedIntegerDialogForm )
	private
		function GetDialog: TKSpeedHexaDialog;
		function GetSpeedControl: TKCustomSpeedHexa;

	protected
		procedure SetSpeedControlDefaults; override;

		property Dialog: TKSpeedHexaDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedHexa
						 read GetSpeedControl;

	end;

{ TKSpeedDateDialogForm }

	TKSpeedDateDialogForm = class( TKCustomUpDownDialogForm )
	private
		function GetDialog: TKSpeedDateDialog;
		function GetSpeedControl: TKCustomSpeedDate;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKSpeedDateDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedDate
						 read GetSpeedControl;

	end;

{ TKSpeedTimeDialogForm }

	TKSpeedTimeDialogForm = class( TKCustomUpDownDialogForm )
	private
		function GetDialog: TKSpeedTimeDialog;
		function GetSpeedControl: TKCustomSpeedTime;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKSpeedTimeDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedTime
						 read GetSpeedControl;

	end;

{ TKSpeedDateTimeDialogForm }

	TKSpeedDateTimeDialogForm = class( TKCustomUpDownDialogForm )
	private
		function GetDialog: TKSpeedDateTimeDialog;
		function GetSpeedControl: TKCustomSpeedDateTime;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKSpeedDateTimeDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedDateTime
						 read GetSpeedControl;

	end;

{ TKSpeedTextDialogForm }

	TKSpeedTextDialogForm = class( TKCustomSpeedDialogForm )
	private
		function GetDialog: TKSpeedTextDialog;
		function GetSpeedControl: TKCustomSpeedText;

	protected
		procedure SetSpeedControlDefaults; override;
		procedure SetDialogResult; override;

		property Dialog: TKSpeedTextDialog
						 read GetDialog;
		property SpeedControl: TKCustomSpeedText
						 read GetSpeedControl;

	end;

{ Hacking Access Classes }

  TKCustomSpeedControlHack = class( TKCustomSpeedControl );
	TKCustomSpeedFloatHack = class( TKFormattedSpeedControl );
	TKCustomSpeedIntegerHack = class( TKCustomSpeedInteger );
	TKCustomSpeedHexaHack = class( TKCustomSpeedHexa );
	TKCustomSpeedDateHack = class( TKCustomSpeedDate );
	TKCustomSpeedTimeHack = class( TKCustomSpeedTime );
	TKCustomSpeedDateTimeHack = class( TKCustomSpeedDateTime );
	TKCustomSpeedTextHack = class( TKCustomSpeedText );

{ TKCustomSpeedDialogForm }

function TKCustomSpeedDialogForm.GetDialog: TKCustomSpeedDialog;
begin
	Result := ( inherited Dialog as TKCustomSpeedDialog );
end;

function TKCustomSpeedDialogForm.GetLabelRect: TRect;
begin
	Result := ED_RECT;
	Result.Top := LB_RECT.Top;
	Result.Bottom := LB_RECT.Bottom;
end;

procedure TKCustomSpeedDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	Width := ScaleX( ED_FMWIDTH );
	FSpeedControl := ( AddControl( 'spdControl', ED_RECT, true, true, Self,
		Dialog.GetSpeedControlClass ) as Dialog.GetSpeedControlClass );
	with FSpeedControl do
	begin
		Width := Self.ClientWidth - 2 * Left;
		Top := TextLabel.Top + TextLabel.Height + ScaleY( ED_GUTTER );
		Self.Height := Top + Height + ScaleY( FM_GUTTER + ED_GUTTER );
		SetSpeedControlDefaults;
	end;
	ActiveControl := FSpeedControl;
end;

procedure TKCustomSpeedDialogForm.UnprepareForm;
begin
	inherited UnprepareForm;
	SetDialogResult;
end;

{ TKCustomUpDownDialogForm }

const
	EDITOR_STYLE: array[Boolean] of TKEditorStyle = ( esUpDown, esRXUpDown );

function TKCustomUpDownDialogForm.GetDialog: TKCustomUpDownDialog;
begin
	Result := ( inherited Dialog as TKCustomUpDownDialog );
end;

procedure TKCustomUpDownDialogForm.SetSpeedControlDefaults;
begin
	TKCustomSpeedControlHack( SpeedControl ).EditorStyle := EDITOR_STYLE[Dialog.RxUpDown];
end;

{ TKSpeedFloatDialogForm }

function TKSpeedFloatDialogForm.GetDialog: TKSpeedFloatDialog;
begin
	Result := ( inherited Dialog as TKSpeedFloatDialog );
end;

function TKSpeedFloatDialogForm.GetSpeedControl: TKFormattedSpeedControl;
begin
	Result := ( inherited SpeedControl as TKFormattedSpeedControl );
end;

procedure TKSpeedFloatDialogForm.SetSpeedControlDefaults;
begin
  inherited SetSpeedControlDefaults;
	with TKCustomSpeedFloatHack( SpeedControl ) do
	begin
		AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;
		
		DisplayFormat := Dialog.SpeedProperties.DisplayFormat;
		EditFormat := Dialog.SpeedProperties.EditFormat;
		Increment := Dialog.SpeedProperties.Increment;
		MaxValue := Dialog.SpeedProperties.MaxValue;
		MinValue := Dialog.SpeedProperties.MinValue;
		Value := Dialog.SpeedProperties.Value;

		OnFormatEditText := Dialog.OnFormatEditText;
		OnFormatDisplayText := Dialog.OnFormatDisplayText;

	end;
end;

procedure TKSpeedFloatDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		Dialog.SpeedProperties.Value := TKCustomSpeedFloatHack( SpeedControl ).Value;
end;

{ TKSpeedIntegerDialogForm }

function TKSpeedIntegerDialogForm.GetDialog: TKCustomSpeedIntegerDialog;
begin
	Result := ( inherited Dialog as TKCustomSpeedIntegerDialog );
end;

function TKSpeedIntegerDialogForm.GetSpeedControl: TKCustomSpeedInteger;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedInteger );
end;

procedure TKSpeedIntegerDialogForm.SetSpeedControlDefaults;
begin
	inherited SetSpeedControlDefaults;
	with TKCustomSpeedIntegerHack( SpeedControl ), TKSpeedIntegerDialog( Dialog ) do
	begin
		AllowNull := SpeedProperties.AllowNull;
		Alignment := SpeedProperties.Alignment;
		EditorEnabled := SpeedProperties.EditorEnabled;

		DisplayFormat := SpeedProperties.DisplayFormat;
		EditFormat := SpeedProperties.EditFormat;
		Increment := SpeedProperties.Increment;
		MaxValue := SpeedProperties.MaxValue;
		MinValue := SpeedProperties.MinValue;
		Value := SpeedProperties.Value;
	end;
	with TKCustomSpeedIntegerHack( SpeedControl ) do
	begin
		OnFormatEditText := Dialog.OnFormatEditText;
		OnFormatDisplayText := Dialog.OnFormatDisplayText;
	end;
end;

procedure TKSpeedIntegerDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		TKSpeedIntegerDialog( Dialog ).SpeedProperties.Value :=
		  TKCustomSpeedIntegerHack( SpeedControl ).Value;
end;

{ TKSpeedHexaDialogForm }

function TKSpeedHexaDialogForm.GetDialog: TKSpeedHexaDialog;
begin
	Result := ( inherited Dialog as TKSpeedHexaDialog );
end;

function TKSpeedHexaDialogForm.GetSpeedControl: TKCustomSpeedHexa;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedHexa );
end;

procedure TKSpeedHexaDialogForm.SetSpeedControlDefaults;
begin
{ Do not call the inherited method }
	with TKCustomSpeedHexaHack( SpeedControl ) do
	begin
{ From TKCustomSpeedDialog }
		AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;

{ From TKCustomUpDownSpeedDialog }
		EditorStyle := EDITOR_STYLE[Dialog.RxUpDown];

{ From TKCustomSpeedIntegerDialog }
		Increment := Dialog.SpeedProperties.Increment;
		MaxValue := Dialog.SpeedProperties.MaxValue;
		MinValue := Dialog.SpeedProperties.MinValue;
		Value := Dialog.SpeedProperties.Value;

		Digits := Dialog.SpeedProperties.Digits;
		DisplayPrefix := Dialog.SpeedProperties.DisplayPrefix;
	end;
end;

{ TKSpeedDateDialogForm }

function TKSpeedDateDialogForm.GetDialog: TKSpeedDateDialog;
begin
	Result := ( inherited Dialog as TKSpeedDateDialog );
end;

function TKSpeedDateDialogForm.GetSpeedControl: TKCustomSpeedDate;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedDate );
end;

procedure TKSpeedDateDialogForm.SetSpeedControlDefaults;
begin
	inherited SetSpeedControlDefaults;
	with TKCustomSpeedDateHack( SpeedControl ) do
	begin
		AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;

		DateIncrement := Dialog.SpeedProperties.DateIncrement;
		MaxValue := Dialog.SpeedProperties.MaxValue;
		MinValue := Dialog.SpeedProperties.MinValue;
		Value := Dialog.SpeedProperties.Value;

		OnFormatEditText := Dialog.OnFormatEditText;
		OnFormatDisplayText := Dialog.OnFormatDisplayText;
	end;
end;

procedure TKSpeedDateDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		Dialog.SpeedProperties.Value := TKCustomSpeedDateHack( SpeedControl ).Value;
end;

{ TKSpeedTimeDialogForm }

function TKSpeedTimeDialogForm.GetDialog: TKSpeedTimeDialog;
begin
	Result := ( inherited Dialog as TKSpeedTimeDialog );
end;

function TKSpeedTimeDialogForm.GetSpeedControl: TKCustomSpeedTime;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedTime );
end;

procedure TKSpeedTimeDialogForm.SetSpeedControlDefaults;
begin
	inherited SetSpeedControlDefaults;
	with TKCustomSpeedTimeHack( SpeedControl ) do
	begin
		AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;

		TimeIncrement := Dialog.SpeedProperties.TimeIncrement;
		MaxValue := Dialog.SpeedProperties.MaxValue;
		MinValue := Dialog.SpeedProperties.MinValue;
		Value := Dialog.SpeedProperties.Value;

		OnFormatEditText := Dialog.OnFormatEditText;
		OnFormatDisplayText := Dialog.OnFormatDisplayText;
	end;
end;

procedure TKSpeedTimeDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		Dialog.SpeedProperties.Value := TKCustomSpeedTimeHack( SpeedControl ).Value;
end;

{ TKSpeedDateTimeDialogForm }

function TKSpeedDateTimeDialogForm.GetDialog: TKSpeedDateTimeDialog;
begin
	Result := ( inherited Dialog as TKSpeedDateTimeDialog );
end;

function TKSpeedDateTimeDialogForm.GetSpeedControl: TKCustomSpeedDateTime;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedDateTime );
end;

procedure TKSpeedDateTimeDialogForm.SetSpeedControlDefaults;
begin
	inherited SetSpeedControlDefaults;
	with TKCustomSpeedDateTimeHack( SpeedControl ) do
	begin
		AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;

		TimeIncrement := Dialog.SpeedProperties.TimeIncrement;
		DateIncrement := Dialog.SpeedProperties.DateIncrement;

		MaxValue := Dialog.SpeedProperties.MaxValue;
		MinValue := Dialog.SpeedProperties.MinValue;
		Value := Dialog.SpeedProperties.Value;

		OnFormatEditText := Dialog.OnFormatEditText;
		OnFormatDisplayText := Dialog.OnFormatDisplayText;
	end;
end;

procedure TKSpeedDateTimeDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		Dialog.SpeedProperties.Value := TKCustomSpeedDateTimeHack( SpeedControl ).Value;
end;

{ TKSpeedTextDialogForm }

function TKSpeedTextDialogForm.GetDialog: TKSpeedTextDialog;
begin
	Result := ( inherited Dialog as TKSpeedTextDialog );
end;

function TKSpeedTextDialogForm.GetSpeedControl: TKCustomSpeedText;
begin
	Result := ( inherited SpeedControl as TKCustomSpeedText );
end;

procedure TKSpeedTextDialogForm.SetSpeedControlDefaults;
begin
	with TKCustomSpeedTextHack( SpeedControl ) do
	begin
    AllowNull := Dialog.SpeedProperties.AllowNull;
		Alignment := Dialog.SpeedProperties.Alignment;
		EditorEnabled := Dialog.SpeedProperties.EditorEnabled;

		if ( not Dialog.SpeedProperties.ButtonGlyph.Empty ) then
		begin
			EditorStyle := esGlyph;
			ButtonGlyph := Dialog.SpeedProperties.ButtonGlyph;
		end;
		ButtonHint := Dialog.SpeedProperties.ButtonHint;
		Value := Dialog.SpeedProperties.Value;

		OnCheckValue := Dialog.OnCheckValue;
		OnButtonClick := Dialog.OnButtonClick;
	end;
end;

procedure TKSpeedTextDialogForm.SetDialogResult;
begin
	if ( ModalResult = mrOk ) then
		Dialog.SpeedProperties.Value := TKCustomSpeedTextHack( SpeedControl ).Value;
end;

{----------------------- Speed Properties Implementation -----------------------}

{ TKCustomSpeedProperties }

constructor TKCustomSpeedProperties.Create;
begin
	inherited Create;
	FAlignment := taLeftJustify;
	FEditorEnabled := true;
end;

procedure TKCustomSpeedProperties.Assign( Source: TPersistent );
begin
	if CheckObjectClass( Source, TKCustomSpeedProperties ) then
	begin
		AllowNull := ( Source as TKCustomSpeedProperties ).AllowNull;
		Alignment := ( Source as TKCustomSpeedProperties ).Alignment;
		EditorEnabled := ( Source as TKCustomSpeedProperties ).EditorEnabled;
		EditFormat := ( Source as TKCustomSpeedProperties ).EditFormat;
		DisplayFormat := ( Source as TKCustomSpeedProperties ).DisplayFormat;
	end
	else
		inherited Assign( Source );
end;

{ TKFloatSpeedProperties }

constructor TKFloatSpeedProperties.Create;
begin
	inherited Create;
	FIncrement := 1;
	EditFormat := DEFAULT_FLOAT_FORMAT;
	DisplayFormat := DEFAULT_FLOAT_FORMAT;
end;

procedure TKFloatSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKFloatSpeedProperties ) then
	begin
		Value := ( Source as TKFloatSpeedProperties ).Value;
		MinValue := ( Source as TKFloatSpeedProperties ).MinValue;
		MaxValue := ( Source as TKFloatSpeedProperties ).MaxValue;
		Increment := ( Source as TKFloatSpeedProperties ).Increment;
	end;
end;

procedure TKFloatSpeedProperties.SetMaxValue( NewValue: Extended );
begin
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
		FMaxValue := NewValue;
end;

procedure TKFloatSpeedProperties.SetMinValue( NewValue: Extended );
begin
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) then
		FMinValue := NewValue;
end;

{ TKCustomIntegerSpeedProperties }

constructor TKCustomIntegerSpeedProperties.Create;
begin
	inherited Create;
	FIncrement := 1;
	EditFormat := '%d';
	DisplayFormat := '%d';
end;

procedure TKCustomIntegerSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKCustomIntegerSpeedProperties ) then
	begin
		Value := ( Source as TKCustomIntegerSpeedProperties ).Value;
		MinValue := ( Source as TKCustomIntegerSpeedProperties ).MinValue;
		MaxValue := ( Source as TKCustomIntegerSpeedProperties ).MaxValue;
		Increment := ( Source as TKCustomIntegerSpeedProperties ).Increment;
	end;
end;

procedure TKCustomIntegerSpeedProperties.SetMaxValue( NewValue: Integer );
begin
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
		FMaxValue := NewValue;
end;

procedure TKCustomIntegerSpeedProperties.SetMinValue( NewValue: Integer );
begin
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) then
		FMinValue := NewValue;
end;

{ TKHexaSpeedProperties }

constructor TKHexaSpeedProperties.Create;
begin
	inherited Create;
	FDigits := 8;
	FDisplayPrefix := true;
	EditFormat := '%.*x';
	DisplayFormat := '$%.*x';
end;

procedure TKHexaSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKHexaSpeedProperties ) then
	begin
		EditFormat := '%.*x';
		DisplayFormat := '$%.*x';
		Digits := ( Source as TKHexaSpeedProperties ).Digits;
		DisplayPrefix := ( Source as TKHexaSpeedProperties ).DisplayPrefix;
	end;
end;

procedure TKHexaSpeedProperties.SetDisplayPrefix( NewValue: Boolean );
begin
	if ( FDisplayPrefix <> NewValue ) then
	begin
		FDisplayPrefix := NewValue;
		if NewValue then
			DisplayFormat := '$%.*x'
		else
			DisplayFormat := '%.*x';
	end;
end;

{ TKDateSpeedProperties }

constructor TKDateSpeedProperties.Create;
begin
	inherited Create;
	FValue := Date;
	FMinValue := Date;
	FMaxValue := Date;
	EditFormat := ShortDateFormat;
	DisplayFormat := ShortDateFormat;
	FDateIncrement := TKDateIncrement.Create( Self );
end;

destructor TKDateSpeedProperties.Destroy;
begin
	FDateIncrement.Free;
	inherited Destroy;
end;

procedure TKDateSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKDateSpeedProperties ) then
	begin
		Value := ( Source as TKDateSpeedProperties ).Value;
		MinValue := ( Source as TKDateSpeedProperties ).MinValue;
		MaxValue := ( Source as TKDateSpeedProperties ).MaxValue;
		DateIncrement := ( Source as TKDateSpeedProperties ).DateIncrement;
	end;
end;

procedure TKDateSpeedProperties.SetDateIncrement( NewValue: TKDateIncrement );
begin
	FDateIncrement.Assign( NewValue );
end;

function TKDateSpeedProperties.GetMaxValue: TDateTime;
begin
	Result := Trunc( FMaxValue );
end;

procedure TKDateSpeedProperties.SetMaxValue( NewValue: TDateTime );
begin
	NewValue := Trunc( NewValue );
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
		FMaxValue := NewValue;
end;

function TKDateSpeedProperties.GetMinValue: TDateTime;
begin
	Result := Trunc( FMinValue );
end;

procedure TKDateSpeedProperties.SetMinValue( NewValue: TDateTime );
begin
	NewValue := Trunc( NewValue );
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) and ( NewValue <= Value ) then
		FMinValue := NewValue;
end;

function TKDateSpeedProperties.GetValue: TDateTime;
begin
	Result := Trunc( FValue );
end;

procedure TKDateSpeedProperties.SetValue( NewValue: TDateTime );
begin
	FValue := Trunc( NewValue );
end;

{ TKTimeSpeedProperties }

constructor TKTimeSpeedProperties.Create;
begin
	inherited Create;
	FValue := Time;
	FMinValue := 0;
	FMaxValue := Frac( EndOfDay( Now ) );
	FTimeIncrement := EncodeTime( 1, 0, 0, 0 );
	EditFormat := ShortTimeFormat;
	DisplayFormat := ShortTimeFormat;
end;

procedure TKTimeSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKTimeSpeedProperties ) then
	begin
		Value := ( Source as TKTimeSpeedProperties ).Value;
		MinValue := ( Source as TKTimeSpeedProperties ).MinValue;
		MaxValue := ( Source as TKTimeSpeedProperties ).MaxValue;
		TimeIncrement := ( Source as TKTimeSpeedProperties ).TimeIncrement;
	end;
end;

function TKTimeSpeedProperties.GetTimeIncrement: TDateTime;
begin
	Result := Frac( FTimeIncrement );
end;

procedure TKTimeSpeedProperties.SetTimeIncrement( NewValue: TDateTime );
begin
	FTimeIncrement := Frac( NewValue );
end;

function TKTimeSpeedProperties.GetMaxValue: TDateTime;
begin
	Result := Frac( FMaxValue );
end;

procedure TKTimeSpeedProperties.SetMaxValue( NewValue: TDateTime );
begin
	NewValue := Frac( NewValue );
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
		FMaxValue := NewValue;
end;

function TKTimeSpeedProperties.GetMinValue: TDateTime;
begin
	Result := Frac( FMinValue );
end;

procedure TKTimeSpeedProperties.SetMinValue( NewValue: TDateTime );
begin
	NewValue := Frac( NewValue );
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) and ( NewValue <= Value ) then
		FMinValue := NewValue;
end;

function TKTimeSpeedProperties.GetValue: TDateTime;
begin
	Result := Frac( FValue );
end;

procedure TKTimeSpeedProperties.SetValue( NewValue: TDateTime );
begin
	FValue := Frac( NewValue );
end;

{ TKDateTimeSpeedProperties }

constructor TKDateTimeSpeedProperties.Create;
begin
	inherited Create;
	FValue := Now;
	FMinValue := 0;
	FMaxValue := EndOfDay( IncDate( Now, 0, 0, 1 ) );
	FTimeIncrement := EncodeTime( 1, 0, 0, 0 );
	EditFormat := ShortTimeFormat;
	DisplayFormat := ShortTimeFormat;
	FDateIncrement := TKDateIncrement.Create( Self );
end;

destructor TKDateTimeSpeedProperties.Destroy;
begin
	FDateIncrement.Free;
	inherited Destroy;
end;

procedure TKDateTimeSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKDateTimeSpeedProperties ) then
	begin
		Value := ( Source as TKDateTimeSpeedProperties ).Value;
		MinValue := ( Source as TKDateTimeSpeedProperties ).MinValue;
		MaxValue := ( Source as TKDateTimeSpeedProperties ).MaxValue;
		TimeIncrement := ( Source as TKDateTimeSpeedProperties ).TimeIncrement;
		DateIncrement := ( Source as TKDateTimeSpeedProperties ).DateIncrement;
	end;
end;

procedure TKDateTimeSpeedProperties.SetDateIncrement( NewValue: TKDateIncrement );
begin
	FDateIncrement.Assign( NewValue );
end;

function TKDateTimeSpeedProperties.GetTimeIncrement: TDateTime;
begin
	Result := Frac( FTimeIncrement );
end;

procedure TKDateTimeSpeedProperties.SetTimeIncrement( NewValue: TDateTime );
begin
	FTimeIncrement := Frac( NewValue );
end;

function TKDateTimeSpeedProperties.GetMaxValue: TDateTime;
begin
	Result := FMaxValue;
end;

procedure TKDateTimeSpeedProperties.SetMaxValue( NewValue: TDateTime );
begin
	if ( FMaxValue <> NewValue ) and ( NewValue >= FMinValue ) then
		FMaxValue := NewValue;
end;

function TKDateTimeSpeedProperties.GetMinValue: TDateTime;
begin
	Result := FMinValue;
end;

procedure TKDateTimeSpeedProperties.SetMinValue( NewValue: TDateTime );
begin
	if ( FMinValue <> NewValue ) and ( NewValue <= FMaxValue ) and ( NewValue <= Value ) then
		FMinValue := NewValue;
end;

{ TKTextSpeedProperties }

constructor TKTextSpeedProperties.Create;
begin
	inherited Create;
	AllowNull := true;
	FButtonGlyph := TBitmap.Create;
end;

destructor TKTextSpeedProperties.Destroy;
begin
	FButtonGlyph.Free;
	inherited Destroy;
end;

procedure TKTextSpeedProperties.Assign( Source: TPersistent );
begin
	inherited Assign( Source );
	if CheckObjectClass( Source, TKTextSpeedProperties ) then
	begin
		Value := ( Source as TKTextSpeedProperties ).Value;
		ButtonHint := ( Source as TKTextSpeedProperties ).ButtonHint;
		ButtonGlyph := ( Source as TKTextSpeedProperties ).ButtonGlyph;
	end;
end;

procedure TKTextSpeedProperties.SetButtonGlyph( Value: TBitmap );
begin
	FButtonGlyph.Assign( Value );
end;

{---------------------------- Dialog Implementation ----------------------------}

{ TKCustomSpeedDialog }

{ TKSpeedFloatDialog }

constructor TKSpeedFloatDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKFloatSpeedProperties.Create;
end;

destructor TKSpeedFloatDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

function TKSpeedFloatDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedFloatDialogForm;
end;

function TKSpeedFloatDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKFormattedSpeedControl;
end;

procedure TKSpeedFloatDialog.SetSpeedProperties( Value: TKFloatSpeedProperties );
begin
  FSpeedProperties.Assign( Value );
end;

{ TKCustomSpeedIntegerDialog }

function TKCustomSpeedIntegerDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedIntegerDialogForm;
end;

function TKCustomSpeedIntegerDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedInteger;
end;

{ TKSpeedIntegerDialog }

constructor TKSpeedIntegerDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKIntegerSpeedProperties.Create;
end;

destructor TKSpeedIntegerDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedIntegerDialog.SetSpeedProperties( Value: TKIntegerSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

{ TKSpeedHexaDialog }

constructor TKSpeedHexaDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKHexaSpeedProperties.Create;
	OnFormatEditText := nil;
	OnFormatDisplayText := nil;
end;

destructor TKSpeedHexaDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedHexaDialog.SetSpeedProperties( Value: TKHexaSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

function TKSpeedHexaDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedHexaDialogForm;
end;

function TKSpeedHexaDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedHexa;
end;

{ TKSpeedDateDialog }

constructor TKSpeedDateDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKDateSpeedProperties.Create;
end;

destructor TKSpeedDateDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedDateDialog.SetSpeedProperties( Value: TKDateSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

function TKSpeedDateDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedDateDialogForm;
end;

function TKSpeedDateDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedDate;
end;

{ TKSpeedTimeDialog }

constructor TKSpeedTimeDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKTimeSpeedProperties.Create;
end;

destructor TKSpeedTimeDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedTimeDialog.SetSpeedProperties( Value: TKTimeSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

function TKSpeedTimeDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedTimeDialogForm;
end;

function TKSpeedTimeDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedTime;
end;

{ TKSpeedDateTimeDialog }

constructor TKSpeedDateTimeDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKDateTimeSpeedProperties.Create;
end;

destructor TKSpeedDateTimeDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedDateTimeDialog.SetSpeedProperties( Value: TKDateTimeSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

function TKSpeedDateTimeDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedDateTimeDialogForm;
end;

function TKSpeedDateTimeDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedDateTime;
end;

{ TKSpeedTextDialog }

constructor TKSpeedTextDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FSpeedProperties := TKTextSpeedProperties.Create;
end;

destructor TKSpeedTextDialog.Destroy;
begin
	FSpeedProperties.Free;
	inherited Destroy;
end;

procedure TKSpeedTextDialog.SetSpeedProperties( Value: TKTextSpeedProperties );
begin
	FSpeedProperties.Assign( Value );
end;

function TKSpeedTextDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKSpeedTextDialogForm;
end;

function TKSpeedTextDialog.GetSpeedControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedText;
end;

{
--------------------------------------------------------------------------------
------------------------ Extended Dialog Implementation ------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TKExtendedDialogForm = class( TKLogoDialogForm )
	private
		FbnOK: TBitBtn;
		FbnCancel: TBitBtn;

	protected
		procedure PrepareForm; override;
		procedure PlaceButtons; override;

		procedure OKClick( Sender: TObject ); virtual; abstract;
		procedure CancelClick( Sender: TObject ); virtual; abstract;

		property bnCancel: TBitBtn
						 read FbnCancel;
		property bnOK: TBitBtn
						 read FbnOK;

	end;

procedure TKExtendedDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	Image.Top := ScaleY( 23 );
	FbnOK := AddButton( sCapOk, MODAL_RESULTS[mrOK], BN_RECT, true, true, true, mrNone,
		nil, Self, OKClick );
	FbnCancel := AddButton( sCapCancel, MODAL_RESULTS[mrCancel], BN_RECT, true, true,
		false, mrNone, nil, Self, CancelClick );
end;

procedure TKExtendedDialogForm.PlaceButtons;
begin
	FbnOK.Left := ClientWidth - FbnOK.Width - ScaleX( BN_GUTTER );
	FbnCancel.Left := FbnOK.Left;
end;

{---------------------------- Public Implementation ----------------------------}

constructor TKExtendedDialog.Create( AOwner: TComponent );
begin
	FormClass := nil;
	inherited Create( AOwner );
	DialogUnits := false;
	SystemMenu := nil;
	AfterExecute := nil;
	BeforeExecute := nil;
	AfterCheckParams := nil;
	BeforeCheckParams := nil;
end;

{--------------------------- Internal Implementation ---------------------------}

{ TKLogonDialogForm }

const

	FM_LOGON_WIDTH = 390;
	FM_LOGON_HEIGHT = 170;
	LOGON_CTRL_LEFT = 68;

	LB_LOGON_TOP = 15;
	ED_LOGON_TOP = 35;
	LOG_CTRL_DY = 55;
	ED_LOGON_WIDTH = 200;

	FM_PASSWORD_HEIGHT = 280;

	FM_PRINT_WIDTH = 350;
	FM_PRINT_HEIGHT = 300;

	LB_PRINT_GUTTER = 25;

	FM_ACTION_WIDTH = 350;
	FM_ACTION_HEIGHT = 500;

	LB_ACTION_GUTTER = 25;

type

{ TKLogonDialogForm }

	TKLogonDialogForm = class( TKExtendedDialogForm )
	private
		FedLogon: TEdit;
		FlbLogon: TLabel;
		FedPassword: TEdit;
		FlbPassword: TLabel;

		function GetDialog: TKCustomLogonDialog;
		procedure CancelClick( Sender: TObject ); override;

	protected
		procedure ValidateDialog; virtual;
		procedure OKClick( Sender: TObject ); override;

		procedure PrepareForm; override;
		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		property Dialog: TKCustomLogonDialog
						 read GetDialog;

	end;

{ TKPasswordDialogForm }

	TKPasswordDialogForm = class( TKLogonDialogForm )
	private
		FedNewPassword: TEdit;
		FlbNewPassword: TLabel;
		FedConfirmPassword: TEdit;
		FlbConfirmPassword: TLabel;

		function GetDialog: TKPasswordDialog;

	protected
		procedure ValidateDialog; override;

		procedure PrepareForm; override;
		procedure PlaceControls; override;

		property Dialog: TKPasswordDialog
						 read GetDialog;

	end;

{ TKPrintDialogForm }

	TKPrintDialogForm = class( TKLogoDialogForm )
	private
		FbnPrint: TBitBtn;
		FbnPreview: TBitBtn;
		FbnCancel: TBitBtn;
		FlbPrintText: TLabel;

		function GetDialog: TKPrintDialog;

	protected
		procedure PrepareForm; override;
		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		function DoShowModal: Integer; override;

		procedure PlaceLabel; virtual;

		procedure PrintClick( Sender: TObject ); virtual;
		procedure PreviewClick( Sender: TObject ); virtual;
		procedure CancelClick( Sender: TObject ); virtual;

		property lbPrintText: TLabel
						 read FlbPrintText;
		property Dialog: TKPrintDialog
						 read GetDialog;

	end;

{ TKCustonActionFormPainter }

	TKCustonActionFormPainter = class( TKFormPainter )
	protected
		procedure FormWndProcException( E: Exception ); override;

	end;

{ TKCustomActionDialogForm }

	TKCustomActionDialogForm = class( TKLogoDialogForm )
	private
		Fbn1: TBitBtn;
		Fbn2: TBitBtn;
		FbnCancel: TBitBtn;
		Flb1: TLabel;
		Flb2: TLabel;
		FlbText: TLabel;
		FProgress: TProgressBar;

		function GetDialog: TKCustomActionDialog;

	protected
		procedure PrepareForm; override;
		procedure PlaceButtons; override;
		procedure PlaceControls; override;

		procedure PlaceLabel; virtual;

		procedure Btn1Click( Sender: TObject ); virtual;
		procedure Btn2Click( Sender: TObject ); virtual;
		procedure CancelClick( Sender: TObject ); virtual;

		property Progress: TProgressBar
						 read FProgress;
		property lbText: TLabel
						 read FlbText;
		property lb1: TLabel
						 read Flb1;
		property lb2: TLabel
						 read Flb2;
		property bn1: TBitBtn
						 read Fbn1;
		property bn2: TBitBtn
						 read Fbn2;
		property bnCancel: TBitBtn
						 read FbnCancel;
		property Dialog: TKCustomActionDialog
						 read GetDialog;

	end;

{ TKLogonDialogForm }

function TKLogonDialogForm.GetDialog: TKCustomLogonDialog;
begin
	Result := ( inherited Dialog as TKCustomLogonDialog );
end;

procedure TKLogonDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	Width := ScaleX( FM_LOGON_WIDTH );
	Height := ScaleY( FM_LOGON_HEIGHT );
end;

procedure TKLogonDialogForm.PlaceButtons;
begin
	inherited PlaceButtons;
	bnOK.Top := FedLogon.Top;
	bnCancel.Top := FedPassword.Top + FedPassword.Height - bnCancel.Height;
end;

procedure TKLogonDialogForm.PlaceControls;
begin
	FedLogon := AddControl( 'edLogon', LB_RECT, true, true, Self, TEdit ) as TEdit;
	FlbLogon := AddControl( 'lbLogon', LB_RECT, true, true, Self, TLabel ) as TLabel;
	FedPassword := AddControl( 'edPassword', LB_RECT, true, true, Self, TEdit ) as TEdit;
	FlbPassword := AddControl( 'lbPassword', LB_RECT, true, true, Self, TLabel ) as TLabel;
	with FlbLogon do
	begin
		Top := ScaleY( LB_LOGON_TOP );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Transparent := true;
		FocusControl := FedLogon;
		Caption := sCapLogon;
	end;
	with FlbPassword do
	begin
		Top := ScaleY( LB_LOGON_TOP + LOG_CTRL_DY );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Transparent := true;
		FocusControl := FedPassword;
		Caption := sCapPassword;
	end;
	with FedLogon do
	begin
		Top := ScaleY( ED_LOGON_TOP );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Width := ScaleX( ED_LOGON_WIDTH );
		Text := Dialog.Logon;
		CharCase := Dialog.LogonCharCase;
	end;
	with FedPassword do
	begin
		Clear;
		Top := ScaleY( ED_LOGON_TOP + LOG_CTRL_DY );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Width := ScaleX( ED_LOGON_WIDTH );
		PasswordChar := Dialog.PasswordChar;
		CharCase := Dialog.PasswordCharCase;
	end;
	ActiveControl := FedLogon;
end;

procedure TKLogonDialogForm.CancelClick( Sender: TObject );
begin
	ModalResult := mrCancel;
	Dialog.DoCancel;
end;

procedure TKLogonDialogForm.OKClick( Sender: TObject );
begin
	Visible := false;
	try
		with Dialog do
		begin
			Inc( FLogActionTries ); { Hard Couple! }
			Logon:= FedLogon.Text;
			FPassword := FedPassword.Text; { Hard Couple! }
			ValidateDialog;
		end;
	except
		Close;
		raise;
	end;
end;

procedure TKLogonDialogForm.ValidateDialog;
begin
	with Dialog do
		if DoValidate then
		begin
			ModalResult := mrOK;
			DoSucceed;
		end
		else
		begin
			if ( LogActionTries < MaxTries ) then
			begin
				ModalResult := mrNone;
				DoFail;
				if ( ActiveControl <> FedPassword ) then
					ActiveControl := FedLogon;
				Visible := true;
			end
			else
			begin
				ModalResult := mrCancel;
				DoLastFail;
			end;
		end;
end;

{ TKPasswordDialogForm }

function TKPasswordDialogForm.GetDialog: TKPasswordDialog;
begin
	Result := ( inherited Dialog as TKPasswordDialog );
end;

procedure TKPasswordDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	Height := ScaleY( FM_PASSWORD_HEIGHT );
end;

procedure TKPasswordDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	FlbNewPassword := AddControl( 'lbNewPassword', LB_RECT, true, true, Self, TLabel ) as TLabel;
	FlbConfirmPassword := AddControl( 'lbConfirmPassword', LB_RECT, true, true, Self, TLabel ) as TLabel;
	FedNewPassword := AddControl( 'edNewPassword', LB_RECT, true, true, Self, TEdit ) as TEdit;
	FedConfirmPassword := AddControl( 'edConfirmPassword', LB_RECT, true, true, Self, TEdit ) as TEdit;
	with FlbNewPassword do
	begin
		Top := ScaleY( LB_LOGON_TOP + ( 2 * LOG_CTRL_DY ) );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Transparent := true;
		FocusControl := FedNewPassword;
		Caption := sCapNewPassword;
	end;
	with FlbConfirmPassword do
	begin
		Top := ScaleY( LB_LOGON_TOP + ( 3 * LOG_CTRL_DY ) );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Transparent := true;
		FocusControl := FedConfirmPassword;
		Caption := sCapConfirmPassword;
	end;
	with FedNewPassword do
	begin
		Clear;
		Dialog.FNewPassword := ''; { Hard Couple! }
		Top := ScaleY( ED_LOGON_TOP + ( 2 * LOG_CTRL_DY ) );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Width := ScaleX( ED_LOGON_WIDTH );
		PasswordChar := Dialog.PasswordChar;
		CharCase := Dialog.PasswordCharCase;
	end;
	with FedConfirmPassword do
	begin
		Clear;
		Top := ScaleY( ED_LOGON_TOP + ( 3 * LOG_CTRL_DY ) );
		Left := ScaleX( LOGON_CTRL_LEFT );
		Width := ScaleX( ED_LOGON_WIDTH );
		PasswordChar := Dialog.PasswordChar;
		CharCase := Dialog.PasswordCharCase;
	end;
end;

procedure TKPasswordDialogForm.ValidateDialog;
begin
	if ( not CheckCaseStrEqual( FedNewPassword.Text, FedConfirmPassword.Text ) ) then
	begin
		ModalResult := mrNone;
		Dialog.DoPasswordMismatch;
		ActiveControl := FedNewPassword;
		Visible := true;
	end
	else
	begin
		Dialog.FNewPassword := FedNewPassword.Text; { Hard Couple! }
		inherited ValidateDialog;
	end;
end;

{ TKPrintDialogForm }

function TKPrintDialogForm.GetDialog: TKPrintDialog;
begin
	Result := ( inherited Dialog as TKPrintDialog );
end;

function TKPrintDialogForm.DoShowModal: Integer;
const
	MODAL_RESULT: array[TKPrintDialogButton] of TModalResult =
		( mrOk, mrOk, mrCancel );
begin
	if Dialog.ShowDialog then
		Result := inherited DoShowModal
	else
	begin
		Result := MODAL_RESULT[Dialog.DefaultAction];
		case Dialog.DefaultAction of
			pdbPrint  : Dialog.DoPrint;
			pdbPreview: Dialog.DoPreview;
			pdbCancel : Dialog.DoCancel;
		else
			Result := mrNone;
		end;
	end;
end;

procedure TKPrintDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	Width := ScaleX( FM_PRINT_WIDTH );
	Height := ScaleY( FM_PRINT_HEIGHT );
end;

procedure TKPrintDialogForm.PlaceLabel;
var
	rt: TRect;
	ibnCount,
	iMinWidth: Integer;
begin
	with lbPrintText do
	begin
		AutoSize := false;
		Transparent := true;
		Caption := Dialog.Text;
		Left := Image.Left + Image.Width + ScaleX( LB_PRINT_GUTTER );
		Top := Image.Top;
		Width := Self.ClientWidth - Left - ScaleX( LB_PRINT_GUTTER );
		Height := ( LB_RECT.Bottom - LB_RECT.Top );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > Width ) then
		begin
			Caption := TextBlock( Caption, Canvas, Width );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Width := rt.Right;
		Height := Max( rt.Bottom, 2 * Height );
		Self.Height := Top + Height + ScaleY( FM_GUTTER );
		Self.Width := Self.Width - ( Width - Width );
		ibnCount := InstanceCount[TBitBtn];
		iMinWidth := ScaleX( ibnCount * BN_WIDTH + ( ibnCount - 1 ) * BN_DISTANCE +
			FM_LEFTMARGIN + FM_RIGHTMARGIN );
		if ( Self.Width < iMinWidth ) then
			Self.Width := iMinWidth;
		DialogData.RightAlign := false;
	end;
end;

procedure TKPrintDialogForm.PlaceControls;
begin
	FlbPrintText := ( AddControl( 'lbPrintText', LB_RECT, true, true, Self, TLabel ) as TLabel );
	FbnPrint := AddButton( sCapPrint, 'bnPrint', BN_RECT, true, ( pdbPrint in Dialog.PrintDialogButtons ),
		( Dialog.DefaultAction = pdbPrint ), mrOk, nil, Self, PrintClick );
	FbnPreview := AddButton( sCapPreview, 'bnPreview', BN_RECT, true, ( pdbPreview in Dialog.PrintDialogButtons ),
		( Dialog.DefaultAction = pdbPreview ), mrOk, nil, Self, PreviewClick );
	FbnCancel := AddButton( sCapCancel, 'bnCancel', BN_RECT, true, ( pdbCancel in Dialog.PrintDialogButtons ),
		( Dialog.DefaultAction = pdbCancel ), mrCancel, nil, Self, CancelClick );
{ do shit here }
	PlaceLabel;
end;

procedure TKPrintDialogForm.PlaceButtons;
var
	i,
	ibnCount: Integer;
	LastButton: TBitBtn;
begin
	LastButton := nil;
	ibnCount := InstanceCount[TBitBtn];
	for i := 0 to ControlCount - 1 do
		if ( CheckObjectClass( Controls[i], TBitBtn ) and Controls[i].Visible ) then
			with TBitBtn( Controls[i] ) do
			begin
				Top := Self.ClientHeight - Height - ScaleY( BN_GUTTER );
				if ( not CheckObject( LastButton ) ) then
					if Dialog.RightAlign then
						Left :=
							( Self.ClientWidth ) - ( ibnCount * Width ) -
							( ScaleX( BN_RIGHTMARGIN ) ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
					else
						Left :=
							( Self.Width div 2 ) -
							( ibnCount * Width div 2 ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
				else if CheckObject( LastButton ) then
					Left := LastButton.Left + Width +	ScaleX( BN_DISTANCE );
				LastButton := TBitBtn( Self.Controls[i] );
			end;
end;

procedure TKPrintDialogForm.PrintClick( Sender: TObject );
begin
	Dialog.Print;
end;

procedure TKPrintDialogForm.PreviewClick( Sender: TObject );
begin
	Dialog.Preview;
end;

procedure TKPrintDialogForm.CancelClick( Sender: TObject );
begin
	Dialog.DoCancel;
end;

{ TKCustonActionFormPainter }

procedure TKCustonActionFormPainter.FormWndProcException( E: Exception );
var
	ctrl: TControl;
begin
	if CheckObjectClass( E, EAbort ) then
	begin
		ctrl := FindControl( Handle );
		if CheckObjectClass( ctrl, TKCustomDialogForm ) then
			( ctrl as TKCustomDialogForm ).ModalResult := mrCancel
		else
		  RaiseException( EKActionDlgCancel, sErrInvActionDlgCancelOp );
	end
	else
		inherited FormWndProcException( E );
end;

{ TKCustomActionDialogForm }

function TKCustomActionDialogForm.GetDialog: TKCustomActionDialog;
begin
	Result := ( inherited Dialog as TKCustomActionDialog );
end;

procedure TKCustomActionDialogForm.PrepareForm;
begin
	inherited PrepareForm;
	Width := ScaleX( FM_ACTION_WIDTH );
	Height := ScaleY( FM_ACTION_HEIGHT );
end;

procedure TKCustomActionDialogForm.PlaceLabel;
var
	rt: TRect;
	ibnCount,
	iMinWidth: Integer;
begin
	with lbText do
	begin
		AutoSize := false;
		Transparent := true;
		Caption := Dialog.Text;
		Left := Image.Left + Image.Width + ScaleX( LB_PRINT_GUTTER );
		Top := Image.Top;
		Width := Self.ClientWidth - Left - ScaleX( LB_PRINT_GUTTER );
		Height := ( LB_RECT.Bottom - LB_RECT.Top );
		rt := ClientRect;
		DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
			DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		if ( rt.Right > Width ) then
		begin
			Caption := TextBlock( Caption, Canvas, Width );
			DrawText( Canvas.Handle, PChar( Caption ), Length( Caption ), rt,
				DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK );
		end
		else
			WordWrap := true;
		Width := rt.Right;
		Height := Max( rt.Bottom, 2 * Height );
		Self.Height := Top + Height + ScaleY( FM_GUTTER );
		Self.Width := Self.Width - ( Width - Width );
		ibnCount := InstanceCount[TBitBtn];
		iMinWidth := ScaleX( ibnCount * BN_WIDTH + ( ibnCount - 1 ) * BN_DISTANCE +
			FM_LEFTMARGIN + FM_RIGHTMARGIN );
		if ( Self.Width < iMinWidth ) then
			Self.Width := iMinWidth;
		DialogData.RightAlign := false;
	end;
	with lb1 do
	begin
		Left := lbText.Left;
		Top := lbText.Top + lbText.Height + ScaleY( LB_ACTION_GUTTER );
		Width := lbText.Width;
		Transparent := True;
		Caption := Dialog.Label1Caption;
	end;
	with lb2 do
	begin
		Left := lb1.Left;
		Top := lb1.Top + lb1.Height + ScaleY( LB_ACTION_GUTTER );
		Width := lb1.Width;
		Transparent := True;
		Caption := Dialog.Label2Caption;
	end;
end;

procedure TKCustomActionDialogForm.PlaceControls;
begin
	FlbText := ( AddControl( 'lbText', LB_RECT, true, true, Self, TLabel ) as TLabel );
	Flb1 := ( AddControl( 'lb1', LB_RECT, true, true, Self, TLabel ) as TLabel );
	Flb2 := ( AddControl( 'lb2', LB_RECT, true, true, Self, TLabel ) as TLabel );

	FProgress := ( AddControl( 'Progress', ED_RECT, true, true, Self, TProgressBar ) as TProgressBar );

	Fbn1 := AddButton( Dialog.Btn1Caption, 'bn1', BN_RECT, Dialog.Btn1Enabled, true, true, mrNone, nil, Self, Btn1Click );
	Fbn2 := AddButton( Dialog.Btn2Caption, 'bn2', BN_RECT, Dialog.Btn2Enabled, true, false, mrNone, nil, Self, Btn2Click );
	FbnCancel := AddButton( sCapCancel, 'bnCancel', BN_RECT, Dialog.CancelEnabled, true, false, mrCancel, nil, Self, CancelClick );

	PlaceLabel;

	with FProgress do
	begin
		Left := Image.Left;
		Position := Low( TKPercent );
		Min := Low( TKPercent );
		Max := High( TKPercent );
		Step := SizeOf( TKPercent );
		Top := lb2.Top + lb2.Height + ScaleY( LB_ACTION_GUTTER );
		Width := Image.Width + ScaleX( LB_ACTION_GUTTER ) + lbText.Width + ( ScaleX( LB_ACTION_GUTTER ) div 2 );
	end;

	Self.ClientHeight := FProgress.Top + FProgress.Height + ( 2 * ScaleY( BN_GUTTER ) ) + Fbn1.Height;
end;

procedure TKCustomActionDialogForm.PlaceButtons;
var
	i,
	ibnCount: Integer;
	LastButton: TBitBtn;
begin
	LastButton := nil;
	ibnCount := InstanceCount[TBitBtn];
	for i := 0 to ControlCount - 1 do
		if CheckObjectClass( Controls[i], TBitBtn ) then
			with TBitBtn( Controls[i] ) do
			begin
				Top := Self.ClientHeight - Height - ScaleY( BN_GUTTER );
				if ( not CheckObject( LastButton ) ) then
					if Dialog.RightAlign then
						Left :=
							( Self.ClientWidth ) - ( ibnCount * Width ) -
							( ScaleX( BN_RIGHTMARGIN ) ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
					else
						Left :=
							( Self.Width div 2 ) -
							( ibnCount * Width div 2 ) -
							( ( ibnCount - 1 ) * ScaleX( BN_DISTANCE ) div 2 )
				else if CheckObject( LastButton ) then
					Left := LastButton.Left + Width +	ScaleX( BN_DISTANCE );
				LastButton := TBitBtn( Self.Controls[i] );
			end;
end;

procedure TKCustomActionDialogForm.Btn1Click( Sender: TObject );
begin
	bn2.Enabled := False;
	try
		Dialog.DoBeforeAction;
		if Dialog.CloseOnTerminate then
			ModalResult := mrOk;
	finally
		bn2.Enabled := True;
	end;
	bnCancel.Enabled := False;
	try
	finally
		bnCancel.Enabled := True;
	end;
end;

procedure TKCustomActionDialogForm.Btn2Click( Sender: TObject );
begin
	bn1.Enabled := False;
	try
		Dialog.DoAfterAction;
		if Dialog.CloseOnTerminate then
			ModalResult := mrOk;
	finally
		bn1.Enabled := True;
	end;
end;

procedure TKCustomActionDialogForm.CancelClick( Sender: TObject );
begin
	Dialog.DoCancel;
	if Dialog.Canceled then
		Abort;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKCustomExtDialog }

constructor TKCustomExtDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	Picture.Bitmap.LoadFromResourceName( HInstance, GetBitmapResName );
end;

{ TKCustomLogonDialog }

constructor TKCustomLogonDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FMaxTries := 3;
	FLocked := false;
	FLogon := UserName;
	FLogActionTries := 0;
	FPasswordChar := '*';
	CloseOnAltF4 := true;
	CloseOnEscape := true;
	Caption := sLogonDialog;
	CloseStyle := csDisabled;
	FLogonCharCase := ecNormal;
	FPasswordCharCase := ecNormal;
end;

procedure TKCustomLogonDialog.KeyboardClose;
begin
	inherited KeyboardClose;
	DoCancel;
end;

procedure TKCustomLogonDialog.DoFail;
begin
	if Assigned( FOnFail ) then
		FOnFail( Self )
	else
		ExpireWarn( DIALOG_DEFAULT_SEC_EXPIRATION, GetDefaultFailMessage );
end;

procedure TKCustomLogonDialog.DoCancel;
begin
	if Assigned( FOnCancel ) then
		FOnCancel( Self );
end;

procedure TKCustomLogonDialog.DoSucceed;
begin
	FLocked := false;
	FLogActionTries := 0;
	if Assigned( FOnSucceed ) then
		FOnSucceed( Self );
end;

procedure TKCustomLogonDialog.DoLastFail;
begin
	FLocked := true;
	if Assigned( FOnLastFail ) then
		FOnLastFail( Self )
	else
		ExpireShowError( DIALOG_DEFAULT_SEC_EXPIRATION, GetDefaultLastFailMessage );
end;

{ TKLogonDialog }

function TKLogonDialog.GetBitmapResName: string;
begin
	Result := DIALOGBMP_LOGON;
end;

function TKLogonDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKLogonDialogForm;
end;

function TKLogonDialog.GetDefaultFailMessage: string;
begin
	Result := sLogonRetry;
end;

function TKLogonDialog.GetDefaultLastFailMessage: string;
begin
	Result := sLogonLocked;
end;

function TKLogonDialog.DoValidate: Boolean;
begin
	Result := false;
	if Assigned( FOnValidate ) then
		FOnValidate( Self, Logon, Password, Result );
end;

{ TKPasswordDialog }

function TKPasswordDialog.GetBitmapResName: string;
begin
	Result := DIALOGBMP_Password;
end;

function TKPasswordDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKPasswordDialogForm;
end;

function TKPasswordDialog.GetDefaultFailMessage: string;
begin
	Result := sPasswordRetry;
end;

function TKPasswordDialog.GetDefaultLastFailMessage: string;
begin
	Result := sPasswordLocked;
end;

function TKPasswordDialog.DoValidate: Boolean;
begin
	Result := false;
	if Assigned( FOnValidate ) then
		FOnValidate( Self, Logon, Password, NewPassword, Result );
end;

procedure TKPasswordDialog.DoPasswordMismatch;
begin
	if Assigned( FOnPasswordMismatch ) then
		FOnPasswordMismatch( Self )
	else
		ExpireWarn( DIALOG_DEFAULT_SEC_EXPIRATION, sPasswordMismatch );
end;

{ TKPrintDialog }

constructor TKPrintDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FText := '';
	FShowDialog := True;
	FRightAlign := True;
	FDefaultAction := pdbPrint;
	FPrintDialogButtons := [pdbPrint, pdbPreview, pdbCancel];
end;

function TKPrintDialog.GetBitmapResName: string;
begin
	Result := DIALOGBMP_PRINT;
end;

function TKPrintDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKPrintDialogForm;
end;

procedure TKPrintDialog.SetDefaultAction( Value: TKPrintDialogButton );
begin
	if ( Value <> FDefaultAction ) and ( Value in FPrintDialogButtons ) then
		FDefaultAction := Value;
end;

procedure TKPrintDialog.SetPrinddialogButtons( Value: TKPrintDialogButtons );
var
	i: TKPrintDialogButton;
begin
	if ( Value <> FPrintDialogButtons ) and ( Value <> [] ) then
	begin
		FPrintDialogButtons := Value;
		i := Low( TKPrintDialogButton );
		while ( not ( FDefaultAction in FPrintDialogButtons ) ) and
			( i <= High( TKPrintDialogButton ) ) do
		begin
			SetDefaultAction( i );
			Inc( i );
		end;
	end;
end;

procedure TKPrintDialog.DoPrint;
begin
	if Assigned( FOnPrint ) then
		FOnPrint( Self );
end;

procedure TKPrintDialog.DoPreview;
begin
	if Assigned( FOnPreview ) then
		FOnPreview( Self );
end;

procedure TKPrintDialog.DoCancel;
begin
	if Assigned( FOnCancel ) then
		FOnCancel( Self );
end;

procedure TKPrintDialog.Print;
begin
	ForceExecuting;
	DoPrint;
end;

procedure TKPrintDialog.Preview;
begin
	ForceExecuting;
	DoPreview;
end;

{ TKCustomActionDialog }

constructor TKCustomActionDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FCanceled := False;
	FRightAlign := True;
	FCloseOnTerminate := True;
	FLabel1Caption := sCapSource;
	FLabel2Caption := sCapDestination;
end;

function TKCustomActionDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKCustomActionDialogForm;
end;

function TKCustomActionDialog.GetFormPainterClass: TKFormPainterClass;
begin
	Result := TKCustonActionFormPainter;
end;

procedure TKCustomActionDialog.ActionProc( Percent: TKPercent; const
	Caption1, Caption2: string );
begin
	ForceExecuting;
	if ( Percent > ( Form as TKCustomActionDialogForm ).Progress.Position ) then
		( Form as TKCustomActionDialogForm ).Progress.Position := Percent;
	if ( Percent = High( TKPercent ) ) then
	  ( Form as TKCustomActionDialogForm ).ModalResult := mrOk;
	if CheckStr( Caption1 ) then
		( Form as TKCustomActionDialogForm ).lb1.Caption := Caption1;
	if CheckStr( Caption2 ) then
		( Form as TKCustomActionDialogForm ).lb2.Caption := Caption2;
	Application.ProcessMessages;
	if FCanceled then
		Abort;
end;

procedure TKCustomActionDialog.DoCancel;
begin
	FCanceled := True;
	if Assigned( FOnCancel ) then
		FOnCancel( Self );
end;

function TKCustomActionDialog.Btn1Enabled: Boolean;
begin
	Result := True;
end;

function TKCustomActionDialog.Btn2Enabled: Boolean;
begin
	Result := True;
end;

function TKCustomActionDialog.CancelEnabled: Boolean;
begin
	Result := True;
end;

procedure TKCustomActionDialog.DoBeforeExecute;
begin
  inherited DoBeforeExecute;
	FCanceled := False;
end;

{ TKBackupDialog }

constructor TKBackupDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBackupButtons := DEFAULT_BACKUP_BUTTONS;
	Btn1Caption := sCapBackup;
	Btn2Caption := sCapRestore;
end;

function TKBackupDialog.GetBitmapResName: string;
begin
  Result := DIALOGBMP_BACKUP;
end;

procedure TKBackupDialog.DoBeforeAction;
begin
	if Assigned( FOnBackup ) then
		FOnBackup( Self, ActionProc );
end;

procedure TKBackupDialog.DoAfterAction;
begin
	if Assigned( FOnRestore ) then
		FOnRestore( Self, ActionProc );
end;

function TKBackupDialog.Btn1Enabled: Boolean;
begin
	Result := ( bdbBackup in FBackupButtons );
end;

function TKBackupDialog.Btn2Enabled: Boolean;
begin
	Result := ( bdbRestore in FBackupButtons );
end;

function TKBackupDialog.CancelEnabled: Boolean;
begin
	Result := ( bdbCancel in FBackupButtons );
end;

procedure TKBackupDialog.Backup;
begin
	ForceExecuting;
	DoBeforeAction;
end;

procedure TKBackupDialog.Restore;
begin
	ForceExecuting;
	DoAfterAction;
end;

{ TKCryptoDialog }

constructor TKCryptoDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FCryptoButtons := DEFAULT_CRYPTO_BUTTONS;
	Btn1Caption := sCapEncipher;
	Btn2Caption := sCapDecipher;
end;

function TKCryptoDialog.GetBitmapResName: string;
begin
	Result := DIALOGBMP_CRYPTO;
end;

procedure TKCryptoDialog.DoBeforeAction;
begin
	if Assigned( FOnEncipher ) then
		FOnEncipher( Self, ActionProc );
end;

procedure TKCryptoDialog.DoAfterAction;
begin
	if Assigned( FOnDecipher ) then
		FOnDecipher( Self, ActionProc );
end;

function TKCryptoDialog.Btn1Enabled: Boolean;
begin
	Result := ( cbEncipher in FCryptoButtons );
end;

function TKCryptoDialog.Btn2Enabled: Boolean;
begin
	Result := ( cbDecipher in FCryptoButtons );
end;

function TKCryptoDialog.CancelEnabled: Boolean;
begin
	Result := ( cbCancel in FCryptoButtons );
end;

procedure TKCryptoDialog.Encipher;
begin
	ForceExecuting;
	DoBeforeAction;
end;

procedure TKCryptoDialog.Decipher;
begin
	ForceExecuting;
	DoAfterAction;
end;

{
--------------------------------------------------------------------------------
-------------------------- DualList Dialog Implementation --------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

const
	FM_DUAL_LIST_HEIGHT = 340;
	FM_DUAL_LIST_WIDTH = 490;

	DUAL_LIST_FRM_NAME = 'frmDualListDlg';

	LB_DUAL_LIST_TOP = 10;
	LB_DUAL_LIST_SRC_LEFT = 5;
	LB_DUAL_LIST_DST_LEFT = 246;
	LB_DUAL_LIST_HEIGHT = 21;
	LB_DUAL_LIST_WIDTH = 65;

	LB_DUALLIST_SRC_RECT: TRect =
	(
		Left:   LB_DUAL_LIST_SRC_LEFT;
		Top:    LB_DUAL_LIST_TOP;
		Right:  LB_DUAL_LIST_SRC_LEFT + LB_DUAL_LIST_WIDTH;
		Bottom: LB_DUAL_LIST_TOP + LB_DUAL_LIST_HEIGHT
	);
	LB_DUALLIST_DST_RECT: TRect =
	(
		Left:   LB_DUAL_LIST_DST_LEFT;
		Top:    LB_DUAL_LIST_TOP;
		Right:  LB_DUAL_LIST_DST_LEFT + LB_DUAL_LIST_WIDTH;
		Bottom: LB_DUAL_LIST_TOP + LB_DUAL_LIST_HEIGHT
	);

	SRC_LBX_LEFT = 5;
	SRC_LBX_TOP = LB_DUAL_LIST_TOP + LB_DUAL_LIST_HEIGHT + 5;
	SRC_LBX_WIDTH = 190;
	SRC_LBX_HEIGHT = 224;

	SRC_LBX_RECT: TRect =
	(
		Left:   SRC_LBX_LEFT;
		Top:    SRC_LBX_TOP;
		Right:  SRC_LBX_LEFT + SRC_LBX_WIDTH;
		Bottom: SRC_LBX_TOP + SRC_LBX_HEIGHT
	);

	DST_LBX_LEFT = 246;

	DST_LBX_RECT: TRect =
	(
		Left:   DST_LBX_LEFT;
		Top:    SRC_LBX_TOP;
		Right:  DST_LBX_LEFT + SRC_LBX_WIDTH;
		Bottom: SRC_LBX_TOP + SRC_LBX_HEIGHT
	);

	SRC_LBX_TAG = 100;
	DST_LBX_TAG = 200;

  LBX_ITEM_HEIGHT = 20;

	SBT_GUTTER_X = 8;
	SBT_GUTTER_Y = 8;
	
	SBT_LEFT = 204;
	SBT_WIDTH = 33;
	SBT_HEIGHT = SBT_WIDTH;

	SBT_RECT: TRect =
	(
		Left: SBT_LEFT;
		Top: 0;
		Right: SBT_LEFT + SBT_WIDTH;
		Bottom: SBT_HEIGHT;
	);

	BTN_INC_TAG = 1;
	BTN_IAL_TAG = 2;
	BTN_EXC_TAG = 3;
	BTN_EAL_TAG = 4;
	BTN_UP__TAG = 5;
	BTN_DWN_TAG = 6;

	BMP_SIZE_X = 48;
	BMP_SIZE_Y = 24;

	BTN_PATTERN = 'DLB_%s';

	BTN_BMP_IDX: array[TKDualListButton] of PChar = ( 'INCLUDE', 'INCLUDEALL', 'EXCLUDE',
		'EXCLUDEALL', 'EXCHANGEUP', 'EXCHANGEDOWN' );

	BTN_NAMES: array[TKDualListButton] of string[12] =
		( 'IncludeBtn', 'IncAllBtn', 'ExcludeBtn', 'ExAllBtn', 'ExchangeUp', 'ExchangeDown' );

	BTN_HINTS: array[TKDualListButton] of string[18] =
		( 'Include item', 'Include all items', 'Exclude item', 'Exclude all items',
			'Exchange item up', 'Exchange item down' );

{ Private Methods }

function TKCustomDualListDialogForm.GetDialog: TKCustomDualListDialog;
begin
	Result := ( inherited Dialog as TKCustomDualListDialog );
end;

function TKCustomDualListDialogForm.GetImgList: TImageList;
begin
	Result := Dialog.ListImages;
end;

function TKCustomDualListDialogForm.GetVisibleButtons: TKDualListButtons;
begin
	Result := Dialog.VisibleButtons;
end;

procedure TKCustomDualListDialogForm.SetVisibleButtons( Value: TKDualListButtons );
var
	i: TKDualListButton;
begin
	for i := Low( TKDualListButton ) to High( TKDualListButton ) do
		( FindComponent( BTN_NAMES[i] ) as TSpeedButton ).Visible := ( i in Value );
end;

{ Form Methods derived from TKCustomDialogForm }

procedure TKCustomDualListDialogForm.PrepareForm;
begin
	Width := FM_DUAL_LIST_WIDTH;
	Height := FM_DUAL_LIST_HEIGHT;
	if CheckObject( ilList ) then
	begin
		ilList.Width  := ScaleX( LBX_ITEM_HEIGHT );
		ilList.Height := ScaleY( LBX_ITEM_HEIGHT );
	end;
	Name := DUAL_LIST_FRM_NAME;
	Position := poScreenCenter;
	KeyPreview := True;
	OnShow := FormShow;
end;

procedure TKCustomDualListDialogForm.PlaceControls;
begin
	FlbSourceList := AddControl( 'lbSourceList', LB_DUALLIST_SRC_RECT, true, true, Self, TLabel ) as TLabel;
	FlbDestList := AddControl( 'lbDestList', LB_DUALLIST_DST_RECT, true, true, Self, TLabel ) as TLabel;
	FSrcList := AddControl( 'SrcList', SRC_LBX_RECT, true, true, Self, TListBox ) as TListBox;
	FDstList := AddControl( 'DstList', DST_LBX_RECT, true, true, Self, TListBox ) as TListBox;
	ActiveControl := FSrcList;
	with FlbSourceList do
	begin
		Transparent := true;
		FocusControl := FSrcList;
		Caption := sCapSourceList;
	end;
	with FSrcList do
	begin
		Tag := SRC_LBX_TAG;
		DragMode := dmAutomatic;
		ShowHint := True;
		Hint := Dialog.SourceListHint;
		MultiSelect := Dialog.MultiSelect;
		Sorted := True;
		Style := lbOwnerDrawFixed;
		TabOrder := 0;
		IntegralHeight := True;
		ItemHeight := LBX_ITEM_HEIGHT;
		OnClick := ListClick;
		OnDblClick := IncludeBtnClick;
		OnDragDrop := ListDragDrop;
		OnDragOver := ListDragOver;
		OnDrawItem := ListDrawItem;
		OnKeyDown := SrcListKeyDown;
		OnMeasureItem := ListMeasureItem;
		OnMouseDown := ListMouseDown;
	end;
	with FlbDestList do
	begin
		Transparent := true;
		FocusControl := FDstList;
		Caption := sCapDestList;
	end;
	with FDstList do
	begin
		Tag := DST_LBX_TAG;
		DragMode := dmAutomatic;
		ShowHint := True;
		Hint := Dialog.DestListHint;
		MultiSelect := Dialog.MultiSelect;
		Style := lbOwnerDrawFixed;
		TabOrder := 1;
		IntegralHeight := True;
		ItemHeight := LBX_ITEM_HEIGHT;
		OnClick := ListClick;
		OnDblClick := ExcludeBtnClick;
		OnDragDrop := ListDragDrop;
		OnDragOver := ListDragOver;
		OnDrawItem := ListDrawItem;
		OnKeyDown := DstListKeyDown;
		OnMeasureItem := ListMeasureItem;
		OnMouseDown := ListMouseDown;
	end;
	SrcList.Items.Assign( Dialog.SourceList );
	DstList.Items.Assign( Dialog.DestList );
	FillSelection( SrcList, Dialog.SourceSel.Bits );
	FillSelection( DstList, Dialog.DestSel.Bits );
end;

procedure TKCustomDualListDialogForm.PlaceButtons;
begin
	FIncludeBtn := AddSpeedButton( '', BTN_NAMES[dlbInclude], SBT_RECT, True,
		( dlbInclude in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, IncludeBtnClick );
	FIncAllBtn := AddSpeedButton( '', BTN_NAMES[dlbIncludeAll], SBT_RECT, True,
		( dlbIncludeAll in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, IncAllBtnClick );
	FExcludeBtn := AddSpeedButton( '', BTN_NAMES[dlbExclude], SBT_RECT, True,
		( dlbExclude in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, ExcludeBtnClick );
	FExAllBtn := AddSpeedButton( '', BTN_NAMES[dlbExcludeAll], SBT_RECT, True,
		( dlbExcludeAll in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, ExcAllBtnClick );
	FExchangeUp := AddSpeedButton( '', BTN_NAMES[dlbExchangeUp], SBT_RECT, True,
		( dlbExchangeUp in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, ExchangeBtnClick );
	FExchangeDown := AddSpeedButton( '', BTN_NAMES[dlbExchangeDown], SBT_RECT, True,
		( dlbExchangeDown in VisibleButtons ), False, Dialog.Flat, False, nil, 0, Self, ExchangeBtnClick );
	FbnOk := AddButton( sCapOk, 'bnOk', BN_RECT, True, True, True, mrOk, nil, Self, nil );
	FbnCancel := AddButton( sCapCancel, 'bnCancel', BN_RECT, True, True, False, mrCancel, nil, Self, nil );
	LoadButtonBitmaps;
	with FbnOk do
	begin
		Top := FSrcList.Top + FSrcList.Height + ScaleY( BTN_GUTTER_Y );
		Left := ( Self.Width div 2 ) - Width - ( ScaleX( BTN_GUTTER_X ) div 2 ) - 1;
	end;
	with FbnCancel do
	begin
		Top := FbnOk.Top;
		Left := ( Self.Width div 2 ) + ( ScaleX( BTN_GUTTER_X ) div 2 ) + 1;
	end;
	with FIncludeBtn do
	begin
		Tag := BTN_INC_TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbInclude];
		Top := FSrcList.Top;
	end;
	with FIncAllBtn do
	begin
		Tag := BTN_IAL_TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbIncludeAll];
		Top := FIncludeBtn.Top + Height + ScaleY( SBT_GUTTER_Y );
	end;
	with FExcludeBtn do
	begin
		Tag := BTN_EXC_TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbExclude];
		Top := FIncAllBtn.Top + Height + ScaleY( SBT_GUTTER_Y );
	end;
	with FExAllBtn do
	begin
		Tag := BTN_EAL_TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbExcludeAll];
		Top := FExcludeBtn.Top + Height + ScaleY( SBT_GUTTER_Y );
	end;
	with FExchangeDown do
	begin
		Tag := BTN_DWN_TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbExchangeDown];
		Left := FDstList.Left + FDstList.Width + ScaleX( SBT_GUTTER_X );
		Top := FDstList.Top + FDstList.Height - Height;
	end;                                     
	with FExchangeUp do
	begin
		Tag := BTN_UP__TAG;
		ShowHint := True;
		Hint := BTN_HINTS[dlbExchangeUp];
		Left := FExchangeDown.Left;
		Top := FExchangeDown.Top - Height - ScaleY( SBT_GUTTER_Y );
	end;
	SetVisibleButtons( Dialog.VisibleButtons );
end;

procedure TKCustomDualListDialogForm.UnprepareForm;
begin
	Dialog.SourceList.Assign( SrcList.Items );
	Dialog.DestList.Assign( DstList.Items );
	RetrieveSelection( SrcList, Dialog.SourceSel.Bits );
	RetrieveSelection( DstList, Dialog.DestSel.Bits );
end;

{ Form Methods introduced in this class }

procedure TKCustomDualListDialogForm.LoadButtonBitmaps;
var
	bmp,
	bmps: TBitmap;
	imSize: TPoint;
	i: TKDualListButton;
begin
	imSize := Point( ScaleX( BMP_SIZE_X ), ScaleY( BMP_SIZE_Y ) );
	bmp := TBitmap.Create;
	try
		bmps := TBitmap.Create;
		try
			bmps.Width := imSize.x;
			bmps.Height := imSize.y;
			for i := Low( TKDualListButton ) to High( TKDualListButton ) do
				with ( FindComponent( BTN_NAMES[i] ) as TSpeedButton ) do
				begin
					bmp.LoadFromResourceName( HInstance, Format( BTN_PATTERN, [BTN_BMP_IDX[i]] ) );
					bmps.Canvas.StretchDraw( Bounds( 0, 0, imSize.x, imSize.y ), bmp );
					Glyph.Assign( bmps );
					NumGlyphs := 2;
				end;
		finally
			bmps.Free;
		end;
	finally
		bmp.Free;
	end;
end;

procedure TKCustomDualListDialogForm.SetButtons;
var
	SrcEmpty,
	DstEmpty: Boolean;
begin
	SrcEmpty := ( SrcList.Items.Count = 0 );
	DstEmpty := ( DstList.Items.Count = 0 );
	IncludeBtn.Enabled := ( not SrcEmpty ) and ( dlbInclude in VisibleButtons );
	IncAllBtn.Enabled := ( not SrcEmpty ) and ( dlbIncludeAll in VisibleButtons );
	ExcludeBtn.Enabled := ( not DstEmpty ) and ( dlbExclude in VisibleButtons );
	ExAllBtn.Enabled := ( not DstEmpty ) and ( dlbExcludeAll in VisibleButtons );
	ExchangeUp.Enabled := ( DstList.SelCount = 1 ) and ( FirstSelection( DstList ) > 0 ) and
		( dlbExchangeUp in VisibleButtons );
	ExchangeDown.Enabled := ( DstList.SelCount = 1 ) and ( FirstSelection( DstList ) < DstList.Items.Count - 1 )
		and ( dlbExchangeDown in VisibleButtons );
end;

function TKCustomDualListDialogForm.FirstSelection( List: TCustomListBox ): Integer;
begin
	for Result := 0 to List.Items.Count - 1 do
		if List.Selected[Result] then
			Exit;
	Result := -1;
end;

procedure TKCustomDualListDialogForm.FillSelection( List: TListBox; Bits: TBits );
var
	i: Integer;
begin
	ForceObjects( [List, Bits] );
	Bits.Size := List.Items.Count;
	for i := 0 to List.Items.Count - 1 do
		List.Selected[i] := Bits[i];
end;

procedure TKCustomDualListDialogForm.RetrieveSelection( List: TListBox; Bits: TBits );
var
	i: Integer;
begin
	ForceObjects( [List, Bits] );
	Bits.Size := List.Items.Count;
	for i := 0 to List.Items.Count - 1 do
		Bits[i] := List.Selected[i];
end;

procedure TKCustomDualListDialogForm.SelectItem( List: TCustomListBox; Index: Integer );
var
	MaxIndex: Integer;
begin
	with List do
	begin
		SetFocus;
		MaxIndex := List.Items.Count - 1;
		if ( Index = LB_ERR ) then
			Index := 0
		else if ( Index > MaxIndex ) then
			Index := MaxIndex;
		if ( Index >= 0 ) then
  		Selected[Index] := true;
	end;
end;

procedure TKCustomDualListDialogForm.InternalMoveSelect( List: TCustomListBox;
  Items: TStrings; Index: Integer );
begin
  Items.AddObject( List.Items[Index], List.Items.Objects[Index] );
end;

procedure TKCustomDualListDialogForm.MoveSelected( List: TCustomListBox; Items: TStrings );
var
	i: Integer;
begin
	for i := List.Items.Count - 1 downto 0 do
		if List.Selected[i] then
		begin
			InternalMoveSelect( List, Items, i );
			List.Items.Delete( i );
		end;
end;

procedure TKCustomDualListDialogForm.ClearSelection( List: TListBox );
var
	i: Integer;
begin
	for i := 0 to List.Items.Count - 1 do
		List.Selected[i] := False;
end;

procedure TKCustomDualListDialogForm.ButtonClicked( Button: TKDualListButton );
begin
	Dialog.DoDualListButton( Button );
end;

{ Form Events }

procedure TKCustomDualListDialogForm.FormShow( Sender: TObject );
begin
	if ( SrcList.Items.Count > 0 ) then
		SrcList.ItemIndex := 0;
	if ( DstList.Items.Count > 0 ) then
		DstList.ItemIndex := 0;
	SetButtons;
end;

procedure TKCustomDualListDialogForm.ListClick( Sender: TObject );
begin
	SetButtons;
	if ( Sender is TListBox ) then
		if ( ( Sender as TListBox ) = SrcList ) then
			Dialog.DoSrcListClick
		else
			Dialog.DoDstListClick;
end;

procedure TKCustomDualListDialogForm.ListMouseDown( Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
	if ( ( Sender as TListBox ).ItemAtPos( Point( X, Y ), True ) <> -1 ) then
		SetButtons;
end;

procedure TKCustomDualListDialogForm.IncludeBtnClick( Sender: TObject );
var
	Index: Integer;
begin
	if IncludeBtn.Visible then
	begin
		Index := FirstSelection( SrcList );
		MoveSelected( SrcList, DstList.Items );
		SelectItem( SrcList, Index );
		SetButtons;
		ButtonClicked( dlbInclude );
	end;
end;

procedure TKCustomDualListDialogForm.ExcludeBtnClick( Sender: TObject );
var
	Index: Integer;
begin
	if ExcludeBtn.Visible then
	begin
		Index := FirstSelection( DstList );
		MoveSelected( DstList, SrcList.Items );
		SelectItem( DstList, Index );
		SetButtons;
		ButtonClicked( dlbExclude );
	end;
end;

procedure TKCustomDualListDialogForm.IncAllBtnClick( Sender: TObject );
var
	i: Integer;
begin
	if IncAllBtn.Visible then
	begin
		for i := 0 to SrcList.Items.Count - 1 do
			InternalMoveSelect( SrcList, DstList.Items, i );
		SrcList.Items.Clear;
		SelectItem( SrcList, 0 );
		SetButtons;
		ButtonClicked( dlbIncludeAll );
	end;
end;

procedure TKCustomDualListDialogForm.ExcAllBtnClick( Sender: TObject );
var
	i: Integer;
begin
	if ExAllBtn.Visible then
	begin
		for i := 0 to DstList.Items.Count - 1 do
			InternalMoveSelect( DstList, SrcList.Items, i );
		DstList.Items.Clear;
		SelectItem( DstList, 0 );
		SetButtons;
		ButtonClicked( dlbExcludeAll );
	end;
end;

procedure TKCustomDualListDialogForm.ExchangeBtnClick( Sender: TObject );
const
	DLG_BTN: array[Boolean] of TKDualListButton = ( dlbExchangeDown, dlbExchangeUp );
var
	i: Integer;
begin
	i := 0;
	while ( not DstList.Selected[i] ) do
		Inc( i );
	with ( Sender as TSpeedButton ) do
	begin
		case Tag of
			BTN_UP__TAG:
				if ExchangeUp.Visible then
				begin
					DstList.Items.Exchange( i, i - 1 );
					DstList.Selected[i - 1] := true;
				end;
			BTN_DWN_TAG:
				if ExchangeDown.Visible then
				begin
					DstList.Items.Exchange( i, i + 1 );
					DstList.Selected[i + 1] := true;
				end;
		end;
		ButtonClicked( DLG_BTN[( Tag = BTN_UP__TAG )] );
	end;
	SetButtons;
end;

{ OwnerDraw Support }

procedure TKCustomDualListDialogForm.ListDrawItem( Control: TWinControl;
	Index: Integer; Rect: TRect; State: TOwnerDrawState );
begin
	with ( Control as TListBox ) do
	begin
		Canvas.FillRect( Rect );
		if CheckObject( ilList ) then
		begin
			ilList.Draw( Canvas, Rect.Left, Rect.Top + 1, ( Tag div SRC_LBX_TAG ) );
			Canvas.TextOut( Rect.Left + ScaleX( LBX_ITEM_HEIGHT ), Rect.Top + 3, Items[Index] );
		end
		else
			Canvas.TextOut( Rect.Left + 2, Rect.Top + 3, Items[Index] );
	end;
end;

procedure TKCustomDualListDialogForm.ListMeasureItem( Control: TWinControl;
	Index: Integer; var Height: Integer );
begin
	Height := ScaleY( LBX_ITEM_HEIGHT );
end;

{ Drag'n Drop Support }

procedure TKCustomDualListDialogForm.ListDragDrop( Sender, Source: TObject; X, Y: Integer );
var
	Index: Integer;
begin
	Index := FirstSelection( Source as TListBox );
	MoveSelected( ( Source as TListBox ) , ( Sender as TListBox ).Items );
	SelectItem( ( Source as TListBox ), Index );
end;

procedure TKCustomDualListDialogForm.ListDragOver( Sender, Source: TObject; X,
	Y: Integer; State: TDragState; var Accept: Boolean );
begin
	Accept := ( Dialog.AcceptDragDrop and CheckObjectClass( Sender, TListBox ) and
		CheckObjectClass( Sender, TListBox ) and
		( ( Sender as TListBox ).Tag <> ( Source as TListBox ).Tag ) and
		( ( DUALLIST_NAV_BUTTONS * VisibleButtons ) <> [] ) );
end;

{ Keyboard Support }

procedure TKCustomDualListDialogForm.SrcListKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if ( Key = VK_RIGHT ) and ( [ssCtrl, ssShift] * Shift = [ssCtrl, ssShift] ) then
		IncAllBtnClick( nil )
	else if ( ssCtrl in Shift ) then
		if ( Key = VK_RIGHT ) then
			IncludeBtnClick( nil )
		else if ( Key = VK_SPACE ) then
			ClearSelection( SrcList );
end;

procedure TKCustomDualListDialogForm.DstListKeyDown( Sender: TObject; var Key: Word;
	Shift: TShiftState );
begin
	if ( Key = VK_LEFT ) and ( [ssCtrl, ssShift] * Shift = [ssCtrl, ssShift] ) then
		ExcAllBtnClick( nil )
	else if ( ssCtrl in Shift ) then
	begin
		case Key of
			VK_LEFT :
				ExcludeBtnClick( nil );
			VK_UP   :
				if ( DstList.SelCount = 1 ) and ( FirstSelection( DstList ) > 0 ) then
					ExchangeBtnClick( ExchangeUp )
				else
					MessageBeep( 0 );
			VK_DOWN :
				if ( DstList.SelCount = 1 ) and ( FirstSelection( DstList ) < DstList.Items.Count - 1 ) then
					ExchangeBtnClick( ExchangeDown )
				else
					MessageBeep( 0 );
			VK_SPACE:
				ClearSelection( DstList );
		end;
		if ( Key in [VK_UP, VK_DOWN] ) then
{ Notify that that message was already processed }
			Key := 0;
	end
	else if ( Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT] ) then
		SetButtons;
end;

procedure AdjustStringsAndBits( DestList, SourceList: TStrings; DestSel, SourceSel: TBits );
var
	i,
	j: Integer;
	sSave: TStrings;
begin
{ Only 65535 items are allowed per StringList... }
	ForceObjects( [DestList, SourceList, SourceSel] );
	DestSel.Size := DestList.Count;
	SourceSel.Size := SourceList.Count;
	sSave := TStringList.Create;
	try
		sSave.Assign( SourceList );
		for i := 0 to SourceList.Count - 1 do
			SourceList.Objects[i] := TObject( MakeLong( Word( i ), Ord( SourceSel[i] ) ) );
		StringsDifference( DestList, SourceList );
		SourceSel.Size := SourceList.Count;
		for i := 0 to SourceList.Count - 1 do
			SourceSel.Bits[LongRec( SourceList.Objects[i] ).Lo] := Boolean( Ord( LongRec( SourceList.Objects[i] ).Hi ) );
		for i := 0 to sSave.Count - 1 do
		begin
			j := SourceList.IndexOf( sSave.Strings[i] );
			if ( j <> -1 ) then
				SourceList.Objects[j] := sSave.Objects[i];
		end;		
	finally
	  sSave.Free;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKCustomDualListDialog }

constructor TKCustomDualListDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	DialogUnits := False;
	FFlat := True;
	FListImages := nil;
	FMultiSelect := True;
	FAcceptDragDrop := True;
	FVisibleButtons := DEFAULT_DUALLIST_VISIBLE_BUTTONS;
	FDestSel := TKBitsWrapper.Create( Self );
	FSourceSel := TKBitsWrapper.Create( Self );
	FDestList := TStringList.Create;
	TStringList( FDestList ).OnChange := ListsChange;
	FSourceList := TStringList.Create;
	TStringList( FSourceList ).OnChange := ListsChange;
	FDestListHint := sDstListHint;
	FSourceListHint := sSrcListHint;
end;

destructor TKCustomDualListDialog.Destroy;
begin
	FDestSel.Free;
	FSourceSel.Free;
	FDestList.Free;
	FSourceList.Free;
	inherited Destroy;
end;

function TKCustomDualListDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKCustomDualListDialogForm;
end;

procedure TKCustomDualListDialog.ListsChange( Sender: TObject );
begin
	TStringList( Sender ).OnChange := nil;
	try
		NotifyListsChange( ( Sender as TStrings ) );
		if Designing( Self ) then
			MarkDesigner( Self );
	finally
		TStringList( Sender ).OnChange := ListsChange;
	end;
end;

procedure TKCustomDualListDialog.NotifyListsChange( sl: TStrings );
begin
	AdjustStringsAndBits( FDestList, FSourceList, FDestSel.Bits, FSourceSel.Bits );
end;

procedure TKCustomDualListDialog.DoSrcListClick;
begin
	if Assigned( FOnSourceListClick ) then
		FOnSourceListClick( Self );
end;

procedure TKCustomDualListDialog.DoDstListClick;
begin
	if Assigned( FOnDestinationListClick ) then
		FOnDestinationListClick( Self );
end;

procedure TKCustomDualListDialog.DoDualListButton( Button: TKDualListButton );
begin
	if Assigned( FOnDualListButtonClick ) then
	  FOnDualListButtonClick( Self, Button );
end;

procedure TKCustomDualListDialog.SetVisibleButtons( Value: TKDualListButtons );
begin
	if ( Value <> FVisibleButtons ) then
		FVisibleButtons := Value;
end;

procedure TKCustomDualListDialog.SetSourceList( Value: TStrings );
begin
	FSourceList.Assign( Value );
end;

procedure TKCustomDualListDialog.SetDestList( Value: TStrings );
begin
	FDestList.Assign( Value );
end;

procedure TKCustomDualListDialog.SetSourceSel( Value: TKBitsWrapper );
begin
	FSourceSel.Assign( Value );
end;

procedure TKCustomDualListDialog.SetDestSel( Value: TKBitsWrapper );
begin
	FDestSel.Assign( Value );
end;

procedure TKCustomDualListDialog.SetListImages( Value: TImageList );
begin
	FListImages := Value;
	if CheckObject( Value ) then
		Value.FreeNotification( Self );
end;

procedure TKCustomDualListDialog.Notification( AComponent: TComponent;
	Operation: TOperation );
begin
	inherited Notification( AComponent, Operation );
	if ( Operation = opRemove ) and ( AComponent = FListImages ) then
		FListImages := nil;
end;

procedure TKCustomDualListDialog.RetrieveSelection( IsSource: Boolean; Bits: TBits );
var
	i: Integer;
begin
	ForceObject( Bits );
	if IsSource then
		Bits.Size := FSourceSel.Bits.Size
	else
		Bits.Size := FDestSel.Bits.Size;
	for i := 0 to Bits.Size - 1 do
		if IsSource then
			Bits[i] := FSourceSel.Bits[i]
		else
			Bits[i] := FDestSel.Bits[i];
end;

function TKCustomDualListDialog.DoCheckParams: Boolean;
begin
	Result := ( CheckStrings( SourceList ) or CheckStrings( DestList ) ) and
	  ( inherited DoCheckParams );
end;

type
	TKDialogExecuteFunc = function ( Dialog: TKCustomDualListDialog; Data: Pointer ): Boolean;

function InternalDualListDialog( DialogClass: TKCustomDualListDialogClass;
	const ACaption: string; ASourceList, ADestList: TStrings; ASourceSel, ADestSel: TBits;
	MultiSelectLists: Boolean; Proc: TKDialogExecuteFunc; Data: Pointer ): Boolean;
var
	dldlg: TKCustomDualListDialog;
begin
	if ( not CheckStrings( ADestList ) ) then
		ForceStrings( ASourceList );
	dldlg := DialogClass.Create( nil );
	try
		with dldlg do
		begin
			Caption := ACaption;
			MultiSelect := MultiSelectLists;
			VisibleButtons := DEFAULT_DUALLIST_VISIBLE_BUTTONS;
			if CheckStrings( ADestList ) then
			begin
				DestList.Assign( ADestList );
				if CheckObject( ADestSel ) then
					DestSel.Bits.Assign( ADestSel );
			end;
			SourceList.Assign( ASourceList );
			if CheckObject( ASourceSel ) then
				SourceSel.Bits.Assign( ASourceSel );
			RefreshDialogData( dldlg );
			if Assigned( Proc ) then
				Result := Proc( dldlg, Data )
			else
				Result := Execute;
			if Result then
			begin
				ASourceList.Assign( SourceList );
				if CheckObject( ASourceSel ) then
					RetrieveSelection( True, ASourceSel );
				if CheckObject( ADestList ) then
				begin
					ADestList.Assign( DestList );
					if CheckObject( ADestSel ) then
						RetrieveSelection( False, ADestSel );
				end;
			end;
		end;
	finally
		dldlg.Free;
	end;
end;

function DualListDialog( const ACaption: string; ASourceList, ADestList: TStrings;
	ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;
begin
	Result := InternalDualListDialog( TKDualListDialog, ACaption, ASourceList,
		ADestList, ASourceSel, ADestSel, MultiSelectLists, nil, nil );
end;

{----------------------- DualListEdit Dialog Implementation --------------------}

{ TKCustomDualListEditDialogForm }

{ Private Methods }

function TKCustomDualListEditDialogForm.GetDialog: TKCustomDualListEditDialog;
begin
	Result := TKCustomDualListEditDialog( inherited Dialog );
end;

{ Form Methods derived from TKCustomDualListDialogForm }

procedure TKCustomDualListEditDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	FEditControl := AddControl( 'edControl', EDT_RECT, True, True, Self,
		Dialog.GetEditControlClass ) as Dialog.GetEditControlClass;
	with TKCustomSpeedControlHack( FEditControl ) do
	begin
		Top := DstList.Top;
		Left := DstList.Left;
		Width := DstList.Width;
		Color := clWindow;
		NullCommand := SC_NULL;
		ButtonCommand := Dialog.EditShortCut;
		OnKeyDown := EditControlKeyDown;
		OnDblClick := EditControlDblClick;
		OnButtonClick := EditControlBtnClick;
	end;
	DstList.Top := FEditControl.Top + FEditControl.Height + ScaleY( EDT_GUTTER );
	DstList.Height := SrcList.Top + SrcList.Height - DstList.Top;
end;

procedure TKCustomDualListEditDialogForm.PlaceButtons;
begin
	inherited PlaceButtons;
	with bnCancel do
		Left := ( ExchangeDown.Left + ExchangeDown.Width - Width );
	with bnOk do
		Left := ( bnCancel.Left - Width - ScaleX( BTN_GUTTER_X ) ) + 1;
	if ( ClientHeight < bnOk.Top + bnOk.Height + ScaleY( BTN_GUTTER_Y ) ) then
  	ClientHeight := bnOk.Top + bnOk.Height + ScaleY( BTN_GUTTER_Y );
end;

{ Form Methods derived from TKCustomDualListDialogForm }

procedure TKCustomDualListEditDialogForm.SetButtons;
begin
	inherited SetButtons;
	UpdateEdit;
end;

procedure TKCustomDualListEditDialogForm.ButtonClicked( Button: TKDualListButton );
begin
	case Button of
		dlbInclude     : SelectFirstDest;
		dlbIncludeAll  : SelectFirstDest;
	end;
	inherited ButtonClicked( Button );
end;

procedure TKCustomDualListEditDialogForm.SelectItem( List: TCustomListBox; Index: Integer );
begin
	inherited SelectItem( List, Index );
	UpdateEdit;
end;

procedure TKCustomDualListEditDialogForm.InternalMoveSelect( List: TCustomListBox;
	Items: TStrings; Index: Integer );
begin
{ Do not call the inherited method! }
	if ( List = SrcList ) then
		Items.AddObject( List.Items[Index] + CH_EQUAL_TOKEN, List.Items.Objects[Index] )
	else
		Items.AddObject( Copy( List.Items[Index], 1, Pos( CH_EQUAL_TOKEN, List.Items[Index] ) - 1 ),
			List.Items.Objects[Index] );
end;

{ Form Methods introduced in this class }

procedure TKCustomDualListEditDialogForm.UpdateEdit;
var
	i: Integer;
begin
	if ( DstList.SelCount = 1 ) then
	begin
		i := FirstSelection( DstList );
		if ( i = LB_ERR ) then
			uksyUtils.RaiseException( EKDualListEditDialog, sErrDLEInvSelection );
		with DstList.Items do
			SetEditControlText( Values[Names[i]] );
	end
	else
		ClearEditControlText;
end;

procedure TKCustomDualListEditDialogForm.AddFieldValue;
var
	i{,
	j}: Integer;
begin
	i := FirstSelection( DstList );
	if ( i = LB_ERR ) then
		uksyUtils.RaiseException( EKDualListEditDialog, sErrDLEInvSelection );
	try
(*
		if DstList.MultiSelect then
			for j := 0 to DstList.SelCount - 1 do
				if DstList.Selected[i] then
	{ do not call Values[Names[i]] directly because a TStrings bug when the value is clear }
					DstList.Items.Strings[i] := DstList.Items.Names[i] + CH_EQUAL_TOKEN + GetEditControlText
		else
*)
		DstList.Items.Strings[i] := DstList.Items.Names[i] + CH_EQUAL_TOKEN + GetEditControlText;
		DstList.SetFocus;
	except
		DstList.Items.Strings[i] := DstList.Items.Names[i] + CH_EQUAL_TOKEN;
		raise;
	end;
end;

procedure TKCustomDualListEditDialogForm.SelectFirstDest;
begin
	if ( DstList.Items.Count > 0 ) and ( DstList.SelCount = 0 ) then
		SelectItem( DstList, 0 );
end;

function TKCustomDualListEditDialogForm.GetControl: TWinControl;
begin
	Result := FEditControl;
end;

{ Form Events }

procedure TKCustomDualListEditDialogForm.EditControlKeyDown( Sender: TObject;
	var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = Dialog.EditShortCut ) then
	begin
		Key := 0;
		AddFieldValue;
	end
	else if ( ShortCut( Key, Shift ) = Dialog.ClearShortCut ) then
	begin
		Key := 0;
		ClearEditControlText;
		AddFieldValue;
	end;
end;

procedure TKCustomDualListEditDialogForm.EditControlDblClick( Sender: TObject );
begin
	AddFieldValue;
end;

procedure TKCustomDualListEditDialogForm.EditControlBtnClick( Sender: TObject );
begin
  AddFieldValue;
end;

{ TKCustomDualListEditDialog }

const

	EDIT_CONTROL_CLASS_MAP: array[TKEditMaskType] of TKCustomSpeedControlClass =
	(
		TKFormattedSpeedControl,  { emtFloat   }
		TKCustomSpeedInteger,     { emtInteger }
		TKCustomSpeedHexa,        { emtHexa    }
		TKCustomSpeedDate,        { emtDate    }
		TKCustomSpeedTime,        { emtTime    }
		TKCustomSpeedText,        { emtText    }
		TKCustomSpeedFile,        { emtFile    }
		TKCustomSpeedFolder,      { emtFolder  }
		TKCustomSpeedMask         { emtCustom  }
	);

constructor TKCustomDualListEditDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FEditShortCut := SC_CTRL_RETURN;
	FClearShortCut := SC_CTRL_DELETE;
end;

function TKCustomDualListEditDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKCustomDualListEditDialogForm;
end;

procedure TKCustomDualListEditDialog.NotifyListsChange( sl: TStrings );
begin
	if CheckStrContains( FSourceList.Text, CH_EQUAL_TOKEN ) then
		uksyUtils.RaiseException( EKDualListEditDialog, sErrDLEInvSourceList );
	inherited NotifyListsChange( sl );
end;

{ TKDualListEditDialogForm }

type

	TKDualListEditDialogForm = class( TKCustomDualListEditDialogForm )
	protected
		procedure PlaceControls; override;

		function GetControl: TKCustomSpeedText;
		procedure ClearEditControlText; override;
		function GetEditControlText: string; override;
		procedure SetEditControlText( const Value: string ); override;

		property EditControl: TKCustomSpeedText
						 read GetControl;

	end;

procedure TKDualListEditDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	with TKCustomSpeedTextHack( EditControl ) do
	begin
		EditorStyle := esEllipse;
		if Assigned( TKDualListEditDialog( Dialog ).OnEditButtonClick ) then
			OnButtonClick := TKDualListEditDialog( Dialog ).OnEditButtonClick
		else
  		OnButtonClick := EditControlDblClick;
	end;
end;

function TKDualListEditDialogForm.GetControl: TKCustomSpeedText;
begin
	Result := TKCustomSpeedText( inherited EditControl );
end;

function TKDualListEditDialogForm.GetEditControlText: string;
begin
	Result := EditControl.Text;
end;

procedure TKDualListEditDialogForm.ClearEditControlText;
begin
	TKCustomSpeedTextHack( EditControl ).Clear;
end;

procedure TKDualListEditDialogForm.SetEditControlText( const Value: string );
begin
	TKCustomSpeedTextHack( EditControl ).Value := Value
end;

{ TKDualListEditDialog }

function TKDualListEditDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKDualListEditDialogForm;
end;

function TKDualListEditDialog.GetEditControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedText;
end;

function DualListEditDialog( const ACaption: string; ASourceList, ADestList: TStrings;
	ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;
begin
	Result := InternalDualListDialog( TKDualListEditDialog, ACaption, ASourceList,
		ADestList, ASourceSel, ADestSel, MultiSelectLists, nil, nil );
end;

{ TKCustomDualListEditMaskDialogForm }

function TKCustomDualListEditMaskDialogForm.GetDialog: TKCustomDualListEditMaskDialog;
begin
	Result := TKCustomDualListEditMaskDialog( inherited Dialog );
end;

procedure TKCustomDualListEditMaskDialogForm.PlaceControls;
begin
	inherited PlaceControls;
	with EditControl do
	begin
{
		EditMask := Dialog.EditMask;
		Value := Dialog.Text;
}		
	end;
end;

function TKCustomDualListEditMaskDialogForm.GetControl: TKCustomSpeedMask;
begin
	Result := TKCustomSpeedMask( inherited EditControl );
end;

function TKCustomDualListEditMaskDialogForm.GetEditControlText: string;
begin
	if ( not FCleared ) then
		Result := EditControl.Text
	else
		Result := '';
end;

procedure TKCustomDualListEditMaskDialogForm.AddFieldValue;
begin
	if ( not FCleaning ) then
		FCleared := False;
	inherited AddFieldValue;
	FCleared := False;
	FCleaning := False;
end;

procedure TKCustomDualListEditMaskDialogForm.ClearEditControlText;
begin
	FCleaning := True;
	FCleared := True;
	EditControl.Clear;
end;

procedure TKCustomDualListEditMaskDialogForm.SetEditControlText( const Value: string );
begin
	FCleared := False;
	TKCustomSpeedTextHack( EditControl ).Value := Value
end;

{ TKCustomDualListEditMaskDialog }

constructor TKCustomDualListEditMaskDialog.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	CloseOnEscape := False;
end;

function TKCustomDualListEditMaskDialog.GetEditControlClass: TKCustomSpeedControlClass;
begin
	Result := TKCustomSpeedMask;
end;

function TKCustomDualListEditMaskDialog.GetDefaultFormClass: TKCustomDialogFormClass;
begin
	Result := TKCustomDualListEditMaskDialogForm;
end;

procedure TKCustomDualListEditMaskDialog.SetEditMask( const Value: string );
begin
	if ( not CheckStrEqual( FEditMask, Value ) ) then
	begin
		Text := '';
		FEditMask := Value;
		if CheckStr( Value ) then
			FEditMaskType := emtCustom;
	end;
end;

procedure TKCustomDualListEditMaskDialog.SetEditMaskType( Value: TKEditMaskType );
begin
	if ( ( FEditMaskType <> Value ) and ( not ( Executing or Checking ) ) ) then
	begin
		if ( ( Value <> emtCustom ) and CheckStr( FEditMask ) ) then
		begin
			Text := '';
			FEditMask := '';
		end;
		FEditMaskType := Value;
	end;
end;

function InternalDualListEditMaskDialog( Dialog: TKCustomDualListDialog;
	Data: Pointer ): Boolean;
begin
	TKCustomDualListEditMaskDialog( Dialog ).EditMask := PChar( Data );
	Result := Dialog.Execute;
end;

function DualListEditMaskDialog( const ACaption, AEditMask: string; ASourceList,
	ADestList: TStrings; ASourceSel, ADestSel: TBits; MultiSelectLists: Boolean ): Boolean;
begin
	if CheckObject( ADestList ) then
		ADestList.Clear;
	if CheckObject( ADestSel ) then
		ADestSel.Size := 0;
	Result := InternalDualListDialog( TKDualListEditMaskDialog, ACaption, ASourceList,
		ADestList, ASourceSel, ADestSel, MultiSelectLists, InternalDualListEditMaskDialog,
		PChar( AEditMask ) );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure InitDialogDataEx;
var
	f: TFont;
begin
	f := TFont.Create;
	try
		GetSystemCaptionFont( GetDesktopWindow, f );
		AssignFontToFontData( f, DialogDataEx.ActiveFont );
		AssignFontToFontData( f, DialogDataEx.InactiveFont );
		DialogDataEx.ActiveFont.Color := DIALOG_DEFAULT_ACTIVE_FONT_COLOR;
		DialogDataEx.InactiveFont.Color := DIALOG_DEFAULT_INACTIVE_FONT_COLOR;
	finally
		f.Free;
	end;
end;

const
	SAVE_DLG_PROCS: array[0..9] of Pointer =
	  ( nil, nil, nil, nil, nil, nil, nil, nil, nil, nil );

procedure RegisterSystemDialogs;
begin
	SAVE_DLG_PROCS[0] := @ShowDialogProc;
	ShowDialogProc := InternalShowDialog;
	SAVE_DLG_PROCS[1] := @ShowDialogOnTopProc;
	ShowDialogOnTopProc := InternalShowDialogOnTop;
	SAVE_DLG_PROCS[2] := @ExpireShowDialogProc;
	ExpireShowDialogProc := InternalExpireShowDialog;
	SAVE_DLG_PROCS[3] := @ExpireShowDialogOnTopProc;
	ExpireShowDialogOnTopProc := InternalExpireShowDialogOnTop;
	SAVE_DLG_PROCS[4] := @InputDialogProc;
	InputDialogProc := InternalInputDialog;
	SAVE_DLG_PROCS[5] := @PasswordDialogProc;
	PasswordDialogProc := InternalPasswordDialog;
	SAVE_DLG_PROCS[6] := @InputListDialogProc;
	InputListDialogProc := InternalInputListDialog;
	SAVE_DLG_PROCS[7] := @InputListDialogExProc;
	InputListDialogExProc := InternalInputListDialogEx;
	SAVE_DLG_PROCS[8] := @InputCheckListDialogFunc;
	InputCheckListDialogFunc := InputCheckListDialog;
	SAVE_DLG_PROCS[9] := @ProgressDialogProc;
	ProgressDialogProc := InternalProgressDialog;
end;

procedure UnregisterSystemDialogs;
begin
	ShowDialogProc := TKShowDialogFunc( SAVE_DLG_PROCS[0] );
	ShowDialogOnTopProc := TKShowDialogProc( SAVE_DLG_PROCS[1] );
	ExpireShowDialogProc := TKExpireShowDialogFunc( SAVE_DLG_PROCS[2] );
	ExpireShowDialogOnTopProc := TKExpireShowDialogProc( SAVE_DLG_PROCS[3] );
	InputDialogProc := TKInputDialogFunc( SAVE_DLG_PROCS[4] );
	PasswordDialogProc := TKPasswordDialogFunc( SAVE_DLG_PROCS[5] );
	InputListDialogProc := TKInputListDialogFunc( SAVE_DLG_PROCS[6] );
	InputListDialogExProc := TKInputListDialogExFunc( SAVE_DLG_PROCS[7] );
	InputCheckListDialogFunc := TKInputCheckListDialogFunc( SAVE_DLG_PROCS[8] );
	ProgressDialogProc := TKProgressDialogFunc( SAVE_DLG_PROCS[9] );
end;

procedure ClearSavedSystemDlgProcs;
begin
	ZeroMemory( @SAVE_DLG_PROCS, SizeOf( SAVE_DLG_PROCS ) );
end;

procedure CloseShowDlgOnTopList;
var
	obj: TObject;
begin
	while CheckList( ShowDlgOnTopList ) do
	begin
		obj := TObject( ShowDlgOnTopList[0] );
		ShowDlgOnTopList.Delete( 0 );
		obj.Free;
	end;
	FreeClean( ShowDlgOnTopList );
	FreeClean( SysDlgInstance );
{	FreeClean( SysDlgInstanceOnTop ); -> no need! the list will collect it! }
end;

procedure Init;
begin
	InitDialogDataEx;
end;

procedure Done;
begin
  CloseShowDlgOnTopList;
end;

initialization
	Init;

finalization
	Done;

end.
