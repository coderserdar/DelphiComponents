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

{##NI##}

{
	Pendências de botões com menu:
		.pintar o bullet de radio quando o item tiver checked e o groupindex for <> 0;
		.cuidar de break <> mbNone;
}

{##NI##}

unit uksCtrls;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Messages, Classes, Forms, Graphics, Controls, StdCtrls, ExtCtrls,
	Buttons, ComCtrls, Menus, uksyConsts, uksyTypes, uksyShortCuts, uksyUtils,
  uksyClasses, ukrConsts, ukrCtrls, uksConsts;

type

  EKSCtrls = class( EKStd );

{
--------------------------------------------------------------------------------
--------------------------- Generic Graphic Controls ---------------------------
--------------------------------------------------------------------------------
}

{ TKCustomGraphicInvCapControl }

	TKCustomGraphicInvCapControl = class( TGraphicControl )
	protected
		procedure SetCaption( Value: TCaption ); virtual;

		property Caption
						 write SetCaption;

	published
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
	
	end;

{ TKBox }

	TKBox = class( TKCustomGraphicInvCapControl )
	protected
		procedure Paint; override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property Align;
		property Caption;
		property Color;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property ParentBiDiMode;
		property Constraints;
		property DragKind;
	{$ENDIF}
			
	end;

{ TKBevel }

	TKBevel = class( TBevel )
	private
		FBevelWidth: Integer;
		FShadowColor: TColor;
		FHighlightColor: TColor;

		procedure SetBevelWidth( Value: Integer );
		procedure SetShadowColor( Value: TColor );
		procedure SetHighlightColor( Value: TColor );

	protected
		procedure Paint; override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property BevelWidth: Integer
						 read FBevelWidth write SetBevelWidth default 1;
		property HighlightColor: TColor
						 read FHighlightColor write SetHighlightColor default clBtnHighlight;
		property ShadowColor: TColor
						 read FShadowColor write SetShadowColor default clBtnShadow;

	end;

{ TKFocusLabel }

	TKFocusLabel = class( TLabel )
	private
		FOldFont: TFont;
		FFocusedFont: TFont;
		FWindowProc: TWndMethod;

		procedure SetFocusControl( Value: TWinControl );
		procedure SetFocusedFont( Value: TFont );

	protected
		procedure Loaded; override;
		procedure NewWindowProc( var Message: TMessage ); dynamic;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property FocusedFont: TFont
						 read FFocusedFont write SetFocusedFont;
		property FocusControl
						 write SetFocusControl;

	end;

{ TKLight }

	TLightSize = ( lz7x7, lz8x8, lz9x9 );

	TKLight = class( TGraphicControl )
	private
		FColor: TColor;
		FSize: TLightSize;
		FBorderStyle: TBorderStyle;

		procedure SetColor( Value: TColor );
		procedure SetSize( Value: TLightSize );
		procedure SetBorderStyle( Value: TBorderStyle );

	protected
		procedure Paint; override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property BorderStyle: TBorderStyle
						 read FBorderStyle write SetBorderStyle default bsNone;
		property Color: TColor
						 read FColor write SetColor default clLime;
		property Size: TLightSize
						 read FSize write SetSize default lz8x8;

		property DragCursor;
		property DragMode;
		property Enabled;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property ParentBiDiMode;
		property Constraints;
		property DragKind;
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

	end;

{ TKLabel3D }

	TKLabel3D = class( TKCustomLabel3D )
	published
    property Align;
		property Alignment;
		property AutoSize;
		property Caption;
		property Color default clWhite;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property IsFolder;
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

	end;

{ TKPathLabel }

	TKPathLabel = class( TCustomLabel )
	protected
		procedure Paint; override;

	public
		constructor Create( AOwner: TComponent ); override;

	published
		property Align;
		property Alignment;
		property AutoSize;
		property Caption;
		property Color;
		property DragCursor;
		property DragMode;
		property Enabled;
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

	end;

{ TKWebLabel }

	TKWebActionEvent = procedure( Sender: TObject; var Continue: Boolean ) of object;

	TKWebLabel = class( TLabel )
	private
		FOldFont: TFont;
		FActionFont: TFont;
		FOldCursor: TCursor;
		FWebCursor: TCursor;
		FWebAction: TKWebAction;
		FAutoWebAction: Boolean;
		FPrefixProtocol: Boolean;
		FShowWindowStyle: TKShowWindowStyle;

		FAfterWebAction: TNotifyEvent;
		FOnCustomWebAction: TNotifyEvent;
		FBeforeWebAction: TKWebActionEvent;

		procedure SetActionFont( Value: TFont );

		procedure CMMouseEnter( var Message: TMessage );
							message CM_MOUSEENTER;
		procedure CMMouseLeave( var Message: TMessage );
							message CM_MOUSELEAVE;
							
	protected
		procedure Click; override;
		procedure DoWebAction; virtual;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property ActionFont: TFont
						 read FActionFont write SetActionFont;
		property AutoWebAction: Boolean
						 read FAutoWebAction write FAutoWebAction default false;
		property PrefixProtocol: Boolean
						 read FPrefixProtocol write FPrefixProtocol default true;
		property ShowWindowStyle: TKShowWindowStyle
						 read FShowWindowStyle write FShowWindowStyle default ssNormal;

		property WebAction: TKWebAction
						 read FWebAction write FWebAction default waHTTP;

		property OnCustomWebAction: TNotifyEvent
						 read FOnCustomWebAction write FOnCustomWebAction;
		property BeforeWebAction: TKWebActionEvent
						 read FBeforeWebAction write FBeforeWebAction;
		property AfterWebAction: TNotifyEvent
						 read FAfterWebAction write FAfterWebAction;

	end;

{
--------------------------------------------------------------------------------
---------------------------- Knowhow Sponsor Panel -----------------------------
--------------------------------------------------------------------------------
}

  EKSponsorPanel = class( EKSCtrls );

  TKSponsorImage = class;
  TKSponsorImages = class;
  TKSponsorPanel = class;

{ TKSponsorImage }

  TKSponsorImage = class( TKCustomCollectionItem )
  private
    FCenter: Boolean;
    FStrech: Boolean;
    FTransparent: Boolean;
    FIncrementalDisplay: Boolean;
    FPicture: TPicture;
    FOnProgress: TProgressEvent;

    function GetImage: TImage;
    function GetBoolOpt( Index: Integer ): Boolean;
    procedure SetBoolOpt( Index: Integer; Value: Boolean );
    procedure SetPicture( Value: TPicture );

    function GetOwnerCollection: TKSponsorImages;

  protected
    procedure DoProgress( Stage: TProgressStage; PercentDone: Byte;
      RedrawNow: Boolean; const R: TRect; const Msg: string ); dynamic;

  public
    destructor Destroy; override;
    constructor Create( ACollection: TCollection ); override;

    procedure Assign( Source: TPersistent ); override;
    function Equals( Item: TKCustomCollectionItem ): Boolean; override;

    property Owner: TKSponsorImages
             read GetOwnerCollection;
    property Image: TImage
             read GetImage;

  published
    property Name;

    property Center: Boolean
             index 0 read GetBoolOpt write SetBoolOpt default True;
    property IncrementalDisplay: Boolean
             index 1 read GetBoolOpt write SetBoolOpt default False;
    property Strech: Boolean
             index 2 read GetBoolOpt write SetBoolOpt default False;
    property Transparent: Boolean
             index 3 read GetBoolOpt write SetBoolOpt default False;
    property Picture: TPicture
             read FPicture write SetPicture;
    property OnProgress: TProgressEvent
             read FOnProgress write FOnProgress;
             
  end;

{ TKSponsorImages }

  TKSponsorImages = class( TKCustomCollection ) 
  private
    function GetOwnerComp: TKSponsorPanel;

    procedure SetItem( Index: Integer; AItem: TKSponsorImage );
    function GetItem( Index: Integer ): TKSponsorImage;

    function GetItemByName( const AName: string ): TKSponsorImage;

  protected
    procedure Update( Item: TCollectionItem ); override;

    property Component: TKSponsorPanel 
             read GetOwnerComp;

  public
    constructor Create( AComp: TKSponsorPanel ); virtual;

    function Add: TKSponsorImage; virtual; 
    property Items[Index: Integer]: TKSponsorImage
             read GetItem write SetItem; default;
    property ItemByName[const AName: string]: TKSponsorImage 
             read GetItemByName; 
    property Names;

  end;

{ TKSponsorPanel }

  TKSponsorPanel = class( TCustomPanel )
  private
    FImage: TImage;
    FTimer: TTimer;
    FOldEnabled: Boolean;
    FImages: TKSponsorImages;
    FCurrentImageIdx: Integer;

    function GetActive: Boolean;
    function GetInterval: Integer;
    function GetCurrentImage: TKSponsorImage;
    procedure SetCurrentImageIdx( Value: Integer );
    procedure SetActive( Value: Boolean );
    procedure SetInterval( Value: Integer );
    procedure SetEnabled( Value: Boolean );
    procedure SetImages( Value: TKSponsorImages );

    procedure TimerEvent( Sender: TObject );

    procedure ClickEvent( Sender: TObject );
    procedure DblClickEvent( Sender: TObject );
    procedure MouseUpEvent( Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer );
    procedure MouseDownEvent( Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer );
    procedure MouseMoveEvent( Sender: TObject; Shift: TShiftState; X, Y: Integer );
    procedure StartDragEvent( Sender: TObject; var DragObject: TDragObject );
    procedure EndDragEvent( Sender, Target: TObject; X, Y: Integer );
    procedure DragDropEvent( Sender, Source: TObject; X, Y: Integer );
    procedure DragOverEvent( Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean );
    procedure ProgressEvent( Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string );

  protected
    procedure Loaded; override;
    procedure DefaultHandler( var Message ); override;
    
    procedure UpdateItem( Item: TKSponsorImage ); virtual;

    property PaintImage: TImage
             read FImage;
    property CurrentImageIdx: Integer
             read FCurrentImageIdx write SetCurrentImageIdx;

  public
    destructor Destroy; override;
    constructor Create( AOwner: TComponent ); override;

    property CurrentImage: TKSponsorImage
             read GetCurrentImage;

  published
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled write SetEnabled;
    property FullRepaint;
    property Color;
    property Ctl3D;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;

    property Images: TKSponsorImages
             read FImages write SetImages;
    property Interval: Integer
             read GetInterval write SetInterval default ( SECOND_TO_MSECOND * 3 );
    property Active: Boolean
             read GetActive write SetActive default False;         

  end;

{
--------------------------------------------------------------------------------
-------------------------- Gradient Graphic Controls ---------------------------
--------------------------------------------------------------------------------
}

type

{ TKGradientControl }

	TKGradientControl = class( TGraphicControl )
	private
		FGradient: TKGradient;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Paint; override;

	published
		property Gradient: TKGradient
						 read FGradient write FGradient;

		property Align;
		property Caption;
		property Color;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
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
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnStartDrag;

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

	end;

{ TKGradientLabel }

	TKGradientLabel = class( TKCustomGradientText )
	private
		FAngle: TKAngle;
		FAutoSize: Boolean;

	protected
		procedure Paint; override;

		procedure SetAngle( Value: TKAngle ); virtual;
		procedure SetAutoSize( Value: Boolean ); virtual;
		procedure SetCaption( Value: TCaption ); virtual;
		procedure SetAlign( Value: TAlign ); override;
		procedure SetAlignment( Value: TAlignment ); override;

	public
		constructor Create( AOwner: TComponent ); override;

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
		property Caption
						 write SetCaption;

	end;

{ TKGradientText }

	TKGradientText = class( TKCustomGradientText )
	private
		FLines: TStrings;

		procedure SetLines( Value: TStrings );

	protected
		procedure Paint; override;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

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

		property Lines: TStrings
						 read FLines write SetLines;

	end;

{ TKGradientPanel }
                           
	TKGradientPanel = class( TPanel )
	private
		FBitmap: TBitmap;
		FGradient: TKGradient;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Paint; override;

	published
		property Gradient: TKGradient
						 read FGradient write FGradient;

	end;

{
--------------------------------------------------------------------------------
----------------------------- Generic WinControls ------------------------------
--------------------------------------------------------------------------------
}

{ TKRadioGroup }

	TKRadioGroup = class( TRadioGroup )
	private
		FTitleFont: TFont;
		FColorFocused: TColor;
		FColorUnfocused: TColor;

		procedure SetColorUnfocused( Value: TColor );

		procedure CMExit( var Message: TCMExit );
							message CM_Exit;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMTitleFontChanged( var Message: TMessage );
							message CM_TITLEFONTCHANGED;

	protected
		procedure Paint; override;
		procedure SetTitleFont( Value: TFont );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure TitleFontChanged( Sender: TObject );

	published
		property ColorFocused: TColor
						 read FColorFocused write FColorFocused default clRed;
		property ColorUnfocused: TColor
						 read FColorUnfocused write SetColorUnfocused default clWindowText;
		property TitleFont: TFont
						 read FTitleFont write SetTitleFont;

	end;

{ TKGroupBox }

	TKGroupBox = class( TGroupBox )
	private
		FTitleFont: TFont;
		FColorFocused: TColor;
		FColorUnfocused: TColor;

		procedure SetColorUnfocused( Value: TColor );

		procedure CMExit( var Message: TCMExit );
							message CM_Exit;
		procedure CMEnter( var Message: TCMEnter );
							message CM_Enter;
		procedure CMTitleFontChanged( var Message: TMessage );
							message CM_TITLEFONTCHANGED;

	protected
		procedure Paint; override;
		procedure SetTitleFont( Value: TFont );

	public
		destructor  Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure TitleFontChanged( Sender: TObject );

	published
		property ColorFocused: TColor
						 read FColorFocused write FColorFocused default clRed;
		property ColorUnfocused: TColor
						 read FColorUnfocused write SetColorUnfocused default clWindowText;
		property TitleFont: TFont
						 read FTitleFont write SetTitleFont;

	end;                               

{ TKCustomComboBox }

	TKCustomComboBox = class( TCustomComboBox )
	private
		FFirstDraw: Boolean;
		FDefItemIndex: Integer;

		function GetItems: TStrings;
		procedure SetItems( Value: TStrings );
		function GetSorted: Boolean;
		procedure SetSorted( Value: Boolean );
		function GetStyle: TComboBoxStyle;
		procedure SetDefItemIndex( Value: Integer );

	protected
		procedure Loaded; override;
		procedure SetStyle( Value: TComboBoxStyle ); override;
		procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

		property DefItemIndex: Integer
						 read FDefItemIndex write SetDefItemIndex default -1;
		property Items: TStrings
						 read GetItems write SetItems;
		property Sorted: Boolean
						 read GetSorted write SetSorted;
		property Style: TComboBoxStyle
						 read GetStyle write SetStyle;

	public
		constructor Create( AOwner: TComponent ); override;

	end;

{ TKComboBox }

	TKComboBox = class( TKCustomComboBox )
	published
	{ from TCustomComboBox }
		property Style; {Must be published before Items}
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property DropDownCount;
		property Enabled;
		property Font;
		property ImeMode;
		property ImeName;
		property ItemHeight;
		property Items;
		property MaxLength;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property Sorted;
		property TabOrder;
		property TabStop;
		property Text;
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

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

	{ from TKCustomComboBox }
		property DefItemIndex;

	end;

{ TKHistoryList }

	TKHistoryList = class( TKCustomComboBox )
	private
		FAutoAdd: Boolean;
		FAutoSave: Boolean;
		FSystemName: string;
		FCompanyName: string;
		FRegistryKey: string;
		FSavedItems: TStrings;
		FAddCommand: TShortCut;
		FAddCommandPressed: Boolean;
		FRegistryBaseKey: TKRegistryKey;

	{$HINTS OFF}
		property Style;
	{$HINTS ON}

		procedure WMDestroy( var Message: TMessage );
							message WM_DESTROY;
		procedure CMExit( var Message: TCMExit );
							message CM_EXIT;

	protected
		procedure Loaded; override;
		procedure KeyPress( var Key: Char ); override;
		procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

		function GetRegistryKey: string; virtual;
		function GetRegistrySection: string; virtual;
		procedure SetRegistryKey( const Value: string ); virtual;

		property AddCommandPressed: Boolean
						 read FAddCommandPressed;

	{##FNS##}

		property RegistryBaseKey: TKRegistryKey
						 read FRegistryBaseKey write FRegistryBaseKey default rkCurrentUser;

	{##FNS##}

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;
		procedure SaveHistoryList; dynamic;
		procedure ClearHistoryList; dynamic;
		procedure DeleteHistoryList; dynamic;
		procedure ReloadHistoryList; dynamic;
		function AddToHistoryList( const AText: string ): Integer;
		function AddObjectToHistoryList( const AText: string; AObject: TObject ): Integer;

		property SavedItems: TStrings
						 read FSavedItems;
		property RegistrySection: string
						 read GetRegistrySection;

	published
	{ from TCustomComboBox }
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property DropDownCount;
		property Enabled;
		property Font;
		property ImeMode;
		property ImeName;
		property ItemHeight;
		property Items;
		property MaxLength;
		property ParentColor;
		property ParentCtl3D;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property Sorted;
		property TabOrder;
		property TabStop;
		property Text;
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

	{$IFDEF DELPHI4}
		property OnStartDock;
		property OnEndDock;
	{$ENDIF}

	{ from TKCustomComboBox }
		property DefItemIndex;

		property AutoAdd: Boolean
						 read FAutoAdd write FAutoAdd default True;
		property AutoSave: Boolean
						 read FAutoSave write FAutoSave default False;
		property AddCommand: TShortCut
						 read FAddCommand write FAddCommand default SC_RETURN;
		property CompanyName: string
						 read FCompanyName write FCompanyName;
		property SystemName: string
						 read FSystemName write FSystemName;
		property RegistryKey: string
						 read GetRegistryKey write SetRegistryKey;

	end;

{ TKSpeedButton }

	TKSpeedButton = class( TKCustomSpeedButton )
	published
		property Caption;
		property Color;
		property DragCursor;
		property DragMode;
		property Enabled;
		property Font;
		property ParentColor;
		property ParentFont;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property Visible;

	{$IFDEF DELPHI4}
		property Anchors;
		property BiDiMode;
		property Constraints;
		property DragKind;
		property ParentBiDiMode;
	{$ENDIF}

		property ButtonStyle;
		property Glyph;
		property TimerEnabled;

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

		property OnDownClick;
		property OnDrawButton;
		property OnUpClick;

	end;

{ TKSpeedText }

	TKSpeedText = class( TKCustomSpeedText )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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

{ New properties/events }
		property Alignment;
		property ButtonHint;
		property ButtonGlyph;
		property ButtonWidth;
		property EditorEnabled;
		property EditorStyle;
		property NullCommand;
		property ButtonCommand;
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnCheckValue;

	end;

{ TKSpeedFolder }

	TKSpeedFolder = class( TKCustomSpeedFolder )
	public
		property IsNull;
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
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property MultiSelect;
		property Title;
		property Value;
		property AllowNull;
    property InitialDirAsLastDir;
    
		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnCheckValue;

	end;

{ TKSpeedFile }

	TKSpeedFile = class( TKCustomSpeedFile )
	public
		property IsNull;
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
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property MultiSelect;
		property Title;
		property Value;
		property AllowNull;
    property InitialDirAsLastDir;
    
		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnCheckValue;

	end;

{ TKSpeedFloat }

	TKSpeedFloat = class( TKFormattedSpeedControl )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKSpeedInteger }

	TKSpeedInteger = class( TKCustomSpeedInteger )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKSpeedHexa }

	TKSpeedHexa = class( TKCustomSpeedHexa )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;

	end;

{ TKSpeedDateTime }

	TKSpeedDateTime = class( TKCustomSpeedDateTime )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;
		property TimeIncrement;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKSpeedDate }

	TKSpeedDate = class( TKCustomSpeedDate )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{ TKSpeedTime }

	TKSpeedTime = class( TKCustomSpeedTime )
	public
		property IsNull;

	published
		property AutoSelect;
		property AutoSize;
		property BorderStyle;
		property CharCase;
		property Color;
		property Ctl3D;
		property DragCursor;
		property DragMode;
		property Enabled;
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
		property ReadOnly;
		property ShowHint;
		property TabOrder;
		property TabStop;
		property Text;
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
		property Value;
		property AllowNull;

		property OnButtonClick;
		property OnDownClick;
		property OnUpClick;
		property OnDrawButton;
		property OnFormatDisplayText;
		property OnFormatEditText;

	end;

{
--------------------------------------------------------------------------------
---------------------- TabStops Enabled MultiLine Editors ----------------------
--------------------------------------------------------------------------------
}

{ TKMemo }

	TKMemo = class( TMemo )
	private
		FTabStops: TStrings;
		procedure SetTabStops( Value: TStrings );
		procedure TabStopsChange( Sender: TObject );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property TabStops: TStrings
						 read FTabStops write SetTabStops;

	end;

{ TKRichEdit }

	TKRichEdit = class( TRichEdit )
	private
		FTabStops: TStrings;

		procedure SetTabStops( Value: TStrings );
		procedure TabStopsChange( Sender: TObject );

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

	published
		property TabStops: TStrings
						 read FTabStops write SetTabStops;

	end;

{
--------------------------------------------------------------------------------
----------------------------- Popup Enabled Buttons ----------------------------
--------------------------------------------------------------------------------
}

{ TKRollButton }

	TKRollStyle = ( rsAbove, rsBelow, rsRight );

	TKRollButton = class( TButton )
	private
		FActionFont: TFont;
		FRollMenu: TPopupMenu;
		FRollStyle: TKRollStyle;
		FRollMenuOnClick: Boolean;

		FOldFont: TFont;
		FActiveFlag: Boolean;

		procedure SetActionFont( Value: TFont );
		procedure SetRollMenu( Value: TPopupMenu );

		procedure CMExit( var Message: TMessage );
							message CM_EXIT;
		procedure CMCancelMode( var Message: TCMCancelMode );
							message CM_CANCELMODE;
		procedure CMMouseEnter( var Message: TMessage );
							message CM_MOUSEENTER;
		procedure CMMouseLeave( var Message: TMessage );
							message CM_MOUSELEAVE;
		procedure WMCommand( var Message: TWMCommand );
							message WM_COMMAND;
		procedure WMMeasureItem( var Msg: TWMMeasureItem );
							message WM_MEASUREITEM;
		procedure WMDrawItem( var Msg: TWMDrawItem );
							message WM_DRAWITEM;

	protected
		procedure SetCustomPopupPainting; dynamic;
		procedure VisibleActive( IsActive: Boolean ); dynamic;
		procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

		property RollMenu: TPopupMenu
						 read FRollMenu write SetRollMenu;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Click; override;
		procedure AssignRollMenu( Value: TPopupMenu ); dynamic;

	published
		property ActionFont: TFont
						 read FActionFont write SetActionFont;
		property RollMenuOnClick: Boolean
						 read FRollMenuOnClick write FRollMenuOnClick default false;
		property RollStyle: TKRollStyle
						 read FRollStyle write FRollStyle default rsBelow;

	end;

{ TKRollBitBtn }

	TKRollBitBtn = class( TBitBtn )
	private
		FActionFont: TFont;
		FRollMenu: TPopupMenu;
		FRollStyle: TKRollStyle;
		FRollMenuOnClick: Boolean;

		FOldFont: TFont;
		FActiveFlag: Boolean;

		procedure SetActionFont( Value: TFont );
		procedure SetRollMenu( Value: TPopupMenu );

		procedure CMExit( var Message: TMessage );
							message CM_EXIT;
		procedure CMCancelMode( var Message: TCMCancelMode );
							message CM_CANCELMODE;
		procedure CMMouseEnter( var Message: TMessage );
							message CM_MOUSEENTER;
		procedure CMMouseLeave( var Message: TMessage );
							message CM_MOUSELEAVE;
		procedure WMCommand( var Message: TWMCommand );
							message WM_COMMAND;
		procedure WMMeasureItem( var Msg: TWMMeasureItem );
							message WM_MEASUREITEM;
		procedure WMDrawItem( var Msg: TWMDrawItem );
							message WM_DRAWITEM;

	protected
		procedure SetCustomPopupPainting; dynamic;
		procedure VisibleActive( IsActive: Boolean ); dynamic;
		procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

		property RollMenu: TPopupMenu
						 read FRollMenu write SetRollMenu;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Click; override;
		procedure AssignRollMenu( Value: TPopupMenu ); dynamic;

	published
		property ActionFont: TFont
						 read FActionFont write SetActionFont;
		property RollMenuOnClick: Boolean
						 read FRollMenuOnClick write FRollMenuOnClick default false;
		property RollStyle: TKRollStyle
						 read FRollStyle write FRollStyle default rsBelow;

	end;

{ TKRollSpeedButton }

	TKRollSpeedButton = class( TSpeedButton )
	private
		FHandle: THandle;
		FActionFont: TFont;
		FRollMenu: TPopupMenu;
		FRollStyle: TKRollStyle;
		FRollMenuOnClick: Boolean;

		FOldFont: TFont;
		FActiveFlag: Boolean;

		function GetHandle: THandle;
		procedure SetActionFont( Value: TFont );
		procedure SetRollMenu( Value: TPopupMenu );

		procedure CMExit( var Message: TMessage );
							message CM_EXIT;
		procedure CMCancelMode( var Message: TCMCancelMode );
							message CM_CANCELMODE;
		procedure CMMouseEnter( var Message: TMessage );
							message CM_MOUSEENTER;
		procedure CMMouseLeave( var Message: TMessage );
							message CM_MOUSELEAVE;
		procedure WMCommand( var Message: TWMCommand );
							message WM_COMMAND;
		procedure WMMeasureItem( var Msg: TWMMeasureItem );
							message WM_MEASUREITEM;
		procedure WMDrawItem( var Msg: TWMDrawItem );
							message WM_DRAWITEM;

	protected
		procedure SetCustomPopupPainting; dynamic;
		procedure VisibleActive( IsActive: Boolean ); dynamic;
		procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

		procedure HandleNeeded; dynamic;
		procedure MenuProc( var Message: TMessage ); dynamic;

		property Handle: THandle
						 read GetHandle;

		property RollMenu: TPopupMenu
						 read FRollMenu write SetRollMenu;

	public
		destructor Destroy; override;
		constructor Create( AOwner: TComponent ); override;

		procedure Click; override;
		procedure AssignRollMenu( Value: TPopupMenu ); dynamic;

	published
		property ActionFont: TFont
						 read FActionFont write SetActionFont;

		property RollMenuOnClick: Boolean
						 read FRollMenuOnClick write FRollMenuOnClick default false;
		property RollStyle: TKRollStyle
						 read FRollStyle write FRollStyle default rsBelow;

	end;

implementation

{$R crsWeb.res}
{.$R u:\delphi\klib100\source\std100\lib\crsWeb.res

 We try to use this form to D4 complaint but this generates a incomatible
 directory list for other delphis.... sorry...
}

uses
	SysUtils, ShellAPI, Registry, ukrUtils, uksResStr, uksUtils;

{
--------------------------------------------------------------------------------
--------------------------- Generic Graphic Controls ---------------------------
--------------------------------------------------------------------------------
}

{ TKCustomGraphicInvCapControl }

procedure TKCustomGraphicInvCapControl.SetCaption( Value: TCaption );
begin
	inherited Caption := Value;
	Invalidate;
end;

{ TKBox }

constructor TKBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks];
	Width := 22;
	Height := 25;
	Color := clLime;
	Font.Name := 'Arial';
	Font.Style := [fsBold];
end;

procedure TKBox.Paint;
var
	X,
	Y: Integer;
begin
	with Canvas do
	begin
		Font := Self.Font;
		Brush.Color := Color;
		Brush.Style := bsSolid;
		Pen.Color := clBlack;
		Pen.Width := 1;
		Pen.Style := psSolid;
		with ClientRect do
			RoundRect( Left, Top, Right, Bottom, 10, 10 );
		X := ( Width - TextWidth( Caption ) ) div 2;
		Y := ( Height - TextHeight( Caption ) ) div 2;
		TextOut( X, Y, Caption );
	end;
end;

{ TKBevel }

constructor TKBevel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBevelWidth := 1;
	FShadowColor := clBtnShadow;
	FHighlightColor := clBtnHighlight;
end;

procedure TKBevel.SetBevelWidth( Value: Integer );
begin
	if ( FBevelWidth = Value ) then
		Exit;
	if ( Value < 1 ) then
		FBevelWidth := 1
	else if ( Value > 5 ) then
		FBevelWidth := 5
	else
		FBevelWidth := Value;
	Invalidate;
end;

procedure TKBevel.SetShadowColor( Value: TColor );
begin
	if ( FShadowColor <> Value ) then
	begin
		FShadowColor := Value;
		Invalidate;
	end;
end;

procedure TKBevel.SetHighlightColor( Value: TColor );
begin
	if ( FHighlightColor <> Value ) then
	begin
		FHighlightColor := Value;
		Invalidate;
	end;
end;

procedure TKBevel.Paint;
var
	Temp,
	Color1,
	Color2: TColor;

	procedure BevelRect( const R: TRect );
	begin
		with Canvas do
		begin
			Pen.Color := Color1;
			PolyLine( [Point( R.Left, R.Bottom ), Point( R.Left, R.Top ),
								 Point( R.Right, R.Top )] );
			Pen.Color := Color2;
			PolyLine( [Point( R.Right, R.Top ), Point( R.Right, R.Bottom ),
								 Point( R.Left, R.Bottom )] );
		end;
	end;

	procedure BevelLine( C: TColor; X1, Y1, X2, Y2: Integer );
	begin
		with Canvas do
		begin
			Pen.Color := C;
			MoveTo( X1, Y1 );
			LineTo( X2, Y2 );
		end;
	end;

begin
	with Canvas do
	begin
		Pen.Width := FBevelWidth;
		if ( Style = bsLowered ) then
		begin
			Color1 := FShadowColor;
			Color2 := FHighlightColor;
		end
		else
		begin
			Color1 := FHighlightColor;
			Color2 := FShadowColor;
		end;
		case Shape of
			bsBox:
				BevelRect( Rect( FBevelWidth, FBevelWidth, Width - FBevelWidth, Height - FBevelWidth ) );
			bsFrame:
				begin
					Temp := Color1;
					Color1 := Color2;
					BevelRect( Rect( 2 * FBevelWidth, 2 * FBevelWidth, Width - FBevelWidth,
						Height - FBevelWidth ) );
					Color2 := Temp;
					Color1 := Temp;
					BevelRect( Rect( FBevelWidth, FBevelWidth, Width - 2 * FBevelWidth,
						Height - 2 * FBevelWidth ) );
				end;
			bsTopLine:
				begin
					BevelLine( Color1, 0, FBevelWidth, Width, FBevelWidth );
					BevelLine( Color2, 0, 2 * FBevelWidth, Width, 2 * FBevelWidth );
				end;
			bsBottomLine:
				begin
					BevelLine( Color1, 0, Height - 2 * FBevelWidth,	Width, Height - 2 * FBevelWidth );
					BevelLine( Color2, 0, Height - FBevelWidth, Width, Height - FBevelWidth );
				end;
			bsLeftLine:
				begin
					BevelLine( Color1, FBevelWidth, 0, FBevelWidth, Height );
					BevelLine( Color2, 2 * FBevelWidth, 0, 2 * FBevelWidth, Height );
				end;
			bsRightLine:
				begin
					BevelLine( Color1, Width - 2 * FBevelWidth, 0, Width - 2 * FBevelWidth, Height );
					BevelLine( Color2, Width - FBevelWidth, 0, Width - FBevelWidth, Height );
				end;
		end;
	end;
end;

{ TKFocusLabel }

constructor TKFocusLabel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FWindowProc := nil;
	FOldFont := TFont.Create;
	FFocusedFont := TFont.Create;
	FFocusedFont.Color := clRed;
end;

destructor TKFocusLabel.Destroy;
begin
	SetFocusControl( nil );
	FOldFont.Free;
	FFocusedFont.Free;
	inherited Destroy;
end;

procedure TKFocusLabel.Loaded;
begin
	inherited Loaded;
	if ( not ( csDesigning in ComponentState ) ) then
		SetFocusControl( FocusControl );
end;

procedure TKFocusLabel.NewWindowProc( var Message: TMessage );
begin
	if ( Message.Msg = CM_ENTER ) then
	begin
		FOldFont.Assign( Font );
		Font.Assign( FFocusedFont );
	end
	else if ( Message.Msg = CM_EXIT ) then
		Font.Assign( FOldFont );
	FWindowProc( Message );
end;

procedure TKFocusLabel.SetFocusedFont( Value: TFont );
begin
	FFocusedFont.Assign( Value );
end;

procedure TKFocusLabel.SetFocusControl( Value: TWinControl );
var
	bRunning: Boolean;
begin
	bRunning := ( not ( Loading( Self ) or Designing( Self ) ) );
{ FocusControl changed }
	if bRunning then
	begin
{ Is there a previous FocusControl? If so, restore its original WindowProc. }
		if ( CheckPointer( @FWindowProc ) and CheckObject( FocusControl ) ) then
		begin
			FocusControl.WindowProc := FWindowProc;
			FWindowProc := nil;
		end;
{ Is there a new FocusControl? If so, change its WindowProc. }
		if CheckObject( Value ) then
		begin
			FWindowProc := Value.WindowProc;
			Value.WindowProc := NewWindowProc;
		end;
	end;
	inherited FocusControl := Value;
end;

{ TKLight }

const
	Pixels7by7: Array[1..7, 1..7] of Byte =
	( ( 0, 0, 2, 2, 2, 0, 0 ),
		( 0, 2, 1, 1, 1, 3, 0 ),
		( 2, 1, 4, 1, 1, 1, 3 ),
		( 2, 1, 4, 1, 1, 1, 3 ),
		( 2, 1, 1, 1, 1, 1, 3 ),
		( 0, 2, 1, 1, 1, 3, 0 ),
		( 0, 0, 3, 3, 3, 0, 0 ) );

	Pixels8by8: Array[1..8, 1..8] of Byte =
	( ( 0, 0, 2, 2, 2, 2, 0, 0 ),
		( 0, 2, 1, 1, 1, 1, 3, 0 ),
		( 2, 1, 1, 1, 1, 1, 1, 3 ),
		( 2, 1, 4, 4, 1, 1, 1, 3 ),
		( 2, 1, 4, 1, 1, 1, 1, 3 ),
		( 2, 1, 1, 1, 1, 1, 1, 3 ),
		( 0, 2, 1, 1, 1, 1, 3, 0 ),
		( 0, 0, 3, 3, 3, 3, 0, 0 ) );

	Pixels9by9: Array[1..9, 1..9] of Byte =
	( ( 0, 0, 2, 2, 2, 2, 2, 0, 0 ),
		( 0, 2, 1, 1, 1, 1, 1, 3, 0 ),
		( 2, 1, 1, 4, 1, 1, 1, 1, 3 ),
		( 2, 1, 4, 4, 1, 1, 1, 1, 3 ),
		( 2, 1, 4, 1, 1, 1, 1, 1, 3 ),
		( 2, 1, 1, 1, 1, 1, 1, 1, 3 ),
		( 2, 1, 1, 1, 1, 1, 1, 1, 3 ),
		( 0, 2, 1, 1, 1, 1, 1, 3, 0 ),
		( 0, 0, 3, 3, 3, 3, 3, 0, 0 ) );

constructor TKLight.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	ControlStyle := [ csCaptureMouse, csClickEvents, csDoubleClicks ];
	Width := 15;
	Height := 15;
	FSize := lz8x8;
	FColor := clLime;
	FBorderStyle := bsNone;
end;

procedure TKLight.SetColor( Value: TColor );
begin
	if ( Value <> FColor ) then
	begin
		FColor := Value;
		Invalidate;
	end;
end;

procedure TKLight.SetSize( Value: TLightSize );
begin
	if ( FSize <> Value ) then
	begin
		FSize := Value;
		Invalidate;
	end;
end;

procedure TKLight.SetBorderStyle( Value: TBorderStyle );
begin
	if ( FBorderStyle <> Value ) then
	begin
		FBorderStyle := Value;
		Invalidate;
	end;
end;

procedure TKLight.Paint;
var
	i,
	j,
	X,
	Y,
	iDim: Integer;
begin
	case FSize of
		lz7x7: iDim := 7;
		lz8x8: iDim := 8
	else
		iDim := 9; 
	end;

	X := ( Width - iDim ) div 2;
	Y := ( Height - iDim ) div 2;

	Canvas.Brush.Style := bsClear;

	if ( FBorderStyle = bsSingle ) then
		with Canvas do
		begin
			Pen.Color := clBlack;
			with ClientRect do
				Rectangle( Left, Top, Right, Bottom );
		end;

	for i := 1 to iDim do
		for j := 1 to iDim do
			with Canvas do
				case iDim of
					7: case Pixels7by7[i, j] of
							 1: Pixels[X + i - 1, Y + j - 1] := FColor;
							 2: Pixels[X + i - 1, Y + j - 1] := clBlack;
							 3: Pixels[X + i - 1, Y + j - 1] := clGray;
							 4: Pixels[X + i - 1, Y + j - 1] := clWhite;
						 end;
					8: case Pixels8by8[i, j] of
							 1: Pixels[X + i - 1, Y + j - 1] := FColor;
							 2: Pixels[X + i - 1, Y + j - 1] := clBlack;
							 3: Pixels[X + i - 1, Y + j - 1] := clGray;
							 4: Pixels[X + i - 1, Y + j - 1] := clWhite;
						 end;
					9: case Pixels9by9[i, j] of
							 1: Pixels[X + i - 1, Y + j - 1] := FColor;
							 2: Pixels[X + i - 1, Y + j - 1] := clBlack;
							 3: Pixels[X + i - 1, Y + j - 1] := clGray;
							 4: Pixels[X + i - 1, Y + j - 1] := clWhite;
						 end;
				end;
end;

{ TKPathLabel }

constructor TKPathLabel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	AutoSize := False;
	Width := 85;
end;

procedure TKPathLabel.Paint;
const
	Alignments: array[TAlignment] of Word = ( DT_LEFT, DT_RIGHT, DT_CENTER );
var
	Rect: TRect;
	OldFont: HGDIOBJ;
begin
	with Canvas do
	begin
		Rect := ClientRect;
		if ( not Transparent ) then
		begin
			Brush.Color := Self.Color;
			Brush.Style := bsSolid;
			FillRect( Rect );
		end;
		Brush.Style := bsClear;
		Font := Self.Font;
		OldFont := SelectObject( Handle, Font.Handle );
		try
			DrawTextEx( Handle, PChar( Caption ), Length( Caption ), Rect,
				DT_PATH_ELLIPSIS or DT_NOPREFIX or Alignments[Alignment], nil );
		finally
			SelectObject( Handle, OldFont );
		end;
	end;
end;

{ TKWebLabel }

constructor TKWebLabel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FWebAction := waHTTP;
	FAutoWebAction := false;
	FPrefixProtocol := true;
	FShowWindowStyle := ssNormal;
	FOldFont := TFont.Create;
	FActionFont := TFont.Create;
	FActionFont.Assign( Font );
	with FActionFont do
	begin
		Style := [fsUnderline];
		Color := clBlue;
	end;
	Screen.Cursors[iWLCursorID] := LoadCursor( HInstance, sWLCursorName );
	FWebCursor := iWLCursorID;
end;

destructor TKWebLabel.Destroy;
begin
	FOldFont.Free;
	FActionFont.Free;
	inherited Destroy;
end;

procedure TKWebLabel.SetActionFont( Value: TFont );
begin
	FActionFont.Assign( Value );
end;

procedure TKWebLabel.Click;
begin
	inherited Click;
	if FAutoWebAction then
		DoWebAction;
end;

procedure TKWebLabel.DoWebAction;
var
	sCommand: string;
	bContinue: Boolean;
begin
	bContinue := true;
	if Assigned( FBeforeWebAction ) then
		FBeforeWebAction( Self, bContinue );
	if bContinue then
	begin
		if ( FWebAction = waCustom ) and Assigned( FOnCustomWebAction ) then
			FOnCustomWebAction( Self )
		else
		begin
{
	If the WebAction is custom and there is no event associated,
	then try to execute independently
}
			sCommand := Caption;
			if FPrefixProtocol then
				sCommand := WEB_PROTOCOL_NAME[FWebAction] + sCommand;
			PerformWebAction( sCommand, waCustom, FShowWindowStyle );
		end;
		if Assigned( FAfterWebAction ) then
			FAfterWebAction( Self );
	end;
end;

procedure TKWebLabel.CMMouseEnter( var Message: TMessage );
begin
	FOldCursor := Cursor;
	Cursor := FWebCursor;
	inherited;
	FOldFont.Assign( Font );
	Font := FActionFont;
end;

procedure TKWebLabel.CMMouseLeave( var Message: TMessage );
begin
	Cursor := FOldCursor;
	inherited;
	Font := FOldFont;
end;

{
--------------------------------------------------------------------------------
---------------------------- Knowhow Sponsor Panel -----------------------------
--------------------------------------------------------------------------------
}

{ TKSponsorImage }

constructor TKSponsorImage.Create( ACollection: TCollection );
begin
  ForceObjectClass( ACollection, TKSponsorImages );
  FCenter := True;
  FStrech := False;
  FTransparent := False;
  FIncrementalDisplay := False;
  FPicture := TPicture.Create;
  inherited Create( ACollection );
  ForceObject( Owner.Component );
end;

destructor TKSponsorImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TKSponsorImage.Assign( Source: TPersistent );
begin
  inherited Assign( Source );
  if CheckObjectClass( Source, TKSponsorImage ) then
  begin
    Center := ( Source as TKSponsorImage ).Center;
    IncrementalDisplay := ( Source as TKSponsorImage ).IncrementalDisplay;
    Strech := ( Source as TKSponsorImage ).Strech;
    Transparent := ( Source as TKSponsorImage ).Transparent;
    Picture := ( Source as TKSponsorImage ).Picture;
  end;
end;

function TKSponsorImage.Equals( Item: TKCustomCollectionItem ): Boolean;
begin
  with ( Item as TKSponsorImage ) do
    Result := ( ( inherited Equals( Item ) ) and ( Self.Center = Center ) and
      ( Self.Strech = Strech ) and ( Self.Transparent = Transparent ) and
      ( Self.IncrementalDisplay = IncrementalDisplay ) );
end;

procedure TKSponsorImage.DoProgress( Stage: TProgressStage; PercentDone: Byte;
  RedrawNow: Boolean; const R: TRect; const Msg: string );
begin
  if Assigned( FOnProgress ) then
    FOnProgress( Self, Stage, PercentDone, RedrawNow, R, Msg );
end;

function TKSponsorImage.GetImage: TImage;
begin
  Result := Owner.Component.PaintImage;
end;

function TKSponsorImage.GetOwnerCollection: TKSponsorImages;
begin
  Result := TKSponsorImages( inherited GetOwnerCollection );
end;

function TKSponsorImage.GetBoolOpt( Index: Integer ): Boolean;
begin
  case Index of
    0: Result := FCenter;
    1: Result := FIncrementalDisplay;
    2: Result := FStrech;
    3: Result := FTransparent;
  else
    Result := False;
  end;
end;

procedure TKSponsorImage.SetBoolOpt( Index: Integer; Value: Boolean );
begin
  case Index of
    0: FCenter := Value;
    1: FIncrementalDisplay := Value;
    2: FStrech := Value;
    3: FTransparent := Value;
  end;
end;

procedure TKSponsorImage.SetPicture( Value: TPicture );
begin
  FPicture.Assign( Value );
end;

{ TKSponsorImages }

constructor TKSponsorImages.Create( AComp: TKSponsorPanel );
begin
  ForceObject( AComp );
  inherited Create( AComp, TKSponsorImage, False ); 
end; 

function TKSponsorImages.Add: TKSponsorImage;
begin
  Result := TKSponsorImage( inherited Add ); 
end; 

function TKSponsorImages.GetItem( Index: Integer ): TKSponsorImage;
begin
  Result := TKSponsorImage( inherited GetItem( Index ) );
end;

function TKSponsorImages.GetItemByName( const AName: string ): TKSponsorImage;
begin
  Result := TKSponsorImage( inherited GetItemByName( AName ) ); 
end; 

function TKSponsorImages.GetOwnerComp: TKSponsorPanel;
begin
  Result := TKSponsorPanel( inherited GetOwnerComp ); 
end; 

procedure TKSponsorImages.SetItem( Index: Integer; AItem: TKSponsorImage );
begin
  inherited SetItem( Index, AItem );
end; 

procedure TKSponsorImages.Update( Item: TCollectionItem ); 
var
  i: Integer;
begin
  if CheckObject( Item ) then
    Component.UpdateItem( TKSponsorImage( Item ) ) 
  else
    for i := 0 to Count - 1 do
      Component.UpdateItem( Items[i] );
end; 

{ TKSponsorPanel }

constructor TKSponsorPanel.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  Text := '';
  ControlStyle := ( ( inherited ControlStyle ) - [csAcceptsControls] );
  FImages := TKSponsorImages.Create( Self );
  FImage := TImage.Create( Self );

  { map image events to panel events }
  FImage.OnClick := ClickEvent;
  FImage.OnDblClick := DblClickEvent;
  FImage.OnMouseUp := MouseUpEvent;
  FImage.OnMouseDown := MouseDownEvent;
  FImage.OnMouseMove := MouseMoveEvent;
  FImage.OnStartDrag := StartDragEvent;
  FImage.OnEndDrag := EndDragEvent;
  FImage.OnDragDrop := DragDropEvent;
  FImage.OnDragOver := DragOverEvent;
  FImage.OnProgress := ProgressEvent;

  FTimer := TTimer.Create( nil );
  FTimer.Enabled := False;
  FOldEnabled := False;
  FTimer.Interval := ( SECOND_TO_MSECOND * 3 );
  if ( not Designing( Self ) ) then
  begin
    FImage.Parent := Self;
    FImage.Align := alClient;
    FTimer.OnTimer := TimerEvent;
  end;
end;

destructor TKSponsorPanel.Destroy;
begin
  FImage.Free;
  FTimer.Free;
  FreeClean( FImages );
  inherited Destroy;
end;

procedure TKSponsorPanel.DefaultHandler( var Message );
begin
  if ( TMessage( Message ).Msg = WM_SETTEXT ) then
    TMessage( Message ).LParam := 0;
  inherited DefaultHandler( Message );
end;

procedure TKSponsorPanel.Loaded;
begin
  inherited Loaded;
  Text := '';
  CurrentImageIdx := -1;
end;

procedure TKSponsorPanel.TimerEvent( Sender: TObject );
begin
  FTimer.Enabled := False;
  try
    CurrentImageIdx := ( CurrentImageIdx + 1 );
  finally
    FTimer.Enabled := True;
  end;
end;

procedure TKSponsorPanel.ProgressEvent( Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string );
var
  si: TKSponsorImage;
begin
  si := CurrentImage;
  if CheckObject( si ) then
    si.DoProgress( Stage, PercentDone, RedrawNow, R, Msg );
end;

procedure TKSponsorPanel.ClickEvent( Sender: TObject );
begin
  Click;
end;

procedure TKSponsorPanel.DblClickEvent( Sender: TObject );
begin
  DblClick;
end;

procedure TKSponsorPanel.MouseUpEvent( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer );
begin
  MouseUp( Button, Shift, X, Y );
end;

procedure TKSponsorPanel.MouseDownEvent( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer );
begin
  MouseDown( Button, Shift, X, Y );
end;

procedure TKSponsorPanel.MouseMoveEvent( Sender: TObject; Shift: TShiftState; X,
  Y: Integer );
begin
  MouseMove( Shift, X, Y );
end;

procedure TKSponsorPanel.StartDragEvent( Sender: TObject;
  var DragObject: TDragObject );
begin
  DoStartDrag( DragObject );
end;

procedure TKSponsorPanel.EndDragEvent( Sender, Target: TObject; X, Y: Integer );
begin
  DoEndDrag( Target, X, Y );
  Enabled := FOldEnabled;
end;

procedure TKSponsorPanel.DragDropEvent( Sender, Source: TObject; X, Y: Integer );
begin
  DragDrop( Source, X, Y );
  Enabled := FOldEnabled;
end;

procedure TKSponsorPanel.DragOverEvent( Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean );
begin
  DragOver( Source, X, Y, State, Accept );
  FOldEnabled := Enabled;
  Enabled := ( not Accept );
end;

function TKSponsorPanel.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TKSponsorPanel.GetInterval: Integer;
begin
  Result := FTimer.Interval;
end;

function TKSponsorPanel.GetCurrentImage: TKSponsorImage;
begin
  Result := nil;
  if ( ( FCurrentImageIdx <> -1 ) and CheckCollection( Images ) ) then
    Result := Images.Items[FCurrentImageIdx];
end;

procedure TKSponsorPanel.SetImages( Value: TKSponsorImages );
begin
  FImages.Assign( Value );
end;

procedure TKSponsorPanel.SetEnabled( Value: Boolean );
begin
  SetActive( FTimer.Enabled and Value ); { Active := ( Active and Value ); }
  inherited Enabled := Value; 
end;

procedure TKSponsorPanel.SetActive( Value: Boolean );
begin
  if Value then
    Enabled := Value;
  FTimer.Enabled := ( CheckCollection( Images ) and Value );
  if FTimer.Enabled then
    CurrentImageIdx := 0;
end;

procedure TKSponsorPanel.SetInterval( Value: Integer );
begin
  FTimer.Interval := Value;
end;

procedure TKSponsorPanel.SetCurrentImageIdx( Value: Integer );
begin
  if ( Value >= Images.Count ) then
    Value := 0;
  FCurrentImageIdx := Value;
  UpdateItem( CurrentImage );
end;

procedure TKSponsorPanel.UpdateItem( Item: TKSponsorImage );
begin
  if CheckObject( Item ) then
  begin
    FImage.Picture.Assign( Item.Picture );
    FImage.Center := Item.Center;
    FImage.Stretch := Item.Strech;
    FImage.Transparent := Item.Transparent;
    FImage.IncrementalDisplay := Item.IncrementalDisplay;
  end
  else
    FImage.Picture.Assign( nil );  
end;

{
--------------------------------------------------------------------------------
-------------------------- Gradient Graphic Controls ---------------------------
--------------------------------------------------------------------------------
}

{----------------------------- TKGradientControl -------------------------------}


constructor TKGradientControl.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FGradient := TKGradient.Create( Self, nil );
	Width := 150;
	Height := 150;
end;         

destructor TKGradientControl.Destroy;
begin
	FGradient.Free;
	inherited Destroy;
end;

procedure TKGradientControl.Paint;
begin
	if ( FGradient.GradientStyle <> gsNone ) then
	begin
		FGradient.Invalidate;
		Canvas.Draw( 0, 0, FGradient.GradientBmp );
	end
	else
		inherited Paint;
end;

{------------------------------ TKGradientLabel --------------------------------}


constructor TKGradientLabel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAngle := 0;
	FAutoSize := true;
end;

procedure TKGradientLabel.SetAlign( Value: TAlign );
begin
	if ( FAutoSize or ( FAngle <> 0 ) ) then
		inherited SetAlign( alNone ) {Call inherited TControl Align property!}
	else
    inherited SetAlign( Value ); {Call TKCustomGradientText protected method}
end;

procedure TKGradientLabel.SetAngle( Value: TKAngle );
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

procedure TKGradientLabel.SetAutoSize( Value: Boolean );
begin
	if ( FAutoSize <> Value ) then
	begin
		if ( FAngle <> 0 ) then
			FAutoSize := true
		else
			FAutoSize := Value;
		if FAutoSize then
			inherited Align := alNone;
		Invalidate;
	end
end;

procedure TKGradientLabel.SetCaption( Value: TCaption );
begin
	if ( inherited Caption <> Value ) then
	begin
		inherited Caption := Value; 
		Invalidate;
	end;
end;

procedure TKGradientLabel.SetAlignment( Value: TAlignment );
begin
  if ( FAngle = 0 ) then
    inherited SetAlignment( Value )
  else
		inherited SetAlignment( taLeftJustify );
end;

procedure TKGradientLabel.Paint;
begin
	inherited Paint;
	DoPaint( Caption, FAngle, FAutoSize );
end;

{------------------------------ TKGradientText --------------------------------}


constructor TKGradientText.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FLines := TStringList.Create;
	FLines.Add( ClassName );
	Height := 37;
	Width := 210;
end;

destructor TKGradientText.Destroy;
begin
	FLines.Free;
	inherited Destroy;
end;

procedure TKGradientText.SetLines( Value: TStrings );
begin
	if ( FLines <> Value ) then
	begin
		FLines.Assign( Value );
		Invalidate;
	end;
end;

procedure TKGradientText.Paint;
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

{------------------------------ TKGradientPanel --------------------------------}

constructor TKGradientPanel.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FBitmap := TBitmap.Create;
	FGradient := TKGradient.Create( Self, FBitmap );
	Width := 150;
	Height := 150;
end;

destructor TKGradientPanel.Destroy;
begin
	FBitmap.Free;
	FGradient.Free;
	inherited Destroy;
end;

procedure TKGradientPanel.Paint;
var
	Rect: TRect;
	TopColor, BottomColor: TColor;

	procedure AdjustColors( Bevel: TPanelBevel );
	begin
		TopColor := clBtnHighlight;
		if ( Bevel = bvLowered ) then
			TopColor := clBtnShadow;
		BottomColor := clBtnShadow;
		if ( Bevel = bvLowered ) then
			BottomColor := clBtnHighlight;
	end;

var
	rtBMP: TRect;
begin
	Rect := GetClientRect;
	if ( BevelOuter <> bvNone ) then
	begin
		AdjustColors( BevelOuter );
		Frame3D( Canvas, Rect, TopColor, BottomColor, BevelWidth );
	end;
	Frame3D( Canvas, Rect, Color, Color, BorderWidth );
	if ( BevelInner <> bvNone ) then
	begin
		AdjustColors( BevelInner );
		Frame3D( Canvas, Rect, TopColor, BottomColor, BevelWidth );
	end;
{ Draws the Gradient }
	if ( FGradient.GradientStyle <> gsNone ) then
		PaintGradient( FGradient );
{ Draws the Text }
	with FBitmap do
	begin
		Canvas.Brush.Style := bsClear;
		Canvas.Font := Self.Font;
		rtBMP := Classes.Rect( BevelWidth + BorderWidth, BevelWidth + BorderWidth, Width, Height );
		rtBMP.Top := ( ( Height - Canvas.TextHeight( 'Wg' ) ) div 2 );
		rtBMP.Bottom := rtBMP.Top + Canvas.TextHeight( 'Wg' );
		DrawText( Canvas.Handle, PChar( Caption ), -1, rtBMP, DT_EXPANDTABS or
			DT_WORDBREAK or TEXT_ALIGNMENT[Alignment] );
		Canvas.Brush.Style := bsSolid;
	end;
	Canvas.Draw( BevelWidth + BorderWidth, BevelWidth + BorderWidth, FBitmap );
end;

{
--------------------------------------------------------------------------------
----------------------------- Generic WinControls ------------------------------
--------------------------------------------------------------------------------
}

{-------------------------------- TKRadioGroup ---------------------------------}

constructor TKRadioGroup.Create( AOwner: TComponent );
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
end;

destructor TKRadioGroup.Destroy;
begin
	FTitleFont.Free;
	inherited Destroy;
end;

procedure TKRadioGroup.SetTitleFont( Value: TFont );
begin
	FTitleFont.Assign( Value );
end;

procedure TKRadioGroup.SetColorUnfocused( Value: TColor );
begin
	FColorUnfocused := Value;
	FTitleFont.Color := Value;
end;

procedure TKRadioGroup.CMTitleFontChanged( var Message: TMessage );
begin
  inherited;
	Invalidate;
end;

procedure TKRadioGroup.TitleFontChanged( Sender: TObject );
begin
	Perform( CM_TITLEFONTCHANGED, 0, 0 );
end;

procedure TKRadioGroup.Paint;
var
	H: Integer;
	R: TRect;
begin
	with Canvas do
	begin
		Font := FTitleFont;
		H := TextHeight( '0' );
		R := Rect( 0, ( ( H div 2 ) - 1 ), Width, Height );
		if Ctl3D then
		begin
			Inc( R.Left );
			Inc( R.Top );
			Brush.Color := clBtnHighlight;
			FrameRect( R );
			OffsetRect( R, -1, -1 );
			Brush.Color := clBtnShadow;
		end
		else
			Brush.Color := clWindowFrame;
		FrameRect( R );
		if CheckStr( Text ) then
		begin
			R := Rect( 8, 0, 0, H );
			DrawText( Handle, PChar( Text ), Length( Text ), R,
				DT_LEFT or DT_SINGLELINE or	DT_CALCRECT );
			Brush.Color := Color;
			DrawText( Handle, PChar( Text ), Length( Text ), R,
				DT_LEFT or DT_SINGLELINE );
		end;
	end;
end;

procedure TKRadioGroup.CMExit( var Message: TCMExit );
begin
	FTitleFont.Color := FColorUnfocused;
end;

procedure TKRadioGroup.CMEnter( var Message: TCMEnter );
begin
	FTitleFont.Color := FColorFocused;
end;

{-------------------------------- TKGroupBox ---------------------------------}

constructor TKGroupBox.Create( AOwner: TComponent );
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
end;

destructor TKGroupBox.Destroy;
begin
	FTitleFont.Free;
	inherited Destroy;
end;

procedure TKGroupBox.SetTitleFont( Value: TFont );
begin
	FTitleFont.Assign( Value );
end;

procedure TKGroupBox.SetColorUnfocused( Value: TColor );
begin
	FColorUnfocused := Value;
	FTitleFont.Color := Value;
end;

procedure TKGroupBox.CMTitleFontChanged( var Message: TMessage );
begin
	inherited;
	Invalidate;
end;

procedure TKGroupBox.TitleFontChanged( Sender: TObject );
begin
	Perform( CM_TITLEFONTCHANGED, 0, 0 );
end;

procedure TKGroupBox.Paint;
var
	H: Integer;
	R: TRect;
begin
	with Canvas do
	begin
		Font := FTitleFont;
		H := TextHeight( '0' );
		R := Rect( 0, ( ( H div 2 ) - 1 ), Width, Height );
		if Ctl3D then
		begin
			Inc( R.Left );
			Inc( R.Top );
			Brush.Color := clBtnHighlight;
			FrameRect( R );
			OffsetRect( R, -1, -1 );
			Brush.Color := clBtnShadow;
		end
		else
			Brush.Color := clWindowFrame;
		FrameRect( R );
		if CheckStr( Text ) then
		begin
			R := Rect( 8, 0, 0, H );
			DrawText( Handle, PChar( Text ), Length( Text ), R,
				DT_LEFT or DT_SINGLELINE or	DT_CALCRECT );
			Brush.Color := Color;
			DrawText( Handle, PChar( Text ), Length( Text ), R,
				DT_LEFT or DT_SINGLELINE );
		end;
	end;
end;

procedure TKGroupBox.CMExit( var Message: TCMExit );
begin
	FTitleFont.Color := FColorUnfocused;
end;

procedure TKGroupBox.CMEnter( var Message: TCMEnter );
begin
	FTitleFont.Color := FColorFocused;
end;

{------------------------------ TKCustomComboBox -------------------------------}

constructor TKCustomComboBox.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FDefItemIndex := -1;
	FFirstDraw := true;
end;

procedure TKCustomComboBox.Loaded;
begin
	inherited Loaded;
	if CheckStrings( Items ) then
		SendMessage( Handle, CB_SETCURSEL, FDefItemIndex, 0 );
end;

function TKCustomComboBox.GetItems: TStrings;
begin
	Result := inherited Items;
end;

procedure TKCustomComboBox.SetItems( Value: TStrings );
begin
	inherited Items.Assign( Value );
	SetDefItemIndex( FDefItemIndex );
end;

function TKCustomComboBox.GetSorted: Boolean;
begin
	Result := inherited Sorted;
end;

procedure TKCustomComboBox.SetSorted( Value: Boolean );
begin
	inherited Sorted := Value;
	SetDefItemIndex( FDefItemIndex );
end;

function TKCustomComboBox.GetStyle: TComboBoxStyle;
begin
	Result := inherited Style;
end;

procedure TKCustomComboBox.SetStyle( Value: TComboBoxStyle );
begin
	inherited SetStyle( Value );
	SetDefItemIndex( FDefItemIndex );
end;

procedure TKCustomComboBox.SetDefItemIndex( Value: Integer );
begin
	if Designing( Self ) then
	begin
		if ValueBetween( Value, 0, Items.Count - 1, True ) then
			FDefItemIndex := Value
		else
			FDefItemIndex := -1;
		ItemIndex := FDefItemIndex;
	end
	else if Loading( Self ) then
	begin
		FDefItemIndex := Value;
		Exit;
	end;
end;

procedure TKCustomComboBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
begin
	if FFirstDraw then
	begin
		FFirstDraw := false;
		Index := FDefItemIndex;
	end;
	inherited DrawItem( Index, Rect, State );
end;

{-------------------------------- TKHistoryList ---------------------------------}

constructor TKHistoryList.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FAutoAdd := True;
	FAutoSave := False;
	FAddCommandPressed := False;
	FRegistryBaseKey := rkCurrentUser;
	FSavedItems := TStringList.Create;
	if ( not Designing( Self ) ) then
		ReLoadHistoryList;
end;

destructor TKHistoryList.Destroy;
begin
	if ( not Designing( Self ) ) then
	begin
		if FAutoSave and CheckStrings( FSavedItems ) then
			SaveHistoryList;
	end
	else
		DeleteHistoryList;
	FSavedItems.Free;
	inherited Destroy;
end;

procedure TKHistoryList.WMDestroy( var Message: TMessage );
begin
	FSavedItems.Assign( Items );
	inherited;
end;

procedure TKHistoryList.CMExit( var Message: TCMExit );
begin
	inherited;
	if FAutoAdd then
	  AddToHistoryList( Text );
end;

procedure TKHistoryList.Loaded;
begin
	inherited Loaded;
	if CheckStrings( FSavedItems ) then
		Items.AddStrings( FSavedItems );
end;

function TKHistoryList.GetRegistryKey: string;
begin
	if ( not CheckTrimStr( FRegistryKey ) ) then
		Result := KLIBBaseRegKey + ClassName
	else
		Result := FRegistryKey;
end;

procedure TKHistoryList.SetRegistryKey( const Value: string );
begin
	if ( not CheckStrEqual( Value, GetRegistryKey ) ) then
		FRegistryKey := Value;
end;

function TKHistoryList.GetRegistrySection: string;
begin
	Result := HISTORY_LIST_SECTION;
	if CheckTrimStr( SystemName ) then
		Result := Result + CH_DOTMARK + SystemName;
	Result := Result + CH_DOTMARK + GetFirstString( [Name, ClassName] );
end;

procedure TKHistoryList.KeyPress( var Key: Char );
begin
	if FAddCommandPressed then
	begin
		{ Do not hear the Beeeppp. Can be via WM_CHAR, but via KeyPress are more
		  VCL Compliant }
		Key := #0;
		FAddCommandPressed := False;
	end;
end;

procedure TKHistoryList.KeyDown( var Key: Word; Shift: TShiftState );
begin
	if ( ShortCut( Key, Shift ) = AddCommand ) then
	begin
		AddToHistoryList( Text );
		FAddCommandPressed := True;
		Key := 0;
	end;
	inherited KeyDown( Key, Shift );
end;

procedure TKHistoryList.SaveHistoryList;
var
	Reg: TRegIniFile;
begin
	if ( ( not uksyUtils.Destroying( Self ) ) and CheckObject( Parent ) ) then
		FSavedItems.Assign( Items );
	if ( not CheckStrings( FSavedItems ) ) then
		RaiseException( EKSCtrls, sErrHistListInvItems );
	Reg := TRegIniFile.Create( RegistryKey );
	try
		Reg.WriteString( CompanyName, GetRegistrySection, StringReplace( FSavedItems.Text, CH_CRLF,
			CH_PIPE_LINE, krfAll ) );
	finally
		Reg.Free;
	end;
end;

procedure TKHistoryList.DeleteHistoryList;
var
	Reg: TRegIniFile;
begin
	Reg := TRegIniFile.Create( RegistryKey );
	try
		Reg.DeleteKey( CompanyName, GetRegistrySection );
		FSavedItems.Clear;
		if ( ( not uksyUtils.Destroying( Self ) ) and CheckObject( Parent ) ) then
			Items.Clear;
	finally
		Reg.Free;
	end;
end;

procedure TKHistoryList.ClearHistoryList;
var
	Reg: TRegIniFile;
begin
	Reg := TRegIniFile.Create( RegistryKey );
	try
		Reg.WriteString( CompanyName, GetRegistrySection, '' );
		FSavedItems.Clear;
		if ( ( not uksyUtils.Destroying( Self ) ) and CheckObject( Parent ) ) then
			Items.Clear;
	finally
		Reg.Free;
	end;
end;

procedure TKHistoryList.ReloadHistoryList;
var
	s: string;
	Reg: TRegIniFile;
begin
	Reg := TRegIniFile.Create( RegistryKey );
	try
		s := StringReplace( Reg.ReadString( CompanyName, GetRegistrySection, '' ), CH_PIPE_LINE, CH_CRLF, krfAll );
		if CheckStr( s ) then
		begin
			FSavedItems.Text := FSavedItems.Text + s;
			if ( ( not uksyUtils.Destroying( Self ) ) and CheckObject( Parent ) ) then
			begin
				Items.Text := Items.Text + s;
				ItemIndex := DefItemIndex;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

function TKHistoryList.AddToHistoryList( const AText: string ): Integer;
begin
	if ( CheckStr( AText ) and ( Items.IndexOf( AText ) = -1 ) ) then
		Result := Items.Add( AText )
	else
		Result := -1;
end;

function TKHistoryList.AddObjectToHistoryList( const AText: string;
	AObject: TObject ): Integer;
begin
	if ( CheckStr( AText ) and ( Items.IndexOf( AText ) = -1 ) ) then
		Result := Items.AddObject( AText, AObject )
	else
		Result := -1;
end;

{
--------------------------------------------------------------------------------
---------------------- TabStops Enabled MultiLine Editors ----------------------
--------------------------------------------------------------------------------
}

{ TKMemo }

constructor TKMemo.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTabStops := TStringList.Create;
	TStringList( FTabStops ).OnChange := TabStopsChange;
end;

destructor TKMemo.Destroy;
begin
	FTabStops.Free;
	inherited Destroy;
end;

procedure TKMemo.TabStopsChange( Sender: TObject );
begin
	SetControlTabStops( Self, FTabStops );
end;

procedure TKMemo.SetTabStops( Value: TStrings );
begin
	if ( not FTabStops.Equals( Value ) ) then
		FTabStops.Assign( Value );
end;

{ TKRichEdit }

constructor TKRichEdit.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FTabStops := TStringList.Create;
	TStringList( FTabStops ).OnChange := TabStopsChange;
end;

destructor TKRichEdit.Destroy;
begin
	FTabStops.Free;
	inherited Destroy;
end;

procedure TKRichEdit.TabStopsChange( Sender: TObject );
begin
	SetControlTabStops( Self, FTabStops );
end;

procedure TKRichEdit.SetTabStops( Value: TStrings );
begin
	if ( not FTabStops.Equals( Value ) ) then
		FTabStops.Assign( Value );
end;

{
--------------------------------------------------------------------------------
----------------------------- Popup Enabled Buttons ----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

procedure MenuVisibleInfo( AMenu: TMenu; var FirstVisible, VisibleCount: Integer );
var
	i: Integer;
begin
	VisibleCount := 0;
	FirstVisible := -1;
	with AMenu, Items do
		for i := 0 to Count - 1 do
			if Items[i].Visible then
			begin
				Inc( VisibleCount );
				if ( FirstVisible < 0 ) then
					FirstVisible := i;
			end;
end;

{ Parts are extracted from « Delphi 4.0 Menus.pas » for use with any
	popupmenu-enabled button }

procedure LOCAL_DrawText( AItem: TMenuItem; ACanvas: TCanvas; const ACaption: string;
	var Rect: TRect; Selected: Boolean; Flags: Longint );
var
	Text: string;
	R: TRect;
begin
	Text := ACaption;
	if ( Flags and DT_CALCRECT <> 0 ) and ( ( Text = '' ) or
		( Text[1] = '&' ) and ( Text[2] = #0 ) ) then
			Text := Text + ' ';
	with ACanvas do
	begin
		if ( Text = '-' ) then
		begin
			if ( Flags and DT_CALCRECT = 0 ) then
			begin
				R := Rect;
				Inc( R.Top, ( R.Bottom - R.Top ) div 2 );
				DrawEdge( Handle, R, EDGE_ETCHED, BF_TOP );
			end;
		end
		else
		begin
			Brush.Style := bsClear;
			if AItem.Default then
				Font.Style := Font.Style + [fsBold];
			if ( not AItem.Enabled ) then
			begin
				if ( not Selected ) then
				begin
					OffsetRect( Rect, 1, 1 );
					Font.Color := clBtnHighlight;
					DrawText( Handle, PChar( Text ), Length( Text ), Rect, Flags );
					OffsetRect( Rect, -1, -1 );
				end;
				if ( Selected and ( ColorToRGB( GetSystemColor( deHighLight ) ) =
					ColorToRGB( GetSystemColor( deBtnShadow ) ) ) ) then
					Font.Color := GetSystemColor( deBtnHighlight )
				else
					Font.Color := GetSystemColor( deBtnShadow );
			end;
			DrawText( Handle, PChar( Text ), Length( Text ), Rect, Flags );
		end;
	end;
end;

procedure LOCAL_MeasureMenuItem( AItem: TMenuItem; APopup: TPopupMenu;
	ACanvas: TCanvas; var Width, Height: Integer );
const
	Alignments: array[TPopupAlignment] of Word =
	( DT_LEFT, DT_RIGHT, DT_CENTER );
var
	R: TRect;
	Text: string;
	DrawStyle: Integer;
	Alignment: TPopupAlignment;
begin
	Width := 16;
	Height := 16;
	Inc( Width, 15 );
	Inc( Height, 3 );
	FillChar( R, SizeOf( R ), 0 );
	Alignment := APopup.Alignment;
	if ( AItem.ShortCut <> 0 ) then
		Text := Concat( AItem.Caption, ShortCutToText( AItem.ShortCut ) )
	else
		Text := AItem.Caption;
	DrawStyle := Alignments[Alignment] or DT_EXPANDTABS or DT_SINGLELINE or
		DT_NOCLIP or DT_CALCRECT;
	LOCAL_DrawText( AItem, ACanvas, Text, R, False, DrawStyle );
	Inc( Width, R.Right - R.Left + 7 );
//AItem.Tag := Height;
end;

procedure LOCAL_WMMeasureItem( AHandle: THandle; APopup: TPopupMenu; var Msg: TWMMeasureItem );

  function GetMenuFont: HFONT;
	var
		ncm: TNonClientMetrics;
	begin
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
			Result := CreateFontIndirect( ncm.lfMenuFont )
		else
			Result := GetStockObject( SYSTEM_FONT );
	end;

var
	DC: HDC;
	mi: TMenuItem;
	Canvas: TCanvas;
	OldFont: HFONT;
begin
	with Msg.MeasureItemStruct^ do
	begin
		mi := APopup.FindItem( itemID, fkCommand );
		if ( not mi.Visible ) then
		begin
			itemWidth := 0;
			itemHeight := 0;
			Exit;
		end;
		DC := GetWindowDC( AHandle );
		try
			Canvas := TControlCanvas.Create;
			with Canvas do
			try
				OldFont := SelectObject( DC, GetStockObject( SYSTEM_FONT ) );
				try
					Handle := DC;
					Font.Handle := GetMenuFont;
					LOCAL_MeasureMenuItem( mi, APopup, Canvas, Integer( itemWidth ),
						Integer( itemHeight ) );
				finally
					Handle := 0;
					if ( OldFont <> 0 ) then
						SelectObject( DC, OldFont );
				end;
			finally
				Canvas.Free;
			end;
		finally
			ReleaseDC( AHandle, DC );
		end;
	end;
end;

procedure LOCAL_DrawMenuItem( AItem: TMenuItem; APopup: TPopupMenu;
	ACanvas: TCanvas; ARect: TRect; Selected: Boolean );
const
	Alignments: array[TPopupAlignment] of Word =
	( DT_LEFT, DT_RIGHT, DT_CENTER );
	EdgeStyle: array[Boolean] of Longint =
	( BDR_RAISEDINNER, BDR_SUNKENOUTER );
var
	SaveRect,
	GlyphRect: TRect;
	DrawStyle: Longint;
	Alignment: TPopupAlignment;
	OldBrushColor: TColor;
  bmp,
	bmpBGD,
	AGlyph: TBitmap;
begin
	with ACanvas do
	begin
		if ( not Selected ) then
			FillRect( ARect );
		Alignment := APopup.Alignment;
		GlyphRect.Top := ARect.Top + 1;
		GlyphRect.Left := ARect.Left + 1;
		if ( AItem.Caption = '-' ) then
		begin
			FillRect( ARect );
			GlyphRect.Left := 0;
			GlyphRect.Right := -4;
		end
		else
		begin
			GlyphRect.Right := GlyphRect.Left + 16;
			GlyphRect.Bottom := GlyphRect.Top + 16;
			if AItem.Checked then
			begin
				OldBrushColor := Brush.Color;
				if ( not Selected ) then
				begin
					OldBrushColor := Brush.Color;
					FillRect( ARect );
				end
				else
				begin
					Brush.Color := GetSystemColor( deHighLight );
					FillRect( ARect );
				end;
				Brush.Color := OldBrushColor;
				Inc( GlyphRect.Right );
				Inc( GlyphRect.Bottom );
				Inc( GlyphRect.Left );
				Inc( GlyphRect.Top );
				AGlyph := TBitmap.Create;
				try
					AGlyph.Transparent := true;
					AGlyph.Handle := LoadBitmap( 0, PChar( OBM_CHECK ) );
					OldBrushColor := Font.Color;
					Font.Color := GetSystemColor( deBtnText );
					if Selected then
					begin
						bmpBGD := TBitmap.Create;
						try
							bmp := TBitmap.Create;
							try
								bmp.Transparent := true;
								bmp.Width := AGlyph.Width;
								bmp.Height := AGlyph.Height;
	{ Build the Background mask }
								bmpBGD.Width := AGlyph.Width;
								bmpBGD.Height := AGlyph.Height;
								bmpBGD.Canvas.Brush.Color := GetSystemColor( deHighLightText );
								bmpBGD.Canvas.FillRect( Rect( 0, 0, bmpBGD.Width, bmpBGD.Height ) );
	{ Apply the check bitmap to the background mask }
								bmp.Canvas.Draw( 0, 0, bmpBGD );
								if ( bmpBGD.Canvas.Brush.Color <> clWhite ) then
									BitBlt( bmp.Canvas.Handle, 0, 0, AGlyph.Width, AGlyph.Height,
										AGlyph.Canvas.Handle, 0, 0, SRCPAINT )
								else
									BitBlt( bmp.Canvas.Handle, 0, 0, AGlyph.Width, AGlyph.Height,
										AGlyph.Canvas.Handle, 0, 0, NOTSRCCOPY );
								Draw( GlyphRect.Left + ( GlyphRect.Right - GlyphRect.Left - AGlyph.Width ) div 2 + 1,
									GlyphRect.Top + ( GlyphRect.Bottom - GlyphRect.Top - AGlyph.Height ) div 2 + 1,
									bmp );
							finally
								bmp.Free;
							end;
						finally
							bmpBGD.Free;
						end;
					end
					else
						Draw( GlyphRect.Left + ( GlyphRect.Right - GlyphRect.Left - AGlyph.Width ) div 2 + 1,
							GlyphRect.Top + ( GlyphRect.Bottom - GlyphRect.Top - AGlyph.Height ) div 2 + 1,
							AGlyph );
					Font.Color := OldBrushColor;
				finally
					AGlyph.Free;
				end;
				Dec( GlyphRect.Right );
				Dec( GlyphRect.Bottom );
			end;
		end;
		with GlyphRect do
		begin
			Dec( Left );
			Dec( Top );
			Inc( Right, 2 );
			Inc( Bottom, 2 );
		end;
		if Selected then
		begin
			if AItem.Checked then
				ARect.Left := GlyphRect.Right + 1;
			Brush.Color := GetSystemColor( deHighlight );
			FillRect( ARect );
		end;
		ARect.Left := GlyphRect.Right + 1;
		Inc( ARect.Left, 2 );
		Dec( ARect.Right, 1 );
		DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];
{ Calculate vertical layout }
		SaveRect := ARect;
		LOCAL_DrawText( AItem, ACanvas, AItem.Caption, ARect, Selected, DrawStyle or
			DT_CALCRECT or DT_NOCLIP );
		OffsetRect( ARect, 0, ( ( SaveRect.Bottom - SaveRect.Top ) - ( ARect.Bottom -
			ARect.Top ) ) div 2 );
		LOCAL_DrawText( AItem, ACanvas, AItem.Caption, ARect, Selected, DrawStyle );
		if ( AItem.ShortCut <> 0 ) then
		begin
			ARect.Left := ARect.Right;
			ARect.Right := SaveRect.Right - 10;
			LOCAL_DrawText( AItem, ACanvas, ShortCutToText( AItem.ShortCut ), ARect,
				Selected, DT_RIGHT );
		end;
	end;
end;

procedure LOCAL_WMDrawItem( APopup: TPopupMenu; var Msg: TWMDrawItem );

	function GetMenuFont: HFONT;
	var
		ncm: TNonClientMetrics;
	begin
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
			Result := CreateFontIndirect( ncm.lfMenuFont )
		else
			Result := GetStockObject( SYSTEM_FONT );
	end;

var
	mi: TMenuItem;
	OldFont: HFONT;
	Canvas: TCanvas;
begin
	with Msg.DrawItemStruct^ do
	begin
		mi := APopup.FindItem( itemID, fkCommand );
		Canvas := TControlCanvas.Create;
		with Canvas do
		try
			OldFont := SelectObject( hDC, GetStockObject( SYSTEM_FONT ) );
			try
				Handle := hDC;
				Font.Handle := GetMenuFont;
				if ( ( ItemState and ODS_SELECTED ) <> 0 ) then
				begin
					Brush.Color := GetSystemColor( deHighlight );
					Font.Color := GetSystemColor( deHighlightText );
				end
				else
				begin
					Brush.Color := GetSystemColor( deMenu );
					Font.Color := GetSystemColor( deMenuText );
				end;
				LOCAL_DrawMenuItem( mi, APopup, Canvas, rcItem, ( ( ItemState and ODS_SELECTED ) <> 0 ) );
			finally
				Handle := 0;
				if ( OldFont <> 0 ) then
					SelectObject( hDC, OldFont );
			end;
		finally
			Canvas.Free;
		end;
	end;
end;

{---------------------------- Public Implementation ----------------------------}

{ TKRollButton }

constructor TKRollButton.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FRollMenu := TPopupMenu.Create( Self );
	FRollMenu.Name := FRollMenu.ClassName;
	FOldFont := TFont.Create;
	FActionFont := TFont.Create;
	FActionFont.Assign( Font );
	with FActionFont do
	begin
		Style := [fsUnderline];
		Color := clBlue;
	end;
	FActiveFlag := false;
	FRollStyle := rsBelow;
	FRollMenuOnClick := false;
end;

destructor TKRollButton.Destroy;
begin
	FRollMenu.Free;
	FOldFont.Free;
	FActionFont.Free;
	inherited Destroy;
end;

procedure TKRollButton.AssignRollMenu( Value: TPopupMenu );
begin
	RollMenu := Value;
end;

procedure TKRollButton.SetActionFont( Value: TFont );
begin
	FActionFont.Assign( Value );
end;

procedure TKRollButton.SetCustomPopupPainting;
var
	i: Integer;
	mi: TMenuItemInfo;
begin
	for i := 0 to FRollMenu.Items.Count - 1 do
	begin
		mi.cbSize := SizeOf( TMenuItemInfo );
		mi.fMask := MIIM_TYPE;
		mi.fType := MFT_OWNERDRAW;
		SetMenuItemInfo( FRollMenu.Handle, i, true, mi );
	end;
end;

procedure TKRollButton.SetRollMenu( Value: TPopupMenu );
begin
	if ( FRollMenu <> Value ) then
	begin
		AssignMenu( Value, FRollMenu );
		SetCustomPopupPainting;
	end;
end;

procedure TKRollButton.WMMeasureItem( var Msg: TWMMeasureItem );
begin
	inherited;
	LOCAL_WMMeasureItem( Handle, FRollMenu, Msg );
end;

procedure TKRollButton.WMDrawItem( var Msg: TWMDrawItem );
begin
	inherited;
	LOCAL_WMDrawItem( FRollMenu, Msg );
end;

procedure TKRollButton.Click;

	procedure GetMenuSize( var Height, Width: Integer );
	var
		ncm: TNonClientMetrics;
	begin
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
		begin
			Width := ncm.iMenuWidth;
			Height := ncm.iMenuHeight;
		end;
	end;

const
	Flags: array[TPopupAlignment] of Word =
	(
		TPM_LEFTALIGN,
		TPM_RIGHTALIGN,
		TPM_CENTERALIGN
	);
var
	iCount,
	iVisible: Integer;
	pt: TPoint;
	mis: TMeasureItemStruct;
begin
  inherited Click;
	MenuVisibleInfo( FRollMenu, iVisible, iCount );
	if ( iCount > 0 ) and FRollMenuOnClick then
	begin
		pt.X := ClientRect.Left;
		pt.Y := ClientRect.Bottom - 1;
{ Need this call in order to calculate the best position of the rollmenu }
		if ( FRollStyle = rsAbove ) then
		begin
			mis.CtlType := ODT_MENU;
			mis.CtlID := 0;
			mis.itemID := FRollMenu.Items[iVisible].Command;
			mis.itemHeight := 16;
			SendMessage( Handle, WM_MEASUREITEM, 0, Integer( @mis ) );
			pt.Y := -( FRollMenu.Items[iVisible].Tag * iCount ) - 5;
		end
		else if ( FRollStyle = rsRight ) then
		begin
			pt.X := ClientRect.Right;
			pt.Y := ClientRect.Top;
		end;
		pt := ClientToScreen( pt );
		TrackPopupMenu( FRollMenu.Items.Handle, Flags[FRollMenu.Alignment] or TPM_LEFTBUTTON,
			pt.X, pt.Y,	0, Handle, nil );
	end;
end;

procedure TKRollButton.VisibleActive( IsActive: Boolean );
begin
	if IsActive then
	begin
		if ( not FActiveFlag ) then
		begin
			FOldFont.Assign( Font );
			Font := FActionFont;
		end;
		FActiveFlag := true;
	end
	else
	begin
		if FActiveFlag then
			Font := FOldFont;
		FActiveFlag := false;
	end;
end;

procedure TKRollButton.CMExit( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollButton.CMCancelMode( var Message: TCMCancelMode );
begin
	inherited;
	VisibleActive( Message.Sender = Self );
end;

procedure TKRollButton.WMCommand( var Message: TWMCommand );
begin
	inherited;
	FRollMenu.DispatchCommand( Message.ItemID );
end;

procedure TKRollButton.CMMouseEnter( var Message: TMessage );
begin
	inherited;
	VisibleActive( true );
end;

procedure TKRollButton.CMMouseLeave( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollButton.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
	inherited MouseMove( Shift, X, Y );
	VisibleActive( PtInRect( ClientRect, Point( X, Y ) ) );
end;

{ TKRollBitBtn }

constructor TKRollBitBtn.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FRollMenu := TPopupMenu.Create( Self );
	FRollMenu.Name := FRollMenu.ClassName;
	FOldFont := TFont.Create;
	FActionFont := TFont.Create;
	FActionFont.Assign( Font );
	with FActionFont do
	begin
		Style := [fsUnderline];
		Color := clBlue;
	end;
	FActiveFlag := false;
	FRollStyle := rsBelow;
	FRollMenuOnClick := false;
end;

destructor TKRollBitBtn.Destroy;
begin
	FRollMenu.Free;
	FOldFont.Free;
	FActionFont.Free;
	inherited Destroy;
end;

procedure TKRollBitBtn.AssignRollMenu( Value: TPopupMenu );
begin
	RollMenu := Value;
end;

procedure TKRollBitBtn.SetActionFont( Value: TFont );
begin
	FActionFont.Assign( Value );
end;

procedure TKRollBitBtn.SetCustomPopupPainting;
var
	i: Integer;
	mi: TMenuItemInfo;
begin
	for i := 0 to FRollMenu.Items.Count - 1 do
	begin
		mi.cbSize := SizeOf( TMenuItemInfo );
		mi.fMask := MIIM_TYPE;
		mi.fType := MFT_OWNERDRAW;
		SetMenuItemInfo( FRollMenu.Handle, i, true, mi );
	end;
end;

procedure TKRollBitBtn.SetRollMenu( Value: TPopupMenu );
begin
	if ( FRollMenu <> Value ) then
	begin
		AssignMenu( Value, FRollMenu );
		SetCustomPopupPainting;
	end;
end;

procedure TKRollBitBtn.WMMeasureItem( var Msg: TWMMeasureItem );
begin
	inherited;
	LOCAL_WMMeasureItem( Handle, FRollMenu, Msg );
end;

procedure TKRollBitBtn.WMDrawItem( var Msg: TWMDrawItem );
begin
	inherited;
	LOCAL_WMDrawItem( FRollMenu, Msg );
end;

procedure TKRollBitBtn.Click;

	procedure GetMenuSize( var Height, Width: Integer );
	var
		ncm: TNonClientMetrics;
	begin
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
		begin
			Width := ncm.iMenuWidth;
			Height := ncm.iMenuHeight;
		end;
	end;
	
const
	Flags: array[TPopupAlignment] of Word =
	(
		TPM_LEFTALIGN,
		TPM_RIGHTALIGN,
		TPM_CENTERALIGN
	);
var
	iCount,
	iVisible: Integer;
	pt: TPoint;
	mis: TMeasureItemStruct;
begin
	inherited Click;
	MenuVisibleInfo( FRollMenu, iVisible, iCount );
	if ( iCount > 0 ) and FRollMenuOnClick then
	begin
		pt.X := ClientRect.Left;
		pt.Y := ClientRect.Bottom - 1;
{ Need this call in order to calculate the best position of the rollmenu }
		if ( FRollStyle = rsAbove ) then
		begin
			mis.CtlType := ODT_MENU;
			mis.CtlID := 0;
			mis.itemID := FRollMenu.Items[iVisible].Command;
			mis.itemHeight := 16;
			SendMessage( Handle, WM_MEASUREITEM, 0, Integer( @mis ) );
			pt.Y := -( FRollMenu.Items[iVisible].Tag * iCount ) - 5;
		end
		else if ( FRollStyle = rsRight ) then
		begin
			pt.X := ClientRect.Right;
			pt.Y := ClientRect.Top;
		end;
		pt := ClientToScreen( pt );
		TrackPopupMenu( FRollMenu.Items.Handle, Flags[FRollMenu.Alignment] or TPM_LEFTBUTTON,
			pt.X, pt.Y,	0, Handle, nil );
	end;
end;

procedure TKRollBitBtn.WMCommand( var Message: TWMCommand );
begin
	inherited;
	FRollMenu.DispatchCommand( Message.ItemID );
end;

procedure TKRollBitBtn.VisibleActive( IsActive: Boolean );
begin
	if IsActive then
	begin
		if ( not FActiveFlag ) then
		begin
			FOldFont.Assign( Font );
			Font := FActionFont;
		end;
		FActiveFlag := true;
	end
	else
	begin
		if FActiveFlag then
			Font := FOldFont;
		FActiveFlag := false;
	end;
end;

procedure TKRollBitBtn.CMExit( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollBitBtn.CMCancelMode( var Message: TCMCancelMode );
begin
	inherited;
	VisibleActive( Message.Sender = Self );
end;

procedure TKRollBitBtn.CMMouseEnter( var Message: TMessage );
begin
	inherited;
	VisibleActive( true );
end;

procedure TKRollBitBtn.CMMouseLeave( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollBitBtn.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
	inherited MouseMove( Shift, X, Y );
	VisibleActive( PtInRect( ClientRect, Point( X, Y ) ) );
end;

{ TKRollSpeedButton }

constructor TKRollSpeedButton.Create( AOwner: TComponent );
begin
	inherited Create( AOwner );
	FRollMenu := TPopupMenu.Create( Self );
	FRollMenu.Name := FRollMenu.ClassName;
	FOldFont := TFont.Create;
	FActionFont := TFont.Create;
	FActionFont.Assign( Font );
	with FActionFont do
	begin
		Style := [fsUnderline];
		Color := clBlue;
	end;
	FActiveFlag := false;
	FRollStyle := rsBelow;
	FRollMenuOnClick := false;
	FHandle := INVALID_HANDLE_VALUE;
end;

destructor TKRollSpeedButton.Destroy;
begin
	if ( FHandle <> INVALID_HANDLE_VALUE ) then
		DeallocateHWnd( FHandle );
	FRollMenu.Free;
	FOldFont.Free;
	FActionFont.Free;
	inherited Destroy;
end;

procedure TKRollSpeedButton.AssignRollMenu( Value: TPopupMenu );
begin
	RollMenu := Value;
end;

function TKRollSpeedButton.GetHandle: THandle;
begin
	if ( FHandle = INVALID_HANDLE_VALUE ) then
		HandleNeeded;
	Result := FHandle;
end;

procedure TKRollSpeedButton.HandleNeeded;
begin
	FHandle := AllocateHWnd( MenuProc );
end;

procedure TKRollSpeedButton.MenuProc( var Message: TMessage );
begin
	Dispatch( Message );
	if ( Message.Msg = WM_EXITMENULOOP ) then
		VisibleActive( false );
end;

procedure TKRollSpeedButton.SetActionFont( Value: TFont );
begin
	FActionFont.Assign( Value );
end;

procedure TKRollSpeedButton.SetCustomPopupPainting;
var
	i: Integer;
	mi: TMenuItemInfo;
begin
	for i := 0 to FRollMenu.Items.Count - 1 do
	begin
		mi.cbSize := SizeOf( TMenuItemInfo );
		mi.fMask := MIIM_TYPE;
		mi.fType := MFT_OWNERDRAW;
		SetMenuItemInfo( FRollMenu.Handle, i, true, mi );
	end;
end;

procedure TKRollSpeedButton.SetRollMenu( Value: TPopupMenu );
begin
	if ( FRollMenu <> Value ) then
	begin
		AssignMenu( Value, FRollMenu );
		SetCustomPopupPainting;
	end;
end;

procedure TKRollSpeedButton.WMMeasureItem( var Msg: TWMMeasureItem );
begin
	inherited;
	if ( ( Parent <> nil ) and Parent.Visible ) then
		LOCAL_WMMeasureItem( Parent.Handle, FRollMenu, Msg );
end;

procedure TKRollSpeedButton.WMDrawItem( var Msg: TWMDrawItem );
begin
	inherited;
	LOCAL_WMDrawItem( FRollMenu, Msg );
end;

procedure TKRollSpeedButton.Click;

	procedure GetMenuSize( var Height, Width: Integer );
	var
		ncm: TNonClientMetrics;
	begin
		ncm.cbSize := SizeOf( TNonClientMetrics );
		if SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 ) then
		begin
			Width := ncm.iMenuWidth;
			Height := ncm.iMenuHeight;
		end;
	end;

const
	Flags: array[TPopupAlignment] of Word =
	(
		TPM_LEFTALIGN,
		TPM_RIGHTALIGN,
		TPM_CENTERALIGN
	);
var
	iCount,
	iVisible: Integer;
	pt: TPoint;
	mis: TMeasureItemStruct;
begin
	inherited Click;
	MenuVisibleInfo( FRollMenu, iVisible, iCount );
	if ( ( iCount > 0 ) and FRollMenuOnClick ) then
	begin
		pt.X := ClientRect.Left;
		pt.Y := ClientRect.Bottom - 1;
{ Need this call in order to calculate the best position of the rollmenu }
		if ( FRollStyle = rsAbove ) then
		begin
			mis.CtlType := ODT_MENU;
			mis.CtlID := 0;
			mis.itemID := FRollMenu.Items[iVisible].Command;
			mis.itemHeight := 16;
			SendMessage( Handle, WM_MEASUREITEM, 0, Integer( @mis ) );
			pt.Y := -( FRollMenu.Items[iVisible].Tag * iCount ) - 5;
		end
		else if ( FRollStyle = rsRight ) then
		begin
			pt.X := ClientRect.Right;
			pt.Y := ClientRect.Top;
		end;
		pt := ClientToScreen( pt );
		TrackPopupMenu( FRollMenu.Items.Handle, Flags[FRollMenu.Alignment] or TPM_LEFTBUTTON,
			pt.X, pt.Y,	0, Handle, nil );
	end;
end;

procedure TKRollSpeedButton.VisibleActive( IsActive: Boolean );
var
	iCount,
	iVisible: Integer;
begin
	if IsActive then
	begin
		if ( not FActiveFlag ) then
		begin
			FOldFont.Assign( Font );
			Font := FActionFont;
		end;
		FActiveFlag := true;
	end
	else
	begin
		if FActiveFlag then
			Font := FOldFont;
		FActiveFlag := false;
		MenuVisibleInfo( FRollMenu, iVisible, iCount );
		if ( ( iCount > 0 ) and Down ) then
			Down := false;
	end;
end;

procedure TKRollSpeedButton.CMExit( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollSpeedButton.CMCancelMode( var Message: TCMCancelMode );
begin
	inherited;
	VisibleActive( Message.Sender = Self );
end;

procedure TKRollSpeedButton.WMCommand( var Message: TWMCommand );
begin
	inherited;
	FRollMenu.DispatchCommand( Message.ItemID );
end;

procedure TKRollSpeedButton.CMMouseEnter( var Message: TMessage );
begin
	inherited;
	VisibleActive( true );
end;

procedure TKRollSpeedButton.CMMouseLeave( var Message: TMessage );
begin
	inherited;
	VisibleActive( false );
end;

procedure TKRollSpeedButton.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
	inherited MouseMove( Shift, X, Y );
	VisibleActive( PtInRect( ClientRect, Point( X, Y ) ) );
end;

end.
