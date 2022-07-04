{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Visual Designer Unit                            }
{                                                       }
{       (c) 1999 - 2002, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELDsgnr;

interface

uses
    Forms, Classes, Controls, Messages, SysUtils, Consts, Graphics, Windows,
    ExtCtrls, Math, StdCtrls, Menus, ELSConsts
{$IFDEF VER140}, RTLConsts{$ENDIF}, Clipbrd, ELControls;

{
   ToDo ( TELCustomDesignPanel ):

   ? При активном дизайнере при нажатии Alt+F4 "Parent" форма не закрывается
}

  { TELDesignPanel }

type
    TELCustomDesignPanel = class (TWinControl)
    private
        FEng: Pointer;
        FBorderStyle: TBorderStyle;
        FForm: TCustomForm;
        procedure SetEng (AEng: Pointer);
        procedure SetBorderStyle (const Value: TBorderStyle);
        function GetIsUsed: Boolean;
        procedure CMColorChanged (var Message: TMessage); message CM_COLORCHANGED;
        procedure CMCursorChanged (var Message: TMessage); message CM_CURSORCHANGED;
        procedure CMBiDiModeChanged (var Message: TMessage); message CM_BIDIMODECHANGED;
        procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
        procedure CMParentBiDiModeChanged (var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
        procedure CMParentColorChanged (var Message: TMessage); message CM_PARENTCOLORCHANGED;
        procedure CMParentFontChanged (var Message: TMessage); message CM_PARENTFONTCHANGED;
        procedure CMParentShowHintChanged (var Message: TMessage); message CM_PARENTSHOWHINTCHANGED;
        procedure WMSetFocus (var Msg: TWMSetFocus); message WM_SETFOCUS;
        function GetAutoScroll: Boolean;
        procedure SetAutoScroll (const Value: Boolean);
        function GetHorzScrollBar: TControlScrollBar;
        procedure SetHorzScrollBar (const Value: TControlScrollBar);
        function GetVertScrollBar: TControlScrollBar;
        procedure SetVertScrollBar (const Value: TControlScrollBar);
        procedure UpdateFormParentWindow;
        procedure UpdateFormBounds;
    protected
        procedure CreateParams (var Params: TCreateParams); override;
        procedure CreateWnd; override;
        function GetPopupMenu: TPopupMenu; override;
        property IsUsed: Boolean read GetIsUsed;
        property Form: TCustomForm read FForm;
        property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
        property AutoScroll: Boolean read GetAutoScroll write SetAutoScroll;
        property HorzScrollBar: TControlScrollBar read GetHorzScrollBar write SetHorzScrollBar;
        property VertScrollBar: TControlScrollBar read GetVertScrollBar write SetVertScrollBar;
    public
        constructor Create (AOwner: TComponent); override;
        destructor Destroy; override;
        procedure SetBounds (ALeft, ATop, AWidth, AHeight: Integer); override;
    end;

    TELDesignPanel = class (TELCustomDesignPanel)
    public
        property IsUsed;
        property Form;
    published
        property BorderStyle;
        property AutoScroll;
        property HorzScrollBar;
        property VertScrollBar;
        property BiDiMode;
        property Align;
        property Anchors;
        property Color;
        property Constraints;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font;
        property ParentBiDiMode;
        property ParentColor;
        property ParentFont;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property OnClick;
        property OnConstrainedResize;
        property OnContextPopup;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnResize;
        property OnStartDock;
        property OnStartDrag;
    end;

    { TELDesigner }

type
    EELDesigner = class (Exception);
    TELCustomDesigner = class;

    TELDesignerLockMode = set of (lmNoMove, lmNoResize, lmNoDelete,
        lmNoInsertIn, lmNoCopy, lmCustom1, lmCustom2, lmCustom3, lmCustom4,
        lmCustom5, lmCustom6, lmCustom7, lmCustom8);
    TELDesignerAlignType = (atNoChanges, atLeftTop, atRightBottom,
        atSpaceEqually, atCenter, atCenterInWindow);

    TELDesignerGridSize = 2..32;

    TELDesignerGrid = class (TPersistent)
    private
        FDesigner: TELCustomDesigner;
        FVisible: Boolean;
        FYStep: TELDesignerGridSize;
        FXStep: TELDesignerGridSize;
        FColor: TColor;
        procedure SetVisible (const Value: Boolean);
        procedure SetXStep (const Value: TELDesignerGridSize);
        procedure SetYStep (const Value: TELDesignerGridSize);
        procedure SetColor (const Value: TColor);
    public
        constructor Create (AOwnerDesigner: TELCustomDesigner);
        procedure Assign (Source: TPersistent); override;
    published
        property XStep: TELDesignerGridSize read FXStep write SetXStep default 8;
        property YStep: TELDesignerGridSize read FYStep write SetYStep default 8;
        property Visible: Boolean read FVisible write SetVisible default True;
        property Color: TColor read FColor write SetColor default clBlack;
    end;

    TELDesignerSelectedControls = class
    private
        FDesigner: TELCustomDesigner;
        FItems: TList;
        FVisible: Boolean;
        FActive: Boolean;
        FRootWinControl: TWinControl;
        FUpdateCount: Integer;
        FChanged: Boolean;
        FDestroying: Boolean;
        function GetCount: Integer;
        procedure SetVisible (const Value: Boolean);
        procedure SetActive (const Value: Boolean);
        procedure SetRootWinControl (const Value: TWinControl);
        function GetItems (I: Integer): TControl;
        function GetDefaultControl: TControl;
        procedure Change;
        procedure UpdateMode;
        procedure DeleteItem (AI: Integer);
        procedure AddItem (AItem: Pointer);
        function IndexOfByItem (AItem: Pointer): Integer;
        procedure CheckDesignControl (AControl: TControl);
    public
        constructor Create (AOwnerDesigner: TELCustomDesigner);
        destructor Destroy; override;
        function Add (AControl: TControl): Integer;
        function IndexOf (AControl: TControl): Integer;
        procedure Delete (AI: Integer);
        procedure Remove (AControl: TControl);
        procedure SelectControls (AControls: TList);
        procedure GetControls (AResult: TList);
        procedure ClearExcept (AControl: TControl);
        procedure ClearNotChildrensOf (AParent: TWinControl);
        procedure Clear;
        procedure SelectAll;
        procedure Update;
        procedure UpdateControl (AIndex: Integer);
        procedure BeginUpdate;
        procedure EndUpdate;
        procedure Lock (ALockMode: TELDesignerLockMode);
        procedure AlignToGrid;
        procedure Align (AHorzAlign, AVertAlign: TELDesignerAlignType);
        procedure BringToFront;
        procedure SendToBack;
        property Items [I: Integer]: TControl read GetItems; default;
        property Count: Integer read GetCount;
        property DefaultControl: TControl read GetDefaultControl;
    end;

    TELDesignerHintType = (htControl, htSize, htMove, htInsert);
    TELDesignerHintTypes = set of TELDesignerHintType;
    TELDesignerOnValidateNameEvent = procedure (Sender: TObject;
        const AName: string; var AIsValidName: Boolean) of object;
    TELDesignerOnControlInsertingEvent = procedure (Sender: TObject;
        var AControlClass: TControlClass) of object;
    TELDesignerOnControlHintEvent = procedure (Sender: TObject;
        AControl: TControl; var AHint: string) of object;
    TELDesignerOnNotificationEvent = procedure (Sender: TObject;
        AnObject: TPersistent; Operation: TOperation) of object;
    TELDesignerOnGetUniqueName = procedure (Sender: TObject;
        const ABaseName: string; var AUniqueName: string) of object;
    TELDesignerDragOverEvent = procedure (Sender, ASource, ATarget: TObject; AX, AY: Integer;
        AState: TDragState; var AAccept: Boolean) of object;
    TELDesignerDragDropEvent = procedure (Sender, ASource, ATarget: TObject;
        AX, AY: Integer) of object;

    TELCustomDesigner = class (TComponent)
    private
        FEng: Pointer;
        FSnapToGrid: Boolean;
        FActive: Boolean;
        FOnControlInserted: TNotifyEvent;
        FOnControlInserting: TELDesignerOnControlInsertingEvent;
        FGrid: TELDesignerGrid;
        FSelectedControls: TELDesignerSelectedControls;
        FDesignPanel: TELCustomDesignPanel;
        FActivating: Boolean;
        FOnModified: TNotifyEvent;
        FOnChangeSelection: TNotifyEvent;
        FOnValidateName: TELDesignerOnValidateNameEvent;
        FDesignControl: TWinControl;
        FOnControlHint: TELDesignerOnControlHintEvent;
        FShowingHints: TELDesignerHintTypes;
        FOnDesignFormClose: TCloseEvent;
        FPopupMenu: TPopupMenu;
        FOnContextPopup: TContextPopupEvent;
        FHandleClr: TColor;
        FMultySelectHandleClr: TColor;
        FHandleBorderClr: TColor;
        FInactiveHandleClr: TColor;
        FInactiveHandleBorderClr: TColor;
        FLockedHandleClr: TColor;
        FMultySelectHandleBorderClr: TColor;
        FClipboardFormat: string;
        FClipbrdFormatRegistered: Boolean;
        FClipboardFormatWord: Word;
        FOnKeyUp: TKeyEvent;
        FOnKeyDown: TKeyEvent;
        FOnKeyPress: TKeyPressEvent;
        FOnNotification: TELDesignerOnNotificationEvent;
        FOnGetUniqueName: TELDesignerOnGetUniqueName;
        FOnDblClick: TNotifyEvent;
        FOnDragDrop: TELDesignerDragDropEvent;
        FOnDragOver: TELDesignerDragOverEvent;
        procedure SetActive (const Value: Boolean);
        procedure SetDesignControl (const Value: TWinControl);
        procedure SetDesignPanel (const Value: TELCustomDesignPanel);
        function GetDesignControlVisible: Boolean;
        procedure SetDesignControlVisible (const Value: Boolean);
        procedure ChangeDesignPanel (const Value: TELCustomDesignPanel);
        procedure SetPopupMenu (const Value: TPopupMenu);
        procedure SetHandleBorderClr (const Value: TColor);
        procedure SetHandleClr (const Value: TColor);
        procedure SetInactiveHandleBorderClr (const Value: TColor);
        procedure SetInactiveHandleClr (const Value: TColor);
        procedure SetLockedHandleClr (const Value: TColor);
        procedure SetMultySelectHandleBorderClr (const Value: TColor);
        procedure SetMultySelectHandleClr (const Value: TColor);
        procedure SetClipboardFormat (const Value: string);
        procedure SetGrid (const Value: TELDesignerGrid);
        function DoKeyDown (var AMessage: TWMKey): Boolean;
        function DoKeyPress (var AMessage: TWMKey): Boolean;
        function DoKeyUp (var AMessage: TWMKey): Boolean;
        procedure GridParamsChanged;
        procedure RegisterClipboardFormat;
    protected
        procedure Notification (AComponent: TComponent; Operation: TOperation); override;
        procedure CheckActive (AIsActiveNeeded: Boolean);
        function GetPopupMenu: TPopupMenu; dynamic;
        procedure DoModified; virtual;
        procedure ValidateName (const AName: string; var AIsValidName: Boolean); virtual;
        procedure GetUniqueName (const ABaseName: string; var AUniqueName: string); virtual;
        procedure ChangeSelection; virtual;
        procedure ControlInserting (var AControlClass: TControlClass); virtual;
        procedure ControlInserted; virtual;
        procedure DoNotification (AnObject: TPersistent; Operation: TOperation); virtual;
        procedure ControlHint (AControl: TControl; var AHint: string); virtual;
        procedure ContextPopup; virtual;
        procedure DesignFormClose (var Action: TCloseAction); virtual;
        procedure KeyDown (var Key: Word; Shift: TShiftState); virtual;
        procedure KeyPress (var Key: Char); virtual;
        procedure KeyUp (var Key: Word; Shift: TShiftState); virtual;
        procedure DblClick; virtual;
        procedure DragOver (ASource, ATarget: TObject; AX, AY: Integer; AState: TDragState;
            var AAccept: Boolean); dynamic;
        property Active: Boolean read FActive write SetActive;
        property DesignControl: TWinControl read FDesignControl write SetDesignControl;
        property SelectedControls: TELDesignerSelectedControls read FSelectedControls;
        property DesignControlVisible: Boolean read GetDesignControlVisible write SetDesignControlVisible;
        property DesignPanel: TELCustomDesignPanel read FDesignPanel write SetDesignPanel;
        property Grid: TELDesignerGrid read FGrid write SetGrid;
        property SnapToGrid: Boolean read FSnapToGrid write FSnapToGrid default True;
        property ShowingHints: TELDesignerHintTypes read FShowingHints write FShowingHints
            default [htControl, htSize, htMove, htInsert];
        property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
        property HandleClr: TColor read FHandleClr write SetHandleClr default clBlack;
        property HandleBorderClr: TColor read FHandleBorderClr write SetHandleBorderClr default clBlack;
        property MultySelectHandleClr: TColor read FMultySelectHandleClr write SetMultySelectHandleClr default clGray;
        property MultySelectHandleBorderClr: TColor read FMultySelectHandleBorderClr write SetMultySelectHandleBorderClr default clGray;
        property InactiveHandleClr: TColor read FInactiveHandleClr write SetInactiveHandleClr default clGray;
        property InactiveHandleBorderClr: TColor read FInactiveHandleBorderClr write SetInactiveHandleBorderClr default clBlack;
        property LockedHandleClr: TColor read FLockedHandleClr write SetLockedHandleClr default clRed;
        property ClipboardFormat: string read FClipboardFormat write SetClipboardFormat;
        property OnModified: TNotifyEvent read FOnModified write FOnModified;
        property OnValidateName: TELDesignerOnValidateNameEvent read FOnValidateName write FOnValidateName;
        property OnGetUniqueName: TELDesignerOnGetUniqueName read FOnGetUniqueName write FOnGetUniqueName;
        property OnControlInserting: TELDesignerOnControlInsertingEvent read FOnControlInserting write FOnControlInserting;
        property OnControlInserted: TNotifyEvent read FOnControlInserted write FOnControlInserted;
        property OnNotification: TELDesignerOnNotificationEvent read FOnNotification write FOnNotification;
        property OnChangeSelection: TNotifyEvent read FOnChangeSelection write FOnChangeSelection;
        property OnControlHint: TELDesignerOnControlHintEvent read FOnControlHint write FOnControlHint;
        property OnDesignFormClose: TCloseEvent read FOnDesignFormClose write FOnDesignFormClose;
        property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
        property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
        property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
        property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
        property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
        property OnDragDrop: TELDesignerDragDropEvent read FOnDragDrop write FOnDragDrop;
        property OnDragOver: TELDesignerDragOverEvent read FOnDragOver write FOnDragOver;
    public
        constructor Create (AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DragDrop (ASource, ATarget: TObject; AX, AY: Integer); dynamic;
        procedure Modified;
        procedure DeleteSelectedControls;
        procedure LockControl (AControl: TControl; ALockMode: TELDesignerLockMode);
        procedure LockAll (ALockMode: TELDesignerLockMode);
        function GetLockMode (AControl: TControl): TELDesignerLockMode;
        function IsUniqueName (const AName: string): Boolean;
        function CanCopy: Boolean;
        function CanCut: Boolean;
        function CanPaste: Boolean;
        procedure Cut;
        procedure Copy;
        procedure Paste;
    end;

    TELDesigner = class (TELCustomDesigner)
    public
        property Active;
        property DesignControl;
        property SelectedControls;
        property DesignControlVisible;
    published
        property DesignPanel;
        property Grid;
        property SnapToGrid;
        property ShowingHints;
        property PopupMenu;
        property HandleClr;
        property HandleBorderClr;
        property MultySelectHandleClr;
        property MultySelectHandleBorderClr;
        property InactiveHandleClr;
        property InactiveHandleBorderClr;
        property LockedHandleClr;
        property ClipboardFormat;
        property OnModified;
        property OnValidateName;
        property OnGetUniqueName;
        property OnControlInserting;
        property OnControlInserted;
        property OnNotification;
        property OnChangeSelection;
        property OnControlHint;
        property OnDesignFormClose;
        property OnContextPopup;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
    end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents (SELComponentPage, [TELDesigner, TELDesignPanel]);
end;

const
    HANDLESIZE = 5;
    DESIGNER_BASE = CM_BASE + $B00;
    DESIGNER_SIZING = DESIGNER_BASE + $002;
    DESIGNER_CANCEL = DESIGNER_BASE + $003;
    DESIGNER_SCUPDT = DESIGNER_BASE + $004;

type
    TCustomFormAccess = class (TCustomForm);
    TControlAccess = class (TControl);
    TWinControlAccess = class (TWinControl);

    PELWMMouse = ^TWMMouse;
    TELDMSizing = packed record
        Msg: Cardinal;
        MouseMessage: PELWMMouse;
        Unused: Longint;
        Result: Longint;
    end;

    PELDesignerLockMode = ^TELDesignerLockMode;

type
    TDPForm = class (TCustomForm)
    private
        FDesignPanel: TELCustomDesignPanel;
    protected
        procedure CreateParams (var Params: TCreateParams); override;
        procedure CreateWnd; override;
        procedure WndProc (var Message: TMessage); override;
    public
        constructor Create (AOwner: TComponent); override;
    end;

type
    TDEngCanvas = class (TCanvas)
    private
        FWinControl: TWinControl;
        FHiddenControl: TWinControl;
        procedure SetWinControl (const Value: TWinControl);
    protected
        procedure CreateHandle; override;
    public
        constructor Create;
        destructor Destroy; override;
        function ScreenToClient (const AP: TPoint): TPoint;
        property WinControl: TWinControl read FWinControl write SetWinControl;
    end;

type
    TDEngHintTimerMode = (tmShow, tmHide);

    TDEngHint = class
    private
        FTimer: TTimer;
        FTimerMode: TDEngHintTimerMode;
        FNeedStartHideTimer: Boolean;
        FCaption: string;
        FActive: Boolean;
        FCheckControl: TControl;
        FUseHooks: Boolean;
        procedure FTimerOnTimer (Sender: TObject);
        procedure DoShowNoPause;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Show (APauseBeforeShow, AHideAfterPause, AUseHooks: Boolean;
            ACheckControl: TControl);
        procedure Hide;
        procedure CheckHideMessageProc (AHandle: Integer; const Message: TMessage);
        property Caption: string read FCaption write FCaption;
        property Active: Boolean read FActive;
    end;

type
    TDEngNames = class
    private
        FSourceNames,
            FDestNames: TStringList;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Clear;
        procedure Add (const ASourceName, ADestName: string);
        function IndexOf (const ASourceName: string): Integer;
        function DestName (AI: Integer): string;
    end;

type
    TDEngDrawProcFlag = (pfInitAndDraw, pfMove, pfOkRemoveAndDispose,
        ddpfCancelRemoveAndDispose);
    TDEngDrawProc = function (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
        const AVirtualCursorPos: TPoint;
        AFlag: TDEngDrawProcFlag): Pointer of object;

{$IFDEF VER140}
    IDEngDesigner = IDesignerHook;
{$ELSE}
    IDEngDesigner = IDesigner;
{$ENDIF}

    TDEng = class (TInterfacedObject, IDEngDesigner)
    private
        FDesigner: TELCustomDesigner;
        FSelCtrls: TELDesignerSelectedControls;
        FGrid: TELDesignerGrid;
        FHint: TDEngHint;
        FHintControl: TControl;
        FCanvas: TDEngCanvas;
        FRoot: TWinControl;
        FForm: TCustomForm;
        FIsInDrawMode: Boolean;
        FOldDrawInfo: Pointer;
        FLastVirtCursorPos: TPoint;
        FInitDrawProc: TDEngDrawProc;
        FInitCursorPos: TPoint;
        FInitSnapToGrid: Boolean;
        FInitSkipXCursorMoving,
            FInitSkipYCursorMoving: Boolean;
        FInsertingControlClass: TControlClass;
        FOldRootParent: TWinControl;
        FOldRootLeft: Integer;
        FOldRootTop: Integer;
        FReaderNames: TDEngNames;
        FGridBitmap: Graphics.TBitmap;
        FGridBrush: HBRUSH;
        FGridBkColor: TColor;
        FGridHScrollPos: Integer;
        FGridVScrollPos: Integer;
        function GetRootVisible: Boolean;
        procedure SetRootVisible (const Value: Boolean);
        procedure GridParamsChanged;
        procedure UpdateGridPattern;
        function SelectRectProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
            const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;
        function MoveControlsProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
            const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;
        function SizeControlProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
            const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;
        function InsertControlProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
            const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;
        procedure ReaderSetName (Reader: TReader; Component: TComponent; var Name: string);
        procedure ReaderReadComponent (Component: TComponent);
        procedure ReaderReferenceName (Reader: TReader; var Name: string);
    protected
        { IUnknown }
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
        { IDesignerNotify }
        procedure Modified;
        procedure Notification (AnObject: TPersistent; Operation: TOperation);
        { IDEngDesigner }
        function GetCustomForm: TCustomForm;
        procedure SetCustomForm (Value: TCustomForm);
        function GetIsControl: Boolean;
        procedure SetIsControl (Value: Boolean);
        function IsDesignMsg (Sender: TControl; var Message: TMessage): Boolean;
        procedure PaintGrid;
        procedure ValidateRename (AComponent: TComponent;
            const CurName, NewName: string);
        function UniqueName (const BaseName: string): string;
        function GetRoot: TComponent;
        { Other }
        function IsUniqueName (const AName: string): Boolean;
        function IsDesignControl (AControl: TControl): Boolean;
        function GetDesignControl (AControl: TControl): TControl;
        function MouseMessage (Sender: TControl; const Message: TMessage): Boolean;
        function KeyMessage (const Message: TMessage): Boolean;
        procedure RecursionRefresh (AWinControl: TWinControl);
        procedure DeleteSelectedControls;
        function FindContainer (ASender: TControl): TWinControl;
        procedure LockControl (AControl: TControl; ALockMode: TELDesignerLockMode);
        class function GetLockMode (AControl: TControl): TELDesignerLockMode;
        class function GetFullLockMode (AControl: TControl): TELDesignerLockMode;
        procedure SaveControlsToStream (AStream: TStream; AControls: TList);
        procedure LoadControlsFromStream (AStream: TStream; AParent: TWinControl);
        procedure SelectAll;
        procedure AlignToGrid (AControl: TControl);
        property RootVisible: Boolean read GetRootVisible write SetRootVisible;
    public
        constructor Create (AOwnerDesigner: TELCustomDesigner; ARoot: TWinControl;
            ADesignPanel: TELCustomDesignPanel);
        destructor Destroy; override;
    end;

type
    TDSelCtrlItemMode = (imNone, imSizeable, imMultySelect);
    TDSelCtrlItemPointPos = (ppTopLeft, ppTop, ppTopRight,
        ppRight, ppBottomRight, ppBottom, ppBottomLeft, ppLeft);

    TDSelCtrlItem = class;

    TDSelCtrlItemPoint = class (TCustomControl)
    private
        FSelCtrl: TDSelCtrlItem;
        FPos: TDSelCtrlItemPointPos;
        FBorderColor: TColor;
    protected
        procedure Paint; override;
        procedure WndProc (var Message: TMessage); override;
    public
        constructor Create (AOwnerSelCtrl: TDSelCtrlItem;
            APos: TDSelCtrlItemPointPos); reintroduce;
        procedure Update; reintroduce;
    end;

    TDSelCtrlItem = class
    private
        FSelCtrls: TELDesignerSelectedControls;
        FPoints: array [TDSelCtrlItemPointPos] of TDSelCtrlItemPoint;
        FControl: TControl;
        FMode: TDSelCtrlItemMode;
        FActivePos: TDSelCtrlItemPointPos;
        procedure SetMode (const Value: TDSelCtrlItemMode);
    public
        constructor Create (AOwnerSelCtrls: TELDesignerSelectedControls; AControl: TControl);
        destructor Destroy; override;
        procedure Update;
        property Mode: TDSelCtrlItemMode read FMode write SetMode;
        property Control: TControl read FControl;
    end;

var
    DHintWindow: THintWindow;
    DHintWindowShower: TDEngHint;
    DHintWindowRefCount: Integer;
    DHintHook: HHOOK;

function HintHookMsgProc (ACode: Integer; AParam: Longint;
    var AMsg: TMsg): Longint; stdcall;
var
    Message: TMessage;
begin
    Result := CallNextHookEx (DHintHook, ACode, AParam, Longint (@AMsg));
    if (DHintWindowShower <> nil) and DHintWindowShower.Active then
        begin
            Message.Msg := AMsg.message;
            Message.WParam := AMsg.wParam;
            Message.LParam := AMsg.lParam;
            if DHintWindowShower.FUseHooks then
                DHintWindowShower.CheckHideMessageProc (AMsg.hwnd, Message);
        end;
end;

procedure HookDesignerHintHooks;
begin
    if DHintHook = 0 then
        DHintHook := SetWindowsHookEx (WH_GETMESSAGE, @HintHookMsgProc,
            0, GetCurrentThreadID);
end;

procedure UnhookDesignerHintHooks;
begin
    if DHintHook <> 0 then
        UnhookWindowsHookEx (DHintHook);
    DHintHook := 0;
end;

function RectFromPoints (AP1, AP2: TPoint): TRect;
begin
    if AP1.x < AP2.x then
        begin
            Result.Left := AP1.x;
            Result.Right := AP2.x;
        end
    else
        begin
            Result.Left := AP2.x;
            Result.Right := AP1.x;
        end;
    if AP1.y < AP2.y then
        begin
            Result.Top := AP1.y;
            Result.Bottom := AP2.y;
        end
    else
        begin
            Result.Top := AP2.y;
            Result.Bottom := AP1.y;
        end;
end;

function IsRectCrossed (AR1, AR2: TRect): Boolean;
begin
    Result := (AR1.Left <= AR2.Right) and (AR2.Left <= AR1.Right) and
        (AR1.Top <= AR2.Bottom) and (AR2.Top <= AR1.Bottom);
end;

procedure DrawSelectRect (ARect: TRect; ACanvas: TCanvas);
begin
    ACanvas.Pen.Color := clWhite;
    ACanvas.Pen.Mode := pmXor;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle (ARect);
end;

procedure DrawControlRect (ARect: TRect; ACanvas: TCanvas);
begin
    ACanvas.Pen.Color := clGray;
    ACanvas.Pen.Mode := pmXor;
    ACanvas.Pen.Width := 2;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Brush.Style := bsClear;
    Inc (ARect.Left);
    Inc (ARect.Top);
    ACanvas.Rectangle (ARect);
end;

constructor TDEng.Create (AOwnerDesigner: TELCustomDesigner; ARoot: TWinControl;
    ADesignPanel: TELCustomDesignPanel);

    procedure _UpdateSelectedControlsActive;
    var
        LActive: Boolean;
    begin
        if FForm is TDPForm then
            LActive := TDPForm (FForm).FDesignPanel.Focused
        else
            LActive := FForm.Active;
        FSelCtrls.SetActive (LActive);
    end;

begin
    FDesigner := AOwnerDesigner;
    FSelCtrls := AOwnerDesigner.SelectedControls;
    FGrid := AOwnerDesigner.Grid;
    FHint := TDEngHint.Create;
    FGridBitmap := Graphics.TBitmap.Create;
    if ARoot is TCustomForm then
        begin
            FRoot := ARoot;
            FForm := TCustomForm (ARoot);
        end
    else
        begin
            ADesignPanel.SetEng (Self);
            FRoot := ARoot;
            FForm := ADesignPanel.Form;
            FOldRootParent := FRoot.Parent;
            FOldRootLeft := FRoot.Left;
            FOldRootTop := FRoot.Top;
            FRoot.Parent := FForm;
            FRoot.Left := 0;
            FRoot.Top := 0;
        end;
    FForm.Designer := Self;
    TCustomFormAccess (FForm).SetDesigning (True, False);
    TWinControlAccess (FRoot).SetDesigning (True);
    FRoot.UpdateControlState;
    RecursionRefresh (FRoot);
    FForm.ActiveControl := nil;
    _UpdateSelectedControlsActive;
end;

destructor TDEng.Destroy;
var
    LDMCancelMessage: TMessage;
begin
    if FIsInDrawMode then
        begin
            LDMCancelMessage.Msg := DESIGNER_CANCEL;
            MouseMessage (FForm, LDMCancelMessage);
        end;
    if not (csDestroying in FForm.ComponentState) then
        TCustomFormAccess (FForm).SetDesigning (False, False);
    if not (csDestroying in FRoot.ComponentState) then
        TWinControlAccess (FRoot).SetDesigning (False);
    FForm.Designer := nil;
    if not (csDestroying in FRoot.ComponentState) then
        begin
            FRoot.UpdateControlState;
            RecursionRefresh (FRoot);
            if not (FRoot is TCustomForm) then
                begin
                    FRoot.Parent := FOldRootParent;
                    FRoot.Left := FOldRootLeft;
                    FRoot.Top := FOldRootTop;
                end;
        end;
    if FForm is TDPForm then
        TDPForm (FForm).FDesignPanel.SetEng (nil);
    FHint.Free;
    FCanvas.Free;
    FGridBitmap.Free;
    if FGridBrush <> 0 then
        DeleteObject (FGridBrush);
    inherited;
end;

function TDEng.GetCustomForm: TCustomForm;
begin
    if FRoot is TCustomForm then
        Result := TCustomForm (FRoot)
    else
        Result := nil;
end;

function TDEng.GetIsControl: Boolean;
begin
    Result := TControlAccess (FRoot).IsControl;
end;

function TDEng.GetRoot: TComponent;
begin
    Result := FRoot;
end;

procedure TDEng.GridParamsChanged;
begin
    UpdateGridPattern;
    FForm.Refresh;
end;

function TDEng.IsDesignMsg (Sender: TControl; var Message: TMessage): Boolean;

    procedure _FormClose;
    var
        LCloseAction: TCloseAction;
        LForm: TCustomFormAccess;
    begin
        LForm := TCustomFormAccess (FRoot);
        if LForm.FormStyle = fsMDIChild then
            if biMinimize in LForm.BorderIcons then
                LCloseAction := caMinimize
            else
                LCloseAction := caNone
        else
            LCloseAction := caHide;
        FDesigner.DesignFormClose (LCloseAction);
        if LCloseAction <> caNone then
            begin
                if Application.MainForm = TCustomForm (LForm) then
                    Application.Terminate
                else
                    case LCloseAction of
                        caHide:
                            RootVisible := False;
                        caMinimize:
                            if RootVisible then
                                ShowWindow (LForm.Handle, SW_SHOWMINIMIZED);
                        else
                            LForm.Release;
                    end;
            end;
    end;

var
    LI: Integer;
    LTarget: TControl;
    S: TObject;
    Accepts, IsDockOp: Boolean;

begin
    Result := False;
    if Sender = nil then
        Exit;
    if not FIsInDrawMode then
        FHint.CheckHideMessageProc (0, Message);
    case Message.Msg of
        CM_DRAG:
            begin
                case TCMDrag (Message).DragMessage of
                    dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop:
                        with TCMDrag (Message) do
                            begin
                                LTarget := TControl (DragRec.Target);
                                if LTarget <> nil then
                                    LTarget := GetDesignControl (LTarget);
                                if (LTarget <> nil) and (DragRec.Source <> nil) then
                                    begin
                                        S := DragRec.Source;
                                        IsDockOp := S is TDragDockObject;
                                        if not IsDockOp then
                                            S := (S as TDragControlObject).Control;
                                        if S <> nil then
                                            with LTarget.ScreenToClient (DragRec.Pos) do
                                                case DragMessage of
                                                    dmDragEnter, dmDragLeave, dmDragMove:
                                                        begin
                                                            Accepts := True;
                                                            FDesigner.DragOver (S, LTarget, X, Y, TDragState (DragMessage), Accepts);
                                                            Result := Ord (Accepts);
                                                        end;
                                                    dmDragDrop:
                                                        if not IsDockOp then
                                                            FDesigner.DragDrop (S, LTarget, X, Y);
                                                end;
                                    end;
                            end;
                    dmFindTarget:
                        begin
                            Sender := GetDesignControl (Sender);
                            with TCMDrag (Message) do
                                begin
                                    if (Sender <> nil) and (Sender is TWinControl) then
                                        begin
                                            Result := Longint (TWinControl (Sender).ControlAtPos (
                                                Sender.ScreenToClient (DragRec.Pos),
                                                False
                                                ));
                                            if Result = 0 then
                                                Result := Longint (Sender);
                                        end
                                    else
                                        TCMDrag (Message).Result := 0;
                                end;
                        end;
                end;
                Result := True;
            end;
        WM_WINDOWPOSCHANGED:
            PostMessage (FForm.Handle, DESIGNER_SCUPDT, Integer (Sender), 0); // PostMessage is
        // improves control
        // repaint speed
        // becouse SelCtrls
        // will be updated after
        // control painting
        DESIGNER_SCUPDT:
            begin
                LI := FSelCtrls.IndexOf (TControl (Message.WParam));
                if LI <> -1 then
                    FSelCtrls.UpdateControl (LI);
            end;
        WM_SETFOCUS:
            FSelCtrls.SetActive (True);
        WM_KILLFOCUS:
            FSelCtrls.SetActive (False);
        WM_MOUSEFIRST..WM_MOUSELAST, DESIGNER_SIZING:
            begin
                Result := MouseMessage (Sender, Message);
                if (Message.Msg = WM_RBUTTONUP) and not FIsInDrawMode then
                    begin
                        FDesigner.ContextPopup;
                        Result := True;
                    end;
            end;
        WM_KEYFIRST..WM_KEYLAST:
            Result := KeyMessage (Message);
        WM_CLOSE:
            begin
                if (Sender = FRoot) and (FRoot is TCustomForm) then
                    _FormClose;
                Result := True;
            end;
    end;
end;

function TDEng.IsUniqueName (const AName: string): Boolean;
begin
    Result := (FRoot.FindComponent (AName) = nil);
end;

procedure TDEng.Modified;
begin
    FSelCtrls.Update;
    FDesigner.DoModified;
end;

procedure TDEng.Notification (AnObject: TPersistent;
    Operation: TOperation);
begin
    FDesigner.DoNotification (AnObject, Operation);
end;

procedure TDEng.PaintGrid;
begin
    if FGrid.Visible then
        begin
            if (FGridBrush = 0) or (FGridBkColor <> FForm.Color) or
                (FForm.HorzScrollBar.ScrollPos <> FGridHScrollPos) or
                (FForm.VertScrollBar.ScrollPos <> FGridVScrollPos) then
                UpdateGridPattern;
            FillRect (FForm.Canvas.Handle, FForm.Canvas.ClipRect, FGridBrush);
        end;
end;

procedure TDEng.SetCustomForm (Value: TCustomForm);
begin
    raise EELDesigner.Create ('Not implemented');
end;

procedure TDEng.SetIsControl (Value: Boolean);
begin
    TControlAccess (FRoot).IsControl := Value;
end;

function TDEng.UniqueName (const BaseName: string): string;
var
    LI: Integer;
    LS: string;
label
    Start;
begin
    FDesigner.GetUniqueName (BaseName, Result);
    if Result = '' then
        begin
            if (Length (BaseName) >= 2) and (BaseName [1] in ['t', 'T']) then
                LS := Copy (BaseName, 2, MaxInt);
            LI := 0;
            goto Start;
            while not IsUniqueName (Result) do
                begin
                    Start:
                    Inc (LI);
                    Result := LS + IntToStr (LI);
                end;
        end;
end;

procedure TDEng.ValidateRename (AComponent: TComponent;
    const CurName, NewName: string);
var
    LIsValidName: Boolean;
begin
    if (AComponent is TControl) and IsDesignControl (TControl (AComponent)) and
        (NewName <> '') then
        begin
            LIsValidName := IsValidIdent (NewName);
            if LIsValidName then
                FDesigner.ValidateName (NewName, LIsValidName);
            if not LIsValidName then
                raise EELDesigner.CreateFmt (SInvalidName, [NewName]);
            if not IsUniqueName (NewName) then
                raise EELDesigner.CreateFmt (SDuplicateName, [NewName]);
        end;
end;

function TDEng.MouseMessage (Sender: TControl; const Message: TMessage): Boolean;

    function _DispatchDesignHitTest (Sender: TControl; Message: TWMMouse): Boolean;
    begin
        Message.Msg := CM_DESIGNHITTEST;
        Message.Result := 0;
        Sender.Dispatch (Message);
        Result := (Message.Result <> 0);
    end;

    function _FindVisibleScreenRect (AClient: TWinControl;
        out ARect: TRect): Boolean;
    var
        LR1, LR2: TRect;
    begin
        LR1 := AClient.ClientRect;
        LR1.TopLeft := AClient.ClientToScreen (LR1.TopLeft);
        LR1.BottomRight := AClient.ClientToScreen (LR1.BottomRight);
        LR2 := FForm.ClientRect;
        LR2.TopLeft := FForm.ClientToScreen (LR2.TopLeft);
        LR2.BottomRight := FForm.ClientToScreen (LR2.BottomRight);
        ARect.Left := Max (LR1.Left, LR2.Left);
        ARect.Top := Max (LR1.Top, LR2.Top);
        ARect.Right := Min (LR1.Right, LR2.Right);
        ARect.Bottom := Min (LR1.Bottom, LR2.Bottom);
        Result := (ARect.Left <= ARect.Right) and (ARect.Top <= ARect.Bottom);
        if not Result then
            ARect := Rect (0, 0, 0, 0);
    end;

    function _GetVirtualCursorPos (AStartCursorPos: TPoint): TPoint;
    begin
        GetCursorPos (Result);
        if FInitSkipXCursorMoving then
            Result.x := AStartCursorPos.x
        else
            if FDesigner.SnapToGrid and FInitSnapToGrid then
                Result.x := Round ((Result.x - AStartCursorPos.x) / FGrid.FXStep) *
                    FGrid.FXStep + AStartCursorPos.x;
        if FInitSkipYCursorMoving then
            Result.y := AStartCursorPos.y
        else
            if FDesigner.SnapToGrid and FInitSnapToGrid then
                Result.y := Round ((Result.y - AStartCursorPos.y) / FGrid.FYStep) *
                    FGrid.FYStep + AStartCursorPos.y;
    end;

    procedure _InitDrawMode (AControlToDrawOn: TWinControl;
        ADrawProc: TDEngDrawProc; ASnapToGrid, AInitSkipXCursorMoving,
        AInitSkipYCursorMoving, ASnapToGridFirstCursorPos: Boolean);
    var
        LR: TRect;
    begin
        FHint.Hide;
        FForm.Update;
        FCanvas := TDEngCanvas.Create;
        FCanvas.WinControl := AControlToDrawOn;
        FInitDrawProc := ADrawProc;
        FInitSnapToGrid := ASnapToGrid;
        FInitSkipXCursorMoving := AInitSkipXCursorMoving;
        FInitSkipYCursorMoving := AInitSkipYCursorMoving;
        GetCursorPos (FInitCursorPos);
        if FDesigner.SnapToGrid and ASnapToGridFirstCursorPos then
            begin
                FInitCursorPos := FForm.ScreenToClient (FInitCursorPos);
                FInitCursorPos.x := ((FInitCursorPos.x +
                    FForm.HorzScrollBar.ScrollPos) div
                    FGrid.FXStep) * FGrid.FXStep - FForm.HorzScrollBar.ScrollPos;
                FInitCursorPos.y := ((FInitCursorPos.y +
                    FForm.VertScrollBar.ScrollPos) div
                    FGrid.FYStep) * FGrid.FYStep - FForm.VertScrollBar.ScrollPos;
                FInitCursorPos := FForm.ClientToScreen (FInitCursorPos);
            end;
        FInitCursorPos := _GetVirtualCursorPos (FInitCursorPos);
        SetCaptureControl (FRoot);
        if _FindVisibleScreenRect (AControlToDrawOn, LR) then
            ClipCursor (@LR);
        FLastVirtCursorPos := FInitCursorPos;
        FOldDrawInfo := FInitDrawProc (FCanvas, nil, FInitCursorPos,
            pfInitAndDraw);
        FIsInDrawMode := True;
    end;

    procedure _DrawModeMouseMove;
    var
        LP: TPoint;
    begin
        LP := _GetVirtualCursorPos (FInitCursorPos);
        if (FLastVirtCursorPos.x <> LP.x) or (FLastVirtCursorPos.y <> LP.y) then
            FOldDrawInfo := FInitDrawProc (FCanvas, FOldDrawInfo, LP, pfMove);
        FLastVirtCursorPos := LP;
    end;

    procedure _DrawModeLButtonUp (ACancel: Boolean);
    var
        LP: TPoint;
        LFlag: TDEngDrawProcFlag;
    begin
        try
            LP := _GetVirtualCursorPos (FInitCursorPos);
            if ACancel then
                LFlag := ddpfCancelRemoveAndDispose
            else
                LFlag := pfOkRemoveAndDispose;
            try
                FInitDrawProc (FCanvas, FOldDrawInfo, LP, LFlag);
            finally
                if GetCaptureControl = FRoot then
                    SetCaptureControl (nil);
                ClipCursor (nil);
                FCanvas.Free;
                FCanvas := nil;
            end;
        finally
            FIsInDrawMode := False;
        end;
    end;

var
    LI: Integer;
    LDesignMessage: Boolean;
    LContainer: TWinControl;
    LS: string;
    LInsertingControl: Boolean;
    LNeedMove: Boolean;
    LControls: TList;

begin
    Result := FIsInDrawMode or (Message.Msg = DESIGNER_SIZING);
    if not Result then
        begin
            LDesignMessage := _DispatchDesignHitTest (Sender, TWMMouse (Message));
            Result := Result or not LDesignMessage;
        end;
    case Message.Msg of
        DESIGNER_SIZING:
            if not (lmNoResize in GetLockMode (Sender)) then
                begin
                    Sender := GetDesignControl (Sender);
                    FSelCtrls.SetVisible (False);
                    FSelCtrls.ClearExcept (Sender);
                    _InitDrawMode (Sender.Parent, SizeControlProc, True,
                        TDSelCtrlItem (FSelCtrls.FItems [0]).FActivePos in [ppTop, ppBottom],
                        TDSelCtrlItem (FSelCtrls.FItems [0]).FActivePos in [ppLeft, ppRight],
                        False);
                end;
        WM_LBUTTONDOWN, WM_RBUTTONDOWN:
            if not ((Sender = FForm) and (FRoot <> FForm)) then
                begin
                    LInsertingControl := False;
                    if Message.Msg = WM_LBUTTONDOWN then
                        begin
                            FInsertingControlClass := nil;
                            LContainer := FindContainer (Sender);
                            if not (lmNoInsertIn in GetLockMode (LContainer)) and
                                (LContainer <> nil) then
                                begin
                                    FDesigner.ControlInserting (FInsertingControlClass);
                                    if FInsertingControlClass <> nil then
                                        begin
                                            LInsertingControl := True;
                                            _InitDrawMode (LContainer, InsertControlProc, True, False,
                                                False, True);
                                        end;
                                end;
                        end;
                    if not LInsertingControl then
                        begin
                            Sender := GetDesignControl (Sender);
                            if ssShift in KeysToShiftState (TWMMouse (Message).Keys) then
                                begin
                                    FSelCtrls.BeginUpdate;
                                    try
                                        if FSelCtrls.IndexOf (Sender) = -1 then
                                            begin
                                                if not ((Sender = FRoot) and (FSelCtrls.Count <> 0)) then
                                                    FSelCtrls.Add (Sender);
                                            end
                                        else
                                            FSelCtrls.Remove (Sender);
                                        if FSelCtrls.Count = 0 then
                                            FSelCtrls.Add (FRoot);
                                    finally
                                        FSelCtrls.EndUpdate;
                                    end;
                                end
                            else
                                begin
                                    if (Sender = FRoot) or ((TWMMouse (Message).Keys and MK_CONTROL) > 0) then
                                        begin
                                            FSelCtrls.ClearExcept (FRoot);
                                            if Result and (Message.Msg = WM_LBUTTONDOWN) then
                                                begin
                                                    while (Sender <> nil) and
                                                        (not IsDesignControl (Sender) or
                                                        not (TControlAccess (Sender).GetChildParent <> nil) or
                                                        not (TControlAccess (Sender).GetChildParent is TWinControl) or
                                                        not (csAcceptsControls in TWinControl (TControlAccess (Sender).GetChildParent).ControlStyle)) do
                                                        Sender := Sender.Parent;
                                                    if (Sender <> nil) and (TControlAccess (Sender).GetChildParent <> nil) then
                                                        _InitDrawMode (TWinControl (TControlAccess (Sender).GetChildParent),
                                                            SelectRectProc, False, False, False, False);
                                                end;
                                        end
                                    else
                                        begin
                                            LControls := TList.Create;
                                            try
                                                if FSelCtrls.IndexOf (Sender) = -1 then
                                                    LControls.Add (Sender)
                                                else
                                                    begin
                                                        FSelCtrls.ClearNotChildrensOf (Sender.Parent);
                                                        FSelCtrls.GetControls (LControls);
                                                    end;
                                                if Result and (Message.Msg = WM_LBUTTONDOWN) then
                                                    begin
                                                        LNeedMove := False;
                                                        for LI := 0 to LControls.Count - 1 do
                                                            if not (lmNoMove in GetLockMode (LControls [LI])) then
                                                                begin
                                                                    LNeedMove := True;
                                                                    Break;
                                                                end;
                                                        if not (lmNoMove in GetLockMode (Sender)) and LNeedMove then
                                                            begin
                                                                for LI := LControls.Count - 1 downto 0 do
                                                                    if lmNoMove in GetLockMode (LControls [LI]) then
                                                                        LControls.Delete (LI);
                                                                FSelCtrls.SetVisible (False);
                                                                FSelCtrls.SelectControls (LControls);
                                                                _InitDrawMode (Sender.Parent, MoveControlsProc,
                                                                    True, False, False, False);
                                                            end
                                                        else
                                                            FSelCtrls.SelectControls (LControls);
                                                    end
                                                else
                                                    FSelCtrls.SelectControls (LControls);
                                            finally
                                                LControls.Free;
                                            end;
                                        end;
                                end;
                        end;
                end;
        WM_MOUSEMOVE:
            if FIsInDrawMode then
                _DrawModeMouseMove
            else
                begin
                    Sender := GetDesignControl (Sender);
                    if Sender <> nil then
                        begin
                            if (Sender = FRoot) or (Sender = FForm) or not Result then
                                FHint.Hide
                            else
                                if FHintControl <> Sender then
                                    begin
                                        if htControl in FDesigner.ShowingHints then
                                            begin
                                                LS := Sender.Name + ': ' + Sender.ClassName;
                                                FDesigner.ControlHint (Sender, LS);
                                                if LS <> '' then
                                                    begin
                                                        FHint.Caption := LS;
                                                        FHint.Show (not FHint.Active, True, True, Sender);
                                                    end
                                                else
                                                    FHint.Hide;
                                            end
                                        else
                                            FHint.Hide;
                                    end;
                            if Result then
                                FHintControl := Sender
                            else
                                FHintControl := nil;
                        end;
                end;
        WM_LBUTTONUP:
            if FIsInDrawMode then
                begin
                    _DrawModeLButtonUp (False);
                    FSelCtrls.SetVisible (True);
                end;
        WM_LBUTTONDBLCLK:
            if not FIsInDrawMode then
                FDesigner.DblClick;
        DESIGNER_CANCEL:
            if FIsInDrawMode then
                begin
                    _DrawModeLButtonUp (True);
                    FSelCtrls.SetVisible (True);
                end;
    end;
end;

function TDEng.SelectRectProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
    const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;

type
    PPointsRect = ^TPointsRect;
    TPointsRect = array [0..1] of TPoint;

    procedure _DoXORDraw (AData: PPointsRect);
    begin
        DrawSelectRect (RectFromPoints (AData^ [0], AData^ [1]), ACanvas);
    end;

    procedure _FillData (PointsRect: PPointsRect);
    var
        LR: TRect;
    begin
        LR := ACanvas.WinControl.ClientRect;
        LR.TopLeft := ACanvas.WinControl.ClientToScreen (LR.TopLeft);
        LR.BottomRight := ACanvas.WinControl.ClientToScreen (LR.BottomRight);
        PointsRect^ [0] := ACanvas.ScreenToClient (AVirtCursorPos);
    end;

var
    LI: Integer;
    LSelectRect: TRect;
    LPointsRect: TPointsRect;
    LControl: TControl;

begin
    Result := nil;
    case AFlag of
        pfInitAndDraw:
            begin
                if AOldDataPtr = nil then
                    New (PPointsRect (Result))
                else
                    Result := AOldDataPtr;
                _FillData (Result);
                if AOldDataPtr = nil then
                    PPointsRect (Result)^ [1] := PPointsRect (Result)^ [0];
                _DoXORDraw (Result);
            end;
        pfMove:
            begin
                LPointsRect := PPointsRect (AOldDataPtr)^;
                _FillData (@LPointsRect);
                _DoXORDraw (AOldDataPtr);
                _DoXORDraw (@LPointsRect);
                PPointsRect (AOldDataPtr)^ := LPointsRect;
                Result := AOldDataPtr;
            end;
        pfOkRemoveAndDispose:
            try
                _DoXORDraw (AOldDataPtr);
                LSelectRect := RectFromPoints (PPointsRect (AOldDataPtr)^ [0],
                    PPointsRect (AOldDataPtr)^ [1]);
                FSelCtrls.BeginUpdate;
                try
                    for LI := 0 to ACanvas.WinControl.ControlCount - 1 do
                        begin
                            LControl := ACanvas.WinControl.Controls [LI];
                            if IsRectCrossed (LControl.BoundsRect, LSelectRect) and
                                IsDesignControl (LControl) then
                                FSelCtrls.Add (ACanvas.WinControl.Controls [LI]);
                        end;
                    if FSelCtrls.Count > 1 then
                        FSelCtrls.Remove (FRoot);
                finally
                    FSelCtrls.EndUpdate;
                end;
            finally
                Dispose (PPointsRect (AOldDataPtr));
            end;
        ddpfCancelRemoveAndDispose:
            try
                _DoXORDraw (AOldDataPtr);
            finally
                Dispose (PPointsRect (AOldDataPtr));
            end;
    end;
end;

function TDEng.MoveControlsProc (ACanvas: TDEngCanvas; AOldDataPtr: Pointer;
    const AVirtCursorPos: TPoint; AFlag: TDEngDrawProcFlag): Pointer;

type
    PControlItem = ^TControlItem;
    TControlItem = record
        Rect: TRect;
        Control: TControl;
    end;

    PDataRec = ^TDataRec;
    TDataRec = record
        ControlList: TList;
        StartClientCursorPos: TPoint;
        Offset: TPoint;
        MinLeftTop: TPoint;
    end;

    function _InitData: PDataRec;

        procedure __AddControl (AData: PDataRec; AControl: TControl);
        var
            LControlItemPtr: PControlItem;
            LR: TRect;
        begin
            New (LControlItemPtr);
            LR := AControl.BoundsRect;
            LControlItemPtr.Rect := LR;
            LControlItemPtr.Control := AControl;
            AData.ControlList.Add (LControlItemPtr);
            if AData.MinLeftTop.x > LR.Left then
                AData.MinLeftTop.x := LR.Left;
            if AData.MinLeftTop.y > LR.Top then
                AData.MinLeftTop.y := LR.Top;
        end;

    var
        LI: Integer;

    begin
        New (Result);
        Result.ControlList := TList.Create;
        Result.MinLeftTop := Point (MaxInt, MaxInt);
        for LI := 0 to FSelCtrls.Count - 1 do
            __AddControl (Result, FSelCtrls [LI]);
        Result.StartClientCursorPos := ACanvas.ScreenToClient (AVirtCursorPos);
        Result.Offset := Point (0, 0);
    end;

    procedure _DisposeData (AData: PDataRec);
    var
        LI: Integer;
    begin
        for LI := 0 to AData.ControlList.Count - 1 do
            Dispose (PControlItem (AData.ControlList [LI]));
        AData.ControlList.Free;
        Dispose (AData);
    end;

    procedure _DoXORDraw (AData: PDataRec);
    var
        LI: Integer;
        LR: TRect;
    begin
        for LI := 0 to AData.ControlList.Count - 1 do
            if not (lmNoMove in GetLockMode (
                PControlItem (AData.ControlList [LI]).Control)) then
                begin
                    LR := PControlItem (AData.ControlList [LI]).Rect;
                    OffsetRect (LR, AData.Offset.x, AData.Offset.y);
                    DrawControlRect (LR, ACanvas);
                end;
    end;

    procedure _ShowHint (P: TPoint);
    begin
        if htMove in FDesigner.ShowingHints then
            begin
                FHint.Caption := IntToStr (P.x) + ', ' + IntToStr (P.y);
                FHint.Show (False, False, False, nil);
            end;
    end;

    procedure _FillData (DataRecPtr: PDataRec);
    var
        LP: TPoint;
    begin
        LP := ACanvas.ScreenToClient (AVirtCursorPos);
        LP.x := LP.x - DataRecPtr.StartClientCursorPos.x;
        LP.y := LP.y - DataRecPtr.StartClientCursorPos.y;
        DataRecPtr.Offset.x := LP.x;
        DataRecPtr.Offset.y := LP.y;
    end;

var
    LI: Integer;
    LNewControlBounds: TRect;
    LDataRec: TDataRec;

begin
    Result := nil;
    case AFlag of
        pfInitAndDraw:
            begin
                Result := _InitData;
                _FillData (Result);
                _DoXORDraw (Result);
                _ShowHint (Point (
                    PDataRec (Result).MinLeftTop.x + PDataRec (Result).Offset.x,
                    PDataRec (Result).MinLeftTop.y + PDataRec (Result).Offset.y
                    ));
            end;
        pfMove:
            begin
                LDataRec := PDataRec (AOldDataPtr)^;
                _FillData (@LDataRec);
                FHint.Hide;
                _DoXORDraw (AOldDataPtr);
                _DoXORDraw (@LDataRec);
                _ShowHint (Point (
                    LDataRec.MinLeftTop.x + LDataRec.Offset.x,
                    LDataRec.MinLeftTop.y + LDataRec.Offset.y
                    ));
                PDataRec (AOldDataPtr)^ := LDataRec;
                Result := AOldDataPtr;
            end;
        pfOkRemoveAndDispose:
            try
                FHint.Hide;
                _DoXORDraw (AOldDataPtr);
                if (PDataRec (AOldDataPtr).Offset.x <> 0) or
                    (PDataRec (AOldDataPtr).Offset.y <> 0) then
                    begin
                        for LI := 0 to PDataRec (AOldDataPtr).ControlList.Count - 1 do
                            if not (lmNoMove in GetLockMode (
                                PControlItem (PDataRec (AOldDataPtr).ControlList [LI]).Control)) then
                                begin
                                    LNewControlBounds := PControlItem (
                                        PDataRec (AOldDataPtr).ControlList [LI]
                                        ).Rect;
                                    OffsetRect (LNewControlBounds, PDataRec (AOldDataPtr).Offset.x,
                                        PDataRec (AOldDataPtr).Offset.y);
                                    with LNewControlBounds do
                                        PControlItem (PDataRec (AOldDataPtr).ControlList [LI]).Control.
                                            SetBounds (Left, Top, Right - Left, Bottom - Top);
                                end;
                        Modified;
                    end;
            finally
                _DisposeData (AOldDataPtr);
            end;
        ddpfCancelRemoveAndDispose:
            try
                FHint.Hide;
                _DoXORDraw (AOldDataPtr);
            finally
                _DisposeData (AOldDataPtr);
            end;
    end;
end;

function TDEng.SizeControlProc (ACanvas: TDEngCanvas;
    AOldDataPtr: Pointer; const AVirtCursorPos: TPoint;
    AFlag: TDEngDrawProcFlag): Pointer;

type
    PDataRec = ^TDataRec;
    TDataRec = record
        Control: TControl;
        PointPosition: TDSelCtrlItemPointPos;
        StartClientCursorPos: TPoint;
        Offset: TPoint;
    end;

    function _InitData: PDataRec;
    begin
        New (Result);
        Result.Control := FSelCtrls [0];
        Result.PointPosition := TDSelCtrlItem (FSelCtrls.FItems [0]).FActivePos;
        Result.StartClientCursorPos := ACanvas.ScreenToClient (AVirtCursorPos);
        Result.Offset := Point (0, 0);
    end;

    procedure _DisposeData (AData: PDataRec);
    begin
        Dispose (AData);
    end;

    procedure _DoXORDraw (ARect: TRect);
    begin
        DrawControlRect (ARect, ACanvas);
    end;

    function _CalcDrawRect (AData: PDataRec): TRect;
    begin
        Result := AData.Control.BoundsRect;
        Result.Left := Result.Left + AData.Offset.x *
            Ord (AData.PointPosition in [ppTopLeft, ppBottomLeft, ppLeft]);
        Result.Top := Result.Top + AData.Offset.y *
            Ord (AData.PointPosition in [ppTopLeft, ppTop, ppTopRight]);
        Result.Right := Result.Right + AData.Offset.x *
            Ord (AData.PointPosition in [ppTopRight, ppRight, ppBottomRight]);
        Result.Bottom := Result.Bottom + AData.Offset.y *
            Ord (AData.PointPosition in [ppBottomRight, ppBottom, ppBottomLeft]);
        Result := RectFromPoints (Result.TopLeft, Result.BottomRight);
    end;

    procedure _ShowHint (R: TRect);
    begin
        if htSize in FDesigner.ShowingHints then
            begin
                FHint.Caption := IntToStr (R.Right - R.Left) + ' x ' +
                    IntToStr (R.Bottom - R.Top);
                FHint.Show (False, False, False, nil);
            end;
    end;

    procedure _FillData (ADataRec: PDataRec);
    var
        LP: TPoint;
    begin
        LP := ACanvas.ScreenToClient (AVirtCursorPos);
        LP.x := LP.x - ADataRec.StartClientCursorPos.x;
        LP.y := LP.y - ADataRec.StartClientCursorPos.y;
        ADataRec.Offset.x := LP.x;
        ADataRec.Offset.y := LP.y;
    end;

var
    LOldControlBoundsRect, LDrawRect: TRect;
    LDataRec: TDataRec;

begin
    Result := nil;
    case AFlag of
        pfInitAndDraw:
            begin
                Result := _InitData;
                _FillData (Result);
                LDrawRect := _CalcDrawRect (Result);
                _DoXORDraw (LDrawRect);
                _ShowHint (LDrawRect);
            end;
        pfMove:
            begin
                LDataRec := PDataRec (AOldDataPtr)^;
                _FillData (@LDataRec);
                LDrawRect := _CalcDrawRect (@LDataRec);
                FHint.Hide;
                _DoXORDraw (_CalcDrawRect (AOldDataPtr));
                _DoXORDraw (LDrawRect);
                _ShowHint (LDrawRect);
                PDataRec (AOldDataPtr)^ := LDataRec;
                Result := AOldDataPtr;
            end;
        pfOkRemoveAndDispose:
            try
                LDrawRect := _CalcDrawRect (AOldDataPtr);
                FHint.Hide;
                _DoXORDraw (LDrawRect);
                LOldControlBoundsRect := PDataRec (AOldDataPtr).Control.BoundsRect;
                if (LDrawRect.Left <> LOldControlBoundsRect.Left) or
                    (LDrawRect.Top <> LOldControlBoundsRect.Top) or
                    (LDrawRect.Right <> LOldControlBoundsRect.Right) or
                    (LDrawRect.Bottom <> LOldControlBoundsRect.Bottom) then
                    begin
                        with LDrawRect do
                            PDataRec (AOldDataPtr).Control.SetBounds (Left, Top,
                                Right - Left, Bottom - Top);
                        PDataRec (AOldDataPtr).Control.Update;
                        Modified;
                    end;
            finally
                _DisposeData (AOldDataPtr);
            end;
        ddpfCancelRemoveAndDispose:
            try
                FHint.Hide;
                _DoXORDraw (_CalcDrawRect (AOldDataPtr));
            finally
                _DisposeData (AOldDataPtr);
            end;
    end;
end;

function TDEng.KeyMessage (const Message: TMessage): Boolean;

type
    TDirection = (dNone, dLeft, dUp, dRight, dDown);

    function _CalcDistance (AFromRect, AToRect: TRect;
        ADirection: TDirection): Real;
    const
        LRotateCount: array [TDirection] of Integer = (-1, 4, 3, 2, 1);
    var
        LI: Integer;
        LBufRect: TRect;
        LFromP, LToP: TPoint;
    begin
        Result := -1;
        if LRotateCount [ADirection] = -1 then
            Exit;
        for LI := 1 to LRotateCount [ADirection] do
            begin
                LBufRect := AFromRect;
                AFromRect.TopLeft := Point (-LBufRect.Bottom, LBufRect.Left);
                AFromRect.BottomRight := Point (-LBufRect.Top, LBufRect.Right);
                LBufRect := AToRect;
                AToRect.TopLeft := Point (-LBufRect.Bottom, LBufRect.Left);
                AToRect.BottomRight := Point (-LBufRect.Top, LBufRect.Right);
            end;
        if AToRect.Right < AFromRect.Left then
            begin
                if (AToRect.Bottom < AFromRect.Top) and
                    (AFromRect.Top - AToRect.Bottom > AFromRect.Left - AToRect.Right) then
                    Exit;
                if (AToRect.Top > AFromRect.Bottom) and
                    (AToRect.Top - AFromRect.Bottom > AFromRect.Left - AToRect.Right) then
                    Exit;
            end
        else
            if (AToRect.Left <= AFromRect.Right) and
                (AToRect.Right >= AFromrect.Left) then
                begin
                    if AToRect.Bottom < AFromRect.Top then
                        Exit;
                    if AToRect.Top > AFromRect.Bottom then
                        Exit;
                    if (AToRect.Left + AToRect.Right) div 2 >=
                        (AFromRect.Left + AFromRect.Right) div 2 then
                        Exit;
                end
            else
                Exit;
        LFromP := Point (AFromRect.Left, (AFromRect.Top + AFromRect.Bottom) div 2);
        if AToRect.Right < AFromRect.Left then
            begin
                if AToRect.Bottom < LFromP.y then
                    LToP := AToRect.BottomRight
                else
                    if AToRect.Bottom > LFromP.y then
                        LToP := Point (AToRect.Right, AToRect.Top)
                    else
                        LToP := Point (AToRect.Right, LFromP.y);
            end
        else
            if AToRect.Top > LFromP.y then
                LToP := Point (AToRect.Top, LFromP.y)
            else
                if AToRect.Bottom < LFromP.y then
                    LToP := Point (AToRect.Bottom, LFromP.y)
                else
                    LToP := LFromP;
        Result := Sqrt ((LFromP.x - LToP.x) * (LFromP.x - LToP.x) +
            (LFromP.y - LToP.y) * (LFromP.y - LToP.y));
    end;

    function _FindNearControl (AForward: Boolean): TControl;

        function __DoFind (AFromIndex, AToIndex: Integer;
            AForward: Boolean): TControl;
        begin
            Result := nil;
            while AFromIndex * (-1 + 2 * Ord (AForward)) <=
                AToIndex * (-1 + 2 * Ord (AForward)) do
                begin
                    if (FRoot.Components [AFromIndex] is TControl) and
                        (TControl (FRoot.Components [AFromIndex]).Parent <> nil) then
                        begin
                            Result := TControl (FRoot.Components [AFromIndex]);
                            Break;
                        end;
                    Inc (AFromIndex, -1 + 2 * Ord (AForward));
                end;
        end;

    var
        LMainControl: TControl;

    begin
        Result := nil;
        if FSelCtrls.Count > 0 then
            begin
                LMainControl := FSelCtrls.DefaultControl;
                if LMainControl.Owner = FRoot then
                    begin
                        Result := __DoFind (LMainControl.ComponentIndex +
                            (-1 + 2 * Ord (AForward)), (FRoot.ComponentCount - 1) *
                            Ord (AForward), AForward);
                        if Result = nil then
                            Result := __DoFind ((FRoot.ComponentCount - 1) * Ord (not AForward),
                                LMainControl.ComponentIndex + (1 - 2 * Ord (AForward)), AForward);
                    end;
            end;
    end;

var
    LDMCancelMessage: TMessage;
    LShiftState: TShiftState;
    LControl, LChousedControl: TControl;
    LI, LdX, LdY: Integer;
    LControlOffset, LChousedControlOffset: Real;
    LDirection: TDirection;

begin
    case Message.Msg of
        WM_KEYDOWN, WM_SYSKEYDOWN:
            begin
                FDesigner.DoKeyDown (TWMKey ((@Message)^));
                LShiftState := KeyDataToShiftState (TWMKeyDown (Message).KeyData);
                case TWMKeyDown (Message).CharCode of
                    VK_ESCAPE:
                        if FIsInDrawMode then
                            begin
                                LDMCancelMessage.Msg := DESIGNER_CANCEL;
                                MouseMessage (FForm, LDMCancelMessage);
                            end
                        else
                            begin
                                LControl := nil;
                                if (FSelCtrls.DefaultControl <> nil) and
                                    (FSelCtrls.DefaultControl.Parent <> nil) then
                                    LControl := GetDesignControl (FSelCtrls.DefaultControl.Parent);
                                if LControl = nil then
                                    LControl := FRoot;
                                FSelCtrls.ClearExcept (LControl);
                            end;
                    VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN:
                        if (FSelCtrls.Count > 0) and not FIsInDrawMode then
                            begin
                                if ((ssShift in LShiftState) or (ssCtrl in LShiftState)) then
                                    begin
                                        if FSelCtrls.DefaultControl <> FRoot then
                                            begin
                                                FSelCtrls.BeginUpdate;
                                                try
                                                    FSelCtrls.ClearNotChildrensOf (
                                                        FSelCtrls.DefaultControl.Parent);
                                                    if ssShift in LShiftState then
                                                        begin
                                                            for LI := FSelCtrls.Count - 1 downto 0 do
                                                                if lmNoResize in GetLockMode (FSelCtrls [LI]) then
                                                                    FSelCtrls.Delete (LI);
                                                        end
                                                    else
                                                        for LI := FSelCtrls.Count - 1 downto 0 do
                                                            if lmNoMove in GetLockMode (FSelCtrls [LI]) then
                                                                FSelCtrls.Delete (LI);
                                                finally
                                                    FSelCtrls.EndUpdate;
                                                end;
                                                if FSelCtrls.Count > 0 then
                                                    begin
                                                        LdX := 0;
                                                        LdY := 0;
                                                        case TWMKeyDown (Message).CharCode of
                                                            VK_LEFT: LdX := -1;
                                                            VK_UP: LdY := -1;
                                                            VK_RIGHT: LdX := 1;
                                                            VK_DOWN: LdY := 1;
                                                        end;
                                                        if (LdX <> 0) or (LdY <> 0) then
                                                            begin
                                                                if ssShift in LShiftState then
                                                                    begin
                                                                        for LI := 0 to FSelCtrls.Count - 1 do
                                                                            with FSelCtrls [LI] do
                                                                                SetBounds (Left, Top, Max (Width + LdX, 0),
                                                                                    Max (Height + LdY, 0));
                                                                    end
                                                                else
                                                                    for LI := 0 to FSelCtrls.Count - 1 do
                                                                        with FSelCtrls [LI] do
                                                                            SetBounds (Left + LdX, Top + LdY, Width, Height);
                                                                Modified;
                                                            end;
                                                    end;
                                            end;
                                    end
                                else
                                    if (LShiftState = []) then
                                        begin
                                            if FSelCtrls.DefaultControl <> FRoot then
                                                begin
                                                    case TWMKeyDown (Message).CharCode of
                                                        VK_LEFT: LDirection := dLeft;
                                                        VK_UP: LDirection := dUp;
                                                        VK_RIGHT: LDirection := dRight;
                                                        VK_DOWN: LDirection := dDown;
                                                        else
                                                            LDirection := dNone;
                                                    end;
                                                    LChousedControl := nil;
                                                    LChousedControlOffset := 1.7E+308; // MaxReal
                                                    for LI := 0 to
                                                        FSelCtrls.DefaultControl.Parent.ControlCount - 1 do
                                                        begin
                                                            LControl := FSelCtrls.DefaultControl.Parent.Controls [LI];
                                                            if (LControl <> FSelCtrls.DefaultControl) and
                                                                IsDesignControl (LControl) then
                                                                begin
                                                                    LControlOffset := _CalcDistance (
                                                                        FSelCtrls.DefaultControl.BoundsRect,
                                                                        LControl.BoundsRect, LDirection);
                                                                    if (LControlOffset >= 0) and
                                                                        (LChousedControlOffset > LControlOffset) then
                                                                        begin
                                                                            LChousedControl := LControl;
                                                                            LChousedControlOffset := LControlOffset;
                                                                        end;
                                                                end;
                                                        end;
                                                    if LChousedControl <> nil then
                                                        FSelCtrls.ClearExcept (LChousedControl);
                                                end;
                                        end;
                            end;
                    VK_TAB:
                        if not FIsInDrawMode then
                            begin
                                LChousedControl := _FindNearControl (not (ssShift in LShiftState));
                                if LChousedControl <> nil then
                                    FSelCtrls.ClearExcept (LChousedControl);
                            end;
                    VK_DELETE:
                        DeleteSelectedControls;
                end;
            end;
        WM_KEYUP, WM_SYSKEYUP:
            FDesigner.DoKeyUp (TWMKey ((@Message)^));
        WM_CHAR:
            FDesigner.DoKeyPress (TWMKey ((@Message)^));
    end;
    Result := True;
end;

procedure TDEng.RecursionRefresh (AWinControl: TWinControl);
var
    LI: Integer;
begin
    AWinControl.Refresh;
    for LI := 0 to AWinControl.ControlCount - 1 do
        if AWinControl.Controls [LI] is TWinControl then
            RecursionRefresh (TWinControl (AWinControl.Controls [LI]));
end;

procedure TDEng.DeleteSelectedControls;
var
    LI: Integer;
    LLockedIndex: Integer;
begin
    if ((FSelCtrls.Count = 1) and (FSelCtrls [0] <> FRoot)) or
        (FSelCtrls.Count > 1) then
        begin
            FSelCtrls.BeginUpdate;
            try
                LLockedIndex := -1;
                for LI := 0 to FSelCtrls.Count - 1 do
                    if (FSelCtrls [LI] <> FRoot) and
                        (lmNoDelete in GetFullLockMode (FSelCtrls [LI])) then
                        if LLockedIndex = -1 then
                            LLockedIndex := LI
                        else
                            LLockedIndex := -2;
                if LLockedIndex = -1 then
                    begin
                        LI := FSelCtrls.Count - 1;
                        while True do
                            begin
                                if LI > FSelCtrls.Count - 1 then
                                    LI := FSelCtrls.Count - 1;
                                if LI < 0 then
                                    Break;
                                if FSelCtrls [LI] <> FRoot then
                                    FSelCtrls [LI].Free;
                                Dec (LI);
                            end;
                    end
                else
                    if LLockedIndex = -2 then
                        raise EELDesigner.Create (SELDsgnrControlsLockedDel)
                    else
                        raise EELDesigner.CreateFmt (SELDsgnrControlLockedDel,
                            [FSelCtrls [LLockedIndex].Name]);
            finally
                FSelCtrls.EndUpdate;
            end;
            Modified;
        end;
end;

function TDEng.InsertControlProc (ACanvas: TDEngCanvas;
    AOldDataPtr: Pointer; const AVirtCursorPos: TPoint;
    AFlag: TDEngDrawProcFlag): Pointer;

type
    PDataRec = ^TDataRec;
    TDataRec = record
        StartClientCursorPos: TPoint;
        Offset: TPoint;
        Drawn: Boolean;
    end;

    function _InitData: PDataRec;
    begin
        New (Result);
        Result.StartClientCursorPos := ACanvas.ScreenToClient (AVirtCursorPos);
        Result.Offset := Point (0, 0);
        Result.Drawn := False;
    end;

    procedure _DisposeData (AData: PDataRec);
    begin
        Dispose (AData);
    end;

    procedure _DoXORDraw (ARect: TRect);
    begin
        DrawControlRect (ARect, ACanvas);
    end;

    function _CalcDrawRect (AData: PDataRec): TRect;
    begin
        Result := RectFromPoints (
            AData.StartClientCursorPos,
            Point (
            AData.StartClientCursorPos.x + AData.Offset.x,
            AData.StartClientCursorPos.y + AData.Offset.y
            )
            );
        Inc (Result.Right);
        Inc (Result.Bottom);
    end;

    procedure _ShowHint (R: TRect);
    begin
        if htInsert in FDesigner.ShowingHints then
            begin
                FHint.Caption := IntToStr (R.Right - R.Left) + ' x ' +
                    IntToStr (R.Bottom - R.Top);
                FHint.Show (False, False, False, nil);
            end;
    end;

    procedure _FillData (ADataRec: PDataRec);
    var
        LP: TPoint;
    begin
        LP := ACanvas.ScreenToClient (AVirtCursorPos);
        LP.x := LP.x - ADataRec.StartClientCursorPos.x;
        LP.y := LP.y - ADataRec.StartClientCursorPos.y;
        ADataRec.Offset.x := LP.x;
        ADataRec.Offset.y := LP.y;
    end;

var
    LDrawRect: TRect;
    LDataRec: TDataRec;
    LInsertingControl: TControl;
    LName: string;

begin
    Result := nil;
    case AFlag of
        pfInitAndDraw:
            begin
                Result := _InitData;
                _FillData (Result);
                LDrawRect := _CalcDrawRect (Result);
            end;
        pfMove:
            begin
                LDataRec := PDataRec (AOldDataPtr)^;
                _FillData (@LDataRec);
                LDrawRect := _CalcDrawRect (@LDataRec);
                FHint.Hide;
                if PDataRec (AOldDataPtr)^.Drawn then
                    _DoXORDraw (_CalcDrawRect (AOldDataPtr));
                _DoXORDraw (LDrawRect);
                _ShowHint (LDrawRect);
                LDataRec.Drawn := True;
                PDataRec (AOldDataPtr)^ := LDataRec;
                Result := AOldDataPtr;
            end;
        pfOkRemoveAndDispose:
            try
                LDrawRect := _CalcDrawRect (AOldDataPtr);
                FHint.Hide;
                if PDataRec (AOldDataPtr)^.Drawn then
                    _DoXORDraw (LDrawRect);
                LInsertingControl := FInsertingControlClass.Create (FRoot);
                try
                    with LInsertingControl do
                        begin
                            with LDrawRect do
                                if (Right - Left > 1) or (Bottom - Top > 1) then
                                    SetBounds (Left, Top, Right - Left, Bottom - Top)
                                else
                                    SetBounds (Left, Top, Width, Height);
                            LName := UniqueName (FInsertingControlClass.ClassName);
                            Name := LName; // <-- Here may be exception
                            Parent := FCanvas.WinControl; // <-- Here may be exception
                        end;
                except
                    LInsertingControl.Free;
                    raise;
                end;
                FSelCtrls.ClearExcept (LInsertingControl);
                FDesigner.ControlInserted;
                Modified;
            finally
                _DisposeData (AOldDataPtr);
            end;
        ddpfCancelRemoveAndDispose:
            try
                FHint.Hide;
                if PDataRec (AOldDataPtr)^.Drawn then
                    _DoXORDraw (_CalcDrawRect (AOldDataPtr));
            finally
                _DisposeData (AOldDataPtr);
            end;
    end;
end;

constructor TELDesignerGrid.Create (AOwnerDesigner: TELCustomDesigner);
begin
    FDesigner := AOwnerDesigner;
    FXStep := 8;
    FYStep := 8;
    FVisible := True;
    FColor := clBlack;
end;

procedure TELDesignerGrid.Assign (Source: TPersistent);
begin
    FVisible := TELDesignerGrid (Source).FVisible;
    FYStep := TELDesignerGrid (Source).FYStep;
    FXStep := TELDesignerGrid (Source).FXStep;
    FColor := TELDesignerGrid (Source).FColor;
end;

procedure TELDesignerGrid.SetColor (const Value: TColor);
begin
    if FColor = Value then
        Exit;
    FColor := Value;
    FDesigner.GridParamsChanged;
end;

procedure TELDesignerGrid.SetVisible (const Value: Boolean);
begin
    if Value = FVisible then
        Exit;
    FVisible := Value;
    FDesigner.GridParamsChanged;
end;

procedure TELDesignerGrid.SetXStep (const Value: TELDesignerGridSize);
begin
    if Value = FXStep then
        Exit;
    if Value < 2 then
        FXStep := 2
    else
        if Value > 32 then
            FXStep := 32
        else
            FXStep := Value;
    FDesigner.GridParamsChanged;
end;

procedure TELDesignerGrid.SetYStep (const Value: TELDesignerGridSize);
begin
    if Value = FYStep then
        Exit;
    if Value < 2 then
        FYStep := 2
    else
        if Value > 32 then
            FYStep := 32
        else
            FYStep := Value;
    FDesigner.GridParamsChanged;
end;

function TDEng.GetRootVisible: Boolean;
begin
    Result := IsWindowVisible (FRoot.Handle);
end;

procedure TDEng.SetRootVisible (const Value: Boolean);
begin
    ShowWindow (
        FRoot.Handle,
        (SW_SHOW * Ord (Value)) or (SW_HIDE * Ord (not Value))
        );
end;

procedure TDEng.SelectAll;

    procedure _SelectChildControls (AControl: TWinControl);
    var
        LI: Integer;
    begin
        if IsDesignControl (AControl) then
            begin
                FSelCtrls.Add (AControl);
                for LI := 0 to AControl.ControlCount - 1 do
                    if AControl.Controls [LI] is TWinControl then
                        _SelectChildControls (TWinControl (AControl.Controls [LI]))
                    else
                        if IsDesignControl (AControl.Controls [LI]) then
                            FSelCtrls.Add (AControl.Controls [LI]);
            end;
    end;

begin
    FSelCtrls.BeginUpdate;
    try
        _SelectChildControls (FRoot);
    finally
        FSelCtrls.EndUpdate;
    end;
end;

procedure TDEng.AlignToGrid (AControl: TControl);
var
    LNewLeft, LNewTop: Integer;
begin
    if (AControl <> FRoot) and IsDesignControl (AControl) then
        begin
            LNewLeft := Round (AControl.Left / FGrid.XStep) * FGrid.XStep;
            LNewTop := Round (AControl.Top / FGrid.YStep) * FGrid.YStep;
            AControl.SetBounds (LNewLeft, LNewTop, AControl.Width, AControl.Height);
            Modified;
        end;
end;

function TDEng.FindContainer (ASender: TControl): TWinControl;
begin
    Result := nil;
    while ASender <> nil do
        begin
            if (ASender is TWinControl) and
                (csAcceptsControls in ASender.ControlStyle) then
                begin
                    Result := TWinControl (ASender);
                    Break;
                end;
            ASender := ASender.Parent;
        end;
end;

function TDEng.GetDesignControl (AControl: TControl): TControl;
begin
    Result := nil;
    while AControl <> nil do
        begin
            if IsDesignControl (AControl) then
                begin
                    Result := AControl;
                    Break;
                end;
            AControl := AControl.Parent;
        end;
end;

function TDEng.IsDesignControl (AControl: TControl): Boolean;
begin
    Result := (AControl = FRoot) or
        ((csDesigning in AControl.ComponentState) and (AControl.Owner = FRoot));
end;

class function TDEng.GetLockMode (AControl: TControl): TELDesignerLockMode;
var
    LIntLockMode: Integer;
begin
    LIntLockMode := AControl.DesignInfo;
    Result := PELDesignerLockMode (@LIntLockMode)^;
end;

procedure TDEng.LockControl (AControl: TControl;
    ALockMode: TELDesignerLockMode);
var
    LIntLockMode: Integer;
begin
    if IsDesignControl (AControl) and (AControl <> FRoot) then
        begin
            PELDesignerLockMode (@LIntLockMode)^ := ALockMode;
            AControl.DesignInfo := LIntLockMode;
            FSelCtrls.Update;
        end;
end;

class function TDEng.GetFullLockMode (
    AControl: TControl): TELDesignerLockMode;
var
    LI: Integer;
begin
    Result := GetLockMode (AControl);
    if AControl is TWinControl then
        for LI := 0 to TWinControl (AControl).ControlCount - 1 do
            Result := Result + GetLockMode (TWinControl (AControl).Controls [LI]);
end;

procedure TDEng.LoadControlsFromStream (AStream: TStream;
    AParent: TWinControl);
var
    LR: TReader;
begin
    FSelCtrls.BeginUpdate;
    try
        FSelCtrls.Clear;
        FReaderNames := TDEngNames.Create;
        try
            LR := TReader.Create (AStream, 1024);
            try
                LR.OnSetName := ReaderSetName;
                LR.OnReferenceName := ReaderReferenceName;
                LR.ReadComponents (FRoot, AParent, ReaderReadComponent);
            finally
                LR.Free;
            end;
        finally
            FReaderNames.Free;
        end;
    finally
        FSelCtrls.EndUpdate;
    end;
    Modified;
end;

procedure TDEng.SaveControlsToStream (AStream: TStream;
    AControls: TList);
var
    LW: TWriter;
    LI: Integer;
begin
    LW := TWriter.Create (AStream, 1024);
    try
        LW.Root := FRoot;
        for LI := 0 to AControls.Count - 1 do
            if not (lmNoCopy in GetFullLockMode (AControls [LI])) then
                begin
                    LW.WriteSignature;
                    LW.WriteComponent (AControls [LI]);
                end;
        LW.WriteListEnd;
    finally
        LW.Free;
    end;
end;

procedure TDEng.ReaderSetName (Reader: TReader;
    Component: TComponent; var Name: string);
var
    LOldName: string;
begin
    if not IsUniqueName (Name) then
        begin
            LOldName := Name;
            Name := UniqueName (Component.ClassName);
            FReaderNames.Add (LOldName, Name);
        end;
end;

procedure TDEng.ReaderReadComponent (Component: TComponent);

    function _ControlExist (AParent: TWinControl; ALeft, ATop: Integer): Boolean;
    var
        LI: Integer;
    begin
        Result := False;
        for LI := 0 to AParent.ControlCount - 1 do
            if AParent.Controls [LI] <> Component then
                with AParent.Controls [LI] do
                    if (Left = ALeft) and (Top = ATop) then
                        begin
                            Result := True;
                            Break;
                        end;
    end;

var
    LNewLeft, LNewTop: Integer;

begin
    if (Component is TControl) and IsDesignControl (TControl (Component)) then
        with TControl (Component) do
            begin
                if Parent <> nil then
                    begin
                        LNewLeft := Left;
                        LNewTop := Top;
                        while _ControlExist (Parent, LNewLeft, LNewTop) do
                            begin
                                Inc (LNewLeft, FGrid.XStep);
                                Inc (LNewTop, FGrid.YStep);
                            end;
                        SetBounds (LNewLeft, LNewTop, Width, Height);
                    end;
                FSelCtrls.Add (TControl (Component));
            end;
end;

procedure TDEng.ReaderReferenceName (Reader: TReader;
    var Name: string);
var
    LI: Integer;
begin
    LI := FReaderNames.IndexOf (Name);
    if LI <> -1 then
        Name := FReaderNames.DestName (LI);
end;

function TDEng._AddRef: Integer;
begin
    Result := -1;
end;

function TDEng._Release: Integer;
begin
    Result := -1;
end;

constructor TDPForm.Create (AOwner: TComponent);
begin
    CreateNew (AOwner);
    //  Screen.RemoveForm(Self); ???
end;

procedure TDPForm.CreateParams (var Params: TCreateParams);
begin
    inherited;
    with Params do
        begin
            Style := Style and not (WS_POPUP or WS_CAPTION or WS_THICKFRAME or
                WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_SYSMENU) or
                (WS_CHILD or WS_GROUP or WS_TABSTOP);
            ExStyle := ExStyle and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or
                WS_EX_TOOLWINDOW);
        end;
end;

procedure TDPForm.CreateWnd;
begin
    inherited;
    if FDesignPanel.IsUsed then
        TDEng (FDesignPanel.FEng).FSelCtrls.Update;
end;

procedure TDPForm.WndProc (var Message: TMessage);
begin
    if Message.Msg in [WM_MOUSEACTIVATE, WM_ACTIVATE] then
        begin
            if (GetParentForm (FDesignPanel) = nil) or
                GetParentForm (FDesignPanel).SetFocusedControl (FDesignPanel) then
                begin
                    Dispatch (Message);
                    Windows.SetFocus (Handle);
                end;
            Exit;
        end;
    inherited;
end;

procedure TELCustomDesignPanel.CMBiDiModeChanged (var Message: TMessage);
begin
    inherited;
    FForm.BiDiMode := BiDiMode;
end;

procedure TELCustomDesignPanel.CMColorChanged (var Message: TMessage);
begin
    inherited;
    FForm.Color := Color;
end;

procedure TELCustomDesignPanel.CMCursorChanged (var Message: TMessage);
begin
    inherited;
    FForm.Cursor := Cursor;
end;

procedure TELCustomDesignPanel.CMFontChanged (var Message: TMessage);
begin
    inherited;
    FForm.Font := Font;
end;

procedure TELCustomDesignPanel.CMParentBiDiModeChanged (var Message: TMessage);
begin
    inherited;
    TDPForm (FForm).ParentBiDiMode := ParentBiDiMode;
end;

procedure TELCustomDesignPanel.CMParentColorChanged (var Message: TMessage);
begin
    inherited;
    TDPForm (FForm).ParentColor := ParentColor;
end;

procedure TELCustomDesignPanel.CMParentFontChanged (var Message: TMessage);
begin
    inherited;
    TDPForm (FForm).ParentFont := ParentFont;
end;

procedure TELCustomDesignPanel.CMParentShowHintChanged (var Message: TMessage);
begin
    inherited;
    TDPForm (FForm).ParentShowHint := ParentShowHint;
end;

constructor TELCustomDesignPanel.Create (AOwner: TComponent);
begin
    inherited;
    TabStop := True;
    Width := 185;
    Height := 41;
    FBorderStyle := bsSingle;
    FForm := TDPForm.Create (nil);
    TDPForm (FForm).FDesignPanel := Self;
    FForm.Visible := True;
end;

procedure TELCustomDesignPanel.CreateParams (var Params: TCreateParams);
const
    BorderStyles: array [TBorderStyle] of DWORD = (0, WS_BORDER);
begin
    inherited;
    with Params do
        begin
            Style := Style or BorderStyles [FBorderStyle];
            if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
                begin
                    Style := Style and not WS_BORDER;
                    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
                end;
            WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
        end;
end;

procedure TELCustomDesignPanel.CreateWnd;
begin
    inherited;
    UpdateFormBounds;
    UpdateFormParentWindow;
    if FForm.ParentWindow <> 0 then
        ShowWindow (FForm.Handle, SW_SHOW);
end;

destructor TELCustomDesignPanel.Destroy;
begin
    inherited;
    FForm.Free;
end;

function TELCustomDesignPanel.GetAutoScroll: Boolean;
begin
    Result := TDPForm (FForm).AutoScroll;
end;

function TELCustomDesignPanel.GetHorzScrollBar: TControlScrollBar;
begin
    Result := FForm.HorzScrollBar;
end;

function TELCustomDesignPanel.GetIsUsed: Boolean;
begin
    Result := FEng <> nil;
end;

function TELCustomDesignPanel.GetPopupMenu: TPopupMenu;
begin
    Result := inherited GetPopupMenu;
    if IsUsed then
        Result := nil;
end;

function TELCustomDesignPanel.GetVertScrollBar: TControlScrollBar;
begin
    Result := FForm.VertScrollBar;
end;

procedure TELCustomDesignPanel.SetAutoScroll (const Value: Boolean);
begin
    TDPForm (FForm).AutoScroll := Value;
end;

procedure TELCustomDesignPanel.SetBorderStyle (const Value: TBorderStyle);
begin
    if (Value <> FBorderStyle) and (Value in [bsSingle, bsNone]) then
        begin
            FBorderStyle := Value;
            RecreateWnd;
            UpdateFormBounds;
        end;
end;

procedure TELCustomDesignPanel.SetBounds (ALeft, ATop, AWidth, AHeight: Integer);
begin
    inherited;
    UpdateFormBounds;
end;

procedure TELCustomDesignPanel.SetEng (AEng: Pointer);
begin
    FEng := AEng;
    UpdateFormParentWindow;
end;

procedure TELCustomDesignPanel.SetHorzScrollBar (const Value: TControlScrollBar);
begin
    FForm.HorzScrollBar := Value;
end;

procedure TELCustomDesignPanel.SetVertScrollBar (const Value: TControlScrollBar);
begin
    FForm.VertScrollBar := value;
end;

procedure TELCustomDesignPanel.UpdateFormBounds;
var
    R: TRect;
begin
    if HandleAllocated then
        begin
            R := ClientRect;
            FForm.SetBounds (R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
        end;
end;

procedure TELCustomDesignPanel.UpdateFormParentWindow;
begin
    if IsUsed and HandleAllocated then
        FForm.ParentWindow := Handle
    else
        FForm.ParentWindow := 0;
end;

procedure TELCustomDesignPanel.WMSetFocus (var Msg: TWMSetFocus);
begin
    if (FForm <> nil) and FForm.HandleAllocated and (FForm.ParentWindow <> 0) then
        Windows.SetFocus (FForm.Handle);
end;

procedure TDEngHint.CheckHideMessageProc (AHandle: Integer;
    const Message: TMessage);
var
    LMsg: TMsg;
begin
    if DHintWindowShower = Self then
        begin
            LMsg.hwnd := AHandle;
            LMsg.message := Message.Msg;
            LMsg.wParam := Message.WParam;
            LMsg.lParam := Message.LParam;
            if DHintWindow.IsHintMsg (LMsg) then
                Hide;
        end;
end;

constructor TDEngHint.Create;
begin
    FTimer := TTimer.Create (nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := FTimerOnTimer;
    if DHintWindow = nil then
        begin
            DHintWindow := THintWindow.Create (nil);
            HookDesignerHintHooks;
        end;
    Inc (DHintWindowRefCount);
end;

destructor TDEngHint.Destroy;
begin
    Hide;
    FTimer.Free;
    Dec (DHintWindowRefCount);
    if DHintWindowRefCount = 0 then
        begin
            DHintWindow.Free;
            DHintWindow := nil;
            UnhookDesignerHintHooks;
        end;
    if DHintWindowShower = Self then
        DHintWindowShower := nil;
    inherited;
end;

procedure TDEngHint.DoShowNoPause;

{ From Forms unit }

function GetCursorHeightMargin: Integer;
    var
        IconInfo: TIconInfo;
        BitmapInfoSize, BitmapBitsSize, ImageSize: DWORD;
        Bitmap: PBitmapInfoHeader;
        Bits: Pointer;
        BytesPerScanline: Integer;

        function FindScanline (Source: Pointer; MaxLen: Cardinal;
            Value: Cardinal): Cardinal; assembler;
        asm
              PUSH    ECX
              MOV     ECX,EDX
              MOV     EDX,EDI
              MOV     EDI,EAX
              POP     EAX
              REPE    SCASB
              MOV     EAX,ECX
              MOV     EDI,EDX
        end;

    begin
        { Default value is entire icon height }
        Result := GetSystemMetrics (SM_CYCURSOR);
        if GetIconInfo (GetCursor, IconInfo) then
            try
                GetDIBSizes (IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
                Bitmap := AllocMem (DWORD (BitmapInfoSize) + BitmapBitsSize);
                try
                    Bits := Pointer (DWORD (Bitmap) + BitmapInfoSize);
                    if GetDIB (IconInfo.hbmMask, 0, Bitmap^, Bits^) and
                        (Bitmap^.biBitCount = 1) then
                        begin
                            { Point Bits to the end of this bottom-up bitmap }
                            with Bitmap^ do
                                begin
                                    BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
                                    ImageSize := biWidth * BytesPerScanline;
                                    Bits := Pointer (DWORD (Bits) + BitmapBitsSize - ImageSize);
                                    { Use the width to determine the height since another mask bitmap
                                      may immediately follow }
                                    Result := FindScanline (Bits, ImageSize, $FF);
                                    { In case the and mask is blank, look for an empty scanline in the
                                      xor mask. }
                                    if (Result = 0) and (biHeight >= 2 * biWidth) then
                                        Result := FindScanline (Pointer (DWORD (Bits) - ImageSize),
                                            ImageSize, $00);
                                    Result := Result div BytesPerScanline;
                                end;
                            Dec (Result, IconInfo.yHotSpot);
                        end;
                finally
                    FreeMem (Bitmap, BitmapInfoSize + BitmapBitsSize);
                end;
            finally
                if IconInfo.hbmColor <> 0 then
                    DeleteObject (IconInfo.hbmColor);
                if IconInfo.hbmMask <> 0 then
                    DeleteObject (IconInfo.hbmMask);
            end;
    end;

var
    LP: TPoint;
    LR: TRect;

begin
    GetCursorPos (LP);
    if (FCheckControl <> nil) and (FindDragTarget (LP, True) <> FCheckControl) then
        Hide
    else
        begin
            FTimer.Enabled := False;
            DHintWindow.Color := Application.HintColor;
            DHintWindow.Font := Screen.HintFont;
            LR := DHintWindow.CalcHintRect (Screen.Width, Caption, nil);
            OffsetRect (LR, LP.x, LP.y + GetCursorHeightMargin);
            DHintWindow.ActivateHint (LR, Caption);
            DHintWindow.Update;
            DHintWindowShower := Self;
            if FNeedStartHideTimer then
                begin
                    FTimer.Interval := Application.HintHidePause;
                    FTimerMode := tmHide;
                    FTimer.Enabled := True;
                end;
            FActive := True;
        end;
end;

procedure TDEngHint.Hide;
begin
    FTimer.Enabled := False;
    if DHintWindowShower = Self then
        begin
            ShowWindow (DHintWindow.Handle, SW_HIDE);
            DHintWindowShower := nil;
        end;
    FActive := False;
end;

procedure TDEngHint.Show (APauseBeforeShow, AHideAfterPause, AUseHooks: Boolean;
    ACheckControl: TControl);
begin
    FNeedStartHideTimer := AHideAfterPause;
    FCheckControl := ACheckControl;
    FUseHooks := AUseHooks;
    if APauseBeforeShow then
        begin
            FTimer.Interval := Application.HintPause;
            FTimerMode := tmShow;
            FTimer.Enabled := True;
        end
    else
        DoShowNoPause;
end;

procedure TDEngHint.FTimerOnTimer (Sender: TObject);
begin
    case FTimerMode of
        tmShow: DoShowNoPause;
        tmHide: Hide;
    end;
end;

constructor TDSelCtrlItemPoint.Create (AOwnerSelCtrl: TDSelCtrlItem;
    APos: TDSelCtrlItemPointPos);
begin
    inherited Create (nil);
    FSelCtrl := AOwnerSelCtrl;
    FPos := APos;
    Width := HANDLESIZE;
    Height := HANDLESIZE;
    Visible := False;
    Update;
end;

procedure TDSelCtrlItemPoint.Paint;
begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle (0, 0, Width, Height);
end;

procedure TDSelCtrlItemPoint.Update;

const
    HANDLECURSORS: array [TDSelCtrlItemPointPos] of TCursor = (crSizeNWSE,
        crSizeNS, crSizeNESW, crSizeWE, crSizeNWSE, crSizeNS, crSizeNESW,
        crSizeWE);

var
    LLockMode: TELDesignerLockMode;
    LDesigner: TELCustomDesigner;

begin
    if (FSelCtrl.Control.Parent <> nil) and
        (FSelCtrl.Control.Parent.HandleAllocated) then
        ParentWindow := FSelCtrl.Control.Parent.Handle
    else
        ParentWindow := 0;
    if ParentWindow = 0 then
        Exit;
    case FPos of
        ppTopLeft, ppTop, ppTopRight:
            Top := FSelCtrl.Control.Top -
                (HANDLESIZE div 2) * Ord (FSelCtrl.Mode = imSizeable);
        ppRight, ppLeft:
            Top := FSelCtrl.Control.Top +
                FSelCtrl.Control.Height div 2 - (HANDLESIZE div 2);
        ppBottomRight, ppBottom, ppBottomLeft:
            Top := FSelCtrl.Control.Top + FSelCtrl.Control.Height - HANDLESIZE +
                (HANDLESIZE div 2) * Ord (FSelCtrl.Mode = imSizeable);
    end;
    case FPos of
        ppTopLeft, ppLeft, ppBottomLeft:
            Left := FSelCtrl.Control.Left -
                (HANDLESIZE div 2) * Ord (FSelCtrl.Mode = imSizeable);
        ppTop, ppBottom:
            Left := FSelCtrl.Control.Left +
                FSelCtrl.Control.Width div 2 - (HANDLESIZE div 2);
        ppTopRight, ppRight, ppBottomRight:
            Left := FSelCtrl.Control.Left + FSelCtrl.Control.Width - HANDLESIZE +
                (HANDLESIZE div 2) * Ord (FSelCtrl.Mode = imSizeable);
    end;
    LDesigner := FSelCtrl.FSelCtrls.FDesigner;
    LLockMode := TDEng.GetLockMode (FSelCtrl.Control);
    if FSelCtrl.FSelCtrls.FActive then
        begin
            if FSelCtrl.Mode = imSizeable then
                begin
                    FBorderColor := LDesigner.HandleBorderClr;
                    Color := LDesigner.HandleClr;
                end
            else
                begin
                    FBorderColor := LDesigner.MultySelectHandleBorderClr;
                    Color := LDesigner.MultySelectHandleClr;
                end;
            if lmNoMove in LLockMode then
                Color := LDesigner.LockedHandleClr;
            if lmNoResize in LLockMode then
                FBorderColor := LDesigner.LockedHandleClr;
        end
    else
        begin
            if FSelCtrl.Mode = imSizeable then
                begin
                    FBorderColor := LDesigner.InactiveHandleBorderClr;
                    Color := LDesigner.InactiveHandleClr;
                end
            else
                begin
                    FBorderColor := LDesigner.MultySelectHandleBorderClr;
                    Color := LDesigner.MultySelectHandleClr;
                end;
        end;

    Visible := (FSelCtrl.Mode = imSizeable) or
        ((FPos in [ppTopLeft, ppTopRight, ppBottomRight, ppBottomLeft]) and
        (FSelCtrl.Mode = imMultySelect));
    BringToFront;
    if (FSelCtrl.Mode = imSizeable) and not (lmNoResize in LLockMode) then
        Cursor := HANDLECURSORS [FPos]
    else
        Cursor := crDefault;
    Invalidate;
end;

constructor TDSelCtrlItem.Create (AOwnerSelCtrls: TELDesignerSelectedControls;
    AControl: TControl);
var
    LI: TDSelCtrlItemPointPos;
begin
    FSelCtrls := AOwnerSelCtrls;
    FControl := AControl;
    for LI := ppTopLeft to ppLeft do
        FPoints [LI] := TDSelCtrlItemPoint.Create (Self, LI);
    AControl.FreeNotification (FSelCtrls.FDesigner);
    FSelCtrls.AddItem (Self);
end;

destructor TDSelCtrlItem.Destroy;
var
    LI: TDSelCtrlItemPointPos;
    LJ: Integer;
begin
    Control.RemoveFreeNotification (FSelCtrls.FDesigner);
    LJ := FSelCtrls.IndexOfByItem (Self);
    if LJ <> -1 then
        FSelCtrls.DeleteItem (LJ);
    for LI := ppTopLeft to ppLeft do
        FPoints [LI].Free;
    inherited;
end;

procedure TDSelCtrlItem.SetMode (
    const Value: TDSelCtrlItemMode);
begin
    if FMode = Value then
        Exit;
    FMode := Value;
    Update;
end;

procedure TDSelCtrlItem.Update;
var
    LI: TDSelCtrlItemPointPos;
begin
    for LI := ppTopLeft to ppLeft do
        FPoints [LI].Update;
end;

procedure TELDesignerSelectedControls.UpdateMode;

    procedure _SetHandlesMode (Value: TDSelCtrlItemMode);
    var
        LI: Integer;
    begin
        for LI := 0 to Count - 1 do
            if TDSelCtrlItem (FItems [LI]).Control <> FRootWinControl then
                TDSelCtrlItem (FItems [LI]).SetMode (Value)
            else
                TDSelCtrlItem (FItems [LI]).SetMode (imNone);
    end;

begin
    if FVisible then
        begin
            if Count = 1 then
                _SetHandlesMode (imSizeable)
            else
                _SetHandlesMode (imMultySelect);
        end
    else
        _SetHandlesMode (imNone);
end;

procedure TELDesignerSelectedControls.AddItem (AItem: Pointer);
begin
    FItems.Add (AItem);
    if FVisible and (TDSelCtrlItem (AItem).Control.Parent <> nil) then
        TWinControlAccess (TDSelCtrlItem (AItem).Control.Parent).ShowControl (
            TDSelCtrlItem (AItem).Control);
    Change;
end;

procedure TELDesignerSelectedControls.Update;
var
    LI: Integer;
begin
    for LI := 0 to Count - 1 do
        TDSelCtrlItem (FItems [LI]).Update;
end;

function TELDesignerSelectedControls.IndexOf (
    AControl: TControl): Integer;
var
    LI: Integer;
begin
    Result := -1;
    for LI := 0 to Count - 1 do
        if TDSelCtrlItem (FItems [LI]).Control = AControl then
            begin
                Result := LI;
                Break;
            end;
end;

procedure TELDesignerSelectedControls.Clear;
begin
    ClearExcept (nil);
end;

procedure TELDesignerSelectedControls.SetActive (const Value: Boolean);
begin
    if FActive = Value then
        Exit;
    FActive := Value;
    Update;
end;

function TELDesignerSelectedControls.Add (AControl: TControl): Integer;
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    LI := IndexOf (AControl);
    if LI = -1 then
        begin
            CheckDesignControl (AControl);
            TDSelCtrlItem.Create (Self, AControl);
            Result := Count - 1;
        end
    else
        Result := LI;
end;

constructor TELDesignerSelectedControls.Create (AOwnerDesigner: TELCustomDesigner);
begin
    FDesigner := AOwnerDesigner;
    FItems := TList.Create;
end;

procedure TELDesignerSelectedControls.Delete (AI: Integer);
begin
    TObject (FItems [AI]).Free;
end;

procedure TELDesignerSelectedControls.DeleteItem (AI: Integer);
begin
    FItems.Delete (AI);
    Change;
end;

destructor TELDesignerSelectedControls.Destroy;
begin
    FDestroying := True;
    Clear;
    FItems.Free;
    inherited;
end;

function TELDesignerSelectedControls.GetCount: Integer;
begin
    Result := FItems.Count;
end;

function TELDesignerSelectedControls.IndexOfByItem (
    AItem: Pointer): Integer;
begin
    Result := FItems.IndexOf (AItem);
end;

procedure TELDesignerSelectedControls.SetVisible (const Value: Boolean);
begin
    if FVisible = Value then
        Exit;
    FVisible := Value;
    UpdateMode;
end;

procedure TELDesignerSelectedControls.SetRootWinControl (
    const Value: TWinControl);
begin
    if FRootWinControl = Value then
        Exit;
    FRootWinControl := Value;
    UpdateMode;
end;

function TELDesignerSelectedControls.GetItems (I: Integer): TControl;
begin
    Result := TDSelCtrlItem (FItems [I]).Control;
end;

procedure TELDesignerSelectedControls.Remove (AControl: TControl);
var
    LI: Integer;
begin
    LI := IndexOf (AControl);
    if LI <> -1 then
        Delete (LI);
end;

procedure TELDesignerSelectedControls.ClearExcept (AControl: TControl);
begin
    if not FDestroying then
        FDesigner.CheckActive (True);
    BeginUpdate;
    try
        while (Count > 0) and (Items [0] <> AControl) do
            Delete (0);
        while Count > 1 do
            Delete (1);
        if Count = 0 then
            begin
                if AControl <> nil then
                    Add (AControl);
            end
        else
            if FVisible and (AControl.Parent <> nil) then
                TWinControlAccess (AControl.Parent).ShowControl (AControl);
    finally
        EndUpdate;
    end;
end;

procedure TELDesignerSelectedControls.Change;
begin
    if FUpdateCount <= 0 then
        begin
            FChanged := False;
            if not FDestroying then
                begin
                    UpdateMode;
                    Update;
                    FDesigner.ChangeSelection;
                end;
        end
    else
        FChanged := True;
end;

procedure TELDesignerSelectedControls.BeginUpdate;
begin
    Inc (FUpdateCount);
end;

procedure TELDesignerSelectedControls.EndUpdate;
begin
    Dec (FUpdateCount);
    if FUpdateCount < 0 then
        FUpdateCount := 0;
    if (FUpdateCount = 0) and FChanged then
        Change;
end;

constructor TDEngCanvas.Create;
begin
    inherited Create;
    FHiddenControl := TWinControl.Create (nil);
end;

procedure TDEngCanvas.CreateHandle;
begin
    inherited;
    Handle := GetDCEx (FHiddenControl.Handle, 0, DCX_PARENTCLIP);
end;

destructor TDEngCanvas.Destroy;
begin
    FHiddenControl.Free;
    inherited;
end;

function TDEngCanvas.ScreenToClient (const AP: TPoint): TPoint;
begin
    Result := FHiddenControl.ScreenToClient (AP);
end;

procedure TDEngCanvas.SetWinControl (const Value: TWinControl);
begin
    FWinControl := Value;
    if Value <> nil then
        FHiddenControl.ParentWindow := Value.Handle
    else
        FHiddenControl.ParentWindow := 0;
    Handle := 0;
end;

procedure TDSelCtrlItemPoint.WndProc (var Message: TMessage);
var
    LDMSng: TELDMSizing;
    LEng: TDEng;
begin
    inherited;
    if (Message.Msg = WM_LBUTTONDOWN) and (FSelCtrl.Mode = imSizeable) then
        begin
            LDMSng.Msg := DESIGNER_SIZING;
            LDMSng.MouseMessage := @Message;
            FSelCtrl.FActivePos := FPos;
            LEng := FSelCtrl.FSelCtrls.FDesigner.FEng;
            LEng.IsDesignMsg (FSelCtrl.Control, TMessage (LDMSng));
        end;
end;

procedure TELCustomDesigner.ChangeSelection;
begin
    if Assigned (OnChangeSelection) then
        OnChangeSelection (Self);
end;

procedure TELCustomDesigner.CheckActive (AIsActiveNeeded: Boolean);
var
    LS: string;
begin
    if AIsActiveNeeded then
        LS := 'Designer mast be active'
    else
        LS := 'Designer mast be inactive';
    if Active <> AIsActiveNeeded then
        raise EELDesigner.Create (LS);
end;

procedure TELCustomDesigner.ControlHint (AControl: TControl; var AHint: string);
begin
    if Assigned (OnControlHint) then
        OnControlHint (Self, AControl, AHint);
end;

procedure TELCustomDesigner.ControlInserted;
begin
    if Assigned (OnControlInserted) then
        OnControlInserted (Self);
end;

procedure TELCustomDesigner.ControlInserting (
    var AControlClass: TControlClass);
begin
    if Assigned (OnControlInserting) then
        OnControlInserting (Self, AControlClass);
end;

constructor TELCustomDesigner.Create (AOwner: TComponent);
begin
    inherited;
    FGrid := TELDesignerGrid.Create (Self);
    FSelectedControls := TELDesignerSelectedControls.Create (Self);
    FSnapToGrid := True;
    FShowingHints := [htControl, htSize, htMove, htInsert];
    FHandleClr := clBlack;
    FHandleBorderClr := clBlack;
    FMultySelectHandleClr := clGray;
    FMultySelectHandleBorderClr := clGray;
    FInactiveHandleClr := clGray;
    FInactiveHandleBorderClr := clBlack;
    FLockedHandleClr := clRed;
    FClipboardFormat := SELDsgnrClipboardFormat;
end;

destructor TELCustomDesigner.Destroy;
begin
    Active := False;
    FGrid.Free;
    FSelectedControls.Free;
    inherited;
end;

function TELCustomDesigner.GetDesignControlVisible: Boolean;
begin
    CheckActive (True);
    Result := TDEng (FEng).RootVisible;
end;

procedure TELCustomDesigner.GridParamsChanged;
begin
    if Active then
        TDEng (FEng).GridParamsChanged;
end;

procedure TELCustomDesigner.Modified;
begin
    CheckActive (True);
    SelectedControls.Update;
end;

procedure TELCustomDesigner.DeleteSelectedControls;
begin
    CheckActive (True);
    TDEng (FEng).DeleteSelectedControls;
end;

procedure TELCustomDesigner.DoModified;
begin
    if Assigned (OnModified) then
        OnModified (Self);
end;

procedure TELCustomDesigner.Notification (AComponent: TComponent;
    Operation: TOperation);
begin
    inherited;
    if Operation = opRemove then
        begin
            if not FActivating then
                begin
                    if AComponent = DesignPanel then
                        begin
                            if DesignPanel.IsUsed then
                                Active := False;
                            ChangeDesignPanel (nil);
                        end;
                    if AComponent = DesignControl then
                        begin
                            Active := False;
                            DesignControl := nil;
                        end;
                end;
            if AComponent = PopupMenu then
                PopupMenu := nil;
            if AComponent is TControl then
                SelectedControls.Remove (TControl (AComponent));
        end;
end;

procedure TELCustomDesigner.DesignFormClose (var Action: TCloseAction);
begin
    if Assigned (OnDesignFormClose) then
        OnDesignFormClose (Self, Action);
end;

procedure TELCustomDesigner.SetActive (const Value: Boolean);
begin
    if FActive = Value then
        Exit;
    if Value then
        begin
            FActivating := True;
            try
                if DesignControl = nil then
                    raise EELDesigner.Create ('Design control not specified');
                if not (DesignControl is TCustomForm) then
                    begin
                        if DesignPanel = nil then
                            raise EELDesigner.Create ('No design panel specified');
                        if DesignPanel.IsUsed then
                            raise EELDesigner.CreateFmt ('Design panel ''%S'' is' +
                                ' used by another designer', [DesignPanel.Name]);
                    end;
                FEng := TDEng.Create (Self, DesignControl, DesignPanel);
                SelectedControls.SetRootWinControl (DesignControl);
            finally
                FActivating := False;
            end;
        end
    else
        begin
            FActivating := True;
            try
                SelectedControls.SetVisible (False);
                SelectedControls.Clear;
                SelectedControls.SetRootWinControl (nil);
                TObject (FEng).Free;
                FEng := nil;
            finally
                FActivating := False;
            end;
        end;
    FActive := Value;
    if FActive then
        begin
            SelectedControls.Add (DesignControl);
            SelectedControls.SetVisible (True);
        end;
end;

procedure TELCustomDesigner.SetDesignControl (const Value: TWinControl);
begin
    CheckActive (False);
    if FDesignControl <> nil then
        FDesignControl.RemoveFreeNotification (Self);
    FDesignControl := Value;
    if FDesignControl <> nil then
        FDesignControl.FreeNotification (Self);
end;

procedure TELCustomDesigner.SetDesignPanel (const Value: TELCustomDesignPanel);
begin
    CheckActive (False);
    ChangeDesignPanel (Value);
end;

procedure TELCustomDesigner.SetDesignControlVisible (const Value: Boolean);
begin
    CheckActive (True);
    TDEng (FEng).RootVisible := Value;
end;

procedure TELCustomDesigner.ValidateName (const AName: string;
    var AIsValidName: Boolean);
begin
    if Assigned (OnValidateName) then
        OnValidateName (Self, AName, AIsValidName);
end;

procedure TELCustomDesigner.ChangeDesignPanel (
    const Value: TELCustomDesignPanel);
begin
    if FDesignPanel <> nil then
        FDesignPanel.RemoveFreeNotification (Self);
    FDesignPanel := Value;
    if FDesignPanel <> nil then
        FDesignPanel.FreeNotification (Self);
end;

procedure TELCustomDesigner.SetPopupMenu (const Value: TPopupMenu);
begin
    FPopupMenu := Value;
end;

procedure TELCustomDesigner.SetGrid (const Value: TELDesignerGrid);
begin
    FGrid.Assign (Value);
end;

procedure TELCustomDesigner.ContextPopup;
var
    LPt: TPoint;
    LHandled: Boolean;
    LPopupMenu: TPopupMenu;
begin
    GetCursorPos (LPt);
    LHandled := False;
    if Assigned (OnContextPopup) then
        OnContextPopup (Self, LPt, LHandled);
    if LHandled then
        Exit;
    LPopupMenu := GetPopupMenu;
    if (LPopupMenu <> nil) and LPopupMenu.AutoPopup then
        begin
            LPopupMenu.PopupComponent := Self;
            LPopupMenu.Popup (LPt.X, LPt.Y);
        end;
end;

function TELCustomDesigner.GetPopupMenu: TPopupMenu;
begin
    Result := FPopupMenu;
end;

procedure TELDesignerSelectedControls.SelectAll;
begin
    FDesigner.CheckActive (True);
    TDEng (FDesigner.FEng).SelectAll;
end;

procedure TELCustomDesigner.LockControl (AControl: TControl;
    ALockMode: TELDesignerLockMode);
begin
    CheckActive (True);
    TDEng (FEng).LockControl (AControl, ALockMode);
end;

function TELCustomDesigner.GetLockMode (AControl: TControl): TELDesignerLockMode;
begin
    if Active then
        Result := TDEng.GetLockMode (AControl)
    else
        Result := [];
end;

procedure TELCustomDesigner.SetHandleBorderClr (const Value: TColor);
begin
    FHandleBorderClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetHandleClr (const Value: TColor);
begin
    FHandleClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetInactiveHandleBorderClr (const Value: TColor);
begin
    FInactiveHandleBorderClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetInactiveHandleClr (const Value: TColor);
begin
    FInactiveHandleClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetLockedHandleClr (const Value: TColor);
begin
    FLockedHandleClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetMultySelectHandleBorderClr (const Value: TColor);
begin
    FMultySelectHandleBorderClr := Value;
    SelectedControls.Update;
end;

procedure TELCustomDesigner.SetMultySelectHandleClr (const Value: TColor);
begin
    FMultySelectHandleClr := Value;
    SelectedControls.Update;
end;

procedure TELDesignerSelectedControls.SelectControls (
    AControls: TList);
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    BeginUpdate;
    try
        for LI := Count - 1 downto 0 do
            if AControls.IndexOf (Items [LI]) = -1 then
                Delete (LI);
        for LI := 0 to AControls.Count - 1 do
            Add (AControls [LI]);
    finally
        EndUpdate;
    end;
end;

procedure TELCustomDesigner.Copy;
var
    LMemHandle: THandle;
    LGlobalDataPtr: Pointer;
    LStream: TMemoryStream;
    LControls: TList;
begin
    if CanCopy then
        begin
            RegisterClipboardFormat;
            SelectedControls.ClearNotChildrensOf (
                SelectedControls.DefaultControl.Parent);
            if SelectedControls.Count > 0 then
                begin
                    LStream := TMemoryStream.Create;
                    try
                        LControls := TList.Create;
                        try
                            SelectedControls.GetControls (LControls);
                            TDEng (FEng).SaveControlsToStream (LStream, LControls);
                        finally
                            LControls.Free;
                        end;
                        LMemHandle := GlobalAlloc (GMEM_MOVEABLE + GMEM_DDESHARE, LStream.Size);
                        try
                            LGlobalDataPtr := GlobalLock (LMemHandle);
                            try
                                Move (LStream.Memory^, LGlobalDataPtr^, LStream.Size);
                            finally
                                GlobalUnlock (LMemHandle);
                            end;
                        except
                            GlobalFree (LMemHandle);
                        end;
                    finally
                        LStream.Free;
                    end;
                    Clipboard.SetAsHandle (FClipboardFormatWord, LMemHandle);
                end;
        end;
end;

procedure TELCustomDesigner.Cut;
begin
    if CanCut then
        begin
            Copy;
            DeleteSelectedControls;
        end;
end;

procedure TELCustomDesigner.Paste;
var
    LMemHandle: THandle;
    LGlobalDataPtr: Pointer;
    LStream: TMemoryStream;
    LLocalDataPtr: Pointer;
    LLocalDataSize: Integer;
    LParent: TWinControl;
begin
    if CanPaste then
        begin
            LMemHandle := Clipboard.GetAsHandle (FClipboardFormatWord);
            LGlobalDataPtr := GlobalLock (LMemHandle);
            try
                LLocalDataPtr := nil;
                LLocalDataSize := GlobalSize (LMemHandle);
                ReallocMem (LLocalDataPtr, LLocalDataSize);
                try
                    Move (LGlobalDataPtr^, LLocalDataPtr^, LLocalDataSize);

                    LStream := TMemoryStream.Create;
                    try
                        LStream.Size := LLocalDataSize;
                        Move (LLocalDataPtr^, LStream.Memory^, LLocalDataSize);

                        if SelectedControls.Count > 0 then
                            LParent := TDEng (FEng).FindContainer (
                                SelectedControls.DefaultControl)
                        else
                            LParent := nil;
                        if LParent = nil then
                            LParent := TDEng (FEng).FRoot;
                        TDEng (FEng).LoadControlsFromStream (LStream, LParent);
                    finally
                        LStream.Free;
                    end;
                finally
                    FreeMem (LLocalDataPtr);
                end;
            finally
                GlobalUnlock (LMemHandle);
            end;
        end;
end;

procedure TELCustomDesigner.SetClipboardFormat (const Value: string);
begin
    if FClipboardFormat = Value then
        Exit;
    FClipboardFormat := Value;
    FClipbrdFormatRegistered := False;
    RegisterClipboardFormat;
end;

procedure TELCustomDesigner.RegisterClipboardFormat;
begin
    if not FClipbrdFormatRegistered then
        FClipboardFormatWord := Windows.RegisterClipboardFormat (
            PChar (FClipboardFormat));
    FClipbrdFormatRegistered := True;
end;

procedure TELCustomDesigner.KeyDown (var Key: Word; Shift: TShiftState);
begin
    if Assigned (OnKeyDown) then
        OnKeyDown (Self, Key, Shift);
end;

procedure TELCustomDesigner.KeyPress (var Key: Char);
begin
    if Assigned (OnKeyPress) then
        OnKeyPress (Self, Key);
end;

procedure TELCustomDesigner.KeyUp (var Key: Word; Shift: TShiftState);
begin
    if Assigned (OnKeyUp) then
        OnKeyUp (Self, Key, Shift);
end;

function TELCustomDesigner.DoKeyDown (var AMessage: TWMKey): Boolean;
var
    LShiftState: TShiftState;
begin
    Result := True;
    with AMessage do
        begin
            LShiftState := KeyDataToShiftState (KeyData);
            KeyDown (CharCode, LShiftState);
            if CharCode = 0 then
                Exit;
        end;
    Result := False;
end;

function TELCustomDesigner.DoKeyPress (var AMessage: TWMKey): Boolean;
var
    LCh: Char;
begin
    Result := True;
    with AMessage do
        begin
            LCh := Char (CharCode);
            KeyPress (LCh);
            CharCode := Word (LCh);
            if Char (CharCode) = #0 then
                Exit;
        end;
    Result := False;
end;

function TELCustomDesigner.DoKeyUp (var AMessage: TWMKey): Boolean;
var
    LShiftState: TShiftState;
begin
    Result := True;
    with AMessage do
        begin
            LShiftState := KeyDataToShiftState (KeyData);
            KeyUp (CharCode, LShiftState);
            if CharCode = 0 then
                Exit;
        end;
    Result := False;
end;

procedure TELCustomDesigner.DoNotification (AnObject: TPersistent;
    Operation: TOperation);
begin
    if Assigned (OnNotification) then
        OnNotification (Self, AnObject, Operation);
end;

procedure TELDesignerSelectedControls.ClearNotChildrensOf (
    AParent: TWinControl);
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    BeginUpdate;
    try
        for LI := Count - 1 downto 0 do
            if Items [LI].Parent <> AParent then
                Delete (LI);
    finally
        EndUpdate;
    end;
end;

function TELCustomDesigner.IsUniqueName (const AName: string): Boolean;
begin
    CheckActive (True);
    Result := TDEng (FEng).IsUniqueName (AName);
end;

procedure TELCustomDesigner.GetUniqueName (const ABaseName: string;
    var AUniqueName: string);
begin
    if Assigned (OnGetUniqueName) then
        OnGetUniqueName (Self, ABaseName, AUniqueName);
end;

procedure TELCustomDesigner.DblClick;
begin
    if Assigned (OnDblClick) then
        OnDblClick (Self);
end;

function TELCustomDesigner.CanCopy: Boolean;
var
    LI: Integer;
begin
    if Active and
        (((SelectedControls.Count = 1) and
        (SelectedControls [0] <> TDEng (FEng).FRoot)) or
        (SelectedControls.Count > 1)) then
        begin
            Result := True;
            for LI := 0 to SelectedControls.Count - 1 do
                if
                    (SelectedControls [LI].Parent = SelectedControls.DefaultControl.Parent)
                    and (SelectedControls [LI] <> TDEng (FEng).FRoot)
                    and (lmNoCopy in TDEng (FEng).GetFullLockMode (SelectedControls [LI])) then
                    begin
                        Result := False;
                        Break;
                    end;
        end
    else
        Result := False;
end;

function TELCustomDesigner.CanPaste: Boolean;
var
    LC: TWinControl;
begin
    if Active then
        begin
            Result := True;
            if SelectedControls.Count > 0 then
                begin
                    LC := TDEng (FEng).FindContainer (SelectedControls.DefaultControl);
                    if (LC = nil) or (lmNoInsertin in TDEng (FEng).GetLockMode (LC)) then
                        Result := False;
                end;
            if Result then
                begin
                    RegisterClipboardFormat;
                    Result := Clipboard.HasFormat (FClipboardFormatWord);
                end;
        end
    else
        Result := False;
end;

function TELCustomDesigner.CanCut: Boolean;
var
    LI: Integer;
begin
    if Active and
        (((SelectedControls.Count = 1) and
        (SelectedControls [0] <> TDEng (FEng).FRoot)) or
        (SelectedControls.Count > 1)) then
        begin
            Result := True;
            for LI := 0 to SelectedControls.Count - 1 do
                if
                    (SelectedControls [LI].Parent = SelectedControls.DefaultControl.Parent)
                    and (SelectedControls [LI] <> TDEng (FEng).FRoot)
                    and ((lmNoCopy in TDEng (FEng).GetFullLockMode (SelectedControls [LI]))
                    or (lmNoDelete in TDEng (FEng).GetFullLockMode (SelectedControls [LI]))) then
                    begin
                        Result := False;
                        Break;
                    end;
        end
    else
        Result := False;
end;

procedure TELCustomDesigner.LockAll (ALockMode: TELDesignerLockMode);
var
    LI: Integer;
begin
    CheckActive (True);
    for LI := 0 to TDEng (FEng).FRoot.ComponentCount - 1 do
        if TDEng (FEng).FRoot.Components [LI] is TControl then
            TDEng (FEng).LockControl (TControl (TDEng (FEng).FRoot.Components [LI]),
                ALockMode);
end;

procedure TDEngNames.Add (const ASourceName, ADestName: string);
begin
    FSourceNames.Add (ASourceName);
    FDestNames.Add (ADestName);
end;

procedure TDEngNames.Clear;
begin
    FSourceNames.Clear;
    FDestNames.Clear;
end;

constructor TDEngNames.Create;
begin
    FSourceNames := TStringList.Create;
    FDestNames := TStringList.Create;
end;

function TDEngNames.DestName (AI: Integer): string;
begin
    Result := FDestNames [AI];
end;

destructor TDEngNames.Destroy;
begin
    Clear;
    FSourceNames.Free;
    FDestNames.Free;
    inherited;
end;

function TDEngNames.IndexOf (const ASourceName: string): Integer;
begin
    Result := FSourceNames.IndexOf (ASourceName);
end;

procedure TELDesignerSelectedControls.GetControls (AResult: TList);
var
    LI: Integer;
begin
    AResult.Clear;
    for LI := 0 to Count - 1 do
        AResult.Add (Items [LI]);
end;

function TELDesignerSelectedControls.GetDefaultControl: TControl;
begin
    if Count > 0 then
        Result := Items [Count - 1]
    else
        Result := nil;
end;

procedure TELDesignerSelectedControls.Lock (ALockMode: TELDesignerLockMode);
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    for LI := 0 to Count - 1 do
        FDesigner.LockControl (Items [LI], ALockMode);
end;

procedure TELDesignerSelectedControls.AlignToGrid;
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    for LI := 0 to Count - 1 do
        TDEng (FDesigner.FEng).AlignToGrid (Items [LI]);
end;

procedure TELDesignerSelectedControls.BringToFront;
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    for LI := 0 to Count - 1 do
        if Items [LI] <> TDEng (FDesigner.FEng).FRoot then
            Items [LI].BringToFront;
    TDEng (FDesigner.FEng).Modified;
end;

procedure TELDesignerSelectedControls.SendToBack;
var
    LI: Integer;
begin
    FDesigner.CheckActive (True);
    for LI := 0 to Count - 1 do
        if Items [LI] <> TDEng (FDesigner.FEng).FRoot then
            Items [LI].SendToBack;
    TDEng (FDesigner.FEng).Modified;
end;

function Align_HorzSortProc (Item1, Item2: Pointer): Integer;
begin
    if TControl (Item1).Left < TControl (Item2).Left then
        Result := 1
    else
        if TControl (Item1).Left > TControl (Item2).Left then
            Result := -1
        else
            Result := 0;
end;

function Align_VertSortProc (Item1, Item2: Pointer): Integer;
begin
    if TControl (Item1).Top < TControl (Item2).Top then
        Result := 1
    else
        if TControl (Item1).Top > TControl (Item2).Top then
            Result := -1
        else
            Result := 0;
end;

procedure TELDesignerSelectedControls.Align (AHorzAlign,
    AVertAlign: TELDesignerAlignType);

type
    _TEdges = (etLeft, etTop, etRight, etBottom);
    _TDirection = (dtHorz, dtVert);

    procedure _AlignAdges (AEdges: _TEdges);
    var
        LBase, LCur, LI: Integer;
    begin
        if Count < 2 then
            Exit;
        LBase := 0 + MaxInt * Ord (AEdges in [etLeft, etTop]);
        for LI := 0 to Count - 1 do
            begin
                LCur := 0; // Initialize
                case AEdges of
                    etLeft: LCur := Items [LI].Left;
                    etTop: LCur := Items [LI].Top;
                    etRight: LCur := Items [LI].Left + Items [LI].Width;
                    etBottom: LCur := Items [LI].Top + Items [LI].Height;
                end;
                if AEdges in [etLeft, etTop] then
                    begin
                        if LCur < LBase then
                            LBase := LCur;
                    end
                else
                    if LCur > LBase then
                        LBase := LCur;
            end;
        for LI := 0 to Count - 1 do
            begin
                case AEdges of
                    etLeft: Items [LI].Left := LBase;
                    etTop: Items [LI].Top := LBase;
                    etRight: Items [LI].Left := LBase - Items [LI].Width;
                    etBottom: Items [LI].Top := LBase - Items [LI].Height;
                end;
            end;
        TDEng (FDesigner.FEng).Modified;
    end;

    procedure _SpaceEqually (ADir: _TDirection);
    var
        LI: Integer;
        LMin, LMax, LCur: Integer;
        LControls: TList;
    begin
        if Count <= 2 then
            Exit;
        LControls := TList.Create;
        GetControls (LControls);
        try
            if ADir = dtHorz then
                begin
                    LControls.Sort (Align_HorzSortProc);
                    LMin := TControl (LControls [0]).Left;
                    LMax := TControl (LControls [LControls.Count - 1]).Left;
                end
            else
                begin
                    LControls.Sort (Align_VertSortProc);
                    LMin := TControl (LControls [0]).Top;
                    LMax := TControl (LControls [LControls.Count - 1]).Top;
                end;
            for LI := 1 to LControls.Count - 2 do
                begin
                    LCur := LMin + Round (LI * (LMax - LMin) / (LControls.Count - 1));
                    if ADir = dtHorz then
                        TControl (LControls [LI]).Left := LCur
                    else
                        TControl (LControls [LI]).Top := LCur;
                end;
        finally
            LControls.Free;
        end;
        TDEng (FDesigner.FEng).Modified;
    end;

    procedure _SetCenters (ADir: _TDirection; AValue: Integer);
    var
        LI: Integer;
    begin
        for LI := 0 to Count - 1 do
            begin
                if ADir = dtHorz then
                    Items [LI].Left := AValue - Items [LI].Width div 2
                else
                    Items [LI].Top := AValue - Items [LI].Height div 2;
            end;
    end;

    procedure _Center (ADir: _TDirection);
    begin
        if Count < 2 then
            Exit;
        if ADir = dtHorz then
            _SetCenters (ADir, Items [0].Left + Items [0].Width div 2)
        else
            _SetCenters (ADir, Items [0].Top + Items [0].Height div 2);
        TDEng (FDesigner.FEng).Modified;
    end;

    procedure _CenterInWindow (ADir: _TDirection);
    begin
        if Count < 1 then
            Exit;
        if ADir = dtHorz then
            _SetCenters (ADir, Items [0].Parent.Width div 2)
        else
            _SetCenters (ADir, Items [0].Parent.Height div 2);
        TDEng (FDesigner.FEng).Modified;
    end;

begin
    FDesigner.CheckActive (True);
    if (DefaultControl <> nil) and
        (DefaultControl <> TDEng (FDesigner.FEng).FRoot) then
        begin
            ClearNotChildrensOf (DefaultControl.Parent);
            BeginUpdate;
            try
                case AHorzAlign of
                    atLeftTop: _AlignAdges (etLeft);
                    atRightBottom: _AlignAdges (etRight);
                    atSpaceEqually: _SpaceEqually (dtHorz);
                    atCenter: _Center (dtHorz);
                    atCenterInWindow: _CenterInWindow (dtHorz);
                end;
                case AVertAlign of
                    atLeftTop: _AlignAdges (etTop);
                    atRightBottom: _AlignAdges (etBottom);
                    atSpaceEqually: _SpaceEqually (dtVert);
                    atCenter: _Center (dtVert);
                    atCenterInWindow: _CenterInWindow (dtVert);
                end;
            finally
                Update;
                EndUpdate;
            end;
        end;
end;

procedure TELDesignerSelectedControls.CheckDesignControl (
    AControl: TControl);
begin
    if not TDEng (FDesigner.FEng).IsDesignControl (AControl) then
        raise EELDesigner.CreateFmt ('Control ''%S'' can not be selected',
            [AControl.Name]);
end;

procedure TDEng.UpdateGridPattern;
var
    LI, LJ, LXOff, LYOff: Integer;
begin
    FGridBkColor := FForm.Color;
    FGridHScrollPos := FForm.HorzScrollBar.ScrollPos;
    FGridVScrollPos := FForm.VertScrollBar.ScrollPos;
    LXOff := FGridHScrollPos mod FGrid.XStep;
    LYOff := FGridVScrollPos mod FGrid.YStep;
    with FGridBitmap do
        begin
            Width := 2 * FGrid.XStep;
            Height := 2 * FGrid.YStep;
            Canvas.Brush.Color := FGridBkColor;
            Canvas.Brush.Style := bsSolid;
            Canvas.FillRect (Rect (0, 0, FGridBitmap.Width, FGridBitmap.Height));

            LI := -LXOff;
            while LI < FGridBitmap.Width do
                begin
                    LJ := -LYOff;
                    while LJ < FGridBitmap.Height do
                        begin
                            if (LI >= 0) and (LJ >= 0) then
                                Canvas.Pixels [LI, LJ] := FGrid.Color;
                            Inc (LJ, FGrid.YStep);
                        end;
                    Inc (LI, FGrid.XStep);
                end;
        end;
    if FGridBrush <> 0 then
        DeleteObject (FGridBrush);
    FGridBrush := CreatePatternBrush (FGridBitmap.Handle);
end;

procedure TELDesignerSelectedControls.UpdateControl (AIndex: Integer);
begin
    TDSelCtrlItem (FItems [AIndex]).Update;
end;

procedure TELCustomDesigner.DragOver (ASource, ATarget: TObject; AX, AY: Integer;
    AState: TDragState; var AAccept: Boolean);
begin
    AAccept := False;
    if Assigned (FOnDragOver) then
        begin
            AAccept := True;
            FOnDragOver (Self, ASource, ATarget, AX, AY, AState, AAccept);
        end;
end;

procedure TELCustomDesigner.DragDrop (ASource, ATarget: TObject; AX, AY: Integer);
begin
    if Assigned (FOnDragDrop) then
        FOnDragDrop (Self, ASource, ATarget, AX, AY);
end;

end.

