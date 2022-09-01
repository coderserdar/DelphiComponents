{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_edit;

interface
{$I psc_defines.inc}

Uses
  stdctrls,
  Winapi.Windows,
  Winapi.messages,
  forms,
  controls,
  menus,
  classes,
  db,
  extctrls,
  SysUtils,
  System.Types,

  myla_system,
  myla_interfaces,

  psc_theme,
  psc_procs,
  psc_wrapper,
  psc_const;

type
  TPSCButtonClickEvent = Procedure(Sender: TObject; ABtnIndex: Integer) Of
    Object;

  TPSCEditBtnKind = (
    bkComboBox,
    bkUpDown
  );

  TPSCCreateThemeProc=function(AUserData:Cardinal):IPSCThemeLib;

  IPSCEditorAccess = Interface
    ['{2EC34889-DFFA-42D3-9ABC-48F7307FD26B}']
    Procedure Clear;
    Procedure ClearSelection;
    Procedure CopyToClipboard;
    Procedure CutToClipboard;
    Procedure PasteFromClipboard;
    Procedure Undo;
    Procedure ClearUndo;
    Procedure SelectAll;

    Function CanUndo:Boolean;
    Function GetModified:Boolean;
    procedure SetModified(V:Boolean);
    property Modified:Boolean Read GetModified Write SetModified;

    Function GetEditText: String;
    Function GetIsMasked: boolean;
    Function GetSelText: String;
    Function GetSelStart: Integer;
    Function GetSelLength: Integer;
    Function GetText: String;
    Function GetReadOnly: boolean;
    Function GetPasswordChar: Char;
    Function GetOEMConvert: boolean;
    Function GetCharCase: TPSCEditCharCase;
    Function GetHideSelection: boolean;
    Function GetMaxLength: Integer;
    Function GetAutoSelect: boolean;
    Function GetEditMask: String;

    Procedure SetBorderStyle(V: TBorderStyle);
    Procedure SetEditText(Const V: String);
    Procedure SetAutoSize(V: boolean);
    Procedure SetDataField(Const V: String);
    Procedure SetDataSource(V: TDataSource);
    Procedure SetSelText(Const V: String);
    Procedure SetSelStart(V: Integer);
    Procedure SetSelLength(V: Integer);
    Procedure SetText(Const V: String);
    Procedure SetReadOnly(V: boolean);
    Procedure SetPasswordChar(V: Char);
    Procedure SetOEMConvert(V: boolean);
    Procedure SetCharCase(V: TPSCEditCharCase);
    Procedure SetHideSelection(V: boolean);
    Procedure SetMaxLength(V: Integer);
    Procedure SetAutoSelect(V: boolean);
    Procedure SetEditMask(Const V: String);
  End;

  TPSCCustomButtonControl = class(TPSCCustomControlAncestor)
  private
    FSaveHotState:Boolean;
    FButtonState:Integer;
    FThemeLib: IPSCThemeLib;
    FAutoSize: boolean;
    FOnButtonClick: TPSCButtonClickEvent;
    FOnButtonDown: TPSCButtonClickEvent;
    FOnButtonUp: TPSCButtonClickEvent;
    FTimer: TObject;
    FThemeName:String;
    FButtonsVisible: Boolean;
    FBtnKind: TPSCEditBtnKind;
    FPressedIndex: Integer;
    FHandler:IPSCEventHandler;

    function GetBtnKind: TPSCEditBtnKind;
    function GetButtonsVisible: Boolean;
    Function UpdatePressedIndex(P: TPoint): boolean;
    function IsThemeNameStored:Boolean;

    procedure SetThemeName(const V:String);
    Procedure EnableTimer;
    Procedure KillTimer;
    Procedure TimerEvent(Timer: TObject; EventID: Integer);
    Procedure SetButtonsVisible(V: Boolean);
    procedure SetTheme(const AValue: IPSCThemeLib);

  protected
    Procedure HandleEvent(const AParams:TPSCEventParams);override;

    function IsModified:Boolean;virtual;
    procedure SelectAll;virtual;

    function GetEditState:Integer;
    function GetButtonState:Integer;
    function GetSpinState(ABtnIndex:Integer):Integer;

    Function GetBtnRect: TRect;
    Function GetBtnUpDownRect(UpButton: boolean): TRect;

    Function GetDroppedDown: Boolean;virtual;
    Function IsHot:Boolean;

    function GetDefaultColor(AState,AColorID:Integer):TPSCColor;

    Procedure AdjustEdit;virtual;
    procedure ThemeChanged;
    procedure Changed;virtual;
    Procedure WMEraseBkgnd(Var Msg: TWMEraseBkgnd); message WM_EraseBkgnd;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MouseLeave;
    Procedure CreateWnd; override;
    Procedure CMFontChanged(Var Message: TMessage); message CM_FONTCHANGED;
    Procedure SetAutoSize(V: boolean);{$IFDEF D6} override;{$ENDIF}
    Procedure SetBtnKind(V: TPSCEditBtnKind); virtual;
    Procedure DoButtonClick(BtnIndex: Integer); virtual;
    Procedure DoButtonDown(BtnIndex: Integer); virtual;
    Procedure DoButtonUp(BtnIndex: Integer); virtual;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure MouseDown(Button: TMouseButton; Shift:TShiftState;
      X,Y: Integer);override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure SetEnabled(Value: Boolean); override;
    Procedure Loaded;override;

    Property BtnKind: TPSCEditBtnKind read GetBtnKind write SetBtnKind default
      bkComboBox;
    Property OnButtonClick: TPSCButtonClickEvent read FOnButtonClick write
      FOnButtonClick;
    Property OnButtonDown: TPSCButtonClickEvent read FOnButtonDown write
      FOnButtonDown;
    Property OnButtonUp: TPSCButtonClickEvent read FOnButtonUp write
      FOnButtonUp;
    Property ButtonsVisible: Boolean read GetButtonsVisible write SetButtonsVisible
      default False;
    property EditState: Integer read GetEditState;
  public
    function GetInplaceEditRect:TRect;virtual;

    Procedure Paint;override;

    Destructor Destroy;override;
    constructor Create(AOwner:TComponent);override;

    property ThemeLib: IPSCThemeLib Read FThemeLib Write SetTheme;
  published
    Property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property ThemeName: String Read FThemeName Write SetThemeName Stored IsThemeNameStored;
  end;

  TPSCOnPopupClosed = Procedure(Sender: TObject; ACanceled: Boolean) Of
    Object;
  TPSCPopupShowKind = (pskUnderParam,pskExact,pskCentered,pskUnderControl);
  TPSCPopupOption = (poParentColor,poParentFontColor);
  TPSCPopupOptions = Set Of TPSCPopupOption;

  TPSCPopupForm = Class(TForm)
  private
    FAlreadyClosing:Boolean;
    FButtonsAdded:Integer;
    FFooter:TPSCPanel;
    FWantReturns: Boolean;
    FOwnerControl: TControl;
    FOnPopupClosed: TPSCOnPopupClosed;
    FOnBeforePopup: TPSCNotifyEvent;
    FOkButton:TPSCSpeedButton;
    FCancelButton:TPSCSpeedButton;

    procedure InitializePopup;
    procedure CancelButtonOnClick(Sender:TObject);
    procedure OkButtonOnClick(Sender:TObject);
  protected
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure WMActivate(Var Message: TWMActivate); message WM_ACTIVATE;
    Procedure WMMouseActivate(Var Message: TWMMouseActivate); message
      WM_MOUSEACTIVATE;
    Procedure WMNCActivate(Var Message: TWMNCActivate); message WM_NCACTIVATE;
    Procedure CMFONTCHANGED(Var M: TMessage); message CM_FONTCHANGED;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure KeyPress(Var Key: Char); override;
    Procedure ClosePopup(Canceled,AHide:boolean); virtual;
  public
    function AddButton:TPSCSpeedButton;overload;
    function GetFooterPanel: TPSCPanel; virtual;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer=0); override;
    Procedure ResizeFormControls; virtual;
    Procedure SetBounds(ALeft,ATop,AWidth,AHeight: Integer); override;
    Procedure Hide;
    Procedure Show;
    Procedure PopupExact(Control: TControl; X,Y: Integer; PopupEvent:
      TPSCOnPopupClosed);
    Procedure Popup(Control: TControl; ParamRect: TRect; PopupEvent:
      TPSCOnPopupClosed); virtual;
    Procedure PopupEx(Control: TControl; Const PopupRect: TRect;
      PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
      ShowOptions: TPSCPopupOptions);virtual;
    Procedure PopupUnderControl(Control: TControl;
      PopupEvent: TPSCOnPopupClosed);
    Property OwnerControl:TControl Read FOwnerControl Write FOwnerControl Stored False;

    property OkButton:TPSCSpeedButton Read FOkButton;
    property CancelButton:TPSCSpeedButton Read FCancelButton;
  published
    Property WantReturns: Boolean Read FWantReturns Write FWantReturns;
    Property BorderStyle default bsToolWindow;
    Property BevelEdges;
    Property BevelInner;
    Property BevelOuter;
    Property BevelKind;
    Property BevelWidth;
    Property OnPopupClosed: TPSCOnPopupClosed read FOnPopupClosed write
      FOnPopupClosed;
    Property OnBeforePopup: TPSCNotifyEvent Read FOnBeforePopup Write FOnBeforePopup;
  End;

  TPSCPopupFormClass=class of TPSCPopupForm;

  TPSCCustomPopupEdit = Class(TPSCCustomButtonControl)
  private
    FOnUpdatePopup: TPSCUpdatePopupEvent;
    FPopupParams: TPSCPopupParams;
    FPopupColor: TPSCColor;
    FPopup: TPSCPopupForm;
    FOnDropDown: TPSCNotifyEvent;
    FOnCloseUp: TPSCNotifyEvent;
    FPopupControl:TControl;

    Function GetPopup: TPSCPopupForm;
    Function InternalPostValue(ASilent: boolean): boolean;

    procedure SetPopupControl(V:TControl);
    Procedure SetDroppedDown(A: Boolean);
    Procedure DoCloseUp;
    Procedure SetPopupColor(V: TPSCColor);
    Procedure SetPopupParams(V: TPSCPopupParams);
    Procedure InternalShowPopup; virtual;
    Procedure UpdatePopupParams(PopupForm: TPSCPopupForm); virtual;
  protected
    Function GetDroppedDown: Boolean;override;
    Function CreatePopup: TPSCPopupForm; virtual;
    Function PostValue: Boolean; virtual;
    Function PopupCreated: boolean;

    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    Procedure DoDropDown; virtual;
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState;
      X, Y: Integer);override;
    Procedure GetDropped(Var Message: TMessage); message CB_GETDROPPEDSTATE;
    Procedure DoButtonDown(BtnIndex: Integer); override;
    Procedure UpdatePopup; virtual;
    Procedure KeyPress(Var Key: Char); override;
    Procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean); virtual;
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); override;
    Procedure CancelValue; virtual;
    Procedure CMExit(Var M: TMessage); message cm_exit;

    Property OnDropDown: TPSCNotifyEvent read FOnDropDown write FOnDropDown;
    Property OnCloseUp: TPSCNotifyEvent read FOnCloseUp write FOnCloseUp;
    Property PopupColor: TPSCColor read FPopupColor write SetPopupColor default
      clPSCWindow;
    Property PopupControl:TControl Read FPopupControl Write SetPopupControl;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Property Popup: TPSCPopupForm read GetPopup;
    Property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    Property WantReturns;
  published
    Property OnUpdatePopup: TPSCUpdatePopupEvent read FOnUpdatePopup write
      FOnUpdatePopup;
    Property PopupParams: TPSCPopupParams read FPopupParams write
      SetPopupParams;
  End;

  TPSCCustomContainerEdit = Class(TPSCCustomPopupEdit)
  private
    FDataSource:TDataSource;
    FDataField:String;
    FEditorAccess: IPSCEditorAccess;
    FEdit: TControl;
    FOnChange: TPSCNotifyEvent;

    Procedure SetEditEditText(Const V: String);
    Procedure SetEditDataSource(V: TDataSource);
    Procedure SetEditDataField(Const V: String);
    Procedure SetEditModified(V: boolean);
    Procedure SetEditSelLength(V: Integer);
    Procedure SetEditSelStart(V: Integer);
    Procedure SetEditSelText(Const V: String);
    Procedure SetEditDragCursor(V: TCursor);
    Procedure SetEditDragKind(V: TDragKind);
    Procedure SetEditDragMode(V: TDragMode);
    Procedure SetEditMaxLength(V: Integer);
    Procedure SetEditPopupMenu(V: TPopupMenu);
    Procedure SetEditAutoSelect(V: boolean);
    Procedure SetEditEditMask(Const V: String);
    Procedure SetEditCharCase(V: TPSCEditCharCase);
    Procedure SetEditOEMConvert(V: boolean);
    Procedure SetEditPasswordChar(V: Char);
    Procedure SetEditReadOnly(V: boolean);
    Procedure SetEditText(Const V: String);
    Procedure SetEditHideSelection(V: boolean);
    procedure UpdateInplaceBounds;

    Function GetEditIsMasked: boolean;
    Function GetEditEditText: String;
    Function GetEditModified: boolean;
    Function GetEditSelLength: Integer;
    Function GetEditSelStart: Integer;
    Function GetEditSelText: String;
    Function GetEditDragCursor: TCursor;
    Function GetEditCanUndo: boolean;
    Function GetEditDragKind: TDragKind;
    Function GetEditDragMode: TDragMode;
    Function GetEditPopupMenu: TPopupMenu;
    Function GetEditAutoSelect: boolean;
    Function GetEditEditMask: String;
    Function IsEditMaskStored:Boolean;
    Function GetEditCharCase: TPSCEditCharCase;
    Function GetEditOEMConvert: boolean;
    Function GetEditPasswordChar: Char;
    Function GetEditReadOnly: boolean;
    Function GetEditText: String;
    Function GetEditHideSelection: boolean;
    Function GetEditMaxLength: Integer;
    Function GetEditTabStop:Boolean;

    procedure SetEditTabStop(V:Boolean);
    Procedure EditOnClick(Sender: TObject); virtual;
    Procedure EditOnDblClick(Sender: TObject); virtual;
    Procedure EditOnEnter(Sender: TObject); virtual;
    Procedure EditOnExit(Sender: TObject); virtual;
    Procedure EditOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer); virtual;
    Procedure EditOnMouseMove(Sender: TObject; Shift: TShiftState;
      X,Y: Integer); virtual;
    Procedure EditOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer); virtual;
    Procedure EditOnChange(Sender: TObject); virtual;
    procedure EditWndProc(var Message: TMessage);
  protected
    Procedure EditOnKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState); virtual;
    Procedure EditOnKeyPress(Sender: TObject; Var Key: Char); virtual;
    Procedure EditOnKeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState); virtual;

    Procedure cmExit(Var M: TMessage);message cm_Exit;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged);message WM_WindowPosChanged;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    Procedure DestroyEdit; virtual;
    Procedure RecreateEdit; virtual;
    Procedure Loaded; override;
    Procedure SetEditParams(AEdit: TControl); virtual;

    Function IsModified:Boolean;override;
    Function IsDataAware:Boolean;virtual;
    Function GetEdit: TControl; virtual;
    Function CreateEditorAccess: IPSCEditorAccess; virtual;
    Function GetEditField: TField; virtual;
    Function GetEditClass: TControlClass; virtual;

    Property DataSource: TDataSource read FDataSource write
      SetEditDataSource;
    Property DataField: String read FDataField write SetEditDataField;
    Property Field: TField read GetEditField;
    Property HideSelection: boolean read GetEditHideSelection write
      SetEditHideSelection default True;
    Property MaxLength: Integer read GetEditMaxLength write SetEditMaxLength
      default 0;
    Property AutoSelect: boolean read GetEditAutoSelect write SetEditAutoSelect
      default True;
    Property EditMask: String read GetEditEditMask write SetEditEditMask Stored IsEditMaskStored;
    Property CharCase: TPSCEditCharCase read GetEditCharCase write SetEditCharCase
      default CharCase_Normal;
    Property OEMConvert: boolean read GetEditOEMConvert write SetEditOEMConvert
      default False;
    Property PasswordChar: Char read GetEditPasswordChar write
      SetEditPasswordChar default #0;
    Property ReadOnly: boolean read GetEditReadOnly write SetEditReadOnly default
      False;
    Property DragCursor: TCursor read GetEditDragCursor write SetEditDragCursor
      default crDrag;
    Property DragKind: TDragKind read GetEditDragKind write SetEditDragKind
      default dkDrag;
    Property DragMode: TDragMode read GetEditDragMode write SetEditDragMode
      default dmManual;
    Property PopupMenu: TPopupMenu read GetEditPopupMenu write SetEditPopupMenu;
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
  public
    function GetInplaceEditRect:TRect;override;
    Function Focused: boolean;override;

    Constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
    Procedure SetFocus; override;
    Procedure Clear;
    Procedure ClearSelection;
    Procedure CopyToClipboard;
    Procedure CutToClipboard;
    Procedure PasteFromClipboard;
    Procedure SelectAll;override;
    Procedure Undo;
    Procedure ClearUndo;

    Property CanUndo: Boolean read GetEditCanUndo;
    Property Edit: TControl read GetEdit;
    Property Modified: Boolean read GetEditModified write SetEditModified;
    Property SelLength: Integer read GetEditSelLength write SetEditSelLength;
    Property SelStart: Integer read GetEditSelStart write SetEditSelStart;
    Property SelText: String read GetEditSelText write SetEditSelText;
    Property Text: String read GetEditText write SetEditText;
    Property IsMasked: boolean read GetEditIsMasked;
    Property EditText: String read GetEditEditText write SetEditEditText;
  published
    Property TabStop Read GetEditTabStop Write SetEditTabStop default True;
  End;

  TPSCEdit = Class(TPSCCustomContainerEdit)
  public
    Property Field;
  published
    Property PopupControl;
    Property OnDropDown;
    Property OnCloseUp;
    Property PopupColor;

    Property DataSource;
    Property DataField;
    Property BtnKind;
    Property EditMask;
    Property ButtonsVisible;

    Property Align;

    Property Anchors;
    Property Constraints;
    Property BiDiMode;
    Property DragKind;
    Property ParentBiDiMode;
    Property AutoSelect;
    Property AutoSize;
    Property CharCase;
    Property DragCursor;
    Property OEMConvert;
    Property PasswordChar;
    Property DragMode;
    Property Enabled;
    Property Font;
    Property HideSelection default True;
    Property MaxLength;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ReadOnly;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Text;
    Property Visible;

    Property OnButtonClick;
    Property OnButtonDown;
    Property OnButtonUp;
    Property OnChange;
    Property OnClick;
    Property OnDblClick;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property OnContextPopup;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;
    Property OnStartDock;
    Property OnStartDrag;
  End;

  TPSCInplaceMaskEdit = Class(TPSCMaskEdit)
  published
    Property IsMasked;
    Property EditText;
    Property HideSelection;
  End;

  IPSCThemeRegister=interface
    ['{8E2EAB95-7011-4EF6-8355-1B12E34A7245}']
    procedure RegisterTheme(const AThemeName:String;
      AProc:TPSCCreateThemeProc;AUserData:Integer=0);
    function GetThemeProc(const AThemeName:String;
      var AUserData:Cardinal):TPSCCreateThemeProc;
    procedure EnumThemeNames(const AThemeNames:IPSCStrings);
  end;

const
  SPSCThemeName_3D='3D';
  SPSCThemeName_Flat='Flat';
  SPSCThemeName_Word2000='Word2000';
  SPSCThemeName_WindowsXP='WindowsXP';
  SPSCThemeName_WordXP='WordXP';

  PSCDefaultThemeName:String=SPSCThemeName_WindowsXP;

  CPSCUsedPopupFormClass:TPSCPopupFormClass=TPSCPopupForm;

function PSCGetThemeRegister:IPSCThemeRegister;

implementation

{----------------------------------------------------------}

type
  TPSCThemeRegisterItem=class
  public
    Proc:TPSCCreateThemeProc;
    UserData:Integer;
  end;

  TPSCThemeRegister=class(TInterfacedObject,IPSCThemeRegister)
  private
    FThemes:IPSCStrings;
  public
    procedure RegisterTheme(const AThemeName:String;
      AProc:TPSCCreateThemeProc;AUserData:Integer=0);
    function GetThemeProc(const AThemeName:String;
      var AUserData:Cardinal):TPSCCreateThemeProc;
    procedure EnumThemeNames(const AThemeNames:IPSCStrings);
    constructor Create;
  end;

var
  FThemeRegister:IPSCThemeRegister;

function PSCGetThemeRegister:IPSCThemeRegister;
begin
  If FThemeRegister=nil then
    FThemeRegister:=TPSCThemeRegister.Create;
  Result:=FThemeRegister;
end;

{--------------------------------------}

constructor TPSCThemeRegister.Create;
begin
  inherited;
  FThemes:=PSCCreateSortedStringList(ioOwned);
  FThemes.Duplicates:=dup_Ignore;
end;

{--------------------------------------}

procedure TPSCThemeRegister.RegisterTheme(const AThemeName:String;
  AProc:TPSCCreateThemeProc;AUserData:Integer=0);
var
  MyItem:TPSCThemeRegisterItem;
  MyIndex:Integer;
begin
  If FThemes.Find(AThemeName,MyIndex) then
    MyItem:=TPSCThemeRegisterItem(FThemes.Objects[MyIndex])
  else
    begin
      MyItem:=TPSCThemeRegisterItem.Create;
      FThemes.AddObject(AThemeName,MyItem);
    end;
  MyItem.Proc:=AProc;
  MyItem.UserData:=AUserData;
end;

{--------------------------------------}

function TPSCThemeRegister.GetThemeProc(const AThemeName:String;
  var AUserData:Cardinal):TPSCCreateThemeProc;
var
  MyItem:TPSCThemeRegisterItem;
  MyIndex:Integer;
begin
  If FThemes.Find(AThemeName,MyIndex) then
    begin
      MyItem:=TPSCThemeRegisterItem(FThemes.Objects[MyIndex]);
      AUserData:=MyItem.UserData;
      Result:=MyItem.Proc;
    end
  else
    Result:=nil;
end;

{--------------------------------------}

procedure TPSCThemeRegister.EnumThemeNames(const AThemeNames:IPSCStrings);
var
  i:Integer;
begin
  for i:=FThemes.Count-1 downto 0 do
    AThemeNames.Add(FThemes[i]);
end;

{--------------------------------------}

var
  FThemesRegistered:Boolean=False;

procedure PSCRegisterDefaultThemes;
begin
  If FThemesRegistered then
    exit;
  FThemesRegistered:=True;

  With PSCGetThemeRegister do
  begin
    RegisterTheme(SPSCThemeName_3D,PSCCreateTheme_3D,0);
    RegisterTheme(SPSCThemeName_Flat,PSCCreateTheme_Flat,0);
    RegisterTheme(SPSCThemeName_Word2000,PSCCreateTheme_Word2000,0);
    RegisterTheme(SPSCThemeName_WindowsXP,PSCCreateTheme_WindowsXP,0);
    RegisterTheme(SPSCThemeName_WordXP,PSCCreateTheme_WordXP,0);
  end;
end;

{--------------------------------------}

Procedure TPSCCustomButtonControl.HandleEvent(const AParams:TPSCEventParams);
begin
  Invalidate;
end;

{-------------------------------------------}

Type
  TDBEditExForPSCEdit = Class(TPSCDBEdit)
  published
    Property IsMasked;
    Property EditText;
    Property HideSelection;
    Property Text;
  End;

{--------------------------------------}

function TPSCCustomContainerEdit.GetInplaceEditRect:TRect;
begin
  Result:=inherited GetInplaceEditRect;
end;

{--------------------------------}

type
  THackCustomEdit = Class(TCustomEdit)
  End;

  THackControlActionLink = Class(TControlActionLink)
  End;

  THackControl = Class(TControl)
  End;

  THackWinControl = Class(TWinControl)
  End;

procedure TPSCCustomContainerEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  THackControl(GetEdit).Color:=Color;
end;

{--------------------------------}

procedure TPSCCustomContainerEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  THackControl(GetEdit).Font:=Font;
  THackControl(GetEdit).Font.Color:=GetDefaultColor(GetEditState,TMT_TEXTCOLOR);
end;

{--------------------------------}

procedure TPSCCustomContainerEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateInplaceBounds;
end;

{--------------------------------}

procedure TPSCCustomContainerEdit.UpdateInplaceBounds;
var
  MyRect:TRect;
begin
  If HandleAllocated Then
    With GetEdit Do
    begin
      MyRect:=GetInplaceEditRect;
      If not PSCRectsEqual(BoundsRect,MyRect) then
        BoundsRect:=MyRect;
    end;
end;

{--------------------------------}

procedure TPSCCustomContainerEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  UpdateInplaceBounds;
end;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditParams(AEdit: TControl);
Begin
  If AEdit <> Nil Then
    Begin
      AEdit.Parent := Self;

      With THackControl(AEdit) Do
        Begin
          THackControl(AEdit).AutoSize := False;
          FEditorAccess.SetAutoSize(False);

          THackControl(AEdit).ParentFont := True;

          FEditorAccess.SetBorderStyle(bsNone);

          If AEdit Is TWinControl Then
            Begin
              THackWinControl(AEdit).TabStop := True;
              THackWinControl(AEdit).OnEnter := EditOnEnter;
              THackWinControl(AEdit).OnExit := EditOnExit;
              THackWinControl(AEdit).OnKeyDown := EditOnKeyDown;
              THackWinControl(AEdit).OnKeyPress := EditOnKeyPress;
              THackWinControl(AEdit).OnKeyUp := EditOnKeyUp;
              THackWinControl(AEdit).WindowProc := EditWndProc;
              THackWinControl(AEdit).DoubleBuffered:=True;
            End;

          THackControl(AEdit).OnClick := EditOnClick;
          THackControl(AEdit).OnDblClick := EditOnDblClick;
          THackControl(AEdit).OnMouseDown := EditOnMouseDown;
          THackControl(AEdit).OnMouseMove := EditOnMouseMove;
          THackControl(AEdit).OnMouseUp := EditOnMouseUp;


          If AEdit Is TCustomEdit Then
            THackCustomEdit(AEdit).OnChange := EditOnChange;

          SetBounds(Left,Top,Width,Height);
        End;
    End;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.IsModified:Boolean;
begin
  Result:=Modified;
end;

{--------------------------------}

Function TPSCCustomContainerEdit.IsDataAware:Boolean;
begin
  Result:=(DataField<>'') or (DataSource<>nil);
end;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditClass: TControlClass;
Begin
  If IsDataAware then
    Result := TDBEditExForPSCEdit
  else
    Result := TPSCInplaceMaskEdit;
End;

{--------------------------------}

Type
  TPSCEditorAccess_Edit = Class(TInterfacedObject,IPSCEditorAccess)
  private
    FEditor: TPSCInplaceMaskEdit;
    Function GetEdit:TPSCInplaceMaskEdit;
    Function GetEditText: String;
    Function GetIsMasked: boolean;
    Function GetSelText: String;
    Function GetSelStart: Integer;
    Function GetSelLength: Integer;
    Function GetText: String;
    Function GetReadOnly: boolean;
    Function GetPasswordChar: Char;
    Function GetOEMConvert: boolean;
    Function GetCharCase: TPSCEditCharCase;
    Function GetHideSelection: boolean;
    Function GetMaxLength: Integer;
    Function GetAutoSelect: boolean;
    Function GetEditMask: String;
    Procedure SetBorderStyle(V: TBorderStyle);
    Procedure SetEditText(Const V: String);
    Procedure SetAutoSize(V: boolean);
    Procedure SetDataField(Const V: String);
    Procedure SetDataSource(V: TDataSource);
    Procedure SetSelText(Const V: String);
    Procedure SetSelStart(V: Integer);
    Procedure SetSelLength(V: Integer);
    Procedure SetText(Const V: String);
    Procedure SetReadOnly(V: boolean);
    Procedure SetPasswordChar(V: Char);
    Procedure SetOEMConvert(V: boolean);
    Procedure SetCharCase(V: TPSCEditCharCase);
    Procedure SetHideSelection(V: boolean);
    Procedure SetMaxLength(V: Integer);
    Procedure SetAutoSelect(V: boolean);
    Procedure SetEditMask(Const V: String);
    Function CanUndo:Boolean;
    Function GetModified:Boolean;
    procedure SetModified(V:Boolean);
    Procedure Clear;
    Procedure ClearSelection;
    Procedure CopyToClipboard;
    Procedure CutToClipboard;
    Procedure PasteFromClipboard;
    Procedure Undo;
    Procedure ClearUndo;
    Procedure SelectAll;
  public
    Constructor Create(AEdit: TPSCInplaceMaskEdit);
  End;

  TPSCEditorAccess_DBEdit = Class(TInterfacedObject,IPSCEditorAccess)
  private
    FEdit: TDBEditExForPSCEdit;
    Function GetEditText: String;
    Function GetIsMasked: boolean;
    Function GetSelText: String;
    Function GetSelStart: Integer;
    Function GetSelLength: Integer;
    Function GetText: String;
    Function GetReadOnly: boolean;
    Function GetPasswordChar: Char;
    Function GetOEMConvert: boolean;
    Function GetCharCase: TPSCEditCharCase;
    Function GetHideSelection: boolean;
    Function GetMaxLength: Integer;
    Function GetAutoSelect: boolean;
    Function GetEditMask: String;
    Procedure SetBorderStyle(V: TBorderStyle);
    Procedure SetEditText(Const V: String);
    Procedure SetAutoSize(V: boolean);
    Procedure SetDataField(Const V: String);
    Procedure SetDataSource(V: TDataSource);
    Procedure SetSelText(Const V: String);
    Procedure SetSelStart(V: Integer);
    Procedure SetSelLength(V: Integer);
    Procedure SetText(Const V: String);
    Procedure SetReadOnly(V: boolean);
    Procedure SetPasswordChar(V: Char);
    Procedure SetOEMConvert(V: boolean);
    Procedure SetCharCase(V: TPSCEditCharCase);
    Procedure SetHideSelection(V: boolean);
    Procedure SetMaxLength(V: Integer);
    Procedure SetAutoSelect(V: boolean);
    Procedure SetEditMask(Const V: String);
    Function CanUndo:Boolean;
    Function GetModified:Boolean;
    procedure SetModified(V:Boolean);
    Procedure Clear;
    Procedure ClearSelection;
    Procedure CopyToClipboard;
    Procedure CutToClipboard;
    Procedure PasteFromClipboard;
    Procedure Undo;
    Procedure ClearUndo;
    Procedure SelectAll;
  public
    Constructor Create(AEdit: TDBEditExForPSCEdit);
  End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetEditText: String;
Begin
  Result := FEdit.EditText;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetIsMasked: boolean;
Begin
  Result := FEdit.IsMasked;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetSelText: String;
Begin
  Result := FEdit.SelText;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetSelStart: Integer;
Begin
  Result := FEdit.SelStart;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetSelLength: Integer;
Begin
  Result := FEdit.SelLength;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetText: String;
Begin
  Result := FEdit.Text;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetReadOnly: boolean;
Begin
  Result := Fedit.ReadOnly;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetPasswordChar: Char;
Begin
  Result := FEdit.PasswordChar;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetOEMConvert: boolean;
Begin
  Result := FEdit.OEMConvert;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetCharCase: TPSCEditCharCase;
Begin
  Result := Fedit.CharCase;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetHideSelection: boolean;
Begin
  Result := FEdit.HideSelection;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetMaxLength: Integer;
Begin
  Result := FEdit.MaxLength;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetAutoSelect: boolean;
Begin
  Result := Fedit.AutoSelect;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetEditMask: String;
Begin
  Result := FEdit.EditMask;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetBorderStyle(V: TBorderStyle);
Begin
  Fedit.BorderStyle := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetEditText(Const V: String);
Begin
  FEdit.EditText := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetAutoSize(V: boolean);
Begin
  FEdit.AutoSize := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetDataField(Const V: String);
Begin
  FEdit.DataField := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetDataSource(V: TDataSource);
Begin
  FEdit.DataSource := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetSelText(Const V: String);
Begin
  FEdit.SelText := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetSelStart(V: Integer);
Begin
  FEdit.SelStart := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetSelLength(V: Integer);
Begin
  FEdit.SelLength := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetText(Const V: String);
Begin
  FEdit.Text := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetReadOnly(V: boolean);
Begin
  FEdit.ReadOnly := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetPasswordChar(V: Char);
Begin
  FEdit.PasswordChar := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetOEMConvert(V: boolean);
Begin
  FEdit.OemConvert := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetCharCase(V: TPSCEditCharCase);
Begin
  FEdit.CharCase := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetHideSelection(V: boolean);
Begin
  FEdit.HideSelection := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetMaxLength(V: Integer);
Begin
  FEdit.MaxLength := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetAutoSelect(V: boolean);
Begin
  Fedit.AutoSelect := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SetEditMask(Const V: String);
Begin
  FEdit.EditMask := V;
End;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.CanUndo:Boolean;
begin
  Result:=FEdit.CanUndo;
end;

{--------------------------------}

Function TPSCEditorAccess_DBEdit.GetModified:Boolean;
begin
  Result:=FEdit.Modified;
end;

{--------------------------------}

procedure TPSCEditorAccess_DBEdit.SetModified(V:Boolean);
begin
  FEdit.Modified:=V;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.Clear;
begin
  FEdit.Clear;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.ClearSelection;
begin
  FEdit.ClearSelection;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.CopyToClipboard;
begin
  FEdit.CopyToClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.CutToClipboard;
begin
  FEdit.CutToClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.PasteFromClipboard;
begin
  FEdit.PasteFromClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.Undo;
begin
  FEdit.Undo;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.ClearUndo;
begin
  FEdit.ClearUndo;
end;

{--------------------------------}

Procedure TPSCEditorAccess_DBEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

{--------------------------------}

Constructor TPSCEditorAccess_DBEdit.Create(AEdit: TDBEditExForPSCEdit);
Begin
  Inherited Create;
  FEdit := AEdit;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.CanUndo:Boolean;
begin
  Result:=GetEdit.CanUndo;
end;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetModified:Boolean;
begin
  Result:=GetEdit.Modified;
end;

{--------------------------------}

procedure TPSCEditorAccess_Edit.SetModified(V:Boolean);
begin
  GetEdit.Modified:=V;
end;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetEdit:TPSCInplaceMaskEdit;
begin
  Result:=FEditor;
end;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetEditText: String;
Begin
  Result := GetEdit.EditText;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetIsMasked: boolean;
Begin
  Result := GetEdit.IsMasked;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetSelText: String;
Begin
  Result := GetEdit.SelText;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetSelStart: Integer;
Begin
  Result := GetEdit.SelStart;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetSelLength: Integer;
Begin
  Result := GetEdit.SelLength;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetText: String;
Begin
  Result := GetEdit.Text;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetReadOnly: boolean;
Begin
  Result := GetEdit.ReadOnly;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetPasswordChar: Char;
Begin
  Result := GetEdit.PasswordChar;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetOEMConvert: boolean;
Begin
  Result := GetEdit.OEMConvert;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetCharCase: TPSCEditCharCase;
Begin
  Result := GetEdit.CharCase;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetHideSelection: boolean;
Begin
  Result := GetEdit.HideSelection;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetMaxLength: Integer;
Begin
  Result := GetEdit.MaxLength;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetAutoSelect: boolean;
Begin
  Result := GetEdit.AutoSelect;
End;

{--------------------------------}

Function TPSCEditorAccess_Edit.GetEditMask: String;
Begin
  Result := GetEdit.EditMask;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetBorderStyle(V: TBorderStyle);
Begin
  GetEdit.BorderStyle := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetEditText(Const V: String);
Begin
  GetEdit.EditText := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetAutoSize(V: boolean);
Begin
  GetEdit.AutoSize := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetDataField(Const V: String);
Begin
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetDataSource(V: TDataSource);
Begin
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetSelText(Const V: String);
Begin
  GetEdit.SelText := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetSelStart(V: Integer);
Begin
  GetEdit.SelStart := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetSelLength(V: Integer);
Begin
  GetEdit.SelLength := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetText(Const V: String);
Begin
  GetEdit.Text := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetReadOnly(V: boolean);
Begin
  GetEdit.ReadOnly := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetPasswordChar(V: Char);
Begin
  GetEdit.PasswordChar := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetOEMConvert(V: boolean);
Begin
  GetEdit.OEMConvert := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetCharCase(V: TPSCEditCharCase);
Begin
  GetEdit.CharCase := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetHideSelection(V: boolean);
Begin
  GetEdit.HideSelection := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetMaxLength(V: Integer);
Begin
  GetEdit.MaxLength := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetAutoSelect(V: boolean);
Begin
  GetEdit.AutoSelect := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SetEditMask(Const V: String);
Begin
  GetEdit.EditMask := V;
End;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.Clear;
begin
  GetEdit.Clear;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.ClearSelection;
begin
  GetEdit.ClearSelection;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.CopyToClipboard;
begin
  GetEdit.CopyToClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.CutToClipboard;
begin
  GetEdit.CutToClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.PasteFromClipboard;
begin
  GetEdit.PasteFromClipboard;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.Undo;
begin
  GetEdit.Undo;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.ClearUndo;
begin
  GetEdit.ClearUndo;
end;

{--------------------------------}

Procedure TPSCEditorAccess_Edit.SelectAll;
begin
  GetEdit.SelectAll;
end;

{--------------------------------}

Constructor TPSCEditorAccess_Edit.Create(AEdit: TPSCInplaceMaskEdit);
Begin
  Inherited Create;
  FEditor := AEdit;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.CreateEditorAccess: IPSCEditorAccess;
Begin
  If FEdit is TPSCInplaceMaskEdit Then
    Result := TPSCEditorAccess_Edit.Create(TPSCInplaceMaskEdit(FEdit))
  Else
  If FEdit is TDBEditExForPSCEdit Then
    Result := TPSCEditorAccess_DBEdit.Create(TDBEditExForPSCEdit(FEdit));
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEdit: TControl;
Begin
  If FEdit = Nil Then
    Begin
      FEdit := GetEditClass.Create(Self);
      FEditorAccess := CreateEditorAccess;
      SetEditParams(FEdit);
    End;

  Result := FEdit;
End;

{--------------------------------}

destructor TPSCCustomContainerEdit.Destroy;
begin
  try
    inherited;
  except
  end;
end;

{--------------------------------}

function TPSCCustomButtonControl.GetDefaultColor(AState,AColorID:Integer):TPSCColor;
begin
  Result:=ThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT,
    AState, AColorID);
end;

{--------------------------------}

Constructor TPSCCustomContainerEdit.Create(AOwner: TComponent);
Begin
  Inherited;
  ControlStyle := [csCaptureMouse,csClickEvents,
    csOpaque,csDoubleClicks,csReplicatable];
  inherited TabStop := False;
  ParentColor:=THackControl(GetEdit).ParentColor;
  ButtonsVisible:=False;
  HideSelection := True;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnChange(Sender: TObject);
Begin
  If Assigned(OnChange) Then
    OnChange(Self);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnClick(Sender: TObject);
Begin
  If Assigned(OnClick) Then
    OnClick(Self);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnDblClick(Sender: TObject);
Begin
  If Assigned(OnDblClick) Then
    OnDblClick(Self);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnEnter(Sender: TObject);
Begin
  If Assigned(OnEnter) Then
    OnEnter(Self);
  Invalidate;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnExit(Sender: TObject);
Begin
  If Assigned(OnExit) Then
    OnExit(Self);
  Invalidate;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  KeyDown(Key,Shift);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnKeyPress(Sender: TObject; Var Key:
  Char);
Begin
  KeyPress(Key);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnKeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  KeyUp(Key,Shift);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnMouseDown(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Begin
  If (Not (GetEdit Is TWinControl)) And CanFocus Then
    SetFocus;

  If Assigned(OnMouseDown) Then
    OnMouseDown(Self,Button,Shift,X,Y);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnMouseMove(Sender: TObject; Shift:
  TShiftState;
  X,Y: Integer);
Begin
  If Assigned(OnMouseMove) Then
    OnMouseMove(Self,Shift,X,Y);
  MouseMove(Shift,X,Y);  
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.EditOnMouseUp(Sender: TObject; Button:
  TMouseButton;
  Shift: TShiftState; X,Y: Integer);
Begin
  If Assigned(OnMouseUp) Then
    OnMouseUp(Self,Button,Shift,X,Y);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditHideSelection: boolean;
Begin
  Result := FEditorAccess.GetHideSelection;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditHideSelection(V: boolean);
Begin
  FEditorAccess.SetHideSelection(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditMaxLength: Integer;
Begin
  Result := FEditorAccess.GetMaxLength;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditMaxLength(V: Integer);
Begin
  FEditorAccess.SetMaxLength(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditAutoSelect: boolean;
Begin
  Result := FEditorAccess.GetAutoSelect;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditAutoSelect(V: boolean);
Begin
  FEditorAccess.SetAutoSelect(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditEditMask: String;
Begin
  Result := FEditorAccess.GetEditMask;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditEditMask(Const V: String);
Var
  S: String;
Begin
  S := Text;
  Try
    FEditorAccess.SetEditMask(V);
  Finally
    Text := S;
  End;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditCharCase: TPSCEditCharCase;
Begin
  Result := FEditorAccess.GetCharCase;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditCharCase(V: TPSCEditCharCase);
Begin
  FEditorAccess.SetCharCase(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditOEMConvert: boolean;
Begin
  Result := FEditorAccess.GetOEMConvert;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditOEMConvert(V: boolean);
Begin
  FEditorAccess.SetOEMConvert(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditPasswordChar: Char;
Begin
  Result := FEditorAccess.GetPasswordChar;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditPasswordChar(V: Char);
Begin
  FEditorAccess.SetPasswordChar(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditReadOnly: boolean;
Begin
  Result := FEditorAccess.GetReadOnly;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditReadOnly(V: boolean);
Begin
  FEditorAccess.SetReadOnly(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditText: String;
Begin
  Result := FEditorAccess.GetText;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditText(Const V: String);
Begin
  FEditorAccess.SetText(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditDragCursor: TCursor;
Begin
  Result := THackControl(GetEdit).DragCursor;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditDragCursor(V: TCursor);
Begin
  THackControl(GetEdit).DragCursor := V;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditDragKind: TDragKind;
Begin
  Result := THackControl(GetEdit).DragKind;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditDragKind(V: TDragKind);
Begin
  THackControl(GetEdit).DragKind := V;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditDragMode: TDragMode;
Begin
  Result := THackControl(GetEdit).DragMode;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditDragMode(V: TDragMode);
Begin
  THackControl(GetEdit).DragMode := V;
End;

{--------------------------------}

procedure TPSCCustomContainerEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  THackControl(GetEdit).Enabled := Enabled;
  Color:= GetDefaultColor(GetEditState,TMT_FILLCOLOR);
end;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditPopupMenu: TPopupMenu;
Begin
  Result := THackControl(GetEdit).PopupMenu;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditPopupMenu(V: TPopupMenu);
Begin
  THackControl(GetEdit).PopupMenu := V;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditCanUndo: boolean;
Begin
  Result := FEditorAccess.CanUndo;
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditModified: boolean;
Begin
  Result := FEditorAccess.Modified;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditModified(V: boolean);
Begin
  FEditorAccess.Modified := V
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditSelLength: Integer;
Begin
  Result:=FEditorAccess.GetSelLength;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditSelLength(V: Integer);
Begin
  FEditorAccess.SetSelLength(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditSelStart: Integer;
Begin
  Result := FEditorAccess.GetSelStart;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditSelStart(V: Integer);
Begin
  FEditorAccess.SetSelStart(V);
End;

{--------------------------------}

Function TPSCCustomContainerEdit.GetEditSelText: String;
Begin
  Result := FEditorAccess.GetSelText;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SetEditSelText(Const V: String);
Begin
  FEditorAccess.SetSelText(V);
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.Clear;
Begin
  FEditorAccess.Clear;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.ClearSelection;
Begin
  FEditorAccess.ClearSelection;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.CopyToClipboard;
Begin
  FEditorAccess.CopyToClipboard;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.CutToClipboard;
Begin
  FEditorAccess.CutToClipboard;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.PasteFromClipboard;
Begin
  FEditorAccess.PasteFromClipboard;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.Undo;
Begin
  FEditorAccess.Undo;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.ClearUndo;
Begin
  FEditorAccess.ClearUndo;
End;

{--------------------------------}

Procedure TPSCCustomContainerEdit.SelectAll;
Begin
  FEditorAccess.SelectAll;
End;

{--------------------------------------}

Function TPSCCustomContainerEdit.Focused: boolean;
Begin
  Result := Inherited Focused Or
    ((GetEdit Is TWinControl) And THackWinControl(GetEdit).Focused);
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.SetFocus;
Begin
  Inherited;

  If (GetEdit is TWinControl) then  // Needed, otherwise in FilterBox popup
    TWinControl(GetEdit).SetFocus;  // by default editor will not be focused

end;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.SetEditDataSource(V: TDataSource);
var
  MyOldIsDataAware:Boolean;
Begin
  If FDataSource<>V then
  begin
    try
      MyOldIsDataAware:=IsDataAware;
      FDataSource:=v;
      If IsDataAware<>MyOldIsDataAware then
        RecreateEdit;
      FEditorAccess.SetDataSource(V);
    except
      raise;
    end;  
  end;
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.SetEditDataField(Const V: String);
var
  MyOldIsDataAware:Boolean;
Begin
  If FDataField<>V then
  begin
    MyOldIsDataAware:=IsDataAware;
    FDataField:=V;
    If IsDataAware<>MyOldIsDataAware then
      RecreateEdit;
    FEditorAccess.SetDataField(V);
  end;
End;

{--------------------------------------}

Function TPSCCustomContainerEdit.GetEditField: TField;
Begin
  If GetEdit Is TPSCDBEdit Then
    Result := TPSCDBEdit(GetEdit).Field
  Else
    Result := Nil;
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.Loaded;
Begin
  Inherited;
  THackControl(GetEdit).Loaded;
End;

{--------------------------------------}

Function TPSCCustomContainerEdit.GetEditIsMasked: boolean;
Begin
  Result := FEditorAccess.GetIsMasked;
End;

{--------------------------------------}

Function TPSCCustomContainerEdit.GetEditEditText: String;
Begin
  Result := FEditorAccess.GetEditText;
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.SetEditEditText(Const V: String);
Begin
  FEditorAccess.SetEditText(V);
End;

{--------------------------------------}

Function TPSCCustomContainerEdit.GetEditTabStop:Boolean;
begin
  If GetEdit is TWinControl then
    Result:=TWinControl(GetEdit).TabStop
  else
    Result:=inherited TabStop;
end;

{--------------------------------------}

procedure TPSCCustomContainerEdit.SetEditTabStop(V:Boolean);
begin
  If GetEdit is TWinControl then
    TWinControl(GetEdit).TabStop:=V
  else
    inherited TabStop:=V;
end;

{--------------------------------------}

Function TPSCCustomContainerEdit.IsEditMaskStored:Boolean;
begin
  Result:=not IsDataAware;
end;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.DestroyEdit;
Begin
  If (FEdit <> Nil) Then
    Begin
      FEditorAccess:=nil;
      FEdit.Parent := Nil;
      FEdit.Free;
      FEdit := Nil;
    End;
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.RecreateEdit;
Var
  SaveText,SaveEditText: String;
  SaveHideSelection: boolean;
  SaveDataSource: TDataSource;
  SaveDataField: String;
  SaveSelLength,SaveSelStart: Integer;
Begin
  {$IFNDEF D6}
  If csDesigning in ComponentState then
    exit;
  {$ENDIF}
  SaveSelLength := SelLength;
  SaveSelStart := SelStart;
  SaveText := Text;
  SaveDataSource := DataSource;
  SaveDataField := DataField;
  SaveEditText := EditText;
  SaveHideSelection := HideSelection;
  DestroyEdit;
  GetEdit;
  Text := SaveText;
  EditText := SaveEditText;
  HideSelection := SaveHideSelection;
  DataSource := SaveDataSource;
  DataField := SaveDataField;
  SelStart := SaveSelStart;
  SelLength := SaveSelLength;
  SetBounds(Left,Top,Width,Height);
  AdjustEdit; // must be here as fixes wrong adjustment bug when switching to dataaware
End;

{---------------------------------------}

procedure TPSCCustomContainerEdit.EditWndProc(var Message: TMessage);
begin
  THackWinControl(GetEdit).WndProc(Message);
  if (Message.Msg in [WM_SETFOCUS, WM_KILLFOCUS]) then
    Invalidate;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.AdjustEdit;
var
  MyRect:TRect;
  MyBorderHeight:Integer;
  MyHeight:Integer;
begin
  if AutoSize and HandleAllocated then
  begin
    MyRect:=GetInplaceEditRect;
    MyBorderHeight:=(ClientHeight-MyRect.Bottom)+MyRect.Top;
    Canvas.Font:=Font;
    ClientHeight := Canvas.TextHeight('Wg09')+MyBorderHeight;

    MyHeight:=PSCGetEditHeight(Font, Ctl3d);
    If Height<MyHeight then
      Height:=MyHeight;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.Changed;
begin
  Invalidate;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.CMFontChanged(var Message: TMessage);
begin
  Inherited;
  AdjustEdit;
  SetBounds(Left,Top,Width,Height);
  Invalidate;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FButtonState:=PUSHBUTTON_STATE_NORMAL;
  Invalidate;
end;

{-----------------------------------------}

constructor TPSCCustomButtonControl.Create(AOwner: TComponent);
begin
  FHandler:=PSCCreateEventHandler(HandleEvent);
  PSCRegisterDefaultThemes;
  inherited;
  ThemeChanged;
  Width := 121;
  Height := 21;
  AutoSize := True;
  FButtonState:=PUSHBUTTON_STATE_NORMAL;
  DoubleBuffered:=True;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style Or CS_DBLCLKS;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.CreateWnd;
begin
  inherited;
  AdjustEdit;
  SetBounds(Left,Top,Width,Height);
end;

{-----------------------------------------}

destructor TPSCCustomButtonControl.Destroy;
begin
  KillTimer;
  ThemeLib:= nil;
  inherited;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.DoButtonClick(BtnIndex: Integer);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self,BtnIndex);
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.DoButtonDown(BtnIndex: Integer);
begin
  if Assigned(FOnButtonDown) then
    FOnButtonDown(Self,BtnIndex);
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.DoButtonUp(BtnIndex: Integer);
begin
  if Assigned(FOnButtonUp) then
    FOnButtonUp(Self,BtnIndex);
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.EnableTimer;
begin
  if (FTimer = nil) and (BtnKind = bkUpDown) then
    FTimer := PSCSetTimer(FPressedIndex,170,TimerEvent);
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetBtnKind: TPSCEditBtnKind;
begin
  Result:= FBtnKind;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetBtnRect: TRect;
var
  BtnWidth: Integer;
  Delta: Integer;
  MyWidth, MyHeight: Integer;
begin
  if FThemeLib = nil then
    Exit;
  if(FThemeLib.GetThemeBoolData(tcComboBox, 0, 0, TMT_NARROWCOMBOBTN))then
    BtnWidth:= GetSystemMetrics(SM_CXMENUCHECK)
  else
    BtnWidth:= PSCGetSystemMetrics(SM_CXHTHUMB);
  ThemeLib.GetThemeIntData(tcEdit, EDIT_PART_EDITTEXT, GetEditState, TMT_BORDERSIZE, Delta);
  with ClientRect do
  begin
    MyWidth  := Right-Left;
    MyHeight := Bottom-Top;
  end;
  SetRect(Result, MyWidth - BtnWidth-Delta, Delta, MyWidth-Delta,
    MyHeight-Delta);
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetBtnUpDownRect(
  UpButton: boolean): TRect;
begin
  Result := GetBtnRect;
  With Result do
    if UpButton Then
      Result:=Rect(Left, Top, Right, (Bottom+Top) div 2)
    else
      Result:=Rect(Left, (Bottom+Top) div 2, Right, Bottom);
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetButtonsVisible: Boolean;
begin
  Result:= FButtonsVisible;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetInplaceEditRect: TRect;
var
  Delta: Integer;
begin
  if(FThemeLib = nil)then
    Exit;
  FThemeLib.GetThemeIntData(tcEdit, EDIT_PART_EDITTEXT, EDIT_STATE_NORMAL, TMT_BORDERSIZE, Delta);
  Inc(Delta,2);
  With Result do
  begin
    Left := Delta;
    Top := Delta;
    If ButtonsVisible Then
      Right := GetBtnRect.Left - 1
    Else
      Right := ClientRect.Right - Delta;
    Bottom:= ClientRect.Bottom - Delta;
  end;
end;

{-----------------------------------------}

Function TPSCCustomButtonControl.GetDroppedDown: Boolean;
begin
  Result:=False;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.IsHot:Boolean;
var
  P: TPoint;
  Handle,CursorHandle,FocusHandle: HWND;
  MyIsChildOfCursorHandle:Boolean;
  MyIsChildOfFocusHandle:Boolean;
begin
  Result:=False;
  If not HandleAllocated then
    exit;
  Handle := Self.Handle;
  GetCursorPos(P);
  CursorHandle := WindowFromPoint(P);
  FocusHandle := GetFocus;
  MyIsChildOfCursorHandle:=(Handle = CursorHandle) or IsChild(Handle,CursorHandle);
  MyIsChildOfFocusHandle:=(Handle = FocusHandle) or IsChild(Handle,FocusHandle);
  Result:= Application.Active and CanFocus and
    (MyIsChildOfFocusHandle or (GetCapture = 0) and MyIsChildOfCursorHandle)
    or (csDesigning in ComponentState);
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetButtonState:Integer;
var
  MyPressed:Boolean;
  MyHot:Boolean;
begin
  if Enabled then
    begin
      MyPressed:=GetDroppedDown and
        FThemeLib.GetThemeBoolData(tcEdit, 0, 0, TMT_PRESSEDBTNWHENDROPPED);
      MyHot:=IsHot and
        (not FThemeLib.GetThemeBoolData(tcEdit, 0, 0,TMT_SEPARATEBUTTONSFROMEDIT));
      if (FButtonState=PUSHBUTTON_STATE_PRESSED) or MyPressed then
        Result:= PUSHBUTTON_STATE_PRESSED
      else
      if (FButtonState=PUSHBUTTON_STATE_HOT) or MyHot then
        Result:= PUSHBUTTON_STATE_HOT
      else
        Result:= PUSHBUTTON_STATE_NORMAL;
    end
  else
    Result:= PUSHBUTTON_STATE_DISABLED;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.IsModified:Boolean;
begin
  Result:=False;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SelectAll;
begin
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetEditState:Integer;
var
  MyFocused:Boolean;
begin
  If Enabled then
    begin
      MyFocused:=FThemeLib.GetThemeBoolData(tcEdit, 0, 0,
        TMT_FOCUSEDEDITWHENDROPPED);
      MyFocused:=(GetDroppedDown and MyFocused) or Focused;

      If MyFocused then
        Result:= EDIT_STATE_FOCUSED
      else
      if IsHot then
        Result:= EDIT_STATE_HOT
      else
        Result:=EDIT_STATE_NORMAL;
    end
  else
    Result:= EDIT_STATE_DISABLED;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.GetSpinState(ABtnIndex:Integer):Integer;
begin
  if not Enabled then
    Result:= PUSHBUTTON_STATE_DISABLED
  else
  if (FButtonState=PUSHBUTTON_STATE_PRESSED) and (FPressedIndex = ABtnIndex) then
    Result:= PUSHBUTTON_STATE_PRESSED
  else
  if((FButtonState=PUSHBUTTON_STATE_HOT) and (FPressedIndex = ABtnIndex)) or
    ((not FThemeLib.GetThemeBoolData(tcEdit, 0, 0, TMT_SEPARATEBUTTONSFROMEDIT)) and IsHot) then
    Result:= PUSHBUTTON_STATE_HOT
  else
    Result:= PUSHBUTTON_STATE_NORMAL;
end;

{-----------------------------------------}

function TPSCCustomButtonControl.IsThemeNameStored: Boolean;
begin
  Result:=FThemeName<>'';
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.KillTimer;
begin
  PSCKillTimer(FTimer);
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.Loaded;
begin
  inherited;
  AdjustEdit;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  if (csDesigning in ComponentState) or (not ButtonsVisible) then
    exit;

  if CanFocus and (not Focused) then
    SetFocus;

  P := PSCPoint(X,Y);

  if (Button=mbLeft) and PtInRect(GetBtnRect,P) then
  begin
    UpdatePressedIndex(P);
    FButtonState:=PUSHBUTTON_STATE_PRESSED;
    Invalidate;
    DoButtonDown(FPressedIndex);
    EnableTimer;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  PrevState:Integer;
  P: TPoint;
  MyIsHot:Boolean;
begin
  inherited;
  if (csDesigning in ComponentState) then
    exit;

  PrevState := FButtonState;
  P := Point(X,Y);
  MyIsHot:=IsHot;

  if PtInRect(GetBtnRect,P) and MyIsHot then
    begin
      if ssLeft in Shift then
        FButtonState:=PUSHBUTTON_STATE_PRESSED
      else
        FButtonState:=PUSHBUTTON_STATE_HOT;
    end
  else
    FButtonState:=PUSHBUTTON_STATE_NORMAL;

  If FSaveHotState<>MyIsHot then
    Invalidate
  else
    if UpdatePressedIndex(P) or (PrevState<>FButtonState) then
      InvalidateRect(GetBtnRect);

  if PrevState<>FButtonState then
  begin
    if FButtonState=PUSHBUTTON_STATE_PRESSED then
      EnableTimer
    else
      KillTimer;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  KillTimer;
  inherited;
  if (csDesigning in ComponentState) or (not ButtonsVisible) then
    exit;

  if PtInRect(GetBtnRect,Point(X,Y)) and
    (FButtonState=PUSHBUTTON_STATE_PRESSED)
  then
  begin
    DoButtonUp(FPressedIndex);
    DoButtonClick(FPressedIndex);
    FButtonState:=PUSHBUTTON_STATE_NORMAL;
    Invalidate;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.Paint;
begin
  if not HandleAllocated then
    exit;

  FSaveHotState:=IsHot;

  ThemeLib.DrawThemeBackground(tcEdit, MylaCanvas, EDIT_PART_EDITTEXT,
    GetEditState, TPSCRect(ClientRect));

  if ButtonsVisible Then
    case BtnKind of
      bkComboBox:
        ThemeLib.DrawThemeBackground(tcComboBox, MylaCanvas,
          COMBOBOX_PART_DROPDOWNBUTTON, GetButtonState, PSCRect(GetBtnRect));
      bkUpDown:
      begin
        ThemeLib.DrawThemeBackground(tcSpin,MylaCanvas,SPIN_PART_UP,GetSpinState(0),
          PSCRect(GetBtnUpDownRect(True)));
        ThemeLib.DrawThemeBackground(tcSpin,MylaCanvas,SPIN_PART_DOWN,GetSpinState(1),
          PSCRect(GetBtnUpDownRect(False)));
      end;
    end;

end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SetAutoSize(V: boolean);
begin
  if FAutoSize <> V then
  begin
    FAutoSize := V;
    AdjustEdit;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SetBtnKind(V: TPSCEditBtnKind);
begin
  if FBtnKind <> V then
  begin
    FBtnKind := V;
    Changed;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SetButtonsVisible(V: Boolean);
begin
  if FButtonsVisible <> V then
  begin
    FButtonsVisible := V;
    SetBounds(Left,Top,Width,Height);
    Changed;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SetEnabled(Value: Boolean);
begin
  inherited;
  Invalidate;
end;

{--------------------------------------}

procedure TPSCCustomButtonControl.SetTheme(const AValue: IPSCThemeLib);
var
  MyEvents:IPSCEvents;
begin
  If FThemeLib<>AValue then
  begin
    If FThemeLib<>nil then
    begin
      If PSCSupports(FThemeLib,IPSCEvents,MyEvents) then
        MyEvents.UnRegisterHandler(FHandler);
    end;

    FThemeLib:=AValue;

    If FThemeLib<>nil then
    begin
      If PSCSupports(FThemeLib,IPSCEvents,MyEvents) then
        MyEvents.RegisterHandler(FHandler);
    end;

    Invalidate;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.SetThemeName(const V: String);
begin
  if (FThemeName<>V) or (V='') then
  begin
    FThemeName:=V;
    ThemeChanged;
  end;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.ThemeChanged;
var
  MyProc:TPSCCreateThemeProc;
  MyUserData:Cardinal;
begin
  If FThemeName='' then
    MyProc:=PSCGetThemeRegister.GetThemeProc(PSCDefaultThemeName,MyUserData)
  else
    MyProc:=PSCGetThemeRegister.GetThemeProc(FThemeName,MyUserData);

  If Assigned(MyProc) then
    ThemeLib:=MyProc(MyUserData)
  else
    ThemeLib:= PSCCreateTheme_WindowsXP(0);

  Color:= GetDefaultColor(GetEditState,TMT_FILLCOLOR);

end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.TimerEvent(Timer: TObject;
  EventID: Integer);
begin
  DoButtonClick(FPressedIndex);
end;

{-----------------------------------------}

function TPSCCustomButtonControl.UpdatePressedIndex(P: TPoint): boolean;
var
  OldValue: Integer;
begin
  OldValue := FPressedIndex;

  if BtnKind = bkUpDown then
    begin
      if PtInRect(GetBtnUpDownRect(True),P) then
        FPressedIndex := 0
      else
        FPressedIndex := 1;
    end
  else
    FPressedIndex := 0;

  Result := OldValue <> FPressedIndex;
end;

{-----------------------------------------}

procedure TPSCCustomButtonControl.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result:= 1;
end;

{--------------------------------------}

Constructor TPSCCustomPopupEdit.Create(AOwner: TComponent);
Begin
  Inherited;
  WantReturns := True;
  FPopupParams:=TPSCPopupParams.Create(Self);
  ButtonsVisible := True;
  FPopupColor := clPSCWindow;
End;

{--------------------------------------}

Function TPSCCustomPopupEdit.PopupCreated: boolean;
Begin
  Result := FPopup <> Nil;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomPopupEdit.UpdatePopupParams(PopupForm: TPSCPopupForm);
Begin
  With PopupForm Do
    Begin
      BevelEdges := PopupParams.BevelEdges;
      BevelInner := PopupParams.BevelInner;
      BevelOuter := PopupParams.BevelOuter;
      BevelKind := PopupParams.BevelKind;
      BevelWidth := PopupParams.BevelWidth;
    End;
  If Assigned(FOnUpdatePopup) Then
    FOnUpdatePopup(Self,PopupForm);
End;

{--------------------------------------}

Destructor TPSCCustomPopupEdit.Destroy;
Begin
  FPopup.Free;
  FPopup := Nil;
  FPopupParams.Free;
  Inherited;
End;

{--------------------------------------}

function TPSCCustomPopupEdit.InternalPostValue(ASilent:boolean):boolean;
begin
  try
    Result:=PostValue;
  except;
    Result:=False;
    If Visible and CanFocus then
    begin
      If not ASilent then
        PSCbeep;
      SetFocus;
      SelectAll;
    end;
  end;
end;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.GetDropped(Var Message: TMessage);
Begin
  Message.result := integer(DroppedDown);
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.cmExit(Var M: TMessage);
Begin
  Inherited;
  If Not DroppedDown And ([csDestroying,csDesigning]*ComponentState=[]) Then
    InternalPostValue(False);
End;

{--------------------------------------}

Procedure TPSCCustomContainerEdit.cmExit(Var M: TMessage);
begin
  If (FEditorAccess<>nil) and (FEdit<>nil) then
    inherited;
end;

{--------------------------------------}

Function TPSCCustomPopupEdit.CreatePopup: TPSCPopupForm;
Begin
  Result := CPSCUsedPopupFormClass.CreateNew(nil);
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.UpdatePopup;
Begin
  If PopupCreated Then
    Begin
      Popup.Color := PopupColor;
      UpdatePopupParams(Popup);
    End;
  // Override this in descendants to prepare popup before it will be shown
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.PopupCloseEvent(Sender: TObject;
  Canceled: Boolean);
Begin
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.DoButtonDown(BtnIndex: Integer);
Begin
  Inherited;
  If (BtnKind <> bkUpDown) and (GetPopup<>nil) Then
    DroppedDown := Not DroppedDown;
End;

{--------------------------------------}

procedure TPSCCustomPopupEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := PSCPoint(X,Y);
  if (Button=mbLeft) and PtInRect(GetBtnRect,P) then
    begin
    end;

  inherited;
end;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  If (AComponent=FPopupControl) and (Operation=opRemove) then
    PopupControl:=nil;
end;

{--------------------------------------}

procedure TPSCCustomPopupEdit.SetPopupControl(V:TControl);
begin
  If FPopupControl<>V then
  begin
    FPopupControl:=V;
    If FPopupControl<>nil then
    begin
      FPopupControl.Visible:=False;
      FPopupControl.FreeNotification(Self);
    end;
  end;
end;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.DoDropDown;
Begin
  Popup; //should be before UpdatePopup, so popup will be created if needed

  If IsModified Then
    InternalPostValue(True);

  If Assigned(FOnDropDown) Then
    FOnDropDown(Self);

  UpdatePopup;

  InternalShowPopup;
  Invalidate;
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.InternalShowPopup;
Begin
  Popup.PopupUnderControl(Self,PopupCloseEvent);
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.DoCloseUp;
Begin
  If Assigned(FOnCloseUp) Then
    FOnCloseUp(Self);
  If Assigned(FPopup) Then
    fPopup.Hide;
  Invalidate;
End;

{--------------------------------------}

Function TPSCCustomPopupEdit.PostValue: boolean;
Begin
  Result := True;
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.CancelValue;
Begin
  UpdatePopup;
  SelectAll;
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.KeyPress(Var Key: Char);
Begin
  Inherited;
  If (Key = Char(VK_RETURN)) And WantReturns Then
    Begin
      Key := #0;
      InternalPostValue(False);
    End;
  If Key = Char(VK_ESCAPE) Then
    Begin
      Key := #0;
      CancelValue;
    End;
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  Inherited KeyDown(Key,Shift);
  Case Key Of
    VK_DOWN,VK_UP:
      Begin
        If (ssAlt In Shift) Then
          Begin
            DroppedDown := Not DroppedDown;
            Key := 0;
          End;
      End;
    VK_ESCAPE:
      Begin
        CancelValue;
        Key := 0;
      End;
  End;
End;

{--------------------------------------}

Function TPSCCustomPopupEdit.GetPopup: TPSCPopupForm;
Begin
  If Not Assigned(FPopup) Then
    FPopup := CreatePopup;
  Result := FPopup;

  If (FPopupControl<>nil) and (not (csDesigning in ComponentState))
    and (FPopupControl.Parent<>FPopup)
  then
    begin
      FPopupControl.Parent:=FPopup;
      FPopupControl.Align:=alClient;
      FPopupControl.Visible:=True;
    end;
End;

{--------------------------------------}

Function TPSCCustomPopupEdit.GetDroppedDown: Boolean;
Begin
  Result := Assigned(FPopup) And fPopup.Visible
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.SetPopupColor(V: TPSCColor);
Begin
  If FPopupColor <> V Then
    Begin
      FPopupColor := V;
      If PopupCreated Then
        Popup.Color := V;
    End;
End;

{------------------------------------------------------------------}

Procedure TPSCCustomPopupEdit.SetPopupParams(V: TPSCPopupParams);
Begin
  FPopupParams.Assign(V);
End;

{--------------------------------------}

Procedure TPSCCustomPopupEdit.SetDroppedDown(A: Boolean);
Begin
  If DroppedDown <> A Then
  begin
    If A Then
      DoDropDown
    Else
      DoCloseUp;
  end;
End;

{------------------------------------------------------------------------------}

Procedure TPSCPopupForm.CMFONTCHANGED(Var M: TMessage);
Begin
  Inherited;
  ResizeFormControls;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.ResizeFormControls;
Begin
End;

{-------------------------------------------}

procedure TPSCPopupForm.InitializePopup;
begin
  AutoScroll := false;
  KeyPreview := True;
  BorderStyle := bsToolWindow;
  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;
  Position := poDesigned;
  FWantReturns:=True;
  BorderWidth:=2;
  Color:=clPSCWindow;
end;

{-------------------------------------------}

procedure TPSCPopupForm.OkButtonOnClick(Sender:TObject);
begin
  ClosePopup(False,True);
end;

{-------------------------------------------}

procedure TPSCPopupForm.CancelButtonOnClick(Sender:TObject);
begin
  ClosePopup(True,True);
end;

{------------------------------------------------------------------------------}

type
  TPSCPanelInPopup=class(TPSCPanel)
  public
    Procedure WMLButtonDown(Var Message: TWMLButtonDown);message WM_LButtonDown;
  end;

Procedure TPSCPanelInPopup.WMLButtonDown(Var Message: TWMLButtonDown);
Begin
  If Parent Is TPSCPopupForm Then
    Begin
      Message.Result := -1;
      ReleaseCapture;
      With TControl(Parent) Do
        Perform(WM_SysCommand,SC_Move Or htCaption,0);
    End
  Else
    Inherited;
End;

{-------------------------------------------}

function TPSCPopupForm.AddButton:TPSCSpeedButton;
begin
  Result:=TPSCSpeedButton.Create(Self);
  With Result do
  begin
    Top:=4;
    Flat:=True;
    Parent:=GetFooterPanel;
    ShowHint:=True;
    Left:=2+FButtonsAdded*Width;
    Caption:='';
  end;
  inc(FButtonsAdded);
end;

{-------------------------------------------}

function TPSCPopupForm.GetFooterPanel:TPSCPanel;
var
  S:String;
  MyBevel:TPSCBevel;
begin
  If FFooter<>nil then
  begin
    Result:=FFooter;
    exit;
  end;

  Result:=TPSCPanelInPopup.Create(Self);
  FFooter:=Result;
  FOkButton:=TPSCSpeedButton.Create(Self);
  FCancelButton:=TPSCSpeedButton.Create(Self);
  With Result do
  begin
    Caption:='';
    Align:=alBottom;
    BevelInner:=bvNone{bvLowered};
    BevelOuter:=bvNone;
    Parent:=Self;
    ParentColor:=True;
  end;

  MyBevel:=TPSCBevel.Create(Self);
  With MyBevel do
  begin
    Align:=AlTop;
    Parent:=Result;
    Shape:=bsBottomLine;
    Height:=4;
  end;

  With FCancelButton do
  begin
    Glyph.LoadFromResourceName(HInstance, SPSCResName_Btn_CloseWin);
    Left:=Self.ClientWidth-Width-2;
    Top:=4;
    Flat:=True;
    Parent:=Result;
    OnClick:=CancelButtonOnClick;
    Hint:=PSCConsts.CancelChanges+' ('+ShortCutToText(ShortCut(VK_ESCAPE,[]))+')';
    ShowHint:=True;
  end;

  With FOkButton do
  begin
    Glyph.LoadFromResourceName(HInstance, SPSCResName_Btn_Go);
    Left:=FCancelButton.Left-Width-1;
    Top:=4;
    Flat:=True;
    Parent:=Result;
    OnClick:=OkButtonOnClick;
    If Self.WantReturns then
      S:=ShortCutToText(ShortCut(VK_RETURN,[]))+','
    else
      S:='';
    Hint:=PSCConsts.ApplyChanges+' ('+S+ShortCutToText(ShortCut(VK_RETURN,[ssCtrl]))+')';
    ShowHint:=True;
  end;

  Result.ClientHeight:=FOkButton.Height+4;
  FOkButton.Anchors:=[akRight,akBottom];
  FCancelButton.Anchors:=[akRight,akBottom];

end;

{-------------------------------------------}

constructor TPSCPopupForm.CreateNew(AOwner: TComponent; Dummy: Integer);
Begin
  inherited CreateNew(AOwner);
  InitializePopup;
end;

{-------------------------------------------}

Procedure TPSCPopupForm.ClosePopup(Canceled,AHide: boolean);
Begin
  If FAlreadyClosing then
    exit;
  FAlreadyClosing:=True;
  try
    If Assigned(FOnPopupClosed) Then
      FOnPopupClosed(Self,Canceled);
    If AHide then
      Hide;
  finally
    FAlreadyClosing:=False;
  end;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.KeyPress(Var Key: Char);
Begin
  Inherited;
  //If Key In [#13,#27] Then
  If CharInSet(Key,[#13,#27]) Then
    Key := #0;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.KeyDown(Var Key: Word; Shift: TShiftState);
Begin
  If ((Key = VK_UP) Or (Key = VK_DOWN)) And (ssAlt In Shift) Then
    Begin
      ClosePopup(True,True);
      Exit;
    End;
  Case Key Of
    VK_RETURN:
      If WantReturns or (ssCtrl in Shift) then
      Begin
        Inherited KeyDown(Key,Shift);
        ClosePopup(False,True);
        Key := 0;
        Exit;
      End;
    VK_ESCAPE:
      Begin
        ClosePopup(True,True);
        Key := 0;
        Exit;
      End;
  End;
  Inherited KeyDown(Key,Shift);
End;

{-------------------------------------------}

Type
  TPSCPopupHook = Class(TCollectionItem)
  private
    FForm: TCustomForm;
    FOldWindowProc: TWndMethod;
    FPopups: IPSCObjectList;
  public
    Constructor Create(Collection: TCollection); override;

    Procedure WindowProc(Var Message: TMessage);
  End;

Var
  PopupHooks: TCollection;

{-------------------------------------}

Constructor TPSCPopupHook.Create(Collection: TCollection);
Begin
  Inherited;
  FPopups := PSCCreateObjectList;
End;

{-------------------------------------}

Procedure TPSCPopupHook.WindowProc(Var Message: TMessage);
Begin
  Case Message.Msg Of
    WM_NCACTIVATE:
      If Not TWMNCActivate(Message).Active Then
        Begin
          Message.Result := 1;
          Exit
        End;
  End;
  FOldWindowProc(Message);
End;

{-------------------------------------}

Function PSCHookPopupWindow(PopupWindow: TPSCPopupForm): TPSCPopupHook;
Var
  Form: TCustomForm;
  I: Integer;
Begin
  Result := Nil;
  If PopupWindow.OwnerControl <> Nil Then
    Begin
      If PopupWindow.OwnerControl Is TControl Then
        Form := GetParentForm(TControl(PopupWindow.OwnerControl))
      Else
        If PopupWindow.OwnerControl.Owner Is TControl Then
          Form := GetParentForm(TControl(PopupWindow.OwnerControl.Owner))
        Else
          Form := Nil;
      If Form <> Nil Then
        Begin
          If PopupHooks = Nil Then
            PopupHooks := TCollection.Create(TPSCPopupHook);
          For I := 0 To PopupHooks.Count - 1 Do
            Begin
              Result := TPSCPopupHook(PopupHooks.Items[I]);
              If Result.FForm = Form Then
                Break;
              Result := Nil
            End;
          If Result = Nil Then
            Begin
              Result := TPSCPopupHook(PopupHooks.Add);
              Result.FForm := Form;
              Result.FOldWindowProc := Form.WindowProc;
              Form.WindowProc := Result.WindowProc
            End;
          If Result.FPopups.IndexOf(PopupWindow) = -1 Then
            Result.FPopups.Add(PopupWindow)
        End
    End
End;

{-------------------------------------}

Procedure PSCUnhookPopupWindow(PopupWindow:TPSCPopupForm);
Var
  Form: TCustomForm;
  I: Integer;
Begin
  If PopupHooks <> Nil Then
    Begin
      Form := Nil;
      If PopupWindow.OwnerControl<>nil then
      begin
        If PopupWindow.OwnerControl Is TControl Then
          Form := GetParentForm(TControl(PopupWindow.OwnerControl))
        Else
          If PopupWindow.OwnerControl.Owner Is TControl Then
            Form := GetParentForm(TControl(PopupWindow.OwnerControl.Owner));
      end;
      If Form <> Nil Then
        For I := PopupHooks.Count - 1 Downto 0 Do
          With TPSCPopupHook(PopupHooks.Items[I]) Do
            If FForm = Form Then
              Begin
                FPopups.Remove(PopupWindow);
                If FPopups.Count = 0 Then
                  Begin
                    Form.WindowProc := FOldWindowProc;
                    Free;
                  End
              End;
      If PopupHooks.Count = 0 Then
        Begin
          PopupHooks.Free;
          PopupHooks := Nil;
        End;
    End
End;

{-------------------------------------------}

Procedure TPSCPopupForm.Hide;

  procedure MyFocusWinControl(OwnerControl:TWinControl);
  begin
    If (OwnerControl<>nil) and OwnerControl.HandleAllocated And OwnerControl.CanFocus
      And OwnerControl.Visible And OwnerControl.Enabled Then
      OwnerControl.SetFocus;
  end;    

Begin
  PSCUnhookPopupWindow(Self);
  SetCaptureControl(Nil);
  If HandleAllocated And IsWindowVisible(Handle) Then
    Begin
      Inherited Hide;
      If OwnerControl is TWinControl then
        MyFocusWinControl(TWinControl(OwnerControl))
    End;
End;

{-------------------------------------------}

Type
  TMWinControl = Class(TWinControl)
  End;

{-------------------------------------------}

Procedure TPSCPopupForm.WMNCActivate(Var Message: TWMNCActivate);
Begin
  Message.result := 1;
End;

{-------------------------------------------}

Type
  THackCustomForm = Class(TCustomForm)
  End;

Procedure TPSCPopupForm.PopupEx(Control: TControl; Const PopupRect: TRect;
  PopupEvent: TPSCOnPopupClosed; ShowKind: TPSCPopupShowKind;
  ShowOptions: TPSCPopupOptions);
Var
  R: TRect;
Begin
  FOnPopupClosed := PopupEvent;
  OwnerControl := GetParentForm(Control);

  If OwnerControl<>nil then
  begin
    If THackCustomForm(OwnerControl).FormStyle = fsStayOnTop Then
      FormStyle := fsStayOnTop
    Else
      FormStyle := fsNormal;
  end;

  If poParentColor In ShowOptions Then
    Color := TMWinControl(Control).Color;

  Font := TMWinControl(Control).Font;

  If Not (poParentFontColor In ShowOptions) Then
    Font.Color := clPSCWindowText;

  ReleaseCapture;
  R := PopupRect;

  If PopupRect.Right <= 0 Then
    R.Right := PopupRect.Left + Width;
  If PopupRect.Bottom <= 0 Then
    R.Bottom := PopupRect.Top + Height;

  If ShowKind = pskUnderControl Then
    R := PSCRectClientToScreen(Control,Control.ClientRect)
  Else
    R := PSCRectClientToScreen(Control,R);

  HandleNeeded;
  ResizeFormControls; //before changes to the left/top

  Case ShowKind Of
    pskUnderControl,pskUnderParam:
      PSCUpdatePopupRect(Self,R);
    pskExact:
      Begin
        Left := R.Left;
        Top := R.Top;
      End;
    pskCentered:
      Begin
        R := PSCCenterRect(BoundsRect,R);
        Left := R.Left;
        Top := R.Top;
      End;
  End;

  If Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self);

  Show;

{$IFNDEF MYLATEST}
  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height, // needed
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);               //
{$ENDIF}

  If (ActiveControl <> Nil) And ActiveControl.CanFocus Then
    ActiveControl.SetFocus
  Else
    If CanFocus Then
      SetFocus;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.PopupUnderControl(Control: TControl; PopupEvent:
  TPSCOnPopupClosed);
Begin
  PopupEx(Control,Rect(0,0,Control.Width,Control.Height),PopupEvent,pskUnderControl, []);
End;

{-------------------------------------------}

Procedure TPSCPopupForm.Popup(Control: TControl; ParamRect: TRect;
  PopupEvent: TPSCOnPopupClosed);
Begin
  PopupEx(Control,ParamRect,PopupEvent,pskUnderParam, []);
End;

{-------------------------------------------}

Procedure TPSCPopupForm.PopupExact(Control: TControl; X,Y: Integer;
  PopupEvent: TPSCOnPopupClosed);
Begin
  PopupEx(Control,Rect(X,Y, -1, -1),PopupEvent,pskExact,
    [poParentColor,poParentFontColor]);
End;

{-------------------------------------------}

Procedure TPSCPopupForm.Show;
Var
  SaveState: TFormState;
Begin
  SaveState := FFormState;
  Include(FFormState,fsModal);
  Try
    PSCHookPopupWindow(Self);
    Inherited Show;
  Finally
    FFormState := SaveState;
  End;
End;

{-------------------------------------------}

function PSCIsChildPopupOf(AParent,AChild:TPSCPopupForm):Boolean;
var
  MyParentForm:TWinControl;
begin
  Result:=AParent=AChild;
  If Result then
    exit;
  Result:=(AChild.FOwnerControl<>nil);
  If Result then
  begin
    MyParentForm:=GetParentForm(AChild.FOwnerControl);
    Result:=MyParentForm is TPSCPopupForm;
    If Result then
      Result:=PSCIsChildPopupOf(AParent,TPSCPopupForm(MyParentForm));
  end;
end;

{-------------------------------------------}

Procedure TPSCPopupForm.WMActivate(Var Message: TWMActivate);

  function NeedToClose(AControl:TWinControl):Boolean;
  begin
    Result:=(Not (AControl Is TPSCPopupForm)) or
      (not PSCIsChildPopupOf(Self,TPSCPopupForm(AControl)));
  end;

Var
  AControl: TWinControl;

Begin
  Inherited;
  If FAlreadyClosing then
    exit;
  AControl := FindControl(Message.ActiveWindow);
  If (Message.Active = WA_INACTIVE) And NeedToClose(AControl) Then
    ClosePopup(True,True);
End;

{-------------------------------------------}

Procedure TPSCPopupForm.WMMouseActivate(Var Message: TWMMouseActivate);
Begin
  Inherited;
  Message.Result := MA_NOACTIVATE;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
  begin
    Style := Style And (Not WS_BORDER) {And (Not WS_DLGFRAME)};
    Style := Style Or WS_POPUP;
  end;
End;

{-------------------------------------------}

Procedure TPSCPopupForm.SetBounds(ALeft,ATop,AWidth,AHeight: Integer);
Var
  R: TRect;
Begin
  R := Bounds(ALeft,ATop,AWidth,AHeight);
  R := PSCEnsureRectInWorkArea(R);
  Inherited SetBounds(R.Left,R.Top,PSCRectWidth(R),PSCRectHeight(R));
End;

{-------------------------------------------}
end.
