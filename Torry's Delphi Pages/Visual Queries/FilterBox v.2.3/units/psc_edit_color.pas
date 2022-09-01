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
unit psc_edit_color;

interface
{$I psc_defines.inc}

Uses
  dialogs,
  Buttons,
  ImgList,
  StdCtrls,
  winapi.commctrl,
  sysutils,
  winapi.messages,
  classes,
  controls,
  winapi.windows,
  forms,

  myla_system,
  myla_interfaces,

  psc_edit,
  psc_colorbox,
  psc_wrapper,
  psc_procs,
  psc_button_color,
  psc_const;

{-------------------------------------}

type

  TPSCCustomColorEdit = class(TPSCCustomPopupEdit)
  private
    FSelectedColor: TPSCColor;
    FSelectedColorOld: TPSCColor;
    FSelectedColorBeforeDropDown: TPSCColor;
    FColorBoxText: String;
    FDisplayNames: Boolean;
    FColorBox: TPSCCustomColorBox;
    FReadOnly: Boolean;
    FTrackColor: Boolean;
    FOnChange: TPSCNotifyEvent;
    FOnChanging: TPSCNotifyEvent;

    procedure ColorBoxOnActiveChange(Sender: TObject; ASlot: TPSCColorSlot);
    procedure ColorBoxOnChange(Sender: TObject; ASlot: TPSCColorSlot);
    procedure ColorBoxOnSelected(Sender: TObject; ASlot: TPSCColorSlot);
    procedure ColorBoxOnButtonClick(Sender: TObject;
      ASlot: TPSCColorSlot; AColor: TPSCColor; AStage: TPSCButtonClickStage);
    procedure PopupOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetSelectedColor(AColor: TPSCColor);
    procedure AdjustColorBox;
    procedure AdjustColorBoxColor(bDoOnChange: Boolean);
    procedure DoChange;
    procedure DoChanging;
    procedure SetDisplayNames(ADisplayNames: Boolean);
    procedure SetHighlightActive(AValue: Boolean);
    procedure SetHighlightColor(AValue: TPSCColor);
    procedure SetOptions(AValue: TPSCColorBoxOptions);
    procedure SetStyle(AValue: TPSCColorBoxStyle);

    function GetColorBoxSlot: TPSCColorSlot;
    function GetColorBox: TPSCCustomColorBox;
    function GetHighlightActive: Boolean;
    function GetHighlightColor: TPSCColor;
    function GetOptions: TPSCColorBoxOptions;
    function GetSelectedColorKind: TPSCColorKind;
    function GetStyle: TPSCColorBoxStyle;
    function GetSelectedColor: TPSCColor;
    function SearchDefaultColor: Boolean;
    function StyleStored: Boolean;

  protected
    procedure WMKillFocus(Var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(Var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState;
      X,Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    procedure PopupCloseEvent(Sender: TObject; Canceled: Boolean); override;

    function CreatePopup: TPSCPopupForm; override;

    procedure DoDropDown; override;
    property SelectedColor: TPSCColor read GetSelectedColor write SetSelectedColor default clPSCBlack;
    property DisplayNames: Boolean read FDisplayNames write SetDisplayNames default False;
    property ColorBox: TPSCCustomColorBox read GetColorBox;
    property HighlightActive: Boolean read GetHighlightActive write SetHighlightActive default False;
    property HighlightColor: TPSCColor read GetHighlightColor write SetHighlightColor default clPSCHighlight;
    property Options: TPSCColorBoxOptions read GetOptions write SetOptions
      default [cboShowDefaultAuto,cboShowFontColors,cboShowMoreColors];
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property SelectedColorKind: TPSCColorKind read GetSelectedColorKind;
    property TrackColor: Boolean read FTrackColor write FTrackColor default true;
    property Style: TPSCColorBoxStyle read GetStyle write SetStyle stored StyleStored;

    property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TPSCNotifyEvent read FOnChanging write FOnChanging;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
  end;


  TPSCColorEdit = class(TPSCCustomColorEdit)
  public
    property ColorBox;
  published
    property OnChanging;
    property OnDropDown;
    property OnCloseUp;
    property OnChange;
    property SelectedColor;
    property DisplayNames;
    property HighlightActive;
    property HighlightColor;
    property Options;
    property ReadOnly;
    property SelectedColorKind;
    property TrackColor;
    property Style;
    Property PopupColor;

    Property Font;
    Property Align;
    Property Anchors;
    Property AutoSize;
    Property BiDiMode;
    Property Constraints;
    Property DragKind;
    Property ParentBiDiMode;
    Property DragCursor;
    Property DragMode;
    Property Enabled;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop default true;
    Property Visible;
    Property OnClick;
    Property OnContextPopup;
    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnStartDock;
    Property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnStartDrag;
  end;

{-------------------------------------}

implementation

uses
  Graphics,

  psc_theme;

{-------------------------------------}

type
  TPSCCustomColorBoxAccess = class(TPSCCustomColorBox)
  end;
  TPSCPopupFormAccess = class(TPSCPopupForm)
  end;

{-----------------------------------------}

procedure PSCDrawColorString(
  ACanvas: TCanvas;
  ARect: TRect;
  AThemeLib: IPSCThemeLib;
  AState: Integer;
  AFocused: Boolean;
  AColor: TPSCColor;
  AText: String);
var
  MyColorRect: TRect;
  MyTextRect: TRect;
  MySize: Integer;
const
  MyIndentation = 1;
begin
  with ACanvas do
  begin
    Pen.Style:= psSolid;
    Brush.Style:= bsSolid;
    if AState = EDIT_STATE_DISABLED then
      Font.Color:= AThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT, AState, TMT_TEXTCOLOR);
    if AFocused then
      begin
        Font.Color:= clHighlightText;
        Brush.Color:= clPSCHighlight;
      end
    else
      Brush.Color:= AThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT, AState, TMT_FILLCOLOR);;
    Pen.Color:= AThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT, AState, TMT_FILLCOLOR);
    FillRect(ARect);
    if AText <> '' then
      begin
        with ARect do
          begin
            MySize:= Bottom - Top - (MyIndentation shl 1);
            SetRect(MyColorRect, Left + MyIndentation, Top + MyIndentation, Left + MySize + MyIndentation, Top + MySize + MyIndentation);
          end;
        with ARect do
          SetRect(MyTextRect, MyColorRect.Right + MyIndentation*2 + 1, Top, Right - MyIndentation, Bottom);
        PSCDrawText(ACanvas,AText,Length(AText),MyTextRect,DT_SINGLELINE or DT_VCENTER);
      end
    else
      with ARect do
        SetRect(MyColorRect, Left + MyIndentation, Top + MyIndentation, Right - MyIndentation, Bottom - MyIndentation);
    Pen.Color:= AThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT, AState, TMT_BORDERCOLOR);
    if AState <> EDIT_STATE_DISABLED then
      Brush.Color:= AColor;
    Rectangle(MyColorRect);
  end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.AdjustColorBoxColor(bDoOnChange: Boolean);
var
  MySlot: TPSCColorSlot;
  MyOldSelectedColor: TPSCColor;
  MyOldColorBoxText: String;
begin
  MyOldSelectedColor:= FSelectedColor;
  MyOldColorBoxText:= FColorBoxText;
  MySlot:= GetColorBoxSlot;
  if Assigned(MySlot) and (MySlot.Section.Visible = False) then
    MySlot:= nil;
  if Assigned(MySlot) then
    begin
      FSelectedColor:= MySlot.Color;
      if not ((MySlot.Section is TPSCDefaultSection) and
        (not (MySlot.Section is TPSCButtonSection))) then
        begin
          if not SearchDefaultColor then
            if FDisplayNames then
              FColorBoxText:= PSCColorsManager.ColorToString(MySlot.Color, FColorBox.HintKind)
            else
              FColorBoxText:= '';
        end
      else
        FColorBoxText:= MySlot.DisplayName
    end
  else
    begin
      if FDisplayNames then
        FColorBoxText:= PSCColorsManager.ColorToString(FSelectedColorOld, FColorBox.HintKind)
      else
        FColorBoxText:= '';
      FSelectedColor:= FSelectedColorOld;
    end;
  if MyOldSelectedColor <> FSelectedColor then
    begin
      Invalidate;
      DoChanging;
    end
  else if MyOldColorBoxText <> FColorBoxText then
    Invalidate;
  if (bDoOnChange) and (SelectedColor <> FSelectedColorBeforeDropDown) then
    begin
      FSelectedColorBeforeDropDown:= SelectedColor;
      DoChange;
    end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.AdjustColorBox;
begin
  if not Assigned(FColorBox) then
    Exit;
  with TPSCCustomColorBoxAccess(FColorBox) do
    begin
      OnSelected:= ColorBoxOnSelected;
      OnActiveChange:= ColorBoxOnActiveChange;
      OnChange:= ColorBoxOnChange;
      OnButtonClick:= ColorBoxOnButtonClick;
      Parent:= Popup;
      HandleNeeded;
      AutoSize:= True;
      Self.Style := cbsWordFont;
      Left:= 0;
      Top:= 0;
    end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.ColorBoxOnActiveChange(Sender: TObject;
  ASlot: TPSCColorSlot);
begin
  if TrackColor then
    AdjustColorBoxColor(False);
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.ColorBoxOnSelected(Sender: TObject;
  ASlot: TPSCColorSlot);
begin
  Popup.Hide;
  if Assigned(ASlot) then
    AdjustColorBoxColor(True);
end;

{-----------------------------------------}

constructor TPSCCustomColorEdit.Create(AOwner: TComponent);
begin
  inherited;
  TabStop:= True;
  TrackColor:= True;
  Popup.AutoSize:= True;
  TPSCPopupFormAccess(Popup).OnKeyDown:= PopupOnKeyDown;
  ColorBox.HighlightColor:= clHighlight;
  SelectedColor:= clBlack;
end;

{-----------------------------------------}

destructor TPSCCustomColorEdit.Destroy;
begin
  if Assigned(FColorBox) then
    FColorBox.Destroy;
  inherited;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetColorBox: TPSCCustomColorBox;
begin
  if not Assigned(FColorBox) then
  begin
    FColorBox:= TPSCCustomColorBox.Create(Popup);
    AdjustColorBox;
  end;
  Result:= FColorBox;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.Paint;
begin
  inherited;
  Canvas.Font.Assign(Font);
  PSCDrawColorString(
    Canvas,
    GetInplaceEditRect,
    ThemeLib,
    EditState,
    Focused,
    SelectedColor,
    FColorBoxText);
    if Focused then
      begin
        Canvas.Brush.Color:= clPSCBlack;//ThemeLib.GetThemeColorData(tcEdit, EDIT_PART_EDITTEXT, EditState, TMT_FILLCOLOR);
        Canvas.DrawFocusRect(GetInplaceEditRect);
      end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetSelectedColor(AColor: TPSCColor);
var
  MySlot: TPSCColorSlot;
begin
  ColorBox.SelectedColor:= AColor;
  MySlot:= ColorBox.SelectedSlot;
  if (MySlot = nil) or (not MySlot.Section.Visible) then
  begin
    FSelectedColorOld:= AColor;
    AdjustColorBoxColor(True);
  end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetDisplayNames(ADisplayNames: Boolean);
begin
  if FDisplayNames <> ADisplayNames then
    begin
      FDisplayNames:= ADisplayNames;
      AdjustColorBoxColor(False);
    end;
end;

procedure TPSCCustomColorEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  Invalidate;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  Invalidate;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.ColorBoxOnChange(Sender: TObject;
  ASlot: TPSCColorSlot);
begin
  if not Popup.Visible then
    begin
      if Assigned(ASlot) then
        if (ASlot.Section is TPSCButtonSection) then
          if TPSCCustomColorBoxAccess(ColorBox).GetDialogWasExecuted then
            if ButtonState_Up in ASlot.State then
              begin
                FSelectedColorOld:= ASlot.Color;
                AdjustColorBoxColor(True);
              end;
    end
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetHighlightActive: Boolean;
begin
  Result:= ColorBox.HighlightActive;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetHighlightActive(AValue: Boolean);
begin
  ColorBox.HighlightActive:= AValue;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetHighlightColor: TPSCColor;
begin
  Result:= ColorBox.HighlightColor;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetHighlightColor(AValue: TPSCColor);
begin
  ColorBox.HighlightColor:= AValue;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetOptions: TPSCColorBoxOptions;
begin
  Result:= ColorBox.SectionsSet;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetOptions(AValue: TPSCColorBoxOptions);
begin
  ColorBox.SectionsSet:= AValue;
  ColorBox.SelectedColor:= SelectedColor;
  TPSCPopupFormAccess(Popup).AdjustSize;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetColorBoxSlot: TPSCColorSlot;
begin
  Result:= nil;
  if Assigned(ColorBox.ActiveSlot) and DroppedDown then
    Result:= ColorBox.ActiveSlot
  else if Assigned(ColorBox.SelectedSlot) then
    Result:= ColorBox.SelectedSlot;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetSelectedColorKind: TPSCColorKind;
var
  MySlot: TPSCColorSlot;
begin
  MySlot:= GetColorBoxSlot;
  Result:= ckColor;
  if Assigned(MySlot) then
    if (MySlot.Section is TPSCDefaultSection) and
      (not (MySlot.Section is TPSCButtonSection)) then
      Result:= ckAuto;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.CreatePopup: TPSCPopupForm;
begin
  Result:= TPSCPopupForm.CreateNew(Self);
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyPoint: TPoint;
begin
  if not ReadOnly then
    begin
      inherited;
      MyPoint:= Point(X, Y);
      if (Button=mbLeft) and (not PtInRect(GetBtnRect,MyPoint)) then
        DoButtonDown(0);
    end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not ReadOnly then
    inherited;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not ReadOnly then
    inherited;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetStyle: TPSCColorBoxStyle;
begin
  Result:= ColorBox.Style;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.SetStyle(AValue: TPSCColorBoxStyle);
begin
  ColorBox.Style:= AValue;
  AdjustColorBoxColor(True);
  TPSCPopupFormAccess(Popup).AdjustSize;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.DoChange;
begin
  if Assigned(OnChange)then
    OnChange(Self);
end;

{-----------------------------------------}

function TPSCCustomColorEdit.GetSelectedColor: TPSCColor;
begin
  Result:= FSelectedColor;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode in [VK_DOWN, VK_RETURN]	then
    DroppedDown:= true;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.PopupCloseEvent(Sender: TObject;
  Canceled: Boolean);
begin
  if ColorBox.ActiveSlot <> nil then
    ColorBox.ActiveSlot:= nil;
    AdjustColorBoxColor(False);
end;

{-----------------------------------------}

function TPSCCustomColorEdit.SearchDefaultColor: Boolean;
var
  i, j: Integer;
begin
  if ColorBox.SelectSameColors then
    for i:= 0 to ColorBox.SectionsCount - 1 do
      if (ColorBox.Sections[i] is TPSCDefaultSection)
        and (not (ColorBox.Sections[i] is TPSCButtonSection))
        and ColorBox.Sections[i].Visible then
        for j:= 0 to ColorBox.Sections[i].Slots.Count - 1 do
          if (ColorBox.Sections[i].Slots.Items[j] as TPSCColorSlot).Color = FSelectedColor then
            begin
              FColorBoxText:= (ColorBox.Sections[i].Slots.Items[j] as TPSCColorSlot).DisplayName;
              Result:= True;
              Exit;
            end;
    Result:= False;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.ColorBoxOnButtonClick(Sender: TObject;
  ASlot: TPSCColorSlot; AColor: TPSCColor; AStage: TPSCButtonClickStage);
begin
  Popup.Hide;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.PopupOnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  MyColor: TPSCColor;
begin
  if Key = VK_RETURN then
    if ColorBox.ActiveSlot <> Nil then
      begin
        MyColor:= ColorBox.ActiveSlot.Color;
        FSelectedColorOld:= MyColor;
        ColorBox.SelectedColor:= MyColor;
        Popup.Hide;
        AdjustColorBoxColor(True);
      end;
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.DoDropDown;
begin
  FSelectedColorBeforeDropDown:= SelectedColor;
  inherited;
  if ColorBox.ActiveSlot <> nil then
    ColorBox.ActiveSlot:= nil;
end;

{-----------------------------------------}

function TPSCCustomColorEdit.StyleStored: Boolean;
begin
  Result:= not (Style in [cbsCustom, cbsWordFont]);
end;

{-----------------------------------------}

procedure TPSCCustomColorEdit.DoChanging;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{-----------------------------------------}

Initialization
  PSCSafeRegisterClasses([TPSCColorEdit]);
Finalization
  UnRegisterClasses([TPSCColorEdit]);
end.
