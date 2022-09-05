unit SpTBXFormPopupMenu;

{==============================================================================
Version 2.4.8

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

History:
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - No changes.

12 March 2010 - version 2.4.5
  - No changes.

2 December 2009 - version 2.4.4
  - Added AutoSize property to TSpTBXFormPopupMenu.

13 September 2009 - version 2.4.3
  - No changes.

8 May 2009 - version 2.4.2
  - No changes.

15 March 2009 - version 2.4.1
  - No changes.

17 January 2009 - version 2.4
  - Fixed incorrect focus handling on TSpTBXFormPopupMenu,
    when a dialog is showed on top of a TSpTBXFormPopupMenu and
    the app is deactivated the Popup is closed but the dialog
    stays, thanks to Sertac Akyuz for reporting this.

26 September 2008 - version 2.3
  - No changes.

29 July 2008 - version 2.2
  - No changes.

26 June 2008 - version 2.1
  - No changes.

3 May 2008 - version 2.0
  - No changes.

2 April 2008 - version 1.9.5
  - No changes.

3 February 2008 - version 1.9.4
  - No changes.

19 January 2008 - version 1.9.3
  - No changes.

26 December 2007 - version 1.9.2
  - Fixed incorrect focus handling on TSpTBXFormPopupMenu,
    thanks to Costas Stergiou for reporting this.

1 December 2007 - version 1.9.1
  - Removed TBX dependency.

20 November 2007 - version 1.9
  - Removed TBX dependency.

8 February 2007 - version 1.8.3
  - No changes.

17 December 2006 - version 1.8.2
  - No changes.

24 November 2006 - version 1.8.1
  - Fixed TSpTBXFormPopupMenu resizing flicker.

27 August 2006 - version 1.8
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation
{$I TB2Ver.inc}

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, TB2Item, TB2Anim, SpTBXSkins, SpTBXItem;

const
  WM_SPTBX_POPUPINVALIDATE = WM_USER + 887;
  WM_SPTBX_POPUPROLLUP = WM_USER + 888;

type
  TSpTBXFormPopupMenu = class;
  TSpTBXCustomWrapperPopupForm = class;

  TSpTBXPopupAnimationType = (
    patNone,
    patSlide,
    patFade
  );

  TSpTBXPopupBorderStyleType = (
    pbsFrame,
    pbsSizeable,
    pbsSizeableBottom,
    pbsSizeableRightBottom
  );

  TSpTBXPopupFormState = record
    PopupForm: TCustomForm;
    BorderStyle: TFormBorderStyle;
    BoundsRect: TRect;
  end;

  TSpTBXRollDownEvent = procedure(Sender: TObject; var FormWidth, FormHeight: Integer) of object;
  TSpTBXRollUpEvent = procedure(Sender: TObject; Selected: Boolean) of object;
  TSpTBXGetFormClassEvent = procedure(Sender: TObject; var AFormClass: TCustomFormClass) of object;

  { TSpTBXPopupSizeGrip }

  TSpTBXPopupSizeGrip = class(TWinControl)
  private
    FOnDrawBackground: TSpTBXDrawEvent;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FPopupForm: TSpTBXCustomWrapperPopupForm;
    procedure DoDrawBackground(ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetGripRect: TRect;
    function GetGripSizerRect: TRect;
    function IsScreenPointInGrip(P: TPoint): Boolean;
  published
    property OnDrawBackground: TSpTBXDrawEvent read FOnDrawBackground write FOnDrawBackground;
  end;

  { TSpTBXWrapperPopupForm }

  TSpTBXCustomWrapperPopupForm = class(TForm)  // Descend from TForm instead of TCustomForm otherwise TStringGrid can't receive focus
  private
    FHooksInstalled: Boolean;
    FOldAppOnMessage: TMessageEvent;
    FOldPopupControlWndProc: TWndMethod;
    FShowShadows: Boolean;
    FAnimation: TSpTBXPopupAnimationType;
    FAnimationDirection: TTBAnimationDirection;
    FBorderStyle: TSpTBXPopupBorderStyleType;
    FOnRollDown: TNotifyEvent;
    FOnRollUp: TSpTBXRollUpEvent;
    procedure SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
    procedure InstallHooks;
    procedure UninstallHooks;
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure WMSpTBXPopupInvalidate(var Message: TMessage); message WM_SPTBX_POPUPINVALIDATE;
    procedure WMSpTBXPopupRollUp(var Message: TMessage); message WM_SPTBX_POPUPROLLUP;
  protected
    FPopupControl: TControl;
    FFormPopupMenu: TSpTBXFormPopupMenu;
    FPaintingClientArea: Boolean;
    FSizeGrip: TSpTBXPopupSizeGrip;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect); virtual; abstract;
    function GetSysAnimation: TSpTBXPopupAnimationType;
    procedure AppOnMessageHook(var Msg: TMsg; var Handled: Boolean); virtual;
    procedure PopupControlWindowProc(var Message: TMessage); virtual;
    procedure DoRollDown; virtual;
    procedure DoRollUp(Selected: Boolean); virtual;
    property Animation: TSpTBXPopupAnimationType read FAnimation write FAnimation;
    property AnimationDirection: TTBAnimationDirection read FAnimationDirection write FAnimationDirection default [];
    property BorderStyle: TSpTBXPopupBorderStyleType read FBorderStyle write SetBorderStyle default pbsFrame;
    property ShowShadows: Boolean read FShowShadows write FShowShadows default True;
    property OnRollDown: TNotifyEvent read FOnRollDown write FOnRollDown;
    property OnRollUp: TSpTBXRollUpEvent read FOnRollUp write FOnRollUp;
  public
    constructor Create(AFormPopupMenu: TSpTBXFormPopupMenu); reintroduce; virtual;
    destructor Destroy; override;
    procedure RollDown(X, Y, AWidth, AHeight: Integer; FocusPopup: Boolean = True); overload; virtual;
    procedure RollDown(APopupControl: TControl; AWidth, AHeight: Integer; IsVertical: Boolean = False; FocusPopup: Boolean = True); overload; virtual;
    procedure RollUp(Selected: Boolean; FocusParentControl: Boolean = True); virtual;
    property FormPopupMenu: TSpTBXFormPopupMenu read FFormPopupMenu;
  end;

  TSpTBXWrapperPopupForm = class(TSpTBXCustomWrapperPopupForm)
  private
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure DestroyWindowHandle; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect); override;
  public
    destructor Destroy; override;
  published
    property Height;
    property Width;
    property BorderStyle;
    property ShowShadows;
    property OnRollDown;
    property OnRollUp;
  end;

  { TSpTBXFormPopupMenu }

  TSpTBXFormPopupMenu = class(TPopupMenu, ISpTBXPopupMenu)
  private
    FAutoSize: Boolean;
    FItems: Boolean;
    FNotifies: TList;
    FPopupFocus: Boolean;
    FPopupFormState: TSpTBXPopupFormState;
    FPopupFormPrevSize: TSize;
    FOnClosePopup: TSpTBXRollUpEvent;
    FOnBeforeClosePopup: TSpTBXRollUpEvent;
    FOnBeforePopup: TSpTBXRollDownEvent;
    FOnGetPopupFormClass: TSpTBXGetFormClassEvent;
    function GetBorderStyle: TSpTBXPopupBorderStyleType;
    function GetShowShadows: Boolean;
    procedure SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
    procedure SetShowShadows(const Value: Boolean);
    procedure SetPopupForm(const Value: TCustomForm);
  protected
    FPopupForm: TCustomForm;
    FWrapperForm: TSpTBXWrapperPopupForm;  // Container of FPopupForm
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BroadcastCloseMessage(Selected: Boolean);
    procedure DoGetPopupFormClass(var AFormClass: TCustomFormClass); virtual;
    function InternalPopup(X, Y: Integer; ForceFocus: Boolean; PopupControl: TControl = nil): Boolean; virtual;
    procedure InternalClosePopup(Sender: TObject; Selected: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X: Integer; Y: Integer); override;
    procedure PopupEx(X, Y: Integer; ForceFocus: Boolean); overload;
    procedure PopupEx(PopupControl: TControl; ForceFocus: Boolean); overload;
    procedure AddCloseNotification(AObject: TObject);
    procedure RemoveCloseNotification(AObject: TObject);
    procedure ClosePopup(Selected: Boolean);
    property PopupForm: TCustomForm read FPopupForm write SetPopupForm;
  published
    property Items: Boolean read FItems; // Hide the Items property
    property AutoSize: Boolean read FAutoSize write FAutoSize default False;
    property BorderStyle: TSpTBXPopupBorderStyleType read GetBorderStyle write SetBorderStyle default pbsFrame;
    property PopupFocus: Boolean read FPopupFocus write FPopupFocus default False;
    property ShowShadows: Boolean read GetShowShadows write SetShowShadows default True;
    property OnClosePopup: TSpTBXRollUpEvent read FOnClosePopup write FOnClosePopup;
    property OnBeforeClosePopup: TSpTBXRollUpEvent read FOnBeforeClosePopup write FOnBeforeClosePopup;
    property OnBeforePopup: TSpTBXRollDownEvent read FOnBeforePopup write FOnBeforePopup;
    property OnGetPopupFormClass: TSpTBXGetFormClassEvent read FOnGetPopupFormClass write FOnGetPopupFormClass;
  end;

var
  ActiveFormPopupMenu: TSpTBXFormPopupMenu = nil;

implementation

uses
  Themes, UxTheme, Types, TB2Common, TB2Acc;

const
  DefaultBorderSize = 2;

type
  TCustomFormAccess = class(TCustomForm);

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXPopupSizeGrip }

constructor TSpTBXPopupSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(AOwner) and (AOwner is TSpTBXCustomWrapperPopupForm) then
    FPopupForm := AOwner as TSpTBXCustomWrapperPopupForm;
  Align := alBottom;
  Height := 10;
end;

procedure TSpTBXPopupSizeGrip.DoDrawBackground(ACanvas: TCanvas; ARect: TRect;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if Assigned(FOnDrawBackground) then FOnDrawBackground(Self, ACanvas, ARect, PaintStage, PaintDefault);
end;

function TSpTBXPopupSizeGrip.GetGripRect: TRect;
begin
  // Return the rect of the area that is sizeable.
  // If BorderStyle = pbsSizeableRightBottom the sizeable area is the
  // bottom right corner, when BorderStyle = pbsSizeableRightBottom the
  // sizeable area is the client rect of the SizeGrip
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and Assigned(FPopupForm) and
    (FPopupForm.FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom]) then
  begin
    Result := ClientRect;
    if FPopupForm.BorderStyle = pbsSizeableRightBottom then
      Result := GetGripSizerRect
    else
      Result := ClientRect;
  end;
end;

function TSpTBXPopupSizeGrip.GetGripSizerRect: TRect;
begin
  // Return the rect of the grip sizer, the area that has the
  // dots on the sizer.
  Result := Rect(0, 0, 0, 0);
  if not (csDestroying in ComponentState) and Assigned(FPopupForm) then begin
    case FPopupForm.BorderStyle of
      pbsSizeableBottom:
        begin
          Result := ClientRect;
          Result.Left := (Result.Right + Result.Left - 20) div 2;
          Result.Right := Result.Left + 20;
        end;
      pbsSizeableRightBottom:
        begin
          Result := ClientRect;
          Result.Left := Result.Right - 14;
        end;
    end;
  end;
end;

function TSpTBXPopupSizeGrip.IsScreenPointInGrip(P: TPoint): Boolean;
var
  GR: TRect;
begin
  Result := False;
  P := ScreenToClient(P);
  GR := GetGripRect;
  if not IsRectEmpty(GR) and PtInRect(GR, P) then
    Result := True;
end;

procedure TSpTBXPopupSizeGrip.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
const
  SC_SizeDown      = $F006;
  SC_SizeDownRight = $F008;
begin
  // Resize the StatusBar if the parent is TSpTBXTitleBar
  if not (csDesigning in ComponentState) and (Button = mbLeft) and Assigned(FPopupForm) then begin
    P := ClientToScreen(Point(X, Y));
    if IsScreenPointInGrip(P) then begin
      ReleaseCapture;
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          SendMessage(FPopupForm.Handle, WM_SYSCOMMAND, SC_SizeDown, 0);
        pbsSizeableRightBottom:
          SendMessage(FPopupForm.Handle, WM_SYSCOMMAND, SC_SizeDownRight, 0);
      end;
      Exit;
    end;
  end;

  inherited;
end;

procedure TSpTBXPopupSizeGrip.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  ACanvas: TCanvas;
  PaintDefault: Boolean;
  R, GR, CellR: TRect;
  C1, C2: TColor;
begin
  Message.Result := 1;
  if (csDestroying in ComponentState) then Exit;

  ACanvas := TCanvas.Create;
  ACanvas.Handle := Message.DC;
  try
    R := ClientRect;

    // Draw the background
    PaintDefault := True;
    DoDrawBackground(ACanvas, R, pstPrePaint, PaintDefault);
    if PaintDefault then begin
      GR := Rect(0, 0, 0, 0);
      SpDrawXPStatusBar(ACanvas, R, GR);
    end;

    // Draw the grip
    PaintDefault := True;
    DoDrawBackground(ACanvas, R, pstPostPaint, PaintDefault);
    if PaintDefault then begin
      C1 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color1;
      if (C1 = clNone) or (SkinManager.GetSkinType <> sknSkin) then C1 := clBtnShadow;
      C2 := SkinManager.CurrentSkin.Options(skncStatusBarGrip).Body.Color2;
      if (C2 = clNone) or (SkinManager.GetSkinType <> sknSkin) then C2 := clBtnHighlight;
      // Grip cells are 4x4 pixels
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          begin
            GR := GetGripSizerRect;
            CellR := GR;
            CellR.Top := (CellR.Top + CellR.Bottom - 4) div 2 + 1;
            CellR.Bottom := CellR.Top + 3;
            SpDrawXPGrip(ACanvas, CellR, C1, C2);
          end;
        pbsSizeableRightBottom:
          begin
            GR := GetGripSizerRect;
            CellR := GR;
            // Draw 2 cells at the bottom
            CellR.Left := GR.Right - 8;
            CellR.Top := CellR.Bottom - 4;
            SpDrawXPGrip(ACanvas, CellR, C1, C2);
            // Draw 1 cell at the top
            CellR.Bottom := CellR.Top;
            CellR.Top := CellR.Bottom - 4;
            CellR.Left := CellR.Left + 4;
            SpDrawXPGrip(ACanvas, CellR, C1, C2);
          end;
      end;
    end;
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXPopupSizeGrip.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) and (Message.CursorWnd = Handle) and
    (Screen.Cursor = crDefault) and Assigned(FPopupForm) then
  begin
    GetCursorPos(P);
    if IsScreenPointInGrip(P) then begin
      case FPopupForm.BorderStyle of
        pbsSizeableBottom:
          Windows.SetCursor(Screen.Cursors[-7]);
        pbsSizeableRightBottom:
          Windows.SetCursor(Screen.Cursors[-8]);
      end;
      Message.Result := 1;
      Exit;
    end;
  end;

  inherited;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXCustomWrapperPopupForm }

constructor TSpTBXCustomWrapperPopupForm.Create(AFormPopupMenu: TSpTBXFormPopupMenu);
begin
  // Form doesn't have DFM info
  inherited CreateNew(nil);

  Visible := False;
  SetBounds(0, 0, 0, 0);
  FFormPopupMenu := AFormPopupMenu;
  FAnimation := GetSysAnimation;
  FAnimationDirection := [];
  FShowShadows := True;
  FSizeGrip := TSpTBXPopupSizeGrip.Create(Self);
  FSizeGrip.Parent := Self;
end;

procedure TSpTBXCustomWrapperPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    with Params do begin
      Style := WS_POPUP or WS_CLIPCHILDREN;

      // Add the thickframe on all the BorderStyles
      // We should handle the NC HitTest
      Style := Style or WS_THICKFRAME;

      ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
      if IsWindowsXP then
        WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
    end;
end;

destructor TSpTBXCustomWrapperPopupForm.Destroy;
begin
  FreeAndNil(FSizeGrip);
  inherited;
end;

procedure TSpTBXCustomWrapperPopupForm.DoRollDown;
begin
  if Assigned(FOnRollDown) then FOnRollDown(Self);
end;

procedure TSpTBXCustomWrapperPopupForm.DoRollUp(Selected: Boolean);
begin
  if Assigned(FOnRollUp) then FOnRollUp(Self, Selected);
end;

function TSpTBXCustomWrapperPopupForm.GetSysAnimation: TSpTBXPopupAnimationType;
const
  SPI_GETMENUFADE = $1012;
var
  Animate: BOOL;
begin
  Result := patNone;
  if SystemParametersInfo(SPI_GETMENUANIMATION, 0, @Animate, 0) and Animate then
    if SystemParametersInfo(SPI_GETMENUFADE, 0, @Animate, 0) and Animate then
      Result := patFade
    else
      Result := patSlide;
end;

procedure TSpTBXCustomWrapperPopupForm.RollDown(X, Y, AWidth,
  AHeight: Integer; FocusPopup: Boolean = True);
begin
  if not Visible then begin
    ActiveFormPopupMenu := FFormPopupMenu; // Set global variable used by RollUp
    // If FPopupControl is nil we should use the ActiveForm to set focus on RollUp
    if not Assigned(FPopupControl) then
      FPopupControl := Screen.ActiveForm;
    // Increase the size of the form if the size grip is visible
    if FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom] then begin
      FSizeGrip.Visible := True;
      AHeight := AHeight + FSizeGrip.Height;
    end
    else
      FSizeGrip.Visible := False;

    InstallHooks;
    HandleNeeded; // We need the handle to set the Bounds and deactivate Vista form thick borders
    SetBounds(X, Y, AWidth, AHeight);
    SpActivateDwmNC(Self, False);
    // Make sure it will be showed on top of all the forms/dialogs
    SetWindowPos(WindowHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
    Visible := True;
    if FocusPopup then
      SetFocus;
    DoRollDown;
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.RollDown(APopupControl: TControl;
  AWidth, AHeight: Integer; IsVertical: Boolean = False;
  FocusPopup: Boolean = True);
var
  P, Size: TPoint;
begin
  if Assigned(APopupControl) and Assigned(APopupControl.Parent) then begin
    FPopupControl := APopupControl;
    // Increase the size of the form if the size grip is visible
    Size.X := AWidth;
    Size.Y := AHeight;
    if FBorderStyle in [pbsSizeableBottom, pbsSizeableRightBottom] then
      Size.Y := Size.Y + FSizeGrip.Height;

    P := SpCalcPopupPosition(0, 0, Size.X, Size.Y, APopupControl, IsVertical);
    RollDown(P.X, P.Y, AWidth, AHeight, FocusPopup);
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.RollUp(Selected: Boolean; FocusParentControl: Boolean = True);
var
  W: TWinControl;
  Msg: TMessage;
begin
  // Instead of checking for Self.Visible check if the actual wrapped form
  // is visible.
  if FFormPopupMenu.PopupForm.Visible then begin
    UninstallHooks;

    if Assigned(FPopupControl) and (FPopupControl is TWinControl) then begin
      W := FPopupControl as TWinControl;
      if FocusParentControl and W.CanFocus then
        W.SetFocus;
      // Send a message to the PopupControl and it's children controls
      // to inform that the Popup was closed.
      Msg.Msg := CM_SPPOPUPCLOSE;
      Msg.WParam := WPARAM(Self);
      if Selected then
        Msg.LParam := 1
      else
        Msg.LParam := 0;
      Msg.Result := 0;
      PostMessage(W.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
      W.Broadcast(Msg);
    end;

    Visible := False;

    // Broadcast the close message to all the notifies
    if Assigned(ActiveFormPopupMenu) then
      ActiveFormPopupMenu.BroadcastCloseMessage(Selected);
    ActiveFormPopupMenu := nil; // Reset global variable
    FPopupControl := nil;
    DoRollUp(Selected);
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
begin
  if FBorderStyle <> Value then
    FBorderStyle := Value;
end;

procedure TSpTBXCustomWrapperPopupForm.InstallHooks;
begin
  if not FHooksInstalled then begin
    FHooksInstalled := True;

    // Needed to handle main form mouse clicks when the popup is visible
    FOldAppOnMessage := Application.OnMessage;
    Application.OnMessage := AppOnMessageHook;

    if Assigned(FPopupControl) then begin
      // Needed to handle focus changes when the popup is visible but the
      // ParentControl has the focus, like the Comboboxes
      FOldPopupControlWndProc := FPopupControl.WindowProc;
      FPopupControl.WindowProc := PopupControlWindowProc;
    end;
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.UninstallHooks;
begin
  if FHooksInstalled then begin
    FHooksInstalled := False;

    Application.OnMessage := FOldAppOnMessage;
    FOldAppOnMessage := nil;

    if Assigned(FPopupControl) then begin
      FPopupControl.WindowProc := FOldPopupControlWndProc;
      FOldPopupControlWndProc := nil;
    end;
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.AppOnMessageHook(var Msg: TMsg; var Handled: Boolean);
begin
  if Assigned(FOldAppOnMessage) then FOldAppOnMessage(Msg, Handled);

  if not IsWindowEnabled(Handle) then begin
    Handled := False;
    Exit;
  end;

  case Msg.message of
    CM_DEACTIVATE:
      begin
        // Rollup when the popup is deactivated
        // Instead of calling Rollup post a message so the
        // Application.OnMessage is processed before
        // the popup is closed, this is needed to handle
        // the mouse clicks on the main form
        PostMessage(Handle, WM_SPTBX_POPUPROLLUP, 0, 0); // Set FocusParentControl param to False
      end;
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN:
      // If the click was not on the popup, rollup and Handle the message
      if (GetCapture = 0) and (Msg.hwnd <> Handle) and not Windows.IsChild(Handle, Msg.hwnd) then
        if Assigned(FPopupControl) and not (FPopupControl is TCustomForm) and (FPopupControl is TWinControl) then begin
          if Msg.hwnd <> TWinControl(FPopupControl).Handle then begin
            Handled := True;
            RollUp(False);
          end;
        end
        else begin
          Handled := True;
          RollUp(False);
        end;
    WM_NCLBUTTONDOWN..WM_NCMBUTTONDBLCLK:
      // If the click was not on the popup, rollup and Handle the message
      if (Msg.hwnd <> Handle) and not Windows.IsChild(Handle, Msg.hwnd) then begin
        Handled := True;
        RollUp(False);
      end;
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.PopupControlWindowProc(var Message: TMessage);
begin
  if Assigned(FOldPopupControlWndProc) then FOldPopupControlWndProc(Message);

  if Visible then
    case Message.Msg of
      CM_FOCUSCHANGED:
        // Rollup when the popup is deactivated
        // Instead of calling Rollup post a message so the
        // Application.OnMessage is processed before
        // the popup is closed, this is needed to handle
        // the focus change on the main form
        PostMessage(Handle, WM_SPTBX_POPUPROLLUP, 0, 0); // Set FocusParentControl param to False
      CM_CHILDKEY:
        if Message.WParam = VK_ESCAPE then
          RollUp(False);
//      CM_CANCELMODE:
//        RollUp(False);
    end;
end;

procedure TSpTBXCustomWrapperPopupForm.CMCancelMode(var Message: TCMCancelMode);
begin
  inherited;
  RollUp(False);
end;

procedure TSpTBXCustomWrapperPopupForm.CMChildKey(var Message: TCMChildKey);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    RollUp(False);
end;

procedure TSpTBXCustomWrapperPopupForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active = WA_INACTIVE then begin
    // Rollup when the popup is deactivated
    // Instead of calling Rollup post a message so the
    // Application.OnMessage is processed before
    // the popup is closed, this is needed to handle
    // the mouse clicks on the main form
//    PostMessage(Handle, WM_SPTBX_POPUPROLLUP, 0, 1);
//    ^ Not needed
  end
  else begin
    // When the popup is activated redraw the caption bar of the Main Form
    // And invalidate the client and non client area
    SendMessage(Message.ActiveWindow, WM_NCACTIVATE, 1, 0);
    // Post the invalidate message on Vista to repaint the form borders
    if SpIsWinVistaOrUp then
      PostMessage(Handle, WM_SPTBX_POPUPINVALIDATE, 0, 0);
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  Message.Result := 0;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -DefaultBorderSize, -DefaultBorderSize);
end;

procedure TSpTBXCustomWrapperPopupForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  GR: TRect;
  HitOnBorder: Boolean;
begin
  inherited;

  HitOnBorder := Message.Result in [HTLEFT, HTTOP, HTTOPLEFT, HTTOPRIGHT, HTRIGHT, HTBOTTOM, HTBOTTOMLEFT, HTBOTTOMRIGHT];

  if HitOnBorder then
    case FBorderStyle of
      pbsFrame:
        Message.Result := HTNOWHERE;
      pbsSizeableBottom:
        begin
          // Make the NC area resizeable
          Message.Result := HTNOWHERE;
          P := FSizeGrip.ScreenToClient(SmallPointToPoint(Message.Pos));
          GR := FSizeGrip.GetGripRect;
          if P.Y >= GR.Top then
            Message.Result := HTBOTTOM;
        end;
      pbsSizeableRightBottom:
        begin
          // Make the NC area resizeable
          Message.Result := HTNOWHERE;
          P := FSizeGrip.ScreenToClient(SmallPointToPoint(Message.Pos));
          GR := FSizeGrip.GetGripRect;
          if P.Y >= GR.Top then
            if P.X >= GR.Left then
              Message.Result := HTBOTTOMRIGHT;
        end;
    end;
end;

procedure PopupWindowNCPaintProc(Wnd: HWND; DC: HDC; AppData: TObject);
// Paints the NC area and the client background, used by WMEraseBkgnd, WMNCPaint, WMPrint
var
  ACanvas: TCanvas;
  R: TRect;
  PopupWindow: TSpTBXCustomWrapperPopupForm;
begin
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := DC;
    GetWindowRect(Wnd, R);
    OffsetRect(R, -R.Left, -R.Top);

    // If it's used by WM_ERASEBKGND offset the rect
    PopupWindow := TSpTBXCustomWrapperPopupForm(AppData);
    if PopupWindow.FPaintingClientArea then begin
      PopupWindow.FPaintingClientArea := False;
      OffsetRect(R, -DefaultBorderSize, -DefaultBorderSize);
    end;

    PopupWindow.PaintBackground(ACanvas, R);
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // There's no need to call PopupWindowNCPaintProc here, because the
  // ClientArea is filled with the child Form
  Message.Result := 1;
end;

procedure TSpTBXCustomWrapperPopupForm.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    FPaintingClientArea := False;
    SelectNCUpdateRgn(Handle, DC, HRGN(Message.WParam));
    PopupWindowNCPaintProc(Handle, DC, Self);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TSpTBXCustomWrapperPopupForm.WMPrint(var Message: TMessage);
begin
  FPaintingClientArea := False;
  HandleWMPrint(Handle, Message, PopupWindowNCPaintProc, Self);
end;

procedure TSpTBXCustomWrapperPopupForm.WMSpTBXPopupInvalidate(var Message: TMessage);
begin
  if HandleAllocated then
    RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_FRAME);
end;

procedure TSpTBXCustomWrapperPopupForm.WMSpTBXPopupRollUp(var Message: TMessage);
var
  Selected, FocusParentControl: Boolean;
begin
  Selected := Message.WParam <> 0;
  FocusParentControl := Message.LParam <> 0;
  RollUp(Selected, FocusParentControl);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXWrapperPopupForm }

destructor TSpTBXWrapperPopupForm.Destroy;
begin
  Destroying;
  if HandleAllocated then
    DestroyWindowHandle;
  inherited;
end;

procedure TSpTBXWrapperPopupForm.DestroyWindowHandle;
begin
  { Cleanly destroy any timers before the window handle is destroyed }
  CallNotifyWinEvent(EVENT_SYSTEM_MENUPOPUPEND, WindowHandle, OBJID_CLIENT, CHILDID_SELF);
  inherited;
end;

procedure TSpTBXWrapperPopupForm.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
var
  Blend: Boolean;
begin
  { Must override TCustomForm/TForm's CM_SHOWINGCHANGED handler so that the
    form doesn't get activated when Visible is set to True. }

  { Handle animation }
  if Showing and not IsWindowVisible(WindowHandle) and (Animation <> patNone) then
  begin
    Blend := Animation = patFade;
    if Assigned(AnimateWindowProc) and (Blend or (FAnimationDirection <> [])) then begin
      AnimateWindowProc(Handle, 150, AW_BLEND);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
      Exit;
    end;
  end;

  { No animation... }
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

procedure TSpTBXWrapperPopupForm.PaintBackground(ACanvas: TCanvas; ARect: TRect);
begin
  SpDrawXPMenuPopupWindow(ACanvas, ARect, Rect(0, 0, 0, 0), False, 0);
end;

procedure TSpTBXWrapperPopupForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXFormPopupMenu }

constructor TSpTBXFormPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FNotifies := TList.Create;
  FWrapperForm := TSpTBXWrapperPopupForm.Create(Self);
  FWrapperForm.DefaultMonitor := dmDesktop;
  FWrapperForm.OnRollUp := InternalClosePopup;
end;

destructor TSpTBXFormPopupMenu.Destroy;
var
  FC: TCustomFormClass;
begin
  if Assigned(FPopupForm) then
    FPopupForm.Parent := nil;
  // Free the PopupForm if the OnGetPopupFormClass event returns
  // a valid form class.
  FC := nil;
  DoGetPopupFormClass(FC);
  if Assigned(FC) then
    FreeAndNil(FPopupForm);

  FreeAndNil(FNotifies);
  FreeAndNil(FWrapperForm);
  inherited;
end;

procedure TSpTBXFormPopupMenu.DoGetPopupFormClass(var AFormClass: TCustomFormClass);
begin
  if Assigned(FOnGetPopupFormClass) then FOnGetPopupFormClass(Self, AFormClass);
end;

procedure TSpTBXFormPopupMenu.AddCloseNotification(AObject: TObject);
begin
  if Assigned(AObject) then
    if FNotifies.IndexOf(AObject) < 0 then FNotifies.Add(AObject);
end;

procedure TSpTBXFormPopupMenu.RemoveCloseNotification(AObject: TObject);
begin
  FNotifies.Remove(AObject);
end;

procedure TSpTBXFormPopupMenu.BroadcastCloseMessage(Selected: Boolean);
var
  Msg: TMessage;
  I: Integer;
begin
  if FNotifies.Count > 0 then begin
    Msg.Msg := CM_SPPOPUPCLOSE;
    Msg.WParam := WPARAM(FWrapperForm);
    if Selected then
      Msg.LParam := 1
    else
      Msg.LParam := 0;
    Msg.Result := 0;
    for I := 0 to FNotifies.Count - 1 do
      TObject(FNotifies[I]).Dispatch(Msg);
  end;
end;

procedure TSpTBXFormPopupMenu.ClosePopup(Selected: Boolean);
begin
  // Instead of calling Rollup post a message so the Application.OnMessage is
  // processed before the popup is closed, this is needed to handle the mouse
  // clicks on the main form.
  PostMessage(FWrapperForm.Handle, WM_SPTBX_POPUPROLLUP, WPARAM(Selected), 1);
end;

procedure TSpTBXFormPopupMenu.InternalClosePopup(Sender: TObject; Selected: Boolean);
var
  FC: TCustomFormClass;
begin
  if Assigned(FOnBeforeClosePopup) then FOnBeforeClosePopup(Self, Selected);

  if Assigned(FPopupForm) then begin
    if FAutoSize then begin
      FPopupFormPrevSize.cx := FPopupForm.ClientRect.Right;
      FPopupFormPrevSize.cy := FPopupForm.ClientRect.Bottom;
    end;
    FPopupForm.Visible := False;
    FPopupForm.Parent := nil;
    FPopupForm.Align := alNone;
    if FPopupFormState.PopupForm = FPopupForm then begin
      FPopupForm.BorderStyle := FPopupFormState.BorderStyle;
      FPopupForm.BoundsRect := FPopupFormState.BoundsRect;
    end;
  end;

  if Assigned(FOnClosePopup) then FOnClosePopup(Self, Selected);

  // Free the PopupForm if the OnGetPopupFormClass event returns
  // a valid form class.
  FC := nil;
  DoGetPopupFormClass(FC);
  if Assigned(FC) then
    FreeAndNil(FPopupForm);
end;

function TSpTBXFormPopupMenu.InternalPopup(X, Y: Integer; ForceFocus: Boolean;
  PopupControl: TControl = nil): Boolean;
var
  ClientR: TRect;
  FC: TCustomFormClass;
begin
  Result := False;

  {$IFDEF JR_D9}
  SetPopupPoint(Point(X, Y));
  {$ELSE}
  PPoint(@PopupPoint)^ := Point(X, Y);
  {$ENDIF}

  // Create the PopupForm if the OnGetPopupFormClass event returns
  // a valid form class.
  // Otherwise try to use the assigned PopupForm property.
  FC := nil;
  DoGetPopupFormClass(FC);
  if Assigned(FC) then
    FPopupForm := FC.Create(nil);

  // Use the WrapperForm to show the PopupForm
  if Assigned(FPopupForm) then begin
    FPopupFormState.PopupForm := FPopupForm;
    FPopupFormState.BorderStyle := FPopupForm.BorderStyle;
    FPopupFormState.BoundsRect := FPopupForm.BoundsRect;

    ClientR := FPopupForm.ClientRect;
    if FAutoSize and (FPopupFormPrevSize.cx > 0) and (FPopupFormPrevSize.cy > 0) then begin
      ClientR.Right := FPopupFormPrevSize.cx;
      ClientR.Bottom := FPopupFormPrevSize.cy;
    end;

    FPopupForm.Parent := FWrapperForm;
    FPopupForm.Align := alClient;
    FPopupForm.BorderStyle := bsNone;
    FPopupForm.Visible := True;

    if Assigned(FOnBeforePopup) then FOnBeforePopup(Self, ClientR.Right, ClientR.Bottom);

    if Assigned(PopupControl) then
      FWrapperForm.RollDown(PopupControl, ClientR.Right + DefaultBorderSize * 2, ClientR.Bottom + DefaultBorderSize * 2, False, ForceFocus)
    else
      FWrapperForm.RollDown(X, Y, ClientR.Right + DefaultBorderSize * 2, ClientR.Bottom + DefaultBorderSize * 2, ForceFocus);

    if Assigned(OnPopup) then OnPopup(Self);
    Result := True;    
  end;
end;

procedure TSpTBXFormPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = PopupForm then begin
      // Weird Delphi IDE bug at design time.
      // When a Form is closed at design time it will fire the
      // FreeNotification, setting PopupForm to nil and not
      // saving it to the dfm

      SetPopupForm(nil);

      { TODO : Delphi IDE bug }
      // This line doesn't seem to fix it, it raises AVs in the IDE
      // PopupForm shouldn't be published.
      // if not (csDesigning in ComponentState) then
      //     SetPopupForm(nil);
    end;
end;

procedure TSpTBXFormPopupMenu.Popup(X, Y: Integer);
begin
  InternalPopup(X, Y, FPopupFocus);
end;

procedure TSpTBXFormPopupMenu.PopupEx(X, Y: Integer; ForceFocus: Boolean);
begin
  InternalPopup(X, Y, ForceFocus);
end;

procedure TSpTBXFormPopupMenu.PopupEx(PopupControl: TControl; ForceFocus: Boolean);
begin
  InternalPopup(0, 0, ForceFocus, PopupControl);
end;

function TSpTBXFormPopupMenu.GetBorderStyle: TSpTBXPopupBorderStyleType;
begin
  Result := FWrapperForm.BorderStyle;
end;

function TSpTBXFormPopupMenu.GetShowShadows: Boolean;
begin
  Result := FWrapperForm.ShowShadows;
end;

procedure TSpTBXFormPopupMenu.SetBorderStyle(const Value: TSpTBXPopupBorderStyleType);
begin
  FWrapperForm.BorderStyle := Value;
end;

procedure TSpTBXFormPopupMenu.SetPopupForm(const Value: TCustomForm);
begin
  if FPopupForm <> Value then begin
    if FWrapperForm.Visible then
      ClosePopup(False);

    if Assigned(FPopupForm) then
      FPopupForm.RemoveFreeNotification(Self);

    FPopupForm := Value;
    if Assigned(FPopupForm) then
      FPopupForm.FreeNotification(Self);
  end;
end;

procedure TSpTBXFormPopupMenu.SetShowShadows(const Value: Boolean);
begin
  FWrapperForm.ShowShadows := Value;
end;

end.
