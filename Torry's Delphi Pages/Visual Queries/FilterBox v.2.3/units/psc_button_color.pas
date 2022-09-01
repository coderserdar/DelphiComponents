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
unit psc_button_color;

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

  psc_colorbox,
  psc_wrapper,
  psc_procs,
  psc_const;

{------------------------------------}

type
  TPSCBorderChanging = (bcEnabled,bcDisabled,bcNeedUpdate);

  TPSCActivatePopupEvent = Procedure(Sender: TObject; PrevWindow: HWND) Of
    Object;
  TPSCPopupCloseEvent = Procedure(Sender: TObject; Canceled: Boolean) Of Object;

  TPSCPopupBorderStyle = (pbsNone,pbsPopup,pbsSizePopup,
    pbsDragBar,pbsSizeDragBar,pbsToolWindow,pbsSizeToolWin);

  TPSCPopupWindow = Class(TCustomForm)
  private
    FPopupComponent: TComponent;
    FAutoHide: Boolean;
    FOnActivatePopup: TPSCActivatePopupEvent;
    FOnDeactivatePopup: TPSCActivatePopupEvent;
    FBorderStyle: TPSCPopupBorderStyle;
    FDragBar: TControl;
    FHook: TCollectionItem;
    FOnDetached: TPSCNotifyEvent;
    FOnPopupClose: TPSCPopupCloseEvent;

    Procedure SetPopupComponent(Value: TComponent);
    Procedure SetBorderStyle(Value: TPSCPopupBorderStyle);
    Function GetDragBar: TControl;
    Procedure UpdateDragBar;
    Procedure DoDragBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
    Procedure WMNCActivate(Var Message: TWMNCActivate); message WM_NCACTIVATE;
    Procedure WMActivate(Var Message: TWMActivate); message WM_ACTIVATE;
    Procedure WMWindowPosChanging(Var Message: TWMWindowPosChanging);
      message WM_WINDOWPOSCHANGING;
    Procedure WMSysCommand(Var Message: TWMSysCommand); message WM_SYSCOMMAND;
    Procedure CMDialogKey(Var Message: TCMDialogKey); message CM_DIALOGKEY;
    Procedure CMActiveChanged(Var Message: TMessage);
      message CM_ACTIVECHANGED;
  protected
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    Procedure CreateParams(Var Params: TCreateParams); override;
    Procedure DoHide; override;
  public
    Constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    Procedure Popup(X,Y: Integer); overload;
    Destructor Destroy; override;
    Procedure Popup(Control: TControl; Alignment: TAlignment);overload;

    Property AutoHide: Boolean read FAutoHide write FAutoHide default true;
    Property DragBar: TControl read GetDragBar;
    Property PopupComponent: TComponent read FPopupComponent
      write SetPopupComponent;
    Property OnActivatePopup: TPSCActivatePopupEvent read FOnActivatePopup
      write FOnActivatePopup;
    Property OnDeactivatePopup: TPSCActivatePopupEvent read FOnDeactivatePopup
      write FOnDeactivatePopup;
    Property OnDetached: TPSCNotifyEvent read FOnDetached write FOnDetached;
  published
    Property BorderStyle: TPSCPopupBorderStyle read FBorderStyle
      write SetBorderStyle default pbsPopup;
    Property OnPopupClose: TPSCPopupCloseEvent read FOnPopupClose
      write FOnPopupClose;

    Property Action;
    Property Anchors;
    Property AutoSize;
    Property BiDiMode;
    Property BorderWidth;
    Property Constraints;
    Property UseDockManager;
    Property DefaultMonitor;
    Property DockSite;
    Property DragKind;
    Property ParentBiDiMode;
    Property ActiveControl;
    Property Align;
    Property AutoScroll;
    Property BorderIcons;
    Property Caption;
    Property ClientHeight;
    Property ClientWidth;
    Property Color;
    Property Ctl3D;
    Property DragMode;
    Property Enabled;
    Property ParentFont default False;
    Property Font;
    Property FormStyle;
    Property Height;
    Property HelpFile;
    Property HorzScrollBar;
    Property Icon;
    Property KeyPreview;
    Property Menu;
    Property ObjectMenuItem;
    Property PixelsPerInch;
    Property PopupMenu;
    Property Position;
    Property PrintScale;
    Property Scaled;
    Property ShowHint;
    Property VertScrollBar;
    Property Visible;
    Property Width;
    Property WindowState;
    Property WindowMenu;
    Property OnActivate;
    Property OnCanResize;
    Property OnConstrainedResize;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnClick;
    Property OnClose;
    Property OnCloseQuery;
    Property OnContextPopup;
    Property OnCreate;
    Property OnDblClick;
    Property OnDestroy;
    Property OnDeactivate;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property OnShortCut;
    Property OnStartDock;
    Property OnHide;
    Property OnHelp;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnPaint;
    Property OnResize;
    Property OnShow;
    Property OnUnDock;
  End;

  TPSCColorBoxPopup = Class(TPSCPopupWindow)
  private
    FColorBox: TPSCCustomColorBox;

    Procedure SetColorBox(Value: TPSCCustomColorBox);
    Function GetColorBox: TPSCCustomColorBox;
    Procedure WMEraseBkgnd(Var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    Procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    Function CanResize(Var NewWidth,NewHeight: Integer): Boolean; override;
  public
    Property ColorBox: TPSCCustomColorBox read GetColorBox write SetColorBox;
  End;

  TPSCColorKind = (ckAuto,ckColor);
  TPSCColorBarOptions = Set Of (cbDisplayNames,cbDoubleBorder);
  TPSCColorBarState = Set Of (cbsPopupUpdating,cbsSelectedUpdating);

  TPSCColorButtonState = (cbsNone,cbsUp,cbsDown,cbsArrowDown,
    cbsBtnDown,cbsExclusive);
  TPSCColorButtonStyle = (bstCustom,bstBkGround,bstBrush,bstFont);
  TPSCDrawButtonEvent = Procedure(Sender: TObject; ACanvas: TPSCCanvas;
    AColorOnly: Boolean) Of Object;

  TPSCColorSelectedEvent = Procedure(Sender: TObject; AColor: TPSCColor) Of Object;

  TPSCColorButton = Class(TGraphicControl)
  private
    FVersion: String;
    FColorBox: TPSCCustomColorBox;
    FTrackColor: Boolean;
    FDetachedPopup: TPSCColorBoxPopup;
    FState: TPSCColorBarState;
    FActiveSlot: TPSCColorSlot;
    FPopupAlignment: TAlignment;
    FCaption: String;
    FDragPopup: Boolean;
    FFlat: Boolean;
    FPopup: TPSCColorBoxPopup;
    FSelectedColor: TPSCColor;
    FColorBoxColor: TPSCColor;
    FOnSelected: TPSCColorSelectedEvent;
    FOnDrawButton: TPSCDrawButtonEvent;
    FBtnStyle: TPSCColorButtonStyle;
    FBtnState: TPSCColorButtonState;
    FPressed: Boolean;
    FPopupShowing: Boolean;
    FMouseInside: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TPSCImageList;
    FImageActive: integer;
    FImageInactive: integer;
    FImagePressed: integer;
    FStyle: TPSCColorBoxStyle;
    FOptions: TPSCColorBoxOptions;
    FGrouped: Boolean;
    FGroupIndex: integer;
    FHasDownState: Boolean;
    FExclusive: Boolean;
    FExclusiveColor: TPSCColor;
    FSizeablePopup: Boolean;
    FHighlightActive: Boolean;
    FHighlightColor: TPSCColor;

    Procedure SetVersion(const AValue:String);
    Procedure SetColorBoxColor(Value: TPSCColor);
    Function IsColorBoxStored: Boolean;
    Function GetColorBox: TPSCCustomColorBox;
    Procedure SetColorBox(Value: TPSCCustomColorBox);
    Procedure UpdateSelectedColor;
    Procedure SetActiveSlot(Value: TPSCColorSlot);
    Procedure SetHighlightActive(Value: Boolean);
    Procedure SetHighlightColor(Value: TPSCColor);
    Procedure SetFlat(Value: Boolean);
    Procedure SetExclusiveColor(Value: TPSCColor);
    Function GetDown: Boolean;
    Procedure SetDown(Value: Boolean);
    Procedure SetHasDownState(Value: Boolean);
    Procedure SetState(Value: TPSCColorButtonState);
    Procedure SetButtonStyle(Value: TPSCColorButtonStyle);
    Procedure SetSelectedColor(Value: TPSCColor);
    Procedure SetImages(Value: TPSCImageList);
    Procedure ImageListChange(Sender: TObject);
    Procedure SetImageActive(Value: Integer);
    Procedure SetImageInactive(Value: Integer);
    Procedure SetImagePressed(Value: Integer);
    Procedure CMHintShow(Var Message: TCMHintShow); message CM_HINTSHOW;
    Procedure CMMouseEnter(Var Message: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MOUSELEAVE;
    Procedure PopupDeactivate(Sender: TObject; PrevWindow: HWND);
    Procedure UpdateTracking;
    Procedure SetStyle(Value: TPSCColorBoxStyle);
    Procedure SetOptions(Value: TPSCColorBoxOptions);
    Procedure UpdateColor;
    Function GetPopup: TPSCColorBoxPopup;
    Procedure MovePopup;
    Procedure DoClick(Sender: TObject);
    Procedure DoDeleteSlot(ColorBox: TObject; Slot: TPSCColorSlot);
    Procedure DoActiveChanged(ColorBox: TObject;
      Slot: TPSCColorSlot);
    Procedure ColorSelected(ColorBox: TObject; Slot: TPSCColorSlot);
    Procedure MoreColorClick(ColorBox: TObject; Slot: TPSCColorSlot;
      Color: TPSCColor; Stage: TPSCButtonClickStage);
    Procedure DoSlotChanged(ColorBox: TObject; Slot: TPSCColorSlot);
    Procedure LinkEvents(ColorBox: TPSCCustomColorBox);
    Procedure DoFindMethod(Reader: TReader; Const MethodName: String;
      Var Address: Pointer; Var Error: Boolean);
  protected
    Procedure DoBeforeShowPopup(Sender: TObject); dynamic;
    Procedure DoDetachPopup(Sender: TObject); dynamic;

    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    Procedure Loaded; override;
    Procedure Paint; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Property ActiveSlot: TPSCColorSlot read FActiveSlot write SetActiveSlot;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Procedure ShowPopup; virtual;
    Procedure UpdatePopup; virtual;

    Procedure Click; override;
    Property Popup: TPSCColorBoxPopup read GetPopup;
    Property State: TPSCColorButtonState read FBtnState write SetState;
  published
    Property Version: String read FVersion write SetVersion stored false;
    Property ButtonStyle: TPSCColorButtonStyle read FBtnStyle write
      SetButtonStyle
      default bstFont;
    Property Caption: String read FCaption write FCaption;
    Property DragPopup: Boolean read FDragPopup write FDragPopup default false;
    Property Enabled;
    Property ExclusiveColor: TPSCColor read FExclusiveColor write SetExclusiveColor
      default clPSCBtnHighlight;
    Property Flat: Boolean read FFlat write SetFlat default true;
    Property Grouped: Boolean read FGrouped write FGrouped default false;
    Property GroupIndex: Integer read FGroupIndex write FGroupIndex
      default 0;
    Property HasDownState: Boolean read FHasDownState write SetHasDownState
      default false;
    Property Hint;
    Property HighlightActive: Boolean read FHighlightActive
      write SetHighlightActive default false;
    Property HighlightColor: TPSCColor read FHighlightColor
      write SetHighlightColor default clPSCHighlight;
    Property Down: Boolean read GetDown write SetDown default false;
    Property Images: TPSCImageList read FImages write SetImages;
    Property ImageActive: integer read FImageActive write SetImageActive
      default -1;
    Property ImageInactive: integer read FImageInactive write SetImageInactive
      default -1;
    Property ImagePressed: integer read FImagePressed write SetImagePressed
      default -1;
    Property PopupMenu;
    Property SelectedColor: TPSCColor read FSelectedColor write SetSelectedColor
      default clPSCBlack;
    Property ColorBoxColor: TPSCColor read FColorBoxColor write SetColorBoxColor
      default clPSCBlack;
    Property SizeablePopup: Boolean read FSizeablePopup write FSizeablePopup
      default false;
    Property Style: TPSCColorBoxStyle read FStyle write SetStyle
      default cbsWordFont;
    Property SectionsSet: TPSCColorBoxOptions read FOptions write SetOptions
      default [];
    Property Visible;

    Property ColorBox: TPSCCustomColorBox read GetColorBox write SetColorBox
      stored IsColorBoxStored;
    Property TrackColor: Boolean read FTrackColor write FTrackColor
      default false;
    Property PopupAlignment: TAlignment read FPopupAlignment
      write FPopupAlignment default taLeftJustify;

    Property OnClick;
    Property OnColorSelected: TPSCColorSelectedEvent read FOnSelected
      write FOnSelected;
    Property OnDrawButton: TPSCDrawButtonEvent read FOnDrawButton
      write FOnDrawButton;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
  End;

Procedure PSCCalcPopupRect(Const ControlRect: TRect;
  Alignment: TAlignment; Var Bounds: TRect);
{------------------------------------}

implementation

{-------------------------------------}

Procedure TPSCColorBoxPopup.SetColorBox(Value: TPSCCustomColorBox);
Var
  AColorBox: TPSCCustomColorBox;
Begin
  If FColorBox <> Value Then
    Begin
      AColorBox := FColorBox;
      FColorBox := Value;
      If AColorBox <> Nil Then
        Begin
          If AColorBox.Owner = Self Then
            AColorBox.Free
          Else
            RemoveFreeNotification(AColorBox)
        End;
      If Value <> Nil Then
        Begin
          FreeNotification(Value);
          If Not (csDesigning In Value.ComponentState) Then
            Value.Parent := Self
        End
    End
End;

{-------------------------------------}

Function TPSCColorBoxPopup.GetColorBox: TPSCCustomColorBox;
Begin
  If FColorBox = Nil Then
    Begin
      FColorBox := TPSCColorBox.Create(Self);
      FColorBox.Style := cbsWordFont;
      FColorBox.Parent := Self;
      FColorBox.AutoSize := true;
    End;
  Result := FColorBox
End;

{-------------------------------------}

Procedure TPSCColorBoxPopup.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (FColorBox = AComponent) Then
    FColorBox := Nil
End;

{-------------------------------------}

Function TPSCColorBoxPopup.CanResize(Var NewWidth,NewHeight: Integer): Boolean;
Var
  dy,dx: Integer;
Begin
  Result := Inherited CanResize(NewWidth,NewHeight);
  If Result And Not (csDesigning In ColorBox.ComponentState) Then
    Begin
      With DragBar Do
        If Visible Then
          dy := Height
        Else
          dy := 0;
      dy := dy + Height - ClientHeight;
      dx := Width - ClientWidth;
      DisableAlign;
      Try
        With ColorBox Do
          Begin
            Try
              Align := alNone;
              SetWindowPos(Handle,0,0,0,NewWidth - dx,NewHeight - dy,
                SWP_HIDEWINDOW Or SWP_NOREDRAW Or
                SWP_NOMOVE Or SWP_NOOWNERZORDER Or SWP_NOZORDER);
              NewWidth := Width + dx;
              NewHeight := Height + dy;
              Align := alClient;
            Finally
              If Visible Then
                SetWindowPos(Handle,0,0,0,0,0,
                  SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOOWNERZORDER Or
                  SWP_NOSIZE Or SWP_NOZORDER);
            End;
          End;
      Finally
        EnableAlign;
      End;
    End;
End;

{-------------------------------------}

Procedure TPSCColorBoxPopup.WMEraseBkgnd(Var Message: TWmEraseBkgnd);
Begin
  Message.Result := 1
End;

Type
  TPSCDraggingState = (dsNoDragging,dsStartDragging,dsDragging,dsStopDragging);

  TPSCDragBar = Class(TGraphicControl)
  private
    FMouseInControl: Boolean;
    FMagnetic: Boolean;

    Procedure CMMouseEnter(Var Message: TMessage); message CM_MOUSEENTER;
    Procedure CMMouseLeave(Var Message: TMessage); message CM_MOUSELEAVE;
    Procedure CMDialogKey(Var Message: TCMDialogKey); message CM_DIALOGKEY;
  protected
    Procedure Paint; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X,Y: Integer); override;

    Property MouseInControl: Boolean read FMouseInControl;
  public
    Constructor Create(AOwner: TComponent); override;

    Function Dragging: TPSCDraggingState;
  published
    Property Align default alTop;
    Property Enabled;
    Property ShowHint;
    Property ParentShowHint;
    Property Visible;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;

    Property Magnetic: Boolean read FMagnetic write FMagnetic default true;
  End;

  TPopupHook = Class(TPSCNamedItem)
  private
    FForm: TCustomForm;
    FOldWindowProc: TWndMethod;
    FPopups: IPSCObjectList;
  public
    Constructor Create(Collection: TCollection); override;

    Procedure WindowProc(Var Message: TMessage);
  End;

  TPopupHooks = Class(TPSCNamedItems)
  public
    Class Function HookPopupWindow(PopupWindow: TPSCPopupWindow): TPopupHook;
    Class Procedure UnhookPopupWindow(PopupWindow: TPSCPopupWindow);
  End;

Var
  PopupHooks: TPopupHooks = Nil;
  DraggingBar: TPSCDragBar = Nil;
  StartDragRect: TRect;
  StartDragPoint: TPoint;

{-------------------------------------}

Constructor TPopupHook.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FPopups := PSCCreateObjectList(ioReferenced);
End;

{-------------------------------------}

Procedure TPopupHook.WindowProc(Var Message: TMessage);
Const
  ShowFlags: Array[Boolean] Of Word = (
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
Var
  I: Integer;
  ShowFlag,NeedToHide: Boolean;
Begin
  Case Message.Msg Of
    WM_NCLBUTTONDOWN:
      With FForm Do
        Begin
          If Not Active Then
            BringToFront;
          SendCancelMode(FForm);
          Update
        End;
    CM_SHOWINGCHANGED:
      Begin
        ShowFlag := FForm.Visible And (FForm.WindowState <> wsMinimized);
        For I := 0 To FPopups.Count - 1 Do
          With TPSCPopupWindow(FPopups[I]) Do
            If HandleAllocated And Visible Then
              SetWindowPos(Handle,0,0,0,0,0,ShowFlags[ShowFlag])
      End;
    WM_NCACTIVATE:
      If Not TWMNCActivate(Message).Active Then
        For I := 0 To FPopups.Count - 1 Do
          With TPSCPopupWindow(FPopups[I]) Do
            If HandleAllocated And IsWindowVisible(Handle) Then
              Begin
                Message.Result := 1;
                Exit
              End;
    WM_ACTIVATE:
      Begin
        If TWMActivate(Message).Active <> WA_INACTIVE Then
          Begin
            For I := 0 To FPopups.Count - 1 Do
              With TPSCPopupWindow(FPopups[I]) Do
                If HandleAllocated And Visible And Not IsWindowVisible(Handle)
                  Then
                  SetWindowPos(Handle,0,0,0,0,0,ShowFlags[true])
          End
        Else
          Begin
            NeedToHide := true;
            For I := 0 To FPopups.Count - 1 Do
              With TPSCPopupWindow(FPopups[I]) Do
                If HandleAllocated And (TWMActivate(Message)
                  .ActiveWindow = Handle) Then
                  Begin
                    NeedToHide := false;
                    Break
                  End;
            If NeedToHide Then
              Begin
                For I := 0 To FPopups.Count - 1 Do
                  With TPSCPopupWindow(FPopups[I]) Do
                    If HandleAllocated And Visible Then
                      SetWindowPos(Handle,0,0,0,0,0,ShowFlags[false]);
                If FForm.HandleAllocated And FForm.Visible Then
                  PostMessage(FForm.Handle,WM_NCACTIVATE,0,0)
              End
          End;
        // this is very very tricky
        If (TForm(FForm).FormStyle = fsMDIForm) And
          Not (csDesigning In FForm.ComponentState) Then
          Begin
            TPSCPopupWindow(FForm).SetDesigning(true,false);
            Try
              FOldWindowProc(Message)
            Finally
              TPSCPopupWindow(FForm).SetDesigning(false,false);
            End;
            Exit
          End
      End;
    WM_PARENTNOTIFY:
      With TWMParentNotify(Message) Do
        Case Event Of
          WM_LBUTTONDOWN,WM_MBUTTONDOWN,WM_RBUTTONDOWN:
            For I := 0 To FPopups.Count - 1 Do
              With TPSCPopupWindow(FPopups[I]) Do
                If HandleAllocated And Visible Then
                  Perform(CM_ACTIVECHANGED,0,
                    WindowFromPoint(FForm.ClientToScreen(Point(XPos,YPos)))
                    );
        End
  End;
  FOldWindowProc(Message);
End;

{-------------------------------------}

Class Function TPopupHooks.HookPopupWindow(
  PopupWindow: TPSCPopupWindow): TPopupHook;
Var
  Form: TCustomForm;
  I: Integer;
Begin
  Result := Nil;
  If PopupWindow.PopupComponent <> Nil Then
    Begin
      If PopupWindow.PopupComponent Is TControl Then
        Form := GetParentForm(TControl(PopupWindow.PopupComponent))
      Else
        If PopupWindow.PopupComponent.Owner Is TControl Then
          Form := GetParentForm(TControl(PopupWindow.PopupComponent.Owner))
        Else
          Form := Nil;
      If Form <> Nil Then
        Begin
          If PopupHooks = Nil Then
            PopupHooks := TPopupHooks.Create(nil,TPopupHook);
          For I := PopupHooks.Count - 1 DownTo 0 Do
            Begin
              Result := TPopupHook(PopupHooks.Items[I]);
              If Result.FForm = Form Then
                Break;
              Result := Nil;
            End;
          If Result = Nil Then
            Begin
              Result := TPopupHook(PopupHooks.Add);
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

Class Procedure TPopupHooks.UnhookPopupWindow(PopupWindow: TPSCPopupWindow);
Var
  Form: TCustomForm;
  I: Integer;
Begin
  If PopupHooks <> Nil Then
    Begin
      If PopupWindow.PopupComponent Is TControl Then
        Form := GetParentForm(TControl(PopupWindow.PopupComponent))
      Else
        If PopupWindow.PopupComponent.Owner Is TControl Then
          Form := GetParentForm(TControl(PopupWindow.PopupComponent.Owner))
        Else
          Form := Nil;
      If Form <> Nil Then
        For I := PopupHooks.Count - 1 DownTo 0 Do
          With TPopupHook(PopupHooks.Items[I]) Do
            If FForm = Form Then
              Begin
                FPopups.Remove(PopupWindow);
                If FPopups.Count = 0 Then
                  Begin
                    Form.WindowProc := FOldWindowProc;
                    Free;
                  End;
              End;
      If PopupHooks.Count = 0 Then
      begin
        PopupHooks.Free;
        PopupHooks:=nil;
      end;
    End
End;

{-------------------------------------}

Constructor TPSCDragBar.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  Height := 9 + 2 + 2;
  Width := 9 + 2 + 2;
  Hint := PSCConsts.DragBarHint;
  ShowHint := true;
  FMagnetic := true
End;

{-------------------------------------}

Function TPSCDragBar.Dragging: TPSCDraggingState;
Begin
  If MouseCapture Then
    If (DraggingBar = Self) Then
      Result := dsDragging
    Else
      Result := dsStartDragging
  Else
    If (DraggingBar = Self) Then
      Result := dsStopDragging
    Else
      Result := dsNoDragging
End;

{-------------------------------------}

Procedure TPSCDragBar.Paint;
Var
  Rect: TRect;
  DeltaX,DeltaY: Integer;
Begin
  Rect := ClientRect;
  Case Align Of
    alTop,alBottom:
      Begin
        DeltaX := -3;
        DeltaY := -2
      End;
    alRight,alLeft:
      Begin
        DeltaX := -2;
        DeltaY := -3
      End
  Else
    Begin
      DeltaX := -2;
      DeltaY := -2
    End
  End;
  InflateRect(Rect,DeltaX,DeltaY);
  Canvas.Brush.Style := BrushStyle_Solid;
  If (FMouseInControl Or MouseCapture) And Enabled Then
    Canvas.Brush.Color := clPSCActiveCaption
  Else
    Canvas.Brush.Color := clPSCInactiveCaption;
  Canvas.FillRect(Rect);
  Canvas.Brush.Color := Color;
  With Rect Do
    ExcludeClipRect(Canvas.Handle,Left,Top,Right,Bottom);
  Canvas.FillRect(ClientRect);
End;

{-------------------------------------}

Procedure TPSCDragBar.CMMouseEnter(Var Message: TMessage);
Begin
  Inherited;
  If Not FMouseInControl And Enabled Then
    Begin
      FMouseInControl := true;
      Repaint
    End
End;

{-------------------------------------}

Procedure TPSCDragBar.CMMouseLeave(Var Message: TMessage);
Begin
  Inherited;
  If FMouseInControl And Enabled Then
    Begin
      FMouseInControl := false;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCDragBar.CMDialogKey(Var Message: TCMDialogKey);
Begin
  Inherited;
  If (Dragging = dsDragging) And (Message.CharCode = VK_ESCAPE) Then
    Begin
      Parent.BoundsRect := StartDragRect;
      Perform(WM_CANCELMODE,0,0);
      Message.CharCode := 0;
      Message.Result := 1
    End
End;

{-------------------------------------}

Procedure TPSCDragBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  Inherited MouseDown(Button,Shift,X,Y);
  If Dragging = dsStartDragging Then
    With Parent Do
      Begin
        DraggingBar := Self;
        StartDragRect := BoundsRect;
        StartDragPoint.x := X + Left;
        StartDragPoint.y := Y + Top
      End;
End;

{-------------------------------------}

Procedure TPSCDragBar.MouseMove(Shift: TShiftState; X,Y: Integer);
Var
  DragRect: TRect;
  P: TPoint;
  Msg: TMsg;
Begin
  Inherited MouseMove(Shift,X,Y);
  If Dragging = dsDragging Then
    Begin
      Application.CancelHint;
      DragRect := BoundsRect;
      With StartDragRect Do
        OffsetRect(DragRect,Left,Top);
      InflateRect(DragRect,
        GetSystemMetrics(SM_CXSIZE),GetSystemMetrics(SM_CYSIZE));
      With Parent Do
        Begin
          P.x := Left + X;
          P.y := Top + Y;
          If FMagnetic And PtInRect(DragRect,P) Then
            BoundsRect := StartDragRect
          Else
            Begin
              DragRect := BoundsRect;
              OffsetRect(DragRect,
                P.x + StartDragRect.Left - StartDragPoint.x - DragRect.Left,
                P.y + StartDragRect.Top - StartDragPoint.y - DragRect.Top);
              BoundsRect := DragRect
            End
        End;
      While PeekMessage(Msg,0,WM_PAINT,WM_PAINT,PM_NOREMOVE) Do
        UpdateWindow(Msg.hwnd)
    End
End;

{-------------------------------------}

Procedure TPSCDragBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  Inherited MouseUp(Button,Shift,X,Y);
  If (Dragging <> dsDragging) And (DraggingBar = Self) Then
    Begin
      DraggingBar := Nil;
      Invalidate
    End
End;

{-------------------------------------}

Constructor TPSCPopupWindow.CreateNew(AOwner: TComponent; Dummy: Integer);
Begin
  Inherited CreateNew(AOwner);
  FormStyle := fsStayOnTop;
  Visible := false;
  FAutoHide := true;
  AutoScroll := false;
  FBorderStyle := pbsPopup
End;

{-------------------------------------}

Destructor TPSCPopupWindow.Destroy;
Begin
  PopupComponent := Nil;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCPopupWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (AComponent = FPopupComponent) Then
    PopupComponent := Nil
End;

{-------------------------------------}

Procedure TPSCPopupWindow.CreateParams(Var Params: TCreateParams);
Const
  BorderStyle: Array[Boolean] Of Cardinal = (WS_BORDER,WS_DLGFRAME);
Begin
  Inherited CreateParams(Params);
  If Not (csDesigning In ComponentState) Then
    With Params Do
      Begin
        Style := Style And Not (WS_CAPTION Or WS_THICKFRAME) Or WS_POPUP;
        ExStyle := ExStyle And Not WS_EX_CLIENTEDGE Or WS_EX_TOOLWINDOW;
        Case FBorderStyle Of
          pbsSizePopup,pbsSizeDragBar,pbsSizeToolWin:
            Style := Style Or WS_DLGFRAME Or WS_THICKFRAME;
          pbsPopup,pbsDragBar,pbsToolWindow:
            Style := Style Or BorderStyle[Ctl3D]
        End;
        Case FBorderStyle Of
          pbsDragBar,pbsSizeDragBar: DragBar;
          pbsToolWindow,pbsSizeToolWin: Style := Style Or WS_CAPTION
        End;
        UpdateDragBar
      End
End;

{-------------------------------------}

Procedure TPSCPopupWindow.DoHide;
Begin
  Inherited DoHide;
  If Assigned(FOnPopupClose) Then
    FOnPopupClose(Self,ModalResult = mrCancel);
End;

{-------------------------------------}

Procedure TPSCPopupWindow.SetPopupComponent(Value: TComponent);
Var
  PopupComponent: TComponent;
Begin
  If FPopupComponent <> Value Then
    Begin
      If FPopupComponent <> Nil Then
        Begin
          TPopupHooks.UnhookPopupWindow(Self);
          FHook := Nil;
          PopupComponent := FPopupComponent;
          FPopupComponent := Nil;
          RemoveFreeNotification(PopupComponent)
        End;
      If Value <> Nil Then
        Begin
          FPopupComponent := Value;
          FreeNotification(Value);
          FHook := TPopupHooks.HookPopupWindow(Self)
        End
    End
End;

{-------------------------------------}

Procedure TPSCPopupWindow.SetBorderStyle(Value: TPSCPopupBorderStyle);
Begin
  If FBorderStyle <> Value Then
    Begin
      FBorderStyle := Value;
      RecreateWnd
    End
End;

{-------------------------------------}

Function TPSCPopupWindow.GetDragBar: TControl;
Begin
  If (FDragBar = Nil) And Not (csDesigning In ComponentState) Then
    Begin
      FDragBar := TPSCDragBar.Create(Self);
      FDragBar.Parent := Self;
      TPSCDragBar(FDragBar).OnMouseUp := DoDragBarMouseUp;
      UpdateDragBar
    End;
  Result := FDragBar
End;

{-------------------------------------}

Procedure TPSCPopupWindow.UpdateDragBar;
Begin
  If FDragBar <> Nil Then
    FDragBar.Visible := FBorderStyle In [pbsDragBar,pbsSizeDragBar]
End;

{-------------------------------------}

Procedure TPSCPopupWindow.DoDragBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
Begin
  If (TPSCDragBar(FDragBar).Dragging = dsStopDragging) And
    ((Top <> StartDragRect.Top) Or (Left <> StartDragRect.Left)) Then
    If Assigned(FOnDetached) Then
      FOnDetached(Sender)
End;

{-------------------------------------}

Procedure TPSCPopupWindow.WMNCActivate(Var Message: TWMNCActivate);
Begin
  Message.Result := 1
End;

{-------------------------------------}

Procedure TPSCPopupWindow.WMActivate(Var Message: TWMActivate);
Begin
  Inherited;
  If (csDestroyingHandle In ControlState) Then
    Exit;
  PostMessage(Handle,CM_ACTIVECHANGED,0,0);
End;

{-------------------------------------}

Procedure TPSCPopupWindow.CMActiveChanged(Var Message: TMessage);
Begin
  If Active Then
    Begin
      If Assigned(FOnActivatePopup) Then
        FOnActivatePopup(Self,0)
    End
  Else
    Begin
      If FHook <> Nil Then
        With TPopupHook(FHook) Do
          If FForm.HandleAllocated And FForm.Visible Then
            WindowProc(Message);
      If Assigned(FOnDeactivatePopup) Then
        FOnDeactivatePopup(Self,Message.LParam);
      If Not (BorderStyle In [pbsToolWindow,pbsSizeToolWin]) And FAutoHide Then
        Hide
    End
End;

{-------------------------------------}

Procedure TPSCPopupWindow.WMWindowPosChanging(Var Message: TWMWindowPosChanging)
  ;
Begin
  Inherited;
  If (Message.WindowPos.flags And SWP_HIDEWINDOW <> 0) And
    Not (BorderStyle In [pbsToolWindow,pbsSizeToolWin]) Then
    Visible := false
End;

{-------------------------------------}

Procedure TPSCPopupWindow.WMSysCommand(Var Message: TWMSysCommand);
Begin
  If Message.CmdType And $FFF0 = SC_KEYMENU Then
    Message.Result := 0
  Else
    Inherited
End;

{-------------------------------------}

Procedure TPSCPopupWindow.CMDialogKey(Var Message: TCMDialogKey);
Begin
  Inherited;
  Case Message.CharCode Of
    VK_ESCAPE:
      If Not (BorderStyle In [pbsToolWindow,pbsSizeToolWin]) Then
        Begin
          ModalResult := mrCancel;
          SendCancelMode(Self);
          Hide;
          Message.Result := 1
        End
  End
End;

{-------------------------------------}

Procedure TPSCPopupWindow.Popup(X,Y: Integer);
Begin
  SetBounds(X,Y,Width,Height);
  ModalResult := mrNone;
  Show
End;

{-------------------------------------}

Procedure TPSCPopupWindow.Popup(Control: TControl; Alignment: TAlignment);
Var
  Bounds,ControlRect: TRect;
Begin
  PopupComponent := Control;
  HandleNeeded;
  Bounds := BoundsRect;
  ControlRect := Control.BoundsRect;
  MapWindowPoints(Control.Parent.Handle,0,ControlRect,2);
  PSCCalcPopupRect(ControlRect,Alignment,Bounds);
  Popup(Bounds.Left,Bounds.Top);
End;

{-------------------------------------}

Procedure PSCCalcPopupRect(Const ControlRect: TRect;
  Alignment: TAlignment; Var Bounds: TRect);
Var
  BoundsWidth,BoundsHeight,ScreenWidth,ScreenHeight: Integer;
Begin
  ScreenWidth := Screen.Width;
  ScreenHeight := Screen.Height;
  BoundsWidth := Bounds.Right - Bounds.Left;
  BoundsHeight := Bounds.Bottom - Bounds.Top;
  With ControlRect Do
    Begin
      If Bottom + BoundsHeight <= ScreenHeight Then
        Begin
          Bounds.Top := Bottom;
          Bounds.Bottom := Bottom + BoundsHeight
        End
      Else
        If Top - BoundsHeight >= 0 Then
          Begin
            Bounds.Top := Top - BoundsHeight;
            Bounds.Bottom := Top
          End
        Else
          Begin
            If Top + BoundsHeight <= ScreenHeight Then
              Begin
                Bounds.Top := Top;
                Bounds.Bottom := Top + BoundsHeight
              End
            Else
              If Bottom - BoundsHeight >= 0 Then
                Begin
                  Bounds.Top := Bottom - BoundsHeight;
                  Bounds.Bottom := Bottom
                End
              Else
                Begin
                  Bounds.Top := ScreenHeight - BoundsHeight;
                  If Bounds.Top < 0 Then
                    Bounds.Top := 0;
                  Bounds.Bottom := Bounds.Top + BoundsHeight
                End;
            If (Right + BoundsWidth <= ScreenWidth) Or (Left - BoundsWidth < 0)
              Then
              Begin
                Bounds.Left := Right;
                Bounds.Right := Right + BoundsWidth
              End
            Else
              Begin
                Bounds.Left := Left - BoundsWidth;
                Bounds.Right := Left
              End;
            Exit
          End;
      Case Alignment Of
        taLeftJustify:
          Begin
            Bounds.Left := Left;
            Bounds.Right := Left + BoundsWidth;
          End;
        taRightJustify:
          Begin
            Bounds.Left := Right - BoundsWidth;
            Bounds.Right := Right;
          End;
        taCenter:
          Begin
            Bounds.Left := (Left + Right - BoundsWidth) Div 2;
            Bounds.Right := Bounds.Left + BoundsWidth
          End
      End
    End;
  With Bounds Do
    If Left < 0 Then
      Begin
        Right := Right - Left;
        Left := 0
      End
    Else
      If Right > ScreenWidth Then
        Begin
          Left := Left + ScreenWidth - Right;
          Right := ScreenWidth
        End
End;

{-------------------------------------}

Function GetRGBColor(Value: TPSCColor): Cardinal;
Begin
  Result := PSCColorToRGB(Value);
  Case Result Of
    clPSCNone: Result := CLR_NONE;
    clPSCDefault: Result := CLR_DEFAULT
  End
End;

{-------------------------------------}

Procedure PSCDrawImageList(ACanvas: TPSCCanvas; X,Y: Integer;
  ImageList: TPSCImageList; Index: Integer; Enabled: Boolean;
  DrawingStyle: TDrawingStyle);
Const
  ROP_DSPDxax = $00E20746;
  DrawingStyles: Array[TDrawingStyle] Of Longint = (ILD_FOCUS,ILD_SELECTED,
    ILD_NORMAL,ILD_TRANSPARENT);
Var
  R: TRect;
  DestDC,SrcDC: HDC;
  MonoBitmap: TPSCBitmap;

  Procedure DrawBitBlt(Pass: Boolean);
  Var
    disp: Integer;
    color: TPSCColor;
  Begin
    If Pass Then
      Begin
        disp := 1;
        color := clPSCBtnHighlight
      End
    Else
      Begin
        disp := 0;
        color := clPSCBtnShadow
      End;
    ACanvas.Brush.Color := color;
    DestDC := ACanvas.Handle;
    winapi.Windows.SetTextColor(DestDC,clPSCWhite);
    winapi.Windows.SetBkColor(DestDC,clPSCBlack);
    BitBlt(DestDC,X + disp,Y + disp,ImageList.Width,ImageList.Height,SrcDC,
      0,0,ROP_DSPDxax);
  End;

Begin
  If Not ImageList.HandleAllocated Then
    Exit;
  If Enabled Then
    Begin
      With ImageList Do
        ImageList_DrawEx(Handle,Index,ACanvas.Handle,X,Y,0,0,
          GetRGBColor(BkColor),GetRGBColor(BlendColor),
          DrawingStyles[DrawingStyle]);
      exit
    End;
  MonoBitmap := TPSCBitmap.Create;
  With MonoBitmap Do
    Begin
      Monochrome := True;
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clPSCWhite;
      Canvas.FillRect(PSCRect(0,0,ImageList.Width,ImageList.Height))
    End;
  ImageList_DrawEx(ImageList.Handle,Index,MonoBitmap.Canvas.Handle,0,0,
    0,0,CLR_DEFAULT,0,ILD_NORMAL);
  R := PSCRect(X,Y,X + ImageList.Width,Y + ImageList.Height);
  SrcDC := MonoBitmap.Canvas.Handle;
  DrawBitBlt(true);
  DrawBitBlt(false)
End;

{-------------------------------------}

Type
  TWrapControl = Class(TControl);

Var
  Glyphs: TPSCImageList = Nil;

Procedure DrawGlyph(Canvas: TPSCCanvas; X,Y: Integer; Style: TPSCColorButtonStyle;
  Enabled: Boolean);
Var
  Index: integer;
Begin
  If Glyphs = Nil Then
    Begin
      Glyphs := TPSCImageList.Create(Nil);
      Glyphs.Width := 18;
      Glyphs.Height := 18;
      PSCAddBitmapFromResource(Glyphs,SPSCResName_Btn_COLOR_BRUSH);
      PSCAddBitmapFromResource(Glyphs,SPSCResName_Btn_COLOR_FILL);
      PSCAddBitmapFromResource(Glyphs,SPSCResName_Btn_COLOR_ALPHA);
    End;
  Case Style Of
    bstBrush: Index := 0;
    bstBkGround: Index := 1;
    bstFont: Index := 2;
  Else
    Index := -1;
  End;
  PSCDrawImageList(Canvas,X,Y,Glyphs,Index,Enabled,dsTransparent)
End;

{--------------------------------------}

Procedure TPSCColorButton.SetVersion(const AValue:String);
begin
end;

{--------------------------------------}

Constructor TPSCColorButton.Create(AOwner: TComponent);
Begin
  FVersion:=SPSCProductVerNo;
  Inherited Create(AOwner);
  ControlStyle := [csCaptureMouse];
  ParentColor := true;
  ShowHint := true;
  SetBounds(Left,Top,36,22);
  ButtonStyle := bstFont;
  FImageActive := -1;
  FImageInactive := -1;
  FImagePressed := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FStyle := cbsWordFont;
  FFlat := true;
  FExclusiveColor := clPSCBtnHighlight
End;

{-------------------------------------}

Procedure TPSCColorButton.Loaded;
Begin
  Inherited Loaded;
  UpdateSelectedColor
End;

{-------------------------------------}

Function TPSCColorButton.GetPopup: TPSCColorBoxPopup;
Begin
  If FPopup = Nil Then
    Begin
      FPopup := TPSCColorBoxPopup.CreateNew(Application,0);
      With FPopup Do
        Begin
          FreeNotification(Self);
          OnDetached := DoDetachPopup;
          OnDeactivatePopup := PopupDeactivate;
          If Self.FColorBox <> Nil Then
            FPopup.ColorBox := Self.FColorBox;
          With FPopup.ColorBox Do
            Begin
              SelectSameColors := false;
              SelectedColor := Self.SelectedColor;
              If Self.FColorBox = Nil Then
                Begin
                  Align := alNone;
                  If FOptions = [] Then
                    Style := Self.FStyle
                  Else
                    SectionsSet := FOptions;
                  HighlightActive := Self.FHighlightActive;
                  HighlightColor := Self.FHighlightColor;
                  FPopup.ClientWidth := ColumnCount * SlotWidth;
                  Align := alClient;
                End
              Else
                FPopup.ClientWidth := Width
            End;
          LinkEvents(FPopup.ColorBox)
        End
    End;
  Result := FPopup
End;

{-------------------------------------}

type
  TPSCArrowKind = (akNone,akUp,akDown,akLeft,akRight);

Procedure PSCDrawArrow(ADC: HDC; AArrowKind: TPSCArrowKind;
  Const ARect: TRect; AEnabled: Boolean);
Const
  Kind: Array[TPSCArrowKind] Of Integer = (0,DFCS_SCROLLUP,DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT,DFCS_SCROLLRIGHT);
  KindByEnabled: Array[Boolean] Of Integer = (DFCS_INACTIVE,0);
Var
  Bitmap: TPSCBitmap;
  Cnv: TPSCCanvas;
  IWidth,IHeight: Integer;
Begin
  If AArrowKind = akNone Then
    Exit;
  IWidth := ARect.Right - ARect.Left - 2;
  IHeight := ARect.Bottom - ARect.Top - 2;
  Bitmap := TPSCBitmap.Create;
  With Bitmap Do
  Try
    Width := IWidth;
    Height := IHeight;
    DrawFrameControl(Canvas.Handle,PSCRect(-1, -1,IWidth + 1,IHeight + 1),
      DFC_SCROLL,Kind[AArrowKind] Or KindByEnabled[AEnabled] Or
      DFCS_ADJUSTRECT Or DFCS_FLAT Or $800);
    Transparent := True;
    TransparentColor := clPSCBtnFace;
    Cnv := TPSCCanvas.Create;
    Try
      Cnv.Handle := ADC;
      Cnv.Draw(ARect.Left + 1,ARect.Top + 1,Bitmap);
    Finally
      Cnv.Free;
    End;
  Finally
    Bitmap.Free
  End;
End;

{-------------------------------------}

Procedure TPSCColorButton.Paint;
Var
  R1,R2: TRect;
  S1,S2: TPSCButtonStates;
  S0: TPSCColorButtonState;
  D1,D2,X,Y: integer;
  ImgIndex: integer;

  Procedure DrawBulkyButton(BgColor,FgColor: TPSCColor);
  Var
    rr1,rr2: TRect;
  Begin
    rr1 := R1;
    rr2 := R2;
    With Canvas Do
      Begin
        If (ButtonState_Down In S1) And (ButtonState_Down In S2) Then
          Begin
            DrawEdge(Handle,rr1,EDGE_SUNKEN,BF_TOPLEFT Or BF_BOTTOM Or
              BF_ADJUST);
            DrawEdge(Handle,rr2,EDGE_SUNKEN,BF_TOPRIGHT Or BF_BOTTOM Or
              BF_ADJUST);
            DrawEdge(Handle,rr2,EDGE_BUMP,BF_LEFT Or BF_ADJUST);
          End
        Else
          If (ButtonState_Down In S1) And (ButtonState_Up In S2) Then
            Begin
              DrawEdge(Handle,rr1,EDGE_SUNKEN,BF_TOPLEFT Or BF_BOTTOM Or
                BF_ADJUST);
              DrawEdge(Handle,rr2,EDGE_RAISED,BF_TOPRIGHT Or BF_BOTTOM Or
                BF_ADJUST);
              DrawEdge(Handle,rr2,EDGE_RAISED,BF_LEFT Or BF_ADJUST);
            End
          Else
            If (ButtonState_Up In S1) And (ButtonState_Down In S2) Then
              Begin
                DrawEdge(Handle,rr1,EDGE_RAISED,BF_TOPLEFT Or BF_BOTTOM Or
                  BF_ADJUST);
                DrawEdge(Handle,rr2,EDGE_ETCHED,BF_RECT Or BF_ADJUST)
              End
            Else
              Begin
                DrawEdge(Handle,rr1,EDGE_RAISED,BF_TOPLEFT Or BF_BOTTOM Or
                  BF_ADJUST);
                DrawEdge(Handle,rr2,BDR_SUNKENOUTER Or BDR_RAISEDINNER,BF_LEFT
                  Or
                  BF_ADJUST);
                DrawEdge(Handle,rr2,EDGE_RAISED,BF_TOPRIGHT Or BF_BOTTOM Or
                  BF_ADJUST);
              End;
        With Brush Do
          Begin
            If S0 = cbsExclusive Then
              Bitmap := PSCAllocPatternBitmap(BgColor,FgColor)
            Else
              Begin
                Color := BgColor;
                Style := BrushStyle_Solid;
              End;
            FillRect(rr1);
            Color := BgColor;
            Style := BrushStyle_Solid;
            FillRect(rr2);
          End;
      End;
  End;

Begin
  R1 := ClientRect;
  R2 := R1;
  If (R1.Right - R1.Left < 4) Or (R1.Bottom - R1.Top < 4) Then
    exit;
  R1.Right := (R1.Right Div 3) * 2 - 1;
  R2.Left := R1.Right;
  D1 := 0;
  D2 := 0;
  If ([csDesigning,csLoading] * ComponentState <> []) And Not Down Then
    S0 := cbsUp
  Else
    S0 := FBtnState;
  Case S0 Of
    cbsNone:
      Begin
        S1 := [];
        S2 := [];
        ImgIndex := FImageInactive;
      End;
    cbsUp:
      Begin
        S1 := [ButtonState_Up];
        S2 := S1;
        ImgIndex := FImageActive;
      End;
    cbsDown:
      Begin
        S1 := [ButtonState_Down];
        S2 := [ButtonState_Up];
        D1 := 1;
        ImgIndex := FImagePressed;
      End;
    cbsArrowDown:
      Begin
        S1 := [ButtonState_Up];
        S2 := [ButtonState_Down];
        D2 := 1;
        ImgIndex := FImageActive;
      End;
    cbsBtnDown:
      Begin
        S1 := [ButtonState_Down];
        S2 := S1;
        D1 := 1;
        D2 := 1;
        ImgIndex := FImagePressed;
      End;
  Else
    Begin
      S1 := [ButtonState_Exclusive,ButtonState_Down];
      S2 := [];
      D1 := 1;
      ImgIndex := FImagePressed;
    End;
  End;
  If FFlat Then
    Begin
      If S0 = cbsExclusive Then
        PSCDrawButton(Canvas,R1,TWrapControl(Parent)
          .Color,FExclusiveColor,S1)
      Else
        PSCDrawButton(Canvas,R1,clPSCNone,clPSCNone,S1);
      PSCDrawButton(Canvas,R2,clPSCNone,clPSCNone,S2);
    End
  Else
    Begin
      If S1 = [] Then
        S1 := [ButtonState_Up];
      If S2 = [] Then
        S2 := [ButtonState_Up];
      If S0 = cbsExclusive Then
        DrawBulkyButton(TWrapControl(Parent).Color,FExclusiveColor)
      Else
        DrawBulkyButton(TWrapControl(Parent).Color,TWrapControl(Parent)
          .Color)
    End;
  InflateRect(R1, -2, -2);
  OffsetRect(R1,1,1);
  If Assigned(FOnDrawButton) Then
    FOnDrawButton(Self,Canvas,false)
  Else
    Begin
      If FImages <> Nil Then
        Begin
          X := (R1.Right + R1.Left - FImages.Width) Div 2 + D1;
          Y := (R1.Bottom + R1.Top - FImages.Height) Div 2 + D1;
          PSCDrawImageList(Canvas,X,Y,FImages,ImgIndex,Enabled,dsTransparent)
        End
      Else
        Begin
          X := (R1.Right + R1.Left - 18) Div 2 + D1;
          Y := (R1.Bottom + R1.Top - 18) Div 2 + D1;
          DrawGlyph(Canvas,X,Y,ButtonStyle,Enabled)
        End;
      If Enabled Then
        With Canvas Do
          Begin
            R1 := Rect(1 + X,13 + Y,17 + X,17 + Y);
            If R1.Bottom > ClientRect.Bottom - 1 Then
              R1.Bottom := ClientRect.Bottom - 1;
            If FSelectedColor = clPSCDefault Then
              Brush.Color := clPSCEmptySlotColor
            Else
              Brush.Color := FSelectedColor;
            FillRect(R1);
            If GetRGBColor(FSelectedColor) = GetRGBColor(Self.Color) Then
              Begin
                If GetRGBColor(Self.Color) = GetRGBColor(clPSCGray) Then
                  Brush.Color := clPSCWhite
                Else
                  Brush.Color := clPSCGray;
                FrameRect(R1)
              End
          End
    End;
  InflateRect(R2,0, -2);
  OffsetRect(R2,D2,D2 + 1);
  PSCDrawArrow(Canvas.Handle,akDown,R2,Enabled)
End;

{-------------------------------------}

Procedure TPSCColorButton.UpdateColor;
Var
  R1: TRect;
  D1,X,Y: integer;
Begin
  R1 := ClientRect;
  If Not Enabled Or (R1.Right - R1.Left < 4) Or (R1.Bottom - R1.Top < 4) Then
    exit;
  R1.Right := (R1.Right Div 3) * 2 - 1;
  If Not (csDesigning In ComponentState) And (FBtnState = cbsDown) Then
    D1 := 1
  Else
    D1 := 0;
  InflateRect(R1, -2, -2);
  OffsetRect(R1,1,1);
  If Assigned(FOnDrawButton) Then
    FOnDrawButton(Self,Canvas,true)
  Else
    Begin
      If FImages <> Nil Then
        Begin
          X := (R1.Right + R1.Left - FImages.Width) Div 2 + D1;
          Y := (R1.Bottom + R1.Top - FImages.Height) Div 2 + D1;
        End
      Else
        Begin
          X := (R1.Right + R1.Left - 18) Div 2 + D1;
          Y := (R1.Bottom + R1.Top - 18) Div 2 + D1;
        End;
      With Canvas Do
        Begin
          R1 := Rect(1 + X,13 + Y,17 + X,17 + Y);
          If R1.Bottom > ClientRect.Bottom - 1 Then
            R1.Bottom := ClientRect.Bottom - 1;
          If FSelectedColor = clPSCDefault Then
            Brush.Color := clPSCEmptySlotColor
          Else
            Brush.Color := FSelectedColor;
          FillRect(R1);
          If GetRGBColor(FSelectedColor) = GetRGBColor(Self.Color) Then
            Begin
              If GetRGBColor(Self.Color) = GetRGBColor(clPSCGray) Then
                Brush.Color := clPSCWhite
              Else
                Brush.Color := clPSCGray;
              FrameRect(R1)
            End
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetState(Value: TPSCColorButtonState);
Var
  R: TRect;
Begin
  If Value <> FBtnState Then
    Begin
      FBtnState := Value;
      If (Value In [cbsDown,cbsArrowDown]) And (Parent <> Nil) And
        Parent.HandleAllocated Then
        Begin
          R := BoundsRect;
          Case Value Of
            cbsDown:
              R.Right := R.Left + ((R.Right - R.Left) Div 3) * 2 - 1;
            cbsArrowDown:
              R.Left := R.Left + ((R.Right - R.Left) Div 3) * 2 - 1;
          End;
          InvalidateRect(Parent.Handle,@R,true);
        End
      Else
        Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetButtonStyle(Value: TPSCColorButtonStyle);
Begin
  If Value <> FBtnStyle Then
    Begin
      FBtnStyle := Value;
      If FPopup <> Nil Then
        Popup.Hide;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.CMHintShow(Var Message: TCMHintShow);
Var
  str: String;
Begin
  str := PSCColorsManager.ColorToString(FSelectedColor,ckHexTriple);
  If str <> '' Then
    With TCMHintShow(Message).HintInfo^ Do
      If HintStr = '' Then
        HintStr := str
      Else
        HintStr := PSCFormat('%s (%s)', [HintStr,str]);//don't resource
  Inherited;
  If Popup.Showing Then
    Message.Result := 1
End;

{-------------------------------------}

Procedure TPSCColorButton.CMMouseEnter(Var Message: TMessage);
Begin
  Inherited;
  FMouseInside := true;
  If Enabled Then
    Begin
      Case State Of
        cbsNone:
          Begin
            FBtnState := cbsUp;
            If FFlat Then
              Repaint
          End;
        cbsUp:
          Begin
            If FPressed Then
              FBtnState := cbsDown;
            If FFlat Then
              Repaint
          End;
        cbsExclusive:
          Begin
            FBtnState := cbsDown;
            If FFlat Then
              Repaint
          End;
        cbsDown:
          If FFlat Then
            Repaint
      End;
      MovePopup
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.CMMouseLeave(Var Message: TMessage);
Begin
  Inherited;
  FMouseInside := false;
  Case State Of
    cbsDown:
      If FExclusive Then
        State := cbsExclusive
      Else
        If FPressed Then
          State := cbsUp;
    cbsUp:
      If FExclusive Then
        State := cbsExclusive
      Else
        Begin
          FBtnState := cbsNone;
          If FFlat Then
            Invalidate
        End
  End
End;

{-------------------------------------}

Procedure TPSCColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Var
  W: integer;
Begin
  Inherited MouseDown(Button,Shift,X,Y);
  If (Button = mbLeft) And Enabled Then
    Begin
      W := (ClientWidth Div 3) * 2;
      Case State Of
        cbsUp:
          If X < W Then
            Begin
              FPressed := true;
              State := cbsDown;
            End
          Else
            Begin
              FPressed := true;
              State := cbsArrowDown;
              ShowPopup
            End;
        cbsBtnDown:
          Begin
            FPressed := false;
            State := cbsDown
          End;
        cbsArrowDown:
          Begin
            FPressed := false;
            State := cbsUp
          End;
        cbsDown:
          If Not FPressed Then
            Begin
              If X > W Then
                Begin
                  FPressed := true;
                  State := cbsArrowDown;
                  ShowPopup
                End
              Else
                State := cbsUp
            End
      End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.MouseMove(Shift: TShiftState; X,Y: Integer);
Begin
  Inherited MouseMove(Shift,X,Y)
End;

{-------------------------------------}

Procedure TPSCColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X,Y: Integer);
Begin
  Inherited MouseUp(Button,Shift,X,Y);
  Case State Of
    cbsDown:
      If FPressed Then
        Begin
          Click;
          If HasDownState Then
            FExclusive := true
          Else
            State := cbsUp
        End;
    cbsUp:
      Begin
        FExclusive := false;
        If Not FMouseInside Then
          State := cbsNone
      End
  End;
  FPressed := false
End;

{-------------------------------------}

Procedure TPSCColorButton.ShowPopup;
Var
  P: TPoint;
Begin
  If FPopupShowing Then
    exit;
  If FBtnStyle = bstCustom Then
    Begin
      If Assigned(PopupMenu) Then
        Begin
          P := Parent.ClientToScreen(Point(Left,Top + Height));
          PopupMenu.Popup(P.x,P.y);
          UpdateTracking;
          If Not FMouseInside Then
            State := cbsNone
        End
    End
  Else
    Begin
      DoBeforeShowPopup(Popup);
      Popup.Popup(Self,FPopupAlignment)
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetSelectedColor(Value: TPSCColor);
Begin
  If Value <> FSelectedColor Then
    Begin
      FSelectedColor := Value;
      ColorBoxColor := Value;
      UpdateColor;
      If Assigned(FOnSelected) Then
        FOnSelected(Self,Value)
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.ColorSelected(ColorBox: TObject;
  Slot: TPSCColorSlot);
Var
  I,J: Integer;
  AColorBox: TPSCCustomColorBox;
Begin
  If cbsSelectedUpdating In FState Then
    Exit;
  State := cbsNone; //Added in V1.6
  If ColorBox = Self.ColorBox Then
    Begin
      If FDetachedPopup <> Nil Then
        AColorBox := FDetachedPopup.ColorBox
      Else
        AColorBox := Nil;
      ActiveSlot := Slot;
      If Slot <> Nil Then
        SelectedColor := Slot.Color;
      Popup.Hide
    End
  Else
    AColorBox := Self.ColorBox;
  If Not (cbsPopupUpdating In FState) And (AColorBox <> Nil) Then
    Begin
      FState := FState + [cbsPopupUpdating];
      Try
        If Slot <> Nil Then
          Begin
            I := Slot.Index;
            J := TPSCCustomColorBox(ColorBox).IndexOfSlots(Slot.Section);
            If J < AColorBox.SectionsCount Then
              With AColorBox.Sections[J] Do
                If I < Count Then
                  Slot := TPSCColorSlot(Slots.Items[I])
          End;
        TPSCCustomColorBox(AColorBox).SelectedSlot := Slot
      Finally
        FState := FState - [cbsPopupUpdating]
      End;
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.MoreColorClick(ColorBox: TObject;
  Slot: TPSCColorSlot; Color: TPSCColor; Stage: TPSCButtonClickStage);
Begin
  Case Stage Of
    bcsBeforeExecute:
      Begin
        If FPopup <> Nil Then
          Popup.Hide;
        Parent.SetFocus
      End
  Else
    Begin
      ActiveSlot := Slot;
      SelectedColor := Color
    End
  End
End;

{-------------------------------------}

Procedure TPSCColorButton.Click;
Begin
  Inherited Click;
  If Assigned(FOnSelected) Then
    FOnSelected(Self,FSelectedColor)
End;

{-------------------------------------}

Procedure TPSCColorButton.UpdateTracking;
Var
  P: TPoint;
Begin
  GetCursorPos(P);
  If FindDragTarget(P,True) = Self Then
    Perform(CM_MOUSEENTER,0,0)
  Else
    Perform(CM_MOUSELEAVE,0,0)
End;

{-------------------------------------}

Procedure TPSCColorButton.PopupDeactivate(Sender: TObject; PrevWindow: HWND);
Begin
  If FPopupShowing Then
    Begin
      If FExclusive Then
        State := cbsDown
      Else
        State := cbsUp;
      UpdateTracking;
      FPopupShowing := false;
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.MovePopup;
Var
  i: integer;
  IsShown: Boolean;
Begin
  If Not FPopupShowing And Grouped And (Parent <> Nil) Then
    With Parent Do
      Begin
        IsShown := false;
        For i := 0 To ControlCount - 1 Do
          If (Controls[i] Is TPSCColorButton) And (Controls[i] <> Self) And
            (TPSCColorButton(Controls[i]).GroupIndex = Self.GroupIndex) Then
            With TPSCColorButton(Controls[i]) Do
              If Grouped And Popup.Showing Then
                Begin
                  IsShown := true;
                  Popup.Hide
                End;
        If IsShown Then
          Begin
            State := cbsArrowDown;
            ShowPopup
          End
      End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetImages(Value: TPSCImageList);
Begin
  If FImages <> Value Then
    Begin
      If FImages <> Nil Then
        RemoveFreeNotification(FImages);
      FImages := Value;
      If FImages <> Nil Then
        FreeNotification(FImages);
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.ImageListChange(Sender: TObject);
Begin
  Invalidate
End;

{-------------------------------------}

Procedure TPSCColorButton.SetImageActive(Value: Integer);
Begin
  If Value <> FImageActive Then
    Begin
      FImageActive := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetImageInactive(Value: Integer);
Begin
  If Value <> FImageInactive Then
    Begin
      FImageInactive := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetImagePressed(Value: Integer);
Begin
  If Value <> FImagePressed Then
    Begin
      FImagePressed := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetStyle(Value: TPSCColorBoxStyle);
Begin
  If FStyle <> Value Then
    Begin
      FStyle := Value;
      FOptions := [];
      If Not (csDesigning In ComponentState) And (FPopup <> Nil) Then
        Begin
          FPopup.Free;
          UpdatePopup
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetOptions(Value: TPSCColorBoxOptions);
Begin
  If FOptions <> Value Then
    Begin
      FOptions := Value;
      FStyle := cbsCustom;
      If Not (csDesigning In ComponentState) And (FPopup <> Nil) Then
        Begin
          FPopup.Free;
          UpdatePopup
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If Operation = opRemove Then
    Begin
      If AComponent = FImages Then
        FImages := Nil
      Else
        If AComponent = FPopup Then
          Begin
            FPopup := Nil;
            FPopupShowing := false
          End
        Else
          If AComponent = FColorBox Then
            Begin
              FColorBox := Nil;
              ActiveSlot := Nil;
              If (FPopup <> Nil) And Not (csDestroying In ComponentState) Then
                FPopup.Release
            End
          Else
            If FDetachedPopup = AComponent Then
              FDetachedPopup := Nil
    End
End;

{-------------------------------------}

Function TPSCColorButton.GetDown: Boolean;
Begin
  Result := FExclusive And FHasDownState
End;

{-------------------------------------}

Procedure TPSCColorButton.SetDown(Value: Boolean);
Begin
  If FHasDownState Then
    Begin
      FExclusive := Value;
      If Value Then
        Begin
          FPressed := false;
          State := cbsExclusive
        End
      Else
        Begin
          State := cbsUp;
          If [csDesigning,csLoading] * ComponentState = [] Then
            UpdateTracking
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetHasDownState(Value: Boolean);
Begin
  If FHasDownState <> Value Then
    Begin
      FHasDownState := Value;
      If Not Value Then
        Begin
          FExclusive := false;
          State := cbsUp;
          If [csDesigning,csLoading] * ComponentState = [] Then
            UpdateTracking
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetExclusiveColor(Value: TPSCColor);
Begin
  If FExclusiveColor <> Value Then
    Begin
      FExclusiveColor := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetFlat(Value: Boolean);
Begin
  If Value <> FFlat Then
    Begin
      FFlat := Value;
      Invalidate
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetHighlightActive(Value: Boolean);
Begin
  If FHighlightActive <> Value Then
    Begin
      FHighlightActive := Value;
      If FPopup <> Nil Then
        ColorBox.HighlightActive := Value
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.SetHighlightColor(Value: TPSCColor);
Begin
  If FHighlightColor <> Value Then
    Begin
      FHighlightColor := Value;
      If FPopup <> Nil Then
        ColorBox.HighlightColor := Value
    End
End;

{-------------------------------------}

Function TPSCColorButton.IsColorBoxStored: Boolean;
Begin
  Result := FColorBox <> Nil
End;

{-------------------------------------}

Function TPSCColorButton.GetColorBox: TPSCCustomColorBox;
Begin
  Result := Popup.ColorBox;
End;

{-------------------------------------}

Procedure TPSCColorButton.SetColorBox(Value: TPSCCustomColorBox);
Begin
  If FColorBox <> Value Then
    Begin
      If FColorBox <> Nil Then
        RemoveFreeNotification(FColorBox);
      FColorBox := Value;
      ActiveSlot := Nil;
      If Value <> Nil Then
        Begin
          FreeNotification(Value);
          If Not (csDesigning In Value.ComponentState) Then
            Value.Parent := Nil
        End
    End;
  If FPopup <> Nil Then
    Popup.Free;
  UpdateSelectedColor;
  UpdatePopup;
End;

{-------------------------------------}

Procedure TPSCColorButton.UpdateSelectedColor;
Var
  Tmp1,Tmp2: TPSCColor;
Begin
  Tmp1 := FSelectedColor;
  Tmp2 := FColorBoxColor;
  FSelectedColor := clPSCNone;
  SelectedColor := Tmp1;
  ColorBoxColor := Tmp2
End;

{-------------------------------------}

Procedure TPSCColorButton.SetActiveSlot(Value: TPSCColorSlot);
Begin
  If FActiveSlot <> Value Then
    Begin
      If FActiveSlot <> Nil Then
        FActiveSlot.State := FActiveSlot.State - [ButtonState_Up,ButtonState_Down];
      FActiveSlot := Value
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.DoDeleteSlot(ColorBox: TObject;
  Slot: TPSCColorSlot);
Begin
  If ActiveSlot = Slot Then
    ActiveSlot := Nil
End;

{-------------------------------------}

Procedure TPSCColorButton.DoActiveChanged(ColorBox: TObject;
  Slot: TPSCColorSlot);
Var
  FOldColor: TPSCColor;
Begin
  If FTrackColor Then
    Begin
      If Slot <> Nil Then
        Begin
          FOldColor := FSelectedColor;
          Try
            FSelectedColor := Slot.Color;
            UpdateColor
          Finally
            FSelectedColor := FOldColor
          End
        End
      Else
        UpdateColor;
      If (ActiveSlot <> Nil) And (Slot <> ActiveSlot) Then
        With ActiveSlot Do
          If Section Is TPSCColorSection Then
            State := State - [ButtonState_Up] + [ButtonState_Down]
          Else
            State := State - [ButtonState_Up,ButtonState_Down]
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.DoSlotChanged(ColorBox: TObject;
  Slot: TPSCColorSlot);
Var
  AColorBox: TPSCCustomColorBox;
  I,J: Integer;
Begin
  If Not (cbsPopupUpdating In FState) And (FDetachedPopup <> Nil) Then
    Begin
      If ColorBox = Self.ColorBox Then
        AColorBox := FDetachedPopup.ColorBox
      Else
        AColorBox := Self.ColorBox;
      FState := FState + [cbsPopupUpdating];
      Try
        AColorBox.Enabled := TPSCCustomColorBox(ColorBox).Enabled;
        If Slot <> Nil Then
          Begin
            I := Slot.Index;
            J := TPSCCustomColorBox(ColorBox).IndexOfSlots(Slot.Section);
            If J < AColorBox.SectionsCount Then
              With AColorBox.Sections[J] Do
                If I < Count Then
                  TPSCColorSlot(Slots.Items[I]).Color := Slot.Color
          End
      Finally
        FState := FState - [cbsPopupUpdating];
      End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.DoDetachPopup(Sender: TObject);
Begin
  If FDetachedPopup = Nil Then
    Begin
      FDetachedPopup := TPSCColorBoxPopup.CreateNew(Application,0);
      FreeNotification(FDetachedPopup);
      With FDetachedPopup Do
        Begin
          With TComponentClass(Self.ColorBox.ClassType) Do
            ColorBox := Create(FDetachedPopup) As TPSCCustomColorBox;
          ColorBox.Parent := FDetachedPopup;
          PopupComponent := Self;
          If SizeablePopup Then
            BorderStyle := pbsSizeToolWin
          Else
            BorderStyle := pbsToolWindow;
          UpdatePopup;
          ClientWidth := Self.ColorBox.Width;
          Caption := Self.Caption
        End
    End;
  With FDetachedPopup Do
    Begin
      SetBounds(Self.Popup.Left,Self.Popup.Top,Width,Height);
      Show;
      ColorBox.ActiveSlot := Nil;
    End;
  Popup.Hide;
  FPopupShowing := false
End;

{-------------------------------------}

Procedure TPSCColorButton.DoBeforeShowPopup(Sender: TObject);
Const
  BorderStyles: Array[Boolean] Of TPSCPopupBorderStyle = (pbsPopup,pbsDragBar);
Var
  BorderStyle: TPSCPopupBorderStyle;
  Slot: TPSCColorSlot;
Begin
  BorderStyle := BorderStyles[DragPopup];
  If SizeablePopup Then
    BorderStyle := Succ(BorderStyle);
  Popup.BorderStyle := BorderStyle;
  Popup.Caption := FCaption;
  FPopupShowing := true;
  If FTrackColor Then
    With ColorBox Do
      Begin
        Slot := Self.ActiveSlot;
        Self.FState := Self.FState + [cbsSelectedUpdating];
        Try
          If FColorBoxColor <> FSelectedColor Then
            Begin
              SelectedColor := FColorBoxColor;
              Slot := SelectedSlot
            End;
          SelectedSlot := Nil
        Finally
          Self.FState := Self.FState - [cbsSelectedUpdating]
        End;
        Self.ActiveSlot := Slot;
        ActiveSlot := Slot;
        If Slot <> Nil Then
          Slot.State := Slot.State + [ButtonState_Up] - [ButtonState_Down]
      End
  Else
    If FColorBoxColor <> FSelectedColor Then
      Begin
        Self.FState := Self.FState + [cbsSelectedUpdating];
        Try
          ColorBox.SelectedColor := FColorBoxColor;
        Finally
          Self.FState := Self.FState - [cbsSelectedUpdating]
        End
      End;
  If FDetachedPopup <> Nil Then
    FDetachedPopup.Caption := Caption
End;

{-------------------------------------}

Procedure TPSCColorButton.DoFindMethod(Reader: TReader; Const MethodName:
  String;
  Var Address: Pointer; Var Error: Boolean);
Begin
  Address := Owner.MethodAddress(MethodName);
  Error := Address = Nil
End;

{-------------------------------------}

Procedure TPSCColorButton.UpdatePopup;
Var
  Stream: TStream;
  AColorBox: TPSCCustomColorBox;
Begin
  If FDetachedPopup <> Nil Then
    Begin
      With TComponentClass(ColorBox.ClassType) Do
        AColorBox := Create(FDetachedPopup) As TPSCCustomColorBox;
      FDetachedPopup.ColorBox := AColorBox;
      AColorBox.Parent := FDetachedPopup;
      FState := FState + [cbsPopupUpdating];
      Try
        FState := FState + [cbsSelectedUpdating];
        Try
          If Not (ActiveSlot Is TPSCButtonSlot) Then
            ColorBox.SelectedSlot := ActiveSlot
        Finally
          FState := FState - [cbsSelectedUpdating]
        End;
        While AColorBox.ControlCount > 0 Do
          AColorBox.Controls[AColorBox.ControlCount - 1].Free;
        Stream := TMemoryStream.Create;
        Try
          With TWriter.Create(Stream,4096) Do
          Try
            Root := ColorBox.Owner;
            WriteComponent(ColorBox)
          Finally
            Free
          End;
          Stream.Position := 0;
          With TReader.Create(Stream,4096) Do
          Try
            Root := FDetachedPopup;
            Owner := FDetachedPopup;
            OnFindMethod := DoFindMethod;
            BeginReferences;
            Try
              ReadComponent(AColorBox);
              FixupReferences
            Finally
              EndReferences
            End
          Finally
            Free
          End
        Finally
          Stream.Free
        End;
        LinkEvents(AColorBox)
      Finally
        FState := FState - [cbsPopupUpdating]
      End
    End
End;

{-------------------------------------}

Destructor TPSCColorButton.Destroy;
Begin
  FPopup.Free;
  FDetachedPopup.Free;
  FImageChangeLink.Free;
  Inherited Destroy
End;

{-------------------------------------}

Procedure TPSCColorButton.SetColorBoxColor(Value: TPSCColor);
Begin
  If FColorBoxColor <> Value Then
    Begin
      FColorBoxColor := Value;
      If Not (csDesigning In ComponentState) Then
        Begin
          FState := FState + [cbsSelectedUpdating];
          Try
            ColorBox.SelectedColor := Value;
            ActiveSlot := ColorBox.SelectedSlot;
            If FDetachedPopup <> Nil Then
              FDetachedPopup.ColorBox.SelectedColor := Value
          Finally
            FState := FState - [cbsSelectedUpdating]
          End
        End
    End
End;

{-------------------------------------}

Procedure TPSCColorButton.DoClick(Sender: TObject);
Begin
  If FPopup <> Nil Then
    Begin
      Popup.Hide;
      FPopupShowing := false;
      State := cbsNone; // Added in V2.0
    End;
  Click;
End;

{-------------------------------------}

Procedure TPSCColorButton.LinkEvents(ColorBox: TPSCCustomColorBox);
Begin
  With ColorBox Do
    Begin
      OnSelected := ColorSelected;
      OnButtonClick := MoreColorClick;
      OnActiveChange := Self.DoActiveChanged;
      OnSlotDeletion := Self.DoDeleteSlot;
      OnClick := Self.DoClick;
      OnChange := Self.DoSlotChanged
    End
End;

{-------------------------------------}
initialization
finalization
  Glyphs.Free;
  Glyphs:=nil;
end.
