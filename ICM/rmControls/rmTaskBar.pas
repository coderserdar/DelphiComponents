{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmTaskBar
Purpose  : To allow window control from a central location.  Also has support
           in it to fix the M$ MDI window bugs.
Date     : 12-01-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmTaskBar;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, extctrls, menus;

type
  TUpdateWindowListEvent = procedure(Sender: TObject; Form: TForm; var AddToList: boolean) of object;
  TUpdatedWindowListEvent = procedure(Sender: TObject; Form: TForm) of object;
{$IFDEF rmDebug}
  TrmTestEvent = procedure(index: integer; msg: integer) of object;
{$ENDIF}
  TWinType = (wtMDIChild, wtDialog, wtToolWin);
  TWinTypes = set of TWinType;

  TrmTaskBar = class(TCustomControl)
  private
    { Private declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;

    OldMDIWndProc: TFarProc;
    NewMDIWndProc: Pointer;

    OldApplicationWndProc: TFarProc;
    NewApplicationWndProc: Pointer;

    FHint: string;
    FTimer: TTimer;
    fDelay: integer;
    FColor: TColor;
    fBufferBMP: TBitmap;
    FIconBMP: TBitmap;
    FLabelBMP: TBitmap;
    FWindowList: TList;
    FTempList: TList;
    fmenuWin: TForm;
    fAutoHide: boolean;
    fAutoMinimize: boolean;
    fLastActiveForm: TForm;
    fLastActiveMDIChild: TForm;
    fExcludeWinTypes: TWinTypes;
    fMinBtnSize: integer;
    fBtnSpace: integer;
    fMaxBtnSize: integer;
    fBtnHeight: integer;
    fLeftMargin: integer;
    fRightMargin: integer;
    FTaskHint: THintWindow;
    fButtons: array of TRect;
    fOnAddingWindow: TUpdateWindowListEvent;
    fOnWindowAdded: TUpdatedWindowListEvent;
    fOnWindowRemoved: TUpdatedWindowListEvent;
{$IFDEF rmDebug}
    fWinMessage: TrmTestEvent;
{$ENDIF}
    fMDIMenuRefresh: TNotifyEvent;
    fTopMargin: integer;
    fMainFormFocused: boolean;
    fFlat: boolean;

    procedure SetColor(const Value: TColor);
    procedure wmCommand(var msg: TMessage); message wm_command;
    procedure wmEraseBkgnd(var msg: TMessage); message WM_ERASEBKGND;
    procedure wmDestroy(var msg: TMessage); message wm_destroy;
    procedure SetAutoHide(const Value: boolean);
    procedure SetExcludes(const Value: TWinTypes);
    procedure SetBtnHeight(const Value: integer);
    procedure SetBtnSpace(const Value: integer);
    procedure SetMaxBtnSize(const Value: integer);
    procedure SetMinBtnSize(const Value: integer);
    procedure SetLeftMargin(const Value: integer);
    procedure SetRightMargin(const Value: integer);
    procedure SetTopMargin(const Value: integer);
    procedure CMMouseLeave(var msg: TMessage); message cm_MouseLeave;

    procedure HookWin;
    procedure UnhookWin;

    procedure HookMDIWin;
    procedure UnhookMDIWin;

    function GetActiveForm: TForm;
    function GetWindowCount: integer;
    function GetWindowItem(index: integer): TForm;
    function GetMDIChild(index: integer): TForm;
    function GetMDIChildCount: integer;
    function GetActiveMDIChild: TForm;
    procedure SetFlat(const Value: boolean);
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UnRegisterWindow(F: TForm);
    procedure RegisterWindow(F: TForm);
    procedure RegisterWindowTemp(F: TForm);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoTimer(Sender: TObject);
    procedure SetDelay(const Value: integer);
    procedure MinimizeWindowTypes(WinTypes: TWinTypes);

    procedure HookWndProc(var AMsg: TMessage);
    procedure HookMDIWndProc(var AMsg: TMessage);

    procedure DoDummyForm(ToggleForm: TForm);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure HideHint(ClearHint: Boolean);
    procedure MinimizeAllMDI;
    procedure MinimizeAll;
    property ActiveMDIChild: TForm read GetActiveMDIChild;
    property MDIChildren[index: integer]: TForm read GetMDIChild;
    property MDIChildCount: integer read GetMDIChildCount;
    property WindowCount: integer read GetWindowCount;
    property ActiveWindow: TForm read GetActiveForm;
    property WindowList[index: integer]: TForm read GetWindowItem;
{$IFDEF rmDebug}
    property OnWinMessage: TrmTestEvent read fWinMessage write fWinMessage;
{$ENDIF}
  published
    { Published declarations }
    property LeftMargin: integer read fLeftMargin write SetLeftMargin default 3;
    property RightMargin: integer read fRightMargin write SetRightMargin default 3;
    property TopMargin: integer read fTopMargin write SetTopMargin default 3;
    property BtnSpace: integer read fBtnSpace write SetBtnSpace default 3;
    property MaxBtnSize: integer read fMaxBtnSize write SetMaxBtnSize default 150;
    property MinBtnsize: integer read fMinBtnSize write SetMinBtnSize default 5;
    property BtnHeight: integer read fBtnHeight write SetBtnHeight default 23;
    property ParentFont;
    property Font;
    property HintDelay: integer read fDelay write SetDelay default 2500;
    property ExcludeWindowTypes: TWinTypes read fExcludeWinTypes write SetExcludes;
    property Color: TColor read FColor write SetColor default clbtnface;
    property Flat : boolean read fFlat write SetFlat default false;
    property AutoHideMDIChildren: boolean read fAutoHide write SetAutoHide default false;
    property AutoMinimize: boolean read fAutoMinimize write fAutoMinimize default false;
    property OnAddingWindow: TUpdateWindowListEvent read fOnAddingWindow write fOnAddingWindow;
    property OnWindowAdded: TUpdatedWindowListEvent read fOnWindowAdded write fOnWindowAdded;
    property OnWindowRemoved: TUpdatedWindowListEvent read fOnWindowRemoved write fOnWindowRemoved;
    property OnMDIMenuRefresh: TNotifyEvent read fMDIMenuRefresh write fMDIMenuRefresh;
  end;

implementation

{ TrmTaskBar }

constructor TrmTaskBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);

  ControlStyle := ControlStyle + [csAcceptsControls, csOpaque];

  NewWndProc := nil;
  OldWndProc := nil;

  NewMDIWndProc := nil;
  OldMDIWndProc := nil;

  OldApplicationWndProc := nil;
  NewApplicationWndProc := nil;

  align := alBottom;
  height := 28;
  fColor := clBtnFace;
  fAutoHide := false;
  fAutoMinimize := false;
  fBufferBMP := tbitmap.create;
  FIconBMP := TBitmap.create;
  FLabelBMP := TBitmap.create;
  FWindowList := TList.create;
  FTempList := TList.create;
  fLastActiveForm := nil;
  fLastActiveMDIChild := nil;
  fExcludeWinTypes := [];
  LeftMargin := 3;
  RightMargin := 3;
  TopMargin := 3;
  BtnSpace := 3;
  MaxBtnSize := 150;
  MinBtnsize := 5;
  BtnHeight := 23;
  fFlat := false;

  SetLength(fButtons, 0);
  fdelay := 2500;

  FTaskHint := THintWindow.create(self);
  FTaskHint.Color := clInfobk;
  FTaskHint.Canvas.Font.color := clInfoText;
  FTaskHint.Canvas.Pen.Color := clWindowFrame;

  FTimer := TTimer.Create(self);
  FTimer.OnTimer := DoTimer;

  fMainFormFocused := false;

  HookWin;
end;

destructor TrmTaskBar.Destroy;
begin
  SetLength(fButtons, 0);
  fBufferBMP.free;
  FIconBMP.free;
  FLabelBMP.free;
  FWindowList.free;
  FTempList.free;
  FTaskHint.free;
  FTimer.free;
  UnHookWin;
  inherited;
end;

procedure TrmTaskBar.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  HideHint(True);
  if Flat then
     Invalidate;
end;

procedure TrmTaskBar.DoTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  HideHint(false);
end;

procedure TrmTaskBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  loop, btncount: integer;
  found: boolean;
  f: tform;
  tempmenuhandle: HMENU;
  newpoint: tpoint;
begin
  fmenuwin := nil;
  btncount := high(fButtons);
  loop := 0;
  newpoint := point(x, y);
  found := false;

  while loop <= btncount do
  begin
    if ptinrect(fbuttons[loop], newpoint) then
    begin
      found := true;
      break;
    end;
    inc(loop);
  end;

  if (found) and (loop < fWindowList.count) then
  begin
    f := TForm(FWindowList[loop]);
    try
      if assigned(f) and isWindow(f.handle) then
      begin
        if (button = mbleft) then
        begin
          if (screen.ActiveForm <> application.mainform) then
          begin
            if (screen.ActiveForm = f) and (fLastActiveForm = f) and (f.WindowState <> wsminimized) then
            begin
              if fautominimize then
                f.WindowState := wsminimized;
            end
            else
            begin
              if f.windowstate = wsminimized then
                f.windowstate := wsNormal;
              f.bringtofront;
              f.setfocus;
              if fMainFormFocused then
              begin
                 fMainFormFocused := false;
                 invalidate;
              end;
            end;
          end
          else if (screen.activeform = application.mainform) and (fLastActiveMDIChild = f) then
          begin
            DoDummyForm(f);
          end
          else
          begin
            if (fLastActiveForm = f) and (f.WindowState <> wsminimized) then
            begin
              if fautominimize then
                f.WindowState := wsminimized;
            end
            else
            begin

              if f.windowstate = wsminimized then
                f.windowstate := wsNormal;

              f.bringtofront;

              if assigned(f.activecontrol) then
                f.activecontrol.SetFocus
              else
              begin
                for loop := 0 to f.ControlCount - 1 do
                begin
                  if f.Controls[loop] is TWincontrol then
                  begin
                    tWinControl(f.Controls[loop]).setfocus;
                    break;
                  end;
                end;
              end;
            end;
          end;
        end;
        if (button = mbright) then
        begin
          newpoint := clienttoscreen(newpoint);
          fmenuWin := f;
          tempmenuhandle := Getsystemmenu(f.handle, false);
          TrackPopupMenu(tempmenuhandle, tpm_leftalign or TPM_LEFTBUTTON, newpoint.x - 1, newpoint.y - 2, 0, self.handle, nil);
        end;
      end;
    except
      UnRegisterWindow(f);
    end;
  end;
end;

procedure TrmTaskBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  tw, th, loop, btncount: integer;
  found: boolean;
  f: tform;
  newpoint: tpoint;
  wrect: TRect;
  oldHint: string;
begin
  fmenuwin := nil;
  btncount := high(fButtons);
  loop := 0;
  newpoint := point(x, y);
  found := false;

  while loop <= btncount do
  begin
    if ptinrect(fbuttons[loop], newpoint) then
    begin
      found := true;
      if Flat then
         Invalidate;
      break;
    end;
    inc(loop);
  end;

  if (found) and (loop < fWindowList.count) then
  begin
    f := TForm(FWindowList[loop]);
    try
      if assigned(f) and isWindow(f.handle) then
      begin
        tw := FLabelBMP.Canvas.TextWidth(f.Caption);
        wrect := fbuttons[loop];
        oldhint := fhint;
        if tw > (((wrect.right - wrect.left) - 4) - 18) then
          fhint := f.caption
        else
          fhint := '';
        if oldhint <> fhint then
        begin
          if fhint <> '' then
          begin
            newpoint := ClientToScreen(point(wrect.Left, 0));
            tw := FTaskHint.Canvas.TextWidth(fhint);
            th := FTaskHint.Canvas.TextHeight(fhint);
            WRect := Rect(newpoint.x, newpoint.y - th - 1, newpoint.x + tw + 8, newpoint.y + 2);
            FTimer.Enabled := false;
            FTaskHint.Tag := loop;
            FTaskHint.ActivateHint(Wrect, fHint);
            FTimer.Interval := fdelay;
            FTimer.Enabled := true;
          end
          else
            HideHint(true);
        end;
      end;
    except
      UnRegisterWindow(f);
    end;
  end
  else
    HideHint(true);
end;

procedure TrmTaskBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TCustomForm then
  begin
    if Operation = opRemove then
      UnRegisterWindow(TForm(AComponent));

    if Operation = opInsert then
      RegisterWindowTemp(TForm(AComponent));
  end;
end;

procedure TrmTaskBar.Paint;
var
  wrect: TRect;
  btnsize: integer;
  btncount: integer;
  loop, xpos: integer;
  F: TForm;
  AddToList, BtnDown, updateList: boolean;
  wCaption: string;
  wIcon: TIcon;
  wPt : TPoint;
begin
  inherited;

  if fTempList.Count > 0 then
  begin
    loop := fTempList.Count;
    while loop > 0 do
    begin
      dec(loop);
      if (TObject(ftemplist[loop]) is tcustomform) then
      begin
        f := TForm(ftemplist[loop]);
        if f.HandleAllocated then
        begin
          ftemplist.Delete(loop);
          AddToList := true;
          if assigned(fOnAddingWindow) then
            fOnAddingWindow(self, f, AddtoList);
          if AddToList then RegisterWindow(f);
        end;
      end
      else
      begin
        ftemplist.Delete(loop);
      end;
    end;
  end;

  if (csdesigning in componentstate) then
    btncount := 2
  else
    btncount := FWindowList.Count;

  updateList := high(fButtons) <> btncount;
  if updatelist then
    setlength(fButtons, btncount);

  fBufferBMP.Height := clientheight;
  fBufferBMP.Width := clientwidth;
  fBufferBMP.Canvas.Brush.color := fColor;
  fbufferbmp.canvas.FillRect(ClientRect);
  btnsize := maxbtnsize;
  while LeftMargin + (btnsize * btncount) + (btnspace * btncount) + RightMargin > clientwidth do
    dec(btnsize, 1);
  if btnsize > maxbtnsize then btnsize := maxbtnsize;
  if btnsize < minbtnsize then btnsize := minbtnsize;
  loop := 0;
  xpos := LeftMargin;
  fLabelBMP.width := btnsize;
  flabelbmp.height := btnheight;
  while loop < btncount do
  begin
    if (csdesigning in componentstate) then
    begin
      case loop of
        0:
          begin
            wcaption := 'Button Up';
            BtnDown := false;
          end;
        1:
          begin
            wcaption := 'Button Down';
            BtnDown := true;
          end;
      else
        BtnDown := false;
      end;
    end
    else
    begin
      f := TForm(fwindowlist[loop]);

      if fAutoHide then
      begin
        if (f.windowstate = wsminimized) and (f.FormStyle = fsMDIChild) then
        begin
          showwindow(f.handle, sw_hide);
        end;

        if ((f.windowstate = wsnormal) or (f.Windowstate = wsmaximized)) and
          (f.FormStyle = fsMDIChild) and (not iswindowvisible(f.handle)) then
        begin
          showwindow(f.handle, sw_show);
        end;
      end;

      if not f.icon.Empty then
      begin
        FIconBMP.Height := f.Icon.height;
        FIconBMP.Width := f.Icon.width;
        FIconBMP.Canvas.brush.color := fcolor;
        fIconBmp.Canvas.FillRect(rect(0, 0, f.Icon.width, f.Icon.height));

        DrawIconEx(FIconBMP.Canvas.handle, 0, 0, f.Icon.handle, 16, 16, 0, 0, DI_NORMAL);

        fIconBMP.Transparent := true;
        FIconBMP.TransparentColor := fcolor;
      end
      else if (f.FormStyle = fsmdiChild) then
      begin
        FIconBMP.Height := 16;
        FIconBMP.Width := 16;
        FIconBMP.Canvas.brush.color := fcolor;
        fIconBmp.Canvas.FillRect(rect(0, 0, 16, 16));
        wIcon := TIcon.create;
        try
          wIcon.Handle := LoadIcon(hinstance, makeintresource(0));

          if wIcon.Handle = 0 then
            wIcon.Handle := LoadIcon(hinstance, 'MAINICON');

          DrawIconEx(FIconBMP.Canvas.handle, 0, 0, wIcon.handle, 16, 16, 0, 0, DI_NORMAL);
        finally
          wIcon.free;
        end;
        fIconBMP.Transparent := true;
        FIconBMP.TransparentColor := fcolor;
      end;
      wCaption := f.caption;
      if screen.activeForm <> Application.Mainform then
        BtnDown := (screen.ActiveForm = f) and (f.windowstate <> wsminimized)
      else
        BtnDown := assigned(fLastActiveForm) and (fLastActiveForm = f) and (fLastActiveForm.windowstate <> wsminimized) and not (fMainFormFocused);
    end;

    wrect := rect(0, 0, btnsize, btnheight);

    flabelbmp.Canvas.brush.color := fcolor;
    flabelbmp.canvas.font := font;
    flabelbmp.canvas.font.Color := clBtnText;
    flabelbmp.canvas.fillrect(wrect);
    if BtnDown then
    begin
      FLabelBMP.Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      FLabelBMP.Canvas.FillRect(wrect);
      inflaterect(wrect, -2, -2);
      flabelbmp.canvas.StretchDraw(rect(wrect.left + 2, wrect.top + 2, wrect.left + 18, wrect.top + 18), fIconBMP);
      inflaterect(wrect, 2, 2);
      if flat then
      begin
         frame3d(flabelbmp.canvas, wrect, cl3DDkShadow, clBtnHighlight, 1);
         inflateRect(wRect, 1, 1);
      end
      else
      begin
         frame3d(flabelbmp.canvas, wrect, cl3DDkShadow, clBtnHighlight, 1);
         frame3d(flabelbmp.canvas, wrect, clBtnShadow, cl3DLight, 1);
      end;
      wrect.left := wrect.left + 20;
      wrect.top := wrect.top + 1;
      wRect.right := wrect.right - 1;
      FLabelBMP.Canvas.Font.Style := FLabelBMP.Canvas.Font.Style + [fsBold];
      FLabelBMP.Canvas.Brush.Style := bsClear;
      DrawText(flabelbmp.canvas.handle, pchar(wCaption), length(wCaption), wrect,
        DT_END_ELLIPSIS or dt_VCenter or DT_SingleLine or DT_Left);
    end
    else
    begin
      inflaterect(wrect, -2, -2);
      flabelbmp.canvas.StretchDraw(rect(wrect.left + 2, wrect.top + 1, wrect.left + 18, wrect.top + 17), fIconBMP);
      inflaterect(wrect, 2, 2);
      if flat then
      begin
         wPt := screentoclient(mouse.CursorPos);
         if PtInRect(rect(xpos, TopMargin, xpos + btnsize, TopMargin + btnheight), wPt) then
         begin
            frame3d(flabelbmp.canvas, wrect, clBtnHighlight, cl3DDkShadow, 1);
            inflateRect(wRect, 1, 1);
         end;
      end
      else
      begin
         frame3d(flabelbmp.canvas, wrect, clBtnHighlight, cl3DDkShadow, 1);
         frame3d(flabelbmp.canvas, wrect, cl3DLight, clBtnShadow, 1);
      end;
      wrect.left := wrect.left + 20;
      wrect.top := wrect.top - 1;
      wRect.right := wrect.right - 1;
      FLabelBMP.Canvas.Font.Style := FLabelBMP.Canvas.Font.Style - [fsBold];
      DrawText(flabelbmp.canvas.handle, pchar(wCaption), length(wCaption), wrect,
        DT_END_ELLIPSIS or dt_VCenter or DT_SingleLine or DT_Left);
    end;

    if updatelist then
      fButtons[loop] := rect(xpos, TopMargin, xpos + btnsize, TopMargin + btnheight);

    fBufferBMP.canvas.Draw(xpos, TopMargin, flabelbmp);

    inc(xpos, btnsize + btnspace);
    inc(loop);
  end;
  BitBlt(canvas.handle, 0, 0, clientwidth, clientheight, fBufferBMP.canvas.handle, 0, 0, SRCCOPY);

  if assigned(screen.ActiveForm) then
  begin
    fLastActiveForm := screen.ActiveForm;

    if (screen.ActiveForm.FormStyle = fsMDIChild) then
    begin
      try
        fLastActiveMDIChild := screen.ActiveForm;
      except
        fLastActiveMDIChild := nil;
      end;
    end;
  end;
end;

procedure TrmTaskBar.RegisterWindow(F: TForm);
var
  loop: integer;
  found: boolean;
  Added: boolean;
begin
  loop := 0;
  found := false;
  while loop < fWindowlist.count do
  begin
    if fwindowlist[loop] = f then
    begin
      found := true;
      break;
    end;
    inc(loop);
  end;
  if not found then
  begin
    added := false;

    if (((f.BorderStyle = bsToolWindow) or (f.BorderStyle = bsSizeToolWin)) and not (wtToolWin in fExcludeWinTypes)) or
      ((f.BorderStyle = bsDialog) and not (wtDialog in fExcludeWinTypes)) or
      ((f.FormStyle = fsMDIChild) and not (wtMDIChild in fExcludeWinTypes)) then
    begin
      fWindowList.add(f);
      added := true;
    end;

    if added then
    begin
      if assigned(fOnWindowAdded) then
        fOnWindowAdded(self, f);
      FreeNotification(f);
      SetLength(fButtons, 0);
    end;
  end;
end;

procedure TrmTaskBar.RegisterWindowTemp(F: TForm);
begin
    FTempList.Add(f);
    invalidate;
end;

procedure TrmTaskBar.SetAutoHide(const Value: boolean);
var
  loop: integer;
  f: TForm;
begin
  fAutoHide := Value;

  if fAutoHide = false then
  begin
    loop := 0;
    while loop < fwindowlist.count do
    begin
      f := TForm(fwindowlist[loop]);
      if ((f.windowstate = wsnormal) or (f.Windowstate = wsmaximized)) and
        (f.FormStyle = fsMDIChild) and (not iswindowvisible(f.handle)) then
      begin
        showwindow(f.handle, sw_show);
      end;
      inc(loop);
    end;
  end;
end;

procedure TrmTaskBar.SetBtnHeight(const Value: integer);
begin
  fBtnHeight := Value;
  invalidate;
end;

procedure TrmTaskBar.SetBtnSpace(const Value: integer);
begin
  fBtnSpace := Value;
  invalidate;
end;

procedure TrmTaskBar.SetColor(const Value: TColor);
begin
  FColor := Value;
  Repaint;
end;

procedure TrmTaskBar.SetExcludes(const Value: TWinTypes);
var
  loop: integer;
  f: TForm;
  added, Removed: boolean;
begin
  if fExcludeWinTypes <> Value then
  begin
    fExcludeWinTypes := Value;
    loop := FWindowList.Count;

    while loop > 0 do
    begin
      removed := false;
      dec(loop);
      f := FWindowList[loop];

      if ((wtToolWin in fExcludeWinTypes) and ((f.BorderStyle = bsToolWindow) or (f.BorderStyle = bsSizeToolWin))) or
        ((wtDialog in fExcludeWinTypes) and (f.BorderStyle = bsDialog)) or
        ((wtMDIChild in fExcludeWinTypes) and (f.FormStyle = fsMDIChild)) then
      begin
        fWindowList.delete(loop);
        removed := true;
      end;

      if removed then
      begin
        if assigned(fOnWindowRemoved) then
          fOnWindowRemoved(self, f);
        SetLength(fButtons, 0);
      end;
    end;

    for loop := 0 to screen.CustomFormCount - 1 do
    begin
      f := TForm(screen.CustomForms[loop]);
      if fWindowList.indexof(f) = -1 then
      begin
        added := false;

        if (((f.BorderStyle = bsToolWindow) or (f.BorderStyle = bsSizeToolWin)) and not (wtToolWin in fExcludeWinTypes)) or
          ((f.BorderStyle = bsDialog) and not (wtDialog in fExcludeWinTypes)) or
          ((f.FormStyle = fsMDIChild) and not (wtMDIChild in fExcludeWinTypes)) then
        begin
          added := true;
          fWindowList.add(f);
        end;

        if added then
        begin
          if assigned(fOnWindowAdded) then
            fOnWindowAdded(self, f);
          FreeNotification(f);
          SetLength(fButtons, 0);
        end;
      end;
    end;
    Invalidate;
  end;
end;

procedure TrmTaskBar.SetMaxBtnSize(const Value: integer);
begin
  fMaxBtnSize := Value;
  invalidate;
end;

procedure TrmTaskBar.SetMinBtnSize(const Value: integer);
begin
  fMinBtnSize := Value;
  invalidate;
end;

procedure TrmTaskBar.UnRegisterWindow(F: TForm);
var
  loop: integer;
  found: boolean;
begin
  loop := 0;
  found := false;
  while loop < fTemplist.count do
  begin
    if fTemplist[loop] = f then
    begin
      found := true;
      break;
    end;
    inc(loop);
  end;
  if found then
  begin
    fTemplist.Delete(loop);
    SetLength(fButtons, 0);
          //if we found it here then it wont be in FWindowList....
    Exit;
  end;

  loop := 0;
  found := false;
  while loop < fWindowlist.count do
  begin
    if fwindowlist[loop] = f then
    begin
      found := true;
      break;
    end;
    inc(loop);
  end;
  if found then
  begin
    fWindowlist.Delete(loop);
    if assigned(fOnWindowRemoved) then
      fOnWindowRemoved(self, f);
    SetLength(fButtons, 0);
    Repaint;
  end;
end;

procedure TrmTaskBar.wmCommand(var msg: TMessage);
begin
  case loword(msg.wparam) of
    SC_SIZE,
      SC_MOVE,
      SC_MINIMIZE,
      SC_MAXIMIZE,
      SC_NEXTWINDOW,
      SC_PREVWINDOW,
      SC_CLOSE,
      SC_VSCROLL,
      SC_HSCROLL,
      SC_MOUSEMENU,
      SC_KEYMENU,
      SC_ARRANGE,
      SC_RESTORE,
      SC_TASKLIST,
      SC_SCREENSAVE,
      SC_HOTKEY,
      SC_DEFAULT,
      SC_MONITORPOWER,
      SC_CONTEXTHELP,
      SC_SEPARATOR:
      begin
        if assigned(fmenuWin) then
          postmessage(fmenuwin.handle, wm_syscommand, msg.wparam, msg.lparam);
        invalidate;
      end;
  else
    if assigned(fmenuWin) then
      postmessage(fmenuwin.handle, wm_command, msg.wparam, msg.lparam);
  end;
end;

procedure TrmTaskBar.SetLeftMargin(const Value: integer);
begin
  fLeftMargin := Value;
  invalidate;
end;

procedure TrmTaskBar.SetRightMargin(const Value: integer);
begin
  fRightMargin := Value;
  invalidate;
end;

procedure TrmTaskBar.SetTopMargin(const Value: integer);
begin
  fTopMargin := Value;
  invalidate;
end;

procedure TrmTaskBar.SetDelay(const Value: integer);
begin
  if fdelay <> value then
    fdelay := value;
end;

procedure TrmTaskBar.HideHint(ClearHint: Boolean);
begin
  FTaskHint.ReleaseHandle;
  FTaskHint.Tag := -1;
  if ClearHint then
    fHint := '';
end;

{ ********** Windows Hooking Procedures ********** }

procedure TrmTaskBar.HookWin;
begin
  if csdesigning in componentstate then exit;
  if not assigned(NewWndProc) then
  begin
    OldWndProc := TFarProc(GetWindowLong(TForm(Owner).handle, GWL_WNDPROC));
    {$ifdef D6_or_higher}
    NewWndProc := Classes.MakeObjectInstance(HookWndProc);
    {$else}
    NewWndProc := MakeObjectInstance(HookWndProc);
    {$endif}
    SetWindowLong(TForm(Owner).handle, GWL_WNDPROC, LongInt(NewWndProc));
    if TForm(Owner).formstyle = fsMDIForm then HookMDIWin;
  end;
end;

procedure TrmTaskBar.UnhookWin;
begin
  if csdesigning in componentstate then exit;
  if assigned(NewWndProc) then
  begin
    SetWindowLong(TForm(Owner).handle, GWL_WNDPROC, LongInt(OldWndProc));
    if assigned(NewWndProc) then
    {$ifdef D6_or_higher}
       Classes.FreeObjectInstance(NewWndProc);
    {$else}
       FreeObjectInstance(NewWndProc);
    {$endif}
    NewWndProc := nil;
  end;
  UnHookMDIWin;
end;

procedure TrmTaskBar.HookWndProc(var AMsg: TMessage);
begin
  case AMsg.msg of
    WM_PARENTNOTIFY:
      begin
        if (AMsg.wParamLo <> wm_create) or (AMsg.wParamLo <> wm_Destroy) then
          invalidate;
      end;

    CM_ACTIVATE:
      begin
        fMainFormFocused := true;
        invalidate;
      end;
  end;
  AMsg.Result := CallWindowProc(OldWndProc, Tform(Owner).handle, AMsg.Msg, AMsg.wParam, AMsg.lParam);

{$IFDEF rmDebug}
  if assigned(fWinMessage) then
    fWinMessage(1, aMsg.msg);
{$ENDIF}
end;

procedure TrmTaskBar.HookMDIWin;
begin
  if csdesigning in componentstate then exit;
  if not assigned(NewMDIWndProc) then
  begin
    OldMDIWndProc := TFarProc(GetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC));
    {$ifdef D6_or_higher}
    NewMDIWndProc := Classes.MakeObjectInstance(HookMDIWndProc);
    {$else}
    NewMDIWndProc := MakeObjectInstance(HookMDIWndProc);
    {$endif}
    SetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC, LongInt(NewMDIWndProc));
  end;
end;

procedure TrmTaskBar.UnhookMDIWin;
begin
  if csdesigning in componentstate then exit;
  if assigned(NewMDIWndProc) then
  begin
    SetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC, LongInt(OldMDIWndProc));
    if assigned(NewMDIWndProc) then
    {$ifdef D6_or_higher}
       Classes.FreeObjectInstance(NewMDIWndProc);
    {$else}
       FreeObjectInstance(NewMDIWndProc);
    {$endif}
    NewMDIWndProc := nil;
    OldMDIWndProc := nil;
  end;
end;

procedure TrmTaskBar.HookMDIWndProc(var AMsg: TMessage);
var
  loop: integer;
begin
  with AMsg do
  begin
    if not ((msg = WM_MDIGETACTIVE) or (msg = WM_NCPaint) or (msg = WM_NCHITTEST)) then
      Invalidate;

    if (msg = WM_MDIREFRESHMENU) and assigned(fMDIMenuRefresh) then
      fMDIMenuRefresh(self);

    Result := CallWindowProc(OldMDIWndProc, TForm(Owner).ClientHandle, Msg, wParam, lParam);

    if (msg = wm_parentNotify) then
    begin
      if WParamLo = WM_LBUTTONDOWN then
      begin
        for loop := WindowCount - 1 downto 0 do
        begin
          if PtInRect(WindowList[loop].BoundsRect, Point(LParamLo, LParamHi)) then
          begin
            if fMainFormFocused and assigned(fLastActiveMDIChild) and (WindowList[loop] = fLastActiveMDIChild) then
              DoDummyForm(fLastActiveMDIChild);
            break;
          end;
        end;
      end;
    end;

  end;

{$IFDEF rmDebug}
  if assigned(fWinMessage) then
    fWinMessage(2, aMsg.msg);
{$ENDIF}
end;

procedure TrmTaskBar.wmEraseBkgnd(var msg: TMessage);
begin
  msg.result := 1;
end;

function TrmTaskBar.GetActiveForm: TForm;
begin
  Result := fLastActiveForm;
end;

function TrmTaskBar.GetWindowCount: integer;
begin
  Result := FWindowList.Count;
end;

function TrmTaskBar.GetWindowItem(index: integer): TForm;
begin
  result := TForm(FWindowList[index]);
end;

function TrmTaskBar.GetMDIChild(index: integer): TForm;
var
  count: integer;
  loop: integer;
begin
  loop := 0;
  count := 0;
  result := nil;
  while loop < FWindowList.count do
  begin
    if TForm(fWindowList[loop]).FormStyle = fsMDIChild then
    begin
      if count = index then
      begin
        result := TForm(fWindowList[loop]);
        break;
      end;
      inc(count);
    end;
    inc(loop);
  end;
end;

function TrmTaskBar.GetMDIChildCount: integer;
var
  count: integer;
  loop: integer;
begin
  loop := 0;
  count := 0;
  while loop < FWindowList.count do
  begin
    if TForm(fWindowList[loop]).FormStyle = fsMDIChild then
      inc(count);
    inc(loop);
  end;
  result := count;
end;

function TrmTaskBar.GetActiveMDIChild: TForm;
begin
  Result := fLastActiveMDIChild;
end;

procedure TrmTaskBar.MinimizeAll;
begin
  MinimizeWindowTypes([wtMDIChild, wtDialog, wtToolWin]);
end;

procedure TrmTaskBar.MinimizeAllMDI;
begin
  MinimizeWindowTypes([wtMDIChild]);
end;

procedure TrmTaskBar.MinimizeWindowTypes(WinTypes: TWinTypes);
var
  loop: integer;
  f: TForm;
begin
  loop := 0;
  while loop < fWindowlist.count do
  begin
    f := fwindowlist[loop];

    if ((wtToolWin in WinTypes) and ((f.BorderStyle = bsToolWindow) or (f.BorderStyle = bsSizeToolWin))) or
      ((wtDialog in WinTypes) and (f.BorderStyle = bsDialog)) or
      ((wtMDIChild in WinTypes) and (f.FormStyle = fsMDIChild)) then
    begin
      f.WindowState := wsMinimized;
    end;

    inc(loop);
  end;
end;

procedure TrmTaskBar.DoDummyForm(ToggleForm: TForm);
var
  wControl : TWinControl;
begin
  if TForm(owner).formstyle = fsMDIForm then
  begin
     if ToggleForm.CanFocus then
     begin
        ToggleForm.SetFocus;
        if assigned(ToggleForm.ActiveControl) then
        begin
           wControl := ToggleForm.ActiveControl;
           ToggleForm.DefocusControl(ToggleForm.ActiveControl, False);
           ToggleForm.SetFocusedControl(wControl);
        end;
        fMainFormFocused := false;
        invalidate;
     end;
  end;
end;

procedure TrmTaskBar.wmDestroy(var msg: TMessage);
begin
  UnhookWin;
end;

procedure TrmTaskBar.SetFlat(const Value: boolean);
begin
  if fFlat <>  value then
  begin
    fFlat := Value;
    Invalidate;
  end;
end;

end.

