{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmCaptionButtons
Purpose  : Allows for new caption bar buttons to be defined.
Date     : 06-18-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmCaptionButtons;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, extctrls,
  imglist;

type
  TrmCaptionButtonStyle = (cbsButton, cbsSpacer);

  TrmCaptionButtonItem = class(TCollectionItem)
  private
    fimageindex: integer;
    fStyle: TrmCaptionButtonStyle;
    fwidth: integer;
    fcaption: string;
    fvisible: boolean;
    fEnabled: boolean;
    procedure setimageindex(value: integer);
    procedure SetStyle(value: TrmCaptionButtonStyle);
    procedure setwidth(value: integer);
    function GetRect: TRect;
    function GetCapBtnRect: TRect;
    procedure SetCaption(value: string);
    procedure SetVisible(value: boolean);
    procedure SetEnabled(const Value: boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure hide;
    procedure show;
    property ButtonRect: trect read GetRect;
    property CapBtnRect: TRect read GetCapBtnRect;
  published
    property Enabled : boolean read fEnabled write SetEnabled default true;
    property ImageIndex: integer read fimageindex write setimageindex default -1;
    property Style: TrmCaptionButtonStyle read fstyle write setStyle default cbsbutton;
    property Width: integer read fwidth write setwidth default 5;
    property Caption: string read fcaption write setcaption;
    property Visible: boolean read fvisible write setvisible default true;
  end;

  TrmCaptionButtonsCollection = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TrmCaptionButtonItem;
    procedure SetItem(Index: Integer; Value: TrmCaptionButtonItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TrmCaptionButtonItem;
    function ExcludeBtnRgn(R: TRect): TRect;
    function GetCaptionRect: TRect;
    function ButtonsRect: TRect;
    property Items[Index: Integer]: TrmCaptionButtonItem read GetItem write SetItem; default;
  end;

  TrmCaptionButtonClick = procedure(ButtonIndex: integer) of object;

  TrmCaptionButtons = class(TComponent)
  private
    { Private declarations }
    fbuttons: TrmCaptionButtonsCollection;
    fimages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    ffont: TFont;
    fCanvas: TCanvas;

    OldWndProc: TFarProc;
    NewWndProc: Pointer;

    fPaintButtons: boolean;
    paintingbutton: boolean;
    fvisible: boolean;
    fncButtonID: integer;
    fncmousedown: boolean;
    fncmousedownrect: trect;

    fCapBtnClick: TrmCaptionButtonClick;

    function calcPoint(x, y: integer): tpoint;
    procedure SetVisible(value: boolean);
    procedure SetFont(value: TFont);
    procedure CMTextChanged(var msg: TMessage); message CM_TextChanged;
    function GetOwnerAsForm: TForm;
    procedure SetImages(const Value: TCustomImagelist);
    procedure ImageListChange(Sender: TObject);

  protected
    { Protected declarations }
    procedure PaintButtons(pt: tpoint);
    function BtnEnabled(pt:TPoint):boolean;

    procedure HookWndProc(var WorkMsg: TMessage);
    procedure HookWin;
    procedure UnhookWin;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;

    procedure updatecaption;
    procedure Show;
    procedure Hide;
  published
    { Published declarations }
    property Buttons: TrmCaptionButtonsCollection read fbuttons write fbuttons;
    property Visible: boolean read fvisible write SetVisible default true;
    property Images: TCustomImagelist read fimages write SetImages;
    property Font: TFont read ffont write Setfont;
    property OnClick: TrmCaptionButtonClick read fCapBtnClick write fCapBtnClick;
  end;

implementation

uses rmGlobalComponentHook;

{ TrmCaptionButtonItem }

constructor TrmCaptionButtonItem.Create(Collection: TCollection);
begin
  fimageindex := -1;
  fvisible := true;
  fEnabled := true;
  inherited Create(Collection);
end;

procedure TrmCaptionButtonItem.Assign(Source: TPersistent);
begin
  if Source is TrmCaptionButtonItem then
  begin
    imageindex := TrmCaptionButtonItem(Source).imageindex;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TrmCaptionButtonItem.setimageindex(value: integer);
begin
  fimageindex := value;
  Changed(False);
end;

procedure TrmCaptionButtonItem.SetStyle(value: TrmCaptionButtonStyle);
begin
  fstyle := value;
  changed(false);
end;

procedure TrmCaptionButtonItem.setwidth(value: integer);
begin
  fwidth := value;
  changed(false);
end;

function TrmCaptionButtonItem.GetCapBtnRect: TRect;
var
  r: trect;
  loop: integer;
begin
  with TrmCaptionButtonsCollection(collection) do
  begin
    r := buttonsrect;
    for loop := index - 1 downto 0 do
      if items[loop].visible then r.left := r.left + items[loop].width;
    r.right := r.left + items[index].width;
    r.top := r.top + 2;
    r.bottom := r.bottom - 2;
    result := r;
  end;
end;

function TrmCaptionButtonItem.GetRect: TRect;
var
  r: trect;
  loop: integer;
begin
  with TrmCaptionButtonsCollection(collection) do
  begin
    r := excludebtnrgn(GetCaptionrect);
    r := rect(0, 0, 0, r.bottom - r.top);
    for loop := 0 to index - 1 do
      if items[loop].visible then r.left := r.left + items[loop].width;
    r.right := r.left + items[index].width;
    r.top := r.top + 2;
    r.bottom := r.bottom - 2;
    result := r;
  end;
end;

procedure TrmCaptionButtonItem.setCaption(value: string);
begin
  fcaption := value;
  changed(false);
end;

procedure TrmCaptionButtonItem.setvisible(value: boolean);
begin
  fvisible := value;
  changed(false);
end;

procedure TrmCaptionButtonItem.hide;
begin
  fvisible := false;
  changed(false);
end;

procedure TrmCaptionButtonItem.show;
begin
  fvisible := true;
  changed(false);
end;

procedure TrmCaptionButtonItem.SetEnabled(const Value: boolean);
begin
  fEnabled := Value;
  changed(false);
end;

{ TrmCaptionButtonsCollection }

constructor TrmCaptionButtonsCollection.Create(AOwner: TPersistent);
begin
  inherited Create(TrmCaptionButtonItem);
  fOwner := AOwner;
end;

function TrmCaptionButtonsCollection.Add: TrmCaptionButtonItem;
begin
  Result := TrmCaptionButtonItem(inherited Add);
end;

function TrmCaptionButtonsCollection.GetItem(Index: Integer): TrmCaptionButtonItem;
begin
  Result := TrmCaptionButtonItem(inherited GetItem(Index));
end;

function TrmCaptionButtonsCollection.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

procedure TrmCaptionButtonsCollection.SetItem(Index: Integer; Value: TrmCaptionButtonItem);
begin
  inherited SetItem(Index, Value);
end;

function TrmCaptionButtonsCollection.ExcludeBtnRgn(R: TRect): TRect;
var
  BtnWidth: integer;
  BS: TFormBorderStyle;
  BI: TBorderIcons;
begin
  BS := TForm(GetOwner).BorderStyle;
  if BS = bsNone then exit;
  BtnWidth := GetSystemMetrics(SM_CXSIZE);
  if BS in [bsToolWindow, bsSizeToolWin] then
  begin
    R.Right := R.Right - GetSystemMetrics(SM_CXSMSIZE) - 2; { close icon only }
    result := r;
    exit;
  end;

  BI := TForm(GetOwner).BorderIcons;

  if (biSystemMenu in BI)
    then R.Right := R.Right - BtnWidth - 2; { close icon - this is OS dependant }
  if ((BS <> bsDialog) and ((biMinimize in BI) or (biMaximize in BI)))
    then R.Right := R.Right - 2 * BtnWidth; { minimise and maximise icon }

  if ((BS = bsDialog) and (biHelp in BI))
    then R.Right := R.Right - BtnWidth - 2 { help icon }
  else R.Right := R.Right - 2;

  result := r;
end;

function TrmCaptionButtonsCollection.GetCaptionRect: TRect;
var
  BS: TFormBorderStyle;
begin
  BS := TForm(GetOwner).BorderStyle;

  { if we have no border style, then just set the rectangle empty. }
  if BS = bsNone then
  begin
    SetRectEmpty(Result);
    exit;
  end;

  GetWindowRect(TForm(GetOwner).handle, Result);
  { Convert rect from screen (absolute) to client (0 based) coordinates. }
  OffsetRect(Result, -Result.Left, -Result.Top);
  { Shrink rectangle to allow for window border.  We let Windows paint the border. }
     { this catches drawing MDI minimised windows caption bars in Win95 }
  if ((GetWindowLong(TForm(GetOwner).handle, GWL_STYLE) and WS_MINIMIZE) <> 0)
    then InflateRect(Result, -GetSystemMetrics(SM_CXFIXEDFRAME),
      -GetSystemMetrics(SM_CYFIXEDFRAME))
  else
    case BS of
      bsToolWindow, bsSingle, bsDialog:
        InflateRect(Result, -GetSystemMetrics(SM_CXFIXEDFRAME),
          -GetSystemMetrics(SM_CYFIXEDFRAME));
      bsSizeToolWin, bsSizeable:
        InflateRect(Result, -GetSystemMetrics(SM_CXSIZEFRAME),
          -GetSystemMetrics(SM_CYSIZEFRAME));
    end;
     { Set the appropriate height of caption bar. }
  if BS in [bsToolWindow, bsSizeToolWin] then
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION) - 1
  else
    Result.Bottom := Result.Top + GetSystemMetrics(SM_CYCAPTION) - 1;
end; { GetTitleBarRect }

function TrmCaptionButtonsCollection.ButtonsRect: TRect;
var
  r: trect;
  loop: integer;
begin
  r := excludebtnrgn(GetCaptionrect);
  r.left := r.right;
  for loop := 0 to count - 1 do
    if items[loop].visible then r.left := r.left - items[loop].width;
  result := r;
end;

{ TrmCaptionButtons }

constructor TrmCaptionButtons.create(AOwner: TComponent);
begin
  inherited create(AOwner);

  OldWndProc := nil;
  NewWndProc := nil;

  fcanvas := tcanvas.create;
  fbuttons := TrmCaptionButtonsCollection.create(Aowner);

  fncmousedown := false;
  fncmousedownrect := rect(0, 0, 0, 0);
  fPaintButtons := true;
  fvisible := true;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FFont := TFont.create;
  ffont.assign(GetOwnerAsForm.Font);

  HookWin;
end;

destructor TrmCaptionButtons.destroy;
begin
  UnHookWin;

  fbuttons.free;
  fcanvas.free;
  ffont.free;

  inherited;
end;

procedure TrmCaptionButtons.HookWin;
begin
  if csdesigning in componentstate then exit;
  if not assigned(NewWndProc) then
  begin
    OldWndProc := TFarProc(GetWindowLong(GetOwnerAsForm.Handle, GWL_WNDPROC));
    {$ifdef D6_or_higher}
    NewWndProc := Classes.MakeObjectInstance(HookWndProc);
    {$else}
    NewWndProc := MakeObjectInstance(HookWndProc);
    {$endif}
    SetWindowLong(GetOwnerAsForm.Handle, GWL_WNDPROC, LongInt(NewWndProc));
    PushOldProc(GetOwnerAsForm, OldWndProc);
  end;
end; { HookWin }

procedure TrmCaptionButtons.UnhookWin;
begin
  if csdesigning in componentstate then exit;
  if assigned(NewWndProc) then
  begin
    SetWindowLong(GetOwnerAsForm.Handle, GWL_WNDPROC, LongInt(PopOldProc(GetOwnerAsForm)));
    if assigned(NewWndProc) then
    {$ifdef D6_or_higher}
       Classes.FreeObjectInstance(NewWndProc);
    {$else}
       FreeObjectInstance(NewWndProc);
    {$endif}
    oldWndProc := nil;
    NewWndProc := nil;
  end;
end; { UnHookWin }

function TrmCaptionButtons.calcPoint(x, y: integer): tpoint;
var
  wp: tpoint;
begin
  with GetOwnerAsForm do
    case WindowState of
      wsnormal: wp := point(x - left, y - top);
      wsMaximized: wp := point(x, y);
      wsMinimized: wp := point(x - left, y - top);
    end;
  if GetOwnerAsForm.left < 0 then inc(wp.x, abs(GetOwnerAsForm.left));
  if GetOwnerAsForm.top < 0 then inc(wp.y, abs(GetOwnerAsForm.top));
  result := wp;
end;

procedure TrmCaptionButtons.HookWndProc(var WorkMsg: TMessage);
var
  xpos, ypos: integer;
  wp: tpoint;
  myMsg: Cardinal;
  oldBtnID : integer;
begin
  with WorkMsg do
  begin
    MyMsg := msg;
    case MyMsg of
      WM_DESTROY:
        begin
          Result := CallWindowProc(OldWndProc, GetOwnerAsForm.handle, WorkMsg.Msg, WorkMsg.wParam, WorkMsg.lParam);
          UnHookWin;
          UpdateCaption;
          exit;
        end;

      WM_ERASEBKGND:
        if paintingbutton then
        begin
          result := 0;
          exit;
        end;

      WM_NCLBUTTONDBLCLK:
        begin
          xpos := TWMNCLButtonDown(WorkMsg).xcursor;
          ypos := TWMNCLButtonDown(WorkMsg).ycursor;
          wp := calcPoint(xpos, ypos);
          if (fvisible) and ptinrect(fButtons.ButtonsRect, wp) then
          begin
            if BtnEnabled(wp) then
            begin
               fncmousedown := true;
               PaintButtons(wp);
            end;
            result := 0;
            exit;
          end
        end;

      wm_nchittest:
        begin
          Result := CallWindowProc(OldWndProc, GetOwnerAsForm.handle, MyMsg, wParam, lParam);
          if result = htCaption then
          begin
            xpos := twmncHitTest(WorkMsg).xpos;
            ypos := twmncHitTest(WorkMsg).ypos;
            wp := GetOwnerAsForm.screentoclient(calcPoint(xpos, ypos));
            if (fvisible) and ptinrect(fButtons.ButtonsRect, wp) and (fNCMouseDown) then
            begin
              if fNCButtonId <> -1 then PaintButtons(point(-1, -1));
              result := 0;
              exit;
            end;
          end
          else
          begin
            PaintButtons(point(-1, -1));
          end;
        end;

      WM_NCLButtonDown:
        begin
          xpos := TWMNCLButtonDown(WorkMsg).xcursor;
          ypos := TWMNCLButtonDown(WorkMsg).ycursor;
          wp := calcPoint(xpos, ypos);
          if (fvisible) and ptinrect(fButtons.ButtonsRect, wp) then
          begin
            if BtnEnabled(wp) then
            begin
               fncmousedown := true;
               PaintButtons(wp);
            end;
            result := 0;
            exit;
          end
        end;

      WM_NCMouseMove:
        begin
          xpos := TWMNCLButtonDown(WorkMsg).xcursor;
          ypos := TWMNCLButtonDown(WorkMsg).ycursor;
          wp := calcPoint(xpos, ypos);
          if (fvisible) and ptinrect(fncMouseDownRect, wp) and (fNCMouseDown) then
          begin
            if fNCButtonId = -1 then PaintButtons(wp);
            result := 0;
            exit;
          end;
          if (fNCButtonId <> -1) then PaintButtons(point(-1, -1));
        end;

      WM_NCLButtonUp:
        begin
          xpos := TWMNCLButtonUp(WorkMsg).xcursor;
          ypos := TWMNCLButtonUp(WorkMsg).ycursor;
          wp := calcPoint(xpos, ypos);

          if (fvisible) and ptinrect(fncMouseDownRect, wp) and (fNCMouseDown) then
          begin
            OldBtnID := fncButtonID;
            fncmousedown := false;
            Result := 0;
            PaintButtons(point(-1, -1));

            if assigned(fCapBtnClick) then
               fCapBtnClick(OldBtnID);
            exit;
          end;
          fncmousedown := false;
        end;
    end;

    Result := CallWindowProc(OldWndProc, (Owner as TForm).handle, MyMsg, wParam, lParam);

    case MyMsg of
      WM_NCPAINT: PaintButtons(point(-1, -1));
      WM_NCACTIVATE: PaintButtons(point(-1, -1));
      WM_MouseMove: if (fNCButtonId <> -1) then PaintButtons(point(-1, -1));
      WM_LButtonUp: if fncmousedown then
        begin
          fncmousedown := false;
          PaintButtons(point(-1, -1));
        end;
    end;
  end;
end; { HookWndProc }

procedure TrmCaptionButtons.PaintButtons(pt: tpoint);
var
  btnrect: trect;
  loop: integer;
  x, y: integer;
  bmp: TBitmap;
  tc1, tc2, bc1, bc2: TColor;
//  xadj, yadj: integer;
  wItem : TrmCaptionButtonItem;
  wDrawFlags : UInt;
  wBtnDown : boolean;
begin
  if (fPaintButtons = false) or (fvisible = false) then exit;
  PaintingButton := true;
  bmp := tbitmap.create;
  fcanvas.handle := getwindowdc(GetOwnerAsForm.handle);
  try
    btnrect := fbuttons.buttonsrect;
    bmp.Width := btnrect.Right - btnrect.left;
    bmp.height := btnrect.bottom - btnrect.top;
    bmp.Canvas.CopyRect(rect(0, 0, bmp.width, bmp.height), fcanvas, btnrect);
    bmp.canvas.font.assign(font);
    fNCbuttonID := -1;

    wDrawFlags := DT_VCenter or DT_CENTER or DT_NOPREFIX or DT_SINGLELINE;
    for loop := 0 to fbuttons.count - 1 do
    begin
      wItem := fbuttons[loop];
      if wItem.style = cbsbutton then
      begin
        btnrect := wItem.ButtonRect;
        bmp.canvas.brush.Color := clbtnface;
        bmp.canvas.FillRect(btnrect);
        if ptinrect(wItem.CapBtnRect, pt) then
        begin
          tc1 := cl3dDkShadow;
          tc2 := clbtnShadow;
          bc1 := clbtnhighlight;
          bc2 := cl3dlight;
          fNCbuttonID := loop;
          fncmousedownrect := wItem.CapBtnRect;
          wBtnDown := true;
        end
        else
        begin
          tc1 := clbtnhighlight;
          tc2 := cl3dlight;
          bc1 := cl3dDkShadow;
          bc2 := clbtnShadow;
          wBtnDown := false;
        end;

        frame3d(bmp.canvas, btnrect, tc1, bc1, 1);
        frame3d(bmp.canvas, btnrect, tc2, bc2, 1);

        if assigned(fimages) and (wItem.imageindex <> -1) then
        begin
          y := btnrect.top + ((btnrect.bottom - btnrect.top) shr 1) - (fimages.height shr 1);
          x := btnrect.left + ((btnrect.right - btnrect.left) shr 1) - (fimages.width shr 1);
          inc(x);
          inc(y);
        end
        else
        begin
          y := btnrect.top + ((btnrect.bottom - btnrect.top) shr 1) - (bmp.canvas.textheight(fbuttons[loop].caption) shr 1);
          x := btnrect.left + ((btnrect.right - btnrect.left) shr 1) - (bmp.canvas.textwidth(fbuttons[loop].caption) shr 1);
        end;
        if assigned(fimages) and (wItem.imageindex <> -1) then
        begin
           if wBtnDown then
              fimages.Draw(bmp.canvas, x, y, wItem.imageindex)
           else
              fimages.Draw(bmp.canvas, x-1, y-1, wItem.imageindex);
        end
        else
        begin
             bmp.Canvas.brush.Style := bsClear;
             if not wbtnDown then
                OffsetRect(btnrect, -1, -1);
             try
                if wItem.enabled then
                   DrawText(bmp.Canvas.handle, pchar(wItem.Caption), length(wItem.Caption), btnRect, wDrawFlags)
                else
                begin
                   bmp.Canvas.Font.Color := clBtnHighlight;
                   try
                      DrawText(bmp.Canvas.handle, pchar(wItem.Caption), length(wItem.Caption), btnRect, wDrawFlags);
                      bmp.Canvas.Font.Color := clBtnShadow;
                      OffsetRect(btnrect, -1, -1);
                      DrawText(bmp.Canvas.handle, pchar(wItem.Caption), length(wItem.Caption), btnRect, wDrawFlags);
                      OffsetRect(btnrect, 1, 1);
                   finally
                      bmp.canvas.font.color := clBtnText;
                   end
                end;
             finally
                if not wBtnDown then
                   OffsetRect(btnrect, 1, 1);
                bmp.Canvas.brush.Style := bsSolid;
             end;
        end;
      end;
    end;
    btnrect := fbuttons.buttonsrect;
    fcanvas.draw(btnrect.left, btnrect.top, bmp);
  finally
    bmp.free;
    if fcanvas.handle <> 0 then
      releasedc(GetOwnerAsForm.handle, fcanvas.handle);
    fcanvas.handle := 0;
    PaintingButton := false;
  end;
end;

procedure TrmCaptionButtons.updatecaption;
begin
  try
     fPaintButtons := false;
     SetWindowPos(GetOwnerAsForm.handle, 0, 0, 0, 0, 0,
       SWP_FRAMECHANGED or SWP_DRAWFRAME or
       SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
     fPaintButtons := true;
     SetWindowPos(GetOwnerAsForm.handle, 0, 0, 0, 0, 0,
       SWP_FRAMECHANGED or SWP_DRAWFRAME or
       SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE);
  except
     //Do Nothing   
  end;
end;


procedure TrmCaptionButtons.Show;
begin
  fvisible := true;
  updatecaption;
end;

procedure TrmCaptionButtons.Hide;
begin
  fvisible := false;
  updatecaption;
end;

procedure TrmCaptionButtons.SetVisible(value: boolean);
begin
  case value of
    true: show;
    false: hide;
  end;
end;

procedure TrmCaptionButtons.SetFont(value: Tfont);
begin
  ffont.assign(value);
  PaintButtons(Point(-1,-1));
end;

procedure TrmCaptionButtons.CMTextChanged(var msg: TMessage);
begin
  inherited;
  PaintButtons(point(-1, -1));
end;

function TrmCaptionButtons.GetOwnerAsForm: TForm;
begin
   if (owner is TForm) then
   begin
     result := TForm(Owner);
     if not result.handleallocated then
       result.handleneeded;
   end
   else
     result := nil;
end;

procedure TrmCaptionButtons.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if operation = opremove then
  begin
     if (AComponent = Images) then
        Images := nil;
  end;
end;

procedure TrmCaptionButtons.SetImages(const Value: TCustomImagelist);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;

  if Not (csdestroying in componentstate) then 
     PaintButtons(point(-1,-1));
end;

procedure TrmCaptionButtons.ImageListChange(Sender: TObject);
begin
   PaintButtons(point(-1,-1));
end;

function TrmCaptionButtons.BtnEnabled(pt: TPoint): boolean;
var
   loop : integer;
begin
   result := false;
   for loop := 0 to fbuttons.Count-1 do
   begin
      if ptinrect(fbuttons.Items[loop].CapBtnRect, pt) then
      begin
         Result := fbuttons.Items[loop].Enabled;
         break;
      end;
   end;
end;

end.

