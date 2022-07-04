{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         vgVCLUtl unit                                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgVCLUtl;

interface
uses Windows, Messages, Classes, Graphics, Controls, {$IFDEF _D4_}ImgList, {$ENDIF}
  Menus, Grids, Forms, Dialogs, StdCtrls, ExtCtrls, CommCtrl, ComCtl98, ComCtrls, vgCtrls;

{ --- Drawing }
function GetRGBColor(Value: TColor): DWORD;
{ Extracts RGB from TColor }

procedure BrushDraw(Canvas: TCanvas; X, Y: Integer; Bmp: TBitmap);
{ Draws Bmp on Canvas transparently }

procedure BrushDrawCentered(Canvas: TCanvas; Rect: TRect; Bmp: TBitmap);
{ Draws Bmp on Canvas transparently centering Bmp within Rect}

procedure BrushDrawImageList(Canvas: TCanvas; X, Y, Index: Integer; ImageList: TControlImageList);
{ Draws image with index Index from ItemList on Canvas transparently }

procedure BrushDrawImageListCentered(Canvas: TCanvas; Rect: TRect; Index: Integer; ImageList: TControlImageList);
{ Draws image with index Index from ItemList on Canvas transparently centered }

procedure DrawCell(Grid: TStringGrid; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState; FontColor, BkgndColor, SelColor: TColor);
{ Draws cell in TStringGrid with specified attributes }

procedure DrawCellImage(Canvas: TCanvas; ImageList: TControlImageList;
  ImageIndex: Integer; const Rect: TRect; StartMargin: Integer);
{ Draws image with index Index from ImageList on canvas }

procedure DrawComboListItem(Control: TWinControl;
  Str1, Str2: String;
  ImageList: TControlImageList; ImageIndex: Integer;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
{ Draws image with index Index from ItemList on TListBox or TComboBox }

{ --- Shadow regions }
procedure MakeShadowRegion(ShadowRgn: HRGN);
{ Makes shadow from specified region }

{ Initializes Page control by given image list }
procedure SetPageControlImages(PageControl: TPageControl; ImageList: TControlImageList);

{ List View Extended Styles }
procedure SetListViewExStyle(ListView: TCustomListView; Style: Integer);
function GetListItemChecked(ListItem: TListItem): Boolean;
procedure SetListItemChecked(ListItem: TListItem; Value: Boolean);

function GetTreeNodeFromItem(TreeView: TCustomTreeView; const Item: TTVItem): TTreeNode;
function GetListItemFromItem(ListView: TCustomListView; const Item: TLVItem): TListItem;

{ --- Rectangles }
function VisibleRect(Control: TControl): TRect;
{ founds rectangle of control that is visible to user }

procedure InvalidateClientRect(Control: TWinControl);
{ Macro for InvalidateRect }

{ --- Moving controls }
procedure DecreaseHeight(Control: TWinControl; Top, H: Integer);
procedure IncreaseHeight(Control: TWinControl; Top, H: Integer);
procedure HideHorizontal(Control: TWinControl; Top, H: Integer);
procedure ShowHorizontal(Control: TWinControl; Top, H: Integer);
procedure ScreenCenter(Form: TForm);

{ --- For Each }
type
  TObjectCallBack  = procedure (AObject: TObject; Data: Pointer);
  TControlCallBack = procedure (Control: TControl; Data: Pointer);

procedure SetEnabled(Controls: array of TControl; Value: Boolean);
{ Sets Enabled property to array of controls }

procedure SetVisible(Controls: array of TControl; Value: Boolean);
{ Sets Visible property to array of controls }

procedure SetEnableColor(Controls: array of TControl; EnableColor: TColor);

procedure AdjustLabelsBounds(Control: TWinControl);
{ Makes sure that label captions is not trancated }

procedure SetLabelColors(Control: TWinControl);
{ Recursively sets TLabel Enabled to Enabled property of their FocusControl }

procedure UpdateControls(Control: TWinControl);
{ Updates all controls within Control }

procedure ForControls(Controls: array of TControl; CallBack:
  TControlCallBack; Data: Pointer);
{ ForEach callback }

procedure ForEachControl(Control: TWinControl;
  ControlClass: TControlClass; CallBack: TControlCallBack; Data: Pointer);
{ Recurrent callback }

{ --- Cursors }
procedure SetCursor(NewCursor: TCursor);
{ Sets new cursor for Screen.Cursor if in main thread and saves old cursor }

procedure AppSetCursor(NewCursor: TCursor);
{ Macro for SetCursor }

procedure RestoreCursor;
{ Restores cursor saved in SetCursor propcedure }

procedure AppRestoreCursor;
{ Macro for RestoreCursor }

{ --- Windows API }
procedure FreeMessageQueue;
{ Waits for empty message queue }

procedure Wait(MSec: Integer);
{ Processes messages for MSec milliseconds }

{ Messages icons }
function CreateImage(Icon: TMsgDlgType): TImage;

{ Streams }
procedure LoadComponentWait(const FileName: string; Instance: TComponent);
procedure SaveComponentWait(const FileName: string; Instance: TComponent);

{$IFNDEF _D4_}
function UseRightToLeftReading: Boolean;

function UseRightToLeftAlignment: Boolean;

function DrawTextBiDiModeFlagsReadingOnly: Longint;

function DrawTextBiDiModeFlags(Flags: Longint): Longint;
{$ENDIF}

implementation

uses SysUtils, vgUtils;

var
  Cursors: TList = nil;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

procedure BrushDraw(Canvas: TCanvas; X, Y: Integer; Bmp: TBitmap);
begin
  Canvas.BrushCopy(Bounds(X, Y, Bmp.Width, Bmp.Height), Bmp,
    Bounds(0, 0, Bmp.Width, Bmp.Height), Bmp.TransparentColor);
end;

procedure BrushDrawCentered(Canvas: TCanvas; Rect: TRect; Bmp: TBitmap);
begin
  BrushDraw(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
    (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp);
end;

procedure BrushDrawImageList(Canvas: TCanvas; X, Y, Index: Integer; ImageList: TControlImageList);
begin
  with ImageList do Draw(Canvas, X, Y, Index);
end;

procedure BrushDrawImageListCentered(Canvas: TCanvas; Rect: TRect; Index: Integer; ImageList: TControlImageList);
begin
  with ImageList do
    Draw(Canvas, (Rect.Left + Rect.Right - Width) div 2, (Rect.Top + Rect.Bottom - Height) div 2,Index);
end;

procedure DrawCellImage(Canvas: TCanvas; ImageList: TControlImageList;
  ImageIndex: Integer; const Rect: TRect; StartMargin: Integer);
var
  X, Y: Integer;
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    ImageList.GetBitmap(ImageIndex, Bmp);
    if (StartMargin = -1) then
      X := Rect.Left + (Rect.Right - Rect.Left - Bmp.Width) div 2
    else
      X := Rect.Left + StartMargin;
    Y := Rect.Top + (Rect.Bottom - Rect.Top - Bmp.Height) div 2;
    BrushDraw(Canvas, X, Y, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure CreateShadowRegion(Rgn: HRGN; x1, y1, x2, y2: Integer);
var
  I, J: Integer;
  PtRgn: HRGN;
begin
  for I := x1 to x2 do
    for J := y1 to y2 do
      if ((I mod 2 = 0) and (J mod 2 =0))
        or ((I mod 2 = 1) and (J mod 2 =1)) then
      if PtInRegion(Rgn, I, J) then
      begin
        PtRgn := CreateRectRgn(I, J, I  + 1, J + 1);
        CombineRgn(Rgn, Rgn, PtRgn, RGN_DIFF);
        DeleteObject(PtRgn);
      end;
end;

procedure DrawCell(Grid: TStringGrid; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState; FontColor, BkgndColor, SelColor: TColor);
  procedure DrawCellText;
  var
    S: string;
  begin
    with Grid do
      begin
        S := Cells[Col, Row];
        Canvas.FillRect(Rect);
        ExtTextOut(Canvas.Handle, Rect.Left + 2, Rect.Top + 2, ETO_CLIPPED or
        ETO_OPAQUE, @Rect, PChar(S), Length(S), nil);
      end;
  end;
var
  OldFontColor, OldBkgndColor: TColor;
begin
  with Grid.Canvas do
    begin
      OldFontColor := Font.Color;
      OldBkgndColor := Brush.Color;
      if gdSelected in State then
        Font.Color := SelColor
      else begin
        Font.Color := FontColor;
        Brush.Color := BkgndColor;
      end;
      DrawCellText;
      Font.Color := OldFontColor;
      Brush.Color := OldBkgndColor;
    end;
end;

procedure DrawComboListItem(Control: TWinControl;
  Str1, Str2: String;
  ImageList: TControlImageList; ImageIndex: Integer;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Bmp, Picture: TBitmap;
  ControlCanvas: TCanvas;
  BmpRect: TRect;
  TxtHeight, TxtWidth, OffsetX, OffsetY: Integer;
begin
  Bmp := nil;
  Picture := nil;
  try
    if Control is TCustomListBox then
      ControlCanvas := (Control as TCustomListBox).Canvas else
      ControlCanvas := (Control as TCustomComboBox).Canvas;

    Bmp := TBitmap.Create;
    Picture := TBitmap.Create;
    Bmp.Width := Rect.Right - Rect.Left + 1;
    Bmp.Height := Rect.Bottom - Rect.Top + 1;
    BmpRect := Bounds(0 ,0, Bmp.Width, Bmp.Height);
    with Bmp.Canvas do
    begin
      Brush := ControlCanvas.Brush;
      FillRect(BmpRect);
      ImageList.GetBitmap(ImageIndex, Picture);
      OffsetX := 2;
      OffsetY := (Bmp.Height - Picture.Height) div 2;
      ImageList.Draw(Bmp.Canvas, OffsetX, OffsetY, ImageIndex);
      Inc(OffsetX, Picture.Width + 2);
      Font := ControlCanvas.Font;
      TxtHeight := TextHeight('0');
      TxtWidth := TextWidth(Str1);
      TextOut(OffsetX, (Bmp.Height - TxtHeight) div 2, Str1);
      Inc(OffsetX, TxtWidth);
      if OffsetX < 2 * Control.ClientWidth div 3 then
        TextOut(2 * Control.ClientWidth div 3, (Bmp.Height - TxtHeight) div 2, Str2);
    end;
    ControlCanvas.CopyRect(Rect, Bmp.Canvas, BmpRect);
  finally
    Bmp.Free;
    Picture.Free;
  end;
end;

const
  FShadowTempl: HRGN = 0;
  ShadowTemplWidth  = 20;

function ShadowTempl: HRGN;
begin
  if FShadowTempl = 0 then
  begin
    FShadowTempl := CreateRectRgn(0, 0, ShadowTemplWidth, ShadowTemplWidth);
    CreateShadowRegion(ShadowTempl, 0, 0, ShadowTemplWidth - 1, ShadowTemplWidth - 1);
  end;
  Result := FShadowTempl;
end;

procedure MakeShadowRegion(ShadowRgn: HRGN);
var
  I, J, CountX, CountY: Integer;
  dx, dy: Integer;
  R: TRect;
begin
  GetRgnBox(ShadowRgn, R);
  with R do
    begin
      OffsetRgn(ShadowTempl, Left, Top);
      CountX := (Right - Left + 1) div ShadowTemplWidth + 1;
      CountY := (Bottom - Top + 1) div ShadowTemplWidth + 1;
      dx := 0;
      dy := 0;
      for I := 0 to CountX - 1 do
        begin
          for J := 0 to CountY - 1 do
            begin
              CombineRgn(ShadowRgn, ShadowRgn, ShadowTempl, RGN_DIFF);
              OffsetRgn(ShadowTempl, 0, ShadowTemplWidth);
              Inc(dy, ShadowTemplWidth);
            end;
          OffsetRgn(ShadowTempl, ShadowTemplWidth, -dy);
          Inc(dx, ShadowTemplWidth);
          dy := 0;
        end;
      OffsetRgn(ShadowTempl, - dx - Left,  - dy - Top);
  end;
end;

procedure SetPageControlImages(PageControl: TPageControl; ImageList: TControlImageList);
var
  I: Integer;
  TCItem: TTCItem;
begin
  SendMessage(PageControl.Handle, TCM_SETIMAGELIST, 0, ImageList.Handle);
  for I := 0 to PageControl.PageCount - 1 do
    begin
      TCItem.Mask := TCIF_IMAGE;
      TCItem.iImage := I;
      SendMessage(PageControl.Handle, TCM_SETITEM, I, LongInt(@TCItem));
    end;
end;

function GetListItemChecked(ListItem: TListItem): Boolean;
begin
  Result := (SendMessage(ListItem.ListView.Handle, LVM_GETITEMSTATE, ListItem.Index, LVIS_STATEIMAGEMASK) shr 12) - 1 <> 0;
end;

procedure SetListItemChecked(ListItem: TListItem; Value: Boolean);
var
  Item: TLVItem;
begin
  Item.statemask := LVIS_STATEIMAGEMASK;
  Item.State := ((Integer(Value) and 1) + 1) shl 12;
  SendMessage(ListItem.ListView.Handle, LVM_SETITEMSTATE, ListItem.Index, Integer(@Item));
end;

procedure SetListViewExStyle(ListView: TCustomListView; Style: Integer);
begin
  SendMessage(ListView.Handle, LVM_FIRST + 54, 0, Style);
end;

type
  TTreeViewHack = class(TCustomTreeView);
  TListViewHack = class(TCustomListView);

function GetTreeNodeFromItem(TreeView: TCustomTreeView; const Item: TTVItem): TTreeNode;
begin
  with Item do
    if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
    else Result := TTreeViewHack(TreeView).Items.GetNode(hItem);
end;

function GetListItemFromItem(ListView: TCustomListView; const Item: TLVItem): TListItem;
begin
  with Item do
    if (mask and LVIF_PARAM) <> 0 then Result := TListItem(lParam)
    else Result := TListViewHack(ListView).Items[IItem];
end;

function VisibleRect(Control: TControl): TRect;
  procedure IntersectParent(ParentControl: TWinControl; var R: TRect);
  var
    Orig: TPoint;
   begin
     if Assigned(ParentControl) then
     begin
       with ParentControl do
       begin
         Orig := ClientOrigin;
         IntersectRect(R, R, Bounds(Orig.X, Orig.Y, Width, Height));
       end;
       IntersectParent(ParentControl.Parent, R)
     end;
  end;
var
  R: TRect;
begin
  with R, Control do
  begin
    TopLeft := ClientToScreen(Point(0, 0));
    BottomRight := ClientToScreen(Point(Width - 1, Height - 1));
  end;
  Result := R;
  IntersectRect(R, R, Bounds(0, 0, Screen.Width, Screen.Height));
  IntersectParent(Control.Parent, R);
  Result := R;
end;

procedure InvalidateClientRect(Control: TWinControl);
var
  R: TRect;
begin
  if Control.HandleAllocated then
  begin
    R := Control.ClientRect;
    InvalidateRect(Control.Handle, @R, False);
  end;
end;

procedure DecreaseHeight(Control: TWinControl; Top, H: Integer);
var
  I: Integer;
  Tmp: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do
  begin
    Tmp := Control.Controls[I];
    if (Tmp.Visible) and (Tmp.Top > Top + H) then
      Tmp.Top := Tmp.Top - H;
  end;
  Control.Height := Control.Height - H;
  if Control.Parent <> nil then
    DecreaseHeight(Control.Parent, Control.Top + Control.Height + H, H);
end;

procedure IncreaseHeight(Control: TWinControl; Top, H: Integer);
var
  I: Integer;
  Tmp: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do
  begin
    Tmp := Control.Controls[I];
    if (Tmp.Visible) and (Tmp.Top >= Top) then
      Tmp.Top := Tmp.Top + H;
  end;
  Control.Height := Control.Height + H;
  if Control.Parent <> nil then
    IncreaseHeight(Control.Parent, Control.Top + Control.Height - H, H);
end;

procedure HideHorizontal(Control: TWinControl; Top, H: Integer);
var
  I: Integer;
  Tmp: TControl;
begin
  for I := 0 to Control.ControlCount - 1 do
  begin
    Tmp := Control.Controls[I];
    if (Tmp.Top >= Top) and (Tmp.Top <= Top + H) then
      Tmp.Visible := False;
  end;
  DecreaseHeight(Control, Top, H);
end;

procedure ShowHorizontal(Control: TWinControl; Top, H: Integer);
var
  I: Integer;
  Tmp: TControl;
begin
  IncreaseHeight(Control, Top, H);
  for I := 0 to Control.ControlCount - 1 do
  begin
    Tmp := Control.Controls[I];
    if (Tmp.Top >= Top) and (Tmp.Top <= Top + H) then
      Tmp.Visible := True;
  end;
end;

procedure ScreenCenter(Form: TForm);
begin
  with Form do
    SetBounds((Screen.Width - Width) div 2, (Screen.Height - Height) div 2, Width, Height);
end;

procedure ForControls(Controls: array of TControl;
  CallBack: TControlCallBack; Data: Pointer);
var
  I: Integer;
begin
  for I := 0 to High(Controls) do
    CallBack(Controls[I], Data);
end;

procedure ForEachControl(Control: TWinControl;
  ControlClass: TControlClass; CallBack: TControlCallBack; Data: Pointer);
var
  I: Integer;
  TmpControl: TControl;
begin
  with Control do
  begin
    for I := 0 to ControlCount - 1 do
    begin
      TmpControl := Controls[I];
      if (TmpControl is ControlClass) then
        CallBack(TmpControl, Data);
      if TmpControl is TWinControl then
        ForEachControl(TWinControl(TmpControl), ControlClass, CallBack, Data);
    end;
  end;
end;

procedure SetEnabled(Controls: array of TControl; Value: Boolean);
  procedure EnableControl(EachControl: TControl; Data: Pointer);
  begin
    EachControl.Enabled := Boolean(Data);
  end;
begin
  ForControls(Controls, @EnableControl, Pointer(Value));
end;

procedure SetVisible(Controls: array of TControl; Value: Boolean);
  procedure VisibleControl(EachControl: TControl; Data: Pointer);
  begin
    EachControl.Visible := Boolean(Data);
  end;
begin
  ForControls(Controls, @VisibleControl, Pointer(Value));
end;

procedure SetEnableColor(Controls: array of TControl; EnableColor: TColor);
  procedure SetColor(EachControl: TControl; Data: Pointer);
  begin
    with TLabel(EachControl) do
      if Enabled then
        Font.Color := TColor(Data)
      else
        Font.Color := clGrayText;
  end;
begin
  ForControls(Controls, @SetColor, Pointer(EnableColor));
end;

procedure AdjustLabelsBounds(Control: TWinControl);
  procedure BroadcastTextChanged(EachControl: TControl; Data: Pointer);
  var
    Msg: TMessage;
  begin
    Msg.Msg := CM_TEXTCHANGED;
    TWinControl(EachControl).Broadcast(Msg);
  end;
begin
  ForEachControl(Control, TWinControl, @BroadcastTextChanged, nil);
end;

procedure SetLabelColors(Control: TWinControl);
  procedure SetColor(EachControl: TControl; Data: Pointer);
  begin
    with EachControl as TLabel do
    begin
      if Assigned(FocusControl) then
        Enabled := FocusControl.Enabled
    end;
  end;
begin
  ForEachControl(Control, TLabel, @SetColor, nil);
end;

procedure UpdateControls(Control: TWinControl);
  procedure UpdateControl(EachControl: TControl; Data: Pointer);
  begin
    with EachControl as TControl do Update;
  end;
begin
  ForEachControl(Control, TControl, @UpdateControl, nil);
end;

procedure SetCursor(NewCursor: TCursor);
begin
  if IsMainThread then
  begin
    ListAdd(Cursors, Pointer(Screen.Cursor));
    Screen.Cursor := NewCursor;
  end;
end;

procedure AppSetCursor(NewCursor: TCursor);
asm
        JMP     SetCursor
end;

procedure RestoreCursor;
begin
  if IsMainThread then
    Screen.Cursor := TCursor(ListRemoveLast(Cursors));
end;

procedure AppRestoreCursor;
asm
        JMP     RestoreCursor
end;

procedure FreeMessageQueue;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

procedure Wait(MSec: Integer);
var
  Ticks: Integer;
begin
  Ticks := GetTickCount;
  while Abs(Integer(GetTickCount) - Ticks) < MSec do 
    Application.ProcessMessages;
end;

function CreateImage(Icon: TMsgDlgType): TImage;
const
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);
begin
  Result := TImage.Create(nil);
  Result.Picture.Icon.Handle := LoadIcon(0, IconIDs[Icon]);
end;

procedure LoadComponentWait(const FileName: string; Instance: TComponent);
begin
  SetCursor(crHourglass);
  try
    LoadComponent(FileName, Instance);
  finally
    RestoreCursor;
  end;
end;

procedure SaveComponentWait(const FileName: string; Instance: TComponent);
begin
  SetCursor(crHourglass);
  try
    SaveComponent(FileName, Instance);
  finally
    RestoreCursor;
  end;
end;

{$IFNDEF _D4_}
function UseRightToLeftReading: Boolean; assembler;
asm
        XOR EAX,EAX
end;

function UseRightToLeftAlignment: Boolean;
asm
        XOR EAX,EAX
end;

function DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  if UseRightToLeftReading then
    Result := DT_RTLREADING
  else
    Result := 0;
end;

function DrawTextBiDiModeFlags(Flags: Longint): Longint;
begin
  Result := Flags;
  { do not change center alignment }
  if UseRightToLeftAlignment then
    if Result and DT_RIGHT = DT_RIGHT then
      Result := Result and not DT_RIGHT { removing DT_RIGHT, makes it DT_LEFT }
    else if not (Result and DT_CENTER = DT_CENTER) then
      Result := Result or DT_RIGHT;
  Result := Result or DrawTextBiDiModeFlagsReadingOnly;
end;
{$ENDIF}

initialization

finalization
  if FShadowTempl <> 0 then DeleteObject(FShadowTempl);

end.
