{*********************************************************}
{*                  VPPRTPRV.PAS 1.03                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{
  This unit contains the visual print preview component.  The display of
  the print preview as well as the navigation is controlled in this component.
  The PrintPreview depends on the print formats (in VpPrtFmt) to generate
  the image that will be displayed to the user.

  VisualPlanIt has a loose definition for starting and stopping reports.
  Basically, the report ends when all the contacts, tasks and dates have
  been displayed.  The print formats know which of these types of elements
  that it is displaying and uses that information in calculating the last
  page.

  It is possible to create print formats that do not have last page (the
  easiest way is to create a print format with a day view and have it
  increment zero days for each page).  This scenario is not yet trapped
  in the printing or in the print preview.

  Scaling is handled simply.  The size of the rectangle in which the print
  format can render is changed to reflect the size.

  The print preview cannot be used as an element of a print format.
}

unit VpPrtPrv;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Graphics,
  Printers,
  VpBase,
  VpMisc,
  VpBaseDS,
  VpSR,
  VpException,
  Menus;

type
  TVpPageChange = procedure (Sender : TObject; NewPage : Integer) of object;

  TVpPPZoomFactor = (zfFitToControl, zfActualSize,
                     zf25Percent, zf33Percent, zf50Percent,
                     zf67Percent, zf75Percent);

  TVpPageInfo = record
    Date     : TDateTime;
    Task     : Integer;
    Contact  : Integer;
    LastPage : Boolean; 
  end;
  PVpPageInfo = ^TVpPageInfo;

  TVpPrintPreview = class (TVpCustomControl)
    private
      FBorderStyle   : TBorderStyle;
      FControlLink   : TVpControlLink;
      FDrawingStyle  : TVpDrawingStyle;
      FCurPage       : Integer;
      FPrinter       : TPrinter;
      RenderBmp      : TBitmap;
      WorkBmp        : TBitmap;                     
      FZoomFactor    : TVpPPZoomFactor;
      FBorderColor   : TColor;
      FOffPageColor  : TColor;
      FPageColor     : TColor;
      FPageInfo      : TList;
      FStartDate     : TDateTime;
      FEndDate       : TDateTime;
      FCurrentFormat : Integer;
      FNeedHScroll   : Boolean;
      FNeedVScroll   : Boolean;
      FScrollX       : Integer;
      FScrollY       : Integer;
      FOnPageChange  : TVpPageChange;
      FDefaultPopup  : TPopupMenu;

    protected
      function  CalculatePageHeight (Printer : TPrinter) : Integer;
      function  CalculatePageWidth (Printer : TPrinter) : Integer;
      procedure ClearPageData;
      procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
                message CM_WANTSPECIALKEY;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure CreateWnd; override;
      procedure DoScroll (var Msg : TWMSCROLL; BarDirection : Integer);
      procedure GeneratePageImage;
      procedure GetLastPage;
      procedure InitHScrollBar (PageSize  : Integer;
                                TotalSize : Integer);
      procedure InitializeDefaultPopup;
      procedure InitVScrollBar (PageSize  : Integer;
                                TotalSize : Integer);
      function  IsPageLoaded (PageNum : Integer) : Boolean;
      procedure KeyDown (var Key: Word; Shift: TShiftState); override;
      procedure Loaded; override;
      function  LoadPage (PageNum   : Integer;
                          StartDate : TDateTime;
                          EndDate   : TDateTime) : Integer;
      procedure Paint; override;
      procedure PopupFirstPage (Sender : TObject);
      procedure PopupLastPage (Sender : TObject);
      procedure PopupNextPage (Sender : TObject);
      procedure PopupPrevPage (Sender : TObject);
      procedure RemoveHScrollbar;
      procedure RemoveVScrollbar;
      procedure SetBorderColor (const v : TColor);
      procedure SetBorderStyle (const v : TBorderStyle);
      procedure SetControlLink (const v : TVpControlLink);
      procedure SetCurPage (const v : Integer);
      procedure SetDrawingStyle (const v : TVpDrawingStyle);
      procedure SetEndDate (const v : TDateTime);
      procedure SetOffPageColor (const v : TColor);
      procedure SetPageColor (const v : TColor);
      procedure SetPrinter (const v : TPrinter);
      procedure SetStartDate (const v : TDateTime);
      procedure SetScrollBars;
      procedure SetZoomFactor (const v : TVpPPZoomFactor);

      procedure VpPrintFormatChanged (var Msg : TMessage); message Vp_PrintFormatChanged;
      procedure WMEraseBackground (var Msg : TWMERASEBKGND); message WM_ERASEBKGND;
      procedure WMHScroll (var Msg : TWMSCROLL ); message WM_HSCROLL;
      procedure WMVScroll (var Msg : TWMSCROLL ); message WM_VSCROLL;
      procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;

    public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;

      procedure FirstPage;
      function  IsFirstPage : Boolean;
      function  IsLastPage : Boolean;
      procedure LastPage;
      procedure NextPage;
      procedure PrevPage;
      procedure ForceUpdate;
      procedure SetBounds (ALeft, ATop, AWidth, AHeight: Integer); override;

      property Printer : TPrinter read FPrinter write SetPrinter;
      property DestPrinter : TPrinter read FPrinter write SetPrinter;    
    published
      property BorderColor : TColor read FBorderColor write SetBorderColor
               default clBlack;
      property BorderStyle : TBorderStyle
               read FBorderStyle write SetBorderStyle default bsSingle;
      property ControlLink : TVpControlLink
               read FControlLink write SetControlLink;
      property CurPage : Integer read FCurPage write SetCurPage;
//      property DestPrinter : TPrinter read FPrinter write SetPrinter;  
      property DrawingStyle : TVpDrawingStyle
               read FDrawingStyle write SetDrawingStyle default ds3d;
      property EndDate : TDateTime read FEndDate write SetEndDate;
      property OffPageColor : TColor read FOffPageColor write SetOffPageColor
               default clSilver;
      property PageColor : TColor read FPageColor write SetPageColor
               default clWhite;
      property StartDate : TDateTime read FStartDate write SetStartDate;
      property ZoomFactor : TVpPPZoomFactor
               read FZoomFactor write SetZoomFactor default zfFitToControl;

      property OnPageChange : TVpPageChange
               read FOnPageChange write FOnPageChange;

      property Anchors;
      property Align;
      property Constraints;
      property Cursor;
      property DragCursor;
      property DragMode;
      property Enabled;
      property HelpContext;
      property Hint;
      property Parent;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabStop;
      property TabOrder;
      property Visible;

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
  end;

implementation

constructor TVpPrintPreview.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  if not (AOwner is TWinControl) then
    raise EVpPrintPreviewError.Create (RSOwnerNotWinCtrl);

  Parent         := TWinControl (AOwner);

  RenderBmp      := TBitmap.Create;
  WorkBmp        := TBitmap.Create;
  FPageInfo      := TList.Create;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  FNeedHScroll   := False;
  FNeedVScroll   := False;

  FCurrentFormat := -1;
  FBorderStyle   := bsSingle;
  FDrawingStyle  := ds3d;
  FZoomFactor    := zfFitToControl;
  FOffPageColor  := clSilver;
  FPageColor     := clWhite;
  FBorderColor   := clBlack;
  FCurPage       := 0;
  FStartDate     := Now;
  FEndDate       := Now + 7;
  FScrollX       := 0;
  FScrollY       := 0;
  Height         := 225;
  Width          := 169;
  FPrinter       := Printer;
end;

destructor TVpPrintPreview.Destroy;
begin
  if (HandleAllocated) and Assigned (FControlLink) then
    FControlLink.Printer.DeregisterWatcher (Handle);

  ClearPageData;

  RenderBmp.Free;
  WorkBmp.Free;

  FPageInfo.Free;

  FDefaultPopup.Free;

  inherited Destroy;
end;

function TVpPrintPreview.CalculatePageHeight (Printer : TPrinter) : Integer;
var
  ScreenPPI  : Integer;
  PrinterPPI : Integer;

begin
  ScreenPPI  := GetDeviceCaps (Canvas.Handle,  LOGPIXELSY);
  PrinterPPI := GetDeviceCaps (Printer.Handle, LOGPIXELSY);

  if PrinterPPI <> 0 then
    Result := Round (ScreenPPI / PrinterPPI * Printer.PageHeight)
  else
    Result := ScreenPPI * Printer.PageHeight;
end;

function TVpPrintPreview.CalculatePageWidth (Printer : TPrinter) : Integer;
var
  ScreenPPI  : Integer;
  PrinterPPI : Integer;

begin
  ScreenPPI  := GetDeviceCaps (Canvas.Handle,  LOGPIXELSX);
  PrinterPPI := GetDeviceCaps (Printer.Handle, LOGPIXELSX);

  if PrinterPPI <> 0 then
    Result := Round (ScreenPPI / PrinterPPI * Printer.PageWidth)
  else
    Result := ScreenPPI * Printer.PageHeight;
end;

procedure TVpPrintPreview.ClearPageData;
var
  i : Integer;

begin
  for i := FPageInfo.Count - 1 downto 0 do begin
    if Assigned (FPageInfo[i]) then
      FreeMem (FPageInfo[i]);
    FPageInfo.Delete (i);
  end;
  CurPage := 0;
end;

procedure TVpPrintPreview.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;

procedure TVpPrintPreview.CreateParams (var Params : TCreateParams);
begin
  inherited CreateParams (Params);

  with Params do begin
    Style := Style or WS_TABSTOP;
    if FNeedHScroll then
      Style := Longint (Style) or WS_HSCROLL;
    if FNeedVScroll then
      Style := Longint (Style) or WS_VSCROLL;
  end;
end;

procedure TVpPrintPreview.CreateWnd;
begin
  if (HandleAllocated) and (Assigned (FControlLink)) then
    FControlLink.Printer.DeregisterWatcher (Handle);

  inherited CreateWnd;

  if Assigned (FControlLink) then
    FControlLink.Printer.RegisterWatcher (Handle);
end;

procedure TVpPrintPreview.DoScroll (var Msg          : TWMSCROLL;
                                        BarDirection : Integer);
var
  ScrollBarInfo : TScrollInfo;

begin
  Msg.Result := 0;
  ScrollBarInfo.cbSize := SizeOf (TscrollInfo);
  ScrollBarInfo.fMask := SIF_ALL;
  GetScrollInfo (Handle, BarDirection, ScrollBarInfo);
  ScrollBarInfo.fMask := SIF_POS;
  case Msg.ScrollCode of
    SB_TOP           : ScrollBarInfo.nPos := ScrollBarInfo.nMin;
    SB_BOTTOM        : ScrollBarInfo.nPos := ScrollBarInfo.nMax;
    SB_LINEUP        : Dec (ScrollBarInfo.nPos, 1);
    SB_LINEDOWN      : Inc (ScrollBarInfo.nPos, 1);
    SB_PAGEUP        : Dec (ScrollBarInfo.nPos, ScrollBarInfo.nPage );
    SB_PAGEDOWN      : Inc (ScrollBarInfo.nPos, ScrollBarInfo.nPage);
    SB_THUMBTRACK,
    SB_THUMBPOSITION : ScrollBarInfo.nPos := Msg.Pos;
    SB_ENDSCROLL     : Exit;
  end;

  ScrollBarInfo.fMask := SIF_POS;
  if ScrollBarInfo.nPos < ScrollBarInfo.nMin Then
    ScrollBarInfo.nPos := ScrollBarInfo.nMin;
  if ScrollBarInfo.nPos + Integer (ScrollBarInfo.nPage) >
     ScrollBarInfo.nMax Then
    ScrollBarInfo.nPos := ScrollBarInfo.nMax - Integer (ScrollBarInfo.nPage);

  case BarDirection of
    SB_HORZ : FScrollX := ScrollBarInfo.nPos;
    SB_VERT : FScrollY := ScrollBarInfo.nPos;
  end;

  SetScrollInfo (Handle, BarDirection, ScrollBarInfo, True);
  Invalidate;
end;

procedure TVpPrintPreview.FirstPage;
begin
  CurPage := 0;
end;

procedure TVpPrintPreview.ForceUpdate;
begin
  if not Assigned (FControlLink) then
    Exit;

  if not Assigned (FControlLink.Printer) then
    Exit;

//  if FCurrentFormat <> FControlLink.Printer.CurFormat then begin     
    ClearPageData;
    GeneratePageImage;
    Invalidate;
//  end;                                                               
end;

procedure TVpPrintPreview.GeneratePageImage;
var
  LastPage   : Boolean;
  UseDate    : TDateTime;
  UseContact : Integer;
  UseTask    : Integer;

begin
  if not Assigned (FControlLink) then
    Exit;

  if not Assigned (FControlLink.Printer) then
    Exit;

  if (FControlLink.Printer.PrintFormats.Count = 0) or                    
     (FControlLink.Printer.CurFormat < 0) then                           
    Exit;                                                                

  FCurrentFormat := FControlLink.Printer.CurFormat;

  if (FPrinter <> nil) and
     ((RenderBmp.Width = 0) or (RenderBmp.Height = 0)) then begin
    RenderBmp.Width := CalculatePageWidth (FPrinter);
    RenderBmp.Height := CalculatePageHeight (FPrinter);
  end else if (FPrinter = nil) and
              ((RenderBmp.Width = 0) or (RenderBmp.Height = 0)) then begin
    RenderBmp.Width := ClientWidth;
    RenderBmp.Height := ClientHeight;
  end;

  if not IsPageLoaded (CurPage) then
    LoadPage (CurPage, FStartDate, FEndDate);

  RenderBmp.Canvas.Brush.Color := FPageColor;
  RenderBmp.Canvas.Pen.Color := FBorderColor;
  RenderBmp.Canvas.FillRect (Rect (0, 0, RenderBmp.Width, RenderBmp.Height));

  if not IsPageLoaded (CurPage) then
    FControlLink.Printer.PaintToCanvasRect (RenderBmp.Canvas,
                                            Rect (0, 0,
                                                  RenderBmp.Width,
                                                  RenderBmp.Height),
                                            StartDate)
  else begin
    UseDate    := PVpPageInfo (FPageInfo[CurPage]).Date;
    UseContact := PVpPageInfo (FPageInfo[CurPage]).Contact;
    UseTask    := PVpPageInfo (FPageInfo[CurPage]).Task;
    FControlLink.Printer.RenderPage (RenderBmp.Canvas,
                                     Rect (0, 0, RenderBmp.Width,
                                           RenderBmp.Height),
                                     CurPage,
                                     UseDate,
                                     EndDate,
                                     UseContact,
                                     UseTask,
                                     LastPage);
  end;
  SetScrollBars;
end;

procedure TVpPrintPreview.GetLastPage;
var
  i : Integer;

begin
  i := FPageInfo.Count - 1;
  while (not PVpPageInfo (FPageInfo[i]).LastPage) and
        (i < FPageInfo.Count) do begin
    Inc (i);
    LoadPage (i, StartDate, EndDate);
  end;
end;

procedure TVpPrintPreview.InitHScrollBar (PageSize  : Integer;
                                          TotalSize : Integer);
var
  ScrollBarInfo : TScrollInfo;

begin
  FNeedHScroll := True;

  ScrollBarInfo.cbSize    := SizeOf (TScrollInfo);
  ScrollBarInfo.fMask     := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollBarInfo.nMin      := 0;

  ScrollBarInfo.nMax      := TotalSize;
  ScrollBarInfo.nPage     := PageSize;
  ScrollBarInfo.nPos      := 0;
  ScrollBarInfo.nTrackPos := 0;
  SetScrollInfo (Handle, SB_HORZ, ScrollBarInfo, True);
end;

procedure TVpPrintPreview.InitializeDefaultPopup;
var
  NewItem : TMenuItem;

begin
  if RSPrintPrvPrevPage <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSPrintPrvPrevPage;
    NewItem.OnClick := PopupPrevPage;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSPrintPrvNextPage <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSPrintPrvNextPage;
    NewItem.OnClick := PopupNextPage;
    NewItem.Tag := 2;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSPrintPrvFirstPage <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSPrintPrvFirstPage;
    NewItem.OnClick := PopupFirstPage;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSPrintPrvLastPage <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSPrintPrvLastPage;
    NewItem.OnClick := PopupLastPage;
    NewItem.Tag := 2;
    FDefaultPopup.Items.Add (NewItem);
  end;
end;

procedure TVpPrintPreview.InitVScrollBar (PageSize  : Integer;
                                          TotalSize : Integer);
var
  ScrollBarInfo : TScrollInfo;

begin
  FNeedVScroll := True;

  ScrollBarInfo.cbSize     := SizeOf (TScrollInfo);
  ScrollBarInfo.fMask      := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollBarInfo.nMin       := 0;
  ScrollBarInfo.nMax       := TotalSize;
  ScrollBarInfo.nPage      := PageSize;
  ScrollBarInfo.nTrackPos  := 0;
  ScrollBarInfo.nPos       := 0;
  SetScrollInfo (Handle, SB_VERT, ScrollBarInfo, True);
end;

function TVpPrintPreview.IsFirstPage : Boolean;
begin
  Result := CurPage = 0;
end;

function TVpPrintPreview.IsLastPage : Boolean;
begin
  if FPageInfo.Count = 0 then
    Result := True
  else if CurPage < FPageInfo.Count then
    Result := PVpPageInfo (FPageInfo[CurPage]).LastPage
  else begin
    GetLastPage;
    Result := PVpPageInfo (FPageInfo[CurPage]).LastPage;
  end;
end;

function TVpPrintPreview.IsPageLoaded (PageNum : Integer) : Boolean;
begin
  Result := PageNum < FPageInfo.Count;
end;


procedure TVpPrintPreview.KeyDown (var Key: Word; Shift: TShiftState);
var
  PopupPoint : TPoint;

begin
  case Key of
    VK_LEFT, VK_PRIOR  :
      if ssCtrl in Shift then
        FirstPage
      else
        PrevPage;
    VK_RIGHT, VK_NEXT :
      if ssCtrl in Shift then
        LastPage
      else
        NextPage;
    $5A : {z}
      if ssCtrl in Shift then begin
        if ZoomFactor = High (FZoomFactor) then
          ZOomFactor := Low (FZoomFactor)
        else
          ZoomFactor := Succ (FZoomFactor);
      end else if ssShift in Shift then begin
        if ZoomFactor = Low (FZoomFactor) then
          ZOomFactor := High (FZoomFactor)
        else
          ZoomFactor := Pred (FZoomFactor);
      end;
    $46 : {f}
      if (ssCtrl in Shift) and Assigned (ControlLink) then begin
        if ControlLink.Printer.CurFormat < ControlLink.Printer.PrintFormats.Count - 1 then
          ControlLink.Printer.CurFormat := ControlLink.Printer.CurFormat + 1
        else
          ControlLink.Printer.CurFormat := 0;
      end else if (ssShift in Shift) and Assigned (ControlLink) then begin
        if ControlLink.Printer.CurFormat > 0 then
          ControlLink.Printer.CurFormat := ControlLink.Printer.CurFormat - 1
        else
          ControlLink.Printer.CurFormat := ControlLink.Printer.PrintFormats.Count - 1;
      end;
    VK_TAB   :
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem (GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem (GetParent(Handle), Handle, True));
    VK_F10   :
      if (ssShift in Shift) and not (Assigned (PopupMenu)) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
    VK_APPS  :
      if not Assigned (PopupMenu) then begin
        PopupPoint := GetClientOrigin;
        FDefaultPopup.Popup (PopupPoint.x + 10,
                             PopupPoint.y + 10);
      end;
    else
      inherited;
  end;
end;

procedure TVpPrintPreview.LastPage;
var
  i : Integer;

begin
  if CurPage < FPageInfo.Count then begin
    i := CurPage;
    if (not PVpPageInfo (FPageInfo[i]).LastPage) and
       (i < FPageInfo.Count) then
      Inc (i);
    if (not PVpPageInfo (FPageInfo[i]).LastPage) then
      GetLastPage;
  end else
    GetLastPage;
  CurPage := FPageInfo.Count - 1;
end;

procedure TVpPrintPreview.Loaded;
begin
  inherited Loaded;

  DestPrinter := Printer;
  GeneratePageImage; 
end;

function TVpPrintPreview.LoadPage (PageNum   : Integer;
                                    StartDate : TDateTime;
                                    EndDate   : TDateTime) : Integer;
{ Loads the requested page.  Returns the last page loaded.  If the
  return value is less than the requested page, the requested page
  is past the last page }
var
  i         : Integer;
  LastPage  : Boolean;
  PPageInfo : PVpPageInfo;
  ADate     : TDateTime;
  ATask     : Integer;
  AContact  : Integer;

begin
  Result := PageNum;
  if PageNum < FPageInfo.Count then
    Exit;

  if not Assigned (FControlLink) then
    Exit;

  if not Assigned (FControlLink.Printer) then
    Exit;

  i := FPageInfo.Count - 1;
  LastPage := False;

  if FPageInfo.Count = 0 then begin
    GetMem (PPageInfo, SizeOf (TVpPageInfo));
    PPageInfo.Date := StartDate;
    PPageInfo.Contact := 0;
    PPageInfo.Task := 0;
    PPageInfo.LastPage := False;
    FPageInfo.Add (PPageInfo);
    i := 0;
  end;

  PPageInfo := PVpPageInfo (FPageInfo[i]);
  ADate    := PPageInfo.Date;
  AContact := PPageInfo.Contact;
  ATask    := PPageInfo.Task;

  { The only way to see how the pages are going to increment is to render
    them and get the return information }

  while (i <= PageNum) and (not LastPage) do begin

    FControlLink.Printer.RenderPage (RenderBmp.Canvas,
                                     Rect (0, 0, RenderBmp.Width,
                                           RenderBmp.Height),
                                     i + 1,
                                     ADate,
                                     FEndDate,
                                     AContact,
                                     ATask,
                                     LastPage);
    Result := i;
    GetMem (PPageInfo, SizeOf (TVpPageInfo));
    PPageInfo.Date     := ADate;
    PPageInfo.Task     := ATask;
    PPageInfo.Contact  := AContact;
    PPageInfo.LastPage := LastPage;
    FPageInfo.Add (PPageInfo);
    Inc (i);
  end;
end;

procedure TVpPrintPreview.NextPage;
begin
  if CurPage < FPageInfo.Count then begin
    if not PVpPageInfo (FPageInfo[CurPage]).LastPage then
      CurPage := CurPage + 1;
  end else
    CurPage := CurPage + 1;
end;

procedure TVpPrintPreview.Paint;
var
  RealWidth  : Integer;
  RealHeight : Integer;

  procedure Clear;
  begin
    if RenderBmp.Width > ClientWidth then
      WorkBmp.Width := RenderBmp.Width
    else
      WorkBmp.Width := ClientWidth;
    if RenderBmp.Height > ClientHeight then
      WorkBmp.Height := RenderBmp.Height
    else
      WorkBmp.Height := ClientHeight;
    RealWidth  := ClientWidth;
    RealHeight := ClientHeight;

    WorkBmp.Canvas.Brush.Color := FOffPageColor;
    WorkBmp.Canvas.FillRect (ClientRect);
    WorkBmp.Canvas.Brush.Color := FPageColor;
    WorkBmp.Canvas.Pen.Color := FBorderColor;
  end;

  procedure DrawBorders;
  begin
    if FBorderStyle = bsSingle then begin
      if FDrawingStyle = dsFlat then begin
        { draw an outer and inner bevel }
        DrawBevelRect (WorkBmp.Canvas,
                       Rect (ClientRect.Left,
                             ClientRect.Top,
                             ClientRect.Right - 1,
                             ClientRect.Bottom - 1),
                       clBtnShadow,
                       clBtnHighlight);
        DrawBevelRect (WorkBmp.Canvas,
                       Rect (ClientRect.Left + 1,
                             ClientRect.Top + 1,
                             ClientRect.Right - 2,
                             ClientRect.Bottom - 2),
                       clBtnHighlight,
                       clBtnShadow);
      end else if FDrawingStyle = ds3d then begin
        { draw a 3d bevel }
        DrawBevelRect (WorkBmp.Canvas,
                       Rect (ClientRect.Left,
                             ClientRect.Top,
                             ClientRect.Right - 1,
                             ClientRect.Bottom - 1),
                       clBtnShadow,
                       clBtnHighlight);
        DrawBevelRect (WorkBmp.Canvas,
                       Rect (ClientRect.Left + 1,
                             ClientRect.Top + 1,
                             ClientRect.Right - 2,
                             ClientRect.Bottom - 2),
                       cl3DDkShadow,
                       clBtnFace);
      end;
    end;
  end;

  procedure AddPageBorder;
  var
    w : Integer;
    h : Integer;
  begin
    if RealWidth > ClientWidth - 2 then
      w := ClientWidth - 2
    else
      w := RealWidth;
    if RealHeight > ClientHeight - 2 then
      h := ClientHeight - 2
    else
      h := RealHeight;
    WorkBmp.Canvas.Pen.Color := FBorderColor;
    WorkBmp.Canvas.MoveTo (2, 2);
    WorkBmp.Canvas.LineTo (2, h);
    WorkBmp.Canvas.MoveTo (2, 2);
    WorkBmp.Canvas.LineTo (w, 2);

    if (RealWidth < ClientWidth - 2) then begin
      WorkBmp.Canvas.MoveTo (RealWidth, 2);
      WorkBmp.Canvas.LineTo (RealWidth, h + 1);
    end;

    if (RealHeight < ClientHeight - 2) then begin
      WorkBmp.Canvas.MoveTo (2, RealHeight);
      WorkBmp.Canvas.LineTo (w + 1, RealHeight);
    end;
  end;

  function GetAspectRectangle : TRect;
  var
    ScaleX  : Extended;
    ScaleY  : Extended;
    Offset1 : Integer;
    Offset2 : Integer;

  begin
    Offset1 := 3;
    Offset2 := 3;

    if Assigned (FPrinter) then begin
      if ClientWidth - (Offset1 + Offset2) <> 0 then
        ScaleX := FPrinter.PageWidth  / (ClientWidth  - (Offset1 + Offset2))
      else
        ScaleX := 1;
      if ClientHeight - (Offset1 + Offset2) <> 0 then
        ScaleY := FPrinter.PageHeight / (ClientHeight - (Offset1 + Offset2))
      else
        ScaleY := 1;
      if ScaleX > ScaleY then
        Result := Rect (Offset1, Offset1, ClientWidth - Offset2,
                        Round (FPrinter.PageHeight / ScaleX))
      else
        Result := Rect (Offset1, Offset1,
                        Round (FPrinter.PageWidth / ScaleY),
                        ClientHeight - Offset2);
    end else
      Result := Rect (3, 3, ClientWidth, ClientHeight);
  end;

  procedure DrawPreview;
  var
    AspectRect : TRect;
    WorkHeight : Integer;
    WorkWidth  : Integer;

  begin
    if FControlLink.Printer.PrintFormats.Count <= 0 then
      Exit;

    if CurPage > FPageInfo.Count then
      GeneratePageImage;

    case FZoomFactor of
      zfFitToControl : begin
        AspectRect := GetAspectRectangle;
        WorkBmp.Canvas.CopyRect (AspectRect, RenderBmp.Canvas,
                                 Rect (0, 0,
                                       RenderBmp.Width, RenderBmp.Height));
        RealWidth  := AspectRect.Right - AspectRect.Left + 3;
        RealHeight := AspectRect.Bottom - AspectRect.Top + 3;
      end;

      zf25Percent : begin
        WorkWidth  := Round (RenderBmp.Width div 4);
        WorkHeight := Round (RenderBmp.Height div 4);
        if WorkHeight > ClientHeight - 3 then
          WorkHeight := ClientHeight - 3;
        if WorkWidth > ClientWidth - 3 then
          WorkWidth := ClientWidth - 3;
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       WorkWidth,
                                       WorkHeight), 
                                 RenderBmp.Canvas,
                                 Rect (Round (FScrollX * 4),
                                       Round (FScrollY * 4),
                                       Round ((WorkWidth + FScrollX) * 4),
                                       Round ((WorkHeight + FScrollY) * 4)));
        RealWidth  := RenderBmp.Width div 4;
        RealHeight := RenderBmp.Height div 4;
      end;

      zf33Percent : begin
        WorkWidth  := Round (RenderBmp.Width div 3);
        WorkHeight := Round (RenderBmp.Height div 3);
        if WorkHeight > ClientHeight - 3 then
          WorkHeight := ClientHeight - 3;
        if WorkWidth > ClientWidth - 3 then
          WorkWidth := ClientWidth - 3;
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       WorkWidth,
                                       WorkHeight),
                                 RenderBmp.Canvas,
                                 Rect (Round (FScrollX * 3),
                                       Round (FScrollY * 3),
                                       Round ((WorkWidth + FScrollX) * 3),
                                       Round ((WorkHeight + FScrollY) * 3)));
        RealWidth  := RenderBmp.Width div 3;
        RealHeight := RenderBmp.Height div 3;
      end;

      zf50Percent : begin
        WorkWidth  := Round (RenderBmp.Width div 2);
        WorkHeight := Round (RenderBmp.Height div 2);
        if WorkHeight > ClientHeight - 3 then
          WorkHeight := ClientHeight - 3;
        if WorkWidth > ClientWidth - 3 then
          WorkWidth := ClientWidth - 3;
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       WorkWidth,
                                       WorkHeight),
                                 RenderBmp.Canvas,
                                 Rect (Round (FScrollX * 2),
                                       Round (FScrollY * 2),
                                       Round ((WorkWidth + FScrollX) * 2),
                                       Round ((WorkHeight + FScrollY) * 2)));
        RealWidth  := RenderBmp.Width div 2;
        RealHeight := RenderBmp.Height div 2;
      end;

      zf67Percent : begin
        WorkWidth  := Round (RenderBmp.Width * 0.67);
        WorkHeight := Round (RenderBmp.Height * 0.67);
        if WorkHeight > ClientHeight - 3 then
          WorkHeight := ClientHeight - 3;
        if WorkWidth > ClientWidth - 3 then
          WorkWidth := ClientWidth - 3;
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       WorkWidth,
                                       WorkHeight),
                                 RenderBmp.Canvas,
                                 Rect (Round (FScrollX * 1.5),
                                       Round (FScrollY * 1.5),
                                       Round ((WorkWidth + FScrollX) * 1.5),
                                       Round ((WorkHeight + FScrollY) * 1.5)));

        RealWidth  := Round (RenderBmp.Width * 0.67);
        RealHeight := Round (RenderBmp.Height * 0.67);
      end;

      zf75Percent : begin
        WorkWidth  := Round (RenderBmp.Width * 0.75);
        WorkHeight := Round (RenderBmp.Height * 0.75);
        if WorkHeight > ClientHeight - 3 then
          WorkHeight := ClientHeight - 3;
        if WorkWidth > ClientWidth - 3 then
          WorkWidth := ClientWidth - 3;
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       WorkWidth,
                                       WorkHeight),
                                 RenderBmp.Canvas,
                                 Rect (Round (FScrollX * 1.33333),
                                       Round (FScrollY * 1.33333),
                                       Round ((WorkWidth + FScrollX) * 1.33333),
                                       Round ((WorkHeight + FScrollY) * 1.33333)));
        RealWidth  := Round (RenderBmp.Width * 0.75);
        RealHeight := Round (RenderBmp.Height * 0.75);
      end;

      zfActualSize : begin
        WorkBmp.Canvas.CopyRect (Rect (3, 3,
                                       ClientWidth - 3, ClientHeight - 3),
                                       RenderBmp.Canvas,
                                       Rect (FScrollX,
                                             FScrollY,
                                             FScrollX + ClientWidth - 3,
                                             FScrollY + ClientHeight - 3));
        RealWidth  := RenderBmp.Width;
        RealHeight := RenderBmp.Height;
      end;
    end;
  end;

  procedure RenderImage;
  begin
    Canvas.CopyRect (ClientRect, WorkBmp.Canvas, ClientRect);
  end;

begin
  try
    Clear;
    DrawBorders;
    if FControlLink = nil then
      Exit;
    DrawPreview;
    AddPageBorder;
  finally
    RenderImage;
  end;
end;

procedure TVpPrintPreview.PrevPage;
begin
  if FCurPage > 0 then
    CurPage := CurPage - 1;
end;

procedure TVpPrintPreview.PopupFirstPage (Sender : TObject);
begin
  FirstPage;
end;

procedure TVpPrintPreview.PopupLastPage (Sender : TObject);
begin
  LastPage;
end;

procedure TVpPrintPreview.PopupNextPage (Sender : TObject);
begin
  NextPage;
end;

procedure TVpPrintPreview.PopupPrevPage (Sender : TObject);
begin
  PrevPage;
end;

procedure TVpPrintPreview.RemoveHScrollbar;
var
  Style : Integer;

begin
  FNeedHScroll := False;
  Style := GetWindowLong (Handle, GWL_STYLE);
  if ((Style and WS_HSCROLL) <> 0) then begin
    SetWindowLong (Handle, GWL_STYLE, Style and not WS_HSCROLL);
    RecreateWnd;
  end;
end;

procedure TVpPrintPreview.RemoveVScrollbar;
var
  Style : Integer;

begin
  FNeedVScroll := False;
  Style := GetWindowLong (Handle, GWL_STYLE);
  if ((Style and WS_VSCROLL) <> 0) then begin
    SetWindowLong (Handle, GWL_STYLE, Style and not WS_VSCROLL);
    RecreateWnd; 
  end;
end;

procedure TVpPrintPreview.SetBorderColor (const v : TColor);
begin
  if v <> FBorderColor then begin
    FBorderColor := v;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetBorderStyle (const v : TBorderStyle);
begin
  if v <> FBorderStyle then begin
    FBorderStyle := v;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetBounds (ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds (aLeft, ATop, AWidth, AHeight);
  SetScrollBars;
end;

procedure TVpPrintPreview.SetControlLink (const v : TVpControlLink);
begin
  if FControlLink <> v then begin
    if Assigned (FControlLink) then
      FControlLink.Printer.DeregisterWatcher (Handle);
    FControlLink := v;
    if Assigned (FControlLink) then
      FControlLink.Printer.RegisterWatcher (Handle);  
    ClearPageData;
    GeneratePageImage;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetCurPage (const v : Integer);
begin
  if (v >= 0) and (v <> FCurPage) then begin
    FCurPage := v;
    if not (csDestroying in ComponentState) then begin
      GeneratePageImage;
      Invalidate;
      if Assigned (FOnPageChange) then
        FOnPageChange (Self, FCurPage);
    end;
  end;
end;

procedure TVpPrintPreview.SetDrawingStyle (const v : TVpDrawingStyle);
begin
  if v <> FDrawingStyle then begin
    FDrawingStyle := v;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetEndDate (const v : TDateTime);
begin
  if v <> FEndDate then begin
    FEndDate := v; 
    ClearPageData;
    GeneratePageImage;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetOffPageColor (const v : TColor);
begin
  if v <> FOffPageColor then begin
    FOffPageColor := v;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetPageColor (const v : TColor);
begin
  if v <> FPageColor then begin
    FPageColor := v;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetPrinter (const v : TPrinter);
begin
  if FPrinter <> v then begin
    FPrinter := v;
    RenderBmp.Width := CalculatePageWidth (v);
    RenderBmp.Height := CalculatePageHeight (v);
    ClearPageData;
    GeneratePageImage;
  end;
end;

procedure TVpPrintPreview.SetScrollBars;
var
  RealWidth     : Integer;
  RealHeight    : Integer;
  Style         : Integer;
  NeedRecreate  : Boolean;

begin
  if csDesigning in ComponentState then
    Exit;
    
  FScrollX := 0;
  FScrollY := 0;
  RealHeight := ClientHeight;
  RealWidth := ClientWidth;                
  case ZoomFactor of
    zfActualSize : begin
      RealHeight := RenderBmp.Height;
      RealWidth  := RenderBmp.Width;
    end;
    zfFitToControl : begin
      RealHeight := ClientHeight - 4;
      RealWidth  := ClientWidth - 4;
    end;
    zf25Percent : begin
      RealHeight := RenderBmp.Height div 4;
      RealWidth  := RenderBmp.Width div 4;
    end;
    zf33Percent : begin
      RealHeight := RenderBmp.Height div 3;
      RealWidth  := RenderBmp.Width div 3;
    end;
    zf50Percent : begin
      RealHeight := RenderBmp.Height div 2;
      RealWidth  := RenderBmp.Width div 2;
    end;
    zf67Percent : begin
      RealHeight := Round (RenderBmp.Height * 0.67);
      RealWidth  := Round (RenderBmp.Width * 0.67);
    end;
    zf75Percent : begin
      RealHeight := Round (RenderBmp.Height * 0.75);
      RealWidth  := Round (RenderBmp.Width * 0.75);
    end;
  end;

  if (RealWidth > ClientWidth) or (RealHeight > ClientHeight) then begin
    NeedRecreate := False;
    Style := GetWindowLong (Handle, GWL_STYLE);
    if ((Style and WS_HSCROLL) = 0) and (RealWidth > ClientWidth) then begin
      Style := Style or WS_HSCROLL;
      FNeedHScroll := True;
      NeedRecreate := True;
    end;
    if ((Style and WS_VSCROLL) = 0) and (RealHeight > ClientHeight) then begin
      Style := Style or WS_VSCROLL;
      FNeedVScroll := True;
      NeedRecreate := True;
    end;
    if NeedRecreate then begin
      SetWindowLong (Handle, GWL_STYLE, Style);
      RecreateWnd; 
    end;
  end;

  if (RealWidth > ClientWidth) then
    InitHScrollBar (ClientWidth, RealWidth)
  else
    RemoveHScrollbar;

  if (RealHeight > ClientHeight) then
    InitVScrollBar (ClientHeight, RealHeight)
  else
    RemoveVScrollbar;
end;

procedure TVpPrintPreview.SetStartDate (const v : TDateTime);
begin
  if v <> FStartDate then begin
    FStartDate := v;
    ClearPageData;
    GeneratePageImage;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.SetZoomFactor (const v : TVpPPZoomFactor);
begin
  if v <> FZoomFactor then begin
    FZoomFactor := v;
    SetScrollBars;
    Invalidate;
  end;
end;

procedure TVpPrintPreview.VpPrintFormatChanged (var Msg : TMessage);
begin
  ForceUpdate;
end;

procedure TVpPrintPreview.WMEraseBackground (var Msg : TWMERASEBKGND);
begin
  Msg.Result := 1;
end;

procedure TVpPrintPreview.WMVScroll (var Msg : TWMSCROLL);
begin
  DoScroll (Msg, SB_VERT);
end;

procedure TVpPrintPreview.WMHScroll (var Msg : TWMSCROLL);
begin
  DoScroll (Msg, SB_HORZ);
end;

procedure TVpPrintPreview.WMRButtonDown(var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;
  i            : Integer;

begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not focused then
      SetFocus;
    ClientOrigin := GetClientOrigin;

    for i := 0 to FDefaultPopup.Items.Count - 1 do begin
      FDefaultPopup.Items[i].Enabled := True;
      if (FDefaultPopup.Items[i].Tag = 1) and (IsFirstPage) then
        FDefaultPopup.Items[i].Enabled := False;
      if (FDefaultPopup.Items[i].Tag = 2) and (IsLastPage) then
        FDefaultPopup.Items[i].Enabled := False;
    end;

    FDefaultPopup.Popup (Msg.XPos + ClientOrigin.x,
                         Msg.YPos + ClientOrigin.y);
  end;
end;


end.

