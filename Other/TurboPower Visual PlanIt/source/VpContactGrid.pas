{*********************************************************}
{*                VPCONTACTGRID.PAS 1.03                 *}
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

{$I Vp.INC}

unit VpContactGrid;

interface

uses
  Windows, Classes, Graphics, Controls, ComCtrls, ExtCtrls, Messages, StdCtrls,
  VpBase, VpBaseDS, VpMisc, VpData, VpConst, VpSR, VpCanvasUtils, Menus;

const
  MaxColumns = 100;  { An arbitrary number representing the maximum number of }
                     { columns allowed in the ContactGrid.  Change it at will }

type
  { Stores location and index of the vertical bars              }
  { These must be in their own type block for BCB compatibility }
  TVpBarRec = packed record
    Rec    : TRect;
    Index  : Integer;
  end;

  TVpContactRec = packed record
    Index       : Integer;
    Contact     : Pointer;
    CompanyRect : TRect;
    EMailRect   : TRect;
    WholeRect   : TRect;
    HeaderRect  : TRect;
    AddressRect : TRect;
    CSZRect     : TRect;
    Phone1Rect  : TRect;
    Phone2Rect  : TRect;
    Phone3Rect  : TRect;
    Phone4Rect  : TRect;
    Phone5Rect  : TRect;
  end;

type
  TVpBarArray = array of TVpBarRec;
  TVpContactArray = array of TVpContactRec;

  { forward declarations }
  TVpContactGrid = class;
  TVpContactGridState = (gsNormal, gsColSizing);

  { InPlace Editor }
  TVpCGInPlaceEdit = class(TCustomEdit)
  protected{private}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    field: string;
    constructor Create(AOwner: TComponent); override;
    procedure Move(const Loc: TRect; Redraw: Boolean);
  end;

  TVpContactHeadAttr = class(TPersistent)
  protected{private}
    FGrid: TVpContactGrid;
    FFont: TFont;
    FColor: TColor;
    FBordered: Boolean;
    procedure SetColor (Value: TColor);
    procedure SetFont (Value: TFont);
    procedure SetBordered (Value: Boolean);
  public
    constructor Create(AOwner: TVpContactGrid);
    destructor Destroy; override;
    property Grid: TVpContactGrid read FGrid;
  published
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property Bordered: Boolean read FBordered write SetBordered;
  end;

  { Contact Grid }
  TVpContactGrid = class(TVpLinkableControl)
  protected{ private }
    FColumnWidth       : Integer;
    FColor             : TColor;
    FBarColor          : TColor;
    FBarWidth          : Integer;
    FAllowInPlaceEdit  : Boolean;
    FScrollBars        : TScrollStyle;
    FContactHeadAttr   : TVpContactHeadAttr;
    FDrawingStyle      : TVpDrawingStyle;
    FContactIndex      : Integer;
    FPrintNumColumns   : Integer;
    FActiveContact     : TVpContact;
    FDefaultPopup      : TPopupMenu;
    FSortBy            : TVpContactSort;
    { contact variables }
    FOwnerDrawContact  : TVpOwnerDrawContactEvent;
    FBeforeEdit        : TVpEditContactEvent;
    FAfterEdit         : TVpContactEvent;
    FOwnerEditContact  : TVpEditContactEvent;
    FOnClickContact    : TVpContactEvent;
    FOnColWidthChange  : TVpCGColWidthChangeEvent;                    
    FVisibleContacts   : Integer;
    FContactsBefore    : Integer;
    FContactsAfter     : Integer;
    { internal variables }
    cgLastXPos         : Integer;
    cgCol1RecCount     : Word;
    cgDragBarNumber    : Integer;
    cgNewColWidth      : Integer;
    cgBarArray         : TVpBarArray;
    cgResizeBarArray   : TVpBarArray;
    cgContactArray     : TVpContactArray;
    cgGridState        : TVpContactGridState;
    cgHitPoint         : TPoint;
    cgClickPoint       : TPoint;
    cgClickTimer       : TTimer;
    cgLoaded           : Boolean;
    cgRowHeight        : Integer;
    cgInPlaceEditor    : TVpCGInPlaceEdit;
    cgCreatingEditor   : Boolean;
    cgPainting         : Boolean;
    cgColCount         : Integer;
    cgVScrollDelta     : Integer;

    { property methods }
    function GetBarWidth: Integer;
    procedure SetBarWidth(Value: Integer);
    procedure SetBarColor(Value: TColor);
    procedure SetContactIndex(Value: Integer);
    procedure SetColumnWidth(Value: Integer);
    procedure SetDrawingStyle(const Value: TVpDrawingStyle);
    procedure SetColor(const Value: TColor);
    procedure SetHScrollPos;
    procedure SetPrintNumColumns (const v : Integer);
    procedure SetSortBy (const v : TVpContactSort);
    procedure SetDataStore (const Value : TVpCustomDataStore); override;
    { internal methods }
    procedure cgCalcRowHeight;
    procedure cgEditInPlace(Sender: TObject);
    procedure cgHookUp;
    procedure Paint; override;
    procedure Loaded; override;
    procedure cgSpawnContactEditDialog(NewContact: Boolean);
    procedure cgSetActiveContactByCoord(Pnt: TPoint);
    procedure cgScrollHorizontal(Rows: Integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer); override;
    procedure PopupAddContact (Sender : TObject);
    procedure PopupDeleteContact (Sender : TObject);
    procedure PopupEditContact (Sender : TObject);
    procedure EditContact;
    procedure EndEdit(Sender: TObject);
    procedure InitializeDefaultPopup;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { message handlers }
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor);
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Msg : TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMRButtonDown(var Msg : TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure VpDataStoreChanged (var Msg : TMessage); message Vp_DataStoreChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LinkHandler(Sender: TComponent;
      NotificationType: TVpNotificationType;
      const Value: Variant); override;
    function GetControlType : TVpItemType; override;
    procedure DeleteActiveContact(Verify: Boolean);
    procedure PaintToCanvas (ACanvas : TCanvas;
                             ARect   : TRect;
                             Angle   : TVpRotationAngle);
    procedure RenderToCanvas (RenderCanvas : TCanvas;
                              RenderIn     : TRect;
                              Angle        : TVpRotationAngle;
                              Scale        : Extended;
                              RenderDate   : TDateTime;
                              StartLine    : Integer;
                              StopLine     : Integer;
                              UseGran      : TVpGranularity;
                              DisplayOnly  : Boolean); override;
    { - Added to support the buttonbar component.                         }
    function SelectContactByName(const Name: String): Boolean;           

    property ActiveContact : TVpContact read FActiveContact;
    property ContactIndex: Integer read FContactIndex write SetContactIndex;
  published
    property Align;
    property Anchors;
    property TabStop;
    property TabOrder;
    property AllowInPlaceEditing: Boolean
      read FAllowInPlaceEdit write FAllowInPlaceEdit;
    property BarWidth: Integer read GetBarWidth write SetBarWidth;
    property BarColor: TColor read FBarColor write SetBarColor;
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;
    property ContactHeadAttributes: TVpContactHeadAttr
      read FContactHeadAttr write FContactHeadAttr;
    property DrawingStyle: TVpDrawingStyle
      read FDrawingStyle write SetDrawingStyle;
    property Color: TColor read FColor write SetColor;
    property PrintNumColumns : Integer
             read FPrintNumColumns write SetPrintNumColumns default 3;
    property SortBy : TVpContactSort read FSortBy write SetSortBy
             default csLastFirst;
    { events }
    property BeforeEdit: TVpEditContactEvent
      read FBeforeEdit write FBeforeEdit;
    property AfterEdit : TVpContactEvent
      read FAfterEdit write FAfterEdit;
    property OnOwnerEditContact: TVpEditContactEvent
      read FOwnerEditContact write FOwnerEditContact;
    property OnColWidthChange : TVpCGColWidthChangeEvent              
      read FOnColWidthChange write FOnColWidthChange;                 
    property OnContactChange: TVpContactEvent
     read FOnClickContact write FOnClickContact;
  end;

implementation

uses
  SysUtils, Math, Forms, Dialogs, VpContactEditDlg;


(*****************************************************************************)
{ TVpContactHeadAttr }
constructor TVpContactHeadAttr.Create(AOwner: TVpContactGrid);
begin
  inherited Create;
  FGrid           := AOwner;
  FFont           := TFont.Create;
  FFont.Assign(FGrid.Font);
  FColor          := clSilver;
  FBordered       := true;
end;
{=====}

destructor TVpContactHeadAttr.Destroy;
begin
  FFont.Free;
end;
{=====}

procedure TVpContactHeadAttr.SetBordered(Value: Boolean);
begin
  if Value <> FBordered then begin
    FBordered := Value;
    Grid.Invalidate;
  end;
end;
{=====}

procedure TVpContactHeadAttr.SetColor(Value: TColor);
begin
  if Value <> FColor then begin
    FColor := Value;
    Grid.Invalidate;
  end;
end;
{=====}

procedure TVpContactHeadAttr.SetFont(Value: TFont);
begin
  if Value <> FFont then begin
    FFont.Assign(Value);
    Grid.Invalidate;
  end;
end;
{=====}


{ TVpCGInPlaceEdit }

constructor TVpCGInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  field := '';
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  {$IFDEF VERSION4}
  DoubleBuffered := False;
  {$ENDIF}
  { make it tiny }
  Height := 1;
  Width := 1;
end;
{=====}

procedure TVpCGInPlaceEdit.Move(const Loc: TRect; Redraw: Boolean);
var
  Margin: Integer;
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible(Handle);
  Invalidate;
  with Loc do begin
    Margin := 0;
    if (Field = 'Address') or (Field = 'Company') or (Field ='CSZ') then
      Margin := (TextMargin * 2);
    SetWindowPos(Handle, HWND_TOP, Left + Margin,
      Top + (TextMargin div 2), Right - Left - TextMargin * 2, Bottom - Top,
      SWP_SHOWWINDOW or SWP_NOREDRAW);
  end;
  if Redraw then Invalidate;
  Windows.SetFocus(Handle);
end;
{=====}

procedure TVpCGInPlaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;
{=====}

procedure TVpCGInPlaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Grid : TVpContactGrid;
begin
  Grid := TVpContactGrid(Owner);

  case Key of
  VK_RETURN: begin
    Key := 0;
    Grid.EndEdit(Self);
    Grid.SetFocus;    
  end;

  VK_UP: begin
    Grid.EndEdit(Self);
    Grid.ContactIndex := Grid.ContactIndex - 1;
    Key := 0;
    Grid.SetFocus;
  end;

  VK_DOWN: begin
    Grid.EndEdit(Self);
    Grid.ContactIndex := Grid.ContactIndex + 1;
    Key := 0;
    Grid.SetFocus;
  end;
  else
    inherited;
  end;
end;
{=====}

(*****************************************************************************)
{ TVpContactGrid }

constructor TVpContactGrid.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  ControlStyle          := [csCaptureMouse, csOpaque, csDoubleClicks];
  cgGridState           := gsNormal;
  { Create internal classes and stuff }
  cgClickTimer          := TTimer.Create (self);
  FContactHeadAttr      := TVpContactHeadAttr.Create (Self);

  { Set styles and initialize internal variables }
  {$IFDEF VERSION4}
  DoubleBuffered        := true;
  {$ENDIF}
  FVisibleContacts      := 0;
  FAllowInPlaceEdit     := true;
  FContactsBefore       := 0;
  FContactsAfter        := 0;
  cgCol1RecCount        := 0;
  cgClickPoint          := Point (0, 0);
  cgClickTimer.Enabled  := false;
  cgClickTimer.Interval := ClickDelay;
  cgClickTimer.OnTimer  := cgEditInPlace;
  cgCreatingEditor      := false;
  FDrawingStyle         := ds3d;
  cgPainting            := false;
  FColor                := clWindow;
  FBarColor             := clSilver;
  BarWidth              := 3;
  FColumnWidth          := 145;
  FContactIndex         := -1;
  FPrintNumColumns      := 3;

  { initialize the bar arrays. }
  SetLength(cgBarArray, MaxColumns);
  for I := 0 to pred(Length(cgBarArray)) do begin
    cgBarArray[I].Rec := Rect(-1, -1, -1, -1);
    cgBarArray[I].Index := -1;
  end;

  SetLength(cgResizeBarArray, MaxColumns);
  for I := 0 to pred(Length(cgResizeBarArray)) do begin
    cgResizeBarArray[I].Rec := Rect(-1, -1, -1, -1);
    cgResizeBarArray[I].Index := -1;
  end;

  cgDragBarNumber := -1;

  { size }
  Height := 299;
  Width  := 225;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  cgHookUp;
end;
{=====}

destructor TVpContactGrid.Destroy;
begin
  if (HandleAllocated) and
     (Assigned (DataStore)) and
     (not (csDesigning in ComponentState)) then
    DataStore.DeregisterWatcher (Handle);

  cgClickTimer.Free;
  FContactHeadAttr.Free;
  FDefaultPopup.Free;
  inherited;
end;
{=====}

procedure TVpContactGrid.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  case NotificationType of
    neDataStoreChange: Invalidate;
    neInvalidate: Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgHookUp;
var
  I: Integer;
begin
  { If the component is being dropped on a form at designtime, then }
  { automatically hook up to the first datastore component found    }
  if csDesigning in ComponentState then
    for I := 0 to pred(Owner.ComponentCount) do begin
      if (Owner.Components[I] is TVpCustomDataStore) then begin
        DataStore := TVpCustomDataStore(Owner.Components[I]);
        Exit;
      end;
    end;
end;
{=====}

procedure TVpContactGrid.Loaded;
begin
  inherited;
  cgLoaded := true;
end;
{=====}

function TVpContactGrid.GetControlType : TVpItemType;
begin
  Result := itContacts;
end;
{=====}

procedure TVpContactGrid.DeleteActiveContact(Verify: Boolean);
var
  Str: string;
  DoIt: Boolean;
begin
  DoIt := not Verify;
  if FActiveContact <> nil then begin
    if FActiveContact.FirstName <> '' then
      Str := FActiveContact.FirstName + ' ' + FActiveContact.LastName
    else
      Str := FActiveContact.LastName;

    if Verify then
      DoIt := (MessageDlg(RSDelete + ' ' + Str + ' ' + RSFromContactList
        + #13#10#10 + RSPermanent, mtconfirmation,
        [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveContact.Deleted := true;
      FActiveContact := nil;
      DataStore.PostContacts;
      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpContactGrid.Paint;
begin
  RenderToCanvas (Canvas,
                  Rect (0, 0, Width, Height),
                  ra0,
                  1,
                  Now,
                  -1,
                  -1,
                  gr30Min,
                  False);
end;
{=====}

procedure TVpContactGrid.PaintToCanvas (ACanvas : TCanvas;
                                         ARect   : TRect;
                                         Angle   : TVpRotationAngle);
begin
  RenderToCanvas (ACanvas, ARect, Angle, 1, Now,
                  -1, -1, gr30Min, True);
end;
{=====}

procedure TVpContactGrid.RenderToCanvas (RenderCanvas : TCanvas;
                                         RenderIn     : TRect;
                                         Angle        : TVpRotationAngle;
                                         Scale        : Extended;
                                         RenderDate   : TDateTime;
                                         StartLine    : Integer;
                                         StopLine     : Integer;
                                         UseGran      : TVpGranularity;
                                         DisplayOnly  : Boolean);
var
  SaveBrushColor : TColor;
  SavePenStyle   : TPenStyle;
  SavePenColor   : TColor;
  PhoneLblWidth  : Integer;
  StartContact   : Integer;

  RealWidth       : Integer;
  RealHeight      : Integer;
  RealLeft        : Integer;
  RealRight       : Integer;
  RealTop         : Integer;
  RealBottom      : Integer;
  RealColumnWidth : Integer;
  Rgn             : HRGN;

  RealColor                : TColor;
  SizingBarColor           : TColor;
  BevelDarkShadow          : TColor;
  BevelShadow              : TColor;
  BevelHighlight           : TColor;
  BevelFace                : TColor;
  RealBarColor             : TColor;
  RealContactHeadAttrColor : TColor;

  procedure Clear;
  var
    I: Integer;
  begin
    { clear Client Area }
    RenderCanvas.Brush.Color := RealColor;
    RenderCanvas.FillRect(RenderIn);

    { clear the vertical bar array }
    for I := 0 to pred(MaxColumns) do begin
      if cgBarArray[I].Index = -1 then
        Break;
      cgBarArray[I].Rec := Rect(-1, -1, -1, -1);
      cgBarArray[I].Index := -1;
    end;

    { initialize the contact array at runtime }
    if not (csDesigning in ComponentState)
    and (DataStore <> nil)
    and (DataStore.Resource <> nil)
    then begin
      SetLength(cgContactArray, DataStore.Resource.Contacts.Count);
      for I := 0 to pred(Length(cgContactArray)) do begin
        with cgContactArray[I] do begin
          Index       := -1;
          Contact     := nil;
          WholeRect   := Rect(-1, -1, -1, -1);
          HeaderRect  := Rect(-1, -1, -1, -1);
          AddressRect := Rect(-1, -1, -1, -1);
          CSZRect     := Rect(-1, -1, -1, -1);
          Phone1Rect  := Rect(-1, -1, -1, -1);
          Phone2Rect  := Rect(-1, -1, -1, -1);
          Phone3Rect  := Rect(-1, -1, -1, -1);
          Phone4Rect  := Rect(-1, -1, -1, -1);
          Phone5Rect  := Rect(-1, -1, -1, -1);
        end;
      end;
    end;
  end;
  {--}

  procedure SetMeasurements;
  begin
    RealWidth  := TPSViewportWidth (Angle, RenderIn);
    RealHeight := TPSViewportHeight (Angle, RenderIn);
    RealLeft   := TPSViewportLeft (Angle, RenderIn);
    RealRight  := TPSViewportRight (Angle, RenderIn);
    RealTop    := TPSViewportTop (Angle, RenderIn);
    RealBottom := TPSViewportBottom (Angle, RenderIn);
  end;

  procedure DrawVerticalBars;
  var
    BarPos, BarCount, I: Integer;
  begin
    { if the component is sufficiently small then no sense in painting it }
    if (Height < 20) then exit;

    { draw vertical bars }
    RenderCanvas.Pen.Color := RealBarColor;
    RenderCanvas.Pen.Style := psSolid;
    BarPos := RealLeft + 2 + RealColumnWidth + ExtraBarWidth;
    BarCount := 0;
    while (BarPos < RealRight) and
          (BarCount < Pred (MaxColumns)) do begin
      cgBarArray[BarCount].Rec := Rect(BarPos - ExtraBarWidth, RealTop,
        BarPos - ExtraBarWidth + FBarWidth, RealBottom);
      cgBarArray[BarCount].Index := BarCount;
      for I := 1 to BarWidth do begin
        TPSMoveTo (RenderCanvas, Angle, RenderIn,
                   BarPos, RealTop + 2 + TextMargin * 2);
        TPSLineTo (RenderCanvas, Angle, RenderIn,
                   BarPos, RealBottom - TextMargin * 2);
        Inc(BarPos);
      end;
      Inc(BarPos, RealColumnWidth);
      Inc(BarCount);
    end;

    { if the columns are being resized, then draw the temporary resizing bars }
    if cgGridState = gsColSizing then begin
      { clear sizing bar array }
      for I := 0 to pred(MaxColumns) do begin
        if cgResizeBarArray[I].Index = -1 then
          Break;
        cgResizeBarArray[I].Rec := Rect(-1, -1, -1, -1);
        cgResizeBarArray[I].Index := -1;
      end;
      { draw sizing bars }
      RenderCanvas.Pen.Color := SizingBarColor;
      RenderCanvas.Pen.Style := psDash;
      BarPos := RealLeft + 2 + cgNewColWidth + ExtraBarWidth;
      BarCount := 0;
      while (BarPos < Width) and (BarCount < pred(MaxColumns)) do begin
        cgResizeBarArray[BarCount].Index := BarCount;
        cgResizeBarArray[BarCount].Rec := Rect(BarPos - ExtraBarWidth,
          RealTop, BarPos - ExtraBarWidth + FBarWidth,
          RealBottom);
        for I := 1 to BarWidth do begin
          TPSMoveTo (RenderCanvas, Angle, RenderIn,
                     RealLeft + BarPos,
                     RealTop + 2 + TextMargin * 2);
          TPSLineTo (RenderCanvas, Angle, RenderIn,
                     RealLeft + BarPos,
                     RealBottom - TextMargin * 2);
          Inc(BarPos);
        end;
        Inc(BarPos, cgNewColWidth);
        Inc(BarCount);
      end;
      RenderCanvas.Pen.Style := psSolid;
    end;
  end;
  {--}

  procedure DrawContacts;
  var
    Anchor: TPoint;
    I, J: Integer;
    Str: string;
    TmpBmp: TBitmap;
    TmpCon: TVpContact;
    Col, RecsInCol: Integer;
    HeadRect, AddrRect, CSZRect, Phone1Rect, Phone2Rect, Phone3Rect: TRect;
    Phone4Rect, Phone5Rect, WholeRect, CompanyRect, EMailRect: TRect;
    TmpBmpRect   : TRect;
    TextColWidth : Integer;
    TextXOffset  : Integer;
    TextYOffset  : Integer;
  begin
    FVisibleContacts := 0;
    cgCol1RecCount := 0;
    TextXOffset := 0;
    TextYOffset := 0;
    { if the component is sufficiently small then no sense in painting it }
    if (Height < 20) then exit;
    { don't paint contacts at designtime or if the data connection is invalid }

    if (csDesigning in ComponentState)
    or (DataStore = nil)
    or (DataStore.Resource = nil) then
      Exit;

    { create a temporary bitmap for painting the items }
    TmpBmp := TBitmap.Create;
    try
      if (Angle = ra0) or (Angle = ra180) then begin
        TmpBmp.Width  := RealColumnWidth - (TextMargin * 4);
        TmpBmp.Height := RealHeight   - (TextMargin * 2);
        TextColWidth := TmpBmp.Width;
      end else begin
        TmpBmp.Height := RealColumnWidth - (TextMargin * 4);
        TmpBmp.Width  := RealHeight   - (TextMargin * 2);
        TextColWidth := TmpBmp.Height;
      end;
      TmpBmpRect := Rect (0, 0, TmpBmp.Width, TmpBmp.Height);

      TmpBmp.Canvas.Font.Assign(Font);

      { Calculate Phone Lbl Width }
      PhoneLblWidth := TmpBmp.Canvas.TextWidth(RSEmail);
      for I := 0 to 7 do begin
        Str := PhoneLabel(TVpPhoneType(I)) + ':       ';
        J := TmpBmp.Canvas.TextWidth(Str);
        if J > PhoneLblWidth then
          PhoneLblWidth := J;
      end;

      Col := 1;
      { clear the bitmap }
      TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));

      { sort the records }
      DataStore.Resource.Contacts.Sort;

      { Set the anchor starting point }
      case Angle of
        ra0 :
          Anchor := Point (2 + (TextMargin * 2),
                           2 + (TextMargin * 2));
        ra90 :
          Anchor := Point (2 + (TextMargin * 2),
                           2 + (TextMargin * 2));
        ra180 :
          Anchor := Point ((RenderIn.Right - RenderIn.Left) - TmpBmp.Width - 2 - (TextMargin * 2),
                           TmpBmp.Height - 2 - (TextMargin * 2));
        ra270 :
          Anchor := Point (2 + (TextMargin * 2),
                           (RenderIn.Bottom - RenderIn.Top) - TmpBmp.Height - 2 - (TextMargin * 2));
      end;
      RecsInCol := 0;

      for I := StartContact to pred(DataStore.Resource.Contacts.Count) do begin
        TmpCon := DataStore.Resource.Contacts.GetContact(I);
        if (TmpCon <> nil) then begin
          { Clear bmp canvas }
          TmpBmp.Canvas.Brush.Color := RealColor;
          TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));

          cgContactArray[I].Contact := TmpCon;
          { start building the WholeRect and build the HeaderRect}
          TmpBmp.Canvas.Pen.Color := BevelDarkShadow;
          TmpBmp.Canvas.Brush.Style := bsSolid;
          TmpBmp.Canvas.Font.Assign(FContactHeadAttr.Font);
          case Angle of
            ra0 : begin
              WholeRect.TopLeft := Point(0, 0);
              HeadRect.TopLeft := Point(TextMargin, 0);
              HeadRect.BottomRight := Point (TmpBmp.Width,
                                      HeadRect.Top + TmpBmp.Canvas.TextHeight(VpProductName)
                                      + (TextMargin div 2));
              WholeRect.BottomRight := HeadRect.BottomRight;
            end;
            ra90 : begin
              HeadRect.TopLeft := Point (TmpBmpRect.Right - TextMargin - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2), 0);
              HeadRect.BottomRight := Point (TmpBmpRect.Right,
                                             TmpBmp.Height);
              WholeRect.TopLeft := HeadRect.TopLeft;
              WholeRect.BottomRight := HeadRect.BottomRight;
            end;
            ra180 : begin
              WholeRect.BottomRight := Point (TmpBmp.Width, TmpBmp.Height);
              HeadRect.TopLeft := Point (TextMargin, TmpBmpRect.Bottom -
                                         TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
              HeadRect.BottomRight := Point (TmpBmp.Width,
                                             TmpBmp.Height - (TextMargin div 2));
              WholeRect.TopLeft := HeadRect.TopLeft;
            end;
            ra270 : begin
              WholeRect.TopLeft := Point (0, 0);
              HeadRect.TopLeft := Point (0, TextMargin);
              HeadRect.BottomRight := Point (TextMargin + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                             TmpBmp.Height);
              WholeRect.BottomRight := HeadRect.BottomRight;
            end;
          end;
          { assemble the header string }
          Str := AssembleName(TmpCon);
          { if the name isn't empty then paint all of the contact information }
          if Str > '' then begin
            { paint the header cell's background }
            if (Angle = ra0) or (Angle = ra180) then
              Str := GetDisplayString (TmpBmp.Canvas, Str, 2,
                                       WidthOf(HeadRect) - TextMargin)
            else
              Str := GetDisplayString (TmpBmp.Canvas, Str, 2,
                                       HeightOf(HeadRect) - TextMargin);
            TmpBmp.Canvas.Brush.Color := RealContactHeadAttrColor;
            TmpBmp.Canvas.FillRect (HeadRect); 
            { paint the header cell's border }
            if FContactHeadAttr.Bordered then begin
              TmpBmp.Canvas.Pen.Style := psSolid;
              {$IFDEF VERSION5}
              TmpBmp.Canvas.Rectangle (HeadRect);
              {$ELSE}
              TmpBmp.Canvas.Rectangle (HeadRect.Left, HeadRect.Top,
                HeadRect.Right, HeadRect.Bottom);
              {$ENDIF}
            end;
            { paint the header cell's text }
            case Angle of
              ra90  : begin
                TextXOffset := HeadRect.Right - HeadRect.Left - TextMargin div 2;
                TextYOffset := TextMargin div 3;
              end;
              ra180 : begin
                TextXOffset := HeadRect.Right - HeadRect.Left - TextMargin;
                TextYOffset := HeadRect.Bottom - HeadRect.Top - TextMargin div 3;
              end;
              ra270 : begin
                TextXOffset := TextMargin div 2;
                TextYOffset := HeadRect.Bottom - HeadRect.Top - TextMargin div 3;
              end;
            end;
            TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                               HeadRect.Left + (TextMargin div 2) + TextXOffset,
                               HeadRect.Top + (TextMargin div 3) + TextYOffset, Str);

            { restore font and colors }
            TmpBmp.Canvas.Font.Assign(Font);
            TmpBmp.Canvas.Brush.Color := RealColor;
            TmpBmp.Canvas.Pen.Color := BevelDarkShadow;
            TmpBmp.Canvas.Pen.Style := psSolid;

            { do Company }
            Str := TmpCon.Company;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  CompanyRect.TopLeft := Point (TextMargin,
                                             WholeRect.Bottom + (TextMargin div 2));
                  CompanyRect.BottomRight := Point(TmpBmp.Width, CompanyRect.Top
                    + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := CompanyRect.Bottom;
                end;
                ra90 : begin
                  CompanyRect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                TextMargin);
                  CompanyRect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left   := CompanyRect.Left;
                end;
                ra180 : begin
                  CompanyRect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  CompanyRect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := CompanyRect.Top;
                end;
                ra270 : begin
                  CompanyRect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  CompanyRect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right  := CompanyRect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - TextMargin * 2);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 CompanyRect.Left + TextMargin,
                                 CompanyRect.Top + (TextMargin div 2),
                                 Str);
            end;

            { do address... }
            if TmpCon.Address <> '' then begin
              case Angle of
                ra0   : begin
                  AddrRect.TopLeft := Point (TextMargin,
                                             WholeRect.Bottom + (TextMargin div 2));
                  AddrRect.BottomRight := Point (TmpBmp.Width,
                                                 AddrRect.Top +
                                                 TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := AddrRect.Bottom;
                  Str := GetDisplayString(TmpBmp.Canvas, TmpCon.Address, 2,
                         WidthOf(AddrRect) - TextMargin);
                end;
                ra90  : begin
                  AddrRect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                             TextMargin);
                  AddrRect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := AddrRect.Left;
                  Str := GetDisplayString(TmpBmp.Canvas, TmpCon.Address, 2,
                         HeightOf (AddrRect) - TextMargin);
                end;
                ra180 : begin
                  AddrRect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  AddrRect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := AddrRect.Top;
                  Str := GetDisplayString(TmpBmp.Canvas, TmpCon.Address, 2,
                         WidthOf(AddrRect) - TextMargin);
                end;
                ra270 : begin
                  AddrRect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  AddrRect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := AddrRect.Right;
                  Str := GetDisplayString(TmpBmp.Canvas, TmpCon.Address, 2,
                         TextColWidth - TextMargin * 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 AddrRect.Left + TextMargin,
                                 AddrRect.Top + (TextMargin div 2), Str);
            end;

            { do City, State, Zip }
            Str := TmpCon.City;
            if Str <> '' then
              Str := Str + ', ' + TmpCon.State
            else
              Str := TmpCon.State;
            if Str <> '' then
              Str := Str + ' ' + TmpCon.Zip
            else
              Str := TmpCon.Zip;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  CSZRect.TopLeft := Point(TextMargin, WholeRect.Bottom
                                     + (TextMargin div 2));
                  CSZRect.BottomRight := Point(TmpBmp.Width, CSZRect.Top +
                                      TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := CSZRect.Bottom;
                  Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                         - TextMargin * 2);
                end;
                ra90 : begin
                  CSZRect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                            TextMargin);
                  CSZRect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Bottom := CSZRect.Bottom;
                  Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                         - TextMargin * 2);
                  WholeRect.Left   := CSZRect.Left;
                end;
                ra180 : begin
                  CSZRect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - (TextMargin div 2));
                  CSZRect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top    := CSZRect.Top;
                end;
                ra270 : begin
                  CSZRect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  CSZRect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right  := CSZRect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - TextMargin * 2);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 CSZRect.Left + TextMargin,
                                 CSZRect.Top + (TextMargin div 2), Str);
            end;

            { do Phone1 }
            Str := TmpCon.Phone1;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  Phone1Rect.TopLeft :=
                      Point (TextMargin,
                             WholeRect.Bottom + (TextMargin div 2));
                  Phone1Rect.BottomRight :=
                      Point (TmpBmp.Width,
                             Phone1Rect.Top +
                             TmpBmp.Canvas.TextHeight (VpProductName) +
                             (TextMargin div 2));
                  WholeRect.Bottom := Phone1Rect.Bottom;
                  Str := GetDisplayString (TmpBmp.Canvas, Str, 2,
                                           TextColWidth - (TextMargin * 2) -
                                           PhoneLblWidth);
                end;
                ra90 : begin
                  Phone1Rect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                               TextMargin);
                  Phone1Rect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := Phone1Rect.Left;
                  Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                      - (TextMargin * 2) - PhoneLblWidth);
                end;
                ra180 : begin
                  Phone1Rect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  Phone1Rect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := Phone1Rect.Top;
                  Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                      - (TextMargin * 2) - PhoneLblWidth);
                end;
                ra270 : begin
                  Phone1Rect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  Phone1Rect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := Phone1Rect.Right;
                  Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                      - (TextMargin * 2) - PhoneLblWidth);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone1Rect.Left + TextMargin,
                                 Phone1Rect.Top + (TextMargin div 2),
                                 PhoneLabel(TVpPhoneType(TmpCon.PhoneType1)) + ': ');
              case Angle of
                ra0 : begin
                  Phone1Rect.Left := Phone1Rect.Left + PhoneLblWidth;
                  Phone1Rect.Top := Phone1Rect.Top + (TextMargin div 2);
                end;
                ra90 : begin
                  Phone1Rect.Top := Phone1Rect.Top + PhoneLblWidth;
                  Phone1Rect.Left := Phone1Rect.Left + (TextMargin);
                end;
                ra180 : begin
                  Phone1Rect.Left := Phone1Rect.Left - PhoneLblWidth;
                  Phone1Rect.Top := Phone1Rect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  Phone1Rect.Top := Phone1Rect.Top - PhoneLblWidth;
                  Phone1Rect.Left := Phone1Rect.Left + (TextMargin div 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone1Rect.Left,
                                 Phone1Rect.Top, Str);
            end;

            { do Phone2 }
            Str := TmpCon.Phone2;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  Phone2Rect.TopLeft := Point(TextMargin, WholeRect.Bottom
                    + (TextMargin div 2));
                  Phone2Rect.BottomRight := Point(TmpBmp.Width, Phone2Rect.Top +
                    TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := Phone2Rect.Bottom;
                end;
                ra90 : begin
                  Phone2Rect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                               TextMargin);
                  Phone2Rect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := Phone2Rect.Left;
                end;
                ra180 : begin
                  Phone2Rect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  Phone2Rect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := Phone2Rect.Top;
                end;
                ra270 : begin
                  Phone2Rect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  Phone2Rect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := Phone2Rect.Right;
               end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - (TextMargin * 2) - PhoneLblWidth);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone2Rect.Left + TextMargin,
                                 Phone2Rect.Top + (TextMargin div 2),
                                 PhoneLabel(TVpPhoneType(TmpCon.PhoneType2)) + ': ');
              case Angle of
                ra0 : begin
                  Phone2Rect.Left := Phone2Rect.Left + PhoneLblWidth;
                  Phone2Rect.Top := Phone2Rect.Top + (TextMargin div 2);
                end;
                ra90 : begin
                  Phone2Rect.Top := Phone2Rect.Top + PhoneLblWidth;
                  Phone2Rect.Left := Phone2Rect.Left + (TextMargin);
                end;
                ra180 : begin
                  Phone2Rect.Left := Phone2Rect.Left - PhoneLblWidth;
                  Phone2Rect.Top := Phone2Rect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  Phone2Rect.Top := Phone2Rect.Top - PhoneLblWidth;
                  Phone2Rect.Left := Phone2Rect.Left + (TextMargin div 2);
                end;
              end;

              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone2Rect.Left,
                                 Phone2Rect.Top, Str);
            end;

            { do Phone3 }
            Str := TmpCon.Phone3;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  Phone3Rect.TopLeft := Point(TextMargin, WholeRect.Bottom
                    + (TextMargin div 2));
                  Phone3Rect.BottomRight := Point(TmpBmp.Width, Phone3Rect.Top +
                    TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := Phone3Rect.Bottom;
                end;
                ra90 : begin
                  Phone3Rect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                               TextMargin);
                  Phone3Rect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := Phone3Rect.Left;
                end;
                ra180 : begin
                  Phone3Rect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  Phone3Rect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := Phone3Rect.Top;
                end;
                ra270 : begin
                  Phone3Rect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  Phone3Rect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := Phone3Rect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - (TextMargin * 2) - PhoneLblWidth);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone3Rect.Left + TextMargin,
                                 Phone3Rect.Top + (TextMargin div 2),
                                 PhoneLabel(TVpPhoneType(TmpCon.PhoneType3)) + ': ');
              case Angle of
                ra0 : begin
                  Phone3Rect.Left := Phone3Rect.Left + PhoneLblWidth;
                  Phone3Rect.Top := Phone3Rect.Top + (TextMargin div 2);
                end;
                ra90 : begin
                  Phone3Rect.Top := Phone3Rect.Top + PhoneLblWidth;
                  Phone3Rect.Left := Phone3Rect.Left + (TextMargin);
                end;
                ra180 : begin
                  Phone3Rect.Left := Phone3Rect.Left - PhoneLblWidth;
                  Phone3Rect.Top := Phone3Rect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  Phone3Rect.Top := Phone3Rect.Top - PhoneLblWidth;
                  Phone3Rect.Left := Phone3Rect.Left + (TextMargin div 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone3Rect.Left,
                                 Phone3Rect.Top, Str);
            end;

            { do Phone4 }
            Str := TmpCon.Phone4;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  Phone4Rect.TopLeft := Point(TextMargin, WholeRect.Bottom
                    + (TextMargin div 2));
                  Phone4Rect.BottomRight := Point(TmpBmp.Width, Phone4Rect.Top +
                    TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := Phone4Rect.Bottom;
                end;
                ra90 : begin
                  Phone4Rect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                               TextMargin);
                  Phone4Rect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := Phone4Rect.Left;
                end;
                ra180 : begin
                  Phone4Rect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  Phone4Rect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := Phone4Rect.Top;
                end;
                ra270 : begin
                  Phone4Rect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  Phone4Rect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := Phone4Rect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - (TextMargin * 2) - PhoneLblWidth);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone4Rect.Left + TextMargin,
                                 Phone4Rect.Top + (TextMargin div 2),
                                 PhoneLabel(TVpPhoneType(TmpCon.PhoneType4)) + ': ');
              case Angle of
                ra0 : begin
                  Phone4Rect.Left := Phone4Rect.Left + PhoneLblWidth;
                  Phone4Rect.Top := Phone4Rect.Top + (TextMargin div 2);
                end;
                ra90 : begin
                  Phone4Rect.Top := Phone4Rect.Top + PhoneLblWidth;
                  Phone4Rect.Left := Phone4Rect.Left + (TextMargin {div 2});
                end;
                ra180 : begin
                  Phone4Rect.Left := Phone4Rect.Left - PhoneLblWidth;
                  Phone4Rect.Top := Phone4Rect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  Phone4Rect.Top := Phone4Rect.Top - PhoneLblWidth;
                  Phone4Rect.Left := Phone4Rect.Left + (TextMargin div 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone4Rect.Left,
                                 Phone4Rect.Top, Str);
            end;

            { do Phone5 }
            Str := TmpCon.Phone5;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  Phone5Rect.TopLeft := Point(TextMargin, WholeRect.Bottom
                    + (TextMargin div 2));
                  Phone5Rect.BottomRight := Point(TmpBmp.Width, Phone5Rect.Top +
                    TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := Phone5Rect.Bottom;
                end;
                ra90 : begin
                  Phone5Rect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                               TextMargin);
                  Phone5Rect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := Phone5Rect.Left;
                end;
                ra180 : begin
                  Phone5Rect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  Phone5Rect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := Phone5Rect.Top;
                end;
                ra270 : begin
                  Phone5Rect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  Phone5Rect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := Phone5Rect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - (TextMargin * 2) - PhoneLblWidth);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone5Rect.Left + TextMargin,
                                 Phone5Rect.Top + (TextMargin div 2),
                                 PhoneLabel(TVpPhoneType(TmpCon.PhoneType5)) + ': ');
              case Angle of
                ra0 : begin
                  Phone5Rect.Left := Phone5Rect.Left + PhoneLblWidth;
                  Phone5Rect.Top := Phone5Rect.Top + (TextMargin div 2);
                end;
                ra90 : begin
                  Phone5Rect.Top := Phone5Rect.Top+ PhoneLblWidth;
                  Phone5Rect.Left := Phone5Rect.Left + (TextMargin);
                end;
                ra180 : begin
                  Phone5Rect.Left := Phone5Rect.Left - PhoneLblWidth;
                  Phone5Rect.Top := Phone5Rect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  Phone5Rect.Top := Phone5Rect.Top - PhoneLblWidth;
                  Phone5Rect.Left := Phone5Rect.Left + (TextMargin div 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 Phone5Rect.Left,
                                 Phone5Rect.Top, Str);
            end;

            { do EMail }
            Str := TmpCon.EMail;
            if Str <> '' then begin
              case Angle of
                ra0 : begin
                  EMailRect.TopLeft := Point(TextMargin, WholeRect.Bottom
                    + (TextMargin div 2));
                  EMailRect.BottomRight := Point(TmpBmp.Width, EMailRect.Top +
                    TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2));
                  WholeRect.Bottom := EMailRect.Bottom;
                end;
                ra90 : begin
                  EMailRect.TopLeft := Point (WholeRect.Left - TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                              TextMargin);
                  EMailRect.BottomRight := Point (WholeRect.Left - (TextMargin div 2),
                                                 WholeRect.Bottom + (TextMargin div 2));
                  WholeRect.Left := EMailRect.Left;
                end;
                ra180 : begin
                  EMailRect.TopLeft := Point (WholeRect.Right - TextMargin * 2,
                                                WholeRect.Top - TmpBmp.Canvas.TextHeight(VpProductName) - TextMargin);
                  EMailRect.BottomRight := Point (WholeRect.Left + TextMargin,
                                                    WholeRect.Top - (TextMargin div 2));
                  WholeRect.Top := EMailRect.Top;
                end;
                ra270 : begin
                  EMailRect.TopLeft := Point (WholeRect.Right,
                                                WholeRect.Bottom - TextMargin);
                  EMailRect.BottomRight := Point (WholeRect.Right + TmpBmp.Canvas.TextHeight(VpProductName) + (TextMargin div 2),
                                                    WholeRect.Top + (TextMargin div 2));
                  WholeRect.Right := EMailRect.Right;
                end;
              end;
              Str := GetDisplayString(TmpBmp.Canvas, Str, 2, TextColWidth
                - (TextMargin * 2) - PhoneLblWidth);
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 EMailRect.Left + TextMargin,
                                 EMailRect.Top + (TextMargin div 2), RSEmail + ': ');
              case Angle of
                ra0   : begin
                  EMailRect.Left := EMailRect.Left + PhoneLblWidth;
                  EmailRect.Top := EMailRect.Top + (TextMargin div 2);
                end;
                ra90  : begin
                  EMailRect.Top := EMailRect.Top + PhoneLblWidth;
                  EmailRect.Left := EMailRect.Left + TextMargin;
                end;
                ra180 : begin
                  EMailRect.Left := EMailRect.Left - PhoneLblWidth;
                  EmailRect.Top := EMailRect.Top + (TextMargin div 2);
                end;
                ra270 : begin
                  EMailRect.Top := EMailRect.Top - PhoneLblWidth;
                  EMailRect.Left := EMailRect.Left + (TextMargin div 2);
                end;
              end;
              TPSTextOutAtPoint (TmpBmp.Canvas, Angle, TmpBmpRect,
                                 EMailRect.Left,
                                 EMailRect.Top, Str);
            end;

            { if this record's too big to fit in the remaining area of this }
            { column, then slide over to the top of the next column }
            case Angle of
              ra0 : begin
                if (RenderIn.Top + Anchor.y + WholeRect.Bottom >= RenderIn.Bottom - TextMargin * 3) and
                   (RecsInCol > 0) then begin
                  Anchor := Point (Anchor.x + WholeRect.Right +
                                   FBarWidth + 1 + (TextMargin * 3),
                                   2 + (TextMargin * 2));
                  if Col = 1 then
                    cgCol1RecCount := RecsInCol;
                  Inc(Col);
                  RecsInCol := 0;
                  if DisplayOnly and
                     (Anchor.X + TextColWidth >= RenderIn.Right) then
                    Exit;
                end;
              end;
              ra90 : begin
                if (Anchor.x + RenderIn.Left + (WholeRect.Right - WholeRect.Left) > RenderIn.Right - TextMargin * 3) and
                   (RecsInCol > 0) then begin
                  Anchor.x := 2 + (TextMargin * 2);
                  Anchor.y := Anchor.y + WholeRect.Bottom + FBarWidth + 1 + TextMargin * 3;
                  if Col = 1 then
                    cgCol1RecCount := RecsInCol;
                  Inc(Col);
                  RecsInCol := 0;
                  if DisplayOnly and
                     (Anchor.y + TextColWidth >= RenderIn.Bottom) then
                    Exit;
                end;
              end;
              ra180 : begin
                if (Anchor.y + RenderIn.Top - (WholeRect.Bottom - WholeRect.Top) <= RenderIn.Top + TextMargin * 3) and
                   (RecsInCol > 0) then begin
                  Anchor.x := Anchor.x - ((WholeRect.Right) + FBarWidth + 1 + TextMargin * 3);
                  Anchor.y := TmpBmp.Height - 2 - (TextMargin * 2);
                  if Col = 1 then
                    cgCol1RecCount := RecsInCol;
                  Inc(Col);
                  RecsInCol := 0;
                  if DisplayOnly and
                     (Anchor.x + TextColWidth < RenderIn.Left) then
                    Exit;
                end;
              end;
              ra270 : begin
                if (Anchor.x + RenderIn.Left + (WholeRect.Right - WholeRect.Left) >= RenderIn.Right - TextMargin * 3) and
                   (RecsInCol > 0) then begin
                  Anchor.x := 2 + (TextMargin * 2);
                  Anchor.y := Anchor.y - (WholeRect.Bottom + FBarWidth + 1 + TextMargin * 3);
                  if Col = 1 then
                    cgCol1RecCount := RecsInCol;
                  Inc(Col);
                  RecsInCol := 0;
                  if DisplayOnly and
                     (Anchor.y + TextColWidth <= RenderIn.Top) then
                    Exit;
                end;
              end;
            end;

            { add a little spacing between records }
            case Angle of
              ra0   :
                WholeRect.Bottom := WholeRect.Bottom + (TextMargin * 2);
              ra90  :
                WholeRect.Left := WholeRect.Left - (TextMargin * 2);
              ra180 :
                WholeRect.Top := WholeRect.Top - (TextMargin * 2);
              ra270 :
                WholeRect.Right := WholeRect.Right + (TextMargin * 2);
            end;

            { Update Array Rects }
            cgContactArray[I].WholeRect.TopLeft := Point(
              Anchor.X, Anchor.Y + WholeRect.Top);
            cgContactArray[I].WholeRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + WholeRect.Bottom);

            cgContactArray[I].HeaderRect.TopLeft := Point(
              Anchor.X, Anchor.Y + HeadRect.Top);
            cgContactArray[I].HeaderRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + HeadRect.Bottom);

            cgContactArray[I].AddressRect.TopLeft := Point(
              Anchor.X, Anchor.Y + AddrRect.Top);
            cgContactArray[I].AddressRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + AddrRect.Bottom);

            cgContactArray[I].CSZRect.TopLeft := Point(
              Anchor.X, Anchor.Y + CSZRect.Top);
            cgContactArray[I].CSZRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + CSZRect.Bottom);

            cgContactArray[I].CompanyRect.TopLeft := Point(
              Anchor.X, Anchor.Y + CompanyRect.Top);
            cgContactArray[I].CompanyRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + CompanyRect.Bottom);

            cgContactArray[I].EMailRect.TopLeft := Point(
              Anchor.X + EMailRect.Left, Anchor.Y + EMailRect.Top);
            cgContactArray[I].EMailRect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + EMailRect.Bottom);

            cgContactArray[I].Phone1Rect.TopLeft := Point(
              Anchor.X + Phone1Rect.Left, Anchor.Y + Phone1Rect.Top);
            cgContactArray[I].Phone1Rect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + Phone1Rect.Bottom);

            cgContactArray[I].Phone2Rect.TopLeft := Point(
              Anchor.X + Phone2Rect.Left, Anchor.Y + Phone2Rect.Top);
            cgContactArray[I].Phone2Rect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + Phone2Rect.Bottom);

            cgContactArray[I].Phone3Rect.TopLeft := Point(
              Anchor.X + Phone3Rect.Left, Anchor.Y + Phone3Rect.Top);
            cgContactArray[I].Phone3Rect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + Phone3Rect.Bottom);

            cgContactArray[I].Phone4Rect.TopLeft := Point(
              Anchor.X + Phone4Rect.Left, Anchor.Y + Phone4Rect.Top);
            cgContactArray[I].Phone4Rect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + Phone4Rect.Bottom);

            cgContactArray[I].Phone5Rect.TopLeft := Point(
              Anchor.X + Phone5Rect.Left, Anchor.Y + Phone5Rect.Top);
            cgContactArray[I].Phone5Rect.BottomRight := Point(
              Anchor.X + TmpBmp.Width, Anchor.Y + Phone5Rect.Bottom);

            { move the drawn record from the bitmap to the component canvas }

            case Angle of
              ra0   :
                RenderCanvas.CopyRect (Rect (Anchor.X + WholeRect.Left + RenderIn.Left,
                                             Anchor.Y + WholeRect.Top + RenderIn.Top,
                                             Anchor.X + TmpBmp.Width + RenderIn.Left,
                                             Anchor.Y + WholeRect.Bottom + RenderIn.Top),
                                       TmpBmp.Canvas, WholeRect);  
              ra90  :
                RenderCanvas.CopyRect (Rect (WholeRect.Left + RenderIn.Left - Anchor.X,
                                             Anchor.Y + WholeRect.Top + RenderIn.Top,
                                             WholeRect.Right + RenderIn.Left - Anchor.X,
                                             Anchor.Y + WholeRect.Bottom + RenderIn.Top),
                                       TmpBmp.Canvas,
                                       Rect (WholeRect.Left,
                                             WholeRect.Top,
                                             WholeRect.Right,
                                             WholeRect.Bottom));

              ra180 :
                RenderCanvas.CopyRect (Rect (Anchor.X + WholeRect.Left + RenderIn.Left,
                                             Anchor.Y - (WholeRect.Bottom - WholeRect.Top) + RenderIn.Top,
                                             Anchor.X + TmpBmp.Width + RenderIn.Left,
                                             Anchor.Y + RenderIn.Top),
                                       TmpBmp.Canvas, WholeRect);

              ra270 :
                RenderCanvas.CopyRect (Rect (Anchor.X + RenderIn.Left,
                                             Anchor.Y + RenderIn.Top,
                                             Anchor.X + RenderIn.Left + (WholeRect.Right - WholeRect.Left),
                                             Anchor.Y + RenderIn.Top + (WholeRect.Bottom - WholeRect.Top)),
                                       TmpBmp.Canvas, WholeRect);
            end; 

            { draw focusrect around selected record }
            if Focused and (TmpCon = FActiveContact) then begin
              with cgContactArray[I] do
                RenderCanvas.DrawFocusRect(Rect(WholeRect.Left, WholeRect.Top - 3,
                  WholeRect.Right + TextMargin, WholeRect.Bottom - 2));
            end;

            { slide anchor down for the next record }
            case Angle of
              ra0   : Anchor.Y := Anchor.Y + WholeRect.Bottom;
              ra90  : Anchor.X := Anchor.X + (WholeRect.Right - WholeRect.Left);
              ra180 : Anchor.Y := Anchor.Y - (WholeRect.Bottom - WholeRect.Top);
              ra270 : Anchor.X := Anchor.X + WholeRect.Right;
            end;
            Inc(RecsInCol);
          end;
        end;

        if not DisplayOnly then
          case Angle of
            ra0 : begin
              if (Anchor.X > RenderIn.Right) and
                 (I < DataStore.Resource.Contacts.Count) then begin
                { we have filled in the visible area }
                FContactsAfter := DataStore.Resource.Contacts.Count - I;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact - FContactsAfter;
                Break;
              end else begin
                FContactsAfter := 0;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact;
              end;
            end;
            ra90 : begin
              if (Anchor.Y > RenderIn.Bottom) and
                 (I < DataStore.Resource.Contacts.Count) then begin
                { we have filled in the visible area }
                FContactsAfter := DataStore.Resource.Contacts.Count - I;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact - FContactsAfter;
                Break;
              end else begin
                FContactsAfter := 0;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact;
              end;
            end;
            ra180 : begin
              if (Anchor.X < RenderIn.Left)
              and (I < DataStore.Resource.Contacts.Count) then begin
                { we have filled in the visible area }
                  FContactsAfter := DataStore.Resource.Contacts.Count - I;
                  FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact
                    - FContactsAfter;
                  Break;
              end
              else
                FContactsAfter := 0;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact;
            end;
            ra270 : begin
              if (Anchor.Y < RenderIn.Top)
              and (I < DataStore.Resource.Contacts.Count) then begin
                { we have filled in the visible area }
                  FContactsAfter := DataStore.Resource.Contacts.Count - I;
                  FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact
                    - FContactsAfter;
                  Break;
              end
              else
                FContactsAfter := 0;
                FVisibleContacts := DataStore.Resource.Contacts.Count - StartContact;
            end;
          end;
      end;
    finally
      TmpBmp.Free;
    end;
    if FContactsAfter = 0 then
      FLastPrintLine := -2
    else
      FLastPrintLine := FContactsAfter;
  end;
  {--}

  procedure DrawBorders;
  begin
    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      DrawBevelRect (RenderCanvas,
                     Rect (RenderIn.Left,
                           RenderIn.Top,
                           RenderIn.Right - 1,
                           RenderIn.Bottom - 1),
                     BevelShadow,
                     BevelHighlight);
      DrawBevelRect (RenderCanvas,
                     Rect (RenderIn.Left + 1,
                           RenderIn.Top + 1,
                           RenderIn.Right - 2,
                           RenderIn.Bottom - 2),
                     BevelHighlight,
                     BevelShadow);
    end else if FDrawingStyle = ds3d then begin
    { draw a 3d bevel }
      DrawBevelRect (RenderCanvas,
                     Rect (RenderIn.Left,
                           RenderIn.Top,
                           RenderIn.Right - 1,
                           RenderIn.Bottom - 1),
                     BevelShadow,
                     BevelHighlight);
      DrawBevelRect (RenderCanvas,
                     Rect (RenderIn.Left + 1,
                           RenderIn.Top + 1,
                           RenderIn.Right - 2,
                           RenderIn.Bottom - 2),
                     BevelDarkShadow,
                     BevelFace);
    end;
  end;
  {--}

begin

  if DisplayOnly then begin
    RealColor                := clWhite;
    SizingBarColor           := clBlack;
    BevelDarkShadow          := clBlack;
    BevelShadow              := clBlack;
    BevelHighlight           := clBlack;
    BevelFace                := clBlack;
    RealBarColor             := clBlack;
    RealContactHeadAttrColor := clSilver;
  end else begin
    RealColor                := Color;
    SizingBarColor           := clBlack;
    BevelDarkShadow          := cl3dDkShadow;
    BevelShadow              := clBtnShadow;
    BevelHighlight           := clBtnHighlight;
    BevelFace                := clBtnFace;
    RealBarColor             := BarColor;
    RealContactHeadAttrColor := FContactHeadAttr.Color;
  end;

  cgPainting := true;
  SavePenStyle := RenderCanvas.Pen.Style;
  SaveBrushColor := RenderCanvas.Brush.Color;
  SavePenColor := RenderCanvas.Pen.Color;

  RenderCanvas.Pen.Style   := psSolid;
  RenderCanvas.Pen.Width   := 1;
  RenderCanvas.Pen.Mode    := pmCopy;
  RenderCanvas.Brush.Style := bsSolid;

  Rgn := CreateRectRgn (RenderIn.Left, RenderIn.Top,
                        RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn (RenderCanvas.Handle, Rgn);

    if StartLine = -1 then
      StartContact := FContactsBefore
    else
      StartContact := StartLine;

    SetMeasurements;

    if DisplayOnly and (PrintNumColumns > 0) then
      RealColumnWidth := (RealWidth - ((2 + ExtraBarWidth) *
                          (PrintNumColumns - 1))) div PrintNumColumns
    else
      RealColumnWidth := ColumnWidth;

    { clear the control }
    Clear;

    { draw the contacts }
    if StartLine <> -2 then
      DrawContacts;

    { draw the vertical bars }
    DrawVerticalBars;

    { draw the borders }
    DrawBorders;

    SetHScrollPos;

  finally
    SelectClipRgn (RenderCanvas.Handle, 0);
    DeleteObject (Rgn);
  end;

  { reinstate canvas settings }
  RenderCanvas.Pen.Style := SavePenStyle;
  RenderCanvas.Brush.Color := SaveBrushColor;
  RenderCanvas.Pen.Color := SavePenColor;
  cgPainting := false;
end;
{=====}

{ Introduced to support the buttonbar component                           !!.02}
function TVpContactGrid.SelectContactByName(const Name: String): Boolean;
var                                                                      
  Contact: TVpContact;                                                   
  ItemIndex: Integer;                                                    
begin                                                                    
  result := false;                                                       
  if (DataStore <> nil) and (DataStore.Resource <> nil) then begin       
    Contact := DataStore.Resource.Contacts.FindContactByName(Name, True);
    if ( Contact <> nil ) then begin                                     
      FActiveContact := Contact;                                         
      ItemIndex :=                                                       
        DataStore.Resource.Contacts.ContactsList.IndexOf(Contact);       
      if (ItemIndex > FContactsBefore + FVisibleContacts)                
      or (ItemIndex <= FContactsBefore)                                  
      then begin                                                         
        if ItemIndex = 0 then                                            
          FContactsBefore := 0                                           
        else                                                             
          FContactsBefore := ItemIndex - 1;                              
      end;                                                               
      result := true;                                                    
      Invalidate;                                                        
    end;                                                                 
  end;                                                                   
end;                                                                     
{=====}                                                                  

procedure TVpContactGrid.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgCalcRowHeight;
var
  SaveFont: TFont;
  Temp: Integer;
begin
  { Calculates row height based on the largest of the RowHead's Minute }
  { font, the standard client font, and a sample character string.     }
  SaveFont := Canvas.Font;
  Canvas.Font := FContactHeadAttr.Font;
  cgRowHeight := Canvas.TextHeight(RSTallShortChars);
  Canvas.Font.Assign(SaveFont);
  Temp := Canvas.TextHeight(RSTallShortChars);
  if Temp > cgRowHeight then
    cgRowHeight := Temp;
  cgRowHeight := cgRowHeight + TextMargin * 2;
  Canvas.Font := SaveFont;
end;
{=====}

procedure TVpContactGrid.SetDrawingStyle(const Value: TVpDrawingStyle);
begin
  if FDrawingStyle <> Value then begin
    FDrawingStyle := Value;
    Repaint;
  end;
end;
{=====}

procedure TVpContactGrid.SetBarColor(Value: TColor);
begin
  if FBarColor <> Value then begin
    FBarColor := Value;
    Invalidate;
  end;
end;
{=====}

function TVpContactGrid.GetBarWidth: Integer;
begin
  result := FBarWidth - (ExtraBarWidth * 2);
end;
{=====}

procedure TVpContactGrid.SetBarWidth(Value: Integer);
begin
  if (Value > 0) and (FBarWidth + (ExtraBarWidth * 2) <> Value) then begin
    FBarWidth := Value + (ExtraBarWidth * 2);
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.SetContactIndex(Value: Integer);
begin
  FContactIndex := Value;
  if (DataStore <> nil) and (DataStore.Resource <> nil) then
    FActiveContact := DataStore.Resource.Contacts.GetContact(Value)
  else
    FContactIndex := -1;
end;
{=====}

procedure TVpContactGrid.SetColumnWidth(Value: Integer);
begin
  if (Value > 49) and (FColumnWidth <> Value) then begin
    FColumnWidth := Value;
    if Assigned(OnColWidthChange) then                                
      OnColWidthChange(self, Value);                                  
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  { Reset the list }
  FContactsBefore := 0;
  FContactsAfter := 0;
  { force a repaint }
  Invalidate;
end;
{=====}

procedure TVpContactGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or WS_HSCROLL;
    WindowClass.style := CS_DBLCLKS;
  end;
end;
{=====}

procedure TVpContactGrid.CreateWnd;
begin
  inherited;
  cgCalcRowHeight;
  SetHScrollPos;
end;
{=====}

procedure TVpContactGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  J, I: Integer;
begin
  if cgGridState = gsNormal then
    inherited MouseMove(Shift, X, Y)

  else begin
    { Column sizing happens here...}
    { if the in-place editor is active then kill it. }
    if cgInPlaceEditor <> nil then
      EndEdit(self);

    if cgDragBarNumber = -1 then begin
      for I := 0 to pred(Length(cgResizeBarArray)) do begin
        if (I = 0) and (cgResizeBarArray[I].Rec.Left = -1) then begin
          for J := 0 to pred(Length(cgBarArray)) do begin
            if cgBarArray[J].Rec.Left = -1 then
              Break;
            if PointInRect(Point(X, Y), cgBarArray[J].Rec) then begin
              cgDragBarNumber := cgBarArray[J].Index;
              Break;
            end;
          end;
        end;
        if cgResizeBarArray[I].Rec.Left = -1 then
          Break;
        if PointInRect(Point(X, Y), cgResizeBarArray[I].Rec) then begin
          cgDragBarNumber := cgResizeBarArray[I].Index;
          Break;
        end;
      end;
    end;

    if cgDragBarNumber > -1 then begin
      cgNewColWidth := (X div (cgDragBarNumber + 1)) - (FBarWidth div 2);
      { Prevent the columns from being dragged closed or past the right }
      { side of the client area }
      if (cgNewColWidth <= 50) then
        cgNewColWidth := 50
      else if (cgNewColWidth >= Width - 50) then
        cgNewColWidth := Width - 50;

      Invalidate;
    end;
  end;
end;
{=====}

procedure TVpContactGrid.WMNCHitTest(var Msg: TWMNCHitTest);
var
  OverBar: Boolean;
  I: Integer;
begin
  DefaultHandler(Msg);
  if not (csDesigning in ComponentState) then begin
    OverBar := false;
    cgHitPoint := ScreenToClient(SmallPointToPoint(Msg.Pos));
    for I := 0 to pred(Length(cgBarArray)) do begin
      if cgBarArray[I].Rec.Left = -1 then
        Break;
      if PointInRect(cgHitPoint, cgBarArray[I].Rec) then begin
        OverBar := true;
        Break;
      end;
    end;
    if OverBar then
      SetCursor(Screen.Cursors[crHSplit]);
  end;
end;
{=====}

procedure TVpContactGrid.WMSetCursor(var Msg: TWMSetCursor);
var
  Cur: HCURSOR;
begin
  Cur := 0;
  with Msg do begin
    if HitTest = HTCLIENT then
      if cgGridState = gsColSizing then
        Cur := Screen.Cursors[crHSplit];
  end;
  if Cur <> 0 then SetCursor(Cur)
  else inherited;
end;
{=====}

procedure TVpContactGrid.WMLButtonDown(var Msg : TWMLButtonDown);
var
  I: Integer;
  Sizing: Boolean;
begin
  inherited;
  Sizing := false;

  cgClickPoint := Point(Msg.XPos, Msg.YPos);

  if not focused then SetFocus;

  if not (csDesigning in ComponentState) then begin
    { Don't allow column dragging at designtime }
    for I := 0 to pred(Length(cgBarArray)) do begin
      if PointInRect(cgClickPoint, cgBarArray[I].Rec) then begin
        Sizing := true;
        Break;
      end
    end;

    if Sizing then begin
      cgGridState := gsColSizing;
      cgLastXPos := cgClickPoint.X;
      cgNewColWidth := ColumnWidth;
    end else begin
      cgGridState := gsNormal;
      cgSetActiveContactByCoord(cgClickPoint);
      if AllowInPlaceEditing then
        cgClickTimer.Enabled := true;
    end;
  end;
end;
{=====}

procedure TVpContactGrid.WMLButtonDblClk(var Msg : TWMLButtonDblClk);
begin
  if not CheckCreateResource then                                      
    Exit;                                                              

  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  inherited;
  cgClickTimer.Enabled := false;
  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus;

  { The mouse click landed inside the client area }
  cgSetActiveContactByCoord(Point(Msg.XPos, Msg.YPos));
  { See if we hit an active contact }
  if FActiveContact <> nil then begin
    { edit this contact }
    cgSpawnContactEditDialog(False);
  end else begin
    { we must want to create a new contact }
    FActiveContact := DataStore.Resource.Contacts.AddContact(
      DataStore.GetNextID(ContactsTableName));
    { Allow the user to fill in all the new information }
    cgSpawnContactEditDialog(True);
  end;
end;
{=====}

procedure TVpContactGrid.WMKillFocus(var Msg : TWMKillFocus);
begin
  if (cgInPlaceEditor = nil) then
    Invalidate;
end;
{=====}

procedure TVpContactGrid.WMRButtonDown(var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;
  i            : Integer;

begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not focused then
      SetFocus;
    cgClickPoint := Point (Msg.XPos, Msg.YPos);
    cgSetActiveContactByCoord (cgClickPoint);
    cgClickTimer.Enabled := False;
    ClientOrigin := GetClientOrigin;

    if not Assigned (FActiveContact) then
      for i := 0 to FDefaultPopup.Items.Count - 1 do begin
        if (FDefaultPopup.Items[i].Tag = 1) or ReadOnly then             
          FDefaultPopup.Items[i].Enabled := False;
      end
    else
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;

    FDefaultPopup.Popup (cgClickPoint.x + ClientOrigin.x,
                         cgClickPoint.y + ClientOrigin.y);
  end;
end;
{=====}

procedure TVpContactGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if cgGridState = gsColSizing then begin
    cgGridState := gsNormal;
    cgDragBarNumber := -1;
    ColumnWidth := cgNewColWidth;
  end;
end;
{=====}

procedure TVpContactGrid.cgSpawnContactEditDialog(NewContact: Boolean);
var
  AllowIt: Boolean;
  Dlg : TVpContactEditDialog;
begin
  AllowIt := false;
  if Assigned(FOwnerEditContact) then
    FOwnerEditContact(self, FActiveContact, DataStore.Resource, AllowIt)
  else begin
    Dlg := TVpContactEditDialog.Create(Owner);
    try
      Dlg.DataStore := DataStore;
      Dlg.ControlLink := ControlLink;
      AllowIt := Dlg.Execute(FActiveContact);
    finally
      Dlg.Free;
    end;
  end;
  if AllowIt then begin
    if FActiveContact.Changed = true then                                
      DataStore.PostContacts;                                            
    Invalidate;
  end else begin
    if NewContact then begin
      DataStore.Resource.Contacts.DeleteContact(FActiveContact);
      FActiveContact := nil;
    end;
    DataStore.PostContacts;
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgEditInPlace(Sender: TObject);
begin
  { this is the timer contact which spawns an in-place editor }
  { if the contact is doublecliked before this timer fires, then the }
  { contact is edited in a dialog based editor. }
  cgClickTimer.Enabled := false;
  EditContact;
end;
{=====}

procedure TVpContactGrid.EditContact;
var
  AllowIt: Boolean;
  field: string;
  I: Integer;
begin
  field := '';
  AllowIt := true;
  { call the user defined BeforeEdit contact }
  if Assigned(FBeforeEdit) then
    FBeforeEdit(Self, FActiveContact, DataStore.Resource, AllowIt);

  if AllowIt then begin
    { find the field to edit }
    for I := 0 to pred(Length(cgContactArray)) do begin
      { find the active contact in the contactarray...}
      if (PointInRect(cgClickPoint, cgContactArray[I].WholeRect)) then begin
        FActiveContact := cgContactArray[I].Contact;
        with cgContactArray[I] do begin
          if PointInRect(cgClickPoint, AddressRect) then
            field := 'Address'
          else if PointInRect(cgClickPoint, CompanyRect) then
            field := 'Company'
          else if PointInRect(cgClickPoint, EMailRect) then
            field := 'EMail'
          else if PointInRect(cgClickPoint, CSZRect) then
            field := 'CSZ'
          else if PointInRect(cgClickPoint, Phone1Rect) then
            field := 'Phone1'
          else if PointInRect(cgClickPoint, Phone2Rect) then
            field := 'Phone2'
          else if PointInRect(cgClickPoint, Phone3Rect) then
            field := 'Phone3'
          else if PointInRect(cgClickPoint, Phone4Rect) then
            field := 'Phone4'
          else if PointInRect(cgClickPoint, Phone5Rect) then
            field := 'Phone5';

          if field <> '' then begin
            { create and spawn the in-place editor }
            cgInPlaceEditor := TVpCGInPlaceEdit.Create(Self);
            cgInPlaceEditor.Parent := self;
            cgInPlaceEditor.OnExit := EndEdit;

            { edit address }
            if field = 'Address' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(AddressRect, true);
              Canvas.DrawFocusRect(Rect(AddressRect.Left + TextMargin - 1,
                AddressRect.Top, AddressRect.Right + 3, AddressRect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Address;
            end;
            { edit company }
            if field = 'Company' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(CompanyRect, true);
              Canvas.DrawFocusRect(Rect(CompanyRect.Left + TextMargin - 1,
                CompanyRect.Top, CompanyRect.Right + 3, CompanyRect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Company;
            end;
            { edit CSZ }
            if field = 'CSZ' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(CSZRect, true);
              Canvas.DrawFocusRect(Rect(CSZRect.Left + TextMargin - 1,
                CSZRect.Top, CSZRect.Right + 3, CSZRect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.City + ', ' + FActiveContact.State
                + ' ' + FActiveContact.Zip;
            end;
            { edit email }
            if field = 'EMail' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(EMailRect, true);
              Canvas.DrawFocusRect(Rect(EMailRect.Left - TextMargin,
                EMailRect.Top, EMailRect.Right + 3, EMailRect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.EMail;
            end;
            { edit Phone1 }
            if field = 'Phone1' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone1Rect, true);
              Canvas.DrawFocusRect(Rect(Phone1Rect.Left - TextMargin,
                Phone1Rect.Top, Phone1Rect.Right + 3, Phone1Rect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Phone1;
            end;
            { edit Phone2 }
            if field = 'Phone2' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone2Rect, true);
              Canvas.DrawFocusRect(Rect(Phone2Rect.Left - TextMargin,
                Phone2Rect.Top, Phone2Rect.Right + 3, Phone2Rect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Phone2;
            end;
            { edit Phone3 }
            if field = 'Phone3' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone3Rect, true);
              Canvas.DrawFocusRect(Rect(Phone3Rect.Left - TextMargin,
                Phone3Rect.Top, Phone3Rect.Right + 3, Phone3Rect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Phone3;
            end;
            { edit Phone4 }
            if field = 'Phone4' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone4Rect, true);
              Canvas.DrawFocusRect(Rect(Phone4Rect.Left - TextMargin ,
                Phone4Rect.Top, Phone4Rect.Right + 3, Phone4Rect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Phone4;
            end;
            { edit Phone5 }
            if field = 'Phone5' then begin
              cgInPlaceEditor.Field := field;
              cgInPlaceEditor.Move(Phone5Rect, true);
              Canvas.DrawFocusRect(Rect(Phone5Rect.Left - TextMargin,
                Phone5Rect.Top, Phone5Rect.Right + 3, Phone5Rect.Bottom + 3));
              cgInPlaceEditor.Text := FActiveContact.Phone5;
            end;
          end;
        end;
      end;
    end;
    if cgInPlaceEditor <> nil then
      cgInPlaceEditor.SelectAll;
  end;
end;
{=====}

procedure TVpContactGrid.EndEdit(Sender: TObject);
var
  City, State, Zip: string;
begin
  if cgInPlaceEditor <> nil then begin
    {Address}
    if cgInPlaceEditor.field = 'Address' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Address then begin
        FActiveContact.Address := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Company}
    else if cgInPlaceEditor.field = 'Company' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Company then begin
        FActiveContact.Company := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {EMail}
    else if cgInPlaceEditor.field = 'EMail' then begin
      if cgInPlaceEditor.Text <> FActiveContact.EMail then begin
        FActiveContact.EMail := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {City, State, Zip}
    else if cgInPlaceEditor.field = 'CSZ' then begin
      ParseCSZ(cgInPlaceEditor.Text, City, State, Zip);
      if (City <> FActiveContact.City)
      or (State <> FActiveContact.State)
      or (Zip <> FActiveContact.Zip) then begin
        FActiveContact.City := City;
        FActiveContact.State := State;
        FActiveContact.Zip := Zip;
        FActiveContact.Changed := true;
      end;
    end
    {Phone1}
    else if cgInPlaceEditor.field = 'Phone1' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone1 then begin
        FActiveContact.Phone1 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone2}
    else if cgInPlaceEditor.field = 'Phone2' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone2 then begin
        FActiveContact.Phone2 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone3}
    else if cgInPlaceEditor.field = 'Phone3' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone3 then begin
        FActiveContact.Phone3 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone4}
    else if cgInPlaceEditor.field = 'Phone4' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone4 then begin
        FActiveContact.Phone4 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end
    {Phone5}
    else if cgInPlaceEditor.field = 'Phone5' then begin
      if cgInPlaceEditor.Text <> FActiveContact.Phone5 then begin
        FActiveContact.Phone5 := cgInPlaceEditor.Text;
        FActiveContact.Changed := true;
      end;
    end;

    cgInPlaceEditor.Free;
    cgInPlaceEditor := nil;

    if FActiveContact.Changed then begin
      DataStore.PostContacts;
      if Assigned(FAfterEdit) then
        FAfterEdit(self, FActiveContact);
    end;
  end;
  Invalidate;
end;
{=====}

procedure TVpContactGrid.InitializeDefaultPopup;
var
  NewItem : TMenuItem;

begin
  if RSContactPopupAdd <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSContactPopupAdd;
    NewItem.OnClick := PopupAddContact;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSContactPopupEdit <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSContactPopupEdit;
    NewItem.OnClick := PopupEditContact;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSContactPopupDelete <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSContactPopupDelete;
    NewItem.OnClick := PopupDeleteContact;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;
end;
{=====}

procedure TVpContactGrid.PopupAddContact (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if not CheckCreateResource then                                      
    Exit;                                                              
  if not Assigned (DataStore) then                                     
    Exit;                                                              
  if not Assigned (DataStore.Resource) then                            
    Exit;                                                              
  { we must want to create a new contact }
  FActiveContact := DataStore.Resource.Contacts.AddContact (
                        DataStore.GetNextID (ContactsTableName));
  { Allow the user to fill in all the new information }
  cgSpawnContactEditDialog(True);
end;
{=====}

procedure TVpContactGrid.PopupDeleteContact (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveContact <> nil then
    DeleteActiveContact (True);
end;
{=====}

procedure TVpContactGrid.PopupEditContact (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  if FActiveContact <> nil then
    { edit this contact }
    cgSpawnContactEditDialog(False);
end;
{=====}

procedure TVpContactGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  PopupPoint : TPoint;

begin
  case Key of
    VK_UP    :
      if ContactIndex > 0 then
        ContactIndex := ContactIndex - 1;
    VK_DOWN  :
      if ContactIndex < Pred(DataStore.Resource.Contacts.Count) then
        ContactIndex := ContactIndex + 1;
    VK_HOME  :
      if ContactIndex > 0 then
        ContactIndex := ContactIndex - 1;
    VK_END   :
      if ContactIndex < Pred(DataStore.Resource.Contacts.Count) then
        ContactIndex := ContactIndex + 1;
    VK_RIGHT :
      if ContactIndex + cgCol1RecCount <= Pred(DataStore.Resource.Contacts.Count) then
        ContactIndex := ContactIndex + cgCol1RecCount
      else
        ContactIndex := Pred(DataStore.Resource.Contacts.Count);
    VK_LEFT :
      if ContactIndex - cgCol1RecCount <= 0 then
        ContactIndex := 0
      else
        ContactIndex := ContactIndex - cgCol1RecCount;
    VK_DELETE :
      DeleteActiveContact (true);
    VK_TAB   :
      if ssShift in Shift then
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, False))
      else
        Windows.SetFocus (GetNextDlgTabItem(GetParent(Handle), Handle, True));
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
  end;
  Invalidate;
end;
{=====}

procedure TVpContactGrid.WMHScroll(var Msg: TWMHScroll);
begin
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;

  { for simplicity, bail out of editing while scrolling. }
  EndEdit(Self);
  if cgInPlaceEditor <> nil then
    Exit;

  case Msg.ScrollCode of
    SB_LINELEFT       : cgScrollHorizontal(-1);
    SB_LINERIGHT      : cgScrollHorizontal(1);
    SB_PAGELEFT       : cgScrollHorizontal(-1);
    SB_PAGERIGHT      : cgScrollHorizontal(1);
    SB_THUMBPOSITION, SB_THUMBTRACK : begin
      if (Msg.Pos > FContactsBefore) and (FContactsAfter = 0) then Exit;
      FContactsBefore := Msg.Pos;
      if (FContactsBefore = 1) and (cgCol1RecCount = 1) then
        FContactsBefore := 0;
      if FContactsBefore >= DataStore.Resource.Contacts.Count then
        FContactsBefore := DataStore.Resource.Contacts.Count - cgCol1RecCount;
    end;
  end;
  Invalidate;
end;
{=====}

procedure TVpContactGrid.VpDataStoreChanged (var Msg : TMessage);
begin
  { The DataStore's Resource may not have been property set (that is
    the DataStore existed, but there was no resource.  Force the sortby
    on the contacts here }
  if Assigned (DataStore) then 
    if Assigned (DataStore.Resource) then
      DataStore.Resource.Contacts.ContactSort := SortBy;
end;
{=====}

procedure TVpContactGrid.cgScrollHorizontal(Rows: Integer);
begin
  if (DataStore = nil) or (DataStore.Resource = nil) then
    Exit;
    
  if (Rows < 0) and (FContactsBefore > 0) then
    FContactsBefore := FContactsBefore - cgCol1RecCount
  else if (Rows > 0) and (FContactsAfter > 0) then
    FContactsBefore := FContactsBefore + cgCol1RecCount;

  if FContactsBefore < 0 then FContactsBefore := 0;
  if FContactsBefore >= DataStore.Resource.Contacts.Count then
    FContactsBefore := DataStore.Resource.Contacts.Count - cgCol1RecCount;
end;
{=====}

procedure TVpContactGrid.SetHScrollPos;
var
  SI : TScrollInfo;
begin
  if (not HandleAllocated)
  or (DataStore = nil)
  or (DataStore.Resource = nil)
  or (csDesigning in ComponentState)
  then Exit;

  with SI do begin
    cbSize := SizeOf(SI);
    fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    nMin := 1;
    nMax := DataStore.Resource.Contacts.Count;
    nPage := FVisibleContacts;
    if FContactsAfter = 0 then
      nPos := DataStore.Resource.Contacts.Count
    else
      nPos := FContactsBefore;
    nTrackPos := nPos;
  end;
  SetScrollInfo(Handle, SB_HORZ, SI, True);
end;
{=====}

procedure TVpContactGrid.SetPrintNumColumns (const v : Integer);
begin
  if v <> FPrintNumColumns then begin
    FPrintNumColumns := v;
    if Assigned (FControlLink) then
      FControlLink.Printer.NotifyLinked;
  end;
end;
{=====}

procedure TVpContactGrid.SetDataStore (const Value : TVpCustomDataStore);
begin
  if (Assigned (DataStore)) and (not (csDesigning in ComponentState)) then
    DataStore.DeregisterWatcher (Handle);

  inherited SetDataStore (Value);

  if (Assigned (DataStore)) and (not (csDesigning in ComponentState)) then
    DataStore.RegisterWatcher (Handle);

  if not Assigned (DataStore) then
    Exit;
  if not Assigned (DataStore.Resource) then
    Exit;
  DataStore.Resource.Contacts.ContactSort := SortBy;
end;
{=====}

procedure TVpContactGrid.SetSortBy (const v : TVpContactSort);
begin
  if v <> FSortBy then begin
    FSortBy := v;
    if not Assigned (DataStore) then
      Exit;
    if not Assigned (DataStore.Resource) then
      Exit;
    DataStore.Resource.Contacts.ContactSort := FSortBy;
    cgClickTimer.Enabled := False;
    FActiveContact := nil;
    Invalidate;
  end;
end;
{=====}

procedure TVpContactGrid.cgSetActiveContactByCoord(Pnt: TPoint);
var
  I: integer;
begin
  FActiveContact := nil;
  for I := 0 to pred(Length(cgContactArray)) do begin
    { if the point is in an active contact...}
    if PointInRect(Pnt, cgContactArray[I].WholeRect) then begin
      { Set ActiveContact to the selected one }
      FContactIndex := I;
      FActiveContact := TVpCOntact(cgContactArray[I].Contact);
      Break;
    end;
  end;
  if (FActiveContact <> nil) then begin
    if Assigned(FOnClickContact) then
      FOnClickContact(Self, FActiveContact);
  end;
  Invalidate;
end;
{=====}

end.
