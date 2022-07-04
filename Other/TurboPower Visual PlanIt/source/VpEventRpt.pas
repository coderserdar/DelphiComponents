{*********************************************************}
{*               VpEventRpt.PAS 1.03                     *}
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

unit VpEventRpt;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  VpBase,
  VpBaseDS,
  VpMisc,
  VpData,
  VpSr,
  VpConst,
  VpCanvasUtils;

const
  VpColorCategoryWidth = 10;

type
  TVpEREventSeparator = (esNone, esSpace, esBigSpace, esLine, es3dLine);

  TVpEventReport = class;

  TVpERInPlaceEdit = class (TCustomMemo)
    protected{private}
      procedure CreateParams (var Params : TCreateParams); override;
      procedure KeyDown (var Key : Word; Shift : TShiftState); override;
    public
      constructor Create (AOwner : TComponent); override;
      procedure Move (const Loc : TRect; Redraw : Boolean);
  end;

  TVpERHeadAttributes = class (TPersistent)
    private
      FOwner      : TVpEventReport;

      FColor      : TColor;
      FFont       : TVpFont;
      FShowBorder : Boolean;

    protected
      procedure SetColor (const v : TColor);
      procedure SetFont (v : TVpFont);
      procedure SetShowBorder (const v : Boolean);

    public
      constructor Create (AOwner : TVpEventReport);
      destructor Destroy; override;

      property Owner : TVpEventReport read FOwner;

    published
      property Color : TColor read FColor write SetColor default clBtnFace;
      property Font : TVpFont read FFont write SetFont;
      property ShowBorder : Boolean read FShowBorder write SetShowBorder
               default True;
  end;

  TVpERReportAttributes = class (TPersistent)
    private
      FOwner          : TVpEventReport;

      FColor          : TColor;
      FDotDotDotColor : TColor;
      FFont           : TVpFont;
      FLineColor      : TColor;

    protected
      procedure SetColor (const v : TColor);
      procedure SetDotDotDotColor (const v : TColor);
      procedure SetFont (v : TVpFont);
      procedure SetLineColor (const v : TColor);

    public
      constructor Create (AOwner : TVpEventReport);
      destructor Destroy; override;

      property Owner : TVpEventReport read FOwner;

    published
      property Color : TColor read FColor write SetColor default clWindow;
      property DotDotDotColor : TColor
               read FDotDotDotColor write SetDotDotDotColor default clBlack;
      property Font : TVpFont read FFont write SetFont;
      property LineColor : TColor read FLineColor write SetLineColor
               default clBlack;
  end;

  TVpERAllDayEventAttributes = class (TPersistent)
    private
      FOwner                : TVpEventReport;

      FBackgroundColor      : TColor;
      FBorderColor          : TColor;
      FColor                : TColor;
      FEventBackgroundColor : TColor;
      FFont                 : TVpFont;

    protected
      procedure SetBackgroundColor (const v : TColor);
      procedure SetBorderColor (const v : TColor);
      procedure SetColor (const v : TColor);
      procedure SetEventBackgroundColor (const v : TColor);
      procedure SetFont (v : TVpFont);

    public
      constructor Create (AOwner : TVpEventReport);
      destructor Destroy; override;

      property Owner : TVpEventReport read FOwner;

    published
      property BackgroundColor : TColor
               read FBackgroundColor write SetBackgroundColor default clWindow;
      property BorderColor : TColor
               read FBorderColor write SetBorderColor default clBlack;
      property Color : TColor read FColor write SetColor default clBtnFace;
      property EventBackgroundColor : TColor
               read FEventBackgroundColor write SetEventBackgroundColor
               default clWindow;
      property Font : TVpFont read FFont write SetFont;
  end;

  TVpEventReport = class(TVpLinkableControl)
    private
      FDateLabelFormat    : string;
      FDisplayDate        : TDateTime;
      FDrawingStyle       : TVpDrawingStyle;
      FMultiLine          : Boolean;
      FSeparateEvents     : TVpEREventSeparator;
      FShowCategoryColors : Boolean;
      FShowEventTime      : Boolean;
      FShowResourceName   : Boolean;
      FTimeFormat         : TVpTimeFormat;
      FTopLine            : Integer;

      FActiveEvent        : TVpEvent;
      FAllDayEventAttr    : TVpERAllDayEventAttributes;
      FDefaultPopup       : TPopupMenu;
      FHeadAttr           : TVpERHeadAttributes;
      FReportAttr         : TVpERReportAttributes;

      erActiveEventRec    : TRect;
      erClickTimer        : TTimer;
      erEventArray        : TVpEventArray;
      erHeaderHeight      : Integer;
      erInLinkHandler     : Boolean;
      erInPlaceEditor     : TVpERInPlaceEdit;
      erLoaded            : Boolean;

      FAfterEdit          : TVpAfterEditEvent;
      FBeforeEdit         : TVpBeforeEditEvent;
      FOwnerEditEvent     : TVpEditEvent;

    protected
      procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey);
                message CM_WANTSPECIALKEY;
      procedure CreateParams (var Params : TCreateParams); override;
      procedure DeleteActiveEvent (Verify : Boolean);
      procedure EditEvent;
      procedure EndEdit (Sender : TObject);
      procedure erEditInPlace (Sender : TObject);
      procedure erHookUp;
      procedure erPopulate;
      procedure erSpawnEventEditDialog (NewEvent : Boolean);
      function  EventAtCoord (Pt : TPoint) : Boolean;
      function  GetNextEvent (AnEvent : TVpEvent) : TVpEvent;
      function  GetPrevEvent (AnEvent : TVpEvent) : TVpEvent;
      procedure InitializeDefaultPopup;
      procedure KeyDown (var Key : Word; Shift : TShiftState); override;
      procedure Loaded; override;
      procedure Paint; override;
      procedure PopupAddEvent (Sender : TObject);
      procedure PopupDeleteEvent (Sender : TObject);
      procedure PopupEditEvent (Sender : TObject);
      procedure PopupNextDay (Sender : TObject);
      procedure PopupNextMonth (Sender : TObject);
      procedure PopupNextWeek (Sender : TObject);
      procedure PopupNextYear (Sender : TObject);
      procedure PopupPrevDay (Sender : TObject);
      procedure PopupPrevMonth(Sender : TObject);
      procedure PopupPrevWeek (Sender : TObject);
      procedure PopupPrevYear (Sender : TObject);
      procedure PopupToday (Sender : TObject);
      procedure PopupTomorrow (Sender : TObject);
      procedure PopupYesterday (Sender : TObject);
      procedure SetDateLabelFormat (const v : string);
      procedure SetDisplayDate (const v : TDateTime);
      procedure SetDrawingStyle (const v : TVpDrawingStyle);
      procedure SetMultiLine (const v : Boolean);
      procedure SetSeparateEvents (const v : TVpEREventSeparator);
      procedure SetShowCategoryColors (const v : Boolean);
      procedure SetShowEventTime (const v : Boolean);
      procedure SetShowResourceName (const v : Boolean);
      procedure SetTimeFormat (const v : TVpTimeFormat);
      procedure WMLButtonDblClk (var Msg : TWMLButtonDblClk);
                message WM_LBUTTONDBLCLK;
      procedure WMLButtonDown (var Msg : TWMLButtonDown);
                message WM_LBUTTONDOWN;
      procedure WMRButtonDown (var Msg : TWMRButtonDown);
                message WM_RBUTTONDOWN;

    public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;

      function GetControlType : TVpItemType; override;
      procedure LinkHandler (      Sender           : TComponent;
                                   NotificationType : TVpNotificationType;
                             const Value            : Variant); override;
      procedure RenderToCanvas (RenderCanvas : TCanvas;
                                RenderIn     : TRect;
                                Angle        : TVpRotationAngle;
                                Scale        : Extended;
                                RenderDate   : TDateTime;
                                StartLine    : Integer;
                                StopLine     : Integer;
                                UseGran      : TVpGranularity;
                                DisplayOnly  : Boolean); override;

      property ActiveEvent: TVpEvent read FActiveEvent;

    published
      property AllDayEventAttr : TVpERAllDayEventAttributes
               read FAllDayEventAttr write FAllDayEventAttr;
      property Date : TDateTime read FDisplayDate write SetDisplayDate;
      property DateLabelFormat: string
               read FDateLabelFormat write SetDateLabelFormat;
      property DrawingStyle : TVpDrawingStyle
               read FDrawingStyle write SetDrawingStyle default ds3d;
      property HeadAttr : TVpERHeadAttributes read FHeadAttr write FHeadAttr;
      property MultiLine : Boolean read FMultiLine write SetMultiLine
               default True;
      property ReportAttr : TVpERReportAttributes
               read FReportAttr write FReportAttr;
      property SeparateEvents : TVpEREventSeparator
               read FSeparateEvents write SetSeparateEvents default esLine;
      property ShowCategoryColors : Boolean
               read FShowCategoryColors write SetShowCategoryColors
               default True;
      property ShowEventTime : Boolean
               read FShowEventTime write SetShowEventTime default True;
      property ShowResourceName : Boolean
               read FShowResourceName write SetShowResourceName default True;
      property TimeFormat : TVpTimeFormat read FTimeFormat write SetTimeFormat
               default tf24Hour;

      property AfterEdit : TVpAfterEditEvent read FAfterEdit write FAfterEdit;
      property BeforeEdit: TVpBeforeEditEvent
               read FBeforeEdit write FBeforeEdit;
      property OnOwnerEditEvent: TVpEditEvent
               read FOwnerEditEvent write FOwnerEditEvent;

      property Align;
      property Anchors;
      property Color;
      property Constraints;
      property Font;
      property ReadOnly;
      property TabStop;
      property TabOrder;

      property OnClick;
  end;

procedure Register;

implementation

uses
  Math, VpEvntEditDlg;

procedure Register;
begin
  RegisterComponents('Visual PlanIt', [TVpEventReport]);
end;

{ TVpERInPlaceEdit ********************************************************** }

constructor TVpERInPlaceEdit.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ParentCtl3D    := False;
  Ctl3D          := False;
  TabStop        := False;
  BorderStyle    := bsNone;
  {$IFDEF VERSION4}
  DoubleBuffered := False;
  {$ENDIF}
end;

procedure TVpERInPlaceEdit.Move (const Loc : TRect; Redraw : Boolean);
begin
  CreateHandle;
  Redraw := Redraw or not IsWindowVisible (Handle);
  with Loc do
    SetWindowPos (Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
                  SWP_SHOWWINDOW or SWP_NOREDRAW);
  if Redraw then
    Invalidate;
  SetFocus;
end;

procedure TVpERInPlaceEdit.CreateParams (var Params : TCreateParams);
begin
  inherited CreateParams (Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TVpERInPlaceEdit.KeyDown (var Key : Word; Shift : TShiftState);
var
  Grid    : TVpEventReport;
  LineNum : Integer;

begin
  Grid    := TVpEventReport (Owner);
  LineNum := Perform (EM_LINEFROMCHAR,SelStart, 0);

  case Key of
    VK_RETURN: begin
      Key := 0;
      Grid.EndEdit (Self);
    end;

    VK_UP: begin
      if LineNum = 0 then begin
        Key := 0;
        Grid.EndEdit (Self);
      end;
    end;

    VK_DOWN: begin
      if LineNum = Lines.Count - 1 then begin
        Key := 0;
        Grid.EndEdit (Self);
      end;
    end;

    VK_ESCAPE: begin
      Key := 0;
      Grid.EndEdit (self);
    end;

    else
      inherited;
  end;
end;

{ TVpERHeadAttributes ******************************************************* }

constructor TVpERHeadAttributes.Create (AOwner : TVpEventReport);
begin
  inherited Create;

   FFont       := TVpFont.Create (AOwner);

   FOwner      := AOwner;
   FColor      := clBtnFace;
   FShowBorder := True;
end;

destructor TVpERHeadAttributes.Destroy;
begin
  FFont.Free;

  inherited Destroy;
end;

procedure TVpERHeadAttributes.SetColor (const v : TColor);
begin
  if v <> FColor then begin
    FColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERHeadAttributes.SetFont (v : TVpFont);
begin
  FFont.Assign (v);
  if Assigned (FOwner) then
    FOwner.Invalidate;
end;

procedure TVpERHeadAttributes.SetShowBorder (const v : Boolean);
begin
  if v <> FShowBorder then begin
    FShowBorder := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

{ TVpERReportAttributes ***************************************************** }

constructor TVpERReportAttributes.Create (AOwner : TVpEventReport);
begin
  inherited Create;

   FFont           := TVpFont.Create (AOwner);

   FOwner          := AOwner;
   FColor          := clWindow;
   FDotDotDotColor := clBlack;
   FLineColor      := clBlack;
end;

destructor TVpERReportAttributes.Destroy;
begin
  FFont.Free;

  inherited Destroy;
end;

procedure TVpERReportAttributes.SetColor (const v : TColor);
begin
  if v <> FColor then begin
    FColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERReportAttributes.SetDotDotDotColor (const v : TColor);
begin
  if v <> FDotDotDotColor then begin
    FDotDotDotColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERReportAttributes.SetFont (v : TVpFont);
begin
  FFont.Assign (v);
  if Assigned (FOwner) then
    FOwner.Invalidate;
end;

procedure TVpERReportAttributes.SetLineColor (const v : TColor);
begin
  if v <> FLineColor then begin
    FLineColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

{ TVpERAllDayEventAttributes ************************************************ }

constructor TVpERAllDayEventAttributes.Create (AOwner : TVpEventReport);
begin
  inherited Create;

   FFont                 := TVpFont.Create (AOwner);

   FOwner                := AOwner;
   FBackgroundColor      := clWindow;
   FBorderColor          := clBlack;
   FColor                := clBtnFace;
   FEventBackgroundColor := clWindow;
end;

destructor TVpERAllDayEventAttributes.Destroy;
begin
  FFont.Free;

  inherited Destroy;
end;

procedure TVpERAllDayEventAttributes.SetBackgroundColor (const v : TColor);
begin
  if v <> FBackgroundColor then begin
    FBackgroundColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERAllDayEventAttributes.SetBorderColor (const v : TColor);
begin
  if v <> FBorderColor then begin
    FBorderColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERAllDayEventAttributes.SetColor (const v : TColor);
begin
  if v <> FColor then begin
    FColor := v;
    if Assigned (FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERAllDayEventAttributes.SetEventBackgroundColor (const v : TColor);
begin
  if v <> FEventBackgroundColor then begin
    FEventBackgroundColor := v;
    if Assigned (fowner) then
      FOwner.Invalidate;
  end;
end;

procedure TVpERAllDayEventAttributes.SetFont (v : TVpFont);
begin
  FFont.Assign (v);
  if Assigned (FOwner) then
    FOwner.Invalidate;
end;

{ TVpEventReport ************************************************************ }

constructor TVpEventReport.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  erInLinkHandler       := False;

  FAllDayEventAttr      := TVpERAllDayEventAttributes.Create (Self);
  FHeadAttr             := TVpERHeadAttributes.Create (Self);
  FReportAttr           := TVpERReportAttributes.Create (Self);
  erClickTimer          := TTimer.Create (Self);

  SetLength (erEventArray, MaxVisibleEvents);

  erClickTimer.Enabled  := False;
  erClickTimer.Interval := ClickDelay;
  erClickTimer.OnTimer  := erEditInPlace;

  FDisplayDate          := Now;
  FMultiLine            := True;
  FShowEventTime        := True;
  FTimeFormat           := tf24Hour;
  Color                 := clWhite;
  FSeparateEvents       := esLine;
  FShowCategoryColors   := True;
  FShowResourceName     := True;
  FDateLabelFormat      := 'dddd, mmmm dd, yyyy';

  { size }
  Height                := 225;
  Width                 := 265;

  FDefaultPopup := TPopupMenu.Create (Self);
  InitializeDefaultPopup;

  erHookUp;
end;

destructor TVpEventReport.Destroy;
begin
  FAllDayEventAttr.Free;
  FHeadAttr.Free;
  FReportAttr.Free;
  erClickTimer.Free;

  inherited Destroy;
end;

procedure TVpEventReport.CMWantSpecialKey (var Msg : TCMWantSpecialKey);
begin
  inherited;
  Msg.Result := 1;
end;

procedure TVpEventReport.CreateParams (var Params : TCreateParams);
begin
  inherited CreateParams (Params);

  with Params do begin
    Style             := Style or WS_TABSTOP;
    WindowClass.style := CS_DBLCLKS;
  end;
end;

procedure TVpEventReport.DeleteActiveEvent (Verify : Boolean);
var
  Str: string;
  DoIt: Boolean;
begin
  DoIt := not Verify;

  if FActiveEvent <> nil then begin
    Str := '"' + FActiveEvent.Description + '"';

    if Verify then
      DoIt := (MessageDlg(RSDelete + ' ' + Str + ' ' + RSFromSchedule
        + #13#10#10 + RSPermanent, mtconfirmation,
        [mbYes, mbNo], 0) = mrYes);

    if DoIt then begin
      FActiveEvent.Deleted := true;
      FActiveEvent         := nil;
      DataStore.PostEvents;
      Invalidate;
    end;
  end;
end;

procedure TVpEventReport.EditEvent;
var
  AllowIt: Boolean;
begin
  if FActiveEvent <> nil then begin
    AllowIt := true;
    { call the user defined BeforeEdit event }
    if Assigned (FBeforeEdit) then
      FBeforeEdit(Self, FActiveEvent, AllowIt);

    if AllowIt then begin
      { create and spawn the in-place editor }
      erInPlaceEditor        := TVpERInPlaceEdit.Create (Self);
      erInPlaceEditor.Parent := self;
      erInPlaceEditor.OnExit := EndEdit;
      if (ShowCategoryColors) and (not FActiveEvent.AllDayEvent) then

        erInPlaceEditor.Move (Rect (erActiveEventRec.Left + TextMargin + VpColorCategoryWidth, 
                                    erActiveEventRec.Top + 2,
                                    erActiveEventRec.Right - TextMargin,
                                    erActiveEventRec.Bottom - 2),
                              True)
      else
        erInPlaceEditor.Move (Rect (erActiveEventRec.Left + TextMargin,
                                    erActiveEventRec.Top + 2,
                                    erActiveEventRec.Right - TextMargin,
                                    erActiveEventRec.Bottom - 2),
                              True);
      erInPlaceEditor.Text   := FActiveEvent.Description;
      Invalidate;
    end;
  end;
end;

procedure TVpEventReport.EndEdit (Sender : TObject);
begin
  if erInPlaceEditor <> nil then begin
    if erInPlaceEditor.Text <> FActiveEvent.Description then begin
      FActiveEvent.Description := erInPlaceEditor.Text;
      FActiveEvent.Changed := True;
      if Assigned (FAfterEdit) then
        FAfterEdit (self, FActiveEvent);
      DataStore.PostEvents;
    end;
    erInPlaceEditor.Free;
    erInPlaceEditor := nil;
    Invalidate;
    SetFocus;
  end;
end;

procedure TVpEventReport.erEditInPlace (Sender : TObject);
begin
  { this is the timer event which spawns an in-place editor }
  { if the event is doublecliked before this timer fires, then the }
  { event is edited in a dialog based editor. }
  erClickTimer.Enabled := False;
  EditEvent;
end;

procedure TVpEventReport.erHookUp;
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

procedure TVpEventReport.erPopulate;
begin
  if DataStore <> nil then begin
    DataStore.Date := FDisplayDate;
    FActiveEvent := GetNextEvent (nil);
  end;
end;

procedure TVpEventReport.erSpawnEventEditDialog (NewEvent : Boolean);
var
  AllowIt: Boolean;
  EventDlg : TVpEventEditDialog;

begin
  if DataStore = nil then Exit;

  AllowIt := false;
  if Assigned (FOwnerEditEvent) then
    FOwnerEditEvent(self, FActiveEvent, DataStore.Resource, AllowIt)
  else begin
    EventDlg := TVpEventEditDialog.Create(nil);
    try
      EventDlg.DataStore := DataStore;
      AllowIt := EventDlg.Execute(FActiveEvent, FTimeFormat);
    finally
      EventDlg.Free;
    end;
  end;

  if AllowIt then begin
    FActiveEvent.Changed := True;
    DataStore.PostEvents;
    Invalidate;
  end else begin
    if NewEvent then begin
      DataStore.Resource.Schedule.DeleteEvent(FActiveEvent);
      FActiveEvent := nil;
    end;
    DataStore.PostEvents;
    Invalidate;
  end;
end;

function TVpEventReport.EventAtCoord (Pt : TPoint) : Boolean;
var
  I : Integer;

begin
  Result := False;
  for I := 0 to Pred (Length (erEventArray)) do begin
    if erEventArray[I].Event = nil then begin
      { we've hit the end of visible events without finding a match }
      FActiveEvent            := nil;
      erActiveEventRec.Top    := 0;
      erActiveEventRec.Bottom := 0;
      erActiveEventRec.Right  := 0;
      erActiveEventRec.Left   := 0;
      Result                  := False;
      Exit;
    end;

    if (Pt.X > erEventArray[I].Rec.Left)   and
       (Pt.X < erEventArray[I].Rec.Right)  and
       (Pt.Y > erEventArray[I].Rec.Top)    and
       (Pt.Y < erEventArray[I].Rec.Bottom) then begin
      { point falls inside this event's rectangle }
      FActiveEvent     := TVpEvent (erEventArray[I].Event);
      erActiveEventRec := erEventArray[I].Rec;
      Result           := True;
      Exit;
    end

    else begin
      { point is not within the boundaries of this event's rectangle. }
      FActiveEvent            := nil;
      erActiveEventRec.Top    := 0;
      erActiveEventRec.Bottom := 0;
      erActiveEventRec.Right  := 0;
      erActiveEventRec.Left   := 0;
      Result                  := False;
    end;
  end;
end;

function TVpEventReport.GetControlType : TVpItemType;
begin
  Result := itDayView; // !!!!! itEventReport
end;

function  TVpEventReport.GetNextEvent (AnEvent : TVpEvent) : TVpEvent;
var
  Idx : Integer;
  i   : Integer;

begin
  Result := nil;

  Idx := -1;
  for i := 0 to Pred (Length (erEventArray)) do
    if erEventArray[i].Event = AnEvent then begin
      Idx := i;
      Break;
    end;

  for i := Idx + 1 to Pred (Length (erEventArray)) do
    if Assigned (erEventArray[i].Event) then begin
      Result := erEventArray[i].Event;
      erActiveEventRec := erEventArray[i].Rec;
      Exit;
    end;

  for i := 0 to Idx - 1 do
    if Assigned (erEventArray[i].Event) then begin
      Result := erEventArray[i].Event;
      erActiveEventRec := erEventArray[i].Rec;
      Break;
    end;
end;

function  TVpEventReport.GetPrevEvent (AnEvent : TVpEvent) : TVpEvent;
var
  Idx : Integer;
  i   : Integer;

begin
  Result := nil;

  Idx := -1;
  for i := 0 to Pred (Length (erEventArray)) do
    if erEventArray[i].Event = AnEvent then begin
      Idx := i;
      Break;
    end;

  if Idx = -1 then
    Exit;

  for i := Idx -1 downto 0 do
    if Assigned (erEventArray[i].Event) then begin
      Result := erEventArray[i].Event;
      Exit;
    end;

  for i := Pred (Length (erEventArray)) downto Idx + 1 do
    if Assigned (erEventArray[i].Event) then begin
      Result := erEventArray[i].Event;
      Break;
    end;
end;

procedure TVpEventReport.InitializeDefaultPopup;
var
  NewItem    : TMenuItem;
  NewSubItem : TMenuItem;

begin
  if RSDayPopupAdd <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupAdd;
    NewItem.OnClick := PopupAddEvent;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupEdit <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupEdit;
    NewItem.OnClick := PopupEditEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupDelete <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupDelete;
    NewItem.OnClick := PopupDeleteEvent;
    NewItem.Tag := 1;
    FDefaultPopup.Items.Add (NewItem);
  end;

  if RSDayPopupNav <> '' then begin
    NewItem := TMenuItem.Create (Self);
    NewItem.Caption := RSDayPopupNav;
    NewItem.Tag := 0;
    FDefaultPopup.Items.Add (NewItem);

    if RSDayPopupNavToday <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavToday;
      NewSubItem.OnClick := PopupToday;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavYesterday <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavYesterday;
      NewSubItem.OnClick := PopupYesterday;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavTomorrow <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavTomorrow;
      NewSubItem.OnClick := PopupTomorrow;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextDay <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextDay;
      NewSubItem.OnClick := PopupNextDay;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevDay <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevDay;
      NewSubItem.OnClick := PopupPrevDay;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextWeek;
      NewSubItem.OnClick := PopupNextWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevWeek <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevWeek;
      NewSubItem.OnClick := PopupPrevWeek;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextMonth;
      NewSubItem.OnClick := PopupNextMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevMonth <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevMonth;
      NewSubItem.OnClick := PopupPrevMonth;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavNextYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavNextYear;
      NewSubItem.OnClick := PopupNextYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;

    if RSDayPopupNavPrevYear <> '' then begin
      NewSubItem := TMenuItem.Create (Self);
      NewSubItem.Caption := RSDayPopupNavPrevYear;
      NewSubItem.OnClick := PopupPrevYear;
      NewSubItem.Tag := 0;
      NewItem.Add (NewSubItem);
    end;
  end;
end;

procedure TVpEventReport.LinkHandler(Sender: TComponent;
  NotificationType: TVpNotificationType; const Value: Variant);
begin
  erInLinkHandler := true;
  try
    case NotificationType of
      neDateChange: begin
        Date := Value;
      end;
      neDataStoreChange: Invalidate;
      neInvalidate: Invalidate;
    end;
  finally
    erInLinkHandler := false;
  end;
end;

procedure TVpEventReport.Loaded;
begin
  inherited;
  erLoaded := true;
  erPopulate;
end;

procedure TVpEventReport.KeyDown (var Key : Word; Shift : TShiftState);
var
  PopupPoint : TPoint;

begin
  case Key of
    VK_DELETE : DeleteActiveEvent(true);
    VK_RIGHT  : if Shift = [ssShift] then
                  PopupNextWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupNextMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupNextYear (Self)
                else if Shift = [] then
                  Date := Date + 1;
    VK_LEFT   : if Shift = [ssShift] then
                  PopupPrevWeek (Self)
                else if (Shift = [ssCtrl]) then
                  PopupPrevMonth (Self)
                else if (Shift = [ssShift, ssCtrl]) then
                  PopupPrevYear (Self)
                else if Shift = [] then
                  Date := Date - 1;
    VK_UP     : begin
                  FActiveEvent := GetPrevEvent (FActiveEvent);
                  Invalidate;
                end;
    VK_DOWN   : begin
                  FActiveEvent := GetNextEvent (FActiveEvent);
                  Invalidate;
                end;
    VK_INSERT : PopupAddEvent (Self);
    VK_TAB    :
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
    VK_F2 :
      EditEvent;
  end;
end;

procedure TVpEventReport.Paint;
begin
  RenderToCanvas (Canvas,
                  Rect (0, 0, Width, Height),
                  ra0,
                  1,
                  FDisplayDate,
                  FTopLine,
                  -1,
                  gr15Min,
                  False);
end;

procedure TVpEventReport.PopupAddEvent (Sender : TObject);
var
  StartTime : TDateTime;
  EndTime   : TDateTime;

begin
  if ReadOnly then                                                     
    Exit;                                                              
  if not CheckCreateResource then                                      
    Exit;                                                              
  if not Assigned (DataStore) then
    Exit;
  if not Assigned (DataStore.Resource) then
    Exit;

  StartTime := trunc (FDisplayDate) +
               EncodeTime (12, 0, 0, 0);
  EndTime := StartTime + EncodeTime (13, 0, 0, 0);;
  FActiveEvent := DataStore.Resource.Schedule.AddEvent (
                      DataStore.GetNextID (EventsTableName),
                      StartTime, EndTime);

  Repaint;                                                             
  { edit this new event }
  erSpawnEventEditDialog (True);
end;

procedure TVpEventReport.PopupDeleteEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  Repaint;                                                             
  if FActiveEvent <> nil then
    DeleteActiveEvent (True);
end;

procedure TVpEventReport.PopupEditEvent (Sender : TObject);
begin
  if ReadOnly then                                                     
    Exit;                                                              
  Repaint;                                                             
  if FActiveEvent <> nil then
    { edit this Event }
    erSpawnEventEditDialog (False);
end;

procedure TVpEventReport.PopupNextDay (Sender : TObject);
begin
  Date := Date + 1;
end;

procedure TVpEventReport.PopupNextMonth (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate(Date, Y, M, D);
  if M = 12 then begin
    M := 1;
    Y := Y + 1;
  end else
    M := M + 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;

procedure TVpEventReport.PopupNextWeek (Sender : TObject);
begin
  Date := Date + 7;
end;

procedure TVpEventReport.PopupNextYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y + 1, M, 1);
end;

procedure TVpEventReport.PopupPrevDay (Sender : TObject);
begin
  Date := Date - 1;
end;

procedure TVpEventReport.PopupPrevMonth(Sender : TObject);
var
  M, D, Y : Word;
begin
  DecodeDate(Date, Y, M, D);
  if M = 1 then begin
    M := 12;
    Y := Y - 1;
  end else
    M := M - 1;
  if (D > DaysInMonth(Y, M)) then
    D := DaysInMonth(Y, M);

  Date := EncodeDate(Y, M, D);
end;

procedure TVpEventReport.PopupPrevWeek (Sender : TObject);
begin
  Date := Date - 7;
end;

procedure TVpEventReport.PopupPrevYear (Sender : TObject);
var
  M, D, Y : Word;

begin
  DecodeDate (Date, Y, M, D);
  Date := EncodeDate (Y - 1, M, 1);
end;

procedure TVpEventReport.PopupToday (Sender : TObject);
begin
  Date := Now;
end;

procedure TVpEventReport.PopupTomorrow (Sender : TObject);
begin
  Date := Now + 1;
end;

procedure TVpEventReport.PopupYesterday (Sender : TObject);
begin
  Date := Now - 1;
end;

procedure TVpEventReport.RenderToCanvas (RenderCanvas : TCanvas;
                                         RenderIn     : TRect;
                                         Angle        : TVpRotationAngle;
                                         Scale        : Extended;
                                         RenderDate   : TDateTime;
                                         StartLine    : Integer;
                                         StopLine     : Integer;
                                         UseGran      : TVpGranularity;
                                         DisplayOnly  : Boolean);


var
  ColHeadRect     : TRect;
  ADEventsRect    : TRect;
  SaveBrushColor  : TColor;
  SavePenStyle    : TPenStyle;
  SavePenColor    : TColor;
  RealWidth       : Integer;
  RealHeight      : Integer;
  RealLeft        : Integer;
  RealRight       : Integer;
  RealTop         : Integer;
  RealBottom      : Integer;
  Rgn             : HRGN;
  RealRowHeight   : Integer;

  BevelShadowColor     : TColor;
  BevelHighlightColor  : TColor;
  BevelButtonFace      : TColor;
  RealDotDotDotColor   : TColor;
  ADBackgroundColor    : TColor;
  ADEventBackgroundColor : TColor;

  BevelDarkShadow      : TColor;
  WindowColor          : TColor;
  RealLineColor        : TColor;
  RealColor            : TColor;
  ADEventBorderColor   : TColor;                                         

  procedure SetMeasurements;
  begin
    RealWidth     := TPSViewportWidth (Angle, RenderIn);
    RealHeight    := TPSViewportHeight (Angle, RenderIn);
    RealLeft      := TPSViewportLeft (Angle, RenderIn);
    RealRight     := TPSViewportRight (Angle, RenderIn);
    RealTop       := TPSViewportTop (Angle, RenderIn);
    RealBottom    := TPSViewportBottom (Angle, RenderIn);
    RenderCanvas.Font.Assign (ReportAttr.Font);
    RealRowHeight := RenderCanvas.TextHeight ('YyGg0');
  end;

  procedure ClearRect;
  begin
    RenderCanvas.Brush.Color := ReportAttr.Color;
    TPSFillRect (RenderCanvas, Angle, RenderIn,
                 Rect (RealLeft, RealTop, RealRight, RealBottom));
  end;

  procedure DrawBorders;
  begin
    if FDrawingStyle = dsFlat then begin
      { draw an outer and inner bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft, RealTop,
                                               RealRight - 1, RealBottom - 1)),
                     BevelShadowColor,
                     BevelHighlightColor);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1, RealTop + 1,
                                               RealRight - 2, RealBottom - 2)),
                     BevelShadowColor,
                     BevelHighlightColor);
    end else if FDrawingStyle = ds3d then begin
    { draw a 3d bevel }
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft, RealTop,
                                               RealRight - 1, RealBottom - 1)),
                     BevelShadowColor,
                     BevelShadowColor);
      DrawBevelRect (RenderCanvas,
                     TPSRotateRectangle (Angle, RenderIn,
                                         Rect (RealLeft + 1, RealTop + 1,
                                               RealRight - 2, RealBottom - 2)),
                     BevelDarkShadow,
                     BevelButtonFace);
    end;
  end;

  procedure DrawColHead;
  var
    ColRowHeight : Integer;
    ResStr       : string;
    ResStrLen    : Integer;

  begin
    RenderCanvas.Font.Assign (HeadAttr.Font);
    try
      ColRowHeight := RenderCanvas.TextHeight ('yYgG0');
      ColHeadRect.Left := RealLeft;
      ColHeadRect.Right := RealRight;
      ColHeadRect.Top := RealTop;
      ColHeadRect.Bottom := ColHeadRect.Top + ColRowHeight +
                            TextMargin * 2 + 2;
      if ShowResourceName and (DataStore <> nil) and
         (DataStore.Resource <> nil)then
        ColHeadRect.Bottom := ColHeadRect.Bottom + ColRowHeight + TextMargin;

      erHeaderHeight := ColHeadRect.Bottom - ColHeadRect.Top;

      RenderCanvas.Brush.Color := HeadAttr.Color;
      TPSFillRect (RenderCanvas, Angle, RenderIn,
                   Rect (ColHeadRect.Left + 2,
                         ColHeadRect.Top + 2,
                         ColHeadRect.Right - 2,
                         ColHeadRect.Bottom));

      ResStr := FormatDateTime (FDateLabelFormat, RenderDate);
      ResStrLen := RenderCanvas.TextWidth (ResStr);
      if ResStrLen > ColHeadRect.Right - ColHeadRect.Left then begin
        ResStr := GetDisplayString(RenderCanvas, ResStr, 0,
          ColHeadRect.Right - ColHeadRect.Left);
      end;

      TPSTextOut (RenderCanvas, Angle, RenderIn,
                  ColHeadRect.Left + TextMargin + 2,
                  ColHeadRect.Top + TextMargin + 2,
                  ResStr);

      if (DataStore <> nil) and (DataStore.Resource <> nil) and
         FShowResourceName then begin
        { fix Res String }
        ResStr := DataStore.Resource.Description;
        ResStrLen := RenderCanvas.TextWidth(ResStr);
        if ResStrLen > ColHeadRect.Right - ColHeadRect.Left then begin
          ResStr := GetDisplayString(RenderCanvas, ResStr, 0,
            ColHeadRect.Right - ColHeadRect.Left);
        end;
        TPSTextOut (RenderCanvas, Angle, RenderIn,
                    ColHeadRect.Left + TextMargin + 2,
                    ColHeadRect.Top + ColRowHeight + TextMargin * 2 + 2,
                    ResStr);
      end;

      if HeadAttr.ShowBorder then begin
        if FDrawingStyle = dsFlat then begin
          { draw an outer and inner bevel }
{          HeadRect.Left := RealLeft + 1;
          HeadRect.Top := RealTop + 1;
          HeadRect.Right := RealRight - 1;
          HeadRect.Bottom := HeadRect.Top + wvHeaderHeight;
          TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect); }
          DrawBevelRect (RenderCanvas,
                         TPSRotateRectangle (Angle, RenderIn,
                                             Rect (ColHeadRect.Left + 2,
                                                   ColHeadRect.Top + 2,
                                                   ColHeadRect.Right - 2,
                                                   ColHeadRect.Bottom)),
                         BevelHighlightColor, BevelShadowColor);
        end else if FDrawingStyle = ds3d then begin
          { draw a 3d bevel }
{          HeadRect.Left := RealLeft + 2;
          HeadRect.Top := RealTop + 2;
          HeadRect.Right := RealRight - 3;
          HeadRect.Bottom := RealTop + wvHeaderHeight;
          TPSFillRect (RenderCanvas, Angle, RenderIn, HeadRect); }
          DrawBevelRect (RenderCanvas,
                         TPSRotateRectangle (Angle, RenderIn,
                                             Rect (ColHeadRect.Left + 3,
                                                   ColHeadRect.Top + 3,
                                                   ColHeadRect.Right - 4,
                                                   ColHeadRect.Bottom - 1)),
                         BevelHighlightColor, BevelDarkShadow);
          end;
      end;
    finally
      RenderCanvas.Font.Assign (FReportAttr.Font);
    end;
  end;

  function GetCategoryBackgroundColor (Event : TVpEvent) : TColor;
  begin
    if Assigned (DataStore) then
      case Event.Category of
        0 : Result := DataStore.CategoryColorMap.Category0.BackgroundColor;
        1 : Result := DataStore.CategoryColorMap.Category1.BackgroundColor;
        2 : Result := DataStore.CategoryColorMap.Category2.BackgroundColor;
        3 : Result := DataStore.CategoryColorMap.Category3.BackgroundColor;
        4 : Result := DataStore.CategoryColorMap.Category4.BackgroundColor;
        5 : Result := DataStore.CategoryColorMap.Category5.BackgroundColor;
        6 : Result := DataStore.CategoryColorMap.Category6.BackgroundColor;
        7 : Result := DataStore.CategoryColorMap.Category7.BackgroundColor;
        8 : Result := DataStore.CategoryColorMap.Category8.BackgroundColor;
        9 : Result := DataStore.CategoryColorMap.Category9.BackgroundColor;
        else
          Result := WindowColor;
      end
    else
      Result := WindowColor;
  end;

  function DrawAllDayEvents (    ADate   : TDateTime;
                                 DayRect : TRect;
                             var EAIndex : Integer) : Boolean;
  var
    ADEventsList       : TList;
    TempList           : TList;
    I, J, K            : Integer;
    Event              : TVpEvent;
    ADEventRect        : TRect;
    StartsBeforeRange  : Boolean;
    MaxADEvents        : Integer;
    Skip               : Boolean;
    ADTextHeight       : Integer;
    EventStr           : string;

  begin
    Result := False;
    { initialize the All Day Events area... }
    ADEventsRect := DayRect;

    if (DataStore = nil) or (DataStore.Resource = nil) then
      Exit;

    { Collect all of the events for this range and determine the maximum     }
    { number of all day events for the range of days covered by the control. }
    MaxADEvents := 0;

    ADEventsList := TList.Create;
    try
      TempList := TList.Create;
      try
        { get the all day events for the day specified by ADate + I }
        DataStore.Resource.Schedule.AllDayEventsByDate(ADate, TempList);

        { Iterate through these events and place them in ADEventsList    }
        Skip := false;
        for J := 0 to pred(TempList.Count) do begin
          if AdEventsList.Count > 0 then begin
            for K := 0 to pred(AdEventsList.Count) do begin
              if TVpEvent(AdEventsList[K]) = TVpEvent(TempList[J]) then begin
                Skip := true;
                Break;
              end;
            end;
            if not Skip then
              AdEventsList.Add(TempList[J]);
          end else
            AdEventsList.Add(TempList[J]);
        end;

        if TempList.Count > MaxADEvents then
          MaxADEvents := TempList.Count;
      finally
        TempList.Free;
      end;

      if MaxADEvents > 0 then begin
        { Set attributes }
        RenderCanvas.Brush.Color := ADBackgroundColor;
        RenderCanvas.Font.Assign (AllDayEventAttr.Font);

        { Measure the AllDayEvent TextHeight }
        ADTextHeight := RenderCanvas.TextHeight(VpProductName) +
                        TextMargin + TextMargin div 2;

        { Build the AllDayEvent rect based on the value of MaxADEvents }
        if AdEventsRect.Top + (MaxADEvents * ADTextHeight) +
           TextMargin * 2 > DayRect.Bottom then
          ADeventsrect.Bottom := DayRect.Bottom
        else
          ADEventsRect.Bottom := AdEventsRect.Top +
                                 (MaxADEvents * ADTextHeight) + TextMargin * 2;

        { Clear the AllDayEvents area }
        TpsFillRect(RenderCanvas, Angle, RenderIn, ADEventsRect);

        StartsBeforeRange  := false;
        { Cycle through the all day events and draw them appropriately }
        for I := 0 to pred(ADEventsList.Count) do begin

          Event := ADEventsList[I];

          { set the top of the event's rect }
          AdEventRect.Top := ADEventsRect.Top + TextMargin +
                             (I  * ADTextHeight);

          if ADEventsRect.Top + TextMargin + ((I + 1)  * ADTextHeight) -
             TextMargin > DayRect.Bottom then begin
            RenderCanvas.Brush.Color := ReportAttr.DotDotDotColor;
            { draw dot dot dot }
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right - 20,  DayRect.Bottom - 7,
                               DayRect.Right - 17,  DayRect.Bottom - 4));
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right - 13,  DayRect.Bottom - 7,
                               DayRect.Right - 10,  DayRect.Bottom - 4));
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (DayRect.Right -  6,  DayRect.Bottom - 7,
                               DayRect.Right -  3,  DayRect.Bottom - 4));
            break;
          end;

          { see if the event began before the start of the range }
          if (Event.StartTime < trunc(RenderDate)) then
            StartsBeforeRange := true;

          AdEventRect.Bottom := ADEventRect.Top + ADTextHeight;
          AdEventRect.Left := AdEventsRect.Left + (TextMargin div 2);
          AdEventRect.Right := DayRect.Right;

          if (StartsBeforeRange) then
            EventStr := '>> '
          else
            EventStr := '';

          EventStr := EventStr + Event.Description;

          RenderCanvas.Brush.Color := ADEventBackgroundColor;
          RenderCanvas.Pen.Color := ADEventBorderColor;
          TPSRectangle (RenderCanvas, Angle, RenderIn,
                        ADEventRect.Left + TextMargin,
                        ADEventRect.Top + TextMargin div 2,
                        ADEventRect.Right - TextMargin,
                        ADEventRect.Top + ADTextHeight + TextMargin div 2);
          TPSTextOut (RenderCanvas,Angle, RenderIn,
                      AdEventRect.Left + TextMargin * 2 + TextMargin div 2,
                      AdEventRect.Top + TextMargin,
                      EventStr);
          Result := True;
          erEventArray[EAIndex].Rec := Rect (ADEventRect.Left + TextMargin,
                                             ADEventRect.Top + TextMargin,
                                             ADEventRect.Right - TextMargin,
                                             ADEventRect.Bottom);
          erEventArray[EAIndex].Event := Event;
          Inc (EAIndex);
        end; { for I := 0 to pred(ADEventsList.Count) do ... }

      end;   { if MaxADEvents > 0 }

    finally
      ADEventsList.Free;
    end;
  end;

  procedure DrawEvents (ADate : TDateTime);
  var
    EventList  : TList;
    j          : Integer;
    TextRect   : TRect;
    EventStr   : string;
    StrLn      : Integer;
    TextDone   : Boolean;
    TextLen    : Integer;
    i          : Integer;
    EAIndex    : Integer;
    TextTop    : Integer;
    TextOffset : Integer;

  begin
    EAIndex := 0;

    for I := 0 to pred (Length (erEventArray)) do begin
      erEventArray[I].Rec.TopLeft := Point(-1, -1);
      erEventArray[I].Rec.BottomRight := Point(-1, -1);
      erEventArray[I].Event := nil;
    end;

    if not Assigned (DataStore) then
      Exit;
    if not Assigned (DataStore.Resource) then
      Exit;

    EventList := TList.Create;
    try
      { populate the eventlist with events for this day }
      DataStore.Resource.Schedule.EventsByDate(ADate, EventList);
      { initialize TextRect for this day }
      TextRect.TopLeft := Point (ColHeadRect.Left, 
                                 ColHeadRect.Bottom);
      TextRect.BottomRight := Point (ColHeadRect.Right,
                                     TextRect.Top + RealRowHeight);

      { Handle All Day Events }
      if DrawAllDayEvents (ADate,
                           Rect (TextRect.Left + 2,
                                 TextRect.Top + 1,
                                 TextRect.Right - 2,
                                 RealBottom),
                           EAIndex) then begin
        TextRect.Bottom := TextRect.Bottom + (ADEventsRect.Bottom - TextRect.Top);
        TextRect.Top := ADEventsRect.Bottom;
      end;

      { Discard AllDayEvents, because they are drawn above. }
      for j := pred(EventList.Count) downto 0 do
        if TVpEvent (EventList[j]).AllDayEvent then
          EventList.Delete(j);

      { iterate the events, painting them one by one }
      for j := 0 to pred (EventList.Count) do begin
        { if the TextRect extends below the available space then draw a   }
        { dot dot dot to indicate there are more events than can be drawn }
        { in the available space }
        if TextRect.Bottom - TextMargin > RealBottom then begin
          RenderCanvas.Brush.Color := RealDotDotDotColor; 
          { draw dot dot dot }
          TPSFillRect (RenderCanvas, Angle, RenderIn,
                       Rect (TextRect.Right - 20,  TextRect.Bottom - 7,
                             TextRect.Right - 17,  TextRect.Bottom - 4));
          TPSFillRect (RenderCanvas, Angle, RenderIn,
                       Rect (TextRect.Right - 13,  TextRect.Bottom - 7,
                             TextRect.Right - 10,  TextRect.Bottom - 4));
          TPSFillRect (RenderCanvas, Angle, RenderIn,
                       Rect (TextRect.Right -  6,  TextRect.Bottom - 7,
                             TextRect.Right -  3,  TextRect.Bottom - 4));
          break;
        end;

        { format the display text }
        EventStr := '';
        if ShowEventTime then begin
          if TimeFormat = tf24Hour then
            EventStr := FormatDateTime ('hh:mm',
                            TVpEvent (EventList.List^[j]).StartTime) +
                            ' - ' + FormatDateTime('hh:mm',
                            TVpEvent (EventList.List^[j]).EndTime) + ': '
          else
            EventStr := FormatDateTime ('hh:mm AM/PM',
                          TVpEvent (EventList.List^[j]).StartTime) +
                          ' - ' + FormatDateTime('hh:mm AM/PM',
                          TVpEvent (EventList.List^[j]).EndTime) + ': ';
        end;

        if EventStr = '' then
          EventStr := TVpEvent (EventList.List^[j]).Description
        else
          EventStr := EventStr + ' '
            + TVpEvent(EventList.List^[j]).Description;

        { set the event font }
        RenderCanvas.Font.Assign (FReportAttr.Font);
        RenderCanvas.Brush.Color := RealColor;

        if ShowCategoryColors then
          TextOffset := VpColorCategoryWidth
        else
          TextOffset := 0;

        if not MultiLine then begin
          TextRect.Bottom := TextRect.Top + RealRowHeight + TextMargin * 2;
          StrLn := RenderCanvas.TextWidth(EventStr);
          if (StrLn > TextRect.Right - TextRect.Left - TextMargin) then begin
            EventStr := GetDisplayString (RenderCanvas,
                                          EventStr,
                                          0,
                                          TextRect.Right - TextRect.Left -
                                          (TextMargin * 2));
          end;
          RenderCanvas.Brush.Color := GetCategoryBackgroundColor (
              TVpEvent (EventList.List^[j]));
          TPSFillRect (RenderCanvas, Angle, RenderIn,
                       Rect (TextRect.Left + 2, TextRect.Top + 1,
                             TextRect.Right - 2, TextRect.Bottom - 1));

          { write the event text }
          TPSTextOut (RenderCanvas, Angle, RenderIn,
                      TextRect.Left + TextMargin + TextOffset,
                      TextRect.Top + (TextMargin div 2), EventStr);
        end else begin
          TextLen  := Length (EventStr);
          TextDone := False;
          TextTop  := TextRect.Top;
          while (not TextDone) and (TextRect.Top < RealBottom) do begin
            RenderCanvas.Brush.Color := GetCategoryBackgroundColor (
                TVpEvent (EventList.List^[j]));
            TPSFillRect (RenderCanvas, Angle, RenderIn,
                         Rect (TextRect.Left + 2, TextRect.Top + 3,
                               TextRect.Right - 2, TextRect.Bottom + TextMargin * 2 - 1));
            i := RenderTextToRect (RenderCanvas, Angle, RenderIn,
                                   Rect (TextRect.Left + TextMargin div 2 + TextOffset,
                                         TextRect.Top,
                                         TextRect.Right,
                                         TextRect.Bottom + TextMargin * 2),
                                   EventStr);
            if i < TextLen then begin
              EventStr := Copy (EventStr, i, TextLen - i + 1);
              TextLen := Length (EventStr);
              TextRect.Top := TextRect.Top + RealRowHeight;
              TextRect.Bottom := TextRect.Top + RealRowHeight;
            end else
              Break;
          end;
          TextRect.Top    := TextTop;
          TextRect.Bottom := TextRect.Bottom + TextMargin * 2;
        end;

        if FActiveEvent = TVpEvent (EventList.List^[j]) then begin
          TPSDrawFocusRect (RenderCanvas, Angle, RenderIn,
                            Rect (TextRect.Left + 2 + TextOffset,
                                  TextRect.Top + 1,
                                  TextRect.Right - 2, TextRect.Bottom - 1));
        end;

        if FShowCategoryColors then begin
          RenderCanvas.Brush.Color := DataStore.CategoryColorMap.GetColor (
              TVpEvent (EventList.List^[j]).Category);
          TPSFillRect (RenderCanvas, Angle, RenderIn,
                       Rect (TextRect.Left + 2, TextRect.Top + 3,
                             TextRect.Left + TextOffset,
                             TextRect.Bottom - 2));
        end;

        { update the EventArray }
        erEventArray[EAIndex].Rec := TextRect;
        erEventArray[EAIndex].Event := TVpEvent (EventList.List^[j]);
        Inc (EAIndex);

        TextRect.Top := TextRect.Bottom;
        case FSeparateEvents of
          esNone     : begin
          end;
          esSpace    : begin
            TextRect.Top := TextRect.Top + TextMargin;
          end;
          esBigSpace : begin
            TextRect.Top := TextRect.Top + TextMargin * 2;
          end;
          esLine     : begin
            TextRect.Top := TextRect.Top + TextMargin + 1;
            RenderCanvas.Pen.Width := 1;
            RenderCanvas.Pen.Color := RealLineColor;
            TPSMoveTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Left + 2,
                       TextRect.Bottom + TextMargin div 2);
            TPSLineTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Right - 2,
                       TextRect.Bottom + TextMargin div 2);
          end;
          es3dLine   : begin
            TextRect.Top := TextRect.Top + TextMargin + 2;
            RenderCanvas.Pen.Width := 1;
            RenderCanvas.Pen.Color := clBtnShadow;
            TPSMoveTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Left + 2,
                       TextRect.Bottom + TextMargin div 2 + 1);
            TPSLineTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Right - 2,
                       TextRect.Bottom + TextMargin div 2 + 1);
            RenderCanvas.Pen.Color := clBtnFace;
            TPSMoveTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Left + 2,
                       TextRect.Bottom + TextMargin div 2);
            TPSLineTo (RenderCanvas, Angle, RenderIn,
                       TextRect.Right - 2,
                       TextRect.Bottom + TextMargin div 2);
          end;
        end;
        TextRect.Bottom := TextRect.Top + RealRowHeight;
      end; { for loop }
    finally
      EventList.Free;
    end;
  end;


begin
  if DisplayOnly then begin
    RealColor              := clWhite;
    BevelShadowColor       := clBlack;
    BevelHighlightColor    := clBlack;
    BevelButtonFace        := clWhite;
    BevelDarkShadow        := clBlack;
    RealDotDotDotColor     := clBlack;
    RealLineColor          := clBlack;
    ADBackgroundColor      := clWhite;
    ADEventBackgroundColor := clWhite;
    ADEventBorderColor     := clBlack;
    WindowColor            := clWhite;
  end else begin
    RealColor              := ReportAttr.Color;
    BevelShadowColor       := clBtnShadow;
    BevelHighlightColor    := clBtnHighlight;
    BevelButtonFace        := clBtnFace;
    BevelDarkShadow        := cl3DDkShadow;
    RealDotDotDotColor     := ReportAttr.DotDotDotColor;
    RealLineColor          := ReportAttr.LineColor;
    ADBackgroundColor      := AllDayEventAttr.BackgroundColor;
    ADEventBackgroundColor := AllDayEventAttr.EventBackgroundColor;
    ADEventBorderColor     := AllDayEventAttr.BorderColor;
    WindowColor            := Color;
  end;

  SetMeasurements;

  if RenderDate = 0 then
    RenderDate := FDisplayDate;

  if RenderDate = 0 then
    RenderDate := Now;

{  if StartLine < 0 then
    StartLine := FTopLine; } // !!!

{  if DisplayOnly then
    ScrollBarOffset := 2
  else
    ScrollBarOffset := 14;} // !!!!

  SavePenStyle   := RenderCanvas.Pen.Style;
  SaveBrushColor := RenderCanvas.Brush.Color;
  SavePenColor   := RenderCanvas.Pen.Color;

  Rgn := CreateRectRgn (RenderIn.Left, RenderIn.Top,
                        RenderIn.Right, RenderIn.Bottom);
  try
    SelectClipRgn (RenderCanvas.Handle, Rgn);

    { Calculate Row Header }

    ClearRect;

    DrawBorders;

    DrawColHead;

    { Draw the regular events }
    DrawEvents (RenderDate);

    { Reinstate RenderCanvas settings        }
    RenderCanvas.Pen.Style := SavePenStyle;
    RenderCanvas.Brush.Color := SaveBrushColor;
    RenderCanvas.Pen.Color := SavePenColor;

  finally
    SelectClipRgn (RenderCanvas.Handle, 0);
    DeleteObject (Rgn);
  end;
end;

procedure TVpEventReport.SetDateLabelFormat (const v : string);
begin
  if v <> FDateLabelFormat then begin
    FDateLabelFormat := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetDisplayDate (const v : TDateTime);
begin
  if v <> FDisplayDate then begin
    erClickTimer.Enabled := false;
    if erInPlaceEditor <> nil then
      EndEdit(Self);
    FDisplayDate := v;
    Invalidate;
    if erLoaded then
      erPopulate;
  end;
end;

procedure TVpEventReport.SetDrawingStyle (const v : TVpDrawingStyle);
begin
  if v <> FDrawingStyle then begin
    FDrawingStyle := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetMultiLine (const v : Boolean);
begin
  if v <> FMultiLine then begin
    FMultiLine := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetSeparateEvents (const v : TVpEREventSeparator);
begin
  if FSeparateEvents <> v then begin
    FSeparateEvents := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetShowCategoryColors (const v : Boolean);
begin
  if v <> FShowCategoryColors then begin
    FShowCategoryColors := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetShowEventTime (const v : Boolean);
begin
  if v <> FShowEventTime then begin
    FShowEventTime := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetShowResourceName (const v : Boolean);
begin
  if v <> FShowResourceName then begin
    FShowResourceName := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.SetTimeFormat (const v : TVpTimeFormat);
begin
  if v <> FTimeFormat then begin
    FTimeFormat := v;
    Invalidate;
  end;
end;

procedure TVpEventReport.WMLButtonDblClk(var Msg : TWMLButtonDblClk);
var
  StartTime, EndTime: TDateTime;

begin
  inherited;

  erClickTimer.Enabled := false;

  if not CheckCreateResource then                                      
    Exit;                                                              

  if DataStore = nil then
    Exit;

  { if the mouse was pressed down in the client area, then select the cell. }
  if not focused then SetFocus; 

  if (Msg.YPos > erHeaderHeight) then
  begin
    { The mouse click landed inside the client area }
    { If we have hit an active event then we must want to edit it }
    if FActiveEvent <> nil then begin
      { edit this event }
      erSpawnEventEditDialog (False);
    end
    else if (DataStore.Resource <> nil) then begin
      { otherwise, we must want to create a new event }
      StartTime := trunc(Date) + 1 / 2; { default to 12:00 noon }
      EndTime := StartTime + (30 / MinutesInDay); { StartTime + 30 minutes }
      FActiveEvent := DataStore.Resource.Schedule.AddEvent(
        DataStore.GetNextID('Events'), StartTime, EndTime);
      { edit this new event }
      erSpawnEventEditDialog (True);
    end;
  end;
end;

procedure TVpEventReport.WMLButtonDown (var Msg : TWMLButtonDown);
begin
  inherited;

  if not Focused then SetFocus; 

  if erInPlaceEditor <> nil then 
    EndEdit(Self);

  if (Msg.YPos > erHeaderHeight) then begin
    { If an active event was clicked, then enable the click timer.  If the }
    { item is double clicked before the click timer fires, then the edit   }
    { dialog will appear, otherwise the in-place editor will appear.       }
    if EventAtCoord (Point(Msg.XPos, Msg.YPos)) then
      erClickTimer.Enabled := true;
  end;
  Invalidate;
end;

procedure TVpEventReport.WMRButtonDown (var Msg : TWMRButtonDown);
var
  ClientOrigin : TPoint;
  i            : Integer;

begin
  inherited;

  if not Assigned (PopupMenu) then begin
    if not focused then
      SetFocus;
    { The mouse click landed inside the client area }
    EventAtCoord (Point (Msg.XPos, Msg.YPos));
    erClickTimer.Enabled := false;
    ClientOrigin := GetClientOrigin;

    if not Assigned (FActiveEvent) then
      for i := 0 to FDefaultPopup.Items.Count - 1 do begin
        if (FDefaultPopup.Items[i].Tag = 1) or (ReadOnly) then           
          FDefaultPopup.Items[i].Enabled := False;
      end
    else
      for i := 0 to FDefaultPopup.Items.Count - 1 do
        FDefaultPopup.Items[i].Enabled := True;

    FDefaultPopup.Popup (Msg.XPos + ClientOrigin.x,
                         Msg.YPos + ClientOrigin.y);
  end;
end;

end.

