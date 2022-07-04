{
{ Author: Daniel Sapundjiev
{ Copyright Microdor Ltd.
}

unit DsDockSite;
//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------
uses
  Windows, Classes, Controls, Contnrs, Messages, Graphics, StdCtrls, DsToolBar,
  DsBandRow, DsBand, DsToolBarStyler;
//------------------------------------------------------------------------------
type
  TBandFacadeH = class(TBandFacade)
  public
    procedure setInsets(aInsets: TRect); override;
    function BandLength: Integer; override;
    function Width: Integer; override;
    function BandStart: Integer; override;
    procedure MoveTo(aPos: Integer); override;
    procedure setPos(aPos, aTop: Integer); override;
    function Top: Integer; override;
    procedure setTop(aTop: Integer); override;
  end;
//------------------------------------------------------------------------------
  TBandFacadeV = class(TBandFacade)
  public
    procedure setInsets(aInsets: TRect); override;
    function BandLength: Integer; override;
    function Width: Integer; override;
    function BandStart: Integer; override;
    procedure MoveTo(aPos: Integer); override;
    procedure setPos(aPos, aTop: Integer); override;
    function Top: Integer; override;
    procedure setTop(aTop: Integer); override;
  end;
//------------------------------------------------------------------------------
  TBandRowList = class(TObject)
  private
    fRows: TObjectList;
    fLength: Integer;
    fVertical: boolean;
    fGrowUp: Boolean;
    fMoving: Integer;
  protected
    procedure setVertical(const Value: boolean);
    procedure setLength(const Value: Integer);
    function getRow(Idx: Integer): TBandList;
    function getCount: Integer;
    function getWidth: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateBandFacade: TBandFacade;
    //
    function InsertRow(aIndex: Integer): TBandList;
    // Adds Band to some row
    procedure addBand(aControl: TControl);
    // Remove Band from the list
    procedure removeBand(aControl: TControl);
    // Start Band placing
    procedure startBand(aBand: TDsBand);
    // Мести намествания Band на нова позиция
    function moveBand(aLenPos, aY: Integer): Boolean;
    // Прекратява наместването на Band
    procedure stopBand;
    // Намира Band по контролата, която обгражда
    function Find(aControl: TControl): TDsBand; overload;
    // Намира Band-а на дадената позиция
    function Find(aX, aY: Integer): TDsBand; overload;
    //
    function FindRowForBand(aBand: TDsBand): TBandList;
    //
    function isMovingBand: Boolean;
    procedure UpdateTop;
    property Rows[Idx: Integer]: TBandList read getRow; default;
    property Count: Integer read getCount;
    property Width: Integer read getWidth;
    property Length: Integer read fLength write setLength;
    property Vertical: boolean read fVertical write setVertical;
    property GrowUp: Boolean read fGrowUp write fGrowUp;
  end;//TBandRowList
//------------------------------------------------------------------------------
  TDsDockSite = class;

  TDSToolBarObserver = class(TObserver)
  private
    fDockSite: TDsDockSite;
  public
    constructor Create(aOwner: TDsDockSite); virtual;
    destructor Destroy; override;
    procedure Update(aSubject: TObject; aChange: TSubjectChange); override;
  end;//TMdToolBarObserver
//------------------------------------------------------------------------------
  TDsDockSite = class(TCustomControl)
  private
    fScreenBitmap: TBitmap;
    fBands: TBandRowList;
    fOffset: TPoint;
    fColorTo: TColor;
    fAutoDock: Boolean;
    fStyler: TDSToolBarStyler;
    fOnBandDraw: TNotifyEvent;
    fObserver: TDSToolBarObserver;
    fLocked: Boolean;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure setLocked(const Value: Boolean);
    procedure setColorTo(const Value: TColor);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    property Bands: TBandRowList read fBands;
    procedure Paint; override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure AlignControls(aControl: TControl; var Rect: TRect); override;
    procedure BeginAutoDrag; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSize;
    property Align;
    property Color;
    property ColorTo: TColor read fColorTo write setColorTo;
    property Locked: Boolean read fLocked write setLocked default False;
    // To automatically dock the control when is dragging over
    property AutoDock: Boolean read fAutoDock write fAutoDock default True;
    property PopupMenu;
    property OnBandDraw: TNotifyEvent read fOnBandDraw write fOnBandDraw;
    property onMouseMove;
  end;
//------------------------------------------------------------------------------
  procedure Register;
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
uses
  SysUtils, Types, ToolWin, Forms, Math;
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('DS Menus', [TDsDockSite]);
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TDsDockSite
//------------------------------------------------------------------------------
constructor TDsDockSite.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csOpaque];
  fStyler := TDSToolBarStyler.Create;
  fStyler.Corner := 2;
  fScreenBitmap := TBitmap.Create;
  fBands := TBandRowList.Create;
  fObserver := TDSToolBarObserver.Create(Self);
  fLocked := False;
  DockSite := True;
  Color := $00F5B789;
  ColorTo := $00FFDEDE;
  AutoDock := True;
  Align := alTop;
  AutoSize := True;
end;//Create
//------------------------------------------------------------------------------
destructor TDsDockSite.Destroy;
begin
  FreeAndNil(fObserver);
  FreeAndNil(fBands);
  FreeAndNil(fScreenBitmap);
  FreeAndNil(fStyler);
  inherited Destroy;
end;//Destroy
//------------------------------------------------------------------------------
procedure TDsDockSite.Loaded;
var
  i: Integer;
  vBands: TList;

  function FindPos: Integer;
  var
    j: Integer;
  begin
    if vBands.Count = 0 then begin
      Result := 0;
      Exit;
    end;

    Result := -1;

    if Align in [alNone, alTop] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Top > Controls[i].Top) or
           ((TControl(vBands[j]).Top = Controls[i].Top) and
            (TControl(vBands[j]).Left > Controls[i].Left)) then
        begin
          Result := j;
          Break;
        end;
      end;
    end
    else
    if Align in [alBottom] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Top < Controls[i].Top) or
           ((TControl(vBands[j]).Top = Controls[i].Top) and
            (TControl(vBands[j]).Left > Controls[i].Left)) then
        begin
          Result := j;
          Break;
        end;
      end;
    end
    else
    if Align in [alLeft] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Left > Controls[i].Left) or
           ((TControl(vBands[j]).Left = Controls[i].Left) and
            (TControl(vBands[j]).Top > Controls[i].Top)) then
        begin
          Result := j;
          Break;
        end
      end;
    end
    else
    if Align in [alRight] then begin
      for j := 0 to vBands.Count - 1 do begin
        if (TControl(vBands[j]).Left < Controls[i].Left) or
           ((TControl(vBands[j]).Top = Controls[i].Top) and
            (TControl(vBands[j]).Top > Controls[i].Top)) then
        begin
          Result := j;
          Break;
        end
      end;
    end;

    if Result = -1 then
      Result := vBands.Count;
  end;
  
begin
  inherited Loaded;
  if Align in [alLeft, alRight] then
    Bands.Length := Height
  else
    Bands.Length := Width;
  Bands.Vertical := Align in [alLeft, alRight];
  Bands.GrowUp := Align in [alRight, alBottom];

  vBands := TList.Create;
  try
    for i := 0 to ControlCount - 1 do
    begin
      vBands.Insert(FindPos, Controls[i]);
    end;

    for i := 0 to vBands.Count - 1 do
    begin
      TControl(vBands[i]).Align := alNone;
      if TControl(vBands[i]).Visible or (csDesigning in ComponentState) then
        Bands.AddBand(TControl(vBands[i]));
      if TControl(vBands[i]) is TDSToolBar then
      begin
        TDSToolBar(vBands[i]).Attach(fObserver);
      end;
    end;
  finally
    vBands.Free;
  end;

  if AutoSize then
    AdjustSize;
end;//Loaded
//------------------------------------------------------------------------------
function TDsDockSite.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  if Bands.fMoving > 0 then
  begin
    Result := False;
    Exit;
  end;

  Bands.Vertical := Align in [alLeft, alRight];
  Bands.GrowUp := Align in [alRight, alBottom];
  Result := True;
  if Align in [alLeft, alRight] then
  begin
    NewWidth := Bands.Width;
    if (NewWidth = 0) and (csDesigning in ComponentState) then
      NewWidth := 15;
  end
  else
  begin
    NewHeight := Bands.Width;
    if (NewHeight = 0) and (csDesigning in ComponentState) then
      NewHeight := 15;
  end;
end;//CanAutoSize
//------------------------------------------------------------------------------
procedure TDsDockSite.Paint;
var
  i, j: integer;

  procedure DrawGrabberH(aCanvas: TCanvas; aBand: TDsBand);
  var
    Cnt, j: Integer;
    vClr: TColor;
    vR, vG, vB: Word;
  begin
    Cnt := (aBand.Height - 3) div 5;
    for j := 0 to Cnt - 1do
    begin
       vClr := aCanvas.Pixels[aBand.X + 4, aBand.Y + (j * 5) + 3+ 1];
       vClr := ColorToRGB(vClr);

       vR := Max(GetRValue(vClr) - 64, 0);
       vG := Max(GetGValue(vClr) - 64, 0);
       vB := Max(GetBValue(vClr) - 64, 0);

       aCanvas.Pen.Color := (vR or (vG shl 8) or (vB shl 16));

       aCanvas.MoveTo(aBand.X + 4,  aBand.Y + (j * 5) + 3+ 1);
       aCanvas.LineTo(aBand.X + 4,  aBand.Y + (j * 5) + 3+ 2);
       aCanvas.LineTo(aBand.X + 5,  aBand.Y + (j * 5) + 3+ 2);
       aCanvas.LineTo(aBand.X + 5,  aBand.Y + (j * 5) + 3+ 0);

       vR := Min(GetRValue(vClr) + 64, 255);
       vG := Min(GetGValue(vClr) + 64, 255);
       vB := Min(GetBValue(vClr) + 64, 255);
       aCanvas.Pen.Color := (vR or (vG shl 8) or (vB shl 16));

       aCanvas.MoveTo(aBand.X + 5,  aBand.Y + (j * 5) + 3+ 3);
       aCanvas.LineTo(aBand.X + 6,  aBand.Y + (j * 5) + 3+ 3);
       aCanvas.LineTo(aBand.X + 6,  aBand.Y + (j * 5) + 3+ 1);
    end;
  end;

  procedure DrawGrabberV(aCanvas: TCanvas; aBand: TDsBand);
  var
    Cnt, j: Integer;
    vClr: TColor;
    vR, vG, vB: Word;
  begin
    Cnt := (aBand.Width - 3) div 5;
    for j := 0 to Cnt - 1do
    begin
       vClr := aCanvas.Pixels[aBand.X + (j*5)+3+1, aBand.Y + 4];
       vClr := ColorToRGB(vClr);
       vR := Max(GetRValue(vClr) - 64, 0);
       vG := Max(GetGValue(vClr) - 64, 0);
       vB := Max(GetBValue(vClr) - 64, 0);
       aCanvas.Pen.Color := (vR or (vG shl 8) or (vB shl 16));

       aCanvas.MoveTo(aBand.X + (j * 5) + 3+ 1, aBand.Y + 4);
       aCanvas.LineTo(aBand.X + (j * 5) + 3+ 2, aBand.Y + 4);
       aCanvas.LineTo(aBand.X + (j * 5) + 3+ 2, aBand.Y + 5);
       aCanvas.LineTo(aBand.X + (j * 5) + 3+ 0, aBand.Y + 5);

       vR := Min(GetRValue(vClr) + 64, 255);
       vG := Min(GetGValue(vClr) + 64, 255);
       vB := Min(GetBValue(vClr) + 64, 255);
       aCanvas.Pen.Color := (vR or (vG shl 8) or (vB shl 16));
       aCanvas.MoveTo(aBand.X + (j * 5) + 3+ 3, aBand.Y + 5);
       aCanvas.LineTo(aBand.X + (j * 5) + 3+ 3, aBand.Y + 6);
       aCanvas.LineTo(aBand.X + (j * 5) + 3+ 1, aBand.Y + 6);
    end;
  end;
  
begin
  if Bands.fMoving > 0 then
  begin
    Exit;
  end;

  fScreenBitmap.Width := Width;
  fScreenBitmap.Height := Height;
  fScreenBitmap.Dormant;


  if Align in [alLeft, alRight] then
    fStyler.DrawGradientv(fScreenBitmap.Canvas, Color, ColorTo, ClientRect)
  else
    fStyler.DrawGradientH(fScreenBitmap.Canvas, Color, ColorTo, ClientRect);

  for i := 0 to Bands.Count - 1 do
  begin

    for j := 0 to Bands[i].Count - 1 do
    begin
      fStyler.DrawEdge(fScreenBitmap.Canvas, Bands[i][j].BoundsRect, True);
      if not Locked then
      begin
        if Align in [alLeft, alRight] then
          DrawGrabberV(fScreenBitmap.Canvas, Bands[i][j])
        else
          DrawGrabberH(fScreenBitmap.Canvas, Bands[i][j]);
      end;
      if Assigned(fOnBandDraw) then
        fOnBandDraw(Bands[i][j]);
    end;

  end;

  Canvas.Draw(0, 0, fScreenBitmap);
end;//Paint
//------------------------------------------------------------------------------
procedure TDsDockSite.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := Ord(Bands.isMovingBand) or
                    Ord(Bands.Find(Message.XPos, Message.YPos) <> nil);
end;//CMDesignHitTest
//------------------------------------------------------------------------------
// Това се вика като се добавя кво ли не и тулбара и бутоните
procedure TDsDockSite.CMControlListChange(var Message: TCMControlListChange);
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if not Message.Inserting then
  begin
    if (Message.Control is TDSToolBar) then
    begin
      Bands.RemoveBand(Message.Control);
      TDSToolBar(Message.Control).Detach(fObserver);
      if AutoSize then
        AdjustSize;
      Invalidate;
    end;
  end;
end;//CMControlListChange
//------------------------------------------------------------------------------
procedure TDsDockSite.CMControlChange(var Message: TCMControlChange);
var
  vWidth: Integer;
begin
  inherited;
  if csLoading in ComponentState then
    Exit;
  if Message.Inserting then // If inserting contol
  begin
    if not (Message.Control is TDSToolBar) then
      Exit;
    Message.Control.Align := alNone;
    Bands.Vertical := Align in [alLeft, alRight];
    TDSToolBar(Message.Control).Vertical := Bands.Vertical;
    TDSToolBar(Message.Control).Color := ColorTo;
    TDSToolBar(Message.Control).ColorTo := Color;
    TDSToolBar(Message.Control).Vertical := Bands.Vertical;
    Bands.GrowUp := Align in [alRight, alBottom];

    if Align in [alLeft, alRight] then
    begin
      vWidth := TDSToolBar(Message.Control).MinWidth;
      if vWidth > 0 then
        Message.Control.Width := vWidth
      else
        Message.Control.SetBounds(Message.Control.Left, Message.Control.Top, 23, 129);
    end
    else
    begin
      vWidth := TDSToolBar(Message.Control).MaxWidth;
      if vWidth > 0 then
        Message.Control.Width := vWidth;
    end;

    if Message.Control.Visible then
      Bands.AddBand(Message.Control);

    TDSToolBar(Message.Control).Attach(fObserver);

    if AutoSize then
      AdjustSize;
    Invalidate;
  end
end;//CMControlChange
//------------------------------------------------------------------------------
procedure TDsDockSite.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  vBand: TDsBand;
begin
  if Locked then
    Accept := False;

  inherited DockOver(Source, X, Y, State, Accept);
  if Accept and (State = dsDragEnter) and AutoDock and not Locked then
  begin
    Source.Control.EndDrag(True);
    vBand := Bands.Find(Source.Control);
    Bands.StartBand(vBand);
    MouseCapture := True;
  end;
end;
//------------------------------------------------------------------------------
procedure TDsDockSite.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vBand: TDsBand;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not MouseCapture then
    Exit;
  if not Locked then begin
    vBand := Bands.Find(X, Y);
    if vBand <> nil then begin
      Bands.StartBand(vBand);
      fOffset.X := vBand.X - X;
      fOffset.Y := vBand.Y - Y;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDsDockSite.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vMoved: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if Bands.Vertical then
    vMoved := Bands.moveBand(X, Y + fOffset.Y)
  else
    vMoved := Bands.moveBand(X + fOffset.X, Y);
  if vMoved then begin
    AdjustSize;
    Refresh; // Когато имаме реално преместване
  end;
end;
//------------------------------------------------------------------------------
procedure TDsDockSite.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fBands.StopBand;
  inherited MouseUp(Button, Shift, X, Y);
end;//MouseUp
//------------------------------------------------------------------------------
procedure TDsDockSite.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin

  Message.Result := 1;
end;//WMEraseBkgnd
//------------------------------------------------------------------------------
procedure TDsDockSite.WMSize(var Message: TWMSize);
begin
  inherited;
  if Align in [alLeft, alRight] then
    Bands.Length := Height
  else
    Bands.Length := Width;
end;//WMSize
//------------------------------------------------------------------------------
procedure TDsDockSite.AlignControls(aControl: TControl; var Rect: TRect);
var
  vRow: TBandList;
  vBand: TDsBand;
begin
  if aControl <> nil then
  begin
    vBand := fBands.Find(aControl);
    if vBand <> nil then
    begin
      vRow := fBands.FindRowForBand(vBand);
      vRow.BandFacade.Band := vBand;
      if vRow.Top <> vRow.BandFacade.Top then
      begin
        vRow.BandFacade.setTop(vRow.Top);
      end;
      if not vBand.PosChanging then
      begin
        vRow.StartBand(vBand);
        vRow.BandFacade.Band := vBand;
        vRow.MoveBand(vRow.BandFacade.BandStart);
        vRow.StopBand;
        Bands.UpdateTop;
        Refresh;
      end;
    end;
  end;

  if Showing then
    AdjustSize
end;//AlignControls
//------------------------------------------------------------------------------
procedure TDsDockSite.BeginAutoDrag;
begin
  if Locked then
    Exit;
  inherited BeginAutoDrag;
end;//BeginAutoDrag
//------------------------------------------------------------------------------
procedure TDsDockSite.SetParent(AParent: TWinControl);

  function CheckForAligned(aAlign: TAlign): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] is TDsDockSite then
        if Parent.Controls[i] <> Self then
          if Parent.Controls[i].Align = aAlign then
          begin
            Result := False;
            Exit;
          end;
    Result := True;
  end;

begin
  if (csLoading in ComponentState) or not (csDesigning in ComponentState) then
  begin
    inherited SetParent(aParent);
    Exit;
  end;

  if aParent is TDsDockSite then
    inherited setParent(aParent.Parent)
  else
    inherited setParent(aParent);

  if not Assigned(Parent) then
    Exit;

  if CheckForAligned(alTop) then
    Align := alTop
  else
  if CheckForAligned(alLeft) then
    Align := alLeft
  else
  if CheckForAligned(alRight) then
    Align := alRight
  else
  if CheckForAligned(alBottom) then
    Align := alBottom;
end;//SetParent
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TBandRowList
//------------------------------------------------------------------------------
constructor TBandRowList.Create;
begin
  inherited Create;
  fRows := TObjectList.Create;
  Vertical := False;
  GrowUp := False;
  fMoving := 0;
end;//Create
//------------------------------------------------------------------------------
destructor TBandRowList.Destroy;
begin
  FreeAndNil(fRows);
  inherited Destroy;
end;//Destroy
//------------------------------------------------------------------------------
function TBandRowList.CreateBandFacade: TBandFacade;
begin
  if Vertical then
    Result := TBandFacadeV.Create
  else
    Result := TBandFacadeH.Create;
end;
//------------------------------------------------------------------------------
function TBandRowList.getRow(Idx: Integer): TBandList;
begin
  Result := TBandList(fRows[Idx]);
end;//getRow
//------------------------------------------------------------------------------
function TBandRowList.getCount: Integer;
begin
  Result := fRows.Count;
end;//getCount
//------------------------------------------------------------------------------
function TBandRowList.getWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Rows[i].Width;
end;//getHeight
//------------------------------------------------------------------------------
procedure TBandRowList.addBand(aControl: TControl);

  procedure AppendRowAtEnd;
  begin
    fRows.Add(TBandList.Create);
    Rows[Count-1].BandFacade := CreateBandFacade;
    Rows[Count-1].Length := Length;
    if Count > 1 then
      Rows[Count-1].Top := Rows[Count-2].Top + Rows[Count-2].Width
    else
      Rows[Count-1].Top := 0;
  end;

  procedure AppendRowAtTop;
  begin
    fRows.Insert(0, TBandList.Create);
    Rows[0].BandFacade := CreateBandFacade;
    Rows[0].Length := Length;
    Rows[0].Top := 0;
  end;

var
  i: Integer;
  vFound: Boolean;
  vTop: Integer;
begin
  Inc(fMoving);
  try
    if Vertical then
      vTop := aControl.Left
    else
      vTop := aControl.Top;

    if (vTop > Width) then begin
      AppendRowAtEnd;
      Rows[Count-1].addBandToRowAndStop(aControl);
    end
    else
    if (vTop < 0) then begin
      AppendRowAtTop;
      Rows[0].addBandToRowAndStop(aControl);
    end
    else begin
      if (Count = 0) then
        AppendRowAtEnd;
      vFound := False;
      for i := 0 to Count - 1 do
        if Rows[i].IsInside(vTop) then begin
          Rows[i].addBandToRowAndStop(aControl);
          vFound := True;
          Break;
        end;
      if not vFound then
        Rows[0].addBandToRowAndStop(aControl);
    end;
    UpdateTop;
  finally
    Dec(fMoving);
  end;
end;//AddBand
//------------------------------------------------------------------------------
procedure TBandRowList.removeBand(aControl: TControl);
var
  i: Integer;
  vRow: TBandList;
begin
  vRow := nil;
  for i := 0 to Count - 1 do
    if Rows[i].Find(aControl) <> nil then begin
      vRow := Rows[i];
      Break;
    end;

  if vRow = nil then
    Exit;

  vRow.RemoveBand(aControl);
  if vRow.Count = 0 then
  begin
    fRows.Remove(vRow);
  end;
  UpdateTop;
end;//RemoveBand
//------------------------------------------------------------------------------
function TBandRowList.InsertRow(aIndex: Integer): TBandList;
begin
  fRows.Insert(aIndex, TBandList.Create);
  Result := Rows[aIndex];
  Result.BandFacade := CreateBandFacade;
  Result.Length := Length;
  Result.Top := 0;
end;//InsertRow
//------------------------------------------------------------------------------
function TBandRowList.moveBand(aLenPos, aY: Integer): Boolean;
var
  vRow, vNewRow: TBandList;
  vBand: TDsBand;

  i, vIdx: Integer;
  vTemp: Integer;
  vControl: TControl;
  vBandWidth: Integer;

  function isFirstRow: Boolean;
  begin
    if GrowUp then
      Result := vIdx = fRows.Count - 1
    else
      Result := vIdx = 0;
  end;

  function isLastRow: Boolean;
  begin
    if GrowUp then
      Result := vIdx = 0
    else
      Result := vIdx = fRows.Count - 1;
  end;

  // The band moves up when the mouse is above the band.
  // Except for the first row where the mouse can be on the upper border of the row
  function isMoveUp(aPos: Integer): Boolean;
  begin
    if GrowUp then begin
      if isFirstRow then
        Result := aPos >= vRow.Top + vRow.Width-1 // Sometimes there is no place to move the mouse above the first row
      else
        Result := aPos >= vRow.Top + vRow.Width
    end
    else begin
      if isFirstRow then
        Result := aPos <= vRow.Top // Sometimes there is no place to move the mouse above the first row
      else
        Result := aPos < vRow.Top ;
    end;
  end;

  function isMoveDn(aPos: Integer): Boolean;
  begin
    if GrowUp then
      Result := aPos < vRow.Top
    else
      Result := aPos >= vRow.Top + vRow.Width;
  end;

  function UpperRow: TBandList;
  begin
    if GrowUp then
      Result := Rows[vIdx + 1]
    else
      Result := Rows[vIdx - 1];
  end;

begin
  Result := False;
  vIdx := -1;

  vRow := nil;
  for i := 0 to Count - 1 do
    if (Rows[i].Band <> nil) then begin
      vRow := Rows[i];
      vIdx := i;
      Break;
    end;
  if (vRow = nil) then
    Exit;

  Inc(fMoving);
  try
    
  if Vertical then begin
    vTemp := aLenPos;
    aLenPos := aY;
    aY := vTemp;
  end;

  vBand := vRow.Band;

  // Starts dragging the Band if it is outside
  if (aLenPos < -20) or (aY < -40) or (aLenPos > Length + 20) or (aY > Width + 20) then
  begin
    if csDesigning in vBand.Control.ComponentState then
      Exit;
    SetCapture(0);
    vRow.StopBand;
    vBand.Control.BeginDrag(True);
    Result := True;
    Exit;
  end;

  if isMoveUp(aY) then begin
    // There are other bands on the row and it is the first row
    if (vRow.Count > 1) and isFirstRow then begin
      // Appends new row
      if GrowUp then
        vNewRow := InsertRow(Count)
      else
        vNewRow := InsertRow(0);
      // Remove the band from its current row
      vBand := vRow.Band;
      vRow.Extract(vBand);
      vRow.RedoBands;
      vRow.StopBand;
      vNewRow.AddBandToRow(vBand.Control); // Adds the band in the new row
      vBand.Free;
      UpdateTop;
      StartBand(vNewRow.Items[vNewRow.Count - 1]);
      Result := True; // Returns that there is a change
    end
    else
    if not isFirstRow then begin // We have upper row
      vBand := vRow.Band;
      vRow.Extract(vBand);
      vRow.RedoBands;
      vRow.StopBand;
      vNewRow := UpperRow;
      vNewRow.AddBandToRow(vBand.Control);
      vBand.Free;
      if vRow.Count = 0 then begin
        fRows.Remove(vRow);
      end;
      UpdateTop;
      StartBand(vNewRow.Items[vNewRow.Count - 1]);
      vNewRow.BandFacade.Band := vNewRow.Items[vNewRow.Count - 1];
      vNewRow.MoveBand(vNewRow.BandFacade.BandStart);
      Result := True;
    end
    else
      Result := vRow.MoveBand(aLenPos);
  end
  else
  if isMoveDn(aY) then begin
    if (vRow.Count = 1) and isLastRow then
      Result := vRow.MoveBand(aLenPos)
    else begin
      vBandWidth := vRow.Width;
      // Изваждаме бенда
      vBand := vRow.Band;
      vRow.Extract(vBand);
      vRow.RedoBands;
      vRow.StopBand;
      vControl := vBand.Control;
      vBand.Free;
      if vRow.Count = 0 then begin
        // The height of the control may be changed
        if GrowUp then
          aY := aY - vBandWidth;
        fRows.Remove(vRow);
      end;
      // After removing the band the height of the row may change
      // also if the whole row is removed the height may change, so do update
      UpdateTop;

      // Put the band to its right place

      for i := 0 to Count - 1 do
        if (aY >= Rows[i].Top) and (aY < Rows[i].Top + Rows[i].Width) then begin
          if GrowUp then begin
            if ((aY + vBandWidth) >= Rows[i].Top) and
               ((aY + vBandWidth) < Rows[i].Top + Rows[i].Width) then
              vNewRow := Rows[i]
            else
              vNewRow := InsertRow(i+1);
          end
          else begin
            // After inserting in the new row, the row can be thinner and the
            // band has to go in its old one
            if ((aY - vBandWidth) >= Rows[i].Top) and
               ((aY - vBandWidth) < Rows[i].Top + Rows[i].Width) then
              vNewRow := Rows[i]
            else
              vNewRow := InsertRow(i);
          end;
          vNewRow.AddBandToRow(vControl);
          UpdateTop;
          StartBand(vNewRow.Items[vNewRow.Count-1]);
          Result := True;
          Break;
        end;

      // Удължаваме в края  
      if not Result then begin
        if GrowUp then
          vNewRow := InsertRow(0)
        else
          vNewRow := InsertRow(Count);
        vNewRow.addBandToRow(vControl);
        UpdateTop;
        vNewRow.StartBand(vNewRow.Items[vNewRow.Count-1]);
        Result := True;
      end;
    end;
  end
  else
    Result := vRow.MoveBand(aLenPos);
  finally
    Dec(fMoving);
  end;
end;//MoveBand;
//------------------------------------------------------------------------------
procedure TBandRowList.startBand(aBand: TDsBand);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Rows[i].Find(aBand.Control) <> nil then
      Rows[i].StartBand(aBand);
end;//StartBand
//------------------------------------------------------------------------------
procedure TBandRowList.stopBand;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rows[i].StopBand;
end;
//------------------------------------------------------------------------------
function TBandRowList.Find(aControl: TControl): TDsBand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Rows[i].Find(aControl) <> nil then begin
      Result := Rows[i].Find(aControl);
      Break;
    end;
end;//Find
//------------------------------------------------------------------------------
function TBandRowList.FindRowForBand(aBand: TDsBand): TBandList;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Rows[i].Find(aBand.Control) <> nil then begin
      Result := Rows[i];
      Break;
    end;
end;//FindRowForBand
//------------------------------------------------------------------------------
function TBandRowList.Find(aX, aY: Integer): TDsBand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Rows[i].Find(aX, aY) <> nil then begin
      Result := Rows[i].Find(aX, aY);
      Break;
    end;
end;
//------------------------------------------------------------------------------
function TBandRowList.isMovingBand: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
    if Rows[i].Band <> nil then begin
      Result := True;
      Break;
    end;
end;//isMovingBand
//------------------------------------------------------------------------------
procedure TBandRowList.setLength(const Value: Integer);
var
  i: integer;
begin
  fLength := Value;
  for i := 0 to Count - 1 do
    Rows[i].Length := Value;
end;//setWidth;
//------------------------------------------------------------------------------
procedure TBandRowList.UpdateTop;
var
  i, CurrRow: Integer;
begin
  CurrRow := 0;
  for i := 0 to Count - 1 do begin
    Rows[i].Top := CurrRow;
    CurrRow := CurrRow + Rows[i].Width;
  end;
end;//UpdateTop
//------------------------------------------------------------------------------
procedure TBandRowList.setVertical(const Value: boolean);
var
  i: Integer;
begin
  fVertical := Value;
  for i := 0 to Count - 1 do begin
    Rows[i].BandFacade.Free;
    Rows[i].BandFacade := CreateBandFacade;
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TBandFacadeH
//------------------------------------------------------------------------------
function TBandFacadeH.BandLength: Integer;
begin
  Result := Band.Width;
end;//BandLength
//------------------------------------------------------------------------------
function TBandFacadeH.BandStart: Integer;
begin
  Result := Band.X;
end;//BandStart
//------------------------------------------------------------------------------
procedure TBandFacadeH.MoveTo(aPos: Integer);
begin
  Band.setPos(aPos, Band.Y);
end;//MoveTo
//------------------------------------------------------------------------------
procedure TBandFacadeH.setPos(aPos, aTop: Integer);
begin
  Band.setPos(aPos, aTop);
end;//setPos
//------------------------------------------------------------------------------
procedure TBandFacadeH.setInsets(aInsets: TRect);
begin
  Band.InsetLeft := aInsets.Left;
  Band.InsetRight := aInsets.Right;
  Band.InsetTop := aInsets.Top;
  Band.InsetBottom := aInsets.Bottom;
end;//setInsets
//------------------------------------------------------------------------------
function TBandFacadeH.Top: Integer;
begin
  Result := Band.Y;
end;
//------------------------------------------------------------------------------
procedure TBandFacadeH.setTop(aTop: Integer);
begin
  Band.setPos(Band.X, aTop);
end;//setTop
//------------------------------------------------------------------------------
function TBandFacadeH.Width: Integer;
begin
  Result := Band.Height;
end;//Width
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TBandFacadeV
//------------------------------------------------------------------------------
function TBandFacadeV.BandLength: Integer;
begin
  Result := Band.Height;
end;//BandLength
//------------------------------------------------------------------------------
function TBandFacadeV.BandStart: Integer;
begin
  Result := Band.Y;
end;//BandStart
//------------------------------------------------------------------------------
procedure TBandFacadeV.MoveTo(aPos: Integer);
begin
  Band.setPos(Band.X, aPos);
end;//MoveTo
//------------------------------------------------------------------------------
procedure TBandFacadeV.setPos(aPos, aTop: Integer);
begin
  Band.setPos(aTop, aPos);
end;//setPos
//------------------------------------------------------------------------------
procedure TBandFacadeV.setInsets(aInsets: TRect);
begin
  Band.InsetLeft := aInsets.Top;
  Band.InsetRight := aInsets.Bottom;
  Band.InsetTop := aInsets.Left;
  Band.InsetBottom := aInsets.Right;
end;//setInsets
//------------------------------------------------------------------------------
function TBandFacadeV.Top: Integer;
begin
  Result := Band.X;
end;
//------------------------------------------------------------------------------
procedure TBandFacadeV.setTop(aTop: Integer);
begin
  Band.setPos(aTop, Band.Y);
end;//setTop
//------------------------------------------------------------------------------
function TBandFacadeV.Width: Integer;
begin
  Result := Band.Width;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TDSToolBarObserver
//------------------------------------------------------------------------------
constructor TDSToolBarObserver.Create(aOwner: TDsDockSite);
begin
  inherited Create;
  fDockSite := aOwner;
end;
//------------------------------------------------------------------------------
destructor TDSToolBarObserver.Destroy;
begin
  inherited Destroy;
end;//Destroy
//------------------------------------------------------------------------------
procedure TDSToolBarObserver.Update(aSubject: TObject; aChange: TSubjectChange);
begin
  case aChange of
  cngVisible:
    if aSubject is TDSToolBar then
    begin
      if TDSToolBar(aSubject).Visible then
      begin
        if fDockSite.Bands.Find(TDSToolBar(aSubject)) = nil then
          fDockSite.Bands.AddBand(TDSToolBar(aSubject));
      end
      else
      begin
        fDockSite.Bands.RemoveBand(TDSToolBar(aSubject));
        fDockSite.Refresh;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDsDockSite.setLocked(const Value: Boolean);
begin
  fLocked := Value;
  Refresh;
end;//setLocked
//------------------------------------------------------------------------------
procedure TDsDockSite.setColorTo(const Value: TColor);
var
  i: Integer;
begin
  fColorTo := Value;
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TDSToolBar then
    begin
      TDSToolBar(Controls[i]).Color := ColorTo;
      TDSToolBar(Controls[i]).ColorTo := Color;
    end;
  Refresh;
end;
//------------------------------------------------------------------------------
end.
