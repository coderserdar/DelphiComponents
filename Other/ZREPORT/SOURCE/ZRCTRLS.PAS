unit ZRCtrls;

interface

{$I ZRDefine.inc}

uses
  Windows, Messages,                               // WinAPI
  SysUtils,                                        // Delphi RTL
  Classes, Graphics, Controls, Forms, Dialogs,     // Delphi VCL
  ZREscape, ZReport;                               // ZReport

type
  { Forward declarations }
  TZRCustomLabel = class;

  { TZRAlignment }
  TZRAlignmentWidth  = (zawLeft, zawRight, zawCenter, zawBandLeft, zawBandRight, zawBandCenter);
  TZRAlignmentHeight = (zahTop, zahBottom, zahCenter, zahBandTop, zahBandBottom, zahBandCenter);
  TZRAlignment = class(TPersistent)
  private
    fOwner: TZRCustomLabel;
    fX    : TZRAlignmentWidth;
    fY    : TZRAlignmentHeight;
    procedure SetX(Value: TZRAlignmentWidth);
    procedure SetY(Value: TZRAlignmentHeight);
    procedure UpdateOwner;
  public
    constructor Create(Owner: TZRCustomLabel);
    procedure Assign(Source: TPersistent); override;
  published
    property X: TZRAlignmentWidth  read fX write SetX default zawLeft;
    property Y: TZRAlignmentHeight read fY write SetY default zahTop;
  end;

  { TZRCustomLabel }
  TZAutoSize = (zasNone, zasHeight, zasWidth);
  TZRCustomLabel = class(TZReportControl)
  private
    fLines     : TStrings;
    fAlignment : TZRAlignment;
    fAutoSize  : TZAutoSize;
    fWordWrap  : Boolean;
    fVariable  : TZRVariable;
    procedure SetAutoSize(Value:  TZAutoSize);
    procedure SetWordWrap(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TextChanged;
    procedure TextToLines(Width: Integer);
    procedure SetAlignment(Value: TZRAlignment);
    procedure SetVariable(Value: TZRVariable);
  protected
    procedure Draw; override;
    procedure SetParent(aParent: TWinControl); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    procedure AlignBounds; override;
    procedure RequestBounds; override;

    function  GetText: String; virtual;
    procedure DoPrint(OfsX, OfsY: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Alignment: TZRAlignment read fAlignment write SetAlignment;
    property AutoSize : TZAutoSize read fAutoSize write SetAutoSize default zasNone;
    property Variable : TZRVariable read fVariable write SetVariable;
    property WordWrap : Boolean read fWordWrap write SetWordWrap default False;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Frame;
    property FontStyles;
    property Visible;
  end;

  { TZRLabel }
  TZRLabel = class(TZRCustomLabel)
  published
    property Alignment;
    property AutoSize;
    property Caption;
    property Variable;
    property WordWrap;
    property BeforePrint;
    property AfterPrint;
  end;

  { TZRSystemLabel }
  TZRSystemData = (zsdTime, zsdDate, zsdDateTime,
                   zsdPageNumber, zsdReportTitle,
                   zsdRecordNumber, zsdRecordCount);

  TZRSystemLabel = class(TZRCustomLabel)
  private
    fDataKind : TZRSystemData;
    fText     : String;
    procedure SetDataKind(const Value: TZRSystemData);
    procedure SetText(const Value: String);
  protected
    function GetText: String; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Alignment;
    property AutoSize;
    property DataKind: TZRSystemData read fDataKind write SetDataKind default zsdDateTime;
    property Text: String read fText write SetText;
    property WordWrap;
    property BeforePrint;
    property AfterPrint;
  end;

  { TZRTotalLabel }
  TZRTotalKind = (ztkValue, ztkCount, ztkSum, ztkMin, ztkMax, ztkAvg);

  TZRTotalLabel = class(TZRCustomLabel)
  private
    fKind  : TZRTotalKind;
    fLevel : TComponent;
    procedure SetKind(Value: TZRTotalKind);
    procedure SetLevel(Value: TComponent);
  protected
    function  GetText: String; override;
    procedure SetParent(aParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment;
    property AutoSize;
    property Kind  : TZRTotalKind read fKind  write SetKind;
    property Level : TComponent   read fLevel write SetLevel;
    property Variable;
    property WordWrap;
    property BeforePrint;
    property AfterPrint;
  end;

  { TZRFrameLine }
  TZRFrameLine = class(TZRCustomLabel)
  protected
    function GetText: String; override;
  end;

var
  ZRFontStyleToEscapeMap : array[TZRFontStyle] of TZREscapeStyle = (
    esBold, esItalic, esUnderline, esSuperScript, esSubScript );

implementation

uses
  Math,
  ZRConst, ZRUtils, ZRStrUtl;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                TZRAlignment                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRAlignment.Create(Owner: TZRCustomLabel);
begin
  inherited Create;
  fOwner:= Owner;
  fX    := zawLeft;
  fY    := zahTop;
  //fAlignToBand := False;
end;

procedure TZRAlignment.UpdateOwner;
begin
  fOwner.UpdateBounds;
  fOwner.Invalidate;
end;

procedure TZRAlignment.SetX(Value: TZRAlignmentWidth);
begin
  if Value <> X then begin
    fX:= Value;
    UpdateOwner;
  end;
end;

procedure TZRAlignment.SetY(Value: TZRAlignmentHeight);
begin
  if Value <> Y then begin
    fY:= Value;
    UpdateOwner;
  end;
end;

procedure TZRAlignment.Assign(Source: TPersistent);
begin
  if Source is TZRAlignment then begin
    fX := TZRAlignment(Source).X;
    fY := TZRAlignment(Source).Y;
    UpdateOwner;
  end else
    inherited;
end;

{procedure TZRAlignment.SetAlignToBand(const Value: Boolean);
begin
  if Value <> AlignToBand then begin
    fAlignToBand := Value;
    UpdateOwner;
  end;
end;}

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRCustomLabel                           !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRCustomLabel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:= ControlStyle + [csSetCaption];
  fAlignment  := TZRAlignment.Create(Self);
  fLines      := TStringList.Create;
  fAutoSize   := zasNone;
  fWordWrap   := False;
end;

destructor TZRCustomLabel.Destroy;
begin
  fLines.Free;
  fAlignment.Free;
  inherited;
end;

procedure TZRCustomLabel.SetAlignment(Value: TZRAlignment);
begin
  Alignment.Assign(Value);
end;

procedure TZRCustomLabel.Draw;
var
  R: TRect;
  C: TPoint;
  i: Integer;
  S: String;
begin
  inherited;
   i:= 0;
  C:= CharSize;
  R:= FramedClientRect;
  while i < fLines.Count do begin
    S:= fLines[i];
    DrawText(Canvas.Handle, PChar(S), length(S), R, 0);
    Inc(i);
    Inc(R.Top, C.Y);
  end;
end;

procedure TZRCustomLabel.DoPrint(OfsX, OfsY: Integer);
var
  F   : TZRFontStyle;
  E   : TZREscapeStyles;
  MaxWidth,
  MaxHeight: Integer;
  i   : Integer;
  S   : String;
begin
  E := [];
  if not Report.Options.IgnoreStyles then
    for F := Low(TZRFontStyle) to High(TZRFontStyle) do
      if F in FontStyles then Include(E, ZRFontStyleToEscapeMap[F]);
  MaxWidth  := Min(Band.Right-Band.Frame.Right-Left-Frame.Left+1, FramedWidth );
  MaxHeight := Min(Band.Bottom-Band.Frame.Bottom-Top-Frame.Top+1, FramedHeight);
  i := 0;
  while (i < fLines.Count) and (i < MaxHeight) do begin
    S:= copy(fLines[i], 1, MaxWidth);
    S:= EscapeFormat(S, [], E);
    PrintString(OfsX+(Left+Frame.Left), OfsY+(Top+Frame.Top)+i, S);
    Inc(i);
  end;
  inherited;
end;

procedure TZRCustomLabel.TextToLines(Width: Integer);
begin
  if WordWrap then
    fLines.Text:= WrapText(GetText, Width)
  else
    fLines.Text:= GetText;
end;

procedure TZRCustomLabel.RequestBounds;
var
  MaxCol: Integer;
  i, dx : Integer;
begin
  inherited;
  if (csDestroying in ComponentState) or not Assigned(fLines) then Exit;

  if (AutoSize = zasWidth) and (Band <> nil) then
    if (Align = zalNone) and (Alignment.X in [zawLeft,zawBandLeft]) then
      MaxCol:= Band.FramedWidth - Left
    else
      MaxCol:= Band.FramedWidth
  else
    MaxCol:= Width;
  Dec(MaxCol, Frame.Width);
  TextToLines(MaxCol);

  case AutoSize of
    zasWidth:
      begin
        {if not (Align in [zalTop, zalBottom, zalWidth]) then} begin
          MaxCol:= 0;
          for i:= 0 to fLines.Count-1 do
            if length(fLines[i]) > MaxCol then MaxCol:= length(fLines[i]);
          Inc(MaxCol, Frame.Width);
          case Alignment.X  of
            zawRight,
            zawBandRight : dx:= (MaxCol - Width);
            zawCenter,
            zawBandCenter: dx:= (MaxCol - Width) div 2;
            else           dx:= 0;
          end;
          Width := MaxCol;
          Left  := {Max(}Left - dx{, Band.Frame.Left)};
        end;
        {if not (Align in [zalLeft, zalRight, zalHeight]) then}
          Height:= Frame.Height + fLines.Count;
      end;
    zasHeight:
      begin
        {if not (Align in [zalLeft, zalRight, zalHeight]) then}
          Height:= Frame.Height + fLines.Count;
      end;
  end;
  if Assigned(Band) then begin
    case Alignment.X of
      zawBandRight  : Left := Band.FramedWidth - Width;
      zawBandCenter : Left := (Band.FramedWidth - Width) div 2;
      zawBandLeft   : Left := Band.Frame.Left;
    end;
    case Alignment.Y of
      zahBandBottom : Top := Band.FramedHeight - Height;
      zahBandCenter : Top := (Band.FramedHeight - Height) div 2;
      zahBandTop    : Top := Band.Frame.Top;
    end;
  end;
end;

procedure TZRCustomLabel.AlignBounds;

  procedure ResizeBand;
  begin
    if IsPrinting and (AutoSize <> zasNone) and Band.Stretch and
       (Bottom >= Band.Height - Band.Frame.Bottom) then
      Band.Height := Bottom + Band.Frame.Bottom + 1;
  end;

var
  i : Integer;
begin
  ResizeBand;
  inherited;
  if not (csDestroying in ComponentState) and Assigned(fLines) then begin
    TextToLines(FramedWidth);
    for i:= 0 to fLines.Count-1 do
      case Alignment.X of
        zawRight,
        zawBandRight : fLines[i]:= PadLeft  (fLines[i], FramedWidth);
        zawCenter,
        zawBandCenter: fLines[i]:= PadCenter(fLines[i], FramedWidth);
      end;
    case Alignment.Y of
      zahBottom,
      zahBandBottom: i:= (FramedHeight-fLines.Count);
      zahCenter,
      zahBandCenter: i:= (FramedHeight-fLines.Count) div 2;
      else           i:= 0;
    end;
    while i > 0 do begin fLines.Insert(0, ''); Dec(i); end;
  end;
end;

function TZRCustomLabel.GetText: String;
begin
  if Assigned(Variable) then
    Result := Variable.Text
  else
    Result:= inherited Text;
end;

procedure TZRCustomLabel.SetAutoSize(Value: TZAutoSize);
begin
  if Value <> fAutoSize then begin
    fAutoSize:= Value;
    UpdateBounds;
    Invalidate;
  end;
end;

procedure TZRCustomLabel.SetWordWrap(Value: Boolean);
begin
  if Value <> fWordWrap then begin
    fWordWrap:= Value;
    UpdateBounds;
    Invalidate;
  end;
end;

procedure TZRCustomLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  UpdateBounds;
  Invalidate;
end;

procedure TZRCustomLabel.SetVariable(Value: TZRVariable);
{
var
  Controller: TZRCustomController;
}
begin
  if Variable <> Value then begin
    fVariable := Value;
    if Assigned(Variable) then Variable.FreeNotification(Self);
    UpdateBounds;
    Invalidate;
  end;
{
  if Variable <> Value then begin
    if Value <> nil then begin
      if Band is TZRCustomController then
        Controller := TZRCustomController(Band)
      else
        Controller := Band.Master;
      if (Controller.VariableList.IndexOf(Value) >= 0) then
        fVariable := Value
      else
        fVariable := nil;
    end else
      fVariable := nil;
    if Assigned(Variable) then Variable.FreeNotification(Self);
    UpdateBounds;
    Invalidate;
  end;
}
end;

procedure TZRCustomLabel.SetParent(AParent: TWinControl);
begin
  if Parent <> AParent then begin
    if Assigned(Band) then Band.LabelList.Remove(Self);
    inherited;
    if Assigned(Band) then Band.LabelList.Add(Self);
  end;
end;

procedure TZRCustomLabel.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (Component = Variable) then Variable := nil;
end;

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                               TZRTotalLabel                            !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRTotalLabel.Create(AOwner: TComponent);
begin
  inherited;
  fKind := ztkSum;
end;

procedure TZRTotalLabel.SetKind(Value: TZRTotalKind);
begin
  if Kind <> Value then begin
    fKind := Value;
    UpdateBounds;
    Invalidate;
  end;
end;

function TZRTotalLabel.GetText: String;
const
  KindName: array[TZRTotalKind] of String[16] = (
    'Value',
    'Count',
    'Sum',
    'Min',
    'Max',
    'Avg'
  );
  function LevelIndex: Integer;
  var
    M: TZRCustomController;
  begin
    if Band is TZRCustomController then
      M := (Band as TZRCustomController)
    else
      M := Band.Master;
    Result := M.GroupList.IndexOf(Level) + 1;
  end;
var
  Total: TZRAggregator;
begin
  if not Assigned(Variable) then
    Result:= inherited Text
  else begin
    Total := TZRAggregator(Variable);
    if not IsPrinting then
      Result := Format('%s(%s)', [KindName[Kind], Total.Name])
    else
      case Kind of
        ztkValue: Result := Variable.Text;
        ztkCount: Result := IntToStr(Total.Count[LevelIndex]);
        ztkSum  : Result := Total.Format.Format(Total.Sum[LevelIndex]);
        ztkMin  : Result := Total.Format.Format(Total.Min[LevelIndex]);
        ztkMax  : Result := Total.Format.Format(Total.Max[LevelIndex]);
        ztkAvg  : Result := Total.Format.Format(Total.Avg[LevelIndex]);
      end;
  end;
end;

procedure TZRTotalLabel.SetLevel(Value: TComponent);
var
  aMaster: TZRCustomController;
begin
  if Level <> Value then begin
    if not (csLoading in ComponentState) and Assigned(Band) then begin
      if Band is TZRCustomController then
        aMaster := TZRCustomController(Band)
      else
        aMaster := Band.Master;
      if (Value = aMaster) or (aMaster.GroupList.IndexOf(Value) >= 0) then begin
        fLevel := Value;
        if aMaster.VariableList.IndexOf(Variable) = -1 then Variable := nil;
      end else
        Abort;
    end else
      fLevel := Value;
  end;
end;

procedure TZRTotalLabel.SetParent(aParent: TWinControl);
var
  aMaster: TZRCustomController;
begin
  inherited;
  if Assigned(Parent) and
     (csDesigning in ComponentState) {and
     (csLoading   in ComponentState)} then begin
    if (Band is TZRCustomController) then
      aMaster := TZRCustomController(Band)
    else
      aMaster := Band.Master;
    if (Band is TZRCustomController) then Level := Band else
    if (Band.Group <> nil)           then Level := Band.Group else
                                          Level := Band.Master;
  end;
end;

{ TZRSystemLabel }

constructor TZRSystemLabel.Create(aOwner: TComponent);
begin
  inherited;
  fDataKind := zsdDateTime;
  //AutoSize := zasWidth;
  Text      := '';
end;

function TZRSystemLabel.GetText: String;
begin
  if IsPrinting or Assigned(Report.Printer) and Report.Printer.Report.IsPrinting then
    case DataKind of
      zsdTime        : Result := FormatDateTime('t', Time);
      zsdDate        : Result := FormatDateTime('c', Date);
      zsdDateTime    : Result := FormatDateTime('c', Now);
      zsdPageNumber  : Result := IntToStr(Report.Printer.PageCount);
      zsdReportTitle : Result := Report.Title;
      zsdRecordNumber: if Band is TZRCustomController then
                         Result := IntToStr((Band as TZRCustomController).RecordNumber)
                       else
                         Result := IntToStr(Band.Master.RecordNumber);
      zsdRecordCount : if Band is TZRCustomController then
                         Result := IntToStr((Band as TZRCustomController).RecordCount)
                       else
                         Result := IntToStr(Band.Master.RecordCount);
    end
  else
    Result := '('+LoadStr(szrTime + ord(DataKind))+')';
  Result := Text + Result;
end;

procedure TZRSystemLabel.SetDataKind(const Value: TZRSystemData);
begin
  if DataKind <> Value then begin
    fDataKind := Value;
    UpdateBounds;
    Invalidate;
  end;
end;

procedure TZRSystemLabel.SetText(const Value: String);
begin
  if Text <> Value then begin
    fText := Value;
    UpdateBounds;
    Invalidate;
  end;
end;

{ TZRFrameLine }

function TZRFrameLine.GetText: String;
begin
  Result := '';
end;

end.

