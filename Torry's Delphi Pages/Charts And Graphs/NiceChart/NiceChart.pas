{-------------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

     The Original Code is NiceChart.pas released at May 26th, 2007.
     The Initial Developer of the Original Code is Priyatna.
     (Website: http://www.priyatna.org Email: me@priyatna.org)
     All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit NiceChart;

interface

uses
  Windows, Messages, Classes, Graphics, Forms, Controls, ExtCtrls, SysUtils,
  BSplines, Math;

const
  UndefinedValue: Double = MaxDouble;

type
  TNiceChart = class;

  TSeriesKind = (skLine, skSmooth, skBar);

  TNiceSeries = class(TObject)
  private
    Top: Integer;
    Values: TList;
    Chart: TNiceChart;
    FCaption: string;
    Spline: TBSpline;
    FKind: TSeriesKind;
    procedure SetCaption(const Value: string);
    function GetMaxXValue: Double;
    function GetMinXValue: Double;
    function GetMinYValue: Double;
    function GetMaxYValue: Double;
    procedure SetKind(const Value: TSeriesKind);
  protected
    procedure InternalClear;
  public
    constructor Create(AChart: TNiceChart; AKind: TSeriesKind);
    destructor Destroy; override;
    function AddXY(AX, AY: Double; AHint: string = ''): Integer;
    procedure Remove(Index: Integer);
    procedure Clear;
    property Caption: string read FCaption write SetCaption;
    property Kind: TSeriesKind read FKind write SetKind;
  end;

  TValueTranslator = record
    MinValue: Double;
    Scale: Double;
    Base: Integer;
  end;

  TMarkerProc = procedure (ACanvas: TCanvas; X, Y, Size: Integer);

  TNiceChart = class(TCustomPanel)
  private
    Brushes: array [0..15] of TBitmap;
    Temp: TStringList;
    MarkSize: Integer;
    Marker: TMarkerProc;
    BarCount: Integer;
    BarWidth: Integer;
    DestWidth, DestHeight: Integer;
    YZero: Integer;
    List: TList;
    XAxis: TList;
    YAxis: TList;
    FShowLegend: Boolean;
    FShowTitle: Boolean;
    FTitle: string;
    FTitleFont: TFont;
    FNormalFont: TFont;
    FUpdating: Boolean;
    RcChart, RcLegend, RcTitle: TRect;
    FXTranslate: TValueTranslator;
    FYTranslate: TValueTranslator;
    FAxisXOnePerValue: Boolean;
    FAxisYTitle: string;
    FAxisXTitle: string;
    FShowYGrid: Boolean;
    FShowXGrid: Boolean;
    FAxisYScale: Single;
    FAxisXScale: Single;
    FMonochrome: Boolean;
    procedure InternalClear;
    procedure InternalPaint(ACanvas: TCanvas);
    procedure Calculate(AWidth, AHeight: Integer);
    procedure DoPaint;
    procedure SetShowLegend(const Value: Boolean);
    procedure SetShowTitle(const Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure SetTitleFont(const Value: TFont);
    procedure TitleFontChanged(Sender: TObject);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    function GetSeries(Index: Integer): TNiceSeries;
    function GetSeriesCount: Integer;
    procedure DrawLegend(ACanvas: TCanvas);
    procedure DrawTitle(ACanvas: TCanvas);
    procedure SetAxisXTitle(const Value: string);
    procedure SetAxisYTitle(const Value: string);
    procedure BuildYAxis;
    procedure DrawYAxis(ACanvas: TCanvas);
    procedure DrawXAxis(ACanvas: TCanvas);
    procedure DrawChart(ACanvas: TCanvas);
    procedure BuildXAxis;
    procedure ClearAxis;
    procedure AdjustYAxis;
    procedure SetAxisXOnePerValue(const Value: Boolean);
    procedure SetShowXGrid(const Value: Boolean);
    procedure SetShowYGrid(const Value: Boolean);
    procedure CalculateSeries;
    procedure DrawSeries(ACanvas: TCanvas; Index: Integer);
    procedure AutoColors(ACanvas: TCanvas; Index: Integer; IsBar: Boolean);
    procedure SetAxisXScale(const Value: Single);
    procedure SetAxisYScale(const Value: Single);
    procedure SetMonochrome(const Value: Boolean);
    function GetLabel(Value: Double): string;
  protected
    procedure Paint; override;
    procedure Changed;
    procedure ChartToClient(const AX, AY: Double; var X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function AddSeries(AKind: TSeriesKind): TNiceSeries;
    function ClientToChart(const X, Y: Integer; var AX, AY: Double): Boolean;
    procedure RemoveSeries(ASeries: TNiceSeries);
    procedure Clear;
    property Series[Index: Integer]: TNiceSeries read GetSeries;
    property SeriesCount: Integer read GetSeriesCount;
    function CreateMetafile: TMetafile;
    procedure CopyToClipboard;
  published
    property ShowLegend: Boolean read FShowLegend write SetShowLegend;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle;
    property ShowXGrid: Boolean read FShowXGrid write SetShowXGrid;
    property ShowYGrid: Boolean read FShowYGrid write SetShowYGrid;
    property Title: string read FTitle write SetTitle;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property AxisXTitle: string read FAxisXTitle write SetAxisXTitle;
    property AxisYTitle: string read FAxisYTitle write SetAxisYTitle;
    property AxisXOnePerValue: Boolean read FAxisXOnePerValue write SetAxisXOnePerValue;
    property AxisXScale: Single read FAxisXScale write SetAxisXScale;
    property AxisYScale: Single read FAxisYScale write SetAxisYScale;
    property Monochrome: Boolean read FMonochrome write SetMonochrome;
    property BorderStyle;
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property Align;
    property Anchors;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property PopupMenu;
  end;

  procedure CalculateAxis(AMin, AMax: Double; Count: Integer;
    out Delta, Lowest: Double);

  procedure Register;

implementation

{$R NiceChart.res}

uses
  ClipBrd;

procedure Register;
begin
  RegisterComponents('priyatna.org', [TNiceChart]);
end;

const
  OUTER_MARGIN = 20;
  INNER_MARGIN = 10;
  SMALL_MARGIN = 2;
  LEGEND_ITEM  = 20;
  AXIS_DEFSIZE = 50;

  Formatter = '0.##';

type
  PXYInfo = ^TXYInfo;
  TXYInfo = record
    X, Y: Double;
    Px, Py: Integer;
    Rc: TRect;
    Hint: string;
  end;

  PAxisInfo = ^TAxisInfo;
  TAxisInfo = record
    Value: Double;
    Px, Py: Integer;
    Caption: string;
  end;


function GetMan10(Value: Double): Double;
var
  Str: string;
begin
  Str := UpperCase(Format('%E', [Value]));
  Result := StrToFloat('1E' + Copy(Str, Pos('E', Str) + 1, Length(Str)));
end;

procedure CalculateAxis(AMin, AMax: Double; Count: Integer;
  out Delta, Lowest: Double);
label
  Retry;
var
  c, n, m10: Double;
begin
  c := Max(2, Count-1);
  n := (Abs(AMax - AMin) / c);
  m10 := GetMan10(n);
  Delta := 0;
  while (Delta < n)
    do Delta := Delta + (0.5 * m10);
  if (Delta = 0) then
  begin
    Delta := 1;
    Lowest := AMin - (Count div 2);
    Exit;
  end;
  Retry:
  Lowest := Trunc(AMin / Delta) * Delta;
  if (Lowest > AMin)
    then Lowest := Lowest - Delta;
  if ((Lowest + (Delta * c)) < AMax) then
  begin
    Delta := Delta + (0.5 * m10);
    goto Retry;
  end;
end;


{ TNiceSeries }

constructor TNiceSeries.Create(AChart: TNiceChart; AKind: TSeriesKind);
begin
  inherited Create;
  Chart := AChart;
  Values := TList.Create;
  FCaption := 'Series';
  Spline := TBSpline.Create;
  FKind := AKind;
end;

destructor TNiceSeries.Destroy;
begin
  Spline.Free;
  InternalClear;
  Values.Free;
  inherited Destroy;
end;

procedure TNiceSeries.InternalClear;
var
  x: Integer;
begin
  for x := 0 to Values.Count-1
    do Dispose(PXYInfo(Values[x]));
  Values.Clear;
end;

procedure TNiceSeries.Clear;
begin
  InternalClear;
  Chart.Changed;
end;

function TNiceSeries.AddXY(AX, AY: Double; AHint: string): Integer;
var
  Info: PXYInfo;
begin
  Info := New(PXYInfo);
  Info^.X := AX;
  Info^.Y := AY;
  Info^.Px := 0;
  Info^.Py := 0;
  Info^.Rc := Rect(0, 0, 0, 0);
  Info^.Hint := AHint;
  Result := Values.Add(Info);
  Chart.Changed;
end;

procedure TNiceSeries.Remove(Index: Integer);
var
  P: PXYInfo;
begin
  if (Index >= 0) and (Index < Values.Count) then
  begin
    P := Values[Index];
    Values.Remove(P);
    Dispose(P);
    Chart.Changed;
  end;
end;

function TNiceSeries.GetMaxXValue: Double;
var
  x: Integer;
begin
  Result := -MaxDouble;
  for x := 0 to Values.Count-1
    do Result := Max(Result, PXYInfo(Values[x])^.X);
end;

function TNiceSeries.GetMinXValue: Double;
var
  x: Integer;
begin
  Result := MaxDouble;
  for x := 0 to Values.Count-1
    do Result := Min(Result, PXYInfo(Values[x])^.X);
end;

function TNiceSeries.GetMaxYValue: Double;
var
  x: Integer;
begin
  Result := -MaxDouble;
  for x := 0 to Values.Count-1
    do Result := Max(Result, PXYInfo(Values[x])^.Y);
end;

function TNiceSeries.GetMinYValue: Double;
var
  x: Integer;
begin
  Result := MaxDouble;
  for x := 0 to Values.Count-1
    do Result := Min(Result, PXYInfo(Values[x])^.Y);
end;

procedure TNiceSeries.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Chart.Changed;
  end;  
end;

procedure TNiceSeries.SetKind(const Value: TSeriesKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;
    Chart.Changed;
  end;
end;

{ TNiceChart }

constructor TNiceChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  ParentBackground := False;
  ParentFont := False;
  Temp := TStringList.Create;
  Width := 300;
  Height := 200;
  Color := clWhite;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BorderStyle := bsSingle;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  List := TList.Create;
  FShowLegend := True;
  FShowTitle := True;
  FShowXGrid := True;
  FShowYGrid := True;
  FMonochrome := False;
  FTitle := 'Chart Title';
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Arial';
  FTitleFont.Size := 14;
  FTitleFont.Style := [];
  FTitleFont.OnChange := TitleFontChanged;
  FNormalFont := TFont.Create;
  FNormalFont.Name := 'Arial';
  FAxisXTitle := 'X Axis';
  FAxisYTitle := 'Y Axis';
  FAxisXScale := 1;
  FAxisYScale := 1;
  XAxis := TList.Create;
  YAxis := TList.Create;
  FUpdating := False;
end;

destructor TNiceChart.Destroy;
var
  x: Integer;
begin
  for x := 0 to 15 do
  begin
    if Assigned(Brushes[x])
      then Brushes[x].Free;
  end;
  InternalClear;
  List.Free;
  FTitleFont.Free;
  FNormalFont.Free;
  XAxis.Free;
  YAxis.Free;
  Temp.Free;
  inherited Destroy;
end;

procedure TNiceChart.InternalClear;
var
  x: Integer;
begin
  for x := 0 to List.Count-1
    do TNiceSeries(List[x]).Free;
  ClearAxis;  
  List.Clear;
end;

procedure TNiceChart.Paint;
begin
  if HandleAllocated
    then DoPaint;
end;

procedure TNiceChart.DoPaint;
begin
  InternalPaint(Canvas);
end;

procedure TNiceChart.SetMonochrome(const Value: Boolean);
begin
  if (FMonochrome <> Value) then
  begin
    FMonochrome := Value;
    Changed;
  end;
end;

procedure TNiceChart.SetShowLegend(const Value: Boolean);
begin
  if (FShowLegend <> Value) then
  begin
    FShowLegend := Value;
    Changed;
  end;  
end;

procedure TNiceChart.SetAxisXOnePerValue(const Value: Boolean);
begin
  if (FAxisXOnePerValue <> Value) then
  begin
    FAxisXOnePerValue := Value;
    Changed;
  end;  
end;

procedure TNiceChart.SetShowTitle(const Value: Boolean);
begin
  if (FShowTitle <> Value) then
  begin
    FShowTitle := Value;
    Changed;
  end;
end;

procedure TNiceChart.SetTitle(const Value: string);
begin
  if (FTitle <> Value) then
  begin
    FTitle := Value;
    Changed;
  end;
end;

procedure TNiceChart.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TNiceChart.TitleFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TNiceChart.SetAxisXTitle(const Value: string);
begin
  if (FAxisXTitle <> Value) then
  begin
    FAxisXTitle := Value;
    Changed;
  end;
end;

procedure TNiceChart.SetAxisYTitle(const Value: string);
begin
  if (FAxisYTitle <> Value) then
  begin
    FAxisYTitle := Value;
    Changed;
  end;
end;

procedure TNiceChart.SetAxisXScale(const Value: Single);
begin
  if (FAxisXScale <> Value) then
  begin
    FAxisXScale := Value;
    if (FAxisXScale = 0)
      then FAxisXScale := 1;
    Changed;
  end;  
end;

procedure TNiceChart.SetAxisYScale(const Value: Single);
begin
  if (FAxisYScale <> Value) then
  begin
    FAxisYScale := Value;
    if (FAxisYScale = 0)
      then FAxisYScale := 1;
    Changed;
  end;  
end;

procedure TNiceChart.SetShowXGrid(const Value: Boolean);
begin
  if (FShowXGrid <> Value) then
  begin
    FShowXGrid := Value;
    DoPaint;
  end;  
end;

procedure TNiceChart.SetShowYGrid(const Value: Boolean);
begin
  if (FShowYGrid <> Value) then
  begin
    FShowYGrid := Value;
    DoPaint;
  end;  
end;

procedure TNiceChart.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TNiceChart.EndUpdate;
begin
  FUpdating := False;
  Calculate(ClientWidth, ClientHeight);
  DoPaint;
end;

procedure TNiceChart.Changed;
begin
  if not FUpdating then
  begin
    Calculate(ClientWidth, ClientHeight);
    DoPaint;
  end;
end;

procedure TNiceChart.WMSize(var Msg: TWMSize);
begin
  inherited;
  Changed;
end;

procedure TNiceChart.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

function TNiceChart.GetSeries(Index: Integer): TNiceSeries;
begin
  Result := TNiceSeries(List[Index]);
end;

function TNiceChart.AddSeries(AKind: TSeriesKind): TNiceSeries;
begin
  Result := TNiceSeries.Create(Self, AKind);
  List.Add(Result);
end;

procedure TNiceChart.Clear;
begin
  InternalClear;
  Changed;
end;

procedure TNiceChart.RemoveSeries(ASeries: TNiceSeries);
begin
  if Assigned(ASeries) then
  begin
    List.Remove(ASeries);
    ASeries.Free;
    Changed;
  end;
end;

function TNiceChart.GetSeriesCount: Integer;
begin
  Result := List.Count;
end;

procedure TNiceChart.DrawLegend(ACanvas: TCanvas);
var
  x, y, l, t: Integer;
  th, g: Integer;
begin
  with ACanvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
    Font.Assign(FNormalFont);
    g := TextHeight('Ag');
    th := (LEGEND_ITEM - g) div 2;
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(Rect(RcLegend.Right, RcLegend.Top + 3, RcLegend.Right + 3, RcLegend.Bottom + 3));
    FillRect(Rect(RcLegend.Left + 3, RcLegend.Bottom, RcLegend.Right + 3, RcLegend.Bottom + 3));
    Brush.Style := bsClear;
    Rectangle(RcLegend);
    Brush.Style := bsClear;
    l := RcLegend.Left + INNER_MARGIN + LEGEND_ITEM + SMALL_MARGIN;
    for x := 0 to List.Count-1 do
    begin
      Temp.Text := Trim(TNiceSeries(List[x]).FCaption);
      t := RcLegend.Top + TNiceSeries(List[x]).Top;
      for y := 0 to Temp.Count-1 do
      begin
        TextOut(l, t + th, Trim(Temp[y]));
        Inc(t, g);
      end;
    end;
  end;
end;

procedure TNiceChart.DrawTitle(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Font.Assign(FTitleFont);
    DrawText(Handle, PChar(FTitle), Length(FTitle), RcTitle,
      DT_CENTER or DT_VCENTER or DT_WORDBREAK);
  end;
end;

procedure RotTextOut(ACanvas: TCanvas; x, y, Angle: Integer; Txt: String);
var
  RotFont, OldFont: Integer;
  FBold, FItalic, FUnderline, FStrikeOut: integer;
begin
  if (Txt = '')
    then Exit;
  SetBkMode(ACanvas.Handle, TRANSPARENT);
  if (fsItalic in ACanvas.Font.Style)
    then FItalic := 1
    else FItalic := 0;
  if (fsUnderline in ACanvas.Font.Style)
    then FUnderline := 1
    else FUnderline := 0;
  if (fsStrikeOut in ACanvas.Font.Style)
    then FStrikeOut := 1
    else FStrikeOut := 0;
  if (fsBold in ACanvas.Font.Style)
    then FBold := FW_BOLD
    else FBold := FW_NORMAL;
  RotFont := CreateFont(ACanvas.Font.Height, 0, Angle*10, 0,
     FBold, FItalic, FUnderline, FStrikeOut, 1, 4, $10,
     ANTIALIASED_QUALITY, 4, PChar(ACanvas.Font.Name));
  OldFont := SelectObject(ACanvas.Handle, RotFont);
  TextOut(ACanvas.Handle, x, y, PChar(Txt), Length(Txt));
  SelectObject(ACanvas.Handle, OldFont);
  DeleteObject(RotFont);
end;

procedure TNiceChart.InternalPaint(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(0, 0, DestWidth, DestHeight));
  end;
  if FShowLegend and (List.Count > 0)
    then DrawLegend(ACanvas);
  if FShowTitle and (FTitle <> '')
    then DrawTitle(ACanvas);
  DrawXAxis(ACanvas);
  DrawYAxis(ACanvas);
  DrawChart(ACanvas);  
end;

procedure TNiceChart.Calculate(AWidth, AHeight: Integer);
var
  x, w, h, y, g: Integer;
  Titled: Boolean;

begin

  ClearAxis;

  DestWidth := AWidth;
  DestHeight := AHeight;
  RcChart := Rect(0, 0, DestWidth, DestHeight);
  MarkSize := Max(1, Round(DestWidth * 0.004));

  InflateRect(RcChart, -OUTER_MARGIN, -OUTER_MARGIN);

  Titled := False;
  if FShowTitle and (FTitle <> '') then
  begin
    Canvas.Font.Assign(TitleFont);
    w := Canvas.TextHeight(FTitle);
    RcTitle := Rect(RcChart.Left, RcChart.Top, RcChart.Right, RcChart.Left + w);
    DrawText(Canvas.Handle, PChar(FTitle), Length(FTitle), RcTitle,
      DT_CENTER or DT_VCENTER or DT_WORDBREAK or DT_CALCRECT);
    RcChart.Top := RcTitle.Bottom + INNER_MARGIN;
    Titled := True;
  end else
    SetRectEmpty(RcTitle);

  Canvas.Font.Assign(FNormalFont);
  h := Canvas.TextHeight('Ag');
  RcChart.Bottom := RcChart.Bottom - (2 * h) - INNER_MARGIN - (2 * SMALL_MARGIN);

  BuildYAxis;
  w := 0;
  for x := 0 to YAxis.Count-1
    do w := Max(w, Canvas.TextWidth(PAxisInfo(YAxis[x])^.Caption));
  RcChart.Left := RcChart.Left + h + INNER_MARGIN + w + (2 * SMALL_MARGIN);
  RcTitle.Left := RcChart.Left;
  AdjustYAxis;

  if FShowLegend and (List.Count > 0) then
  begin
    Canvas.Font.Assign(FNormalFont);
    w := 0;
    h := INNER_MARGIN;
    g := Canvas.TextHeight('Ag');
    for x := 0 to List.Count-1 do
    begin
      TNiceSeries(List[x]).Top := h;
      Temp.Text := Trim(TNiceSeries(List[x]).FCaption);
      for y := 0 to Temp.Count-1
        do w := Max(w, Canvas.TextWidth(Trim(Temp[y])));
      h := h + Max(LEGEND_ITEM, Temp.Count * g);
      if (x <> List.Count-1)
        then h := h + SMALL_MARGIN;
    end;
    w := w + (2 * INNER_MARGIN) + LEGEND_ITEM + SMALL_MARGIN;
    h := h + INNER_MARGIN;
    RcLegend := Rect(RcChart.Right - w, RcChart.Top, RcChart.Right, RcChart.Top + h);
    RcChart.Right := RcLegend.Left - (2 * INNER_MARGIN);
    if Titled
      then RcTitle.Right := RcChart.Right;
  end else
    SetRectEmpty(RcLegend);

  BuildXAxis;

  CalculateSeries;

end;

procedure TNiceChart.ClearAxis;
var
  x: Integer;
begin
  for x := 0 to XAxis.Count-1
    do Dispose(PAxisInfo(XAxis[x]));
  XAxis.Clear;
  for x := 0 to YAxis.Count-1
    do Dispose(PAxisInfo(YAxis[x]));
  YAxis.Clear;
end;

type
  PDoubleList = ^TDoubleList;
  TDoubleList = array [0..0] of Double;

procedure QuickSortDouble(SortList: PDoubleList; L, R: Integer);
var
  I, J: Integer;
  P, T: Double;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (SortList^[I] < P)
        do Inc(I);
      while (SortList^[J] > P)
        do Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J
      then QuickSortDouble(SortList, L, J);
    L := I;
  until I >= R;
end;

function TNiceChart.GetLabel(Value: Double): string;
begin
  if (Value = UndefinedValue)
    then Result := '~'
    else Result := FormatFloat(Formatter, Value);
end;

procedure TNiceChart.BuildXAxis;
var
  x, y, w: Integer;
  mi, ma: Double;
  Cnt, i, n: Integer;
  Delta, Lowest, l: Double;
  P: PAxisInfo;
  Temp: PDoubleList;
  Vals: TList;
  Last: Double;
  Scale: Double;
  dx: Integer;

begin

  if (List.Count = 0)
    then Exit;

  BarCount := 0;
  for x := 0 to List.Count-1 do
  begin
    if (TNiceSeries(List[x]).FKind = skBar)
      then Inc(BarCount);
  end;
  if (BarCount > 0)
    then FAxisXOnePerValue := True;

  if FAxisXOnePerValue then
  begin
    w := RcChart.Right - RcChart.Left;
    Cnt := 0;
    for x := 0 to List.Count-1
      do Cnt := Cnt + Series[x].Values.Count;
    GetMem(Temp, Cnt * SizeOf(Double));
    i := 0;
    for x := 0 to List.Count-1 do
    begin
      Vals := TNiceSeries(List[x]).Values;
      for y := 0 to Vals.Count-1 do
      begin
        Temp^[i] := PXYInfo(Vals[y])^.X;
        Inc(i);
      end;
    end;
    QuickSortDouble(Temp, 0, Cnt-1);
    n := 0;
    Last := MaxDouble;
    for x := 0 to Cnt-1 do
    begin
      l := Temp^[x];
      if (l = Last)
        then Continue;
      Inc(n);
      Last := l;
    end;
    if (BarCount > 0) then
    begin
      Scale := w / n;
      dx := Round(Scale / 2);
      BarWidth := Round(Scale);
    end else
    begin
      Scale := w / (n-1);
      dx := 0;
    end;
    Last := MaxDouble;
    i := 0;
    for x := 0 to Cnt-1 do
    begin
      l := Temp^[x];
      if (l = Last)
        then Continue;
      P := New(PAxisInfo);
      P^.Value := l;
      P^.Py := RcChart.Bottom;
      P^.Px := RcChart.Left + dx + Round(i * Scale);
      P^.Caption := GetLabel(l / FAxisXScale);
      XAxis.Add(P);
      Last := l;
      Inc(i);
    end;
    FreeMem(Temp);
  end else
  begin
    w := RcChart.Right - RcChart.Left;
    Cnt := (w div AXIS_DEFSIZE) + 1;
    mi := MaxDouble;
    ma := -MaxDouble;
    for x := 0 to List.Count-1 do
    begin
      mi := Min(mi, Series[x].GetMinXValue);
      ma := Max(ma, Series[x].GetMaxXValue);
    end;
    CalculateAxis(mi, ma, Cnt, Delta, Lowest);
    Scale := w / (Delta * Max(1, Cnt-1));
    for x := 0 to Cnt-1 do
    begin
      l := x * Delta;
      P := New(PAxisInfo);
      P^.Py := RcChart.Bottom;
      P^.Px := RcChart.Left + Round(l * Scale);
      P^.Caption := GetLabel((Lowest + l) / FAxisXScale);
      XAxis.Add(P);
    end;
    FXTranslate.MinValue := Lowest;
    FXTranslate.Scale := Scale;
    FXTranslate.Base := RcChart.Left;
  end;
  
end;

procedure TNiceChart.BuildYAxis;
var
  x, w: Integer;
  mi, ma: Double;
  Cnt: Integer;
  Delta, Lowest, t: Double;
  P: PAxisInfo;
  Scale: Double;
begin
  if (List.Count = 0)
    then Exit;
  w := RcChart.Bottom - RcChart.Top;
  Cnt := (w div AXIS_DEFSIZE) + 1;
  mi := MaxDouble;
  ma := -MaxDouble;
  for x := 0 to List.Count-1 do
  begin
    mi := Min(mi, Series[x].GetMinYValue);
    ma := Max(ma, Series[x].GetMaxYValue);
  end;
  CalculateAxis(mi, ma, Cnt, Delta, Lowest);
  Scale := w / (Delta * Max(1, Cnt-1));
  for x := 0 to Cnt-1 do
  begin
    t := x * Delta;
    P := New(PAxisInfo);
    P^.Value := Lowest + t;
    P^.Py := Round(t * Scale);
    P^.Caption := GetLabel((Lowest + t) / FAxisYScale);
    YAxis.Add(P);
  end;
  FYTranslate.MinValue := Lowest;
  FYTranslate.Scale := Scale;
end;

procedure TNiceChart.AdjustYAxis;
var
  x: Integer;
  P: PAxisInfo;
  l: Integer;
begin
  l := RcChart.Left;
  YZero := -1;
  for x := 0 to YAxis.Count-1 do
  begin
    P := PAxisInfo(YAxis[x]);
    P^.Px := l;
    P^.Py := RcChart.Bottom - P^.Py;
    if (P^.Value = 0)
      then YZero := P^.Py;
  end;
  if (YZero = -1)
    then YZero := RcChart.Bottom;
  FYTranslate.Base := RcChart.Bottom;
end;

procedure TNiceChart.DrawXAxis(ACanvas: TCanvas);
var
  l, t, w, x: Integer;
  P: PAxisInfo;
  Str: string;
  Last: Integer;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 3;
    MoveTo(RcChart.Left, RcChart.Bottom);
    LineTo(RcChart.Right, RcChart.Bottom);
    Font.Assign(FNormalFont);
    Font.Style := [fsBold];
    w := RcChart.Right - RcChart.Left;
    t := RcChart.Bottom + INNER_MARGIN + (2 * SMALL_MARGIN) + TextHeight('Ag');
    l := RcChart.Left + ((w - TextWidth(FAxisXTitle)) div 2);
    TextOut(l, t, FAxisXTitle);
    Font.Assign(FNormalFont);
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    t := RcChart.Bottom + (2 * SMALL_MARGIN);
    Last := 0;
    for x := 0 to XAxis.Count-1 do
    begin
      P :=  PAxisInfo(XAxis[x]);
      Str := P^.Caption;
      w := TextWidth(Str);
      l := P^.Px - (w div 2);
      if (Last < l) then
      begin
        TextOut(l, t, Str);
        Last := l + w;
      end;
      MoveTo(P^.Px, P^.Py);
      LineTo(P^.Px, P^.Py + SMALL_MARGIN);
    end;
    if FShowXGrid then
    begin
      Pen.Style := psDot;
      Pen.Color := clGray;
      t := RcChart.Top;
      for x := 1 to XAxis.Count-2 do
      begin
        P := PAxisInfo(XAxis[x]);
        MoveTo(P^.Px, P^.Py);
        LineTo(P^.px, t);
      end;
      Pen.Color := clBlack;
    end;
  end;
end;

procedure TNiceChart.DrawYAxis(ACanvas: TCanvas);
var
  l, t, h, w: Integer;
  x: Integer;
  Str: string;
  P: PAxisInfo;
begin
  with ACanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 3;
    MoveTo(RcChart.Left, RcChart.Top);
    LineTo(RcChart.Left, RcChart.Bottom);
    h := RcChart.Bottom - RcChart.Top;
    l := OUTER_MARGIN;
    Font.Assign(FNormalFont);
    Font.Style := [fsBold];
    t := RcChart.Bottom - ((h - TextWidth(FAxisYTitle)) div 2);
    RotTextOut(ACanvas, l, t, 90, FAxisYTitle);
    Font.Assign(FNormalFont);
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    l := RcChart.Left - (2 * SMALL_MARGIN);
    for x := 0 to YAxis.Count-1 do
    begin
      P :=  PAxisInfo(YAxis[x]);
      Str := P^.Caption;
      w := TextWidth(Str);
      h := TextHeight(Str);
      t := P^.Py - (h div 2);
      TextOut(l - w, t, Str);
      MoveTo(P^.Px - SMALL_MARGIN, P^.Py);
      LineTo(P^.Px, P^.Py);
    end;
    if FShowYGrid then
    begin
      l := RcChart.Right;
      for x := 1 to YAxis.Count-2 do
      begin
        P := PAxisInfo(YAxis[x]);
        if (P^.Value = 0) then
        begin
          Pen.Style := psSolid;
          Pen.Color := clBlack;
        end else
        begin
          Pen.Style := psDot;
          Pen.Color := clGray;
        end;
        MoveTo(P^.Px, P^.Py);
        LineTo(l, P^.Py);
      end;
      Pen.Color := clBlack;
    end;
  end;
end;

procedure TNiceChart.DrawChart(ACanvas: TCanvas);
var
  x: Integer;
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Width := 1;
    MoveTo(RcChart.Left, RcChart.Top);
    LineTo(RcChart.Right, RcChart.Top);
    LineTo(RcChart.Right, RcChart.Bottom);
  end;
  for x := 0 to List.Count-1 do
  begin
    if (TNiceSeries(List[x]).FKind = skBar)
      then DrawSeries(ACanvas, x);
  end;
  for x := 0 to List.Count-1 do
  begin
    if (TNiceSeries(List[x]).FKind <> skBar)
      then DrawSeries(ACanvas, x);
  end;
end;


//-----------------------------------------------------------------------------//


procedure MarkerRectangle(ACanvas: TCanvas; X, Y, Size: Integer);
begin
  ACanvas.Rectangle(X-Size, Y-Size, X+Size, Y+Size);
end;

procedure MarkerCircle(ACanvas: TCanvas; X, Y, Size: Integer);
begin
  ACanvas.Ellipse(X-Size, Y-Size, X+Size, Y+Size);
end;

procedure MarkerTriangle1(ACanvas: TCanvas; X, Y, Size: Integer);
begin
  ACanvas.Polygon([Point(x, y-Size), Point(x+Size, y+Size), Point(x-Size, y+Size)]);
end;

procedure MarkerTriangle2(ACanvas: TCanvas; X, Y, Size: Integer);
begin
  ACanvas.Polygon([Point(x+Size, y-Size), Point(x-Size, y-Size), Point(x, y+Size)]);
end;

procedure MarkerDiamond(ACanvas: TCanvas; X, Y, Size: Integer);
begin
  ACanvas.Polygon([Point(x, y-Size), Point(x+Size, y), Point(x, y+Size), Point(x-Size, y)]);
end;

const
  Colors: array [0..13] of TColor = (
    clRed, clBlue, clGreen, clFuchsia, clNavy, clMaroon, clBlack, clOlive,
    clPurple, clTeal, clGray, clLime, clYellow, clAqua);

  Markers: array [0..4] of TMarkerProc = (
    MarkerRectangle, MarkerCircle, MarkerTriangle1, MarkerTriangle2,
    MarkerDiamond);

    
procedure TNiceChart.AutoColors(ACanvas: TCanvas; Index: Integer; Isbar: Boolean);
var
  cl: TColor;
  Idx: Integer;
  Bmp: TBitmap;
begin
  if FMonochrome
    then cl := clBlack
    else cl := Colors[Index mod 14];
  Marker := Markers[Index mod 5];
  with ACanvas do
  begin
    Pen.Color := cl;
    Brush.Bitmap := nil;
    Brush.Style := bsSolid;
    if IsBar
      then Brush.Color := cl
      else Brush.Color := clWhite;
    if IsBar and FMonochrome then
    begin
      Idx := Index mod 16;
      if not Assigned(Brushes[Idx]) then
      begin
        Bmp := TBitmap.Create;
        Bmp.LoadFromResourceName(hInstance, Format('brush%.2d', [Idx+1]));
        Brushes[Idx] := Bmp;
      end;
      Brush.Bitmap := Brushes[Idx];
    end;
  end;
end;


//-----------------------------------------------------------------------------//


procedure TNiceChart.DrawSeries(ACanvas: TCanvas; Index: Integer);
var
  x: Integer;
  P: PXYInfo;
  l, t, t2: Integer;
  Sr: TNiceSeries;
  Rc: TRect;
begin
  Sr := TNiceSeries(List[Index]);
  AutoColors(ACanvas, Index, sr.FKind = skBar);
  with ACanvas do
  begin
    if (sr.FKind = skBar) then
    begin
      for x := 0 to Sr.Values.Count-1 do
      begin
        P := PXYInfo(Sr.Values[x]);
        Rectangle(P^.Rc);
      end;
    end else
    begin
      if (sr.FKind = skLine) then
      begin
        for x := 0 to Sr.Values.Count-1 do
        begin
          P := PXYInfo(Sr.Values[x]);
          if (x = 0)
            then MoveTo(P^.Px, P^.Py)
            else LineTo(P^.Px, P^.Py);
        end;
      end else
      if (sr.FKind = skSmooth)
        then sr.Spline.Draw(ACanvas);
      for x := 0 to Sr.Values.Count-1 do
      begin
        P := PXYInfo(Sr.Values[x]);
        Marker(ACanvas, P^.Px, P^.Py, MarkSize);
      end;
    end;
    if FShowLegend then
    begin
      l := RcLegend.Left + INNER_MARGIN;
      t := RcLegend.Top + Sr.Top;
      if (sr.FKind = skBar) then
      begin
        Rc := Rect(l, t, l + LEGEND_ITEM, t + LEGEND_ITEM);
        InflateRect(Rc, -2, -2);
        Rectangle(Rc);
      end else
      begin
        t2 := t + (LEGEND_ITEM div 2);
        MoveTo(l, t2);
        LineTo(l + LEGEND_ITEM, t2);
        Marker(ACanvas, l + (LEGEND_ITEM div 2), t2, MarkSize);
      end;
    end;
  end;
end;

procedure TNiceChart.CalculateSeries;
var
  x, y: Integer;
  Values: TList;
  P: PXYInfo;
  S: TBSpline;
  Vertex: TVertex;
  sr: TNiceSeries;
  bw, rw, bi, dx, l: Integer;
begin
  bi := 0;
  bw := 0;
  if (BarCount > 0)
    then bw := Round(BarWidth / (BarCount + 1));
  for x := 0 to List.Count-1 do
  begin
    sr := TNiceSeries(List[x]);
    s := sr.Spline;
    s.Clear;
    Values := sr.Values;
    case sr.FKind of
      skBar:
        begin
          dx := Round(-(BarWidth / 2) + (bw / 2) + (bi * bw) + (bw * 0.1));
          rw := Round(bw * 0.8);
          for y := 0 to Values.Count-1 do
          begin
            P := PXYInfo(Values[y]);
            ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
            l := P^.Px + dx;
            if (P^.Y < 0)
              then P^.Rc := Rect(l, YZero, l + rw, P^.Py)
              else P^.Rc := Rect(l, P^.Py, l + rw, YZero);
          end;
          Inc(bi);
        end;
      skLine:
        begin
          for y := 0 to Values.Count-1 do
          begin
            P := PXYInfo(Values[y]);
            ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
            P^.Rc := Rect(P^.Px-MarkSize, P.Py-MarkSize, P^.Px+MarkSize, P^.Py+MarkSize);
          end;
        end;
      skSmooth:
        begin
          for y := 0 to Values.Count-1 do
          begin
            P := PXYInfo(Values[y]);
            ChartToClient(P^.X, P^.Y, P^.Px, P^.Py);
            P^.Rc := Rect(P^.Px-MarkSize, P.Py-MarkSize, P^.Px+MarkSize, P^.Py+MarkSize);
            Vertex.X := P^.Px;
            Vertex.Y := P^.Py;
            s.AddPoint(Vertex);
          end;
          s.Interpolated := True;
          s.Fragments := s.NumberOfPoints * 20;
        end;
    end;
  end;
end;

procedure TNiceChart.ChartToClient(const AX, AY: Double; var X, Y: Integer);
var
  i: Integer;
begin
  if FAxisXOnePerValue then
  begin
    for i := 0 to XAxis.Count-1 do
    begin
      if (AX = PAxisInfo(XAxis[i])^.Value) then
      begin
        X := PAxisInfo(XAxis[i])^.Px;
        Break;
      end;
    end;
  end else
    X := FXTranslate.Base + Round((AX - FXTranslate.MinValue) * FXTranslate.Scale);
  Y := FYTranslate.Base - Round((AY - FYTranslate.MinValue) * FYTranslate.Scale);
end;

function TNiceChart.ClientToChart(const X, Y: Integer; var AX, AY: Double): Boolean;
var
  i: Integer;
  n, d: Integer;
begin
  Result := PtInRect(RcChart, Point(X, Y));
  if Result then
  begin
    if FAxisXOnePerValue then
    begin
      n := MaxInt;
      for i := 0 to XAxis.Count-1 do
      begin
        d := Abs(X - PAxisInfo(XAxis[i])^.Px);
        if (d < n) then
        begin
          AX := PAxisInfo(XAxis[i])^.Value;
          n := d;
        end;
      end;
    end else
      AX := FXTranslate.MinValue + ((X - FXTranslate.Base) / FXTranslate.Scale);
    AY := FYTranslate.MinValue + ((FYTranslate.Base - Y) / FYTranslate.Scale);
  end;
end;

function TNiceChart.CreateMetafile: TMetafile;
const
  InitWidth = 800;
  InitHeight = 600;
var
  mc: TMetafileCanvas;
  AWidth, AHeight: Integer;
begin
  AWidth := InitWidth;
  AHeight := InitHeight;
  Calculate(AWidth, AHeight);
  if (RcLegend.Bottom > (AHeight - OUTER_MARGIN))
    then AHeight := RcLegend.Bottom + OUTER_MARGIN;
  if ((RcChart.Right - RcChart.Left) < (RcChart.Bottom - RcChart.Top))
    then AWidth := AWidth + ((RcChart.Bottom - RcChart.Top) - (RCChart.Right - RcChart.Left));
  if (AWidth <> InitWidth) or (AHeight <> InitHeight)
    then Calculate(AWidth, AHeight);
  Result := TMetafile.Create;
  Result.Width := AWidth;
  Result.Height := AHeight;
  mc := TMetafileCanvas.Create(Result, 0);
  InternalPaint(mc);
  mc.Free;
  Calculate(ClientWidth, ClientHeight);
end;

procedure TNiceChart.CopyToClipboard;
var
  Wmf: TMetafile;
begin
  Wmf := CreateMetafile;
  Clipboard.Assign(Wmf);
  Wmf.Free;
end;

end.

