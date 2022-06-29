
{*****************************************}
{                                         }
{             FastReport v2.3             }
{           Chart Add-In Object           }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Chart;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_Class, ExtCtrls, TeeProcs, TeEngine, Chart, Series, StdCtrls, FR_Ctrls,
  ComCtrls, Menus;

type
  TChartOptions = packed record
    ChartType: Byte;
    Dim3D, IsSingle, ShowLegend, ShowAxis, ShowMarks, Colored: Boolean;
    MarksStyle: Byte;
    Top10Num: Integer;
    Reserved: Array[0..35] of Byte;
  end;

  TfrChartObject = class(TComponent)  // fake component
  end;

  TfrChartView = class(TfrView)
  private
    CurStr: Integer;
    LastLegend: String;
    function ShowChart: Boolean;
  public
    Chart: TChartOptions;
    LegendObj, ValueObj, Top10Label: String;
    constructor Create; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
    procedure OnHook(View: TfrView); override;
  end;

  TfrChartForm = class(TfrObjEditorForm)
    Image1: TImage;
    Page1: TPageControl;
    Tab1: TTabSheet;
    GroupBox1: TGroupBox;
    SB1: TfrSpeedButton;
    SB2: TfrSpeedButton;
    SB3: TfrSpeedButton;
    SB4: TfrSpeedButton;
    SB5: TfrSpeedButton;
    SB6: TfrSpeedButton;
    Tab2: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    E1: TEdit;
    Label2: TLabel;
    E2: TEdit;
    GroupBox3: TGroupBox;
    CB1: TCheckBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    CB4: TCheckBox;
    CB6: TCheckBox;
    CB5: TCheckBox;
    Tab3: TTabSheet;
    GroupBox4: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    RB4: TRadioButton;
    RB5: TRadioButton;
    GroupBox5: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    E3: TEdit;
    E4: TEdit;
    Label5: TLabel;
    Chart1: TChart;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

uses FR_Intrp, FR_Pars, FR_Utils, FR_Const;

{$R *.DFM}

type
  THackView = class(TfrView)
  end;

  TSeriesClass = class of TChartSeries;

var
  frChartForm: TfrChartForm;
  SChart: TChart;

const
  ChartTypes: Array[0..5] of TSeriesClass =
    (TLineSeries, TAreaSeries, TPointSeries,
     TBarSeries, THorizBarSeries, TPieSeries);

function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  i: Integer;
begin
  i := Pos;
  while (i <= Length(Fields)) and (Fields[i] <> ';') do Inc(i);
  Result := Copy(Fields, Pos, i - Pos);
  if (i <= Length(Fields)) and (Fields[i] = ';') then Inc(i);
  Pos := i;
end;

constructor TfrChartView.Create;
begin
  inherited Create;
  with Chart do
  begin
    Dim3D := True;
    IsSingle := True;
    ShowLegend := True;
    ShowMarks := True;
    Colored := True;
  end;
  Typ := gtAddIn;
  BaseName := 'Chart';
  Flags := Flags or flWantHook;
end;

procedure TfrChartView.Assign(From: TfrView);
begin
  inherited Assign(From);
  Chart := TfrChartView(From).Chart;
  LegendObj := TfrChartView(From).LegendObj;
  ValueObj := TfrChartView(From).ValueObj;
  Top10Label := TfrChartView(From).Top10Label;
end;

function TfrChartView.ShowChart: Boolean;
var
  i, j, c1, c2: Integer;
  LegS, ValS, s: String;
  Ser: TChartSeries;
  EMF: TMetafile;

  function Str2Float(s: String): Double;
  begin
    s := Trim(s);
    while (Length(s) > 0) and not (s[1] in ['0'..'9']) do
      s := Copy(s, 2, 255);           // trim all non-digit chars at the begin
    while (Length(s) > 0) and not (s[Length(s)] in ['0'..'9']) do
      s := Copy(s, 1, Length(s) - 1); // trim all non-digit chars at the end
    while Pos(ThousandSeparator, s) <> 0 do
      Delete(s, Pos(ThousandSeparator, s), 1);
    Result := 0;
    try
      Result := StrToFloat(s);
    except
      on exception do;
    end;
  end;

  procedure SortValues(var LegS, ValS: String);
  var
    i, j: Integer;
    sl: TStringList;
    s: String;
    d: Double;
  begin
    sl := TStringList.Create;
    sl.Sorted := True;

    i := 1; j := 1;
    while i <= Length(LegS) do
      sl.Add(SysUtils.Format('%12.3f', [Str2Float(ExtractFieldName(ValS, j))]) + '=' +
             ExtractFieldName(LegS, i));

    LegS := ''; ValS := '';
    for i := 1 to Chart.Top10Num do
    begin
      s := sl[sl.Count - i];
      ValS := ValS + Copy(s, 1, Pos('=', s) - 1) + ';';
      LegS := LegS + Copy(s, Pos('=', s) + 1, 255) + ';';
    end;

    i := sl.Count - Chart.Top10Num - 1; d := 0;
    while i >= 0 do
    begin
      s := sl[i];
      d := d + Str2Float(Copy(s, 1, Pos('=', s) - 1));
      Dec(i);
    end;

    LegS := LegS + Top10Label + ';';
    ValS := ValS + FloatToStr(d) + ';';
    sl.Free;
  end;


begin
  Result := False;
  SChart.RemoveAllSeries;
  with Chart do
  begin
    SChart.Frame.Visible := False;
    SChart.LeftWall.Brush.Style := bsClear;
    SChart.BottomWall.Brush.Style := bsClear;

    SChart.View3D := Dim3D;
    SChart.Legend.Visible := ShowLegend;
    SChart.AxisVisible := ShowAxis;
    SChart.View3DWalls := ChartType <> 5;
{$IFDEF Delphi4}
    SChart.BackWall.Brush.Style := bsClear;
    SChart.View3DOptions.Elevation := 315;
    SChart.View3DOptions.Rotation := 360;
    SChart.View3DOptions.Orthogonal := ChartType <> 5;
{$ENDIF}
  end;

  if Memo.Count > 0 then
    LegS := Memo[0] else
    LegS := '';
  if Memo.Count > 1 then
    ValS := Memo[1] else
    ValS := '';

  if (LegS = '') or (ValS = '') then Exit;
  if LegS[Length(LegS)] <> ';' then
    LegS := LegS + ';';
  if ValS[Length(ValS)] <> ';' then
    ValS := ValS + ';';

  if Chart.IsSingle then
  begin
    Ser := ChartTypes[Chart.ChartType].Create(SChart);
    SChart.AddSeries(Ser);
    if Chart.Colored then
      Ser.ColorEachPoint := True;
    Ser.Marks.Visible := Chart.ShowMarks;
    Ser.Marks.Style := TSeriesMarksStyle(Chart.MarksStyle);

    c1 := 0;
    for i := 1 to Length(LegS) do
      if LegS[i] = ';' then Inc(c1);
    c2 := 0;
    for i := 1 to Length(ValS) do
      if ValS[i] = ';' then Inc(c2);
    if c1 <> c2 then Exit;

    if (Chart.Top10Num > 0) and (c1 > Chart.Top10Num) then
      SortValues(LegS, ValS);
    i := 1; j := 1;
    while i <= Length(LegS) do
    begin
      s := ExtractFieldName(ValS, j);
      Ser.Add(Str2Float(s), ExtractFieldName(LegS, i), clTeeColor);
    end;
  end
  else
  begin
    c1 := 0;
    for i := 1 to Length(LegS) do
      if LegS[i] = ';' then Inc(c1);
    if c1 <> Memo.Count - 1 then Exit;

    i := 1;
    c1 := 1;
    while i <= Length(LegS) do
    begin
      Ser := ChartTypes[Chart.ChartType].Create(SChart);
      SChart.AddSeries(Ser);
      Ser.Title := ExtractFieldName(LegS, i);
      Ser.Marks.Visible := Chart.ShowMarks;
      Ser.Marks.Style := TSeriesMarksStyle(Chart.MarksStyle);
      ValS := Memo[c1];
      if ValS[Length(ValS)] <> ';' then
        ValS := ValS + ';';
      j := 1;
      while j <= Length(ValS) do
      begin
        s := ExtractFieldName(ValS, j);
        Ser.Add(Str2Float(s), '', clTeeColor);
      end;
      Inc(c1);
    end;
  end;

  with Canvas do
  begin
    SChart.Color := FillColor;
    EMF := SChart.TeeCreateMetafile(False, Rect(0, 0, SaveDX, SaveDY));
    StretchDraw(DRect, EMF);
    EMF.Free;
  end;
  Result := True;
end;

procedure TfrChartView.Draw(Canvas: TCanvas);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CalcGaps;
  if not ShowChart then
    ShowBackground;
  ShowFrame;
  RestoreCoord;
end;

procedure TfrChartView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrChartView.LoadFromStream(Stream: TStream);
var
  b: Byte;
  function ReadString(Stream: TStream): String;
  begin
    if frVersion >= 23 then
    {$IFDEF FREEREP2217READ}
    begin
      if (frVersion = 23) and FRE_COMPATIBLE_READ then
        Result := frReadString2217(Stream) // load in bad format
      else
        Result := frReadString(Stream); // load in current format
    end
    else
    {$ELSE}
      Result := frReadString(Stream) else
    {$ENDIF}
      Result := frReadString22(Stream);
  end;
begin
  inherited LoadFromStream(Stream);
  with Stream do
  begin
    Read(b, 1);
    Read(Chart, SizeOf(Chart));
    LegendObj := ReadString(Stream);
    ValueObj := ReadString(Stream);
    Top10Label := ReadString(Stream);
  end;
end;

procedure TfrChartView.SaveToStream(Stream: TStream);
var
  b: Byte;
begin
  inherited SaveToStream(Stream);
  with Stream do
  begin
    b := 0; // internal chart version
    Write(b, 1);
    Write(Chart, SizeOf(Chart));
    frWriteString(Stream, LegendObj);
    frWriteString(Stream, ValueObj);
    frWriteString(Stream, Top10Label);
  end;
end;

procedure TfrChartView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;

procedure TfrChartView.OnHook(View: TfrView);
var
  i: Integer;
  s: String;
begin
  if Memo.Count < 2 then
  begin
    Memo.Clear;
    Memo.Add('');
    Memo.Add('');
  end;
  i := -1;
  if AnsiCompareText(View.Name, LegendObj) = 0 then
  begin
    i := 0;
    Inc(CurStr);
  end
  else if AnsiCompareText(View.Name, ValueObj) = 0 then
    i := CurStr;
  if Chart.IsSingle then
    CurStr := 1;

  if i >= 0 then
  begin
    if Memo.Count <= i then
      while Memo.Count <= i do
        Memo.Add('');
    if THackView(View).Memo1.Count > 0 then
    begin
      s := THackView(View).Memo1[0];
      if LastLegend <> s then
        Memo[i] := Memo[i] + s + ';';
      LastLegend := s;
    end;
  end;
end;


{------------------------------------------------------------------------}
procedure TfrChartForm.ShowEditor(t: TfrView);
  procedure SetButton(b: Array of TfrSpeedButton; n: Integer);
  begin
    b[n].Down := True;
  end;
  function GetButton(b: Array of TfrSpeedButton): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(b) do
      if b[i].Down then
        Result := i;
  end;
  procedure SetRButton(b: Array of TRadioButton; n: Integer);
  begin
    b[n].Checked := True;
  end;
  function GetRButton(b: Array of TRadioButton): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to High(b) do
      if b[i].Checked then
        Result := i;
  end;
begin
  Page1.ActivePage := Tab1;
  with TfrChartView(t), Chart do
  begin
    SetButton([SB1, SB2, SB3, SB4, SB5, SB6], ChartType);
    SetRButton([RB1, RB2, RB3, RB4, RB5], MarksStyle);
    CB1.Checked := Dim3D;
    CB2.Checked := IsSingle;
    CB3.Checked := ShowLegend;
    CB4.Checked := ShowAxis;
    CB5.Checked := ShowMarks;
    CB6.Checked := Colored;
    E1.Text := LegendObj;
    E2.Text := ValueObj;
    E3.Text := IntToStr(Top10Num);
    E4.Text := Top10Label;
    if ShowModal = mrOk then
    begin
      frDesigner.BeforeChange;
      ChartType := GetButton([SB1, SB2, SB3, SB4, SB5, SB6]);
      MarksStyle := GetRButton([RB1, RB2, RB3, RB4, RB5]);
      Dim3D := CB1.Checked;
      IsSingle := CB2.Checked;
      ShowLegend := CB3.Checked;
      ShowAxis := CB4.Checked;
      ShowMarks := CB5.Checked;
      Colored := CB6.Checked;
      LegendObj := E1.Text;
      ValueObj := E2.Text;
      Top10Num := StrToInt(E3.Text);
      Top10Label := E4.Text;
    end;
  end;
end;

procedure TfrChartForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 590);
  Tab1.Caption := LoadStr(frRes + 591);
  Tab2.Caption := LoadStr(frRes + 592);
  Tab3.Caption := LoadStr(frRes + 604);
  GroupBox1.Caption := LoadStr(frRes + 593);
  GroupBox2.Caption := LoadStr(frRes + 594);
  GroupBox3.Caption := LoadStr(frRes + 595);
  GroupBox4.Caption := LoadStr(frRes + 605);
  GroupBox5.Caption := LoadStr(frRes + 611);
  CB1.Caption := LoadStr(frRes + 596);
  CB2.Caption := LoadStr(frRes + 597);
  CB3.Caption := LoadStr(frRes + 598);
  CB4.Caption := LoadStr(frRes + 599);
  CB5.Caption := LoadStr(frRes + 600);
  CB6.Caption := LoadStr(frRes + 601);
  RB1.Caption := LoadStr(frRes + 606);
  RB2.Caption := LoadStr(frRes + 607);
  RB3.Caption := LoadStr(frRes + 608);
  RB4.Caption := LoadStr(frRes + 609);
  RB5.Caption := LoadStr(frRes + 610);
  Label1.Caption := LoadStr(frRes + 602);
  Label2.Caption := LoadStr(frRes + 603);
  Label3.Caption := LoadStr(frRes + 612);
  Label4.Caption := LoadStr(frRes + 613);
  Label5.Caption := LoadStr(frRes + 614);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

initialization
  frChartForm := TfrChartForm.Create(nil);
  SChart := frChartForm.Chart1;
  frRegisterObject(TfrChartView, frChartForm.Image1.Picture.Bitmap,
    LoadStr(SInsChart), frChartForm);

finalization
  frChartForm.Free;
  frChartForm := nil;

end.

