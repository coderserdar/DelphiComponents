// The TJBKTrend component is a graphical component designed to show
// an animated plot of parameter values. As values are added old values
// scroll off the graph to the left. The trend can be viewed in a number of ways
//
// tsBar - Each value is represented as a rectangle
// tsLine - A line is drawn connecting each value
// tsScatter - A small rectangle is drawn at each value
// ts3D - Each value is represented by a 3D rectangle
// tsFilled - As tsLine except that the area below the line is filled
//
// A few options can be applied
// toValues - On tsBar and ts3D text values are added to each column.
// toTwin - Two values are displayed for each slot
//
// One area where the last option is useful is when monitoring systems with
// demand and feedback values. Here it is important to know that a given command
// is tracked accuratley. Differences between command and feedback are especially
// well highlighted by using ts3D and toTwin
//
// Version 1.0 Mark Dodson 3rd July 1997
//
// Version 2.0 Dr. Juergen Kehrel
// Grid, AutoScale, BackGroundColor and About by Dr. Juergen Kehrel, 6.5.2002
// Version 2.01
// separate runtime and designtime packages for Delphi 6  jbk 17.7.2003
// Version 2.02
// minor adjustments for Delphi 7 compatibility. Use of jedi.inc. jbk 8.3.2004
// Version 2.03 Bart Michel, March 2005
// Some changes in Paint procedure. Used Font.Name, Size, Style and Color.
// Added options:
//  toTransparent - Values draw transparent 
//  toRotated - Values vertical (only TrueType or Adobe PS, not for .fon)
//  toNoZeros - do not display values = 0 when toValues in Options
// minor adjustments for Delphi 2005 compatibility. jbk 17.03.2005

unit JBKTrend;

interface

uses
  Windows, Messages, SysUtils, ExtCtrls, Graphics, Classes, Dialogs, Math;

type
  TTrendStyle = (tsBar, tsLine, tsScatter, ts3D, tsFilled);
  TTrendOption = (toValues, toTwin, toTransparent, toRotated, toNoZeros);
  TTrendOptions = set of TTrendOption;
  TGridStyle = (gsNone, gsXGrid, gsYGrid, gsFull);

  TJBKTrendAboutBox = class
  end;

  TJBKTrend = class(TPanel)
  private
    { Private declarations }
    FAbout: TJBKTrendAboutBox;
    Fvals: array[0..99] of Integer;
    FBaseVals: array[0..99] of Integer;
    FDivisions: Integer;
    FMinVal: Integer;
    FMaxVal: Integer;
    FStyle: TTrendStyle;
    FOptions: TTrendOptions;
    FColorBase: TColor;
    FBackGroundColor: TColor; // jbk
    FGridStyle: TGridStyle; // jbk
    FGridColor: TColor; // jbk
    FGridX: Integer; // jbk
    FGridY: Integer; // jbk


    H_3d, W_3d: Integer;
    FAutoScale: Boolean; //jbk
    procedure SetStyle(Value: TTrendStyle);
    procedure SetMaxVal(Value: Integer);
    procedure SetMinVal(Value: Integer);
    procedure SetDivisions(Value: Integer);
    procedure SetOptions(Value: TTrendOptions);
    procedure SetColorBase(Value: TColor);
    procedure SetBackGroundColor(Value: TColor); // jbk
    procedure SetGridStyle(Value: TGridStyle);
    procedure SetGridColor(Value: TColor); // jbk
    procedure SetStepX(Value: Integer); // jbk
    procedure SetStepY(Value: Integer); // jbk
    procedure SetAutoScale(Value: Boolean); // jbk
    procedure SetAutoMax(Value: Integer); // jbk
    function Brightness(OriginalColor: TColor; Change: Integer): TColor;
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Demo;
    procedure Add(NewVal: Integer);
    procedure AddBase(NewVal: Integer);
    procedure Clear;
    procedure ClearBase;
    function Get(i: Integer): Integer;
    function GetBase(i: Integer): Integer;
  published
    { Published declarations }
        // Displays information about the product's version and authors.
    property About: TJBKTrendAboutBox read FAbout write FAbout;
    property MinVal: Integer read FMinVal write SetMinVal default 0; // jbk
    property MaxVal: Integer read FMaxVal write SetMaxVal default 100; // jbk
    property Divisions: Integer read FDivisions write SetDivisions default 20; // jbk
    property Options: TTrendOptions read FOptions write SetOptions;
    property Style: TTrendStyle read FStyle write SetStyle;
    property ColorBase: TColor read FColorBase write SetColorBase default clAqua; // jbk
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default
      clWhite; // jbk
    property GridStyle: TGridStyle read FGridStyle write SetGridStyle default gsFull;
    // jbk
    property GridColor: tColor read FGridColor write SetGridColor default clSilver; // jbk
    property GridXstep: Integer read FGridX write SetStepX default 10; // jbk
    property GridYstep: Integer read FGridY write SetStepY default 10; // jbk
    property AutoScale: Boolean read FAutoScale write SetAutoScale default False; // jbk
  end;

implementation

constructor TJBKTrend.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FMinVal := 0;
  FMaxVal := 100;
  FDivisions := 100;
  FAutoScale := False; // jbk
  FStyle := tsBar;
  Color := clRed; // jbk
  FColorBase := clAqua;
  FGridStyle := gsFull; // jbk
  FGridColor := clSilver; // jbk
  FGridX := 10; // jbk
  FGridY := 10; // jbk
  FBackGroundColor := clWhite; // jbk
  H_3d := 10;
  W_3d := 10;
  if csDesigning in ComponentState then // jbk
    for i := 0 to (FDivisions - 1) do
      begin
        Fvals[i] := Random(FMaxVal - FMinVal) + FMinVal;
        FBaseVals[i] := Random(Fvals[i] - FMinVal) + FMinVal;
      end
  else
    for i := 0 to (FDivisions - 1) do
      begin
        Fvals[i] := 0;
        FBaseVals[i] := 0;
      end;
  FDivisions := 20;
end;

destructor TJBKTrend.Destroy;
begin
  inherited Destroy;
end;

procedure TJBKTrend.SetStyle(Value: TTrendStyle);
begin
  if Value <> FStyle then
    begin
      FStyle := Value;
      Invalidate;
    end;
end;

procedure TJBKTrend.SetColorBase(Value: TColor);
begin
  if Value <> FColorBase then
    begin
      FColorBase := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetBackGroundColor
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: TColor
  Result:    None
  Purpose:   Set Color of Background
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetBackGroundColor(Value: TColor); // jbk
begin
  if Value <> FBackGroundColor then
    begin
      FBackGroundColor := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetGridStyle
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: TGridStyle
  Result:    None
  Purpose:   Set Gridstyle to Off, Full, only Y or only X
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetGridStyle(Value: TGridStyle); // jbk
begin
  if FGridStyle <> Value then
    begin
      FGridStyle := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetGridColor
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: TColor
  Result:    None
  Purpose:   Set Grid color
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetGridColor(Value: TColor); // jbk
begin
  if Value <> FGridColor then
    begin
      FGridColor := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetStepY
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Integer
  Result:    None
  Purpose:   Set distance of grid lines Y
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetStepY(Value: Integer); // jbk
begin
  if Value > 0 then
    begin
      if Value > Height then Value := Height;
      if Value <> FGridY then
        begin
          FGridY := Value;
          Invalidate
        end
    end
  else
    if csDesigning in ComponentState then
      MessageDlg('Value must be greater than 0!', mtError, [mbOK], 0);
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetStepX
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Integer
  Result:    None
  Purpose:   Set distance of grid lines X
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetStepX(Value: Integer); // jbk
begin
  if Value > 0 then
    begin
      if Value > Width then Value := Width;
      if Value <> FGridX then
        begin
          FGridX := Value;
          Invalidate
        end
    end
  else
    if csDesigning in ComponentState then
      MessageDlg('Value must be greater than 0!', mtError, [mbOK], 0);
end;

procedure TJBKTrend.SetOptions(Value: TTrendOptions);
begin
  if Value <> FOptions then
    begin
      FOptions := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetMinVal
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Integer
  Result:    None
  Purpose:   Set Min Y Value, Min < Max
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetMinVal(Value: Integer);
begin
  if Value >= FMaxVal then Value := FMaxVal - 1; // jbk
  if Value <> FMinVal then
    begin
      FMinVal := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetMaxVal
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Integer
  Result:    None
  Purpose:   set Max Y value, Max > Min
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetMaxVal(Value: Integer);
begin
  if Value <= FMinVal then Value := FMinVal + 1; // jbk
  if Value <> FMaxVal then
    begin
      FMaxVal := Value;
      Invalidate;
    end;
end;

procedure TJBKTrend.SetDivisions(Value: Integer);
begin
  if Value > 100 then Value := 100;
  if Value < 2 then Value := 2;
  if Value <> FDivisions then
    begin
      FDivisions := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetAutoScale
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Boolean
  Result:    None
  Purpose:   Activate/Deactivate Autoscaling
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetAutoScale(Value: Boolean); // jbk
begin
  if Value <> FAutoScale then
    begin
      FAutoScale := Value;
      Invalidate;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.SetAutoMax
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: Value: Integer
  Result:    None
  Purpose:   Find biggest Y in array and set Max to it
-----------------------------------------------------------------------------}

procedure TJBKTrend.SetAutoMax(Value: Integer); // jbk
var
  I, J: Integer;
begin
  I := MaxIntValue(Fvals);
  J := MaxIntValue(FBasevals);
  I := Max(I, J);
  I := Max(I, Value);
  if I <> FMaxVal then SetMaxVal(I);
end;

function TJBKTrend.Brightness(OriginalColor: TColor; Change: Integer): TColor;
var
  R, G, B: Integer; // Colour components
  nR, nG, nB: Integer; // New Colour components
begin
  R := OriginalColor and $FF;
  G := (OriginalColor and $FF00) shr 8;
  B := (OriginalColor and $FF0000) shr 16;
  nR := R + Change;
  nG := G + Change;
  nB := B + Change;
  // Limit Brightness change to prevent colour change
  if nR > $FF then nR := $FF;
  if nG > $FF then nG := $FF;
  if nB > $FF then nB := $FF;
  if nR < 0 then nR := 0;
  if nG < 0 then nG := 0;
  if nB < 0 then nB := 0;
  Brightness := (nB shl 16) + (nG shl 8) + nR;
end;

procedure TJBKTrend.Demo;
var
  i: Integer;
begin
  // Shift old data left
  for i := 0 to (FDivisions - 2) do
    begin
      Fvals[i] := Fvals[i + 1];
      FBaseVals[i] := FBaseVals[i + 1];
    end;

  // Create new data item
  Fvals[FDivisions - 1] := Random(FMaxVal - FMinVal) + FMinVal;
  FBaseVals[FDivisions - 1] := Random(FMaxVal - FMinVal) + FMinVal;

  // Redraw
  Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.Add
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: NewVal: Integer
  Result:    None
  Purpose:   Add new value to array, scale if autoscale is on
-----------------------------------------------------------------------------}

procedure TJBKTrend.Add(NewVal: Integer);
var
  i: Integer;
begin
  // Shift old data left
  for i := 0 to (FDivisions - 2) do
    begin
      Fvals[i] := Fvals[i + 1];
    end;

  if (NewVal < FMinVal) then NewVal := FMinVal;

  if AutoScale then
    SetAutoMax(NewVal)
  else
    if (NewVal > FMaxVal) then NewVal := FMaxVal;

  // Add new data item
  Fvals[FDivisions - 1] := NewVal;

  // Redraw
  Invalidate;
end;

{-----------------------------------------------------------------------------
  Procedure: TJBKTrend.AddBase
  Author:    Dr. Juergen Kehrel, Heidelberg
  Date:      08-Mai-2002
  Arguments: NewVal: Integer
  Result:    None
  Purpose:   Add new value to base array, scale if autoscale is on
-----------------------------------------------------------------------------}

procedure TJBKTrend.AddBase(NewVal: Integer);
var
  i: Integer;
begin
  // Shift old data left
  for i := 0 to (FDivisions - 2) do
    begin
      FBaseVals[i] := FBaseVals[i + 1];
    end;

  if (NewVal < FMinVal) then NewVal := FMinVal;

  if AutoScale then
    SetAutoMax(NewVal)
  else
    if (NewVal > FMaxVal) then NewVal := FMaxVal;

  // Add new data item
  FBaseVals[FDivisions - 1] := NewVal;

  // Redraw
  Invalidate;
end;

procedure TJBKTrend.Clear;
var
  i: Integer;
begin
  // Set to Zero
  for i := 0 to (FDivisions - 1) do
    Fvals[i] := 0;

  // Redraw
  Invalidate;
end;

procedure TJBKTrend.ClearBase;
var
  i: Integer;
begin
  // Set to Zero
  for i := 0 to (FDivisions - 1) do
    FBaseVals[i] := 0;

  // Redraw
  Invalidate;
end;

function TJBKTrend.Get(i: Integer): Integer;
begin
  if ((i >= 0) and (i < 100)) then
    Get := Fvals[i]
  else
    Get := 0;
end;

function TJBKTrend.GetBase(i: Integer): Integer;
begin
  if ((i >= 0) and (i < 100)) then
    GetBase := FBaseVals[i]
  else
    GetBase := 0;
end;

procedure TJBKTrend.Paint;
var
  BarWidth: Variant;
  BorderWidth: Integer;
  i, x, y, w, y1, y2, LastX, Val1: Integer;
  BT2: Double;
  J, K, MM, BT: Integer;
  R, L, B, T: Integer;
  TheImage: TBitmap;
  TR: TRect;
  BrushColor: TColor;
  NewDir: Integer; //BM
  tf: TFont; //BM
  lf : TLogFont; //BM

begin
  TheImage := TBitmap.Create;
  try
    begin
      TheImage.Height := Height;
      TheImage.Width := Width;
      TR := ClientRect;
      with TheImage.Canvas do
        begin
          //BM
          if toValues in Options then
          begin
            Font.Height := Self.Font.Size; //BM
            Font.Name := Self.Font.Name;//BM  'Arial'
            Font.Style := Self.Font.Style;//BM
            Font.Color := Self.Font.Color;//BM
            if toRotated in Options then
            begin
              NewDir := 900;
              tf := nil;
              try
                tf := TFont.Create;
                tf.Assign(Font) ;
                GetObject(tf.Handle, SizeOf(lf), @lf) ;
                lf.lfEscapement := NewDir;
                lf.lfOrientation := NewDir;
                tf.Handle := CreateFontIndirect(lf) ;
                Font.Assign(tf) ;
              finally
                if Assigned(tf) then
                  tf.Free;
              end;//BM(**)
            end;
          end;

          Brush.color := FBackGroundColor; // jbk
          FillRect(TR); // Draw background   jbk
          //jbk start
          Pen.Width := 1;
          with TR do
            begin
              B := Bottom;
              T := Top;
              L := Left;
              R := Right;
            end;
          MM := FMaxVal - FMinVal;
          BT2 := (B - T) / 100;
          // Draw the grid
          if (FGridStyle <> gsNone) then
            begin
              Pen.Color := FGridColor;
              if (FGridStyle = gsXGrid) or (FGridStyle = gsFull) then
                for J := L + 1 to R - 1 do
                  if (J mod FGridX) = 0 then
                    PolyLine([Point(R - J, T), Point(R - J, B - 1)]);
              if (FGridStyle = gsYGrid) or (FGridStyle = gsFull) then
                for J := FMinVal + 1 to FMaxVal - 1 do
                  if (J mod FGridY) = 0 then
                    begin
                      K := Round((BT2 * (((100 * (J - FMinVal)) div MM))));
                      PolyLine([Point(L, B - K), Point(R - 1, B - K)]);
                    end;
            end;
          // jbk end
          BorderWidth := 3;
          BarWidth := (Width - 2 * BorderWidth) / FDivisions;
          Brush.Assign(self.Brush);
          case FStyle of
            tsScatter:
              begin
                Pen.Color := Brush.Color;
                for i := 0 to (FDivisions - 1) do
                  begin
                    Pen.Color := Color;
                    Brush.Color := Color;
                    w := BarWidth / 4;
                    if w < 1 then w := 1;
                    x := BorderWidth + (i * BarWidth) + (2 * w);
                    y := ((Fvals[i] - FMinVal) * (Height - 2 * BorderWidth - (2 * w)) div
                      (FMaxVal - FMinVal))
                      + BorderWidth + w;
                    Rectangle(x - w,
                      Height - (y - w),
                      x + w,
                      Height - (y + w));

                    if (toTwin in FOptions) then
                      begin
                        Pen.Color := ColorBase;
                        Brush.Color := ColorBase;
                        y := ((FBaseVals[i] - FMinVal) * (Height - 2 * BorderWidth - (2 *
                          w)) div (FMaxVal - FMinVal))
                          + BorderWidth + w;
                        Rectangle(x - w,
                          Height - (y - w),
                          x + w,
                          Height - (y + w));
                      end;
                  end;
              end;

            tsLine:
              begin
                Pen.Color := Color;
                moveto(BorderWidth + (BarWidth div 2),
                  Height - (((Fvals[0] - FMinVal) * (Height - 2 * BorderWidth) div (FMaxVal
                  - FMinVal))
                  + BorderWidth));
                for i := 1 to (FDivisions - 1) do
                  begin
                    lineto(BorderWidth + (i * BarWidth) + (BarWidth div 2),
                      Height - (((Fvals[i] - FMinVal) * (Height - 2 * BorderWidth) div
                      (FMaxVal - FMinVal))
                      + BorderWidth));
                  end;

                if (toTwin in FOptions) then
                  begin
                    Pen.Color := ColorBase;
                    Brush.Color := ColorBase;
                    moveto(BorderWidth + (BarWidth div 2),
                      Height - (((FBaseVals[0] - FMinVal) * (Height - 2 * BorderWidth) div
                      (FMaxVal - FMinVal))
                      + BorderWidth));
                    for i := 1 to (FDivisions - 1) do
                      begin
                        lineto(BorderWidth + (i * BarWidth) + (BarWidth div 2),
                          Height - (((FBaseVals[i] - FMinVal) * (Height - 2 * BorderWidth)
                          div (FMaxVal - FMinVal))
                          + BorderWidth));
                      end;
                  end;
              end;

            tsFilled:
              begin
                Pen.Color := Brush.Color;
                for i := 0 to (FDivisions - 2) do
                  begin
                    x := BorderWidth + (i * BarWidth) + (BarWidth / 2);
                    y1 := ((Fvals[i] - FMinVal) * (Height - 2 * BorderWidth) div (FMaxVal
                      - FMinVal)) + BorderWidth;
                    y2 := ((Fvals[i + 1] - FMinVal) * (Height - 2 * BorderWidth) div
                      (FMaxVal - FMinVal)) + BorderWidth;
                    Polygon([Point(x, Height - BorderWidth),
                      Point(x + BarWidth, Height - BorderWidth),
                        Point(x + BarWidth, Height - y2),
                        Point(x, Height - y1)]);
                  end;
              end;

            ts3D:
              begin
                BarWidth := (Width - 2 * BorderWidth - W_3d) / FDivisions;
                BrushColor := Brush.Color;
                x := BorderWidth;
                for i := 0 to (FDivisions - 1) do
                  begin
                    LastX := x;
                    x := BorderWidth + ((i + 1) * BarWidth);
                    y := ((Fvals[i] - FMinVal) * (Height - 2 * BorderWidth - H_3d) div
                      (FMaxVal - FMinVal))
                      + BorderWidth;
                    if (toTwin in FOptions) then
                      y1 := ((FBaseVals[i] - FMinVal) * (Height - 2 * BorderWidth - H_3d)
                        div (FMaxVal - FMinVal))
                        + BorderWidth
                    else
                      y1 := BorderWidth;

                    // Make sure y is the smallest
                    if y < y1 then
                      begin
                        y2 := y;
                        y := y1;
                        y1 := y2;
                      end;
                    Brush.Color := BrushColor;
                    Pen.Color := Brush.Color;
                    Polygon([Point(LastX, Height - y1),
                      Point(x, Height - y1),
                        Point(x, Height - y),
                        Point(LastX, Height - y)]);
                    // Draw side perspective extension in darker colour
                    Brush.Color := Brightness(BrushColor, -60);
                    Pen.Color := Brush.Color;
                    Polygon([Point(x, Height - y1),
                      Point(x + W_3d, Height - (y1 + H_3d)),
                        Point(x + W_3d, Height - (y + H_3d)),
                        Point(x, Height - y)]);
                    // Draw top perspective extension in lighter colour
                    Brush.Color := Brightness(BrushColor, 60);
                    Pen.Color := Brush.Color;
                    Polygon([Point(x, Height - y),
                      Point(x + W_3d, Height - (y + H_3d)),
                        Point(LastX + W_3d, Height - (y + H_3d)),
                        Point(LastX, Height - y)]);
                    if (toValues in FOptions) then
                      begin
                        Brush.Color := FBackGroundColor; // jbk
                        if toTransparent in Options then
                          Brush.Style := bsClear; //BM
                        if (toTwin in FOptions) then
                        begin
                          //BM
                          Val1 := Fvals[i] - FBaseVals[i];
                          if (Val1 <> 0) or
                            (Val1 = 0) and not (toNoZeros in Options) then
                            TextOut(LastX + (BarWidth div 2) -
                              (TextWidth(IntToStr(abs(Fvals[i] - FBaseVals[i]))) div 2),
                              Height - y - (Font.Height), IntToStr(abs(Val1)))
                        end
                        else
                          //BM
                          Val1 := Fvals[i];
                          if (Val1 <> 0) or
                            (Val1 = 0) and not (toNoZeros in Options) then
                            if not (toRotated in Options) then
                              TextOut(LastX + (BarWidth div 2) - (TextWidth(IntToStr(Val1))
                                div 2), Height - y - (Font.Height), IntToStr(Val1))
                            else
                              TextOut(LastX + (BarWidth div 2) - Font.Height div 2,
                                Max(TextWidth(IntToStr(Val1)), Height - y - TextWidth(IntToStr(Val1))),
                                IntToStr(Val1));                                
                      end;
                  end;
              end;

            tsBar:
              begin
                BrushColor := Brush.Color;
                for i := 0 to (FDivisions - 1) do
                  begin
                    x := BorderWidth + (i * BarWidth);
                    y := Round(((Fvals[i] - FMinVal) * (Height - 2 * BorderWidth) /
                        (FMaxVal - FMinVal)) + BorderWidth);
                    Brush.Color := BrushColor;
                    rectangle(x,
                      Height - BorderWidth,
                      x + BarWidth,
                      Height - y);
                    if (toValues in FOptions) then
                      begin
                        //BM
                        Val1 := Fvals[i];
                        if (Val1 <> 0) or
                          (Val1 = 0) and not (toNoZeros in Options) then
                        begin
                          Brush.Color := FBackGroundColor; // jbk
                          if toTransparent in Options then
                            Brush.Style := bsClear; //BM
                            if not (toRotated in Options) then
                              TextOut(X + (BarWidth div 2) - (TextWidth(IntToStr(Val1))
                                div 2), Max(0, Height - y - (Font.Height)), //BM Max
                                IntToStr(Val1))
                            else
                              TextOut(X + (BarWidth div 2) - Font.Height div 2,
                                Max(TextWidth(IntToStr(Val1)), Height - y),
                                IntToStr(Val1));
                        end;
                      end;
                  end;
              end; // of case FStyle
          end;
        end;

      // Now copy onto screen canvas
      Canvas.CopyMode := cmSrcCopy;
      Canvas.CopyRect(ClientRect, TheImage.Canvas, ClientRect);
    end;
  finally
    TheImage.Destroy;
  end;
end;



end.

