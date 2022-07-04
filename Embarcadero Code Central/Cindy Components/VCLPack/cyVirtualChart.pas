unit cyVirtualChart;
{   Component(s):
    tcyVirtualChart

    Description:
    Component that help you generate graph (for stats etc ...) on any canvas

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

interface

uses Windows, Classes, SysUtils, Graphics, stdCtrls, VCL.cyGraphics;

type
  TcyVirtualChart = class(TComponent)
  private
    FCanvas_RightMargin: word;
    FCanvas_Height: word;
    FCanvas_BottomMargin: word;
    FCanvas_Width: word;
    FCanvas_TopMargin: word;
    FCanvas_LeftMargin: word;
    FXAxisMax: Extended;
    FYAxisMax: Extended;
    FXAxisMin: Extended;
    FYAxisMin: Extended;
    FChartHeight: Integer;
    FChartWidth: Integer;
    FChartRect: TRect;
    procedure SetCanvas_BottomMargin(const Value: word);
    procedure SetCanvas_Height(const Value: word);
    procedure SetCanvas_LeftMargin(const Value: word);
    procedure SetCanvas_RightMargin(const Value: word);
    procedure SetCanvas_TopMargin(const Value: word);
    procedure SetCanvas_Width(const Value: word);
  protected
    procedure UpdateChartDimensions;
  public
    constructor Create(AOwner: TComponent); override;

    function ChartValueToPixelPoint(const XAxisValue, YAxisValue: Extended): TPoint;
    function PixelValueToXAxisValue(const PixelValue: Integer): Extended;
    function PixelValueToYAxisValue(const PixelValue: Integer): Extended;

    procedure Clear(Canvas: TCanvas; const BackGroundColor: TColor);
    procedure DrawHorizontalGuideLine(Canvas: TCanvas; const YAxisValue: Extended; const OffsetPixelsLeft, OffsetPixelsRight: Integer);
    procedure DrawVerticalGuideLine(Canvas: TCanvas; const XAxisValue: Extended; const OffsetPixelsTop, OffsetPixelsBottom: Integer);
    procedure DrawLine(Canvas: TCanvas; const XAxisStart, YAxisStart, XAxisEnd, YAxisEnd: Extended;
                const IndentPixelsXStart: Integer = 0; const IndentPixelsYStart: Integer = 0; const IndentPixelsXEnd: Integer = 0; const IndentPixelsYEnd: Integer = 0);
    procedure DrawText(Canvas: TCanvas; const Text: String; const XAxisX, XAxisY: Extended; const HorizontalPositionFromXAxisX, TextAlignment: TAlignment;
                const VerticalPositionFromXAxisY: TTextLayout = tlCenter; const IndentPixelsX: Integer = 0; const IndentPixelsY: Integer = 0);
    property ChartRect: TRect read FChartRect;
    property ChartWidth: Integer read FChartWidth;
    property ChartHeight: Integer read FChartHeight;
  published
    property Canvas_Width: word read FCanvas_Width write SetCanvas_Width default 100;
    property Canvas_Height: word read FCanvas_Height write SetCanvas_Height default 50;
    property Canvas_LeftMargin: word read FCanvas_LeftMargin write SetCanvas_LeftMargin default 0;
    property Canvas_TopMargin: word read FCanvas_TopMargin write SetCanvas_TopMargin default 0;
    property Canvas_RightMargin: word read FCanvas_RightMargin write SetCanvas_RightMargin default 0;
    property Canvas_BottomMargin: word read FCanvas_BottomMargin write SetCanvas_BottomMargin default 0;

    property XAxisMin: Extended read FXAxisMin write FXAxisMin;
    property XAxisMax: Extended read FXAxisMax write FXAxisMax;
    property YAxisMin: Extended read FYAxisMin write FYAxisMin;
    property YAxisMax: Extended read FYAxisMax write FYAxisMax;
  end;

implementation

{ TcyVirtualChart }

constructor TcyVirtualChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas_Width := 100;
  FCanvas_Height := 50;
  FCanvas_LeftMargin := 0;
  FCanvas_TopMargin := 0;
  FCanvas_RightMargin := 0;
  FCanvas_BottomMargin := 0;

  FXAxisMin := 0;
  FXAxisMax := 100;
  FYAxisMin := 0;
  FYAxisMax := 100;
end;

function TcyVirtualChart.ChartValueToPixelPoint(const XAxisValue, YAxisValue: Extended): TPoint;
begin
  Result.X := Round( FChartRect.Left + (FChartWidth / (FXAxisMax - FXAxisMin)) * (XAxisValue - FXAxisMin) );
  Result.Y := Round( FChartRect.Bottom - (FChartHeight / FYAxisMax - FYAxisMin) * YAxisValue );
end;

function TcyVirtualChart.PixelValueToXAxisValue(const PixelValue: Integer): Extended;
begin
  Result := FXAxisMin + (FXAxisMax - FXAxisMin) / (FChartRect.Right - FChartRect.Left) * PixelValue;
end;

function TcyVirtualChart.PixelValueToYAxisValue(const PixelValue: Integer): Extended;
begin
  Result := FYAxisMin + (FYAxisMax - FYAxisMin) / (FChartRect.Bottom - FChartRect.Top) * PixelValue;
end;

procedure TcyVirtualChart.UpdateChartDimensions;
begin
  // Initialize variables :
  FChartRect := classes.Rect(FCanvas_LeftMargin, FCanvas_TopMargin, FCanvas_width - FCanvas_RightMargin, FCanvas_height - FCanvas_BottomMargin);
  FChartWidth := FChartRect.Right - FChartRect.Left;
  FChartHeight := FChartRect.Bottom - FChartRect.Top;
end;

procedure TcyVirtualChart.SetCanvas_Width(const Value: word);
begin
  if Value = 0
  then FCanvas_Width := 1
  else FCanvas_Width := Value;

  UpdateChartDimensions;
end;

procedure TcyVirtualChart.SetCanvas_Height(const Value: word);
begin
  if Value = 0
  then FCanvas_Height := 1
  else FCanvas_Height := Value;

  UpdateChartDimensions;
end;

procedure TcyVirtualChart.SetCanvas_LeftMargin(const Value: word);
begin
  FCanvas_LeftMargin := Value;
  UpdateChartDimensions;
end;

procedure TcyVirtualChart.SetCanvas_TopMargin(const Value: word);
begin
  FCanvas_TopMargin := Value;
  UpdateChartDimensions;
end;

procedure TcyVirtualChart.SetCanvas_RightMargin(const Value: word);
begin
  FCanvas_RightMargin := Value;
  UpdateChartDimensions;
end;

procedure TcyVirtualChart.SetCanvas_BottomMargin(const Value: word);
begin
  FCanvas_BottomMargin := Value;
  UpdateChartDimensions;
end;
procedure TcyVirtualChart.Clear(Canvas: TCanvas; const BackGroundColor: TColor);
begin
  // Clear bitmap :
  Canvas.Brush.Color := BackGroundColor;
  Canvas.FillRect( classes.Rect(0, 0, FCanvas_width + 1, FCanvas_height + 1) );
end;

procedure TcyVirtualChart.DrawHorizontalGuideLine(Canvas: TCanvas; const YAxisValue: Extended; const OffsetPixelsLeft, OffsetPixelsRight: Integer);
begin
  DrawLine(Canvas, XAxisMin, YAxisValue, XAxisMax, YAxisValue, OffsetPixelsLeft, 0, OffsetPixelsRight, 0);
end;

procedure TcyVirtualChart.DrawVerticalGuideLine(Canvas: TCanvas; const XAxisValue: Extended; const OffsetPixelsTop, OffsetPixelsBottom: Integer);
begin
  DrawLine(Canvas, XAxisValue, YAxisMax, XAxisValue, YAxisMin, 0, OffsetPixelsTop, 0, OffsetPixelsBottom);
end;

procedure TcyVirtualChart.DrawLine(Canvas: TCanvas; const XAxisStart, YAxisStart, XAxisEnd, YAxisEnd: Extended;
            const IndentPixelsXStart: Integer = 0; const IndentPixelsYStart: Integer = 0; const IndentPixelsXEnd: Integer = 0; const IndentPixelsYEnd: Integer = 0);
var
  pt1, pt2: TPoint;
begin
  pt1 := ChartValueToPixelPoint(XAxisStart, YAxisStart);
  Canvas.MoveTo(pt1.X + IndentPixelsXStart, pt1.Y + IndentPixelsYStart);

  pt2 := ChartValueToPixelPoint(XAxisEnd, YAxisEnd);
  Canvas.LineTo(pt2.X + IndentPixelsXEnd, pt2.Y + IndentPixelsYEnd);
end;

procedure TcyVirtualChart.DrawText(Canvas: TCanvas; const Text: String; const XAxisX, XAxisY: Extended; const HorizontalPositionFromXAxisX, TextAlignment: TAlignment;
            const VerticalPositionFromXAxisY: TTextLayout = tlCenter; const IndentPixelsX: Integer = 0; const IndentPixelsY: Integer = 0);
var
  pt: TPoint;
  CalcFlags: LongInt;
  RectLeft, RectTop, RectRight, RectBottom: Integer;
  ARect: TRect;
const
  TextFormat = 0;
begin
  // Pixel position:
  pt := ChartValueToPixelPoint(XAxisX, XAxisY);

  // Calculate RECT :
  CalcFlags := TextFormat or DT_CALCRECT;

  ARect := Rect(0, 0, Canvas_Width, Canvas_Height);
  {$IFDEF UNICODE}
  Windows.DrawText(Canvas.Handle, Text, -1, ARect, CalcFlags);
  {$ELSE}
  Windows.DrawText(Canvas.Handle, PChar(Text), -1, ARect, CalcFlags);
  {$ENDIF}

  case HorizontalPositionFromXAxisX of
    taLeftJustify:     // Text on left side of pt :
    begin
      RectLeft := pt.X - (ARect.Right - ARect.Left);
      RectRight := pt.X;
    end;

    taRightJustify:    // Text on right side of pt :
    begin
      RectLeft := pt.X;
      RectRight := pt.X + (ARect.Right - ARect.Left);
    end;

    taCenter:
    begin
      RectLeft := pt.X - (ARect.Right - ARect.Left) div 2;
      RectRight := RectLeft + (ARect.Right - ARect.Left);
    end;
  end;

  case VerticalPositionFromXAxisY of
    tlTop:
    begin
      RectTop := pt.Y;
      RectBottom := pt.Y + (ARect.Bottom - ARect.Top);
    end;

    tlBottom:
    begin
      RectTop := pt.Y - (ARect.Bottom - ARect.Top);
      RectBottom := pt.Y;
    end;

    tlCenter:
    begin
      RectTop := pt.Y - (ARect.Bottom - ARect.Top) div 2;
      RectBottom := RectTop + (ARect.Bottom - ARect.Top);
    end;
  end;

  ARect := Rect(RectLeft + IndentPixelsX, RectTop + IndentPixelsY, RectRight + IndentPixelsX, RectBottom + IndentPixelsY);

  cyDrawSingleLineText(Canvas, Text, ARect, TextAlignment, VerticalPositionFromXAxisY);
end;

end.
