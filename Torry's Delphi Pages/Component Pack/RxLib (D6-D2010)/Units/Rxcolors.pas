{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RXColors;

{$C PRELOAD}
{$I RX.INC}

interface

uses
  Classes, Controls, Graphics, Forms, rxVCLUtils;

function RxIdentToColor(const Ident: string; var Color: Longint): Boolean;
function RxColorToString(Color: TColor): string;
function RxStringToColor(S: string): TColor;
procedure RxGetColorValues(Proc: TGetStrProc);

procedure RegisterRxColors;

implementation

uses
  {$IFDEF RX_D5} Windows, {$ENDIF}
  {$IFDEF RX_D6} DesignIntf, VCLEditors, Types,
  {$ELSE} DsgnIntf, {$ENDIF} // Polaris
  SysUtils;

type
  TColorEntry = record
    Value: TColor;
    Name: PChar;
  end;

const
  clInfoBk16 = TColor($02E1FFFF);
  clNone16 = TColor($02FFFFFF);
  ColorCount = 3;
  Colors: array[0..ColorCount - 1] of TColorEntry = (
    (Value: clCream;       Name: 'clCream'),
    (Value: clMoneyGreen;  Name: 'clMoneyGreen'),
    (Value: clSkyBlue;     Name: 'clSkyBlue'));

function RxColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then
  begin
    for I := Low(Colors) to High(Colors) do
      if Colors[I].Value = Color then
      begin
        Result := StrPas(Colors[I].Name);
        Exit;
      end;
    FmtStr(Result, '$%.8x', [Color]);
  end;
end;

function RxIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
  Text: array[0..63] of Char;
begin
  StrPLCopy(Text, Ident, Length(Text) - 1);
  for I := Low(Colors) to High(Colors) do
    if StrIComp(Colors[I].Name, Text) = 0 then
    begin
      Color := Colors[I].Value;
      Result := True;
      Exit;
    end;
  Result := IdentToColor(Ident, Color);
end;

function RxStringToColor(S: string): TColor;
begin
  if not RxIdentToColor(S, Longint(Result)) then
    Result := StringToColor(S);
end;

procedure RxGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(Colors) to High(Colors) do
    Proc(StrPas(Colors[I].Name));
end;

{ TRxColorProperty }

type
  TRxColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
{$IFDEF RX_D5}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF RX_D6}override;{$ENDIF} // Polaris
{$ENDIF}
  end;

function TRxColorProperty.GetValue: string;
var
  Color: TColor;
begin
  Color := TColor(GetOrdValue);
{$IFDEF WIN32}
  if Color = clNone16 then
    Color := clNone
  else
    if Color = clInfoBk16 then
      Color := clInfoBk;
{$ENDIF}
  Result := RxColorToString(Color);
end;

procedure TRxColorProperty.GetValues(Proc: TGetStrProc);
begin
  RxGetColorValues(Proc);
end;

procedure TRxColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(RxStringToColor(Value));
end;

{$IFDEF RX_D5}
procedure TRxColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red, Green, Blue, Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else
      if ASelected then
        Result := clWhite
      else
        Result := AColor;
  end;

var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  try
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);
    Brush.Color := RxStringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  finally
    ACanvas.TextRect(Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), 
      vRight + 1, ARect.Top + 1, Value);
  end;
end;
{$ENDIF}

procedure RegisterRxColors;
begin
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TRxColorProperty);
end;

end.