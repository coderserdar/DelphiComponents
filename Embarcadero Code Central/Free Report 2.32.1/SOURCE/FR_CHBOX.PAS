
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Checkbox Add-In Object          }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_ChBox;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Menus, FR_Class;


type
  TfrCheckBoxObject = class(TComponent)  // fake component
  end;

  TfrCheckBoxView = class(TfrView)
  private
    procedure DrawCheck(ARect: TRect; Checked: Boolean);
  public
    constructor Create; override;
    procedure Draw(Canvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure ExportData; override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
  end;


implementation

uses FR_Intrp, FR_Pars, FR_Utils, FR_Const;

{$R *.RES}

procedure TfrCheckBoxView.DrawCheck(ARect: TRect; Checked: Boolean);
  procedure Line(x, y, x1, y1: Integer);
  begin
    Canvas.MoveTo(x, y);
    Canvas.LineTo(x1, y1);
  end;
begin
  InflateRect(ARect, Round(-4 * ScaleX), Round(-4 * ScaleY));
  with Canvas, ARect do
  begin
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    if Checked then
    begin
      Pen.Color := clBlack;
      Pen.Width := Round(3 * ScaleX);
      Line(Left, Top, Right, Bottom);
      Line(Left, Bottom, Right, Top);
    end;
  end;
end;

constructor TfrCheckBoxView.Create;
begin
  inherited Create;
  Typ := gtAddIn;
  FrameWidth := 2;
  BaseName := 'Check';
end;

procedure TfrCheckBoxView.Draw(Canvas: TCanvas);
var
  Res: Boolean;
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CalcGaps;
  ShowBackground;
  Res := False;
  if (DocMode = dmPrinting) and (Memo1.Count > 0) and (Memo1[0] <> '') then
    Res := Memo1[0][1] <> '0';
  if DocMode = dmDesigning then
    Res := True;
  DrawCheck(DRect, Res);
  ShowFrame;
  RestoreCoord;
end;

procedure TfrCheckBoxView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    Memo1[0] := IntToStr(Trunc(frParser.Calc(Memo1[0])));
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrCheckBoxView.ExportData;
var
  s: String;
begin
  inherited;
  s := '';
  if (Memo.Count > 0) and (Memo[0] <> '') then
    if Memo[0][1] <> '0' then
      s := 'X';
  CurReport.InternalOnExportText(x, y, s, Self);
end;

procedure TfrCheckBoxView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;



var
  Bmp: TBitmap;

initialization
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(hInstance, 'FR_CHBOX');
  frRegisterObject(TfrCheckBoxView, Bmp, LoadStr(SInsCheckBox), nil);

finalization
  Bmp.Free;

end.
