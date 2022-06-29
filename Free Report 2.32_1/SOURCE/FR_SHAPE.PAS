
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Checkbox Add-In Object          }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_Shape;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, FR_Class, StdCtrls,
  Controls, ExtCtrls, Menus;


const
  skRectangle = 0;
  skRoundRectangle = 1;
  skEllipse = 2;
  skTriangle = 3;

type
  TfrShapeObject = class(TComponent)  // fake component
  end;

  TfrShapeView = class(TfrView)
  private
    procedure DrawShape;
  public
    ShapeType: Byte;
    constructor Create; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
  end;

  TfrShapeForm = class(TfrObjEditorForm)
    GroupBox1: TGroupBox;
    CB1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

uses FR_Utils, FR_Const;

{$R *.DFM}

var
  ShapeForm: TfrShapeForm;

constructor TfrShapeView.Create;
begin
  inherited Create;
  Typ := gtAddIn;
  ShapeType := skRectangle;
  BaseName := 'Shape';
end;

procedure TfrShapeView.Assign(From: TfrView);
begin
  inherited Assign(From);
  ShapeType := TfrShapeView(From).ShapeType;
end;

procedure TfrShapeView.DrawShape;
var
  x1, y1, min: Integer;
begin
  x1 := Round((SaveX + SaveDX) * ScaleX + OffsX);
  y1 := Round((SaveY + SaveDY) * ScaleY + OffsY);
  min := dx;
  if dy < dx then
    min := dy;
  with Canvas do
  begin
    Pen.Width := Round(FrameWidth);
    Pen.Color := FrameColor;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Brush.Color := FillColor;
    case ShapeType of
      skRectangle:
        Rectangle(x, y, x1 + 1, y1 + 1);
      skRoundRectangle:
        RoundRect(x, y, x1 + 1, y1 + 1, min div 4, min div 4);
      skEllipse:
        Ellipse(x, y, x1 + 1, y1 + 1);
      skTriangle:
        begin
          MoveTo(x1, y1);
          LineTo(x, y1);
          LineTo(x + (x1 - x) div 2, y);
          LineTo(x1, y1);
          FloodFill(x + (x1 - x) div 2, y + (y1 - y) div 2, clNone, fsSurface);
        end;
    end;
  end;
end;

procedure TfrShapeView.Draw(Canvas: TCanvas);
var
  FillC: Integer;
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CalcGaps;
  FillC := FillColor;
  FillColor := clNone;
  FrameTyp := 0;
  ShowBackground;
  FillColor := FillC;
  DrawShape;
  RestoreCoord;
end;

procedure TfrShapeView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(ShapeType, 1);
end;

procedure TfrShapeView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(ShapeType, 1);
end;

procedure TfrShapeView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;


{------------------------------------------------------------------------}
procedure TfrShapeForm.ShowEditor(t: TfrView);
var
  i: Integer;
begin
  CB1.Items.Clear;
  for i := 0 to 3 do
    CB1.Items.Add(LoadStr(SShape1 + i));
  with TfrShapeView(t) do
  begin
    CB1.ItemIndex := ShapeType;
    if ShowModal = mrOk then
      ShapeType := CB1.ItemIndex;
  end;
end;

procedure TfrShapeForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(frRes + 620);
  GroupBox1.Caption := LoadStr(frRes + 621);
  Button1.Caption := LoadStr(SOk);
  Button2.Caption := LoadStr(SCancel);
end;

initialization
  ShapeForm := TfrShapeForm.Create(nil);
  frRegisterObject(TfrShapeView, ShapeForm.Image1.Picture.Bitmap,
    LoadStr(SInsShape), ShapeForm);

finalization
  ShapeForm.Free;
  ShapeForm := nil;

end.

