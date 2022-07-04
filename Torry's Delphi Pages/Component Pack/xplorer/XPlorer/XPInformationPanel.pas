unit XPInformationPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TXPInformationPanel = class(TPanel)
  private
   FHeader: TPanel;
   FHeaderCaption: TLabel;
   FIcon, FGradient, FFrame, FFrame1, FBackgroundImage: TImage;
   FHeaderEndColor, FBackground:TColor;
    { Private-Deklarationen }

   function GetIcon: TBitmap;
   procedure SetIcon(const Value:TBitmap);
   function GetHeaderVisibility: Boolean;
   procedure SetHeaderVisibility(const Value:Boolean);
   function GetHeaderCaption: String;
   procedure SetHeaderCaption(const Value:String);
   function GetHeaderColor: TColor;
   procedure SetHeaderColor(const Value:TColor);
   function GetHeaderEndColor: TColor;
   procedure SetHeaderEndColor(const Value:TColor);
   function GetHeaderFont: TFont;
   procedure SetHeaderFont(const Value:TFont);
   function GetHeaderHeight: Integer;
   procedure SetHeaderHeight(const Value:Integer);
   function GetBackgroundImage: TBitmap;
   procedure SetBackgroundImage(const Value:TBitmap);
   function GetBackTransparent: Boolean;
   procedure SetBackTransparent(const Value:Boolean);
   function GetBackCenter: Boolean;
   procedure SetBackCenter(const Value:Boolean);
   function GetBackStretch: Boolean;
   procedure SetBackStretch(const Value:Boolean);
   function GiveColor(BeginColor, EndColor:TColor; count, position:integer): TColor;
   procedure Paint; override;
   procedure RePaint; override;

  protected
    { Protected-Deklarationen }
  public
   constructor Create (AOwner: TComponent); override;
   destructor Destroy; override;
    { Public-Deklarationen }
  published
   property HeaderBitmap: TBitmap read GetIcon write SetIcon;
   property HeaderVisible: boolean read GetHeaderVisibility write SetHeaderVisibility;
   property HeaderCaption: string read GetHeaderCaption write SetHeaderCaption;
   property HeaderStartColor: TColor read GetHeaderColor write SetHeaderColor;
   property HeaderEndColor: TColor read GetHeaderEndColor write SetHeaderEndColor;
   property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
   property HeaderHeight: Integer read GetHeaderHeight write SetHeaderHeight;
   property HeaderBackgroundColor: TColor read FBackground write FBackground;
   property BackgroundBitmap: TBitmap read GetBackgroundImage write SetBackgroundImage;
   property BackgroundBitmapTransparent: Boolean read GetBackTransparent write SetBackTransparent;
   property BackgroundBitmapCenter: Boolean read GetBackCenter write SetBackCenter;
   property BackgroundBitmapStretch: Boolean read GetBackStretch write SetBackStretch;
  { Published-Deklarationen }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XPlorer', [TXPInformationPanel]);
end;



constructor TXPInformationPanel.Create (AOwner : TComponent);
begin
 inherited Create(AOwner);
 Align :=AlNone;
 BorderWidth :=0;
 Font.Name:='Tahoma';
 Color:=$00FAE1D7;
 BevelOuter:=BvNone;
 width:=223;
 height:=151;
 Font.Color:=ClWhite;

 FHeader:=TPanel.create(Self);
 with FHeader do
 begin
  Parent:=self;
  Visible:=true;
  BorderWidth:=0;
  BevelOuter:=BvNone;
  Color:=ClWhite;
  Alignment:=TaLeftJustify;
  Font.Color:=ClWhite;
  Font.Style:=[FsBold];
  Caption:='';
  Height:=24;
  SetBounds(0, 0, width, height);
  Align:=AlTop;
 end;

 FFrame:=TImage.Create(self);
 with FFrame do
 begin
  Parent:=FHeader;
  width:=3;
  Visible:=true;
  Transparent:=false;
  SetBounds(0, 0, 3, height);
  Align:=alLeft;
 end;

 FIcon:=TImage.create(Self);
 with FIcon do
 begin
  Parent:=FHeader;
  Autosize:=True;
  Visible:=true;
  Center:=true;
  Transparent:=true;
  SetBounds(0, 0, 16, height);
  Align:=AlLeft;
 end;

 FFrame1:=TImage.Create(self);
 with FFrame1 do
 begin
  Parent:=FHeader;
  width:=3;
  Visible:=true;
  Transparent:=false;
  SetBounds(FHeader.width, 0, 3, height);
  Align:=alRight;
 end;

 FGradient:=TImage.Create(self);
 with FGradient do
 begin
  Parent:=FHeader;
  width:=110;
  Visible:=true;
  Transparent:=false;
  SetBounds(0, 0, width, height);
  Align:=alRight;
 end;

 FHeaderCaption:=TLabel.create(self);
 with FHeaderCaption do
 begin
  Parent:=FHeader;
  Visible:=true;
  Caption:=' Header';
  Transparent:=True;
  SetBounds(FIcon.width, 5, width, height);
 end;

 FBackgroundImage:=TImage.Create(self);
 with FBackgroundImage do
 begin
  Parent:=self;
  Visible:=true;
  Transparent:=true;
  SetBounds(FHeader.Width, FHeader.Height, width, height);
  Align:=alClient;
 end;


 Font.Color:=$00C05C20;
 FHeaderCaption.Font.Color:=$00C05C20;
 HeaderEndColor:=$00FAD2C8;
 HeaderBackgroundColor:=$00EFA27B;
 FBackgroundImage.Transparent:=false;
end;



destructor TXPInformationPanel.Destroy;
begin

 FFrame.Free;
 FIcon.Free;
 FGradient.Free;
 FHeaderCaption.Free;
 FHeader.Free;
 FBackgroundImage.Free;

 inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////// Prozeduren und Funktionen der Komponente
///////////////////////////////////////////////////////////////////////////////////////////////////////


function TXPInformationPanel.GiveColor(BeginColor, EndColor:TColor; count, position:integer): TColor;
// gradient calculating routine taken from RX Library
var
  BeginRGBValue: array[0..2] of Byte; { Begin RGB values }
  RGBDifference: array[0..2] of Integer; { Difference between begin and end RGB values }
  I: Integer;
  R, G, B: Byte; { Color band Red, Green, Blue values }
begin
 if position = count then
 begin
  Result := EndColor; // protects last block of progress bar from being draw in wrong color
  Exit;
 end;
 BeginColor := ColorToRGB(BeginColor);
 EndColor := ColorToRGB(EndColor);
 { Set the Red, Green and Blue colors }
 BeginRGBValue[0] := GetRValue(BeginColor);
 BeginRGBValue[1] := GetGValue(BeginColor);
 BeginRGBValue[2] := GetBValue(BeginColor);
 { Calculate the difference between begin and end RGB values }
 RGBDifference[0] := GetRValue(EndColor) - BeginRGBValue[0];
 RGBDifference[1] := GetGValue(EndColor) - BeginRGBValue[1];
 RGBDifference[2] := GetBValue(EndColor) - BeginRGBValue[2];
 for I := 0 to position do
 begin
 { Calculate the color band's color }
  R := BeginRGBValue[0] + MulDiv(I, RGBDifference[0], count - 1);
  G := BeginRGBValue[1] + MulDiv(I, RGBDifference[1], count - 1);
  B := BeginRGBValue[2] + MulDiv(I, RGBDifference[2], count - 1);
 end;
 Result := RGB(R, G, B);
end;



procedure TXPInformationPanel.Paint;
var i, j: integer;
begin
 for i := 0 to FGradient.Width do
 begin
  with FGradient.Canvas do
  begin
   Pen.Color:=GiveColor(FHeader.color, FHeaderEndColor, FGradient.width, i);
   pen.Width:=1;
   moveto(i, 0);
   lineto(i, FGradient.height);
  end;
 end;

 with FFrame.Canvas do
 begin
  for i:=0 to FFrame.width do for j:=0 to FFrame.Height do Pixels[i,j]:=FHeader.Color;

  Pixels[0,0]:=FBackground;
  Pixels[1,0]:=FBackground;
  Pixels[2,0]:=FBackground;
  Pixels[0,1]:=FBackground;
  Pixels[0,2]:=FBackground;
 end;

 with FFrame1.Canvas do
 begin
  for i:=0 to FFrame.width do for j:=0 to FFrame.Height do Pixels[i,j]:=FHeaderEndColor;

  Pixels[0,0]:=FBackground;
  Pixels[1,0]:=FBackground;
  Pixels[2,0]:=FBackground;
  Pixels[2,1]:=FBackground;
  Pixels[2,2]:=FBackground;
 end;

end;


procedure TXPInformationPanel.RePaint;
var i, j: integer;
begin
 for i := 0 to FGradient.Width do
 begin
  with FGradient.Canvas do
  begin
   Pen.Color:=GiveColor(FHeader.color, FHeaderEndColor, FGradient.width, i);
   pen.Width:=1;
   moveto(i, 0);
   lineto(i, FGradient.height);
  end;
 end;

 with FFrame.Canvas do
 begin
  for i:=0 to FFrame.width do for j:=0 to FFrame.Height do Pixels[i,j]:=FHeader.Color;

  Pixels[0,0]:=FBackground;
  Pixels[1,0]:=FBackground;
  Pixels[2,0]:=FBackground;
  Pixels[0,1]:=FBackground;
  Pixels[0,2]:=FBackground;
 end;

 with FFrame1.Canvas do
 begin
  for i:=0 to FFrame.width do for j:=0 to FFrame.Height do Pixels[i,j]:=FHeaderEndColor;

  Pixels[0,0]:=FBackground;
  Pixels[1,0]:=FBackground;
  Pixels[2,0]:=FBackground;
  Pixels[2,1]:=FBackground;
  Pixels[2,2]:=FBackground;
 end;

end;


function TXPInformationPanel.GetIcon: TBitmap;
begin
 result:=FIcon.Picture.Bitmap;
end;



procedure TXPInformationPanel.SetIcon(const Value:TBitmap);
begin
 FIcon.Picture.Bitmap:=Value;
 FHeaderCaption.Left:=FIcon.Width;
end;



function TXPInformationPanel.GetHeaderVisibility: boolean;
begin
 result:=FHeader.Visible;
end;



procedure TXPInformationPanel.SetHeaderVisibility(const Value:boolean);
begin
 FHeader.Visible:=Value;
end;



function TXPInformationPanel.GetHeaderCaption: String;
var cap:String;
begin
 cap:=FHeaderCaption.Caption;
 delete(cap, 1, 1);
 result:=cap;
end;



procedure TXPInformationPanel.SetHeaderCaption(const Value:String);
begin
 FHeaderCaption.Caption:=' '+Value;
 FHeaderCaption.Left:=FIcon.Width;
end;



function TXPInformationPanel.GetHeaderColor: TColor;
begin
 result:=FHeader.Color;
end;



procedure TXPInformationPanel.SetHeaderColor(const Value:TColor);
begin
 FHeader.color:=Value;
end;



function TXPInformationPanel.GetHeaderEndColor: TColor;
begin
 result:=FHeaderEndColor;
end;



procedure TXPInformationPanel.SetHeaderEndColor(const Value:TColor);
begin
 FHeaderEndcolor:=Value;
end;



function TXPInformationPanel.GetHeaderFont: TFont;
begin
 result:= FHeaderCaption.Font;
end;



procedure TXPInformationPanel.SetHeaderFont(const Value:TFont);
begin
 FHeaderCaption.Font:= Value;
end;



function TXPInformationPanel.GetHeaderHeight: Integer;
begin
 result:= FHeader.Height;
end;



procedure TXPInformationPanel.SetHeaderHeight(const Value:Integer);
begin
 FHeader.Height:= Value;
 FHeaderCaption.Top:= Value div 4;
end;



function TXPInformationPanel.GetBackgroundImage: TBitmap;
begin
 result:= FBackgroundImage.Picture.Bitmap;
end;



procedure TXPInformationPanel.SetBackgroundImage(const Value:TBitmap);
begin
 FBackgroundImage.Picture.Bitmap:= Value
end;



function TXPInformationPanel.GetBackTransparent: Boolean;
begin
 result:= FBackgroundImage.Transparent;
end;



procedure TXPInformationPanel.SetBackTransparent(const Value:Boolean);
begin
 FBackgroundImage.Transparent:= Value;
end;



function TXPInformationPanel.GetBackCenter: Boolean;
begin
 result:= FBackgroundImage.Center;
end;



procedure TXPInformationPanel.SetBackCenter(const Value:Boolean);
begin
 FBackgroundImage.Center:= Value;
end;



function TXPInformationPanel.GetBackStretch: Boolean;
begin
 result:= FBackgroundImage.Stretch;
end;



procedure TXPInformationPanel.SetBackStretch(const Value:Boolean);
begin
 FBackgroundImage.Stretch:= Value;
end;


end.
