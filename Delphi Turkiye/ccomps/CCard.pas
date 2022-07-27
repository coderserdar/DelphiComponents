unit CCard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TSekil = (Kupa, Maca, Sinek, Karo);
  TDurum = (Acik, Kapali);
  TCCard = class(TCustomControl)
  private
    { Private declarations }
    FSelectable: Boolean;
    FOnMouseUp : TMouseEvent;
    FOnClick   : TNotifyEvent;
    FSayi      : Byte;
    FSekil     : TSekil;
    FDurum     : TDurum;
    FBColor    : TColor;
    FPenWidth  : Byte;
    FBackColor : TColor;
    FSelected  : Boolean;
//    FBmps      : Array[1..28] of TBitmap;
  protected
    { Protected declarations }
    procedure SetSayi      (Value : Byte);
    procedure SetSekil     (Value : TSekil);
    procedure SetDurum     (Value : TDurum);
    procedure SetBColor    (Value : TColor);
    procedure SetPenWidth  (Value : Byte);
    procedure SetBackColor (Value : TColor);
    procedure SetSelected  (Value : Boolean);
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure Click; dynamic;
    procedure DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
    constructor Create (AOwner : TComponent);override;
    destructor Destroy;override;
    procedure Paint; override;
  published
    { Published declarations }
    property Sayi        : Byte         read FSayi       write SetSayi;
    property Sekil       : TSekil       read FSekil      write SetSekil;
    property Durum       : TDurum       read FDurum      write SetDurum;
    property BorderColor : TColor       read FBColor     write SetBColor;
    property PenWidth    : Byte         read FPenWidth   write SetPenWidth;
    property BackColor   : TColor       read FBackColor  write SetBackColor;
    property Selected    : Boolean      read FSelected   write SetSelected;
    property Selectable  : Boolean      read FSelectable write FSelectable;
    property OnClick     : TNotifyEvent read FOnClick    write FOnClick;
    property OnMouseUp   : TMouseEvent  read FOnMouseUp  write FOnMouseUp;
  end;

procedure Register;

implementation
{$R CCard.Res}

var
  Bmps  : Array[1..28] of TBitmap;

const
  BN      : Array[1..28] of String=
          ('KUPA','MACA','SINEK','KARO',
           'KKUPA','KMACA','KSINEK','KKARO',
           'RKUPA','RMACA','RSINEK','RKARO',
           'KUPABACAK','MACABACAK','SINEKBACAK','KAROBACAK',
           'KUPAKIZ','MACAKIZ','SINEKKIZ','KAROKIZ',
           'KUPAPAPAZ','MACAPAPAZ','SINEKPAPAZ','KAROPAPAZ',
           'KUPATERS','MACATERS','SINEKTERS','KAROTERS');

  Sayilar : Array[1..13] of String=
          ('A','2','3','4','5','6','7','8','9','10','J','Q','K');
  Clrs    : Array[1..4] of TColor=
          (clRed,clBlack,clBlack,clRed);

constructor TCCard.Create (AOwner : TComponent);
var
   I        : Byte;
   t:tbitmap;
begin
  inherited;
  Width       := 71;
  Height      := 96;
  Sayi        := 1;
  PenWidth    := 2;
  BorderColor := clWhite;
  BackColor   := $111111;
  FSelected   := False;
{  for i:=1 to 28 do
      begin
        FBmps[i] := TBitmap.Create;
//        FBmps[i].LoadFromResourceName(HInstance,BN[i]);
        FBmps[i].Handle := LoadBitmap(HInstance,PChar(BN[i]));
      end;}
end;

destructor TCCard.Destroy;
begin
  inherited;
end;

procedure TCCard.Paint;
var
   I      : Byte;
begin
  inherited;

  try
{    B1 .LoadFromResourceName(HInstance,BN[Ord(Sekil)+ 1]);
    Bmps[Ord(Sekil)+ 5] .LoadFromResourceName(HInstance,BN[Ord(Sekil)+ 5]);
    Bmps[Ord(Sekil)+ 9] .LoadFromResourceName(HInstance,BN[Ord(Sekil)+ 9]);
    Bmps[Ord(Sekil)+ 13].LoadFromResourceName(HInstance,BN[Ord(Sekil)+13]);
    Bmps[Ord(Sekil)+ 17].LoadFromResourceName(HInstance,BN[Ord(Sekil)+17]);
    Bmps[Ord(Sekil)+ 21].LoadFromResourceName(HInstance,BN[Ord(Sekil)+21]);
    Bmps[Ord(Sekil)+ 25].LoadFromResourceName(HInstance,BN[Ord(Sekil)+25]);}
    if Height <> 96 then
       Height := 96;

    if Width <> 71 then
       Width := 71;


    Canvas.Pen.Color   := FBColor;
    Canvas.Pen.Width   := FPenWidth;
    Canvas.Font.Style  := Canvas.Font.Style + [fsBold];

    if Durum = Acik then
       begin
         Canvas.Brush.Color := clWhite;
         Canvas.Brush.Style := bsSolid;
         Canvas.RoundRect(0,0,Width,Height,8,8);

         Canvas.Brush.Color := Clrs[Ord(Sekil)+1];
         Canvas.Brush.Style := bsClear;
         Canvas.Font.Color  := Clrs[Ord(Sekil)+1];
         Canvas.Font.Name   := 'MS SANS SERIF';
         Canvas.Font.Size   := 11;

         if Sayi=10 then
            begin
              Canvas.Font.Size   := 10;
              Canvas.TextOut(1,2,'l');
              Canvas.TextOut(5,2,'0');
               Canvas.TextOut(65,77,'l');
              Canvas.TextOut(58,77,'0');
            end
         else
            begin
              Canvas.TextOut(1,2,Sayilar[Sayi]);
              Canvas.TextOut(60,77,Sayilar[Sayi]);
            end;


         Canvas.CopyMode := cmSrcAnd	;
         Canvas.Draw(-1,15,Bmps[Ord(Sekil)+ 5]);
         Canvas.Draw(56,66,Bmps[Ord(Sekil)+ 9]);

         case Sayi of
              1:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2),
                              (Height Div 2) - (Bmps[Ord(Sekil)+ 1].Height Div 2),
                              Bmps[Ord(Sekil)+ 1]);
                end;
               2:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2),
                              1,
                              Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 25].Width  Div 2),
                              64,
                              Bmps[Ord(Sekil)+ 25]);
                end;
              3:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2),
                              1,
                              Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2),
                              32,
                              Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 25].Width  Div 2),
                              64,
                              Bmps[Ord(Sekil)+ 25]);
                end;
              4:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
               5:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);

                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,32,Bmps[Ord(Sekil)+ 1]);
                end;
              6:begin
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,32,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,32,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
              7:begin
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,16,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,32,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,32,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
              8:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,22,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,22,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,43,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,43,Bmps[Ord(Sekil)+ 25]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
              9:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,22,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,22,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw((round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)+1)
                              ,32,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,43,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,43,Bmps[Ord(Sekil)+ 25]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
             10:begin
                  Canvas.CopyMode := cmSrcAnd;
                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,2,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,2,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw((round(Width / 1.95) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)+1)
                              ,13,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 1].Width  Div 4)
                              ,22,Bmps[Ord(Sekil)+ 1]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 1].Width  Div 2)
                              ,22,Bmps[Ord(Sekil)+ 1]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,43,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,43,Bmps[Ord(Sekil)+ 25]);

                  Canvas.Draw((round(Width / 1.95) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)+1)
                              ,53,Bmps[Ord(Sekil)+ 25]);

                  Canvas.Draw(round(Width / 4.9) - (Bmps[Ord(Sekil)+ 25].Width  Div 4)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                  Canvas.Draw(round(Width / 1.4) - (Bmps[Ord(Sekil)+ 25].Width  Div 2)
                              ,64,Bmps[Ord(Sekil)+ 25]);
                end;
             11:begin
                  Canvas.Draw(10,11,Bmps[Ord(Sekil)+ 13]);
                end;
             12:begin
                  Canvas.Draw(10,11,Bmps[Ord(Sekil)+ 17]);
                end;
             13:begin
                  Canvas.Draw(10,11,Bmps[Ord(Sekil)+ 21]);
                end;
         end; // case
       end  // if Durum=Acik then
    else
        begin
          Canvas.Brush.Color := FBackColor;
          Canvas.Brush.Style := bsSolid;
          Canvas.RoundRect(0,0,Width,Height,10,10);

          Canvas.Brush.Color := clBlue;
          Canvas.Brush.Style := bsDiagCross;
          Canvas.RoundRect(0,0,Width,Height,10,10);
        end; // if Durum=Acik then ... else

    if FSelected then
       begin
         Canvas.Brush.Color := clGray;
         Canvas.Brush.Style := bsCross;
         Canvas.RoundRect(0,0,Width,Height,10,10);
       end;

  finally
  end;
end;

procedure TCCard.SetSayi (Value : Byte);
begin
  if (Sayi<0) or (Sayi>13) then
     begin
       FSayi := 1;
       Invalidate;
       exit;
     end;
  if Value <> FSayi then
     begin
       FSayi := Value;
       Invalidate;
     end;
end;

procedure TCCard.SetSekil (Value : TSekil);
begin
  if Value <> FSekil then
     begin
       FSekil := Value;
       Invalidate;
     end;
end;

procedure TCCard.SetDurum (Value : TDurum);
begin
  if Value <> FDurum then
     begin
       FDurum := Value;
       Invalidate;
     end;
end;

procedure TCCard.SetBColor (Value : TColor);
begin
  if Value <> FBColor then
     begin
       FBColor := Value;
       Invalidate;
     end;
end;

procedure TCCard.SetPenWidth (Value : Byte);
begin
  if Value <> FPenWidth then
     begin
       FPenWidth := Value;
       Invalidate;
     end;
end;

procedure TCCard.SetBackColor (Value : TColor);
begin
  if Value <> FBackColor then
     begin
       FBackColor := Value;
       Invalidate;
     end;
end;

procedure TCCard.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if csCaptureMouse in ControlStyle then MouseCapture := False;
  if csClicked in ControlState then
  begin
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
       Click;
  end;
  DoMouseUp(Message, mbLeft);
end;

procedure TCCard.DoMouseUp(var Message: TWMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TCCard.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCCard.Click;
var
   P : TPoint;
begin
  if Selectable then
     Selected := Not(Selected);

  ShowCursor(False);
  GetCursorPos(P);
  SetCursorPos(Left + 50,Top + Height + 5);

  if Assigned(FOnClick) then FOnClick(Self);

  SetCursorPos(P.x,P.y);
  ShowCursor(True);
end;

procedure TCCard.SetSelected  (Value : Boolean);
begin
  if (Value <> FSelected) and (FSelectable) then
     begin
       FSelected := Value;
       Invalidate;
     end;
end;

procedure Register;
begin
  RegisterComponents('Cc', [TCCard]);
end;

var
   IJK : Byte;

initialization

begin
  for IJK := 1 to 28 do
      begin
        Bmps[IJK] := TBitmap.Create;
        Bmps[IJK].LoadFromResourceName(HInstance,BN[IJK]);
      end;
end;

finalization

begin
  for IJK := 1 to 28 do
      begin
        Bmps[IJK].Free;
      end;
end;

end.



