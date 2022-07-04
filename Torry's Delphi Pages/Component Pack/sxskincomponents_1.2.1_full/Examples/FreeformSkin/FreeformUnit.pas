unit FreeformUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SXSkinForm, SXSkinLibrary, SXSkinControl, SXSkinImage, SXSkinButton,
  SXSkinEdit;

type
  TForm1 = class(TForm)
    SXSkinLibrary1: TSXSkinLibrary;
    SXSkinImage1: TSXSkinImage;
    SXSkinForm1: TSXSkinForm;
    SXSkinButton1: TSXSkinButton;
    SXSkinButton2: TSXSkinButton;
    SXSkinButton3: TSXSkinButton;
    SXSkinButton4: TSXSkinButton;
    SXSkinButton5: TSXSkinButton;
    SXSkinButton6: TSXSkinButton;
    SXSkinButton7: TSXSkinButton;
    SXSkinButton8: TSXSkinButton;
    SXSkinButton9: TSXSkinButton;
    SXSkinButton10: TSXSkinButton;
    SXSkinButton11: TSXSkinButton;
    SXSkinButton12: TSXSkinButton;
    SXSkinButton13: TSXSkinButton;
    SXSkinEdit1: TSXSkinEdit;
    SXStoredSkin1: TSXStoredSkin;
    procedure SXSkinButton2Click(Sender: TObject);
    procedure SXSkinButton1Click(Sender: TObject);
    procedure SXSkinImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SXSkinImage1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SXSkinImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DraggingForm:Boolean;
    StartDragPT:TPoint;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 SXSkinLibrary1.StoredSkin:=GetStoredSkinByIndex(0);
// SXSkinLibrary1.SkinDir:=ExtractFilePath(Application.ExeName)+'\Skin';
 SXSkinLibrary1.Active:=True;
end;

procedure TForm1.SXSkinButton1Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.SXSkinButton2Click(Sender: TObject);
var S:String;
begin
 S:='';
 if Sender=SXSkinButton2 then S:='1' else
  if Sender=SXSkinButton3 then S:='2' else
  if Sender=SXSkinButton4 then S:='3' else
  if Sender=SXSkinButton5 then S:='4' else
  if Sender=SXSkinButton6 then S:='5' else
  if Sender=SXSkinButton7 then S:='6' else
  if Sender=SXSkinButton8 then S:='7' else
  if Sender=SXSkinButton9 then S:='8' else
  if Sender=SXSkinButton10 then S:='9' else
  if Sender=SXSkinButton11 then S:='*' else
  if Sender=SXSkinButton12 then S:='0' else
  if Sender=SXSkinButton13 then S:='#';
 SXSkinEdit1.Text:=SXSkinEdit1.Text+S;
end;

procedure TForm1.SXSkinImage1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button=mbLeft then
  begin
   DraggingForm:=True;
   StartDragPT:=Mouse.CursorPos;
   Dec(StartDragPT.X,Left);
   Dec(StartDragPT.Y,Top);
  end;
end;

procedure TForm1.SXSkinImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var PT:TPoint;
begin
 if DraggingForm then
  begin
   PT:=Mouse.CursorPos;
   SetWindowPos(Handle,0,PT.X-StartDragPT.X,PT.Y-StartDragPT.Y,0,0,SWP_NOSIZE or SWP_NOZORDER);
  end;
end;

procedure TForm1.SXSkinImage1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button=mbLeft then
  DraggingForm:=False;
end;

end.
