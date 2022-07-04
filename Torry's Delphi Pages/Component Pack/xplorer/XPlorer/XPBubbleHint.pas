unit XPBubbleHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Bubblehint;

type
  TXPBubbleHint = class(TComponent)
  private
    { Private-Deklarationen }

   FCaption: string;
   FInfoText: string;
   FIList: TImageList;
   FImageIndex: Integer;
   FStretched: boolean;
   FCentered: boolean;
   FBitmap: TBitmap;
   FLeft: integer;
   FTop: integer;
   function GetLeft: Integer;
   procedure SetLeft(const Value:Integer);
   function GetTop: Integer;
   procedure SetTop(const Value:Integer);

  protected
    { Protected-Deklarationen }
  public
   constructor Create (AOwner: TComponent); override;
   destructor Destroy; override;
   function Execute: Boolean;
    { Public-Deklarationen }
  published
   property Title: string read FCaption write FCaption;
   property InformationText: string read FInfoText write FInfoText;
   property LogoImageList: TImagelist read FIList write FIList;
   property LogoImageIndex: Integer read FImageIndex write FImageIndex;
   property LogoImageStretched: boolean read FStretched write FStretched;
   property LogoImageCentered: boolean read FCentered write FCentered;
   property Left: Integer read GetLeft write SetLeft;
   property Top: Integer read GetTop write SetTop;
    { Published-Deklarationen }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XPlorer', [TXPBubbleHint]);
end;


constructor TXPBubbleHint.Create (AOwner : TComponent);
begin
 inherited Create(AOwner);

 Title:= 'Hinweis';
 InformationText:= '<Hinweistext>';
 LogoImageIndex:= -1;

end;



destructor TXPBubbleHint.Destroy;
begin

 inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////// Prozeduren und Funktionen der Komponente
///////////////////////////////////////////////////////////////////////////////////////////////////////


function TXPBubbleHint.GetLeft: Integer;
begin
 result:= FLeft+375;

end;



procedure TXPBubbleHint.SetLeft(const Value:Integer);
begin
 FLeft:= Value-375;

end;


function TXPBubbleHint.GetTop: Integer;
begin
 result:= FTop+90;

end;



procedure TXPBubbleHint.SetTop(const Value:Integer);
begin
 FTop:= Value-90;

end;


function TXPBubbleHint.Execute: Boolean;
var Diag: TBubbleHintForm;
begin
 result:=false;
 FBitmap:= TBitmap.Create;
 Diag:= TBubbleHintForm.create(Owner);
 Diag.left:= FLeft;
 Diag.Top:= FTop;
 Diag.TitleLabel.Caption:= FCaption;
 Diag.InfoText.Caption:= FInfoText;

 if (FImageIndex<>-1) and (FIList<>nil) then
 begin
  FIList.GetBitmap(FImageIndex, FBitmap);
  Diag.IconImage.Picture.Bitmap:= FBitmap;
  Diag.IconImage.Stretch:= FStretched;
  Diag.IconImage.Center:= FCentered;
 end;

 if Diag.ShowModal= mrOK then Result:= true;
 Diag.Free;
 FBitmap.Free;
end;

end.
