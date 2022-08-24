unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DecoRatingStars, DecoRatingStarsPlus, ImgList, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    ilStars: TImageList;
    rgStyle: TRadioGroup;
    gbInteractive: TPanel;
    lbHoveredRating: TLabel;
    lbCurrentRating: TLabel;
    tbSize: TTrackBar;
    lbSize: TLabel;
    Label1: TLabel;
    tbMargin: TTrackBar;
    rating1: TDecoRatingStarsPlus;
    procedure FormCreate(Sender: TObject);
    procedure rating1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure rating1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure rating1MouseLeave(Sender: TObject);
    procedure rgStyleClick(Sender: TObject);
    procedure tbSizeChange(Sender: TObject);
    procedure tbMarginChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateCurrentRating;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  lbHoveredRating.Caption:='';
  UpdateCurrentRating;
end;

procedure TfrmMain.UpdateCurrentRating;
begin
  if rating1.RatingsCount=0 then
    lbCurrentRating.Caption:=''
  else
    lbCurrentRating.Caption:=Format('%.2n/%.0n  (by %d ratings)',[rating1.Rating,rating1.MaxValue,rating1.RatingsCount]);
end;

procedure TfrmMain.rating1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateCurrentRating;
end;

procedure TfrmMain.rating1MouseLeave(Sender: TObject);
begin
  lbHoveredRating.Caption:='';
end;

procedure TfrmMain.rating1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lbHoveredRating.Caption:=Format('%.0n/%.0n',[rating1.HoveredRating,rating1.MaxValue]);
end;

procedure TfrmMain.rgStyleClick(Sender: TObject);
begin
  if rgStyle.ItemIndex=0 then
    rating1.Images:=ilStars
  else
  begin
    rating1.Images:=nil;
    rating1.StarShape:=TRatingStarShape(rgStyle.ItemIndex-1);
  end;
end;

procedure TfrmMain.tbMarginChange(Sender: TObject);
begin
  rating1.StarMargin:=tbMargin.Position;
end;

procedure TfrmMain.tbSizeChange(Sender: TObject);
begin
  rating1.StarSize:=tbSize.Position;
end;

end.
