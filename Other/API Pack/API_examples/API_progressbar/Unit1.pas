unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_progressbar, ComCtrls, StdCtrls, API_trackbar,
  API_base;

type
  TForm1 = class(TForm)
    tAPI_progressbar1: tAPI_progressbar;
    API_trackbar1: TAPI_trackbar;
    API_progressbar1: TAPI_progressbar;
    procedure API_trackbar1Change(sender: TObject; value: Extended);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    procedure PaintProgressbars(const value: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.PaintProgressbars(const value: integer);
begin
  tapi_progressbar1.Position:= value;
  api_progressbar1.Position:= value;
end;

procedure TForm1.API_trackbar1Change(sender: TObject; value: Extended);
begin
  paintprogressbars( trunc(api_trackbar1.value) );
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  paintprogressbars( trunc(api_trackbar1.value) );
end;

end.
