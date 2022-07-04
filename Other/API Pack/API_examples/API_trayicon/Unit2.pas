unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_trayicon;

type
  TForm1 = class(TForm)
    API_trayicon1: TAPI_trayicon;
    procedure API_trayicon1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_trayicon1Click(Sender: TObject);
begin
  application.restore;
  application.BringToFront;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  //application.Minimize;
end;

end.
