unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_bk8x00;

type
  TForm1 = class(TForm)
    API_bk8x001: TAPI_bk8x00;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_bk8x001Error(Sender: TObject; ErrNo: Integer;
      const ErrText: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_bk8x001Error(Sender: TObject; ErrNo: Integer;
  const ErrText: string);
begin
  messagedlg(api_bk8x001.geterror(errno),mterror,[mbok],0);
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_bk8x001.showsettings;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  api_bk8x001.open:=not api_Bk8x001.open;
end;

end.
