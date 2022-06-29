unit FormAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frAbout: TfrAbout;

implementation

{$R *.DFM}

procedure TfrAbout.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
