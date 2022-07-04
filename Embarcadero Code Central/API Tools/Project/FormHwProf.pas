unit FormHwProf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrHwProfile = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frHwProfile: TfrHwProfile;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrHwProfile.FormShow(Sender: TObject);
begin
	with frMain.APITools1 do
	begin
		Label1.Caption:= Format('Docking Info :	%d',[HwProfDocking]);
		Label2.Caption:= 'Profile GUID :	'+HwProfGuid;
		Label3.Caption:= 'Profile name :	'+HwProfName;
	end;
end;

procedure TfrHwProfile.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
