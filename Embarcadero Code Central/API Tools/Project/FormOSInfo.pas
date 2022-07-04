unit FormOSInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
  TfrOSInfo = class(TForm)
    Panel1: TPanel;
    lbPlatformName: TLabel;
    lbInfoEx: TLabel;
    lbMajorVer: TLabel;
		lbMinorVer: TLabel;
    lbBuild: TLabel;
    lbAll: TLabel;
    lbPlatform: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frOSInfo: TfrOSInfo;

implementation

uses FormMain, APITool;

{$R *.DFM}

procedure TfrOSInfo.FormShow(Sender: TObject);
begin
	with frMain.APITools1 do
	begin
		lbPlatform.Caption:= '  Platform	'+Platform;
		lbPlatformName.Caption:= '  OS name	'+PlatformName;
		lbInfoEx.Caption:= '  Extra info	'+VerInfoEx;
		lbMajorVer.Caption:= Format('  Major version	%d',[VerMajor]);
		lbMinorVer.Caption:= Format('  Minor version	%d',[VerMinor]);
		lbBuild.Caption:= Format('  Build		%d',[VerBuild]);
		lbAll.Caption:= Format('%s  %d.%d.%d  %s',[PlatformName,VerMajor,VerMinor,VerBuild,VerInfoEx]);
	end;
end;

procedure TfrOSInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
