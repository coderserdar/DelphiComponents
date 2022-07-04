unit FormVolInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrVolInfo = class(TForm)
    GroupBox1: TGroupBox;
    cbxFNCase: TCheckBox;
    cbxCaseSen: TCheckBox;
    cbxUniCode: TCheckBox;
    cbxACLs: TCheckBox;
    cbxFComp: TCheckBox;
    cbxVolComp: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
	frVolInfo: TfrVolInfo;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrVolInfo.FormShow(Sender: TObject);
begin
	with frMain.APITools1 do
	begin
		cbxFNCase.Checked:= (VolSystemFlags and FS_CASE_IS_PRESERVED) = FS_CASE_IS_PRESERVED;
		cbxCaseSen.Checked:= (VolSystemFlags and FS_CASE_SENSITIVE) = FS_CASE_SENSITIVE;
		cbxUniCode.Checked:= (VolSystemFlags and FS_UNICODE_STORED_ON_DISK) = FS_UNICODE_STORED_ON_DISK;
		cbxACLs.Checked:= (VolSystemFlags and FS_PERSISTENT_ACLS) = FS_PERSISTENT_ACLS;
		cbxFComp.Checked:= (VolSystemFlags and FS_FILE_COMPRESSION) = FS_FILE_COMPRESSION;
		cbxVolComp.Checked:= (VolSystemFlags and FS_VOL_IS_COMPRESSED) = FS_VOL_IS_COMPRESSED;
	end;
end;

procedure TfrVolInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
