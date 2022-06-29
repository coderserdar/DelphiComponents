unit FormEnviro;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

type
  TfrEnvVars = class(TForm)
    sgEnvVar: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure sgEnvVarKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frEnvVars: TfrEnvVars;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrEnvVars.FormShow(Sender: TObject);
const
	Max_Val = 13;
	EnvVals: array[0..Max_Val-1] of ShortString= (
		'COMSPEC', 'HOMEPATH', 'LIBPATH', 'OS', 'PATH', 'PATHEXT',
		'PROCESSOR_ARCHITECTURE', 'PROCESSOR_IDENTIFIER', 'PROCESSOR_LEVEL',
		'PROCESSOR_REVISION', 'TEMP', 'TMP', 'WINDIR');
var
	i: integer;
begin
	sgEnvVar.RowCount:= 2;
	with frMain.APITools1 do
	begin
		sgEnvVar.Cells[0,0]:= 'Variable';
		sgEnvVar.Cells[1,0]:= 'Value';
		for i:= 0 to Max_Val -1  do
		begin
			sgEnvVar.RowCount:= sgEnvVar.RowCount + 1;
			sgEnvVar.Cells[0,i+1]:= EnvVals[i];
			sgEnvVar.Cells[1,i+1]:= GetEnvironmentVar(EnvVals[i]);
		end;
	end;
end;

procedure TfrEnvVars.sgEnvVarKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
