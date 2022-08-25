unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SqlExpr, WideStrings, DB, DBXSybaseASE;

type
  TMainForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var C: TSQLConnection;
begin
  C := TSQLConnection.Create(Self);
  try
    C.DriverName := 'FirebirdConnection';
//    C.LibraryName := 'dbxufb40.dll';
//    C.VendorLib := 'C:\Program Files\Firebird\Firebird_1_5\bin\fbclient.dll';
//    C.GetDriverFunc := 'getSQLDriverFIREBIRD';
    C.Params.Add('User_Name=SYSDBA');
    C.Params.Add('Password=masterkey');
    C.Params.Add('Database=localhost:%ProgramFiles%\Firebird\Firebird_1_5\examples\employee.fdb');
    C.Open;
    if C.Connected then
      ShowMessage('Connection is active')
  finally
    C.Free;
  end;
end;

end.
