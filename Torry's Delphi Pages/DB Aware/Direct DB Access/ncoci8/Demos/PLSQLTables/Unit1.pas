// This demo shows how to use PL/SQL tables. This programm
// reverse array of strings, using PL/SQL packaged procedure.
// To use this demo you must first run script PLSQLTables.sql
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, NCOci, NCOciWrapper, NCOciDB;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIStoredProc1: TOCIStoredProc;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  OCIStoredProc1.Params[0].ArrayLen := Memo1.Lines.Count;
  for i := 0 to Memo1.Lines.Count - 1 do
    OCIStoredProc1.Params[0].AsStrings[i] := Memo1.Lines[i];
  OCIStoredProc1.ExecProc;
  Memo1.Lines.Clear;
  for i := 0 to OCIStoredProc1.Params[0].ArrayLen - 1 do
    Memo1.Lines.Add(OCIStoredProc1.Params[0].AsStrings[i]);
end;

end.
