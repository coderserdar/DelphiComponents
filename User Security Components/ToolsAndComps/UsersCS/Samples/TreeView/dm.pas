unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables;

type
  TDataModule1 = class(TDataModule)
    dbUsers: TDatabase;
    procedure dbUsersLogin(Database: TDatabase; LoginParams: TStrings);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.DFM}

procedure TDataModule1.dbUsersLogin(Database: TDatabase;
  LoginParams: TStrings);
begin
  dbUsers.Params.Values['PATH']:=ExtractFilePath(Application.ExeName)+'..\Data';
end;

end.
