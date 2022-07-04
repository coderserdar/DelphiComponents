unit bDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, Db;

type
  Tdm = class(TDataModule)
    db: TDatabase;
    quOrders: TQuery;
    quCust: TQuery;
    dsCust: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm: Tdm;

implementation
uses vgDBUtl;

{$R *.DFM}

initialization
  RegisterFieldClasses;
  
end.
