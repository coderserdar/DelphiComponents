unit ADm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables;

type
  Tdm = class(TDataModule)
    tbTree: TTable;
    tbTreeID: TAutoIncField;
    tbTreeParent_ID: TIntegerField;
    tbTreeName: TStringField;
    db: TDatabase;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm: Tdm;

implementation

{$R *.DFM}

end.
