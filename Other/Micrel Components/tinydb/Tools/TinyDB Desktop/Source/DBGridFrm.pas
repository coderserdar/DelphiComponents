unit DBGridFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, DBTableFrm, Db, TinyDB,
  DBCtrls, ComCtrls, StdCtrls, ExtCtrls, ToolWin,
  Grids, DBGrids;

type
  TDBGridForm = class(TDBTableForm)
    DBGrid: TDBGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DBGridForm: TDBGridForm;

procedure ShowDBGridForm(Value: TDBTableFormData);

implementation

{$R *.DFM}

procedure ShowDBGridForm(Value: TDBTableFormData);
var
  Frm: TDBTableForm;
begin
  Frm := TDBGridForm.Create(Application);
  Frm.SetData(Value);
  Frm.Show;
end;

end.
