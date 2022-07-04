unit fparmsfill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, ValEdit, StdCtrls, DB, sql3_defs, IniFiles;

type
  TmanFillParms = class(TForm)
    Panel: TPanel;
    parms: TValueListEditor;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    q: TSivak3Query;
  public
    { Public declarations }
  end;

function fill_params(query: TSivak3Query): Boolean;

implementation

uses udm;

{$R *.dfm}

function fill_params(query: TSivak3Query): Boolean;
begin
  Result := query.Params.Count < 1;
  if not Result then
  with TmanFillParms.Create(query) do
  Result := ShowModal = mrOk;
end;

procedure TmanFillParms.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  q := TSivak3Query(Owner);
  for i := 0 to q.Params.Count - 1 do
  parms.InsertRow(q.Params[i].Name, '', true);

  with TIniFile.Create(get_work_folder_file('sql3man.ini')) do
  try
    Left := ReadInteger('params', 'left', 100);
    Top := ReadInteger('params', 'top', 100);
    Width := ReadInteger('params', 'width', Width);
    Height := ReadInteger('params', 'height', Height);
  finally
    Free;
  end;
end;

procedure TmanFillParms.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  with TIniFile.Create(get_work_folder_file('sql3man.ini')) do
  try
    WriteInteger('params', 'left', Left);
    WriteInteger('params', 'top', Top);
    WriteInteger('params', 'width', Width);
    WriteInteger('params', 'height', Height);
  finally
    Action := caFree;
    Free;
  end;
end;

procedure TmanFillParms.Button1Click(Sender: TObject);
var
  i: Integer;
  k, v: String;
begin
  for i := 1 to parms.RowCount - 1 do
  begin
    k := trim(parms.Keys[i]);
    v := trim(parms.Values[k]);
    if length(v) > 0 then
    q.ParamByName(k).AsString := v;
  end;
  ModalResult := mrOK;
end;

end.
