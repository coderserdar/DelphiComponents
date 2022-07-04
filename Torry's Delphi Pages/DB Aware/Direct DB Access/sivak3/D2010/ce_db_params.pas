unit ce_db_params;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit;

type
  Tfce_db_params = class(TForm)
    ValueList: TValueListEditor;
    Button1: TButton;
    Cancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure get_values(values: String);
  public
    { Public declarations }
    function show_params(values: String; var res: String): Integer;
  end;

implementation

{$R *.dfm}

{ Tfce_db_params }

function Tfce_db_params.show_params(values: String; var res: String): Integer;
begin
  get_values(values);
  Result := ShowModal;
  if Result = mrOK then
  res := StringReplace(ValueList.Strings.Text, #13#10, ';', [rfReplaceAll]);
  while pos(#32, res) > 0 do
  Delete(res, pos(#32, res), 1);
end;

procedure Tfce_db_params.FormCreate(Sender: TObject);
begin
  ValueList.Values['auto_vacuum'] := '0'; //0 NONE | 1 FULL | 2 INCREMENTAL
  with ValueList.ItemProps['auto_vacuum'] do
  begin
    KeyDesc := 'auto_vacuum (0=none, 1=full, 2=incremental)';
    PickList.Add('0');
    PickList.Add('1');
    PickList.Add('2');
    ReadOnly := true;
  end;

  ValueList.Values['cache_size'] := '2000';
  ValueList.ItemProps['cache_size'].EditMask := '99999';

  ValueList.Values['locking_mode'] := 'NORMAL';
  with ValueList.ItemProps['locking_mode'] do
  begin
    PickList.Add('NORMAL');
    PickList.Add('EXCLUSIVE');
    ReadOnly := true;
  end;

  ValueList.Values['synchronous'] := '2'; //0 OFF | 1 NORMAL | 2 FULL;
  with ValueList.ItemProps['synchronous'] do
  begin
    KeyDesc := 'synchronous (0=off, 1=normal, 2=full)';
    PickList.Add('0');
    PickList.Add('1');
    PickList.Add('2');
    ReadOnly := true;
  end;

  ValueList.Values['journal_mode'] := 'DELETE';
  with ValueList.ItemProps['journal_mode'] do
  begin
    PickList.Add('DELETE');
    PickList.Add('TRUNCATE');
    PickList.Add('PERSIST');
    PickList.Add('MEMORY');
    PickList.Add('WAL');
    PickList.Add('OFF');
    ReadOnly := true;
  end;

  ValueList.Values['journal_size_limit'] := '-1';
  with ValueList.ItemProps['journal_size_limit'] do
  begin
    KeyDesc := 'journal_size_limit [bytes], -1=no limit';
    EditMask := '########';
  end;

end;

procedure Tfce_db_params.get_values(values: String);
var
  l: TStringList;
  i: Integer;
begin
  l := TStringList.Create;
  try
    l.Text := StringReplace(values, ';', #13#10, [rfReplaceAll]);
    for i := 0 to l.Count - 1 do
    if length(trim(l.ValueFromIndex[i])) > 0 then
    ValueList.Values[l.Names[i]] := l.ValueFromIndex[i];
  finally
    l.Free;
  end;
end;

end.
