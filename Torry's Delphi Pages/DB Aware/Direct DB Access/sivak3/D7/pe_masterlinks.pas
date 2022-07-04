unit pe_masterlinks;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Grids, ValEdit;

type
  Tfpe_masterlinks = class(TForm)
    ValueList: TValueListEditor;
    Button1: TButton;
    Cancel: TButton;
  private
    { Private declarations }
    procedure set_prop_list(Field: String; list: TStringList);
    function original_values(Values: String): TStringList;
  public
    { Public declarations }
    function show_fields(DetailList, MasterList: TStringList; var res: String): Integer;
  end;

implementation

uses sql3_utils;

{$R *.dfm}

{ Tfpe_masterlinks }

function Tfpe_masterlinks.show_fields(DetailList, MasterList: TStringList; var res: String): Integer;
var
  i: Integer;
  l: TStringList;
  n, v: String;
begin
  for i := 0 to DetailList.Count - 1 do
  set_prop_list(DetailList.Strings[i], MasterList);
  l := original_values(res);
  try
    for i := 0 to l.Count - 1 do
    begin
      n := l.Names[i];
      if str_empty(n) then
      n := l.Strings[i];
      v := l.Values[n];
      if str_empty(v) then
      v := n;
      ValueList.Values[v] := n;
    end;
    Result := ShowModal;
    res := '';
    for i := 0 to ValueList.Strings.Count - 1 do
    begin
      n := trim(ValueList.Keys[i]);
      if str_empty(n) then Continue;
      v := trim(ValueList.Values[n]);
      if str_empty(v) then Continue;
      if CompareText(v, n) = 0 then
      res := res + v + ';'
      else res := res + v + '=' + n + ';'
    end;
    if length(res) > 0 then
    System.Delete(res, length(res), 1);
  finally
    l.Free;
  end;
end;

procedure Tfpe_masterlinks.set_prop_list(Field: String; list: TStringList);
begin
  ValueList.Values[Field] := '';
  ValueList.ItemProps[Field].PickList.Assign(list);
end;

function Tfpe_masterlinks.original_values(Values: String): TStringList;
var
  s: String;
begin
  Result := TStringList.Create;
  s := StringReplace(Values, ';', #13#10, [rfReplaceAll]);
  Result.Text := StringReplace(s, ',', #13#10, [rfReplaceAll]);
end;

end.
