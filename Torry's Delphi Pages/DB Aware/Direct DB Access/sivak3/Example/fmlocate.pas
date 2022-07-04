unit fmlocate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SynEdit, SynMemo, SynEditHighlighter, IniFiles,
  SynHighlighterGeneral, Buttons, DB, sql3_defs;

type
  Tflocate = class(TForm)
    list: TListBox;
    Panel: TPanel;
    syntax: TSynGeneralSyn;
    memo: TSynMemo;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    sp_apply: TSpeedButton;
    Label1: TLabel;
    sp_load: TSpeedButton;
    sp_save: TSpeedButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure apply_click(Sender: TObject);
    procedure listDblClick(Sender: TObject);
    procedure listKeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
    procedure memoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure memoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure listDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure sp_loadClick(Sender: TObject);
    procedure sp_saveClick(Sender: TObject);
  private
    { Private declarations }
    tbl: TSivak3Table;
  public
    { Public declarations }
  end;

procedure create_locate(table: TSivak3Table);

implementation

uses udm;

{$R *.dfm}

procedure create_locate(table: TSivak3Table);
begin
  if Assigned(table) then
  if table.Active then
  with Tflocate.Create(table) do
  ShowModal;
end;

procedure Tflocate.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  tbl := TSivak3Table(Owner);
  Caption := 'Create locate (find) expression, table: ' + tbl.TableName;
  memo.Text := tbl.Filter;
  syntax.KeyWords.Add('AND');
  syntax.KeyWords.Add('OR');
  syntax.KeyWords.Add('IS');
  syntax.KeyWords.Add('NULL');
  syntax.KeyWords.Add('LIKE');
  for i := 0 to tbl.FieldCount - 1 do
  begin
    list.Items.Add(CheckQuotation(tbl.Fields[i].FieldName));
    syntax.KeyWords.Add(tbl.Fields[i].FieldName);
  end;

  with TIniFile.Create(get_work_folder_file('sql3man.ini')) do
  try
    Left := ReadInteger('locate', 'left', 100);
    Top := ReadInteger('locate', 'top', 100);
    Width := ReadInteger('locate', 'width', Width);
    Height := ReadInteger('locate', 'height', Height);
  finally
    Free;
  end;
end;

procedure Tflocate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(get_work_folder_file('sql3man.ini')) do
  try
    WriteInteger('locate', 'left', Left);
    WriteInteger('locate', 'top', Top);
    WriteInteger('locate', 'width', Width);
    WriteInteger('locate', 'height', Height);
  finally
    Action := caFree;
    Free;
  end;
end;

procedure Tflocate.apply_click(Sender: TObject);
var
  v: Variant;
  f, s, s2: String;
  i, r: Integer;
begin
  f := '';
  r := 0;
  for i := 0 to memo.Lines.Count - 1 do
  begin
    s := memo.Lines.Names[i];
    if str_empty(s) then Continue;
    s2 := trim(memo.Lines.Values[s]);
    if str_empty(s2) then Continue;
    f := f + trim(s) + ';';
    if r > 0 then
    v := VarArrayOf([v, s2])
    else v := s2;
    inc(r, 1);
  end;
  if not str_empty(f) then
  begin
    Delete(f, length(f), 1);
    if tbl.Locate(f, v, [loCaseInsensitive, loPartialKey]) then
    ModalResult := mrOk
    else MessageBox(Handle, 'Data not found!                ', 'Locate', MB_OK);
  end;
end;

procedure Tflocate.Button2Click(Sender: TObject);
begin
  tbl.Filtered := false;
  ModalResult := mrCancel;
end;

procedure Tflocate.listDblClick(Sender: TObject);
var
  s: String;
begin
  if (list.ItemIndex >= 0) and (list.ItemIndex < list.Count) then
  begin
    s := list.Items[list.ItemIndex] + '=value';
    memo.Lines.Add(s);
  end;
end;

procedure Tflocate.listKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #00;
    listDblClick(list);
  end;
end;

procedure Tflocate.memoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = list) and (Sender = memo) then
  listDblClick(Source);
end;

procedure Tflocate.memoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = list;
end;

procedure Tflocate.listDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = list;
end;

procedure Tflocate.sp_loadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  memo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure Tflocate.sp_saveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  memo.Lines.SaveToFile(SaveDialog.FileName);
end;

end.
