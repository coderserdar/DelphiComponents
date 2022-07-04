unit fmfilter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, SynEdit, SynMemo, SynEditHighlighter, IniFiles,
  SynHighlighterGeneral, Buttons, sql3_defs;

type
  Tffilter = class(TForm)
    list: TListBox;
    Panel: TPanel;
    syntax: TSynGeneralSyn;
    memo: TSynMemo;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    sp_load: TSpeedButton;
    sp_apply: TSpeedButton;
    sp_save: TSpeedButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure apply_click(Sender: TObject);
    procedure listDblClick(Sender: TObject);
    procedure listKeyPress(Sender: TObject; var Key: Char);
    procedure sp_loadClick(Sender: TObject);
    procedure sp_saveClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure memoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure memoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure listDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  private
    { Private declarations }
    tbl: TSivak3Table;
  public
    { Public declarations }
  end;

procedure create_filter(table: TSivak3Table);

implementation

uses udm;

{$R *.dfm}

procedure create_filter(table: TSivak3Table);
begin
  if Assigned(table) then
  if table.Active then
  with Tffilter.Create(table) do
  ShowModal;
end;

procedure Tffilter.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  tbl := TSivak3Table(Owner);
  Caption := 'Create filter expression, table: ' + tbl.TableName;
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
    Left := ReadInteger('filter', 'left', 100);
    Top := ReadInteger('filter', 'top', 100);
    Width := ReadInteger('filter', 'width', Width);
    Height := ReadInteger('filter', 'height', Height);
  finally
    Free;
  end;
end;

procedure Tffilter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with TIniFile.Create(get_work_folder_file('sql3man.ini')) do
  try
    WriteInteger('filter', 'left', Left);
    WriteInteger('filter', 'top', Top);
    WriteInteger('filter', 'width', Width);
    WriteInteger('filter', 'height', Height);
  finally
    Action := caFree;
    Free;
  end;
end;

procedure Tffilter.apply_click(Sender: TObject);
begin
  tbl.Filter := trim(StringReplace(memo.Text, #13#10, #32, [rfReplaceAll]));
  tbl.Filtered := length(tbl.Filter) > 0;
  ModalResult := mrOk;
end;

procedure Tffilter.Button2Click(Sender: TObject);
begin
  tbl.Filtered := false;
  ModalResult := mrCancel;
end;

procedure Tffilter.listDblClick(Sender: TObject);
var
  s: String;
begin
  if (list.ItemIndex >= 0) and (list.ItemIndex < list.Count) then
  begin
    s := list.Items[list.ItemIndex] + ' = ' + QuotedStr('[value]');
    if length(trim(memo.Text)) > 0 then
    s := ' AND ' + s;
    memo.Lines.Add(s);
  end;
end;

procedure Tffilter.listKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #00;
    listDblClick(list);
  end;
end;

procedure Tffilter.sp_loadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  memo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure Tffilter.sp_saveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  memo.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure Tffilter.memoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = list) and (Sender = memo) then
  listDblClick(Source);
end;

procedure Tffilter.memoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = list;
end;

procedure Tffilter.listDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = list;
end;

end.
