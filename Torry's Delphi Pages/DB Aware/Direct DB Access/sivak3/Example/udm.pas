unit udm;

interface

uses
  Windows, SysUtils, Classes, sql3_defs, DB, ComCtrls, ImgList, Controls,
  Graphics, SynEdit, SynEditHighlighter;

type
  Tdm = class(TDataModule)
    ImagesDB: TImageList;
    ImagesBtn: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FSqlWindows: TList;
  public
    { Public declarations }
    procedure RegisterSqlClient(Client: TObject);
    procedure UnregisterSqlClient(Client: TObject);
    procedure CloseDatabase(Node: TTreeNode);
    procedure TableTable(simple: TSivak3SimpleTable; list, names: TStrings);
    function OpenDatabase(Node: TTreeNode): TSivak3Database;
    function ClientCount: Integer;
    function GetPragma: String;
  end;

function get_work_folder_file(file_name: String): String;
function opr_path(path: String): String;
function str_empty(s: String): Boolean;
function str_fixlen(s: String; fixed_len: Integer): String;
function str_fill(c: Char; fill_len: Integer): String;

var
  dm: Tdm;

implementation

{$R *.dfm}

function SHGetFolderPath(hwndOwner: HWND; nFolder: Integer; hToken: THANDLE; dwFlags: DWORD; pszPath: LPTSTR): HRESULT; stdcall; external 'shell32.dll' name 'SHGetFolderPathA';
const
  CSIDL_LOCAL_APPDATA = $0000001C;
  CSIDL_APPDATA       = $0000001A;
  CSIDL_FLAG_CREATE   = $00008000;
  SHGFP_TYPE_CURRENT  = $00000000;

function opr_path(path: String): String;
var
  unc: Boolean;
begin
  Result := trim(path);
  unc := pos('\\', Result) = 1;
  while pos('\\', Result) > 0 do
  Delete(Result, pos('\\', Result), 1);
  if unc then
  Insert('\', Result, 1);
end;

function str_empty(s: String): Boolean;
begin
  Result := length(trim(s)) < 1;
end;

function str_fixlen(s: String; fixed_len: Integer): String;
begin
  Result := s;
  if length(Result) > fixed_len then
  Result := copy(Result, 1, fixed_len)
  else
  while length(Result) < fixed_len do
  Result := Result + #32;
end;

function str_fill(c: Char; fill_len: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to fill_len do
  Result := Result + c;
end;

function get_work_folder_file(file_name: String): String;
const
  work_sub_folder = 'sql3man';
var
  p: Array[0 .. MAX_PATH] of AnsiChar;
  r: HRESULT;
begin
  Result := '';
  FillChar(p, SizeOf(p), #00);
  r := SHGetFolderPath(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @p);
  if r = 0 then
  begin
    Result := String(p);
    Result := opr_path(Result + '\iisivak8');
    if not DirectoryExists(Result) then
    if not CreateDirectory(PChar(Result), nil) then
    raise Exception.Create('Cannot create work directory: ' + Result);
    Result := opr_path(Result + '\' + work_sub_folder);
    if not DirectoryExists(Result) then
    if not CreateDirectory(PChar(Result), nil) then
    raise Exception.Create('Cannot create work directory: ' + Result);
    Result := opr_path(Result + '\' + file_name);
  end;
end;

{ Tdm }

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  FSqlWindows := TList.Create;
end;

procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  FSqlWindows.Free;
end;

function Tdm.OpenDatabase(Node: TTreeNode): TSivak3Database;
begin
  Result := TSivak3Database(Node.Data);
  Result.Params := 'auto_vacuum=0;cache_size=2000;locking_mode=NORMAL';
  if Boolean(Result.Tag) then
  Result.Options := Result.Options + [poForeignKeysSupport]
  else Result.Options := Result.Options - [poForeignKeysSupport];
  Result.Open;
end;

procedure Tdm.CloseDatabase(Node: TTreeNode);
var
  d: TSivak3Database;
  i: Integer;
begin
  d := TSivak3Database(Node.Data);
  for i := FSqlWindows.Count - 1 downto 0 do
  if TComponent(FSqlWindows[i]).Tag = Integer(d) then
  TObject(FSqlWindows.Items[i]).Free;
  if Assigned(d) then
  if d.Connected then
  d.Close;
  Node.DeleteChildren;
end;

procedure Tdm.RegisterSqlClient(Client: TObject);
begin
  FSqlWindows.Add(Client);
end;

procedure Tdm.UnregisterSqlClient(Client: TObject);
begin
  FSqlWindows.Remove(Client);
end;

function Tdm.ClientCount: Integer;
begin
  Result := FSqlWindows.Count;
end;

procedure Tdm.TableTable(simple: TSivak3SimpleTable; list, names: TStrings);
var
  w: Array of Word;
  r, c: Integer;
  s: String;
begin
  if simple.IsEmpty then Exit;
  SetLength(w, simple.ColCount);

  list.Clear;
  names.Clear;

  for c := 0 to simple.ColCount - 1 do
  names.Add(simple.ColumnName[c]);

  for c := 0 to simple.ColCount - 1 do //init length pomoci names
  w[c] := length(simple.ColumnName[c]);

  for r := 0 to simple.RowCount - 1 do //set length values
  for c := 0 to simple.ColCount - 1 do
  if w[c] < length(simple.ColumnValue[c, r]) then
  w[c] := length(simple.ColumnValue[c, r]);

  for c := 0 to simple.ColCount - 1 do //max length limit
  if w[c] > 255 then
  w[c] := 255;

  s := '';
  for c := 0 to simple.ColCount - 1 do
  s := s + UpperCase(str_fixlen(simple.ColumnName[c], w[c])) + ' ';
  list.Add(s);
  for r := 0 to simple.RowCount - 1 do
  begin
    s := '';
    for c := 0 to simple.ColCount - 1 do
    s := s + str_fixlen(simple.ColumnValue[c, r], w[c]) + ' ';
    list.Add(s);
  end;
end;

function Tdm.GetPragma: String;
begin
  Result :=
  'pragma encoding;'#13#10 +
  'pragma freelist_count;'#13#10 +
  'pragma auto_vacuum;'#13#10 +
  'pragma automatic_index;'#13#10 +
  'pragma count_changes;'#13#10 +
  'pragma foreign_keys;'#13#10 +
  'pragma full_column_names;'#13#10 +
  'pragma locking_mode;'#13#10 +
  'pragma read_uncommitted;'#13#10 +
  'pragma recursive_triggers;'#13#10 +
  'pragma reverse_unordered_selects;'#13#10 +
  'pragma secure_delete;'#13#10 +
  'pragma short_column_names;'#13#10 +
  'pragma journal_mode;'#13#10 +
  'pragma journal_size_limit;'#13#10 +
  'pragma synchronous;'#13#10;
end;

end.
