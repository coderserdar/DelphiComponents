unit sql3_utils;

interface

{$I db_sql.inc}

uses
  Windows, Messages, SysUtils, Classes;

function opr_path(path: String): String;
function str_empty(s: AnsiString): Boolean; overload;
function str_empty(s: WideString): Boolean; overload;
function check_brakes(field_name: AnsiString; unicode: Boolean): AnsiString;
function check_fields_brakes(field_names: AnsiString; unicode: Boolean): AnsiString;
function get_param_name(field_name: String): String;
function table_name_from_sql(sql: String; brakes: Boolean): String;

function str_validA(s: PAnsiChar): DWORD;
function str_validW(s: PWideChar): DWORD;
function wide_str_copy(source, destination: Pointer; max_len: DWORD): DWORD;
function xchange_endian(source: Pointer; len: DWORD): PWideChar;

resourcestring
  e_invalid_dll_driver     = 'Invalid DLL driver!';
  e_missing_dll_driver     = 'Missing DLL driver property!';
  e_database_not_exists    = 'Database file don'#39't exist'#39's!';
  e_database_not_connected = 'Not connected!';
  e_database_not_assigned  = 'Missing Database property!';
  e_database_not_file      = 'Missing DatabaseFile property!';
  e_dataset_not_assigned   = 'Missing DataSet property!';
  e_database_file_exists   = 'Database file already exists!';
  e_database_connected     = 'Close a database before creation!';
  e_database_denied        = 'Cannot create new database file. Access denied!';
  e_table_name_missing     = 'Missing TableName property!';
  e_invalid_command_handle = 'Invalid command handle!';
  e_not_command_text       = 'Invalid SQL command!';
  e_mastersource_missing   = 'Missing MasterSource property!';
  e_mastersource_error     = 'Cannot open MasterSource dataset!';
  e_unsupported_param      = 'Unsupported parameter type!';
  e_row_unaffected         = 'Data write error, rows affected = 0!';
  e_invalid_rec_size       = 'Invalid record size!';
  e_rowid_unavailable      = 'Cannot post, current rowid is not available!';
  e_modify_view            = 'Cannot modify a VIEW object!';
  e_invalid_filter         = 'Invalid filter expression!';
  e_invalid_index_fields   = 'Invalid index field(s)!';
  e_invalid_index          = 'Invalid index!';

implementation

{$IFDEF D9_UP}
uses AnsiStrings;
{$ENDIF}

function _clean_field_name(field_name: PAnsiChar): BOOL; far; assembler;
//zjisti, jestli nazev pole (tabulky, indexu...) obsahuje specialni znaky
asm
               push   edi       //povinny uklid EDI, ESI, ESP, EBP, EBX
               push   esi
               push   esp
               push   ebp
               push   ebx

               mov    esi, eax
               xor    eax, eax
               mov    ah, 0001h //pripravi "true"
               mov    ecx, 64   //max delka nazvu pole

               mov    al, byte ptr [esi]
               cmp    al, 039h    //zacina cislici, nebo paznakem?
               jbe    @false      //ano, ven s false

@loop_1:
               mov    al, byte ptr [esi]
               or     al, al
               jz     @return_func //konec retezce
               inc    esi
               cmp    al, '_'   //povoli podtrzitko
               je     @cont
               cmp    al, 030h  //0 - 9 = 30h - 39h
               jb     @false
               cmp    al, 039h
               jbe    @cont
               cmp    al, 041h  //A - Z = 41h - 5Ah
               jb     @false
               cmp    al, 05Ah
               jbe    @cont
               cmp    al, 061h  //a - z = 61h - 7Ah
               jb     @false
               cmp    al, 07Ah
               jbe    @cont
               jmp    @false
@cont:
               loop   @loop_1
@false:
               xor    ah, ah
@return_func:
               mov    al, ah    //navratova hodnota
               xor    ah, ah

               pop    ebx       //povinna obnova
               pop    ebp
               pop    esp
               pop    esi
               pop    edi
end;

function str_validA(s: PAnsiChar): DWORD;
//ansi string - vrati nulu, pokud string nebsahuje zadny znak vetsi, nez mezera
asm
               push   esi

               mov    esi, eax
@loop_1:
               mov    ah, byte ptr [esi]
               or     ah, 00h
               jz     @false
               cmp    ah, 20h
               ja     @return_func
               inc    esi
               jmp    @loop_1
@false:
               xor    eax, eax
@return_func:
               pop    esi
end;

function str_validW(s: PWideChar): DWORD;
//unicode 16 string - vrati nulu, pokud string nebsahuje zadny znak vetsi, nez mezera
asm
               push   esi

               mov    esi, eax
@loop_1:
               mov    ax, word ptr [esi]
               or     ax, 0000h
               jz     @false
               cmp    ax, 20h
               ja     @return_func
               inc    esi
               inc    esi
               jmp    @loop_1
@false:
               xor    eax, eax
@return_func:
               pop    esi
end;

function wide_str_copy(source, destination: Pointer; max_len: DWORD): DWORD;
//unicode 16 string copy, max_len is in BYTES!!!
asm
               push   esi
               push   edi

               mov    esi, eax
               mov    edi, edx
               shr    ecx, 00000001h // divide by 2
@loop_1:
               mov    ax, word ptr [esi]
               mov    word ptr [edi], ax
               inc    esi
               inc    esi
               inc    edi
               inc    edi
               or     ax, ax
               loopnz @loop_1

               mov    eax, edi
               pop    edi
               pop    esi
               sub    eax, edi //retun value (count)
end;

function xchange_endian(source: Pointer; len: DWORD): PWideChar;
//convert wide string big endian <-> little endian
asm
               push   esi
               push   eax  // prepare return value (stejna jako source)

               mov    esi, eax
               mov    ecx, edx
               shr    ecx, 00000001h // divide by 2
@loop_1:
               mov    ax, word ptr [esi]
               xchg   al, ah
               mov    word ptr [esi], ax
               inc    esi
               inc    esi
               or     ax, ax
               loopnz @loop_1

               pop    eax // return value
               pop    esi
end;

// public ----------------------------------------------------------------------

function opr_path(path: String): String;
begin
  Result := path;
  while pos('\\', Result) > 0 do
  Delete(Result, pos('\\', Result), 1);
end;

function str_empty(s: AnsiString): Boolean;
begin
  Result := str_validA(PAnsiChar(s)) = 0; //length(trim(s)) < 1;
end;

function str_empty(s: WideString): Boolean;
begin
  Result := str_validW(PWideChar(s)) = 0 //length(trim(s)) < 1;
end;

function check_brakes(field_name: AnsiString; unicode: Boolean): AnsiString;
const
  open_brake = '"';
  close_brake = '"';
  upper_case = false;
begin
  Result := trim(field_name);
  if not str_empty(Result) then
  if Result[1] = open_brake then Exit; //pokud uz je v brakes tak nic

  if _clean_field_name(PAnsiChar(AnsiString(Result))) then
  begin
    if upper_case then
    Result := UpperCase(Result);
  end
  else
  if unicode then
  Result := open_brake + AnsiToUtf8(String(Result)) + close_brake
  else
  Result := open_brake + Result + close_brake;
end;

function check_fields_brakes(field_names: AnsiString; unicode: Boolean): AnsiString;
var
  l: TStringList;
  i: Integer;
  s: AnsiString;
begin
  Result := '';
  s := AnsiString(StringReplace(String(field_names), ';', ',', [rfReplaceAll]));
  s := AnsiString(StringReplace(String(s), ',', #13#10, [rfReplaceAll]));
  l := TStringList.Create;
  try
    l.Text := String(s);
    for i := 0 to l.Count - 1 do
    Result := Result + check_brakes(AnsiString(l.Strings[i]), unicode) + ',';
    if not str_empty(Result) then
    Delete(Result, length(Result), 1);
  finally
    l.Free;
  end;
end;

function get_param_name(field_name: String): String;
var
  i: Integer;
  n: Integer;
begin
  Result := Trim(field_name);
  while not _clean_field_name(PAnsiChar(AnsiString(Result))) do
  begin
    for i := 1 to length(Result) - 1 do
    if not (AnsiChar(Result[i]) in ['A' .. 'Z', 'a' .. 'z', '0' .. '9']) then
    begin
      n := ord(AnsiChar(Result[i]));
      Result := StringReplace(Result, Result[i], IntToStr(n), [rfReplaceAll]);
      Break;
    end;
  end;
end;

function table_name_from_sql(sql: String; brakes: Boolean): String;
var
  s: String;
  i: Integer;
begin
  Result := '';
  s := UpperCase(StringReplace(sql, #13#10, ' ', [rfReplaceAll])) + ' ';
  i := pos(' FROM ', s);
  if i < 1 then Exit;
  Delete(s, 1, i + 5);
  s := trim(s);
  if str_empty(s) then Exit;
  if s[1] = '"' then
  begin
    Delete(s, 1, 1);
    i := pos('"', s);
    if i > 1 then
    Result := copy(s, 1, i - 1)
  end
  else
  begin
    i := pos(' ', s);
    if i > 1 then
    Result := copy(s, 1, i - 1)
    else Result := trim(s);
  end;
  if brakes then
  Result := String(check_brakes(AnsiString(Result), false));
end;

end.
