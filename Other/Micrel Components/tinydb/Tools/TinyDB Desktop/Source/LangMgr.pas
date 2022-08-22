unit LangMgr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, TypInfo, TinyDB, IniFiles;

const
  //LCID Consts
  LangChinesePR = (SUBLANG_CHINESE_SIMPLIFIED shl 10) or LANG_CHINESE;
  LangChineseTW = (SUBLANG_CHINESE_TRADITIONAL shl 10) or LANG_CHINESE;
  LangEnglish = (SUBLANG_ENGLISH_US shl 10) or LANG_ENGLISH;
  LangFrench = (SUBLANG_FRENCH shl 10) or LANG_FRENCH;
  LangGerman = (SUBLANG_GERMAN shl 10) or LANG_GERMAN;

  SLangMgrDbName = 'LangMgrDbName';

type
  PLangItem = ^TLangItem;
  TLangItem = record
    LangName: string;
    FileName: string;
  end;
  TLangItems = array of TLangItem;

  TLangFontData = record
    Name: string;
    Size: Integer;
    Charset: TFontCharset;
  end;

{ TLangMgrBase }

  TLangMgrBase = class(TObject)
  protected
    FLangItems: TLangItems;
    FLangFontData: TLangFontData;
    FFontInited: Boolean;
    FDefaultLangIndex: Integer; //Default language
    FCurrentLangIndex: Integer; //Current language

    function GetLangItem(Index: Integer): TLangItem;
    function GetLangCount: Integer;
    function InternalInitLang(Index: Integer): Boolean; virtual;
  public
    function GetFormString(Name: string; var Value: string): Boolean; virtual; abstract;
    function GetMiscString(Name: string; var Value: string): Boolean; virtual; abstract;
    function GetFontData(var FontData: TLangFontData): Boolean; virtual; abstract;
    function IsLangFile(FileName: string): Boolean; virtual; abstract;
    function GetLangNameFromFileName(FileName: string): string; virtual; abstract;
    function GetDefaultLangIndex(LangItems: TLangItems): Integer; virtual;
    function GetLangFileList(List: TStrings; Path: string): Integer;
    function IndexOfLangName(LangName: string): Integer;
    //Misc functions
    class function GetLangFileExt(Locale: LCID): string;
    class function GetDefaultLangFileExt: string;
  public
    constructor Create;
    destructor Destroy; override;
    function InitPath(Path: string): Integer; virtual;
    function InitLang(Index: Integer): Boolean; virtual;

    procedure Trans(Form: TForm); overload;
    function Trans(Src: string): string; overload;
    function Trans(Src: string; Args: array of const): string; overload;

    property Items[Index: Integer]: TLangItem read GetLangItem;
    property Count: Integer read GetLangCount;
    property DefaultLangIndex: Integer read FDefaultLangIndex;
    property CurrentLangIndex: Integer read FCurrentLangIndex;
    property FontData: TLangFontData read FLangFontData;
  end;

{ TTDBLangMgr }

  TTDBLangMgr = class(TLangMgrBase)
  private
    FTinyDatabase: TTinyDatabase;
    FFormTable: TTinyTable;
    FMiscTable: TTinyTable;
    FDBStream: TMemoryStream;
  protected
    function InternalInitLang(Index: Integer): Boolean; override;
  public
    function GetFormString(Name: string; var Value: string): Boolean; override;
    function GetMiscString(Name: string; var Value: string): Boolean; override;
    function GetFontData(var FontData: TLangFontData): Boolean; override;
    function IsLangFile(FileName: string): Boolean; override;
    function GetLangNameFromFileName(FileName: string): string; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TIniLangMgr }

  TIniLangMgr = class(TLangMgrBase)
  private
    FIniFile: TIniFile;
  protected
    function InternalInitLang(Index: Integer): Boolean; override;
  public
    function GetFormString(Name: string; var Value: string): Boolean; override;
    function GetMiscString(Name: string; var Value: string): Boolean; override;
    function GetFontData(var FontData: TLangFontData): Boolean; override;
    function IsLangFile(FileName: string): Boolean; override;
    function GetLangNameFromFileName(FileName: string): string; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  AppLangMgr: TLangMgrBase;

implementation

{ TLangMgrBase }

constructor TLangMgrBase.Create;
begin
  inherited;
  FDefaultLangIndex := -1;
  FCurrentLangIndex := -1;
end;

destructor TLangMgrBase.Destroy;
begin
  inherited;
end;

function TLangMgrBase.GetLangItem(Index: Integer): TLangItem;
begin
  Result := FLangItems[Index];
end;

function TLangMgrBase.GetLangCount: Integer;
begin
  Result := Length(FLangItems);
end;

function TLangMgrBase.InternalInitLang(Index: Integer): Boolean;
begin
  Result := True;
end;

function TLangMgrBase.GetDefaultLangIndex(LangItems: TLangItems): Integer;
var
  I: Integer;
  DefaultExt: string;
begin
  Result := -1;
  DefaultExt := GetDefaultLangFileExt;
  for I := 0 to Length(LangItems) - 1 do
  begin
    if CompareText(ExtractFileExt(LangItems[I].FileName), DefaultExt) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TLangMgrBase.GetLangFileList(List: TStrings; Path: string): Integer;
var
  Sr: TSearchRec;
  FileName: string;
begin
  if Copy(Path, Length(Path), 1) <> '\' then
    Path := Path + '\';
  List.BeginUpdate;
  try
    List.Clear;
    if FindFirst(Path + '*.*', faAnyFile, Sr) = 0 then
    begin
      repeat
        FileName := Path + Sr.Name;
        if IsLangFile(FileName) then List.Add(FileName);
      until FindNext(Sr) <> 0;
      FindClose(Sr);
    end;
  finally
    List.EndUpdate;
  end;
  Result := List.Count;
end;

function TLangMgrBase.IndexOfLangName(LangName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FLangItems) - 1 do
  begin
    if CompareText(LangName, FLangItems[I].LangName) = 0 then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//---------------------------------------------------------------------------
//Return the file ext name by system default locale identifier.
//---------------------------------------------------------------------------

class function TLangMgrBase.GetDefaultLangFileExt: string;
begin
  Result := GetLangFileExt(SysLocale.DefaultLCID);
end;

//---------------------------------------------------------------------------
//Return the file ext name by a locale identifier(LCID).
//e.g. ".chs"
//---------------------------------------------------------------------------

class function TLangMgrBase.GetLangFileExt(Locale: LCID): string;
var
  LocaleName: array[0..4] of Char;
begin
  GetLocaleInfo(Locale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  case Locale of
    LangChineseTW: Result := 'cht';
  else
    Result := LocaleName;
  end;
  Result := '.' + Result;
end;

function TLangMgrBase.InitPath(Path: string): Integer;
var
  FileList: TStrings;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    GetLangFileList(FileList, Path);
    SetLength(FLangItems, FileList.Count);
    for I := 0 to FileList.Count - 1 do
    begin
      FLangItems[I].FileName := FileList[I];
      FLangItems[I].LangName := GetLangNameFromFileName(FileList[I]);
    end;
    FDefaultLangIndex := GetDefaultLangIndex(FLangItems);
  finally
    Result := FileList.Count;
    FileList.Free;
  end;
end;

function TLangMgrBase.InitLang(Index: Integer): Boolean;
begin
  if (Index < 0) or (Index >= Count) then
  begin
    Result := False;
    Exit;
  end;
  FCurrentLangIndex := Index;
  Result := InternalInitLang(Index);
  if not Result then Exit;
  FFontInited := GetFontData(FLangFontData);
end;

procedure TLangMgrBase.Trans(Form: TForm);
const
  PropNames: array[0..2] of PChar = ('Caption', 'Text', 'Hint');
var
  I, J: Integer;
  Compo: TComponent;
  PropInfoPtr: PPropInfo;
  FullName, PropName, PropOldValue, PropNewValue: string;
begin
  for I := -1 to Form.ComponentCount - 1 do
  begin
    if I = -1 then
      Compo := Form
    else
      Compo := Form.Components[I];
    for J := Low(PropNames) to High(PropNames) do
    begin
      PropName := PropNames[J];
      PropInfoPtr := GetPropInfo(Compo, PropName);
      if PropInfoPtr <> nil then
        if PropInfoPtr^.PropType^.Kind in [tkLString, {$IFDEF UNICODE}tkUString, {$ENDIF}tkWString, tkString] then
        begin
          PropOldValue := GetStrProp(Compo, PropInfoPtr);
          if ((PropOldValue <> '') and (PropOldValue <> '-')) and (Compo.Name <> '') then
          begin
            //FullName := Form.Name + '.';
            FullName := Form.ClassName + '.';
            if I <> -1 then FullName := FullName + Compo.Name + '.';
            FullName := FullName + PropName;
            if GetFormString(FullName, PropNewValue) then
              SetStrProp(Compo, PropName, PropNewValue);
          end;
        end;
    end;
  end;
  if FFontInited then
  begin
    Form.Font.Name := FLangFontData.Name;
    Form.Font.Size := FLangFontData.Size;
    Form.Font.Charset := FLangFontData.Charset;
  end;
end;

function TLangMgrBase.Trans(Src: string): string;
begin
  GetMiscString(Src, Result);
end;

function TLangMgrBase.Trans(Src: string; Args: array of const): string;
begin
  Result := Format(Trans(Src), Args);
end;

{ TTDBLangMgr }

constructor TTDBLangMgr.Create;
begin
  inherited;
  FTinyDatabase := TTinyDatabase.Create(nil);
  FFormTable := TTinyTable.Create(nil);
  FMiscTable := TTinyTable.Create(nil);
  FDBStream := TMemoryStream.Create;
end;

destructor TTDBLangMgr.Destroy;
begin
  FTinyDatabase.Free;
  FFormTable.Free;
  FMiscTable.Free;
  FDBStream.Free;
  inherited;
end;

function TTDBLangMgr.IsLangFile(FileName: string): Boolean;
var
  TinyDB: TTinyDatabase;
begin
  Result := False;
  if not TTinyDatabase.IsTinyDBFile(FileName) then Exit;
  TinyDB := TTinyDatabase.Create(nil);
  try
    try
      TinyDB.DatabaseName := FileName;
      TinyDB.Open;
      if (TinyDB.TableDefs.IndexOf('Info') = -1) or
        (TinyDB.TableDefs.IndexOf('Form') = -1) or
        (TinyDB.TableDefs.IndexOf('Misc') = -1) then Exit;
    except
      Exit;
    end;
  finally
    TinyDB.Free;
  end;
  Result := True;
end;

function TTDBLangMgr.GetLangNameFromFileName(FileName: string): string;
var
  InfoTable: TTinyTable;
begin
  Result := '';
  InfoTable := TTinyTable.Create(nil);
  try
    try
      InfoTable.DatabaseName := FileName;
      InfoTable.TableName := 'Info';
      InfoTable.IndexName := 'Name';
      InfoTable.Open;
      if InfoTable.FindKey(['Language']) then
        Result := InfoTable.FieldByName('Value').AsString;
    except
    end;
  finally
    InfoTable.Close;
    InfoTable.Free;
  end;
end;

function TTDBLangMgr.GetFormString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if FFormTable.Active then
  begin
    try
      Result := FFormTable.FindKey([Name]);
      if Result then Value := FFormTable.FieldByName('Value').AsString;
    except
      Result := False;
    end;
  end;
  if not Result then Value := '';
end;

function TTDBLangMgr.GetMiscString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if FMiscTable.Active then
  begin
    try
      Result := FMiscTable.FindKey([Name]);
      if Result then Value := FMiscTable.FieldByName('Value').AsString;
    except
      Result := False;
    end;
  end;
  if not Result then Value := Name;
end;

function TTDBLangMgr.GetFontData(var FontData: TLangFontData): Boolean;
var
  InfoTable: TTinyTable;
begin
  Result := True;
  InfoTable := TTinyTable.Create(nil);
  try
    try
      with InfoTable do
      begin
        DatabaseName := SLangMgrDbName;
        TableName := 'Info';
        IndexName := 'Name';
        Open;
      end;
      if InfoTable.FindKey(['FontName']) then
        FontData.Name := InfoTable.FieldByName('Value').AsString
      else
        Result := False;
      if InfoTable.FindKey(['FontSize']) then
        FontData.Size := StrToInt(InfoTable.FieldByName('Value').AsString)
      else
        Result := False;
      if InfoTable.FindKey(['FontCharset']) then
        FontData.Charset := StrToInt(InfoTable.FieldByName('Value').AsString)
      else
        Result := False;
    except
      Result := False;
    end;
  finally
    InfoTable.Close;
    InfoTable.Free;
  end;
end;

function TTDBLangMgr.InternalInitLang(Index: Integer): Boolean;
begin
  Result := True;
  try
    FDBStream.LoadFromFile(FLangItems[Index].FileName);
    with FTinyDatabase do
    begin
      Close;
      MediumType := mtMemory;
      DatabaseName := SLangMgrDbName;
      FileName := PointerToStr(FDBStream);
      Open;
    end;

    with FFormTable do
    begin
      Close;
      DatabaseName := SLangMgrDbName;
      TableName := 'Form';
      IndexName := 'Name';
      Open;
    end;
    with FMiscTable do
    begin
      Close;
      DatabaseName := SLangMgrDbName;
      TableName := 'Misc';
      IndexName := 'Name';
      Open;
    end;
  except
    Result := False;
  end;
end;

{ TIniLangMgr }

constructor TIniLangMgr.Create;
begin
  inherited;
end;

destructor TIniLangMgr.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TIniLangMgr.GetFormString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if Assigned(FIniFile) then
  begin
    Result := FIniFile.ValueExists('Form', Name);
    if Result then
      Value := FIniFile.ReadString('Form', Name, Value);
  end;
end;

function TIniLangMgr.GetMiscString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if Assigned(FIniFile) then
  begin
    Result := FIniFile.ValueExists('Misc', Name);
    if Result then
      Value := FIniFile.ReadString('Misc', Name, Name)
    else
      Value := Name;
  end;
end;

function TIniLangMgr.GetFontData(var FontData: TLangFontData): Boolean;
begin
  Result := True;
  try
    if FIniFile.ValueExists('Info', 'FontName') then
      FontData.Name := FIniFile.ReadString('Info', 'FontName', '')
    else
      Result := False;
    if FIniFile.ValueExists('Info', 'FontSize') then
      FontData.Size := FIniFile.ReadInteger('Info', 'FontSize', 9)
    else
      Result := False;
    if FIniFile.ValueExists('Info', 'FontCharset') then
      FontData.Charset := FIniFile.ReadInteger('Info', 'FontCharset', 1)
    else
      Result := False;
  finally
  end;
end;

function TIniLangMgr.GetLangNameFromFileName(FileName: string): string;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    Result := IniFile.ReadString('Info', 'Language', '');
  finally
    IniFile.Free;
  end;
end;

function TIniLangMgr.InternalInitLang(Index: Integer): Boolean;
begin
  Result := True;
  try
    FIniFile.Free;
    FIniFile := TIniFile.Create(FLangItems[Index].FileName);
  except
    Result := False;
  end;
end;

function TIniLangMgr.IsLangFile(FileName: string): Boolean;
var
  IniFile: TIniFile;
begin
  Result := True;
  IniFile := TIniFile.Create(FileName);
  try
    if not IniFile.SectionExists('Form') then Result := False;
    if not IniFile.SectionExists('Misc') then Result := False;
    if not IniFile.SectionExists('Info') then Result := False;
  finally
    IniFile.Free;
  end;
end;

initialization
  AppLangMgr := TTDBLangMgr.Create;

finalization
  AppLangMgr.Free;

end.
