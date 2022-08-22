
{**********************************************************}
{                                                          }
{  TTinyDBIniFile Class                                    }
{  Last Modified Date: 2003-03-23                          }
{                                                          }
{**********************************************************}

unit TinyDBIni;

interface

uses
  SysUtils, Classes, IniFiles, Graphics, Db, TinyDB;

const
  SFileOpenError = 'Cannot open file %s';
  SNameFieldName = 'Name';
  SValueFieldName = 'Value';

type
  TTinyDBIniFile = class(TCustomIniFile)
  private
    FTinyDatabase: TTinyDatabase;
    FTinyTable: TTinyTable;
    FEncrypt: Boolean;
    FPassword: string;

    function CreateDatabase: Boolean;
    function OpenDatabase: Boolean;
    function OpenTable(const TableName: string; Write: Boolean): Boolean;
  public
    constructor Create(const FileName: string); overload;
    constructor Create(const FileName, Password: string); overload;
    destructor Destroy; override;

    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;

    function ReadBlob(const Section, Ident: string; Value: TStream): Boolean;
    procedure WriteBlob(const Section, Ident: string; Value: TStream);
    function ReadGraphic(const Section, Ident: string; Value: TGraphic): Boolean;
    procedure WriteGraphic(const Section, Ident: string; Value: TGraphic);

    property Encrypt: Boolean read FEncrypt;
  end;

implementation

{ TTinyDBIniFile }

constructor TTinyDBIniFile.Create(const FileName: string);
begin
  inherited;
  FTinyDatabase := TTinyDatabase.Create(nil);
  FTinyTable := TTinyTable.Create(nil);
  FEncrypt := False;
end;

constructor TTinyDBIniFile.Create(const FileName, Password: string);
begin
  inherited Create(FileName);
  FTinyDatabase := TTinyDatabase.Create(nil);
  FTinyTable := TTinyTable.Create(nil);
  FEncrypt := True;
  FPassword := Password;
end;

destructor TTinyDBIniFile.Destroy;
begin
  FTinyDatabase.Close;
  FTinyTable.Free;
  FTinyDatabase.Free;
  inherited;
end;

function TTinyDBIniFile.CreateDatabase: Boolean;
begin
  Result := True;
  if FileExists(FileName) then Exit;
  try
    if FEncrypt then
      Result := FTinyDatabase.CreateDatabase(FileName, False, clNormal, 'ZLIB', True, 'Blowfish', FPassword)
    else
      Result := FTinyDatabase.CreateDatabase(FileName);
  except
    Result := False;
  end;
end;

function TTinyDBIniFile.OpenDatabase: Boolean;
begin
  Result := True;
  if not FTinyDatabase.Connected then
  begin
    try
      FTinyDatabase.DatabaseName := FileName;
      FTinyDatabase.Exclusive := False;
      FTinyDatabase.KeepConnection := True;
      FTinyDatabase.Password := FPassword;
      FTinyDatabase.Open;
    except
      Result := False;
    end;
  end;
end;

function TTinyDBIniFile.OpenTable(const TableName: string; Write: Boolean): Boolean;
begin
  Result := True;
  if Write then Result := CreateDatabase;
  if not Result then Exit;
  Result := OpenDatabase;
  if not Result then Exit;

  Result := True;
  if FTinyTable.Active and (CompareText(FTinyTable.TableName, TableName) = 0) then Exit;

  try
    if Write and not FTinyDatabase.TableExists(TableName) then
    begin
      FTinyDatabase.CreateTable(TableName, [
        FieldItem(SNameFieldName, ftString, 64),
        FieldItem(SValueFieldName, ftBlob)
        ] );
      FTinyDatabase.CreateIndex(TableName, SNameFieldName, [tiCaseInsensitive], [SNameFieldName])
    end;

    FTinyTable.Close;
    FTinyTable.DatabaseName := FileName;
    FTinyTable.TableName := TableName;
    FTinyTable.Open;
  except
    Result := False;
  end;
end;

function TTinyDBIniFile.ReadString(const Section, Ident, Default: string): string;
begin
  try
    if OpenTable(Section, False) then
    begin
      if FTinyTable.FindKey(SNameFieldName, [Ident]) then
        Result := FTinyTable.FieldByName(SValueFieldName).AsString
      else
        Result := Default;
    end else
      Result := Default;
  except
    Result := Default;
  end;
end;

procedure TTinyDBIniFile.WriteString(const Section, Ident, Value: string);
begin
  if OpenTable(Section, True) then
  begin
    FTinyTable.BeginUpdate;
    try
      if FTinyTable.FindKey(SNameFieldName, [Ident]) then
        FTinyTable.Edit
      else
        FTinyTable.Append;
      FTinyTable.FieldByName(SNameFieldName).AsString := Ident;
      FTinyTable.FieldByName(SValueFieldName).AsString := Value;
      FTinyTable.Post;
    finally
      FTinyTable.EndUpdate;
    end;
  end else
  begin
    DatabaseErrorFmt(SFileOpenError, [FileName]);
  end;
end;

procedure TTinyDBIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  S: string;
begin
  try
    Strings.BeginUpdate;
    Strings.Clear;
    try
      if OpenTable(Section, False) then
      begin
        FTinyTable.First;
        while not FTinyTable.Eof do
        begin
          S := FTinyTable.FieldByName(SNameFieldName).AsString;
          Strings.Add(S);
          FTinyTable.Next;
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  except
  end;
end;

procedure TTinyDBIniFile.ReadSections(Strings: TStrings);
begin
  try
    if not OpenDatabase then Exit;

    Strings.BeginUpdate;
    Strings.Clear;
    try
      FTinyDatabase.GetTableNames(Strings);
    finally
      Strings.EndUpdate;
    end;
  except
  end;
end;

procedure TTinyDBIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  S: string;
begin
  try
    Strings.BeginUpdate;
    Strings.Clear;
    try
      if OpenTable(Section, False) then
      begin
        FTinyTable.First;
        while not FTinyTable.Eof do
        begin
          S := FTinyTable.FieldByName(SNameFieldName).AsString + '=' +
            FTinyTable.FieldByName(SValueFieldName).AsString;
          Strings.Add(S);
          FTinyTable.Next;
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  except
  end;
end;

procedure TTinyDBIniFile.EraseSection(const Section: string);
begin
  try
    if not OpenDatabase then Exit;

    if CompareText(FTinyTable.TableName, Section) = 0 then
      FTinyTable.Close;
    FTinyDatabase.DeleteTable(Section);
  except
  end;
end;

procedure TTinyDBIniFile.DeleteKey(const Section, Ident: String);
begin
  if OpenTable(Section, False) then
  begin
    if FTinyTable.FindKey(SNameFieldName, [Ident]) then
      FTinyTable.Delete;
  end else
    DatabaseErrorFmt(SFileOpenError, [FileName]);
end;

procedure TTinyDBIniFile.UpdateFile;
begin
  if FTinyDatabase.Connected then
    FTinyDatabase.FlushCache;
end;

function TTinyDBIniFile.ReadBlob(const Section, Ident: string; Value: TStream): Boolean;
begin
  try
    if OpenTable(Section, False) then
    begin
      if FTinyTable.FindKey(SNameFieldName, [Ident]) then
      begin
        TBlobField(FTinyTable.FieldByName(SValueFieldName)).SaveToStream(Value);
        Result := True;
      end else
        Result := False;
    end else
      Result := False;
  except
    Result := False;
  end;
end;

procedure TTinyDBIniFile.WriteBlob(const Section, Ident: string; Value: TStream);
var
  S: string;
begin
  SetLength(S, Value.Size);
  Value.Position := 0;
  Value.ReadBuffer(S[1], Value.Size);
  WriteString(Section, Ident, S);
end;

function TTinyDBIniFile.ReadGraphic(const Section, Ident: string; Value: TGraphic): Boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := ReadBlob(Section, Ident, Stream);
    if Result then
    begin
      Stream.Position := 0;
      Value.LoadFromStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTinyDBIniFile.WriteGraphic(const Section, Ident: string; Value: TGraphic);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Value.SaveToStream(Stream);
    Stream.Position := 0;
    WriteBlob(Section, Ident, Stream);
  finally
    Stream.Free;
  end;
end;

end.
