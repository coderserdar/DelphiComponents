unit TntSysUtils;

interface

function WideExtractFileName(const FN: WideString): WideString;
function WideExtractFileDir(const FN: WideString): WideString;
function WideExtractFileExt(const FN: WideString): WideString;
function WideExtractFileDrive(const FN: WideString): WideString;
function WideChangeFileExt(const FN, Ext: WideString): WideString;


implementation

uses
  SysUtils;

function WideExtractFileName(const FN: WideString): WideString;
begin
  Result := ExtractFileName(FN);
end;

function WideExtractFileDir(const FN: WideString): WideString;
begin
  Result := ExtractFileDir(FN);
end;

function WideExtractFileExt(const FN: WideString): WideString;
begin
  Result := ExtractFileExt(FN);
end;

function WideExtractFileDrive(const FN: WideString): WideString;
begin
  Result := ExtractFileDrive(FN);
end;

function WideCreateDir(const FN: WideString): Boolean;
begin
  Result := CreateDir(FN);
end;

function WideChangeFileExt(const FN, Ext: WideString): WideString;
begin
  Result := ChangeFileExt(FN, Ext);
end;

function WideDirectoryExists(const FN: WideString): Boolean;
begin
  Result := DirectoryExists(FN);
end;

function Tnt_WideLowerCase(const S: WideString): WideString;
begin
  Result := WideLowerCase(S);
end;

function Tnt_WideUpperCase(const S: WideString): WideString;
begin
  Result := WideUpperCase(S);
end;

function WideIncludeTrailingBackslash(const S: WideString): WideString;
begin
  Result := IncludeTrailingBackslash(S);
end;



end.
