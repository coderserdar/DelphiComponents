{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtFileInfo                                     }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_FileInfo;

interface
uses
  Classes;
type
  TgtFileInfo = class(TComponent)
  private
    { Private declarations }
    FVerSize          : Integer;
    FVerBuf           : PChar;
    FVerBufValue      : Pointer;
    FVerHandle        : Cardinal;
    FVerBufLen        : Cardinal;
    FVerKey           : string;
    FCompanyName      : string;
    FLegalCopyright   : string;
    FLegalTrademarks  : string;
    FOriginalFilename : string;
    FInternalName     : string;
    FFileDescription  : string;
    FFileName         : string;
    FFileVersion      : string;
    FProductVersion   : string;
    FProductName      : string;
    FComments         : string;
    procedure SetFileName(const Value: string);
    function GetFileVersionAsNumeric: Integer;
  protected
    { Protected declarations }
    procedure ParseFile;
    function  GetInfo(const aKey: string): string;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent);override;
    destructor  Destroy;override;
  public
    function QueryValue(S : string):string;
    function AllInfoAsIni:string;
  published
    { Published declarations}
    property FileName              : string read FFileName write SetFileName;
    property CompanyName           : string read FCompanyName;
    property FileDescription       : string read FFileDescription;
    property FileVersion           : string read FFileVersion;
    property FileVersionAsNumeric  : Integer read GetFileVersionAsNumeric;
    property InternalName          : string read FInternalName;
    property LegalCopyright        : string read FLegalCopyright;
    property LegalTrademarks       : string read FLegalTrademarks;
    property OriginalFilename      : string read FOriginalFilename;
    property ProductName           : string read FProductName;
    property ProductVersion        : string read FProductVersion;
    property Comments              : string read FComments;
  end;

implementation
uses
   Forms
  ,SysUtils
  ,StrUtils
  ,Windows
  ;

{ TgtFileInfo }
{------------------------------------------------------------------------------}
constructor TgtFileInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
{------------------------------------------------------------------------------}
destructor TgtFileInfo.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFileInfo.ParseFile;
begin
  if Trim(FileName) = '' then FileName := Application.ExeName;
  {if not FileExists(FileName) then
    raise Exception.CreateFmt('File %s does not exist',[FileName]);}
  if FileExists(FileName) then begin
    FVerSize := GetFileVersionInfoSize(PChar(FileName), FVerHandle);
    if FVerSize > 0 then
    begin
      try
        FVerBuf           := AllocMem(FVerSize);
        FCompanyName      := QueryValue('CompanyName');
        FFileDescription  := QueryValue('FileDescription');
        FFileVersion      := QueryValue('FileVersion');
        FInternalName     := QueryValue('InternalName');
        FLegalCopyRight   := QueryValue('LegalCopyRight');
        FLegalTradeMarks  := QueryValue('LegalTradeMarks');
        FOriginalFileName := QueryValue('OriginalFileName');
        FProductName      := QueryValue('ProductName');
        FProductVersion   := QueryValue('ProductVersion');
        FComments         := QueryValue('Comments');
      finally
        FreeMem(FVerBuf);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtFileInfo.QueryValue(S: string): string;
begin
  Result := '';
  if GetFileVersionInfo(PChar(FileName), FVerHandle, FVerSize, FVerBuf) and
      VerQueryValue(FVerBuf, '\VarFileInfo\Translation', FVerBufValue, FVerBufLen) then
   Result := GetInfo(S);
end;
{------------------------------------------------------------------------------}
function TgtFileInfo.GetInfo(const aKey: string): string;
begin
  Result := '';
  FVerKey := Format('\StringFileInfo\%.4x%.4x\%s',
    [LoWord(Integer(FVerBufValue^)),
    HiWord(Integer(FVerBufValue^)), aKey]);
  if VerQueryValue(FVerBuf, PChar(FVerKey), FVerBufValue, FVerBufLen) then
    Result := StrPas(PChar(FVerBufValue));
end;
{------------------------------------------------------------------------------}
function TgtFileInfo.AllInfoAsIni: string;
begin
  Result := '';
end;
{------------------------------------------------------------------------------}
function TgtFileInfo.GetFileVersionAsNumeric: Integer;
var
  Temp : string;
  i    : Integer;
  Temp2: string;
begin
  Temp := '';
  Temp2:= '';
  Temp := ReverseString(FFileVersion);
  for i := 1 to Length(Temp) do
  begin
    if Temp[i] <> '.' then
      Temp2 := Temp2 + Temp[i]
    else
      Break;
  end;
  if not TryStrToInt(Trim(ReverseString(Temp2)),Result) then
    Result := 0;
end;
{------------------------------------------------------------------------------}

//Getters - Setters \\
{------------------------------------------------------------------------------}
procedure TgtFileInfo.SetFileName(const Value: string);
begin
  FFileName := Value;
  ParseFile;
end;
{------------------------------------------------------------------------------}

end.
