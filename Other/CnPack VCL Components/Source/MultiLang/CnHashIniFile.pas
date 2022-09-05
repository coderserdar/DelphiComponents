{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnHashIniFile;
{* |<PRE>
================================================================================
* ������ƣ�CnPack
* ��Ԫ���ƣ�CnHashIniFile ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�Circus Monkey
* ��    ע���õ�ԪΪ CnHashIniFile ��ʵ�ֵ�Ԫ��
* ����ƽ̨��EWinXPPro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��v1.0   2004/1/19 by Circus Monkey
*              ���ؼ� TCnHashIniFile �ǻ��ڹ�ϣ�����ġ����� CnHashLangStorage
*              �����ơ�������֧�ֶ�� Sections�����ԱȽϺõļ��� INIFile������
*              ���ԣ�������ļ����ٶȱ� CnHashLangStorage ��������Զ���� IniFile
*              ����ԭ���ǣ����� Sections ����Ϣû�б����� HashMap �С�����ʵ��
*              ���ǳ���С��
* ��֪���⣺- �ر�ʱ����ļ������иı䣬����������ļ������Ǿֲ����¡�
*           - �ڱ���ʱ����������еĿո��У��Լ�;---��ͷ��ע���С�
* ˵    �������Ҫʵ�� Unicode ���ݣ�ֻҪ�滻���е� string �� WideString��Ȼ��
*           ʹ�� TTntStringList �滻 TStringList, ������ TntClasses ���ɡ�
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, IniFiles, CnHashMap;

type
  TCnCustomHashIniSection = class(TObject)
  private
    FSection: string;
    FHashMap: TCnStrToStrHashMap;
  protected
    procedure InitHashMap;
    procedure UpdateFile(AList: TStringList);
    function GetString(Name: string; var Value: string): Boolean;
    procedure SetString(Name, Value: string);
  public
    property Section: string read FSection;
    constructor Create(const Section: string);
    destructor Destroy; override;
  end;

  TCnCustomHashIniFile = class(TObject)
  private
    FFileName: string;
    FUpdated: Boolean;

  protected
    FMapList: TStringList;
    function InitFile: Boolean;
    procedure UpdateFile;
    function GetSection(const Section: string): TCnCustomHashIniSection;
  public
    property FileName: string read FFileName;
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

  TCnHashIniFile = class(TCnCustomHashIniFile)
  public
    function ReadString(const Section, Ident, Default: string): string;
    {* ��ȡһ���ַ���, �� TIniFile.ReadString ����ʹ�÷�����ȫһ�� }
    function ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
    {* ��ȡһ������, �� TIniFile.ReadInteger ����ʹ�÷�����ȫһ�� }
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    {* ��ȡһ��������, �� TIniFile.ReadBool ����ʹ�÷�����ȫһ�� }
    procedure WriteString(const Section, Ident, Value: string);
    {* ����һ���ַ���, �� TIniFile.WriteString ����ʹ�÷�����ȫһ�� }
    procedure WriteInteger(const Section, Ident: string; Value: LongInt);
    {* ����һ������, �� TIniFile.WriteInteger ����ʹ�÷�����ȫһ�� }
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    {* ����һ��������, �� TIniFile.WriteBool ����ʹ�÷�����ȫһ�� }
  end;

implementation

const
  _SCnCRLF = #13#10;
  _SCnBR = '<BR>';
  _DefDelimeter = '.';
  _DefEqual = '=';
  _ListLength = 1024;
  _IncSize = 2;

function TCnCustomHashIniSection.GetString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if Assigned(FHashMap) then
  begin
    Result := FHashMap.Find(Name, Value);
    if Result then
      Value := StringReplace(Value, _SCnBR, _SCnCRLF, [rfReplaceAll, rfIgnoreCase])
    else
      Value := '';
  end;
end;

procedure TCnCustomHashIniSection.SetString(Name, Value: string);
var
  myValue: string;
begin
  if Assigned(FHashMap) then
  begin
    if FHashMap.Find(Name, myValue) then
      FHashMap.Delete(Name);
    FHashMap.Add(Name, StringReplace(Value, _SCnCRLF, _SCnBR, [rfReplaceAll, rfIgnoreCase]));
  end;
end;

procedure TCnCustomHashIniSection.InitHashMap;
begin
  if Assigned(FHashMap) then
    FreeAndNil(FHashMap);
  FHashMap := TCnStrToStrHashMap.Create(_ListLength, _IncSize);
end;

procedure TCnCustomHashIniSection.UpdateFile(AList: TStringList);
var
  Key, Value: string;
  List: TStringList;
begin
  if Assigned(FHashMap) then
  begin
    List := TStringList.Create;
    try
      FHashMap.StartEnum;
      while FHashMap.GetNext(Key, Value) do
        if Key = '' then Continue else
        List.Add(Key + _DefEqual + Value);

      List.Sort;
      List.Insert(0, '[' + FSection + ']');
      AList.AddStrings(List);
    finally
      List.Free;
    end;
  end;
end;

// C: 2003-04-19  M: 2004-01-19
constructor TCnCustomHashIniSection.Create(const Section: string);
begin
  FSection := Section;
  InitHashMap;
end;

// C: 2003-04-19  M: 2004-01-19
destructor TCnCustomHashIniSection.Destroy;
begin
  // update file
  if Assigned(FHashMap) then FHashMap.Free;
  inherited;
end;

{**************************** TCnCustomHashIniFile ****************************}

constructor TCnCustomHashIniFile.Create(const FileName: string);
begin
  FFileName := FileName;
  FMapList := TStringList.Create;
  InitFile;
end;

destructor TCnCustomHashIniFile.Destroy;
begin
  UpdateFile;
//  if Assigned(FHashMap) then FHashMap.Free;
  FMapList.Free;
  inherited;
end;

{$WARNINGS OFF}
function TCnCustomHashIniFile.InitFile: Boolean;
var
  List: TStringList;
  Section: TCnCustomHashIniSection;
  i, EPos, Len: Integer;
  S, SectionN: string;
begin
  Result := True;
  //
  List := TStringList.Create;
  try
    List.LoadFromFile(FFileName);
  except
    Result := False;
    List.Free;
    Exit;
  end;

  for i := 0 to List.Count - 1 do
  begin
    S := Trim(List[i]);
    // Check, if this line contains a section tag.
    Len := Length(S);
    if (Len > 1) and (S[1] = '[') then
    begin
      SectionN := S;
      Delete(SectionN, 1, 1);         // remove '['
      EPos := Pos(']', SectionN);
      if EPos > 0 then
        Delete(SectionN, EPos, Len - EPos);
      SectionN := Trim(SectionN);

      Section := TCnCustomHashIniSection.Create(SectionN);
      FMapList.AddObject(SectionN, Section);
      Continue;
    end;

    if FMapList.Count = 0 then Continue;
    EPos := Pos(_DefEqual, S);

    if EPos > 0 then
      Section.SetString(Copy(S, 1, EPos - 1), Copy(S, EPos + 1, Length(S) - EPos)) else
//      FHashMap.Add(S, '');//Copy(S, 1, EPos - 1), '');

  end;
  List.Free;
end;
{$WARNINGS ON}

procedure TCnCustomHashIniFile.UpdateFile;
var
  Section: TCnCustomHashIniSection;
  List: TStringList;
begin
  if not FUpdated then Exit;

  List := TStringList.Create;
  try
    while (FMapList.Count > 0) do
    begin
      Section := TCnCustomHashIniSection(FMapList.Objects[0]);
      Section.UpdateFile(List);
      FMapList.Delete(0);
      Section.Free;
    end;
    List.SaveToFile(FFileName);
  finally
    List.Free;
  end;
end;

function TCnCustomHashIniFile.GetSection(const Section: string): TCnCustomHashIniSection;
var
  ID: Integer;
begin
  ID := FMapList.IndexOf(Section);
  if ID <> -1 then
    Result := TCnCustomHashIniSection(FMapList.Objects[ID])
  else
    Result := nil;
end;

function TCnHashIniFile.ReadString(const Section, Ident, Default: string): string;
var
  aSection: TCnCustomHashIniSection;
begin
  aSection := GetSection(Section);
  if aSection <> nil then
  begin
    if not aSection.GetString(Ident, Result) then
      Result := Default;
  end;
end;

function TCnHashIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
var
  IntStrW: string;
begin
  IntStrW := ReadString(Section, Ident, '');
  if (Length(IntStrW) > 2) and (IntStrW[1] = '0') and
    ((IntStrW[2] = 'X') or (IntStrW[2] = 'x')) then
    IntStrW := '$' + Copy(IntStrW, 3, Maxint);
  Result := StrToIntDef(IntStrW, Default);
end;

function TCnHashIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

procedure TCnHashIniFile.WriteString(const Section, Ident, Value: string);
var
  aSection: TCnCustomHashIniSection;
begin
  aSection := GetSection(Section);
  if aSection <> nil then
  begin
    aSection.SetString(Ident, Value);
    FUpdated := True;  // if update = False then skip UpdateFile
  end;
end;

procedure TCnHashIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

procedure TCnHashIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

end.
