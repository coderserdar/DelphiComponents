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

unit CnIniLangFileStorage;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�Ini ����洢�����Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫʵ���� Ini ����洢����࣬�ڲ�ʹ���� HashMap
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.11.19 V1.1
*               Efeis ���� Ini ������������
*           2003.08.20 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, IniFiles, Dialogs, FileCtrl, CnCommon,
  CnConsts, CnIniStrUtils, CnWideStrings, CnLangStorage, CnHashLangStorage;

const
  SCnGlobalSectionName = SystemNamePrefix + 'Global';
  SCnStringsSectionName = SystemNamePrefix + 'Strings';

type
  TCnCustomIniLangFileStorage = class(TCnCustomHashLangStorage)
  private
    FIniFile: TIniFile;
  protected
    procedure InternalInit; override;
    procedure CreateCurrentLanguage; override;
    procedure InitFromAFile(const AFileName: WideString); override;
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageFileExt: WideString; override;
    {* ���ض������ļ�����չ��.INI }

    function IsLanguageFile(const FileName: WideString): Boolean; override;
    {* �ж�һ�ļ��Ƿ�Ϸ��������ļ� }
    function LoadCurrentLanguage: Boolean; override;
    {* �� Ini �ļ������뵱ǰ������Ŀ��Ϊ�����ִ���׼�� }
    procedure SaveCurrentLanguage; override;
    {* ���浱ǰ�����ļ� }

  published
    property StorageMode;
    {* �����ļ��洢ģʽ����Ŀ¼�洢���ǰ��ļ��洢 }
    property LanguagePath;
    {* �����ļ��洢��ͳһ·�� }
    property FileName;
    {* ���ļ��洢ʱ��ͳһ�����ļ��� }
    property Languages;
    {* ���Զ����б� }
    property AutoDetect;
    {* LanguagePath �ı�ʱ�Ƿ��Զ�������� }    
  end;

  TCnIniLangFileStorage = class(TCnCustomIniLangFileStorage)
  end;

implementation

uses
  CnLangConsts;

{ TCnCustomIniLangFileStorage}

constructor TCnCustomIniLangFileStorage.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCnCustomIniLangFileStorage.Destroy;
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  inherited;
end;

procedure TCnCustomIniLangFileStorage.CreateCurrentLanguage;
begin

end;

class function TCnCustomIniLangFileStorage.GetLanguageFileExt: WideString;
begin
  Result := '.ini';
end;

procedure TCnCustomIniLangFileStorage.InternalInit;
begin
  inherited;

end;

function TCnCustomIniLangFileStorage.IsLanguageFile(const FileName: WideString): Boolean;
var
  IniFile: TIniFile;
begin
  Result := True;
  IniFile := TIniFile.Create(FileName);
  try
    if not IniFile.SectionExists(SCnGlobalSectionName) then
      Result := False
    else if not IniFile.ValueExists(SCnGlobalSectionName, SystemNamePrefix
      + SCnLanguageID) then
        Result := False;
  finally
    IniFile.Free;
  end;
end;

function TCnCustomIniLangFileStorage.LoadCurrentLanguage: Boolean;
var
  S: string;
  Sections, Lines: TStrings;
  I, J: Integer;
begin
  Result := True;
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);

  //Added by Efeis on 2008-11-18 û�����л�����ڸ����еĹ��������У�Ӧ����Ҫ��ʼ��һ�µ�
  InitHashMap;

  try
    // ������������ֵ��������ļ��洢��Ŀ¼����浽��Ŀ¼��
    if (csDesigning in ComponentState) and (LanguagePath = '') and (DesignLangPath <> '') then
      S := IncludeTrailingBackslash(DesignLangPath) + GetCurrentLanguageFileName
    else
      S := IncludeTrailingBackslash(LanguagePath) + GetCurrentLanguageFileName;

    if not ForceDirectories(_CnExtractFilePath(S)) then
      raise ELanguageStorageError.Create(SCnCanNotCreateDir + _CnExtractFilePath(S));

    FIniFile := TIniFile.Create(S);
    Sections := TStringList.Create;
    Lines := TStringList.Create;
    FIniFile.ReadSections(Sections);

    try
      for I := 0 to Sections.Count - 1 do
      begin
        if (Sections[I] = SCnGlobalSectionName) or
          (Sections[I] = SCnStringsSectionName) then // ���ڲ�������������
        begin
          FIniFile.ReadSection(Sections[I], Lines);
          for J := 0 to Lines.Count - 1 do
          begin
            //Modified by Efeis on 2008-11-18 ԭ���߽�ReadSection�����˰ɣ��ҳ���Ҳ�ܸ���д����Ӱ��
            AddStringToHashMap(Lines[J], FIniFile.ReadString(Sections[I], Lines[J], ''));
          end;
        end
        else // ����ͨ�����
        begin
          FIniFile.ReadSection(Sections[I], Lines);
          for J := 0 to Lines.Count - 1 do
          begin
            //Modified by Efeis on 2008-11-18 ͬ�ϣ���ReadSection�Ĵ�����⼰IniFile��ʹ������
            AddStringToHashMap(Sections[I] + DefDelimeter + Lines[J], FIniFile.ReadString(Sections[I], Lines[J], ''));
          end;
        end;
      end;
    finally
      Sections.Free;
      Lines.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TCnCustomIniLangFileStorage.SaveCurrentLanguage;
var
  Sections, List: TStringList;
  Key, Value, Sec: WideString;
  I, EPos: Integer;
begin
  if Assigned(FIniFile) then
  begin
    Sections := TStringList.Create;
    try
      FIniFile.ReadSections(Sections);
      for I := 0 to Sections.Count - 1 do
        FIniFile.EraseSection(Sections[I]);
    finally
      Sections.Free;
    end;

    List := TStringList.Create;
    HashMap.StartEnum;
    while HashMap.GetNext(Key, Value) do
      List.Add(Key + DefEqual + Value);
    List.Sort;

    for I := 0 to List.Count - 1 do
    begin
      if Pos(SystemNamePrefix, List[I]) = 1 then // �жϵ�һ���ǲ��Ǹ�̾��
      begin
        Sec := SCnGlobalSectionName;
      end
      else  // ���ж����޵��
      begin
        EPos := Pos(DefDelimeter, List[I]);
        if EPos > 0 then  // �е�ţ�ȡ����һ�����ǰ���� Section ����
        begin
          Sec := Copy(List[I], 1, EPos - 1);
          List[I] := Copy(List[I], EPos + 1, MaxInt);
        end
        else
          Sec := SCnStringsSectionName; // �޵�ţ����ַ���
      end;

      // ���
      EPos := Pos(DefEqual, List[I]);
      if EPos > 0 then // �еȺ�
      begin
        Key := Copy(List[I], 1, EPos - 1);
        Value := Copy(List[I], EPos + 1, MaxInt);
      end
      else
      begin
        Key := List[I];
        Value := '';
      end;
      FIniFile.WriteString(Sec, Key, Value);
    end;
    FIniFile.UpdateFile;
  end;
end;

procedure TCnCustomIniLangFileStorage.InitFromAFile(const AFileName: WideString);
begin
  // ��һ�����ļ�������������
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);

  with Languages.Add do
  begin
    LanguageFileName := _CnExtractFileName(_CnChangeFileExt(AFileName, ''));

    FIniFile := TIniFile.Create(AFileName);
    try
      LanguageID := StrToIntDef(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnLanguageID, ''), 0);
    except
      LanguageID := 0;
    end;

    if LanguageID <> 0 then
    begin
      LanguageName := FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnLanguageName, '');
      Author := FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnAuthor, '');
      AuthorEmail := FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnAuthorEmail, '');
      if FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnDefaultFont, '') <> '' then
        StringToFont(FIniFile.ReadString(SCnGlobalSectionName, SystemNamePrefix + SCnDefaultFont, ''), DefaultFont);
    end
    else
    begin
      Self.FCurrentLanguageIndex := -1;
      Languages.Delete(Index);
    end;
  end;
end;

procedure TCnCustomIniLangFileStorage.GetComponentInfo(var AName, Author,
  Email, Comment: string);
begin
  AName := SCnIniLangStorageName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnIniLangStorageComment;
end;

end.
