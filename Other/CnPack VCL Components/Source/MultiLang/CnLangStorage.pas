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

unit CnLangStorage;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�������洢������൥Ԫ
* ��Ԫ���ߣ�CnPack������ ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫʵ���˶�����Ĵ洢�������
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2004.10.23 V1.2
*               �޸ĳ�ʼ���ļ��Ĵ���ʽ�����Ӷ�Ŀ¼������
*           2003.12.13 V1.1
*               �� DefaultFont ��������Դ�ƶ��� LanguageItem ��
*           2003.08.20 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, FileCtrl, CnCommon,
  CnConsts, CnClasses, CnLangCollection, CnLangConsts, CnIniStrUtils;

{$IFDEF Linux}
  {$I QLangIDs.inc}
{$ENDIF}

const
  DefDelimeter        = '.';
  DefEqual            = '=';
  
  SystemNamePrefix    = '!';
  SCnLanguageID       = 'LanguageID';
  SCnLanguageName     = 'LanguageName';
  SCnAuthor           = 'TranslationAuthor';
  SCnAuthorEmail      = 'TranslationAuthorEmail';
  SCnDefaultFont      = 'TranslationDefaultFont';
  SCnSize             = 'Size';
  SCnName             = 'Name';
  SCnCharset          = 'Charset';

  SCnControlFont      = SystemNamePrefix + 'Font';

type
  TLanguageChangeEvent = procedure(Sender: TObject; ALanguageIndex: Integer)
    of object;
  TLanguageChangingEvent = procedure(Sender: TObject; ALanguageIndex: Integer;
    var AllowChange: Boolean) of object;

  ELanguageStorageError = class(Exception)
  end;

  ICnLangStringIterator = interface(IUnknown)
  {* ���ȫ���ַ����� Iterator �ӿڶ��� }
    ['{247CB225-2257-41C0-87F8-F43834E2966F}']
    procedure StartIterate(const FrontPattern: WideString = '');
    procedure Previous;
    procedure Next;
    procedure EndIterate;
    procedure GetCurrentKeyValue(var Key: WideString; var Value: WideString);
    function GetCurrentString: WideString;
    function GetEof: Boolean;
    function GetBof: Boolean;
    
    property Eof: Boolean read GetEof;
    property Bof: Boolean read GetBof;
  end;

  TCnCustomLangStorage = class(TCnComponent)
  private
    FDefaultFont: TFont;
    FFontInited: Boolean;
    FLanguages: TCnLanguageCollection;
    FOnLanguageChanged: TLanguageChangeEvent;
    FOnLanguageChanging: TLanguageChangingEvent;
    function GetCurrentLanguage: TCnLanguageItem;
    function GetLanguageCount: Integer;
    procedure SetCurrentLanguageIndex(Value: Integer);
    procedure SetLanguages(Value: TCnLanguageCollection);
  protected
    FCurrentLanguageIndex: Integer;
    FDefaultLanguageID: Integer;
    procedure DoLanguageChanged(ALanguageIndex: Integer); virtual;
    procedure DoLanguageChanging(ALanguageIndex: Integer;
      var AllowChange: Boolean); virtual;

    function GetAuthor(var Value: WideString): Boolean;

    function GetAuthorEmail(var Value: WideString): Boolean;

    function GetDefaultFont(const Value: TFont): Boolean;
    
    function GetLanguageID(var Value: LongWord): Boolean;
    
    procedure InternalInit; virtual; abstract;
    {* ���󷽷����Ӵ洢�����г�ʼ������������Ŀ }
    
    procedure CreateCurrentLanguage; virtual; abstract;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure AddLanguage(ALanguageID: LongWord);
    {* ����һ������ }
    function GetString(Name: WideString; var Value: WideString): Boolean; virtual;
      abstract;
    {* ���󷽷�����÷����ִ� }
    procedure GetNamesList(List: TStrings); virtual; abstract;
    {* ���󷽷�����õ�ǰ���Ե����з�����Ŀ�����б� }
    procedure ClearCurrentLanguage; virtual; abstract;
    {* ���󷽷���ɾ����ǰ���Ե����з�����Ŀ�б� }
    function LoadCurrentLanguage: Boolean; virtual; abstract;
    {* ���󷽷��������ǴӴ洢���������뵱ǰ������Ŀ��Ϊ�����ִ���׼�� }
    procedure SaveCurrentLanguage; virtual; abstract;
    {* ���󷽷��������Ǳ��浱ǰ������Ŀ���洢������ }
    procedure SetString(Name, Value: WideString); virtual; abstract;
    {* ���󷽷������÷����ִ� }
    function CreateIterator: ICnLangStringIterator; virtual;
    {* ���󷽷�����ñ�������������಻֧�ֱ���������뷵�� nil}
    property CurrentLanguage: TCnLanguageItem read GetCurrentLanguage;
    {* ��ǰ������Ŀ���� }
    property CurrentLanguageIndex: Integer read FCurrentLanguageIndex write
      SetCurrentLanguageIndex;
    {* ��ǰ���Ժ� }
    property DefaultFont: TFont read FDefaultFont; // write SetDefaultFont;
    {* Ĭ�ϵ����� }
    property DefaultLanguageID: Integer read FDefaultLanguageID;
    {* Ĭ�����Ե� ID }
    property FontInited: Boolean read FFontInited write FFontInited;
    {* �����Ƿ��Ѿ���ʼ�� }
    property LanguageCount: Integer read GetLanguageCount;
    {* ������ }
    property Languages: TCnLanguageCollection read FLanguages write
      SetLanguages;
    {* ���������б� }
  published
    property OnLanguageChanged: TLanguageChangeEvent read FOnLanguageChanged
      write FOnLanguageChanged;
    {* ��ǰ���ԺŸı�󴥷� }
    property OnLanguageChanging: TLanguageChangingEvent read FOnLanguageChanging
      write FOnLanguageChanging;
    {* ��ǰ���ԺŸı�ǰ�������ɿ����Ƿ�����ı� }
  end;

  TCnStorageMode = (smByFile, smByDirectory);
  {* �洢���ͣ���ͬһĿ¼�¶��ļ��洢���ǲ�ͬĿ¼�µ�ͬһ�ļ����洢 }

  TCnCustomLangFileStorage = class (TCnCustomLangStorage)
  private
    FLanguagePath: WideString;
    FAutoDetect: Boolean;
    FStorageMode: TCnStorageMode;
    FFileName: WideString;
    FDesignLangPath: WideString;
    FDesignLangFile: WideString;
    procedure SetLanguagePath(Value: WideString);
    procedure SetAutoDetect(const Value: Boolean);
    procedure SetStorageMode(const Value: TCnStorageMode);
    procedure SetFileName(const Value: WideString);
    // LanguagePath Ϊ��ʱ������Ϊ��Ҫ��Ŀ¼
    procedure AdjustLangPath;
    procedure AdjustLangFile;
  protected
    procedure InternalInit; override;
    procedure Loaded; override;
    procedure InitFromAFile(const AFileName: WideString); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDesignLangPath(const aPath: WideString);
    procedure SetDesignLangFile(const aFile: WideString);

    function GetCurrentLanguageFileName: WideString; virtual;
   {* ��õ�ǰ���Ե������ļ�����������չ�� }
    class function GetLanguageFileExt: WideString; virtual;
    {* ���󷽷���������������ļ���ͳһ��չ�� }
    function IsLanguageFile(const FileName: WideString): Boolean; virtual; abstract;
    {* ���󷽷����ж�ĳһ�ļ��Ƿ��ǺϷ��������ļ� }

    property StorageMode: TCnStorageMode read FStorageMode write SetStorageMode;
    {* �����ļ���Ŀ¼�洢���ǰ��ļ��洢 }
    property LanguagePath: WideString read FLanguagePath write SetLanguagePath;
    {* ���������ļ��洢��ͳһĿ¼�� }
    property FileName: WideString read FFileName write SetFileName;
    {* �����ļ���Ŀ¼�洢ʱ���е�ͳһ�ļ��� }
    property AutoDetect: Boolean read FAutoDetect write SetAutoDetect default True;
    {* LanguagePath �ı�ʱ�Ƿ��Զ�������� }

    // ������������������ڵ�ʵ�ʵ� LangPath/File ʹ�ã�
    // ������ LanguagePath/File Ϊ�ն�������Ҫ����Ŀ¼/�ļ��ĳ���
    // ֻ���� Translator ������༭����ֵ�������û�����
    property DesignLangPath: WideString read FDesignLangPath;
    property DesignLangFile: WideString read FDesignLangFile;
  end;

implementation

uses
  CnLangMgr;

//==============================================================================
// TCustomLanguageStorage
//==============================================================================

constructor TCnCustomLangStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultLanguageID := GetSystemDefaultLCID;
  FDefaultFont := TFont.Create;
  FDefaultFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
  FCurrentLanguageIndex := -1;
  FLanguages := TCnLanguageCollection.Create(Self);

  if CnLanguageManager <> nil then
    if CnLanguageManager.LanguageStorage = nil then
      CnLanguageManager.LanguageStorage := Self;
end;

destructor TCnCustomLangStorage.Destroy;
begin
  FreeAndNil(FDefaultFont);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

procedure TCnCustomLangStorage.AddLanguage(ALanguageID: LongWord);
begin
  with Languages.Add do
  begin
    
    LanguageID := ALanguageID;
  end;
  CreateCurrentLanguage;
end;

procedure TCnCustomLangStorage.DoLanguageChanged(ALanguageIndex: Integer);
begin
  if Assigned(FOnLanguageChanged) then
    FOnLanguageChanged(Self, ALanguageIndex);
end;

procedure TCnCustomLangStorage.DoLanguageChanging(ALanguageIndex: Integer;
  var AllowChange: Boolean);
begin
  if Assigned(FOnLanguageChanging) then
    FOnLanguageChanging(Self, ALanguageIndex, AllowChange);
end;

function TCnCustomLangStorage.GetAuthor(var Value: WideString): Boolean;
begin
  Result := GetString(SystemNamePrefix + SCnAuthor, Value);
end;

function TCnCustomLangStorage.GetAuthorEmail(var Value: WideString): Boolean;
begin
  Result := GetString(SystemNamePrefix + SCnAuthorEmail, Value);
end;

function TCnCustomLangStorage.GetCurrentLanguage: TCnLanguageItem;
begin
  if CurrentLanguageIndex <> -1 then
    Result := TCnLanguageItem(Languages.Items[CurrentLanguageIndex])
  else
    Result := nil;
end;

function TCnCustomLangStorage.GetDefaultFont(const Value: TFont): Boolean;
var
  S: WideString;
begin
  S := '';
  if GetString(SystemNamePrefix + SCnDefaultFont, S) then
    StringToFont(S, Value);
  Result := (S <> '');
end;

function TCnCustomLangStorage.GetLanguageCount: Integer;
begin
  Result := FLanguages.Count;
end;

function TCnCustomLangStorage.GetLanguageID(var Value: LongWord): Boolean;
var
  S: WideString;
begin
  Result := GetString(SystemNamePrefix + SCnLanguageID, S);
  if Result then
    Value := StrToIntDef(S, 0);
end;

procedure TCnCustomLangStorage.SetCurrentLanguageIndex(Value: Integer);
var
  AllowChange: Boolean;
begin
  if (Value >= 0) and (Value < LanguageCount) then
  begin
    //if Value <> FCurrentLanguageIndex then
    begin
      AllowChange := True;
      DoLanguageChanging(Value, AllowChange);
      if not AllowChange then
        Exit;

      FCurrentLanguageIndex := Value;
      if LoadCurrentLanguage then
      begin
        FDefaultFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
        GetDefaultFont(FDefaultFont);
        FontInited := True;
        DoLanguageChanged(Value);
      end;
    end;
  end;
end;

procedure TCnCustomLangStorage.SetLanguages(Value: TCnLanguageCollection);
begin
  FLanguages.Assign(Value);
end;

//==============================================================================
// TCustomLanguageFileStorage
//==============================================================================

procedure TCnCustomLangFileStorage.AdjustLangPath;
begin
  FDesignLangPath := FLanguagePath;
  if FLanguagePath = '' then
  begin
    if not (csDesigning in ComponentState) then
    begin
      // �����ڣ�ֻ�в��ÿ�ִ���ļ�������Ŀ¼
      FDesignLangPath := IncludeTrailingBackslash(_CnExtractFilePath(Application.ExeName));
    end;
  end;
end;

procedure TCnCustomLangFileStorage.AdjustLangFile;
begin
  FDesignLangFile := FFileName;
  if FFileName = '' then
  begin
    if not (csDesigning in ComponentState) then
    begin
      // �����ڣ�ֻ�в��ÿ�ִ���ļ����ļ������Լ�����չ��
      FFileName := _CnChangeFileExt(_CnExtractFileName(Application.ExeName), GetLanguageFileExt);
    end;
  end;
end;

constructor TCnCustomLangFileStorage.Create(AOwner: TComponent);
begin
  inherited;
  FAutoDetect := True;
end;

destructor TCnCustomLangFileStorage.Destroy;
begin
  inherited;
end;

function TCnCustomLangFileStorage.GetCurrentLanguageFileName: WideString;
begin
  if Assigned(CurrentLanguage) then
  begin
    if FStorageMode = smByFile then
      Result := CurrentLanguage.LanguageFileName + GetLanguageFileExt
    else if (FFileName = '') and (FDesignLangFile <> '') then // ����ڲ�������༭�����õĹ����ļ���
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + _CnChangeFileExt(FDesignLangFile, GetLanguageFileExt)
    else if Pos('.', FFileName) > 0 then
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + FFileName
    else
      Result := IncludeTrailingBackslash(CurrentLanguage.LanguageDirName) + _CnChangeFileExt(FFileName, GetLanguageFileExt);
  end
  else
    Result := '';
end;

// ���������ļ�����չ����Ĭ��Ϊtxt������һ�����ء�
class function TCnCustomLangFileStorage.GetLanguageFileExt: WideString;
begin
  Result := '.txt';
end;

procedure TCnCustomLangFileStorage.InitFromAFile(const AFileName: WideString);
var
  AStr: WideString;
  AID: LongWord;
begin
  // ����û�ã����Բ�д
  with Languages.Add do
  begin
    LanguageFileName := _CnExtractFileName(_CnChangeFileExt(AFileName, ''));
    Self.CurrentLanguageIndex := Index;
    try
      if GetLanguageID(AID) then
        LanguageID := AID;
      if GetString(SystemNamePrefix + SCnLanguageName, AStr) then
        LanguageName := AStr;
      if GetString(SystemNamePrefix + SCnAuthor, AStr) then
        Author := AStr;
      if GetString(SystemNamePrefix + SCnAuthorEmail, AStr) then
        AuthorEmail := AStr;
      if GetString(SystemNamePrefix + SCnDefaultFont, AStr) then
      begin
        StringToFont(AStr, DefaultFont);
      end;
    except
      Self.FCurrentLanguageIndex := -1;
      Free;
    end;
  end;
end;

procedure TCnCustomLangFileStorage.InternalInit;
var
  Sr: TSearchRec;
  ActualPath,  AFileName, ASearchPath: WideString;
begin
  Languages.BeginUpdate;
  try
    Languages.Clear;
    Self.FCurrentLanguageIndex := -1;

    ActualPath := FLanguagePath;
    if (csDesigning in ComponentState) then
    begin
      if (ActualPath = '') and (FDesignLangPath <> '') then
      begin
        ActualPath := FDesignLangPath;
      end;
    end
    else if ActualPath = '' then
      ActualPath := _CnExtractFileDir(Application.ExeName);

    if ActualPath = '' then
      Exit;
      
    ActualPath := IncludeTrailingBackslash(ActualPath);

    if FStorageMode = smByFile then
    begin
      ASearchPath := ActualPath + '*' + GetLanguageFileExt;
      if FindFirst(ASearchPath, faAnyFile, Sr) = 0 then
      begin
        repeat
          AFileName := ActualPath + Sr.Name;
          if IsLanguageFile(AFileName) then
            InitFromAFile(AFileName);
        until FindNext(Sr) <> 0;
        FindClose(Sr);
      end;
    end
    else
    begin
      // �������δ���� FileName�����޷��Զ��������������ļ�
      if FFileName = '' then
        Exit;

      ASearchPath := ActualPath + '*';
      if FindFirst(ASearchPath, faDirectory, Sr) = 0 then
      begin
        repeat
          if (Sr.Name = '.') or (Sr.Name = '..') or (Sr.Attr and faDirectory = 0) then
            Continue;

          AFileName := ActualPath + Sr.Name + '\' + _CnChangeFileExt(FFileName, GetLanguageFileExt);

          if FileExists(AFileName) and IsLanguageFile(AFileName) then
            InitFromAFile(AFileName);

        until FindNext(Sr) <> 0;
        FindClose(Sr);
      end;
    end;
  finally
    Languages.EndUpdate;
  end;
end;

// ��������δ����·�����Կ�ִ���ļ�����·��Ϊ׼��δ�����ļ������Կ�ִ���ļ���Ϊ׼
procedure TCnCustomLangFileStorage.Loaded;
begin
  inherited;
  if csDesigning in ComponentState then Exit;

  if Self.FLanguagePath = '' then
    Self.FLanguagePath := _CnExtractFilePath(Application.ExeName);
  AdjustLangPath;
  if Self.FFileName = '' then
    Self.FFileName := _CnChangeFileExt(_CnExtractFileName(Application.ExeName), '');
  AdjustLangFile;
end;

procedure TCnCustomLangFileStorage.SetAutoDetect(const Value: Boolean);
begin
  FAutoDetect := Value;
  if Value then
  begin
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

procedure TCnCustomLangFileStorage.SetFileName(const Value: WideString);
begin
  FFileName := Value;
  if FAutoDetect and (StorageMode = smByDirectory) then
  begin
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

procedure TCnCustomLangFileStorage.SetLanguagePath(Value: WideString);
begin
  if FLanguagePath <> Value then
  begin
    if Value <> '' then
      FLanguagePath := IncludeTrailingBackslash(Value)
    else
      FLanguagePath := Value;

    if FAutoDetect then
    begin
      AdjustLangPath;
      AdjustLangFile;
      InternalInit;
    end;
  end;
end;

procedure TCnCustomLangFileStorage.SetDesignLangPath(const aPath: WideString);
begin
  if csDesigning in ComponentState then
    FDesignLangPath := aPath;
end;

procedure TCnCustomLangFileStorage.SetDesignLangFile(const aFile: WideString);
begin
  if csDesigning in ComponentState then
    FDesignLangFile := aFile;
end;

procedure TCnCustomLangFileStorage.SetStorageMode(
  const Value: TCnStorageMode);
begin
  if (FStorageMode <> Value) or (csLoading in ComponentState) then
  begin
    FStorageMode := Value;
    AdjustLangPath;
    AdjustLangFile;
    InternalInit;
  end;
end;

function TCnCustomLangStorage.CreateIterator: ICnLangStringIterator;
begin
  Result := nil;
end;

procedure TCnCustomLangStorage.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
// ��������Ϣ
end;

end.
