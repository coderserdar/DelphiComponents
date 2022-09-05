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

unit CnIni;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���չ��INI���ʵ�Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ��дʱ�ο��� RxLib 2.75 �е� RxIni.pas
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.10.20 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF DELPHI}
{$DEFINE SUPPORT_ZLIB}
{$ENDIF}

uses
  Windows, Classes, SysUtils, TypInfo, Forms, IniFiles, Graphics,
  CnIniStrUtils, CnStream{$IFDEF SUPPORT_ZLIB}, ZLib{$ENDIF};

type

//==============================================================================
// ��չ�� INI ������
//==============================================================================
   
{ TCnIniFile }

  TCnIniFile = class(TCustomIniFile)
  {* ��չ�� INI �����࣬ʹ�� Wrap ģʽ�� TCustomIniFile ������չ����������������
     �ȿɵ���ͨ���ļ��� INI ��������ֿɽ�����Ϊ���� TCustomIniFile ����İ�װ��
     �ǽ�����չ�Ĳ�����}
  private
    FIni: TCustomIniFile;
    FOwned: Boolean;
    function GetFileName: string;
    function IsBooleanType(PInfo: PTypeInfo): Boolean;
    function IsBoolType(PInfo: PTypeInfo): Boolean;
    function IsColorType(PInfo: PTypeInfo): Boolean;
    function IsDateTimeType(PInfo: PTypeInfo): Boolean;
  protected
    property Owned: Boolean read FOwned;
    property Ini: TCustomIniFile read FIni;
  public
    constructor Create(AIni: TCustomIniFile; AOwned: Boolean = False); overload;
    {* ��װ��������ʹ�øù���������ʵ���������е� TCustomIniFile ������й�����չ
       ��������з�����ת��ԭ INI ������ִ��
     |<PRE>
       AIni: TCustomIniFile    - ����װ�� INI ����
       AOwned: Boolean         - �ڸö����ͷ�ʱ�Ƿ�ͬʱ�ͷű���װ�� INI ����
     |</PRE>}
    constructor Create(const FileName: string; MemIniFile: Boolean = True); overload;
    {* ��ͨ INI �ļ���������ʹ�øù���������ʵ������ʵ������ͨ�� INI ����ʹ�á�
     |<PRE>
       FileName: string        - INI �ļ���
       MemIniFile: Boolean     - �Ƿ�ʹ���ڴ滺�巽ʽ���� INI�����ڲ�ʹ�� TMemIniFile ����
     |</PRE>}
    destructor Destroy; override;
    
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
    {* ��ȡ��ɫ}
    procedure WriteColor(const Section, Ident: string; Value: TColor);
    {* д����ɫ}
    function ReadFont(const Section, Ident: string; Font: TFont): TFont;
    {* ��ȡ����}
    procedure WriteFont(const Section, Ident: string; Font: TFont);
    {* д������}
    function ReadRect(const Section, Ident: string; const Default: TRect): TRect;
    {* ��ȡ Rect}
    procedure WriteRect(const Section, Ident: string; const Value: TRect);
    {* д�� Rect}
    function ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
    {* ��ȡ Point}
    procedure WritePoint(const Section, Ident: string; const Value: TPoint);
    {* д�� Point}
    function ReadStrings(const Section, Ident: string; Strings: TStrings): TStrings; overload;
    {* ��һ���ı��ж�ȡ�ַ����б�}
    function ReadStrings(const Section: string; Strings: TStrings): TStrings; overload;
    {* �ӵ����Ľ��ж�ȡ�ַ����б�}
    procedure WriteStrings(const Section, Ident: string; Strings: TStrings); overload;
    {* д���ַ����б�һ���ı���}
    procedure WriteStrings(const Section: string; Strings: TStrings); overload;
    {* д���ַ����б������Ľ���}
    procedure ReadObject(const Section: string; AObject: TObject);
    {* ��ȡ���� published ���ԣ�������������}
    procedure WriteObject(const Section: string; AObject: TObject; NoDef: Boolean = True);
    {* д����󵽵����Ľ��У������������ԣ�֧�� TFont �� TStrings ���͡�NoDef ָ���Ƿ񲻱���Ĭ��ֵ}
    property FileName: string read GetFileName;
    {* INI �ļ���}
  end;

//==============================================================================
// ֧���������� IniFile ��
//==============================================================================
   
{ TCnStreamIniFile }

  TCnStreamIniFile = class (TMemIniFile)
  {* ֧���������� IniFile �࣬�ṩ�� LoadFromStream��SaveToStream ��������ж�ȡ
     Ini ���ݡ� }
  private
    FFileName: string;
    FInitData: string;
  protected

  public
    constructor Create(const FileName: string = '');
    {* �๹����������Ϊ INI �ļ�����������ļ���������Զ�װ���ļ� }
    destructor Destroy; override;
    {* �������� }
    function LoadFromFile(const FileName: string): Boolean;
    {* ���ļ���װ�� INI ���� }
    function LoadFromStream(Stream: TStream): Boolean; virtual;
    {* ������װ�� INI ���� }
    function SaveToFile(const FileName: string): Boolean;
    {* ���� INI ���ݵ��ļ� } 
    function SaveToStream(Stream: TStream): Boolean; virtual;
    {* ���� INI ���ݵ��� }
    procedure UpdateFile; override;
    {* ���µ�ǰ INI ���ݵ��ļ� }

    property FileName: string read FFileName;
    {* ��������ʱ���ݵ��ļ�����ֻ������ }
  end;

//==============================================================================
// ֧�����ݼ��ܼ��������� IniFile ����
//==============================================================================

{ TCnBaseEncryptIniFile }

  TCnBaseEncryptIniFile = class (TCnStreamIniFile)
  {* ֧�����ݼ��ܼ��������� IniFile ������࣬����� INI ���ݽ��м��ܡ� }
  private
  {$IFDEF SUPPORT_ZLIB}
    FUseZLib: Boolean;
  {$ENDIF}
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; virtual; abstract;
  public
    constructor Create(const FileName: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    function LoadFromStream(Stream: TStream): Boolean; override;
    {* ������װ�� INI ���ݣ����е����ݽ��Զ����� }
    function SaveToStream(Stream: TStream): Boolean; override;
    {* ���� INI ���ݵ��������е����ݽ��Զ����� }
  {$IFDEF SUPPORT_ZLIB}
    property UseZLib: Boolean read FUseZLib;
  {$ENDIF}
    {* �Ƿ�ʹ�� ZLib ѹ�� }
  end;

//==============================================================================
// ֧������ Xor ���ܼ��������� IniFile ��
//==============================================================================

{ TCnXorIniFile }

  TCnXorIniFile = class (TCnBaseEncryptIniFile)
  {* ֧������ Xor ���ܼ��������� IniFile �࣬����� INI ���ݽ��� Xor ���ܡ� }
  private
    FXorStr: string;
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; override;
  public
    constructor Create(const FileName: string; const XorStr: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    {* �๹������
     |<PRE>
       FileName: string     - INI �ļ���������ļ����ڽ��Զ�����
       XorStr: string       - ���� Xor �������ַ���
       UseZLib: string      - �Ƿ�ʹ�� ZLib ѹ��
     |</PRE>}
  end;

//==============================================================================
// ֧�����ݼ��ܼ��������� IniFile ��
//==============================================================================

{ TCnEncryptIniFile }

  TCnEncryptIniFile = class (TCnBaseEncryptIniFile)
  {* ֧�����ݼ��ܼ��������� IniFile �࣬����� INI ���ݽ��л����ַ�ӳ���ļ��ܡ� }
  private
    FSeedStr: string;
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; override;
  public
    constructor Create(const FileName: string; const SeedStr: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    {* �๹������
     |<PRE>
       FileName: string     - INI �ļ���������ļ����ڽ��Զ�����
       SeedStr: string      - ���ڼ��ܵ��ַ���
       UseZLib: string      - �Ƿ�ʹ�� ZLib ѹ��
     |</PRE>}
  end;

implementation

uses
  CnCommon;

function IsDefaultPropertyValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result :=  (Default <> LongInt($80000000)) and (Value = Default);
  end;
  
  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    Result := Value = 0;;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

begin
  Result := False;
  if (PropInfo^.GetProc <> nil) and
     (PropInfo^.SetProc <> nil) then
  begin
    PropType := PropInfo^.PropType^;
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkString, tkLString, tkWString:
        Result := IsDefaultStrProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
    end;
  end;
end;

//==============================================================================
// ��չ�� INI ������
//==============================================================================
   
{ TCnIniFile }

constructor TCnIniFile.Create(AIni: TCustomIniFile; AOwned: Boolean);
begin
  inherited Create('');
  Assert(Assigned(AIni));
  FIni := AIni;
  FOwned := AOwned;
end;

constructor TCnIniFile.Create(const FileName: string; MemIniFile: Boolean);
begin
  if MemIniFile then
    Create(TMemIniFile.Create(FileName), True)
  else
    Create(TIniFile.Create(FileName), True);
end;

destructor TCnIniFile.Destroy;
begin
  if FOwned then
    FreeAndNil(FIni);
  inherited;
end;

//------------------------------------------------------------------------------
// ��չ�� INI ���ʷ���
//------------------------------------------------------------------------------
   
function TCnIniFile.ReadColor(const Section, Ident: string;
  Default: TColor): TColor;
begin
  try
    Result := StringToColor(ReadString(Section, Ident,
      ColorToString(Default)));
  except
    Result := Default;
  end;
end;

procedure TCnIniFile.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section, Ident, ColorToString(Value));
end;

function TCnIniFile.ReadRect(const Section, Ident: string; const Default: TRect): TRect;
begin
  Result := StrToRect(ReadString(Section, Ident, RectToStr(Default)), Default);
end;

procedure TCnIniFile.WriteRect(const Section, Ident: string; const Value: TRect);
begin
  WriteString(Section, Ident, RectToStr(Value));
end;

function TCnIniFile.ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
begin
  Result := StrToPoint(ReadString(Section, Ident, PointToStr(Default)), Default);
end;

procedure TCnIniFile.WritePoint(const Section, Ident: string; const Value: TPoint);
begin
  WriteString(Section, Ident, PointToStr(Value));
end;

function TCnIniFile.ReadFont(const Section, Ident: string; Font: TFont): TFont;
begin
  Result := Font;
  try
    StringToFont(ReadString(Section, Ident, FontToString(Font)), Result);
  except
    { do nothing, ignore any exceptions }
  end;
end;

procedure TCnIniFile.WriteFont(const Section, Ident: string; Font: TFont);
begin
  WriteString(Section, Ident, FontToString(Font));
end;

function TCnIniFile.ReadStrings(const Section, Ident: string;
  Strings: TStrings): TStrings;
begin
  Result := Strings;
  Strings.Text := StrToLines(ReadString(Section, Ident, LinesToStr(Strings.Text)));
end;

function TCnIniFile.ReadStrings(const Section: string; Strings: TStrings): TStrings;
begin
  Result := Strings;
  if SectionExists(Section) then
    ReadStringsFromIni(Self, Section, Result);
end;

procedure TCnIniFile.WriteStrings(const Section, Ident: string; Strings: TStrings);
begin
  WriteString(Section, Ident, LinesToStr(Strings.Text));
end;

procedure TCnIniFile.WriteStrings(const Section: string; Strings: TStrings);
begin
  WriteStringsToIni(Self, Section, Strings);
end;

function TCnIniFile.IsColorType(PInfo: PTypeInfo): Boolean;
begin
  Result := PInfo = TypeInfo(TColor);
end;

function TCnIniFile.IsBoolType(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo^.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.MinValue < 0); // Longbool/wordbool/bytebool
end;

function TCnIniFile.IsBooleanType(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.BaseType^ = TypeInfo(Boolean));
end;

function TCnIniFile.IsDateTimeType(PInfo: PTypeInfo): Boolean;
begin
  Result := PInfo = TypeInfo(TDateTime);
end;

procedure TCnIniFile.ReadObject(const Section: string; AObject: TObject);
var
  S: string;
  WS: WideString;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  Obj: TObject;
begin
  Count := GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkRecord,
    tkInterface], nil);
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkDynArray, tkRecord,
      tkVariant, tkMethod, tkInterface], @PropList^[0]);
    for PropIdx := 0 to Count - 1 do
    begin
      PropInfo := PropList^[PropIdx];
      try
        if ValueExists(Section, PropInfoName(PropInfo)) then
        begin
          if IsColorType(PropInfo^.PropType^) then
            SetOrdProp(AObject, PropInfo, ReadColor(Section, PropInfoName(PropInfo),
              GetOrdProp(AObject, PropInfo)))
          else if IsBooleanType(PropInfo^.PropType^) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetEnumProp(AObject, PropInfo, BoolToStr(True, True))
            else
              SetEnumProp(AObject, PropInfo, BoolToStr(False, True));
          end
          else if IsBoolType(PropInfo^.PropType^) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetOrdProp(AObject, PropInfo, -1)
            else
              SetOrdProp(AObject, PropInfo, 0);
          end
          else if IsDateTimeType(PropInfo^.PropType^) then
            SetFloatProp(AObject, PropInfo, ReadDateTime(Section, PropInfoName(PropInfo),
              GetFloatProp(AObject, PropInfo)))
          else
          begin
            case PropInfo^.PropType^^.Kind of
              tkInteger:
                SetOrdProp(AObject, PropInfo, ReadInteger(Section, PropInfoName(PropInfo),
                  GetOrdProp(AObject, PropInfo)));
              tkChar:
                begin
                  S := ReadString(Section, PropInfoName(PropInfo), Char(GetOrdProp(AObject, PropInfo)));
                  if S <> '' then
                    SetOrdProp(AObject, PropInfo, Ord(S[1]));
                end;
              tkWChar:
                begin
                  WS := ReadString(Section, PropInfoName(PropInfo), WideChar(GetOrdProp(AObject, PropInfo)));
                  if WS <> '' then
                    SetOrdProp(AObject, PropInfo, Ord(WS[1]));
                end;
              tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
                SetStrProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetStrProp(AObject, PropInfo)));
              tkFloat:
                SetFloatProp(AObject, PropInfo, ReadFloat(Section, PropInfoName(PropInfo),
                  GetFloatProp(AObject, PropInfo)));
              tkInt64:
                SetInt64Prop(AObject, PropInfo, StrToInt64(ReadString(Section,
                  PropInfoName(PropInfo), IntToStr(GetInt64Prop(AObject, PropInfo)))));
              tkEnumeration:
                SetEnumProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetEnumProp(AObject, PropInfo)));
              tkSet:
                SetSetProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetSetProp(AObject, PropInfo, True)));
              tkClass:
                begin
                  Obj := TObject(GetOrdProp(AObject, PropInfo));
                  if Obj <> nil then
                  begin
                    if Obj is TFont then
                      ReadFont(Section, PropInfoName(PropInfo), TFont(Obj))
                    else if Obj is TStrings then
                      ReadStrings(Section, PropInfoName(PropInfo), TStrings(Obj));
                  end;
                end;
            end;
          end;            
        end;
      except
        ;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure TCnIniFile.WriteObject(const Section: string; AObject: TObject;
  NoDef: Boolean);
var
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  Obj: TObject;
begin
  Count := GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkRecord,
    tkInterface], nil);
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkDynArray, tkRecord,
      tkVariant, tkMethod, tkInterface], @PropList^[0]);
    for PropIdx := 0 to Count - 1 do
    begin
      PropInfo := PropList^[PropIdx];
      try
        if not NoDef or IsStoredProp(AObject, PropInfo) and
          not IsDefaultPropertyValue(AObject, PropInfo) then
        begin
          if IsColorType(PropInfo^.PropType^) then
            WriteColor(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo))
          else if IsBooleanType(PropInfo^.PropType^) or IsBoolType(PropInfo^.PropType^) then
            WriteBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0)
          else if IsDateTimeType(PropInfo^.PropType^) then
            WriteDateTime(Section, PropInfoName(PropInfo), GetFloatProp(AObject, PropInfo))
          else
          begin
            case PropInfo^.PropType^^.Kind of
              tkInteger:
                WriteInteger(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo));
              tkChar:
                WriteString(Section, PropInfoName(PropInfo), Char(GetOrdProp(AObject, PropInfo)));
              tkWChar:
                WriteString(Section, PropInfoName(PropInfo), WideChar(GetOrdProp(AObject, PropInfo)));
              tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
                WriteString(Section, PropInfoName(PropInfo), GetStrProp(AObject, PropInfo));
              tkFloat:
                WriteFloat(Section, PropInfoName(PropInfo), GetFloatProp(AObject, PropInfo));
              tkInt64:
                WriteString(Section, PropInfoName(PropInfo), IntToStr(GetInt64Prop(AObject, PropInfo)));
              tkEnumeration:
                WriteString(Section, PropInfoName(PropInfo), GetEnumProp(AObject, PropInfo));
              tkSet:
                WriteString(Section, PropInfoName(PropInfo), GetSetProp(AObject, PropInfo, True));
              tkClass:
                begin
                  Obj := TObject(GetOrdProp(AObject, PropInfo));
                  if Obj <> nil then
                  begin
                    if Obj is TFont then
                      WriteFont(Section, PropInfoName(PropInfo), TFont(Obj))
                    else if Obj is TStrings then
                      WriteStrings(Section, PropInfoName(PropInfo), TStrings(Obj));
                  end;
                end;
            end;
          end;            
        end
        else
        begin
          DeleteKey(Section, PropInfoName(PropInfo));
        end;
      except
        ;
      end;                      
    end;
  finally
    FreeMem(PropList);
  end;
end;

//------------------------------------------------------------------------------
// ���ñ���װ�� INI ���ʷ���
//------------------------------------------------------------------------------

procedure TCnIniFile.DeleteKey(const Section, Ident: String);
begin
  Ini.DeleteKey(Section, Ident);
end;

procedure TCnIniFile.EraseSection(const Section: string);
begin
  Ini.EraseSection(Section);
end;

function TCnIniFile.GetFileName: string;
begin
  Result := Ini.FileName;
end;

procedure TCnIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  Ini.ReadSection(Section, Strings);
end;

procedure TCnIniFile.ReadSections(Strings: TStrings);
begin
  Ini.ReadSections(Strings);
end;

procedure TCnIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  Ini.ReadSectionValues(Section, Strings);
end;

function TCnIniFile.ReadString(const Section, Ident,
  Default: string): string;
begin
  Result := Ini.ReadString(Section, Ident, Default);
end;

procedure TCnIniFile.UpdateFile;
begin
  Ini.UpdateFile;
end;

procedure TCnIniFile.WriteString(const Section, Ident, Value: String);
begin
  Ini.WriteString(Section, Ident, Value);
end;

function TCnIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := Ini.ReadBool(Section, Ident, Default);
end;

function TCnIniFile.ReadDate(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadDate(Section, Name, Default);
end;

function TCnIniFile.ReadDateTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadDateTime(Section, Name, Default);
end;

function TCnIniFile.ReadFloat(const Section, Name: string;
  Default: Double): Double;
begin
  Result := Ini.ReadFloat(Section, Name, Default);
end;

function TCnIniFile.ReadInteger(const Section, Ident: string;
  Default: Integer): Longint;
begin
  Result := Ini.ReadInteger(Section, Ident, Default);
end;

function TCnIniFile.ReadTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadTime(Section, Name, Default);
end;

procedure TCnIniFile.WriteBool(const Section, Ident: string;
  Value: Boolean);
begin
  Ini.WriteBool(Section, Ident, Value);
end;

procedure TCnIniFile.WriteDate(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteDate(Section, Name, Value);
end;

procedure TCnIniFile.WriteDateTime(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteDateTime(Section, Name, Value);
end;

procedure TCnIniFile.WriteFloat(const Section, Name: string;
  Value: Double);
begin
  Ini.WriteFloat(Section, Name, Value);
end;

procedure TCnIniFile.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  Ini.WriteInteger(Section, Ident, Value);
end;

procedure TCnIniFile.WriteTime(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteTime(Section, Name, Value);
end;

//==============================================================================
// ֧���������� IniFile ��
//==============================================================================

{ TCnStreamIniFile }

constructor TCnStreamIniFile.Create(const FileName: string);
var
  Strings: TStrings;
begin
  inherited Create('');
  FFileName := FileName;
  if FileExists(FFileName) then
    LoadFromFile(FFileName);

  if FFileName <> '' then
  begin
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      FInitData := Strings.Text;
    finally
      Strings.Free;
    end;
  end;    
end;

destructor TCnStreamIniFile.Destroy;
var
  Strings: TStrings;
begin
  if FFileName <> '' then
  begin
    // �б��ʱ�ű���
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      if CompareStr(Strings.Text, FInitData) <> 0 then
        UpdateFile;
    finally
      Strings.Free;
    end;
  end;
  inherited;
end;

function TCnStreamIniFile.LoadFromFile(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.LoadFromStream(Stream: TStream): Boolean;
var
  Strings: TStrings;
begin
  try
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(Stream);
      SetStrings(Strings);
    finally
      Strings.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.SaveToFile(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.Size := 0;
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.SaveToStream(Stream: TStream): Boolean;
var
  Strings: TStrings;
begin
  try
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      Strings.SaveToStream(Stream);
    finally
      Strings.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TCnStreamIniFile.UpdateFile;
begin
  if FFileName <> '' then
    SaveToFile(FFileName);
end;

//==============================================================================
// ֧�����ݼ��ܼ��������� IniFile ����
//==============================================================================

{ TCnBaseEncryptIniFile }

constructor TCnBaseEncryptIniFile.Create(const FileName: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
begin
{$IFDEF SUPPORT_ZLIB}
  FUseZLib := AUseZLib;
{$ENDIF}
  inherited Create(FileName);
end;

function TCnBaseEncryptIniFile.LoadFromStream(Stream: TStream): Boolean;
var
  EncryptStream: TCnEncryptStream;
{$IFDEF SUPPORT_ZLIB}
  DecompStream: TDecompressionStream;
  MemStream: TMemoryStream;
{$ENDIF}
begin
  EncryptStream := nil;
{$IFDEF SUPPORT_ZLIB}
  DecompStream := nil;
  MemStream := nil;
{$ENDIF}
  try
  {$IFDEF SUPPORT_ZLIB}
    if FUseZLib then
    begin
      EncryptStream := CreateEncryptStream(Stream);
      MemStream := TMemoryStream.Create;
      MemStream.LoadFromStream(EncryptStream);
      DecompStream := TDecompressionStream.Create(MemStream);
      Result := inherited LoadFromStream(DecompStream);
    end
    else
  {$ENDIF}
    begin
      EncryptStream := CreateEncryptStream(Stream);
      Result := inherited LoadFromStream(EncryptStream);
    end;
  finally
    EncryptStream.Free;
  {$IFDEF SUPPORT_ZLIB}
    DecompStream.Free;
    MemStream.Free;
  {$ENDIF}
  end;
end;

function TCnBaseEncryptIniFile.SaveToStream(Stream: TStream): Boolean;
var
  EncryptStream: TCnEncryptStream;
{$IFDEF SUPPORT_ZLIB}
  MemStream: TMemoryStream;
  CompStream: TCompressionStream;
{$ENDIF}
begin
  EncryptStream := nil;
{$IFDEF SUPPORT_ZLIB}
  CompStream := nil;
  MemStream := nil;
{$ENDIF}
  try
  {$IFDEF SUPPORT_ZLIB}
    if FUseZLib then
    begin
      MemStream := TMemoryStream.Create;
    {$IFNDEF DELPHI2009_UP}
      CompStream := TCompressionStream.Create(clMax, MemStream);
    {$ELSE}
      {$IFDEF DELPHIXE2_UP}
      CompStream := TCompressionStream.Create(MemStream, zcMax, 15);
      {$ELSE}
      CompStream := TCompressionStream.Create(MemStream, zcMax);
      {$ENDIF}
    {$ENDIF}
      Result := inherited SaveToStream(CompStream);
      FreeAndNil(CompStream); // �ͷ�ʱ�Ż����ѹ�����
      EncryptStream := CreateEncryptStream(Stream);
      MemStream.SaveToStream(EncryptStream);
    end
    else
  {$ENDIF}
    begin
      EncryptStream := CreateEncryptStream(Stream);
      Result := inherited SaveToStream(EncryptStream);
    end;
  finally
    EncryptStream.Free;
  {$IFDEF SUPPORT_ZLIB}
    MemStream.Free;
    CompStream.Free;
  {$ENDIF}
  end;
end;

//==============================================================================
// ֧���ı� Xor ���ܼ��������� IniFile ��
//==============================================================================

{ TCnXorIniFile }

constructor TCnXorIniFile.Create(const FileName, XorStr: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean{$ENDIF});
begin
  FXorStr := XorStr;
  inherited Create(FileName{$IFDEF SUPPORT_ZLIB}, AUseZLib{$ENDIF});
end;

function TCnXorIniFile.CreateEncryptStream(AStream: TStream): TCnEncryptStream;
begin
  Result := TCnXorStream.Create(AStream, AnsiString(FXorStr));
end;

//==============================================================================
// ֧�����ݼ��ܼ��������� IniFile ��
//==============================================================================

{ TCnEncryptIniFile }

constructor TCnEncryptIniFile.Create(const FileName: string; const SeedStr: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
begin
  FSeedStr := SeedStr;
  inherited Create(FileName{$IFDEF SUPPORT_ZLIB}, AUseZLib{$ENDIF});
end;

function TCnEncryptIniFile.CreateEncryptStream(AStream: TStream): TCnEncryptStream;
begin
  Result := TCnCodeMapStream.Create(AStream, AnsiString(FSeedStr));
end;

end.
