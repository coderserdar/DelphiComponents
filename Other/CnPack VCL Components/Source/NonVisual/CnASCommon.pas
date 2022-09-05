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

unit CnASCommon;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ActiveScript Host ����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ�����˹� ActiveScript ʹ�õ�һЩ��������
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.10.31
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Registry, IniFiles, CnASIDispatchProxy, CnCommon;

type
  ICnASRegistry = interface(IActiveScriptInvokable)
  ['{428BFF29-68CC-4376-ACEB-16B4670CA920}']
    procedure CloseKey; stdcall;
    function CreateKey(const Key: string): Boolean; stdcall;
    function DeleteKey(const Key: string): Boolean; stdcall;
    function DeleteValue(const Name: string): Boolean; stdcall;
    function GetAccess: LongWord; stdcall;
    function GetCurrentKey: HKEY; stdcall;
    function GetCurrentPath: string; stdcall;
    function GetDataSize(const ValueName: string): Integer; stdcall;
    function GetDataType(const ValueName: string): TRegDataType; stdcall;
    function GetKeyNames: OleVariant; stdcall;
    function GetLazyWrite: Boolean; stdcall;
    function GetRootKey: HKEY; stdcall;
    function GetValueNames: OleVariant; stdcall;
    function HasSubKeys: Boolean; stdcall;
    function KeyExists(const Key: string): Boolean; stdcall;
    function LoadKey(const Key, FileName: string): Boolean; stdcall;
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean); stdcall;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean; stdcall;
    function OpenKeyReadOnly(const Key: String): Boolean; stdcall;
    function ReadBinaryData(const Name: string): OleVariant; stdcall;
    function ReadBool(const Name: string): Boolean; stdcall;
    function ReadCurrency(const Name: string): Currency; stdcall;
    function ReadDate(const Name: string): TDateTime; stdcall;
    function ReadDateTime(const Name: string): TDateTime; stdcall;
    function ReadFloat(const Name: string): Double; stdcall;
    function ReadInteger(const Name: string): Integer; stdcall;
    function ReadString(const Name: string): string; stdcall;
    function ReadTime(const Name: string): TDateTime; stdcall;
    function RegistryConnect(const UNCName: string): Boolean; stdcall;
    procedure RenameValue(const OldName, NewName: string); stdcall;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean; stdcall;
    function RestoreKey(const Key, FileName: string): Boolean; stdcall;
    function SaveKey(const Key, FileName: string): Boolean; stdcall;
    procedure SetAccess(const Value: LongWord); stdcall;
    procedure SetLazyWrite(const Value: Boolean); stdcall;
    procedure SetRootKey(const Value: HKEY); stdcall;
    function UnLoadKey(const Key: string): Boolean; stdcall;
    function ValueExists(const Name: string): Boolean; stdcall;
    procedure WriteBinaryData(const Name: string; Value: OleVariant); stdcall;
    procedure WriteBool(const Name: string; Value: Boolean); stdcall;
    procedure WriteCurrency(const Name: string; Value: Currency); stdcall;
    procedure WriteDate(const Name: string; Value: TDateTime); stdcall;
    procedure WriteDateTime(const Name: string; Value: TDateTime); stdcall;
    procedure WriteExpandString(const Name, Value: string); stdcall;
    procedure WriteFloat(const Name: string; Value: Double); stdcall;
    procedure WriteInteger(const Name: string; Value: Integer); stdcall;
    procedure WriteString(const Name, Value: string); stdcall;
    procedure WriteTime(const Name: string; Value: TDateTime); stdcall;
    property Access: LongWord read GetAccess write SetAccess;
    property CurrentKey: HKEY read GetCurrentKey;
    property CurrentPath: string read GetCurrentPath;
    property LazyWrite: Boolean read GetLazyWrite write SetLazyWrite;
    property RootKey: HKEY read GetRootKey write SetRootKey;
  end;

implementation

end.
