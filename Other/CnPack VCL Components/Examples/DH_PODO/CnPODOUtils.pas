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

unit CnPODOUtils;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate PODO ���ɹ���
* ��Ԫ���ƣ�PODO ���ɹ���
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnPODOUtils.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, DB, CnPODOConsts;

{ change dataType to string }
function DataTypeToString(dataType: TFieldType): string;

{ delete the spaces in string }
function DeleteSpaces(const Str: string): string;

{ change the first letter to upperCase }
function UpperCaseFirst(const Str: string):string;

implementation

function DataTypeToString(dataType: TFieldType): string;
begin
  case dataType of
    ftWideString, ftString, ftFixedChar {$IFDEF BDS2006_UP}, ftFixedWideChar{$ENDIF}: Result := PODO_DATA_TYPE_STRING;
    ftSmallint, ftInteger, ftWord, ftLargeint {$IFDEF BDS2006_UP}, ftOraInterval{$ENDIF}, ftAutoInc: Result := PODO_DATA_TYPE_INTEGER;
    ftBoolean: Result := PODO_DATA_TYPE_BOOLEAN;
    ftFloat, ftCurrency, ftBCD{$IFDEF DELPHI6_UP}, ftFMTBcd{$ENDIF}: Result := PODO_DATA_TYPE_FLOAT;
    ftDate, ftTime, ftDateTime{$IFDEF DELPHI6_UP}, ftTimeStamp{$ENDIF} {$IFDEF BDS2006_UP}, ftOraTimeStamp{$ENDIF}: Result := PODO_DATA_TYPE_DATETIME;
  else
    Result := PODO_DATA_TYPE_VARIANT;
  end;
end;

function DeleteSpaces(const Str: string): string;
begin
  Result := StringReplace(Str, STR_SPACE, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
end;

function UpperCaseFirst(const Str:string):string;
begin
  Result := UpperCase(Str[1]) + Copy(Str, 2, Length(Str) - 1)
end;    

end.

