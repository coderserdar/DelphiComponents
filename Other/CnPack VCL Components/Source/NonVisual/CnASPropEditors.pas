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

unit CnASPropEditors;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ActiveScript �ű��������Ա༭����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.10.31
*               ���������Ȩ��Ϣ���Ա༭��TCnCopyrightProperty
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  CnActiveScript, CnConsts;

type

{ TCnScriptLangProperty }

  TCnScriptLangProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings);
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

{ TCnScriptLangProperty }

function TCnScriptLangProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TCnScriptLangProperty.GetValueList(List: TStrings);
begin
  GetActiveScriptParse(List);
end;

procedure TCnScriptLangProperty.GetValues(Proc: TGetStrProc);
var
  j: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for j := 0 to Values.Count - 1 do
      Proc(Values[j]);
  finally
    Values.Free;
  end;
end;

end.



