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

unit CnPropEditors;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���������������༭����Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.1
*               ���������Ȩ��Ϣ���Ա༭��TCnCopyrightProperty
*           2002.04.08 V1.0
*               ������Ԫ���յ�Ԫ��
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
  CnClasses, CnCommon, CnConsts;

type

{ TCnCopyrightProperty }

  TCnCopyrightProperty = class(TPropertyEditor)
  {* TCopyright���Ա༭���࣬����TCnImage�У��ڲ�������}
  public
    procedure Edit; override;
    {* �༭����}
    function GetAttributes: TPropertyAttributes; override;
    {* ȡ���Ա༭״̬}
    function GetValue: string; override;
    {* ȡ������ʾ���崮}
  end;

implementation

type
  TCnComponentAccess = class(TCnComponent);

{ TCnCopyrightProperty }

procedure TCnCopyrightProperty.Edit;
var
  Comp: TCnComponentAccess;
  AName, Author, Email, Comment: string;
begin
  if GetComponent(0) is TCnComponent then
  begin
    Comp := TCnComponentAccess(GetComponent(0));
    Comp.GetComponentInfo(AName, Author, Email, Comment);
    InfoDlg(Format(SCopyrightFmtStr, [AName, Author, Email, Comment]),
      SCnPackAbout);
  end
  else
    InfoDlg(Format(SCopyrightFmtStr, [GetComponent(0).ClassName, SCnPackGroup,
      SCnPackEmail, '']), SCnPackAbout);
end;

function TCnCopyrightProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TCnCopyrightProperty.GetValue: string;
begin
  Result := 'CnPack';
end;

end.



