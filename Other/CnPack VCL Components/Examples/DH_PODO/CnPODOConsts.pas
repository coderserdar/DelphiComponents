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

unit CnPODOConsts;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate PODO ���ɹ���
* ��Ԫ���ƣ�PODO ���ɹ���
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnPODOConsts.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

const
  PODO_CONNECT_SUCCESS = '�������ݿ�ɹ���';
  PODO_CONNECT_FAIL = '�������ݿ�ʧ�ܣ�';
  PODO_SAVE_SUCCESS = '�����ļ��ɹ���';
  PODO_SAVE_FAIL = '�����ļ�ʧ�ܣ�';
  PODO_MSGBOX_TITLE = '��ʾ';


  PODO_DATA_TYPE_STRING = 'String';
  PODO_DATA_TYPE_INTEGER = 'Integer';
  PODO_DATA_TYPE_FLOAT = 'Real';
  PODO_DATA_TYPE_DATETIME = 'TDateTime';
  PODO_DATA_TYPE_BOOLEAN = 'Boolean';
  PODO_DATA_TYPE_VARIANT = 'Variant';

  STR_SPACE = ' ';
  STR_NULL = #0;

  FILTER_PODO = '%s.pas';
  FILTER_FILE_NAME = 'PODO_%s';

  PREVIEW_UNIT_HEAD_COMMENT = '(* This unit is created by PODO generator *)';
  PREVIEW_UNIT_NAME = 'unit PODO_%s;';
  PREVIEW_UNIT_MPLUS = '{$M+}';
  PREVIEW_UNIT_INTERFACE = 'interface';
  PREVIEW_UNIT_USES = 'uses';
  PREVIEW_UNIT_BASE_UNIT = '  Classes, SysUtils, CnDHibernateBase;';
  PREVIEW_UNIT_TYPE = 'type';
  PREVIEW_UNIT_CLASS_NAME = '  T%s = class(TCnDHibernateBase)';
  PREVIEW_UNIT_PRIVATE = '  private';
  PREVIEW_UNIT_PRIVATE_ATTR = '    F%s : %s;';
  PREVIEW_UNIT_PUBLISHED = '  published';
  PREVIEW_UNIT_PUBLISHED_ATTR = '    property %s : %s read F%s write F%s;';
  PREVIEW_UNIT_END = '  end;';
  PREVIEW_UNIT_IMPLEMENTATION = 'implementation';
  PREVIEW_UNIT_INITIALIZATION = 'initialization';
  PREVIEW_UNIT_REGISTER_CLASS = '  RegisterClass(T%s);';
  PREVIEW_UNIT_FULL_END = 'end.';

implementation

end.
