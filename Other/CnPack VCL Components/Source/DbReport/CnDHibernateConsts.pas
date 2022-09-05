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

unit CnDHibernateConsts; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate������
* ��Ԫ���ƣ�������
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils;

const
  DH_VERSION = '1.8 (D6~D2009)';

  // cnt: empty string
  DH_NULL_VAR = ''; 

  // cnt: components registry panel
  DH_REG_PANEL = 'CnPack DHibernate'; 

  // fmt: class name
  DH_CLASS_NAME = 'T%s'; 

  // fmt: get record T-sql with a param
  DH_GET_RECORD = 'select * from %s where %s=:%s'; 

  // fmt: id-generator T-sql
  DH_ID_GENERATOR = 'select * from %s where %s=''%s'''; 

  // fmt: normal selection T-sql
  // for events, add "and" after this
  DH_SELECT = 'select * from %s where 1=1'; 

  // fmt: nnormal deletion T-sql
  // for events, add "and" after this
  DH_DELETE_RECORD = 'delete from %s where 1=1'; 

  // fmt: events
  DH_SEARCH_FILTER = ' and %s=''%s'''; 

  // msg: no record found
  DH_MEM_NO_RECORDS = 'No data found';

  // msg: no fields defined
  DH_INVALID_FIELDS = 'No fields defined'; 

  // cnt: the formatter of date in kinds of databases
  DH_DATE_FMT_STD_16 = '''"''mm''/''dd''/''yyyy''"''';  {"mm/dd/yyyy"}
  DH_DATE_FMT_STD_32 = '''''''dd/mm/yyyy''''''';  {'dd/mm/yyyy'}
  DH_DATE_FMT_ORACLE = '"TO_DATE(''"dd/mm/yyyy"'', ''DD/MM/YYYY'')"';
  DH_DATE_FMT_INTERBASE = '"CAST(''"mm"/"dd"/"yyyy"'' AS DATE)"';
  DH_DATE_FMT_MSSQL = '"CONVERT(datetime, ''"mm"/"dd"/"yyyy"'', 103)"'; 

  // cnt: the true event
  DH_TRUE_EXPRESS = '1=1'; 

  // cnt: the extension ids
  DH_MAX_EXT_STR_ID = 61300;
  DH_CONFIRM_SAVE = DH_MAX_EXT_STR_ID - 126;
  DH_DATABASE_NAME = DH_MAX_EXT_STR_ID - 127; 

  // cnt: strings and formatters
  DH_FLAGS = 'Flags';
  DH_SHOW_CMD = 'ShowCmd';
  DH_MIN_MAX_POS = 'MinMaxPos';
  DH_NORM_POS = 'NormPos';
  DH_PIXELS = 'PixelsPerInch';
  DH_MDI_CHILD = 'MDI Children';
  DH_LIST_COUNT = 'Count';
  DH_ITEM = 'Item%d'; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$ENDIF SUPPORT_ADO}
end.
