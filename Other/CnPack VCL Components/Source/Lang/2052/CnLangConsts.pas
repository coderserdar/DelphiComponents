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

unit CnLangConsts;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ������������Ԫ
* ��Ԫ���ߣ�CnPack������ ��Х (liuxiao@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin2000 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.08.20 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

resourcestring

  SCnLangMgrName = '���������';
  SCnLangMgrComment = '������������';
  SCnIniLangStorageName = 'INI ����洢���';
  SCnIniLangStorageComment = 'INI ����洢���';
  SCnHashLangStorageName = 'ɢ���ı�����洢���';
  SCnHashLangStorageComment = 'ɢ���ı�����洢������нϿ���ٶ�';
  SCnLangTranslatorName = '���������';
  SCnLangTranslatorComment = '����������������������������';

  SCnMultiInstanceError = 'ֻ������һ�� %s ʵ����';

  SCnLoadLangFileError = '����װ�������ļ�: %s';
  SCnInvalidLanguageIDError = '%d ���ǺϷ��� Language ID';
  SCnErrorInCheckingLanguage = 'ϵͳ���Լ������볢�Թر� DEP';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnFormTranslationManager = '&T.��������� ...';
  SCnEditLanguageStrings = '�༭������Ŀ...';
  SCnLangExtractStrings = 'ȡ�������ַ�����������(&T)';
  SCnErrorCaption = '����';
  SCnErrorNoLangManager = 'δ�ҵ������Թ����������ȴ��������Թ�����ʵ��';
  SCnErrorNoStorage = 'δ�ҵ������Դ洢ʵ�������ȴ��������Դ洢ʵ��';
  SCnLanguagePath = '��ѡ�������ļ��Ĵ洢·��';
  SCnCanNotCreateDir = '�޷�����Ŀ¼';

  // ��������������ַ���
  SCnactAddLineCaption = '����';
  SCnactAddLineHint = '���ַ����б�ĩβ����һ��';
  SCnactClearCaption = '���';
  SCnactClearHint = 'ɾ����ǰ���еķ���������Ŀ';
  SCnactCloseCaption = '�ر�';
  SCnactCollectFormCaption = '����';
  SCnactCollectFormHint = '�Զ�����������Ŀ���������� Form �ķ����ַ����б�';
  SCnactFilterCaption = '����';
  SCnactFilterHint = '�������ɷ����ַ����б�ʱ��Ҫ���˵�����';
  SCnactCopyStrsCaption = '����';
  SCnactCopyStrsHint = '������ԭ�ĸ��Ƶ�������ı�';
  SCnactDelBlankCaption = 'ɾ��';
  SCnactDelBlankHint = 'ɾ�������ַ���ֵΪ�ջ���ֻ�������ֺͷ��ŵ���';
  SCnactDelLineCaption = 'ɾ��';
  SCnactDelLineHint = 'ɾ�������ַ����б�ĵ�ǰ��';
  SCnactGenStrsCaption = '����';
  SCnactGenStrsHint = '���ɵ�ǰ Form �Ĵ������ַ����б�';
  SCnactSaveCurItemCaption = '����';
  SCnactSaveCurItemHint = '���浱ǰ������Ŀ�ķ����ַ����б�';
  SCnactUpdateStrsCaption = '����';
  SCnactUpdateStrsHint = '���µ�ǰ�������ַ����б�';
  SCnCaption = '���巭�������';
  SCnlbl1Caption = '�ļ�����';
  SCnlblIndexCaption = '��ţ�';
  SCnlblLangIDCaption = '���� ID��';
  SCnlblLangNameCaption = '��������';
  SCnStringGridHint = '�������ֱ༭��';
  SCntvStoragesHint = '������Ŀ������';

  SCnStringGridCells10 = '��������Ŀ';
  SCnStringGridCells20 = 'ԭ��';
  SCnStringGridCells30 = '������ı�';

  // �������ô����ַ���
  SCnFilterFrmCaption = '��������';
  SCnFilterCaption = 'ֻΪ�����������ɷ����ַ�����';
  SCnOKCaption     = 'ȷ��(&O)';
  SCnCancelCaption = 'ȡ��(&C)';

  SCnWarningCaption = '����';
  SCnLangInvalidLine = '������Ŀ�з��ַǷ��л���У�����ɾ����';

{ SCnMultiInstanceError = 'Only one %s Instance is allowed!';

  SCnLoadLangFileError = 'Can''t Load the language File: %s';
  SCnInvalidLanguageIDError = 'Invalid Language ID Error';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnCreateLangFile = '&Create Language files';
  SCnUpdateLangFile = '&Update Language files';
  SCnFormTranslationManager = 'Form &Translation Manager';    }

const
  SCnCRLF = #13#10;
  SCnBR = '<BR>';
  SCnCommentChar1 = ';';
  SCnCommentChar2 = '#';
  SCnCommentChar3 = '/';

implementation

end.
