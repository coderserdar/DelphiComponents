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

  SCnLangMgrName = '�h�y�޲z��';
  SCnLangMgrComment = '�h�y�޲z���ե�';
  SCnIniLangStorageName = 'INI �h�y�s�x�ե�';
  SCnIniLangStorageComment = 'INI �h�y�s�x�ե�';
  SCnHashLangStorageName = '���C�奻�h�y�s�x�ե�';
  SCnHashLangStorageComment = '���C�奻�h�y�s�x�ե�A�����֪��t��';
  SCnLangTranslatorName = '½Ķ���ե�';
  SCnLangTranslatorComment = '½Ķ���ե�A�ΨӱҰ�½Ķ�޲z��';

  SCnMultiInstanceError = '�u���\�Ыؤ@�� %s ��ҡI';

  SCnLoadLangFileError = '����˸��y�����: %s';
  SCnInvalidLanguageIDError = '%d ���O�X�k�� Language ID';
  SCnErrorInCheckingLanguage = '�t�λy���˴��X���A�й������� DEP';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnFormTranslationManager = '&T.½Ķ�޲z�� ...';
  SCnEditLanguageStrings = '�s��y������...';
  SCnLangExtractStrings = '���X�y���r�Ŧ��ŶK�O(&T)';
  SCnErrorCaption = '���~';
  SCnErrorNoLangManager = '�����h�y���޲z���A�Х��Ыئh�y���޲z�����';
  SCnErrorNoStorage = '�����h�y���s�x��ҡA�Х��Ыئh�y���s�x���';
  SCnLanguagePath = '�п�ܻy����󪺦s�x���|';
  SCnCanNotCreateDir = '�L�k�Ыإؿ�';

  // ½Ķ�޲z������r�Ŧ�
  SCnactAddLineCaption = '�[��';
  SCnactAddLineHint = '�b�r�Ŧ�C�����W�[�@��';
  SCnactClearCaption = '�M��';
  SCnactClearHint = '�R����e�Ҧ���½Ķ�y������';
  SCnactCloseCaption = '����';
  SCnactCollectFormCaption = '���`';
  SCnactCollectFormHint = '�۰ʹM����Ӷ��ءA�ͦ��Ҧ� Form ��½Ķ�r�Ŧ�C��';
  SCnactFilterCaption = '�L�o';
  SCnactFilterHint = '�]�m�ͦ�½Ķ�r�Ŧ�C��ɻݭn�L�o���ݩ�';
  SCnactCopyStrsCaption = '�ƻs';
  SCnactCopyStrsHint = '�N�Ҧ����ƻs��½Ķ��奻';
  SCnactDelBlankCaption = '�R��';
  SCnactDelBlankHint = '�R���Ҧ��r�Ŧ�Ȭ��ũΪ̥u�]�t�Ʀr�M�Ÿ�����';
  SCnactDelLineCaption = '�R��';
  SCnactDelLineHint = '�R��½Ķ�r�Ŧ�C����e��';
  SCnactGenStrsCaption = '�ͦ�';
  SCnactGenStrsHint = '�ͦ���e Form ����½Ķ�r�Ŧ�C��';
  SCnactSaveCurItemCaption = '�O�s';
  SCnactSaveCurItemHint = '�O�s��e�y�����ت�½Ķ�r�Ŧ�C��';
  SCnactUpdateStrsCaption = '��s';
  SCnactUpdateStrsHint = '��s��e��½Ķ�r�Ŧ�C��';
  SCnCaption = '����½Ķ�޲z��';
  SCnlbl1Caption = '���W�G';
  SCnlblIndexCaption = '�Ǹ��G';
  SCnlblLangIDCaption = '�y�� ID�G';
  SCnlblLangNameCaption = '�y���W�G';
  SCnStringGridHint = '½Ķ��r�s���';
  SCntvStoragesHint = '�y�����غ޲z��';

  SCnStringGridCells10 = '��½Ķ����';
  SCnStringGridCells20 = '���';
  SCnStringGridCells30 = '½Ķ��奻';

  // �L�o�]�m����r�Ŧ�
  SCnFilterFrmCaption = '�L�o�]�m';
  SCnFilterCaption = '�u���H�U�ݩʥͦ�½Ķ�r�Ŧ�G';
  SCnOKCaption     = '�T�w(&O)';
  SCnCancelCaption = '����(&C)';

  SCnWarningCaption = 'ĵ�i';
  SCnLangInvalidLine = '�h�y���ؤ��o�{�D�k��ΪŦ�A�N�Q�R���C';

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
