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
* �޸ļ�¼��2005.12.24 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

resourcestring
  
  SCnLangMgrName = 'Language Manager';
  SCnLangMgrComment = 'Language Manager';
  SCnIniLangStorageName = 'INI Language Storage Component';
  SCnIniLangStorageComment = 'INI Language Storage Component';
  SCnHashLangStorageName = 'Hash TXT Language Storage Component';
  SCnHashLangStorageComment = 'Hash TXT Language Storage Component';
  SCnLangTranslatorName = 'Language Translator';
  SCnLangTranslatorComment = 'Language Translator and Translation Manager';
  
  SCnMultiInstanceError = 'Only one %s Instance is allowed!';

  SCnLoadLangFileError = 'Can''t Load the language File: %s';
  SCnInvalidLanguageIDError = 'Invalid Language ID %d';
  SCnErrorInCheckingLanguage = 'Error when Checking Language. Please close DEP';
  
  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnFormTranslationManager = '&Translation Manager...';
  SCnEditLanguageStrings = 'Edit Language Items...';
  SCnLangExtractStrings = '&Extract Language Strings to Clipboard';
  SCnErrorCaption = 'Error';
  SCnErrorNoLangManager = 'NO Language Manager, Please Create it First.';
  SCnErrorNoStorage = 'NO Language Storage, Please Create it First.';
  SCnLanguagePath = 'Select the Path to Store the Language File.';
  SCnCanNotCreateDir = 'Can NOT create Directories when Saving.';
  
  // Translator UI Resource Strings
  SCnactAddLineCaption = 'Add Line';
  SCnactAddLineHint = 'Add a New Line at the List End';
  SCnactClearCaption = 'Clear';
  SCnactClearHint = 'Delete All String Items from Translation List';
  SCnactCloseCaption = 'Close';
  SCnactCollectFormCaption = 'Gen All';
  SCnactCollectFormHint = 'Search All Forms in Project and Generate Strings for All Forms.';
  SCnactFilterCaption = 'Filter';
  SCnactFilterHint = 'Filter Settings for Strings Generation';
  SCnactCopyStrsCaption = 'Copy';
  SCnactCopyStrsHint = 'Copy All the Original Strings to Translated';
  SCnactDelBlankCaption = 'Del Blank';
  SCnactDelBlankHint = 'Delete All the Blank Strings';
  SCnactDelLineCaption = 'Del Line';
  SCnactDelLineHint = 'Delete Selected Line in the List';
  SCnactGenStrsCaption = 'Generate';
  SCnactGenStrsHint = 'Search Current Form and Generate Strings';
  SCnactSaveCurItemCaption = 'Save';
  SCnactSaveCurItemHint = 'Save Strings for Current Language';
  SCnactUpdateStrsCaption = 'Update';
  SCnactUpdateStrsHint = 'Re-search Current Form and Update Strings to Translate';
  SCnCaption = 'Form Translator';
  SCnlbl1Caption = 'File: ';
  SCnlblIndexCaption = 'Index: ';
  SCnlblLangIDCaption = 'Lang ID:';
  SCnlblLangNameCaption = 'Lang Name: ';
  SCnStringGridHint = 'Translation Grid';
  SCntvStoragesHint = 'Languages Tree';

  SCnStringGridCells10 = 'Strings ID';
  SCnStringGridCells20 = 'Original Text';
  SCnStringGridCells30 = 'Translated Text';

  // Filter settings UI Resource Strings
  SCnFilterFrmCaption = 'Filter Settings';
  SCnFilterCaption = 'Only Generate Strings for Properties:';
  SCnOKCaption     = '&OK';
  SCnCancelCaption = '&Cancel';

  SCnWarningCaption  = 'Warning';
  SCnLangInvalidLine = 'Found Blank or Invalid Lines, Deleted.';

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
