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

unit CnLangReg;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ���������ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������ ��Х (liuxiao@cnpack.org)
* ��    ע���õ�Ԫʵ���˶����������ͱ༭��ע��
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
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes,
  CnLangCollection, CnLangConsts, CnLangMgr, CnLangStorage, CnLangTranslator,
  CnLangEditors, CnTransEditor, CnHashLangStorage, CnIniLangFileStorage;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CnMutiLang', [TCnLangManager, TCnLangTranslator,
    TCnHashLangFileStorage, TCnIniLangFileStorage]);
  RegisterPropertyEditor(TypeInfo(LongWord), TCnLanguageItem, 'LanguageID',
    TCnLanguageItemProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCnLangManager, 'CurrentLanguageIndex',
    TCnLangManagerProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TCnCustomLangFileStorage, 'LanguagePath',
    TCnLanguagePathProperty);
  RegisterComponentEditor(TCnLangTranslator, TCnTranslatorEditor);
{$IFDEF DELPHI}
  RegisterComponentEditor(TCnCustomLangStorage, TCnStorageEditor);
{$ENDIF}
{$IFDEF COMPILER6_UP}
  RegisterSelectionEditor(TComponent, TCnLangDesignerEditor);
{$ENDIF}
end;

end.
