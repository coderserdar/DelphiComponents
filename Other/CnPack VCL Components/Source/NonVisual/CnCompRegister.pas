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

unit CnCompRegister;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ������ӹ��������ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
{$IFDEF SUPPORT_ADO}
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.AdoConEd {$ELSE} AdoConEd {$ENDIF},
{$ENDIF}
{$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  CnTimer, CnFormScaler, CnControlHook, CnActionListHook, CnMenuHook, CnThreadPool,
  CnActiveScript, CnASPropEditors, CnTrayIcon, CnObjectPool, CnConsole,
  CnVolumeCtrl, CnMDIBackGround, CnWinampCtrl, CnRestoreSystemMenu, CnDockFormControl, 
  CnDelphiDockStyle, CnVCDockStyle, CnVIDDockStyle, CnVSNETDockStyle, CnDockPropertyReg,
  CnFileSystemWatcher, CnDragResizer, CnKeyBlocker, CnFilePacker, CnGlobalKeyHook,
  CnOuterControls, CnTaskBar, CnRawInput, CnSystemDebugControl, CnConsts;

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
  RegisterComponents(SCnNonVisualPalette, [TCnTimer, TCnTimerList, TCnTrayIcon]);
  RegisterComponents(SCnNonVisualPalette, [TCnControlHook, TCnActionListHook,
    TCnMenuHook]);
  RegisterComponents(SCnNonVisualPalette, [TCnFormScaler, TCnVolumeCtrl, TCnMDIBackGround]);
  RegisterComponents(SCnNonVisualPalette, [TCnActiveScriptSite, TCnActiveScriptWindow]);
  RegisterComponents(SCnNonVisualPalette, [TCnObjectPool, TCnThreadPool, TCnWinampCtrl]);
  RegisterComponents(SCnNonVisualPalette, [TCnRestoreSystemMenu, TCnConsole]);
  RegisterComponents(SCnNonVisualPalette, [TCnFileSystemWatcher]);
  RegisterComponents(SCnNonVisualPalette, [TCnDragResizer]);
  RegisterComponents(SCnNonVisualPalette, [TCnKeyBlocker]);
  RegisterComponents(SCnNonVisualPalette, [TCnFilePacker]);
  RegisterComponents(SCnNonVisualPalette, [TCnGlobalKeyHook, TCnOuterControls]);
  RegisterComponents(SCnNonVisualPalette, [TCnTaskBar, TCnRawKeyboard, TCnSystemDebugControl]);

  RegisterPropertyEditor(TypeInfo(TScriptLanguage), TCnActiveScriptSite, 'ScriptLanguage',
    TCnScriptLangProperty);

  // ע�� Dock ϵ�����
  RegisterComponents(SCnNonVisualPalette, [TCnDockServer, TCnDockClient, 
    TCnDelphiDockStyle, TCnVCDockStyle, TCnVIDDockStyle, TCnVSNETDockStyle]);
  RegisterNoIcon([TCnVIDDockTabSheet]);
  RegisterClass(TCnVIDDockTabSheet);
    
  RegisterComponentEditor(TCnDockBaseControl, TCnDockControlEditor);
  RegisterComponentEditor(TCnBasicDockStyle, TCnDockStyleEditor);
  RegisterComponentEditor(TCnVIDTabPageControl, TCnVIDTabPageControlEditor);
  RegisterComponentEditor(TCnVIDDockTabSheet, TCnVIDTabPageControlEditor);
end;

end.
