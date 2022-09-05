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

unit CnCompConsts;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ���Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫ�����˲����ӹ���������õ�����Դ�ַ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnTimer
  SCnTimerName = '����שw�ɾ��ե�';
  SCnTimerComment = '����שw�ɾ��ե�';
  SCnTimerListName = '����שw�ɾ��C��ե�';
  SCnTimerListComment = '����שw�ɾ��C��ե�';

  // CnControlHook
  SCnControlHookName = '���󱾱��ե�';
  SCnControlHookComment = '�q�L�קﱱ�� WindowProc �ݩʨӱ���������B�z�D�L�{';

  // CnActionListHook
  SCnActionListHookName = 'ActionList �����ե�';
  SCnActionListHookComment = '���� ActionList ���U�� Action ���ե�';

  // CnActiveScriptSite
  SCnActiveScriptSiteName = 'ActiveScript Site �ʸ˲ե�';
  SCnActiveScriptSiteComment = 'ActiveScript Site �}�������ʸ˲ե�';

  // CnActiveScriptWindow
  SCnActiveScriptWindowName = 'ActiveScript Window �ʸ˲ե�';
  SCnActiveScriptWindowComment = 'ActiveScript Window �}�������ʸ˲ե�';

  // CnADOConPool
  SCnADOConPoolName = 'ADO Connection �s�����ե�';
  SCnADOConPoolComment = 'ADO Connection �s�����ե�';

  // CnFormScaler
  SCnFormScalerName = 'Form Scale �۰ʳB�z�ե�';
  SCnFormScalerComment = 'Form Scale �۰ʳB�z�ե�';

  // CnMDIBackGround
  SCnMDIBackGroundName = 'MDI �D����I���ե�';
  SCnMDIBackGroundComment = 'MDI �D����I��ø�s�P����ե�';

  // CnMenuHook
  SCnMenuHookName = '��汾���ե�';
  SCnMenuHookComment = '��{��汾���\�઺�ե�';

  // CnObjectPool
  SCnObjectPoolName = '�ﹳ���ե�';
  SCnObjectPoolComment = '��{�ﹳ�����ե�';

  // CnThreadPool
  SCnThreadPoolName = '�u�{���ե�';
  SCnThreadPoolComment = '��{�u�{�����ե�';

  // CnRestoreSystemMenu
  SCnRestoreSystemMenuName = '�t�ε���_�ե�';
  SCnRestoreSystemMenuComment = '��_�s�边����k���檺�ե�';
  
  // CnConsole
  SCnConsoleName = '����x�ե�';
  SCnConsoleComment = '�� GUI ���ε{�ǼW�[����x';
  
  // CnTrayIcon
  SCnTrayIconName = '�t�Φ��L�ե�';
  SCnTrayIconComment = '�t�Φ��L�ե�';

  // CnVolumnCtrl
  SCnVolumnCtrlName = '���q����ե�';
  SCnVolumnCtrlComment = '�Ω󱱨�t�έ��q�A����h�]�Ʀh�u��';
  SCnMixerOpenError         = '���}���W�]�ƥ���!';
  SCnMixerGetDevCapsError   = '����]�Ƽ��D����!';
  SCnMixerGetLineInfoError  = '���}���W�u������!';
  SCnMixerGetVolumeError    = '�����e���q����!';
  SCnMixerGetMuteError      = '����R�����A����!';
  SCnMixerSetVolumeError    = '�]�m��e���q����!';
  SCnMixerSetMuteError      = '�]�m�R�����A����!';

  // CnWinampCtrl
  SCnWinampCtrlName = 'Winamp ����ե�';
  SCnWinampCtrlComment = 'Winamp ����ե�A�i�Ψӱ��� Winamp';

  // CnSkinMagic
  SCnSkinMagicName = '�B����ֽ��ج[�ե�';
  SCnSkinMagicComment = '�B����ֽ��ج[�ե�A�ϥΦ۩w�qø�s';

  // CnDockServer
  SCnDockServerName = '���a�A�Ⱥݲե�';
  SCnDockServerComment = '���a�A�Ⱥݲե�A�ϵ��鱵�����a';

  // CnDockClient
  SCnDockClientName = '���a�Ȥ�ݲե�';
  SCnDockClientComment = '���a�Ȥ�ݲե�A�ϵ���i���a';

  // CnDelphiDockStyle
  SCnDelphiDockStyleName = '���� Delphi �����a����ե�';
  SCnDelphiDockStyleComment = '���� Delphi �����a����ե�';

  // CnVCDockStyle
  SCnVCDockStyleName = '���� Visual C++ �����a����ե�';
  SCnVCDockStyleComment = '���� Visual C++ �����a����ե�';

  // CnVIDDockStyle
  SCnVIDDockStyleName = '���� Visual InterDev �����a����ե�';
  SCnVIDDockStyleComment = '���� Visual InterDev �����a����ե�';

  // CnVSNETDockStyle
  SCnVSNETDockStyleName = '���� Visual Studio.NET �����a����ե�';
  SCnVSNETDockStyleComment = '���� Visual Studio.NET �����a����ե�';
  
  // CnFileSystemWatcher
  SCnFileSystemWatcherName = '���ؿ��ʵ��ե�';
  SCnFileSystemWatcherComment = '���ؿ��ʵ��ե�';
  
  // CnFilePacker
  SCnFilePackerName = '�ɥؿ����]����';
  SCnFilePackerComment = '�ɥؿ����]����';

implementation

end.
