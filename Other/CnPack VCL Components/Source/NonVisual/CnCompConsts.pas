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
  SCnTimerName = 'Thread Timer Component';
  SCnTimerComment = 'Thread Timer Component';
  SCnTimerListName = 'Thread Timer List Component';
  SCnTimerListComment = 'Thread Timer List Component';
  
  // CnControlHook
  SCnControlHookName = 'Control Hook Component';
  SCnControlHookComment = 'Control Hooker by Modify WindowProc Property';

  // CnActionListHook
  SCnActionListHookName = 'ActionList Hook Component';
  SCnActionListHookComment = 'Hook Actions in ActionList';

  // CnActiveScriptSite
  SCnActiveScriptSiteName = 'ActiveScript Site Component';
  SCnActiveScriptSiteComment = 'ActiveScript Site Engine Wrapper Component';

  // CnActiveScriptWindow
  SCnActiveScriptWindowName = 'ActiveScript Window Component';
  SCnActiveScriptWindowComment = 'ActiveScript Window Engine Wrapper Component';

  // CnADOConPool
  SCnADOConPoolName = 'ADO Connection Pool Component';
  SCnADOConPoolComment = 'ADO Connection Pool Component';

  // CnFormScaler
  SCnFormScalerName = 'Form Scale Component';
  SCnFormScalerComment = 'Auto Process Form Scale';

  // CnMDIBackGround
  SCnMDIBackGroundName = 'MDI Background Component';
  SCnMDIBackGroundComment = 'Draw MDI Window Background';

  // CnMenuHook
  SCnMenuHookName = 'Menu Hook Component';
  SCnMenuHookComment = 'Hook Menu and Items';

  // CnObjectPool
  SCnObjectPoolName = 'Object Pool Component';
  SCnObjectPoolComment = 'Object Pool Component';

  // CnThreadPool
  SCnThreadPoolName = 'Thread Pool Component';
  SCnThreadPoolComment = 'Thread Pool Component';

  // CnRestoreSystemMenu
  SCnRestoreSystemMenuName = 'System Menu Restore Component';
  SCnRestoreSystemMenuComment = 'Component to Restore System Menu';

  // CnConsole
  SCnConsoleName = 'Console Component';
  SCnConsoleComment = 'Add Console to GUI Application';
  
  // CnTrayIcon
  SCnTrayIconName = 'Tray Icon Component';
  SCnTrayIconComment = 'Tray Icon Component';

  // CnVolumnCtrl
  SCnVolumnCtrlName = 'Volume Control Component';
  SCnVolumnCtrlComment = 'Volume Control Component';
  SCnMixerOpenError         = 'Open Audio Device Failed!';
  SCnMixerGetDevCapsError   = 'Get Device Caps Failed!';
  SCnMixerGetLineInfoError  = 'Get Line Failed!';
  SCnMixerGetVolumeError    = 'Get Volume Failed!';
  SCnMixerGetMuteError      = 'Get Mute State Failed!';
  SCnMixerSetVolumeError    = 'Set Volume Failed!';
  SCnMixerSetMuteError      = 'Set Mute State Failed!';

  // CnWinampCtrl
  SCnWinampCtrlName = 'Winamp Control Component';
  SCnWinampCtrlComment = 'Winamp Control Component';  
  
  // CnSkinMagic
  SCnSkinMagicName = 'Runtime Skin Frame';
  SCnSkinMagicComment = 'Runtime Skin Frame using Customized Painting';  

  // CnDockServer
  SCnDockServerName = 'Dock Server Component';
  SCnDockServerComment = 'Dock Server Component, Makes a DockSite Form';

  // CnDockClient
  SCnDockClientName = 'Dock Client Component';
  SCnDockClientComment = 'Dock Client Component, Makes a Dockable Form';

  // CnDelphiDockStyle
  SCnDelphiDockStyleName = 'Delphi Dock Style Component';
  SCnDelphiDockStyleComment = 'A Dock Style Component with Delphi Dock Style';

  // CnVCDockStyle
  SCnVCDockStyleName = 'Visual C++ Dock Style Component';
  SCnVCDockStyleComment = 'A Dock Style Component with Visual C++ Dock Style';

  // CnVIDDockStyle
  SCnVIDDockStyleName = 'Visual InterDev Dock Style Component';
  SCnVIDDockStyleComment = 'A Dock Style Component with Visual InterDev Dock Style';

  // CnVSNETDockStyle
  SCnVSNETDockStyleName = 'Visual Studio.NET Dock Style Component';
  SCnVSNETDockStyleComment = 'A Dock Style Component with Visual Studio.NET Dock Style';
  
  // CnFileSystemWatcher
  SCnFileSystemWatcherName = 'File System Watcher';
  SCnFileSystemWatcherComment = 'File System Watcher';
  
  // CnFilePacker
  SCnFilePackerName = 'File Packer and Unpacker';
  SCnFilePackerComment = 'File Packer and Unpacker';

implementation

end.
