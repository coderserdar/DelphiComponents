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
  SCnTimerName = '�߾��ȶ�ʱ�����';
  SCnTimerComment = '�߾��ȶ�ʱ�����';
  SCnTimerListName = '�߾��ȶ�ʱ���б����';
  SCnTimerListComment = '�߾��ȶ�ʱ���б����';

  // CnControlHook
  SCnControlHookName = '�ؼ��ҽ����';
  SCnControlHookComment = 'ͨ���޸Ŀؼ��� WindowProc �������ҽ�����Ϣ����������';

  // CnActionListHook
  SCnActionListHookName = 'ActionList �ҽ����';
  SCnActionListHookComment = '�ҽ� ActionList �ĸ��� Action �����';

  // CnActiveScriptSite
  SCnActiveScriptSiteName = 'ActiveScript Site ��װ���';
  SCnActiveScriptSiteComment = 'ActiveScript Site �ű������װ���';

  // CnActiveScriptWindow
  SCnActiveScriptWindowName = 'ActiveScript Window ��װ���';
  SCnActiveScriptWindowComment = 'ActiveScript Window �ű������װ���';

  // CnADOConPool
  SCnADOConPoolName = 'ADO Connection ���ӳ����';
  SCnADOConPoolComment = 'ADO Connection ���ӳ����';

  // CnFormScaler
  SCnFormScalerName = 'Form Scale �Զ��������';
  SCnFormScalerComment = 'Form Scale �Զ��������';

  // CnMDIBackGround
  SCnMDIBackGroundName = 'MDI �����屳�����';
  SCnMDIBackGroundComment = 'MDI �����屳��������������';

  // CnMenuHook
  SCnMenuHookName = '�˵��ҽ����';
  SCnMenuHookComment = 'ʵ�ֲ˵��ҽӹ��ܵ����';

  // CnObjectPool
  SCnObjectPoolName = '��������';
  SCnObjectPoolComment = 'ʵ�ֶ���ص����';

  // CnThreadPool
  SCnThreadPoolName = '�̳߳����';
  SCnThreadPoolComment = 'ʵ���̳߳ص����';

  // CnRestoreSystemMenu
  SCnRestoreSystemMenuName = 'ϵͳ�˵��ָ����';
  SCnRestoreSystemMenuComment = '�ָ��༭���ؼ��Ҽ��˵������';
  
  // CnConsole
  SCnConsoleName = '����̨���';
  SCnConsoleComment = 'Ϊ GUI Ӧ�ó������ӿ���̨';
  
  // CnTrayIcon
  SCnTrayIconName = 'ϵͳ�������';
  SCnTrayIconComment = 'ϵͳ�������';

  // CnVolumnCtrl
  SCnVolumnCtrlName = '�����������';
  SCnVolumnCtrlComment = '���ڿ���ϵͳ������֧�ֶ��豸����·';
  SCnMixerOpenError         = '����Ƶ�豸ʧ��!';
  SCnMixerGetDevCapsError   = '��ȡ�豸����ʧ��!';
  SCnMixerGetLineInfoError  = '����Ƶ��·ʧ��!';
  SCnMixerGetVolumeError    = '��ȡ��ǰ����ʧ��!';
  SCnMixerGetMuteError      = '��ȡ����״̬ʧ��!';
  SCnMixerSetVolumeError    = '���õ�ǰ����ʧ��!';
  SCnMixerSetMuteError      = '���þ���״̬ʧ��!';

  // CnWinampCtrl
  SCnWinampCtrlName = 'Winamp ���������';
  SCnWinampCtrlComment = 'Winamp ��������������������� Winamp';

  // CnSkinMagic
  SCnSkinMagicName = '������Ƥ��������';
  SCnSkinMagicComment = '������Ƥ����������ʹ���Զ������';

  // CnDockServer
  SCnDockServerName = 'ͣ����������';
  SCnDockServerComment = 'ͣ������������ʹ�������ͣ��';

  // CnDockClient
  SCnDockClientName = 'ͣ���ͻ������';
  SCnDockClientComment = 'ͣ���ͻ��������ʹ�����ͣ��';

  // CnDelphiDockStyle
  SCnDelphiDockStyleName = '���� Delphi ��ͣ��������';
  SCnDelphiDockStyleComment = '���� Delphi ��ͣ��������';

  // CnVCDockStyle
  SCnVCDockStyleName = '���� Visual C++ ��ͣ��������';
  SCnVCDockStyleComment = '���� Visual C++ ��ͣ��������';

  // CnVIDDockStyle
  SCnVIDDockStyleName = '���� Visual InterDev ��ͣ��������';
  SCnVIDDockStyleComment = '���� Visual InterDev ��ͣ��������';

  // CnVSNETDockStyle
  SCnVSNETDockStyleName = '���� Visual Studio.NET ��ͣ��������';
  SCnVSNETDockStyleComment = '���� Visual Studio.NET ��ͣ��������';
  
  // CnFileSystemWatcher
  SCnFileSystemWatcherName = '�ļ�Ŀ¼�������';
  SCnFileSystemWatcherComment = '�ļ�Ŀ¼�������';
  
  // CnFilePacker
  SCnFilePackerName = '�ļ�Ŀ¼������';
  SCnFilePackerComment = '�ļ�Ŀ¼������';

implementation

end.
