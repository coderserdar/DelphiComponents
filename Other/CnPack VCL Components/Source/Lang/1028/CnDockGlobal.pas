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

{*******************************************************}
{                                                       }
{       ����һЩȫ�ֵı���                              }
{       CnDockGlobal ��Ԫ                               }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockGlobal;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�ͣ�������һЩȫ�ֱ�����Ԫ
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.17 V1.0
*                ���뵥Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, CnDockFormControl, CnDockInfo, CnDockSupportControl;

const
  {����W��}
  gs_CnProductName = 'CnPack Dock Component';
  gs_CnDcokServerName = '���a�A�ȱ��';
  gs_CnDcokClientName = '���a�Ȥᱱ�';
  gs_CnDockStyleName = '���a���汱�';
  {����}
  gs_CnDockManagerVersion = '1.0.0.0'; {���O�T�w��}
  gs_CnDockStyleVersion   = '1.0.0.0'; {���O�T�w��}
  {�ɶ�}
  gs_CnDockManagerCopyRightBegin = '2002';
  gs_CnDockManagerCopyRightEnd = '2003';
  gs_CnDockStyleCopyRightBegin = '2002';
  gs_CnDockStyleCopyRightEnd = '2003';
  {�@��}
  gs_CnAuthorName = '�P�q�i';
  gs_CnComparyName = '�٨S����';
  gs_CnHomePage = 'http://www.pigtwo.com' + #10#13 +
  'http://www.pigtwo.com/CtrlData/WebSite/luxiaoban.htm';
  gs_CnEmail = 'zhouyibo2000@sina.com' + #10#13 +
  'luxiaoban@sina.com';
  {����}
  gs_CnAbout = '����';
  gs_CnDockManagerAbout = '�o�O�@�� %s, �����O %s,' + #10#13 +
                          '���v: %s-%s, �@��: %s,���q: %s,' + #10#13 +
                          '�ӤH�D��: %s,' + #10#13 +
                          'Email: %s';
  gs_CnDockStyleAbout =   '�o�O�@�� %s, �����O %s,' + #10#13 +
                          '���v: %s-%s, �@��: %s,���q: %s,' + #10#13 +
                          '�ӤH�D��: %s,' + #10#13 +
                          'Email: %s';
  {�r�����βŸ�}
  gs_CnStringSplitter = ' ';
  gs_CnDockInfoSplitter = '@';

  {���ܸ�T}
  gs_CnDockTreeCloseBtnHint = '����';
  gs_CnVCDockTreeExpandBtnHint = '�X�i';
  gs_CnVSNETDockTreeAutoHideBtnHint = '�۰�����';
  gs_CnDockTreeVSplitterHint = '�������α�';
  gs_CnDockTreeHSplitterHint = '���Ǥ��α�';

  { Hash�����ܸ�T }
  gs_CnTableIndexError = '����޶W�X�d��';
  gs_CnNodeExistedError = '�`�I�w�g�s�b�F';
  gs_CnComProcError = '�������ƫ��Ь���';

  { CnDockTree�����~��T }
  gs_ControlCannotIsNil = '�Ѽ�Control���ରnil';
  gs_CannotGetValueWithNoOrient = '������o�S�����a��V��Control�����';
  gs_CannotSetValueWithNoOrient = '����]�m�S�����a��V��Control�����';

  { CnDockFormControl�����~��T }
  gs_CannotChangeDockStyleProperty = '����b�B�������DockStyle�ݩ�';
  gs_CannotLayAnother = '�b���W�w�g��F�@��%s,����A��t�@��%s�F';

  { CnDelphiDockStyle���H�� }
  gs_LikeDelphiStyle = '���� Delphi ��%s';

  { CnVCDockStyle���H�� }
  gs_LikeVCStyle = '���� Visual C++ ��%s';

  { CnVIDDockStyle���H�� }
  gs_CannotSetTabPosition = '����]�mTabPosition��tpLeft�Ϊ�tpRight';
  gs_LikeVIDStyle = '���� Visual InterDev ��%s';
  gs_TabPositionMustBetpBottom = 'TabPosition�@�w�n�]�m��tpBottom';

  { CnVSNETDockStyle���H�� }
  gs_LikeVSNETStyle = '���� Visual Studio.net ��%s';

  { CnEclipseDockStyle���H�� }
  gs_LikeEclipseStyle = '���� Java eclipse ��%s';

  { CnDcokInfo���H�� }
  gs_CannotFindWindow = '!@#�䤣��o�Ӫ��#@!';

  { DockTree������,��i�氱�a��T���˸��M�s�x���ɭԭn�Ψ쥦 }
  gs_BaseDockTreeVersion = $00040000;
  { DockTree��VC���� }
  gs_VCDockTreeVersion = $00040010;

  { ���a�Ȥ�M���A�����Z���p��DefExpandoRect�ɱN�o�Ͱ��a�ާ@ }
  DefExpandoRect = 10;

  { WM_NCxxx���}�l�MWM_NCxxx������ }
  WM_NCMOUSEFIRST = WM_NCMOUSEMOVE;
  WM_NCMOUSELAST  = WM_NCMBUTTONDBLCLK;

var
  { CnGlobalDockManager�Q�ߤ@�ЫءA�ΨӺ޲z���a��� }
  CnGlobalDockPresident: TCnDockPresident = nil;

  { �O�_���b�q�ɩε��U���˸����a��T }
  IsLoading: Boolean = False;

  { ��ƹ�����������W�����D��ɡA���W����TCnDockClient��ȵ�GlobalDockClient }
  GlobalDockClient: TCnDockClient = nil;
  
implementation

end.
