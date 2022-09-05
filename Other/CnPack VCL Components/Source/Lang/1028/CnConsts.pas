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

unit CnConsts;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2004.09.18 V1.2
*                ����CnMemProf���ַ�������
*           2002.04.18 V1.1
*                ���������ַ�������
*           2002.04.08 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows;

//==============================================================================
// ���ݭn���a�ƪ��r�Ŧ�
//==============================================================================

resourcestring

  // ���U����|
  SCnPackRegPath = '\Software\CnPack';

  // ���U�u����|
  SCnPackToolRegPath = 'CnTools';

//==============================================================================
// �ݭn���a�ƪ��r�Ŧ�
//==============================================================================


var
  // ���@�H��
  SCnInformation: string = '����';
  SCnWarning: string = 'ĵ�i';
  SCnError: string = '���~';
  SCnEnabled: string = '����';
  SCnDisabled: string = '�T��';
  SCnMsgDlgOK: string = '�T�{(&O)';
  SCnMsgDlgCancel: string = '����(&C)';

const
  // �}�o�]�H��
  SCnPackAbout = 'CnPack';
  SCnPackVer = 'Ver 0.0.8.8';
  SCnPackStr = SCnPackAbout + ' ' + SCnPackVer;
  SCnPackUrl = 'http://www.cnpack.org';
  SCnPackBbsUrl = 'http://bbs.cnpack.org';
  SCnPackNewsUrl = 'news://news.cnpack.org';
  SCnPackEmail = 'master@cnpack.org';
  SCnPackBugEmail = 'bugs@cnpack.org';
  SCnPackSuggestionsEmail = 'suggestions@cnpack.org';

  SCnPackDonationUrl = 'http://www.cnpack.org/foundation.php';
  SCnPackDonationUrlSF = 'http://sourceforge.net/donate/index.php?group_id=110999';
  SCnPackGroup = 'CnPack �}�o��';
  SCnPackCopyright = '(C)Copyright 2001-2020 ' + SCnPackGroup;

  // CnPropEditors
  SCopyrightFmtStr =
    SCnPackStr + #13#10#13#10 +
    '�ե�W��: %s' + #13#10 +
    '�ե�@��: %s(%s)' + #13#10 +
    '�ե󻡩�: %s' + #13#10#13#10 +
    '�U������: ' + SCnPackUrl + #13#10 +
    '�޳N���: ' + SCnPackEmail + #13#10#13#10 +
    SCnPackCopyright;

resourcestring

  // �ե�w�˭��O�W
  SCnNonVisualPalette = 'CnPack Tools';
  SCnGraphicPalette = 'CnPack VCL';
  SCnNetPalette = 'CnPack Net';
  SCnDatabasePalette = 'CnPack DB';
  SCnReportPalette = 'CnPack Report';

  // �}�o�զ����H���Цb�᭱�K�[�A�`�N���a�ƳB�z
var
  SCnPack_Zjy: string = '�P�l��';
  SCnPack_Shenloqi: string = '�H�s�j(Chinbo)';
  SCnPack_xiaolv: string = '�f���y';
  SCnPack_Flier: string = 'Flier Lu';
  SCnPack_LiuXiao: string = '�B�S(Passion)';
  SCnPack_PanYing: string = '���N(Pan Ying)';
  SCnPack_Hubdog: string = '����(Hubdog)';
  SCnPack_Wyb_star: string = '�����_';
  SCnPack_Licwing: string = '���U(Licwing Zue)';
  SCnPack_Alan: string = '�i��(Alan)';
  SCnPack_Aimingoo: string = '�P�R��(Aimingoo)';
  SCnPack_QSoft: string = '��M(QSoft)';
  SCnPack_Hospitality: string = '�i�R�a(Hospitality)';
  SCnPack_SQuall: string = '�Bâ(SQUALL)';
  SCnPack_Hhha: string = 'Hhha';
  SCnPack_Beta: string = '����(beta)';
  SCnPack_Leeon: string = '���_(Leeon)';
  SCnPack_SuperYoyoNc: string = '�\�l��';
  SCnPack_JohnsonZhong: string = 'Johnson Zhong';
  SCnPack_DragonPC: string = 'Dragon P.C.';
  SCnPack_Kendling: string = '�p�V(Kending)';
  SCnPack_ccrun: string = 'ccRun(�ѧ�)';
  SCnPack_Dingbaosheng: string = 'dingbaosheng';
  SCnPack_LuXiaoban: string = '�P�q�i(�|�p�Z)';
  SCnPack_Savetime: string = 'savetime';
  SCnPack_solokey: string = 'solokey';
  SCnPack_Bahamut: string = '�ګ��i�S';
  SCnPack_Sesame: string = '�J���x(Sesame)';
  SCnPack_BuDeXian: string = '���o��';
  SCnPack_XiaoXia: string = '�p�L';
  SCnPack_ZiMin: string = '�l��';

  // CnCommon
  SUnknowError: string = '�������~';
  SErrorCode: string = '���~�N�X�G';

const
  SCnPack_ZjyEmail = 'zjy@cnpack.org';
  SCnPack_ShenloqiEmail = 'Shenloqi@hotmail.com';
  SCnPack_xiaolvEmail = 'xiaolv888@etang.com';
  SCnPack_FlierEmail = 'flier_lu@sina.com';
  SCnPack_LiuXiaoEmail = 'liuxiao@cnpack.org';
  SCnPack_PanYingEmail = 'panying@sina.com';
  SCnPack_HubdogEmail = 'hubdog@263.net';
  SCnPack_Wyb_starMail = 'wyb_star@sina.com';
  SCnPack_LicwingEmail = 'licwing@chinasystemsn.com';
  SCnPack_AlanEmail = 'BeyondStudio@163.com';
  SCnPack_AimingooEmail = 'aim@263.net';
  SCnPack_QSoftEmail = 'hq.com@263.net';
  SCnPack_HospitalityEmail = 'Hospitality_ZJX@msn.com';
  SCnPack_SQuallEmail = 'squall_sa@163.com';
  SCnPack_HhhaEmail = 'Hhha@eyou.com';
  SCnPack_BetaEmail = 'beta@01cn.net';
  SCnPack_LeeonEmail = 'real-like@163.com';
  SCnPack_SuperYoyoNcEmail = 'superyoyonc@sohu.com';
  SCnPack_JohnsonZhongEmail = 'zhongs@tom.com';
  SCnPack_DragonPCEmail = 'dragonpc@21cn.com';
  SCnPack_KendlingEmail = 'kendling@21cn.com';
  SCnPack_ccRunEmail = 'info@ccrun.com';
  SCnPack_DingbaoshengEmail = 'yzdbs@msn.com';
  SCnPack_LuXiaobanEmail = 'zhouyibo2000@sina.com';
  SCnPack_SavetimeEmail = 'savetime2k@hotmail.com';
  SCnPack_solokeyEmail = 'crh611@163.com';
  SCnPack_BahamutEmail = 'fantasyfinal@126.com';
  SCnPack_SesameEmail = 'sesamehch@163.com';
  SCnPack_BuDeXianEmail = 'appleak46@yahoo.com.cn';
  SCnPack_XiaoXiaEmail = 'summercore@163.com';
  SCnPack_ZiMinEmail: string = '441414288@qq.com';

  // CnMemProf
  SCnPackMemMgr = '���s�޲z�ʵ���';
  SMemLeakDlgReport = '�X�{ %d �B���s�|�}[�������s�޲z�����e�w���t %d �B]�C';
  SMemMgrODSReport = '��� = %d�A���� = %d�A�����t = %d';
  SMemMgrOverflow = '���s�޲z�ʵ������w�C���X�A�мW�j�C���ơI';
  SMemMgrRunTime = '%d �p�� %d �� %d ��C';
  SOldAllocMemCount = '�������s�޲z���e�w���t %d �B���s�C';
  SAppRunTime = '�{�ǹB��ɶ�: ';
  SMemSpaceCanUse = '�i�Φa�}�Ŷ�: %d �d�r�`';
  SUncommittedSpace = '�����泡��: %d �d�r�`';
  SCommittedSpace = '�w���泡��: %d �d�r�`';
  SFreeSpace = '�Ŷ�����: %d �d�r�`';
  SAllocatedSpace = '�w���t����: %d �d�r�`';
  SAllocatedSpacePercent = '�a�}�Ŷ����J: %d%%';
  SFreeSmallSpace = '�����p�Ŷ����s��: %d �d�r�`';
  SFreeBigSpace = '�����j�Ŷ����s��: %d �d�r�`';
  SUnusedSpace = '�䥦���Τ��s��: %d �d�r�`';
  SOverheadSpace = '���s�޲z������: %d �d�r�`';
  SObjectCountInMemory = '���s�ﹳ�ƥ�: ';
  SNoMemLeak = '�S�����s���|�C';
  SNoName = '(���R�W)';
  SNotAnObject = '���O�ﹳ';
  SByte = '�r�`';
  SCommaString = '�A';
  SPeriodString = '�C';

implementation

end.

