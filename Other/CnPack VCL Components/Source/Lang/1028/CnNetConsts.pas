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

unit CnNetConsts;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ���Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫ����������ͨѶ���õ�����Դ�ַ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2005.12.24 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnRS232
  SInvalidXonXoffChar = 'Xon�r�ŻPXoff�r�Ť���ۦP';
  SSerialPortAlreadyOpened = '�Ӧ�f�w�g���}�F';
  SSerialPortOpenError = '��f���}����';
  SNotACommHandle = '���w���]�Ƥ��O�@�Ӧ��Ī��q�T�ݤf';
  SSetupBuffFail = '�]�m��f�q�T�w�İϥ���';
  SCreateEventFail = '�Ыبƥ󥢱�';
  SCreateReadFail = '�Ы�Ū�ƾڽu�{����';
  SCreateWriteFail = '�Ыؼg�ƾڽu�{����';
  SCnRS232Name = 'RS232��f�q�T�ե�';
  SCnRS232Comment = 'RS232��f�q�T�ե�';

  // CnModem
  SCnModemName = '�зǽջs�ѽվ��q�T�ե�';
  SCnModemComment = '�зǽջs�ѽվ��q�T�ե�';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232��f�]�m��ܮزե�';
  SCnRS232DialogComment = 'RS232��f�]�m��ܮزե�';
  SRS232Option = '��f�]�m';
  SRS232TimeoutsOption = '�W�ɳ]�m';
  SBaudRateError = '�i�S�v�u���J���';
  SInputASCIICode = '�п�JASCII�s�X(0..255)';
  SInputInteger = '�п�J���';

  // CnPing
  SCnPingName = 'Ping�ե�';
  SCnPingComment = 'Ping�ե�';
  SInitFailed = 'Winsock ��l�ƥ��ѡA�i��O���������T';
  SInvalidAddr = 'IP�a�}���X�k';
  SNoResponse = '[%0:S] �S���T��';
  STimeOut = '�T���W��';
  SICMPRunError = 'ICMP�B����~';
  SPingResultString = '[%0:S]: �r�`��:%1:D �ɶ�: %2:Dms TTL: %3:D';

  // CnIP
  SCnIPName = 'IP�ե�';
  SCnIPComment = 'IP�ե�';
  SCnErrorAddress = '���~��IP�a�}';
  SCnErrorAddrRang = '�W�XIP�a�}�d��';

  // CnDNS
  SCnDNSName = 'DNS�ե�';
  SCnDNSComment = 'DNS�ե�';
  SCnDNSTooLong = '�r�Ŧ���׶W��';
  SCnDNSInvalidHeadByteFmt = '�r�Ŧ���� %d ��� %d �B�D�k';

implementation

end.
