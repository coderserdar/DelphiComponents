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

unit CnGraphConsts;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ���Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫ�����˽������õ�����Դ�ַ���
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.02.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

var
  SCnAOCaptionColor: string = '��ɫ(&C)';
  SCnAOCaptionFont: string = '����(&F)';
  SCnAOCaptionOption: string = '����(&O)';

  SCreateDCFromEmptyBmp: string = '����Ϊ��λͼ����DC';
  SAllocDIBFail: string = '����DIB������ʧ��';
  SCreateDCFail: string = '����DCʧ��';
  SSelectBmpToDCFail: string = '�޷���λͼ����ѡ��DC��';
  SBitmapIsEmpty: string = '�޷�����һ����λͼ����������';
  SInvalidPixel: string = '��Ч�����ص� x: %d, y: %d';
  SInvalidPixelF: string = '��Ч�����ص� x: %f, y: %f';
  SInvalidScanLine: string = '��Ч��ɨ���� Row: %d';
  SInvalidAlphaBitmap: string = '��Alpha��ϴ����У����ڻ�ϵ�ͼ���С�����뵱ǰͼ��һ��';
  SInvalidForeBitmap: string = '�������ɰ��ϴ����У�ǰ��ͼ���ɰ��С����һ��';
  SReadBmpError: string = '��λͼ���ݳ���';

  // CnSkinMagic �쳣��Ϣ
  SCNE_WRITEVMTFAILED: string = '�޷���д VMT ���ݣ�CnSkinMagic ��ע��ʧ��';
  SCNE_FINDCLASSDATAFAILED: string = '�޷���� CnSkingMagic ����Ϣ';
  SCNE_REGISTERMESSAGEFAILED: string = '�޷��� CnSkinMagic ע�ᴰ����Ϣ';

implementation

end.
