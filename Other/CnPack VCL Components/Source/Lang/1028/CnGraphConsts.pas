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
  SCnAOCaptionColor: string = '�C��(&C)';
  SCnAOCaptionFont: string = '�r��(&F)';
  SCnAOCaptionOption: string = '�]�m(&O)';

  SCreateDCFromEmptyBmp: string = '���ର�Ŧ�Ϥ��tDC';
  SAllocDIBFail: string = '�Ы�DIB�ﹳ�y�`����';
  SCreateDCFail: string = '�Ы�DC����';
  SSelectBmpToDCFail: string = '�L�k�N��Ϲﹳ��ܨ�DC��';
  SBitmapIsEmpty: string = '�L�k�X�ݤ@�ӪŦ�Ϫ��H���ƾ�';
  SInvalidPixel: string = '�L�Ī��H���I x: %d, y: %d';
  SInvalidPixelF: string = '�L�Ī��H���I x: %f, y: %f';
  SInvalidScanLine: string = '�L�Ī����˽u Row: %d';
  SInvalidAlphaBitmap: string = '�bAlpha�V�X�B�z���A�Ω�V�X���Ϲ��j�p�����P��e�Ϲ��@�P';
  SInvalidForeBitmap: string = '�b�r��X�O�V�X�B�z���A�e���ϻP�X�O�j�p�����@�P';
  SReadBmpError: string = 'Ū��ϼƾڥX��';

  // CnSkinMagic ���`�H��
  SCNE_WRITEVMTFAILED: string = '�L�k��g VMT �ƾڡACnSkinMagic �����U����';
  SCNE_FINDCLASSDATAFAILED: string = '�L�k��o CnSkingMagic ���H��';
  SCNE_REGISTERMESSAGEFAILED: string = '�L�k�� CnSkinMagic ���U���f����';

implementation

end.
