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

unit CnGraphRegister;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ���ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.03.30 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  CnConsts, CnGraphics, CnImage, CnSpin, CnGraphPropEditors, CnCheckTreeView,
  CnWizardImage, CnEdit, CnShellCtrls, CnWaterImage, CnAOTreeView, CnAACtrls,
  CnAAFont, CnAAFontEditor, CnAAFontDialog, CnTabSet, CnButtonEdit, CnPanel,
  CnSkinMagic, CnButtons, CnHexEditor, CnHint, CnGauge, CnListBox, CnColorGrid,
  CnMonthCalendar, CnValidateImage, CnErrorProvider, CnLED;

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
  //RegisterComponents(SCnGraphicPalette, [TCnPaintBox]);
  //RegisterComponents(SCnGraphicPalette, [TCnImage]);
  RegisterComponents(SCnGraphicPalette, [TCnButton, TCnBitBtn, TCnSpeedButton]);
  RegisterComponents(SCnGraphicPalette, [TCnPanel, TCnEdit, TCnSpinEdit]);
  RegisterComponents(SCnGraphicPalette, [TCnListBox]);
  RegisterComponents(SCnGraphicPalette, [TCnTabSet, TCnButtonEdit]);
  RegisterComponents(SCnGraphicPalette, [TCnCheckTreeView]);
  RegisterComponents(SCnGraphicPalette, [TCnAOTreeView]);
  RegisterComponents(SCnGraphicPalette, [TCnWizardImage]);
  RegisterComponents(SCnGraphicPalette, [TCnWaterImage]);
  RegisterComponents(SCnGraphicPalette, [TCnValidateImage]);
  RegisterComponents(SCnGraphicPalette, [TCnGauge]);
  RegisterComponents(SCnGraphicPalette, [TCnHexEditor]);
  RegisterComponents(SCnGraphicPalette, [TCnMonthCalendar]);
  RegisterComponents(SCnGraphicPalette, [TCnColorGrid]);
  RegisterComponents(SCnGraphicPalette, [TCnErrorProvider]);
  RegisterComponents(SCnGraphicPalette, [TCnLEDText]);
  RegisterComponents(SCnGraphicPalette, [TCnSkinMagic]);
  RegisterComponents(SCnGraphicPalette, [TCnHint, TCnHintWindow]);
  //RegisterComponents(SCnGraphicPalette, [TCnCheckGroupBox]);
  //RegisterPropertyEditor(TypeInfo(TCnBitmap), nil, '', TCnBitmapProperty);
  RegisterComponents(SCnGraphicPalette, [TCnShellTreeView, TCnShellListView,
    TCnShellChangeNotifier]);

  RegisterComponents(SCnGraphicPalette, [TCnAAFadeText, TCnAALabel, 
    TCnAALinkLabel, TCnAAText, TCnAAScrollText, TCnAAMarqueeText]);
  RegisterComponents(SCnGraphicPalette, [TCnAAFontDialog]);
  RegisterPropertyEditor(TypeInfo(TCnAAEffect), nil, '', TCnAAEffectProperty);
  RegisterComponentEditor(TCnAALabel, TCnAALabelEditor);
end;

end.
