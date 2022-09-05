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

unit CnSkinXPGreenStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPGreenStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;
  end;

implementation

{$R CnSkinXPGreenStyle.res}

const
  SCN_SKIN_XPGREEN_BACKGROUND      = 'CN_SKIN_XPGREEN_BACKGROUND';
  SCN_SKIN_XPGREEN_BUTTON          = 'CN_SKIN_XPGREEN_BUTTON';
  SCN_SKIN_XPGREEN_CHECKBOX        = 'CN_SKIN_XPGREEN_CHECKBOX';
  SCN_SKIN_XPGREEN_COMBO           = 'CN_SKIN_XPGREEN_COMBO';
  SCN_SKIN_XPGREEN_RADIO           = 'CN_SKIN_XPGREEN_RADIO';
  SCN_SKIN_XPGREEN_SCROLLBAR       = 'CN_SKIN_XPGREEN_SCROLLBAR';
  SCN_SKIN_XPGREEN_WINDOW          = 'CN_SKIN_XPGREEN_WINDOW';
  SCN_SKIN_XPGREEN_WINDOW_BUTTON   = 'CN_SKIN_XPGREEN_WINDOW_BUTTON';

{ TCnSkinXPGreenStyle }

procedure TCnSkinXPGreenStyle.InitConsts;
begin
  inherited;
  FaceColor := $E7DFE7;
  InactiveCaptionColor := $AAAAAA;
  ActiveCaptionColor := $333333;
  ShadowColor := $B5AEA5;
  LightColor := $F7EFF7;
  MenuHotColor := $C6B6BD;
end;

procedure TCnSkinXPGreenStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPGREEN_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPGREEN_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPGREEN_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPGREEN_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPGREEN_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPGREEN_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPGREEN_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.