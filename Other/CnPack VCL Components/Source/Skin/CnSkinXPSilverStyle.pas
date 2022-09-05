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

unit CnSkinXPSilverStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPSilverStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;  
  end;

implementation

{$R CnSkinXPSilverStyle.res}

const
  SCN_SKIN_XPSILVER_BACKGROUND      = 'CN_SKIN_XPSILVER_BACKGROUND';
  SCN_SKIN_XPSILVER_BUTTON          = 'CN_SKIN_XPSILVER_BUTTON';
  SCN_SKIN_XPSILVER_CHECKBOX        = 'CN_SKIN_XPSILVER_CHECKBOX';
  SCN_SKIN_XPSILVER_COMBO           = 'CN_SKIN_XPSILVER_COMBO';
  SCN_SKIN_XPSILVER_RADIO           = 'CN_SKIN_XPSILVER_RADIO';
  SCN_SKIN_XPSILVER_SCROLLBAR       = 'CN_SKIN_XPSILVER_SCROLLBAR';
  SCN_SKIN_XPSILVER_WINDOW          = 'CN_SKIN_XPSILVER_WINDOW';
  SCN_SKIN_XPSILVER_WINDOW_BUTTON   = 'CN_SKIN_XPSILVER_WINDOW_BUTTON';

{ TCnSkinXPSilverStyle }

procedure TCnSkinXPSilverStyle.InitConsts;
begin
  inherited;
  FaceColor := $DBE9EC;
  InactiveCaptionColor := $EEEEEE;
  ActiveCaptionColor := $F0F0F0;
  ShadowColor := $ADB397;
  LightColor := $F5F3F6;
  MenuHotColor := $66BEA4;
end;

procedure TCnSkinXPSilverStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPSILVER_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPSILVER_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPSILVER_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPSILVER_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPSILVER_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPSILVER_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPSILVER_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.