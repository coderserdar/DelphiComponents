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

unit CnSkinXPBlueStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPBlueStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;
  end;

implementation

{$R CnSkinXPBlueStyle.res}

{ TCnSkinXPBlueStyle }

const
  SCN_SKIN_XPBLUE_BACKGROUND      = 'CN_SKIN_XPBLUE_BACKGROUND';
  SCN_SKIN_XPBLUE_BUTTON          = 'CN_SKIN_XPBLUE_BUTTON';
  SCN_SKIN_XPBLUE_CHECKBOX        = 'CN_SKIN_XPBLUE_CHECKBOX';
  SCN_SKIN_XPBLUE_COMBO           = 'CN_SKIN_XPBLUE_COMBO';
  SCN_SKIN_XPBLUE_RADIO           = 'CN_SKIN_XPBLUE_RADIO';
  SCN_SKIN_XPBLUE_SCROLLBAR       = 'CN_SKIN_XPBLUE_SCROLLBAR';
  SCN_SKIN_XPBLUE_WINDOW          = 'CN_SKIN_XPBLUE_WINDOW';
  SCN_SKIN_XPBLUE_WINDOW_BUTTON   = 'CN_SKIN_XPBLUE_WINDOW_BUTTON';

procedure TCnSkinXPBlueStyle.InitConsts;
begin
  inherited;
  FaceColor := $DBE9EC;
  InactiveCaptionColor := $DDDDDD;
  ActiveCaptionColor := $EEEEEE;
  ShadowColor := $ADB397;
  LightColor := $F5F3F6;
  MenuHotColor := $D58656;
end;

procedure TCnSkinXPBlueStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPBLUE_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPBLUE_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPBLUE_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPBLUE_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPBLUE_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPBLUE_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPBLUE_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.