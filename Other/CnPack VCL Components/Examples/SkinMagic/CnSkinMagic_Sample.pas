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

{------------------------------------------------------------------------------}
{ ��Ԫ����: CnSkinMagic_Sample.pas                                             }
{                                                                              }
{ ��Ԫ����: savetime (savetime2k@hotmail.com, http://savetime.delphibbs.com)   }
{ ��������: 2004-12-03                                                         }
{                                                                              }
{ ���ܽ���:                                                                    }
{     SkinMagic ���ʹ�þ���                                                   }
{                                                                              }
{ ʹ��˵��:                                                                    }
{                                                                              }
{ ������ʷ:                                                                    }
{                                                                              }
{ �д�����:                                                                    }
{                                                                              }
{------------------------------------------------------------------------------}
unit CnSkinMagic_Sample;

interface

uses
  Windows, Messages, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, Mask,
  DBCtrls, CnSkinMagic;

implementation

type

  TControlEx = class(TControl)
  public
    property Color;
  end;

procedure DrawLineLeftTop(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  MoveToEx(DC, 0, 0, nil); LineTo(DC, Pt.X - 1, 0);
  MoveToEx(DC, 0, 1, nil); LineTo(DC, 0, Pt.Y - 1);
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawLineLeftTop2(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  MoveToEx(DC, 1, 1, nil); LineTo(DC, Pt.X - 2, 1);
  MoveToEx(DC, 1, 2, nil); LineTo(DC, 1, Pt.Y - 2);
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawFrame(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
  Brush: HBRUSH;
begin
  Pen := SelectObject(DC, CreatePen(PS_INSIDEFRAME, 1, ColorToRGB(Color)));
  Brush := SelectObject(DC, GetStockObject(NULL_BRUSH));
  Rectangle(DC, 0, 0, Pt.X, Pt.Y);
  DeleteObject(SelectObject(DC, Pen));
  SelectObject(DC, Brush);
end;

procedure Cn_WindowProc_FrameWindow(Self: TControlSubClass; var Message: TMessage);
var
  Wnd: HWND;
  DC: HDC;
  Pt: TPoint;
  Control: TControl;          // ʹ����ʱ�������ٶ������õĿ���
  WinControl: TWinControl;
begin
  Self.OldWindowProc(Message);

  if (Message.Msg = WM_PAINT) or
     (Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONUP) or
     (Message.Msg = CM_MOUSEENTER)  or (Message.Msg = CM_MOUSELEAVE) or
     (Message.Msg = WM_KILLFOCUS)   or (Message.Msg = WM_SETFOCUS) then
  begin
    Control := Self.Control;
    Wnd := 0;
    WinControl := nil;
    
    if Self.IsWinControl then
    begin
      WinControl := TWinControl(Control);
      Wnd := WinControl.Handle;
      DC := GetWindowDC(Wnd);
    end
    else
    begin
      DC := HDC(Message.WParam);
    end;

    Pt.X := Control.Width;
    Pt.Y := Control.Height;

    if Self.IsWinControl then
    begin
      if(WinControl is TEdit) or (WinControl is TListBox) or
        (WinControl is TMemo) or (WinControl is TComboBox) or
        (WinControl is TMaskEdit) or (WinControl is TDBEdit) or
        (WinControl is TDBMemo) then
      begin
        DrawLineLeftTop(DC, Pt, ColorToRGB(clBtnShadow));
        if Self.MouseInControl or WinControl.Focused then
          DrawLineLeftTop2(DC, Pt, ColorToRGB(cl3DDkShadow))
        else
          DrawLineLeftTop2(DC, Pt, ColorToRGB(clBtnFace));
      end
      else if (WinControl is TButton) or (WinControl is TBitBtn) then
      begin
        if Self.MouseInControl or WinControl.Focused then
          DrawLineLeftTop(DC, Pt, ColorToRGB(cl3DDkShadow))
        else
          DrawLineLeftTop(DC, Pt, ColorToRGB(clBtnShadow));

        DrawLineLeftTop2(DC, Pt, ColorToRGB(clBtnHighlight));
      end
      else
      begin
        DrawFrame(DC, Pt, ColorToRGB(clRed));   // �������ͻ�����ɫ��
      end;
    end
    else      // �� WinControl ��, ������ɫ��
    begin
      DrawFrame(DC, Pt, ColorToRGB(clBlue));
    end;

    if Self.IsWinControl then
      ReleaseDC(Wnd, DC);
  end;
end;


initialization
  // TWinControl
  TCnSkinMagic.RegisterClass(TEdit, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TListBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TMemo, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TCheckBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TRadioButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TRadioGroup, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TPanel, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TComboBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TBitBtn, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TMaskEdit, @Cn_WindowProc_FrameWindow);

  // TControl
  TCnSkinMagic.RegisterClass(TSpeedButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TBevel, @Cn_WindowProc_FrameWindow);

  // DB Controls
  TCnSkinMagic.RegisterClass(TDBEdit, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TDBMemo, @Cn_WindowProc_FrameWindow);
end.
