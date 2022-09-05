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

unit CnVclFmxMixed;
{* |<PRE>
================================================================================
* ������ƣ�CnPack IDE ר�Ұ�
* ��Ԫ���ƣ������� FMX ��ع��ܵĹ��̿��ϵ�Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע���õ�Ԫ�� CnCommon ���������������Щ��Ҫͬʱ���� VCL �� FMX �����ݣ�
*           ��Ҫ�� CnFmxUtils ���ʹ�ã��Լ����� CnCommon ���ֲ��� FMX ʱ�������
* ����ƽ̨��WinXP + Delphi XE2
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2018.12.24 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Windows
{$IFDEF SUPPORT_FMX}
  , CnFmxUtils
{$ENDIF};

function GetControlScreenRect(AControl: TComponent): TRect;
{* ���ؿؼ�����Ļ�ϵ�������������� FMX����Ҫ���ص����������������������ꡣ��Ŀǰʵ���� Bug��ֻ��ʹ��������ꡣ }

procedure SetControlScreenRect(AControl: TComponent; ARect: TRect);
{* ���ÿؼ�����Ļ�ϵ�������������� FMX����Ҫ��������������������������ꡣ��Ŀǰʵ���� Bug��ֻ��ʹ��������ꡣ }

function GetControlParent(AControl: TComponent): TComponent;
{* ��װ�Ļ�ȡ Conrol �� Parent �Ĺ��̣���װ�� FMX ��ʵ��}

function GetControlTop(AControl: TComponent): Integer;
{* ��װ�Ļ�ȡ Control �� Top �Ĺ��̣���װ�� FMX ��ʵ��}

function GetControlLeft(AControl: TComponent): Integer;
{* ��װ�Ļ�ȡ Control �� Left �Ĺ��̣���װ�� FMX ��ʵ��}

function GetControlWidth(AControl: TComponent): Integer;
{* ��װ�Ļ�ȡ Control �� Width �Ĺ��̣���װ�� FMX ��ʵ��}

function GetControlHeight(AControl: TComponent): Integer;
{* ��װ�Ļ�ȡ Control �� Height �Ĺ��̣���װ�� FMX ��ʵ��}

procedure SetControlTop(AControl: TComponent; AValue: Integer);
{* ��װ������ Control �� Top �Ĺ��̣���װ�� FMX ��ʵ��}

procedure SetControlLeft(AControl: TComponent; AValue: Integer);
{* ��װ������ Control �� Left �Ĺ��̣���װ�� FMX ��ʵ��}

procedure SetControlWidth(AControl: TComponent; AValue: Integer);
{* ��װ������ Control �� Width �Ĺ��̣���װ�� FMX ��ʵ��}

procedure SetControlHeight(AControl: TComponent; AValue: Integer);
{* ��װ������ Control �� Height �Ĺ��̣���װ�� FMX ��ʵ��}

procedure ControlBringToFront(AControl: TComponent);
{* ��װ������ Control �� BringToFront �Ĺ��̣���װ�� FMX ��ʵ��}

procedure ControlSendToBack(AControl: TComponent);
{* ��װ������ Control �� SendToBack �Ĺ��̣���װ�� FMX ��ʵ��}

implementation

// ���ؿؼ�����Ļ�ϵ���������
function GetControlScreenRect(AControl: TComponent): TRect;
var
  AParent: TWinControl;
begin
  Assert(Assigned(AControl));
  if AControl is TControl then
  begin
    AParent := TControl(AControl).Parent;
    Assert(Assigned(AParent));
    with TControl(AControl) do
    begin
      Result.TopLeft := AParent.ClientToScreen(Point(Left, Top));
      Result.BottomRight := AParent.ClientToScreen(Point(Left + Width, Top + Height));
    end;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  if CnFmxIsInheritedFromControl(AControl) then
    Result := CnFmxGetControlRect(AControl);
{$ENDIF}
end;

// ���ÿؼ�����Ļ�ϵ���������
procedure SetControlScreenRect(AControl: TComponent; ARect: TRect);
var
  AParent: TWinControl;
  P1, P2: TPoint;
begin
  Assert(Assigned(AControl));
  if AControl is TControl then
  begin
    AParent := TControl(AControl).Parent;
    Assert(Assigned(AParent));
    P1 := AParent.ScreenToClient(ARect.TopLeft);
    P2 := AParent.ScreenToClient(ARect.BottomRight);
    TControl(AControl).SetBounds(P1.x, P1.y, P2.x - P1.x, P2.y - P1.y);
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  if CnFmxIsInheritedFromControl(AControl) then
    CnFmxSetControlRect(AControl, ARect);
{$ENDIF}
end;

// ��װ�Ļ�ȡ Conrol �� Parent �Ĺ��̣���װ�� FMX ��ʵ��
function GetControlParent(AControl: TComponent): TComponent;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Parent;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlParent(AControl);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

// ��װ�Ļ�ȡ Control �� Top �Ĺ��̣���װ�� FMX ��ʵ��
function GetControlTop(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Top;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptTop);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// ��װ�Ļ�ȡ Control �� Left �Ĺ��̣���װ�� FMX ��ʵ��
function GetControlLeft(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Left;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptLeft);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// ��װ�Ļ�ȡ Control �� Width �Ĺ��̣���װ�� FMX ��ʵ��}
function GetControlWidth(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Width;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptWidth);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// ��װ�Ļ�ȡ Control �� Height �Ĺ��̣���װ�� FMX ��ʵ��}
function GetControlHeight(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Height;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptHeight);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// ��װ������ Control �� Top �Ĺ��̣���װ�� FMX ��ʵ��}
procedure SetControlTop(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Top := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptTop);
{$ENDIF}
end;

{* ��װ������ Control �� Left �Ĺ��̣���װ�� FMX ��ʵ��}
procedure SetControlLeft(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Left := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptLeft);
{$ENDIF}
end;

{* ��װ������ Control �� Width �Ĺ��̣���װ�� FMX ��ʵ��}
procedure SetControlWidth(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Width := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptWidth);
{$ENDIF}
end;

{* ��װ������ Control �� Height �Ĺ��̣���װ�� FMX ��ʵ��}
procedure SetControlHeight(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Height := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptHeight);
{$ENDIF}
end;

// ��װ������ Control �� BringToFront �Ĺ��̣���װ�� FMX ��ʵ��
procedure ControlBringToFront(AControl: TComponent);
begin
  if AControl is TControl then
  begin
    TControl(AControl).BringToFront;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxControlBringToFront(AControl);
{$ENDIF}
end;

// ��װ������ Control �� SendToBack �Ĺ��̣���װ�� FMX ��ʵ��
procedure ControlSendToBack(AControl: TComponent);
begin
  if AControl is TControl then
  begin
    TControl(AControl).SendToBack;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxControlSendToBack(AControl);
{$ENDIF}
end;

end.
