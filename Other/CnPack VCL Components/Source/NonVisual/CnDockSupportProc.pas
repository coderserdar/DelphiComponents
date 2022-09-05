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

{*******************************************************}
{                                                       }
{       һЩͨ�õĺ���                                  }
{       CnDockSupportProc ��Ԫ                          }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockSupportProc;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�һЩͨ�õĺ�����Ԫ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.11.18 V1.1
*                wqyfavor ���� D2009 �µĲ���������
*           2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Classes, Windows, SysUtils, Graphics, Forms, Controls, Messages;

type
  TListScanKind = (lskForward, lskBackward);

{ ---------------------------------------------------------------------------- }
function Cn_StreamDataToString(Stream: TStream): string;
procedure Cn_StringToStreamData(Stream: TStream; Data: string);
{ ---------------------------------------------------------------------------- }
function Cn_FindDockFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockServerFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockClientFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockServerFromDockManager(FormName: string;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockClientFromDockManager(FormName: string;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockFormFromScreen(FormName: string;
              ScanKind: TListScanKind = lskForward): TCustomForm;
{ ---------------------------------------------------------------------------- }
function Cn_GetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
{ ---------------------------------------------------------------------------- }
function Cn_GetNoNClientMetrics: TNONCLIENTMETRICS;
{ ���ϵͳ�ı������ĸ߶� }
function Cn_GetSysCaptionHeight: Integer;
{ ���ϵͳ�Ĵ���ı߿� }
function Cn_GetSysBorderWidth: Integer;
function Cn_GetSysCaptionHeightAndBorderWidth: Integer;
{ ---------------------------------------------------------------------------- }
{ ��û�ı������Ŀ�ʼ��ɫ }
function Cn_GetActiveTitleBeginColor: TColor;
{ ��û�ı������Ľ�����ɫ }
function Cn_GetActiveTitleEndColor: TColor;
{ ��÷ǻ�ı������Ŀ�ʼ��ɫ }
function Cn_GetInactiveTitleBeginColor: TColor;
{ ��÷ǻ�ı������Ľ�����ɫ }
function Cn_GetInactiveTitleEndColor: TColor;
{ ��ñ�������������ɫ��Activeָʾ�Ƿ��ǻ�ý��� }
function Cn_GetTitleFontColor(Active: Boolean): TColor;
{ ��û�ı�������������ɫ }
function Cn_GetActiveTitleFontColor: TColor;
{ ��÷ǻ�ı�������������ɫ }
function Cn_GetInactiveTitleFontColor: TColor;
{ ��ñ����������� }
function Cn_GetTitleFont: TFont;
{ ��ס���� }
procedure Cn_LockWindow(Control: TWinControl);
{ �������� }
procedure Cn_UnLockWindow;
{ ---------------------------------------------------------------------------- }
{ ����һЩֵ����һ��TWMNCHitMessage�ṹ���ҷ��� }
function Cn_CreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
{ ��������Orient��ֵ }
function Cn_ExchangeOrient(Orient: TDockOrientation): TDockOrientation;
{ ���������Control��Align���Եõ����ķ��� }
function Cn_GetControlOrient(AControl: TControl): TDockOrientation;
{ ���������Control��Align���Եõ����Ŀ�Ȼ��߸߶� }
function Cn_GetControlSize(AControl: TControl): Integer;

implementation

uses
  Math, CnDockFormControl, CnDockGlobal;

var
  Cn_TitleFont: TFont;

function Cn_StreamDataToString(Stream: TStream): string;
var
  B: Byte;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(B, SizeOf(B));
    Result := Result + IntToHex(B, 2);
  end;
end;

procedure Cn_StringToStreamData(Stream: TStream; Data: string);
var
  i: Integer;
  B: Byte;
begin
  i := 1;
  while i < Length(Data) do
  begin
    B := StrToInt('$' + Copy(Data, i, 2));
    Stream.Write(B, SizeOf(B));
    Inc(i, 2);
  end;
end;

function Cn_FindDockFormWithName(FormName: string;
              FromDockManager: Boolean;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
begin
  Result := Cn_FindDockClientFormWithName(FormName, FromDockManager, FromList, ScanKind);
  if Result = nil then
    Result := Cn_FindDockServerFormWithName(FormName, FromDockManager, FromList, ScanKind);
end;

function Cn_FindDockServerFormWithName(FormName: string;
  FromDockManager: Boolean;
  FromList: Boolean;
  ScanKind: TListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := Cn_FindDockServerFromDockManager(FormName, FromList, ScanKind)
  else Result := Cn_FindDockFormFromScreen(FormName, ScanKind);
end;

function Cn_FindDockClientFormWithName(FormName: string;
  FromDockManager: Boolean;
  FromList: Boolean;
  ScanKind: TListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := Cn_FindDockClientFromDockManager(FormName, FromList, ScanKind)
  else Result := Cn_FindDockFormFromScreen(FormName, ScanKind);
end;

function Cn_FindDockServerFromDockManager(FormName: string;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to CnGlobalDockPresident.DockServersList.Count - 1 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockServersList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockServersList[i]);
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := CnGlobalDockPresident.DockServersList.Count - 1 downto 0 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockServersList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockServersList[i]);
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_FindDockClientFromDockManager(FormName: string;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to CnGlobalDockPresident.DockClientsList.Count - 1 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockClientsList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockClientsList[i]);
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := CnGlobalDockPresident.DockClientsList.Count - 1 downto 0 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockClientsList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockClientsList[i]);
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_FindDockFormFromScreen(FormName: string;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to Screen.CustomFormCount - 1 do
        if FormName = Screen.CustomForms[i].Name then
        begin
          Result := Screen.CustomForms[i];
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := Screen.CustomFormCount - 1 downto 0 do
        if FormName = Screen.CustomForms[i].Name then
        begin
          Result := Screen.CustomForms[i];
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_GetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
begin
  if (Scale < 0) or (Scale > 1) then
    Scale := 1;
  Result := Min(TBDockSize, Round(ControlSize * Scale));
end;

function Cn_GetNoNClientMetrics: TNONCLIENTMETRICS;
begin
  Result.cbSize := Sizeof(TNONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Result.cbSize,
    @Result, 0);
end;

function Cn_GetSysCaptionHeight: Integer;
begin
  Result := Cn_GetNoNClientMetrics.iCaptionHeight
end;

function Cn_GetSysBorderWidth: Integer;
begin
  Result := Cn_GetNoNClientMetrics.iBorderWidth;
end;

function Cn_GetSysCaptionHeightAndBorderWidth: Integer;
var NoNCM: TNONCLIENTMETRICS;
begin
  NoNCM := Cn_GetNoNClientMetrics;
  Result := NoNCM.iBorderWidth + NoNCM.iCaptionHeight;
end;

function Cn_GetActiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_ACTIVECAPTION);
end;

function Cn_GetActiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
end;

function Cn_GetInactiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTION);
end;

function Cn_GetInactiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
end;

function Cn_GetTitleFontColor(Active: Boolean): TColor;
begin
  if Active then
    Result := Cn_GetActiveTitleFontColor
  else Result := Cn_GetInactiveTitleFontColor;
end;

function Cn_GetActiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_CAPTIONTEXT);
end;

function Cn_GetInactiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
end;

{ ��ñ����������� }
function Cn_GetTitleFont: TFont;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  Result := Cn_TitleFont;
  NoNCM := Cn_GetNoNClientMetrics;
  Result.Handle := CreateFontIndirect(NoNCM.lfCaptionFont);
end;

procedure Cn_LockWindow(Control: TWinControl);
var
  Handle: HWND;
begin
  if Control = nil then
    Handle := GetDesktopWindow
  else
    Handle := Control.Handle;
  LockWindowUpdate(Handle);
end;

procedure Cn_UnLockWindow;
begin
  LockWindowUpdate(0);
end;

function Cn_CreateNCMessage(Control: TControl; Msg: Cardinal; 
  HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
begin
  { �������������TWMNCHitMessage��ֵ }
  Result.Msg := Msg;
  Result.HitTest := HTFlag;
  Pos := Control.ClientToScreen(Pos);
  Result.XCursor := Pos.X;
  Result.YCursor := Pos.Y;
end;

function Cn_ExchangeOrient(Orient: TDockOrientation): TDockOrientation;
begin
  case Orient of
    doHorizontal: Result := doVertical;
    doVertical: Result := doHorizontal;
  else
    Result := doNoOrient;
  end;
end;

function Cn_GetControlOrient(AControl: TControl): TDockOrientation;
begin
  Assert(AControl <> nil);
  Result := doNoOrient;
  case AControl.Align of
    alClient, alNone: Result := doNoOrient;
    alLeft, alRight:  Result := doVertical;
    alTop, alBottom:  Result := doHorizontal;
  end;
end;

function Cn_GetControlSize(AControl: TControl): Integer;
begin
  case Cn_GetControlOrient(AControl) of
    doVertical: Result := AControl.Width;
    doHorizontal: Result := AControl.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

initialization
  Cn_TitleFont := TFont.Create;

finalization
  Cn_TitleFont.Free;

end.
