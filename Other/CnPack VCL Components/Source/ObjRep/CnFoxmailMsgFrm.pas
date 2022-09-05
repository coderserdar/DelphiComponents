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

unit CnFoxmailMsgFrm;
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ���Foxmail��̬��ʾ���嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע����̬��ʾ�����Ƿ�ģ̬���壬����ģ��Foxmail V4.0����ʾ����
*           �õ�Ԫ�ṩ���¼�������������ʾ��̬��ʾ���壺
*             ShowMsg     - ��ʾָ�����͵Ķ�̬����
*             ShowInfo    - ��ʾ����ʾ�����͵Ķ�̬����
*             ShowWarning - ��ʾ�����桱���͵Ķ�̬����
*             ShowError   - ��ʾ���������͵Ķ�̬����
*           ͨ������ȫ�ֱ�����ָ����������
*             ShowPos     - ָ��������ֵ�λ��
*             ShowDelay   - ָ��������ʾ��ʱ���ڴ����ϵ�������ǿ����ر�
*           ���⣬����ͨ�����ø���ɫֵָ��������ɫ��
* ʹ�÷���������Ҫ��ʾ��ʾ���ڵĵ�Ԫ��uses����Ԫ������Ҫ��ʾ��ʾ��Ϣʱֱ��
*           ֱ�ӵ���ShowXXXX���̼��ɡ�
* ע�����ͬһʱ����Ļ�Ͽ�ͬʱ���ֶ����ʾ���壬�໥�ص���
*           ����ֱ�Ӵ�������ʵ����
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ����в����ϱ��ػ�����ʽ
* �޸ļ�¼��2002.10.11 V1.1 by Chinbo
*                �����̶߳�ʱ����ʱ�������رճ���ʱ���������
*           2002.04.03 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnConsts;

type
  TMsgKind = (mkError, mkInfo, mkWarning);
  {* ��ʾ��������
   |<PRE>
     mkError:           - ���󴰿ڣ�Ĭ��Ϊ��ɫ
     mkInfo:            - ��ʾ���ڣ�Ĭ��Ϊ��ɫ
     mkWarning:         - ���洰�ڣ�Ĭ��Ϊ��ɫ
   |</PRE>}

  TShowPos = (spLeft, spRight, spRightTop, spRightBottom);
  {* ��ʾ���ڳ���λ��
   |<PRE>
     spLeft:            - ����������Ƴ�
     spRight:           - �������ұ��Ƴ�
     spRightTop:        - ���������Ͻ��Ƴ�
     spRightBottom:     - ���������½��Ƴ�
   |</PRE>}

{ TMsgForm }

  TMsgForm = class(TForm)
    lblMsg: TLabel;
    imgWarning: TImage;
    lblIcon: TLabel;
    imgError: TImage;
    imgInfo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure tmFadeInTimer(Sender: TObject);
    procedure tmDelayTimer(Sender: TObject);
    procedure tmFadeOutTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FMsg: string;
    Bmp: TBitmap;
    FMsgKind: TMsgKind;
    StartColor, EndColor: TColor;
    FMsgPos: TShowPos;
    InTimer: Integer;
    InValue: Integer;
    InDraw: Boolean;
    tmFadeIn: TTimer;
    tmFadeOut: TTimer;
    tmDelay: TTimer;
    procedure SetMsg(const Value: string);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetMsgKind(const Value: TMsgKind);
    procedure SetMsgPos(const Value: TShowPos);
    procedure CreateTimers;
  protected
    function CalcRect(MaxWidth: Integer; const ACap: string; AData: Pointer):
      TRect;
    procedure DrawBk;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Msg: string read FMsg write SetMsg;
    property MsgKind: TMsgKind read FMsgKind write SetMsgKind;
    property MsgPos: TShowPos read FMsgPos write SetMsgPos;
  end;

procedure ShowMsg(const Info: string; Kind: TMsgKind = mkInfo); overload;
{* �Է�ģ̬��ʽ��ʾ��̬��ʾ���ڣ�����Ϊ��ʾ���ݺ���ʾ����}
procedure ShowInfo(const Info: string);
{* �Է�ģ̬��ʽ��ʾ��ʾ���ڣ�����Ϊ��ʾ����}
procedure ShowWarning(const Info: string);
{* �Է�ģ̬��ʽ��ʾ���洰�ڣ�����Ϊ��ʾ����}
procedure ShowError(const Info: string);
{* �Է�ģ̬��ʽ��ʾ���󴰿ڣ�����Ϊ��ʾ����}

var
  ShowPos: TShowPos = spRightBottom;
  {* ��ʾ���ڳ��ֵ�λ�ã�Ĭ��Ϊ���ұ߳��֣��û����޸�}
  ShowDelay: Integer = 5;
  {* ��ʾ������ʾ��ʱ��Ĭ��Ϊ��5�룬�û����޸�}

  InfoStartColor: TColor = clWhite;
  {* ��ʾ���ʹ�����ʼ��ɫ��Ĭ��Ϊ��ɫ���û����޸�}
  InfoEndColor: TColor = $00F0E080;
  {* ��ʾ���ʹ��ڽ�����ɫ��Ĭ��Ϊǳ��ɫ���û����޸�}
  WarningStartColor: TColor = clWhite;
  {* �������ʹ�����ʼ��ɫ��Ĭ��Ϊ��ɫ���û����޸�}
  WarningEndColor: TColor = $0080F0E0;
  {* �������ʹ�����ʼ��ɫ��Ĭ��Ϊǳ��ɫ���û����޸�}
  ErrorStartColor: TColor = clWhite;
  {* �������ʹ�����ʼ��ɫ��Ĭ��Ϊ��ɫ���û����޸�}
  ErrorEndColor: TColor = $008080F0;
  {* �������ʹ�����ʼ��ɫ��Ĭ��Ϊǳ��ɫ���û����޸�}

implementation

{$R *.DFM}

var
  ThisList: TThreadList;

function GetWorkRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0)
end;

procedure CleanUp;
var
  iLoop: Integer;
begin
  for iLoop := ThisList.LockList.Count - 1 downto 0 do
    TForm(ThisList.LockList.Items[iLoop]).Free;
end;

procedure ShowMsg(const Info: string; Kind: TMsgKind);
var
  H: Integer;
begin
  with TMsgForm.Create(nil) do
  begin
    Msg := Info;
    MsgKind := Kind;
    MsgPos := ShowPos;
    H := CalcRect(lblMsg.Width, Info, nil).Bottom - lblMsg.Height;
    if H > 0 then
    begin
      lblMsg.Height := lblMsg.Height + H;
      Height := Height + H;
      //û�п��ǳ�����Ļ�߶ȵ�����
    end;
    ShowWindow(Handle, SW_SHOWNOACTIVATE); // ���岻��Ծ
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOOWNERZORDER); // ������ʾ������������
    FormShow(nil);
  end;
end;

procedure ShowInfo(const Info: string);
begin
  ShowMsg(Info, mkInfo);
end;

procedure ShowWarning(const Info: string);
begin
  ShowMsg(Info, mkWarning);
end;

procedure ShowError(const Info: string);
begin
  ShowMsg(Info, mkError);
end;

{ TMsgForm }

procedure TMsgForm.FormCreate(Sender: TObject);
begin
  Left := -300;
  Top := Screen.Height + 300;
  with ThisList.LockList do
  try
    Add(Self);
  finally
    ThisList.UnlockList;
  end;
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  CreateTimers;
  tmFadeIn.Enabled := True;
end;

procedure TMsgForm.FormShow(Sender: TObject);
begin
  case MsgPos of
    spLeft:
      begin
        Top := (GetWorkRect.Bottom - Height) div 2;
        Left := GetWorkRect.Left + 1 - Width;
      end;
    spRight:
      begin
        Top := (GetWorkRect.Bottom - Height) div 2;
        Left := GetWorkRect.Right - 1;
      end;
    spRightTop:
      begin
        Top := GetWorkRect.Top + 1 - Height;
        Left := GetWorkRect.Right - Width;
      end;
  else
    begin
      Top := GetWorkRect.Bottom - 1;
      Left := GetWorkRect.Right - Width;
    end;
  end;
  tmDelay.Interval := ShowDelay * 1000;
  DrawBk;
end;

procedure TMsgForm.FormDestroy(Sender: TObject);
begin
  Bmp.Free;
  with ThisList.LockList do
  try
    Delete(IndexOf(Self));
  finally
    ThisList.UnlockList;
  end;
end;

procedure TMsgForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmFadeIn.Enabled := False;
  tmDelay.Enabled := False;
  tmFadeOut.Enabled := False;
  Action := caFree; // �ر�ʱ�ͷ�
end;

procedure TMsgForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_BORDER;
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_TOPMOST;
end;

procedure TMsgForm.FormClick(Sender: TObject);
begin
  tmDelayTimer(Self);
end;

procedure TMsgForm.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Bitblt(Msg.DC, 0, 0, ClientWidth, ClientHeight, Bmp.Canvas.Handle, 0, 0,
    SRCCOPY);
  Msg.Result := 1; // �Ѵ���
end;

procedure TMsgForm.tmFadeInTimer(Sender: TObject);
begin
  Inc(InTimer);
  InValue := 1 + InTimer * 3 div 11;
  case MsgPos of
    spLeft:
      begin
        if Left >= GetWorkRect.Left then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Left - InValue >= GetWorkRect.Left then
          Left := GetWorkRect.Left
        else
          Left := Left + InValue;
      end;
    spRight:
      begin
        if Left <= GetWorkRect.Right - Width then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Left + InValue <= GetWorkRect.Right - Width then
          Left := GetWorkRect.Right - Width
        else
          Left := Left - InValue;
      end;
    spRightTop:
      begin
        if Top >= GetWorkRect.Top then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Top + InValue >= GetWorkRect.Top then
          Top := GetWorkRect.Top
        else
          Top := Top + InValue;
      end;
  else
    begin
      if Top + Height <= GetWorkRect.Bottom then
      begin
        tmFadeIn.Enabled := False;
        tmDelay.Enabled := True;
      end
      else if Top - InValue <= GetWorkRect.Bottom - Height then
        Top := GetWorkRect.Bottom - Height
      else
        Top := Top - InValue;
    end;
  end;
end;

procedure TMsgForm.tmDelayTimer(Sender: TObject);
begin
  tmFadeIn.Enabled := False;
  tmDelay.Enabled := False;
  tmFadeOut.Enabled := True;
end;

procedure TMsgForm.tmFadeOutTimer(Sender: TObject);
begin
  if InTimer > 0 then
    Dec(InTimer);
  InValue := 1 + InTimer * 3 div 11;
  case MsgPos of
    spLeft:
      begin
        Left := Left - InValue;
        if Left <= GetWorkRect.Left + 2 - Width then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
    spRight:
      begin
        Left := Left + InValue;
        if Left >= GetWorkRect.Right - 2 then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
    spRightTop:
      begin
        Top := Top - InValue;
        if Top <= GetWorkRect.Top + 1 - Height then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
  else
    begin
      Top := Top + InValue;
      if Top >= GetWorkRect.Bottom - 2 then
      begin
        tmFadeOut.Enabled := False;
        Close;
      end;
    end;
  end;
end;

procedure TMsgForm.SetMsg(const Value: string);
begin
  FMsg := Value;
  lblMsg.Caption := FMsg;
end;

procedure TMsgForm.SetMsgKind(const Value: TMsgKind);
begin
  FMsgKind := Value;
  case FMsgKind of
    mkError:
      begin
        imgError.Visible := True;
        lblIcon.Caption := SCnError;
        StartColor := ColorToRGB(ErrorStartColor);
        EndColor := ColorToRGB(ErrorEndColor);
      end;
    mkWarning:
      begin
        imgWarning.Visible := True;
        lblIcon.Caption := SCnWarning;
        StartColor := ColorToRGB(WarningStartColor);
        EndColor := ColorToRGB(WarningEndColor);
      end;
  else
    begin
      imgInfo.Visible := True;
      lblIcon.Caption := SCnInformation;
      StartColor := ColorToRGB(InfoStartColor);
      EndColor := ColorToRGB(InfoEndColor);
    end;
  end;
end;

procedure TMsgForm.SetMsgPos(const Value: TShowPos);
begin
  FMsgPos := Value;
end;

procedure TMsgForm.CreateTimers;
begin
  tmFadeIn := TTimer.Create(Self);
  with tmFadeIn do
  begin
    Enabled := False;
    Interval := 15;
    OnTimer := tmFadeInTimer;
  end;
  tmFadeOut := TTimer.Create(Self);
  with tmFadeOut do
  begin
    Enabled := False;
    Interval := 15;
    OnTimer := tmFadeOutTimer;
  end;
  tmDelay := TTimer.Create(Self);
  with tmDelay do
  begin
    Enabled := False;
    Interval := 5000;
    OnTimer := tmDelayTimer;
  end;
end;

function TMsgForm.CalcRect(MaxWidth: Integer; const ACap: string;
  AData: Pointer): TRect;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(lblMsg.Canvas.Handle, PChar(ACap), -1, Result, DT_CALCRECT or DT_LEFT
    or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
end;

procedure TMsgForm.DrawBk;
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[Byte] of TRGBTriple;
var
  PLine: PRGBArray;
  x, y: Integer;
  ARect: TRect;
  RowInc: Integer;
  sr, sg, sb, er, eg, eb: Integer;
begin
  if InDraw then Exit;
  InDraw := True;
  Bmp.Width := ClientWidth;
  Bmp.Height := ClientHeight;
  sr := GetRValue(StartColor);
  sg := GetGValue(StartColor);
  sb := GetBValue(StartColor);
  er := GetRValue(EndColor);
  eg := GetGValue(EndColor);
  eb := GetBValue(EndColor);
  PLine := PRGBArray(Bmp.ScanLine[0]);
  for x := 0 to Bmp.Width - 1 do
  begin
    PLine[x].rgbtRed := sr + (er - sr) * x div Bmp.Width;
    PLine[x].rgbtGreen := sg + (eg - sg) * x div Bmp.Width;
    PLine[x].rgbtBlue := sb + (eb - sb) * x div Bmp.Width;
  end;
  RowInc := (Bmp.Width * 3 + 3) div 4 * 4;
  for y := 1 to Bmp.Height - 1 do
    Move(PLine^, Bmp.ScanLine[y]^, RowInc);
  ARect := Rect(0, 0, Width, Height);
  Frame3D(Bmp.Canvas, ARect, $777777, $777777, 1);
  InDraw := False;
  Refresh;
end;

initialization
  ThisList := TThreadList.Create;

finalization
  CleanUp;
  FreeAndNil(ThisList);

end.

