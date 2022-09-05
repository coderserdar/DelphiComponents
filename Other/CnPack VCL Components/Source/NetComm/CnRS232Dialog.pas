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

unit CnRS232Dialog;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�CnRS232Dialog �������öԻ�����������嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�����е��ַ������ϱ��ػ�����ʽ
*           �õ�Ԫ�����е��ַ����������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.04.18 V1.1
*                ������CommConfig��TimeoutsΪ��������
*           2002.04.08 V1.0
*                ������Ԫ
*                ����ע��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, CnClasses, CnConsts, CnRS232, CnNetConsts,
  CnSpin;

type

//------------------------------------------------------------------------------
// �������öԻ�����
//------------------------------------------------------------------------------

{ TCnRS232Dlg }

  TCnRS232Dlg = class(TForm)
    pcCommConfig: TPageControl;
    tsNormal: TTabSheet;
    tsXonXoff: TTabSheet;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    cbbBaudRate: TComboBox;
    lblBaudRate: TLabel;
    cbTxContinueOnXoff: TCheckBox;
    cbOutx_XonXoffFlow: TCheckBox;
    cbInx_XonXoffFlow: TCheckBox;
    lblByteSize: TLabel;
    cbbByteSize: TComboBox;
    lblParity: TLabel;
    cbbParity: TComboBox;
    lblStopBits: TLabel;
    cbbStopBits: TComboBox;
    lblXonLimit: TLabel;
    lblXoffLimit: TLabel;
    lblXonChar: TLabel;
    lblXoffChar: TLabel;
    tsHardware: TTabSheet;
    lblDtrControl: TLabel;
    lblRtsControl: TLabel;
    cbOutx_CtsFlow: TCheckBox;
    cbOutx_DsrFlow: TCheckBox;
    cbDsrSensitivity: TCheckBox;
    cbbDtrControl: TComboBox;
    cbbRtsControl: TComboBox;
    cbReplaceWhenParityError: TCheckBox;
    cbIgnoreNullChar: TCheckBox;
    lblInCtrl: TLabel;
    lblOutCtrl: TLabel;
    tsTimeouts: TTabSheet;
    lblReadIntervalTimeout: TLabel;
    lblReadTotalTimeoutMultiplier: TLabel;
    lblMSec1: TLabel;
    lblMSec2: TLabel;
    lblReadTotalTimeoutConstant: TLabel;
    lblMSec3: TLabel;
    lblWriteTotalTimeoutMultiplier: TLabel;
    lblMSec4: TLabel;
    lblWriteTotalTimeoutConstant: TLabel;
    lblMSec5: TLabel;
    cbShowHint: TCheckBox;
    seReplacedChar: TCnSpinEdit;
    seXonLimit: TCnSpinEdit;
    seXonChar: TCnSpinEdit;
    seXoffChar: TCnSpinEdit;
    seXoffLimit: TCnSpinEdit;
    seReadIntervalTimeout: TCnSpinEdit;
    seReadTotalTimeoutMultiplier: TCnSpinEdit;
    seReadTotalTimeoutConstant: TCnSpinEdit;
    seWriteTotalTimeoutMultiplier: TCnSpinEdit;
    seWriteTotalTimeoutConstant: TCnSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbbBaudRateExit(Sender: TObject);
    procedure bbtnOkClick(Sender: TObject);
    procedure seReplacedCharExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seXonLimitExit(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure cbShowHintClick(Sender: TObject);
  private
    { Private declarations }
    FCommConfig: TCnRS232Config;
    FTimeouts: TCnRS232Timeouts;
    procedure SetCommConfig(const Value: TCnRS232Config);
    procedure SetCommTimeouts(const Value: TCnRS232Timeouts);
    procedure ReadCommConfig;
    procedure WriteCommConfig;
    procedure ReadCommTimeouts;
    procedure WriteCommTimeouts;
  public
    { Public declarations }
    property CommConfig: TCnRS232Config read FCommConfig write SetCommConfig;
    property CommTimeouts: TCnRS232Timeouts read FTimeouts write SetCommTimeouts;
  end;

//------------------------------------------------------------------------------
// �������öԻ������
//------------------------------------------------------------------------------

{ TCnRS232Dialog }

  TCnRS232DialogKind = (ckWin32, ckExtended);
  {* �������öԻ�����
   |<PRE>
     ckWin32:           - Win32��׼���
     ckExtended:        - ��չ�Ի�����
   |</PRE>}
  TCnRS232DialogPages = set of (cpNormal, cpXonXoff, cpHardware, cpTimeouts);
  {* �������öԻ�����ʾҳ�漯��
   |<PRE>
     cpNormal:          - ��������ҳ��
     cpXonXoff:         - �����������ҳ��
     cpHardware:        - Ӳ����������ҳ��
     cpTimeouts:        - ��ʱ����ҳ��
   |</PRE>}
  TCnRS232DialogShowHint = (csHint, csNoHint, csCheckHint, csCheckNoHint);
  {* �������öԻ��򹤾���ʾ��Ϣ��ʾ��ʽ
   |<PRE>
     csHint:            - ��ʾ������ʾ
     csNoHint:          - ����ʾ������ʾ
     csCheckHint:       - �ɵ�ѡ�������Ĭ��Ϊ��ʾ
     csCheckNoHint:     - �ɵ�ѡ�������Ĭ��Ϊ����ʾ
   |</PRE>}

  TCnRS232Dialog = class(TCnComponent)
  {* RS232�������öԻ��������
   |<PRE>
     * ���������ʾ�������öԻ���һ�����TCnRS232���ʹ�á�
     * ʹ�÷�ʽ������VCL�еĳ���Ի��������
   |</PRE>}
  private
    FCommConfig: TCnRS232Config;
    FTimeouts: TCnRS232Timeouts;
    FKind: TCnRS232DialogKind;
    FPages: TCnRS232DialogPages;
    FCommName: string;
    FTitle: string;
    FBaudRateList: Boolean;
    FShowHint: TCnRS232DialogShowHint;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetCommConfig(const Value: TCnRS232Config);
    procedure SetTimeouts(const Value: TCnRS232Timeouts);
    function GetHandle: THandle;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoShow; virtual;
    procedure DoClose; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ�����������TCnRS232�и�ֵ}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    {* ��ʾ�Ի�������û�����ˡ�ȷ�ϡ���ť������Ϊ��}
    property CommName: string read FCommName write FCommName;
    {* ���ڶ˿���}
  published
    property Title: string read FTitle write FTitle;
    {* �Ի�����⣬���� Kind ����Ϊ ckExtended ��չ���ʱ��Ч}
    property Kind: TCnRS232DialogKind read FKind write FKind default ckExtended;
    {* �Ի�����}
    property Pages: TCnRS232DialogPages read FPages write FPages default
      [cpNormal, cpXonXoff, cpHardware];
    {* �Ի������ʾ��ҳ�漯�ϣ����� Kind ����Ϊ ckExtended ��չ���ʱ��Ч}
    property BaudRateList: Boolean read FBaudRateList write FBaudRateList default True;
    {* �Ի����еĲ����ʲ����Ƿ�ֻ����������б���ѡ�����Ϊ�٣��û����Զ���
       �Ǳ�׼��С���ʡ����� Kind ����Ϊ ckExtended ��չ���ʱ��Ч}
    property ShowHint: TCnRS232DialogShowHint read FShowHint write FShowHint default
      csNoHint;
    {* ��ʾ�Ի����й�����ʾ�ķ�ʽ������ Kind ����Ϊ ckExtended ��չ���ʱ��Ч}
    property CommConfig: TCnRS232Config read FCommConfig write SetCommConfig;
    {* ����ͨѶ����}
    property Timeouts: TCnRS232Timeouts read FTimeouts write SetTimeouts;
    {* ����ͨѶ��ʱ����}
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    {* �Ի���ر��¼�}
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    {* �Ի�����ʾ�¼�}
  end;

implementation

{$R *.DFM}

//------------------------------------------------------------------------------
// �������öԻ�����
//------------------------------------------------------------------------------

{ TCnRS232Dlg }

// ���崴��
procedure TCnRS232Dlg.FormCreate(Sender: TObject);
begin
  FCommConfig := TCnRS232Config.Create;
  FTimeouts := TCnRS232Timeouts.Create;
  WriteCommConfig;
  WriteCommTimeouts;
end;

// �����ͷ�
procedure TCnRS232Dlg.FormDestroy(Sender: TObject);
begin
  FCommConfig.Free;
  FTimeouts.Free;
end;

// ������ʾ
procedure TCnRS232Dlg.FormShow(Sender: TObject);
begin
  WriteCommConfig;
  WriteCommTimeouts;
  ControlChanged(Self);
end;

// ȷ��
procedure TCnRS232Dlg.bbtnOkClick(Sender: TObject);
begin
  ReadCommConfig;
  ReadCommTimeouts;
  ModalResult := mrOK;
end;

// �ӿؼ���ȡ��������
procedure TCnRS232Dlg.ReadCommConfig;
begin
  with FCommConfig do
  begin
    XoffChar := Char(seXoffChar.Value);
    ReplacedChar := Char(seReplacedChar.Value);
    XonChar := Char(seXonChar.Value);
    Outx_CtsFlow := cbOutx_CtsFlow.Checked;
    Outx_DsrFlow := cbOutx_DsrFlow.Checked;
    ParityCheck := cbbParity.ItemIndex <> 0;
    IgnoreNullChar := cbIgnoreNullChar.Checked;
    Inx_XonXoffFlow := cbInx_XonXoffFlow.Checked;
    TxContinueOnXoff := cbTxContinueOnXoff.Checked;
    ReplaceWhenParityError := cbReplaceWhenParityError.Checked;
    Outx_XonXoffFlow := cbOutx_XonXoffFlow.Checked;
    DsrSensitivity := cbDsrSensitivity.Checked;
    BaudRate := StrToInt(cbbBaudRate.Text);
    ByteSize := TByteSize(cbbByteSize.ItemIndex);
    DtrControl := TDtrControl(cbbDtrControl.ItemIndex);
    Parity := TParity(cbbParity.ItemIndex);
    RtsControl := TRtsControl(cbbRtsControl.ItemIndex);
    StopBits := TStopBits(cbbStopBits.ItemIndex);
    XoffLimit := seXoffLimit.Value;
    XonLimit := seXonLimit.Value;
  end;
end;

// �ӿؼ���ȡ��ʱ����
procedure TCnRS232Dlg.ReadCommTimeouts;
begin
  with FTimeouts do
  begin
    ReadTotalTimeoutConstant := seReadTotalTimeoutConstant.Value;
    ReadIntervalTimeout := seReadIntervalTimeout.Value;
    ReadTotalTimeoutMultiplier := seReadTotalTimeoutMultiplier.Value;
    WriteTotalTimeoutConstant := seWriteTotalTimeoutConstant.Value;
    WriteTotalTimeoutMultiplier := seWriteTotalTimeoutMultiplier.Value;
  end;
end;

// ���ݲ������ÿؼ�
procedure TCnRS232Dlg.WriteCommConfig;
begin
  with FCommConfig do
  begin
    seXoffChar.Value := Byte(XoffChar);
    seReplacedChar.Value := Byte(ReplacedChar);
    seXonChar.Value := Byte(XonChar);
    cbOutx_CtsFlow.Checked := Outx_CtsFlow;
    cbOutx_DsrFlow.Checked := Outx_DsrFlow;
    cbIgnoreNullChar.Checked := IgnoreNullChar;
    cbInx_XonXoffFlow.Checked := Inx_XonXoffFlow;
    cbTxContinueOnXoff.Checked := TxContinueOnXoff;
    cbReplaceWhenParityError.Checked := ReplaceWhenParityError;
    cbOutx_XonXoffFlow.Checked := Outx_XonXoffFlow;
    cbDsrSensitivity.Checked := DsrSensitivity;
    if cbbBaudRate.Style = csDropDown then
      cbbBaudRate.Text := IntToStr(BaudRate)
    else
    begin
      cbbBaudRate.ItemIndex := cbbBaudRate.Items.IndexOf(IntToStr(BaudRate));
      if cbbBaudRate.ItemIndex < 0 then
        cbbBaudRate.ItemIndex := cbbBaudRate.Items.Add(IntToStr(BaudRate));
    end;
    cbbByteSize.ItemIndex := Ord(ByteSize);
    cbbDtrControl.ItemIndex := Ord(DtrControl);
    cbbParity.ItemIndex := Ord(Parity);
    cbbRtsControl.ItemIndex := Ord(RtsControl);
    cbbStopBits.ItemIndex := Ord(StopBits);
    seXoffLimit.Value := XoffLimit;
    seXonLimit.Value := XonLimit;
  end;
end;

// ���ݳ�ʱ�������ÿؼ�
procedure TCnRS232Dlg.WriteCommTimeouts;
begin
  with FTimeouts do
  begin
    seReadTotalTimeoutConstant.Value := ReadTotalTimeoutConstant;
    seReadIntervalTimeout.Value := ReadIntervalTimeout;
    seReadTotalTimeoutMultiplier.Value := ReadTotalTimeoutMultiplier;
    seWriteTotalTimeoutConstant.Value := WriteTotalTimeoutConstant;
    seWriteTotalTimeoutMultiplier.Value := WriteTotalTimeoutMultiplier;
  end;
end;

// ���ò���
procedure TCnRS232Dlg.SetCommConfig(const Value: TCnRS232Config);
begin
  FCommConfig.Assign(Value);
  WriteCommConfig;
end;

// ���ó�ʱ
procedure TCnRS232Dlg.SetCommTimeouts(const Value: TCnRS232Timeouts);
begin
  FTimeouts.Assign(Value);
  WriteCommTimeouts;
end;

// Լ��������
procedure TCnRS232Dlg.cbbBaudRateExit(Sender: TObject);
begin
  try
    StrToInt(cbbBaudRate.Text);
  except
    MessageBox(Handle, PChar(SBaudRateError), PChar(SCnError), MB_OK + MB_ICONSTOP);
    cbbBaudRate.SetFocus;
  end;
end;

// Լ���ַ��༭�ؼ�
procedure TCnRS232Dlg.seReplacedCharExit(Sender: TObject);
var
  i: Integer;
begin
  if Sender is TCnSpinEdit then
  try
    i := StrToInt(TCnSpinEdit(Sender).Text);
    if (i > 255) or (i < 0) then
      raise Exception.Create(SCnError);
    if seXonChar.Text = seXoffChar.Text then
    begin
      MessageBox(Handle, PChar(SInvalidXonXoffChar), PChar(SCnError),
        MB_OK + MB_ICONSTOP);
      TCnSpinEdit(Sender).SetFocus;
    end;
  except
    MessageBox(Handle, PChar(SInputASCIICode), PChar(SCnError), MB_OK + MB_ICONSTOP);
    TCnSpinEdit(Sender).SetFocus;
  end;
end;

// Լ�������༭�ؼ�
procedure TCnRS232Dlg.seXonLimitExit(Sender: TObject);
var
  i: Integer;
begin
  if Sender is TCnSpinEdit then
  try
    i := StrToInt(TCnSpinEdit(Sender).Text);
    if (i > MaxWord) or (i < 0) then
      raise Exception.Create(SCnError);
  except
    MessageBox(Handle, PChar(SInputInteger), PChar(SCnError), MB_OK + MB_ICONSTOP);
    TCnSpinEdit(Sender).SetFocus;
  end;
end;

// ���ÿؼ�״̬
procedure TCnRS232Dlg.ControlChanged(Sender: TObject);
begin
  cbReplaceWhenParityError.Enabled := cbbParity.ItemIndex > 0;
  seReplacedChar.Enabled := cbReplaceWhenParityError.Enabled and
    cbReplaceWhenParityError.Checked;
end;

// ���ù�����ʾ
procedure TCnRS232Dlg.cbShowHintClick(Sender: TObject);
begin
  ShowHint := cbShowHint.Checked;
end;

//------------------------------------------------------------------------------
// �������öԻ������
//------------------------------------------------------------------------------

{ TCnRS232Dialog }

// ����ֵ����
procedure TCnRS232Dialog.Assign(Source: TPersistent);
begin
  if Source is TCnRS232 then
  begin
    FCommConfig.Assign(TCnRS232(Source).CommConfig);
    FTimeouts.Assign(TCnRS232(Source).Timeouts);
    FCommName := TCnRS232(Source).CommName;
  end
  else if Source is TCnRS232Dialog then
  begin
    TCnRS232Dialog(Source).AssignTo(Self);
  end
  else
    inherited;
end;

// Ŀ�����ֵ����
procedure TCnRS232Dialog.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnRS232 then
  begin
    TCnRS232(Dest).CommConfig := FCommConfig;
    TCnRS232(Dest).Timeouts := FTimeouts;
  end
  else if Dest is TCnRS232Dialog then
  begin
    TCnRS232Dialog(Dest).FCommConfig.Assign(FCommConfig);
    TCnRS232Dialog(Dest).FTimeouts.Assign(FTimeouts);
    TCnRS232Dialog(Dest).FCommName := FCommName;
  end
  else
    inherited;
end;

// ��ʼ��
constructor TCnRS232Dialog.Create(AOwner: TComponent);
begin
  inherited;
  FCommConfig := TCnRS232Config.Create;
  FTimeouts := TCnRS232Timeouts.Create;
  FKind := ckExtended;
  FPages := [cpNormal, cpXonXoff, cpHardware];
  FBaudRateList := True;
  FShowHint := csNoHint;
end;

// �ͷ�
destructor TCnRS232Dialog.Destroy;
begin
  FCommConfig.Free;
  FTimeouts.Free;
  inherited;
end;

// �Ի���ر�
procedure TCnRS232Dialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

// �Ի�����ʾ
procedure TCnRS232Dialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

// ��ʾ�Ի���
function TCnRS232Dialog.Execute: Boolean;
var
  CnRS232Dlg: TCnRS232Dlg;
  lpCC: TCommConfig;
begin
  if FKind = ckExtended then  // ��չ���
  begin
    CnRS232Dlg := TCnRS232Dlg.Create(Owner);
    try
      CnRS232Dlg.FCommConfig.Assign(FCommConfig);
      CnRS232Dlg.FTimeouts.Assign(FTimeouts);
      if FTitle <> '' then
        CnRS232Dlg.Caption := FTitle
      else if FCommName <> '' then
        CnRS232Dlg.Caption := Format('%s (%s)', [CnRS232Dlg.Caption, FCommName]);
      if FBaudRateList then
        CnRS232Dlg.cbbBaudRate.Style := csDropDownList
      else
        CnRS232Dlg.cbbBaudRate.Style := csDropDown;
      CnRS232Dlg.cbShowHint.Visible := FShowHint in [csCheckHint, csCheckNoHint];
      CnRS232Dlg.cbShowHint.Checked := FShowHint in [csHint, csCheckHint];
      CnRS232Dlg.ShowHint := CnRS232Dlg.cbShowHint.Checked;
      CnRS232Dlg.tsNormal.TabVisible := cpNormal in FPages;
      CnRS232Dlg.tsXonXoff.TabVisible := cpXonXoff in FPages;
      CnRS232Dlg.tsHardware.TabVisible := cpHardware in FPages;
      CnRS232Dlg.tsTimeouts.TabVisible := cpTimeouts in FPages;
      if FPages = [] then
        CnRS232Dlg.tsNormal.TabVisible := True;
      DoShow;
      Result := CnRS232Dlg.ShowModal = mrOK;
      if Result then
      begin
        FCommConfig.Assign(CnRS232Dlg.FCommConfig);
        FTimeouts.Assign(CnRS232Dlg.FTimeouts);
      end;
      DoClose;
    finally
      CnRS232Dlg.Free;
    end;
  end
  else
  begin
    FillChar(lpCC, SizeOf(lpCC), 0);
    lpCC.dwSize := SizeOf(lpCC);
    FCommConfig.GetDCB(lpCC.DCB);
    DoShow;
    Result := CommConfigDialog(PChar(FCommName), GetHandle, lpCC);
    if Result then
      FCommConfig.SetDCB(lpCC.DCB);
    DoClose;
  end;
end;

procedure TCnRS232Dialog.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnRS232DialogName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnRS232DialogComment;
end;

// ȡ��������
function TCnRS232Dialog.GetHandle: THandle;
begin
  if Owner is TForm then
    Result := TForm(Owner).Handle
  else
    Result := 0;
end;

// ���ò���
procedure TCnRS232Dialog.SetCommConfig(const Value: TCnRS232Config);
begin
  FCommConfig.Assign(Value);
end;

// ���ó�ʱ
procedure TCnRS232Dialog.SetTimeouts(const Value: TCnRS232Timeouts);
begin
  FTimeouts.Assign(Value);
end;

end.

