unit uFrmModem;

{
  �������ƣ�TCnModem��ʾ����
  �����ܣ�����TCnMode����ʹ��
  ������
     1��TCnModem�е�Dial�����ڲ�����˳�ʼ�����ڡ�Modem�ȹ�����

     2���û�ʹ��TCnModem��InitAtCommand����ֻ�������ȶ���һ��ATָ�
       ����û����������ATָ���Ҫ�޸�TCnModem��Դ���롣
       InitModem���������ǰ�����AT&Fָ��
       if not SendATOk('AT&F') then exit;
       
     3�����ʣ��ѽ������CnModem��bug��Ӧ���ٳ���1000����
     // �л�����������״̬
     procedure TCnModem.Escape; �е������������������ͷ�ԡ�
     Tick := Round(FWaitEscapeTime * 0.02 * 1.3);
     Sleep(Tick);
     ��ʼ��ʱ���Ѿ����� S12Ϊ FWaitEscapeTime��
     �����Sleep(Tick);�ǲ����е�̵ĳ��档����Tick��ֵ�о�������ô���ӵ����㡣

  ��ʾ�������ߣ�
  Written By SkyJacker
  Email:Hemiaoyu@gmail.com
  2006-12-11
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnClasses, CnRS232, CnModem, Spin, ExtCtrls;

type
  TFrmModem = class(TForm)
    cm1: TCnModem;
    grp1: TGroupBox;
    pnlLeft: TPanel;
    grp2: TGroupBox;
    edtPhone: TEdit;
    se1: TSpinEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    grp3: TGroupBox;
    btnDial: TButton;
    mmoLog: TMemo;
    lbl3: TLabel;
    edtSendData: TEdit;
    btnSend: TButton;
    btnHangUp: TButton;
    procedure btnDialClick(Sender: TObject);
    procedure cm1ReceiveData(Sender: TObject; Buffer: Pointer;
      BufferLength: Word);
    procedure btnSendClick(Sender: TObject);
    procedure btnHangUpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(const Ainfo: string);

    //��ʼ������
    function  InitCom(ComNum: Word): Boolean;
    //��ʼ��Modem
    function InitModem(): Boolean;
    //����
    function Dial(const PhoneNo: string):Boolean;
    //�һ�
    function HangUp(): Boolean;
    //��������
    function SendData(const StrData: string):Boolean;
  end;

var
  FrmModem: TFrmModem;

implementation

{$R *.dfm}
  
procedure TFrmModem.Log(const Ainfo: string);
begin
  mmoLog.Lines.Add(FormatDateTime('YYYY-MM-DD HH:MM:SS',now)+ ' ' + Ainfo);
end;

function TFrmModem.InitCom(ComNum: Word): Boolean;
begin
  Result := true;
  cm1.CommName := 'COM' + Char($30+ComNum);
  Log(cm1.CommName);
end;

function TFrmModem.InitModem(): Boolean;
begin
  Result := true;
end;

function TFrmModem.HangUp(): Boolean;
begin
  Result := True;
  cm1.Hangup;
end;

function TFrmModem.Dial(const PhoneNo: string):Boolean;
var
  msgtxt: string;
begin
  Result := false;
  case cm1.Dial(PhoneNo) of
     drConnect:
       begin
         msgtxt := '���ӳɹ�';
         Result := true;
       end;
     drOpenCommFail:    msgtxt := '�򿪴���ʧ��';
     drNoModem:         msgtxt := 'û�м�⵽Modem';
     drNoDialtone:      msgtxt := '�޲�����';
     drBusy:            msgtxt := '��⵽æ�ź�';
     drNoAnswer:        msgtxt := '��Ӧ���ź�';
     drNoCarrier:       msgtxt := 'û�м�⵽�ز��ź�';
     drTimeout:         msgtxt := '��ʱ����';
     drUnknow:          msgtxt := 'δ֪����';
  end;
  Log(msgtxt);
end;


function TFrmModem.SendData(const StrData: string):Boolean;
begin
  result := cm1.WriteCommData(PChar(StrData),Length(StrData));
end;

procedure TFrmModem.btnDialClick(Sender: TObject);
var
  phone:string;
  iCom: Word;
begin
  iCom := se1.Value;
  InitCom(iCom);

  //�շ�����
  phone := Trim(edtPhone.Text);
  if phone<>'' then
  begin
    if Dial(phone) then
    begin
      Log('��ʼ�շ�����');
    end
    else
    begin
      Log('����ʧ��');
      HangUp;
    end;
  end;
end;

procedure TFrmModem.btnHangUpClick(Sender: TObject);
begin
  HangUp;
end;

procedure TFrmModem.cm1ReceiveData(Sender: TObject; Buffer: Pointer;
  BufferLength: Word);
var
   I: Integer;
  S: string;
  Input: array of byte;
begin
  Input := Buffer;
  for I:=0 to BufferLength-1 do
  begin
    S := S + Chr(Input[I]);
  end;
  Log('�յ���'+S);
end;

procedure TFrmModem.btnSendClick(Sender: TObject);
var
  sData: string;
begin
  sData := edtSendData.Text;
  if SendData(sData) then
    Log('���ͳɹ�: ' + sData)
  else
    Log('����ʧ��' + sData);
end;

end.
