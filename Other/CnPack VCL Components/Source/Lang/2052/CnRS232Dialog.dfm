object CnRS232Dlg: TCnRS232Dlg
  Left = 238
  Top = 123
  BorderStyle = bsDialog
  Caption = '��������'
  ClientHeight = 316
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '����'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object pcCommConfig: TPageControl
    Left = 8
    Top = 8
    Width = 297
    Height = 273
    ActivePage = tsNormal
    TabOrder = 0
    object tsNormal: TTabSheet
      Caption = '��������'
      object lblBaudRate: TLabel
        Left = 16
        Top = 12
        Width = 60
        Height = 12
        Caption = '������(&B):'
      end
      object lblByteSize: TLabel
        Left = 16
        Top = 41
        Width = 60
        Height = 12
        Caption = '����λ(&D):'
      end
      object lblParity: TLabel
        Left = 16
        Top = 70
        Width = 72
        Height = 12
        Caption = '��żУ��(&P):'
      end
      object lblStopBits: TLabel
        Left = 16
        Top = 98
        Width = 60
        Height = 12
        Caption = 'ֹͣλ(&S):'
      end
      object cbbBaudRate: TComboBox
        Left = 120
        Top = 8
        Width = 145
        Height = 20
        Hint = '����ͨѶ������ٶ�'#13#10'��λ��bps'
        ItemHeight = 12
        TabOrder = 0
        OnExit = cbbBaudRateExit
        Items.Strings = (
          '110'
          '300'
          '600'
          '1200'
          '2400'
          '4800'
          '9600'
          '14400'
          '19200'
          '38400'
          '56000'
          '57600'
          '115200'
          '128000'
          '256000')
      end
      object cbbByteSize: TComboBox
        Left = 120
        Top = 37
        Width = 145
        Height = 20
        Hint = '��������λ��'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '5'
          '6'
          '7'
          '8')
      end
      object cbbParity: TComboBox
        Left = 120
        Top = 66
        Width = 145
        Height = 20
        Hint = '��żУ�鷽ʽ'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 2
        OnChange = ControlChanged
        Items.Strings = (
          '��'
          '��У��'
          'żУ��'
          '����У��'
          '�պ�У�� ')
      end
      object cbbStopBits: TComboBox
        Left = 120
        Top = 94
        Width = 145
        Height = 20
        Hint = 'ֹͣλ��'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 3
        Items.Strings = (
          '1'
          '1.5'
          '2 ')
      end
      object cbReplaceWhenParityError: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = '������żУ���ʱ��ָ���ַ�����'
        Caption = '��żУ������滻���ַ�(&ASCII):'
        TabOrder = 4
        OnClick = ControlChanged
      end
      object cbIgnoreNullChar: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = '�Ƿ������յ���NULL(ASCII 0)�ַ�'
        Caption = '����NULL�ַ�(&N)'
        TabOrder = 6
      end
      object seReplacedChar: TCnSpinEdit
        Left = 216
        Top = 126
        Width = 49
        Height = 21
        Hint = '�滻�ַ���ASCII��'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
    end
    object tsXonXoff: TTabSheet
      Caption = '�����������'
      ImageIndex = 1
      object lblXonLimit: TLabel
        Left = 16
        Top = 87
        Width = 66
        Height = 12
        Caption = 'Xon��ֵ(&B):'
      end
      object lblXoffLimit: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 12
        Caption = 'Xoff��ֵ(&H):'
      end
      object lblXonChar: TLabel
        Left = 16
        Top = 146
        Width = 108
        Height = 12
        Caption = 'Xon�ַ�(ASCII)(&K):'
      end
      object lblXoffChar: TLabel
        Left = 16
        Top = 176
        Width = 114
        Height = 12
        Caption = 'Xoff�ַ�(ASCII)(&I):'
      end
      object cbTxContinueOnXoff: TCheckBox
        Left = 16
        Top = 56
        Width = 200
        Height = 17
        Hint = 
          '�����ջ������������ѷ��͡�Xoff�ַ��������Ƿ�ֹͣ��'#13#10'���ѡ��' +
          '���������Ľ��ջ������е��ֽ���δ�ﵽ��Xoff��ֵ��'#13#10'������������' +
          '���ˡ�Xoff�ַ�����ֹͣ�����ֽ�ʱ���������ͣ�'#13#10'�����ѡ�������ſ�' +
          '�Ļ������е��ֽ������㡰Xon��ֵ������'#13#10'�ڣ��������������ˡ�Xon' +
          '�ַ�����ָ�����ʱ���������͡�'
        Caption = 'Xoff�������������(&C)'
        TabOrder = 2
      end
      object cbOutx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 32
        Width = 200
        Height = 17
        Hint = 
          '���ݷ���ʱ�Ƿ�ʹ��Xon/Xoff��Ϣ������'#13#10'���ѡ�񣬵����յ���Xoff��' +
          '����ʱ��ͣ���ͣ�����'#13#10'���յ���Xon�ַ���ʱ�ָ����͡�'
        Caption = '���Xon/Xoff��Ч(&O)'
        TabOrder = 1
      end
      object cbInx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 8
        Width = 200
        Height = 17
        Hint = 
          '���ݽ���ʱ�Ƿ�ʹ��Xon/Xoff��Ϣ������'#13#10'���ѡ�񣬵����ջ���������' +
          '��ֻʣ��Xoff��ֵ����'#13#10'�ַ�����ʱ���͡�Xoff�ַ����������ջ�������' +
          'ֻ��'#13#10'��Xon��ֵ�����ַ�ʱ�����͡�Xon�ַ�����'
        Caption = '����Xon/Xoff��Ч(&F)'
        TabOrder = 0
      end
      object seXonLimit: TCnSpinEdit
        Left = 136
        Top = 83
        Width = 65
        Height = 21
        Hint = 'ָ���ڷ��͡�Xon�ַ���֮ǰ�����ջ�����������������ַ�����'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnExit = seXonLimitExit
      end
      object seXonChar: TCnSpinEdit
        Left = 136
        Top = 144
        Width = 65
        Height = 21
        Hint = '���ͺͽ��յġ�Xon�ַ�����ASCII�룬��ʾ����������䡣'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
      object seXoffChar: TCnSpinEdit
        Left = 136
        Top = 174
        Width = 65
        Height = 21
        Hint = '���ͺͽ��յġ�Xoff�ַ�����ASCII�룬��ʾ������ͣ���䡣'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnExit = seReplacedCharExit
      end
      object seXoffLimit: TCnSpinEdit
        Left = 136
        Top = 113
        Width = 65
        Height = 21
        Hint = 
          'ָ���ڷ��͡�Xoff�ַ���֮ǰ�����ջ����������������ַ�����'#13#10'����' +
          '�������ĳ��ȼ�ȥ��ֵ�������������ַ�����'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = seXonLimitExit
      end
    end
    object tsHardware: TTabSheet
      Caption = 'Ӳ����������'
      ImageIndex = 2
      object lblDtrControl: TLabel
        Left = 16
        Top = 39
        Width = 90
        Height = 12
        Caption = 'DTR��������(&T):'
      end
      object lblRtsControl: TLabel
        Left = 16
        Top = 68
        Width = 90
        Height = 12
        Caption = 'RTS��������(&R):'
      end
      object lblInCtrl: TLabel
        Left = 16
        Top = 16
        Width = 54
        Height = 12
        Caption = '�������:'
      end
      object lblOutCtrl: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 12
        Caption = '�������:'
      end
      object cbOutx_CtsFlow: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = 'CTS(�������)'
        Caption = 'ʹ��CTS�źŽ��������������(&C)'
        TabOrder = 3
      end
      object cbOutx_DsrFlow: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = 'DSR(�����豸����)'
        Caption = 'ʹ��DSR�źŽ��������������(&B)'
        TabOrder = 2
      end
      object cbDsrSensitivity: TCheckBox
        Left = 16
        Top = 176
        Width = 200
        Height = 17
        Hint = 
          'ָ��ͨ�����������DSR�źŵ�״̬�Ƿ����С�'#13#10'���ѡ�񣬵�Modem��DS' +
          'R������Ϊ��ʱ����������'#13#10'���Խ��յ����κ��ֽڡ�'
        Caption = 'DSR���ж�(&E)'
        TabOrder = 4
      end
      object cbbDtrControl: TComboBox
        Left = 120
        Top = 35
        Width = 145
        Height = 20
        Hint = 'ʹ��DTR(�����ն˾���)�źŽ����������Ƶķ�ʽ'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 0
        Items.Strings = (
          '����DTR�߲�����'
          '��ֹDTR�߲�����'
          '����DTR����')
      end
      object cbbRtsControl: TComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 20
        Hint = 'ʹ��RTS(������)�źŽ����������Ƶķ�ʽ'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '����RTS������'
          '��ֹRTS������'
          '����RTS����'
          'ʹ�ô�����ʽ')
      end
    end
    object tsTimeouts: TTabSheet
      Caption = '��ʱ����'
      ImageIndex = 3
      object lblReadIntervalTimeout: TLabel
        Left = 16
        Top = 15
        Width = 84
        Height = 12
        Caption = '�������ʱ(&R):'
      end
      object lblReadTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 45
        Width = 96
        Height = 12
        Caption = '���ܳ�ʱϵ��(&T):'
      end
      object lblMSec1: TLabel
        Left = 240
        Top = 16
        Width = 24
        Height = 12
        Caption = '����'
      end
      object lblMSec2: TLabel
        Left = 240
        Top = 46
        Width = 24
        Height = 12
        Caption = '����'
      end
      object lblReadTotalTimeoutConstant: TLabel
        Left = 16
        Top = 75
        Width = 96
        Height = 12
        Caption = '���ܳ�ʱ����(&A):'
      end
      object lblMSec3: TLabel
        Left = 240
        Top = 76
        Width = 24
        Height = 12
        Caption = '����'
      end
      object lblWriteTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 105
        Width = 96
        Height = 12
        Caption = 'д�ܳ�ʱϵ��(&W):'
      end
      object lblMSec4: TLabel
        Left = 240
        Top = 106
        Width = 24
        Height = 12
        Caption = '����'
      end
      object lblWriteTotalTimeoutConstant: TLabel
        Left = 16
        Top = 135
        Width = 96
        Height = 12
        Caption = 'д�ܳ�ʱ����(&B):'
      end
      object lblMSec5: TLabel
        Left = 240
        Top = 136
        Width = 24
        Height = 12
        Caption = '����'
      end
      object seReadIntervalTimeout: TCnSpinEdit
        Left = 136
        Top = 11
        Width = 97
        Height = 21
        Hint = 
          'ָ��ͨ����·�������ַ�����֮������ʱ�䡣'#13#10'�ڶ�ȡ�����ڼ䣬�ӽ�' +
          '�յ���һ���ַ�ʱ��ʼ��ʱ��'#13#10'�����������ַ�����֮���ʱ��������' +
          '������'#13#10'ֵ�����ȡ������ɣ����ػ������ݡ�'#13#10'�����0����ʾ��ʹ' +
          '�ü����ʱ��'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object seReadTotalTimeoutMultiplier: TCnSpinEdit
        Left = 136
        Top = 41
        Width = 97
        Height = 21
        Hint = 
          '�����趨���ܳ�ʱʱ�䡣'#13#10'���ܳ�ʱʱ�� = (�ܳ�ʱϵ�� X �����ַ���)' +
          ' + �ܳ�ʱ����'#13#10'������ϵ���ɷֱ�Ϊ0��'#13#10'�����Ϊ0����ʹ���ܳ�ʱ' +
          '�趨��'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object seReadTotalTimeoutConstant: TCnSpinEdit
        Left = 136
        Top = 71
        Width = 97
        Height = 21
        Hint = 
          '�����趨���ܳ�ʱʱ�䡣'#13#10'���ܳ�ʱʱ�� = (�ܳ�ʱϵ�� X �����ַ���)' +
          ' + �ܳ�ʱ����'#13#10'������ϵ���ɷֱ�Ϊ0��'#13#10'�����Ϊ0����ʹ���ܳ�ʱ' +
          '�趨��'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 2
        Value = 0
      end
      object seWriteTotalTimeoutMultiplier: TCnSpinEdit
        Left = 136
        Top = 101
        Width = 97
        Height = 21
        Hint = 
          '�����趨д�ܳ�ʱʱ�䡣'#13#10'д�ܳ�ʱʱ�� = (�ܳ�ʱϵ�� X �����ַ���)' +
          ' + �ܳ�ʱ����'#13#10'������ϵ���ɷֱ�Ϊ0��'#13#10'�����Ϊ0����ʹ���ܳ�ʱ' +
          '�趨��'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object seWriteTotalTimeoutConstant: TCnSpinEdit
        Left = 136
        Top = 131
        Width = 97
        Height = 21
        Hint = 
          '�����趨д�ܳ�ʱʱ�䡣'#13#10'д�ܳ�ʱʱ�� = (�ܳ�ʱϵ�� X �����ַ���)' +
          ' + �ܳ�ʱ����'#13#10'������ϵ���ɷֱ�Ϊ0��'#13#10'�����Ϊ0����ʹ���ܳ�ʱ' +
          '�趨��'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
      end
    end
  end
  object bbtnOk: TBitBtn
    Left = 144
    Top = 288
    Width = 75
    Height = 21
    Caption = 'ȷ��(&O)'
    Default = True
    TabOrder = 2
    OnClick = bbtnOkClick
  end
  object bbtnCancel: TBitBtn
    Left = 224
    Top = 288
    Width = 75
    Height = 21
    Cancel = True
    Caption = 'ȡ��(&C)'
    ModalResult = 2
    TabOrder = 3
  end
  object cbShowHint: TCheckBox
    Left = 8
    Top = 290
    Width = 105
    Height = 17
    Caption = '��ʾ��ʾ��Ϣ'
    TabOrder = 1
    OnClick = cbShowHintClick
  end
end
