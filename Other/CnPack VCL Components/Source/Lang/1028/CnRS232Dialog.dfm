object CnRS232Dlg: TCnRS232Dlg
  Left = 238
  Top = 123
  BorderStyle = bsDialog
  Caption = '��f�]�m'
  ClientHeight = 316
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = CHINESEBIG5_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '�ө���'
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
      Caption = '�`�W�]�m'
      object lblBaudRate: TLabel
        Left = 16
        Top = 12
        Width = 60
        Height = 12
        Caption = '�i�S�v(&B):'
      end
      object lblByteSize: TLabel
        Left = 16
        Top = 41
        Width = 60
        Height = 12
        Caption = '�ƾڦ�(&D):'
      end
      object lblParity: TLabel
        Left = 16
        Top = 70
        Width = 72
        Height = 12
        Caption = '�_������(&P):'
      end
      object lblStopBits: TLabel
        Left = 16
        Top = 98
        Width = 60
        Height = 12
        Caption = '�����(&S):'
      end
      object cbbBaudRate: TComboBox
        Left = 120
        Top = 8
        Width = 145
        Height = 20
        Hint = '��f�q�T���̤j�t��'#13#10'���Gbps'
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
        Hint = '�i�μƾڦ��'
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
        Hint = '�_������覡'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 2
        OnChange = ControlChanged
        Items.Strings = (
          '�L'
          '�_����'
          '������'
          '�Ǹ�����'
          '�Ÿ����� ')
      end
      object cbbStopBits: TComboBox
        Left = 120
        Top = 94
        Width = 145
        Height = 20
        Hint = '������'
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
        Hint = '�X�{�_��������ɥΫ��w�r�ťN��'
        Caption = '�_��������~�������r��(&ASCII):'
        TabOrder = 4
        OnClick = ControlChanged
      end
      object cbIgnoreNullChar: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = '�O�_��󱵦��쪺NULL(ASCII 0)�r��'
        Caption = '����NULL�r��(&N)'
        TabOrder = 6
      end
      object seReplacedChar: TCnSpinEdit
        Left = 216
        Top = 126
        Width = 49
        Height = 21
        Hint = '�����r�Ū�ASCII�X'
        MaxLength = 3
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnExit = seReplacedCharExit
      end
    end
    object tsXonXoff: TTabSheet
      Caption = '�n��y�q����'
      ImageIndex = 1
      object lblXonLimit: TLabel
        Left = 16
        Top = 87
        Width = 66
        Height = 12
        Caption = 'Xon�H��(&B):'
      end
      object lblXoffLimit: TLabel
        Left = 16
        Top = 117
        Width = 72
        Height = 12
        Caption = 'Xoff�H��(&H):'
      end
      object lblXonChar: TLabel
        Left = 16
        Top = 146
        Width = 108
        Height = 12
        Caption = 'Xon�r��(ASCII)(&K):'
      end
      object lblXoffChar: TLabel
        Left = 16
        Top = 176
        Width = 114
        Height = 12
        Caption = 'Xoff�r��(ASCII)(&I):'
      end
      object cbTxContinueOnXoff: TCheckBox
        Left = 16
        Top = 56
        Width = 200
        Height = 17
        Hint = 
          '�����w�İϤw���A�w�o�e�uXoff�r�šv��o�e�O�_����C'#13#10'�p�G��ܡA' +
          '��Q�񺡪������w�İϤ����r�`�ƥ��F��uXoff�H�ȡv'#13#10'�åB�X�ʵ{�ǵo' +
          '�e�F�uXoff�r�šv�ᰱ����r�`�ɡA�~��o�e�F'#13#10'�p�G����A��Q�ƪ�' +
          '���w�İϤ����r�`�Ƥ����uXon�H�ȡv�Ӧr'#13#10'�`�A�B�X�ʵ{�ǵo�e�F�uXon' +
          '�r�šv���_�����ɡA�~��o�e�C'
        Caption = 'Xoff���~��o�e�ƾ�(&C)'
        TabOrder = 2
      end
      object cbOutx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 32
        Width = 200
        Height = 17
        Hint = 
          '�ƾڵo�e�ɬO�_�ϥ�Xon/Xoff�H���y����'#13#10'�p�G��ܡA������uXoff�r' +
          '�šv�ɼȰ��o�e�A�æb'#13#10'������uXon�r�šv�ɫ�_�o�e�C'
        Caption = '��XXon/Xoff����(&O)'
        TabOrder = 1
      end
      object cbInx_XonXoffFlow: TCheckBox
        Left = 16
        Top = 8
        Width = 200
        Height = 17
        Hint = 
          '�ƾڱ����ɬO�_�ϥ�Xon/Xoff�H���y����'#13#10'�p�G��ܡA�����w�İϧֺ�' +
          '�A�u�ѡuXoff�H�ȡv��'#13#10'�r�ŪŶ��ɵo�e�uXoff�r�šv�F�����w�İϤ�' +
          '�u��'#13#10'�uXon�H�ȡv�Ӧr�ŮɡA�o�e�uXon�r�šv�C'
        Caption = '��JXon/Xoff����(&F)'
        TabOrder = 0
      end
      object seXonLimit: TCnSpinEdit
        Left = 136
        Top = 83
        Width = 65
        Height = 21
        Hint = '�����b�o�e�uXon�r�šv���e�A�����w�İϤ����\���̤֦r�żơC'
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
        Hint = '�o�e�M�������uXon�r�šv��ASCII�X�A��ܤ��\�~��ǿ�C'
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
        Hint = '�o�e�M�������uXoff�r�šv��ASCII�X�A��ܤ��\�Ȱ��ǿ�C'
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
          '�����b�o�e�uXoff�r�šv���e�A�����w�İϤ����\���̦h�r�żơC'#13#10'����' +
          '�w�İϪ����״�h�ӭȡA�Y���\���̦h�r�żơC'
        MaxLength = 5
        MaxValue = 65535
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnExit = seXonLimitExit
      end
    end
    object tsHardware: TTabSheet
      Caption = '�w��y�q����'
      ImageIndex = 2
      object lblDtrControl: TLabel
        Left = 16
        Top = 39
        Width = 90
        Height = 12
        Caption = 'DTR�y�q����(&T):'
      end
      object lblRtsControl: TLabel
        Left = 16
        Top = 68
        Width = 90
        Height = 12
        Caption = 'RTS�y�q����(&R):'
      end
      object lblInCtrl: TLabel
        Left = 16
        Top = 16
        Width = 54
        Height = 12
        Caption = '��J����:'
      end
      object lblOutCtrl: TLabel
        Left = 16
        Top = 108
        Width = 54
        Height = 12
        Caption = '��X����:'
      end
      object cbOutx_CtsFlow: TCheckBox
        Left = 16
        Top = 152
        Width = 200
        Height = 17
        Hint = 'CTS(�M���o�e)'
        Caption = '�ϥ�CTS�H���i���X�y�q����(&C)'
        TabOrder = 3
      end
      object cbOutx_DsrFlow: TCheckBox
        Left = 16
        Top = 128
        Width = 200
        Height = 17
        Hint = 'DSR(�ƾڳ]�ƴN��)'
        Caption = '�ϥ�DSR�H���i���X�y�q����(&B)'
        TabOrder = 2
      end
      object cbDsrSensitivity: TCheckBox
        Left = 16
        Top = 176
        Width = 200
        Height = 17
        Hint = 
          '���w�q�H�X�ʵ{�ǹ�DSR�H�������A�O�_�ӷP�C'#13#10'�p�G��ܡA��Modem��DS' +
          'R��J�u���C�ɡA�X�ʵ{�ǱN'#13#10'���������쪺����r�`�C'
        Caption = 'DSR�ӷP��(&E)'
        TabOrder = 4
      end
      object cbbDtrControl: TComboBox
        Left = 120
        Top = 35
        Width = 145
        Height = 20
        Hint = '�ϥ�DTR(�ƾڲ׺ݴN��)�H���i��y�q����覡'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 0
        Items.Strings = (
          '���\DTR�u�ëO��'
          '�T��DTR�u�ëO��'
          '���\DTR����')
      end
      object cbbRtsControl: TComboBox
        Left = 120
        Top = 64
        Width = 145
        Height = 20
        Hint = '�ϥ�RTS(�ШD�o�e)�H���i��y�q����覡'
        Style = csDropDownList
        ItemHeight = 12
        TabOrder = 1
        Items.Strings = (
          '���\RTS�ëO��'
          '�T��RTS�ëO��'
          '���\RTS����'
          '�ϥ�Ĳ�o�覡')
      end
    end
    object tsTimeouts: TTabSheet
      Caption = '�W�ɳ]�m'
      ImageIndex = 3
      object lblReadIntervalTimeout: TLabel
        Left = 16
        Top = 15
        Width = 84
        Height = 12
        Caption = 'Ū���j�W��(&R):'
      end
      object lblReadTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 45
        Width = 96
        Height = 12
        Caption = 'Ū�`�W�ɫY��(&T):'
      end
      object lblMSec1: TLabel
        Left = 240
        Top = 16
        Width = 24
        Height = 12
        Caption = '�@��'
      end
      object lblMSec2: TLabel
        Left = 240
        Top = 46
        Width = 24
        Height = 12
        Caption = '�@��'
      end
      object lblReadTotalTimeoutConstant: TLabel
        Left = 16
        Top = 75
        Width = 96
        Height = 12
        Caption = 'Ū�`�W�ɱ`�q(&A):'
      end
      object lblMSec3: TLabel
        Left = 240
        Top = 76
        Width = 24
        Height = 12
        Caption = '�@��'
      end
      object lblWriteTotalTimeoutMultiplier: TLabel
        Left = 16
        Top = 105
        Width = 96
        Height = 12
        Caption = '�g�`�W�ɫY��(&W):'
      end
      object lblMSec4: TLabel
        Left = 240
        Top = 106
        Width = 24
        Height = 12
        Caption = '�@��'
      end
      object lblWriteTotalTimeoutConstant: TLabel
        Left = 16
        Top = 135
        Width = 96
        Height = 12
        Caption = '�g�`�W�ɱ`�q(&B):'
      end
      object lblMSec5: TLabel
        Left = 240
        Top = 136
        Width = 24
        Height = 12
        Caption = '�@��'
      end
      object seReadIntervalTimeout: TCnSpinEdit
        Left = 136
        Top = 11
        Width = 97
        Height = 21
        Hint = 
          '���w�q�H�u���W��Ӧr�Ũ�F�������̤j�ɶ��C'#13#10'�bŪ���ާ@�����A�q��' +
          '����Ĥ@�Ӧr�Ůɶ}�l�p�ɡA'#13#10'�Y���N��Ӧr�Ũ�F�������ɶ����j�W�L' +
          '�o�ӳ̤j'#13#10'�ȡA�hŪ���ާ@�����A��^�w�ļƾڡC'#13#10'�p�G�m0�A��ܤ���' +
          '�ζ��j�W�ɡC'
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
          '�Ω�]�wŪ�`�W�ɮɶ��C'#13#10'Ū�`�W�ɮɶ� = (�`�W�ɫY�� X �����r�ż�)' +
          ' + �`�W�ɱ`�q'#13#10'�`�q�M�Y�ƥi���O��0�C'#13#10'�p�G����0�A�h���ϥ��`�W��' +
          '�]�w�C'
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
          '�Ω�]�wŪ�`�W�ɮɶ��C'#13#10'Ū�`�W�ɮɶ� = (�`�W�ɫY�� X �����r�ż�)' +
          ' + �`�W�ɱ`�q'#13#10'�`�q�M�Y�ƥi���O��0�C'#13#10'�p�G����0�A�h���ϥ��`�W��' +
          '�]�w�C'
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
          '�Ω�]�w�g�`�W�ɮɶ��C'#13#10'�g�`�W�ɮɶ� = (�`�W�ɫY�� X �����r�ż�)' +
          ' + �`�W�ɱ`�q'#13#10'�`�q�M�Y�ƥi���O��0�C'#13#10'�p�G����0�A�h���ϥ��`�W��' +
          '�]�w�C'
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
          '�Ω�]�w�g�`�W�ɮɶ��C'#13#10'�g�`�W�ɮɶ� = (�`�W�ɫY�� X �����r�ż�)' +
          ' + �`�W�ɱ`�q'#13#10'�`�q�M�Y�ƥi���O��0�C'#13#10'�p�G����0�A�h���ϥ��`�W��' +
          '�]�w�C'
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
    Caption = '�T�w(&O)'
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
    Caption = '����(&C)'
    ModalResult = 2
    TabOrder = 3
  end
  object cbShowHint: TCheckBox
    Left = 8
    Top = 290
    Width = 105
    Height = 17
    Caption = '��ܴ��ܫH��'
    TabOrder = 1
    OnClick = cbShowHintClick
  end
end
