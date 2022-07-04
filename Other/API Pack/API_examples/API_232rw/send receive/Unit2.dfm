object Form1: TForm1
  Left = 770
  Top = 106
  Width = 524
  Height = 307
  Caption = 'API_232rw example 2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 0
    Top = 160
    Width = 60
    Height = 13
    Caption = 'Example log:'
  end
  object Memo1: TMemo
    Left = 0
    Top = 173
    Width = 499
    Height = 110
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 8
    Width = 497
    Height = 49
    Caption = 'Settings'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 78
      Height = 13
      Caption = 'Select serial port'
    end
    object Label2: TLabel
      Left = 168
      Top = 16
      Width = 82
      Height = 13
      Caption = ', with baudrate of'
    end
    object Label3: TLabel
      Left = 344
      Top = 16
      Width = 20
      Height = 13
      Caption = 'bps.'
    end
    object ComboBox1: TComboBox
      Left = 96
      Top = 16
      Width = 65
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Com1'
      Items.Strings = (
        'Com1'
        'Com2'
        'Com3'
        'Com4'
        'Com5'
        'Com6'
        'Com7'
        'Com8')
    end
    object Edit1: TEdit
      Left = 256
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 1
      Text = '38400'
    end
    object Button4: TButton
      Left = 416
      Top = 16
      Width = 75
      Height = 25
      Caption = 'open'
      TabOrder = 2
      OnClick = Button4Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 56
    Width = 497
    Height = 49
    Caption = 'Receive'
    TabOrder = 2
    object Label4: TLabel
      Left = 8
      Top = 16
      Width = 102
      Height = 13
      Caption = 'Receive files to folder'
    end
    object Edit2: TEdit
      Left = 120
      Top = 16
      Width = 289
      Height = 21
      TabOrder = 0
      Text = 'C:\TEMP'
    end
    object Button1: TButton
      Left = 416
      Top = 16
      Width = 75
      Height = 25
      Caption = 'browse'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 104
    Width = 497
    Height = 49
    Caption = 'Send'
    TabOrder = 3
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 41
      Height = 13
      Caption = 'Send file'
    end
    object Edit3: TEdit
      Left = 56
      Top = 16
      Width = 281
      Height = 21
      TabOrder = 0
    end
    object Button2: TButton
      Left = 344
      Top = 16
      Width = 75
      Height = 25
      Caption = 'browse'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 416
      Top = 16
      Width = 75
      Height = 25
      Caption = 'send'
      TabOrder = 2
    end
  end
  object API_files1: TAPI_files
    ShowProgress = True
    Confirmation = False
    Left = 256
    Top = 144
  end
  object OpenDialog1: TOpenDialog
    Left = 288
    Top = 144
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 224
    Top = 144
  end
end
