object fbk8x00_settings: Tfbk8x00_settings
  Left = 208
  Top = 151
  BorderStyle = bsDialog
  Caption = 'BK8x00 settings'
  ClientHeight = 166
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 265
    Height = 121
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 48
      Height = 13
      Caption = 'Serial Port'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 43
      Height = 13
      Caption = 'Baudrate'
    end
    object Label3: TLabel
      Left = 8
      Top = 64
      Width = 59
      Height = 13
      Caption = 'Bus Address'
    end
    object Label4: TLabel
      Left = 8
      Top = 88
      Width = 61
      Height = 13
      Caption = 'Write Length'
    end
    object ComboBox1: TComboBox
      Left = 112
      Top = 16
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 0
      Text = 'Com1'
      Items.Strings = (
        'None'
        'Com1'
        'Com2'
        'Com3'
        'Com4'
        'Com5'
        'Com6'
        'Com7'
        'Com8')
    end
    object ComboBox2: TComboBox
      Left = 112
      Top = 40
      Width = 145
      Height = 21
      ItemHeight = 13
      ItemIndex = 9
      TabOrder = 1
      Text = '38400'
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
        '115200'
        '128000'
        '256000')
    end
    object Edit1: TEdit
      Left = 136
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '11'
    end
    object Edit2: TEdit
      Left = 136
      Top = 88
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '0'
    end
  end
  object Apply: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 1
    OnClick = ApplyClick
  end
  object Cancel: TButton
    Left = 200
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelClick
  end
end
