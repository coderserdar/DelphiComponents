object frmConDlg: TfrmConDlg
  Left = 506
  Top = 174
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Connect to'
  ClientHeight = 207
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 166
    Width = 324
    Height = 41
    Align = alBottom
    TabOrder = 0
    object bOK: TBitBtn
      Left = 157
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = bOKClick
      Kind = bkOK
    end
    object bCancel: TBitBtn
      Left = 241
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = bCancelClick
      Kind = bkCancel
    end
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 7
    Width = 310
    Height = 69
    Caption = ' Connect to: '
    TabOrder = 1
    object Label5: TLabel
      Left = 11
      Top = 20
      Width = 54
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server host'
    end
    object Label8: TLabel
      Left = 229
      Top = 20
      Width = 52
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server port'
    end
    object edHost: TEdit
      Left = 10
      Top = 36
      Width = 207
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object sedPort: TSpinEdit
      Left = 224
      Top = 35
      Width = 78
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 84
    Width = 309
    Height = 69
    Caption = ' as: '
    TabOrder = 2
    object Label1: TLabel
      Left = 14
      Top = 17
      Width = 48
      Height = 13
      Alignment = taRightJustify
      Caption = 'Username'
    end
    object Label6: TLabel
      Left = 181
      Top = 17
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = 'Password'
    end
    object edUser: TEdit
      Left = 12
      Top = 33
      Width = 165
      Height = 21
      TabOrder = 0
      Text = 'Test'
    end
    object edPassword: TEdit
      Left = 180
      Top = 33
      Width = 121
      Height = 21
      Color = cl3DLight
      Enabled = False
      ReadOnly = True
      TabOrder = 1
    end
  end
end
