object frmMain: TfrmMain
  Left = 201
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Property Inspector Demo'
  ClientHeight = 515
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 368
    Top = 256
    Width = 108
    Height = 13
    Caption = 'Active Property: (none)'
  end
  object Label1: TLabel
    Left = 368
    Top = 280
    Width = 74
    Height = 13
    Caption = 'Current objects:'
  end
  object ELPropertyInspector1: TELPropertyInspector
    Left = 8
    Top = 8
    Width = 193
    Height = 233
    Splitter = 92
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnGetComponent = ELPropertyInspector1GetComponent
    OnGetComponentName = ELPropertyInspector1GetComponentName
    OnFilterProp = ELPropertyInspector1FilterProp
    OnGetCaptionColor = ELPropertyInspector1GetCaptionColor
    OnClick = ELPropertyInspector1Click
  end
  object pnlContainer: TPanel
    Left = 208
    Top = 8
    Width = 353
    Height = 233
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 1
    object Label2: TLabel
      Left = 24
      Top = 120
      Width = 32
      Height = 13
      Caption = 'La&bel2'
    end
    object Panel1: TPanel
      Left = 12
      Top = 13
      Width = 185
      Height = 41
      Caption = 'Panel1'
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 52
      Top = 81
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
    end
    object Memo1: TMemo
      Left = 108
      Top = 122
      Width = 185
      Height = 89
      Lines.Strings = (
        'Memo1')
      TabOrder = 2
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 248
    Width = 193
    Height = 257
    Caption = 'Edit controls'
    TabOrder = 2
    object Shape1: TShape
      Left = 128
      Top = 184
      Width = 49
      Height = 25
    end
    object Button1: TButton
      Left = 16
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Edit Panel1'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 16
      Top = 56
      Width = 105
      Height = 25
      Caption = 'Edit CheckBox1'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 16
      Top = 88
      Width = 105
      Height = 25
      Caption = 'Edit Memo1'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 16
      Top = 151
      Width = 105
      Height = 26
      Caption = 'Edit Memo1.Font'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 16
      Top = 184
      Width = 105
      Height = 25
      Caption = 'Edit TShape.Brush'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button7: TButton
      Left = 16
      Top = 120
      Width = 105
      Height = 25
      Caption = 'Edit all'
      TabOrder = 5
      OnClick = Button7Click
    end
    object Button10: TButton
      Left = 16
      Top = 215
      Width = 161
      Height = 26
      Caption = 'Edit Label2 with FocusControl'
      TabOrder = 6
      OnClick = Button10Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 208
    Top = 248
    Width = 153
    Height = 257
    Caption = 'Filtering and highlighting'
    TabOrder = 3
    object Label4: TLabel
      Left = 5
      Top = 73
      Width = 142
      Height = 13
      Caption = 'Type here affected properties:'
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 16
      Width = 129
      Height = 17
      Caption = 'Filter properties'
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object Memo3: TMemo
      Left = 8
      Top = 88
      Width = 137
      Height = 161
      Lines.Strings = (
        'Color'
        'Font'
        'Align')
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 40
      Width = 129
      Height = 17
      Caption = 'Highlight properties'
      TabOrder = 2
      OnClick = CheckBox3Click
    end
  end
  object Memo2: TMemo
    Left = 368
    Top = 296
    Width = 193
    Height = 105
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object GroupBox3: TGroupBox
    Left = 368
    Top = 408
    Width = 193
    Height = 97
    Caption = 'BeveInner custom property editor'
    TabOrder = 5
    object RadioButton1: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Standart'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 32
      Width = 161
      Height = 17
      Caption = 'With replaced values'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object RadioButton3: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'With dialog'
      TabOrder = 2
      OnClick = RadioButton1Click
    end
    object RadioButton4: TRadioButton
      Left = 8
      Top = 64
      Width = 113
      Height = 17
      Caption = 'With graphics'
      TabOrder = 3
      OnClick = RadioButton1Click
    end
  end
end
