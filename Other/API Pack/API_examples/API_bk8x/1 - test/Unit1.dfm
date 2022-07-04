object Form1: TForm1
  Left = 206
  Top = 114
  Width = 403
  Height = 254
  Caption = 'API_BK8x Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 55
    Height = 13
    Caption = 'Last error..'
  end
  object Label2: TLabel
    Left = 16
    Top = 138
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 16
    Top = 157
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 16
    Top = 176
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label5: TLabel
    Left = 16
    Top = 195
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label6: TLabel
    Left = 330
    Top = 138
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label7: TLabel
    Left = 330
    Top = 157
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object ListBox1: TListBox
    Left = 16
    Top = 56
    Width = 345
    Height = 76
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 286
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 280
    Top = 33
    Width = 81
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Stop on Error'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 208
    Top = 8
  end
  object API_bk8x1: TAPI_bk8x
    Port = 1
    Baudrate = 38400
    Open = False
    Address = 11
    MessageIdent = 11
    WriteLength = 1
    ThreadEvent = API_bk8x1ThreadEvent
    ThreadPriority = tpIdle
    ThreadError = API_bk8x1ThreadError
    ReadTimeout = 500
    ExpectedReadLength = 0
    Left = 208
    Top = 176
  end
end
