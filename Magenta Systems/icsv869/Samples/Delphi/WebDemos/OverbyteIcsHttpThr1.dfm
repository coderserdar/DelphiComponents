object HttpThreadForm: THttpThreadForm
  Left = 168
  Top = 120
  Caption = 'HTTP Treaded Test'
  ClientHeight = 266
  ClientWidth = 602
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 344
    Top = 64
    Width = 43
    Height = 13
    Caption = 'Thread 0'
  end
  object Label2: TLabel
    Tag = 1
    Left = 344
    Top = 86
    Width = 43
    Height = 13
    Caption = 'Thread 1'
  end
  object Label3: TLabel
    Tag = 2
    Left = 344
    Top = 109
    Width = 43
    Height = 13
    Caption = 'Thread 2'
  end
  object Label4: TLabel
    Tag = 3
    Left = 344
    Top = 131
    Width = 43
    Height = 13
    Caption = 'Thread 3'
  end
  object Label5: TLabel
    Tag = 4
    Left = 344
    Top = 154
    Width = 43
    Height = 13
    Caption = 'Thread 4'
  end
  object Label6: TLabel
    Tag = 5
    Left = 344
    Top = 176
    Width = 43
    Height = 13
    Caption = 'Thread 5'
  end
  object Thread0Label: TLabel
    Left = 408
    Top = 64
    Width = 43
    Height = 13
    Caption = 'Thread 0'
  end
  object Thread1Label: TLabel
    Left = 408
    Top = 86
    Width = 43
    Height = 13
    Caption = 'Thread 1'
  end
  object Thread2Label: TLabel
    Left = 408
    Top = 109
    Width = 43
    Height = 13
    Caption = 'Thread 2'
  end
  object Thread3Label: TLabel
    Left = 408
    Top = 131
    Width = 43
    Height = 13
    Caption = 'Thread 3'
  end
  object Thread4Label: TLabel
    Left = 408
    Top = 154
    Width = 43
    Height = 13
    Caption = 'Thread 4'
  end
  object Thread5Label: TLabel
    Left = 408
    Top = 176
    Width = 43
    Height = 13
    Caption = 'Thread 5'
  end
  object Label14: TLabel
    Left = 24
    Top = 16
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object Label15: TLabel
    Left = 16
    Top = 40
    Width = 26
    Height = 13
    Caption = 'Proxy'
  end
  object URLEdit: TEdit
    Left = 64
    Top = 8
    Width = 161
    Height = 21
    TabOrder = 0
    Text = 'http://www.borland.com'
  end
  object ResultsMemo: TMemo
    Left = 8
    Top = 64
    Width = 329
    Height = 177
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'ResultsMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object DoItButton: TButton
    Left = 264
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Do It'
    TabOrder = 2
    OnClick = DoItButtonClick
  end
  object ProgressListBox: TListBox
    Left = 456
    Top = 64
    Width = 105
    Height = 177
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 3
  end
  object ProxyEdit: TEdit
    Left = 64
    Top = 32
    Width = 161
    Height = 21
    TabOrder = 4
    Text = 'ProxyEdit'
  end
end
