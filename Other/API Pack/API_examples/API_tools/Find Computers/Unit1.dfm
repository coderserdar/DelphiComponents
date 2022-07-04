object Form1: TForm1
  Left = 192
  Top = 107
  Width = 209
  Height = 356
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    201
    322)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 88
    Width = 86
    Height = 13
    Caption = 'Computers Found:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Workgroup:'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 104
    Width = 185
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 56
    Width = 185
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Find Computers on Lan'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 304
    Width = 185
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object API_tools1: TAPI_tools
    Left = 104
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    IPVersion = Id_IPv4
    Port = 0
    ReadTimeout = 0
    Left = 136
  end
end
