object FormClipboard: TFormClipboard
  Left = 0
  Top = 0
  Caption = 'Clipboard'
  ClientHeight = 240
  ClientWidth = 459
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 475
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 204
    Width = 459
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      459
      36)
    object Clear1: TButton
      Left = 6
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Clear1Click
    end
    object Close1: TButton
      Left = 380
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      TabOrder = 1
      OnClick = Close1Click
    end
    object cxColorComboBox1: TcxColorComboBox
      Left = 87
      Top = 8
      ColorValue = clBlack
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncX11
      Properties.PrepareList = cxplX11
      Properties.OnChange = cxColorComboBox1PropertiesChange
      TabOrder = 2
      Width = 121
    end
    object ClipboardFormatNames1: TComboBox
      Left = 214
      Top = 7
      Width = 159
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
  end
  object JvClipboardViewer1: TJvClipboardViewer
    Left = 0
    Top = 0
    Width = 459
    Height = 204
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clBlack
    TabOrder = 1
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Kind = lfFlat
    NativeStyle = True
    Left = 22
    Top = 16
  end
  object JvClipboardMonitor1: TJvClipboardMonitor
    OnChange = JvClipboardMonitor1Change
    Left = 22
    Top = 46
  end
end
