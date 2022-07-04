object QBDemoForm: TQBDemoForm
  Left = 353
  Top = 194
  Width = 455
  Height = 180
  Caption = 'QBuilder with NCOCI8 Engine'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 33
    Width = 447
    Height = 120
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 33
    Align = alTop
    TabOrder = 1
    object BtnQBuilder: TButton
      Left = 1
      Top = 0
      Width = 144
      Height = 33
      Caption = 'Run QBuilder-NCOCI8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BtnQBuilderClick
    end
  end
  object OQBDialog: TOQBuilderDialog
    OQBEngine = OQBEngineNCOCI8
    Left = 172
    Top = 4
  end
  object OQBEngineNCOCI8: TOQBEngineNCOCI8
    Left = 205
    Top = 4
  end
  object OCIDatabase1: TOCIDatabase
    Left = 237
    Top = 4
  end
end
