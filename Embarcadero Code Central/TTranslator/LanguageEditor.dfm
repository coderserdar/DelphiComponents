object dlgLangEditor: TdlgLangEditor
  Left = 529
  Top = 343
  BorderStyle = bsDialog
  Caption = 'dlgLangEditor'
  ClientHeight = 129
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRight: TPanel
    Left = 297
    Top = 0
    Width = 86
    Height = 129
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object cmdOk: TBitBtn
      Left = 4
      Top = 11
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = cmdOkClick
      Kind = bkOK
    end
    object cmdCancel: TBitBtn
      Left = 4
      Top = 45
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 129
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    Caption = 'pnlBottom'
    TabOrder = 0
    object GroupBox: TGroupBox
      Left = 5
      Top = 5
      Width = 287
      Height = 119
      Align = alClient
      Caption = 'Properties'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object lbl1: TLabel
        Left = 16
        Top = 24
        Width = 73
        Height = 13
        Caption = 'Add Language:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbl2: TLabel
        Left = 16
        Top = 56
        Width = 53
        Height = 13
        Caption = 'At Position:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbl3: TLabel
        Left = 16
        Top = 88
        Width = 100
        Height = 13
        Caption = 'Copy Properties from:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object edt1: TEdit
        Left = 120
        Top = 20
        Width = 105
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = edt1Change
      end
      object cbo1: TComboBox
        Left = 120
        Top = 52
        Width = 153
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 1
        OnChange = cbo1Change
      end
      object cbo2: TComboBox
        Left = 120
        Top = 84
        Width = 153
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 2
        OnChange = cbo2Change
      end
    end
  end
end
