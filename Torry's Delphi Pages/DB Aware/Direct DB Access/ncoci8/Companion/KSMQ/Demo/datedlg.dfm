object DateDialog: TDateDialog
  Left = 261
  Top = 162
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Calendar'
  ClientHeight = 254
  ClientWidth = 383
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Calendar1: TCalendar
    Left = 0
    Top = 30
    Width = 383
    Height = 194
    Align = alClient
    StartOfWeek = 0
    TabOrder = 0
    OnDblClick = Calendar1DblClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 224
    Width = 383
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object bbOk: TBitBtn
      Left = 216
      Top = 3
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object bbCancel: TBitBtn
      Left = 304
      Top = 2
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 383
    Height = 30
    Align = alTop
    Caption = ' '
    TabOrder = 2
    object seYear: TSpinEdit
      Left = 8
      Top = 4
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = seYearChange
    end
    object cbMonth: TComboBox
      Left = 88
      Top = 4
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbMonthChange
    end
  end
end
