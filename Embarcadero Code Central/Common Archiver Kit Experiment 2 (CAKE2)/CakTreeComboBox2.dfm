object DropDownForm: TDropDownForm
  Left = 194
  Top = 168
  BorderStyle = bsNone
  BorderWidth = 1
  Caption = 'DropDownForm'
  ClientHeight = 155
  ClientWidth = 131
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CAKTreeView21: TCAKTreeView2
    Left = 0
    Top = 0
    Width = 131
    Height = 155
    Align = alClient
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = CAKTreeView21Change
    TreeTopNode = '{Dir View}'
    PassiveMode = True
    UpdateCak = True
  end
end
