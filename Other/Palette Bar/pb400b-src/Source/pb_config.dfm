object PaletteConfig: TPaletteConfig
  Left = 941
  Top = 158
  Width = 300
  Height = 300
  BorderStyle = bsSizeToolWin
  Caption = 'PaletteBar konfigurieren'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    292
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TButton
    Left = 187
    Top = 205
    Width = 96
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 187
    Top = 237
    Width = 96
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Abbruch'
    ModalResult = 2
    TabOrder = 1
  end
  object ConfigList: TTreeView
    Left = 8
    Top = 8
    Width = 172
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoExpand = True
    Images = Categories.MainImages
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnClick = ConfigListClick
  end
  object RadioGroup: TRadioGroup
    Left = 187
    Top = 8
    Width = 97
    Height = 81
    Anchors = [akTop, akRight]
    Caption = ' Clear '
    Items.Strings = (
      'History'
      'Favorites'
      'Searchtext')
    TabOrder = 3
  end
  object BtnClear: TButton
    Left = 187
    Top = 97
    Width = 97
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 4
    OnClick = BtnClearClick
  end
end
