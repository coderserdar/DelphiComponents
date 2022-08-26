object PaletteConfig: TPaletteConfig
  Left = 511
  Top = 366
  BorderStyle = bsDialog
  Caption = 'PaletteBar konfigurieren'
  ClientHeight = 334
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    401
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TButton
    Left = 296
    Top = 265
    Width = 96
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 296
    Top = 297
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
    Width = 281
    Height = 313
    AutoExpand = True
    Images = PBar.MainImages
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnClick = ConfigListClick
  end
  object RadioGroup: TRadioGroup
    Left = 296
    Top = 8
    Width = 97
    Height = 81
    Caption = ' Clear '
    Items.Strings = (
      'History'
      'Favorites'
      'Searchtext')
    TabOrder = 3
  end
  object BtnClear: TButton
    Left = 296
    Top = 97
    Width = 97
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Clear'
    TabOrder = 4
    OnClick = BtnClearClick
  end
end
