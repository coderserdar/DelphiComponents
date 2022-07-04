object adbform: Tadbform
  Left = 349
  Top = 317
  Width = 398
  Height = 243
  Caption = 'adbform'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  PopupMenu = PopupMenu
  Position = poDefault
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 120
  TextHeight = 16
  object smDBText: TsmDBText
    Left = 24
    Top = 24
    Width = 337
    Height = 33
    DataField = 'lowercase'
    DataSource = DataSource
    Language = 0
    Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
  end
  object DBNavigator: TDBNavigator
    Left = 0
    Top = 185
    Width = 390
    Height = 25
    DataSource = DataSource
    Align = alBottom
    TabOrder = 0
  end
  object DataSource: TDataSource
    DataSet = ADOTable
    Left = 104
    Top = 24
  end
  object ADOTable: TADOTable
    TableName = 'mtlng'
    Left = 72
    Top = 24
  end
  object ActionList1: TActionList
    Left = 304
    Top = 112
    object smLowercase: TsmAction
      Category = 'Language'
      Caption = 'lowercase'
      GroupIndex = 1
      OnExecute = smLowercaseExecute
      Language = 0
      Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
    end
    object smUppercase: TsmAction
      Category = 'Language'
      Caption = 'uppercase'
      GroupIndex = 1
      OnExecute = smUppercaseExecute
      Language = 0
      Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
    end
  end
  object PopupMenu: TPopupMenu
    Left = 256
    Top = 112
    object LOWERCASE1: TMenuItem
      Action = smLowercase
    end
    object UPPERCASE1: TMenuItem
      Action = smUppercase
    end
  end
end
