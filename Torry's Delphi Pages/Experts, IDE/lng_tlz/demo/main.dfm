object mainform: Tmainform
  Left = 353
  Top = 280
  Width = 531
  Height = 319
  Caption = 'mainform'
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object smLabel: TsmLabel
    Left = 163
    Top = 224
    Width = 3
    Height = 16
    HelpType = htKeyword
    Language = 0
  end
  object lLabel: TLabel
    Left = 160
    Top = 184
    Width = 37
    Height = 16
    Caption = 'lLabel'
  end
  object smADODriver: TsmADODriver
    DriverOptions = []
    Connected = True
    Left = 152
    Top = 216
  end
  object MainMenu: TMainMenu
    Left = 184
    Top = 216
    object File1: TMenuItem
      Action = smFile
      object OPEN1: TMenuItem
        Action = smOpenWithParentLanguage
      end
      object SAVE1: TMenuItem
        Action = smOpenWithoutParent
      end
      object OPENDBAWAREFORM1: TMenuItem
        Action = smDBForm
      end
      object EXIT1: TMenuItem
        Action = smExit
      end
    end
    object LANGUAGE1: TMenuItem
      Action = smLanguage
      object LOWERCASE1: TMenuItem
        Action = smLowercase
        GroupIndex = 1
        RadioItem = True
      end
      object UPPERCASE1: TMenuItem
        Action = smUppercase
        GroupIndex = 1
        RadioItem = True
      end
    end
  end
  object smLanguageOptions: TsmLanguageOptions
    ID = 'demo'
    Language = 0
    Left = 120
    Top = 216
  end
  object ActionList: TActionList
    Left = 216
    Top = 216
    object smFile: TsmAction
      Category = 'Language'
      Caption = 'file'
      OnExecute = smFileExecute
      Language = 0
    end
    object smExit: TsmAction
      Category = 'Language'
      Caption = 'exit'
      OnExecute = smExitExecute
      Language = 0
    end
    object smLowercase: TsmAction
      Category = 'Language'
      Caption = 'lowercase'
      GroupIndex = 1
      OnExecute = smLowercaseExecute
      Language = 0
    end
    object smUppercase: TsmAction
      Category = 'Language'
      Caption = 'uppercase'
      GroupIndex = 1
      OnExecute = smUppercaseExecute
      Language = 0
    end
    object smLanguage: TsmAction
      Category = 'Language'
      Caption = 'language'
      OnExecute = smLanguageExecute
      Language = 0
    end
    object smOpenWithParentLanguage: TsmAction
      Category = 'Language'
      Caption = 'open with parent language'
      OnExecute = smOpenWithParentLanguageExecute
      Language = 0
    end
    object smOpenWithoutParent: TsmAction
      Category = 'Language'
      Caption = 'OPEN WITHOUT PARENT'
      OnExecute = smOpenWithoutParentExecute
      Language = 1
    end
    object smDBForm: TsmAction
      Category = 'Language'
      Caption = 'OPEN DB AWARE FORM'
      OnExecute = smDBFormExecute
      Language = 1
    end
  end
end
