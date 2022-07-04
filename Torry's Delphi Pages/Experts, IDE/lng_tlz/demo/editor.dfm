object editorForm: TeditorForm
  Left = 289
  Top = 304
  Width = 440
  Height = 234
  Caption = 'editorForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 120
  TextHeight = 16
  object smRichEdit: TsmRichEdit
    Left = 0
    Top = 0
    Width = 432
    Height = 201
    HelpType = htKeyword
    Align = alClient
    Lines.Strings = (
      'THIS IS UPPERCASE MESSAGE')
    PopupMenu = PopupMenu
    ReadOnly = True
    TabOrder = 0
    Language = 1
    Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
  end
  object ActionList: TActionList
    Left = 272
    Top = 64
    object smLowercase: TsmAction
      Category = 'Language'
      GroupIndex = 1
      OnExecute = smLowercaseExecute
      Language = 1
      Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
    end
    object smUppercase: TsmAction
      Category = 'Language'
      GroupIndex = 1
      OnExecute = smUppercaseExecute
      Language = 1
      Options = [toSuppressTranslateIfLanguageActionIsLinked, toIgnorePostfix]
    end
  end
  object PopupMenu: TPopupMenu
    Left = 120
    Top = 88
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
