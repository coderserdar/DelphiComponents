object dlgStringsEditor: TdlgStringsEditor
  Left = 442
  Top = 211
  Width = 754
  Height = 481
  Caption = 'dlgStringsEditor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000888800000000000000000
    444000000888888888000000000000004C400008888888FF8888800000000000
    C44008888888877777777770000000004C478888888700000000800000000000
    C4478888887888FFF8044800000000006CC78888888FF8877744008000000000
    C6C788888888777CCC0220000000000066C7777777772CC40422220800000000
    77687AAA2A22CCC62AA222080000000066078AAAA26CCCC4A7AA220000000000
    00038AAA26CCCCC677AA22200000000000076FFA2666CC6AA7AAA22000000000
    0008E2766C666CC727AAA220000000000007866CCC666C44C42AA22000000000
    0008FEEE26C6C6A42447AA280000000000088FE622CC6666A2443A2800000000
    00007F62A22CC6CC6A24420000000000000007FA2222CCCCC444408000000000
    0000088FAA222C22CCCC08000000000000000088F8AA22222C44800000000000
    000000087888AA22224800000000000000000000088877778800000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE1FFFF3F803FFF1E0007FF180001FF000
    001FF000007FF000003FF000001FF000001FF000000FF000000FF200000FFE00
    000FFE00000FFE00000FFE00000FFE00000FFE00000FFF00001FFF80001FFF80
    003FFFC0007FFFE000FFFFF803FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  Menu = Menu
  OldCreateOrder = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object bvlTop: TBevel
    Left = 0
    Top = 0
    Width = 746
    Height = 2
    Align = alTop
    Shape = bsBottomLine
  end
  object PageControl: TPageControl
    Left = 0
    Top = 2
    Width = 746
    Height = 433
    ActivePage = tsClassesProperties
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    object tsComponentProperties: TTabSheet
      Caption = '&Component properties'
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 738
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblShowProperties: TLabel
          Left = 312
          Top = 12
          Width = 79
          Height = 13
          Caption = 'Show properties:'
        end
        object Label1: TLabel
          Left = 8
          Top = 12
          Width = 50
          Height = 13
          Caption = 'Show unit:'
        end
        object cboShow: TComboBox
          Left = 400
          Top = 8
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cboShowChange
          Items.Strings = (
            'All added'
            'Translated only')
        end
        object cboUnits: TComboBox
          Left = 72
          Top = 8
          Width = 225
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          OnChange = cboUnitsChange
          Items.Strings = (
            'All added'
            'Translated only')
        end
      end
    end
    object tsClassesProperties: TTabSheet
      Caption = 'Classes and Properties'
      object spltrCRLeft: TSplitter
        Left = 193
        Top = 0
        Width = 3
        Height = 405
        Cursor = crHSplit
      end
      object TreeViewComponents: TTreeView
        Left = 0
        Top = 0
        Width = 193
        Height = 405
        Align = alLeft
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnChange = TreeViewComponentsChange
      end
      object pnlPropertiesRightSide: TPanel
        Left = 196
        Top = 0
        Width = 542
        Height = 405
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlPropertiesRightSide'
        TabOrder = 0
        object spltrCRRight: TSplitter
          Left = 129
          Top = 0
          Width = 3
          Height = 405
          Cursor = crHSplit
        end
        object ListBoxProps: TListBox
          Left = 0
          Top = 0
          Width = 129
          Height = 405
          Align = alLeft
          ItemHeight = 13
          MultiSelect = True
          TabOrder = 2
          OnDblClick = ListBoxPropsDblClick
        end
        object pnlCPButtons: TPanel
          Left = 132
          Top = 0
          Width = 45
          Height = 405
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonAdd: TButton
            Left = 7
            Top = 67
            Width = 34
            Height = 25
            Caption = '->'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = ButtonAddClick
          end
          object ButtonRemove: TButton
            Left = 7
            Top = 109
            Width = 34
            Height = 25
            Caption = '<-'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = ButtonRemoveClick
          end
        end
        object pnlAddedPropertiesEditor: TPanel
          Left = 177
          Top = 0
          Width = 365
          Height = 405
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object tsCodeStrings: TTabSheet
      Caption = 'Code &Strings'
      PopupMenu = PopupMenuCodeStrings
      object pnlRight: TPanel
        Left = 639
        Top = 0
        Width = 99
        Height = 405
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object cmdAddString: TButton
          Left = 5
          Top = 8
          Width = 89
          Height = 25
          Caption = '&Add String'
          TabOrder = 0
          OnClick = cmdAddStringClick
        end
        object cmdDeleteString: TButton
          Left = 5
          Top = 70
          Width = 89
          Height = 25
          Caption = '&Delete String'
          TabOrder = 1
          OnClick = cmdDeleteStringClick
        end
        object pnlBottom: TPanel
          Left = 0
          Top = 364
          Width = 99
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
        end
        object cmdDuplicateString: TButton
          Left = 5
          Top = 39
          Width = 89
          Height = 25
          Caption = 'D&uplicate String'
          TabOrder = 3
          OnClick = cmdDuplicateStringClick
        end
      end
      object pnlCodeStrings: TPanel
        Left = 0
        Top = 0
        Width = 639
        Height = 405
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object Menu: TMainMenu
    Left = 696
    Top = 7
    object MenuLanguages: TMenuItem
      Caption = '&Languages'
      OnClick = MenuLanguagesClick
      object MenuLanguageAdd: TMenuItem
        Caption = '&Add Language...'
        OnClick = MenuLanguageAddClick
      end
      object menuLanguageRename: TMenuItem
        Caption = 'Re&name Language...'
        OnClick = menuLanguageRenameClick
      end
      object MenuLanguageRemove: TMenuItem
        Caption = '&Remove Language...'
        OnClick = MenuLanguageRemoveClick
      end
      object MenuLanguagesSeparator: TMenuItem
        Caption = '-'
      end
      object MenuClose: TMenuItem
        Caption = '&Close'
        OnClick = MenuCloseClick
      end
    end
    object menuEdit: TMenuItem
      Caption = '&Edit'
      object menuEditCut: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 16472
        OnClick = menuEditCutClick
      end
      object menuEditCopy: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = menuEditCopyClick
      end
      object menuEditPaste: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = menuEditPasteClick
      end
      object MenuEditSeparator1: TMenuItem
        Caption = '-'
      end
      object mnuExcelEdit: TMenuItem
        Caption = 'Edit in &Excel'
        OnClick = mnuExcelEditClick
      end
      object MenuEditSeparator2: TMenuItem
        Caption = '-'
      end
      object menuEditRemoveDeletedComps: TMenuItem
        Caption = 'Remove Translations for &Deleted Components'
        OnClick = menuEditRemoveDeletedCompsClick
      end
      object menuEditRemoveClientTranslations: TMenuItem
        Caption = 'Remove Translations for a Client &Form'
        OnClick = menuEditRemoveClientTranslationsClick
      end
    end
  end
  object PopupMenuCodeStrings: TPopupMenu
    Left = 584
    Top = 42
    object menuPopupCut: TMenuItem
      Caption = 'Cu&t'
      OnClick = menuEditCutClick
    end
    object menuPopupCopy: TMenuItem
      Caption = '&Copy'
      OnClick = menuEditCopyClick
    end
    object menuPopupPaste: TMenuItem
      Caption = '&Paste'
      OnClick = menuEditPasteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object menuPopupAddString: TMenuItem
      Caption = '&Add String'
      ShortCut = 16429
      OnClick = menuEditAddStringClick
    end
    object menuPopupDuplicateString: TMenuItem
      Caption = 'D&uplicate String'
      ShortCut = 16452
      OnClick = menuEditDuplicateStringClick
    end
    object menuPopupDeleteString: TMenuItem
      Caption = '&Delete String'
      ShortCut = 16430
      OnClick = menuEditDeleteStringClick
    end
  end
end
