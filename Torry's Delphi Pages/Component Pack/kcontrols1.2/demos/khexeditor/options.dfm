object OptionsForm: TOptionsForm
  Left = 367
  Top = 319
  BorderStyle = bsDialog
  Caption = 'Editor options'
  ClientHeight = 350
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PCMain: TPageControl
    Left = 8
    Top = 8
    Width = 425
    Height = 305
    ActivePage = TSEditorFont
    TabOrder = 0
    object TSEditorOptions: TTabSheet
      Caption = 'Editor Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LBAddressPrefix: TLabel
        Left = 212
        Top = 58
        Width = 74
        Height = 13
        Caption = 'Address prefix:'
        FocusControl = EDAddressPrefix
      end
      object LBAddressSize: TLabel
        Left = 212
        Top = 82
        Width = 64
        Height = 13
        Caption = 'Address size:'
        FocusControl = EDAddressSize
      end
      object LBCharSpacing: TLabel
        Left = 212
        Top = 106
        Width = 117
        Height = 13
        Caption = 'Inter-character spacing:'
        FocusControl = EDCharSpacing
      end
      object LBDigitGrouping: TLabel
        Left = 212
        Top = 154
        Width = 70
        Height = 13
        Caption = 'Digit grouping:'
        FocusControl = EDDigitGrouping
      end
      object LBLineSize: TLabel
        Left = 212
        Top = 130
        Width = 44
        Height = 13
        Caption = 'Line size:'
        FocusControl = EDLineSize
      end
      object LBbytes: TLabel
        Left = 381
        Top = 130
        Width = 27
        Height = 13
        Caption = 'bytes'
      end
      object LBpoints: TLabel
        Left = 381
        Top = 107
        Width = 29
        Height = 13
        Caption = 'points'
      end
      object LBbytes2: TLabel
        Left = 381
        Top = 154
        Width = 27
        Height = 13
        Caption = 'bytes'
      end
      object LBLineHeightPercent: TLabel
        Left = 212
        Top = 178
        Width = 114
        Height = 13
        Caption = 'Line height percentage:'
        FocusControl = EDLineHeightPercent
      end
      object LBpercent: TLabel
        Left = 381
        Top = 178
        Width = 11
        Height = 13
        Caption = '%'
      end
      object LBUndoLimit: TLabel
        Left = 212
        Top = 202
        Width = 50
        Height = 13
        Caption = 'Undo limit:'
        FocusControl = EDUndoLimit
      end
      object GBGeneral: TGroupBox
        Left = 4
        Top = 2
        Width = 201
        Height = 69
        Caption = 'General'
        TabOrder = 0
        object CBDropFiles: TCheckBox
          Left = 8
          Top = 16
          Width = 177
          Height = 17
          Caption = 'Drop files'
          TabOrder = 0
        end
        object CBUndoAfterSave: TCheckBox
          Left = 8
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Undo after save'
          TabOrder = 2
        end
        object CBGroupUndo: TCheckBox
          Left = 8
          Top = 32
          Width = 177
          Height = 17
          Caption = 'Group undo'
          TabOrder = 1
        end
      end
      object RGAddressMode: TRadioGroup
        Left = 212
        Top = 6
        Width = 201
        Height = 45
        Caption = 'Address mode'
        Columns = 2
        Items.Strings = (
          'Decimal'
          'Hexadecimal')
        TabOrder = 3
      end
      object RGDisabledDrawStyle: TRadioGroup
        Left = 4
        Top = 207
        Width = 201
        Height = 67
        Caption = 'Draw style when disabled'
        Items.Strings = (
          'Brighter colors'
          'Gray and white'
          'Normal colors')
        TabOrder = 2
      end
      object GBAppearance: TGroupBox
        Left = 4
        Top = 72
        Width = 201
        Height = 133
        Caption = 'Appearance'
        TabOrder = 1
        object CBShowAddress: TCheckBox
          Left = 8
          Top = 16
          Width = 177
          Height = 17
          Caption = 'Show address area'
          TabOrder = 0
        end
        object CBShowDigits: TCheckBox
          Left = 8
          Top = 32
          Width = 177
          Height = 17
          Caption = 'Show digit area'
          TabOrder = 1
        end
        object CBShowText: TCheckBox
          Left = 8
          Top = 48
          Width = 177
          Height = 17
          Caption = 'Show text area'
          TabOrder = 2
        end
        object CBShowHorzLines: TCheckBox
          Left = 8
          Top = 64
          Width = 177
          Height = 17
          Caption = 'Show horizontal lines'
          TabOrder = 3
        end
        object CBShowVertLines: TCheckBox
          Left = 8
          Top = 80
          Width = 177
          Height = 17
          Caption = 'Show vertical lines'
          TabOrder = 4
        end
        object CBShowSeparators: TCheckBox
          Left = 8
          Top = 96
          Width = 177
          Height = 17
          Caption = 'Show area separators'
          TabOrder = 5
        end
        object CBShowInactiveCaret: TCheckBox
          Left = 8
          Top = 112
          Width = 177
          Height = 17
          Caption = 'Show inactive caret'
          TabOrder = 6
        end
      end
      object EDAddressPrefix: TEdit
        Left = 328
        Top = 55
        Width = 49
        Height = 21
        TabOrder = 4
      end
      object EDAddressSize: TEdit
        Left = 344
        Top = 79
        Width = 33
        Height = 21
        TabOrder = 5
      end
      object EDCharSpacing: TEdit
        Left = 344
        Top = 103
        Width = 33
        Height = 21
        TabOrder = 6
      end
      object EDDigitGrouping: TEdit
        Left = 344
        Top = 151
        Width = 33
        Height = 21
        TabOrder = 7
      end
      object EDLineSize: TEdit
        Left = 344
        Top = 127
        Width = 33
        Height = 21
        TabOrder = 8
      end
      object EDLineHeightPercent: TEdit
        Left = 344
        Top = 175
        Width = 33
        Height = 21
        TabOrder = 9
      end
      object EDUndoLimit: TEdit
        Left = 328
        Top = 199
        Width = 49
        Height = 21
        TabOrder = 10
      end
    end
    object TSEditorFont: TTabSheet
      Caption = 'Editor Font'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LBFontName: TLabel
        Left = 16
        Top = 16
        Width = 55
        Height = 13
        Caption = '&Font name:'
        FocusControl = CoBFontName
      end
      object LBSample: TLabel
        Left = 16
        Top = 120
        Width = 38
        Height = 13
        Caption = 'Sample:'
        FocusControl = PNFontSample
      end
      object LBFontSize: TLabel
        Left = 315
        Top = 34
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Font size:'
        FocusControl = EDFontSize
      end
      object CoBFontName: TComboBox
        Left = 16
        Top = 32
        Width = 201
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnSelect = CoBFontNameClick
      end
      object PNFontSample: TPanel
        Left = 16
        Top = 136
        Width = 385
        Height = 105
        BevelOuter = bvLowered
        Caption = 'AaBbCcXxYyZz'
        TabOrder = 1
      end
      object EDFontSize: TEdit
        Left = 368
        Top = 31
        Width = 33
        Height = 21
        TabOrder = 2
        OnExit = EDFontSizeExit
      end
      object CBFontStyleBold: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Bold'
        TabOrder = 3
        OnClick = CoBFontNameClick
      end
      object CBFontStyleItalic: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Italic'
        TabOrder = 4
        OnClick = CoBFontNameClick
      end
    end
    object TSColors: TTabSheet
      Caption = 'Colors'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LBAttributes: TLabel
        Left = 8
        Top = 8
        Width = 52
        Height = 13
        Caption = 'Color item:'
        FocusControl = LiBColors
      end
      object LiBColors: TListBox
        Left = 8
        Top = 24
        Width = 209
        Height = 233
        Style = lbOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 0
        OnClick = LiBColorsClick
        OnDrawItem = LiBColorsDrawItem
        OnMouseMove = LiBColorsMouseMove
      end
      object GBColors: TGroupBox
        Left = 224
        Top = 19
        Width = 185
        Height = 78
        Caption = 'Color assignment'
        TabOrder = 1
        object LBHighFG: TLabel
          Left = 10
          Top = 18
          Width = 67
          Height = 13
          Caption = 'Current color:'
        end
        object SHColor: TShape
          Left = 112
          Top = 16
          Width = 65
          Height = 49
        end
        object BUColorChange: TButton
          Left = 8
          Top = 40
          Width = 75
          Height = 25
          Caption = 'Change...'
          TabOrder = 0
          OnClick = BUColorChangeClick
        end
      end
    end
  end
  object BUOk: TButton
    Left = 275
    Top = 320
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BuCancel: TButton
    Left = 358
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object BUDefault: TButton
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 1
    OnClick = BUDefaultClick
  end
  object CDChange: TColorDialog
    Left = 404
  end
end
