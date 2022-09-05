object FormOCRLearn: TFormOCRLearn
  Left = 274
  Top = 119
  BorderStyle = bsDialog
  Caption = 'OCR Learn'
  ClientHeight = 378
  ClientWidth = 392
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 312
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object btnOpen: TButton
    Left = 312
    Top = 72
    Width = 75
    Height = 25
    Caption = '&Open'
    TabOrder = 3
    OnClick = btnOpenClick
  end
  object btnSave: TButton
    Left = 312
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Save'
    Enabled = False
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object btnNew: TButton
    Left = 312
    Top = 40
    Width = 75
    Height = 25
    Caption = '&New'
    TabOrder = 2
    OnClick = btnNewClick
  end
  object btnRead: TButton
    Left = 312
    Top = 136
    Width = 75
    Height = 25
    Caption = '&Read'
    Enabled = False
    TabOrder = 5
    OnClick = btnReadClick
  end
  object pcOCR: TPageControl
    Left = 0
    Top = 0
    Width = 305
    Height = 378
    ActivePage = tsTemplates
    Align = alLeft
    TabOrder = 0
    object tsParameters: TTabSheet
      Caption = 'Parameters'
      object gbGlyphSize: TGroupBox
        Left = 4
        Top = 0
        Width = 285
        Height = 57
        Caption = 'Glyph size'
        TabOrder = 0
        object lGlyphWidth: TLabel
          Left = 16
          Top = 29
          Width = 31
          Height = 13
          Caption = '&Width:'
          FocusControl = seGlyphWidth
        end
        object lGlyphHeight: TLabel
          Left = 152
          Top = 29
          Width = 34
          Height = 13
          Caption = '&Height:'
          FocusControl = seGlyphHeight
        end
        object seGlyphWidth: TSpinEdit
          Left = 72
          Top = 24
          Width = 57
          Height = 22
          MaxValue = 256
          MinValue = 2
          TabOrder = 0
          Value = 5
          OnChange = seGlyphWidthChange
        end
        object seGlyphHeight: TSpinEdit
          Left = 208
          Top = 24
          Width = 57
          Height = 22
          MaxValue = 256
          MinValue = 2
          TabOrder = 1
          Value = 7
          OnChange = seGlyphHeightChange
        end
      end
      object gbGlyphColors: TGroupBox
        Left = 4
        Top = 60
        Width = 285
        Height = 125
        Caption = 'Color mode'
        TabOrder = 1
        object lGlyphColor: TLabel
          Left = 16
          Top = 28
          Width = 30
          Height = 13
          Caption = '&Color: '
          FocusControl = cbGlyphColor
        end
        object lThreshold: TLabel
          Left = 16
          Top = 61
          Width = 50
          Height = 13
          Caption = '&Threshold:'
          FocusControl = cbThreshold
        end
        object lLevel: TLabel
          Left = 16
          Top = 96
          Width = 26
          Height = 13
          Caption = '&Level'
          FocusControl = seThreshold
        end
        object cbGlyphColor: TComboBox
          Left = 72
          Top = 24
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Black on White'
            'White on Black')
          TabOrder = 0
          OnChange = cbGlyphColorChange
        end
        object seThreshold: TSpinEdit
          Left = 72
          Top = 88
          Width = 57
          Height = 22
          MaxValue = 255
          MinValue = 1
          TabOrder = 2
          Value = 128
          OnChange = seThresholdChange
        end
        object cbThreshold: TComboBox
          Left = 72
          Top = 56
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'Intensity Level'
            'ISO Data'
            'Background Symmetry'
            'Edge'
            'Optimized'
            'Statistic')
          TabOrder = 1
          OnChange = cbThresholdChange
        end
      end
      object gbGlyphSeg: TGroupBox
        Left = 4
        Top = 188
        Width = 285
        Height = 157
        Caption = 'Segmentation'
        TabOrder = 2
        object lMinWidth: TLabel
          Left = 16
          Top = 61
          Width = 48
          Height = 13
          Caption = 'Min width:'
        end
        object lMaxWidth: TLabel
          Left = 16
          Top = 101
          Width = 51
          Height = 13
          Caption = 'Max width:'
        end
        object lMinHeight: TLabel
          Left = 144
          Top = 61
          Width = 52
          Height = 13
          Caption = 'Min height:'
        end
        object lMaxHeight: TLabel
          Left = 144
          Top = 101
          Width = 55
          Height = 13
          Caption = 'Max height:'
        end
        object lMethod: TLabel
          Left = 16
          Top = 28
          Width = 39
          Height = 13
          Caption = 'Method:'
          FocusControl = cbMethod
        end
        object cbCutLarge: TCheckBox
          Left = 16
          Top = 128
          Width = 97
          Height = 17
          Caption = 'C&ut large glyphs'
          State = cbChecked
          TabOrder = 5
          OnClick = cbCutLargeClick
        end
        object seMinWidth: TSpinEdit
          Left = 72
          Top = 56
          Width = 57
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = seMinWidthChange
        end
        object seMaxWidth: TSpinEdit
          Left = 72
          Top = 96
          Width = 57
          Height = 22
          Enabled = False
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = seMaxWidthChange
        end
        object seMinHeight: TSpinEdit
          Left = 208
          Top = 56
          Width = 57
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = seMinHeightChange
        end
        object seMaxHeight: TSpinEdit
          Left = 208
          Top = 96
          Width = 57
          Height = 22
          Enabled = False
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = seMaxHeightChange
        end
        object cbAspectRatio: TCheckBox
          Left = 144
          Top = 128
          Width = 97
          Height = 17
          Caption = 'Aspect ratio'
          State = cbChecked
          TabOrder = 6
          OnClick = cbAspectRatioClick
        end
        object cbMethod: TComboBox
          Left = 72
          Top = 24
          Width = 193
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'All pixel match'
            'Feature pixel match')
          TabOrder = 0
          OnChange = cbMethodChange
        end
      end
    end
    object tsLearn: TTabSheet
      Caption = 'Learn'
      object gbGlyph: TGroupBox
        Left = 4
        Top = 0
        Width = 285
        Height = 345
        TabOrder = 0
        object icActual: TmcmImageCtrl
          Left = 16
          Top = 32
          Width = 177
          Height = 73
          BorderStyle = BS_SUNKEN
          Center = True
          Color = clWhite
          ParentColor = False
          Scale = 1.000000000000000000
          ScaleToFit = True
        end
        object lGlyph: TLabel
          Left = 16
          Top = 172
          Width = 40
          Height = 13
          Caption = '&Glyph is:'
          FocusControl = eGlyph
        end
        object lGlyphGroup: TLabel
          Left = 16
          Top = 200
          Width = 60
          Height = 13
          Caption = 'Glyph gro&up:'
          FocusControl = slbGlyphCase
        end
        object lSeparate: TLabel
          Left = 136
          Top = 172
          Width = 58
          Height = 13
          Caption = 'Separate &at:'
          FocusControl = cbSeparators
        end
        object icResized: TmcmImageCtrl
          Left = 200
          Top = 32
          Width = 73
          Height = 73
          BorderStyle = BS_SUNKEN
          Center = True
          Color = clWhite
          ParentColor = False
          Scale = 1.000000000000000000
          ScaleToFit = True
        end
        object lActual: TLabel
          Left = 16
          Top = 16
          Width = 33
          Height = 13
          Caption = 'Actual:'
        end
        object lResized: TLabel
          Left = 200
          Top = 16
          Width = 47
          Height = 13
          Caption = 'Template:'
        end
        object lMatchError: TLabel
          Left = 136
          Top = 144
          Width = 58
          Height = 13
          Caption = 'Match Error:'
        end
        object lMatchErrorVal: TLabel
          Left = 200
          Top = 144
          Width = 26
          Height = 13
          Caption = 'None'
        end
        object lGuess: TLabel
          Left = 152
          Top = 120
          Width = 43
          Height = 13
          Caption = 'Estimate:'
        end
        object lGuessVal: TLabel
          Left = 200
          Top = 120
          Width = 29
          Height = 13
          Caption = 'Empty'
        end
        object lLHeightWidth: TLabel
          Left = 16
          Top = 120
          Width = 73
          Height = 13
          Caption = 'Width x Height:'
        end
        object lLWidthVal: TLabel
          Left = 104
          Top = 120
          Width = 6
          Height = 13
          Caption = '0'
        end
        object eGlyph: TEdit
          Left = 72
          Top = 168
          Width = 41
          Height = 21
          AutoSize = False
          MaxLength = 1
          TabOrder = 0
          OnChange = eGlyphChange
        end
        object btnNext: TButton
          Left = 95
          Top = 304
          Width = 75
          Height = 25
          Caption = 'Ne&xt'
          Enabled = False
          TabOrder = 1
          OnClick = btnNextClick
        end
        object slbGlyphCase: TCheckListBox
          Left = 16
          Top = 216
          Width = 257
          Height = 81
          OnClickCheck = slbGlyphCaseClickCheck
          ItemHeight = 13
          Items.Strings = (
            'All'
            'Digits'
            'Upper case'
            'Lower case'
            'Special case'
            '1 case'
            '2 case'
            '3 case'
            '4 case'
            '5 case'
            '6 case'
            '7 case'
            '8 case'
            '9 case')
          TabOrder = 2
        end
        object btnAdd: TButton
          Left = 200
          Top = 304
          Width = 75
          Height = 25
          Caption = '&Add'
          Enabled = False
          TabOrder = 3
          OnClick = btnAddClick
        end
        object cbSeparators: TComboBox
          Left = 200
          Top = 168
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 4
          OnChange = cbSeparatorsChange
        end
        object btnStart: TButton
          Left = 16
          Top = 304
          Width = 75
          Height = 25
          Caption = 'Start'
          TabOrder = 5
          OnClick = btnStartClick
        end
      end
    end
    object tsAutoLearn: TTabSheet
      Caption = 'Auto Learn'
      TabVisible = False
      object gbFonts: TGroupBox
        Left = 4
        Top = 0
        Width = 285
        Height = 345
        TabOrder = 0
        OnExit = gbFontsExit
        object ImageCtrlFonts: TmcmImageCtrl
          Left = 16
          Top = 232
          Width = 257
          Height = 73
          BorderStyle = BS_SINGLE
          Center = True
          Color = clWhite
          ParentColor = False
          Scale = 1.000000000000000000
          ScaleToFit = False
        end
        object lFonts: TLabel
          Left = 16
          Top = 16
          Width = 29
          Height = 13
          Caption = '&Fonts:'
          FocusControl = clbFonts
        end
        object lLearningFont: TLabel
          Left = 16
          Top = 216
          Width = 65
          Height = 13
          Caption = 'Learning font:'
        end
        object lLearningFontVal: TLabel
          Left = 88
          Top = 216
          Width = 185
          Height = 13
          AutoSize = False
          Caption = 'This font'
        end
        object clbFonts: TCheckListBox
          Left = 16
          Top = 32
          Width = 257
          Height = 73
          ItemHeight = 13
          TabOrder = 0
        end
        object btnCheckAll: TButton
          Left = 120
          Top = 112
          Width = 75
          Height = 25
          Caption = 'C&heck All'
          TabOrder = 1
          OnClick = btnCheckAllClick
        end
        object btnUncheckAll: TButton
          Left = 200
          Top = 112
          Width = 75
          Height = 25
          Caption = '&Uncheck All'
          TabOrder = 2
          OnClick = btnUncheckAllClick
        end
        object btnAutoLearn: TButton
          Left = 16
          Top = 312
          Width = 75
          Height = 25
          Caption = '&Learn'
          TabOrder = 3
          OnClick = btnAutoLearnClick
        end
        object cbNormal: TCheckBox
          Left = 16
          Top = 112
          Width = 73
          Height = 17
          Caption = 'Normal'
          State = cbChecked
          TabOrder = 4
          OnClick = cbFontStyleClick
        end
        object cbBold: TCheckBox
          Left = 16
          Top = 136
          Width = 73
          Height = 17
          Caption = 'Bold'
          TabOrder = 5
        end
        object cbItalic: TCheckBox
          Left = 16
          Top = 160
          Width = 73
          Height = 17
          Caption = 'Italic'
          TabOrder = 6
        end
      end
    end
    object tsTemplates: TTabSheet
      Caption = 'Templates'
      object gbTemplates: TGroupBox
        Left = 4
        Top = 0
        Width = 285
        Height = 345
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object icTemplate: TmcmImageCtrl
          Left = 16
          Top = 32
          Width = 121
          Height = 73
          BorderStyle = BS_SUNKEN
          Center = True
          Color = clWhite
          ParentColor = False
          Scale = 1.000000000000000000
          ScaleToFit = True
        end
        object lGlyphTemplate: TLabel
          Left = 152
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Glyph:'
        end
        object sbFirst: TSpeedButton
          Left = 16
          Top = 312
          Width = 23
          Height = 22
          Hint = 'Display first glyph'
          Flat = True
          Glyph.Data = {
            3E040000424D3E04000000000000760000002800000058000000160000000100
            040000000000C803000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333337333333333333333333333733333333333333333333733333
            3333333333333333F33333333333333033333307333333333333303333330733
            3333333333033333307333333333333373333337F33333333333333033333007
            3333333333333033333007333333333333033333007333333333333373333377
            F333333333333330333300073333333333333033330007333333333333033330
            007333333333333373333777F333333333333330333000073333333333333033
            300007333333333333033300007333333333333373337777F333333333333330
            3300000733333333333330330000073333333333330330000073333333333333
            73377777F3333333333333303000000733333333333330300000073333333333
            33030000007333333333333373777777F3333333333333303300000733333333
            33333033000007333333333333033000007333333333333373377777F3333333
            3333333033300007333333333333303330000733333333333303330000733333
            3333333373337777F33333333333333033330007333333333333303333000733
            3333333333033330007333333333333373333777F33333333333333033333007
            3333333333333033333007333333333333033333007333333333333373333377
            F333333333333330333333033333333333333033333303333333333333033333
            3033333333333333733333373333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333}
          NumGlyphs = 4
          ParentShowHint = False
          ShowHint = True
          OnClick = sbFirstClick
        end
        object sbPrevious: TSpeedButton
          Left = 40
          Top = 312
          Width = 23
          Height = 22
          Hint = 'Display previous glyph'
          Flat = True
          Glyph.Data = {
            3E040000424D3E04000000000000760000002800000058000000160000000100
            040000000000C803000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333337333333333333333333333733333333333333333333733333
            3333333333333333F33333333333333333333307333333333333333333330733
            3333333333333333307333333333333333333337F33333333333333333333007
            3333333333333333333007333333333333333333007333333333333333333377
            F333333333333333333300073333333333333333330007333333333333333330
            007333333333333333333777F333333333333333333000073333333333333333
            300007333333333333333300007333333333333333337777F333333333333333
            3300000733333333333333330000073333333333333330000073333333333333
            33377777F3333333333333333000000733333333333333300000073333333333
            33330000007333333333333333777777F3333333333333333300000733333333
            33333333000007333333333333333000007333333333333333377777F3333333
            3333333333300007333333333333333330000733333333333333330000733333
            3333333333337777F33333333333333333330007333333333333333333000733
            3333333333333330007333333333333333333777F33333333333333333333007
            3333333333333333333007333333333333333333007333333333333333333377
            F333333333333333333333033333333333333333333303333333333333333333
            3033333333333333333333373333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333}
          NumGlyphs = 4
          ParentShowHint = False
          ShowHint = True
          OnClick = sbPreviousClick
        end
        object sbNext: TSpeedButton
          Left = 64
          Top = 312
          Width = 23
          Height = 22
          Hint = 'Display next glyph'
          Flat = True
          Glyph.Data = {
            3E040000424D3E04000000000000760000002800000058000000160000000100
            040000000000C803000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333733333333333333333333373333333333333333333373333333333
            33333333333F3333333333333333333330373333333333333333333037333333
            333333333333037333333333333333333373F333333333333333333330037333
            3333333333333330037333333333333333330037333333333333333333773F33
            3333333333333333300037333333333333333330003733333333333333330003
            7333333333333333337773F33333333333333333300003733333333333333330
            00037333333333333333000037333333333333333377773F3333333333333333
            3000003733333333333333300000373333333333333300000373333333333333
            33777773F3333333333333333000000333333333333333300000033333333333
            3333000000333333333333333377777733333333333333333000003333333333
            3333333000003333333333333333000003333333333333333377777333333333
            3333333330000333333333333333333000033333333333333333000033333333
            3333333333777733333333333333333330003333333333333333333000333333
            3333333333330003333333333333333333777333333333333333333330033333
            3333333333333330033333333333333333330033333333333333333333773333
            3333333333333333303333333333333333333330333333333333333333330333
            3333333333333333337333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333}
          NumGlyphs = 4
          ParentShowHint = False
          ShowHint = True
          OnClick = sbNextClick
        end
        object sbLast: TSpeedButton
          Left = 88
          Top = 312
          Width = 23
          Height = 22
          Hint = 'Show last glyph'
          Flat = True
          Glyph.Data = {
            3E040000424D3E04000000000000760000002800000058000000160000000100
            040000000000C803000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333337333333333333333333333733333333333333333333733333333333
            3333333333F33333333333333333333303733330333333333333330373333033
            33333333333037333303333333333333373F3333733333333333333300373330
            333333333333330037333033333333333330037333033333333333333773F333
            7333333333333333000373303333333333333300037330333333333333300037
            330333333333333337773F337333333333333333000037303333333333333300
            0037303333333333333000037303333333333333377773F37333333333333333
            0000037033333333333333000003703333333333333000003703333333333333
            3777773F73333333333333330000003033333333333333000000303333333333
            3330000003033333333333333777777373333333333333330000033033333333
            3333330000033033333333333330000033033333333333333777773373333333
            3333333300003330333333333333330000333033333333333330000333033333
            3333333337777333733333333333333300033330333333333333330003333033
            3333333333300033330333333333333337773333733333333333333300333330
            3333333333333300333330333333333333300333330333333333333337733333
            7333333333333333033333303333333333333303333330333333333333303333
            3303333333333333373333337333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333}
          NumGlyphs = 4
          ParentShowHint = False
          ShowHint = True
          OnClick = sbLastClick
        end
        object sbDelete: TSpeedButton
          Left = 120
          Top = 312
          Width = 23
          Height = 22
          Hint = 'Delete glyph'
          Flat = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            71F777777777777778F77771F777777777777778F7777777777777111F777777
            1F7777888F7777778F7777111F777771F77777888F777778F777777111F77711
            F777777888F77788F7777777111F711F77777777888F788F77777777711111F7
            77777777788888F77777777777111F777777777777888F7777777777711111F7
            77777777788888F777777777111F71F777777777888F78F77777771111F77711
            F777778888F77788F77771111F7777711F7778888F7777788F77711F77777777
            11F7788F7777777788F777777777777777777777777777777777}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = sbDeleteClick
        end
        object lGlyphIndex: TLabel
          Left = 16
          Top = 288
          Width = 29
          Height = 13
          Caption = 'Index:'
        end
        object lGlyphIndexVal: TLabel
          Left = 48
          Top = 288
          Width = 57
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '0'
        end
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 47
          Height = 13
          Caption = 'Template:'
        end
        object Label2: TLabel
          Left = 16
          Top = 112
          Width = 60
          Height = 13
          Caption = 'Glyph gro&up:'
          FocusControl = clbGlyphGroup
        end
        object eGlypgTemplate: TEdit
          Left = 152
          Top = 32
          Width = 121
          Height = 73
          AutoSize = False
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -48
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
        object clbGlyphGroup: TCheckListBox
          Left = 16
          Top = 128
          Width = 257
          Height = 81
          OnClickCheck = clbGlyphGroupClickCheck
          ItemHeight = 13
          Items.Strings = (
            'All'
            'Digits'
            'Upper case'
            'Lower case'
            'Special case'
            '1 case'
            '2 case'
            '3 case'
            '4 case'
            '5 case'
            '6 case'
            '7 case'
            '8 case'
            '9 case')
          TabOrder = 1
        end
      end
    end
    object tsResult: TTabSheet
      Caption = 'Result'
      object sgResults: TStringGrid
        Left = 0
        Top = 25
        Width = 297
        Height = 325
        Align = alClient
        ColCount = 6
        DefaultColWidth = 44
        DefaultRowHeight = 18
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
        TabOrder = 0
        OnSelectCell = sgResultsSelectCell
        ColWidths = (
          44
          44
          44
          44
          44
          44)
      end
      object pReadError: TPanel
        Left = 0
        Top = 0
        Width = 297
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lSumError: TLabel
          Left = 8
          Top = 6
          Width = 48
          Height = 13
          Caption = 'Max Error:'
        end
        object lSumErrorVal: TLabel
          Left = 64
          Top = 6
          Width = 3
          Height = 13
          Caption = '-'
        end
      end
    end
  end
  object mcmOCR: TmcmOCR
    GlyphMinHeight = 6
    OnDuplicateGlyph = mcmOCRDuplicateGlyph
    Left = 336
    Top = 176
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ocr'
    Filter = 'OCR|*.ocr'
    Title = 'Open OCR patterns'
    Left = 336
    Top = 232
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'ocr'
    Filter = 'OCR|*.ocr'
    Title = 'Save OCR patterns'
    Left = 336
    Top = 264
  end
  object ImageList: TImageList
    Left = 336
    Top = 296
    Bitmap = {
      3610000004000000424D36100000000000003600000028000000400000001000
      0000010020000000000000100000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F4F7F7009CB3B300E0E8E8000000
      0000CCDADA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      BF000000BF000000BF000000BF000000BF000000BF0000000000000000000000
      0000000000000000000000000000F4F7F700738F8F001D494900406363005A81
      810039696900FBFCFC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      BF000000BF000000000000000000000000000000BF0000000000000000000000
      000000000000F1F5F5006F8B8B003F77770079C8C8003F6D6D000C52520000DF
      DF0000F3F300224F4F0093A9A900DDE6E6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000BF000000BF000000
      BF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000BF000000BF0000000000000000000000000000000000000000000000
      0000000000006F8B8B003F5E5E0075A9A9003953530019B7B70019FFFF0019FF
      FF0019FFFF001383830009171700375252008AA9A90000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00008484000000
      0000000000000000000000000000008484000084840000000000000000000000
      000000000000000000000000000000000000FFFFFF000000BF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000BF000000BF00000000000000000000000000000000000000
      0000F4F7F700294141003D5C5C00375F5F0066D7D70075FFFF0075FFFF0066E8
      E8006BF0F00075FFFF004E9B9B004E6D6D00344B4B009CB3B300AEC4C4000000
      000000000000000000000000000000000000000000000000000000FFFF000084
      840000848400000000000084840000FFFF000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000BF000000BF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000BF000000BF00000000000000000000000000F4F7
      F700718B8B00405F5F0076BFBF009BFFFF009BFFFF008EF2F20071C2C20084E4
      E4007EC9C9009BFFFF009BFFFF006797970074989800142F2F00305858000000
      000000000000000000000000000000000000000000000084840000FFFF00FFFF
      FF000000000000000000FFFFFF0000FFFF000084840000000000000000000000
      000000000000000000000000000000000000FFFFFF000000BF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      BF000000000000000000000000000000BF000000BF000000000000000000A3A3
      A3009FA7A700D2FFFF00D2FFFF00AEDADA0097CBCB00A6D5D500A1CDCD00ACDF
      DF00A5CECE0088BCBC00C8FAFA00D5FFFF00D6FAFA00335858006F9696000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF000000BF000000BF000000
      BF00FFFFFF000000000000000000BF000000BF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      BF000000BF000000BF000000BF000000BF000000BF0000000000000000000000
      0000AFAFAF00EBFFFF00E8FFFF00E4FFFF00A6D1D100B0C9C900CEE5E50080B5
      B500C0E4E400DDFAFA00D8F8F800E4FFFF00E4FFFF00819B9B00C3C3C3000000
      000000000000FFFFFF008484840000000000000000000000000000FFFF000084
      8400848484000000000000FFFF00008484000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000BF000000BF00000000000000000000000000
      000000000000BF000000BF000000BF00000000000000BF000000BF000000BF00
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AFAFAF00EBFFFF00E8FFFF00E8FFFF00A2CECE00CEEBEB00E8FF
      FF00BEEBEB00E4FFFF00E4FFFF00E8FFFF00ABB7B70087878700000000000000
      00000000000000000000C6C6C600848484008484840000000000008484000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000FFFFFF000000BF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BF000000000000000000000000000000BF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AFAFAF00AFAFAF00F7FFFF00F3FFFF00C6DFDF00DEF3F300F3FF
      FF00F3FFFF00F1F6F600C6DEDE00585F5F00A7A7A70000000000000000000000
      000000000000C6C6C600FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000BF00FFFFFF000000BF00FFFFFF0000000000000000000000
      0000000000000000000000000000BF000000BF00000000000000000000000000
      00000000000000000000BF000000BF000000BF000000BF000000BF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AFAFAF00AFAFAF00F3FFFF00EBFFFF00ABBE
      BE004E4F4F00E7E7E70000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000BF000000BF000000BF00FFFFFF0000000000000000000000
      0000000000000000000000000000BF000000BF000000BF000000000000000000
      0000000000000000000000000000BF00000000000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AFAFAF00D7D7D700545F5F009F9F
      9F00ABABAB000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000BF00FFFFFF000000BF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000BF000000BF0000000000
      0000000000000000000000000000BF000000BF000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000087878700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400C6C6C60000000000FFFFFF000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000BF000000BF000000BF00FFFFFF0000000000000000000000
      000000000000BF000000BF0000000000000000000000BF000000BF0000000000
      000000000000000000000000000000000000BF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      000000000000BF000000BF000000BF000000BF000000BF000000BF0000000000
      000000000000000000000000000000000000BF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BF000000BF000000BF000000BF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000424D
      BE000000000000003E0000002800000040000000100000000100010000000000
      800000000000000000000000020000000200000000000000FFFFFF00FFFFFFFF
      FFFFF8BFFFFFF01FFF03F01FFF9FF01FFF3BC007FC93F01FFF9FC003FC03F01F
      FFCF8000FC03F01FFFE70000FC03F01FFF730000C8030013FF038000C0030013
      C47FC001C003001FEEFFC003C03F01F3E0FFF01FC03F01F1F5FFF83FC03F01FC
      F1FFFDFFC03F01CCFBFFFFFFFFFF01C0FBFFFFFFFFFF01E1FFFFFFFF}
  end
end
