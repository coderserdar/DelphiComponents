object FBCustomDataSetSQLEditor: TFBCustomDataSetSQLEditor
  Left = 298
  Top = 142
  Width = 788
  Height = 613
  Caption = 'FBCustomDataSetSQLEditor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  DesignSize = (
    780
    586)
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 613
    Top = 553
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 697
    Top = 553
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 780
    Height = 545
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 530
      Top = 1
      Width = 5
      Height = 543
      Align = alRight
    end
    object Panel1: TPanel
      Left = 535
      Top = 1
      Width = 244
      Height = 543
      Align = alRight
      FullRepaint = False
      TabOrder = 0
      object Splitter2: TSplitter
        Left = 1
        Top = 261
        Width = 242
        Height = 5
        Cursor = crVSplit
        Align = alTop
      end
      object ListBoxRelations: TListBox
        Left = 1
        Top = 81
        Width = 242
        Height = 180
        Align = alTop
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBoxRelationsClick
      end
      object ListBoxFields: TListBox
        Left = 1
        Top = 266
        Width = 242
        Height = 214
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 1
        OnClick = ListBoxFieldsClick
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 242
        Height = 80
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          242
          80)
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 51
          Height = 13
          Caption = 'Table alias'
        end
        object Edit1: TEdit
          Left = 64
          Top = 8
          Width = 169
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Replase SQL'
          TabOrder = 1
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 52
          Width = 49
          Height = 17
          Caption = 'Filter'
          TabOrder = 2
          OnClick = CheckBox2Click
        end
        object CheckBox3: TCheckBox
          Left = 144
          Top = 32
          Width = 89
          Height = 17
          Hint = 'Show field info'
          Caption = 'Show info'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = CheckBox3Click
        end
        object Edit2: TEdit
          Left = 64
          Top = 48
          Width = 177
          Height = 21
          Enabled = False
          TabOrder = 4
          OnKeyUp = Edit2KeyUp
        end
      end
      object Panel4: TPanel
        Left = 1
        Top = 480
        Width = 242
        Height = 62
        Align = alBottom
        BevelOuter = bvNone
        Color = 10930928
        TabOrder = 3
        object Label2: TLabel
          Left = 0
          Top = 5
          Width = 242
          Height = 13
          Align = alBottom
          Caption = 'Description'
        end
        object Memo1: TMemo
          Left = 0
          Top = 18
          Width = 242
          Height = 44
          Align = alBottom
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 529
      Height = 543
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PageControl1: TPageControl
        Left = 0
        Top = 0
        Width = 529
        Height = 524
        ActivePage = TabSheet2
        Align = alClient
        TabOrder = 0
        OnChange = PageControl1Change
        object TabSheet2: TTabSheet
          Caption = 'Select SQL'
          object edtSelect: TSynEdit
            Left = 0
            Top = 25
            Width = 521
            Height = 471
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Pitch = fpFixed
            Font.Style = []
            TabOrder = 0
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynSQLSyn1
            Lines.Strings = (
              'select'
              '  *'
              'from'
              '  viewer')
            SearchEngine = SynEditSearch1
            OnStatusChange = edtSelectStatusChange
            RemovedKeystrokes = <
              item
                Command = ecLineBreak
                ShortCut = 8205
              end
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <>
          end
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 521
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object SpeedButton1: TSpeedButton
              Left = 0
              Top = 0
              Width = 23
              Height = 22
              Flat = True
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00EEEEEEEEEEEE
                EEEEE00000EEEEE00000E0F000EEEEE0F000E0F000EEEEE0F000E0000000E000
                0000E00F000000F00000E00F000800F00000E00F000800F00000EE0000000000
                000EEEE0F000E0F000EEEEE00000E00000EEEEEE000EEE000EEEEEEE0F0EEE0F
                0EEEEEEE000EEE000EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
              OnClick = SpeedButton1Click
            end
            object SpeedButton2: TSpeedButton
              Left = 25
              Top = 0
              Width = 23
              Height = 22
              Flat = True
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                0400000000008000000000000000000000001000000010000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
                8888888888888888888888888888888888888444488844444488887488888744
                7888888448888744888888874888844788888888444444488888888874884478
                8888888884484488888888888744478888888888884448888888888888747888
                8888888888848888888888888888888888888888888888888888}
              OnClick = SpeedButton2Click
            end
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'Insert SQL'
          ImageIndex = 4
          object edtInsertSQL: TSynEdit
            Left = 0
            Top = 0
            Width = 521
            Height = 496
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Pitch = fpFixed
            Font.Style = []
            TabOrder = 0
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynSQLSyn1
            Lines.Strings = (
              'select'
              '  *'
              'from'
              '  viewer')
            OnStatusChange = edtSelectStatusChange
            RemovedKeystrokes = <
              item
                Command = ecLineBreak
                ShortCut = 8205
              end
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <>
          end
        end
        object TabSheet1: TTabSheet
          Caption = 'Edit SQL'
          object edtEditSql: TSynEdit
            Left = 0
            Top = 0
            Width = 521
            Height = 496
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Pitch = fpFixed
            Font.Style = []
            TabOrder = 0
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynSQLSyn1
            Lines.Strings = (
              'select'
              '  *'
              'from'
              '  viewer')
            OnStatusChange = edtSelectStatusChange
            RemovedKeystrokes = <
              item
                Command = ecLineBreak
                ShortCut = 8205
              end
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <>
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Delete SQL'
          ImageIndex = 2
          object edtDeleteSQL: TSynEdit
            Left = 0
            Top = 0
            Width = 521
            Height = 496
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Pitch = fpFixed
            Font.Style = []
            TabOrder = 0
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynSQLSyn1
            Lines.Strings = (
              'select'
              '  *'
              'from'
              '  viewer')
            OnStatusChange = edtSelectStatusChange
            RemovedKeystrokes = <
              item
                Command = ecLineBreak
                ShortCut = 8205
              end
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <>
          end
        end
        object TabSheet4: TTabSheet
          Caption = 'Refresh SQL'
          ImageIndex = 3
          object edtRefreshSQL: TSynEdit
            Left = 0
            Top = 0
            Width = 521
            Height = 496
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Courier New'
            Font.Pitch = fpFixed
            Font.Style = []
            TabOrder = 0
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -11
            Gutter.Font.Name = 'Terminal'
            Gutter.Font.Style = []
            Highlighter = SynSQLSyn1
            Lines.Strings = (
              'select'
              '  *'
              'from'
              '  viewer')
            OnStatusChange = edtSelectStatusChange
            RemovedKeystrokes = <
              item
                Command = ecLineBreak
                ShortCut = 8205
              end
              item
                Command = ecContextHelp
                ShortCut = 112
              end>
            AddedKeystrokes = <>
          end
        end
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 524
        Width = 529
        Height = 19
        Panels = <
          item
            Width = 100
          end
          item
            Width = 100
          end
          item
            Width = 50
          end>
      end
    end
  end
  object Button1: TButton
    Left = 8
    Top = 552
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Generate'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 552
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Check'
    Enabled = False
    TabOrder = 4
    OnClick = Button2Click
  end
  object btnTest: TButton
    Left = 168
    Top = 552
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Test'
    TabOrder = 5
    OnClick = btnTestClick
  end
  object JvUIBQuery1: TJvUIBQuery
    CachedFetch = False
    Left = 208
    Top = 80
  end
  object SynSQLSyn1: TSynSQLSyn
    DefaultFilter = 'Files SQL (*.sql)|*.sql'
    TableNameAttri.Background = clWindow
    TableNameAttri.Foreground = clGreen
    TableNameAttri.Style = [fsUnderline]
    TableNames.Strings = (
      'viewer')
    SQLDialect = sqlInterbase6
    Left = 240
    Top = 72
  end
  object SynCompletionProposal1: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoEndCharCompletion, scoCompleteWithEnter]
    ItemList.Strings = (
      'del'
      'vvv'
      'eaa')
    InsertList.Strings = (
      '11'
      '22'
      '33'
      '44')
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Title = 'Fields'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end
      item
        BiggestWord = 'CONSTRUCTOR'
      end>
    OnExecute = SynCompletionProposal1Execute
    ShortCut = 16416
    Left = 205
    Top = 113
  end
  object SynEditSearch1: TSynEditSearch
    Left = 245
    Top = 113
  end
  object FindDialog1: TFindDialog
    OnFind = FindDialog1Find
    Left = 245
    Top = 153
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 277
    Top = 153
  end
end
