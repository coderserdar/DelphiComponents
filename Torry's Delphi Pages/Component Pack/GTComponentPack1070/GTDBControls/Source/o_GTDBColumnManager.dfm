object gtFrmColumnManager: TgtFrmColumnManager
  Left = 553
  Top = 467
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 451
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 592
    Height = 410
    Align = alClient
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 254
      Top = 6
      Width = 5
      Height = 295
    end
    object ColumnsListView: TListView
      Left = 4
      Top = 6
      Width = 250
      Height = 295
      Align = alLeft
      Columns = <
        item
          Caption = 'FieldName'
          Width = 120
        end
        item
          Caption = 'Title'
          Width = 120
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ColumnsListViewClick
      OnSelectItem = ColumnsListViewSelectItem
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 301
      Width = 584
      Height = 105
      Align = alBottom
      Caption = 'Preview'
      TabOrder = 1
      object DBGridPreview: TDBGrid
        Left = 2
        Top = 15
        Width = 580
        Height = 88
        Align = alClient
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabControl3: TTabControl
      Left = 259
      Top = 6
      Width = 329
      Height = 295
      Align = alClient
      TabOrder = 2
      object Image1: TImage
        Left = 4
        Top = 216
        Width = 321
        Height = 75
        Align = alBottom
      end
      object lblFieldName: TLabel
        Left = 6
        Top = 16
        Width = 56
        Height = 13
        Alignment = taRightJustify
        Caption = 'FieldName :'
      end
      object lblTitleCaption: TLabel
        Left = 36
        Top = 40
        Width = 26
        Height = 13
        Alignment = taRightJustify
        Caption = 'Title :'
      end
      object lblWidth: TLabel
        Left = 28
        Top = 67
        Width = 34
        Height = 13
        Alignment = taRightJustify
        Caption = 'Width :'
      end
      object lblFont: TLabel
        Left = 35
        Top = 94
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = 'Font :'
      end
      object lblColor: TLabel
        Left = 32
        Top = 121
        Width = 30
        Height = 13
        Alignment = taRightJustify
        Caption = 'Color :'
      end
      object shpColumnColor: TShape
        Left = 68
        Top = 119
        Width = 213
        Height = 21
        Shape = stRoundRect
      end
      object cmbFieldNames: TComboBox
        Left = 67
        Top = 13
        Width = 238
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
      object edtTitleCaption: TEdit
        Left = 68
        Top = 38
        Width = 237
        Height = 21
        TabOrder = 1
      end
      object edtColumnWidth: TEdit
        Left = 68
        Top = 65
        Width = 197
        Height = 21
        TabOrder = 2
        Text = '0'
      end
      object UpDown1: TUpDown
        Left = 265
        Top = 65
        Width = 16
        Height = 21
        Associate = edtColumnWidth
        TabOrder = 3
      end
      object edtColumnFont: TEdit
        Left = 68
        Top = 92
        Width = 213
        Height = 21
        TabOrder = 4
      end
      object btnSetFont: TButton
        Left = 287
        Top = 92
        Width = 25
        Height = 21
        Caption = '...'
        TabOrder = 5
        OnClick = btnSetFontClick
      end
      object btnSetColor: TButton
        Left = 287
        Top = 119
        Width = 25
        Height = 21
        Caption = '...'
        TabOrder = 6
        OnClick = btnSetColorClick
      end
      object chkBoxVisible: TCheckBox
        Left = 32
        Top = 160
        Width = 105
        Height = 17
        Caption = 'Visible'
        TabOrder = 7
        OnClick = chkBoxVisibleClick
      end
      object chkReadOnly: TCheckBox
        Left = 192
        Top = 160
        Width = 105
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 8
        OnClick = chkReadOnlyClick
      end
    end
  end
  object TabControl2: TTabControl
    Left = 0
    Top = 410
    Width = 592
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOk: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 496
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
