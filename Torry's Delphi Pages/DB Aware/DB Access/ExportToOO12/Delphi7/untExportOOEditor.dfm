object frmExportOOEditor: TfrmExportOOEditor
  Left = 220
  Top = 170
  ActiveControl = FieldList
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'ExportOOEditor Component Editor'
  ClientHeight = 592
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Palatino Linotype'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 22
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 329
    Height = 569
    Caption = ' [ DataSet Info ] '
    TabOrder = 0
    object edtDataSet: TEdit
      Left = 16
      Top = 48
      Width = 297
      Height = 30
      Enabled = False
      TabOrder = 0
      Text = 'edtDataSet'
    end
    object FieldList: TListBox
      Left = 16
      Top = 104
      Width = 297
      Height = 441
      ItemHeight = 22
      TabOrder = 1
      OnClick = FieldListClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 360
    Top = 16
    Width = 409
    Height = 505
    Caption = ' [ Field Info ] '
    TabOrder = 1
    object cbVisible: TCheckBox
      Left = 72
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Visible'
      TabOrder = 0
      OnClick = cbVisibleClick
    end
    object P: TPageControl
      Left = 16
      Top = 72
      Width = 369
      Height = 409
      ActivePage = ts_Header
      TabOrder = 1
      OnChange = PChange
      object ts_Header: TTabSheet
        Caption = 'Header'
        object StaticText1: TStaticText
          Left = 44
          Top = 14
          Width = 273
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Header text'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        object edtHeader: TEdit
          Left = 44
          Top = 40
          Width = 273
          Height = 30
          TabOrder = 1
          Text = 'edtHeader'
          OnChange = edtHeaderChange
        end
        object BitBtn1: TBitBtn
          Left = 124
          Top = 96
          Width = 113
          Height = 33
          Caption = 'Select font'
          TabOrder = 2
          OnClick = BitBtn1Click
        end
        object gbFontStyle: TGroupBox
          Left = 20
          Top = 200
          Width = 321
          Height = 73
          Caption = ' [ Font style ] '
          Enabled = False
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          object cbHBold: TCheckBox
            Left = 16
            Top = 32
            Width = 97
            Height = 17
            Caption = 'Bold'
            TabOrder = 0
          end
          object cbHItalic: TCheckBox
            Left = 184
            Top = 32
            Width = 97
            Height = 17
            Caption = 'Italic'
            TabOrder = 1
          end
        end
        object edtHFontName: TEdit
          Left = 8
          Top = 160
          Width = 260
          Height = 28
          Enabled = False
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Text = 'edtHeader'
        end
        object StaticText2: TStaticText
          Left = 8
          Top = 134
          Width = 260
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Font name'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
        end
        object edtHFontSize: TEdit
          Left = 272
          Top = 160
          Width = 50
          Height = 28
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          Text = '10'
        end
        object StaticText3: TStaticText
          Left = 272
          Top = 134
          Width = 50
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Size'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 7
        end
        object UDHFS: TUpDown
          Left = 322
          Top = 160
          Width = 23
          Height = 28
          Associate = edtHFontSize
          Min = 8
          Max = 70
          Position = 10
          TabOrder = 8
        end
        object StaticText4: TStaticText
          Left = 72
          Top = 294
          Width = 209
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Aligment'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 9
        end
        object cbHAligment: TComboBox
          Left = 72
          Top = 322
          Width = 209
          Height = 30
          ItemHeight = 22
          TabOrder = 10
          Text = 'cbHAligment'
          OnChange = cbHAligmentChange
          Items.Strings = (
            'Left'
            'Right'
            'Center')
        end
      end
      object ts_Data: TTabSheet
        Caption = 'Data'
        ImageIndex = 1
        object BitBtn2: TBitBtn
          Left = 124
          Top = 8
          Width = 113
          Height = 33
          Caption = 'Select font'
          TabOrder = 0
          OnClick = BitBtn1Click
        end
        object StaticText5: TStaticText
          Left = 8
          Top = 46
          Width = 260
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Font name'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
        object edtDFontName: TEdit
          Left = 8
          Top = 72
          Width = 260
          Height = 28
          Enabled = False
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Text = 'edtHeader'
        end
        object StaticText6: TStaticText
          Left = 272
          Top = 46
          Width = 50
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Size'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
        end
        object edtDFontSize: TEdit
          Left = 272
          Top = 72
          Width = 50
          Height = 28
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Text = '10'
        end
        object UDDFS: TUpDown
          Left = 322
          Top = 72
          Width = 23
          Height = 28
          Associate = edtDFontSize
          Min = 8
          Max = 70
          Position = 10
          TabOrder = 5
        end
        object GroupBox3: TGroupBox
          Left = 20
          Top = 112
          Width = 321
          Height = 73
          Caption = ' [ Font style ] '
          Enabled = False
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          object cbDBold: TCheckBox
            Left = 16
            Top = 32
            Width = 97
            Height = 17
            Caption = 'Bold'
            TabOrder = 0
          end
          object cbDItalic: TCheckBox
            Left = 184
            Top = 32
            Width = 97
            Height = 17
            Caption = 'Italic'
            TabOrder = 1
          end
        end
        object StaticText7: TStaticText
          Left = 72
          Top = 206
          Width = 209
          Height = 26
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Aligment'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 7
        end
        object cbDAligment: TComboBox
          Left = 72
          Top = 234
          Width = 209
          Height = 30
          ItemHeight = 22
          TabOrder = 8
          Text = 'cbHAligment'
          OnChange = cbDAligmentChange
          Items.Strings = (
            'Left'
            'Right'
            'Center')
        end
        object edtDataWidth: TEdit
          Left = 8
          Top = 320
          Width = 113
          Height = 28
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          Text = '40'
          OnChange = edtDataWidthChange
        end
        object UDDCW: TUpDown
          Left = 121
          Top = 320
          Width = 23
          Height = 28
          Associate = edtDataWidth
          Max = 1000
          Position = 40
          TabOrder = 10
          OnClick = UDDCWClick
        end
        object StaticText8: TStaticText
          Left = 8
          Top = 294
          Width = 136
          Height = 25
          Alignment = taCenter
          BorderStyle = sbsSunken
          Caption = 'Width (0= AutoFit)'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 11
        end
        object StaticText9: TStaticText
          Left = 168
          Top = 294
          Width = 180
          Height = 25
          Alignment = taCenter
          AutoSize = False
          BorderStyle = sbsSunken
          Caption = 'Aligment'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Palatino Linotype'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 12
        end
        object cbDataTypes: TComboBox
          Left = 168
          Top = 320
          Width = 180
          Height = 30
          ItemHeight = 22
          TabOrder = 13
          Text = 'cbDataTypes'
          OnChange = cbDataTypesChange
          Items.Strings = (
            'String'
            'Integer'
            'Float (2 decimals)'
            'Currency'
            'Date'
            '')
        end
      end
    end
  end
  object btnClose: TBitBtn
    Left = 376
    Top = 536
    Width = 161
    Height = 33
    TabOrder = 2
    OnClick = btnCloseClick
    Kind = bkOK
  end
  object btnRun: TBitBtn
    Left = 584
    Top = 536
    Width = 169
    Height = 33
    Caption = 'Try'
    TabOrder = 3
    OnClick = btnRunClick
  end
  object FD: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 376
    Top = 40
  end
end
