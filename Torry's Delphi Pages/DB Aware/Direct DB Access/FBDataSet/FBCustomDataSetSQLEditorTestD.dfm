object FBCustomDataSetSQLEditorTestForm: TFBCustomDataSetSQLEditorTestForm
  Left = 277
  Top = 555
  Width = 693
  Height = 314
  Caption = 'SQL Editor test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 685
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      685
      41)
    object Button1: TButton
      Left = 600
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 685
    Height = 246
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    TabPosition = tpBottom
    object TabSheet1: TTabSheet
      Caption = 'Result'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 677
        Height = 220
        Align = alClient
        DataSource = DataSource1
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Plan'
      ImageIndex = 1
      object SynEdit1: TSynEdit
        Left = 0
        Top = 0
        Width = 677
        Height = 220
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Lines.Strings = (
          'SynEdit1')
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = FBDataSetTest
    Left = 32
    Top = 40
  end
  object FBDataSetTest: TFBDataSet
    AutoUpdateOptions.WhenGetGenID = wgNever
    AutoUpdateOptions.IncrementBy = 1
    DefaultFormats.DisplayFormatNumeric = '#,##0.0'
    DefaultFormats.DisplayFormatInteger = '#,##0'
    DetailConditions = []
    Macros = <>
    Option = [poTrimCharFields, poRefreshAfterPost]
    UpdateRecordTypes = [cusUnmodified, cusModified, cusInserted]
    Left = 64
    Top = 40
  end
end
