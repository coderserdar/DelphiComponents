object frmExcelEdit: TfrmExcelEdit
  Left = 468
  Top = 237
  Width = 509
  Height = 486
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Edit in Excel'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 415
    Width = 501
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object lblCancel: TLabel
      Left = 152
      Top = 15
      Width = 321
      Height = 17
      AutoSize = False
      Caption = 'Aborts the operation and discards any changes made.'
      WordWrap = True
    end
    object btnCancel: TButton
      Left = 24
      Top = 10
      Width = 105
      Height = 25
      Caption = '&Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 415
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object lblCounter: TLabel
      Left = 96
      Top = 382
      Width = 369
      Height = 13
      AutoSize = False
    end
    object lblImportFromExcel: TLabel
      Left = 160
      Top = 309
      Width = 249
      Height = 33
      AutoSize = False
      Caption = 'Imports the contents of the Excel worksheet from step 1a or 1b. '
      Enabled = False
      WordWrap = True
    end
    object lblExportToExcel2: TLabel
      Left = 40
      Top = 76
      Width = 313
      Height = 41
      AutoSize = False
      Caption = 
        'Edit the table in Excel. The first line always determines the fi' +
        'eld for each column. If you change the column ordering or delete' +
        ' columns, make sure the column headers match their contents.'
      WordWrap = True
    end
    object lblStepOneA: TLabel
      Left = 8
      Top = 40
      Width = 25
      Height = 25
      AutoSize = False
      Caption = '1a.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblStepOneB: TLabel
      Left = 8
      Top = 146
      Width = 25
      Height = 25
      AutoSize = False
      Caption = '1b.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblStepTwo: TLabel
      Left = 8
      Top = 206
      Width = 25
      Height = 25
      AutoSize = False
      Caption = '2.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblStepThree: TLabel
      Left = 8
      Top = 309
      Width = 25
      Height = 25
      AutoSize = False
      Caption = '3.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblMainHeader: TLabel
      Left = 8
      Top = 8
      Width = 483
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Instructions for using Excel to edit table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object lblExportToExcel1: TLabel
      Left = 153
      Top = 41
      Width = 281
      Height = 33
      AutoSize = False
      Caption = 
        'Opens Excel automatically. Exports the contents of the table edi' +
        'ted to be edited within Excel.'
      WordWrap = True
    end
    object lblProgressCaption: TLabel
      Left = 24
      Top = 382
      Width = 44
      Height = 13
      Caption = 'Progress:'
    end
    object lblExportToExcel3: TLabel
      Left = 40
      Top = 123
      Width = 401
      Height = 17
      AutoSize = False
      Caption = 
        'When ready, return from Excel, either using Alt-Tab or by minimi' +
        'zing Excel.'
      WordWrap = True
    end
    object lblImportFromFile: TLabel
      Left = 40
      Top = 150
      Width = 377
      Height = 18
      AutoSize = False
      Caption = 'No export. Import directly from an exisiting Excel file'
      WordWrap = True
    end
    object btnChooseFile: TSpeedButton
      Left = 356
      Top = 166
      Width = 23
      Height = 22
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
        B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
        000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
        555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
        55555575FFF75555555555700007555555555557777555555555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = btnChooseFileClick
    end
    object btnExport: TButton
      Left = 40
      Top = 41
      Width = 105
      Height = 25
      Caption = '&Export to Excel'
      TabOrder = 0
      OnClick = btnExportClick
    end
    object btnImport: TButton
      Left = 40
      Top = 309
      Width = 105
      Height = 25
      Caption = '&Import from Excel'
      Enabled = False
      TabOrder = 1
      OnClick = btnImportClick
    end
    object RadioGroupImportRules: TRadioGroup
      Left = 40
      Top = 207
      Width = 321
      Height = 87
      Caption = 'Import Rules'
      ItemIndex = 0
      Items.Strings = (
        '&Replace existing row with imported row if key values equal'
        '&Add imported row values to existing row if key values equal'
        '&Delete the existing data before importing')
      TabOrder = 2
    end
    object txtExcelFile: TEdit
      Left = 40
      Top = 167
      Width = 313
      Height = 21
      TabOrder = 3
      OnChange = OnExcelFileChange
    end
  end
  object opendialogExcelFile: TOpenDialog
    DefaultExt = 'xls'
    Filter = 'Excel files|*.xls|All files|*.*'
    Left = 383
    Top = 162
  end
end
