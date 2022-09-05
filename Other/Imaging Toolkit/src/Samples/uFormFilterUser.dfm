object FormFilterUser: TFormFilterUser
  Left = 347
  Top = 136
  BorderStyle = bsDialog
  Caption = 'User defined filter'
  ClientHeight = 463
  ClientWidth = 431
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gbKernel: TGroupBox
    Left = 8
    Top = 8
    Width = 417
    Height = 417
    Caption = 'Kernel'
    TabOrder = 0
    object lFilterKernel: TLabel
      Left = 16
      Top = 20
      Width = 57
      Height = 13
      Caption = 'Filter kernel:'
    end
    object lKernelHeight: TLabel
      Left = 216
      Top = 21
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object lKernelWidth: TLabel
      Left = 216
      Top = 53
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lScale: TLabel
      Left = 312
      Top = 21
      Width = 30
      Height = 13
      Caption = 'Scale:'
    end
    object lBias: TLabel
      Left = 312
      Top = 53
      Width = 23
      Height = 13
      Caption = 'Bias:'
    end
    object cbFilterKernel: TComboBox
      Left = 80
      Top = 16
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbFilterKernelChange
    end
    object pcKernel: TPageControl
      Left = 8
      Top = 80
      Width = 401
      Height = 329
      ActivePage = tsKernel1
      TabOrder = 1
      object tsKernel1: TTabSheet
        Caption = 'Kernel 1'
        object sgKernel1: TStringGrid
          Left = 7
          Top = 5
          Width = 378
          Height = 288
          ColCount = 15
          DefaultColWidth = 24
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 15
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs]
          TabOrder = 0
        end
      end
      object tsKernel2: TTabSheet
        Caption = 'Kernel 2'
        object sgKernel2: TStringGrid
          Left = 7
          Top = 5
          Width = 378
          Height = 288
          ColCount = 15
          DefaultColWidth = 24
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 15
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goTabs]
          TabOrder = 0
        end
      end
    end
    object seKernelHeight: TmcmIntSpin
      Left = 256
      Top = 16
      Width = 49
      Height = 22
      MaxLength = 2
      TabOrder = 2
      OnChange = seKernelHeightChange
      Value = 3
      MaxValue = 15
      MinValue = 1
    end
    object seKernelWidth: TmcmIntSpin
      Left = 256
      Top = 48
      Width = 49
      Height = 22
      MaxLength = 2
      TabOrder = 3
      OnChange = seKernelWidthChange
      Value = 3
      MaxValue = 15
      MinValue = 1
    end
    object seScale: TmcmIntSpin
      Left = 352
      Top = 16
      Width = 49
      Height = 22
      MaxLength = 5
      TabOrder = 4
      Value = 1
      MaxValue = 10000
      MinValue = 0
    end
    object seBias: TmcmIntSpin
      Left = 352
      Top = 48
      Width = 49
      Height = 22
      MaxLength = 5
      TabOrder = 5
      Value = 0
      MaxValue = 10000
      MinValue = 0
    end
  end
  object btnOK: TButton
    Left = 8
    Top = 432
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 88
    Top = 432
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
