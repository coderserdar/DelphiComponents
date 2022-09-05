object DlgTwnSend: TDlgTwnSend
  Left = 376
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Negotiate'
  ClientHeight = 415
  ClientWidth = 610
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 184
    Top = 384
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnSend: TButton
    Left = 8
    Top = 384
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object btnStatus: TButton
    Left = 96
    Top = 384
    Width = 75
    Height = 25
    Caption = 'S&tatus'
    TabOrder = 2
    OnClick = btnStatusClick
  end
  object gbSend: TGroupBox
    Left = 8
    Top = 0
    Width = 289
    Height = 217
    TabOrder = 3
    object lResult: TLabel
      Left = 16
      Top = 16
      Width = 33
      Height = 13
      Caption = 'Result:'
    end
    object lResultVal: TLabel
      Left = 96
      Top = 16
      Width = 161
      Height = 13
      AutoSize = False
      Caption = 'lResultVal'
    end
    object lStatus: TLabel
      Left = 16
      Top = 40
      Width = 33
      Height = 13
      Caption = 'Status:'
    end
    object lStatusVal: TLabel
      Left = 96
      Top = 40
      Width = 161
      Height = 13
      AutoSize = False
      Caption = 'lStatusVal'
    end
    object lDest: TLabel
      Left = 16
      Top = 96
      Width = 73
      Height = 13
      AutoSize = False
      Caption = '&Destination:'
    end
    object lDG: TLabel
      Left = 16
      Top = 128
      Width = 72
      Height = 13
      AutoSize = False
      Caption = 'D&G:'
    end
    object lDAT: TLabel
      Left = 16
      Top = 160
      Width = 72
      Height = 13
      AutoSize = False
      Caption = 'D&AT:'
    end
    object lMSG: TLabel
      Left = 16
      Top = 192
      Width = 72
      Height = 13
      AutoSize = False
      Caption = '&Message:'
    end
    object lState: TLabel
      Left = 16
      Top = 64
      Width = 28
      Height = 13
      Caption = 'State:'
    end
    object lStateVal: TLabel
      Left = 96
      Top = 64
      Width = 42
      Height = 13
      Caption = 'lStateVal'
    end
    object cbDest: TComboBox
      Left = 96
      Top = 88
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbDestChange
    end
    object cbDG: TComboBox
      Left = 96
      Top = 120
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbDGChange
    end
    object cbDAT: TComboBox
      Left = 96
      Top = 152
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = cbDATChange
    end
    object cbMSG: TComboBox
      Left = 96
      Top = 184
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbMSGChange
    end
  end
  object sgData: TStringGrid
    Left = 304
    Top = 8
    Width = 297
    Height = 369
    ColCount = 2
    DefaultColWidth = 128
    DefaultRowHeight = 18
    RowCount = 256
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 4
    RowHeights = (
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18
      18)
  end
  object gbCapability: TGroupBox
    Left = 8
    Top = 224
    Width = 289
    Height = 153
    Caption = 'Capability'
    TabOrder = 5
    object lCapability: TLabel
      Left = 16
      Top = 32
      Width = 72
      Height = 13
      AutoSize = False
      Caption = 'Ca&pability:'
    end
    object lContainer: TLabel
      Left = 16
      Top = 64
      Width = 72
      Height = 13
      AutoSize = False
      Caption = 'Co&ntainer:'
    end
    object lItemType: TLabel
      Left = 16
      Top = 96
      Width = 50
      Height = 13
      Caption = 'Item Type:'
    end
    object lNumItems: TLabel
      Left = 16
      Top = 128
      Width = 53
      Height = 13
      Caption = 'Num Items:'
    end
    object cbCAP: TComboBox
      Left = 96
      Top = 24
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbCAPChange
    end
    object cbCON: TComboBox
      Left = 96
      Top = 56
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbCONChange
    end
    object cbItemType: TComboBox
      Left = 96
      Top = 88
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object rsNumItems: TmcmRealSpin
      Left = 96
      Top = 120
      Width = 65
      Height = 22
      TabOrder = 3
      OnChange = rsNumItemsChange
      Value = 1.000000000000000000
      MaxValue = 1024.000000000000000000
      Decimals = 0
      Increment = 1.000000000000000000
    end
  end
end
