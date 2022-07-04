object FormPluginsOptions: TFormPluginsOptions
  Left = 200
  Top = 172
  ActiveControl = List
  BorderStyle = bsDialog
  Caption = 'Plugins configuration'
  ClientHeight = 397
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 424
    Top = 368
    Width = 89
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 232
    Top = 368
    Width = 89
    Height = 23
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object boxPlugins: TGroupBox
    Left = 8
    Top = 4
    Width = 505
    Height = 281
    Caption = 'Plugins installed'
    TabOrder = 0
    object List: TListView
      Left = 8
      Top = 16
      Width = 489
      Height = 221
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'Detect string'
          Width = 0
        end
        item
          Caption = 'Filename'
          Width = 500
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = btnConfigClick
      OnSelectItem = ListSelectItem
    end
    object btnAdd: TButton
      Left = 8
      Top = 246
      Width = 89
      Height = 23
      Caption = '&Add...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 104
      Top = 246
      Width = 89
      Height = 23
      Caption = '&Remove'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object btnConfig: TButton
      Left = 200
      Top = 246
      Width = 89
      Height = 23
      Caption = '&Configure...'
      TabOrder = 3
      OnClick = btnConfigClick
    end
    object btnUp: TButton
      Left = 296
      Top = 246
      Width = 89
      Height = 23
      Caption = '&Up'
      TabOrder = 4
      OnClick = btnUpClick
    end
    object btnDown: TButton
      Left = 392
      Top = 246
      Width = 89
      Height = 23
      Caption = '&Down'
      TabOrder = 5
      OnClick = btnDownClick
    end
  end
  object boxOptions: TGroupBox
    Left = 8
    Top = 288
    Width = 505
    Height = 73
    Caption = 'Options'
    TabOrder = 1
    object chkPriority: TCheckBox
      Left = 9
      Top = 16
      Width = 488
      Height = 17
      Caption = 'Plugins have higher priority than internal viewers'
      TabOrder = 0
    end
    object chkTCVar: TCheckBox
      Left = 9
      Top = 48
      Width = 488
      Height = 17
      Caption = 'Save filenames with %Commander_Path% variable'
      TabOrder = 2
    end
    object chkHideKeys: TCheckBox
      Left = 9
      Top = 32
      Width = 488
      Height = 17
      Caption = 'Add the '#39'Alt'#39' key to shortcuts when Plugins mode is active'
      TabOrder = 1
    end
  end
  object btnHelp: TButton
    Left = 328
    Top = 368
    Width = 89
    Height = 23
    Caption = 'Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
