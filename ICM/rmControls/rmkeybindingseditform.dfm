object FrmEditKeyBindings: TFrmEditKeyBindings
  Left = 237
  Top = 183
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Edit Application Key Bindings'
  ClientHeight = 232
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblErrorInfo: TrmLabel
    Left = 3
    Top = 194
    Width = 358
    Height = 33
    WordWrap = True
    BorderStyle = rmbsSunken
  end
  object Label1: TLabel
    Left = 5
    Top = 5
    Width = 56
    Height = 13
    Caption = 'Categories:'
  end
  object Label2: TLabel
    Left = 155
    Top = 5
    Width = 56
    Height = 13
    Caption = 'Commands:'
  end
  object lbxCommands: TListBox
    Left = 153
    Top = 20
    Width = 208
    Height = 106
    Style = lbOwnerDrawFixed
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbxCommandsClick
    OnDrawItem = lbxCommandsDrawItem
  end
  object lbxCategories: TListBox
    Left = 3
    Top = 20
    Width = 144
    Height = 106
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnClick = lbxCategoriesClick
  end
  object btnClose: TButton
    Left = 366
    Top = 202
    Width = 93
    Height = 25
    Caption = 'Close'
    Default = True
    TabOrder = 2
  end
  object btnChange: TButton
    Left = 366
    Top = 133
    Width = 93
    Height = 25
    Action = actChange
    TabOrder = 3
  end
  object btnResetAll: TButton
    Left = 366
    Top = 160
    Width = 93
    Height = 25
    Action = actResetAll
    TabOrder = 5
  end
  object GroupBox1: TGroupBox
    Left = 153
    Top = 128
    Width = 208
    Height = 61
    Caption = 'Command Description'
    TabOrder = 8
    object lblDescription: TrmLabel
      Left = 5
      Top = 16
      Width = 198
      Height = 40
      WordWrap = True
    end
  end
  object GroupBox2: TGroupBox
    Left = 3
    Top = 128
    Width = 144
    Height = 42
    Caption = 'Current Binding'
    TabOrder = 9
    object lblKeys: TrmLabel
      Left = 5
      Top = 16
      Width = 134
      Height = 21
      Alignment = taCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clActiveCaption
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = rmtlCenter
    end
  end
  object cbxDesignLock: TCheckBox
    Left = 5
    Top = 172
    Width = 140
    Height = 17
    Caption = 'Lock Command Binding'
    TabOrder = 4
    OnClick = cbxDesignLockClick
  end
  object btnSave: TButton
    Left = 366
    Top = 20
    Width = 93
    Height = 25
    Action = actSaveBindings
    TabOrder = 6
  end
  object btnLoad: TButton
    Left = 366
    Top = 47
    Width = 93
    Height = 25
    Action = actLoadBindings
    TabOrder = 7
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 307
    Top = 46
    object actChange: TAction
      Caption = 'Edit Binding...'
      OnExecute = actChangeExecute
    end
    object actResetAll: TAction
      Caption = 'Reset All'
      OnExecute = actResetAllExecute
    end
    object actSaveBindings: TAction
      Caption = 'Save Bindings...'
      OnExecute = actSaveBindingsExecute
    end
    object actLoadBindings: TAction
      Caption = 'Load Bindings...'
      OnExecute = actLoadBindingsExecute
    end
  end
end
