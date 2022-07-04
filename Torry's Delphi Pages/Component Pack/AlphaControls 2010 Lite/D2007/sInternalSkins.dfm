object FormInternalSkins: TFormInternalSkins
  Left = 439
  Top = 316
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Internal skins'
  ClientHeight = 240
  ClientWidth = 354
  Color = clBtnFace
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TsListBox
    Left = 13
    Top = 13
    Width = 198
    Height = 212
    BoundLabel.Indent = 0
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
    SkinData.SkinSection = 'EDIT'
    ItemHeight = 13
    Sorted = True
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object sBitBtn1: TsButton
    Left = 239
    Top = 187
    Width = 94
    Height = 36
    Action = ActionClose
    Cancel = True
    Default = True
    TabOrder = 1
    SkinData.SkinSection = 'BUTTON'
  end
  object sPanel1: TsPanel
    Left = 228
    Top = 13
    Width = 117
    Height = 156
    BevelOuter = bvLowered
    Caption = ' '
    TabOrder = 2
    SkinData.SkinSection = 'PANEL_LOW'
    object sButton2: TsButton
      Left = 13
      Top = 12
      Width = 90
      Height = 25
      Action = ActionAddNew
      TabOrder = 0
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton3: TsButton
      Left = 13
      Top = 39
      Width = 90
      Height = 25
      Action = ActionRename
      TabOrder = 1
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton4: TsButton
      Left = 13
      Top = 66
      Width = 90
      Height = 25
      Action = ActionExtract
      TabOrder = 2
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton1: TsButton
      Left = 13
      Top = 93
      Width = 90
      Height = 25
      Action = ActionDelete
      TabOrder = 3
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton5: TsButton
      Left = 13
      Top = 120
      Width = 90
      Height = 25
      Action = ActionClear
      TabOrder = 4
      SkinData.SkinSection = 'BUTTON'
    end
  end
  object ActionList1: TActionList
    Left = 76
    Top = 95
    object ActionAddNew: TAction
      Caption = 'Add new'
      OnExecute = ActionAddNewExecute
    end
    object ActionDelete: TAction
      Caption = 'Delete'
      Enabled = False
      OnExecute = ActionDeleteExecute
    end
    object ActionRename: TAction
      Caption = 'Rename'
      Enabled = False
      OnExecute = ActionRenameExecute
    end
    object ActionClose: TAction
      Caption = 'Close'
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
    object ActionClear: TAction
      Caption = 'Clear'
      OnExecute = ActionClearExecute
    end
    object ActionExtract: TAction
      Caption = 'Extract'
      Enabled = False
      OnExecute = ActionExtractExecute
    end
  end
  object sSkinProvider1: TsSkinProvider
    CaptionAlignment = taCenter
    SkinData.SkinSection = 'FORM'
    ShowAppIcon = False
    TitleButtons = <>
    Left = 104
    Top = 95
  end
end
