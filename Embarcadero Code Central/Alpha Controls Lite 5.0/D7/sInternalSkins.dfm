object FormInternalSkins: TFormInternalSkins
  Left = 183
  Top = 116
  Width = 340
  Height = 250
  BorderIcons = []
  Caption = 'Internal skins'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 140
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TsListBox
    Left = 9
    Top = 9
    Width = 198
    Height = 202
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    SkinData.SkinSection = 'EDIT'
    ItemHeight = 13
    Sorted = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object sBitBtn1: TsButton
    Left = 223
    Top = 175
    Width = 94
    Height = 36
    Action = ActionClose
    Anchors = [akRight, akBottom]
    Cancel = True
    Default = True
    TabOrder = 1
    SkinData.SkinSection = 'BUTTON'
  end
  object sPanel1: TsPanel
    Left = 216
    Top = 9
    Width = 109
    Height = 155
    Anchors = [akTop, akRight]
    Caption = ' '
    TabOrder = 2
    SkinData.SkinSection = 'PANEL_LOW'
    object sButton2: TsButton
      Left = 9
      Top = 12
      Width = 90
      Height = 25
      Action = ActionAddNew
      Anchors = [akTop, akRight]
      TabOrder = 0
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton3: TsButton
      Left = 9
      Top = 39
      Width = 90
      Height = 25
      Action = ActionRename
      Anchors = [akTop, akRight]
      TabOrder = 1
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton4: TsButton
      Left = 9
      Top = 66
      Width = 90
      Height = 25
      Action = ActionExtract
      Anchors = [akTop, akRight]
      TabOrder = 2
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton1: TsButton
      Left = 9
      Top = 93
      Width = 90
      Height = 25
      Action = ActionDelete
      Anchors = [akTop, akRight]
      TabOrder = 3
      SkinData.SkinSection = 'BUTTON'
    end
    object sButton5: TsButton
      Left = 9
      Top = 120
      Width = 90
      Height = 25
      Action = ActionClear
      Anchors = [akTop, akRight]
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
    BorderIcons = []
    CaptionAlignment = taCenter
    SkinData.SkinSection = 'FORM'
    GripMode = gmRightBottom
    ShowAppIcon = False
    TitleButtons = <>
    Left = 104
    Top = 95
  end
end
