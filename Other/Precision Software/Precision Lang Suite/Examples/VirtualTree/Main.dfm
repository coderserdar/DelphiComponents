object MainForm: TMainForm
  Left = 717
  Top = 268
  Caption = 'Simple Virtual Treeview demo'
  ClientHeight = 454
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    417
    454)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 116
    Height = 13
    Caption = 'Last operation duration:'
  end
  object VST: TVirtualStringTree
    Left = 8
    Top = 36
    Width = 397
    Height = 318
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    DragType = dtVCL
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Options = [hoColumnResize, hoDrag, hoShowHint, hoVisible]
    Header.ParentFont = True
    HintAnimation = hatNone
    IncrementalSearch = isAll
    ParentBiDiMode = False
    ParentShowHint = False
    RootNodeCount = 100
    ShowHint = True
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toEditable, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnInitNode = VSTInitNode
    Columns = <
      item
        Position = 0
        Width = 164
        WideText = 'Level and index'
      end
      item
        Position = 1
        Width = 208
        WideText = 'Child count'
      end>
  end
  object ClearButton: TButton
    Left = 97
    Top = 421
    Width = 140
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear tree'
    TabOrder = 1
    OnClick = ClearButtonClick
  end
  object AddOneButton: TButton
    Left = 96
    Top = 361
    Width = 140
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add node(s) to root'
    TabOrder = 2
    OnClick = AddButtonClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 377
    Width = 81
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '1'
  end
  object Button1: TButton
    Tag = 1
    Left = 96
    Top = 389
    Width = 140
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add node(s) as children'
    TabOrder = 4
    OnClick = AddButtonClick
  end
  object CloseButton: TButton
    Left = 330
    Top = 421
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = CloseButtonClick
  end
  object sbLang: TButton
    Left = 280
    Top = 361
    Width = 125
    Height = 25
    Caption = 'sbLang'
    DropDownMenu = pumLangs
    TabOrder = 6
    OnClick = sbLangClick
  end
  object plsController1: TplsController
    OnBeforeLangChange = plsController1BeforeLangChange
    OnInitLangManager = plsController1InitLangManager
    OnLanguageChanged = plsController1LanguageChanged
    Left = 260
    Top = 116
  end
  object pumLangs: TPopupMenu
    Left = 260
    Top = 172
  end
end
