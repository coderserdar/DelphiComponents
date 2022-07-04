object FormMain: TFormMain
  Left = 232
  Top = 249
  ActiveControl = btnBrowse
  AutoScroll = False
  Caption = 'ATStreamSearch Demo (powered by ATBinHex and DIRegEx)'
  ClientHeight = 397
  ClientWidth = 542
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
    542
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 217
    Width = 542
    Height = 180
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      542
      180)
    object GroupBox1: TGroupBox
      Left = 8
      Top = 2
      Width = 526
      Height = 62
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = ' File '
      TabOrder = 0
      object labFN: TLabel
        Left = 8
        Top = 16
        Width = 49
        Height = 13
        Caption = 'F&ile name:'
        FocusControl = edFilename
      end
      object Label1: TLabel
        Left = 8
        Top = 40
        Width = 65
        Height = 13
        Caption = 'File contents:'
      end
      object edFilename: TTntEdit
        Left = 104
        Top = 16
        Width = 241
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object btnBrowse: TButton
        Left = 352
        Top = 16
        Width = 73
        Height = 21
        Caption = '&Browse...'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object chkOEM: TCheckBox
        Left = 104
        Top = 40
        Width = 73
        Height = 17
        Caption = '&OEM'
        TabOrder = 2
        OnClick = chkOEMClick
      end
      object chkUnicode: TCheckBox
        Left = 176
        Top = 40
        Width = 81
        Height = 17
        Caption = '&Unicode'
        TabOrder = 3
        OnClick = chkUnicodeClick
      end
      object chkUnicodeBE: TCheckBox
        Left = 256
        Top = 40
        Width = 137
        Height = 17
        Caption = 'Unicode BE detected'
        Enabled = False
        TabOrder = 4
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 66
      Width = 526
      Height = 110
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = ' Search '
      TabOrder = 1
      object labString: TLabel
        Left = 8
        Top = 16
        Width = 67
        Height = 13
        Caption = '&Search string:'
        FocusControl = edString
        WordWrap = True
      end
      object labOptions: TLabel
        Left = 8
        Top = 40
        Width = 75
        Height = 13
        Caption = 'Search options:'
      end
      object edString: TTntEdit
        Left = 104
        Top = 16
        Width = 241
        Height = 21
        TabOrder = 0
        OnChange = edStringChange
      end
      object btnFind: TButton
        Left = 352
        Top = 16
        Width = 73
        Height = 21
        Caption = '&Find first'
        Default = True
        Enabled = False
        TabOrder = 1
        OnClick = btnFindClick
      end
      object btnFindNext: TButton
        Left = 432
        Top = 16
        Width = 73
        Height = 21
        Caption = 'Find &next'
        Enabled = False
        TabOrder = 2
        OnClick = btnFindNextClick
      end
      object chkCase: TCheckBox
        Left = 104
        Top = 56
        Width = 313
        Height = 17
        Caption = '&Case sensitive'
        TabOrder = 4
      end
      object chkWords: TCheckBox
        Left = 104
        Top = 72
        Width = 313
        Height = 17
        Caption = '&Whole words only'
        TabOrder = 5
      end
      object chkRegex: TCheckBox
        Left = 104
        Top = 40
        Width = 313
        Height = 17
        Caption = '&Regular expressions'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkRegexClick
      end
      object chkBack: TCheckBox
        Left = 104
        Top = 88
        Width = 313
        Height = 17
        Caption = 'Search bac&kward (not for RegEx)'
        Enabled = False
        TabOrder = 6
      end
    end
  end
  object GroupBoxViewer: TGroupBox
    Left = 8
    Top = 2
    Width = 526
    Height = 214
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Viewer '
    TabOrder = 0
    object Viewer: TATBinHex
      Left = 2
      Top = 15
      Width = 522
      Height = 165
      Cursor = crIBeam
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Viewer'
      Color = clWindow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      FontOEM.Charset = OEM_CHARSET
      FontOEM.Color = clWindowText
      FontOEM.Height = -12
      FontOEM.Name = 'Terminal'
      FontOEM.Style = []
    end
    object Panel2: TPanel
      Left = 2
      Top = 180
      Width = 522
      Height = 32
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 64
        Height = 13
        Caption = 'Found string:'
      end
      object Label3: TLabel
        Left = 312
        Top = 8
        Width = 35
        Height = 13
        Caption = 'Offset:'
      end
      object Label4: TLabel
        Left = 448
        Top = 8
        Width = 21
        Height = 13
        Caption = 'Len:'
      end
      object edSelection: TTntEdit
        Left = 102
        Top = 6
        Width = 203
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object edOffset: TTntEdit
        Left = 350
        Top = 6
        Width = 91
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object edLength: TTntEdit
        Left = 472
        Top = 6
        Width = 33
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object Search: TATStreamSearch
    OnProgress = SearchProgress
    Left = 480
    Top = 232
  end
  object OpenDialog1: TTntOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 232
  end
end
