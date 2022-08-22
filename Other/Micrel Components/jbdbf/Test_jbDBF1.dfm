object frmMain: TfrmMain
  Left = 0
  Top = 0
  Width = 434
  Height = 340
  Caption = 'Test jbDBF'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 269
    Width = 426
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 305
      Top = 0
      Width = 121
      Height = 17
      Align = alRight
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      BevelWidth = 2
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 305
      Height = 17
      Align = alClient
      BevelOuter = bvLowered
      BevelWidth = 2
      TabOrder = 1
      object Gauge1: TGauge
        Left = 2
        Top = 2
        Width = 301
        Height = 13
        Align = alClient
        BackColor = clBtnFace
        BorderStyle = bsNone
        ForeColor = clBlue
        Progress = 0
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 8
      Width = 75
      Height = 13
      Caption = 'Database path:'
    end
    object DBF_filename: TEdit
      Left = 101
      Top = 5
      Width = 193
      Height = 21
      TabOrder = 0
    end
  end
  object ListBox1: TListBox
    Left = 0
    Top = 33
    Width = 426
    Height = 236
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 320
    Top = 8
    object Database1: TMenuItem
      Caption = 'Database'
      object Create1: TMenuItem
        Caption = 'Create'
        OnClick = Create1Click
      end
      object Reindex1: TMenuItem
        Caption = 'Reindex'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object est1: TMenuItem
      Caption = 'Test'
      object Createtestdata1: TMenuItem
        Caption = 'Create test data'
      end
      object Deletetestdata1: TMenuItem
        Caption = 'Delete test data'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Searchup1: TMenuItem
        Caption = 'Search up'
        OnClick = Searchup1Click
      end
      object Searchdown1: TMenuItem
        Caption = 'Search down'
        OnClick = Searchdown1Click
      end
    end
    object About1: TMenuItem
      Caption = 'About'
      object About2: TMenuItem
        Caption = 'About'
        OnClick = About2Click
      end
    end
  end
  object DBF1: TjbDBF
    Active = True
    DBFields = <>
    LangCodePage = 852
    ReadOnly = False
    SaveOnClose = False
    StoreByIndex = False
    OnClosed = DBF1Closed
    OnDeleted = DBF1Deleted
    OnFound = DBF1Found
    OnOpened = DBF1Opened
    OnUpdate = DBF1Update
    Left = 360
    Top = 1
  end
end
