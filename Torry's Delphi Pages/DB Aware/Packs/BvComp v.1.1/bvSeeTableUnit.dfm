object SeeTableForm: TSeeTableForm
  Left = 200
  Top = 108
  Width = 544
  Height = 375
  Caption = 'Seetableform'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 24
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object DBNavigator: TDBNavigator
      Left = 1
      Top = 1
      Width = 288
      Height = 22
      Cursor = crHandPoint
      DataSource = DataSource
      Align = alLeft
      Flat = True
      Hints.Strings = (
        'First'
        'Previous'
        'Next'
        'Last'
        'Insert'
        'Delete'
        'Edit'
        'Post'
        'Cancel'
        'Refresh')
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object PanelStatus: TPanel
    Left = 0
    Top = 330
    Width = 536
    Height = 18
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '?????'
    TabOrder = 0
    object LabData: TLabel
      Left = 523
      Top = 1
      Width = 12
      Height = 16
      Align = alRight
      Alignment = taRightJustify
      Caption = '??'
    end
  end
  object DataSource: TDataSource
    DataSet = Table
    Left = 168
    Top = 72
  end
  object Table: TTable
    AfterClose = TableAfterRefresh
    BeforeEdit = TableAfterRefresh
    AfterPost = TableAfterRefresh
    AfterCancel = TableAfterRefresh
    AfterScroll = TableAfterRefresh
    AfterRefresh = TableAfterRefresh
    Left = 328
    Top = 72
  end
end
