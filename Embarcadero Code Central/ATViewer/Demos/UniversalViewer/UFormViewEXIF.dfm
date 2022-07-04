object FormViewEXIF: TFormViewEXIF
  Left = 230
  Top = 38
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'EXIF Reader'
  ClientHeight = 496
  ClientWidth = 560
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    560
    496)
  PixelsPerInch = 96
  TextHeight = 13
  object tc: TTabControl
    Left = 8
    Top = 4
    Width = 545
    Height = 461
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    Tabs.Strings = (
      ' IFD 0 '
      ' EXIF Sub IFD'
      ' EXIF Interoperability IFD '
      ' Maker Note '
      ' IFD 1'
      ' Thumbnail ')
    TabIndex = 0
    OnChange = tcChange
    DesignSize = (
      545
      461)
    object List: TListView
      Left = 15
      Top = 33
      Width = 514
      Height = 410
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Tag'
        end
        item
          Caption = 'Property'
          Width = 150
        end
        item
          Caption = 'Value'
          Width = 300
        end>
      ColumnClick = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = ListDblClick
    end
    object ImagePanel: TPanel
      Left = 4
      Top = 24
      Width = 537
      Height = 433
      Align = alClient
      BevelOuter = bvNone
      Caption = ' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      DesignSize = (
        537
        433)
      object Image: TImage
        Left = 7
        Top = 5
        Width = 522
        Height = 408
        Anchors = [akLeft, akTop, akRight, akBottom]
      end
      object Label1: TLabel
        Left = 0
        Top = 420
        Width = 537
        Height = 13
        Align = alBottom
        Caption = ' '
      end
    end
  end
  object btnClose: TButton
    Left = 472
    Top = 468
    Width = 81
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
end
