object SearchForm: TSearchForm
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 215
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl: TPageControl
    Left = 5
    Top = 5
    Width = 361
    Height = 173
    ActivePage = LocateTabSheet
    HotTrack = True
    MultiLine = True
    TabOrder = 0
    TabStop = False
    object LocateTabSheet: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      object Label1: TLabel
        Left = 123
        Top = 13
        Width = 9
        Height = 14
        Caption = '='
      end
      object LocateFieldComboBox: TComboBox
        Left = 8
        Top = 9
        Width = 105
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 8
      end
      object LocateValueEdit: TEdit
        Left = 137
        Top = 9
        Width = 136
        Height = 22
        TabOrder = 0
      end
      object LocateAddButton: TButton
        Left = 284
        Top = 9
        Width = 60
        Height = 20
        Caption = 'Add'
        TabOrder = 1
        OnClick = LocateAddButtonClick
      end
      object LocateConListView: TListView
        Left = 8
        Top = 37
        Width = 265
        Height = 73
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Condic'
            Width = 140
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 2
        ViewStyle = vsReport
      end
      object LocateCaseInsCheckBox: TCheckBox
        Left = 8
        Top = 120
        Width = 113
        Height = 17
        Caption = 'Case insensitive'
        TabOrder = 6
      end
      object LocatePartCheckBox: TCheckBox
        Left = 141
        Top = 120
        Width = 128
        Height = 17
        Caption = 'Partial compare'
        TabOrder = 7
      end
      object LocateClearButton: TButton
        Left = 284
        Top = 91
        Width = 60
        Height = 20
        Caption = 'Clear'
        TabOrder = 5
        OnClick = LocateClearButtonClick
      end
      object LocateDeleteButton: TButton
        Left = 284
        Top = 64
        Width = 60
        Height = 20
        Caption = 'Delete'
        TabOrder = 4
        OnClick = LocateDeleteButtonClick
      end
      object LocateReplaceButton: TButton
        Left = 284
        Top = 37
        Width = 60
        Height = 20
        Caption = 'Replace'
        TabOrder = 3
        OnClick = LocateReplaceButtonClick
      end
    end
    object FindTabSheet: TTabSheet
      Caption = 'Find'
      ImageIndex = 2
      object FindFieldComboBox: TComboBox
        Left = 8
        Top = 9
        Width = 103
        Height = 22
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 4
      end
      object FindValueEdit: TEdit
        Left = 180
        Top = 9
        Width = 93
        Height = 22
        TabOrder = 0
      end
      object FindAddButton: TButton
        Left = 283
        Top = 9
        Width = 61
        Height = 20
        Caption = 'Add'
        TabOrder = 1
        OnClick = FindAddButtonClick
      end
      object FindOprComboBox: TComboBox
        Left = 119
        Top = 9
        Width = 54
        Height = 22
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 5
        Items.Strings = (
          '='
          '>'
          '<'
          '>='
          '<='
          '<>')
      end
      object FindConMemo: TMemo
        Left = 8
        Top = 38
        Width = 217
        Height = 98
        ScrollBars = ssVertical
        TabOrder = 2
      end
      object FindModeRadioGroup: TRadioGroup
        Left = 234
        Top = 33
        Width = 110
        Height = 103
        ItemIndex = 0
        Items.Strings = (
          'Find first'
          'Find next'
          'Find last'
          'Find prior')
        TabOrder = 3
      end
    end
    object GotoKeyTabSheet: TTabSheet
      Caption = 'GotoKey'
      ImageIndex = 2
      object GotoKeyScrollBox: TScrollBox
        Left = 7
        Top = 7
        Width = 339
        Height = 131
        HorzScrollBar.Visible = False
        VertScrollBar.Tracking = True
        TabOrder = 0
      end
    end
  end
  object OkButton: TButton
    Left = 208
    Top = 184
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 291
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
