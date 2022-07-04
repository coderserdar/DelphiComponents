object UserSearchForm: TUserSearchForm
  Left = 196
  Top = 156
  Width = 786
  Height = 556
  Caption = 'Find/Add Contacts'
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    778
    527)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 0
    Width = 209
    Height = 89
    Caption = 'ICQ'
    TabOrder = 0
    object SearchByNumberRadio: TRadioButton
      Left = 8
      Top = 24
      Width = 177
      Height = 17
      Caption = 'Search by ICQ number'
      TabOrder = 0
      OnClick = SearchByNumberRadioClick
    end
    object NumberEdit: TEdit
      Left = 8
      Top = 48
      Width = 193
      Height = 21
      TabOrder = 1
      OnChange = NumberEditChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 96
    Width = 209
    Height = 89
    Caption = 'Email'
    TabOrder = 1
    object SearchByEmailRadio: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Search by email'
      TabOrder = 0
      OnClick = SearchByEmailRadioClick
    end
    object EmailEdit: TEdit
      Left = 8
      Top = 48
      Width = 193
      Height = 21
      TabOrder = 1
      OnChange = EmailEditChange
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 192
    Width = 209
    Height = 145
    Caption = 'Name'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 56
      Width = 23
      Height = 13
      Caption = 'Nick:'
    end
    object Label2: TLabel
      Left = 8
      Top = 80
      Width = 25
      Height = 13
      Caption = 'First:'
    end
    object Label3: TLabel
      Left = 8
      Top = 104
      Width = 24
      Height = 13
      Caption = 'Last:'
    end
    object SearchByNameRadio: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Search by name'
      TabOrder = 0
      OnClick = SearchByNameRadioClick
    end
    object NickEdit: TEdit
      Left = 48
      Top = 48
      Width = 153
      Height = 21
      TabOrder = 1
      OnChange = NickEditChange
    end
    object FirstEdit: TEdit
      Left = 48
      Top = 72
      Width = 153
      Height = 21
      TabOrder = 2
      OnChange = NickEditChange
    end
    object LastEdit: TEdit
      Left = 48
      Top = 96
      Width = 153
      Height = 21
      TabOrder = 3
      OnChange = NickEditChange
    end
  end
  object GroupBox4: TGroupBox
    Left = 216
    Top = 0
    Width = 561
    Height = 515
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Results'
    TabOrder = 4
    object ListView1: TListView
      Left = 2
      Top = 15
      Width = 557
      Height = 498
      Align = alClient
      Columns = <
        item
          Caption = 'Nick'
          Width = 90
        end
        item
          Caption = 'First'
          Width = 90
        end
        item
          Caption = 'Last'
          Width = 100
        end
        item
          Caption = 'Email'
          Width = 90
        end
        item
          Caption = 'UIN'
          Width = 73
        end
        item
          Caption = 'Age'
        end
        item
          Caption = 'Authorize'
          Width = 60
        end>
      ReadOnly = True
      RowSelect = True
      PopupMenu = UserSearchPopup
      SmallImages = MainForm.IconList
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 508
    Width = 778
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object GroupBox5: TGroupBox
    Left = 4
    Top = 344
    Width = 209
    Height = 105
    Caption = 'Random'
    TabOrder = 6
    object Label4: TLabel
      Left = 8
      Top = 64
      Width = 33
      Height = 13
      Caption = 'Group:'
    end
    object RandomSearchRadio: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Random search'
      TabOrder = 0
      OnClick = RandomSearchRadioClick
    end
    object RandomComboBox: TComboBox
      Left = 48
      Top = 56
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnChange = RandomComboBoxChange
    end
  end
  object AdvancedButton: TButton
    Left = 4
    Top = 452
    Width = 209
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Advanced >>'
    TabOrder = 7
    OnClick = AdvancedButtonClick
  end
  object Button1: TButton
    Left = 4
    Top = 480
    Width = 209
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Search'
    TabOrder = 3
    OnClick = Button1Click
  end
  object UserSearchPopup: TPopupMenu
    Images = MainForm.AdvIconList
    OnPopup = UserSearchPopupPopup
    Left = 144
    Top = 16
    object AddToList1: TMenuItem
      Caption = '&Add to list'
      ImageIndex = 11
      OnClick = AddToList1Click
    end
    object UsersInfo1: TMenuItem
      Caption = 'Us&er'#39's info'
      ImageIndex = 2
      OnClick = UsersInfo1Click
    end
  end
end
