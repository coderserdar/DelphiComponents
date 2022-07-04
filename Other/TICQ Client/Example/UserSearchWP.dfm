object UserSearchWPForm: TUserSearchWPForm
  Left = 246
  Top = 122
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Advanced search'
  ClientHeight = 240
  ClientWidth = 651
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
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 649
    Height = 87
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 65
      Height = 13
      Caption = 'First Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 36
      Width = 35
      Height = 13
      Caption = 'Email:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 60
      Width = 62
      Height = 13
      Caption = 'City/State:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 208
      Top = 12
      Width = 65
      Height = 13
      Caption = 'Last Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 208
      Top = 36
      Width = 27
      Height = 13
      Caption = 'Age:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 208
      Top = 60
      Width = 48
      Height = 13
      Caption = 'Country:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 424
      Top = 12
      Width = 67
      Height = 13
      Caption = 'Nick Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 424
      Top = 36
      Width = 46
      Height = 13
      Caption = 'Gender:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label9: TLabel
      Left = 424
      Top = 60
      Width = 61
      Height = 13
      Caption = 'Language:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object FirstNameEdit: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object EmailEdit: TEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object CityEdit: TEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object lastNameEdit: TEdit
      Left = 296
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object AgeCB: TComboBox
      Left = 296
      Top = 32
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 4
    end
    object CountryCB: TComboBox
      Left = 296
      Top = 56
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 5
    end
    object NickNameEdit: TEdit
      Left = 520
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 6
    end
    object GenderCB: TComboBox
      Left = 520
      Top = 32
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 7
    end
    object LanguageCB: TComboBox
      Left = 520
      Top = 56
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 8
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 88
    Width = 212
    Height = 75
    TabOrder = 1
    object Label10: TLabel
      Left = 8
      Top = 17
      Width = 49
      Height = 13
      Caption = 'Company:'
    end
    object Label11: TLabel
      Left = 8
      Top = 49
      Width = 61
      Height = 13
      Caption = 'Department:'
    end
    object CompanyEdit: TEdit
      Left = 80
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object DepartmentEdit: TEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 218
    Top = 88
    Width = 212
    Height = 75
    TabOrder = 2
    object Label12: TLabel
      Left = 8
      Top = 17
      Width = 58
      Height = 13
      Caption = 'Occupation:'
    end
    object Label13: TLabel
      Left = 8
      Top = 49
      Width = 41
      Height = 13
      Caption = 'Position:'
    end
    object PositionEdit: TEdit
      Left = 78
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object OccupationCB: TComboBox
      Left = 78
      Top = 13
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object GroupBox4: TGroupBox
    Left = 437
    Top = 88
    Width = 212
    Height = 75
    TabOrder = 3
    object Label16: TLabel
      Left = 8
      Top = 9
      Width = 21
      Height = 13
      Caption = 'Past'
    end
    object Label18: TLabel
      Left = 8
      Top = 22
      Width = 49
      Height = 13
      Caption = 'Affiliation:'
    end
    object Label19: TLabel
      Left = 8
      Top = 41
      Width = 51
      Height = 13
      Caption = 'Keywords:'
    end
    object Label20: TLabel
      Left = 8
      Top = 54
      Width = 56
      Height = 13
      Caption = '(e.g. Navy)'
    end
    object PastAffilKeyWordsEdit: TEdit
      Left = 83
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object PastAffilCB: TComboBox
      Left = 83
      Top = 14
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object GroupBox5: TGroupBox
    Left = 0
    Top = 164
    Width = 212
    Height = 75
    TabOrder = 4
    object Label15: TLabel
      Left = 8
      Top = 22
      Width = 28
      Height = 13
      Caption = 'Type:'
    end
    object Label21: TLabel
      Left = 8
      Top = 41
      Width = 51
      Height = 13
      Caption = 'Keywords:'
    end
    object Label22: TLabel
      Left = 8
      Top = 54
      Width = 45
      Height = 13
      Caption = '(e.g. UN)'
    end
    object Label14: TLabel
      Left = 8
      Top = 9
      Width = 61
      Height = 13
      Caption = 'Organization'
    end
    object OrganizationKeyWordsEdit: TEdit
      Left = 80
      Top = 45
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object OrganizationCB: TComboBox
      Left = 80
      Top = 14
      Width = 121
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object GroupBox6: TGroupBox
    Left = 218
    Top = 164
    Width = 431
    Height = 75
    TabOrder = 5
    object Label17: TLabel
      Left = 8
      Top = 17
      Width = 115
      Height = 13
      Caption = 'Search By Keyword:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object KeyWordEdit: TEdit
      Left = 128
      Top = 13
      Width = 295
      Height = 21
      TabOrder = 0
    end
    object OnlineCB: TCheckBox
      Left = 8
      Top = 45
      Width = 137
      Height = 17
      Caption = 'Search Only Online Users'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 296
      Top = 40
      Width = 129
      Height = 25
      Caption = 'Search!'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
end
