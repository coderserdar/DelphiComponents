object frmSingle: TfrmSingle
  Left = 328
  Top = 184
  Width = 506
  Height = 454
  Caption = 'FSSQL Remote Administration [One2one]'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 465
    Height = 233
    Caption = ' User '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 37
      Top = 38
      Width = 33
      Height = 13
      Caption = 'UserID'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 22
      Top = 96
      Width = 48
      Height = 13
      Caption = 'First name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 21
      Top = 128
      Width = 49
      Height = 13
      Caption = 'Last name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 24
      Top = 68
      Width = 46
      Height = 13
      Caption = 'Password'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edtID: TEdit
      Left = 88
      Top = 32
      Width = 193
      Height = 21
      TabOrder = 0
    end
    object edtPasswd: TEdit
      Left = 88
      Top = 64
      Width = 193
      Height = 21
      TabOrder = 1
    end
    object edtFirst: TEdit
      Left = 88
      Top = 96
      Width = 193
      Height = 21
      TabOrder = 2
    end
    object edtLast: TEdit
      Left = 88
      Top = 128
      Width = 193
      Height = 21
      TabOrder = 3
    end
    object chkAdmin: TCheckBox
      Left = 344
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Admin'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object chkRead: TCheckBox
      Left = 344
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Read'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object chkInsert: TCheckBox
      Left = 344
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Insert'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object chkUpdate: TCheckBox
      Left = 344
      Top = 104
      Width = 97
      Height = 17
      Caption = 'Update'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
    end
    object chkDelete: TCheckBox
      Left = 344
      Top = 128
      Width = 97
      Height = 17
      Caption = 'Delete'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object Panel1: TPanel
      Left = 2
      Top = 176
      Width = 461
      Height = 55
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 9
      object btnGet: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Get'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = btnGetClick
      end
      object btnAdd: TButton
        Left = 104
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Add'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnUpdate: TButton
        Left = 192
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Update'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = btnUpdateClick
      end
      object btnDelete: TButton
        Left = 280
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Delete'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = btnDeleteClick
      end
      object btnCheck: TButton
        Left = 368
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Check'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = btnCheckClick
      end
    end
  end
  object rgSecure: TRadioGroup
    Left = 16
    Top = 264
    Width = 193
    Height = 105
    Caption = ' Server security '
    Items.Strings = (
      'ON'
      'OFF')
    TabOrder = 1
  end
  object btnToggle: TButton
    Left = 88
    Top = 296
    Width = 75
    Height = 57
    Caption = 'Toggle'
    TabOrder = 2
    OnClick = btnToggleClick
  end
  object GroupBox2: TGroupBox
    Left = 224
    Top = 264
    Width = 257
    Height = 105
    Caption = ' Userlist '
    TabOrder = 3
    object lblUserCount: TLabel
      Left = 40
      Top = 32
      Width = 60
      Height = 13
      Caption = 'lblUserCount'
    end
    object btnClear: TButton
      Left = 136
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Clear all'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnCount: TButton
      Left = 38
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Count'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnCountClick
    end
  end
  object pnlButton: TPanel
    Left = 0
    Top = 379
    Width = 498
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btnClose: TButton
      Left = 211
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 0
    end
  end
end
