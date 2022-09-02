object frmUsrModify: TfrmUsrModify
  Left = 192
  Top = 107
  BorderStyle = bsToolWindow
  Caption = 'User Maintenance'
  ClientHeight = 280
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 45
    Top = 70
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
  object Label4: TLabel
    Left = 32
    Top = 100
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
  object Label2: TLabel
    Left = 30
    Top = 128
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
    Left = 29
    Top = 160
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
  object edtID: TEdit
    Left = 96
    Top = 64
    Width = 193
    Height = 21
    TabOrder = 0
  end
  object edtPasswd: TEdit
    Left = 96
    Top = 96
    Width = 193
    Height = 21
    TabOrder = 1
  end
  object edtFirst: TEdit
    Left = 96
    Top = 128
    Width = 193
    Height = 21
    TabOrder = 2
  end
  object edtLast: TEdit
    Left = 96
    Top = 160
    Width = 193
    Height = 21
    TabOrder = 3
  end
  object chkDelete: TCheckBox
    Left = 328
    Top = 160
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
  object chkUpdate: TCheckBox
    Left = 328
    Top = 136
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
  object chkInsert: TCheckBox
    Left = 328
    Top = 112
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
  object chkRead: TCheckBox
    Left = 328
    Top = 88
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
  object chkAdmin: TCheckBox
    Left = 328
    Top = 64
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
  object pnlButtons: TPanel
    Left = 0
    Top = 234
    Width = 403
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    object btnOK: TButton
      Left = 93
      Top = 3
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 197
      Top = 3
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
