object ChangePasswordForm: TChangePasswordForm
  Left = 167
  Top = 176
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ChangePasswordForm'
  ClientHeight = 140
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 359
    Height = 140
    Align = alClient
  end
  object LabAlias: TLabel
    Left = 16
    Top = 8
    Width = 43
    Height = 15
    Caption = 'LabAlias'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object LabTable: TLabel
    Left = 16
    Top = 56
    Width = 45
    Height = 15
    Caption = 'LabTable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object LabNewPassword: TLabel
    Left = 208
    Top = 8
    Width = 88
    Height = 15
    Caption = 'LabNewPassword'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object LabMaskOfPassword: TLabel
    Left = 208
    Top = 48
    Width = 145
    Height = 33
    AutoSize = False
    Caption = 'LabMaskOfPassword'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object LabConfirmPassword: TLabel
    Left = 208
    Top = 88
    Width = 105
    Height = 15
    Caption = 'LabConfirmPassword'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
  end
  object EditAlias: TComboBox
    Left = 16
    Top = 24
    Width = 177
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnChange = EditAliasChange
  end
  object EditTableName: TComboBox
    Left = 16
    Top = 72
    Width = 177
    Height = 21
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnChange = EditTableNameChange
  end
  object BitBtnChangePassword: TBitBtn
    Left = 24
    Top = 104
    Width = 129
    Height = 25
    Cursor = crHandPoint
    Caption = 'BitBtnChangePassword'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -12
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = BitBtnChangePasswordClick
  end
  object EditPassword: TEdit
    Left = 208
    Top = 24
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object EditPassword1: TEdit
    Left = 208
    Top = 104
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 4
  end
  object Table: TTable
    Exclusive = True
    Left = 88
    Top = 48
  end
end
