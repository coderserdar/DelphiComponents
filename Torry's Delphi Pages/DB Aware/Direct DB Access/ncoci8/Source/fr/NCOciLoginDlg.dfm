object NCOciLoginFrm: TNCOciLoginFrm
  Left = 469
  Top = 253
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Login Oracle8'
  ClientHeight = 176
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 5
    Top = 104
    Width = 101
    Height = 13
    Caption = '&Nouv. mot de passe :'
  end
  object tbshtNewPassword: TPanel
    Left = 1
    Top = 2
    Width = 289
    Height = 143
    BevelOuter = bvNone
    TabOrder = 3
    object Label6: TLabel
      Left = 5
      Top = 48
      Width = 70
      Height = 13
      Caption = '&Mot de passe :'
      FocusControl = edtNewPassword
    end
    object Label8: TLabel
      Left = 16
      Top = 80
      Width = 58
      Height = 13
      Caption = '&Vérification :'
      FocusControl = edtVerify
    end
    object edtNewPassword: TEdit
      Left = 85
      Top = 45
      Width = 193
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
      OnChange = edtNewPasswordChange
    end
    object Memo1: TMemo
      Left = 5
      Top = 8
      Width = 268
      Height = 33
      TabStop = False
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'Mot de passe expiré. Merci de le changer.'
        'Puis retapez le pour vérification.')
      ReadOnly = True
      TabOrder = 0
    end
    object edtVerify: TEdit
      Left = 85
      Top = 77
      Width = 193
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
      OnChange = edtNewPasswordChange
    end
  end
  object btnCancel: TBitBtn
    Left = 212
    Top = 148
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object btnOk: TBitBtn
    Left = 132
    Top = 148
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
  object tbshtLogin: TPanel
    Left = 1
    Top = 1
    Width = 289
    Height = 143
    BevelOuter = bvNone
    Caption = 'Login'
    TabOrder = 0
    object pnlAuthMode: TPanel
      Left = 0
      Top = 83
      Width = 289
      Height = 26
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Label4: TLabel
        Left = 41
        Top = 3
        Width = 33
        Height = 13
        Caption = '&Mode :'
        FocusControl = cmbbxAuthMode
      end
      object cmbbxAuthMode: TComboBox
        Left = 85
        Top = 0
        Width = 193
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        Items.Strings = (
          'Défaut'
          'SysDBA'
          'SysOPER')
      end
    end
    object pnlProfile: TPanel
      Left = 0
      Top = 109
      Width = 289
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 289
        Height = 30
        Align = alClient
        Shape = bsTopLine
      end
      object Label5: TLabel
        Left = 38
        Top = 10
        Width = 35
        Height = 13
        Caption = 'P&rofile :'
        FocusControl = cmbbxProfile
      end
      object spdbttnDelProfile: TSpeedButton
        Left = 254
        Top = 7
        Width = 23
        Height = 21
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -19
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = spdbttnDelProfileClick
      end
      object cmbbxProfile: TComboBox
        Left = 84
        Top = 7
        Width = 170
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        OnClick = cmbbxProfileClick
      end
    end
    object pnlService: TPanel
      Left = 0
      Top = 56
      Width = 289
      Height = 27
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label3: TLabel
        Left = 32
        Top = 4
        Width = 42
        Height = 13
        Caption = '&Service :'
        FocusControl = cmbbxService
      end
      object cmbbxService: TComboBox
        Left = 85
        Top = 0
        Width = 193
        Height = 21
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlStd: TPanel
      Left = 0
      Top = 0
      Width = 289
      Height = 56
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 23
        Top = 6
        Width = 52
        Height = 13
        Caption = '&Utilisateur :'
        FocusControl = edtUserName
      end
      object Label2: TLabel
        Left = 4
        Top = 32
        Width = 70
        Height = 13
        Caption = '&Mot de passe :'
        FocusControl = edtPassword
      end
      object edtUserName: TEdit
        Left = 85
        Top = 3
        Width = 193
        Height = 21
        TabOrder = 0
      end
      object edtPassword: TEdit
        Left = 85
        Top = 29
        Width = 193
        Height = 21
        PasswordChar = '*'
        TabOrder = 1
      end
    end
  end
end
