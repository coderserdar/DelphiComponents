object F_ZConnectionWindow: TF_ZConnectionWindow
  Left = 448
  Top = 218
  Width = 694
  Height = 337
  Caption = 'Se Connecter vers'#160' une base de travail'
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 117
    Width = 132
    Height = 13
    Caption = 'Nom de la base de donn'#233'es'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 94
    Width = 23
    Height = 13
    Caption = 'Hote'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 166
    Width = 64
    Height = 13
    Caption = 'Mot de passe'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 24
    Top = 190
    Width = 49
    Height = 13
    Caption = 'Catalogue'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 24
    Top = 142
    Width = 48
    Height = 13
    Caption = 'Utilisateur'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 24
    Top = 69
    Width = 45
    Height = 13
    Caption = 'Protocole'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label7: TLabel
    Left = 152
    Top = 16
    Width = 414
    Height = 33
    Caption = 'Sauver une base dans le fichier INI'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lb_Collation: TLabel
    Left = 24
    Top = 214
    Width = 118
    Height = 13
    Caption = 'Caract'#232'res de l'#39'interface'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object ed_Base: TEdit
    Left = 284
    Top = 112
    Width = 376
    Height = 21
    TabOrder = 0
    OnEnter = ed_BaseEnter
  end
  object ed_Host: TEdit
    Left = 284
    Top = 88
    Width = 376
    Height = 21
    TabOrder = 1
    Text = 'localhost'
    OnEnter = ed_HostEnter
  end
  object ed_Password: TEdit
    Left = 284
    Top = 160
    Width = 376
    Height = 21
    TabOrder = 2
    OnEnter = ed_PasswordEnter
  end
  object ed_Catalog: TEdit
    Left = 284
    Top = 184
    Width = 376
    Height = 21
    TabOrder = 3
    OnEnter = ed_CatalogEnter
  end
  object ed_User: TEdit
    Left = 284
    Top = 136
    Width = 376
    Height = 21
    TabOrder = 4
    OnEnter = ed_UserEnter
  end
  object Test: TJvXPButton
    Left = 56
    Top = 256
    Width = 75
    Height = 32
    Caption = 'Tester'
    TabOrder = 5
    OnClick = TestClick
  end
  object Save: TJvXPButton
    Left = 181
    Top = 256
    Width = 139
    Height = 33
    Caption = 'Sauver et quitter'
    TabOrder = 6
    OnClick = SaveClick
  end
  object quitall: TJvXPButton
    Left = 501
    Top = 256
    Width = 107
    Height = 32
    Caption = 'Tout quitter'
    TabOrder = 7
    OnClick = quitallClick
  end
  object quit: TJvXPButton
    Left = 365
    Top = 256
    Width = 91
    Height = 33
    Caption = 'Quitter'
    TabOrder = 8
    OnClick = quitClick
  end
  object cbx_Protocol: TComboBox
    Left = 284
    Top = 61
    Width = 376
    Height = 21
    ItemHeight = 13
    MaxLength = 65535
    TabOrder = 9
  end
  object ed_Collation: TEdit
    Left = 284
    Top = 208
    Width = 376
    Height = 21
    TabOrder = 10
    Text = 'latin1'
    OnEnter = ed_CollationEnter
  end
end
