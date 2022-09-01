object Form1: TForm1
  Left = 295
  Top = 152
  BorderStyle = bsDialog
  Caption = 'Demo TPSCColorEdit'
  ClientHeight = 475
  ClientWidth = 383
  Color = clBtnFace
  Constraints.MinHeight = 504
  Constraints.MinWidth = 389
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    383
    475)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 136
    Top = 429
    Width = 57
    Height = 33
    Anchors = [akRight, akBottom]
  end
  object Label4: TLabel
    Left = 14
    Top = 436
    Width = 111
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Selected color is:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PSCColorEdit1: TPSCColorEdit
    Left = 8
    Top = 8
    Width = 137
    Height = 24
    ThemeName = 'WindowsXP'
    OnChange = PSCColorEdit1Change
    HighlightColor = clBtnShadow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 208
    Top = 8
    Width = 169
    Height = 457
    Anchors = [akTop, akRight, akBottom]
    BorderStyle = bsSingle
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 216
      Width = 101
      Height = 13
      Caption = 'Show Color Sections:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 26
      Height = 13
      Caption = 'Style:'
    end
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 64
      Height = 13
      Caption = 'ThemeName:'
    end
    object cbTrackColor: TCheckBox
      Left = 8
      Top = 96
      Width = 97
      Height = 17
      Caption = 'TrackColor'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbTrackColorClick
    end
    object cbDisplayNames: TCheckBox
      Left = 8
      Top = 120
      Width = 97
      Height = 17
      Caption = 'DisplayNames'
      TabOrder = 1
      OnClick = cbDisplayNamesClick
    end
    object cbEnabled: TCheckBox
      Left = 8
      Top = 144
      Width = 97
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbEnabledClick
    end
    object cbHighlightActive: TCheckBox
      Left = 8
      Top = 168
      Width = 97
      Height = 17
      Caption = 'HighlightActive'
      TabOrder = 3
      OnClick = cbHighlightActiveClick
    end
    object cbShowDefaultAuto: TCheckBox
      Left = 17
      Top = 380
      Width = 121
      Height = 17
      Caption = 'Default Auto'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowDefaultNone: TCheckBox
      Left = 17
      Top = 356
      Width = 121
      Height = 17
      Caption = 'Default None'
      TabOrder = 8
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowCustomColors: TCheckBox
      Left = 17
      Top = 404
      Width = 121
      Height = 17
      Caption = 'Custom Colors'
      TabOrder = 9
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowStdColors: TCheckBox
      Left = 17
      Top = 284
      Width = 121
      Height = 17
      Caption = 'Standard Colors'
      TabOrder = 10
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowDocColors: TCheckBox
      Left = 17
      Top = 308
      Width = 121
      Height = 17
      Caption = 'Document Colors'
      TabOrder = 11
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowWinColors: TCheckBox
      Left = 17
      Top = 332
      Width = 121
      Height = 17
      Caption = 'Windows Colors'
      TabOrder = 12
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowBkgndColors: TCheckBox
      Left = 17
      Top = 260
      Width = 121
      Height = 17
      Caption = 'Bkgnd Colors'
      Checked = True
      State = cbChecked
      TabOrder = 13
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowFontColors: TCheckBox
      Left = 17
      Top = 236
      Width = 121
      Height = 17
      Caption = 'Font Colors'
      Checked = True
      State = cbChecked
      TabOrder = 14
      OnClick = cbShowDefaultAutoClick
    end
    object cbShowMoreColors: TCheckBox
      Left = 17
      Top = 428
      Width = 121
      Height = 17
      Caption = 'More Colors'
      TabOrder = 15
      OnClick = cbShowDefaultAutoClick
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 64
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 6
      OnChange = ComboBox1Change
      Items.Strings = (
        'Custom'
        'WordBk'
        'WordFont'
        'FrontPage'
        'SysColors'
        'FrontPageBtn')
    end
    object ComboBox2: TComboBox
      Left = 8
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = ComboBox2Change
      Items.Strings = (
        '3D'
        'Flat'
        'Word2000'
        'WindowsXP'
        'WordXP')
    end
    object PSCColorEdit2: TPSCColorEdit
      Left = 27
      Top = 188
      Width = 121
      Height = 21
      ThemeName = 'WindowsXP'
      OnChange = PSCColorEdit2Change
      SelectedColor = clSilver
      HighlightActive = True
      HighlightColor = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Enabled = False
      ParentFont = False
      TabOrder = 4
    end
  end
end
