object CnAAFontDlg: TCnAAFontDlg
  Left = 172
  Top = 130
  BorderStyle = bsDialog
  Caption = '���ƯS�Ħr��'
  ClientHeight = 249
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = CHINESEBIG5_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '�ө���'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 8
    Top = 200
    Width = 249
    Height = 43
    AutoSize = True
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 7
  end
  object gbShadow: TGroupBox
    Left = 8
    Top = 8
    Width = 129
    Height = 185
    Caption = '���v(&S)'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 64
      Width = 42
      Height = 12
      Caption = '�ҽk��:'
    end
    object Label2: TLabel
      Left = 8
      Top = 96
      Width = 54
      Height = 12
      Caption = '���z����:'
    end
    object Label3: TLabel
      Left = 8
      Top = 128
      Width = 54
      Height = 12
      Caption = '��������:'
    end
    object Label4: TLabel
      Left = 8
      Top = 160
      Width = 54
      Height = 12
      Caption = '��������:'
    end
    object spShadow: TShape
      Left = 64
      Top = 37
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label5: TLabel
      Left = 8
      Top = 40
      Width = 54
      Height = 12
      Caption = '���v�C��:'
    end
    object cbShadow: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = '��ܳ��v'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object seShadowBlur: TCnSpinEdit
      Left = 64
      Top = 60
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seShadowAlpha: TCnSpinEdit
      Left = 64
      Top = 92
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seOffsetX: TCnSpinEdit
      Left = 64
      Top = 124
      Width = 57
      Height = 21
      MaxLength = 2
      MaxValue = 20
      MinValue = -20
      TabOrder = 3
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seOffsetY: TCnSpinEdit
      Left = 64
      Top = 156
      Width = 57
      Height = 21
      MaxLength = 2
      MaxValue = 20
      MinValue = -20
      TabOrder = 4
      Value = 0
      OnChange = seShadowBlurClick
    end
  end
  object gbGradual: TGroupBox
    Left = 144
    Top = 8
    Width = 113
    Height = 185
    Caption = '����(&G)'
    TabOrder = 1
    object spStartColor: TShape
      Left = 64
      Top = 33
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label10: TLabel
      Left = 8
      Top = 36
      Width = 54
      Height = 12
      Caption = '�_�l�C��:'
    end
    object spEndColor: TShape
      Left = 64
      Top = 54
      Width = 16
      Height = 16
      OnMouseDown = spShadowMouseDown
    end
    object Label6: TLabel
      Left = 8
      Top = 57
      Width = 54
      Height = 12
      Caption = '�����C��:'
    end
    object cbGradual: TCheckBox
      Left = 8
      Top = 16
      Width = 100
      Height = 17
      Caption = '���\����ø�s'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object rbLeftToRight: TRadioButton
      Left = 8
      Top = 75
      Width = 100
      Height = 17
      Caption = '�q����k����'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = seShadowBlurClick
    end
    object rbRightToLeft: TRadioButton
      Left = 8
      Top = 92
      Width = 100
      Height = 17
      Caption = '�q�k�쥪����'
      TabOrder = 2
      OnClick = seShadowBlurClick
    end
    object rbTopToBottom: TRadioButton
      Left = 8
      Top = 109
      Width = 100
      Height = 17
      Caption = '�q�W��U����'
      TabOrder = 3
      OnClick = seShadowBlurClick
    end
    object rbCenterToLR: TRadioButton
      Left = 8
      Top = 142
      Width = 100
      Height = 17
      Caption = '�q�����쥪�k'
      TabOrder = 5
      OnClick = seShadowBlurClick
    end
    object rbBottomToTop: TRadioButton
      Left = 8
      Top = 125
      Width = 100
      Height = 17
      Caption = '�q�U��W����'
      TabOrder = 4
      OnClick = seShadowBlurClick
    end
    object rbCenterToTB: TRadioButton
      Left = 8
      Top = 159
      Width = 100
      Height = 17
      Caption = '�q������W�U'
      TabOrder = 6
      OnClick = seShadowBlurClick
    end
  end
  object gbTexture: TGroupBox
    Left = 264
    Top = 8
    Width = 113
    Height = 185
    Caption = '�r�鯾�z(&T)'
    TabOrder = 2
    object cbTexture: TCheckBox
      Left = 8
      Top = 16
      Width = 100
      Height = 17
      Caption = '���\�r�鯾�z'
      TabOrder = 0
      OnClick = cbShadowClick
    end
    object rbTile: TRadioButton
      Left = 8
      Top = 40
      Width = 50
      Height = 17
      Caption = '���Q'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = seShadowBlurClick
    end
    object rbStretched: TRadioButton
      Left = 56
      Top = 40
      Width = 50
      Height = 17
      Caption = '�Y��'
      TabOrder = 2
      OnClick = seShadowBlurClick
    end
    object rbCenter: TRadioButton
      Left = 8
      Top = 61
      Width = 50
      Height = 17
      Caption = '����'
      TabOrder = 3
      OnClick = seShadowBlurClick
    end
    object rbNormal: TRadioButton
      Left = 56
      Top = 61
      Width = 50
      Height = 17
      Caption = '�q�{'
      TabOrder = 4
      OnClick = seShadowBlurClick
    end
    object btnOpenPic: TButton
      Left = 8
      Top = 104
      Width = 97
      Height = 25
      Caption = '��ܹϤ�(&P)...'
      TabOrder = 5
      OnClick = btnOpenPicClick
    end
    object btnClearPic: TButton
      Left = 8
      Top = 144
      Width = 97
      Height = 25
      Caption = '�M���Ϥ�(&L)'
      TabOrder = 6
      OnClick = btnClearPicClick
    end
  end
  object gbOther: TGroupBox
    Left = 384
    Top = 8
    Width = 129
    Height = 185
    Caption = '�䥦�ĪG(&O)'
    TabOrder = 3
    object Label7: TLabel
      Left = 8
      Top = 20
      Width = 42
      Height = 12
      Caption = '�ҽk��:'
    end
    object Label8: TLabel
      Left = 8
      Top = 43
      Width = 54
      Height = 12
      Caption = '���z����:'
    end
    object Label9: TLabel
      Left = 8
      Top = 89
      Width = 42
      Height = 12
      Caption = '���n�I:'
    end
    object Label11: TLabel
      Left = 8
      Top = 112
      Width = 54
      Height = 12
      Caption = '�Q�q�ĪG:'
    end
    object Label12: TLabel
      Left = 8
      Top = 66
      Width = 54
      Height = 12
      Caption = '���ਤ��:'
    end
    object cbOutline: TCheckBox
      Left = 8
      Top = 131
      Width = 100
      Height = 17
      Caption = '�����ĪG'
      TabOrder = 5
      OnClick = seShadowBlurClick
    end
    object seBlur: TCnSpinEdit
      Left = 64
      Top = 16
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seAlpha: TCnSpinEdit
      Left = 64
      Top = 39
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 100
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seNoise: TCnSpinEdit
      Left = 64
      Top = 85
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seSpray: TCnSpinEdit
      Left = 64
      Top = 108
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 255
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = seShadowBlurClick
    end
    object seAngle: TCnSpinEdit
      Left = 64
      Top = 62
      Width = 57
      Height = 21
      MaxLength = 3
      MaxValue = 360
      MinValue = -360
      TabOrder = 2
      Value = 0
      OnChange = seShadowBlurClick
    end
    object cbHorzMirror: TCheckBox
      Left = 8
      Top = 147
      Width = 100
      Height = 17
      Caption = '������H�ĪG'
      TabOrder = 6
      OnClick = seShadowBlurClick
    end
    object cbVertMirror: TCheckBox
      Left = 8
      Top = 163
      Width = 100
      Height = 17
      Caption = '������H�ĪG'
      TabOrder = 7
      OnClick = seShadowBlurClick
    end
  end
  object btnFont: TButton
    Left = 264
    Top = 216
    Width = 75
    Height = 25
    Caption = '�r��(&F)...'
    TabOrder = 4
    OnClick = btnFontClick
  end
  object btnOK: TButton
    Left = 352
    Top = 216
    Width = 75
    Height = 25
    Caption = '�T�w(&O)'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 432
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = '����(&C)'
    ModalResult = 2
    TabOrder = 6
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 24
    Top = 208
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 56
    Top = 208
  end
  object ColorDialog: TColorDialog
    Left = 88
    Top = 208
  end
end
