object OciCompNamerSetupFrm: TOciCompNamerSetupFrm
  Left = 290
  Top = 288
  Width = 340
  Height = 243
  BorderIcons = [biSystemMenu]
  Caption = 'Navngivningsekspert valgmuligheder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 190
    Top = 187
    Width = 67
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 261
    Top = 187
    Width = 68
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkCancel
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 332
    Height = 179
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object TabSheet3: TTabSheet
      Caption = 'Generelt'
      object Label2: TLabel
        Left = 9
        Top = 7
        Width = 84
        Height = 13
        Caption = 'Omdøb genvej :'
      end
      object Label5: TLabel
        Left = 9
        Top = 49
        Width = 69
        Height = 13
        Caption = 'Rediger genvej :'
      end
      object HotKey1: THotKey
        Left = 9
        Top = 24
        Width = 142
        Height = 19
        HotKey = 16461
        InvalidKeys = [hcNone, hcShift]
        Modifiers = [hkCtrl]
        TabOrder = 0
      end
      object HotKey2: THotKey
        Left = 9
        Top = 66
        Width = 142
        Height = 19
        HotKey = 49229
        InvalidKeys = [hcNone, hcShift]
        Modifiers = [hkCtrl, hkAlt]
        TabOrder = 1
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Ordreduktion'
      object Label1: TLabel
        Left = 9
        Top = 108
        Width = 155
        Height = 13
        Caption = 'Flytbare klassenavns prefikser :'
        FocusControl = Edit1
      end
      object CheckBox1: TCheckBox
        Left = 9
        Top = 6
        Width = 235
        Height = 14
        Caption = 'Fjern AEI... i store bogstaver'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 9
        Top = 23
        Width = 235
        Height = 14
        Caption = 'Fjern AEI... i små bogstaver'
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 9
        Top = 40
        Width = 235
        Height = 14
        Caption = 'Fjern BCD... i store bogstaver'
        TabOrder = 2
      end
      object CheckBox4: TCheckBox
        Left = 9
        Top = 57
        Width = 235
        Height = 14
        Caption = 'Fjern BCD... i små bogstaver'
        TabOrder = 3
      end
      object CheckBox5: TCheckBox
        Left = 9
        Top = 91
        Width = 235
        Height = 14
        Caption = 'Prefikser case insensitive'
        TabOrder = 5
      end
      object Edit1: TEdit
        Left = 9
        Top = 124
        Width = 306
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
      end
      object CheckBox6: TCheckBox
        Left = 9
        Top = 74
        Width = 235
        Height = 14
        Caption = 'Fjern andre symboler'
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Navneformater'
      object Label3: TLabel
        Left = 9
        Top = 6
        Width = 299
        Height = 13
        Caption = '<CLASS NAME> = <format 1> ; ... ; <format N> [ ; * ]'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Memo1: TMemo
        Left = 11
        Top = 25
        Width = 306
        Height = 118
        Anchors = [akLeft, akTop, akRight, akBottom]
        PopupMenu = PopupMenu1
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 340
    Top = 105
    object N2: TMenuItem
      Caption = 'Templateenheder'
      Enabled = False
    end
    object C2: TMenuItem
      Caption = '%C      Klassenavn'
      Hint = '%C'
      OnClick = MIClick
    end
    object C1: TMenuItem
      Caption = '%C(?)  Enhedsklassenavn'
      Hint = '%C()'
      OnClick = MIClick
    end
    object T2: TMenuItem
      Caption = '%T      Brugertekst'
      Hint = '%T'
      OnClick = MIClick
    end
    object T1: TMenuItem
      Caption = '%T(?)  Brugertekst ved spørgsmål'
      Hint = '%T()'
      OnClick = MIClick
    end
    object V1: TMenuItem
      Caption = '%V(?) Enhedsværdier'
      Hint = '%V()'
      OnClick = MIClick
    end
    object N4: TMenuItem
      Caption = ' *         Undlad omdøbning'
      Hint = '*'
      OnClick = MIClick
    end
    object N5: TMenuItem
      Caption = ' ;         Format delimiter'
      Hint = ';'
      OnClick = MIClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object N3: TMenuItem
      Caption = 'Template enhedsmodifikatorer'
      Enabled = False
    end
    object U1: TMenuItem
      Caption = 'U        STORE BOGSTAVER'
      Hint = 'U'
      OnClick = MIClick
    end
    object L1: TMenuItem
      Caption = 'L        små bogstaver'
      Hint = 'L'
      OnClick = MIClick
    end
    object P1: TMenuItem
      Caption = 'P        Korrekt syntaks'
      Hint = 'P'
      OnClick = MIClick
    end
    object S1: TMenuItem
      Caption = 'S        Ordgenveje'
      Hint = 'S'
      OnClick = MIClick
    end
  end
end
