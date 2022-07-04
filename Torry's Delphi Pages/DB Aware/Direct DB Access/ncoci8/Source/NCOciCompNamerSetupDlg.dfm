object OciCompNamerSetupFrm: TOciCompNamerSetupFrm
  Left = 290
  Top = 288
  Width = 340
  Height = 243
  BorderIcons = [biSystemMenu]
  Caption = 'NCOCI8 Component Namer options'
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
      Caption = 'General'
      object Label2: TLabel
        Left = 9
        Top = 7
        Width = 84
        Height = 13
        Caption = 'Rename hot key :'
      end
      object Label5: TLabel
        Left = 9
        Top = 49
        Width = 69
        Height = 13
        Caption = 'Tune hot key :'
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
      Caption = 'Words reduction'
      object Label1: TLabel
        Left = 9
        Top = 108
        Width = 155
        Height = 13
        Caption = 'Removable class name prefixes :'
        FocusControl = Edit1
      end
      object CheckBox1: TCheckBox
        Left = 9
        Top = 6
        Width = 235
        Height = 14
        Caption = 'Remove AEI... in upper case'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 9
        Top = 23
        Width = 235
        Height = 14
        Caption = 'Remove AEI... in lower case'
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 9
        Top = 40
        Width = 235
        Height = 14
        Caption = 'Remove BCD... in upper case'
        TabOrder = 2
      end
      object CheckBox4: TCheckBox
        Left = 9
        Top = 57
        Width = 235
        Height = 14
        Caption = 'Remove BCD... in lower case'
        TabOrder = 3
      end
      object CheckBox5: TCheckBox
        Left = 9
        Top = 91
        Width = 235
        Height = 14
        Caption = 'Prefixes case insensitive'
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
        Caption = 'Remove other symbols'
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Name formats'
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
        Left = 9
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
      Caption = 'Template items'
      Enabled = False
    end
    object C2: TMenuItem
      Caption = '%C      Class name'
      Hint = '%C'
      OnClick = MIClick
    end
    object C1: TMenuItem
      Caption = '%C(?)  Property class name'
      Hint = '%C()'
      OnClick = MIClick
    end
    object T2: TMenuItem
      Caption = '%T      User text'
      Hint = '%T'
      OnClick = MIClick
    end
    object T1: TMenuItem
      Caption = '%T(?)  User text with prompt'
      Hint = '%T()'
      OnClick = MIClick
    end
    object V1: TMenuItem
      Caption = '%V(?) Property value'
      Hint = '%V()'
      OnClick = MIClick
    end
    object N4: TMenuItem
      Caption = ' *         Suppress renaming'
      Hint = '*'
      OnClick = MIClick
    end
    object N5: TMenuItem
      Caption = ' ;         Formats delimiter'
      Hint = ';'
      OnClick = MIClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object N3: TMenuItem
      Caption = 'Template item modificators'
      Enabled = False
    end
    object U1: TMenuItem
      Caption = 'U        UPPER CASE'
      Hint = 'U'
      OnClick = MIClick
    end
    object L1: TMenuItem
      Caption = 'L        lower case'
      Hint = 'L'
      OnClick = MIClick
    end
    object P1: TMenuItem
      Caption = 'P        Proper Case'
      Hint = 'P'
      OnClick = MIClick
    end
    object S1: TMenuItem
      Caption = 'S        Word shortcut'
      Hint = 'S'
      OnClick = MIClick
    end
  end
end
