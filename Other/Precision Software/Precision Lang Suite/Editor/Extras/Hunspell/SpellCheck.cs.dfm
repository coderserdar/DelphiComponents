object PSForm: TPSForm
  Left = 275
  Top = 111
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Kontrola pravopisu'
  ClientHeight = 417
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pBtns: TPanel
    Left = 0
    Top = 57
    Width = 439
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object sbAbort: TButton
      Left = 340
      Top = 8
      Width = 99
      Height = 25
      Cancel = True
      Caption = 'P'#345'eru'#353'it'
      TabOrder = 0
    end
    object cbAutoClose: TCheckBox
      Left = 0
      Top = 13
      Width = 329
      Height = 17
      Caption = 'Zav'#345#237't dialog po dokon'#269'en'#237' kontroly'
      TabOrder = 1
    end
  end
  object pSuggest: TPanel
    Left = 0
    Top = 90
    Width = 439
    Height = 250
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 0
      Top = 10
      Width = 60
      Height = 13
      Caption = 'Nenalezeno:'
    end
    object Label2: TLabel
      Left = 0
      Top = 130
      Width = 39
      Height = 13
      Caption = 'N'#225'vrhy:'
    end
    object Panel1: TPanel
      Left = 336
      Top = 0
      Width = 103
      Height = 250
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object sbSkip: TButton
        Left = 4
        Top = 28
        Width = 99
        Height = 25
        Caption = '&P'#345'esko'#269'it'
        Default = True
        TabOrder = 0
      end
      object sbSkipAll: TButton
        Left = 4
        Top = 60
        Width = 99
        Height = 25
        Caption = 'P'#345'es&ko'#269'it v'#353'e'
        TabOrder = 1
      end
      object sbAdd: TButton
        Left = 4
        Top = 92
        Width = 99
        Height = 25
        Caption = 'P'#345'idat &do slovn'#237'ku'
        TabOrder = 2
      end
      object sbReplace: TButton
        Left = 4
        Top = 148
        Width = 99
        Height = 25
        Caption = 'Nah&radit'
        TabOrder = 3
      end
      object sbReplaceAll: TButton
        Left = 4
        Top = 180
        Width = 99
        Height = 25
        Caption = 'Nahradit &v'#353'e'
        TabOrder = 4
      end
    end
    object mCurrent: TMemo
      Left = 0
      Top = 28
      Width = 333
      Height = 89
      HideSelection = False
      TabOrder = 0
      WantReturns = False
    end
    object lbSuggest: TListBox
      Left = 0
      Top = 148
      Width = 333
      Height = 97
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object pProgress: TPanel
    Left = 0
    Top = 0
    Width = 439
    Height = 57
    Align = alTop
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 2
    object lbStatus: TLabel
      Left = 10
      Top = 8
      Width = 143
      Height = 13
      Caption = 'Prob'#237'h'#225' kontrola pravopisu ...'
    end
    object pgBar: TProgressBar
      Left = 10
      Top = 27
      Width = 415
      Height = 16
      Align = alBottom
      Smooth = True
      TabOrder = 0
    end
  end
  object mConsts: TMemo
    Left = 0
    Top = 340
    Width = 439
    Height = 77
    Align = alTop
    Lines.Strings = (
      'Slovn'#237'k nebyl nalezen!'
      'Nen'#237' co kontrolovat!'
      'Knihovna pro kontrolu pravopisu nebyla nalezena!'
      'Zav'#345#237't'
      'Dokon'#269'eno.')
    TabOrder = 3
    Visible = False
  end
  object TimerStart: TTimer
    Enabled = False
    Interval = 50
  end
  object TimerNextWord: TTimer
    Enabled = False
    Interval = 1
  end
  object TimerNextItem: TTimer
    Enabled = False
    Interval = 1
  end
  object TimerNextLang: TTimer
    Enabled = False
    Interval = 1
  end
end
