object FormAutoHide: TFormAutoHide
  Left = 455
  Top = 497
  Width = 437
  Height = 248
  BorderIcons = [biSystemMenu]
  Caption = 'AutoHide Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 339
    Height = 224
    ActivePage = TabInclude
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabInclude: TTabSheet
      Caption = '&Windows'
      object Panel2: TPanel
        Left = 0
        Top = 170
        Width = 331
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object ButtonAdd: TButton
          Left = 7
          Top = 1
          Width = 76
          Height = 25
          Caption = '&Add'
          TabOrder = 0
          OnClick = ButtonAddClick
        end
        object ButtonEdit: TButton
          Left = 87
          Top = 1
          Width = 76
          Height = 25
          Caption = '&Edit'
          Enabled = False
          TabOrder = 1
          OnClick = ButtonEditClick
        end
        object ButtonDel: TButton
          Left = 250
          Top = 1
          Width = 76
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '&Delete'
          TabOrder = 2
          OnClick = ButtonDelClick
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 331
        Height = 170
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 2
        TabOrder = 0
        object LBoxClassname: TListBox
          Left = 2
          Top = 2
          Width = 327
          Height = 166
          Align = alClient
          ItemHeight = 16
          MultiSelect = True
          Style = lbOwnerDrawFixed
          TabOrder = 0
          OnClick = LBoxClassnameClick
          OnDblClick = LBoxClassnameDblClick
          OnDrawItem = LBoxClassnameDrawItem
          OnKeyUp = LBoxClassnameKeyUp
        end
      end
    end
    object TabSheetOptions: TTabSheet
      Caption = '&Options'
      ImageIndex = 2
      object Label1: TLabel
        Left = 9
        Top = 11
        Width = 83
        Height = 13
        Caption = 'Update inter&val in'
      end
      object Label2: TLabel
        Left = 158
        Top = 12
        Width = 54
        Height = 13
        Caption = 'miliseconds'
      end
      object CBoxCaptionButton: TCheckBox
        Left = 9
        Top = 55
        Width = 170
        Height = 17
        Caption = '&Button on the captured caption'
        TabOrder = 2
        OnClick = OnChangeAnything
      end
      object CBoxShutdown: TCheckBox
        Left = 9
        Top = 90
        Width = 196
        Height = 17
        Caption = '&Shutdown engine on Delphi closing'
        TabOrder = 4
        OnClick = OnChangeAnything
      end
      object CBoxModal: TCheckBox
        Left = 9
        Top = 38
        Width = 228
        Height = 17
        Caption = 'Restore window size on a modal &dialog box'
        TabOrder = 1
        OnClick = OnChangeAnything
      end
      object CBoxSound: TCheckBox
        Left = 9
        Top = 107
        Width = 123
        Height = 17
        Caption = 'S&ound on executing'
        TabOrder = 5
        OnClick = OnChangeAnything
      end
      object CBoxInit: TCheckBox
        Left = 9
        Top = 73
        Width = 185
        Height = 17
        Caption = '&Initialize engine on Delphi loading'
        TabOrder = 3
        OnClick = OnChangeAnything
      end
      object EditInterval: TEdit
        Left = 98
        Top = 9
        Width = 43
        Height = 21
        TabOrder = 0
        Text = '750'
        OnExit = EditIntervalExit
        OnKeyPress = EditIntervalKeyPress
      end
      object UpDownInterval: TUpDown
        Left = 141
        Top = 9
        Width = 13
        Height = 21
        Associate = EditInterval
        Min = 100
        Max = 10000
        Increment = 50
        Position = 750
        TabOrder = 6
        Thousands = False
        Wrap = False
        OnChanging = UpDownIntervalChanging
      end
    end
  end
  object PanelRight: TPanel
    Left = 339
    Top = 0
    Width = 90
    Height = 224
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonLoad: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Load'
      Default = True
      TabOrder = 0
      OnClick = ButtonLoadClick
    end
    object ButtonCancel: TButton
      Left = 8
      Top = 63
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 2
      OnClick = ButtonCancelClick
    end
    object ButtonUpdate: TButton
      Left = 8
      Top = 35
      Width = 75
      Height = 25
      Caption = '&Update'
      TabOrder = 1
      OnClick = ButtonUpdateClick
    end
    object Panel1: TPanel
      Left = 0
      Top = 192
      Width = 90
      Height = 32
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object ButtonAbout: TButton
        Left = 8
        Top = 3
        Width = 75
        Height = 25
        Caption = '&About'
        TabOrder = 0
        OnClick = ButtonAboutClick
      end
    end
  end
end
