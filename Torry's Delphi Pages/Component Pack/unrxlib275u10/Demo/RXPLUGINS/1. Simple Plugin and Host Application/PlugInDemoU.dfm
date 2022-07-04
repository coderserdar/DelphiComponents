object Form1: TForm1
  Left = 353
  Top = 148
  Caption = 'Sample Plugin Host Application'
  ClientHeight = 250
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    398
    250)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 4
    Top = 152
    Width = 60
    Height = 13
    Caption = 'Load Status:'
  end
  object clbPlugins: TListBox
    Left = 4
    Top = 36
    Width = 189
    Height = 109
    ItemHeight = 13
    TabOrder = 0
    OnClick = clbPluginsClick
    OnDblClick = clbPluginsDblClick
  end
  object lbStatus: TListBox
    Left = 4
    Top = 168
    Width = 189
    Height = 87
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitHeight = 96
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 29
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object Panel2: TPanel
    Left = 200
    Top = 36
    Width = 198
    Height = 219
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    TabOrder = 3
    ExplicitHeight = 228
    DesignSize = (
      198
      219)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 13
      Caption = 'Author: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object labAuthor: TLabel
      Left = 52
      Top = 8
      Width = 138
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 69
      Height = 13
      Caption = 'Description:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object labDescription: TLabel
      Left = 8
      Top = 48
      Width = 182
      Height = 147
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
      ExplicitHeight = 156
    end
    object Label3: TLabel
      Left = 1
      Top = 201
      Width = 196
      Height = 17
      Align = alBottom
      Alignment = taCenter
      AutoSize = False
      Caption = 'Double-click the plugin to configure.'
      ExplicitTop = 210
    end
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 44
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Plugin1: TMenuItem
      Caption = '&Plug-in'
      object SendMessagetoPlugins1: TMenuItem
        Caption = '&Send Message to Plugins'
        OnClick = SendMessagetoPlugins1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
  object RxPluginManager1: TRxPluginManager
    Extension = 'dll'
    PluginKind = plgDLL
    OnBeforeLoad = RxPluginManager1BeforeLoad
    OnNewCommand = RxPluginManager1NewCommand
    OnAfterLoad = RxPluginManager1AfterLoad
    Left = 48
    Top = 40
  end
end
