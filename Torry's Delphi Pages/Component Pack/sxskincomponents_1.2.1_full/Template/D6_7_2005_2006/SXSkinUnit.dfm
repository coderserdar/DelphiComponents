object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Template'
  ClientHeight = 320
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 514
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SXSkinImage1: TSXSkinImage
      Top = 19
      Width = 514
      Height = 301
      Align = alClient
      SkinLibrary = SXSkinLibrary1
      SkinStyle = 'Background'
      TabOrder = 0
      object Label1: TLabel
        Left = 80
        Top = 56
        Width = 345
        Height = 13
        Caption = 
          '*****  !!!!!  Do not forget to put "Skins" folder to the project' +
          ' folder !!!!!  *****'
        Transparent = True
      end
      object Label2: TLabel
        Left = 80
        Top = 80
        Width = 287
        Height = 13
        Caption = 'To see skin styles in the Delphi designer set these properties:'
        Transparent = True
      end
      object Label4: TLabel
        Left = 80
        Top = 96
        Width = 370
        Height = 26
        Caption = 
          'SXSkinLibrary1.SkinFile to "Project_Folder\Skins\Opera_Standard\' +
          'skin.ini" or'#13#10'                                            other ' +
          'skin.ini, skin.sxs or .zip-file (zipped skin) path.'
        Transparent = True
      end
      object Label6: TLabel
        Left = 80
        Top = 128
        Width = 142
        Height = 13
        Caption = 'SXSkinLibrary1.Active to True'
        Transparent = True
      end
      object SXSkinButton1: TSXSkinButton
        Left = 304
        Top = 176
        Width = 121
        Height = 41
        Caption = 'Test Button'
        SkinLibrary = SXSkinLibrary1
        TabOrder = 0
      end
    end
    object ToolBar1: TToolBar
      Left = 0
      Top = 0
      Width = 514
      Height = 19
      AutoSize = True
      ButtonHeight = 19
      ButtonWidth = 38
      Caption = 'ToolBar1'
      List = True
      Menu = MainMenu1
      ShowCaptions = True
      TabOrder = 1
      Transparent = False
    end
  end
  object SXSkinLibrary1: TSXSkinLibrary
    Top = 24
  end
  object MainMenu1: TMainMenu
    Left = 64
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Skins1: TMenuItem
      Caption = 'Skins'
    end
  end
  object SXSkinForm1: TSXSkinForm
    IconStyle = '_Selective_StdIcon'
    SkinLibrary = SXSkinLibrary1
    Left = 32
    Top = 24
  end
end
