object Form1: TForm1
  Left = 235
  Top = 254
  Width = 573
  Height = 340
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SXSkinImage1: TSXSkinImage
    Top = 23
    Width = 565
    Height = 283
    Align = alClient
    SkinLibrary = SXSkinLibrary1
    SkinStyle = 'Background'
    TabOrder = 0
    object Label1: TLabel
      Left = 80
      Top = 56
      Width = 384
      Height = 13
      Caption = 
        '*****  !!!!!  Do not forget to put "Skins" folder to the project' +
        ' folder !!!!!  *****'
      Transparent = True
    end
    object Label2: TLabel
      Left = 80
      Top = 80
      Width = 294
      Height = 13
      Caption = 'To see skin styles in the Delphi designer set these properties:'
      Transparent = True
    end
    object Label4: TLabel
      Left = 80
      Top = 96
      Width = 381
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
      Left = 320
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
    Width = 565
    Height = 23
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 31
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Menu = MainMenu1
    ShowCaptions = True
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 176
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
  object SXSkinLibrary1: TSXSkinLibrary
    Left = 24
    Top = 16
  end
  object SXSkinForm1: TSXSkinForm
    IconStyle = '_Selective_StdIcon'
    SkinLibrary = SXSkinLibrary1
    Left = 56
    Top = 16
  end
end
