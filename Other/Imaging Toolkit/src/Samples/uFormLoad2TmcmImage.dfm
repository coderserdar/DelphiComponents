object Form1: TForm1
  Left = 207
  Top = 122
  Width = 410
  Height = 400
  Caption = 'Load2TmcmImage'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageCtrl: TmcmImageCtrl
    Left = 0
    Top = 0
    Width = 402
    Height = 346
    Align = alClient
    Center = True
    Flat = True
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 24
    object FileMenu: TMenuItem
      Caption = '&File'
      object OpenItem: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = OpenItemClick
      end
      object SaveItem: TMenuItem
        Caption = 'Save As...'
        OnClick = SaveItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitItemClick
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      OnClick = EditMenuClick
      object CopyItem: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = CopyItemClick
      end
      object PasteItem: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = PasteItemClick
      end
    end
  end
  object OpenDialog: TmcmOpenDialog
    HelpContext = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofFileMustExist, ofNoNetworkButton]
    Title = 'Open Image'
    ViewStyle = vsPreview
    Left = 96
    Top = 24
  end
  object SaveDialog: TmcmSaveDialog
    HelpContext = 0
    Title = 'Save'
    OptionHelpContext = 0
    Left = 56
    Top = 24
  end
end
