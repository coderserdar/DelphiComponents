object FormMain: TFormMain
  Left = 240
  Top = 116
  Width = 533
  Height = 519
  Caption = 'Register External File Formats'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageCtrl: TmcmImageCtrl
    Left = 0
    Top = 0
    Width = 517
    Height = 460
    Align = alClient
    BorderStyle = BS_SUNKEN
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = False
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 8
    object FileMenu: TMenuItem
      Caption = '&File'
      object OpenItem: TMenuItem
        Caption = '&Open'
        ShortCut = 16463
        OnClick = OpenItemClick
      end
      object SaveItem: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
        OnClick = SaveItemClick
      end
      object BrowseItem: TMenuItem
        Caption = 'Browse'
        ShortCut = 16450
        OnClick = BrowseItemClick
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitItemClick
      end
    end
  end
  object mcmOpenDialog: TmcmOpenDialog
    DefaultExt = 'bmp'
    HelpContext = 0
    Title = 'Open'
    Left = 48
    Top = 8
  end
  object mcmSaveDialog: TmcmSaveDialog
    DefaultExt = 'bmp'
    HelpContext = 0
    Title = 'Save'
    OptionHelpContext = 0
    OnHasOption = mcmSaveDialogHasOption
    OnShowOption = mcmSaveDialogShowOption
    Left = 80
    Top = 8
  end
end
