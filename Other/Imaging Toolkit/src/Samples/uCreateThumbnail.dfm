object Form1: TForm1
  Left = 264
  Top = 132
  Width = 404
  Height = 371
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mcmImageCtrl: TmcmImageCtrl
    Left = 8
    Top = 8
    Width = 300
    Height = 300
    BorderStyle = BS_BUMP
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = True
  end
  object mcmImageThumb: TmcmImageCtrl
    Left = 320
    Top = 8
    Width = 68
    Height = 68
    BorderStyle = BS_BUMP
    Center = True
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleToFit = False
  end
  object OpenDialog: TmcmOpenDialog
    FileName = '*.*'
    HelpContext = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofFileMustExist, ofNoNetworkButton]
    Title = 'Open Image'
    ViewStyle = vsPreview
    Left = 96
    Top = 24
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 24
    object FileMenu: TMenuItem
      Caption = 'File'
      object OpenItem: TMenuItem
        Caption = 'Open...'
        OnClick = OpenItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'Exit'
        OnClick = ExitItemClick
      end
    end
  end
end
