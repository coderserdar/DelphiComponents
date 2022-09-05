object Form1: TForm1
  Left = 275
  Top = 134
  Width = 445
  Height = 471
  Caption = 'Form1'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 437
    Height = 417
    Align = alClient
    Stretch = True
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 8
    object FileMenu: TMenuItem
      Caption = 'File'
      object OpenItem: TMenuItem
        Caption = 'Open'
        OnClick = OpenItemClick
      end
      object SaveAsItem: TMenuItem
        Caption = 'Save As'
        OnClick = SaveAsItemClick
      end
      object ExitItem: TMenuItem
        Caption = 'Exit'
        OnClick = ExitItemClick
      end
    end
    object EditMenu: TMenuItem
      Caption = 'Edit'
      object CopyItem: TMenuItem
        Caption = 'Copy'
        OnClick = CopyItemClick
      end
      object PasteItem: TMenuItem
        Caption = 'Paste'
        OnClick = PasteItemClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 56
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 96
    Top = 8
  end
end
