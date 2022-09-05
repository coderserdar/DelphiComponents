object D3DApplication: TD3DApplication
  Left = 356
  Top = 258
  Width = 462
  Height = 355
  Caption = 'D3DApplication'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  PopupMenu = ContextMenu
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu: TMainMenu
    Left = 88
    Top = 24
    object mFile: TMenuItem
      Caption = '&File'
      object mFileGoStop: TMenuItem
        Caption = '&Go/Stop'
        OnClick = mFileGoStopClick
      end
      object mFileSingleStep: TMenuItem
        Caption = '&Single Step'
        OnClick = mFileSingleStepClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mFileFullscreen: TMenuItem
        Caption = '&Full screen'
        OnClick = mFileFullscreenClick
      end
      object mFileChangeDevice: TMenuItem
        Caption = '&Change device...'
        ShortCut = 113
        OnClick = mFileChangeDeviceClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mFileAbout: TMenuItem
        Caption = '&About...'
        ShortCut = 112
        OnClick = mFileAboutClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mFileExitClick
      end
    end
  end
  object ContextMenu: TPopupMenu
    Left = 200
    Top = 24
    object mContextGoStop: TMenuItem
      Caption = '&Go/Stop'
      OnClick = mFileGoStopClick
    end
    object mContextSingleStep: TMenuItem
      Caption = '&Single Step'
      OnClick = mFileSingleStepClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mContextFullscreen: TMenuItem
      Caption = '&Full screen'
      OnClick = mFileFullscreenClick
    end
    object mContextChangeDevice: TMenuItem
      Caption = '&Change device...'
      OnClick = mFileChangeDeviceClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object mContextAbout: TMenuItem
      Caption = '&About...'
      OnClick = mFileAboutClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object mContextExit: TMenuItem
      Caption = 'E&xit'
      OnClick = mFileExitClick
    end
  end
end
