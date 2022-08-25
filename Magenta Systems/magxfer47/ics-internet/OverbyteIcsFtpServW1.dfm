object FtpServerForm: TFtpServerForm
  Left = 205
  Top = 121
  Caption = 'FtpServerSForm'
  ClientHeight = 256
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InfoMemo: TMemo
    Left = 0
    Top = 66
    Width = 376
    Height = 190
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'InfoMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 376
    Height = 66
    Align = alTop
    TabOrder = 1
    object GreenImage: TImage
      Left = 16
      Top = 12
      Width = 9
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000900
        00000F0000000100040000000000780000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF003A00000A3000000007A000A700000000070AAA07000000000A0AAA0A0000
        0000070AAA070000000007A000A7000000000A07770A000000000707B7070000
        0000070777070000000007700077000000000707770700000000070797070000
        0000070777070000000007700077000000003000000030000000}
      OnDblClick = ImagesDblClick
    end
    object ClientCountLabel: TLabel
      Left = 44
      Top = 12
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object RedImage: TImage
      Left = 32
      Top = 12
      Width = 9
      Height = 15
      Picture.Data = {
        07544269746D6170EE000000424DEE0000000000000076000000280000000900
        00000F0000000100040000000000780000000000000000000000100000001000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF003000000030000000077000770000000007077707000000000707A7070000
        00000707770700000000077000770000000007077707000000000707B7070000
        0000090777090000000007900097000000000709990700000000090999090000
        0000070999070000000007900097000000003900000930000000}
      OnDblClick = ImagesDblClick
    end
    object Label1: TLabel
      Left = 10
      Top = 40
      Width = 68
      Height = 13
      Caption = 'Root Directory'
    end
    object StartMinimizedCheckBox: TCheckBox
      Left = 136
      Top = 12
      Width = 100
      Height = 17
      Caption = 'Start Minimized'
      TabOrder = 0
    end
    object RootDirectory: TEdit
      Left = 85
      Top = 35
      Width = 281
      Height = 21
      TabOrder = 1
      Text = 'c:\temp'
    end
  end
  object MainMenu1: TMainMenu
    Left = 81
    Top = 102
    object File1: TMenuItem
      Caption = '&File'
      object MnuStartServer: TMenuItem
        Caption = '&Start server'
        OnClick = MnuStartServerClick
      end
      object MnuStopServer: TMenuItem
        Caption = 'S&top server'
        OnClick = MnuStopServerClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MnuListClients: TMenuItem
        Caption = '&List clients'
        OnClick = MnuListClientsClick
      end
      object DisconnectAllMnu: TMenuItem
        Caption = '&Disconnect all'
        OnClick = DisconnectAllMnuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MnuQuit: TMenuItem
        Caption = '&Quit'
        OnClick = MnuQuitClick
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      object Cleardisplay1: TMenuItem
        Caption = '&Clear display'
        OnClick = Cleardisplay1Click
      end
      object DisplayDirectories1: TMenuItem
        Caption = 'Display &Directories'
        OnClick = DisplayDirectories1Click
      end
    end
    object About1: TMenuItem
      Caption = '&About'
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object ForceHomeDir: TMenuItem
        AutoCheck = True
        Caption = '&ForceHomeDir'
        OnClick = ForceHomeDirClick
      end
      object HidePhysicalPath: TMenuItem
        AutoCheck = True
        Caption = '&HidePhysicalPath'
        OnClick = HidePhysicalPathClick
      end
      object Authenticateotpmd5: TMenuItem
        Caption = 'Authenticate otp-md&5'
        OnClick = Authenticateotpmd5Click
      end
      object Authenticateotpmd4: TMenuItem
        Caption = 'Authenticate otp-md&4'
        OnClick = Authenticateotpmd4Click
      end
      object Authenticateotpsha1: TMenuItem
        Caption = 'Authenticate otp-&sha1'
        OnClick = Authenticateotpsha1Click
      end
    end
  end
end
