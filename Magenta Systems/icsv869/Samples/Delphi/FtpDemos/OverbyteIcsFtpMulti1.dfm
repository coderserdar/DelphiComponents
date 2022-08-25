object FtpMultiForm: TFtpMultiForm
  Left = 138
  Top = 123
  Caption = 'FTP - Multiple simultaneous transfers'
  ClientHeight = 483
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 433
    Height = 325
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 85
      Height = 13
      Caption = 'Files to download:'
    end
    object Label2: TLabel
      Left = 8
      Top = 188
      Width = 38
      Height = 13
      Caption = 'Dest Dir'
    end
    object Label3: TLabel
      Left = 8
      Top = 208
      Width = 136
      Height = 13
      Caption = 'Nbr of simultaneous transfers'
    end
    object Label4: TLabel
      Left = 12
      Top = 232
      Width = 48
      Height = 13
      Caption = 'Done List:'
    end
    object FileListMemo: TMemo
      Left = 24
      Top = 36
      Width = 365
      Height = 137
      Lines.Strings = (
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/RapidSplit.exe'
        
          'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/SplitWizard.ex' +
          'e'
        
          'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/UniversalSplic' +
          'er.exe'
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/axzip.zip'
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/axzipf.zip'
        
          'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/cryptocrat_set' +
          'up.exe'
        
          'ftp://ftp.simtel.net/pub/simtelnet/winxp/compress/swzipperxt_ins' +
          'tall.msi'
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/clocks/pctime.zip'
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/clocks/clock.exe'
        'ftp://ftp.simtel.net/pub/simtelnet/winxp/clocks/alarm.exe')
      TabOrder = 0
    end
    object DownloadButton: TButton
      Left = 312
      Top = 180
      Width = 75
      Height = 25
      Caption = 'Download'
      TabOrder = 1
      OnClick = DownloadButtonClick
    end
    object DirEdit: TEdit
      Left = 56
      Top = 180
      Width = 241
      Height = 21
      TabOrder = 2
      Text = 'c:\temp'
    end
    object ComponentCountEdit: TEdit
      Left = 152
      Top = 204
      Width = 41
      Height = 21
      TabOrder = 3
      Text = '3'
    end
    object DoneMemo: TMemo
      Left = 24
      Top = 252
      Width = 365
      Height = 69
      Lines.Strings = (
        '')
      TabOrder = 4
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 325
    Width = 433
    Height = 158
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
