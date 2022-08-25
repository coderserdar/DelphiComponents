object FtpAsyncForm: TFtpAsyncForm
  Left = 153
  Top = 122
  Caption = 'FtpAsyncForm'
  ClientHeight = 346
  ClientWidth = 640
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
    Width = 640
    Height = 125
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 11
      Top = 12
      Width = 50
      Height = 13
      Caption = 'HostName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 10
      Top = 39
      Width = 50
      Height = 13
      Caption = 'UserName'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 220
      Top = 39
      Width = 49
      Height = 13
      Caption = 'PassWord'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 252
      Top = 12
      Width = 19
      Height = 13
      Caption = 'Port'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 95
      Width = 41
      Height = 13
      Caption = 'Host File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 21
      Top = 67
      Width = 38
      Height = 13
      Caption = 'Host Dir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 224
      Top = 68
      Width = 42
      Height = 13
      Caption = 'Local Dir'
    end
    object HostNameEdit: TEdit
      Left = 72
      Top = 8
      Width = 137
      Height = 21
      Hint = 'Host where the file is located'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'HostNameEdit'
    end
    object UserNameEdit: TEdit
      Left = 72
      Top = 36
      Width = 137
      Height = 21
      Hint = 'User name used to log on the host'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'UserNameEdit'
    end
    object PassWordEdit: TEdit
      Left = 276
      Top = 36
      Width = 197
      Height = 21
      Hint = 'Password used to validate user access'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'PassWordEdit'
    end
    object PortEdit: TEdit
      Left = 276
      Top = 8
      Width = 85
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = 'PortEdit'
    end
    object cbBinary: TCheckBox
      Left = 376
      Top = 12
      Width = 93
      Height = 17
      Hint = 'Select to use binary mode transfert'
      Caption = 'Binary Mode'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 4
    end
    object HostDirEdit: TEdit
      Left = 72
      Top = 64
      Width = 137
      Height = 21
      Hint = 'Enter the host directory where the file is located'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'HostDirEdit'
    end
    object HostFileEdit: TEdit
      Left = 72
      Top = 91
      Width = 305
      Height = 21
      Hint = 'Enter local file name and path or new file name for rename'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'HostFileEdit'
    end
    object AddFileButton: TButton
      Left = 396
      Top = 92
      Width = 75
      Height = 21
      Caption = 'Add File'
      TabOrder = 7
      OnClick = AddFileButtonClick
    end
    object LocalDirEdit: TEdit
      Left = 276
      Top = 64
      Width = 197
      Height = 21
      TabOrder = 8
      Text = 'LocalDirEdit'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 125
    Width = 640
    Height = 221
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    object DisplayMemo: TMemo
      Left = 345
      Top = 1
      Width = 294
      Height = 219
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
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 265
      Top = 1
      Width = 80
      Height = 219
      Align = alLeft
      TabOrder = 1
      object ExecButton: TButton
        Left = 8
        Top = 16
        Width = 65
        Height = 21
        Caption = 'Execute'
        TabOrder = 0
        OnClick = ExecButtonClick
      end
      object RemoveButton: TButton
        Left = 8
        Top = 48
        Width = 65
        Height = 21
        Caption = 'Remove'
        TabOrder = 1
        OnClick = RemoveButtonClick
      end
      object ReplaceButton: TButton
        Left = 8
        Top = 80
        Width = 65
        Height = 21
        Caption = 'Replace'
        TabOrder = 2
        OnClick = ReplaceButtonClick
      end
      object AbortButton: TButton
        Left = 8
        Top = 112
        Width = 65
        Height = 21
        Caption = 'Abort'
        TabOrder = 3
        OnClick = AbortButtonClick
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 264
      Height = 219
      Align = alLeft
      Caption = 'Panel3'
      TabOrder = 2
      object FilesListBox: TListBox
        Left = 1
        Top = 1
        Width = 262
        Height = 109
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
      object ResultsListBox: TListBox
        Left = 1
        Top = 110
        Width = 262
        Height = 108
        Align = alBottom
        ItemHeight = 13
        TabOrder = 1
      end
    end
  end
  object FtpClient1: TFtpClient
    Timeout = 15
    MultiThreaded = False
    Port = 'ftp'
    CodePage = 0
    DataPortRangeStart = 0
    DataPortRangeEnd = 0
    LocalAddr = '0.0.0.0'
    DisplayFileFlag = False
    Binary = True
    ShareMode = ftpShareExclusive
    Options = [ftpAcceptLF]
    ConnectionType = ftpDirect
    Language = 'EN'
    OnDisplay = FtpClient1Display
    OnRequestDone = FtpClient1RequestDone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Left = 24
    Top = 149
  end
end
