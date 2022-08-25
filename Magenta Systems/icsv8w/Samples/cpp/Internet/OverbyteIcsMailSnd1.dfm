object MailSndForm: TMailSndForm
  Left = 235
  Top = 106
  Caption = 'MailSndForm'
  ClientHeight = 432
  ClientWidth = 524
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
  object MsgMemo: TMemo
    Left = 0
    Top = 109
    Width = 524
    Height = 120
    Hint = 'Enter the message text in this memo'
    Align = alTop
    Lines.Strings = (
      'This is the first line'
      'Then the second one'
      'The next one is empty'
      ''
      'The next one has only a single dot'
      '.'
      'Finally the last one')
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 312
    Width = 524
    Height = 120
    Hint = 'This memo shows info messages'
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 109
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 11
      Width = 55
      Height = 13
      Caption = 'SMTP Host'
    end
    object Label2: TLabel
      Left = 48
      Top = 36
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Label3: TLabel
      Left = 214
      Top = 36
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object Subject: TLabel
      Left = 35
      Top = 58
      Width = 36
      Height = 13
      Caption = 'Subject'
    end
    object Label4: TLabel
      Left = 208
      Top = 11
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label5: TLabel
      Left = 16
      Top = 88
      Width = 66
      Height = 13
      Caption = 'Message text:'
    end
    object Label8: TLabel
      Left = 208
      Top = 60
      Width = 21
      Height = 13
      Caption = 'Sign'
    end
    object HostEdit: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Mail server hostname or IP address'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'HostEdit'
    end
    object FromEdit: TEdit
      Left = 80
      Top = 32
      Width = 121
      Height = 21
      Hint = 'Author'#39's EMail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'FromEdit'
    end
    object ToEdit: TEdit
      Left = 232
      Top = 32
      Width = 121
      Height = 21
      Hint = 'Destinators, delimited by semicolons'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'ToEdit'
    end
    object SubjectEdit: TEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      Hint = 'Message subject'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'SubjectEdit'
    end
    object SignOnEdit: TEdit
      Left = 232
      Top = 56
      Width = 121
      Height = 21
      Hint = 'Signon message for the HELO command'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'SignOnEdit'
    end
    object PortEdit: TEdit
      Left = 232
      Top = 8
      Width = 121
      Height = 21
      Hint = 'Mail server port (should be smtp)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'PortEdit'
    end
    object ClearDisplayButton: TButton
      Left = 440
      Top = 88
      Width = 73
      Height = 17
      Hint = 'Clear info message memo'
      Caption = 'Clear &Info'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = ClearDisplayButtonClick
    end
    object ConnectButton: TButton
      Left = 360
      Top = 8
      Width = 73
      Height = 17
      Hint = 'Connect to the mail server'
      Caption = 'Connect'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = ConnectButtonClick
    end
    object HeloButton: TButton
      Left = 360
      Top = 28
      Width = 73
      Height = 17
      Hint = 'Send the signon message'
      Caption = 'Helo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = HeloButtonClick
    end
    object MailFromButton: TButton
      Left = 360
      Top = 48
      Width = 73
      Height = 17
      Hint = 'Send the mail originator'
      Caption = 'MailFrom'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = MailFromButtonClick
    end
    object RcptToButton: TButton
      Left = 360
      Top = 68
      Width = 73
      Height = 17
      Hint = 'Send the mail recipents'
      Caption = 'RcptTo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = RcptToButtonClick
    end
    object DataButton: TButton
      Left = 360
      Top = 88
      Width = 73
      Height = 17
      Hint = 'Send mail text and attached files'
      Caption = 'Data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = DataButtonClick
    end
    object AbortButton: TButton
      Left = 440
      Top = 68
      Width = 73
      Height = 17
      Hint = 'Abort current operation and close'
      Caption = 'Abort'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = AbortButtonClick
    end
    object QuitButton: TButton
      Left = 440
      Top = 48
      Width = 73
      Height = 17
      Hint = 'Quit mail server'
      Caption = 'Quit'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = QuitButtonClick
    end
    object MailButton: TButton
      Left = 440
      Top = 28
      Width = 73
      Height = 17
      Hint = 'MailFrom, RcptTo and Data combined'
      Caption = 'Mail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = MailButtonClick
    end
    object OpenButton: TButton
      Left = 440
      Top = 8
      Width = 73
      Height = 17
      Hint = 'Connect and Helo combined'
      Caption = 'Open'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = OpenButtonClick
    end
  end
  object AttachPanel: TPanel
    Left = 0
    Top = 229
    Width = 524
    Height = 17
    Align = alTop
    TabOrder = 3
    object Label6: TLabel
      Left = 16
      Top = 2
      Width = 67
      Height = 13
      Caption = 'Attached files:'
    end
  end
  object FileAttachMemo: TMemo
    Left = 0
    Top = 246
    Width = 524
    Height = 49
    Hint = 'Enter the attached file path, one per line'
    Align = alTop
    Lines.Strings = (
      'c:\temp\brol.txt'
      'c:\temp\test.txt')
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 4
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 295
    Width = 524
    Height = 17
    Align = alTop
    TabOrder = 5
    object Label7: TLabel
      Left = 16
      Top = 2
      Width = 71
      Height = 13
      Caption = 'Info messages:'
    end
  end
  object SmtpClient: TSmtpCli
    Tag = 0
    ShareMode = smtpShareDenyWrite
    LocalAddr = '0.0.0.0'
    Port = 'smtp'
    AuthType = smtpAuthNone
    ConfirmReceipt = False
    HdrPriority = smtpPriorityNone
    CharSet = 'iso-8859-1'
    ConvertToCharset = False
    WrapMsgMaxLineLen = 76
    SendMode = smtpToSocket
    DefaultEncoding = smtpEnc7bit
    Allow8bitChars = True
    FoldHeaders = False
    WrapMessageText = False
    ContentType = smtpPlainText
    OwnHeaders = False
    OnDisplay = SmtpClientDisplay
    OnGetData = SmtpClientGetData
    OnHeaderLine = SmtpClientHeaderLine
    OnRequestDone = SmtpClientRequestDone
    XMailer = 'ICS SMTP Component V%VER%'
    Left = 420
    Top = 136
  end
end
