object HtmlMailForm: THtmlMailForm
  Left = 338
  Top = 228
  Caption = 'Html Mail - ICS - http://www.overbyte.be'
  ClientHeight = 444
  ClientWidth = 661
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
    Width = 661
    Height = 117
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 22
      Top = 11
      Width = 55
      Height = 13
      Caption = 'SMTP Host'
    end
    object Label4: TLabel
      Left = 208
      Top = 11
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label2: TLabel
      Left = 54
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
    object Label12: TLabel
      Left = 64
      Top = 60
      Width = 13
      Height = 13
      Caption = 'Cc'
    end
    object Label13: TLabel
      Left = 208
      Top = 60
      Width = 19
      Height = 13
      Caption = 'Bcc'
    end
    object Subject: TLabel
      Left = 41
      Top = 82
      Width = 36
      Height = 13
      Caption = 'Subject'
    end
    object Label8: TLabel
      Left = 206
      Top = 84
      Width = 21
      Height = 13
      Caption = 'Sign'
    end
    object Label5: TLabel
      Left = 364
      Top = 12
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label6: TLabel
      Left = 366
      Top = 36
      Width = 46
      Height = 13
      Caption = 'Password'
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
    object CcEdit: TEdit
      Left = 80
      Top = 56
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'CcEdit'
    end
    object BccEdit: TEdit
      Left = 232
      Top = 56
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = 'BccEdit'
    end
    object SubjectEdit: TEdit
      Left = 80
      Top = 80
      Width = 121
      Height = 21
      Hint = 'Message subject'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'SubjectEdit'
    end
    object SignOnEdit: TEdit
      Left = 232
      Top = 80
      Width = 121
      Height = 21
      Hint = 'Signon message for the HELO command'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      Text = 'SignOnEdit'
    end
    object SendButton: TButton
      Left = 572
      Top = 8
      Width = 75
      Height = 21
      Hint = 
        'Connect, Helo, MailFrom, RcptTo, Data and Quit all chained in a ' +
        'single action.'
      Caption = '&Send '
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = SendButtonClick
    end
    object AbortButton: TButton
      Left = 572
      Top = 32
      Width = 75
      Height = 21
      Caption = '&Abort'
      TabOrder = 9
      OnClick = AbortButtonClick
    end
    object PlainTextCheckBox: TCheckBox
      Left = 572
      Top = 60
      Width = 77
      Height = 17
      Caption = 'Plain Text'
      TabOrder = 10
    end
    object ConfirmCheckBox: TCheckBox
      Left = 572
      Top = 80
      Width = 63
      Height = 17
      Caption = 'Confirm'
      TabOrder = 11
    end
    object UsernameEdit: TEdit
      Left = 418
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 12
      Text = 'UsernameEdit'
    end
    object PasswordEdit: TEdit
      Left = 418
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 13
      Text = 'PasswordEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 346
    Width = 661
    Height = 98
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 117
    Width = 323
    Height = 229
    Align = alLeft
    TabOrder = 2
    object PlainTextMemo: TMemo
      Left = 1
      Top = 1
      Width = 321
      Height = 85
      Hint = 'Enter the plain text message text here.'
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'PlainTextMemo')
      ParentFont = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 1
      Top = 86
      Width = 321
      Height = 142
      Align = alBottom
      TabOrder = 1
      object ImageFilesMemo: TMemo
        Left = 1
        Top = 1
        Width = 319
        Height = 72
        Hint = 'Enter the list of image files here, one per line.'
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'ImageFilesMemo')
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssBoth
        ShowHint = True
        TabOrder = 0
      end
      object AttachedFilesMemo: TMemo
        Left = 1
        Top = 73
        Width = 319
        Height = 68
        Hint = 'Enter the list of attached files files here, one per line.'
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'AttachedFilesMemo')
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssBoth
        ShowHint = True
        TabOrder = 1
      end
    end
  end
  object HtmlTextMemo: TMemo
    Left = 323
    Top = 117
    Width = 338
    Height = 229
    Hint = 
      'Enter the HTML text for the message. Special tags <#IMAGEn> will' +
      ' be replaced by image references.'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'HtmlTextMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 3
    WordWrap = False
  end
  object HtmlSmtpClient: THtmlSmtpCli
    Tag = 0
    ShareMode = smtpShareDenyWrite
    LocalAddr = '0.0.0.0'
    Port = 'smtp'
    AuthType = smtpAuthNone
    ConfirmReceipt = False
    HdrPriority = smtpPriorityNone
    CharSet = 'windows-1252'
    ConvertToCharset = True
    WrapMsgMaxLineLen = 76
    SendMode = smtpToSocket
    DefaultEncoding = smtpEnc7bit
    Allow8bitChars = True
    FoldHeaders = False
    WrapMessageText = False
    ContentType = smtpHtml
    OwnHeaders = False
    OnDisplay = HtmlSmtpClientDisplay
    OnRequestDone = HtmlSmtpClientRequestDone
    OnSessionClosed = HtmlSmtpClientSessionClosed
    XMailer = 'ICS SMTP Component V%VER%'
    HtmlCharSet = 'windows-1252'
    HtmlConvertToCharset = True
    Left = 362
    Top = 152
  end
end
