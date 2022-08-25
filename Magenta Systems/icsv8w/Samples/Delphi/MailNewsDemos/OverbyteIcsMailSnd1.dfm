object SmtpTestForm: TSmtpTestForm
  Left = 197
  Top = 144
  Width = 635
  Height = 525
  Caption = 'SmtpTestForm - http://www.overbyte.be'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MsgMemo: TMemo
    Left = 0
    Top = 235
    Width = 627
    Height = 120
    Hint = 'Enter the message text in this memo'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'MsgMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 441
    Width = 627
    Height = 50
    Hint = 'This memo shows info messages'
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 1
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 627
    Height = 235
    Align = alTop
    TabOrder = 2
    DesignSize = (
      627
      235)
    object Label5: TLabel
      Left = 12
      Top = 219
      Width = 69
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Message text:'
    end
    object ClearDisplayButton: TButton
      Left = 480
      Top = 102
      Width = 73
      Height = 17
      Hint = 'Clear info message memo'
      Caption = 'Clear &Info'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = ClearDisplayButtonClick
    end
    object ConnectButton: TButton
      Left = 400
      Top = 24
      Width = 73
      Height = 17
      Hint = 'Connect to the mail server'
      Caption = 'Connect'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = ConnectButtonClick
    end
    object HeloButton: TButton
      Left = 400
      Top = 43
      Width = 73
      Height = 17
      Hint = 'Send the signon message'
      Caption = 'Helo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = HeloButtonClick
    end
    object MailFromButton: TButton
      Left = 400
      Top = 102
      Width = 73
      Height = 17
      Hint = 'Send the mail originator'
      Caption = 'MailFrom'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = MailFromButtonClick
    end
    object RcptToButton: TButton
      Left = 400
      Top = 142
      Width = 73
      Height = 17
      Hint = 'Send the mail recipents'
      Caption = 'RcptTo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = RcptToButtonClick
    end
    object DataButton: TButton
      Left = 400
      Top = 162
      Width = 73
      Height = 17
      Hint = 'Send mail text and attached files'
      Caption = 'Data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = DataButtonClick
    end
    object AbortButton: TButton
      Left = 480
      Top = 82
      Width = 73
      Height = 17
      Hint = 'Abort current operation and close'
      Caption = 'Abort'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = AbortButtonClick
    end
    object QuitButton: TButton
      Left = 480
      Top = 63
      Width = 73
      Height = 17
      Hint = 'Quit mail server'
      Caption = 'Quit'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = QuitButtonClick
    end
    object MailButton: TButton
      Left = 480
      Top = 43
      Width = 73
      Height = 17
      Hint = 'MailFrom, RcptTo and Data combined'
      Caption = 'Mail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = MailButtonClick
    end
    object OpenButton: TButton
      Left = 480
      Top = 24
      Width = 73
      Height = 17
      Hint = 'Connect and Helo combined'
      Caption = 'Open'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = OpenButtonClick
    end
    object EhloButton: TButton
      Left = 400
      Top = 63
      Width = 73
      Height = 17
      Caption = 'Ehlo'
      TabOrder = 2
      OnClick = EhloButtonClick
    end
    object AuthButton: TButton
      Left = 400
      Top = 82
      Width = 73
      Height = 17
      Caption = 'Auth'
      TabOrder = 3
      OnClick = AuthButtonClick
    end
    object AllInOneButton: TButton
      Left = 480
      Top = 142
      Width = 73
      Height = 17
      Hint = 
        'Connect, Helo, MailFrom, RcptTo, Data and Quit all chained in a ' +
        'single action.'
      Caption = 'All In One'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = AllInOneButtonClick
    end
    object SendToFileButton: TButton
      Left = 400
      Top = 198
      Width = 73
      Height = 17
      Hint = 'Write the message to file'
      Caption = 'Send To File'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = SendToFileButtonClick
    end
    object MsgSizeButton: TButton
      Left = 480
      Top = 198
      Width = 73
      Height = 17
      Hint = 'Calculate message size'
      Caption = 'CalcMsgSize'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = MsgSizeButtonClick
    end
    object MailFromSIZEButton: TButton
      Left = 400
      Top = 122
      Width = 73
      Height = 17
      Hint = 'Send the mail originator with SIZE extension'
      Caption = 'MailFromSIZE'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = MailFromSIZEButtonClick
    end
    object SettingsPageControl: TPageControl
      Left = 4
      Top = 4
      Width = 385
      Height = 211
      ActivePage = BasicSettingsTabSheet
      TabOrder = 16
      object BasicSettingsTabSheet: TTabSheet
        Caption = 'Basic Settings'
        object Label1: TLabel
          Left = 25
          Top = 11
          Width = 51
          Height = 13
          Caption = 'SMTP Host'
        end
        object Label4: TLabel
          Left = 222
          Top = 11
          Width = 20
          Height = 13
          Caption = 'Port'
        end
        object Label2: TLabel
          Left = 52
          Top = 36
          Width = 24
          Height = 13
          Caption = 'From'
        end
        object Label3: TLabel
          Left = 230
          Top = 36
          Width = 12
          Height = 13
          Caption = 'To'
        end
        object Label12: TLabel
          Left = 64
          Top = 60
          Width = 12
          Height = 13
          Caption = 'Cc'
        end
        object Label13: TLabel
          Left = 226
          Top = 60
          Width = 16
          Height = 13
          Caption = 'Bcc'
        end
        object Subject: TLabel
          Left = 40
          Top = 156
          Width = 36
          Height = 13
          Caption = 'Subject'
        end
        object Label8: TLabel
          Left = 222
          Top = 84
          Width = 20
          Height = 13
          Caption = 'Sign'
        end
        object Label9: TLabel
          Left = 28
          Top = 108
          Width = 48
          Height = 13
          Caption = 'Username'
        end
        object Label10: TLabel
          Left = 220
          Top = 108
          Width = 22
          Height = 13
          Caption = 'Pass'
        end
        object Label11: TLabel
          Left = 6
          Top = 132
          Width = 70
          Height = 13
          Caption = 'Authentication'
        end
        object Label14: TLabel
          Left = 208
          Top = 132
          Width = 34
          Height = 13
          Caption = 'Priority'
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
          Left = 246
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
          Left = 246
          Top = 32
          Width = 121
          Height = 21
          Hint = 'Recipients, delimited by semicolons'
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
          Hint = 'Carbon copies, delimited by semicolons'
          TabOrder = 4
          Text = 'CcEdit'
        end
        object BccEdit: TEdit
          Left = 246
          Top = 56
          Width = 121
          Height = 21
          Hint = 'Blind carbon copies, delimited by semicolons'
          TabOrder = 5
          Text = 'BccEdit'
        end
        object SubjectEdit: TEdit
          Left = 80
          Top = 152
          Width = 287
          Height = 21
          Hint = 'Message subject'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Text = 'SubjectEdit'
        end
        object SignOnEdit: TEdit
          Left = 246
          Top = 80
          Width = 121
          Height = 21
          Hint = 
            'Signon message for the HELO command. If empty current host name ' +
            'is used.'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Text = 'SignOnEdit'
        end
        object UsernameEdit: TEdit
          Left = 80
          Top = 104
          Width = 121
          Height = 21
          TabOrder = 8
          Text = 'UsernameEdit'
        end
        object PasswordEdit: TEdit
          Left = 246
          Top = 104
          Width = 121
          Height = 21
          TabOrder = 9
          Text = 'PasswordEdit'
        end
        object AuthComboBox: TComboBox
          Left = 80
          Top = 128
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 10
          Items.Strings = (
            'None'
            'Plain'
            'Login'
            'CramMD5'
            'CramSHA1'
            'NTLM'
            'AutoSelect')
        end
        object PriorityComboBox: TComboBox
          Left = 246
          Top = 128
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 11
          Items.Strings = (
            'Not specified'
            'Highest'
            'High'
            'Normal'
            'Low'
            'Lowest')
        end
        object ConfirmCheckBox: TCheckBox
          Left = 0
          Top = 80
          Width = 93
          Height = 17
          Hint = 'Ask for confirmation of receipt.'
          Alignment = taLeftJustify
          Caption = 'Confirm Receipt'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 12
        end
      end
      object CharsetSettingsTabSheet: TTabSheet
        Caption = 'Charset && MIME Settings'
        ImageIndex = 1
        object UseMailMessageCheckBox: TCheckBox
          Left = 19
          Top = 6
          Width = 335
          Height = 17
          Hint = 'Use property MailMessage rather than OnGetData event'
          Caption = 'Use property MailMessage (required for some properties below).'
          TabOrder = 0
        end
        object CharSetPanel: TPanel
          Left = 12
          Top = 26
          Width = 365
          Height = 147
          BevelOuter = bvNone
          TabOrder = 1
          object Label15: TLabel
            Left = 7
            Top = 2
            Width = 38
            Height = 13
            Caption = 'Charset'
          end
          object DefEnc: TLabel
            Left = 6
            Top = 92
            Width = 127
            Height = 13
            Caption = 'Default(Transfer)Encoding'
          end
          object Label16: TLabel
            Left = 208
            Top = 92
            Width = 138
            Height = 13
            Caption = 'Wrap text at (bytes per line)'
          end
          object Label17: TLabel
            Left = 256
            Top = 109
            Width = 68
            Height = 13
            Caption = '(default = 76)'
          end
          object CharsetInfoLabel1: TLabel
            Left = 48
            Top = 2
            Width = 89
            Height = 13
            Caption = 'CharsetInfoLabel1'
          end
          object CharsetTestButton: TButton
            Left = 249
            Top = 16
            Width = 73
            Height = 21
            Hint = 'Test whether the charset is supported'
            Caption = 'Test Charset'
            TabOrder = 1
            OnClick = CharsetTestButtonClick
          end
          object ConvertToCharsetCheckBox: TCheckBox
            Left = 6
            Top = 40
            Width = 275
            Height = 17
            Hint = 
              'Ignored by Unicode compilers. With ANSI compilers usually OFF ex' +
              'cept for '#13#10'UTF-8 and UTF-7. '
            Caption = 'ConvertToCharset (effects ANSI compilers only) '
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object Allow8BitCheckBox: TCheckBox
            Left = 6
            Top = 56
            Width = 107
            Height = 17
            Hint = 
              'Should be OFF by default, if ON ensures backwards compatibility ' +
              'as well.'
            Caption = 'Allow8BitChars'
            TabOrder = 3
          end
          object DefEncodingComboBox: TComboBox
            Left = 6
            Top = 106
            Width = 125
            Height = 21
            Hint = 
              'Default Tranfer-Encoding. Value actually used may change if set ' +
              'to 7bit or 8bit'#13#10'and Allow8BitChars is disabled. If set to QP or' +
              ' Base64 this encoding is enforced.'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 5
            Items.Strings = (
              'smtpEnc7bit'
              'smtpEnc8bit'
              'smtpEncQuotedPrintable'
              'smtpEncBase64')
          end
          object FoldHeadersCheckBox: TCheckBox
            Left = 6
            Top = 72
            Width = 87
            Height = 17
            Hint = 'Fold header lines? Should be ON by default '
            Caption = 'FoldHeaders'
            TabOrder = 4
          end
          object WrapTextCheckBox: TCheckBox
            Left = 208
            Top = 72
            Width = 153
            Height = 17
            Hint = 'Wrap MailMessage text'
            Caption = 'WrapMailMessageText'
            TabOrder = 6
          end
          object WrapAtEdit: TEdit
            Left = 208
            Top = 106
            Width = 39
            Height = 21
            Hint = 
              'Attempts to wrap lines of MailMessage text not encoded QP or B64' +
              ' at this'#13#10'value, lines may get longer though '
            TabOrder = 7
            Text = 'WrapAtEdit'
          end
          object ToggleCsViewButton: TButton
            Left = 177
            Top = 16
            Width = 73
            Height = 21
            Hint = 'Toggle view between user-friendly names and MIME charset names. '
            Caption = 'Toggle View'
            TabOrder = 8
            OnClick = ToggleCsViewButtonClick
          end
          object IcsCharsetComboBox1: TIcsCharsetComboBox
            Left = 6
            Top = 16
            Width = 171
            Height = 21
            Hint = 
              'List of common charsets. Aliases are supported as well, just typ' +
              'e for example, '#13#10'"latin1" and press button Test Charset '
            CharSet = 'utf-7'
            DropDownCount = 32
            ItemHeight = 13
            TabOrder = 0
            OnChange = IcsCharsetComboBox1Change
          end
        end
      end
      object ProxyTabsheet: TTabSheet
        Caption = 'Transparent Proxy'
        ImageIndex = 2
        object Label18: TLabel
          Left = 35
          Top = 73
          Width = 56
          Height = 13
          Caption = 'Proxy host:'
        end
        object Label19: TLabel
          Left = 36
          Top = 99
          Width = 55
          Height = 13
          Caption = 'Proxy port:'
        end
        object Label20: TLabel
          Left = 35
          Top = 125
          Width = 56
          Height = 13
          Caption = 'Proxy user:'
        end
        object Label21: TLabel
          Left = 10
          Top = 153
          Width = 81
          Height = 13
          Caption = 'Proxy password:'
        end
        object Label22: TLabel
          Left = 34
          Top = 16
          Width = 57
          Height = 13
          Caption = 'Proxy type:'
        end
        object Label23: TLabel
          Left = 2
          Top = 43
          Width = 89
          Height = 13
          Caption = 'HTTP proxy auth.:'
        end
        object ProxyHostEdit: TEdit
          Left = 97
          Top = 70
          Width = 145
          Height = 21
          TabOrder = 2
          Text = 'ProxyHostEdit'
        end
        object ProxyPortEdit: TEdit
          Left = 97
          Top = 97
          Width = 56
          Height = 21
          TabOrder = 3
          Text = 'ProxyPortEdit'
        end
        object ProxyPasswordEdit: TEdit
          Left = 97
          Top = 151
          Width = 145
          Height = 21
          TabOrder = 5
          Text = 'ProxyPasswordEdit'
        end
        object ProxyUserEdit: TEdit
          Left = 97
          Top = 124
          Width = 145
          Height = 21
          TabOrder = 4
          Text = 'ProxyUserEdit'
        end
        object ProxyTypeComboBox: TComboBox
          Left = 97
          Top = 13
          Width = 145
          Height = 21
          ItemHeight = 0
          TabOrder = 0
          Text = 'ProxyTypeComboBox'
          OnCloseUp = ProxyTypeComboBoxCloseUp
        end
        object ProxyHttpAuthTypeComboBox: TComboBox
          Left = 97
          Top = 40
          Width = 145
          Height = 21
          ItemHeight = 0
          TabOrder = 1
          Text = 'ProxyHttpAuthTypeComboBox'
          OnCloseUp = ProxyHttpAuthTypeComboBoxCloseUp
        end
      end
    end
  end
  object AttachPanel: TPanel
    Left = 0
    Top = 355
    Width = 627
    Height = 17
    Align = alTop
    TabOrder = 3
    object Label6: TLabel
      Left = 16
      Top = 2
      Width = 70
      Height = 13
      Caption = 'Attached files:'
    end
  end
  object FileAttachMemo: TMemo
    Left = 0
    Top = 372
    Width = 627
    Height = 49
    Hint = 'Enter the attached file path, one per line'
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'FileAttachMemo')
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 4
    WordWrap = False
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 421
    Width = 627
    Height = 20
    Align = alTop
    TabOrder = 5
    DesignSize = (
      627
      20)
    object Label7: TLabel
      Left = 16
      Top = 2
      Width = 74
      Height = 13
      Caption = 'Info messages:'
    end
    object ProgressBar1: TProgressBar
      Left = 192
      Top = 3
      Width = 411
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object ProgressCheckBox: TCheckBox
      Left = 120
      Top = 1
      Width = 65
      Height = 17
      Hint = 'Show progress'
      Alignment = taLeftJustify
      Caption = 'Progress'
      TabOrder = 1
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
    CharSet = 'windows-1252'
    ConvertToCharset = True
    WrapMsgMaxLineLen = 76
    SendMode = smtpToSocket
    DefaultEncoding = smtpEnc7bit
    Allow8bitChars = True
    FoldHeaders = False
    WrapMessageText = False
    ContentType = smtpPlainText
    OwnHeaders = False
    OnDisplay = SmtpClientDisplay
    OnCommand = SmtpClientDisplay
    OnResponse = SmtpClientDisplay
    OnGetData = SmtpClientGetData
    OnHeaderLine = SmtpClientHeaderLine
    OnRequestDone = SmtpClientRequestDone
    XMailer = 'ICS SMTP Component V%VER%'
    ProxyType = smtpNoProxy
    ProxyHttpAuthType = htatDetect
    OnAttachContentTypeEh = SmtpClientAttachContentTypeEh
    Left = 32
    Top = 258
  end
end
