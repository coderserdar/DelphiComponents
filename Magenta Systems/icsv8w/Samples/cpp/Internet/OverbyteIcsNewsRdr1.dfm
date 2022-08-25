object NNTPForm: TNNTPForm
  Left = 192
  Top = 160
  Caption = 'NNTPForm'
  ClientHeight = 343
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 185
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object Label2: TLabel
      Left = 32
      Top = 84
      Width = 11
      Height = 13
      Caption = 'ID'
    end
    object Label3: TLabel
      Left = 4
      Top = 59
      Width = 44
      Height = 13
      Caption = 'Art. Num.'
    end
    object Label4: TLabel
      Left = 12
      Top = 34
      Width = 29
      Height = 13
      Caption = 'Group'
    end
    object Label5: TLabel
      Left = 26
      Top = 107
      Width = 16
      Height = 13
      Caption = 'File'
    end
    object Label6: TLabel
      Left = 20
      Top = 130
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object Label7: TLabel
      Left = 56
      Top = 155
      Width = 50
      Height = 13
      Caption = 'UserName'
    end
    object Label8: TLabel
      Left = 200
      Top = 155
      Width = 49
      Height = 13
      Caption = 'PassWord'
    end
    object ServerEdit: TEdit
      Left = 48
      Top = 8
      Width = 209
      Height = 21
      Hint = 'Enter the NNTP server host name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'ServerEdit'
    end
    object ConnectButton: TButton
      Left = 264
      Top = 8
      Width = 75
      Height = 21
      Hint = 'Connect to the NNTP server'
      Caption = '&Connect'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = ConnectButtonClick
    end
    object AbortButton: TButton
      Left = 344
      Top = 32
      Width = 75
      Height = 21
      Hint = 'Abort current job'
      Caption = 'A&bort'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = AbortButtonClick
    end
    object GroupButton: TButton
      Left = 264
      Top = 32
      Width = 75
      Height = 21
      Hint = 'Select the group'
      Caption = '&Group'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = GroupButtonClick
    end
    object GroupEdit: TEdit
      Left = 48
      Top = 32
      Width = 209
      Height = 21
      Hint = 'Enter the newsgroup name such as borland.public.delphi.internet'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'GroupEdit'
    end
    object ArticleNumEdit: TEdit
      Left = 48
      Top = 56
      Width = 209
      Height = 21
      Hint = 'Enter the article number to retreive'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'ArticleNumEdit'
    end
    object ArticleByNumberButton: TButton
      Left = 424
      Top = 8
      Width = 75
      Height = 21
      Hint = 'Retreive an article (header and body) by article number'
      Caption = '&ArticleByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = ArticleByNumberButtonClick
    end
    object ArticleByIDButton: TButton
      Left = 504
      Top = 8
      Width = 75
      Height = 21
      Hint = 'Retreive an article (header and body) by article ID'
      Caption = 'ArticleBy&ID'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = ArticleByIDButtonClick
    end
    object NextButton: TButton
      Left = 344
      Top = 56
      Width = 75
      Height = 21
      Hint = 'Get next article info'
      Caption = '&Next'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = NextButtonClick
    end
    object LastButton: TButton
      Left = 344
      Top = 80
      Width = 75
      Height = 21
      Hint = 'Get previous article info'
      Caption = '&Last'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = LastButtonClick
    end
    object HeadByNumberButton: TButton
      Left = 424
      Top = 32
      Width = 75
      Height = 21
      Hint = 'Request a header only, by article number'
      Caption = 'HeadByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = HeadByNumberButtonClick
    end
    object HeadByIDButton: TButton
      Left = 504
      Top = 32
      Width = 75
      Height = 21
      Hint = 'Retreive an article header, by article ID'
      Caption = 'HeadByID'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      OnClick = HeadByIDButtonClick
    end
    object BodyByNumberButton: TButton
      Left = 424
      Top = 56
      Width = 75
      Height = 21
      Hint = 'Request an article body, by article number'
      Caption = 'BodyByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = BodyByNumberButtonClick
    end
    object BodyByIDButton: TButton
      Left = 504
      Top = 56
      Width = 75
      Height = 21
      Hint = 'Retreive an article body, by article ID'
      Caption = 'BodyByID'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 22
      OnClick = BodyByIDButtonClick
    end
    object StatByNumberButton: TButton
      Left = 424
      Top = 80
      Width = 75
      Height = 21
      Hint = 'Request stats about an article by number'
      Caption = 'StatByNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = StatByNumberButtonClick
    end
    object StatByIDButton: TButton
      Left = 504
      Top = 80
      Width = 75
      Height = 21
      Hint = 'Request stats about an article by ID'
      Caption = 'StatByID'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 23
      OnClick = StatByIDButtonClick
    end
    object ListButton: TButton
      Left = 424
      Top = 104
      Width = 75
      Height = 21
      Hint = 'List ALL newsgroups. Will take a VERY LONG time.'
      Caption = 'List'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = ListButtonClick
    end
    object ArticleIDEdit: TEdit
      Left = 48
      Top = 80
      Width = 209
      Height = 21
      Hint = 'Enter the article ID to retreive'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'ArticleIDEdit'
    end
    object PostButton: TButton
      Left = 264
      Top = 56
      Width = 75
      Height = 21
      Hint = 'Post a hard coded article to the group'
      Caption = '&Post'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = PostButtonClick
    end
    object QuitButton: TButton
      Left = 344
      Top = 8
      Width = 75
      Height = 21
      Hint = 'Quit the NNTP server'
      Caption = '&Quit'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = QuitButtonClick
    end
    object FileEdit: TEdit
      Left = 48
      Top = 104
      Width = 209
      Height = 21
      Hint = 'Enter the file name and path for the destination file'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'FileEdit'
    end
    object NewGroupsButton: TButton
      Left = 264
      Top = 104
      Width = 75
      Height = 21
      Hint = 'Request the new groups (hardcoded for 10 days)'
      Caption = 'NewGroups'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = NewGroupsButtonClick
    end
    object NewNewsButton: TButton
      Left = 344
      Top = 104
      Width = 75
      Height = 21
      Hint = 'Request the new news (hardcoded for 1 day)'
      Caption = 'NewNews'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = NewNewsButtonClick
    end
    object HelpButton: TButton
      Left = 264
      Top = 80
      Width = 75
      Height = 21
      Hint = 'Request help from the NNTP server'
      Caption = 'Help'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = HelpButtonClick
    end
    object XOverButton: TButton
      Left = 504
      Top = 104
      Width = 75
      Height = 21
      Hint = 
        'List articles overview. Art. Num. can be a single number, a numb' +
        'er and a dash or two numbers separated by a dash.'
      Caption = 'XOver'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 24
      OnClick = XOverButtonClick
    end
    object OverViewFmtButton: TButton
      Left = 264
      Top = 128
      Width = 75
      Height = 21
      Caption = 'OverViewFmt'
      TabOrder = 25
      OnClick = OverViewFmtButtonClick
    end
    object DateButton: TButton
      Left = 344
      Top = 128
      Width = 75
      Height = 21
      Caption = 'Date'
      TabOrder = 26
      OnClick = DateButtonClick
    end
    object UserEdit: TEdit
      Left = 48
      Top = 128
      Width = 209
      Height = 21
      Hint = 
        'Enter "your name" <your.name@yourcompany.domain> with the double' +
        ' quotes and angle backets.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 27
      Text = 'UserEdit'
    end
    object UserNameEdit: TEdit
      Left = 112
      Top = 152
      Width = 81
      Height = 21
      TabOrder = 28
      Text = 'UserNameEdit'
    end
    object PasswordEdit: TEdit
      Left = 264
      Top = 152
      Width = 73
      Height = 21
      TabOrder = 29
      Text = 'PasswordEdit'
    end
    object AuthenticateButton: TButton
      Left = 344
      Top = 152
      Width = 75
      Height = 21
      Caption = 'Authenticate'
      TabOrder = 30
      OnClick = AuthenticateButtonClick
    end
    object XHdrButton: TButton
      Left = 504
      Top = 128
      Width = 75
      Height = 21
      Caption = 'XHdr'
      TabOrder = 31
      OnClick = XHdrButtonClick
    end
    object ModeReaderButton: TButton
      Left = 424
      Top = 128
      Width = 75
      Height = 21
      Caption = 'Mode Reader'
      TabOrder = 32
      OnClick = ModeReaderButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 185
    Width = 588
    Height = 158
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
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
  object NntpCli1: TNntpCli
    Port = 'nntp'
    LineLimit = 65536
    OnSessionConnected = NntpCli1SessionConnected
    OnSessionClosed = NntpCli1SessionClosed
    OnDataAvailable = NntpCli1DataAvailable
    OnRequestDone = NntpCli1RequestDone
    OnMessageBegin = NntpCli1MessageBegin
    OnMessageEnd = NntpCli1MessageEnd
    OnMessageLine = NntpCli1MessageLine
    OnXHdrBegin = NntpCli1XHdrBegin
    OnXHdrEnd = NntpCli1XHdrEnd
    OnXHdrLine = NntpCli1XHdrLine
    Left = 24
    Top = 224
  end
end
