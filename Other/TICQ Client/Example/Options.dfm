object OptionsForm: TOptionsForm
  Left = 218
  Top = 127
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'eICQ Options'
  ClientHeight = 439
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001000001680500001600000028000000100000002000
    0000010008000000000040010000000000000000000000000000000000000000
    0000800080008000000080800000008000000080800000008000C0C0C000C0DC
    C000F0CAA60080808000FF00FF00FF000000FFFF000000FF000000FFFF000000
    FF00FFFFFF00F0FBFF0032323200262626001A1A1A000E0E0E00A4A0A000D4F0
    FF00B1E2FF008ED4FF006BC6FF0048B8FF0025AAFF0000AAFF000092DC00007A
    B90000629600004A730000325000D4E3FF00B1C7FF008EABFF006B8FFF004873
    FF002557FF000055FF000049DC00003DB900003196000025730000195000D4D4
    FF00B1B1FF008E8EFF006B6BFF004848FF002525FF000000FF000000DC000000
    B900000096000000730000005000E3D4FF00C7B1FF00AB8EFF008F6BFF007348
    FF005725FF005500FF004900DC003D00B900310096002500730019005000F0D4
    FF00E2B1FF00D48EFF00C66BFF00B848FF00AA25FF00AA00FF009200DC007A00
    B900620096004A00730032005000FFD4FF00FFB1FF00FF8EFF00FF6BFF00FF48
    FF00FF25FF00FF00FF00DC00DC00B900B900960096007300730050005000FFD4
    F000FFB1E200FF8ED400FF6BC600FF48B800FF25AA00FF00AA00DC009200B900
    7A009600620073004A0050003200FFD4E300FFB1C700FF8EAB00FF6B8F00FF48
    7300FF255700FF005500DC004900B9003D00960031007300250050001900FFD4
    D400FFB1B100FF8E8E00FF6B6B00FF484800FF252500FF000000DC000000B900
    0000960000007300000050000000FFE3D400FFC7B100FFAB8E00FF8F6B00FF73
    4800FF572500FF550000DC490000B93D0000963100007325000050190000FFF0
    D400FFE2B100FFD48E00FFC66B00FFB84800FFAA2500FFAA0000DC920000B97A
    000096620000734A000050320000FFFFD400FFFFB100FFFF8E00FFFF6B00FFFF
    4800FFFF2500FFFF0000DCDC0000B9B90000969600007373000050500000F0FF
    D400E2FFB100D4FF8E00C6FF6B00B8FF4800AAFF2500AAFF000092DC00007AB9
    0000629600004A73000032500000E3FFD400C7FFB100ABFF8E008FFF6B0073FF
    480057FF250055FF000049DC00003DB90000319600002573000019500000D4FF
    D400B1FFB1008EFF8E006BFF6B0048FF480025FF250000FF000000DC000000B9
    0000009600000073000000500000D4FFE300B1FFC7008EFFAB006BFF8F0048FF
    730025FF570000FF550000DC490000B93D00009631000073250000501900D4FF
    F000B1FFE2008EFFD4006BFFC60048FFB80025FFAA0000FFAA0000DC920000B9
    7A000096620000734A0000503200D4FFFF00B1FFFF008EFFFF006BFFFF0048FF
    FF0025FFFF0000FFFF0000DCDC0000B9B900009696000073730000505000F2F2
    F200E6E6E600DADADA00CECECE00C2C2C200B6B6B600AAAAAA009E9E9E009292
    9200868686007A7A7A006E6E6E0062626200565656004A4A4A003E3E3E000000
    0000000000000000000000000000F3F8F8F8F8F8F8F8F8F8F8F8F8F8F500F711
    111111111111111111111111F800F711F316FAF111F4F411F411F411F800F711
    161116FA11FAFA11FA11FA11F800F711111111161111111111111111F800F711
    111111111111111111111111F800F711111111111111111111111111F800F711
    F316FAF111F4F411F4F4F411F800F711161116FA11FAFA11FAFAFA11F800F711
    111111161111111111111111F800F711111111111111111111111111F800F7F1
    F1F1F1F1F1F1F1F1F1F1F1F1F80098989898989898989898989898989800891B
    7D89897D8989899189918933980087898989898989898989898989898700FFFF
    0000000100000001000000010000000100000001000000010000000100000001
    000000010000000100000001000000010000000100000001000000010000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Notebook: TNotebook
    Left = 152
    Top = 8
    Width = 481
    Height = 385
    PageIndex = 2
    TabOrder = 1
    object TPage
      Left = 0
      Top = 0
      Caption = 'Window'
      object gbWindow: TGroupBox
        Left = 16
        Top = 16
        Width = 449
        Height = 129
        Caption = ' Window '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 92
          Width = 66
          Height = 13
          Caption = 'Title bar text:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbOnTop: TCheckBox
          Left = 16
          Top = 24
          Width = 217
          Height = 17
          Caption = 'Always on top'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object edTitleText: TEdit
          Left = 88
          Top = 88
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 15
          ParentFont = False
          TabOrder = 2
        end
        object cbHide: TCheckBox
          Left = 16
          Top = 48
          Width = 217
          Height = 17
          Caption = 'Hide main window instead of closing'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
      object gbTrans: TGroupBox
        Left = 16
        Top = 160
        Width = 449
        Height = 113
        Caption = ' Translucency option (Windows 2000/XP only) '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object lblTransp: TLabel
          Left = 352
          Top = 66
          Width = 29
          Height = 13
          Caption = '100%'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbTransparent: TCheckBox
          Left = 16
          Top = 24
          Width = 217
          Height = 17
          Caption = 'Transparent contact list'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = cbTransparentClick
        end
        object tbTransparent: TTrackBar
          Left = 64
          Top = 64
          Width = 273
          Height = 25
          Max = 100
          TabOrder = 1
          ThumbLength = 11
          TickMarks = tmTopLeft
          TickStyle = tsNone
          OnChange = tbTransparentChange
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Network'
      object gbProxy: TGroupBox
        Left = 16
        Top = 16
        Width = 449
        Height = 233
        Caption = ' Proxy settings '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblProxyType: TLabel
          Left = 16
          Top = 32
          Width = 57
          Height = 13
          Caption = 'Proxy type:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblProxyHost: TLabel
          Left = 16
          Top = 72
          Width = 66
          Height = 13
          Caption = 'Proxy server:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblProxyPort1: TLabel
          Left = 248
          Top = 72
          Width = 24
          Height = 13
          Caption = 'Port:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblProxyPort2: TLabel
          Left = 360
          Top = 72
          Width = 61
          Height = 13
          Caption = '(often 1080)'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblProxyUserID: TLabel
          Left = 32
          Top = 152
          Width = 52
          Height = 13
          Caption = 'Username:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblProxyPass: TLabel
          Left = 248
          Top = 152
          Width = 50
          Height = 13
          Caption = 'Password:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbProxyType: TComboBox
          Left = 104
          Top = 24
          Width = 121
          Height = 21
          Style = csDropDownList
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ItemIndex = 4
          ParentFont = False
          TabOrder = 0
          Text = 'Proxy disabled'
          OnChange = cbProxyTypeChange
          Items.Strings = (
            'SOCKS4'
            'SOCKS5'
            'HTTP'
            'HTTPS'
            'Proxy disabled')
        end
        object edProxyHost: TEdit
          Left = 104
          Top = 64
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 50
          ParentFont = False
          TabOrder = 1
        end
        object edProxyPort: TEdit
          Left = 296
          Top = 64
          Width = 49
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 5
          ParentFont = False
          TabOrder = 2
          Text = '1080'
        end
        object cbProxyAuth: TCheckBox
          Left = 16
          Top = 112
          Width = 273
          Height = 17
          Caption = 'Proxy requires authorization'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object edProxyUserID: TEdit
          Left = 104
          Top = 144
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 50
          ParentFont = False
          TabOrder = 4
        end
        object edProxyPass: TPasswordEdit
          Left = 312
          Top = 144
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 50
          ParentFont = False
          TabOrder = 5
        end
        object cbProxyResolve: TCheckBox
          Left = 16
          Top = 192
          Width = 273
          Height = 17
          Caption = 'Resolve hostnames through proxy'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'ICQ'
      object lblICQChange: TLabel
        Left = 80
        Top = 320
        Width = 297
        Height = 49
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'You will need to reconnect to the ICQ network for the changes yo' +
          'u have made on this page to take effect.'
        Enabled = False
        Visible = False
        WordWrap = True
      end
      object gbICQ: TGroupBox
        Left = 16
        Top = 16
        Width = 449
        Height = 161
        Caption = ' ICQ '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblUIN: TLabel
          Left = 56
          Top = 32
          Width = 22
          Height = 13
          Alignment = taRightJustify
          Caption = 'UIN:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblPasswd: TLabel
          Left = 29
          Top = 64
          Width = 50
          Height = 13
          Alignment = taRightJustify
          Caption = 'Password:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblCreateUIN: TLabel
          Left = 16
          Top = 104
          Width = 237
          Height = 13
          Caption = 'Create a new ICQ account using the ICQ website'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = 13339492
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = lblCreateUINClick
        end
        object lblRetPasswd: TLabel
          Left = 16
          Top = 128
          Width = 193
          Height = 13
          Caption = 'Retrieve a lost password or ICQ number'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = 13339492
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          ParentFont = False
          OnClick = lblRetPasswdClick
        end
        object edUIN: TEdit
          Left = 96
          Top = 24
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 10
          ParentFont = False
          TabOrder = 0
          OnChange = edUINChange
        end
        object edPasswd: TPasswordEdit
          Left = 96
          Top = 56
          Width = 121
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 15
          ParentFont = False
          TabOrder = 1
          OnChange = edPasswdChange
        end
      end
      object gbExpert: TGroupBox
        Left = 16
        Top = 192
        Width = 449
        Height = 113
        Caption = ' Advanced settings '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object lblICQServer: TLabel
          Left = 16
          Top = 32
          Width = 64
          Height = 13
          Caption = 'Login Server:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblICQPort: TLabel
          Left = 248
          Top = 32
          Width = 24
          Height = 13
          Caption = 'Port:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edICQServer: TEdit
          Left = 96
          Top = 24
          Width = 137
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 50
          ParentFont = False
          TabOrder = 0
          Text = 'login.icq.com'
          OnChange = edICQServerChange
        end
        object edICQPort: TEdit
          Left = 288
          Top = 24
          Width = 49
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 5
          ParentFont = False
          TabOrder = 1
          Text = '5190'
          OnChange = edICQPortChange
        end
        object btnReset: TButton
          Left = 368
          Top = 24
          Width = 57
          Height = 21
          Caption = 'Reset'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = btnResetClick
        end
        object cbKeepAlive: TCheckBox
          Left = 16
          Top = 64
          Width = 409
          Height = 33
          Caption = 
            'Keep connection alive (send a blank packet every minute to preve' +
            'nt some proxies idling out)'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          WordWrap = True
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'AutoAway'
      object gbAutoAway: TGroupBox
        Left = 16
        Top = 16
        Width = 449
        Height = 169
        Caption = ' Auto Away '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object lblAwayMin: TLabel
          Left = 272
          Top = 96
          Width = 96
          Height = 13
          Caption = 'minutes of inactivity'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblNaMin: TLabel
          Left = 272
          Top = 136
          Width = 96
          Height = 13
          Caption = 'minutes of inactivity'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbOnSaver: TCheckBox
          Left = 16
          Top = 24
          Width = 369
          Height = 17
          Caption = 'Set Away mode when screen saver engages'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object cbOnWLock: TCheckBox
          Left = 16
          Top = 56
          Width = 369
          Height = 17
          Caption = 'Set Away mode when workstation is locked'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object cbOnMouse: TCheckBox
          Left = 16
          Top = 96
          Width = 185
          Height = 17
          Caption = 'Set Away mode after'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = cbOnMouseClick
        end
        object cbSetNA: TCheckBox
          Left = 16
          Top = 136
          Width = 185
          Height = 17
          Caption = 'Set N/A mode after'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = cbSetNAClick
        end
        object edAwayTime: TEdit
          Left = 216
          Top = 88
          Width = 41
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 4
          ParentFont = False
          TabOrder = 3
          Text = '5'
        end
        object edNATime: TEdit
          Left = 216
          Top = 128
          Width = 41
          Height = 21
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 4
          ParentFont = False
          TabOrder = 5
          Text = '20'
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'StatusMsgs'
      object gbStatusMsgs: TGroupBox
        Left = 16
        Top = 16
        Width = 449
        Height = 201
        Caption = ' Status Messages '
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = 13977088
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label5: TLabel
          Left = 16
          Top = 64
          Width = 87
          Height = 13
          Caption = 'Use this message:'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 56
          Top = 168
          Width = 310
          Height = 13
          Alignment = taCenter
          Caption = 'Use %time% for the current time, %date% for the current date'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object cbAwayMsgs: TComboBox
          Left = 16
          Top = 24
          Width = 417
          Height = 21
          Style = csDropDownList
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ItemIndex = 0
          ParentFont = False
          TabOrder = 0
          Text = 'Away'
          OnChange = cbAwayMsgsChange
          Items.Strings = (
            'Away'
            'N/A'
            'Occupied'
            'DND'
            'Free for chat')
        end
        object mAwayMsg: TMemo
          Left = 56
          Top = 88
          Width = 329
          Height = 73
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 100
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
          OnExit = mAwayMsgExit
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Empty'
      object Label2: TLabel
        Left = 0
        Top = 176
        Width = 473
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Please select a subentry from the list'
      end
    end
  end
  object TreeView: TTreeView
    Left = 8
    Top = 8
    Width = 137
    Height = 385
    HideSelection = False
    HotTrack = True
    Indent = 19
    MultiSelectStyle = []
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeViewChange
    Items.Data = {
      03000000250000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
      0C436F6E74616374204C697374200000000000000000000000FFFFFFFFFFFFFF
      FF00000000000000000757696E646F7773200000000000000000000000FFFFFF
      FFFFFFFFFF0000000001000000074E6574776F726B1C00000000000000000000
      00FFFFFFFFFFFFFFFF0000000000000000034943511F00000000000000000000
      00FFFFFFFFFFFFFFFF0000000002000000065374617475732200000000000000
      00000000FFFFFFFFFFFFFFFF0000000000000000094175746F20417761792800
      00000000000000000000FFFFFFFFFFFFFFFF00000000000000000F5374617475
      73204D65737361676573}
  end
  object btnOK: TButton
    Left = 376
    Top = 408
    Width = 75
    Height = 23
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 464
    Top = 408
    Width = 75
    Height = 23
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object btnApply: TButton
    Left = 552
    Top = 408
    Width = 75
    Height = 23
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnApplyClick
  end
end
