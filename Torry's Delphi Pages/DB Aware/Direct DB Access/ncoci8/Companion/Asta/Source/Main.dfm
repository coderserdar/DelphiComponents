object frmMain: TfrmMain
  Left = 423
  Top = 156
  Width = 647
  Height = 369
  Caption = 'Asta NCOCI8 Server-www.astatech.com'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  WindowState = wsMinimized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 639
    Height = 342
    ActivePage = MemoTabsheet
    Align = alClient
    TabOrder = 0
    object MemoTabsheet: TTabSheet
      Caption = 'Client Requests'
      object mRequests: TMemo
        Left = 0
        Top = 0
        Width = 631
        Height = 314
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object ConnectedTabSheet: TTabSheet
      Caption = 'Connected Users'
      object UserGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 631
        Height = 314
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Chat Line'
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 633
        Height = 113
        Caption = ' Received Messages '
        TabOrder = 0
        object ReceiveMemo: TMemo
          Left = 6
          Top = 15
          Width = 620
          Height = 82
          TabOrder = 0
        end
      end
      object BitBtn1: TBitBtn
        Left = 2
        Top = 137
        Width = 143
        Height = 25
        Caption = 'BroadCast Popup Message'
        TabOrder = 1
        OnClick = BitBtn1Click
      end
      object GroupBox1: TGroupBox
        Left = 2
        Top = 180
        Width = 632
        Height = 131
        Caption = ' Enter Outgoing Messages  Below '
        TabOrder = 2
        object SendMemo: TMemo
          Left = 7
          Top = 16
          Width = 617
          Height = 108
          Lines.Strings = (
            'SendMemo')
          TabOrder = 0
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Client Errors'
      object ErrorMemo: TMemo
        Left = 0
        Top = 0
        Width = 631
        Height = 314
        Align = alClient
        Lines.Strings = (
          'ErrorMemo')
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Broadcasts'
      object Memo1: TMemo
        Left = 22
        Top = 8
        Width = 593
        Height = 129
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          
            'ASTA provides two methods to broadcast messages to all clients (' +
            'the "Messaging" page exposes how to send messages '
          
            'to a single client or to a subset of clients).  You should use t' +
            'his page with the "Messages" tutorial.'
          ''
          
            'SENDBROADCASTPOPUP(S: String);  This method sends your message t' +
            'o all connected clients and displays the message '
          
            'in a "ShowMessage" dialog.  This is done automatically and requi' +
            'res no coding by the developer.'
          ''
          
            'The SENDBROADCAST(S: String);  method raises the OnServerBroadca' +
            'st even handler on the client.  By writing custom '
          
            'code for that event, the broadcast can be displayed or handled a' +
            's you see fit.')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object Button3: TButton
        Left = 20
        Top = 149
        Width = 280
        Height = 25
        Caption = 'Send A Popup Broadcast'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 333
        Top = 149
        Width = 280
        Height = 25
        Caption = 'Send Broadcast to OnServerBroadCast Event'
        TabOrder = 2
        OnClick = Button4Click
      end
      object mBroadcastMessage: TMemo
        Left = 334
        Top = 186
        Width = 280
        Height = 117
        Lines.Strings = (
          'So that developers can have control over the way that '
          'broadcast messages are displayed and handled, ASTA '
          'provides a SENDBROADCAST method.  '
          ''
          'The SendBroadcast method corresponds to the '
          'AstaClientSocket'#39's OnServerBroadcast Event.  If you wish '
          'to provide for special handling of your broadcast, assign '
          'your code to that event handler.')
        ReadOnly = True
        TabOrder = 3
      end
      object mBroadcastPopupMessage: TMemo
        Left = 22
        Top = 186
        Width = 280
        Height = 118
        Lines.Strings = (
          'ASTA currently provides two broadcast methods.  '
          'SendBroadcastPopup will send a message to '
          'every connected client.  When the client recieves '
          'the message, a ShowMessage dialog will display '
          'the message.'
          ''
          'All clients will receive this message.')
        ReadOnly = True
        TabOrder = 4
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Messaging'
      object CheckListBox1: TCheckListBox
        Left = 3
        Top = 16
        Width = 286
        Height = 289
        ItemHeight = 13
        TabOrder = 0
      end
      object Button5: TButton
        Left = 296
        Top = 16
        Width = 321
        Height = 25
        Caption = 'Disconnect Selected Users'
        TabOrder = 1
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 296
        Top = 48
        Width = 321
        Height = 25
        Caption = 'SendSelectPopup Message to the Selected Users'
        TabOrder = 2
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 296
        Top = 80
        Width = 321
        Height = 25
        Caption = 'SendSelectCoded Message to the Selected Users'
        TabOrder = 3
        OnClick = Button7Click
      end
      object mSelectSend: TMemo
        Left = 299
        Top = 112
        Width = 324
        Height = 193
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Login'
      object Memo2: TMemo
        Left = 12
        Top = 8
        Width = 607
        Height = 89
        Color = clSilver
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          
            'The OnClientLogin event handler should be used to handle user ve' +
            'rification.  It exposes UserName, Password, and the '
          'Application Name:'
          ''
          
            'ClientLogin(Sender: TObject; UserName, Password, AppName: String' +
            '; var Verified);'
          ''
          
            'A valid login should set Verified := True;  A bad attempt should' +
            ' set Verified := False;')
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
      object mLogin: TMemo
        Left = 12
        Top = 112
        Width = 607
        Height = 193
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
  object AstaServerSocket1: TAstaServerSocket
    Active = False
    Port = 9000
    ServerType = stNonBlocking
    OnClientConnect = AstaServerSocket1ClientConnect
    OnClientDisconnect = AstaServerSocket1ClientDisconnect
    OnTransactionBegin = AstaServerSocket1TransactionBegin
    OnTransactionEnd = AstaServerSocket1TransactionEnd
    ThreadedDBSupplyQuery = AstaServerSocket1ThreadedDBSupplyQuery
    ThreadedDBSupplySession = AstaServerSocket1ThreadedDBSupplySession
    DisposeofQueriesForThreads = False
    MetaDataSet = AstaDataSet1
    UserCheckList = CheckListBox1
    AstaServerName = 'NCOCI8 Server'
    OnStoredProcedure = AstaServerSocket1StoredProcedure
    OnSubmitSQL = AstaServerSocket1SubmitSQL
    OnShowServerMessage = AstaServerSocket1ShowServerMessage
    OnFetchMetaData = AstaServerSocket1FetchMetaData
    OnChatLine = AstaServerSocket1ChatLine
    OnExecRowsAffected = AstaServerSocket1ExecRowsAffected
    OnExecSQLParamList = AstaServerSocket1ExecSQLParamList
    OnFetchBlobEvent = AstaServerSocket1FetchBlobEvent
    OnClientDBLogin = AstaServerSocket1ClientDBLogin
    OnClientLogin = AstaServerSocket1ClientLogin
    OnEncrypt = AstaServerSocket1Encrypt
    OnDecrypt = AstaServerSocket1Decrypt
    OnCodedMessage = AstaServerSocket1CodedMessage
    ThreadingModel = tmPersistentSessions
    ServerSQLGenerator = AstaSQLGenerator1
    Left = 19
    Top = 50
  end
  object UserDataSet: TAstaDataSet
    Active = True
    Constraints = <>
    Left = 88
    Top = 52
    DetailFilter = ''
    FastFields = (
      'Address,1,15'
      'Activity,1,25'
      'Date,11,0')
    object UserDataSetAddress: TStringField
      FieldName = 'Address'
      Size = 15
    end
    object UserDataSetActivity: TStringField
      FieldName = 'Activity'
      Size = 25
    end
    object UserDataSetDate: TDateTimeField
      DisplayLabel = 'Asta Date Display Label'
      DisplayWidth = 50
      FieldName = 'Date'
      DisplayFormat = 'mm/dd/yyyy tt'
    end
  end
  object DataSource1: TDataSource
    DataSet = UserDataSet
    Left = 115
    Top = 52
  end
  object AstaSQLGenerator1: TAstaSQLGenerator
    DateMaskForSQL = 'DD-MMM-YYYY'
    DateTimeMaskForSQL = 'DD-MMM-YYYY'
    SQLDialect = sqlOracle
    Left = 20
    Top = 80
  end
  object AstaMetaData: TAsta2MetaData
    OnDBMSName = AstaMetaDataDBMSName
    OnTables = AstaMetaDataTables
    OnIndexes = AstaMetaDataIndexes
    OnFields = AstaMetaDataFields
    OnViews = AstaMetaDataViews
    OnStoredProcs = AstaMetaDataStoredProcs
    OnPrimeKeys = AstaMetaDataPrimeKeys
    OnStoredProcColumns = AstaMetaDataStoredProcColumns
    OnVCLFields = AstaMetaDataFields
    AstaServerSocket = AstaServerSocket1
    AliasFileName = 'Oracle'
    Left = 20
    Top = 112
  end
  object AstaDataSet1: TAstaDataSet
    Constraints = <>
    Left = 20
    Top = 144
    DetailFilter = ''
    FastFields = ()
  end
end
