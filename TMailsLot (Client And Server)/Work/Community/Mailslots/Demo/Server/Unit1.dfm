object Form1: TForm1
  Left = 244
  Top = 223
  Width = 568
  Height = 292
  Caption = 'Mailslot Server Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Mailslot Name :'
    end
    object Edit1: TEdit
      Left = 88
      Top = 12
      Width = 465
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'mailslotname\demo'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 224
    Width = 560
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 0
      Width = 194
      Height = 32
      Caption = 'Mailslot Server'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Button1: TButton
      Left = 480
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Listen'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 560
    Height = 183
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Panel3'
    TabOrder = 2
    object Memo1: TMemo
      Left = 4
      Top = 4
      Width = 552
      Height = 175
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object MailslotServer1: TMailslotServer
    Active = False
    MailslotName = 'mailslotname'
    MaxSize = 4000
    OnMessageReceived = MailslotServer1MessageReceived
    Left = 16
    Top = 81
  end
end
