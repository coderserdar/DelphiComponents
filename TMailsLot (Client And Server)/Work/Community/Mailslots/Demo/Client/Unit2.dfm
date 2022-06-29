object Form2: TForm2
  Left = 305
  Top = 256
  Width = 568
  Height = 292
  Caption = 'Mailslot client demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Mailslot Name :'
    end
    object Label2: TLabel
      Left = 256
      Top = 52
      Width = 123
      Height = 13
      Caption = 'Computer/Domain Name :'
    end
    object Edit1: TEdit
      Left = 88
      Top = 12
      Width = 281
      Height = 21
      TabOrder = 0
      Text = 'mailslotname\demo'
    end
    object ComboBox1: TComboBox
      Left = 400
      Top = 12
      Width = 152
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = ComboBox1Change
      Items.Strings = (
        'Default domain broadcast'
        'Computer broadcast'
        'Domain  broadcast'
        'Local mailslot')
    end
    object Edit2: TEdit
      Left = 400
      Top = 48
      Width = 152
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
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
    object Label3: TLabel
      Left = 8
      Top = 0
      Width = 186
      Height = 32
      Caption = 'Mailslot Client'
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
      Caption = '&Send'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 73
    Width = 560
    Height = 151
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Panel3'
    TabOrder = 2
    object Memo1: TMemo
      Left = 4
      Top = 4
      Width = 552
      Height = 143
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object MailslotClient1: TMailslotClient
    Active = False
    MailslotName = 'mailslotname'
    MailslotKind = mkBroadcast
    MailslotTarget = '*'
    Left = 232
    Top = 121
  end
end
