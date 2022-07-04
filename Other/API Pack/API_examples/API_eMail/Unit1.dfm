object Form1: TForm1
  Left = 192
  Top = 107
  Caption = 'Form1'
  ClientHeight = 449
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Mail to:'
  end
  object Label2: TLabel
    Left = 8
    Top = 136
    Width = 39
    Height = 13
    Caption = 'Subject:'
  end
  object Label3: TLabel
    Left = 8
    Top = 168
    Width = 27
    Height = 13
    Caption = 'Body:'
  end
  object Label4: TLabel
    Left = 8
    Top = 272
    Width = 34
    Height = 13
    Caption = 'Attach:'
  end
  object Label5: TLabel
    Left = 8
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Copy to:'
  end
  object Label6: TLabel
    Left = 8
    Top = 88
    Width = 46
    Height = 13
    Caption = 'BCopy to:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 328
    Width = 329
    Height = 18
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 144
    Width = 329
    Height = 18
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 112
    Width = 329
    Height = 18
    Shape = bsBottomLine
  end
  object Label7: TLabel
    Left = 8
    Top = 376
    Width = 32
    Height = 13
    Caption = 'Profile:'
  end
  object Label8: TLabel
    Left = 8
    Top = 400
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Edit2: TEdit
    Left = 64
    Top = 136
    Width = 273
    Height = 17
    AutoSize = False
    TabOrder = 0
    Text = 'Subject'
  end
  object body: TMemo
    Left = 64
    Top = 168
    Width = 193
    Height = 97
    Lines.Strings = (
      'Body')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 424
    Width = 249
    Height = 17
    Caption = 'send'
    TabOrder = 2
    OnClick = Button1Click
  end
  object attach: TListBox
    Left = 64
    Top = 272
    Width = 193
    Height = 65
    ItemHeight = 13
    TabOrder = 3
  end
  object Button2: TButton
    Left = 264
    Top = 296
    Width = 73
    Height = 17
    Caption = 'add'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 264
    Top = 320
    Width = 73
    Height = 17
    Caption = 'remove'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Mailto: TMemo
    Left = 64
    Top = 8
    Width = 193
    Height = 33
    TabOrder = 6
  end
  object Copyto: TMemo
    Left = 64
    Top = 48
    Width = 193
    Height = 33
    TabOrder = 7
  end
  object BCopyto: TMemo
    Left = 64
    Top = 88
    Width = 193
    Height = 33
    TabOrder = 8
  end
  object Button4: TButton
    Left = 264
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 264
    Top = 48
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 10
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 264
    Top = 88
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 11
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 264
    Top = 168
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 12
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 264
    Top = 272
    Width = 75
    Height = 17
    Caption = 'Clear'
    TabOrder = 13
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 264
    Top = 424
    Width = 75
    Height = 17
    Caption = 'Exit'
    TabOrder = 14
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 8
    Top = 352
    Width = 329
    Height = 17
    Caption = 'Get Default Profile'
    TabOrder = 15
    OnClick = Button10Click
  end
  object Edit1: TEdit
    Left = 64
    Top = 376
    Width = 273
    Height = 17
    AutoSize = False
    TabOrder = 16
  end
  object Edit3: TEdit
    Left = 64
    Top = 400
    Width = 273
    Height = 17
    AutoSize = False
    PasswordChar = '#'
    TabOrder = 17
  end
  object tAPI_eMail1: TAPI_eMail
    ShowOnSend = False
    OnMapiError = tAPI_eMail1MapiError
    LogonToMapi = True
    Left = 8
    Top = 208
  end
  object OpenDialog1: TOpenDialog
    Left = 40
    Top = 208
  end
end
