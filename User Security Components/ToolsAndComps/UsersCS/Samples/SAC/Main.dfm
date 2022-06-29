object Form1: TForm1
  Left = 241
  Top = 198
  Width = 227
  Height = 283
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object UsersCS1: TUsersCS
    AutoLogin = False
    AppKey = 'SAC'
    FormList.Strings = (
      
        'Form1=Form1|Button1=Button1'#13#10'Button1Parent=Form1'#13#10'Button2=Button' +
        '2'#13#10'Button2Parent=Form1'#13#10)
    Version = '1.90 (27/Dez)'
    DatabaseName = 'dbUsers'
    FileNames83DOS = False
    Left = 64
    Top = 112
  end
  object dbUsers: TDatabase
    DatabaseName = 'dbUsers'
    DriverName = 'STANDARD'
    LoginPrompt = False
    Params.Strings = (
      'PATH=C:\ToolsAndComps\UsersCS\Samples\Data'
      'ENABLE BCD=FALSE'
      'DEFAULT DRIVER=PARADOX')
    SessionName = 'Default'
    TransIsolation = tiDirtyRead
    BeforeConnect = dbUsersBeforeConnect
    Left = 128
    Top = 40
  end
  object UsersCSReg1: TUsersCSReg
    FormName = 'Form1'
    FormCaption = 'Form1'
    ComponentList.Strings = (
      'Button1=Button1'
      'Button1Parent=Form1'
      'Button2=Button2'
      'Button2Parent=Form1')
    SecurityComponent = UsersCS1
    IsRepositoryForm = False
    AutoApplySecurity = True
    Left = 136
    Top = 128
  end
end
