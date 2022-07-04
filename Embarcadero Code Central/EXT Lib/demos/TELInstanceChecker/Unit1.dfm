object Form1: TForm1
  Left = 196
  Top = 110
  BorderStyle = bsSingle
  Caption = 'TELInstanceChecker demo'
  ClientHeight = 388
  ClientWidth = 484
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
  object Bevel1: TBevel
    Left = 2
    Top = 376
    Width = 481
    Height = 3
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 8
    Top = 369
    Width = 80
    Height = 13
    Caption = 'Extension Library'
    Enabled = False
  end
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 102
    Height = 13
    Caption = 'Resieved data strings'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 457
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'If you see this form then this is the first instance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 26
    Width = 457
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'of this application running in system'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 192
    Top = 64
    Width = 281
    Height = 265
    Lines.Strings = (
      'This example illustrates how to use '
      'TELInstanceChecker.'
      ''
      'TELInstanceChecker. enables you '
      'to detect if there is in the system '
      '(in other active applications) another '
      'similar component with the same '
      'ObjectName property, and also '
      'to transfer to it some data. '
      ''
      'The process of data transfer is completely '
      'thread-safe. Moreover, execution of '
      'the PostData procedure, which sends data '
      'from the application copy, and execution of '
      'the OnResieveData event handler, which '
      'receives data are asynchronous. This '
      'makes the speed of the PostData execution '
      'higher and provides its independent work from'
      'the code of the OnResieveData event handler. '
      ''
      'The component can be used to forbid simultaneous '
      'work of more than one of application copies. '
      ''
      'For instance if you click on a Microsoft'
      'Word icon in Windows Navigator, it will open '
      'some document. If you click again, the second '
      'copy of Microsoft Word will not be open and '
      'another document will appear in the fist working '
      'window. Such application behavior '
      'can be achieved by TELInstanceChecker.'
      ''
      'This example should be started several times. Do '
      'this using Windows navigator or press button '
      '"Start another instance of this application", placed '
      'on the main form. At the same time you will see '
      'current form only after the first start. '
      ''
      'In the rest cases there will appear a dialogue '
      'suggesting to transfer the data line to the first '
      'application. All transferred data will be shown in '
      'the left Memo field of the main form of the first '
      'application copy.  '
      ''
      'As unique object name (the ObjectName property) '
      'you can use the system identifier called GUID (in '
      'the Delphi code editor press Shift+Ctrl+G).'
      ''
      'See also CD Player demo.')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 64
    Width = 177
    Height = 265
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 336
    Width = 273
    Height = 25
    Caption = 'Start another instance of this application'
    TabOrder = 2
    OnClick = Button1Click
  end
end
