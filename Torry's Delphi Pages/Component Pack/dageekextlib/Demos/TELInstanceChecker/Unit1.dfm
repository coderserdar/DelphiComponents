object Form1: TForm1
  Left = 165
  Top = 111
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
      'Этот пример показывает использование'
      'компонента TELInstanceChecker.'
      ''
      'Компонент позволяет определить,'
      'существует ли в системе (в других'
      'запущенных приложениях) еще один такой'
      'же компонент с таким же свойством'
      'ObjectName, а также передать ему'
      'какие-либо данные.'
      ''
      'Процесс передачи данных полностью'
      'потоко защищенный. Более того'
      'выполнение процедуры PostData'
      'посылающей данные копии приложения'
      'и выполнение обработчика события'
      'OnResieveData принимающей данные'
      'копии приложения происходит'
      'асинхронно, что повышает скорость'
      'выполнения процедуры PostData и'
      'делает ее выполнение независимым от'
      'кода обработчика события OnResieveData.'
      ''
      'Компонент может использоваться для'
      'организации невозможности одновременного'
      'существования в системе более одной копии'
      'приложения.'
      ''
      'Например, если в проводнике Windows'
      'щелкнуть на иконке документа Microsoft'
      'Word, то он откроется во вновь запущенной'
      'программе Microsoft Word. Но если затем'
      'щелкнуть на иконке другого документа'
      'Microsoft Word, то запуска второго'
      'приложения не происходит, документ'
      'открывается в уже работающей программе.'
      'Такое поведение приложения можно'
      'организовать при помощи компонента'
      'TELInstanceChecker.'
      ''
      'Данный пример нужно запустить несколько'
      'раз. Это можно сделать при помощи'
      'проводника Windows или кнопкой'
      '"Start another instance of this'
      'application", находящейся на этой форме.'
      'При этом текущую форму Вы увидите'
      'только при первом запуске приложения.'
      'В остальных случаях на экране появится'
      'диалог, предлагающий передать строку'
      'данных этому (первому) приложению.'
      'Все переданные данные будут показыватся'
      'в левом Memo поле этой формы.'
      ''
      'В качестве уникального имени объекта'
      '(свойство ObjectName) можно использовать'
      'GUID (в редакторе кода Delphi нажмите'
      'Shift+Ctrl+G).'
      ''
      'См. также: CD Player demo.')
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
