object MainForm: TMainForm
  Left = 68
  Top = 59
  Caption = 'Delphi Registry and Initialisation Variables Code Generator'
  ClientHeight = 452
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Status: TStatusBar
    Left = 0
    Top = 433
    Width = 521
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 521
    Height = 433
    ActivePage = TabSheet6
    Align = alClient
    MultiLine = True
    TabOrder = 1
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Form Available Components'
      object FormComps: TMemo
        Left = 0
        Top = 0
        Width = 513
        Height = 385
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        OnChange = Changed
        OnDblClick = FormCompsDblClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Saved Form Components'
      object SavedProps: TMemo
        Left = 0
        Top = 0
        Width = 513
        Height = 385
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = SavedPropsDblClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Supported Properties'
      object SuppProps: TMemo
        Left = 0
        Top = 0
        Width = 513
        Height = 385
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = SuppPropsDblClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Code Statement Masks'
      object CodeMasks: TMemo
        Left = 0
        Top = 0
        Width = 513
        Height = 385
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        OnDblClick = CodeMasksDblClick
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Operations'
      ExplicitTop = 42
      ExplicitHeight = 387
      object Label8: TLabel
        Left = 8
        Top = 8
        Width = 87
        Height = 14
        Caption = 'Old Specifications'
      end
      object Label4: TLabel
        Left = 8
        Top = 152
        Width = 80
        Height = 14
        Caption = 'Project Directory'
      end
      object Label7: TLabel
        Left = 8
        Top = 204
        Width = 72
        Height = 42
        Caption = 'Array Elements (zero none)'
        WordWrap = True
      end
      object Label5: TLabel
        Left = 156
        Top = 216
        Width = 47
        Height = 14
        Caption = 'File Prefix'
      end
      object LabelCopyright: TLabel
        Left = 16
        Top = 336
        Width = 350
        Height = 55
        AutoSize = False
        Caption = 
          'Copyright Magenta Systems Ltd, http://www.magsys.co.uk/delphi/'#13#10 +
          'Version 1.4 - 11th May 2010'
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label2: TLabel
        Left = 340
        Top = 8
        Width = 91
        Height = 14
        Caption = 'Output Files/Masks'
      end
      object OldSpecs: TComboBox
        Left = 8
        Top = 32
        Width = 313
        Height = 22
        ItemHeight = 14
        Sorted = True
        TabOrder = 0
        OnDblClick = doOpenOldClick
      end
      object OutDir: TDirectoryEdit
        Left = 8
        Top = 172
        Width = 317
        Height = 21
        NumGlyphs = 1
        TabOrder = 6
      end
      object ArraySize: TSpinEdit
        Left = 88
        Top = 211
        Width = 49
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 7
        Value = 0
      end
      object FilePrefix: TEdit
        Left = 208
        Top = 212
        Width = 89
        Height = 22
        TabOrder = 8
        Text = 'Unit'
      end
      object doClear: TButton
        Left = 24
        Top = 68
        Width = 80
        Height = 25
        Caption = 'Clear'
        TabOrder = 1
        OnClick = doClearClick
      end
      object doOpenOld: TButton
        Left = 120
        Top = 68
        Width = 80
        Height = 25
        Caption = 'Open Old'
        TabOrder = 2
        OnClick = doOpenOldClick
      end
      object doOpenAny: TButton
        Left = 220
        Top = 68
        Width = 80
        Height = 25
        Caption = 'Open Any'
        TabOrder = 3
        OnClick = doOpenAnyClick
      end
      object doVarCreate: TButton
        Left = 24
        Top = 296
        Width = 80
        Height = 25
        Caption = 'Write Code '
        TabOrder = 9
        OnClick = doVarCreateClick
      end
      object doSelectSave: TButton
        Left = 48
        Top = 116
        Width = 125
        Height = 25
        Caption = 'Select Saved Propeties'
        TabOrder = 4
        OnClick = doSelectSaveClick
      end
      object doSave: TButton
        Left = 120
        Top = 296
        Width = 80
        Height = 25
        Caption = 'Save Spec'
        TabOrder = 10
        OnClick = doSaveClick
      end
      object doExit: TButton
        Left = 220
        Top = 296
        Width = 80
        Height = 25
        Caption = 'Exit'
        TabOrder = 11
        OnClick = doExitClick
      end
      object OutFiles: TMemo
        Left = 336
        Top = 32
        Width = 161
        Height = 285
        TabOrder = 12
        OnDblClick = OutFilesDblClick
      end
      object doFont: TButton
        Left = 196
        Top = 116
        Width = 80
        Height = 25
        Caption = 'Editing Font'
        TabOrder = 5
        OnClick = doFontClick
      end
    end
  end
  object FormStorage: TFormStorage
    IniFileName = 'Software\Magenta-Systems\Delphvar'
    IniSection = 'Common'
    UseRegistry = True
    StoredProps.Strings = (
      'ArraySize.Value'
      'CodeMasks.Lines'
      'FilePrefix.Text'
      'FormComps.Lines'
      'OutDir.Text'
      'OutFiles.Lines'
      'SuppProps.Lines'
      'FontDialog.Font'
      'OldSpecs.Items'
      'OldSpecs.Text'
      'SavedProps.Lines')
    StoredValues = <>
    Left = 480
    Top = 188
  end
  object DefSuppProps: TStrHolder
    Capacity = 44
    Macros = <>
    Duplicates = dupError
    Sorted = True
    Left = 480
    Top = 88
    InternalVer = 1
    StrData = (
      ''
      
        '5442434746696c7465724469616c6f672e4272696768746e657373202d20496e' +
        '7465676572'
      
        '5442434746696c7465724469616c6f672e436f6e7472617374202d20446f7562' +
        '6c65'
      '5442434746696c7465724469616c6f672e47616d6d61202d20446f75626c65'
      '54436865636b426f782e436865636b6564202d20426f6f6c65616e'
      '54436865636b426f782e5374617465202d2054436865636b426f785374617465'
      
        '54436f6c6f72436f6d626f426f782e436f6c6f7256616c7565202d20496e7465' +
        '676572'
      
        '54436f6c6f72436f6d626f426f782e4974656d496e646578202d20496e746567' +
        '6572'
      '54436f6c6f724469616c6f672e436f6c6f72202d20496e7465676572'
      
        '54436f6c6f724469616c6f672e437573746f6d436f6c6f7273202d2054537472' +
        '696e674c697374'
      '54436f6d626f426f782e4974656d496e646578202d20496e7465676572'
      '54436f6d626f426f782e54657874202d20537472696e67'
      '544469726563746f7279456469742e54657874202d20537472696e67'
      
        '544469726563746f72794c697374426f782e4469726563746f7279202d205374' +
        '72696e67'
      '544472697665436f6d626f426f782e4472697665202d20537472696e67'
      '54456469742e54657874202d20537472696e67'
      '5446696c656e616d65456469742e54657874202d20537472696e67'
      '54466f6e74436f6d626f426f782e466f6e74202d2054466f6e74'
      '54466f6e744469616c6f672e466f6e74202d2054466f6e74'
      '544c6162656c2e43617074696f6e202d20537472696e67'
      '544c6162656c2e466f6e74202d2054466f6e74'
      '544c697374426f782e4974656d496e646578202d20496e7465676572'
      '544c697374426f782e4974656d73202d2054537472696e674c697374'
      '544d61736b6564456469742e54657874202d20537472696e67'
      '544d656d6f2e4c696e6573202d2054537472696e674c697374'
      '544d656e754974656d2e436865636b6564202d20426f6f6c65616e'
      '544f70656e4469616c6f672e46696c656e616d65202d20537472696e67'
      '544f70656e4469616c6f672e496e697469616c446972202d20537472696e67'
      '54526164696f427574746f6e2e436865636b6564202d20426f6f6c65616e'
      '54526164696f47726f75702e4974656d496e646578202d20496e7465676572'
      '54536176654469616c6f672e46696c656e616d65202d20537472696e67'
      '54536176654469616c6f672e496e697469616c446972202d20537472696e67'
      '545370696e456469742e56616c7565202d20496e7465676572')
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'sdt'
    Filter = 'Saved Details (*.sdt) |*.sdt'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select Saved Details File to Open'
    Left = 480
    Top = 292
  end
  object DefFiles: TStrHolder
    Capacity = 8
    Macros = <>
    Left = 480
    Top = 136
    InternalVer = 1
    StrData = (
      ''
      '4456202d2064656676617273'
      '5256202d207265616476617273'
      '5756202d20777269746576617273'
      '4356202d20636c65617276617273'
      '5243202d2072656164636f6d70'
      '5743202d207772697465636f6d70'
      '4343202d20636c656172636f6d70')
  end
  object Defmasks: TStrHolder
    Capacity = 60
    Macros = <>
    Duplicates = dupError
    Sorted = True
    Left = 480
    Top = 32
    InternalVer = 1
    StrData = (
      ''
      '4343426f6f6c65616e202d2024636f6d70203a3d2066616c7365203b'
      '4343446f75626c65202d2024636f6d70203a3d2030203b'
      '4343496e7465676572202d2024636f6d70203a3d2030203b'
      '4343537472696e67202d2024636f6d70203a3d202727203b'
      
        '434354436865636b426f785374617465202d2024636f6d70203a3d206362556e' +
        '636865636b6564203b'
      
        '434354466f6e74202d2024636f6d70203a3d204c6f6164466f6e742028277827' +
        '29203b'
      '434354537472696e674c697374202d2024636f6d702e436c656172203b'
      '4356426f6f6c65616e202d2024766172203a3d2066616c7365203b'
      '4356446f75626c65202d2024766172203a3d2030203b'
      '4356496e7465676572202d2024766172203a3d2030203b'
      '4356537472696e67202d2024766172203a3d202727203b'
      
        '435654436865636b426f785374617465202d2024766172203a3d206362556e63' +
        '6865636b6564203b'
      
        '435654466f6e74202d2024766172203a3d204c6f6164466f6e74202827782729' +
        '203b'
      '435654537472696e674c697374202d20247661722e436c656172203b'
      '4456426f6f6c65616e202d20247661723a202474797065203b'
      '4456446f75626c65202d20247661723a202474797065203b'
      '4456496e7465676572202d20247661723a202474797065203b'
      '4456537472696e67202d20247661723a202474797065203b'
      '445654466f6e74202d20247661723a202474797065203b'
      '445654537472696e674c697374202d20247661723a202474797065203b'
      
        '5243426f6f6c65616e202d2069662052656164537472696e6720287365637469' +
        '6f6e2c2027246c6974272c202746616c73652729203d20275472756527207468' +
        '656e2024636f6d70203a3d207472756520656c73652024636f6d70203a3d2066' +
        '616c7365203b'
      
        '5243446f75626c65202d2024636f6d70203a3d2052656164466c6f6174202873' +
        '656374696f6e2c2027246c6974272c203029203b'
      
        '5243496e7465676572202d2024636f6d70203a3d2052656164496e7465676572' +
        '202873656374696f6e2c2027246c6974272c203029203b'
      
        '5243537472696e67202d2024636f6d70203a3d2052656164537472696e672028' +
        '73656374696f6e2c2027246c6974272c20272729203b'
      
        '524354436865636b426f785374617465202d2024636f6d70203a3d2054436865' +
        '636b426f785374617465202852656164496e7465676572202873656374696f6e' +
        '2c2027246c6974272c204f726420286362556e636865636b6564292929203b'
      
        '524354466f6e74202d2024636f6d70203a3d204c6f6164466f6e74202827246c' +
        '69742729203b'
      
        '524354537472696e674c697374202d2024636f6d70203a3d204c6f6164534c69' +
        '7374202873656374696f6e202b20272e246c69742729203b'
      
        '5256426f6f6c65616e202d2069662052656164537472696e6720287365637469' +
        '6f6e2c2027246c6974272c202746616c73652729203d20275472756527207468' +
        '656e2024766172203a3d207472756520656c73652024766172203a3d2066616c' +
        '7365203b'
      
        '5256446f75626c65202d2024766172203a3d2052656164466c6f617420287365' +
        '6374696f6e2c2027246c6974272c203029203b'
      
        '5256496e7465676572202d2024766172203a3d2052656164496e746567657220' +
        '2873656374696f6e2c2027246c6974272c203029203b'
      
        '5256537472696e67202d2024766172203a3d2052656164537472696e67202873' +
        '656374696f6e2c2027246c6974272c20272729203b'
      
        '525654436865636b426f785374617465202d2024766172203a3d205443686563' +
        '6b426f785374617465202852656164496e7465676572202873656374696f6e2c' +
        '2027246c6974272c204f726420286362556e636865636b6564292929203b'
      
        '525654466f6e74202d2024766172203a3d204c6f6164466f6e74202827246c69' +
        '742729203b'
      
        '525654537472696e674c697374202d2024766172203a3d204c6f6164534c6973' +
        '74202873656374696f6e202b20272e246c697427293b'
      
        '5743426f6f6c65616e202d2069662024636f6d70207468656e2074656d70203a' +
        '3d2027547275652720656c73652074656d70203a3d202746616c736527203b20' +
        '5772697465537472696e67202873656374696f6e2c2027246c6974272c207465' +
        '6d7029203b'
      
        '5743446f75626c65202d205772697465466c6f6174202873656374696f6e2c20' +
        '27246c6974272c2024636f6d7029203b'
      
        '5743496e7465676572202d205772697465496e7465676572202873656374696f' +
        '6e2c2027246c6974272c2024636f6d7029203b'
      
        '5743537472696e67202d205772697465537472696e67202873656374696f6e2c' +
        '2027246c6974272c2024636f6d7029203b'
      
        '574354436865636b426f785374617465202d205772697465496e746567657220' +
        '2873656374696f6e2c2027246c6974272c204f7264202824636f6d702929203b'
      
        '574354466f6e74202d2053617665466f6e74202827246c6974272c2024636f6d' +
        '7029203b'
      
        '574354537472696e674c697374202d2053617665534c69737420287365637469' +
        '6f6e202b20272e246c6974272c2024636f6d7029203b'
      
        '5756426f6f6c65616e202d2069662024766172207468656e2074656d70203a3d' +
        '2027547275652720656c73652074656d70203a3d202746616c736527203b2057' +
        '72697465537472696e67202873656374696f6e2c2027246c6974272c2074656d' +
        '7029203b'
      
        '5756446f75626c65202d205772697465466c6f6174202873656374696f6e2c20' +
        '27246c6974272c202476617229203b'
      
        '5756496e7465676572202d205772697465496e7465676572202873656374696f' +
        '6e2c2027246c6974272c202476617229203b'
      
        '5756537472696e67202d205772697465537472696e67202873656374696f6e2c' +
        '2027246c6974272c202476617229203b'
      
        '575654436865636b426f785374617465202d205772697465496e746567657220' +
        '2873656374696f6e2c2027246c6974272c204f72642028247661722929203b'
      
        '575654466f6e74202d2053617665466f6e74202827246c6974272c2024766172' +
        '29203b'
      
        '575654537472696e674c697374202d2053617665534c69737420287365637469' +
        '6f6e202b20272e246c6974272c202476617229203b'
      
        '585243426f6f6c65616e202d2024636f6d70203a3d2052656164426f6f6c2028' +
        '73656374696f6e2c2027246c6974272c2066616c736529203b'
      
        '58524354537472696e674c697374202d2024636f6d702e436c656172203b246c' +
        '696e6520204a203a3d2052656164496e7465676572202873656374696f6e202b' +
        '20272e246c6974272c2027436f756e74272c203029203b246c696e6520206966' +
        '204a203c3e2030207468656e20666f722049203a3d203020746f204a202d2031' +
        '20646f20247461622024636f6d702e416464202852656164537472696e672028' +
        '73656374696f6e202b20272e246c6974272c2474616220274974656d27202b20' +
        '496e74546f537472202849292c202727202929203b'
      
        '585256426f6f6c65616e202d2024766172203a3d2052656164426f6f6c202873' +
        '656374696f6e2c2027246c6974272c2066616c736529203b'
      
        '58525654537472696e674c697374202d20247661722e436c656172203b20246c' +
        '696e6520204a203a3d2052656164496e7465676572202873656374696f6e202b' +
        '20272e246c6974272c2027436f756e74272c203029203b246c696e6520206966' +
        '204a203c3e2030207468656e20666f722049203a3d203020746f204a202d2031' +
        '20646f202474616220247661722e416464202852656164537472696e67202873' +
        '656374696f6e202b20272e246c6974272c2474616220274974656d27202b2049' +
        '6e74546f537472202849292c202727202929203b'
      
        '585743426f6f6c65616e202d205772697465426f6f6c202873656374696f6e2c' +
        '2027246c6974272c2024636f6d7029203b'
      
        '58574354537472696e674c697374202d20457261736553656374696f6e202873' +
        '656374696f6e202b20272e246c69742729203b246c696e65202069662024636f' +
        '6d702e436f756e74203c3e2030207468656e202474616220666f722049203a3d' +
        '203020746f2024636f6d702e436f756e74202d203120646f2024746162205772' +
        '697465537472696e67202873656374696f6e202b20272e246c6974272c202749' +
        '74656d27202b202474616220496e74546f537472202849292c2024636f6d705b' +
        '495d29203b20246c696e6520205772697465496e746567657220287365637469' +
        '6f6e202b20272e246c6974272c2027436f756e74272c2024636f6d702e436f'
      
        '585756426f6f6c65616e202d205772697465426f6f6c202873656374696f6e2c' +
        '2027246c6974272c202476617229203b'
      
        '58575654537472696e674c697374202d20457261736553656374696f6e202873' +
        '656374696f6e202b20272e246c69742729203b246c696e652020696620247661' +
        '722e436f756e74203c3e2030207468656e202474616220666f722049203a3d20' +
        '3020746f20247661722e436f756e74202d203120646f20247461622057726974' +
        '65537472696e67202873656374696f6e202b20272e246c6974272c2027497465' +
        '6d27202b202474616220496e74546f537472202849292c2024766172205b495d' +
        '29203b20246c696e6520205772697465496e7465676572202873656374696f6e' +
        '202b20272e246c6974272c2027436f756e74272c20247661722e436f756e74')
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -8
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 480
    Top = 344
  end
  object DualListDialog: TRxDualListDialog
    Sorted = False
    HelpContext = 0
    Left = 400
    Top = 300
  end
end
