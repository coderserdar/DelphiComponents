object DemoForm: TDemoForm
  Left = 273
  Top = 130
  Width = 682
  Height = 384
  Caption = 'Standard Demo (TPJRegWdwState)'
  Color = clBtnFace
  Constraints.MaxWidth = 2000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    666
    346)
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 148
    Top = 7
    Width = 285
    Height = 35
    AutoSize = False
    Caption = 'Label1'
    WordWrap = True
  end
  object btnShowDlg: TButton
    Left = 10
    Top = 10
    Width = 129
    Height = 31
    Caption = 'Show Dialog'
    TabOrder = 0
    OnClick = btnShowDlgClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 56
    Width = 666
    Height = 290
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 313
      Top = 0
      Height = 290
    end
    object Memo2: TMemo
      Left = 316
      Top = 0
      Width = 350
      Height = 290
      Align = alClient
      Lines.Strings = (
        
          'Drag the splitter to the left of this control to change the size' +
          ' of the memo controls then close and re-open the application.'
        ''
        
          'You should find that the splitter remembers its position. This i' +
          's done by handling the OnGettingRegData and OnPuttingRegData eve' +
          'nts')
      ParentColor = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 313
      Height = 290
      Align = alLeft
      Lines.Strings = (
        
          'Try moving and resizing this form and then closing it. When rest' +
          'arted it should appear with the same shape and size.'
        ''
        
          'Next try minimizing and maximising the form then closing it. Whe' +
          'n restarted the state should be remembered.'
        ''
        
          'Now click the button to display a dialogue box. Move the dialogu' +
          'e and then close it. Click the button again and the dialogue box' +
          ' should appear in the same place.'
        ''
        
          'The window settings for the main form and dialogue box are store' +
          'd in the registry under the keys: HKCU\Software\DelphiDabbler\De' +
          'mos\WindowState\Main and HKCU\Software\DelphiDabbler\Demos\Windo' +
          'wState\Dlg respectively. The main form sets this in the OnGetReg' +
          'DataEx event handler while the dialogue sets the RootKeyEx and S' +
          'ubKey properties at run time.'
        ''
        
          'Note that window position and size read from the registry are di' +
          'splayed in both the main form and the dialogue box.')
      ParentColor = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
  end
  object PJRegWdwState1: TPJRegWdwState
    AutoSaveRestore = True
    OnReadWdwState = PJRegWdwState1ReadWdwState
    OnGetRegDataEx = PJRegWdwState1GetRegDataEx
    OnGettingRegData = PJRegWdwState1GettingRegData
    OnPuttingRegData = PJRegWdwState1PuttingRegData
    Left = 24
    Top = 68
  end
end
