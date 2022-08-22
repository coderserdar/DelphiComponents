object Form1: TForm1
  Left = 223
  Top = 106
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Test string library "jbStr" (Demonstration 24.XI.2009 corrected)'
  ClientHeight = 280
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000007777770000000000000000
    0000000000000007000000000000000000000000000000070000000000000000
    0000000000777007000000000000000000077070007770070000700000000000
    0077000700787000000007000000000007708000077877000070007000000000
    07088807777777770777000700000000008F88877FFFFF077887700700000000
    00088888F88888FF08870070000000000000880888877778F070007000000007
    77088888880007778F770077777000700008F088007777077F07000000700700
    008F08880800077778F7700000700708888F0880F08F807078F7777700700708
    F88F0780F070F07078F7887700700708888F0780F077807088F7777700700700
    008F0788FF00080888F77000007000000008F0780FFFF0088F77007000000000
    0008F07788000888887700700000000000008F07788888880870007000000000
    00088FF0077788088887000700000000008F888FF00000F87887700700000000
    0708F8088FFFFF88078700700000000007708000088888000070070000000000
    0077007000888007000070000000000000077700008F80070007000000000000
    0000000000888007000000000000000000000000000000070000000000000000
    000000000777777700000000000000000000000000000000000000000000FFFF
    FFFFFFFC0FFFFFFC0FFFFFF80FFFFFF80FFFFE180E7FFC00043FF800001FF800
    000FF800000FFC00001FFE00001FE0000001C000000180000001800000018000
    00018000000180000001FC00001FFC00001FFE00001FFC00000FF800000FF800
    001FF800003FFC180C7FFE380EFFFFF80FFFFFF80FFFFFF80FFFFFFFFFFF}
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 206
    Width = 417
    Height = 25
  end
  object Label2: TLabel
    Left = 8
    Top = 238
    Width = 172
    Height = 13
    Hint = 
      'Here is stored string after any operation under any button. '#13#10'It' +
      ' is generated string from function.'
    Caption = 'Test string after possible operation:'
    ParentShowHint = False
    ShowHint = True
  end
  object Label1: TLabel
    Left = 8
    Top = 190
    Width = 175
    Height = 13
    Hint = 
      'Here is stored string before any operation. '#13#10'It is constant or ' +
      'generated string for demonstration'#13#10'of function.'
    Caption = 'Original testing string is stored here:'
    ParentShowHint = False
    ShowHint = True
  end
  object OrigStr: TLabel
    Left = 13
    Top = 211
    Width = 3
    Height = 13
  end
  object Bevel2: TBevel
    Left = 8
    Top = 254
    Width = 417
    Height = 25
  end
  object NewStr: TLabel
    Left = 13
    Top = 258
    Width = 411
    Height = 16
    AutoSize = False
  end
  object Notes: TLabel
    Left = 8
    Top = 294
    Width = 3
    Height = 13
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 449
    Height = 184
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object _ShortFileName: TButton
      Left = 179
      Top = 41
      Width = 89
      Height = 20
      Hint = 'Function ShortFileName'
      Caption = 'Short File Name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = _ShortFileNameClick
    end
    object _TrimRight: TButton
      Left = 268
      Top = 41
      Width = 89
      Height = 20
      Hint = 'Function TrimTrail'
      Caption = 'Trim Right'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = _TrimRightClick
    end
    object _TrimBoth: TButton
      Left = 268
      Top = 21
      Width = 89
      Height = 20
      Hint = 'Function Trim'
      Caption = 'Trim Both'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = _TrimBothClick
    end
    object _TrimLeft: TButton
      Left = 268
      Top = 1
      Width = 89
      Height = 20
      Hint = 'Function TrimLead'
      Caption = 'Trim Left'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = _TrimLeftClick
    end
    object _PadRight: TButton
      Left = 357
      Top = 41
      Width = 89
      Height = 20
      Hint = 'Function PadCh'
      Caption = 'Pad Right'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 14
      OnClick = _PadRightClick
    end
    object _Center: TButton
      Left = 357
      Top = 21
      Width = 89
      Height = 20
      Hint = 'Function CenterCh'
      Caption = 'Center'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = _CenterClick
    end
    object _PadLeft: TButton
      Left = 357
      Top = 1
      Width = 89
      Height = 20
      Hint = 'Function LeftPadCh'
      Caption = 'Pad Left'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = _PadLeftClick
    end
    object _ExtensionOnly: TButton
      Left = 90
      Top = 41
      Width = 89
      Height = 20
      Hint = 'Function JustExtension'
      Caption = 'ExtensionOnly'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 11
      OnClick = _ExtensionOnlyClick
    end
    object _Capitalize: TButton
      Left = 1
      Top = 41
      Width = 89
      Height = 20
      Hint = 'Function CapitalizeWord'
      Caption = 'Capitalize'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = _CapitalizeClick
    end
    object _StrLoCase: TButton
      Left = 1
      Top = 21
      Width = 89
      Height = 20
      Hint = 'Function StrLoCase'
      Caption = 'String Low Case'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = _StrLoCaseClick
    end
    object _ShortPath: TButton
      Left = 179
      Top = 21
      Width = 89
      Height = 20
      Hint = 'Function ShortDirName'
      Caption = 'Short Path'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = _ShortPathClick
    end
    object _FileNameOnly: TButton
      Left = 179
      Top = 1
      Width = 89
      Height = 20
      Hint = 'Function JustFileName'
      Caption = 'File Name Only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = _FileNameOnlyClick
    end
    object _PathOnly: TButton
      Left = 90
      Top = 1
      Width = 89
      Height = 20
      Hint = 'Function JustPathName'
      Caption = 'Path Only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = _PathOnlyClick
    end
    object _StrUpCase: TButton
      Left = 1
      Top = 1
      Width = 89
      Height = 20
      Hint = 'Function StrUpCase'
      Caption = 'String Up Case'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = _StrUpCaseClick
    end
    object _NameOnly: TButton
      Left = 90
      Top = 21
      Width = 89
      Height = 20
      Hint = 'Function JustName'
      Caption = 'Name Only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = _NameOnlyClick
    end
    object _Zip: TButton
      Left = 1
      Top = 61
      Width = 89
      Height = 20
      Hint = 'Function Zip'
      Caption = 'Zip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 15
      OnClick = _ZipClick
    end
    object _Smash: TButton
      Left = 90
      Top = 61
      Width = 89
      Height = 20
      Hint = 'Function Smash'
      Caption = 'Smash'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = _SmashClick
    end
    object _Change: TButton
      Left = 179
      Top = 61
      Width = 89
      Height = 20
      Hint = 'Function Change'
      Caption = 'Change'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = _ChangeClick
    end
    object _Form: TButton
      Left = 268
      Top = 61
      Width = 89
      Height = 20
      Hint = 'Function Form'
      Caption = 'Form'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = _FormClick
    end
    object _Strip: TButton
      Left = 357
      Top = 61
      Width = 89
      Height = 20
      Hint = 'Function Strip'
      Caption = 'Strip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = _StripClick
    end
    object _InsertWord: TButton
      Left = 1
      Top = 81
      Width = 89
      Height = 20
      Hint = 'Function InsWord'
      Caption = 'Insert Word'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = _InsertWordClick
    end
    object _Push: TButton
      Left = 90
      Top = 81
      Width = 89
      Height = 20
      Hint = 'Function Push'
      Caption = 'Push'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 21
      OnClick = _PushClick
    end
    object _Hash: TButton
      Left = 179
      Top = 81
      Width = 89
      Height = 20
      Hint = 'Function Hash'
      Caption = 'Hash'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 22
      OnClick = _HashClick
    end
    object _MaskAndZeroClip: TButton
      Left = 268
      Top = 81
      Width = 89
      Height = 20
      Hint = 'Function Mask'#13#10'Function ZeroClip'
      Caption = 'Mask+Zero Clip'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 23
      OnClick = _MaskAndZeroClipClick
    end
    object _Count: TButton
      Left = 357
      Top = 81
      Width = 89
      Height = 20
      Hint = 'Function Count'
      Caption = 'Count'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 24
      OnClick = _CountClick
    end
    object _PopWord: TButton
      Left = 1
      Top = 101
      Width = 89
      Height = 20
      Hint = 'Function PopWord'
      Caption = 'Pop Word'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 25
      OnClick = _PopWordClick
    end
    object _ExtractWord: TButton
      Left = 90
      Top = 101
      Width = 89
      Height = 20
      Hint = 'Function ExtractWord'
      Caption = 'Extract Word'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 26
      OnClick = _ExtractWordClick
    end
    object _GetPosition: TButton
      Left = 179
      Top = 101
      Width = 89
      Height = 20
      Hint = 'Function GetPos'
      Caption = 'Get Position'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 27
      OnClick = _GetPositionClick
    end
    object _GetEnd: TButton
      Left = 268
      Top = 101
      Width = 89
      Height = 20
      Hint = 'Function GetEnd'
      Caption = 'Get End'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 28
      OnClick = _GetEndClick
    end
    object _WordCount: TButton
      Left = 357
      Top = 101
      Width = 89
      Height = 20
      Hint = 'Function WordCount'
      Caption = 'Word Count'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 29
      OnClick = _WordCountClick
    end
    object _ChangeWord: TButton
      Left = 1
      Top = 121
      Width = 89
      Height = 20
      Hint = 'Function ChangeWord'
      Caption = 'ChangeWord'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 30
      OnClick = _ChangeWordClick
    end
    object _GetLastWord: TButton
      Left = 90
      Top = 121
      Width = 89
      Height = 20
      Hint = 'Function GetLastWord'
      Caption = 'GetLastWord'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 31
      OnClick = _GetLastWordClick
    end
    object _GetFirstWord: TButton
      Left = 179
      Top = 121
      Width = 89
      Height = 20
      Hint = 'Function GetFirstWord'
      Caption = 'GetFirstWord'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 32
      OnClick = _GetFirstWordClick
    end
    object _Romanum: TButton
      Left = 268
      Top = 121
      Width = 89
      Height = 20
      Hint = 'Function Int2Roman'#13#10'Function Roman2Int'
      Caption = 'Romanum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 33
      OnClick = _RomanumClick
    end
    object _ExtractEmail: TButton
      Left = 357
      Top = 121
      Width = 89
      Height = 20
      Hint = 'Function htmlSrcEmail'
      Caption = 'ExtractEmail'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 34
      OnClick = _ExtractEmailClick
    end
    object _Alter: TButton
      Left = 1
      Top = 141
      Width = 89
      Height = 20
      Hint = 'Function Alter'
      Caption = 'Alter'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 35
      OnClick = _AlterClick
    end
    object _Reduce: TButton
      Left = 90
      Top = 141
      Width = 89
      Height = 20
      Hint = 'Function Reduce'
      Caption = 'Reduce'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 36
      OnClick = _ReduceClick
    end
    object _ExtractNumber: TButton
      Left = 179
      Top = 141
      Width = 89
      Height = 20
      Hint = 'Function EctractNumber'
      Caption = 'ExtractNumber'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 37
      OnClick = _ExtractNumberClick
    end
    object _FindWord: TButton
      Left = 268
      Top = 141
      Width = 89
      Height = 20
      Hint = 'Function FindWord'
      Caption = 'FindWord'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 38
      OnClick = _FindWordClick
    end
    object _JoinTo: TButton
      Left = 357
      Top = 141
      Width = 89
      Height = 20
      Hint = 'Function JoinTo'
      Caption = 'JoinTo'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 39
      OnClick = _JoinToClick
    end
    object Button1: TButton
      Left = 90
      Top = 161
      Width = 89
      Height = 20
      Caption = 'Rand word seq'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 40
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 357
      Top = 161
      Width = 89
      Height = 20
      ParentShowHint = False
      ShowHint = True
      TabOrder = 41
    end
    object Button4: TButton
      Left = 268
      Top = 161
      Width = 89
      Height = 20
      ParentShowHint = False
      ShowHint = True
      TabOrder = 42
    end
    object Button5: TButton
      Left = 179
      Top = 161
      Width = 89
      Height = 20
      ParentShowHint = False
      ShowHint = True
      TabOrder = 43
    end
    object _PackNum: TButton
      Left = 1
      Top = 161
      Width = 89
      Height = 20
      Hint = 'Function PackNum/UnpackNum'
      Caption = 'PackNum'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 44
      OnClick = _PackNumClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 255
    Width = 449
    Height = 25
    Hint = 'Send me your comments'
    Align = alBottom
    BevelOuter = bvLowered
    Caption = '(c) 2000-2009 Jaro Benes, E-mail: JBenes@micrel.cz'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 408
    Top = 184
    object Files1: TMenuItem
      Caption = 'Files'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
end
