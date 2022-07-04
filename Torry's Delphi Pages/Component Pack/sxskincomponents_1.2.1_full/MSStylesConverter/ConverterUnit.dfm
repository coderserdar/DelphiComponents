object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'MSStyles-to-SXSkin Converter'
  ClientHeight = 445
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    589
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 44
    Height = 13
    Caption = 'File Path:'
  end
  object Edit1: TEdit
    Left = 56
    Top = 8
    Width = 381
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 
      'I:\Program Files\Borland\Delphi6\SXComponents\SXSkin\MSStylesCon' +
      'verter\Test\*.msstyles'
  end
  object Button1: TButton
    Left = 444
    Top = 8
    Width = 65
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Choose'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 573
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button2: TButton
    Left = 516
    Top = 8
    Width = 65
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Convert'
    TabOrder = 3
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 325
    Width = 573
    Height = 113
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Parameters'
    TabOrder = 4
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 217
      Height = 17
      Caption = 'Process only skins with "NORMAL"-prefix'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 137
      Height = 17
      Caption = 'Covert skin.ini to skin.sxs'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Pack skin to ZIP-file'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 160
      Top = 32
      Width = 89
      Height = 17
      Caption = 'Delete skin.ini'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox6: TCheckBox
      Left = 328
      Top = 16
      Width = 257
      Height = 17
      Caption = 'Set "MouseCapture=ByTransparency" for buttons'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBox7: TCheckBox
      Left = 328
      Top = 32
      Width = 121
      Height = 17
      Caption = 'Use Blending Effects'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBox5: TCheckBox
      Left = 160
      Top = 48
      Width = 81
      Height = 17
      Caption = 'Delete folder'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MSStyles|*.msstyles|All Files|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 240
    Top = 88
  end
end
