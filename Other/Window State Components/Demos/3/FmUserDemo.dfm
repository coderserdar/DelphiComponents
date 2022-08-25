object Form1: TForm1
  Left = 351
  Top = 251
  Width = 537
  Height = 288
  Caption = 'Use Demo (TPJUserWdwState)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 521
    Height = 250
    Align = alClient
    Lines.Strings = (
      'Try moving and resizing this form and then closing it.'
      'When restarted it should appear with the same shape and size.'
      ''
      'Next try minimizing and maximising the form then closing.'
      'When restarted the state should be remembered.'
      ''
      'Finally try moving part of the form off-screen before closing.'
      'The form should restart wholly within the workspace.'
      ''
      
        'The window settings are stored in the [Window] section of Custom' +
        '.ini '
      'in the same directory as the demo application.')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object PJUserWdwState1: TPJUserWdwState
    AutoSaveRestore = True
    Options = [woFitWorkArea]
    OnReadData = PJUserWdwState1ReadData
    OnSaveData = PJUserWdwState1SaveData
    Left = 208
    Top = 8
  end
end
