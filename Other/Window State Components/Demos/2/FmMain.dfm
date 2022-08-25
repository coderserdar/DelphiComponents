object Form1: TForm1
  Left = 351
  Top = 251
  Width = 537
  Height = 288
  Caption = 'Stand-alone Demo (TPJWdwState)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    521
    250)
  PixelsPerInch = 96
  TextHeight = 15
  object CheckBox1: TCheckBox
    Left = 10
    Top = 10
    Width = 208
    Height = 21
    Caption = 'Keep in workspace'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 40
    Width = 521
    Height = 210
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'Try moving and resizing this form and then closing it. When rest' +
        'arted it should appear with '
      'the same shape and size.'
      ''
      
        'Next try minimizing and maximising the form then closing. When r' +
        'estarted the state should '
      'be remembered.'
      ''
      
        'Finally try moving part of the form off-screen before closing. I' +
        'f the "Keep in workspace" '
      
        'check box is ticked the form should restart wholly within the wo' +
        'rkspace but when the check '
      
        'box is clear the form will redisplay exactly in the position it ' +
        'was closed.'
      ''
      
        'The window settings are stored in the [Window] section of Custom' +
        '.ini in the same '
      'directory as the demo application.')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
