object Form1: TForm1
  Left = 354
  Top = 175
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 130
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 64
    Width = 99
    Height = 25
    Caption = 'Open media file'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 136
    Top = 64
    Width = 97
    Height = 25
    Caption = 'Save media file'
    TabOrder = 1
    OnClick = Button2Click
  end
  object BMOpenMediaDialog1: TBMOpenMediaDialog
    Filter = 
      'All media (*.wav;*.mid;*.avi)|*.wav;*.mid;*.avi|Wave File (*.wav' +
      ')|*.wav|MIDI File (*.mid)|*.mid|Video (*.avi)|*.avi|All files (*' +
      '.*)|*.*'
    Options = [ofHideReadOnly, ofShareAware, ofEnableSizing]
    Left = 48
    Top = 16
  end
  object BMSaveMediaDialog1: TBMSaveMediaDialog
    Filter = 
      'All media (*.wav;*.mid;*.avi)|*.wav;*.mid;*.avi|Wave File (*.wav' +
      ')|*.wav|MIDI File (*.mid)|*.mid|Video (*.avi)|*.avi|All files (*' +
      '.*)|*.*'
    Options = [ofHideReadOnly, ofShareAware, ofEnableSizing]
    Left = 168
    Top = 16
  end
end
