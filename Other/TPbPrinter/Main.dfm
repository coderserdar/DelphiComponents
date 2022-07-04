object Form1: TForm1
  Left = 364
  Top = 117
  ActiveControl = SetupTypeGroup
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PBPrinterSetupDialog demo'
  ClientHeight = 188
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 175
    Width = 365
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = '(Build using PBPrinterSetupDialog version )'
  end
  object ExecuteDialog: TButton
    Left = 236
    Top = 98
    Width = 86
    Height = 25
    Caption = 'Execute Dialog'
    TabOrder = 0
    OnClick = ExecuteDialogClick
  end
  object Save: TButton
    Left = 43
    Top = 98
    Width = 86
    Height = 25
    Caption = 'Save setup'
    TabOrder = 1
    OnClick = SaveClick
  end
  object SetupTypeGroup: TRadioGroup
    Left = 11
    Top = 11
    Width = 342
    Height = 48
    Caption = 'SetupType:'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'stDefault'
      'stInitial'
      'stSaved'
      'stUser')
    TabOrder = 2
    OnClick = SetupTypeGroupClick
  end
  object AutoSave: TCheckBox
    Left = 34
    Top = 70
    Width = 71
    Height = 17
    Caption = 'AutoSave'
    TabOrder = 3
    OnClick = AutoSaveClick
  end
  object ForceInitialSetupValues: TCheckBox
    Left = 186
    Top = 70
    Width = 133
    Height = 17
    Caption = 'ForceInitialSetupValues'
    TabOrder = 4
    OnClick = ForceInitialSetupValuesClick
  end
  object Button1: TButton
    Left = 43
    Top = 137
    Width = 279
    Height = 25
    Caption = 'Print example (see code)'
    TabOrder = 5
    OnClick = Button1Click
  end
  object PBPrinterSetupDialog1: TPBPrinterSetupDialog
    InitialSetupOptions = [isOrientation, isPaperSize]
    InitialSetupValues.Orientation = 2
    InitialSetupValues.PaperSize = 7
    InitialSetupValues.PrinterName = 'HP DeskJet 695C'
    SetupFileName = 'PrinterSetup.cfg'
    SetupType = stInitial
    Left = 168
    Top = 96
  end
end
