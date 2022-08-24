object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Nastaven'#237' VirtualTree'
  ClientHeight = 320
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 118
    Width = 207
    Height = 13
    Caption = 'Seznam t'#345#237'd zd'#283'd'#283'n'#253'ch z TBaseVirtualTree:'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 382
    Height = 97
    AutoSize = False
    Caption = 
      'Komponenty typu TVirtualStringTree pou'#382#237'vaj'#237' specifick'#253' p'#345#237'stup ' +
      'k ukl'#225'd'#225'n'#237' a na'#269#237't'#225'n'#237' n'#283'kter'#253'ch vlastnost'#237' do soubor'#367' DFM. Pokud' +
      ' chcete v PLS Editoru pracovat i s komponentami zd'#283'd'#283'n'#253'mi z t'#233'to' +
      ' t'#345#237'dy, zadejte jejich n'#225'zvy do n'#237#382'e uveden'#233'ho seznamu.'#13#10#13#10'Aby s' +
      'e zm'#283'ny v nastaven'#237' mohly projevit, mus'#237'te restartovat PLS Edito' +
      'r.'
    WordWrap = True
  end
  object mList: TMemo
    Left = 8
    Top = 136
    Width = 381
    Height = 141
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object sbOK: TButton
    Left = 236
    Top = 288
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
  end
  object sbCancel: TButton
    Left = 316
    Top = 288
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Storno'
    TabOrder = 2
  end
end
