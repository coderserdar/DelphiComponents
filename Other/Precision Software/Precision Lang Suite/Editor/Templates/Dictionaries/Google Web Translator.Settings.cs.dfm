object PSForm: TPSForm
  Left = 276
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Google Web Translator'
  ClientHeight = 160
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    420
    160)
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 16
    Top = 16
    Width = 389
    Height = 89
    AutoSize = False
    Caption = 
      'Pros'#237'me, nepou'#382#237'vejte slu'#382'bu "Google Web Translator", pokud to n' +
      'en'#237' nezbytn'#283' nutn'#233'! Toto rozhran'#237' je implementov'#225'no pouze pro vy' +
      'j'#237'me'#269'n'#233' situace, kdy nen'#237' k dispozici jin'#225' p'#345'ekladov'#225' slu'#382'ba, '#269'i' +
      ' p'#345'ekladatel, a p'#345'elo'#382'en'#237' dan'#233'ho textu je pro V'#225's v danou chv'#237'li' +
      ' kritick'#233'.'#13#10#13#10'Pokud slu'#382'bu pou'#382'ijete, p'#345'ekl'#225'dejte jen n'#283'kolik m'#225 +
      'lo polo'#382'ek najednou.'
    WordWrap = True
  end
  object Bevel2: TBevel
    AlignWithMargins = True
    Left = 0
    Top = 118
    Width = 420
    Height = 2
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 40
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 296
    ExplicitWidth = 600
  end
  object sbCancel: TButton
    Left = 334
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Zav'#345#237't'
    Default = True
    TabOrder = 0
  end
end
