object DemoDlg: TDemoDlg
  Left = 198
  Top = 387
  Width = 314
  Height = 264
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Dialog Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    298
    226)
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 9
    Top = 7
    Width = 260
    Height = 40
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Label1'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 9
    Top = 55
    Width = 260
    Height = 140
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Try moving and changing the size of, or maximising, this dialogu' +
      'e box. Then close the dialogue. Re-open by clicking the button o' +
      'n the main form. You will find that the position is remembered b' +
      'ut the size and state are not.'#13#10#13#10'This is because the TPJRegWdwS' +
      'tate woIgnoreSize and woIgnoreState options are specified.'
    WordWrap = True
  end
  object PJRegWdwState1: TPJRegWdwState
    IgnoreState = True
    Options = [woIgnoreState, woIgnoreSize]
    OnReadWdwState = PJRegWdwState1ReadWdwState
    SubKey = 'Software\DelphiDabbler\Demos\WindowState\Dlg'
    Left = 8
    Top = 8
  end
end
