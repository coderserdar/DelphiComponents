object Form2: TForm2
  Left = 432
  Top = 185
  Width = 292
  Height = 235
  Caption = 'MDI Child Form'
  Color = clActiveBorder
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'CHILD FORM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PJWdwState1: TPJWdwState
    AutoSaveRestore = True
    Options = [woFitWorkArea]
    IniRootDir = rdExeDir
    Section = 'ChildForm'
    Left = 164
    Top = 96
  end
end
