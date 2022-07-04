object FormNewThirdClass: TFormNewThirdClass
  Left = 405
  Top = 430
  BorderStyle = bsDialog
  Caption = 'New third-party control'
  ClientHeight = 141
  ClientWidth = 334
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sEdit1: TsEdit
    Left = 112
    Top = 24
    Width = 190
    Height = 21
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Active = True
    BoundLabel.Caption = 'Class name :'
    BoundLabel.Indent = 0
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object sComboBox1: TsComboBox
    Left = 112
    Top = 56
    Width = 190
    Height = 21
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'Type of skin :'
    BoundLabel.Indent = 0
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
    SkinData.SkinSection = 'COMBOBOX'
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = -1
    TabOrder = 1
  end
  object sBitBtn1: TsBitBtn
    Left = 88
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
    SkinData.SkinSection = 'BUTTON'
  end
  object sBitBtn2: TsBitBtn
    Left = 168
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
    SkinData.SkinSection = 'BUTTON'
  end
  object sSkinProvider1: TsSkinProvider
    SkinData.SkinSection = 'DIALOG'
    TitleButtons = <>
    Left = 288
    Top = 8
  end
end
