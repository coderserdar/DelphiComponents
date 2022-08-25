object ClientCertDlg: TClientCertDlg
  Left = 437
  Top = 331
  BorderStyle = bsDialog
  Caption = 'Client Certificate Requested'
  ClientHeight = 192
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 6
    Width = 334
    Height = 13
    Caption = 
      'Server requested a client certificate, which one do you want to ' +
      'send?'
  end
  object CertListBox: TListBox
    Left = 6
    Top = 26
    Width = 335
    Height = 133
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = CertListBoxDblClick
  end
  object OKButton: TButton
    Left = 266
    Top = 166
    Width = 75
    Height = 21
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 6
    Top = 166
    Width = 129
    Height = 21
    Caption = 'Don'#39't send a certificate'
    ModalResult = 2
    TabOrder = 2
  end
end
