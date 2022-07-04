object Form1: TForm1
  Left = 192
  Top = 107
  Width = 607
  Height = 466
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    599
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 376
    Width = 25
    Height = 13
    Caption = 'Path:'
  end
  object Label3: TLabel
    Left = 8
    Top = 400
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 599
    Height = 369
    Align = alTop
    Lines.Strings = (
      '<XML>'
      '   <tree1>'
      '       <node1>'
      '           <value1>'
      '               this should be possible to read here'
      '           </value1>'
      '           <value2>'
      '                another value..'
      '           </value2>'
      '         </node1>'
      '    </tree1>'
      '    <humm>'
      '       <value2>'
      '           p'#246#246
      '       </value2>'
      '    </humm>'
      '</XML>')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 48
    Top = 374
    Width = 273
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Text = 'XML\tree1\node1\value2'
  end
  object Edit2: TEdit
    Left = 48
    Top = 398
    Width = 273
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object Button1: TButton
    Left = 330
    Top = 374
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Get Nodes'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 330
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Get Value'
    TabOrder = 4
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 416
    Top = 376
    Width = 169
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Include Sub Nodes to Search'
    TabOrder = 5
  end
  object Button3: TButton
    Left = 410
    Top = 398
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Set Value'
    TabOrder = 6
    OnClick = Button3Click
  end
  object API_XMLDoc1: TAPI_XMLDoc
    Left = 264
    Top = 40
  end
end
