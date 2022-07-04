object frmMain: TfrmMain
  Left = 215
  Top = 55
  BorderStyle = bsSingle
  Caption = 'TELDiagram Demo'
  ClientHeight = 459
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 330
    Width = 151
    Height = 13
    Caption = 'Events trace (only some events)'
  end
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 268
    Height = 13
    Caption = 'Drag one item to another to create new link betwen them'
  end
  object ELDiagram1: TELDiagram
    Left = 10
    Top = 113
    Width = 377
    Height = 209
    OnPaintItem = ELDiagram1PaintItem
    OnCreateLinkDrawInfo = ELDiagram1CreateLinkDrawInfo
    OnIsEqualDrawInfos = ELDiagram1IsEqualDrawInfos
    OnIsOnLink = ELDiagram1IsOnLink
    OnPaintLink = ELDiagram1PaintLink
    DiagramWidth = 1000
    DiagramHeight = 1000
    Links = <
      item
        BeginItemName = 'Item1'
        EndItemName = 'Item2'
      end>
    Items = <
      item
        Left = 8
        Top = 22
        Width = 89
        Height = 82
        Name = 'Item1'
        Caption = 'Item1'
        Color = clBackground
      end
      item
        Left = 216
        Top = 60
        Width = 89
        Height = 89
        Name = 'Item2'
        Caption = 'Item2'
      end>
    OnChange = ELDiagram1Change
    OnChangeSelection = ELDiagram1ChangeSelection
    OnInsertItem = ELDiagram1InsertItem
    OnDeleteItem = ELDiagram1DeleteItem
    OnInsertLink = ELDiagram1InsertLink
    OnDeleteLink = ELDiagram1DeleteLink
    BorderStyle = bsSingle
    DragMode = dmAutomatic
    TabOrder = 0
    OnClick = ELDiagram1Click
    OnDblClick = ELDiagram1DblClick
    OnDragDrop = ELDiagram1DragDrop
    OnDragOver = ELDiagram1DragOver
    OnEnter = ELDiagram1Enter
    OnExit = ELDiagram1Exit
    OnKeyDown = ELDiagram1KeyDown
    OnKeyPress = ELDiagram1KeyPress
    OnKeyUp = ELDiagram1KeyUp
    OnMouseDown = ELDiagram1MouseDown
    OnMouseMove = ELDiagram1MouseMove
    OnMouseUp = ELDiagram1MouseUp
  end
  object Memo1: TMemo
    Left = 8
    Top = 376
    Width = 377
    Height = 73
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object CheckBox8: TCheckBox
    Left = 16
    Top = 352
    Width = 129
    Height = 17
    Caption = 'Trace mouse events'
    TabOrder = 2
  end
  object CheckBox9: TCheckBox
    Left = 160
    Top = 352
    Width = 137
    Height = 17
    Caption = 'Trace keyboard events'
    TabOrder = 3
  end
  object Button5: TButton
    Left = 311
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Panel1: TPanel
    Left = 9
    Top = 64
    Width = 169
    Height = 41
    Caption = 'Drag diagram item on me'
    TabOrder = 5
    OnDragDrop = Panel1DragDrop
    OnDragOver = Panel1DragOver
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 65
    Height = 25
    Caption = 'New item'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 80
    Top = 32
    Width = 97
    Height = 25
    Caption = 'Delete item or link'
    TabOrder = 7
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 184
    Top = 32
    Width = 113
    Height = 17
    Caption = 'Custom draw items'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 184
    Top = 56
    Width = 113
    Height = 17
    Caption = 'Custom draw links'
    TabOrder = 9
    OnClick = CheckBox2Click
  end
end
