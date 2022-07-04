object frmMain: TfrmMain
  Left = 267
  Top = 229
  BorderStyle = bsSingle
  Caption = 'TELDBDiagram Demo'
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
    Width = 290
    Height = 13
    Caption = 'Drag one item field to another to create new link betwen them'
  end
  object Label2: TLabel
    Left = 16
    Top = 24
    Width = 225
    Height = 13
    Caption = 'Double click item or link to change its properties'
  end
  object ELDBDiagram1: TELDBDiagram
    Left = 15
    Top = 114
    Width = 371
    Height = 209
    DiagramWidth = 1000
    DiagramHeight = 1000
    Links = <
      item
        BeginItemName = 'Item1'
        EndItemName = 'Item2'
        BeginLineIndex = 0
        BeginPointType = lptOne
        EndLineIndex = 1
        EndPointType = lptInfinity
      end>
    Items = <
      item
        Left = 13
        Top = 13
        Width = 98
        Height = 102
        Name = 'Item1'
        Caption = 'Item1'
        Lines.Strings = (
          'Obj#'
          'Data1'
          'Data2'
          'Creator#'
          'CreationDate#')
      end
      item
        Left = 197
        Top = 53
        Width = 115
        Height = 101
        Name = 'Item2'
        Caption = 'Item2'
        Lines.Strings = (
          'Obj#'
          'ParentObj#'
          'Data1'
          'Data2'
          'Data3'
          'Creator#'
          'CreationDate#')
      end>
    OnChange = ELDBDiagram1Change
    OnChangeSelection = ELDBDiagram1ChangeSelection
    OnInsertItem = ELDBDiagram1InsertItem
    OnDeleteItem = ELDBDiagram1DeleteItem
    OnInsertLink = ELDBDiagram1InsertLink
    OnDeleteLink = ELDBDiagram1DeleteLink
    BorderStyle = bsSingle
    DragMode = dmAutomatic
    TabOrder = 0
    OnClick = ELDBDiagram1Click
    OnDblClick = ELDBDiagram1DblClick
    OnDragDrop = ELDBDiagram1DragDrop
    OnDragOver = ELDBDiagram1DragOver
    OnEnter = ELDBDiagram1Enter
    OnExit = ELDBDiagram1Exit
    OnKeyDown = ELDBDiagram1KeyDown
    OnKeyPress = ELDBDiagram1KeyPress
    OnKeyUp = ELDBDiagram1KeyUp
    OnMouseDown = ELDBDiagram1MouseDown
    OnMouseMove = ELDBDiagram1MouseMove
    OnMouseUp = ELDBDiagram1MouseUp
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
    Left = 209
    Top = 56
    Width = 169
    Height = 41
    Caption = 'Drag diagram item on me'
    TabOrder = 5
    OnDragDrop = Panel1DragDrop
    OnDragOver = Panel1DragOver
  end
  object Button1: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'New item'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 56
    Width = 105
    Height = 25
    Caption = 'Delete item or link'
    TabOrder = 7
    OnClick = Button2Click
  end
end
