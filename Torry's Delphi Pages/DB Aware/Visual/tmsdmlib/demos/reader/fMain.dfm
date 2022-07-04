object Form1: TForm1
  Left = 231
  Top = 112
  Width = 734
  Height = 545
  BorderWidth = 3
  Caption = 'Database Model Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 33
    Width = 712
    Height = 468
    ActivePage = tsTable
    Align = alClient
    TabOrder = 1
    object tsTable: TTabSheet
      BorderWidth = 3
      Caption = 'Tables'
      object Splitter1: TSplitter
        Left = 200
        Top = 0
        Width = 3
        Height = 434
        Cursor = crHSplit
      end
      object ListBoxTables: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 434
        Align = alLeft
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = ListBoxTablesClick
      end
      object PageControl2: TPageControl
        Left = 203
        Top = 0
        Width = 495
        Height = 434
        ActivePage = tsFields
        Align = alClient
        TabOrder = 1
        object tsFields: TTabSheet
          BorderWidth = 3
          Caption = 'Fields'
          object Splitter3: TSplitter
            Left = 200
            Top = 0
            Width = 3
            Height = 400
            Cursor = crHSplit
          end
          object ListBoxFields: TListBox
            Left = 0
            Top = 0
            Width = 200
            Height = 400
            Align = alLeft
            ItemHeight = 13
            TabOrder = 0
            OnClick = ListBoxFieldsClick
          end
          object Panel1: TPanel
            Left = 203
            Top = 0
            Width = 278
            Height = 400
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object Splitter2: TSplitter
              Left = 0
              Top = 276
              Width = 278
              Height = 3
              Cursor = crVSplit
              Align = alBottom
            end
            object lbFieldProperties: TListBox
              Left = 0
              Top = 0
              Width = 278
              Height = 276
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
            object mmFieldDescription: TMemo
              Left = 0
              Top = 279
              Width = 278
              Height = 121
              Align = alBottom
              BorderStyle = bsNone
              Lines.Strings = (
                '')
              ParentColor = True
              ReadOnly = True
              TabOrder = 1
            end
          end
        end
        object tsIndexes: TTabSheet
          Caption = 'Indexes'
          ImageIndex = 2
          object ListBoxIndexes: TListBox
            Left = 0
            Top = 0
            Width = 487
            Height = 399
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
            OnClick = ListBoxFieldsClick
          end
        end
        object tsTableDescription: TTabSheet
          Caption = 'Table Description'
          ImageIndex = 1
          object mmTableDescription: TMemo
            Left = 0
            Top = 0
            Width = 487
            Height = 399
            Align = alClient
            ReadOnly = True
            TabOrder = 0
          end
        end
      end
    end
    object tsRelationship: TTabSheet
      Caption = 'Relationships'
      ImageIndex = 3
      object Splitter6: TSplitter
        Left = 289
        Top = 0
        Width = 3
        Height = 433
        Cursor = crHSplit
      end
      object lbRelationships: TListBox
        Left = 0
        Top = 0
        Width = 289
        Height = 433
        Align = alLeft
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbRelationshipsClick
      end
      object mmRelationship: TMemo
        Left = 292
        Top = 0
        Width = 412
        Height = 440
        Align = alClient
        TabOrder = 1
      end
    end
    object tsProcedures: TTabSheet
      Caption = 'Procedures'
      ImageIndex = 1
      object Splitter4: TSplitter
        Left = 200
        Top = 0
        Width = 3
        Height = 433
        Cursor = crHSplit
      end
      object lbProcedures: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 433
        Align = alLeft
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbProceduresClick
      end
      object mmProcedure: TMemo
        Left = 203
        Top = 0
        Width = 501
        Height = 440
        Align = alClient
        TabOrder = 1
      end
    end
    object tsViews: TTabSheet
      Caption = 'Views'
      ImageIndex = 2
      object Splitter5: TSplitter
        Left = 200
        Top = 0
        Width = 3
        Height = 433
        Cursor = crHSplit
      end
      object lbViews: TListBox
        Left = 0
        Top = 0
        Width = 200
        Height = 433
        Align = alLeft
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbViewsClick
      end
      object mmView: TMemo
        Left = 203
        Top = 0
        Width = 501
        Height = 440
        Align = alClient
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 712
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButton
      Left = 2
      Top = 2
      Width = 175
      Height = 25
      Caption = '&Open Data Modeler project file'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btAbout: TButton
      Left = 184
      Top = 2
      Width = 105
      Height = 25
      Caption = '&About this demo'
      TabOrder = 1
      OnClick = btAboutClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'TMS Data Modeler files (*.dgp)|*.dgp'
    Left = 464
    Top = 16
  end
end
