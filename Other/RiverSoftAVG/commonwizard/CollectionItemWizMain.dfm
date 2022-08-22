object frmNewCollection: TfrmNewCollection
  Left = 409
  Top = 179
  BorderStyle = bsDialog
  Caption = 'New Collection Wizard...  '#169' 2002, Thomas G. Grubb'
  ClientHeight = 447
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    632
    447)
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 19
    Top = 19
    Width = 593
    Height = 379
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      593
      379)
    object lblOwner: TLabel
      Left = 131
      Top = 128
      Width = 38
      Height = 16
      Alignment = taRightJustify
      Caption = 'O&wner'
      FocusControl = cbOwner
    end
    object lblCollectionDesc: TLabel
      Left = 34
      Top = 96
      Width = 135
      Height = 16
      Alignment = taRightJustify
      Caption = 'Collection &Descendant'
      FocusControl = cbCollectionDesc
    end
    object lblCollectionItemName: TLabel
      Left = 42
      Top = 32
      Width = 127
      Height = 16
      Alignment = taRightJustify
      Caption = 'Collection &Item Name'
      FocusControl = eCollectionItemName
    end
    object lblCollectionName: TLabel
      Left = 70
      Top = 64
      Width = 99
      Height = 16
      Alignment = taRightJustify
      Caption = '&Collection Name'
      FocusControl = eCollectionName
    end
    object cbOwner: TComboBox
      Left = 176
      Top = 120
      Width = 375
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      Sorted = True
      TabOrder = 3
      Text = 'TComponent'
      OnChange = eCollectionItemNameChange
    end
    object eCollectionItemName: TEdit
      Left = 176
      Top = 24
      Width = 375
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'TCollectionItem1'
      OnChange = eCollectionItemNameChange
    end
    object cbCollectionDesc: TComboBox
      Left = 176
      Top = 88
      Width = 375
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      ItemIndex = 1
      TabOrder = 2
      Text = 'TOwnedCollection'
      OnChange = eCollectionItemNameChange
      Items.Strings = (
        'TCollection'
        'TOwnedCollection')
    end
    object eCollectionName: TEdit
      Left = 176
      Top = 56
      Width = 375
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'TCollection1'
      OnChange = eCollectionItemNameChange
    end
    object pcOptions: TPageControl
      Left = 16
      Top = 160
      Width = 567
      Height = 201
      ActivePage = tsItemProperties
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabIndex = 1
      TabOrder = 4
      OnChange = pcOptionsChange
      object tsOptions: TTabSheet
        Caption = '&Options'
        DesignSize = (
          559
          170)
        object cbCreateCollectionItem: TCheckBox
          Left = 24
          Top = 8
          Width = 511
          Height = 17
          Action = actCreateItem
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 0
        end
        object cbCreateCollection: TCheckBox
          Tag = 1
          Left = 24
          Top = 30
          Width = 511
          Height = 17
          Action = actCreateCollection
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 1
        end
        object cbCollectionHasRef: TCheckBox
          Tag = 3
          Left = 24
          Top = 53
          Width = 511
          Height = 17
          Action = actItemHasRef
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 2
        end
        object cbItemHasRef: TCheckBox
          Tag = 2
          Left = 24
          Top = 76
          Width = 511
          Height = 17
          Action = actCollectionHasRef
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 3
        end
        object cbSafeConstructor: TCheckBox
          Tag = 4
          Left = 24
          Top = 98
          Width = 511
          Height = 17
          Action = actSafeConstructor
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 4
        end
        object cbAddAssign: TCheckBox
          Tag = 5
          Left = 24
          Top = 121
          Width = 511
          Height = 17
          Action = actAssignMethod
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 5
        end
        object cbGetDisplayName: TCheckBox
          Tag = 5
          Left = 24
          Top = 144
          Width = 511
          Height = 17
          Action = actGetDisplayName
          Anchors = [akLeft, akTop, akRight]
          State = cbChecked
          TabOrder = 6
        end
      end
      object tsItemProperties: TTabSheet
        Caption = 'Collection Item &Properties'
        ImageIndex = 1
        object sgItemProps: TStringGrid
          Left = 0
          Top = 0
          Width = 559
          Height = 170
          Align = alClient
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goTabs, goThumbTracking]
          TabOrder = 0
          OnGetEditText = sgItemPropsGetEditText
          OnKeyDown = sgItemPropsKeyDown
          OnSelectCell = sgItemPropsSelectCell
          OnSetEditText = sgItemPropsSetEditText
          ColWidths = (
            145
            120
            90
            157
            143)
        end
      end
      object tsPreview: TTabSheet
        Caption = 'Pre&view'
        ImageIndex = 2
        object mmPreview: TMemo
          Left = 0
          Top = 0
          Width = 559
          Height = 170
          Align = alClient
          Lines.Strings = (
            'Memo1')
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
  end
  object bbOk: TBitBtn
    Left = 398
    Top = 405
    Width = 105
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = bbOkClick
    Kind = bkOK
  end
  object bbCancel: TBitBtn
    Left = 510
    Top = 405
    Width = 105
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
  end
  object ActionList1: TActionList
    Left = 8
    Top = 408
    object actCreateItem: TAction
      AutoCheck = True
      Caption = '&1. Create Item'
      Checked = True
      Hint = 'Create the Collection Item Class'
      OnExecute = actCollectionHasRefExecute
    end
    object actCreateCollection: TAction
      Tag = 1
      AutoCheck = True
      Caption = '&2. Create Collection'
      Checked = True
      Hint = 'Create the New Collection Class'
      OnExecute = actCollectionHasRefExecute
    end
    object actItemHasRef: TAction
      Tag = 2
      AutoCheck = True
      Caption = '&3. Add Collection Reference to Owned Collection Item'
      Checked = True
      Hint = 
        'Create a correctly-typed reference in the Collection Item to the' +
        ' Collection'
      OnExecute = actCollectionHasRefExecute
    end
    object actCollectionHasRef: TAction
      Tag = 3
      AutoCheck = True
      Caption = '&4. Add Owner Reference to Owned Collection'
      Checked = True
      Hint = 
        'Create a reference in the Collection to the Owner of the Collect' +
        'ion'
      OnExecute = actCollectionHasRefExecute
    end
    object actSafeConstructor: TAction
      Tag = 4
      AutoCheck = True
      Caption = '&5. Add "Safe" Constructor'
      Checked = True
      Hint = 
        'Create a constructor which doesn'#39't allow changing which collecti' +
        'on item to make'
      OnExecute = actCollectionHasRefExecute
    end
    object actAssignMethod: TAction
      Tag = 5
      AutoCheck = True
      Caption = '&6. Add Assign Method to Collection Item'
      Checked = True
      Hint = 'Add Assign method to the Collection Item class'
      OnExecute = actCollectionHasRefExecute
    end
    object actGetDisplayName: TAction
      Tag = 6
      AutoCheck = True
      Caption = '&7. Override GetDisplayName method in Collection Item'
      Checked = True
      Hint = 
        'Override GetDisplayName method in Collection item (you will need' +
        ' to fill in code)'
      OnExecute = actCollectionHasRefExecute
    end
  end
end
