object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Delete Tags GUI Demo'
  ClientHeight = 255
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    371
    255)
  PixelsPerInch = 96
  TextHeight = 13
  object imgDel: TImage
    Left = 353
    Top = 20
    Width = 10
    Height = 10
    Picture.Data = {
      07544269746D6170C6010000424DC60100000000000036000000280000000A00
      00000A0000000100200000000000900100000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008C8C8CFF6B6B6BFF00000000000000000000
      0000000000006B6B6BFF8C8C8CFF00000000000000006E6E6EFF6E6E6EFF6E6E
      6EFF00000000000000006E6E6EFF6E6E6EFF6E6E6EFF00000000000000000000
      00006E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF6E6E6EFF000000000000
      00000000000000000000000000006B6B6BFF6B6B6BFF6B6B6BFF6B6B6BFF0000
      00000000000000000000000000000000000000000000707070FF707070FF7070
      70FF707070FF0000000000000000000000000000000000000000707070FF7070
      70FF707070FF707070FF707070FF707070FF0000000000000000000000007070
      70FF707070FF707070FF0000000000000000707070FF707070FF707070FF0000
      000000000000999999FF737373FF000000000000000000000000000000007373
      73FF999999FF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000}
    Transparent = True
    Visible = False
  end
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 337
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    Color = clWhite
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    ParentBackground = False
    TabOrder = 0
    object tgcEdit: TTagCloud
      Left = 4
      Top = 4
      Width = 325
      Height = 210
      Align = alClient
      Alignment = taLeftJustify
      AutoShrinkRows = True
      Color = clWhite
      ColSpacing = 14
      HoverStyle = []
      HoverColSpace = True
      ItemFrame.BackColor = 14680063
      ItemFrame.FrameColor = 14737632
      ItemFrame.FrameMargin = 3
      ItemFrame.FrameSize = 1
      ItemFrame.RoundedSize = 6
      HoverFrame.BackColor = 9632254
      HoverFrame.FrameColor = clSilver
      HoverFrame.FrameMargin = 3
      HoverFrame.FrameSize = 1
      HoverFrame.RoundedSize = 6
      Items = <
        item
          Caption = 'cat'
        end
        item
          Caption = 'chickaree'
        end
        item
          Caption = 'dog'
        end
        item
          Caption = 'fox'
        end
        item
          Caption = 'leopard'
        end
        item
          Caption = 'penguin'
        end
        item
          Caption = 'raccoon'
        end
        item
          Caption = 'snake'
        end
        item
          Caption = 'tiger'
        end>
      MaxFontSize = 17
      Padding.Left = 3
      Padding.Top = 3
      Padding.Right = 3
      Padding.Bottom = 3
      ParentColor = False
      RowSpacing = 6
      Sorted = False
      Transparent = False
      OnAdvancedCustomDrawItem = tgcEditAdvancedCustomDrawItem
      OnTagClick = tgcEditTagClick
      ExplicitLeft = -80
      ExplicitTop = 24
      ExplicitWidth = 313
      ExplicitHeight = 133
    end
  end
end
