 TESDBGrid v 1.3 is based on native VCL TCustomDBGrid and has some functional
enhancements.
 Now you can display memo fields and multiline text fields of
different length. The height of strings depend on length of the text.
 You have possibility to limit string height and use the scrollbar
in inplace editor, to use multiline column headers, 
to select multiple strings with <Shift>, <Ctrl> and mouse
keys combination, to realize an enhanced control on colors,.
New properties:
1. in TESDBGrid
  ActiveColor:     TColor
  ActiveFontColor: TColor
  MemoAllowed:     Boolean
  MemoMaxRows:     Integer
  MemoOptions:     [moOnlyTabMove, moScrollBar, moWantArrows, moWantReturn]
  SelectedColor:   TColor
2. In TColum
  Memo:            Boolean
  Title
     TextAngle:    (taHorizontal, taVertical)

Installation:
1. Compile ESDBGrid60.dpk
2. Compile and install dESDBGrid60.dpk
3. Find new controls on "DataControls" page

S.Yermack
yys23@yahoo.com