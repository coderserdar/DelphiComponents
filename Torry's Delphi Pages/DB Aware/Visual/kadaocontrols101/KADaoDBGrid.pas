unit KADaoDBGrid;
{$I KADaoControlsCommonDirectives.pas}
interface
{*******************************************************************************
  exoSelectCheckBoxes and Ascending/Descending
  sort indicators is inspired by SMDBGrid from
  Shkolnik Mike
  FIDOnet: 2:463/106.14
  E-Mail:  mshkolnik@scalabium.com
*******************************************************************************}
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, DB, Grids, DBGrids, Menus, Clipbrd;


type
  ExOptionsEnum = (exoRowSizing, exoWrapTitles, exoLoadAllRecords, exoSelectCheckBoxes);
  ExOptionsSet  = Set of ExOptionsEnum;
  TKADaoDBGrid = class(TDBGrid)
  private
    { Private declarations }
    F_TopRect            : TRect;
    F_Clicked            : Boolean;
    F_PopupMenu          : TPopupMenu;
    F_MenuBMP            : TBitmap;
    F_ASCBMP             : TBitmap;
    F_DESCBMP            : TBitmap;
    F_MenuTitles         : TStringList;
    F_OriginalTitles     : TStringList;
    F_LastFilter         : TStringList;
    F_FrameIndex         : Boolean;
    F_ShowGridMenu       : Boolean;
    F_AutoSize           : Boolean;
    F_AccessClpFormat    : Boolean;
    F_FindFieldsOnPaste  : Boolean;
    F_ShowCheckBoxes     : Boolean;
    F_NumMenus           : Integer;
    F_ExOptions          : ExOptionsSet;
    F_OrigRC             : Integer;
    F_ExportDir          : String;
    F_ColWidthZero       : Integer;
    CurrentRowsHeigh     : Integer;
    GlobalOptions        : TDBGridOptions;
    CheckW               : Integer;
    CheckH               : Integer;
    CoordX               : Integer;
    CoordY               : Integer;
    ColumnsOrigWidth     : Array [0..300] of Integer;
    Procedure            F_Set_MenuTitles(Value:TStringList);
    Procedure            F_Set_ShowGridMenu(Value : Boolean);
    Procedure            F_Set_ShowCheckBoxes(Value : Boolean);
    Procedure            F_Set_FrameIndex(Value : Boolean);
    Procedure            F_Set_AutoSize(Value : Boolean);
    Procedure            F_Set_ExOptions(Value : ExOptionsSet);
  protected
    { Protected declarations }
    Procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    Procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); override;
    Procedure DblClick; override;
    Procedure ColEnter;override;
    Function  IsInFieldList(FieldName, FieldList:String): Boolean;
    Procedure SizeColumns;
    Function  CalcTitlesHeight : Integer;
    Procedure RowHeightsChanged; override;
    Procedure LayoutChanged; override;
    Procedure SetColumnAttributes; override;
    Procedure Loaded; override;
    Procedure LinkActive(Value: Boolean);override;
    Procedure OnSelectIndex(Sender: TObject);
    Procedure OnSortBy(Sender: TObject);
    Procedure OnFilterBy(Sender: TObject);
    Procedure OnFind(Sender: TObject);
    Procedure OnSeek(Sender: TObject);
    Procedure OnQuickFind(Sender: TObject);
    Procedure OnQuickSeek(Sender: TObject);
    Procedure OnShowMemos(Sender: TObject);
    Procedure OnShowGUIDs(Sender: TObject);
    Procedure OnAutoSize(Sender: TObject);
    Procedure OnFastLookup(Sender: TObject);
    Procedure OnFrameIndexFields(Sender: TObject);
    Procedure OnChooseVisibleFields(Sender: TObject);
    Procedure OnSaveAs(Sender: TObject);
    Procedure OnSelectAll(Sender: TObject);
    Procedure OnDeSelectAll(Sender: TObject);
    Function  IsAccessDataInClipboard(Clp:TClipboard):Boolean;
    Function  ConvertToWideString(A: AnsiString): WideString;
    Function  StrToHex(S:String) : String;
    Function  HexToStr(S:String) : String;
    Function  GetFieldAsString(Field:TField):String;
    Function  GetFieldAsHEXString(Field:TField):String;
    Procedure CopyToClipboard;
    Function  PropertyExists(PropObject:OleVariant;PropertyName:String):Boolean;
    Function  GetCaption(FieldName : String):String;
    Function  FindField(FieldName:String; MSAccess:Boolean):TField;
    Function  FindColumn(FieldName:String):TColumn;
    Procedure ParseHexLine(RD,FN: TStringList; Line:String);
    Procedure ParseTextLine(RD : TStringList; Line:String);
    Function  ConvertToTS(S:String):TTimeStamp;
    Function  CountQuotes (S : String) : Integer;
    Function  CountTabs (S : String) : Integer;
    Function  ReplaceTabQuotes (Var S : String) : Integer;
    Function  ReplaceDoubleQuotes (Var S : String) : Integer;
    Procedure PreprocessRecords(Records : TStringList);
    Function  GetTextDateTime(S:String):TDateTime;
    Procedure PasteNativeFormat(CFFormat : Word; Clp:TClipboard);
    Procedure PasteTextFormat(CFFormat : Word; Clp:TClipboard);
    Procedure PasteFromClipboard;
    Procedure OnCutToClp(Sender: TObject);
    Procedure OnCopyToClp(Sender: TObject);
    Procedure OnPasteFromClp(Sender: TObject);
    Procedure OnUseAccessClpFormat(Sender: TObject);
    Procedure OnDelete(Sender: TObject);
    Procedure OnMailGridData(Sender: TObject);
  public
    { Public declarations }
    Procedure   SelectAll;
    Procedure   DeSelectAll;
    Procedure   CutToClp;
    Procedure   CopyToClp;
    Procedure   PasteFromClp;
    Procedure   FastLookup(Fast:Boolean);
    Procedure   ShowMemos(Show:Boolean);
    Property    GridPopupMenu : TPopupMenu  Read  F_PopupMenu;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    Property AutoSizeColumns             : Boolean      Read F_AutoSize           Write F_Set_AutoSize;
    Property ExOptions                   : ExOptionsSet Read F_ExOptions          Write F_Set_ExOptions;
    Property FindFieldsOnPaste           : Boolean      Read F_FindFieldsOnPaste  Write F_FindFieldsOnPaste;
    Property ShowGridMenu                : Boolean      Read F_ShowGridMenu       Write F_Set_ShowGridMenu;
    Property ShowCheckBoxes              : Boolean      Read F_ShowCheckBoxes     Write F_Set_ShowCheckBoxes;
    Property OfficeClipboardFormat       : Boolean      Read F_AccessClpFormat    Write F_AccessClpFormat;
    Property MenuTitles                  : TStringList  Read F_MenuTitles         Write F_Set_MenuTitles;
    Property FrameIndexFields            : Boolean      Read F_FrameIndex         Write F_Set_FrameIndex;
    Property DefaultExportDir            : String       Read F_ExportDir          Write F_ExportDir;
  end;

Procedure Register;

implementation
{$R KADaoDBGrid.res}
Uses Forms, Dialogs, KDaoTable, DaoApi, KADaoSelectIndexUnit,
  KADaoFilterByUnit, KADaoSortByDialogUnit, KADaoFindSeekUnit,
  KADaoQuickFindSeekUnit, KADaoSASUnit, KADaoDateTime, MAPI
  {$IFNDEF D4UP}, KADaoCVFUnit{$ENDIF}
  {$IFDEF  D4UP}, KADaoCVCUnit{$ENDIF}
  {$IFDEF  D6UP}, Variants{$ENDIF};

Type
 THackGrid = class(TCustomGrid)
    public
        Property Options;
    end;

constructor TKADaoDBGrid.Create(AOwner: TComponent);
Var
 MI  : TMenuItem;
 X   : Integer;
 BMP : TBitmap;
Begin
 Inherited Create(AOwner);
 F_ShowGridMenu      := True;
 F_ShowCheckBoxes    := True;
 F_AccessClpFormat   := False;
 F_FindFieldsOnPaste := False;
 F_Clicked           := False;
 F_MenuBMP           := TBitmap.Create;
 F_ASCBMP            := TBitmap.Create;
 F_DESCBMP           := TBitmap.Create;
 F_MenuBMP.LoadFromResourceName(HInstance, 'MENUBMP');
 F_ASCBMP.LoadFromResourceName(HInstance, 'ASC');
 F_DESCBMP.LoadFromResourceName(HInstance, 'DESC');

 F_MenuBMP.Transparent := True;
 F_MenuBMP.TransparentColor := clSilver;

 F_ASCBMP.Transparent := True;
 F_ASCBMP.TransparentColor := clSilver;

 F_DESCBMP.Transparent := True;
 F_DESCBMP.TransparentColor := clSilver;

 F_MenuTitles        := TStringList.Create;
 F_OriginalTitles    := TStringList.Create;
 F_LastFilter        := TStringList.Create;
 F_FrameIndex        := False;
 F_AutoSize          := False;
 F_OrigRC            := RowCount;
 CurrentRowsHeigh    := -1;
 GlobalOptions       := [];
 F_ExportDir         := '';

 //*************************************************************
 {00} F_MenuTitles.Add('Select Index'#9'Ctrl+I');
 {01} F_MenuTitles.Add('Sort');
 {02} F_MenuTitles.Add('Filter');
 {03} F_MenuTitles.Add('-');
 {04} F_MenuTitles.Add('Find');
 {05} F_MenuTitles.Add('Seek');
 {06} F_MenuTitles.Add('Quick Find');
 {07} F_MenuTitles.Add('Quick Seek');
 {08} F_MenuTitles.Add('-');
 {09} F_MenuTitles.Add('Fast Lookup'#9'Ctrl+L');
 {10} F_MenuTitles.Add('Show Memos'#9'Ctrl+M');
 {11} F_MenuTitles.Add('Show GUID''s'#9'Ctrl+G');

 {12} F_MenuTitles.Add('Frame Index Fields'#9'Ctrl+R');
 {13} F_MenuTitles.Add('Auto Size Columns'#9'Ctrl+Z');
 {14} F_MenuTitles.Add('Choose Visible Columns'#9'Ctrl+O');
 {15} F_MenuTitles.Add('-');
 {16} F_MenuTitles.Add('Save as - export'#9'Ctrl+S');
 {17} F_MenuTitles.Add('Mail as XLS/HTML'#9'Ctrl+T');
 {18} F_MenuTitles.Add('-');
 {19} F_MenuTitles.Add('Select All'#9'Ctrl+A');
 {20} F_MenuTitles.Add('Deselect All'#9'Ctrl+N');
 {21} F_MenuTitles.Add('Cut'#9'Ctrl+X');
 {22} F_MenuTitles.Add('Copy'#9'Ctrl+C');
 {23} F_MenuTitles.Add('Paste'#9'Ctrl+V');
 {24} F_MenuTitles.Add('Office-compatible Clipboard');

 F_OriginalTitles.Assign(F_MenuTitles);

 F_NumMenus        := F_MenuTitles.Count;
 F_PopupMenu       := TPopupMenu.Create(Self);
 F_PopupMenu.Name  := 'OptionsMenu';
 For X := 1 to F_NumMenus do
     Begin
      MI := TMenuItem.Create(F_PopupMenu);
      F_PopupMenu.Items.Insert(0,MI);
     End;

 F_PopupMenu.Items[0].OnClick  := OnSelectIndex;
 F_PopupMenu.Items[1].OnClick  := OnSortBy;
 F_PopupMenu.Items[2].OnClick  := OnFilterBy;
 F_PopupMenu.Items[4].OnClick  := OnFind;
 F_PopupMenu.Items[5].OnClick  := OnSeek;
 F_PopupMenu.Items[6].OnClick  := OnQuickFind;
 F_PopupMenu.Items[7].OnClick  := OnQuickSeek;
 F_PopupMenu.Items[9].OnClick  := OnFastLookup;
 F_PopupMenu.Items[10].OnClick := OnShowMemos;
 F_PopupMenu.Items[11].OnClick := OnShowGUIDs;
 F_PopupMenu.Items[12].OnClick := OnFrameIndexFields;
 F_PopupMenu.Items[13].OnClick := OnAutoSize;
 F_PopupMenu.Items[14].OnClick := OnChooseVisibleFields;
 F_PopupMenu.Items[17].OnClick := OnMailGridData;
 F_PopupMenu.Items[16].OnClick := OnSaveAs;
 F_PopupMenu.Items[19].OnClick := OnSelectAll;
 F_PopupMenu.Items[20].OnClick := OnDeSelectAll;
 F_PopupMenu.Items[21].OnClick := OnCutToClp;
 F_PopupMenu.Items[22].OnClick := OnCopyToClp;
 F_PopupMenu.Items[23].OnClick := OnPasteFromClp;
 F_PopupMenu.Items[24].OnClick := OnUseAccessClpFormat;
 //***************************************************

 //*************************************************** COMBO BOX BOOLEANS
 BMP        := TBitmap.Create;
 BMP.Handle := LoadBitmap(0, PChar(32759));
 CheckW     := BMP.Width  div 4;
 CheckH     := BMP.Height div 3;
 BMP.Free;
 //*************************************************** COMBO BOX BOOLEANS

 F_ColWidthZero := CheckW;
End;


destructor  TKADaoDBGrid.Destroy;
Begin
 F_DESCBMP.Free;
 F_ASCBMP.Free;
 F_MenuBMP.Free;
 F_PopupMenu.Free;
 F_MenuTitles.Free;
 F_OriginalTitles.Free;
 F_LastFilter.Free;
 Inherited Destroy;
End;

Procedure TKADaoDBGrid.F_Set_ExOptions(Value : ExOptionsSet);
Var
  RC : Integer;
  OO : ExOptionsSet;
Begin
  OO          := F_ExOptions;
  F_ExOptions := Value;
  if csLoading    in ComponentState Then Exit;
  if (NOT Assigned(DataSource)) Then Exit;
  if (NOT Assigned(DataSource.DataSet)) Then Exit;
  if (NOT DataSource.DataSet.Active) Then Exit;

  //////////////////////////////////////////////////////////////////////////////
  if csDesigning  in ComponentState Then
     Begin
       if  (exoSelectCheckBoxes in F_ExOptions)
       And (dgMultiSelect in Options) Then
           Begin
             SetColumnAttributes;
           End;

       if  (NOT (exoSelectCheckBoxes in F_ExOptions))
       And (exoSelectCheckBoxes in OO)
       And (dgMultiSelect in Options) Then
            Begin
             SetColumnAttributes;
            End;
       Exit;
     End;
  //////////////////////////////////////////////////////////////////////////////
  if exoWrapTitles in F_ExOptions Then
     Begin
       THackGrid(Self).Options := THackGrid(Self).Options + [goRowSizing];
       if dgTitles in Options Then
          Begin
           RowHeights[0] := CalcTitlesHeight;
           RecreateWnd;
          End;
       if NOT (exoRowSizing in F_ExOptions) Then
          THackGrid(Self).Options := THackGrid(Self).Options - [goRowSizing];
     End;
  //////////////////////////////////////////////////////////////////////////////
  if exoRowSizing in F_ExOptions Then
     Begin
       THackGrid(Self).Options := THackGrid(Self).Options + [goRowSizing];
     End
  Else
     Begin
       THackGrid(Self).Options := THackGrid(Self).Options - [goRowSizing];
       DefaultRowHeight := -1;
       Canvas.Font := Font;
       DefaultRowHeight := Canvas.TextHeight('I')+4;
       if dgTitles in Options Then
          Begin
           RowHeights[0] := CalcTitlesHeight;
          End;
       RecreateWnd;
     End;
  //////////////////////////////////////////////////////////////////////////////
  if exoLoadAllRecords in F_ExOptions Then
     Begin
        if  Assigned(DataSource)
        And Assigned(DataSource.Dataset)
        And (DataSource.Dataset is TKADaoTable)
        And (DataSource.DataSet.Active) Then                                
            Begin
              RC := DataSource.DataSet.RecordCount;
              if dgTitles in Options Then Inc(RC);
              if RC > RowCount Then RowCount := RC;
            End;
     End
  Else
     Begin
       RowCount := F_OrigRC;
     End;

  if (exoSelectCheckBoxes in F_ExOptions)
  And (dgMultiSelect in Options) Then SetColumnAttributes;

  if  (NOT (exoSelectCheckBoxes in F_ExOptions))
  And (exoSelectCheckBoxes in OO)
  And (dgMultiSelect in Options) Then SetColumnAttributes;
End;

Procedure TKADaoDBGrid.Loaded;
Var
 X   : Integer;
 S   : String;
Begin
  Inherited Loaded;
  if F_OriginalTitles.Count <> F_MenuTitles.Count Then F_MenuTitles.Assign(F_OriginalTitles);
  For X := 0 To F_MenuTitles.Count-1 do
      Begin
       if X < F_PopupMenu.Items.Count Then
          Begin
            S:=F_MenuTitles.Strings[X];
            if (Length(S) > 0) And (S[1]='!') Then System.Delete(S,1,1);
            F_PopupMenu.Items[X].Caption:=S;
          End;
      End;
  Try
   if  (Assigned(DataSource))
   And (Assigned(DataSource.DataSet))
   And (DataSource.DataSet is TKaDaoTable)
   And (Assigned(TKaDaoTable(DataSource.Dataset).Database)) Then
       Begin
          TKaDaoTable(DataSource.Dataset).Database.Idle;
       End;
  Except
  End;
  F_Set_AutoSize(F_AutoSize);
End;

Procedure TKADaoDBGrid.LinkActive(Value: Boolean);
Var
 X : Integer;
begin
    Inherited LinkActive(Value);
    if  Assigned(DataSource)
    And Assigned(DataSource.Dataset)
    And (DataSource.DataSet is TKaDaoTable)
    And (TKADaoTable(DataSource.DataSet).TableType=dbOpenTable) Then
        Begin
         For X := 0 To Columns.Count-1 do
            Columns.Items[X].Width := Columns.Items[X].Width+F_ASCBMP.Width;
        End;
    if  (Assigned(DataSource))
    And (Assigned(DataSource.DataSet))
    And (DataSource.DataSet is TKaDaoTable)
    And (DataSource.DataSet.Active)
    And (DataSource.DataSet.State = dsBrowse) Then
        Begin
         F_PopupMenu.Items[10].Checked := TKaDaoTable(DataSource.Dataset).CacheMemos;
         F_PopupMenu.Items[11].Checked := TKaDaoTable(DataSource.Dataset).ShowGUID;
         F_PopupMenu.Items[13].Checked := F_AutoSize;
         F_PopupMenu.Items[23].Checked := F_AccessClpFormat;
         if (F_ExOptions <> []) Then ExOptions := F_ExOptions;
        End;
    if  (Assigned(DataSource))
    And (Assigned(DataSource.DataSet))
    And (DataSource.DataSet.Active)
    And (DataSource.DataSet.State = dsBrowse) Then
        Begin
          if F_AutoSize Then AutoSizeColumns:=True;
        End;  
    //**************************************************** COMBO BOX BOOLEANS
    // If First field is ftBoolean then simulate ColEnter
    //****************************************************
    ColEnter;
    //**************************************************** COMBO BOX BOOLEANS
end;

Procedure TKADaoDBGrid.F_Set_MenuTitles(Value:TStringList);
Begin
 F_MenuTitles.Assign(Value);
End;

Procedure TKADaoDBGrid.F_Set_ShowGridMenu(Value : Boolean);
Begin
 F_ShowGridMenu:=Value;
 Refresh;
End;

Procedure TKADaoDBGrid.F_Set_ShowCheckBoxes(Value : Boolean);
Var
 Tmp : Boolean;
Begin
 Tmp := F_ShowCheckBoxes;
 F_ShowCheckBoxes:=Value;
 If csLoading in ComponentState Then Exit;
 if Tmp Then
    Begin
      if (dgEditing in Options)
      And (NOT (goEditing in THackGrid(Self).Options)) Then
          THackGrid(Self).Options := THackGrid(Self).Options + [goEditing];
    End;
 ColEnter;                                                                         
 Refresh;
End;

Procedure TKADaoDBGrid.F_Set_FrameIndex(Value : Boolean);
Begin
 F_FrameIndex := Value;
 F_PopupMenu.Items[12].Checked := Value;
 Refresh;
End;

Procedure TKADaoDBGrid.F_Set_AutoSize(Value : Boolean);
Var
 X   : Integer;
 Vis : Boolean;
Begin
 F_AutoSize := Value;
 if csLoading In ComponentState Then Exit;
 Vis := Visible;
 if F_AutoSize Then
    Begin
      Hide;
      Try
       For X := 0 To Columns.Count-1 do
          Begin
            if Assigned(Columns.Items[X].Field) Then
               ColumnsOrigWidth[X] := Columns.Items[X].Width;
          End;
       SizeColumns;
      Finally
       if Vis then Show;
      End;
   End
 Else
   Begin
      Hide;
      Try
        For X := 0 To Columns.Count-1 do
          Begin
            if Assigned(Columns.Items[X].Field) Then
               Begin
                 if ColumnsOrigWidth[X] > 0 Then
                    Columns.Items[X].Width := ColumnsOrigWidth[X]
                 Else
                    if Columns.Items[X].Width > 0 Then
                       ColumnsOrigWidth[X] := Columns.Items[X].Width;
               End;
          End;
      Finally
       if Vis then Show;
      End;
   End;
 Visible := Vis;
End;

Function TKADaoDBGrid.CalcTitlesHeight : Integer;
Var
 X     : Integer;
 H     : Integer;
 Title : String;
 ARect : Trect;
Begin
 Result := 0;
 For X := 0 to Columns.Count-1 do
     Begin
       Canvas.Font := Columns.Items[X].Title.Font;
       if (exoWrapTitles in F_ExOptions) Then
          Begin
            Title       := Columns.Items[X].Title.Caption;
            ARect       := CellRect(X,0);
            ARect.Left  := 0;
            ARect.Right := Columns.Items[X].Width-2;                                                                       
            H := DrawText(Canvas.Handle, PChar(Title), Length(Title), ARect, DT_CALCRECT OR DT_WORDBREAK OR DT_EXPANDTABS OR DT_NOPREFIX);
          End
       Else
          Begin
           H := Canvas.TextHeight(Columns.Items[X].Title.Caption);
          End;
       if H > Result Then Result := H;
     End;
 Result := Result + 4;
End;

Procedure TKADaoDBGrid.RowHeightsChanged;
Var
  RH : Integer;
  X  : Integer;
  BR : Integer;
Begin
  if  (exoRowSizing in F_ExOptions) Then
      Begin
       RH := DefaultRowHeight;
       BR := 0;
       If dgTitles in Options Then Inc(BR);
       For X := BR To RowCount-1 do
           Begin
             if RowHeights[X] <> RH Then
                Begin                                                       
                  Inherited DefaultRowHeight := RowHeights[X];
                  CurrentRowsHeigh :=  RowHeights[X];
                  if BR=1 Then RowHeights[0] := CalcTitlesHeight;
                  System.Break;
                End;
           End;
      End;
  Inherited RowHeightsChanged;
End;

Procedure TKADaoDBGrid.LayoutChanged;
Begin
 inherited LayoutChanged;
 if (NOT Assigned(DataSource)) Then Exit;
 if (NOT Assigned(DataSource.DataSet)) Then Exit;
 if (NOT DataSource.DataSet.Active) Then Exit;
 if exoWrapTitles in F_ExOptions Then
     Begin
       THackGrid(Self).Options := THackGrid(Self).Options + [goRowSizing];
       if dgTitles in Options Then
          Begin
           RowHeights[0] := CalcTitlesHeight;
           RecreateWnd;
          End;
       if NOT (exoRowSizing in F_ExOptions) Then
          THackGrid(Self).Options := THackGrid(Self).Options - [goRowSizing];
     End;
 if  (exoRowSizing in F_ExOptions)
 And (CurrentRowsHeigh <> -1) Then
     Begin
       Inherited DefaultRowHeight := CurrentRowsHeigh;
       If dgTitles in Options Then RowHeights[0] := CalcTitlesHeight;
     End;
End;

Procedure TKADaoDBGrid.SetColumnAttributes;
Begin
 Inherited SetColumnAttributes;

 if  (dgIndicator in Options)
 And (dgMultiSelect in Options)
 And (exoSelectCheckBoxes in ExOptions) Then
    Begin
      ColWidths[0] := CheckW*2;
    End;

 if  (dgIndicator in Options)
 And (dgMultiSelect in Options)
 And (NOT (exoSelectCheckBoxes in ExOptions)) Then
    Begin
      ColWidths[0] := F_ColWidthZero;
    End;
End;

Procedure TKADaoDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell      : TGridCoord;
  SaveState : TGridState;
  Rect      : TRect;
Begin
   SaveState := FGridState;
   if (SaveState = gsRowSizing) or (SaveState = gsColSizing) or (Not F_ShowGridMenu) or
      ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
      (PtInRect(InplaceEditor.BoundsRect, Point(X,Y)))) then
       Begin
         inherited MouseDown(Button, Shift, X, Y);
         Exit;
       End;
 if  (Assigned(DataSource))
 And (Assigned(DataSource.DataSet))
 And (DataSource.DataSet.Active)
 And (DataSource.DataSet.State = dsBrowse) Then
     Begin
      Cell := MouseCoord(X,Y);
      if (Button = mbLeft) And (Cell.X =0 ) And (Cell.Y = 0) And (dgIndicator in Options) And (dgTitles in Options) then
         Begin
          //***********************************************************************
          // Upper Left Rectange is clicked - draw a flat style fixed cell
          //***********************************************************************
          F_Clicked := True;
          Rect := F_TopRect;
          Canvas.Brush.Color := FixedColor;
          Canvas.FillRect(Rect);
          if (dgColLines in Options) Then
             Begin
              InflateRect(Rect, 1, 1);
              DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_FLAT);
              DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_FLAT);
             End;
          Canvas.Draw(((Rect.Left+Rect.Right-F_MenuBMP.Width) div 2)+1,((Rect.Top + Rect.Bottom - F_MenuBMP.Height) div 2)+1, F_MenuBMP);
         End;
      If  (dgIndicator in Options)
      And (dgMultiSelect in Options)
      And (exoSelectCheckBoxes in ExOptions) Then
          Begin
            //***********************************************************************
            // CkeckMark
            //***********************************************************************
            if Cell.X = 0 Then
               Begin
                 if     (ssShift in Shift) Then Shift := Shift-[ssShift];
                 if     (ssAlt   in Shift) Then Shift := Shift-[ssAlt];
                 if Not (ssCtrl  in Shift) Then Shift := Shift+[ssCtrl];
               End
            Else
               Begin
               End;
           End;
     End;
 inherited MouseDown(Button, Shift, X, Y);
End;

Procedure TKADaoDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cell      : TGridCoord;
  SaveState : TGridState;
  Rect      : TRect;
  ShowMenu  : Boolean;
  MPT       : TPoint;
begin
  CoordX    := X;
  CoordY    := Y;
  ShowMenu  := False;
  SaveState := FGridState;
  if (SaveState = gsRowSizing) or (SaveState = gsColSizing) or (Not F_ShowGridMenu) or
     ((InplaceEditor <> nil) and (InplaceEditor.Visible) and
     (PtInRect(InplaceEditor.BoundsRect, Point(X,Y)))) then
     Begin
       inherited MouseUp(Button, Shift, X, Y);
       Exit;
     End;
  if  (Assigned(DataSource))
  And (Assigned(DataSource.DataSet))
  And (DataSource.DataSet.Active)
  And (DataSource.DataSet.State = dsBrowse) Then
      Begin
       Cell := MouseCoord(X,Y);
       if ((Button = mbLeft) and (Cell.X =0 ) and (Cell.Y = 0) And (dgIndicator in Options) And (dgTitles in Options)) Or (F_Clicked) then
          Begin
           //***********************************************************************
           // Upper Left Rectange is clicked -
           // draw a raised style fixed cell and popup a context menu
           //***********************************************************************
           Rect := F_TopRect;
           Canvas.Brush.Color := FixedColor;
           Canvas.FillRect(Rect);
           if (dgColLines in Options) Then
              Begin
                DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
                DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_TOPLEFT);
              End;
           F_Clicked := False;
           ShowMenu  := True;
           //***********************************************************************
           Button:=mbRight;
          End;
      IF  (dgIndicator in Options)
      And (dgMultiSelect in Options)
      And (exoSelectCheckBoxes in ExOptions) Then
          Begin
           //***********************************************************************
           // CkeckMark
           //***********************************************************************
           if Cell.X = 0 Then
              Begin
                if     (ssShift in Shift) Then Shift := Shift-[ssShift];
                if     (ssAlt   in Shift) Then Shift := Shift-[ssAlt];
                if Not (ssCtrl  in Shift) Then Shift := Shift+[ssCtrl];
              End
           Else
              Begin
              End;
          End;
      End;
  inherited MouseUp(Button, Shift, X, Y);
  if  (Assigned(DataSource))
  And (Assigned(DataSource.DataSet))
  And (DataSource.DataSet.Active)
  And (DataSource.DataSet.State = dsBrowse)
  And (ShowMenu)  Then
     Begin
       MPT.X := F_TopRect.Left;
       MPT.Y := F_TopRect.Bottom;
       MPT := ClientToScreen(MPT);
       DrawCell(0,0,F_TopRect,[gdFixed]);
       If  (DataSource.DataSet is TKaDaoTable)
       And (DataSource.DataSet.Active)
       And (DataSource.DataSet.State=dsBrowse) Then
           Begin
             For X := 0 to F_NumMenus-1 do F_PopupMenu.Items[X].Enabled := True;
             if TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable Then
                Begin
                  F_PopupMenu.Items[1].Enabled := False;
                  F_PopupMenu.Items[2].Enabled := False;
                  F_PopupMenu.Items[4].Enabled := False;
                  F_PopupMenu.Items[6].Enabled := False;
                  if TKaDaoTable(DataSource.Dataset).IndexFieldCount=0 Then
                     Begin
                       F_PopupMenu.Items[5].Enabled  := False;
                       F_PopupMenu.Items[7].Enabled  := False;
                       F_PopupMenu.Items[12].Enabled := False;
                     End;
                End
             Else
                Begin
                  F_PopupMenu.Items[0].Enabled  := False;
                  F_PopupMenu.Items[5].Enabled  := False;
                  F_PopupMenu.Items[7].Enabled  := False;
                  F_PopupMenu.Items[12].Enabled := False;
                End;
             F_PopupMenu.Items[10].Checked := TKaDaoTable(DataSource.Dataset).CacheMemos;
             F_PopupMenu.Items[11].Checked := TKaDaoTable(DataSource.Dataset).ShowGUID;
             F_PopupMenu.Items[13].Checked := F_AutoSize;
             F_PopupMenu.Items[24].Checked := F_AccessClpFormat;
              //****************************************************************
              // No SelectAll, Cut, Copy, Paste and Delete on EMPTY Dataset
              //****************************************************************
              If (DataSource.DataSet.IsEmpty) Then
                Begin
                  F_PopupMenu.Items[16].Enabled := False;
                  F_PopupMenu.Items[19].Enabled := False;
                  F_PopupMenu.Items[20].Enabled := False;
                  F_PopupMenu.Items[21].Enabled := False;
                  F_PopupMenu.Items[22].Enabled := False;
                End;
              //****************************************************************
              // No Cut, Paste and Delete on ReadOnly Dataset
              //****************************************************************
              if  (NOT DataSource.DataSet.CanModify)
              Or  (ReadOnly) Then
                 Begin
                  F_PopupMenu.Items[21].Enabled := False;
                  F_PopupMenu.Items[23].Enabled := False;
                 End;
              //****************************************************************
              // Hided by the Programmer
              //****************************************************************
              For X := 0 to F_NumMenus-1 do
                 Begin
                   if X < F_MenuTitles.Count-1 Then
                      Begin
                        if F_MenuTitles.Strings[X][1]='!' Then F_PopupMenu.Items[X].Visible := False;
                      End;
                 End;
             F_PopupMenu.Popup(MPT.X,MPT.Y);
           End;
     End;
End;

Function TKADaoDBGrid.IsInFieldList(FieldName, FieldList:String): Boolean;
Var
 SL : TStringList;
 FL : String;
 X  : Integer;
 L  : Integer;
Begin
 Result := False;
 FL     := FieldList;
 L      := Length(FL);
 if L=0 Then Exit;
 For X := 1 To L Do If FL[X]=';' Then FL[X]:=','; 
 SL     := TStringList.Create;
 Try
  SL.CommaText := FL;
  if SL.IndexOf(FieldName) <> -1 Then Result  := True;
 Except
 End;
 SL.Free;
End;

Procedure TKADaoDBGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
Var
 Title     : String;
 Al        : Integer;
 Col_      : Integer;
 Row_      : Integer;
 Style     : UINT;
 CheckRect : TRect;
 OldActive : Integer;

 Column    : TColumn;
 ADBMP     : TBitmap;
 ID        : TIndexDef;
Begin
 inherited DrawCell(ACol, ARow, ARect, AState);
 if (AState <> [gdFixed])                  Then Exit;
 if (NOT Assigned(DataSource))             Then Exit;
 if (NOT Assigned(DataSource.DataSet))     Then Exit;
 If (NOT DataSource.DataSet.Active)        Then Exit;
 ///////////////////////////////////////////////////////////////////////////////
 Col_ := ACol;
 if dgIndicator in Options Then Dec(Col_);
 Row_ := ARow;
 if (dgTitles in Options) Then  Dec(Row_);
 ///////////////////////////////////////////////////////////////////////////////
 if  (dgIndicator in Options)
 And (dgTitles in Options)
 And (ACol=0)
 And (ARow=0) Then
    Begin
      //////////////////////////////////////////////////////////////////////////
      if (F_ShowGridMenu) And (DataSource.DataSet.State = dsBrowse) Then
          Begin
           F_TopRect := ARect;
           BeginUpdate;
           Canvas.Lock;
           Canvas.Draw((ARect.Left+ARect.Right-F_MenuBMP.Width) div 2,(ARect.Top + ARect.Bottom - F_MenuBMP.Height) div 2, F_MenuBMP);
           Canvas.UnLock;
           EndUpdate;
          End;
      //////////////////////////////////////////////////////////////////////////
    End
 Else
    Begin
      //////////////////////////////////////////////////////////////////////////
      if  (ARow=0)
      And (exoWrapTitles in ExOptions)
      And (dgTitles in Options) Then
          Begin
            Title := Columns.Items[Col_].Title.Caption;
            BeginUpdate;
            Canvas.Lock;
            if (dgColLines in Options) Then InflateRect(ARect, -1, -1);
            Canvas.FillRect(ARect);
            Canvas.Font := Columns.Items[Col_].Title.Font;
            Canvas.Brush.Color := Columns.Items[Col_].Title.Color;
            Al := DT_LEFT;
            Case Columns.Items[Col_].Title.Alignment of
                 taLeftJustify   : Al := DT_LEFT;
                 taRightJustify  : Al := DT_RIGHT;
                 taCenter        : Al := DT_CENTER;
            End;
            DrawText(Canvas.Handle, PChar(Title), Length(Title), ARect, DT_WORDBREAK OR DT_EXPANDTABS OR DT_NOPREFIX Or AL);
            if (dgColLines in Options) Then
               Begin
                InflateRect(ARect, 1, 1);
                DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
                DrawEdge(Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT);
               End;
            Canvas.UnLock;
            EndUpdate;
          End;
      //////////////////////////////////////////////////////////////////////////
      if  (ACol=0)
      And (Row_ >= 0)
      And (dgIndicator in Options)
      And (dgMultiSelect in Options)
      And (exoSelectCheckBoxes in ExOptions) Then
          Begin
           //***********************************************************************
           // Draw Ckeckbox and CheckMark if the row is selected
           //***********************************************************************
           BeginUpdate;
           Canvas.Lock;
           CheckRect := ARect;
           CheckRect.Left := ARect.Left + 1;
           CheckRect.Right := CheckRect.Left + CheckW;
           CheckRect.Top := ARect.Top + (ARect.Bottom - ARect.Top - CheckH) div 2;
           CheckRect.Bottom := CheckRect.Top + CheckH;
           Style := DFCS_BUTTONCHECK;
           OldActive := DataLink.ActiveRecord;
           Try
             DataLink.ActiveRecord := Row_;
             if SelectedRows.IndexOf(Datalink.DataSet.Bookmark) <> -1 Then Style := DFCS_CHECKED;
           Except
           End;
           DataLink.ActiveRecord := OldActive;
           DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, Style);
           Canvas.UnLock;
           EndUpdate;
         End;
      //////////////////////////////////////////////////////////////////////////
      if (dgTitles in Options)
      And (ARow=0) Then
      Begin
        if (dgIndicator in Options) And (ACol=0) Then
            Begin
             //***************************************** NOTHING
            End
        Else
            Begin
              {$IFDEF D4UP}
              if  (DataSource.DataSet is TKADaoTable)                                              
              And (TKADaoTable(DataSource.DataSet).TableType=dbOpenTable)
              And (TKADaoTable(DataSource.DataSet).IndexName<> '') Then
                  Begin
                   Column := Columns[Col_];
                   ID     := TKaDaoTable(DataSource.Dataset).IndexDefs.Find(TKADaoTable(DataSource.DataSet).IndexName);
                   if (ID <> Nil)
                   And (Assigned(Column))
                   And (Column.FieldName <> '')
                   And (IsInFieldList(Column.FieldName, ID.Fields)) Then
                       Begin
                         if IsInFieldList(Column.FieldName, ID.DescFields) Then
                            ADBMP := F_DESCBMP
                         Else
                            ADBMP := F_ASCBMP;
                         if (dgColLines in Options) Then InflateRect(ARect, -4, -2);
                         BeginUpdate;
                         Canvas.Lock;
                         Canvas.Draw(ARect.Right-ADBMP.Width, ARect.Top, ADBMP);
                         Canvas.UnLock;
                         EndUpdate;
                         if (dgColLines in Options) Then InflateRect(ARect, 4, 2);
                       End;
                  End;
              {$ENDIF}
            End;
      End;
      //////////////////////////////////////////////////////////////////////////
    End;
End;

Procedure TKADaoDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
Var
  X         : Integer;
  CheckRect : TRect;
  Style     : UINT;
Begin
 Inherited DrawColumnCell(Rect,DataCol,Column,State);
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (Assigned(Column.Field)) Then
     Begin
       if (F_FrameIndex) And (TKaDaoTable(DataSource.Dataset).IndexFieldCount > 0) Then
          Begin
            For X := 0 To  TKaDaoTable(DataSource.Dataset).IndexFieldCount-1 do
                Begin
                  if AnsiLowerCase(Column.Field.FieldName)= AnsiLowerCase(TKaDaoTable(DataSource.Dataset).IndexFields[X].FieldName) Then
                     Begin
                       if Column.FieldName <> '' then
                          Begin
                           BeginUpdate;
                           Canvas.Lock;
                           Canvas.Brush.Color:=clRed;
                           Canvas.FrameRect(Rect);
                           Canvas.UnLock;
                           EndUpdate;
                          End;
                     End;
                End;
          End;
       //**************************************************** COMBO BOX BOOLEANS
       if  (F_ShowCheckBoxes)
       And (Assigned(Column.Field))
       And (Column.Field.DataType=ftBoolean) Then
          Begin
            if Column.FieldName <> '' then
               Begin
                 BeginUpdate;
                 Canvas.Lock;
                 Canvas.FillRect(Rect);
                 CheckRect := Rect;
                 CheckRect.Left := Rect.Left + (Rect.Right - Rect.Left - CheckW) div 2;
                 CheckRect.Right := CheckRect.Left + CheckW;
                 CheckRect.Top := Rect.Top + (Rect.Bottom - Rect.Top - CheckH) div 2;
                 CheckRect.Bottom := CheckRect.Top + CheckH;
                 if Column.Field.AsBoolean Then
                    Style := DFCS_CHECKED
                 Else
                    Style := DFCS_BUTTONCHECK;
                 DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, Style);
                 Canvas.UnLock;
                 EndUpdate;
               End;
          End;
     End;
End;

Procedure TKADaoDBGrid.DblClick;
Var
  Column : TColumn;
  GC     : TGridCoord;
  PozX   : Integer;
  PozY   : Integer;
Begin
  //**************************************************** COMBO BOX BOOLEANS
  GC   := MouseCoord(CoordX,CoordY);
  PozX := -1;
  PozY := -1;
  If  (dgIndicator in Options) Then PozX := 0;
  If  (dgTitles in Options)    Then PozY := 0;
  if  (GC.X > PozX)
  And (GC.Y > PozY)
  And Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active) Then
      Begin
        Column := Columns.Items[SelectedIndex];
        if  (F_ShowCheckBoxes)
        And (Assigned(Column.Field))
        And (Column.Field.DataType = ftBoolean)
        And (NOT ReadOnly)
        And (NOT (dgRowSelect in Options))
        And ((dgEditing in Options))
        And (DataSource.Dataset.CanModify)
        And (Not Column.Field.ReadOnly) Then
             Begin
                If (CanEditModify) Then
                   Begin
                    BeginUpdate;
                    Try
                     Column.Field.AsBoolean := Not Column.Field.AsBoolean;
                    Except
                    End;
                    inherited DblClick;
                    EndUpdate;
                    Exit;
                   End;
             End;
      End;
  //**************************************************** COMBO BOX BOOLEANS
  inherited DblClick;
End;

Procedure TKADaoDBGrid.ColEnter;
Var
  Col : TColumn;
Begin
  if  (csLoading in ComponentState) Then Exit;
  Col := Columns.Items[SelectedIndex];
  If  (F_ShowCheckBoxes)
  And ((dgEditing in Options))
  And (NOT (dgRowSelect in Options))
  And (Assigned(Col.Field)) Then
      Begin
        //*************************************************** COMBO BOX BOOLEANS
        If (Col.Field.DataType=ftBoolean) Then
           Begin
              if (goEditing in THackGrid(Self).Options) Then
                Begin
                  BeginUpdate;
                  THackGrid(Self).Options := THackGrid(Self).Options - [goEditing];
                  EndUpdate;
                End;
              if (goAlwaysShowEditor in THackGrid(Self).Options) Then
                Begin
                  BeginUpdate;
                  HideEditor;
                  EndUpdate;
                End;
           End
        //*************************************************** COMBO BOX BOOLEANS
        Else
           Begin
             if NOT (goEditing in THackGrid(Self).Options) Then
                Begin
                  BeginUpdate;
                  THackGrid(Self).Options := THackGrid(Self).Options + [goEditing];
                  EndUpdate;
                End;
           End;
      End;
 Inherited ColEnter;
End;

Procedure TKADaoDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
Var
 PozX : Integer;
 PozY : Integer;
Begin
 Inherited KeyDown(Key, Shift);
 if (Key=Ord('A')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[19].Visible Then Exit;
      if NOT F_PopupMenu.Items[19].Enabled Then Exit;
      OnSelectAll(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('N')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[20].Visible Then Exit;
      if NOT F_PopupMenu.Items[20].Enabled Then Exit;
      OnDeSelectAll(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('I')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[0].Visible Then Exit;
      if NOT F_PopupMenu.Items[0].Enabled Then Exit;
      OnSelectIndex(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('L')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[9].Visible Then Exit;
      if NOT F_PopupMenu.Items[9].Enabled Then Exit;
      OnFastLookup(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('M')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[10].Visible Then Exit;
      if NOT F_PopupMenu.Items[10].Enabled Then Exit;
      OnShowMemos(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('G')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[11].Visible Then Exit;
      if NOT F_PopupMenu.Items[11].Enabled Then Exit;
      OnShowGUIDs(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('R')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[12].Visible Then Exit;
      if NOT F_PopupMenu.Items[12].Enabled Then Exit;
      OnFrameIndexFields(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('Z')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[13].Visible Then Exit;
      if NOT F_PopupMenu.Items[13].Enabled Then Exit;
      OnAutoSize(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('O')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[14].Visible Then Exit;
      if NOT F_PopupMenu.Items[14].Enabled Then Exit;
      OnChooseVisibleFields(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('S')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[16].Visible Then Exit;
      if NOT F_PopupMenu.Items[16].Enabled Then Exit;
      OnSaveAs(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('T')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[17].Visible Then Exit;
      if NOT F_PopupMenu.Items[17].Enabled Then Exit;
      OnMailGridData(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('X')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[21].Visible Then Exit;
      if NOT F_PopupMenu.Items[21].Enabled Then Exit;
      OnCutToClp(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('C')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[22].Visible Then Exit;
      if NOT F_PopupMenu.Items[22].Enabled Then Exit;
      OnCopyToClp(F_PopupMenu);
      SetFocus;
    End;
 if (Key=Ord('V')) And (ssCtrl in Shift) And (Datalink.DataSet.State=dsBrowse) Then
    Begin
      if NOT F_PopupMenu.Items[23].Visible Then Exit;
      if NOT F_PopupMenu.Items[23].Enabled Then Exit;
      OnPasteFromClp(F_PopupMenu);
      SetFocus;
    End;
 //**************************************************** COMBO BOX BOOLEANS
 if (Key = VK_SPACE) Or (Key = VK_RETURN) Or (Key = VK_F2) Then
    Begin
      if NOT (dgRowSelect in Options)
      And    ((dgEditing in Options)) Then
         Begin
           If  (F_ShowCheckBoxes)
           And (Columns.Items[SelectedIndex].Field.DataType = ftBoolean) Then
              Begin
                if  (DataLink.DataSet.CanModify)
                And (Not ReadOnly)
                And (Not Columns.Items[SelectedIndex].Field.ReadOnly) Then
                    Begin
                     Key := 0;
                     BeginUpdate;
                     PozX := SelectedIndex;
                     PozY := SelectedIndex;
                     If  (dgIndicator in Options) Then Inc(PozX);
                     If  (dgTitles in Options)    Then Inc(PozY);
                     CoordX := CellRect(PozX,PozY).Left;
                     CoordY := CellRect(PozX,PozY).Top;
                     DblClick;
                     EndUpdate;
                    End;
              End;
         End;
    End;
 //**************************************************** COMBO BOX BOOLEANS
End;


Procedure TKADaoDBGrid.SizeColumns;
Var
  RS : TDataset;
  CW : TStringList;
  X  : Integer;
  W  : Integer;
  S  : String;
  BK : TBookmarkStr;
Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet.Active)
  And (NOT DataSource.DataSet.IsEmpty) Then
      Begin
        CW := TStringList.Create;
        For X := 0 To Columns.Count-1 do
            Begin
              W := Canvas.TextWidth(Columns.Items[X].Title.Caption+'WWW');
              if (DataSource.DataSet is TKaDaoTable) And (TKADaoTable(DataSource.DataSet).TableType=dbOpenTable) Then Inc(W, F_ASCBMP.Width);
              CW.AddObject(Columns.Items[X].Field.FieldName,TObject(W));
            End;
        Enabled := False;
        DataSource.DataSet.DisableControls;
        Try
          RS := DataSource.DataSet;
          BK := RS.Bookmark;
          RS.First;
          While Not RS.EOF Do
            Begin
              For X := 0 To Columns.Count-1 do
                  Begin
                    if Fields[X].IsBlob Then
                       S := Fields[X].DisplayText
                    Else
                       S := Fields[X].Text;
                    W := Canvas.TextWidth(S+'WWW');
                    if W > Integer(CW.Objects[X]) Then CW.Objects[X] := TObject(W);
                  End;
              RS.Next;
            End;
          RS.First;
          RS.Bookmark := BK;
          For X := 0 To Columns.Count-1 Do Columns.Items[X].Width := Integer(CW.Objects[X]);
        Finally
          CW.Free;
          Enabled := True;
          DataSource.DataSet.EnableControls;
        End;
      End;
End;

//******************************************************************************
Procedure TKADaoDBGrid.OnSelectIndex(Sender: TObject);
Var
  NewIndex : String;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
    Begin
      if TKaDaoTable(DataSource.Dataset).TableType=dbOpenTable Then
         Begin
           NewIndex:=TKaDaoTable(DataSource.Dataset).IndexName;
           Application.CreateForm(TKADaoSelectIndex, KADaoSelectIndex);
           if KADaoSelectIndex.Execute(TKaDaoTable(DataSource.Dataset), NewIndex) Then
              Begin
              End;
           KADaoSelectIndex.Free;
           TKaDaoTable(DataSource.Dataset).IndexName := NewIndex;
         End;
    End;
End;

Procedure TKADaoDBGrid.OnSortBy(Sender: TObject);
Var
 DT : TKADaoTable;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
      DT := TKaDaoTable(DataSource.Dataset);
      Application.CreateForm(TKADaoSortByDialog,KADaoSortByDialog);
      if KADaoSortByDialog.Execute(DT.SortFieldNames,DT.SortedBy,DT.UseBrackets) Then
       Begin
         DT.Sort;
       End;
      KADaoSortByDialog.Free;
     End;
End;

Procedure TKADaoDBGrid.OnFilterBy(Sender: TObject);
Var
  OldFilter   : String;
  OldFiltered : Boolean;
  NewFilter   : String;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
      Application.CreateForm(TKADaoFilterBy, KADaoFilterBy);
      NewFilter:=TKaDaoTable(DataSource.Dataset).Filter;
      if KADaoFilterBy.Execute(TKaDaoTable(DataSource.Dataset), NewFilter, F_LastFilter) Then
         Begin
           if NewFilter  <> '' Then
              Begin
               OldFilter   := TKaDaoTable(DataSource.Dataset).Filter;
               OldFiltered := TKaDaoTable(DataSource.Dataset).Filtered;
               Try
                 TKaDaoTable(DataSource.Dataset).Filter   := NewFilter;
                 TKaDaoTable(DataSource.Dataset).Filtered := True;
               Except
                 TKaDaoTable(DataSource.Dataset).Filter   := OldFilter;
                 TKaDaoTable(DataSource.Dataset).Filtered := OldFiltered;
               End;
              End
           Else
              Begin
               TKaDaoTable(DataSource.Dataset).Filtered := False;
              End;
         End;
      KADaoFilterBy.Free;
     End;
End;

Procedure TKADaoDBGrid.OnFind(Sender: TObject);
Var
 BK : TBookmarkStr;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
          BK := TKaDaoTable(DataSource.Dataset).Bookmark;
       Application.CreateForm(TKADaoFindSeekDialog, KADaoFindSeekDialog);
       if KADaoFindSeekDialog.Execute(TKaDaoTable(DataSource.DataSet), False) Then
          Begin
          End
       Else
          Begin
           if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
              TKaDaoTable(DataSource.Dataset).Bookmark:=BK;
          End;
       KADaoFindSeekDialog.Free;
     End;
End;

Procedure TKADaoDBGrid.OnSeek(Sender: TObject);
Var
 BK : TBookmarkStr;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (TKaDaoTable(DataSource.Dataset).IndexFieldCount > 0)  Then
     Begin
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
          BK := TKaDaoTable(DataSource.Dataset).Bookmark;
       Application.CreateForm(TKADaoFindSeekDialog, KADaoFindSeekDialog);
       if KADaoFindSeekDialog.Execute(TKaDaoTable(DataSource.DataSet), True) Then
          Begin
          End
       Else
          Begin
           if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
              TKaDaoTable(DataSource.Dataset).Bookmark:=BK;
          End;
       KADaoFindSeekDialog.Free;
     End;
End;

Procedure TKADaoDBGrid.OnQuickFind(Sender: TObject);
Var
 BK : TBookmarkStr;
 FN : String;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       FN := Self.SelectedField.FieldName;
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
          BK := TKaDaoTable(DataSource.Dataset).Bookmark;
       Application.CreateForm(TQFS, QFS);
       if QFS.Execute(TKaDaoTable(DataSource.DataSet), False, FN) Then
          Begin
          End
       Else
          Begin
           if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
              TKaDaoTable(DataSource.Dataset).Bookmark:=BK;
          End;
       QFS.Free;
     End;
End;

Procedure TKADaoDBGrid.OnQuickSeek(Sender: TObject);
Var
 BK : TBookmarkStr;
 FN : String;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (TKaDaoTable(DataSource.Dataset).IndexFieldCount > 0)  Then
     Begin
       FN := Self.SelectedField.FieldName;
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
          BK := TKaDaoTable(DataSource.Dataset).Bookmark;
       Application.CreateForm(TQFS, QFS);
       if QFS.Execute(TKaDaoTable(DataSource.DataSet), True, FN) Then
          Begin
          End
       Else
          Begin
           if TKaDaoTable(DataSource.Dataset).Bookmarkable Then
              TKaDaoTable(DataSource.Dataset).Bookmark:=BK;
          End;
       QFS.Free;
     End;
End;

Procedure TKADaoDBGrid.OnAutoSize(Sender: TObject);
Begin
 F_AutoSize := NOT F_AutoSize;
 F_Set_AutoSize(F_AutoSize);
 F_PopupMenu.Items[13].Checked := F_AutoSize;
End;

Procedure TKADaoDBGrid.OnShowMemos(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       F_PopupMenu.Items[10].Checked := Not F_PopupMenu.Items[10].Checked;
       TKaDaoTable(DataSource.Dataset).CacheMemos := F_PopupMenu.Items[10].Checked;
       if F_AutoSize Then SizeColumns;
     End;
End;

Procedure TKADaoDBGrid.OnShowGUIDs(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       F_PopupMenu.Items[11].Checked := Not F_PopupMenu.Items[11].Checked;
       TKaDaoTable(DataSource.Dataset).ShowGUID := F_PopupMenu.Items[11].Checked;
       if F_AutoSize Then SizeColumns;
     End;
End;

Procedure TKADaoDBGrid.OnFastLookup(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
      Begin
       F_PopupMenu.Items[9].Checked := Not F_PopupMenu.Items[9].Checked;
       TKaDaoTable(DataSource.Dataset).CacheLookups := F_PopupMenu.Items[9].Checked;
      End;
End;

Procedure TKADaoDBGrid.OnFrameIndexFields(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       F_PopupMenu.Items[12].Checked := Not F_PopupMenu.Items[12].Checked;
       F_FrameIndex := F_PopupMenu.Items[12].Checked;
       Self.Refresh;
     End;
End;

Procedure TKADaoDBGrid.OnChooseVisibleFields(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     Begin
       {$IFDEF D4UP}
       Application.CreateForm(TCVC, CVC);
       if CVC.Execute(Self) Then
          Begin
          End;
       CVC.Free;
       {$ENDIF}
       {$IFNDEF D4UP}
       Application.CreateForm(TCVF, CVF);
       if CVF.Execute(DataSource.Dataset) Then
          Begin
          End;
       CVF.Free;
       {$ENDIF}
       Self.Refresh;
     End;
End;

Procedure TKADaoDBGrid.OnSaveAs(Sender: TObject);
Var
 Visible_   : TStringList;
 Invisible_ : TStringList;
 ExType     : TExportMethod;
 Table      : TKaDaoTable;
 FC         : TColumn;
 FF         : TField;
 X          : Integer;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (NOT DataSource.DataSet.IsEmpty) Then
     Begin
       Application.CreateForm(TSAS, SAS);
       Table              := TKaDaoTable(DataSource.DataSet);
       Visible_           := TStringList.Create;
       Invisible_         := TStringList.Create;
       ExType             := Table.ExportMethod;
       Table.ExportMethod := VisibleFields;
       Table.DisableControls;
       For X := 0 To Table.FieldCount-1 do
           Begin
            if Table.Fields[X].Visible Then
               Begin
                 FC := FindColumn(Table.Fields[X].FieldName);
                 {$IFDEF D4UP}
                 IF FC = Nil Then Visible_.Add(Table.Fields[X].FieldName)
                    Else
                      if NOT FC.Visible Then Visible_.Add(Table.Fields[X].FieldName);
                 {$ELSE}
                 IF FC = Nil Then Visible_.Add(Table.Fields[X].FieldName);
                 {$ENDIF}
               End
            Else
               Begin
                 {$IFDEF D4UP}
                 FC := FindColumn(Table.Fields[X].FieldName);
                 IF FC <> Nil Then
                    if FC.Visible Then Invisible_.Add(Table.Fields[X].FieldName);
                 {$ENDIF}
               End;
           End;
       For X := 0 To Visible_.Count-1 do
             Begin
               FF := Table.FindField(Visible_.Strings[X]);
               if FF <> Nil Then FF.Visible := False;
             End;
       For X := 0 To Invisible_.Count-1 do
             Begin
               FF := Table.FindField(Invisible_.Strings[X]);
               if FF <> Nil Then FF.Visible := True;
             End;
       Try
         SAS.ExportDir := F_ExportDir;
         if SAS.ExecuteFromGrid(DataSource.DataSet, Self) Then
            Begin
            End;
       Finally
         For X := 0 To Invisible_.Count-1 do
             Begin
               FF := Table.FindField(Invisible_.Strings[X]);
               if FF <> Nil Then FF.Visible := False;
             End;
         For X := 0 To Visible_.Count-1 do
             Begin
               FF := Table.FindField(Visible_.Strings[X]);
               if FF <> Nil Then FF.Visible := True;
             End;
         Table.EnableControls;
         Table.ExportMethod := ExType;
         Invisible_.Free;
         Visible_.Free;
         SAS.Free;
       End;
     End;
End;

Procedure TKADaoDBGrid.OnDeSelectAll(Sender: TObject);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (TKaDaoTable(DataSource.Dataset).TableType <> DaoApi.dbOpenForwardOnly)
 And (NOT DataSource.DataSet.IsEmpty) Then
     Begin
       if Not (dgMultiSelect In Options) Then
          Begin
            GlobalOptions := [];
            Exit;
          End
       Else                                                
          Begin
           SelectedRows.Clear;
           Refresh;
           if GlobalOptions = [dgMultiSelect] Then
              Begin
                Options       := Options-[dgMultiSelect];
                GlobalOptions := [];
              End;
          End;
     End;
End;
Procedure TKADaoDBGrid.OnSelectAll(Sender: TObject);
Var
  BK : TBookmarkStr;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (TKaDaoTable(DataSource.Dataset).TableType <> DaoApi.dbOpenForwardOnly)
 And (NOT DataSource.DataSet.IsEmpty) Then
     Begin
       if Not (dgMultiSelect In Options) Then
          Begin
            Options       := Options+[dgMultiSelect];
            GlobalOptions := [dgMultiSelect];
          End;
       if TKaDaoTable(DataSource.Dataset).Bookmarkable Then BK := DataSource.DataSet.Bookmark;
       SelectedRows.Clear;
       Try
        Enabled:=False;
        DataSource.DataSet.DisableControls;
        DataSource.DataSet.First;
        While NOT DataSource.DataSet.EOF do
         Begin
          SelectedRows.CurrentRowSelected:=True;
          DataSource.DataSet.Next;
         End;
        Try
         if TKaDaoTable(DataSource.Dataset).Bookmarkable Then DataSource.DataSet.Bookmark := BK;
        Except
        End;
       Finally
        DataSource.DataSet.EnableControls;
        Enabled:=True;
       End;
     End;
End;

Function TKADaoDBGrid.StrToHex(S:String) : String;
Var
 L,X : Integer;
Begin
 Result := '';
 L := Length(S);
 if L=0 Then Exit;
 For X := 1 to L Do
     Begin
       Result:=Result+UpperCase(Format('%.2x',[Ord(S[X])]));
     End;
End;

Function TKADaoDBGrid.HexToStr(S:String) : String;
Var
   X,Y,L  : Integer;
   TC     : Array[1..2] of Byte;
Begin
 Result := '';
 L:=Length(S);
 if L=0 Then Exit;
 if L Mod 2 > 0 Then Exit;
 SetLength(Result,L Div 2);
 X:=1;
 Y:=1;
 Repeat
   TC[1]:=Ord(S[X]);
   TC[2]:=Ord(S[X+1]);
   if TC[1] < 65 Then Dec(TC[1],48) Else Dec(TC[1],55);
   if TC[2] < 65 Then Dec(TC[2],48) Else Dec(TC[2],55);
   Result[Y]:=Chr((TC[1] SHL 4)+TC[2]);
   Inc(X,2);
   Inc(Y);
 Until X > L;
End;


Function TKADaoDBGrid.GetFieldAsHEXString(Field:TField):String;
Var
 Date_     : TDate;
 Time_     : TTime;
 DateTime_ : TDateTime;
 DT        : TTimeStamp;
Begin
  Result := '';
  if Field.DataType=ftDate Then
     Begin
      Date_   := Field.AsDateTime;
      DT      := DateTimeToTimeStamp(Date_);
      DT.Time := 0;
      Result  := Format('%d %d',[DT.Date,DT.Time]);
     End
  Else
  if Field.DataType=ftTime Then
     Begin
       Time_   := Field.AsDateTime;
       DT      := DateTimeToTimeStamp(Time_);
       DT.Date := 0;
       Result  := Format('%d %d',[DT.Date,DT.Time]);
     End
  Else
  if Field.DataType=ftDateTime Then
     Begin
      DateTime_ := Field.AsDateTime;
      DT := DateTimeToTimeStamp(DateTime_);
      Result  := Format('%d %d',[DT.Date,DT.Time]);
     End
  Else
  if Field.DataType=ftBlob Then
     Begin
       Result := StrToHex(Field.AsString);
     End
  Else
  if Field.DataType=ftMemo Then
     Begin
       Result := StrToHex(Field.AsString);
     End
  Else
  if Field.DataType=ftString Then
     Begin
       Result := StrToHex(Field.AsString);
     End
  Else
     Begin
      Result := Field.AsString;
     End;
End;

Function TKADaoDBGrid.GetFieldAsString(Field:TField):String;
Var
 Date_     : TDate;
 Time_     : TTime;
 DateTime_ : TDateTime;
 S         : String;
 P         : Integer;
 HasQuote  : Boolean;
Begin
  Result := '';
  if Field.DataType=ftDate Then
     Begin
      Date_  := Field.AsDateTime;
      if F_AccessClpFormat Then
         Result := FormatDateTime('mm"/"dd"/"yyyy',Date_)
      Else
         Result := DateToStr(Date_);
     End
  Else
  if Field.DataType=ftTime Then
     Begin
       Time_ := Field.AsDateTime;
       if F_AccessClpFormat Then
          Result := FormatDateTime('hh":"nn":"ss',Time_)
       Else
          Result := TimeToStr(Time_);
     End
  Else
  if Field.DataType=ftDateTime Then
     Begin
      DateTime_ := Field.AsDateTime;
      if F_AccessClpFormat Then
         Result := FormatDateTime('mm"/"dd"/"yyyy hh":"nn":"ss',DateTime_)
      Else
         Result := DateTimeToStr(DateTime_);
     End
  Else
  if Field.DataType=ftBlob Then
     Begin
       //**************************************** Access Style
       // Result := StrPas(PChar(Field.AsString));
       //****************************************
       Result := '(Blob)';
     End
  Else
  if (Field.DataType=ftMemo) Or (Field.DataType=ftString) Then
     Begin
       S := Field.AsString;
       if F_AccessClpFormat Then
          Begin
           if (Pos(#13,S) > 0) Or (Pos(#10,S) > 0) Then
              Begin
              P := Pos('"',S);
              While P > 0 Do
                Begin
                 Result := Result+System.Copy(S,1,P)+'"';
                 System.Delete(S,1,P);
                 P := Pos('"',S);
                End;
              Result := '"'+Result+S+'"';
              End
           Else
              Begin
              HasQuote := False;
              P := Pos('"',S);
              While P > 0 Do
                Begin
                  HasQuote := True;
                  Result := Result+System.Copy(S,1,P)+'"';
                  System.Delete(S,1,P);
                  P := Pos('"',S);
                End;
              if HasQuote Then
                Result := '"'+Result+S+'"'
              Else
                Result := Result+S;
              End;
          End
       Else
          Begin
            P := Pos(#13,S);
            While P > 0 Do
              Begin
               S[P] := ' ';
               P    := Pos(#13,S);
              End;
            P := Pos(#10,S);
            While P > 0 Do
              Begin
               System.Delete(S,P,1);
               P    := Pos(#10,S);
              End;
            Result := S;
          End;
     End
  Else
     Begin
      Result := Field.AsString;
     End;
End;

Function TKADaoDBGrid.ConvertToWideString(A: AnsiString): WideString;
Var
  Buf      : Array[0..6] of Char;
  CodePage : Integer;
begin
  SetLength(Result, Length(A));
  GetLocaleInfo(GetUserDefaultLCID, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  CodePage := StrToIntDef(Buf, GetACP);
  MultiByteToWideChar(CodePage, 0, PChar(A), Length(A), PWideChar(Result), Length(A) * 2);
end;

Procedure TKADaoDBGrid.CopyToClipboard;
Var
  BK      : TBookmarkStr;
  X       : Integer;
  Y       : Integer;
  L       : Integer;
  HeadText: String;
  FTText  : String;
  CLPText : String;
  HEXText : String;
  Clp     : TClipboard;
  Locale  : LCID;
  Mem     : HGLOBAL;
  HEXMem  : HGLOBAL;
  UTMem   : HGLOBAL;
  PMem    : Pointer;
  WS      : WideString;
  VisRow  : Integer;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (NOT DataSource.DataSet.IsEmpty) Then
     Begin
       VisRow := Row;
       if dgTitles in Options Then Dec(VisRow);
       DataSource.DataSet.MoveBy(-VisRow);
       BK     := DataSource.DataSet.Bookmark;
       Locale := GetThreadLocale;
       SetThreadLocale(LOCALE_USER_DEFAULT);
       Clp := TClipboard.Create;
       Try
        Enabled:=False;
        DataSource.DataSet.DisableControls;
        CLPText  := '';
        HeadText := '';
        FTText   := '';
        //***************************************************** Copy Field Names
        For X := 0 To Columns.Count-1 do
            Begin
              if Columns.Items[X].Field.FieldKind=fkData Then
                 Begin
                  HeadText := HeadText + Columns.Items[X].Field.FieldName;
                  if X < Columns.Count-1 Then HeadText := HeadText+#9;
                 End;
            End;
        L := Length(HeadText);
        if HeadText[L]=#9 Then System.Delete(HeadText,L,1);
        HeadText := HeadText+#13#10;
        //**************************************************
        //  If this is enabled you cannot paste in MS Access
        //  However Access exports Field Names
        //**************************************************
        //  ClpText  := HeadText;
        //**************************************************
        HEXText  := HeadText;
        //************************************************* Copy Field DataTypes
        For X := 0 To Columns.Count-1 do
            Begin
              if Columns.Items[X].Field.FieldKind=fkData Then
                 Begin
                  FTText := FTText + IntToStr(Integer(Columns.Items[X].Field.DataType));
                  if X < Columns.Count-1 Then FTText := FTText+#9;
                 End;
            End;
        L := Length(FTText);
        if FTText[L]=#9 Then System.Delete(FTText,L,1);
        FTText  := FTText+#13#10;
        HEXText := HEXText+FTText;
        //***************************************************** Copy Actual Data
        if SelectedRows.Count > 0 Then
           Begin
             For Y := 0 To SelectedRows.Count-1 Do
               Begin
                DataSource.DataSet.Bookmark:=SelectedRows.Items[Y];
                For X := 0 To Columns.Count-1 do
                  Begin
                    if Columns.Items[X].Field.FieldKind=fkData Then
                       Begin
                        CLPText := CLPText + GetFieldAsString(Columns.Items[X].Field);
                        if X < Columns.Count-1 Then CLPText := CLPText+#9;

                        HEXText := HEXText + GetFieldAsHexString(Columns.Items[X].Field);
                        if X < Columns.Count-1 Then HEXText := HEXText+#9;
                       End;
                  End;
                L := Length(CLPText);
                if CLPText[L]=#9 Then System.Delete(CLPText,L,1);
                CLPText := CLPText+#13#10;

                L := Length(HEXText);
                if HEXText[L]=#9 Then System.Delete(HEXText,L,1);
                HEXText := HEXText+#13#10; 
               End;
           End
        Else
           Begin
             For X := 0 To Columns.Count-1 do
                 Begin
                   if Columns.Items[X].Field.FieldKind=fkData Then
                      Begin
                       CLPText := CLPText + GetFieldAsString(Columns.Items[X].Field);
                       if X < Columns.Count-1 Then CLPText := CLPText+#9;

                       HEXText := HEXText + GetFieldAsHexString(Columns.Items[X].Field);
                       if X < Columns.Count-1 Then HEXText := HEXText+#9;
                      End;
                 End;
             L := Length(CLPText);
             if CLPText[L]=#9 Then System.Delete(CLPText,L,1);
             CLPText := CLPText+#13#10;
                                        
             L := Length(HEXText);
             if HEXText[L]=#9 Then System.Delete(HEXText,L,1);
             HEXText := HEXText+#13#10;
           End;

        //**********************************************************************
        Clp.Open;
        Clp.Clear;
        //************************* CF_TEXT
        L := Length(CLPText);
        Mem  := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, L);
        PMem := GlobalLock(Mem);
        Move(CLPText[1],PMem^,L);
        GlobalUnlock(Mem);
        Clp.SetAsHandle(CF_TEXT,Mem);

        //************************* CF_UNICODETEXT
        WS := ConvertToWideString(CLPText);
        L  := (Length(WS)-1)*2;
        UTMem  := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, L);
        PMem := GlobalLock(UTMem);
        Move(WS[1],PMem^,L);
        GlobalUnlock(UTMem);
        Clp.SetAsHandle(CF_UNICODETEXT,UTMem);

        //************************* CF_HEX_RECORDSET
        L := Length(HEXText);
        HEXMem  := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, L);
        PMem := GlobalLock(HEXMem);
        Move(HEXText[1],PMem^,L);
        GlobalUnlock(HEXMem);
        Clp.SetAsHandle(RegisterClipboardFormat('HEX Recordset'),HEXMem);
        Clp.Close;
        //**********************************************************************
        Try
         DataSource.DataSet.Bookmark := BK;
         DataSource.DataSet.MoveBy(VisRow);
        Except
        End;
       Finally
        DataSource.DataSet.EnableControls;
        Enabled:=True;
        SetThreadLocale(Locale);
        Clp.Free;
       End;
     End;
End;

Function TKADaoDBGrid.PropertyExists(PropObject:OleVariant;PropertyName:String):Boolean;
Var
  X : Integer;
Begin
  Result := False;
  For X := 0 to PropObject.Count-1 do
      Begin
        if AnsiCompareText(PropObject.Item[X].Name,PropertyName)=0 Then
           Begin
             Result := True;
             Exit;
           End;
      End;
End;

Function TKADaoDBGrid.GetCaption(FieldName : String):String;
Var
  RS : OleVariant;
Begin
  Result := '';
  RS := OleVariant(TKaDaoTable(DataSource.Dataset).CoreRecordset);
  if PropertyExists(RS.Fields.Item[FieldName].Properties,'Caption') Then
     Result := RS.Fields.Item[FieldName].Properties['Caption'];
End;

Function  TKADaoDBGrid.FindColumn(FieldName:String):TColumn;
Var
 X : Integer;
Begin
 Result := Nil;
 For X := 0 To Columns.Count-1 do
     Begin
       if Columns.Items[X].Field.FieldName=FieldName Then
          Begin
            Result := Columns.Items[X];
          End;
     End;
End;

Function TKADaoDBGrid.FindField(FieldName:String; MSAccess:Boolean):TField;
Var
  X    : Integer;
  CN   : String;
Begin
  Result := Nil;
  if NOT F_FindFieldsOnPaste Then Exit;
  if MSAccess Then
     Begin
      For X := 0 To Columns.Count-1 do
      Begin
        CN := GetCaption(Columns.Items[X].Field.FieldName);
        if AnsiCompareText(CN,FieldName)=0 Then
           Begin
             Result := Columns.Items[X].Field;
             Exit;
           End;
      End;
     End;
  For X := 0 To Columns.Count-1 do
      Begin
        if AnsiCompareText(Columns.Items[X].Field.FieldName,FieldName)=0 Then
           Begin
             Result := Columns.Items[X].Field;
             Exit;
           End;
      End;
End;

Procedure TKADaoDBGrid.ParseHexLine(RD,FN: TStringList; Line:String);
Var
 S   : String;
 Rec : String;
 P   : Integer;
 BR  : Integer;
 FT  : TFieldType;
Begin
 BR := 0;
 S  := Line;
 Repeat
  P := Pos(#9,S);
  if P > 0 Then
   Begin
    Rec := System.Copy(S,1,P-1);
    System.Delete(S,1,P);
    if BR < RD.Count Then
       Begin
        FT := TFieldType(Integer(FN.Objects[BR]));
        if (FT = ftString) or (FT=ftMemo) or (FT=ftBlob) Then Rec := HexToStr(Rec);
        RD.Strings[BR]:=Rec;
       End;
    Inc(BR);
   End;
  Until P=0;
  if BR < RD.Count Then
     Begin
       FT := TFieldType(Integer(FN.Objects[BR]));
       if (FT = ftString) or (FT=ftMemo) or (FT=ftBlob) Then S := HexToStr(S);
       RD.Strings[BR]:=S;
     End;
End;


Function TKADaoDBGrid.ConvertToTS(S:String):TTimeStamp;
Var
 DS : String;
 TS : String;
 D  : Integer;
 T  : Integer;
 P  : Integer;
 Err: Integer;
Begin
 Result.Date := 0;
 Result.Time := 0;
 P := Pos(' ',S);
 if P =0 Then Exit;
 DS := System.Copy(S,1,P-1);
 System.Delete(S,1,P);
 TS := S;
 Val(DS,D,Err);
 if Err <> 0 Then Exit;
 Val(TS,T,Err);
 if Err <> 0 Then Exit;
 Result.Date := D;
 Result.Time := T;
End;

Procedure TKADaoDBGrid.PasteNativeFormat(CFFormat : Word; Clp:TClipboard);
Var
 Records     : TStringList;
 FieldNames  : TStringList;
 RecordData  : TStringList;
 Mem         : HGLOBAL;
 PMem        : Pointer;
 Sz          : Integer;
 S           : String;
 FN          : String;
 FT          : Integer;
 P           : Integer;
 BR          : Integer;
 FF          : TField;
 SourceFT    : TFieldType;
 X           : Integer;
 Y           : Integer;
 Date_       : TDate;
 Time_       : TTime;
 DateTime_   : TDateTime;
 DT          : TTimeStamp;
 NumErrors   : Integer;
 PasteWarning:String;
Begin
  Records     := TStringList.Create;
  FieldNames  := TStringList.Create;
  RecordData  := TStringList.Create;
  Enabled     := False;
  NumErrors   := 0;
  Try
    Clp.Open;
    Mem := Clp.GetAsHandle(CFFormat);
    If Mem <> 0 Then
       Begin
        Sz   := GlobalSize(Mem);
        PMem := GlobalLock(Mem);
        SetLength(S,Sz);
        Move(PMem^,S[1],Sz);
        GlobalUnlock(Mem);
        Records.Text:=S;
        S:='';
       End;
    Clp.Close;
    //**************************************************************************
    if Records.Count > 2 Then
       Begin
         //**************************************** Get Field Names
         S := Records.Strings[0];
         Repeat
           P := Pos(#9,S);
           if P > 0 Then
              Begin
                FN := System.Copy(S,1,P-1);
                System.Delete(S,1,P);
                FieldNames.Add(FN);
              End;
         Until P=0;
         FieldNames.Add(S);
         //**************************************** Get Field Types
         S  := Records.Strings[1];
         BR := 0;
         Repeat
           P := Pos(#9,S);
           if P > 0 Then
              Begin
                FN := System.Copy(S,1,P-1);
                FT := StrToInt(FN);
                System.Delete(S,1,P);
                if BR < FieldNames.Count Then FieldNames.Objects[BR]:=TObject(FT);
                Inc(BR);
              End;
         Until P=0;
         FT := StrToInt(S);
         if BR < FieldNames.Count Then FieldNames.Objects[BR]:=TObject(FT);
         //**************************************** Removes first two rows
         Records.Delete(0);
         Records.Delete(0);
         //**************************************** Actualy Add data
         RecordData.Assign(FieldNames);
         PasteWarning := 'You are about to paste '+IntToStr(Records.Count)+' records!'+#13#10+
                         'There will be no undo for this operation!'+#13#10+
                         'Are You sure?';
         If Application.MessageBox(PChar(PasteWarning),'Warning',MB_IconExclamation or MB_YesNoCancel)=ID_Yes
         Then
         if Records.Count > 0 Then
            Begin
              TKaDaoTable(DataSource.Dataset).BatchMode := True;
              For X := 0 To FieldNames.Count-1 do
                  Begin
                    FF := FindField(FieldNames.Strings[X], False);
                    // ********************  Field not found by Name
                    // ********************  so it is by ordinal
                    if FF=Nil Then
                       Begin
                         if X < Columns.Count Then FF := Columns.Items[X].Field;
                       End;
                    //********************* Only Data Fields can be pasted
                    if (FF <> Nil) And (FF.FieldKind <> fkData) Then FF := Nil;
                    if (FF <> Nil) And (FF.ReadOnly) Then FF := Nil;
                    //********************* Duplication is NOT allowed
                    if FF <> Nil Then
                       Begin
                         if RecordData.IndexOfObject(FF)=-1 Then
                            RecordData.Objects[X] := FF
                         Else
                            RecordData.Objects[X] := Nil;
                       End
                    Else
                       Begin
                         RecordData.Objects[X] := FF;
                       End;
                   End;
              For Y := 0 To Records.Count-1 do
                  Begin
                    ParseHexLine(RecordData,FieldNames, Records.Strings[Y]);
                    Try
                     DataSource.DataSet.Append;
                     For X := 0 To RecordData.Count-1 do
                        Begin
                          FF := TField(RecordData.Objects[X]);
                          if FF <> Nil Then
                             Begin
                               SourceFT := TFieldType(Integer(FieldNames.Objects[X]));
                               if FF.DataType=ftDate Then
                                  Begin
                                    if (SourceFT=ftDate) or (SourceFT=ftTime) or (SourceFT=ftDateTime) Then
                                       Begin
                                         DT    := ConvertToTS(RecordData.Strings[X]);
                                         Date_ := TimeStampToDateTime(DT);
                                         FF.AsDateTime:=Date_;
                                       End
                                    Else
                                       Begin
                                         FF.AsString:=RecordData.Strings[X];
                                       End;
                                  End
                               Else
                               if FF.DataType=ftTime Then
                                  Begin
                                    if (SourceFT=ftDate) or (SourceFT=ftTime) or (SourceFT=ftDateTime) Then
                                       Begin
                                         DT    := ConvertToTS(RecordData.Strings[X]);
                                         Time_ := TimeStampToDateTime(DT);
                                         FF.AsDateTime:=Time_;
                                       End
                                    Else
                                       Begin
                                         FF.AsString:=RecordData.Strings[X];
                                       End;
                                  End
                               Else
                               if FF.DataType=ftDateTime Then
                                  Begin
                                    if (SourceFT=ftDate) or (SourceFT=ftTime) or (SourceFT=ftDateTime) Then
                                       Begin
                                         DT    := ConvertToTS(RecordData.Strings[X]);
                                         DateTime_ := TimeStampToDateTime(DT);
                                         FF.AsDateTime:=DateTime_;
                                       End
                                    Else
                                       Begin
                                         FF.AsString:=RecordData.Strings[X];
                                       End;
                                  End
                               Else
                                  Begin
                                    FF.AsString := RecordData.Strings[X];
                                  End;
                             End;
                        End;
                      DataSource.DataSet.Post;
                    Except
                      DataSource.DataSet.Cancel;
                      Inc(NumErrors);
                    End;
                  End;
            End;
       End;
  Finally
   RecordData.Free;
   FieldNames.Free;
   Records.Free;
   if TKaDaoTable(DataSource.Dataset).BatchMode = True Then
      TKaDaoTable(DataSource.Dataset).BatchMode := False;
   Enabled := True;
  End;
  if NumErrors > 0 Then
    Begin
      ShowMessage(IntToStr(NumErrors) + ' records was not pasted due to an error!');
    End;
End;

Function  TKADaoDBGrid.IsAccessDataInClipboard(Clp:TClipboard):Boolean;
Var
 CFFormat : Integer;
Begin
 Result := False;
 CFFormat := RegisterClipboardFormat('Data Records v8');  //****** Access 97
 If Clp.HasFormat(CFFormat) Then Result := True;
 CFFormat := RegisterClipboardFormat('Data Records v9');  //****** Access 2000
 If Clp.HasFormat(CFFormat) Then Result := True;
 CFFormat := RegisterClipboardFormat('Data Records v10'); //****** Access 2002?
 If Clp.HasFormat(CFFormat) Then Result := True;
End;


Function TKADaoDBGrid.CountQuotes (S : String) : Integer;
var
  P : Integer;
Begin
    Result := 0;
    P := Pos('"', S);
    While P > 0 do
      Begin
        Inc (Result);
        System.Delete(S,1,P);
        P := Pos('"', S);
      End;
End;

Function TKADaoDBGrid.CountTabs (S : String) : Integer;
var
  P : Integer;
Begin
    Result := 0;
    P := Pos(#9, S);
    While P > 0 do
      Begin
        Inc (Result);
        System.Delete(S,1,P);
        P := Pos(#9, S);
      End;
End;

Function TKADaoDBGrid.ReplaceDoubleQuotes (Var S : String) : Integer;
Var
  P : Integer;
Begin
    Result := 0;
    P := Pos('""', S);
    While P > 0 do
      Begin
        Inc (Result);
        System.Delete(S,P,1);
        P := Pos('""', S);
      end;
End;

Function TKADaoDBGrid.ReplaceTabQuotes (Var S : String) : Integer;
Var
  P : Integer;
  L : Integer;
Begin
    Result := 0;
    P := Pos(#9'"', S);
    While P > 0 do
      Begin 
        Inc (Result);
        System.Delete(S,P+1,1);
        S[P]:=#0;
        P := Pos(#9'"', S);
      end;

    P := Pos('"'#9, S);
    While P > 0 do
      Begin
        Inc (Result);
        System.Delete(S,P,1);
        S[P]:=#0;
        P := Pos('"'#9, S);
      end;

    P := Pos(#0, S);
    While P > 0 do
      Begin
        S[P]:=#9;
        P := Pos(#0, S);
      end;

    L := Length(S);
    if S[L]='"' Then System.Delete(S,L,1);
End;

Procedure TKADaoDBGrid.PreprocessRecords(Records : TStringList);
Var
 X     : Integer;
 Q     : Integer;
 Equal : Boolean;
 S     : String;
 Tmp   : TStringList;
 MSS   : Boolean;
Begin
  Tmp  := TStringList.Create;
  Tmp.Assign(Records);
  MSS := True;
  Repeat
    Equal := True;
    For X := 0 to Records.Count-1 do
        Begin
          Q := CountQuotes(Records.Strings[X]);
          if (Q > 0) And Odd(Q) Then
             Begin
               Try
                 Records.Strings[X] := Records.Strings[X]+#13#10+Records.Strings[X+1];
                 Records.Delete(X+1);
                 Equal := False;
                 System.Break;
               Except
                 MSS   := False;
                 Equal := True;
                 System.Break;
               End;
             End;
        End;
  Until Equal;
  if Not MSS Then Records.Assign(Tmp);
  Tmp.Free;

  For X := 0 to Records.Count-1 do
      Begin
       S := Records.Strings[X];
       ReplaceTabQuotes(S);
       ReplaceDoubleQuotes(S);
       Records.Strings[X] := S;
      End;
End;

Procedure TKADaoDBGrid.ParseTextLine(RD : TStringList; Line:String);
Var
 S   : String;
 Rec : String;
 P   : Integer;
Begin
 RD.Clear;
 S  := Line;
 Repeat
  P := Pos(#9,S);
  if P > 0 Then
   Begin
    Rec := System.Copy(S,1,P-1);
    System.Delete(S,1,P);
    RD.Add(Rec);
   End;
  Until P=0;
  RD.Add(S);
End;

Function TKADaoDBGrid.GetTextDateTime(S:String):TDateTime;
Var
  DT : TKADaoDateTime;
Begin
  Result := 0;
  DT := TKADaoDateTime.Create(Self);
  DT.AccessShorting := False;
  Try
   DT.DateTimeString:=S;
   Result := DT.DateTime;
  Except
   DT.AccessShorting := True;
   Try
    DT.DateTimeString:=S;
    Result := DT.DateTime;
   Except
   End;
  End;
  DT.Free;
End;

Procedure TKADaoDBGrid.PasteTextFormat(CFFormat : Word; Clp:TClipboard);
Var
 MSAccess    : Boolean;
 Records     : TStringList;
 FieldNames  : TStringList;
 RecordData  : TStringList;
 Mem         : HGLOBAL;
 PMem        : Pointer;
 Sz          : Integer;
 S           : String;
 FN          : String;
 P           : Integer;
 X           : Integer;
 Y           : Integer;
 NumFields   : Integer;
 FF          : TField;
 NumErrors   : Integer;
 DT          : TDateTime;
 PasteWarning: String;
Begin
 MSAccess    := IsAccessDataInClipboard(Clp);
 Records     := TStringList.Create;
 FieldNames  := TStringList.Create;
 RecordData  := TStringList.Create;
 Enabled     := False;
 NumErrors   := 0;
 Try
    Clp.Open;
    Mem := Clp.GetAsHandle(CFFormat);
    If Mem <> 0 Then
       Begin
        Sz   := GlobalSize(Mem);
        PMem := GlobalLock(Mem);
        SetLength(S,Sz);
        Move(PMem^,S[1],Sz);
        GlobalUnlock(Mem);
        Records.Text:=S;
        S:='';
       End;
    Clp.Close;
    if Records.Count > 0 Then
       Begin
        if MSAccess Then
           Begin
             //**************************************** Get Field Names
             S := Records.Strings[0];
             Repeat
               P := Pos(#9,S);
               if P > 0 Then
                 Begin
                  FN := System.Copy(S,1,P-1);
                  System.Delete(S,1,P);
                  FieldNames.Add(FN);
                 End;
             Until P=0;
             FieldNames.Add(S);
             Records.Delete(0);
             For X := 0 To FieldNames.Count-1 Do
                 Begin
                   FF := FindField(FieldNames.Strings[X],True);
                   FieldNames.Objects[X]:= FF;
                 End;
           End;
        PreprocessRecords(Records);
        PasteWarning := 'You are about to paste '+IntToStr(Records.Count)+' records!'+#13#10+
                         'There will be no undo for this operation!'+#13#10+
                         'Are You sure?';
        If Application.MessageBox(PChar(PasteWarning),'Warning',MB_IconExclamation or MB_YesNoCancel)=ID_Yes
        Then
        if Records.Count > 0 Then
          Begin
           TKaDaoTable(DataSource.Dataset).BatchMode := True;
           For Y := 0 To Records.Count-1 do
            Begin
              Try
               DataSource.DataSet.Append;
               NumFields := CountTabs(Records.Strings[Y])+1;
               ParseTextLine(RecordData,Records.Strings[Y]);
               //***************************************************************
               if MSAccess Then
                  Begin
                    For X := 0 To FieldNames.Count-1 Do
                        Begin
                          FF := TField(FieldNames.Objects[X]);
                          if (FF = Nil) and (X < Columns.Count) Then
                             Begin
                               FF := Columns.Items[X].Field;
                               if FF.FieldKind <> fkData Then FF := Nil;
                               if FF.ReadOnly Then FF := Nil;
                             End;
                          if FF <> Nil Then
                             Begin
                               if (FF.DataType=ftDate)
                               or (FF.DataType=ftTime)
                               or (FF.DataType=ftDateTime) Then
                                  Begin
                                     DT := GetTextDateTime(RecordData.Strings[X]);
                                     if DT <> 0 Then FF.AsDateTime := DT;
                                  End
                               Else
                                  Begin
                                    FF.AsString := RecordData.Strings[X];
                                  End;
                             End;
                        End;
                  End
               Else
                  Begin
                    For X := 0 To NumFields-1 Do
                        Begin
                          if X < Columns.Count Then
                             Begin
                              FF := Columns.Items[X].Field;
                              if  (NOT FF.ReadOnly)
                              And (FF.FieldKind = fkData) Then
                                  Begin
                                    if (FF.DataType=ftDate)
                                    or (FF.DataType=ftTime)
                                    or (FF.DataType=ftDateTime) Then
                                       Begin
                                         DT := GetTextDateTime(RecordData.Strings[X]);
                                         if DT <> 0 Then FF.AsDateTime := DT;
                                       End
                                    Else
                                       Begin
                                         FF.AsString := RecordData.Strings[X];
                                       End;
                                  End;
                             End;
                        End;
                  End;
               DataSource.DataSet.Post;
              Except
               Inc(NumErrors);
               DataSource.DataSet.Cancel;
              End;
            End;
          End;                                                              
       End;
 Finally
   RecordData.Free;
   FieldNames.Free;
   Records.Free;
   if TKaDaoTable(DataSource.Dataset).BatchMode = True Then
      TKaDaoTable(DataSource.Dataset).BatchMode := False;
   Enabled := True;
 End;
 if NumErrors > 0 Then
    Begin
      ShowMessage(IntToStr(NumErrors) + ' records was not pasted due to an error!');
    End;
End;

Procedure TKADaoDBGrid.PasteFromClipboard;
Var
  Clp       : TClipboard;
  CFFormat  : Word;
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active)
 And (NOT ReadOnly) Then
  Begin
    Clp := TClipboard.Create;
    Try
     CFFormat := RegisterClipboardFormat('HEX Recordset');
     If Clp.HasFormat(CFFormat)  Then PasteNativeFormat(CFFormat, Clp)
        Else
          If Clp.HasFormat(CF_TEXT) Then PasteTextFormat(CF_TEXT, Clp);
    Finally
     Clp.Free;
    End;
  End;                       
End;

Procedure TKADaoDBGrid.OnCutToClp(Sender: TObject);
Var
  PasteWarning : String;
  SelCount     : Integer;
Begin
  If ReadOnly Then Exit;
  SelCount := SelectedRows.Count;
  if SelCount=0 Then SelCount := 1;
  PasteWarning := 'You are about to cut '+IntToStr(SelCount)+' records!'+#13#10+
                   'There will be no undo for this operation!'+#13#10+
                   'Are You sure?';
  If Application.MessageBox(PChar(PasteWarning),'Warning',MB_IconExclamation or MB_YesNoCancel)=ID_Yes Then
     Begin
      CopyToClipboard;
      OnDelete(Sender);
     End;
End;

Procedure TKADaoDBGrid.OnCopyToClp(Sender: TObject);
Begin
  CopyToClipboard;
End;

Procedure TKADaoDBGrid.OnPasteFromClp(Sender: TObject);
Begin
 PasteFromClipboard;
End;

Procedure TKADaoDBGrid.OnUseAccessClpFormat(Sender: TObject);
Begin
  F_AccessClpFormat := Not F_AccessClpFormat;
End;

Procedure TKADaoDBGrid.OnDelete(Sender: TObject);
Var
 Key   : Word;
 Shift : TShiftState;
Begin
 Key    := VK_DELETE;
 Shift  := [ssCtrl];
 KeyDown(Key, Shift);
End;


Procedure TKADaoDBGrid.SelectAll;
Begin
 OnSelectAll(Self);
End;

Procedure TKADaoDBGrid.DeSelectAll;
Begin
  OnDeSelectAll(Self);
End;

Procedure TKADaoDBGrid.CutToClp;
Begin
 OnCutToClp(Self);
End;

Procedure TKADaoDBGrid.CopyToClp;
Begin
 OnCopyToClp(Self);
End;

Procedure TKADaoDBGrid.PasteFromClp;
Begin
 OnPasteFromClp(Self);
End;

Procedure TKADaoDBGrid.FastLookup(Fast:Boolean);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     TKaDaoTable(DataSource.Dataset).CacheLookups := Fast;
End;

Procedure TKADaoDBGrid.ShowMemos(Show:Boolean);
Begin
 if  Assigned(DataSource)
 And Assigned(DataSource.Dataset)
 And (DataSource.DataSet is TKaDaoTable)
 And (DataSource.DataSet.Active) Then
     TKaDaoTable(DataSource.Dataset).CacheMemos := Show;
End;

Procedure   TKADaoDBGrid.OnMailGridData(Sender: TObject);
Const
  MAPIDLL  : String = 'MAPI32.DLL';
Var
  MSG        : TMapiMessage;
  FIL        : Array[0..1] of TMapiFileDesc;
  Err        : Integer;
  SendMail   : TFNMapiSendMail;
  MAPIModule : HModule;
  XLSFileName: String;
  HTMFileName: String;
  Path       : Array[0..1000] of Char;
  Temp       : Array[0..1000] of Char;
  TN         : String;
  Visible_   : TStringList;
  Invisible_ : TStringList;
  ExType     : TExportMethod;
  Table      : TKaDaoTable;
  FC         : TColumn;
  FF         : TField;
  X          : Integer;

Begin
  if  Assigned(DataSource)
  And Assigned(DataSource.Dataset)
  And (DataSource.DataSet is TKaDaoTable)
  And (DataSource.DataSet.Active)
  And (NOT DataSource.DataSet.IsEmpty) Then
     Begin
       //******************************************************* CREATE SNAPSHOT
       Table              := TKaDaoTable(DataSource.DataSet);
       Visible_           := TStringList.Create;
       Invisible_         := TStringList.Create;
       ExType             := Table.ExportMethod;
       Table.ExportMethod := VisibleFields;
       Table.DisableControls;
       For X := 0 To Table.FieldCount-1 do
           Begin
            if Table.Fields[X].Visible Then
               Begin
                 FC := FindColumn(Table.Fields[X].FieldName);
                 {$IFDEF D4UP}
                 IF FC = Nil Then Visible_.Add(Table.Fields[X].FieldName)
                    Else
                      if NOT FC.Visible Then Visible_.Add(Table.Fields[X].FieldName);
                 {$ELSE}
                 IF FC = Nil Then Visible_.Add(Table.Fields[X].FieldName);
                 {$ENDIF}
               End
            Else
               Begin
                 {$IFDEF D4UP}
                 FC := FindColumn(Table.Fields[X].FieldName);
                 IF FC <> Nil Then
                    if FC.Visible Then Invisible_.Add(Table.Fields[X].FieldName);
                 {$ENDIF}
               End;
           End;
       For X := 0 To Visible_.Count-1 do
             Begin
               FF := Table.FindField(Visible_.Strings[X]);
               if FF <> Nil Then FF.Visible := False;
             End;
       For X := 0 To Invisible_.Count-1 do
             Begin
               FF := Table.FindField(Invisible_.Strings[X]);
               if FF <> Nil Then FF.Visible := True;
             End;
       //*********************************************************************
       Try
         //************************************************* Export to temp file
         TN               := Table.TableName;
         if TN='' Then TN := Table.QueryDefName;
         if TN='' Then TN := 'Table';
         GetTempPath(1000,Path);

         XLSFileName := StrPas(Path)+TN+'.xls';
         DeleteFile(XLSFileName);
         TKaDaoTable(DataSource.Dataset).AccessExportToExcel(XLSFileName, TN, 8, False, True);

         HTMFileName := StrPas(Path)+TN+'.htm';
         DeleteFile(HTMFileName);
         TKaDaoTable(DataSource.Dataset).AccessExportToHTML(HTMFileName, False, True);
         //*********************************************************************
       Finally
         //**************************************************** RESTORE SNAPSHOT
         For X := 0 To Invisible_.Count-1 do
             Begin
               FF := Table.FindField(Invisible_.Strings[X]);
               if FF <> Nil Then FF.Visible := False;
             End;
         For X := 0 To Visible_.Count-1 do
             Begin
               FF := Table.FindField(Visible_.Strings[X]);
               if FF <> Nil Then FF.Visible := True;
             End;
         Table.EnableControls;
         Table.ExportMethod := ExType;
         Invisible_.Free;
         Visible_.Free;
         //*********************************************************************
       End;
       //*************************************************** Use MAPI to send it
       FIL[0].ulReserved         := 0;
       FIL[0].flFlags            := 0;
       FIL[0].nPosition          := Cardinal($FFFFFFFF);
       FIL[0].lpszPathName       := PChar(XLSFileName);
       FIL[0].lpszFileName       := PChar(TN+'.xls');
       FIL[0].lpFileType         := Nil;

       FIL[1].ulReserved         := 0;
       FIL[1].flFlags            := 0;
       FIL[1].nPosition          := Cardinal($FFFFFFFF);
       FIL[1].lpszPathName       := PChar(HTMFileName);
       FIL[1].lpszFileName       := PChar(TN+'.htm');
       FIL[1].lpFileType         := Nil;

       MSG.ulReserved            := 0;
       MSG.lpszSubject           := PChar(TKaDaoTable(DataSource.Dataset).TableName);
       MSG.lpszNoteText          := Nil;
       MSG.lpszMessageType       := Nil;
       MSG.lpszDateReceived      := Nil;
       MSG.lpszConversationID    := Nil;
       MSG.flFlags               := 0;
       MSG.lpOriginator          := Nil;
       MSG.nRecipCount           := 0;
       MSG.nFileCount            := 2;
       MSG.lpFiles               := @FIL;

       Err                       := 0;
       SendMail                  := Nil;
       MAPIModule                := LoadLibrary(PChar(MAPIDLL));
       @SendMail                 := GetProcAddress(MAPIModule, 'MAPISendMail');
       if @SendMail <> Nil Then
          Err := SendMail(0, Application.Handle, MSG, MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
       FreeLibrary(MAPIModule);
       //***********************************************************************

       //*************************************************************** CLEANUP
       DeleteFile(HTMFileName);
       DeleteFile(XLSFileName);
       DeleteFile(StrPas(Path)+'schema.ini');
       //***********************************************************************
   End;
End;

Procedure Register;
begin
  RegisterComponents('KADao Controls', [TKADaoDBGrid]);
end;
end.
