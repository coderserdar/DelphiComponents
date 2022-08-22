unit Main;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//      10/19/2000 T. Grubb
//	           - Added TSimplePathPlanner component, which descends from
//                   from TCustomPathPlanner
//                 - Renamed all references to TSimplePathPlanner
//                 - Modified Demo to use design-time components
//                 - General Code cleanup (a LOT more needed :) )
//
//	03/2000	   T. Grubb
//		   Initial version.
//
//      File Contents:
//           Main unit for Path Planning Demo
//
//
//--- Warning ----------------------------------------------------------------
//	This software is property of RiverSoftAVG. Unauthorized use or
//      duplication of this software is strictly prohibited. Authorized users
//      are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//      This software is freeware.  You are authorized to duplicate and modify
//      this software subject to the restrictions above.
//
//=== End File Prolog ========================================================


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, ExtCtrls, Grids, ComCtrls, PathPlanner, ImgList,
  Buttons, Menus, AppEvnts;

type
  TTerrainTypes = (ttRoad, ttPlain, ttBrush, ttForest, ttSwamp, ttWall, ttVisited {for TPathPlanner});
  TTerrainCosts = Array[TTerrainTypes] of Integer;
  TTerrainStrings = Array[TTerrainTypes] of String;
  TTerrainColors = Array[TTerrainTypes] of TColor;

const
  TerrainCost: TTerrainCosts = (1, 2, 4, 8, 16, 255, -1);
  Terrains: TTerrainStrings = ('Road', 'Plain', 'Brush', 'Forest', 'Swamp', 'Wall', 'Visited');
  TerrainColors: TTerrainColors = ( clWhite, clLime, clOlive, clGreen, clGray, clTeal, clMaroon );

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    btnFindPath: TButton;
    ActionList1: TActionList;
    actFindPath: TAction;
    PageControl1: TPageControl;
    tsSimple: TTabSheet;
    sgMap: TStringGrid;
    StatusBar1: TStatusBar;
    btnStep: TButton;
    actStepPath: TAction;
    btnClear: TButton;
    actClear: TAction;
    btnReset: TButton;
    actReset: TAction;
    gbBrushes: TGroupBox;
    sbStart: TSpeedButton;
    ImageList1: TImageList;
    sbEnd: TSpeedButton;
    sbClear: TSpeedButton;
    pnlFour: TPanel;
    pnlThree: TPanel;
    pnlTwo: TPanel;
    pnlOne: TPanel;
    sbWalls: TSpeedButton;
    btnAStarFindPath: TButton;
    actFindAStarPath: TAction;
    pnlFive: TPanel;
    tsBitmap: TTabSheet;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Terrain1: TMenuItem;
    Road1: TMenuItem;
    Plain1: TMenuItem;
    Brush1: TMenuItem;
    Forest1: TMenuItem;
    Swamp1: TMenuItem;
    N3: TMenuItem;
    Start1: TMenuItem;
    Goal1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    actSave: TAction;
    actSaveAs: TAction;
    actOpen: TAction;
    Wall1: TMenuItem;
    tbRepetitions: TTrackBar;
    lblTimes: TLabel;
    Debug1: TMenuItem;
    Sleep1: TMenuItem;
    ShowSearches1: TMenuItem;
    N01: TMenuItem;
    N1001: TMenuItem;
    N2501: TMenuItem;
    N5001: TMenuItem;
    N10001: TMenuItem;
    N101: TMenuItem;
    N251: TMenuItem;
    N501: TMenuItem;
    N751: TMenuItem;
    tbHeuristic: TTrackBar;
    lblHeuristic: TLabel;
    btnAverage: TButton;
    btnRandomize: TButton;
    ScrollBox1: TScrollBox;
    Image1: TPaintBox;
    Image2: TImage;
    ApplicationEvents1: TApplicationEvents;
    BitBtn1: TBitBtn;
    SimplePathPlanner1: TSimplePathPlanner;
    AStarPathPlanner1: TAStarPathPlanner;
    SearchableMap1: TSearchableMap;
    procedure actFindPathExecute(Sender: TObject);
    procedure DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure sgMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure actStepPathExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actResetExecute(Sender: TObject);
    procedure actSetTerrainExecute(Sender: TObject);
    procedure actFindAStarPathExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sgMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbRepetitionsChange(Sender: TObject);
    procedure N10001Click(Sender: TObject);
    procedure ShowSearches1Click(Sender: TObject);
    procedure tbHeuristicChange(Sender: TObject);
    procedure btnAverageClick(Sender: TObject);
    procedure btnRandomizeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Image1Paint(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure SimplePathPlanner1MapPassability(Sender: TObject; X,
      Y: Integer; var result: Single);
    procedure SimplePathPlanner1BlockLocation(Sender: TObject; X,
      Y: Integer);
    procedure SimplePathPlanner1PrepareMap(Sender: TObject);
    procedure SimplePathPlanner1ValidLocationCheck(Sender: TObject; X,
      Y: Integer; var result: Boolean);
  private
    FSleepMS: Integer;
    FOriginalBitmap: TBitmap;
  private
    { Private declarations }
    FEndCell: TPoint;
    FStartCell: TPoint;
    FVisited: TBits;
    FSelected: Integer;
    FDrawRect: TRect;
    procedure SearchEvent( Sender: TObject; State: TCustomSearchState );
    property Visited: TBits read FVisited;
    property OriginalBitmap: TBitmap read FOriginalBitmap;
    procedure SetSelected(const Value: Integer);
    property SleepMS: Integer read FSleepMS;
  public
    { Public declarations }
    property StartCell: TPoint read FStartCell;
    property EndCell: TPoint read FEndCell;
    property Selected: Integer read FSelected write SetSelected;
    property DrawRect: TRect read FDrawRect;
    procedure ClearMap;
    procedure ResetMap;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
uses
    MMSystem;

procedure TMainForm.actFindPathExecute(Sender: TObject);
var
   i: Integer;
   Atime: Double;
begin
     // Find Path
     ResetMap;
     SimplePathPlanner1.SetStart( StartCell );
     SimplePathPlanner1.SetEnd( EndCell );
     ATime := timeGetTime;
     StatusBar1.SimpleText := 'Iterations = '+IntToStr(SimplePathPlanner1.FindPath)+
                           ', ms = '+IntToStr(round(timeGetTime-ATime));

     if SimplePathPlanner1.IsFound then
     begin
          for i := 1 to SimplePathPlanner1.Path.Count - 2 do
          begin
               sgMap.Cells[SimplePathPlanner1.Path[i].X, SimplePathPlanner1.Path[i].Y] := IntToStr(i);
          end;
     end;
end;

procedure TMainForm.ClearMap;
var
   ARow, ACol: Integer;
begin
   if PageControl1.ActivePage = tsSimple then
   begin
     for ARow := 0 to sgMap.RowCount - 1 do
         for ACol := 0 to sgMap.ColCount - 1 do
         begin
              sgMap.Cells[ACol,ARow] := '';
              sgMap.Objects[ACol,ARow] := nil;
         end;
   end
   else
       Image1.Invalidate;
   SaveDialog1.Filename := '';

   SimplePathPlanner1.Clear;
   SearchableMap1.Clear;
end;

procedure TMainForm.DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
   i: TTerrainTypes;
   FoundIt: Boolean;
begin
     with Sender as TStringGrid do
     begin
          if (ACol = StartCell.x) and (ARow = StartCell.Y) then
             Canvas.BrushCopy( Rect, sbStart.Glyph, Classes.Rect(0,0,16,16), clLime )
          else if (ACol = EndCell.x) and (ARow = EndCell.Y) then
             Canvas.BrushCopy( Rect, sbEnd.Glyph, Classes.Rect(0,0,16,16), clWhite )
          else if Cells[ACol,ARow] = Terrains[ttWall] then
               Canvas.BrushCopy( Rect, sbWalls.Glyph, Classes.Rect(0,0,16,16), clLime )
          else
          begin
               if Cells[ACol,ARow] = '' then
               begin
                    Canvas.Brush.Color := clWhite;
                    FoundIt := True;
               end
               else
               begin
                    FoundIt := False;
                    Canvas.Brush.Color := clTeal;
                    for i := Low(TTerrainTypes) to High(TTerrainTypes) do
                        if Cells[ACol,ARow] = Terrains[i] then
                        begin
                             Canvas.Brush.Color := TerrainColors[i];
                             FoundIt := True;
                        end;
               end;
               Canvas.FillRect(Rect);
               if not FoundIt then
               begin
                    Canvas.TextRect( Rect, Rect.Left, Rect.Top, Cells[ACol,ARow] );
               end;
          end;
     end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
     PageControl1.ActivePage := tsSimple;
     FSleepMS := 25;
     FOriginalBitmap := TBitmap.Create;
     // Randomly set the start and stop
     FStartCell := Point( random(sgMap.ColCount),Random(sgMap.RowCount) );
     FEndCell := Point( random(sgMap.ColCount),Random(sgMap.RowCount) );
     Selected := 100;
     ResetMap;
end;

procedure TMainForm.sgMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   ARow, ACol: Integer;
   ACell: TPoint;
begin
     with Sender as TStringGrid do
     begin
          MouseToCell( X, Y, ACol, ARow );
          if (ACol >= ColCount) or
             (ACol < 0) or
             (ARow >= RowCount) or
             (ARow < 0) then Exit;
          if Selected in [100,101] then
          begin
                 if ((ACol = EndCell.X) and (ARow = EndCell.Y)) or
                    ((ACol = StartCell.X) and (ARow = StartCell.Y)) then
                 begin
                      ACell := StartCell;
                      FStartCell := EndCell;
                      FEndCell := ACell;
                 end
                 else if Selected = 100 then // start
                      FStartCell := Point( ACol, ARow )
                 else
                     FEndCell := Point( ACol, ARow );
                 SimplePathPlanner1.SetStart(StartCell);
                 SimplePathPlanner1.SetEnd(EndCell);
                 ResetMap;
          end
          else if ssShift in Shift then
          begin
               FDrawRect := Rect( X, Y, X, Y );
          end
          else
          begin
               Cells[ACol,ARow] := Terrains[TTerrainTypes(Selected)];
               Objects[ACol,ARow] := TObject( TTerrainTypes(Selected) );
          end;
     end;
end;

procedure TMainForm.sgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if ssLeft in Shift then
   begin
     if (Selected in [100,101]) or (not (ssShift in Shift)) then
        sgMapMouseDown(Sender,mbLeft,Shift,X,Y)
     else
     begin
          (Sender as TStringGrid).Canvas.DrawFocusRect( DrawRect );
          FDrawRect.Right := X;
          FDrawRect.Bottom := Y;          
          (Sender as TStringGrid).Canvas.DrawFocusRect( DrawRect );
     end;
   end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
     if Visited <> nil then Visited.Free;
     OriginalBitmap.Free;
end;

procedure TMainForm.ResetMap;
var
   ARow, ACol: Integer;
begin
   if PageControl1.ActivePage = tsSimple then
   begin
     for ARow := 0 to sgMap.RowCount - 1 do
         for ACol := 0 to sgMap.ColCount - 1 do
             sgMap.Cells[ACol,ARow] := Terrains[TTerrainTypes(sgMap.Objects[ACol,ARow])];
     sgMap.Invalidate;
   end
   else
       Image1.Invalidate;
     SimplePathPlanner1.Reset;
     SimplePathPlanner1.SetStart( StartCell );
     SimplePathPlanner1.SetEnd( EndCell );
end;

procedure TMainForm.actStepPathExecute(Sender: TObject);
var
   i: Integer;
begin
     if SimplePathPlanner1.Path.Count > 1 then
        with SimplePathPlanner1.Path[SimplePathPlanner1.Path.Count - 1] do
             sgMap.Cells[X, Y] := Terrains[ttVisited];
     StatusBar1.SimpleText := 'Iterations = '+IntToStr(SimplePathPlanner1.FindPath(1));
     if SimplePathPlanner1.IsFound then
     begin
          for i := 1 to SimplePathPlanner1.Path.Count - 2 do
          begin
               sgMap.Cells[SimplePathPlanner1.Path[i].X, SimplePathPlanner1.Path[i].Y] := IntToStr(i);
          end;
     end
     else if SimplePathPlanner1.Path.Count > 1 then
         with SimplePathPlanner1.Path[SimplePathPlanner1.Path.Count - 1] do
         begin
              sgMap.Cells[X,Y] := '*';
         end;
end;

procedure TMainForm.actClearExecute(Sender: TObject);
begin
     ClearMap;
end;

procedure TMainForm.actResetExecute(Sender: TObject);
begin
     ResetMap;
end;

procedure TMainForm.actSetTerrainExecute(Sender: TObject);
begin
     Selected := (Sender as TComponent).Tag;
end;

procedure TMainForm.SetSelected(const Value: Integer);
begin
     // Unselect first one
     case Selected of
     0:
     begin
          pnlOne.BevelOuter := bvRaised;
          sbClear.Down := False;
     end;
     1:   pnlTwo.BevelOuter := bvRaised;
     2:   pnlThree.BevelOuter := bvRaised;
     3:   pnlFour.BevelOuter := bvRaised;
     4:   pnlFive.BevelOuter := bvRaised;
     5:   sbWalls.Down := False;
     100: sbStart.Down := False;
     101: sbEnd.Down := False;
     end;
     FSelected := Value;
     case Selected of
     0:
     begin
          pnlOne.BevelOuter := bvLowered;
          sbClear.Down := True;
     end;
     1:   pnlTwo.BevelOuter := bvLowered;
     2:   pnlThree.BevelOuter := bvLowered;
     3:   pnlFour.BevelOuter := bvLowered;
     4:   pnlFive.BevelOuter := bvLowered;
     5:   sbWalls.Down := True;
     100: sbStart.Down := True;
     101: sbEnd.Down := True;
     end;
     // Menu Items
     case Selected of
     0:   Road1.Checked := True;
     1:   Plain1.Checked := True;
     2:   Brush1.Checked := True;
     3:   Forest1.Checked := True;
     4:   Swamp1.Checked := True;
     5:   Wall1.Checked := True;
     100: Start1.Checked := True;
     101: Goal1.Checked := True;
     end;

end;

procedure TMainForm.actFindAStarPathExecute(Sender: TObject);
var
   i,j: Integer;
   t1: Double;
   r: Integer;
   TotalTime: Double;
   AnEvent: TStateEvent;
begin
  AnEvent := AStarPathPlanner1.OnSearchState;
  if (tbRepetitions.Position > 1) and Assigned(AStarPathPlanner1.OnSearchState) then
     AStarPathPlanner1.OnSearchState := nil;
  SearchableMap1.Weight := tbHeuristic.Position/10;
  Screen.Cursor := crHourGlass;
  try
    // Fill the map
    ResetMap;
    Application.ProcessMessages;
    if PageControl1.ActivePage = tsSimple then
    begin
        SearchableMap1.Clear;
        for i := 0 to sgMap.ColCount - 1 do
           for j := 0 to sgMap.RowCount - 1 do
           begin
                SearchableMap1[i,j].Terrain := TerrainCost[TTerrainTypes(sgMap.Objects[i,j])];
           end;
        TotalTime := 0;
        for r := 1 to tbRepetitions.Position do
        begin
           t1 := timeGetTime;
           SearchableMap1.Reset;
           j := AStarPathPlanner1.FindPath( StartCell, EndCell );
           TotalTime := TotalTime + (timeGetTime - t1);
           if j > -1 then
           begin
                if r = tbRepetitions.Position then
                begin
                   for i := 1 to AStarPathPlanner1.Path.Count - 2 do
                   begin
                        sgMap.Cells[AStarPathPlanner1.Path[i].X, AStarPathPlanner1.Path[i].Y] := IntToStr(AStarPathPlanner1.Path[i].Tag);
                   end;
                   StatusBar1.SimpleText := 'Found it '+IntToStr(r)+' times (avg time): '+FloatToStr(TotalTime / r)+
                                            '  Nodes Examined: '+IntToStr(j);
                end;
           end
           else
           begin
                StatusBar1.SimpleText := 'Path not found (avg time): '+FloatToStr(TotalTime / r);
                Exit;
           end;
        end;
    end
    else
    begin
        SearchableMap1.Clear;
        for i := 0 to OriginalBitmap.Width - 1 do
           for j := 0 to OriginalBitmap.Height - 1 do
           begin
                SearchableMap1[i,j].Terrain := GetBValue(ColorToRGB(OriginalBitmap.Canvas.Pixels[i,j]));
           end;
        //SearchableMap1.MaxPassableTerrainCost := 128;
        TotalTime := 0;
        for r := 1 to tbRepetitions.Position do
        begin
           t1 := timeGetTime;
           SearchableMap1.Reset;
           j := AStarPathPlanner1.FindPath( StartCell, EndCell );
           TotalTime := TotalTime + (timeGetTime - t1);
           if j > -1 then
           begin
                if r = tbRepetitions.Position then
                begin
                   Image1.Invalidate;
                   StatusBar1.SimpleText := 'Found it '+IntToStr(r)+' times (avg time): '+FloatToStr(TotalTime / r)+
                                            '  Nodes Examined: '+IntToStr(j);
                end;
           end
           else
           begin
                StatusBar1.SimpleText := 'Path not found (avg time): '+FloatToStr(TotalTime / r);
                Exit;
           end;
        end;
    end
  finally
    Screen.Cursor := crDefault;
    AStarPathPlanner1.OnSearchState := AnEvent;
  end;
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
var
   FS: TFileStream;
   S: String;
   ACol, ARow: Integer;
begin
     if SaveDialog1.FileName = '' then
        actSaveAsExecute( Sender )
     else
     begin
          ResetMap;
          FS := TFileStream.Create( SaveDialog1.Filename, fmCreate );
          try
             S := IntToStr(sgMap.ColCount)+#13;
             FS.Write( S[1], Length(S) );
             S := IntToStr(sgMap.RowCount)+#13;
             FS.Write( S[1], Length(S) );
             for ACol := 0 to sgMap.ColCount - 1 do
                 for ARow := 0 to sgMap.RowCount - 1 do
                 begin
                      S := IntToStr(Integer(sgMap.Objects[ACol,ARow]))+#13;
                      FS.Write( S[1], Length(S) );
                 end;
          finally
             FS.Free;
          end;
          OpenDialog1.FileName := SaveDialog1.FileName;
     end;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
begin
     if SaveDialog1.Execute then
        actSaveExecute( Sender );
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
  function GetToken( const FS: TFileStream ): String;
  var
     CH: Char;
  begin
    result := '';
    { Read the input one character at a time. }
    while FS.Read(Ch, 1) = 1 do
    begin
       if Pos(Ch, ' '+#13#10+#9) > 0 then
       begin
            Break;
       end;
       { Append to the token String }
       AppendStr(result, Ch);
    end;
  end;

var
   ACol, ARow: Integer;
   i: TTerrainTypes;
   FS: TFileStream;
   S: String;
begin
     if OpenDialog1.Execute then
     begin
          // Load image if it is a bitmap
          if Lowercase(ExtractFileExt(OpenDialog1.FileName)) = '.bmp' then
          begin
               Image2.Picture.LoadFromFile( OpenDialog1.FileName );
               OriginalBitmap.Width := Image2.Picture.Width;
               OriginalBitmap.Height := Image2.Picture.Height;
               OriginalBitmap.Canvas.BrushCopy( OriginalBitmap.Canvas.ClipRect,
                                                Image2.Picture.Bitmap,
                                                Image2.Picture.Bitmap.Canvas.ClipRect,
                                                clLime );
               Image1.SetBounds( 0, 0, OriginalBitmap.Width, OriginalBitmap.Height );
               PageControl1.ActivePage := tsBitmap;
               PageControl1.OnChange( PageControl1 );
               Exit;
          end;
          PageControl1.ActivePage := tsSimple;
          PageControl1.OnChange( PageControl1 );
          FS := TFileStream.Create( OpenDialog1.Filename, fmOpenRead );
          try
            // Get the Row Count
            S := GetToken(FS);
            sgMap.ColCount := StrToInt(S);
            S := GetToken(FS);
            sgMap.RowCount := StrToInt(S);
            SearchableMap1.ResizeMap( sgMap.ColCount, sgMap.RowCount );
            for ACol := 0 to sgMap.ColCount - 1 do
                for ARow := 0 to sgMap.RowCount - 1 do
                begin
                     S := GetToken(FS);
                     i := TTerrainTypes(StrToInt(S));
                     sgMap.Objects[ACol,ARow] := TObject(i);
                     sgMap.Cells[ACol,ARow] := Terrains[i];
                end;
          finally
            FS.Free;
          end;
          for ACol := 0 to sgMap.ColCount - 1 do
              for ARow := 0 to sgMap.RowCount - 1 do
              begin
                   for i := Low(TTerrainTypes) to High(TTerrainTypes) do
                       if sgMap.Cells[ACol,ARow] = Terrains[i] then
                       begin
                            sgMap.Objects[ACol,ARow] := TObject(i);
                            break;
                       end;
              end;
          SaveDialog1.FileName := OpenDialog1.FileName;
     end;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
     Close;
end;

procedure TMainForm.sgMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   sCol, sRow, eCol, eRow: Integer;
   ARow, ACol: Integer;
begin
   if (Button = mbLeft) and (ssShift in Shift) then
   begin
     if not (Selected in [100,101]) then
     begin
          (Sender as TStringGrid).Canvas.DrawFocusRect( DrawRect );
          FDrawRect.Right := X;
          FDrawRect.Bottom := Y;
          sgMap.MouseToCell( FDrawRect.Left,FDrawRect.Top, sCol, sRow );
          sgMap.MouseToCell( FDrawRect.Right,FDrawRect.Bottom, eCol, eRow );
          for ACol := sCol to eCol do
              for ARow := sRow to eRow do
              begin
                   sgMap.Cells[ACol,ARow] := Terrains[TTerrainTypes(Selected)];
                   sgMap.Objects[ACol,ARow] := TObject( TTerrainTypes(Selected) );
              end;
     end;
   end;
end;

procedure TMainForm.tbRepetitionsChange(Sender: TObject);
begin
     lblTimes.Caption := IntToStr((Sender as TTrackBar).Position);
end;

procedure TMainForm.N10001Click(Sender: TObject);
begin
     FSleepMS := (Sender as TComponent).Tag;
     (Sender as TMenuItem).Checked := True;
end;

procedure TMainForm.ShowSearches1Click(Sender: TObject);
begin
     (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
     if (Sender as TMenuItem).Checked then
        AStarPathPlanner1.OnSearchState := SearchEvent
     else
        AStarPathPlanner1.OnSearchState := nil;
end;

procedure TMainForm.SearchEvent(Sender: TObject;
  State: TCustomSearchState);
var
   r1, r2: TRect;
begin
     if State.Parent <> nil then
     begin
          with TMaplocation(State) do
               r1 := sgMap.CellRect(X,Y);
          with TMapLocation(State.Parent) do
               r2 := sgMap.CellRect(X,Y);
          sgMap.Canvas.Pen.Color := clRed;
          sgMap.Canvas.MoveTo( r2.Left + (r2.Right - r2.Left) div 2,
                               r2.Top + (r2.Bottom - r2.Top) div 2 );
          sgMap.Canvas.LineTo( r1.Left + (r1.Right - r1.Left) div 2,
                               r1.Top + (r1.Bottom - r1.Top) div 2 );
          Application.ProcessMessages;
          Sleep(SleepMS);
     end;
end;

procedure TMainForm.tbHeuristicChange(Sender: TObject);
begin
     lblHeuristic.Caption := FloatToStr((Sender as TTrackBar).Position / 10);
end;

procedure TMainForm.btnAverageClick(Sender: TObject);
var
   ACol, ARow: Integer;
   Sum: Double;
   i, j: Integer;
begin
   Screen.Cursor := crHourGlass;
   try
     SearchableMap1.Clear;
     if PageControl1.ActivePage = tsSimple then
     begin
        for i := 0 to sgMap.ColCount - 1 do
           for j := 0 to sgMap.RowCount - 1 do
           begin
                SearchableMap1[i,j].Terrain := TerrainCost[TTerrainTypes(sgMap.Objects[i,j])];
           end;
     end
     else
     begin
        for i := 0 to OriginalBitmap.Width - 1 do
           for j := 0 to OriginalBitmap.Height - 1 do
           begin
                SearchableMap1[i,j].Terrain := GetBValue(ColorToRGB(OriginalBitmap.Canvas.Pixels[i,j]));
           end;
     end;
     Sum := 0;
     // get the passable terrain cost, don't include walls
     for ACol := 0 to SearchableMap1.Width - 1 do
         for ARow := 0 to SearchableMap1.Height - 1 do
             if SearchableMap1[ACol,ARow].Terrain <> (TerrainCost[ttWall]) then
                Sum := Sum + SearchableMap1[ACol,ARow].TerrainCost;
     tbHeuristic.Position := Round((Sum / (SearchableMap1.Width*SearchableMap1.Height))*10);
     tbHeuristic.OnChange( tbHeuristic );
   finally
     Screen.Cursor := crDefault;
   end;
end;

procedure TMainForm.btnRandomizeClick(Sender: TObject);
begin
     if PageControl1.ActivePage = tsSimple then
     begin
          FStartCell := Point( random(sgMap.ColCount),Random(sgMap.RowCount) );
          FEndCell := Point( random(sgMap.ColCount),Random(sgMap.RowCount) );
          while sgMap.Objects[EndCell.X,EndCell.Y] = TObject(ttWall) do
                FEndCell := Point( random(sgMap.ColCount),Random(sgMap.RowCount) );
     end
     else
     begin
          FStartCell := Point( Random(OriginalBitmap.Width),Random(OriginalBitmap.Height) );
          FEndCell := Point( random(OriginalBitmap.Width),Random(OriginalBitmap.Height) );
     end;
     ResetMap;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
     sgMap.DefaultColWidth := trunc(sgMap.ClientWidth / (sgMap.ColCount+2));
     sgMap.DefaultRowHeight := trunc(sgMap.ClientHeight / (sgMap.RowCount+2));
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
     if PageControl1.ActivePage = tsSimple then
     begin
          tbHeuristic.Max := 100;
          tbHeuristic.Frequency := 5;
          btnFindPath.Enabled := True;
          btnStep.Enabled := True;
          sbClear.Enabled := True;
          sbWalls.Enabled := True;
          pnlOne.Enabled := True;
          pnlTwo.Enabled := True;
          pnlThree.Enabled := True;
          pnlFour.Enabled := True;
          pnlFive.Enabled := True;
          SearchableMap1.ResizeMap( sgMap.ColCount, sgMap.RowCount );
     end
     else
     begin
          tbHeuristic.Max := 2560;
          tbHeuristic.Frequency := 100;
          btnFindPath.Enabled := False;
          btnStep.Enabled := False;
          sbClear.Enabled := False;
          sbWalls.Enabled := False;
          pnlOne.Enabled := False;
          pnlTwo.Enabled := False;
          pnlThree.Enabled := False;
          pnlFour.Enabled := False;
          pnlFive.Enabled := False;
          SearchableMap1.ResizeMap( OriginalBitmap.Width, OriginalBitmap.Height );
     end;
end;

procedure TMainForm.Image1Paint(Sender: TObject);
var
   OffScreen: TBitmap;
   i: Integer;
   ARect: TRect;
begin
   if OriginalBitmap = nil then Exit;
   OffScreen := TBitmap.Create;
   try
      ARect := Rect( 0, 0, OriginalBitmap.Width, OriginalBitmap.Height );
      OffScreen.Width := OriginalBitmap.Width;
      OffScreen.Height := OriginalBitmap.Height;
      OffScreen.Canvas.BrushCopy( ARect, OriginalBitmap, ARect,
                              clLime );
      OffScreen.Canvas.BrushCopy( Rect( StartCell.X, StartCell.Y,
                                   StartCell.X+sbStart.Glyph.Width div 2,
                                   StartCell.y+sbStart.Glyph.Height ),
                             sbStart.Glyph,
                             Rect(0,0,sbStart.Glyph.Width div 2,sbStart.Glyph.Height), clOlive );
      OffScreen.Canvas.BrushCopy( Rect( EndCell.X, EndCell.Y,
                                   EndCell.X+sbEnd.Glyph.Width div 2,
                                   EndCell.y+sbEnd.Glyph.Height ),
                             sbEnd.Glyph,
                             Rect(0,0,sbEnd.Glyph.Width div 2,sbEnd.Glyph.Height),
                             clOlive );
      if AStarPathPlanner1.Path.Count > 0 then
      begin
           for i := 0 to AStarPathPlanner1.Path.Count - 1 do
               OffScreen.Canvas.Pixels[AStarPathPlanner1.Path[i].X,AStarPathPlanner1.Path[i].Y] := clLime;
      end;
      if Image1.Canvas.ClipRect.Right > 0 then
         Image1.Canvas.StretchDraw( ARect, OffScreen );
   finally
      OffScreen.Free;
   end;
end;

procedure TMainForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     case Selected of
     100: FStartCell := Point(X,Y);
     101: FEndCell := Point(X,Y);
     end;
     (Sender as TControl).Invalidate;
end;

procedure TMainForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
     if ssLeft in Shift then Image1MouseDown(sender,mbLeft,shift,X,Y);
end;

procedure TMainForm.ApplicationEvents1Hint(Sender: TObject);
begin
     StatusBar1.SimpleText := (Sender as TApplication).Hint;
end;

procedure TMainForm.SimplePathPlanner1MapPassability(Sender: TObject; X,
  Y: Integer; var result: Single);
begin
     if (X < 0) or (X >= sgMap.ColCount) or (Y < 0) or (Y >= sgMap.RowCount) then
        result := 0
     else if (sgMap.Cells[X,Y] = Terrains[ttWall]) or (Visited[X+sgMap.RowCount*Y]) then
          result := 0
     else
         result := 1;
end;

procedure TMainForm.SimplePathPlanner1BlockLocation(Sender: TObject; X,
  Y: Integer);
begin
     Visited[X+sgMap.RowCount*Y] := True;
     if not (((StartCell.X = X) and (StartCell.Y = Y)) or ((EndCell.X = X) and (EndCell.Y = Y))) then
        sgMap.Cells[X,Y] := Terrains[ttVisited];
end;

procedure TMainForm.SimplePathPlanner1PrepareMap(Sender: TObject);
begin
     if Visited <> nil then Visited.Free;
     FVisited := TBits.Create;
     FVisited.Size := sgMap.ColCount * sgMap.RowCount;
end;

procedure TMainForm.SimplePathPlanner1ValidLocationCheck(Sender: TObject;
  X, Y: Integer; var result: Boolean);
begin
     if (X < 0) or (X >= sgMap.ColCount) or (Y < 0) or (Y >= sgMap.RowCount) then
        result := False
     else
         result := not Visited[X+sgMap.ColCount*Y];
end;

end.
