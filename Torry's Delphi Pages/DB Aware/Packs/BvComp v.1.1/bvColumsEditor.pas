unit bvColumsEditor;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
{$else}
  QGraphics,          
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QDialogs,
  QMenus,
  Types,
  QControls,
  QComCtrls,
  QGrids,
  QTypes,
{$endif}
  Classes,
  SysUtils,
  bvDBGrid,
  bvLocalization;


const CheckBmp:TBitMap=nil;

{$ifdef LINUX}


type
  TColEditForm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    BBDefault: TBitBtn;
    ColorDialog: TColorDialog;
    CheckReadOnly: TCheckBox;
    FontDialog: TFontDialog;
    BitBtnInsert: TBitBtn;
    BitBtnDelete: TBitBtn;
    PopupMenu1: TPopupMenu;
    NDelete: TMenuItem;
    NInsert: TMenuItem;
    Bevel3: TBevel;
    Bevel4: TBevel;
    PageControl: TPageControl;
    TabSheetColumns: TTabSheet;
    TabSheetOptions: TTabSheet;
    SGFields: TStringGrid;
    TabSheetStrippedRows: TTabSheet;
    LabColCount: TLabel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    LabelContents: TLabel;
    LabelTitle: TLabel;
    EditColor: TBitBtn;
    EditFixedColor: TBitBtn;
    EditFont: TBitBtn;
    EditTitleFont: TBitBtn;
    Bevel5: TBevel;
    LabStrippedRows: TLabel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    LabelEach: TLabel;
    LabelY: TLabel;
    LabelColor: TLabel;
    EditStrippedColor: TShape;
    SpeedButtonIncFont: TSpeedButton;
    SpeedButtonDecFont: TSpeedButton;
    Bevel9: TBevel;
    LabTitleMinHeight: TLabel;
    CheckEnter2Tab: TCheckBox;
    CheckGridFixedColor: TCheckBox;
    Check3dvInner: TSpeedButton;
    Check3dvOuter: TSpeedButton;
    CheckHORZLINE: TSpeedButton;
    CHECKVertLine: TSpeedButton;
    Check3dHOuter: TSpeedButton;
    Check3dHInner: TSpeedButton;
    CheckTitle: TSpeedButton;
    CheckIndicator: TSpeedButton;
    CheckSelectROws: TSpeedButton;
    CheckMultiSelect: TSpeedButton;
    CheckCELLHint: TSpeedButton;
    LabCellROWS: TLabel;
    LabTitleMinHeight1: TLabel;
    LabCellROWS1: TLabel;
    CheckAlwaysShowEditor: TSpeedButton;
    CheckAlwaysShowSelection: TSpeedButton;
    CheckTabStop: TSpeedButton;
    EditColCount: TSpinEdit;
    EditTitleMinHeight: TSpinEdit;
    EditCellRows: TSpinEdit;
    EditStrippedRows: TSpinEdit;
    procedure SGFieldsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure SGFieldsSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure BBDefaultClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure SGFieldsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SGFieldsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditColorClick(Sender: TObject);
    procedure EditFixedColorClick(Sender: TObject);
    procedure EditFontClick(Sender: TObject);
    procedure EditTitleFontClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnDeleteClick(Sender: TObject);
    procedure BitBtnInsertClick(Sender: TObject);
    procedure NDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditStrippedRowsChange(Sender: TObject);
    procedure EditStrippedColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonIncFontClick(Sender: TObject);
    procedure SpeedButtonDecFontClick(Sender: TObject);
    procedure Check3dvInnerClick(Sender: TObject);
    procedure Check3dHInnerClick(Sender: TObject);
    procedure NInsertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
//    thFont:TFont;
    thTitleFont:TFont;

    constructor Create(AOwner:TComponent); override;
  end;

{$else}
type
  TColEditForm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    BBDefault: TBitBtn;
    ColorDialog: TColorDialog;
    CheckReadOnly: TCheckBox;
    FontDialog: TFontDialog;
    BitBtnInsert: TBitBtn;
    BitBtnDelete: TBitBtn;
    PopupMenu1: TPopupMenu;
    NDelete: TMenuItem;
    NInsert: TMenuItem;
    Bevel3: TBevel;
    Bevel4: TBevel;
    PageControl: TPageControl;
    TabSheetColumns: TTabSheet;
    TabSheetOptions: TTabSheet;
    SGFields: TStringGrid;
    TabSheetStrippedRows: TTabSheet;
    LabColCount: TLabel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    LabelContents: TLabel;
    LabelTitle: TLabel;
    EditColor: TBitBtn;
    EditFixedColor: TBitBtn;
    EditFont: TBitBtn;
    EditTitleFont: TBitBtn;
    Bevel5: TBevel;
    LabStrippedRows: TLabel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    LabelEach: TLabel;
    LabelY: TLabel;
    LabelColor: TLabel;
    EditStrippedColor: TShape;
    SpeedButtonIncFont: TSpeedButton;
    SpeedButtonDecFont: TSpeedButton;
    Bevel9: TBevel;
    LabTitleMinHeight: TLabel;
    CheckEnter2Tab: TCheckBox;
    CheckGridFixedColor: TCheckBox;
    Check3dvInner: TSpeedButton;
    Check3dvOuter: TSpeedButton;
    CheckHORZLINE: TSpeedButton;
    CHECKVertLine: TSpeedButton;
    Check3dHOuter: TSpeedButton;
    Check3dHInner: TSpeedButton;
    CheckTitle: TSpeedButton;
    CheckIndicator: TSpeedButton;
    CheckSelectROws: TSpeedButton;
    CheckMultiSelect: TSpeedButton;
    CheckCELLHint: TSpeedButton;
    LabCellROWS: TLabel;
    LabTitleMinHeight1: TLabel;
    LabCellROWS1: TLabel;
    CheckAlwaysShowEditor: TSpeedButton;
    CheckAlwaysShowSelection: TSpeedButton;
    CheckTabStop: TSpeedButton;
    EditColCount: TUpDown;
    EditColCountP: TEdit;
    EditTitleMinHeightP: TEdit;
    EditTitleMinHeight: TUpDown;
    EditCellRowsP: TEdit;
    EditCellRows: TUpDown;
    EditStrippedRowsP: TEdit;
    EditStrippedRows: TUpDown;
    procedure SGFieldsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure SGFieldsSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure BBDefaultClick(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
    procedure SGFieldsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SGFieldsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditColorClick(Sender: TObject);
    procedure EditFixedColorClick(Sender: TObject);
    procedure EditFontClick(Sender: TObject);
    procedure EditTitleFontClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtnDeleteClick(Sender: TObject);
    procedure BitBtnInsertClick(Sender: TObject);
    procedure NDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditStrippedRowsChange(Sender: TObject);
    procedure EditStrippedColorMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonIncFontClick(Sender: TObject);
    procedure SpeedButtonDecFontClick(Sender: TObject);
    procedure Check3dvInnerClick(Sender: TObject);
    procedure Check3dHInnerClick(Sender: TObject);
    procedure NInsertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    thTitleFont:TFont;

    constructor Create(AOwner:TComponent); override;
  end;

{$endif}

var
  ColEditForm: TColEditForm;

implementation

uses bvDBGridSaver,math,bvGetAlignmentUnit,bvNewFieldUnit, bvMessageUnit,
  bvDBGridDrawType;

{$ifndef LINUX}
{$R *.dfm}
{$else}
{$R *.xfm}
{$endif}

{$R bvcheck.res}


function GetAlignStr(Value:tAlignMent):string;
begin
  Case value of
    taLeftJustify: Result:=StrLeftAlignMent;
    taCenter: Result:=StrCenterAlignMent;
    taRightJustify: Result:=StrRightAlignMent;
  end;
end;

function GetAlignValue(Value:string):TAlignMent;
begin
 if Value=StrLeftAlignMent then Result:=taLeftJustify
 else if Value=StrCenterAlignMent then Result:=taCenter
 else {if Value=RightAlignMent then} Result:=taRightJustify
// else Result:=taLeftJustify;

end;

function ExecuteAlignmentDialog(Value:string):string;
begin
  Result:=Value;

  with TAlignmentForm.Create(Application) do
  try

    if value=StrLeftAlignMent then EditAlignment.ItemIndex:=0
    else if Value=StrCenterAlignment then EditAlignMent.itemIndex:=1
    else EditAlignment.ItemIndex:=2;

    if ShowModal=mrOk then begin
      case EditAlignMent.ItemIndex of
         0: Result:=StrLeftAlignment;
         1: Result:=StrCenterAlignment;
         else Result:=StrRightAlignment;
      end;
    end;
  finally
    free;
  end;
//  sgfields.Cells[thCol,thRow]:=ExecuteAlignmentDialog(sgFields.Cells[thCol,thRow]);

end;



constructor TColEditForm.Create(AOwner:TComponent);
var
    ColCount,i:integer;
    ThisColumns:TDBGridColumns;
    ThisGrid:TDBGrid;
//    ThFieldName,ThColumnName:string;
//    ThWidth,ThIndex:integer;
begin
  inherited Create(AOwner);

  ThisGrid:=(Owner as TDBGridSaver).ThisGrid;
  ThisColumns:=(ThisGrid as TDBGrid).Columns;

  SGFields.Cols[0].Text:=''; // Индекс
  SGFields.Cols[1].Text:=StrField;
  SGFields.Cols[2].Text:=StrTitle;
  SGFields.Cols[3].Text:=StrWidth;
  SGFields.Cols[4].Text:=StrVisible;

  SGFields.Cols[5].Text:=StrAlignment;
  SGFields.Cols[6].Text:=StrReadOnly;
  SGFields.Cols[7].Text:=StrTitleAlignment;
  SGFields.Cols[8].Text:=StrColor;
  SGFields.Cols[9].Text:=StrFont;


  ColCount:=ThisColumns.Count;
  SGFields.RowCount:=ColCount+1;

  for i:=1 to ColCount do begin
    SGFields.Cells[0,i]:=inttostr(max(0,ThisColumns[i-1].ID));
    SGFields.Cells[1,i]:=trim(ThisColumns[i-1].FieldName);
    SGFields.Cells[2,i]:=trim(ThisColumns[i-1].Title.Caption);
    SGFields.Cells[3,i]:=inttostr(max(0,ThisColumns[i-1].Width));

    if ThisColumns[i-1].Visible then SGFields.Cells[4,i]:=BoolTrueText
    else SGFields.Cells[4,i]:=BoolFalseText;

    SGFields.Cells[5,i]:=GetAlignStr(ThisColumns[i-1].AlignMent);

    if ThisColumns[i-1].ReadOnly
    then SGFields.Cells[6,i]:= BoolTrueText
    else SGFields.Cells[6,i]:= BoolFalseText;

    SGFields.Cells[7,i]:=GetAlignStr(ThisColumns[i-1].Title.AlignMent);

    SGFields.Cells[8,i]:=inttostr(ThisColumns[i-1].Color);

    sgfields.Objects[9,i]:=TFont.Create;
    (sgFields.Objects[9,i] as TFont).Assign(ThisColumns[i-1].Font);
  end;

  if (Owner as tdbGridSaver).FixedCols>-1
  then begin
    LabColCount.Enabled:=true;
    EditColCount.Enabled:=true;
    {$ifndef LINUX}
    EditColCount.Position
    {$else}
    EditColCount.Value
    {$endif}
        := (Owner as tdbGridSaver).FixedCols;
  end
  else begin
    EditColCount.Enabled:=false;
    {$ifndef LINUX}
    EditColCount.position
    {$else}
    EditColCount.Value
    {$endif}
             :=0;
    LabColCount.enabled:=false;
  end;

  CheckIndicator.Down:=(Owner as tdbGridSaver).Indicator;
  CheckTabStop.down:=(Owner as TdbGridSaver).TabStop;
  CheckAlwaysShowEditor.Down:=(Owner as tdbGridSaver).alwaysShowEditor;
  CheckAlwaysShowSelection.Down:=(Owner as tdbGridSaver).alwaysShowSelection;
  CheckHorzLine.Down:=(Owner as tdbGridSaver).RowLines;
  CheckVertLine.Down:=(Owner as tdbGridSaver).ColLines;

  CheckTitle.Down:=(Owner as tdbGridSaver).CheckTitle;
  CheckSelectRows.Down:=(Owner as tdbGridSaver).selectRows;
  SGFields.Color:=(Owner as tdbGridSaver).Color;
  SGFields.FixedColor:=(Owner as tdbGridSaver).FixedColor;
  CheckMultiSelect.Down:=(Owner as TdbGridSAver).MultiSelect;

  {$ifndef LINUX}
  EditStrippedrows.position
  {$else}
  EditStrippedrows.value
  {$endif}
     :=(Owner as TdbGridSAver).StrippedRows;

  EditStrippedColor.Brush.color:=(Owner as TdbGridSAver).StrippedColor;

//  thFont:=TFont.Create;
  sgFields.Font:=(Owner as tdbGridSaver).Font;
  thtitleFont:=TFont.Create;
  thTitleFont.Assign((Owner as tdbGridSaver).TitleFont);

  CheckReadOnly.Checked:=(Owner as tdbGridSaver).ReadOnly;
  CheckReadOnly.Enabled:=not (Owner as tdbgridSaver).DefaultReadOnly;

  if ThisGrid is TBVDBGrid then begin
     {$ifndef LINUX}
     EditTitleMinHeight.position
     {$else}
     EditTitleMinHeight.value
     {$endif}
        :=(Owner as tdbgridSaver).TitleMinHeight;

     {$ifndef LINUX}
     EditCellROWS.Position
     {$else}
     EditCellROWS.value
     {$endif}
        :=(Owner as tdbgridSaver).CellROWS;

     CheckEnter2Tab.checked:=(Owner as tdbgridSaver).Enter2Tab;
     CheckCellHint.down:=(Owner as tdbgridSaver).CellHINT;
     case (Owner as tdbgridSaver).Lines3dV of
       C3dInner: Check3dVInner.down:=true;
       c3dOuter: check3dVOuter.down:=true;
     end;

     case (Owner as tdbgridSaver).Lines3dH of
       C3dInner: Check3dHInner.down:=true;
       c3dOuter: check3dHOuter.down:=true;
     end;

     //Check3D.checked:=(Owner as tdbgridSaver).Lines3d;
     //Check3D.checked:=(Owner as tdbgridSaver).Lines3d;
  end
  else begin
     EDitTitleMinHeight.enabled:=false;
     LabTitleMinHeight.enabled:=false;
     LabTitleMinHeight1.enabled:=false;

     EDitCellROWS.enabled:=false;
     LabCELLROWS.enabled:=false;
     LabCELLROWS1.enabled:=false;

     CheckEnter2Tab.enabled:=false;
     CheckcellHInt.enabled:=false;

     Check3dVInner.enabled:=false;
     Check3dVOuter.enabled:=false;
     Check3dHInner.enabled:=false;
     Check3dHOuter.enabled:=false;
  end;


  if (sgfields.cells[0,sgfields.row]='-1')
     or
    (Owner as tdbGridSaver).IsInserted(strtoint(sgfields.cells[0,sgfields.row]))
  then BitBtnDelete.Enabled:=true
  else BitBtnDelete.Enabled:=false;

end;

procedure TColEditForm.SGFieldsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if ACol=3 then begin
    if (Value<>'') then
      try
        strtoint(Value);
      except
        bvMessageError(StrWhatNumber);
      end;
  end;
end;

procedure TColEditForm.SGFieldsSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  if Col in [0,1,4,5,6,7,8,9] then begin
     CanSelect:=false;
  end;
  if Canselect and (row>0) then begin
    try
      if (sgfields.cells[0,sgfields.row]='-1')
         or
         (Owner as tdbGridSaver).IsInserted(strtoint(sgfields.cells[0,Row]))
      then BitBtnDelete.Enabled:=true
      else BitBtnDelete.Enabled:=false;
    except
    end;
//    BitBtnDelete.Enabled:=true;
  end;
end;

procedure TColEditForm.BBDefaultClick(Sender: TObject);
begin
  if (Owner is TDBGridSaver) then (Owner as TDBGridSaver).SetDefault;
  bvMessage(StrFutureSaving);
end;

procedure TColEditForm.BitBtnOkClick(Sender: TObject);
var i:integer;
    thItem:TCollectionItem;
begin
//    procedure SetValues(ID:integer;Nomer:integer;FieldName:string;ColumnName:String;
//                                  Width:integer;pVisible:boolean;AlignMent:TAlignMent;ReadOnly:boolean;
//                                  Title_AlignMent:TAlignMent;Color:TColor);

   if EditColCount.Enabled
   then (Owner as tdbGridSaver).FixedCols:=
      {$ifndef LINUX}
      EditColCount.Position;
      {$else}
      EditColCount.value;
      {$endif}

   (Owner as tdbGridSaver).Indicator:=CheckIndicator.down;
   (Owner as tdbGridSaver).TabStop:=CheckTabStop.down;
   (Owner as tdbGridSaver).AlwaysShowEditor:=CheckAlwaysShowEditor.down;
   (Owner as tdbGridSaver).AlwaysShowSelection:=CheckAlwaysShowSElection.down;
   (Owner as tdbGridSaver).RowLines:=CheckHorzLine.down;
   (Owner as tdbGridSaver).ColLines:=CheckVertLine.down;
   (Owner as tDBGridSaver).MultiSelect:=CheckMultiSelect.down;
   (Owner as tdbGridSaver).CheckTitle:=CheckTitle.down;
   (Owner as tdbGridSaver).selectRows:=CheckSelectRows.down;

   if CheckGridFixedColor.checked
   then (Owner as tdbGridSaver).Color:=clBtnFace
   else (Owner as tdbGridSaver).Color:=sgFields.Color;

   (Owner as tdbGridSaver).FixedColor:=sgFields.FixedColor;
   (Owner as tdbGridSaver).Font:=sgFields.Font;
   (Owner as tdbGridSaver).titleFont:=thtitleFont;
   (Owner as TdbGridSAver).StrippedRows:=
      {$ifndef LINUX}
      EditStrippedrows.position;
      {$else}
      EditStrippedrows.value;
      {$endif}
   (Owner as TdbGridSAver).StrippedColor:=EditStrippedColor.Brush.color;

   if (Owner as TdbGridSAver).ThisGrid is TBVDBGrid then begin
      (Owner as tdbgridSaver).TitleMinHeight:=
         {$ifndef LINUX}
         EditTitleMinHeight.Position;
         {$else}
         EditTitleMinHeight.value;
         {$endif}
      (Owner as tdbgridSaver).CELLROWS:=
         {$ifndef LINUX}
         EditCELLROWS.position;
         {$else}
         EditCELLROWS.value;
         {$endif}

      (Owner as tdbgridSaver).Enter2Tab:=CheckEnter2Tab.Checked;
      (Owner as tdbgridSaver).CellHINT:=CheckCellHint.down;

      if check3dVInner.down then (Owner as tdbgridSaver).Lines3dV:=c3dInner
      else if Check3dVOuter.down then (Owner as tdbgridSaver).Lines3dV:=c3dOuter
      else (Owner as tdbgridSaver).Lines3dV:=c3dNONE;

      if check3dHInner.down then (Owner as tdbgridSaver).Lines3dH:=c3dInner
      else if Check3dHOuter.down then (Owner as tdbgridSaver).Lines3dH:=c3dOuter
      else (Owner as tdbgridSaver).Lines3dH:=c3dNONE;
   end;

   if CheckReadOnly.Enabled
   then (Owner as tdbGridSaver).ReadOnly:=CheckReadOnly.checked;

   for i:=1 to SGFields.RowCount-1 do begin

//    procedure SetValues(ID:integer;Nomer:integer;FieldName:string;ColumnName:String;
//    Width:integer;pVisible:boolean;AlignMent:TAlignMent;ReadOnly:boolean;
//    Title_AlignMent:TAlignMent;Color:TColor);

     (Owner as TDBGridSaver).SetValues( max(0,strtoint(sgfields.Cells[0,i])),
        i-1,
        SGFields.Cells[1,i],
        SGFields.Cells[2,i],
        max(0,strtoint(SGFields.Cells[3,i])),
        not (SGFields.Cells[4,i]=BoolFalseText),
        GetAlignValue(SGFields.Cells[5,i]),
        (SGFields.Cells[6,i]=BoolTrueText),
        GetAlignValue(SGFields.Cells[7,i]),
        TColor(strtoint(SGFields.Cells[8,i])),
        sgFields.Color,
        sgFields.objects[9,i] as TFont,
        sgFields.Font,
        (sgfields.cells[0,i]='-1')
         or
         (Owner as TDBGridSaver).IsInserted(max(0,strtoint(sgfields.Cells[0,i])))
        );

   end;

   (Owner as TDBGridSaver).EndValues(SGFields.RowCount-1);

   if Assigned((Owner as TDBGridSaver).inserted) then
   for i:=Low((Owner as TDBGridSaver).inserted)  to High((Owner as TDBGridSaver).Inserted)
   do begin
     thItem:=(Owner as tdbGridSaver).ThisGrid.Columns.FindItemID((Owner as tdbGridSaver).Inserted[i]);
     if assigned(thItem)
     then thItem.free;
   end;

   (Owner as tdbgridsaver).Inserted:=nil;
   (Owner as TDBGridSaver).RestoreGrid;

end;

procedure TColEditForm.SGFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var ThWidth,ThHeight:integer;
const BlW=6;
begin

  if (ACol in [4,6]) and (ARow>0) then begin

//    DrawEdge(Canvas.Handle, Rect, EDGE_SUNKEN, BF_ADJUST);

    //1 sgFields.Canvas.pen.Color:=clBlack;
    //1 sgFields.Canvas.Rectangle(rect.Left,rect.top-1,rect.right+1,rect.bottom+1);

    SGFields.Canvas.Brush.Color:=sgFields.color; //1 clBtnFace;
    sgFields.Canvas.FillRect(rect);

    //1 Frame3D( SGFields.Canvas, Rect, clBtnHighlight, clBtnShadow, 1);

    ThWidth:=Rect.Right-Rect.Left;
    ThHeight:=Rect.Bottom-Rect.Top;
    rect.Left:=max(Rect.Left+(ThWidth) div 4,Rect.Left+(ThWidth) div 2 -BlW);
    rect.Right:=min(Rect.Right-(thWidth) div 4,Rect.Right-(Thwidth) div 2+BlW);
    rect.Top:=max(Rect.Top+(ThHeight) div 4,Rect.Top+(ThHeight) div 2-BlW);
    rect.Bottom:=min(Rect.Bottom-(ThHeight) div 4,Rect.Bottom-(thHeight) div 2+BlW);
    SGFields.Canvas.Brush.Color:=SGFields.Color;
    sgFields.Canvas.FillRect(rect);

    Frame3D( SGFields.Canvas, Rect, clBtnShadow,clBtnHighlight,  1);

    if sgFields.cells[ACol,ARow]=BoolTrueText then begin
      sgFields.Canvas.Draw(Rect.Left+(Rect.Right-Rect.Left) div 2-CheckBmp.Width div 2,
                         Rect.Top+(Rect.Bottom-Rect.Top) div 2-CheckBmp.Height div 2,
                         CheckBmp);
    end;
  end
  else if (ACol=8) and (ARow>0) then begin
    SGFields.Canvas.Brush.Color:=SGFields.Color;
    sgFields.Canvas.FillRect(rect);
    sgFields.Canvas.pen.Color:=clBlack;

    REct.Left:=Rect.Left+3;
    Rect.Right:=Rect.Right-3;
    Rect.Top:=Rect.Top+3;
    rect.bottom:=rect.Bottom-3;

    sgFields.Canvas.Rectangle(rect.Left,rect.top,rect.right,rect.bottom);
    SGFields.Canvas.Brush.Color:=SGFields.Color;

    REct.Left:=Rect.Left+1;
    Rect.Right:=Rect.Right-1;
    Rect.Top:=Rect.Top+1;
    rect.bottom:=rect.Bottom-1;

    try
      SGFields.Canvas.Brush.Color:=strtoint(sgFields.cells[ACol,ARow]);
    except
    end;
    sgFields.Canvas.FillRect(rect);
  end
  else if (ACol=9) and (ARow>0) then begin
    try
      SGFields.Canvas.Brush.Color:=strtoint(sgFields.cells[8,ARow]);
    except
    end;
    sgFields.Canvas.FillRect(rect);

     sgfields.canvas.Font:=sgFields.objects[acol,arow] as TFont;
     sgfields.canvas.TextRect( rect,Rect.left+2,Rect.top,StrFont);
//     sgfields.canvas.TextOut( Rect.left,Rect.top,'Шрифт');
  END
end;

procedure TColEditForm.SGFieldsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var thCol,ThRow:integer;
begin
  if Button=mbLeft then begin
      SGFields.MouseToCell(x,y,thCol,thRow);
      if (thCol in [4,6]) and (thRow>0) then begin
//        if thCol=4 then SGFields.col:=3
//        else SGFields.col:=5;
//        sgfields.Row:=ThRow;

        if sgFields.Cells[thCol,thRow]=BoolTrueText
        then sgFields.Cells[thCol,thRow]:=BoolFalseText
        else sgFields.Cells[ThCol,thRow]:=BoolTrueText;

        SGFields.Refresh;
      end
      else if (thRow>0) and (thCol=8)
      then begin
         ColorDialog.Color:=strtoint(sgFields.cells[thCol,thRow]);
         if ColorDialog.execute then begin
           sgFields.cells[thCol,thRow]:=inttostr(cOLORDialog.Color);
           sgFields.Refresh;
         end;

      end
      else if (thRow>0) and (thCol in [5,7]) then begin
         sgfields.Cells[thCol,thRow]:=ExecuteAlignmentDialog(sgFields.Cells[thCol,thRow]);
      end
      else if (thRow>0) and (thCol=9) then begin
         FontDialog.Font:=(sgFields.Objects[thCol,thRow] as TFont);
         if FontDialog.Execute
         then begin
           (sgFields.Objects[thCol,thRow] as TFont).Assign(FontDialog.Font);
           sgfields.Refresh;
         end;
      end
  end
end;

procedure TColEditForm.EditColorClick(Sender: TObject);
var OldColor:TColor;
    i:integer;
begin
  ColorDialog.Color:=SGFields.Color;
  OldColor:=sgFields.Color;
  if ColorDialog.Execute then begin
    sgFields.Color:=ColorDialog.Color;

    for i:=1 to sgFields.rowCount-1 do begin
      try
        if strtoint(SGFields.Cells[8,i])=OldColor
        then sgFields.Cells[8,i]:=inttostr(ColorDialog.Color);
      except
      end;
    end;

  end;
end;

procedure TColEditForm.EditFixedColorClick(Sender: TObject);
begin
  ColorDialog.Color:=SGFields.FixedColor;
  if ColorDialog.Execute then sgFields.FixedColor:=ColorDialog.Color;
end;

procedure TColEditForm.EditFontClick(Sender: TObject);
var i:integer;
    OldFont:tFont;
begin
  FontDialog.Font:=sgFields.Font;
  OldFont:=TFont.Create;
  try
    OldFont.assign(sgFields.Font);

    if FontDialog.Execute then begin
       sgFields.Font:=FontDialog.Font;

       for i:=1 to sgFields.rowCount-1 do begin
         try
           if ((sgfields.Objects[9,i] as TFont).Charset=OldFont.CharSet)
              and
              ((sgfields.Objects[9,i] as TFont).Color=OldFont.Color)
              and
              ((sgfields.Objects[9,i] as TFont).Size=OldFont.Size)
              and
              ((sgfields.Objects[9,i] as TFont).Name=OldFont.Name)
              and
              ((sgfields.Objects[9,i] as TFont).Style=OldFont.Style)
            then begin
              (sgfields.Objects[9,i] as TFont).Charset:=FontDialog.Font.Charset;
              (sgfields.Objects[9,i] as TFont).Color:=FontDialog.Font.Color;
              (sgfields.Objects[9,i] as TFont).Size:=FontDialog.Font.Size;
              (sgfields.Objects[9,i] as TFont).Name:=FontDialog.Font.Name;
              (sgfields.Objects[9,i] as TFont).Style:=FontDialog.Font.style;
            end;
         except
         end;
       end;

    end;
  finally
    OldFont.free;
  end;
end;

procedure TColEditForm.EditTitleFontClick(Sender: TObject);
begin
  FontDialog.Font:=thTitleFont;
  if FontDialog.Execute then thTitleFont.Assign(FontDialog.Font);
end;

procedure TColEditForm.FormDestroy(Sender: TObject);
var i:integer;
begin
//  ThFont.Free;
  thTitleFont.Free;
  for i:=1 to sgFields.rowCount-1 do begin
    if Assigned(sgfields.objects[9,i]) then  sgfields.Objects[9,i].Free;
  end;
end;

procedure TColEditForm.BitBtnDeleteClick(Sender: TObject);
var i,ic,j,jc:integer;
begin
  if bitBtnDelete.enabled and (sgfields.rowcount>1) then begin
    if assigned(sgfields.Objects[9,sgfields.row])
    then begin
       sgfields.Objects[9,sgfields.row].free;
//       sgfields.Objects[9,sgfields.row]:=nil;
    end;
//    sgFields.Row [sgfields.Row].Delet (sgfields.Row);
    ic:=sgFields.RowCount;

    jc:=sgFields.ColCount;

    for i:=sgFields.row to ic-2 do begin
//       sgFields.rows[i].assign(sgFields.Rows[i+1]);
       sgFields.objects[9,i]:=sgFields.objects[9,i+1];
       for j:=0 to jc-1 do sgfields.Cells[j,i]:=sgfields.cells[j,i+1];
    end;

    sgFields.rowcount:=sgFields.RowCount-1;

    sgfields.Refresh;
  end;
end;

procedure TColEditForm.BitBtnInsertClick(Sender: TObject);
var i:integer;
    fc:integer;
begin
  with TNewFieldForm.Create(self) do
  try
    fc:=(self.Owner as tdbGridSaver).ThisGrid.DataSource.DataSet.fieldcount;
    EditField.items.clear;
    for i:=0 to fc-1 do begin
      EditField.items.add((self.Owner as tdbGridSaver).ThisGrid.DataSource.DataSet.fields[i].FieldName);
    end;
    EditField.ItemIndex:=-1;
    if (ShowModal=mrOk) and not (Editfield.Text='')
    then begin
        sgfields.rowcount:=sgFields.rowcount+1;
        sgfields.cells[0,sgfields.rowcount-1]:='-1';
        sgfields.cells[1,sgfields.rowcount-1]:=EditField.text;
        sgfields.cells[2,sgfields.rowcount-1]:=EditField.text;
        sgfields.refresh;
        sgFields.objects[9,sgfields.rowcount-1]:=tfont.create;
        (sgFields.Objects[9,sgfields.rowcount-1] as TFont).Assign(sgFields.font);

///
        SGFields.Cells[3,sgfields.rowcount-1]:=inttostr(bvdbGridSaver.defaultwidth);

        SGFields.Cells[4,sgfields.rowcount-1]:=BoolTrueText;

        SGFields.Cells[5,sgfields.rowcount-1]:=GetAlignStr(taLeftJustify);

        SGFields.Cells[6,sgfields.rowcount-1]:= BoolFalseText;

        SGFields.Cells[7,sgfields.rowcount-1]:=GetAlignStr(taLeftJustify);

        SGFields.Cells[8,sgfields.rowcount-1]:=inttostr(sgFields.Color);

        sgfields.Objects[9,sgfields.rowcount-1]:=TFont.Create;
        (sgFields.Objects[9,sgfields.rowcount-1] as TFont).Assign(sgFields.Font);

        sgfields.Row:=sgfields.rowcount-1;
    end;
  finally
    free;
  end;
end;

procedure TColEditForm.NDeleteClick(Sender: TObject);
begin
  BitBtnDelete.Click
end;

procedure TColEditForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex:=0;
  SElf.caption:=StrEditorCaption;
  Ndelete.caption:=StrDelete;
  NInsert.caption:=StrInsert;
  BitBtnDelete.caption:=Ndelete.caption;
  BitBtnInsert.caption:=NInsert.caption;
  SpeedButtonIncFont.hint:=StrIncFont;
  SpeedButtonDecFont.hint:=StrDecFont;
  BBDefault.caption:=StrByDefault;
  BitBtnCancel.caption:=StrCancel;
  TabSheetOptions.caption:=StrOptions;
  TabSheetColumns.caption:=StrColumns;
  TabSheetStrippedRows.caption:=strStrippedRows;
  LabColCount.caption:=StrFixedCols;
  LabTitleMinheight.Caption:=StrTitleMinHeight;
  LabCellRows.caption:=StrCellRows;
  CheckEnter2Tab.caption:=StrEnter2Tab;
  LabelTitle.caption:=' '+StrTitle+' ';
  LabelContents.caption:=' '+StrContents+' ';
  EditFixedColor.caption:=StrColor;
  EditColor.caption:=StrColor;
  EditFont.caption:=StrFont;
  EditTitleFont.caption:=StrFont;
  CheckGridFixedColor.caption:=StrWindowColor;
  LabStrippedRows.caption:=StrStrippedRows;
  LabelEach.caption:=StrEach;
  LabelY.caption:=StrY;
  LabelColor.caption:=strColor;

  CheckAlwaysShowEditor.hint:=strAlwaysShowEditor;
  CheckAlwaysShowSelection.hint:=StrAlwaysShowSelection;
  CheckCellHint.hint:=StrCellHint;
  CheckEnter2Tab.hint:=StrEnter2Tab;
  CheckHorzLine.hint:=StrHorzLines;
  CheckVertLine.hint:=StrVertLines;
  checkIndicator.hint:=StrIndicator;
  CheckTabStop.hint:=StrTabStop;
  CheckMultiSelect.hint:=StrmultiSelect;
  CheckSElectRows.hint:=StrSelectRows;
  CheckTitle.hint:=StrTitleHint;


  Checkreadonly.caption:=StrREadOnly;
end;

procedure TColEditForm.EditStrippedRowsChange(Sender: TObject);
begin
//  if EditStrippedRows.AsInteger<0 then EditStrippedRows.asinteger:=0
//  else if EditStrippedRows.asinteger=1 then EditStrippedRows.asinteger:=2
end;

procedure TColEditForm.EditStrippedColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color:=EditStrippedColor.Brush.Color;
  if ColorDialog.Execute then EditStrippedColor.brush.color:=ColorDialog.Color;
end;

procedure TColEditForm.SpeedButtonIncFontClick(Sender: TObject);
var i:integer;
begin
  sgFields.Font.Size:=sgFields.Font.size+1;
  thTitleFont.size:=thTitleFont.Size+1;
  for i:=0 to sgFields.rowCount-1 do begin
    if sgFields.objects[9,i] is TFont
    then (sgFields.objects[9,i] as tFont).size:=(sgFields.objects[9,i] as tFont).size+1;
  end;
end;

procedure TColEditForm.SpeedButtonDecFontClick(Sender: TObject);
var i:integer;
begin
  if sgFields.Font.size>1 then sgFields.Font.Size:=sgFields.Font.size-1;
  if thTitleFont.Size>1 then thTitleFont.size:=thTitleFont.Size-1;
  for i:=0 to sgFields.rowCount-1 do begin
    if sgFields.objects[9,i] is TFont
       and ((sgFields.objects[9,i] as tFont).size>1)
    then (sgFields.objects[9,i] as tFont).size:=(sgFields.objects[9,i] as tFont).size-1;
  end;
end;

procedure TColEditForm.Check3dvInnerClick(Sender: TObject);
begin
 if (Sender=Check3dVInner)
    and Check3dVInner.down
 then Check3dVOuter.down:=false
 else if (Sender = check3dVOuter)
    and Check3dVOuter.down
 then Check3dVInner.down:=false;

end;

procedure TColEditForm.Check3dHInnerClick(Sender: TObject);
begin
 if (Sender=Check3dHInner)
    and Check3dHInner.down
 then Check3dHOuter.down:=false
 else if (Sender = check3dHOuter)
    and Check3dHOuter.down
 then Check3dHInner.down:=false;

end;

procedure TColEditForm.NInsertClick(Sender: TObject);
begin
  BitBtnInsert.click;
end;

initialization

  CheckBmp:=TBitmap.Create;
  CheckBMP.loadfromresourcename(Hinstance,'CheckButton');
  checkbmp.transparent:=true;

end.
