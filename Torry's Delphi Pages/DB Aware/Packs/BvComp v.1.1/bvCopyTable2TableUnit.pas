unit bvCopyTable2TableUnit;

interface

{$ifdef LINUX}
  ERROR: not compatible with LINUX (component ttable is not declared in Kylix)
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,db,
  ExtCtrls, bvFormSaver, StdCtrls, Buttons, Grids, Menus,  dbtables,imglist,
  bvLocalization,dbGrids,FileCtrl;

type
  TCopyTab2TabForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    GridDestination: TStringGrid;
    Splitter1: TSplitter;
    BtnOk: TBitBtn;
    Menu: TPopupMenu;
    NSet: TMenuItem;
    NDelete: TMenuItem;
    Panel2: TPanel;
    GridSource: TStringGrid;
    Memo: TMemo;
    Splitter2: TSplitter;
    LabVar: TLabel;
    EditVarName: TComboBox;
    SpeedButtonGetVar: TSpeedButton;
    NClearAll: TMenuItem;
    SpeedButtonSetVar: TSpeedButton;
    Panel3: TPanel;
    LabFilterFields: TLabel;
    Label2: TLabel;
    EditFilterName: TComboBox;
    EditFilterValue: TComboBox;
    procedure MenuPopup(Sender: TObject);
    procedure GridDestinationDblClick(Sender: TObject);
    procedure NSetClick(Sender: TObject);
    procedure NDeleteClick(Sender: TObject);
    procedure GridDestinationDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure GridDestinationDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure GridSourceStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButtonGetVarClick(Sender: TObject);
    procedure NClearAllClick(Sender: TObject);
    procedure EditVarNameChange(Sender: TObject);
    procedure SpeedButtonSetVarClick(Sender: TObject);
    procedure EditFilterValueChange(Sender: TObject);
    procedure EditFilterNameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    //source,destination:TDataSet;
    thsource:Tdataset;
    procedure RestoreDestinationTitle;
    function IsFieldMapChanged:boolean;
  end;

var
  CopyTab2TabForm: TCopyTab2TabForm;

function CopyTab2Tab(pSource:TDataSet;pDestination:TDBGrid):boolean;


implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

uses bvCursorUnit, bvWaitUnit1, bvMessageUnit, bvBDE, bvUtils,
  bvStringUtils, bvFiles, bvConfirmUnit;

type
  TDragTabObject = class(TDragControlObject)
  private
  protected
    DragImageList : TDragImageList;

    //function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages : TDragImageList; override;
  public
    Text:string;
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

constructor TDragTabObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  //DragtargetPos:=point(40,40);
  DragImageList := TDragImageList.Create(AControl);

end;

destructor TDragTabObject.Destroy;
begin
  DragImageList.Free;
  inherited;
end;

{function TDragTabObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := crHandPoint
  else
    Result := crNoDrop;
end;
}


function TDragTabObject.GetDragImages : TDragImageList;
var    DragBmp : TBitmap;
//       str:string;
begin
  if DragImageList.Count <= 0
  then
  with DragImageList do
  begin
    Clear;
    DragBmp := TBitmap.Create;
    try
      with DragBmp do
      begin

        //transparent:=true;
        canvas.font:=(self.control as TStringGrid).font;
        canvas.brush.color:=clWHITE;
        width  := canvas.TextWidth(Self.text)+26;
        height := canvas.TextHeight(Self.text)+6;
        //canvas.Rectangle(RECT(0,0,width,height));
        DragImageList.width:=width;
        dragImageList.height:=height;
        canvas.FillRect(RECT(0,0,width,height));
        canvas.TextOut( 23,3,self.text);
        { Вариант рисования самого перетаскиваемого WinControl'а
             в качестве перетаскиваемого объекта }
        //(Control as TWinControl).PaintTo(Canvas.Handle, 0, 0);
      end;
      DragImageList.AddMasked(DragBMP,clWhite); // bvsoft - просто проверил, в чем разница
      //DragImageList.add(DragBMP,nil); // bvsoft - просто проверил, в чем разница


      //DrawingStyle:=dstransparent;
      //DragImageList.SetDragImage( 0,-200,0);
    finally
      DragBMP.free
    end;
  end;

  Result := DragImageList;
end;



function CheckDir:string;
begin
  Result:=includetrailingbackslash(extractfilepath(application.exename))+'Tab2Tab\';
  if not DirectoryExists(Result)
  then CreateDir(Result);
end;

function CopyTab2Tab(pSource:TDataSet;pDestination:TDBGrid):boolean;
var i,k:integer;
    DestCd,SourcCd:boolean;
    DestClosed,SourcClosed:boolean;
    REcC:integer;
    FC:integer;
    Str:string;
    Field,Field1:Tfield;
    CalcFunc:procedure(DataSet: TDataSet) of object;

    destTable:TTable;

begin
  REsult:=false;

  if not assigned(pSource)
     or not assigned(pDestination)
     or not Assigned(pDestination.datasource)
     or not assigned(pDestination.datasource.dataset)
     or not (pDestination.datasource.dataset is ttable)
     or (pDestination.datasource.dataset as TTable).readonly
  then exit;

  DestTable:=pDestination.datasource.dataset as ttable;

  with TCopyTab2TAbForm.create(Application) do
  try
    thSource:=pSource;

    //thList:=TStringList.create;
    try

      DestClosed:=not destTable.active;
      SourcClosed:=not pSource.active;
      DestCd:=destTAble.ControlsDisabled;
      SourcCd:=pSource.ControlsDisabled;
      Calcfunc:=DestTable.OnCalcFields;

      try

        pSource.Open;
        desttable.open;

        if not SourcCD then  psource.disablecontrols;
        if not DestCD  then  desttable.disablecontrols;

        destTable.onCalcFields:=nil;




        with TWaitCursor.create do
        try

          FC:=0;
          for i:=0 to pdestination.Columns.Count-1 do
            if assigned(pDestination.columns[i].field)
               and pDestination.columns[i].visible
               and (pDestination.columns[i].field.fieldkind=fkData)
            then inc(FC);

          GridSource.RowCount:=psource.fieldcount+1;

          GridDestination.RowCount:=FC+1;

          gridsource.Cells[0,0]:=StrFieldsInSource;


          k:=0;
          for i:=0 to pSource.Fieldcount-1 do
          begin
             if pSource.fields[i].visible then begin
               GridSource.Cells[0,k+1]:=pSource.Fields[k].fieldName;
               inc(k);
             end;
          end;

          RestoreDestinationTitle;

          {
          try
            if FileExists(IniName)
            then thList.LoadFromFile(IniName);
          except
            thList.clear;
          end;
          }

          fc:=0;
          for i:=1 to pDestination.columns.count do
          begin
             if assigned(pdestination.columns[i-1].field)
                and pdestination.columns[i-1].visible
                and (pDestination.columns[i-1].field.fieldKind=fkData)
             then begin
                inc(FC);

                if pDestination.columns[i-1].title.caption<>''
                then GridDestination.Cells[0,FC]:=pDestination.columns[i-1].Title.Caption //DisplayName
                else GridDestination.Cells[0,FC]:=pDestination.columns[i-1].field.fieldName;

                GridDestination.Objects[1,fc]:=pDestination.columns[i-1].field;

                {str1:=thList.Values[GridDestination.cells[0,FC]];
                if (Str1<>'') and (pSource.FindField(str1)<>nil)
                   and (pSource.fieldbyName(str1).visible)
                then  GridDestination.Cells[1,FC]:=str1;}
             end;
          end;
        finally
         free
        end;

        EditfilterName.clear;
        EditFilterName.items.add('');
        if assigned(thSource) then
        for i:=0 to thSource.FieldCount-1 do begin
           if thSource.fields[i].FieldKind=fkdata
           then EditFilterName.Items.add(thsource.fields[i].fieldname);
        end;
        //editFilterName.Itemindex:=0;

        while ShowModal=mrOk do
        with TMywait.create(StrCopying) do
        try
           try
             UpdateScreen;
             bvExecuteQuery(desttable.databaseName,'delete from '+Quotedstr(desttable.tableName));
             //PDestination.EmptyTable;

             ShowProgress;

             RecC:=PSource.RecordCount;
             FC:=GridDestination.Rowcount-1;
             PSource.First;
             while not PSource.eof do
             begin
                ProgressBarPos:=PSource.Recno*100 div RecC;

                desttable.append;
                for i:=1 to FC do begin

                   str:=trim(GridDestination.Cells[1,i]);
                   if str<>'' then begin
                      Field:=PSource.FindField(str);
                      if assigned(Field)
                         and not Field.IsNULL
                         or not assigned(field)
                      then begin
                          Field1:=TField(GridDestination.Objects[1,i]);
                          try
                            if (Field1 is TFloatField)
                            then begin
                              if assigned(Field) then Field1.asFloat:=StrToFloatprotected(field.asstring)
                              else Field1.asFloat:=StrToFloatprotected(str)
                            end
                            else if Field1 is TDateField
                            then begin
                              if assigned(Field) then Field1.asDateTime:=StrToDateProtected(Field.asstring)
                              else Field1.asDateTime:=StrToDateProtected(str)
                            end
                            else begin
                              if assigned(Field) then Field1.asstring:=Field.asString
                              else Field1.asstring:=Str;
                            end;
                          except
                            // no error!
                          end;
                      end;
                   end;
                end;
                desttable.post;

                PSource.next;
             end;

             bvFlush(desttable);

             if (trim(EditVarName.text)<>'')
                and isFieldMapChanged
                and (GetConfirmSmall(StrConfirmChangesInFieldMap)=mrOk)
             then begin
                SpeedButtonSetVar.click;
             end;

           except
             on e:exception do begin
                 bvMessageerror(StrError+#13+E.message);
                 continue
             end
           end;
           REsult:=true;
           break;
        finally
          Free
        end;


      finally
        pSource.active:=not SourcClosed;
        desttable.active:=not DestClosed;
        if not sourcCD then  pSource.EnableControls;
        if not DestCD then desttable.EnableControls;
        desttable.onCalcFields:=CalcFunc;
      end;
    finally
      //thList.free
    end;
  finally
     free
  end;
end;

function TCopyTab2TabForm.IsFieldMapChanged:boolean;
var i:integer;
begin
  Result:=false;
  for i:=1 to GridDestination.RowCount-1 do
  begin
     if GridDestination.Cells[1,i]<>GridDestination.Cells[2,i]
     then begin
       Result:=true;
       break;
     end;
  end;
end;

procedure TCopyTab2TabForm.MenuPopup(Sender: TObject);
begin
  NDelete.enabled:=Menu.PopupComponent=GridDestination
end;

procedure TCopyTab2TabForm.GridDestinationDblClick(Sender: TObject);
begin
  NSet.Click
end;

procedure TCopyTab2TabForm.NSetClick(Sender: TObject);
begin
   GridDestination.Cells[1,GridDestination.row]:=GridSource.cells[0,GridSource.Row];
end;

procedure TCopyTab2TabForm.NDeleteClick(Sender: TObject);
begin
   GridDestination.Cells[1,GridDestination.row]:='';
end;

procedure TCopyTab2TabForm.GridDestinationDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var ARow,ACol:integer;
begin
   GridDestination.MouseToCell(x,y,ACol,ARow);
   if ARow>0 then begin
      GridDestination.Cells[1,ARow]:= (source as TDRAGTABObject).text;
      GridDestination.row:=ARow;
   end
end;

procedure TCopyTab2TabForm.GridDestinationDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TCopyTab2TabForm.GridSourceStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
    thPoint:TPoint;
    ACOL,AROW:integer;
    i:integer;
    REPCount:integer;
begin

  GetCursorPos(thPoint);
  thPoint:=GridSource.ScreenToClient(thPoint);
  GridSource.MouseToCell( thPoint.x,thpoint.y, ACOL,AROW);
  if AROW<=0 then AROW:=1;

  for i:=0 to self.controlcount-1 do begin
    //include(self.controls[i].controlStyle,csDisplayDragImage);
    self.controls[i].controlStyle:=self.controls[i].controlStyle+[csDisplayDragImage];
  end;

  DragObject:=TDragTabObject.create(TControl(sender));
  (DragObject as TDRAGTABOBJECT).Text:=GridSource.Cells[0,AROW];

   if assigned(thSource)
      and thsource.active
      and (thSource.findfield(GridSource.cells[ACOL,AROW])<>nil)
   then begin
      thsource.disablecontrols;
      Memo.Clear;
      try
        thsource.first;
        RepCount:=0;

        while (RepCount<10) and not thSource.eof do begin
           if thSource.fieldbyName(gridSource.cells[ACOL,AROW]).asstring<>''
           then begin
             inc(Repcount);
             Memo.Lines.add(thSource.fieldbyname(gridSource.cells[ACOL,AROW]).asstring);
           end;

           thSource.next;
        end;

        memo.SelStart:=0;
        memo.sellength:=0;

      finally
        thSource.enablecontrols;
      end;
   end;

end;

procedure TCopyTab2TabForm.FormCreate(Sender: TObject);
begin
  self.caption:=strCopyTab2TAbCaption;
  BtnOk.caption:=StrAccept;
  Nset.caption:=StrCompSelCols;

  NDelete.caption:=StrUnCompSelCols;
  NClearAll.caption:=StrClearAll;
  LabVar.caption:=StrVar;
  LabFilterFields.caption:=StrFilter+':';

  thsource:=nil;
  restoreform(self);

  GridDestination.ColWidths[2]:=0;

  GetFileList(CheckDir,EditVarName.items);

  SpeedButtonGetVar.Hint:=StrGetVarCaption;
  SpeedButtonSetVar.hint:=StrSetVarCaption;
end;

procedure TCopyTab2TabForm.FormDestroy(Sender: TObject);
begin
   saveform(self);
end;

procedure TCopyTab2TabForm.SpeedButtonGetVarClick(Sender: TObject);
var i:integer;
    str1:string;

    thList:TStringList;
begin
  thList:=TStringList.create;
  try
    thList.clear;
    if (trim(EditVarName.text)='')
    then //bvMessageWarning(StrErrorSetupNotIndicated)
    else
    if  not FileExists(CheckDir+trim(EditVarName.text))
    then //bvMessageWarning(StrErrorSetupNotFound)
    else
    try
      thList.LoadFromFile(CheckDir+trim(EditVarName.text));
    except
      thList.clear;
    end;

    if thList.count>0 then begin
       NClearAll.click;
       for i:=1 to GridDestination.RowCount-1 do begin
          Griddestination.cells[2,i]:='';
          str1:=thList.Values[GridDestination.cells[0,i]];
          if (Str1<>'') {and (GridSource.Cols[0].IndexOf(str1)>=0)}
          then  begin
             GridDestination.Cells[1,i]:=str1;
             Griddestination.cells[2,i]:=Str1;
          end;
       end;
    end;
  finally
    thList.free
  end;
end;

procedure TCopyTab2TabForm.NClearAllClick(Sender: TObject);
var i:integer;
begin
   for i:=1 to GridDestination.Rowcount-1 do begin
     gridDestination.Cells[1,i]:='';
   end;
end;

procedure TCopyTab2TabForm.restoreDestinationTitle;
begin
   GridDestination.cells[0,0]:=StrFieldsInDestination;
   GridDestination.cells[1,0]:=StrChoiseFields;
end;

procedure TCopyTab2TabForm.EditVarNameChange(Sender: TObject);
begin
  SpeedButtonGetVar.Click;
end;

procedure TCopyTab2TabForm.SpeedButtonSetVarClick(Sender: TObject);
var thList:TStringList;
    i:integer;
begin
   thList:=TstringList.create;
   try
     if trim(EditVarName.text)<>'' then
     try
       thList.clear;
       for i:=1 to gridDestination.RowCount-1 do
       begin
          if GridDestination.cells[1,i]<>''
          then thList.Values[GridDestination.cells[0,i]]:=Griddestination.cells[1,i];
          GridDestination.cells[2,i]:=GridDestination.Cells[1,i];
       end;

       thList.savetofile(CheckDir+trim(EditVarName.text));
     except
     end;
   finally
     thList.clear;
   end;
end;

procedure TCopyTab2TabForm.EditFilterValueChange(Sender: TObject);
begin
  if (EditFilterName.ItemIndex<1)
     or (trim(EditFilterValue.Text)='')
  then thsource.Filtered:=false
  else begin
     thSource.filter:='['+EditFilterName.Text+']='+quotedStr(EditFilterValue.text);
     thSource.filtered:=true;
  end;
end;

procedure TCopyTab2TabForm.EditFilterNameChange(Sender: TObject);
var thF:TField;
begin
   EditFilterValue.Clear;
   EditFilterValueChange(nil);
   if EditFilterName.itemindex>0
   then begin
      //with TQuery.create(self) do
      try

         EditFilterValue.Items.add('');

         if assigned(thSource)
            and thsource.active
            and (thSource.findfield(EditFilterName.text)<>nil)
         then begin
            thsource.disablecontrols;

            try
              thsource.first;
              //RepCount:=0;

              while {(RepCount<10) and} not thSource.eof do begin
                 thF:=thSource.fieldbyName(EditFilterName.text);
                 if (thF.asstring<>'')
                 then begin
                   //inc(Repcount);
                   if EditFilterValue.items.IndexOf(thF.asstring)<0
                   then EditFilterValue.items.Add(thF.asstring);
                 end;

                 thSource.next;
              end;
            finally
              thSource.enablecontrols;
            end;
         end;


      finally
         //free
      end;
   end;
end;

end.
