unit bvSaveTableGetColumnsUnit;

interface

{$ifdef LINUX}
 ERROR: not compatible with LINUX
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids,DBGrids,dbTables, Db,
  ComCtrls, Menus,shellapi,

{$ifndef VER140} //Delphi 6
  //{$ifndef BCB}

    FileCtrl,

  //{$endif}
{$endif}
  
  bvLocalization;



type
  TSaveTableGetColumnsForm = class(TForm)
    FormGrid: TStringGrid;
    PanelButtons: TPanel;
    Table: TTable;
    Panel2: TPanel;
    LabVersion: TLabel;
    StatusBar: TStatusBar;
    SpeedButtonCancel: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Panel3: TPanel;
    Panel4: TPanel;
    SpeedButtonUp: TSpeedButton;
    Panel5: TPanel;
    SpeedButtonDown: TSpeedButton;
    CheckShowResult: TCheckBox;
    PopupMenu: TPopupMenu;
    NPlus: TMenuItem;
    NMinus: TMenuItem;
    SpeedButtonRestore: TSpeedButton;
    EditVersion: TEdit;
    UpDown1: TUpDown;
    CheckUseBDEDriver: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BitBtnOkClick(Sender: TObject);
    procedure SpeedButtonCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormGridDrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormGridSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure SpeedButtonUpClick(Sender: TObject);
    procedure SpeedButtonDownClick(Sender: TObject);
    procedure FormGridDblClick(Sender: TObject);
    procedure NPlusClick(Sender: TObject);
    procedure SpeedButtonRestoreClick(Sender: TObject);
  private
    { Private declarations }
    //DefWidth:integer;
  public
    { Public declarations }
//    isExcel50:boolean;
    Grid:TDBGrid;
    ThData:TDBDataSet;
    SaverFileName:string;
  end;

var
  SaveTableGetColumnsForm: TSaveTableGetColumnsForm;
  TekCol, TekRow: Integer;

Const TableSaverdir='TableSaver';
      TableSaverEXT='.mem';

procedure CheckIniDir;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

uses bvWaitUnit1,ComObj,bvSaveTableToExcel8Unit,bvSeeTableUnit,bvSeeDocUnit,
  bvMessageUnit, bvBDE;

const FirstCol='A';
      FirstRow=1;


procedure CheckIniDir;
var str:string;
begin
  str:=
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}
           (extractFilePath(Application.ExeName)+TableSaverDir);

  if not directoryexists(str)
  then CreateDir(str);
end;

procedure TSaveTableGetColumnsForm.FormCreate(Sender: TObject);
begin
   Caption:=strSaveColumns;
   NPlus.caption:=StrSelectAll;
   NMinus.caption:=StrSelectNone;
   SpeedButtonCancel.caption:=StrCancel;
   CheckShowResult.caption:=StrShowResult;
   CheckUseBDEDriver.caption:=StrUseBDEDriver;
   SpeedButtonREstore.hint:=StrREstoreProperties;
   LabVersion.caption:=StrVersion;
   table.TableName:='';
   //DefWidth:=Width;

end;

procedure TSaveTableGetColumnsForm.FormShow(Sender: TObject);
var thColumns:integer; VisibleColumns:integer;
    i,iVC:integer;
    ThName:string;
    ThHeight:integer;
begin
  try

    if assigned(Grid) then begin
       VisibleColumns:=0;
       for i:=0 to Grid.Columns.count-1 do
          if Grid.columns[i].visible then inc(VisibleColumns);
       ThColumns:=Grid.Columns.Count;
       FormGrid.RowCount:=visibleColumns+1;
    end
    else begin
       thColumns:=ThData.FieldCount;
       FormGrid.RowCount:=thColumns+1;
    end;


    FormGrid.Cells[1,0]:=StrTitleOfColumn;

    iVC:=0;
    for i:=0 to thColumns-1 do begin
       if assigned(Grid) then begin
         if Grid.Columns[i].Visible then begin
           inc(IVC);
           ThName:=Grid.Columns[i].Title.Caption;
           FormGrid.Objects[0,IVC]:=Grid.Columns[i].Field;
           FormGrid.Objects[1,IVC]:=Grid.Columns[i].Title;
           FormGrid.Cells[0,IVC]:=PlusStr;
           FormGrid.Cells[1,IVC]:=ThName;
         end;
       end
       else begin
         ThName:=ThData.Fields[i].FieldName;
         FormGrid.Objects[0,i+1]:=ThData.fields[i];
         FormGrid.Objects[1,i+1]:=Grid.Fields[i];
         FormGrid.Cells[0,i+1]:=PlusStr;
         FormGrid.Cells[1,i+1]:=ThName;
       end;
  //////////     FormGrid.Cells[2,i+1]:=ThName;
    end;


    if (ExtractFileExt(table.TableName)='') or
       (uppercase(ExtractFileExt(table.TableName))='.DB')
    then
       EditVersion.Text:= inttostr(5)
    else if  (uppercase(ExtractFileExt(table.TableName))='.DBF')
    then
       EditVersion.Text:='0'
    else begin
       EditVersion.Text:='-1';
       EditVersion.Visible:= false;
       LabVersion.Visible:=false;
    end;

    CheckUseBDEDriver.visible:=(uppercase(ExtractFileExt(table.TableName))='.TXT');

    StatusBar.Panels[1].Text:= ' ' + table.TableName; //////////

    ThHeight:={GetSystemmetrics(SM_CYCAPTION)+GetSystemMetrics(SM_CYBORDER)+}
               FormGrid.DefaultRowHeight*(FormGrid.RowCount) +FormGrid.RowCount+5+
               PanelButtons.Height+StatusBar.Height;

    Constraints.MaxHeight:=Screen.Height-GetSystemmetrics(SM_CYCAPTION)-GetSystemMetrics(SM_CYBORDER)-80;
    ClientHeight:=thHeight;
  finally
  end;

end;

procedure TSaveTableGetColumnsForm.FormGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Column,Row: Longint;
begin
  try
    FormGrid.MouseToCell(X, Y, Column,Row);
    If Row > 0 then   FormGrid.Row:=Row;
    TekRow:= Row;
    TekCol:= Column;
  except
  end;
end;

procedure TSaveTableGetColumnsForm.BitBtnOkClick(Sender: TObject);
var i:integer;
    k:integer;
    thWF:TMyWait;
    ThRecC:integer;
    IsControlsDisabled:boolean;
    Str,Str1:string;
//    MSExcel,ThWorkBook,thWorkSheet:OleVariant;
    MSWord:OleVariant;
    RowC:integer;
//    Excel50Sheet:
    varList:TStringList;
    thDataType:TFieldType;
begin
   IsControlsDisabled:=ThData.ControlsDisabled;
   if not IsControlsDisabled then ThData.DisableControls;
   thWF:=tmywait.create(StrSavingTable);
//   Self.Windowstate:=wsMinimized;
   SElf.Hide;
   Application.ProcessMessages;
   try
   try
     if table.active then table.Close;
     table.FieldDefs.Clear;
//     BatchMove.Mappings.Clear;
//     BatchMove.Source:=ThData;

     try
       if (ExtractFileExt(table.TableName)='') or
          (uppercase(ExtractFileExt(table.TableName))='.DB')
          or (uppercase(ExtractFileExt(table.TableName))='.DBF')
          or (uppercase(ExtractFileExt(table.TableName))='.TXT')
             and CheckUseBDEDriver.Checked
       then begin
          if (ExtractFileExt(table.TableName)='') or
             (uppercase(ExtractFileExt(table.TableName))='.DB')
          then
             table.TableType:=ttparadox
          else if  (uppercase(ExtractFileExt(table.TableName))='.DBF')
          then
             table.TableType:=ttdbase
          else if  (uppercase(ExtractFileExt(table.TableName))='.TXT')
          then
             table.TableType:=ttascii;

          for i:=1 to FormGrid.RowCount-1 do
            if (FormGrid.Cells[0,i]=PlusStr)
               and Assigned(FormGrid.objects[0,i])
            then begin
               thDataType:=TField(FormGrid.objects[0,i]).DataType;
               if thDataType=ftAutoInc then thDataType:=ftInteger;
               table.FieldDefs.add( FormGrid.Cells[1,i],thDataType,TField(FormGrid.objects[0,i]).Size,false);
            end;

          if EditVersion.Visible and (strtoint(EditVersion.text) > 0)
            then  Table.TableLevel:= strtoint(EditVersion.text);

          Table.CreateTable;
          Table.Open;

          k:=0;
          for i:=1 to FormGrid.RowCount-1 do
            if (FormGrid.Cells[0,i]=PlusStr)
            then begin
               if Assigned(FormGrid.objects[0,i]) then begin
                 FormGrid.Cells[1,i]:=Table.Fields[k].FieldName;
                 inc(k);
               end;
            end;


          Self.Update;

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=ThData.RecordCount;

          ThData.First;

          k:=0;
          while not ThData.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            table.Append;

            for i:=1 to FormGrid.RowCount-1 do
              if (FormGrid.Cells[0,i]=PlusStr)
                 and Assigned(FormGrid.objects[0,i])
              then begin
                 Table.FieldByName(FormGrid.Cells[1,i]).AsString:=
                   TField(FormGrid.objects[0,i]).AsString;
              end;

            table.Post;
            thData.Next;
          end;

          bvFlush(Table);
          Table.Close;
//          BatchMove.Execute
          if CheckShowResult.checked then begin
//             SElf.WindowState:=wsNormal;
             (TSeeTableForm.Create(Application)).SetTable(Self.Table.TableName);
          end;
       end
       else if (uppercase(ExtractFileExt(table.TableName))='.TXT')
       then begin

           SaveTableToTxt(table.tableName,Formgrid,thData,thWF);

       (*
          AssignFile(ThFile,Table.TableName);
          Rewrite(ThFile);

          Str:='';
          for i:=1 to FormGrid.RowCount-1 do
            if (FormGrid.Cells[0,i]=PlusStr)
               and Assigned(FormGrid.objects[0,i])
            then begin
               if Str>'' then Str:=Str+#9;
               Str:=Str+FormGrid.Cells[1,i];
            end;

          Writeln(ThFile,Str);

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=ThData.RecordCount;

          ThData.First;

          k:=0;
          while not ThData.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            Str:='';

            for i:=1 to FormGrid.RowCount-1 do
              if (FormGrid.Cells[0,i]=PlusStr)
                 and Assigned(FormGrid.objects[0,i])
              then begin
                 if Str>'' then Str:=Str+#9;
                 Str:=Str+TField(FormGrid.objects[0,i]).AsString;
              end;

            Writeln(ThFile,Str);
            thData.Next;
          end;
          CloseFile(Thfile);
//          SElf.WindowState:=wsNormal;
       *)
          if CheckShowResult.Checked then
          with  (TSeeDocForm.Create(Application)) do begin
             Show;
             FileName:=Table.TableName;
             SpButtonOpen.Click;
          end;
       end
       else if (uppercase(ExtractFileExt(table.TableName))='.CSV')
       then begin

          SaveTableToTxt(table.tableName,Formgrid,thData,thWF);
          if CheckShowResult.Checked
          then ShellAPI.ShellExecute(Application.Handle, nil, pchar(table.tablename),nil,nil,sw_SHOWnormal)
          (*
          with  (TSeeDocForm.Create(Application)) do begin
             Show;
             FileName:=Table.TableName;
             SpButtonOpen.Click;
          end;
          *)
       end
       else if  (uppercase(ExtractFileExt(table.TableName))='.XLS')
       then begin
          begin // Excel 8.0
              SaveTableToExcel8( Table.TableName,FormGrid,ThData,ThWF,CheckShowResult.Checked);
          end
       end
       else if  (uppercase(ExtractFileExt(table.TableName))='.DOC')
       then begin
          MSWord:=CreateOleObject('Word.Basic');

          Str:='';

          RoWC:=0;

          for i:=1 to FormGrid.RowCount-1 do
            if (FormGrid.Cells[0,i]=PlusStr)
               and Assigned(FormGrid.objects[0,i])
            then begin
               inc(RowC);
               if Str<>'' then   Str:=Str+#9;
               Str:=Str+FormGrid.Cells[1,i];
            end;


          str:=Str+#13;

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=ThData.RecordCount;

          ThData.First;

          k:=0;
          while not ThData.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            Str1:='';

            for i:=1 to FormGrid.RowCount-1 do
              if (FormGrid.Cells[0,i]=PlusStr)
                 and Assigned(FormGrid.objects[0,i])
              then begin
                 if Str1<>'' then Str1:=Str1+#9;
                 Str1:=Str1+TField(FormGrid.objects[0,i]).AsString;
              end;

            Str:=Str+Str1+#13;

            thData.Next;
          end;

          if CheckShowResult.Checked then  MsWord.AppShow;
          MSWord.FileNew;
          MSWord.Insert(Str);
//          MSWord.LineUp(1, 0);
          MSWord.EditSelectAll;
//          MSWord.TextToTable(ConvertFrom := 2,NumColumns := FormGrid.RowCount);
          MSWord.TextToTable(NumColumns := RowC);
          MSWord.FileSaveAs(Self.Table.TableName);
          if not checkShowResult.Checked then msword.AppClose;
       end
       else begin
         bvMessageError(StrErrorCannotWriteToThisFormat+': '+ExtractFileExt(table.TableName));
         if not Self.Visible then Self.Show;
         exit;
       end;

       /////////// сохранение варианта настроек колонок для потомства //

       if SaverFileName<>'' then
       try
         varList:=TStringList.create;
         try
           VarList.Clear;
           for i:=1 to FormGrid.RowCount-1 do
           if Assigned(FormGrid.objects[0,i])
           then begin



              VarList.Add('Column'
                          +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                          +'Field='+tField(FormGrid.Objects[0,i]).FieldName);
              VarList.Add('Column'
                          +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                          +'Visible='+inttostr(integer(FormGrid.Cells[0,i]=PlusStr)));
              VarList.Add('Column'
                          +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                          +'Title='+FormGrid.cells[1,i]);
           end;

           CheckIniDir;

           VArList.SaveToFile(SaverFileName);
         finally
           VarList.Free;
         end;
       except
       end;



     except
       on e:exception do begin
         bvMessageError(StrErrorCannotSaveData+#13+#13+
                     StrMessage+': '+e.message);
         if not Self.Visible then Self.Show;
         exit;
       end
       else raise;
     end;
     Close;
   finally
     ThWF.free;
//     MessageDlg('  Операция сохранения успешно завершена !!!',
//                 mtInformation, [mbOk], 0);
     if not IsControlsDisabled then ThData.EnableControls;
     Application.ProcessMessages;
   end;
   except
     if not Self.Visible then Self.Show;
//     SElf.WindowState:=wsNormal;
   end;
end;

procedure TSaveTableGetColumnsForm.SpeedButtonCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;


procedure TSaveTableGetColumnsForm.FormResize(Sender: TObject);
begin
  //Width:=DefWidth ;
end;

procedure TSaveTableGetColumnsForm.FormGridDrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
begin  // CheckBox - Width = 16  Column - Fixed !!!
  If (Col = 0) and (Row > 0) then  // CheckBox - рамка
  With FormGrid.Canvas do
  begin
    Brush.Color:= clWindow;
    Pen.Color:= clSilver;
    Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    Pen.Color:= clGray;
    Rectangle(Rect.Left+1, Rect.Top+1, Rect.Right-1, Rect.Bottom-1);
    Pen.Color:= clWhite;
    MoveTo(Rect.Left+2, Rect.Bottom-2);
    LineTo(Rect.Right-2, Rect.Bottom-2);
    LineTo(Rect.Right-2, Rect.Top+1);
  end;

  If (Col = 0) and (Row > 0) and (FormGrid.Cells[0,Row]=PlusStr) then   // Метка
  With FormGrid.Canvas do
  begin
    Pen.Color:= clBlack;
    Brush.Color:= clBlack;
    Ellipse(Rect.Left+6, Rect.Top+5, Rect.Left+12, Rect.Top+11);
  end;

  If (FormGrid.Cells[0,Row]=MinusStr) and (Row > 0) and (Col = 1) then
    With FormGrid.Canvas do
    begin
      Font.Color:=clGray;
      TextOut(Rect.Left+2, Rect.Top+2, FormGrid.Cells[Col,Row]);
    end;
end;

procedure TSaveTableGetColumnsForm.FormGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If (TekRow > 0) and (TekCol = 0) then
  begin
    if FormGrid.Cells[0,FormGrid.row]=PlusStr
    then FormGrid.Cells[0,FormGrid.row]:=MinusStr
    else FormGrid.Cells[0,FormGrid.row]:=PlusStr;
    FormGrid.Refresh;
  end;
end;

procedure TSaveTableGetColumnsForm.FormGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Column,Row: Longint;
begin
  try
    FormGrid.MouseToCell(X, Y, Column,Row);
    if (Row>0) and (Row<FormGrid.RowCount)
    then begin
       if (FormGrid.Objects[1,Row] is TColumnTitle)
       then
          StatusBar.Panels[1].Text:=StrColumn +' : '+(FormGrid.Objects[1,Row] as TColumnTitle).Caption
       else
       if (FormGrid.Objects[1,Row] is TField)
       then
          StatusBar.Panels[1].Text:=StrField+' : '+(FormGrid.Objects[1,Row] as TField).FieldName;
    end;
  except
  end;
end;

procedure TSaveTableGetColumnsForm.FormGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  try
    if (Row>0) and (Row<FormGrid.RowCount)
    then begin
       if (FormGrid.Objects[1,Row] is TColumnTitle)
       then
          StatusBar.Panels[1].Text:=StrColumn+' : '+(FormGrid.Objects[1,Row] as TColumnTitle).Caption
       else
       if (FormGrid.Objects[1,Row] is TField)
       then
          StatusBar.Panels[1].Text:=StrField+' : '+(FormGrid.Objects[1,Row] as TField).FieldName;
    end;
  except
  end;
end;

procedure TSaveTableGetColumnsForm.SpeedButtonUpClick(Sender: TObject);
var ThOBJ1,ThObj2:TObject;
    ThStr1,ThStr2:string;
    ThRow:integer;
begin
   ThRow:=FormGrid.Row;
  if ThRow>1 then
  with FormGrid do begin
      ThObj1:=Objects[0,ThRow-1];
      ThObj2:=Objects[1,ThRow-1];
      ThStr1:=Cells[0,ThRow-1];
      ThStr2:=cells[1,thRow-1];

      Objects[0,THRow-1]:=Objects[0,ThRow];
      Objects[1,ThRow-1]:=Objects[1,ThRow];
      Cells[0,ThRow-1]:=Cells[0,ThRow];
      Cells[1,ThRow-1]:=Cells[1,ThRow];

      Objects[0,ThRow]:=ThObj1;
      Objects[1,ThRow]:=ThObj2;
      Cells[0,ThRow]:=ThStr1;
      Cells[1,ThRow]:=ThStr2;

      Row:=ThRow-1;

      Repaint;

  end;
end;

procedure TSaveTableGetColumnsForm.SpeedButtonDownClick(Sender: TObject);
var ThOBJ1,ThObj2:TObject;
    ThStr1,ThStr2:string;
    ThRow:integer;
begin
  ThRow:=FormGrid.Row;
  if ThRow<formGrid.RowCount-1 then
  with FormGrid do begin
      ThObj1:=Objects[0,ThRow+1];
      ThObj2:=Objects[1,ThRow+1];
      ThStr1:=Cells[0,ThRow+1];
      ThStr2:=cells[1,thRow+1];

      Objects[0,THRow+1]:=Objects[0,ThRow];
      Objects[1,ThRow+1]:=Objects[1,ThRow];
      Cells[0,ThRow+1]:=Cells[0,ThRow];
      Cells[1,ThRow+1]:=Cells[1,ThRow];

      Objects[0,ThRow]:=ThObj1;
      Objects[1,ThRow]:=ThObj2;
      Cells[0,ThRow]:=ThStr1;
      Cells[1,ThRow]:=ThStr2;

      Row:=ThRow+1;

      Repaint;

  end;
end;

procedure TSaveTableGetColumnsForm.FormGridDblClick(Sender: TObject);
begin
  If (TekRow > 0) and (TekCol > 0) then
  begin
    if FormGrid.Cells[0,FormGrid.row]=PlusStr
    then FormGrid.Cells[0,FormGrid.row]:=MinusStr
    else FormGrid.Cells[0,FormGrid.row]:=PlusStr;
    FormGrid.Refresh;
  end;
end;

procedure TSaveTableGetColumnsForm.NPlusClick(Sender: TObject);
var i:integer;
begin
  for i:=1 to formGrid.RowCount-1 do
  begin
     if Sender = NPlus
     then formGrid.Cells[0,i]:=PlusStr
     else FormGrid.Cells[0,i]:=MinusStr;
  end;
  FormGrid.Refresh;
end;

procedure TSaveTableGetColumnsForm.SpeedButtonRestoreClick(Sender: TObject);
var VisibleColumns:integer;
    i,k:integer;
//    Maxheight:integer;
    VarList:TStringList;
    thTitle:string;
    thVisible:boolean;
    thField:string;
    OldStr1,OldStr2:string;
    OldObject1,OldObject2:tObject;
begin
  varList:=TStringList.create;
  try
    varList.Clear;

    if (SaverFileName<>'')
       and FileExists(SaverFilename)
    then
    try
      varList.LoadFromFile(SaverFileName);
      //VarList.Sort;
    except
    end;

    if assigned(Grid) then begin
       VisibleColumns:=0;
       for i:=0 to Grid.Columns.count-1 do
          if Grid.columns[i].visible then inc(VisibleColumns);
       FormGrid.RowCount:=visibleColumns+1;
    end
    else begin
       FormGrid.RowCount:=ThData.FieldCount+1;
    end;



    if VarList.count>0 then
    try
      i:=1;
      while true do begin
          thField:=VarList.Values['Column'
                          +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                          +'Field'];
          if (thField<>'')
          then begin
            thVisible:=boolean(strtoint(
                VarList.Values['Column'
                            +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                            +'Visible']));
            thTitle:=
                VarList.Values['Column'
                            +StringReplace( format('%3d',[i]),' ','0',[rfReplaceAll])
                            +'Title'];

            for k:=i to FormGrid.RowCount-1 do
            if assigned(Formgrid.objects[0,k])
               and (TField(FormGrid.objects[0,k]).FieldName=thField)
            then begin
               if not thVisible then FormGrid.Cells[0,k]:=MinusStr
               else FormGrid.Cells[0,k]:=PlusStr;


               FormGrid.Cells[1,k]:=thTitle;

               if k<>i then begin

                 OldStr1:=FormGrid.Cells[0,i];
                 OldStr2:=FormGrid.cells[1,i];
                 OldObject1:=FormGrid.Objects[0,i];
                 OldObject2:=FormGrid.Objects[1,i];

                 FormGrid.Cells[0,i]:=FormGrid.Cells[0,k];
                 FormGrid.Cells[1,i]:=FormGrid.Cells[1,k];
                 FormGrid.Objects[0,i]:=FormGrid.Objects[0,k];
                 FormGrid.Objects[1,i]:=FormGrid.Objects[1,k];

                 FormGrid.Cells[0,k]:=OldStr1;
                 FormGrid.Cells[1,k]:=OldStr2;
                 FormGrid.Objects[0,k]:=OldObject1;
                 FormGrid.Objects[1,k]:=OldObject2;

               end;

               break;
            end;

            inc(i);

          end
          else break;
      end


    except
    end;

  finally
    VarList.Free;
  end;

end;

end.
