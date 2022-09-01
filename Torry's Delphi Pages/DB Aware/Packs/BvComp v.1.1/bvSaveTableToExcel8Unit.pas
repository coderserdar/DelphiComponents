unit bvSaveTableToExcel8Unit;

interface

uses forms,Grids,DBTables,
{$ifdef VER140} // Delphi 6
     variants,
{$else}
   {$ifdef VER130} // Delphi 5
      {$ifdef BCB} // CBUILDER 5
          //excel_97,
      {$else}
          //excel97,
      {$endif}
   {$else}
   {$endif}
{$ENDIF}
     comobj,
     bvWaitUnit1,bvbde,bvstringutils,bvFiles,
     bvLocalization;

procedure SaveTableToExcel8(FileName:string;FormGrid:TStringGrid;ThData:TDBDataSet;WF:TMyWait;ShowResult:boolean);


implementation

uses windows,DB,dialogs,
     bvSaveTableGetColumnsUnit,SysUtils,ShellApi;

const
  xlDelimited = $00000001;
  xlWindows = $00000002;
  xlDoubleQuote = $00000001;
  xlNormal = $FFFFEFD1;
  xlExclusive = $00000003;
  xlUserResolution = $00000001;
  xlExcel9795 = 43;

procedure SaveTableToExcel8(FileName:string;FormGrid:TStringGrid;ThData:TDBDataSet;WF:TMyWait;ShowResult:boolean);
var ExcelApp:OLEVariant;
    i,RowC{ColC,}{ThRecC,k}:integer;
    NeedShow:boolean;
    thRowCount:integer;
    DSOpened:boolean;
    txttabname:string;
    //s:string;
    thField:TField;


    procedure SetFType(Row:integer;FieldType:TFieldType);
    begin
        case FieldType of
              ftString,ftMemo,ftWideString
                ,//: ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='@';
              ftWord,ftSmallint,ftInteger,ftautoInc,ftLargeint:
                ExcelApp.ActiveCell.offset[0,Row].EntireColumn.NumberFormat:='0';
              ftCurrency:
                ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='#0,00';
              //ftFloat:
              //  ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='General';
{             ftDate:
                ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='dd/mm/yy';
              ftTime:
                ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='hh:mm';
              ftDateTime:
                ExcelApp.ActiveCell.offset[0,row].EntireColumn.NumberFormat:='dd/mm/yyyy  hh:mm';
}
        end;
    end;
begin

  FileName:=DeleteQuoted(FileName);
  txttabname:=GetTempDir+extractfilename(FileName);
  Delete( txttabname,pos(extractFileExt( txttabname ),txttabname),10);
  txttabname:=txttabname+'.TXT';

  SAveTAbleToTXT( txttabname,FormGrid,ThData,WF);

  if not fileexists(txttabname) then exit;


  ExcelApp:=createoleobject('Excel.Application'); //  TExcelApplication.Create(Application);
  //ShowMessage(ExcelApp.GetVersion);
  NeedShow:=false;
  DSOpened:=thData.active;

  try
  try
    //1 ExcelApp.AutoConnect:=true;
    try
      ExcelApp.ScreenUpdating:=false;
    except
      on e:exception do begin
         e.Message:=e.message+#13+'(ExcelApp.ScreenUpdating)';
         raise;
      end;
    end;

    try
      ExcelApp.visible:=false;
    except
      on e:exception do begin
         e.Message:=e.message+#13+'(ExcelApp.visible)';
         raise
      end;
    end;
    //ExcelApp.Connect;
    //ExcelApp.AutoQuit:=true;

    try
      ExcelApp.Caption:=ExcelApp.Caption+'('+StrOpenedFrom+' '+forms.Application.ExeName+')';
    except
      on e:exception do begin
         e.Message:=e.message+#13+'(ExcelApp.Caption)';
         raise
      end;
    end;

    thData.active:=true;


     ////  новое

     if assigned(wf) then wf.DopText:=StrSavingToExcel;

     //ExcelApp.Workbooks.Open( DelKav(txttabname),0,FALSE,1,'','',true,xlWindows,' ',true,false,null,false,0);

     if assigned(wf) then begin
        wf.ShowProgress;
        wf.ProgressbarPos:=0;
     end;

     try
       ExcelApp.Workbooks.OpenText( deletequoted(txttabname),xlWindows,1,xlDelimited,
           xlDoubleQuote,False,True,False,False ,False,False );//,' ',NULL,0 ,0);


    {
    97
    procedure OpenText(const Filename: WideString;
                       Origin: OleVariant;
                       StartRow: OleVariant;
                       DataType: OleVariant;
                       TextQualifier: XlTextQualifier;

                       ConsecutiveDelimiter: OleVariant;
                       Tab: OleVariant;
                       Semicolon: OleVariant;
                       Comma: OleVariant;
                       Space: OleVariant;

                       Other: OleVariant;
                       OtherChar: OleVariant;
                       FieldInfo: OleVariant;

                       TextVisualLayout: OleVariant;

                       lcid: Integer);

    2000
    procedure OpenText (const Filename: WideString;
                        Origin: OleVariant;
                        StartRow: OleVariant;
                        DataType: OleVariant;
                        TextQualifier: XlTextQualifier;

                        ConsecutiveDelimiter: OleVariant;
                        Tab: OleVariant;
                        Semicolon: OleVariant;
                        Comma: OleVariant;
                        Space: OleVariant;

                        Other: OleVariant;
                        OtherChar: OleVariant;
                        FieldInfo: OleVariant;

                        TextVisualLayout: OleVariant;

                        DecimalSeparator: OleVariant;
                        ThousandsSeparator: OleVariant;

                        lcid: Integer);
    }

     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Workbooks.OpenText)';
          raise
       end;
     end;


     if assigned(wf) then begin
        wf.ProgressbarPos:=30;
     end;


     try
       ExcelApp.Range['A2','A2'].Select;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Range[A2,A2].Select)';
          raise
       end;
     end;

     try
       ExcelApp.ActiveWindow.FreezePanes:=true;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.ActiveWindow.FreezePanes)';
          raise
       end;
     end;

     if assigned(wf) then begin
        wf.ProgressbarPos:=40;
     end;

     //ExcelApp.cells.Rows[1,1].select;

     //ExcelApp.ActiveCell.offset[0,0].Rows[1,1].select;            //Range['A1','A'+  // Row.:=Select; //  Range['A2','A2'].Select;

     try
       ExcelApp.Range['A1','A1'].Select;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Range[A1,A1].Select)';
          raise
       end;
     end;

     try
       ExcelApp.activecell.EntireRow.Font.bold:=true;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.activecell.EntireRow.Font.bold)';
          raise
       end;
     end;

     if assigned(wf) then begin
        wf.ProgressbarPos:=50;
     end;

     //ExcelApp.cells.END_[xltoRight].select;
     //ColC:=ExcelApp.activecell.Column;


{
     s:=chr(ord('A')+i)+'1';
     ExcelApp.Cells.Range['A1',s].Font.Bold:=true;
}

     try
       ExcelApp.Cells.ShrinkToFit:=true;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Cells.ShrinkToFit)';
          raise
       end;
     end;

     if assigned(wf) then begin
        wf.ProgressbarPos:=65;
     end;

     //ExcelApp.cells.select;
     try
       ExcelApp.Columns.AutoFit;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Columns.AutoFit)';
          raise
       end;
     end;

     if assigned(wf) then begin
        wf.ProgressbarPos:=80;
     end;

     if Assigned(FormGrid) then thRowCount:=FormGrid.RowCount
     else thRowCount:=thData.fieldCount;

     try
        ExcelApp.Range['A1','A1'].Select;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Range[A1,A1].Select)';
          raise
       end;
     end;

     RowC:=0;
     for i:=0 to thRowCount-1 do begin
        if assigned(FormGrid) then begin
           if (FormGrid.Cells[0,i]=PlusStr)
              and Assigned(FormGrid.objects[0,i])
           then begin
              thField:=TField(FormGrid.objects[0,i]);
              try
                SetFType(RowC,thField.datatype);
              except
              end;
              inc(RowC);
           end
           //else thField:=nil;
        end
        else begin
           thField:=thDAta.Fields[i];
           if assigned(thField) then
           try
              SetFType(i,thField.dataType);
           except
           end;
        end;
     end;

     try
        ExcelApp.Range['A1','A1'].Select;
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.Range[A1,A1].Select)';
          raise
       end;
     end;

     //ExcelApp.Columns['A','A'].select;
     //ExcelApp.activecell.Columns['A','A'].select;

     //ExcelApp.ActiveCell.Rows[1,1].select;  //Font.Bold:=true;

     //ExcelApp.selection[0].;

     if assigned(wf) then begin
        wf.ProgressbarPos:=90;
     end;

     if FileExists(FileName) then DeleteFile(FileName);


     try
       ExcelApp.ActiveWorkbook.SaveAs( Filename,xlExcel9795,
            '','',false,false,
            xlExclusive,xlUserResolution,
            False,0,0);
     except
       on e:exception do begin
          e.Message:=e.message+#13+'(ExcelApp.ActiveWorkbook.SaveAs)';
          raise
       end;
     end;

     {
     app.ActiveWorkbook.SaveAs( Filename,xlNormal,'','',False,False,
             xlExclusive,xlUserResolution, false,0,0,0);
     }
     if assigned(wf) then begin
        wf.ProgressbarPos:=100;
     end;

  except
    NeedShow:=true;
    raise;
  end
  finally
     try
       thData.active:=dsOpened;

       try
         ExcelApp.ScreenUpdating:=true;
       except
         on e:exception do begin
            e.Message:=e.message+#13+'(ExcelApp.ScreenUpdating)';
            raise
         end;
       end;

       if NeedShow or ShowResult then begin
          //1 ExcelApp.Autoconnect:=false;
          //ExcelApp.AutoQuit:=false;
          try
             ExcelApp.visible:=true;
          except
            on e:exception do begin
               e.Message:=e.message+#13+'(ExcelApp.visible)';
               raise
            end;
          end;
       end
       else begin
          try
            ExcelApp.ActiveWorkbook.Close(false); //,AnsiString(Filename),boolean(false),integer(0));
          except
            on e:exception do begin
               e.Message:=e.message+#13+'(ExcelApp.ActiveWorkbook.Close)';
               raise
            end;
          end;

          //ExcelApp.ActiveWorkbook.Close( boolean(false),AnsiString(Filename),boolean(false),integer(0));
          //procedure Close(SaveChanges, Filename, RouteWorkbook: OleVariant; lcid: Integer); safecall;
          try
             excelapp.quit;
          except
            on e:exception do begin
               e.Message:=e.message+#13+'(excelapp.quit)';
               raise
            end;
          end;
       end;
     finally
     end;
     (*
     if not needShow and ShowResult then
          ShellExecute( Forms.Application.Handle, nil, pchar(FileName), nil,
          nil, SW_MAXIMIZE);
     *)
  end;

end;


end.
