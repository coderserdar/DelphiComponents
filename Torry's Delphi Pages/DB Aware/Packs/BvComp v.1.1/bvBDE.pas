unit bvBDE;

interface


uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Buttons,ExtCtrls,
  DB,BDE,dbTables,
{$else}
  ERROR: NOT LINUX COMPATIBLE
{$endif}

  SysUtils,
  classes,
  bvConfirmUnit,
  math,bvwaitunit1,Grids,

{$ifndef VER140} //Delphi 6
    FileCtrl,
{$endif}

    bvLocalization;


const PlusStr=' v';
      MinusStr='';

const DefaultFileName='bv_';

const DefaultDatabaseName:string='';

function bvLockTable(Table:TTable;TimeOut:integer;ConfirmExit:boolean):boolean;
procedure bvUnlockTable(Table:TTable);

function TableExists(Table:TTable):boolean; overload;
function TableExists(pTable:string;pDatabaseName:string=''):boolean; overload;
procedure DeleteTable(Table:TTable); overload;
procedure Deletetable(ptable:string;pDatabaseName:string=''); overload;
procedure ChangeLevel(Table:TTAble;newlevel:integer); overload;
procedure ChangeLevel(ptable:string;NewLevel:integer;pDatabaseName:string=''); overload;

function GetAllRecordCount(Dataset: TBDEDataSet): LongInt;

procedure TableCopy(ResultName:string;DataSet:TDBDataSet;WF:TMyWait=nil;IDFieldName:string=''); overload;
procedure TableCopy(ResultTable:TTable;DataSet:TdbDataSet;wf:tMyWait=nil;IDFieldName:string=''); overload;
procedure TableCopy(ResultName,Source:string;wf:tMyWait=nil;IDFieldName:string=''); overload;
procedure TableCopy(ResultTable:TTable;Source:string;wf:tMyWait=nil;IDFieldName:string=''); overload;

procedure bvFlush(DataSet:TdbDataSet=nil);

function  BatchMoveToTMP(DATAset:TDBDataSet;p_tableName:string='';WaitPrompt:boolean=false):string; overload;
function  BatchMoveToTMP(pDATABASEName:string;pSQL:string;p_tableName:string='';pSessionName:string='';WaitPrompt:boolean=false):string; overload;

function bvBatchMove(pDestination:TTAble;pSource:TBDEDataSet;AMode:TBAtchMode):integer; overload;
function bvBatchMove(pDestination:TTable;pSource:string;AMode:TBAtchMode;pSrcDatabaseName:string=''):integer; overload;
function bvBatchMove(pDestination:string;pSource:TBDEDataSet;AMode:TBAtchMode;pDescdatabaseName:string=''):integer; overload;
function bvBatchMove(pDestination:string;pSource:string;AMode:TBAtchMode;pdatabaseName:string=''):integer; overload;

function bvBatchAppend(pDestination:TTable;pSource:string;pSrcDatabaseName:string=''):integer; overload;

function BDENumberStr(Value:extended):string;
function BDEDateStr(Value:TDateTime;Quoted:boolean=true;TypeCastString:boolean=true;BDESEPARATOR:boolean=false):string;

function GetRecordCount(pTableName:string;pDatabaseName:string=''):integer;



procedure bvExecuteQuery(pDATABASE:TdAtabase;pSQL:string); overload;
procedure bvExecuteQuery(pDATABASE:string;pSQL:string); overload;
procedure bvExecuteQuery(pSQL:string); overload;
procedure bvExecuteQuery(SessName, pDATABASE:string;pSQL:string); overload;
procedure bvExecuteQuery(SessName:string; pDATABASE:TdAtabase;pSQL:string); overload;


procedure bvFlushAllBuffers;
procedure bvRefreshAll;
procedure bvRefresh;

function RoundSQL(Value:string;n:integer=0):string; //  get sql-text of round(v,n)

procedure SaveTableToTxt(FileName:string;FormGrid:TStringGrid;ThData:TDBDataSet;WF:TMyWait{;ShowResult:boolean});

function GetFieldAsString(dataset:TDataset;FieldName:string):string;


function UniqueTableName(pAlias:string='';pFirstStr:string='';pEXT:string='';pSession:TSession=nil):string;

procedure DeleteNULLs(Table:TTable);

function getDoubleValue(dSet:TDataSet;FName:string):double;
function getStringValue(dSet:TDataSet;FName:string):string;
function getDateValue(dSet:TDataSet;FName:string):TDateTime;
function getIntegerValue(dSet:TDataSet;FName:string):Integer;

function getNextKey(
                  Table:TTAble;
                  IDName:string='ID'):integer; overload;

function getNextKey(
                  Sessionname:string;
                  databaseName:string;
                  TableName:string;
                  IDName:string='ID'):integer; overload;

implementation

uses bvSaveTableToExcel8Unit,ComObj,{IniUnit,}bvMessageUnit,bvFiles,bvStringutils;


function GetFieldAsString(dataset:TDataset;FieldName:string):string;
var thField:TField;
begin
  Result:='';
  if assigned(Dataset) and (FieldName<>'')
  then begin
    thField:=dataset.FindField(FieldName);
    if assigned(thField)
    then result:=thField.asString;
  end;
end;


function UniqueTableName(pAlias:string='';pFirstStr:string='';pEXT:string='';pSession:TSession=nil):string;
var
    List:TStringlist;
    thpath:string;
begin
  List:=TStringList.create;
  try
     if pAlias='' then begin
        pAlias:=DefaultDatabasename;
        //bvsoft - stub
        try
          thPath:=Extractfilepath(Application.exename)+'DBLOCK';
          if not directoryexists(thPath) then CreateDir(thPath);
        finally
        end;
     end
     else thPath:='';

     if not assigned(pSession) then pSession:=Session;

     if pFirstStr='' then pFirstStr:=DefaultFileName;

     pext:=trim(pext);

     if (pext<>'') and (pext[1]<>'.') then pext:='.'+pext;


     pSession.GetTableNames( pAlias,pFirstStr+'*'+pext,false,true,List);

     Randomize;

     repeat

         REsult:= pFirstStr+trim(inttostr(random(99999)));

     until List.IndexOf(REsult)<0;

     REsult:=REsult+pext;

     if thPath<>'' then result:=
       //includetrailingbackslash(thPath)+Result;
     {$ifndef VER140}
     includetrailingbackslash
     {$else}
     includetrailingpathdelimiter
     {$endif}
          (thPath)+Result;

  finally
     List.Free;
  end;

end;


procedure bvExecuteQuery(pDATABASE:string;pSQL:string);
begin
  //bdeutils.bvExecuteQuery(pdatabase,psql);

  bvExecuteQuery('',pDatabase,pSQL);
  {
  with TQuery.create(Application) do
  try
    if trim(pDatabase)='' then pDatabase:=DefaultDatabaseName;
    DatabaseName:=pDATABASE;
    sql.clear;
    sql.add(psql);
    execsql;
  finally
    free
  end;
  }
end;

procedure bvExecuteQuery(pDATABASE:TDatabase;pSQL:string);
begin
  bvbde.bvExecuteQuery(pdatabase.databasename,psql);
end;

procedure bvExecuteQuery(pSQL:string);
begin
  bvbde.bvExecuteQuery(DefaultDatabaseName,psql);
end;

procedure bvExecuteQuery(SessName:string; pDATABASE:TDatabase;pSQL:string);
begin
  bvExecuteQuery(SessName,pdatabase.databasename,psql);
end;

procedure bvExecuteQuery(SessName, pDATABASE:string;pSQL:string);
begin
   if pdatabase='' then pdatabase:=defaultdatabaseName;

   with TQuery.Create(Application) do
   try
     DatabaseName := pDatabase;
     if trim(sessname)<>'' then  SessionName := SessName;
     SQL.Add(pSQL);
     ExecSQL;
   finally
     Free;
  end;

end;


function GetAllRecordCount(Dataset: TBDEDataSet): LongInt;
begin
  Check(DbiGetRecordCount(Dataset.Handle, Result));
end;

procedure DeleteTable(Table:TTable);
begin
    if (trim(table.tablename)>'') and TableExists(Table)
    then begin
      Table.Active:=false;
      Table.Deletetable;
    end
end;

procedure Deletetable(ptable:string;pDatabaseName:string='');
var thTable:TTable;
begin
  thTable:=TTable.create(Application);
  try
    thTable.TableName:=deleteQuoted(trim(ptable));
    if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
    thTable.databaseName:=pdatabaseName;
    Deletetable(thTable)
  finally
    thTable.free;
  end;
end;

procedure ChangeLevel(ptable:string;NewLevel:integer;pDatabaseName:string='');
var thTable:TTable;
begin
  thTable:=TTable.create(Application);
  try
    thTable.TableName:=ptable;
    if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
    thTable.databaseName:=pdatabaseName;
    ChangeLevel(thTable,Newlevel);
  finally
    thTable.free;
  end;
end;

procedure ChangeLevel(Table:TTable;NewLevel:integer);
var
    Desc:CRTblDesc;
    Level: DBINAME;
    LvlFldDesc: FLDDesc;
    OldTableActive:boolean;
    thDatabase:TDatabase;
begin
    oldtableActive:=table.Active;

    table.active:=false;

    fillchar(desc,sizeof(crtbldesc),#0);

    desc.iOptParams := 1;
    StrCopy(@Level, pchar(inttostr(NewLevel)));
    desc.pOptData := @Level;
    StrCopy(LvlFldDesc.szName, szCFGDRVLEVEL);
    LvlFldDesc.iLen := StrLen(Level) + 1;
    LvlFldDesc.iOffset := 0;
    desc.pfldOptParams :=  @LvlFldDesc;

    StrPCopy(desc.szTblName, table.tableName);

    thDatabase:=Table.database;
    if not Assigned(thDatabase) then thDatabase:=Table.OpenDatabase;

    if thdatabase=nil then raise Exception.Create(StrErrorChangeLevel);

    if not thdatabase.Connected
    then thDatabase.Open;

    check(DbiDoRestructure( thDatabase.Handle,1,@desc,nil,nil,nil,false));

    Table.Active:=OldTableActive;
end;

function TableExists(pTable:string;pDatabaseName:string=''):boolean; overload;
var thTable:ttable;
begin
  thTable:=TTable.Create(Application);
  try
    if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
    thTable.DatabaseName:=pDatabaseName;
    thTable.TableName:=pTable;
    Result:=TableExists(thTable);
  finally
    thTable.free;
  end
end;

function TableExists(Table:TTable):boolean;
var Thext:string;
    ThPath:string;
    //MyStringList:TStringList;
    //thName:string;
begin
  ThPath:=ExtractFilePath(Table.TableName);

  if (ThPath<>'') or (table.databaseName='')
  then begin
    //Result:=false;
    //MyStringList := TStringList.Create;
    //try
    Thext:=ExtractFileExt(Table.TableName);
    //thName:=ExtractFileNameNotExt(table.tablename);

    Result:=(ThExt<>'') and FileExists(Table.TableName)
          or (ThExt='') and (FileExists(Table.TableName+'.DB')
                             or
                             FileExists(Table.TableName+'.DBF')
                             or
                             FileExists(Table.TableName+'.TXT')
                             )

    //finally
      //MyStringList.Free;
    //end;
  end
  else REsult:=Table.Exists;

end;

function bvLockTable(Table:TTable;TimeOut:integer;ConfirmExit:boolean):boolean;
var  NumLocks: Word;
     ThTime:TDateTime;
begin
  try
    bvrefreshAll; // table.refresh;
    DbiIsTableLocked(Table.Handle, DBIWriteLock, NumLocks);
  except
    bvMessageError(StrErrorCannotLock+#13+#13+StrErrorCannotReadState);
    Result:=false;
    exit;
  end;
  if NumLocks<>0 then begin
    Result:=true;
    exit
  end
  else while true do begin
    ThTime:=now;
    repeat
      try
        table.refresh;
        Table.LockTable(ltWriteLock);
        bvFlush;
        Result:=true;
        exit;
      except
//        dElay(1000);
      end;
    until now-ThTime>encodeTime(0,0,TimeOut,0);
    if not ConfirmExit then begin
      Result:=false;
      exit
    end
    else
    if GetConfirmSmall(StrErrorCannotLock+' -'+Table.TableName+#13+
       StrContinue+'?')<>mrOk
    then begin
       Result:=false;
       Exit;
    end
    else bvrefreshAll;
  end;

end;

procedure bvUnlockTable(Table:TTable);
var  NumLocks: Word;
     i:integer;
begin
  if not Table.Active then begin
    bvMessageError(StrErrorCannotUnLock+#13+#13+strErrortableisclosed);
    exit;
  end;

  try
    bvREfreshAll; //table.refresh;
    DbiIsTableLocked(Table.Handle, DBIWriteLock, NumLocks);
  except
    bvMessageError(StrErrorCannotUnLock+#13+#13+
                   StrErrorCannotReadState);
    exit;
  end;

  try
    for i:=1 to NumLocks do Table.UnlockTable(ltWriteLock);
    bvFlush;
  except
    bvMessageError(StrErrorCannotUnLock);
    exit;
  end;
end;

procedure TableCopy(ResultName:string;DataSet:TDBDataSet;WF:TMyWait;IDFieldName:string);
var TAble:TTAble;
begin
   table:=TTable.create(Application);
   try
      TAble.tableName:=REsultName;
      table.databaseName:=defaultdatabaseName;

      TableCopy(TAble,Dataset,wf,IDFieldName);
   finally
     table.free;
   end;
end;


procedure TableCopy(ResultName,Source:string;wf:tMyWait;IDFieldName:string);
var TAble:TTAble;
begin
   table:=TTable.create(Application);
   try
      TAble.tableName:=Source;
      table.databaseName:=defaultdatabaseName;
      TableCopy(REsultName,Table,wf,idFieldName);
   finally
     table.free;
   end;
end;

procedure TableCopy(ResultTable:TTable;Source:string;wf:tMyWait;IDFieldName:string);
var TAble:TTAble;
begin
   table:=TTable.create(Application);
   try
      TAble.tableName:=Source;
      table.databaseName:=defaultdatabaseName;
      TableCopy(REsultTAble,Table,wf,idFieldName);
   finally
     table.free;
   end;
end;

procedure TableCopy(ResultTable:TTable;DataSet:TdbDataSet;wf:tMyWait;IDFieldName:string);
var i:integer;
    k:integer;
    thWF:TMyWait;
    ThRecC:integer;
    IsControlsDisabled:boolean;
    //ThFile:TextFile;
    Str,Str1:string;
//    MSExcel,ThWorkBook,thWorkSheet:OleVariant;
    MSWord:OleVariant;
    RowC:integer;
//    Excel50Sheet:

    oldText:string;
    DSOpened:boolean;
    REsOpened:boolean;
    IncI:integer;
begin
   IsControlsDisabled:=DataSet.ControlsDisabled;
   OldTExt:='';
   if not IsControlsDisabled then DataSEt.DisableControls;
   if WF=nil then  thWF:=tmywait.create(StrsavingTAble)
   else begin
      thWF:=WF;
      OldText:=WF.doptext;
      WF.doptext:=strcopyingofFiles;
   end;
   DSOpened:=dataset.active;
   REsOpened:=ResultTable.Active;
   try
     //ResultTable.Tablename:=ResultName;

     dataset.open;

     if Resulttable.active then Resulttable.Close;
     Resulttable.FieldDefs.Clear;
     REsultTable.IndexDefs.clear;
//     BatchMove.Mappings.Clear;
//     BatchMove.Source:=ThData;

     try
       if (ExtractFileExt(Resulttable.TableName)='') or
          (uppercase(ExtractFileExt(Resulttable.TableName))='.DB')
          or (uppercase(ExtractFileExt(Resulttable.TableName))='.DBF')
       then begin
          if (ExtractFileExt(Resulttable.TableName)='') or
             (uppercase(ExtractFileExt(Resulttable.TableName))='.DB')
          then
             Resulttable.TableType:=ttparadox
          else if  (uppercase(ExtractFileExt(ResultTable.TableName))='.DBF')
          then
             ResultTable.TableType:=ttdbase
          else if  (uppercase(ExtractFileExt(ResultTable.TableName))='.TXT') ///???
          then
             ResultTable.TableType:=ttascii;



          if (IDFieldName<>'')
          then begin
             IncI:=1;

             if (Resulttable.TableType=ttparadox)
             then begin
                ResultTable.FieldDefs.add(IDFieldName,ftAutoInc,0,false);
                Resulttable.IndexDefs.Add('PRIMARYKEY',idFieldName,[ixPrimary,ixUnique]);
             end
             else ResultTable.FieldDefs.add(IDFieldName,ftInteger,0,false)
          end
          else incI:=0;



          for i:=0 to DataSet.FieldCount-1 do begin
               if DataSet.Fields[i].DataType=ftAutoInc
               then ResultTable.FieldDefs.add(DataSet.fields[i].FieldName,ftInteger,DataSet.Fields[i].Size,false)
               else ResultTable.FieldDefs.add(DataSet.fields[i].FieldName,DataSet.Fields[i].DataType,DataSet.Fields[i].Size,false);
          end;

          if ResultTable.TableType=ttParadox then ResultTable.TableLevel:=7;

          ResultTable.CreateTable;
          ResultTable.Open;

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=DataSet.RecordCount;

          DataSet.First;

          k:=0;
          while not DataSet.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            ResultTable.Append;

            for i:=0 to DataSet.FieldCount-1 do  begin
               ResultTable.Fields[i+IncI].AsString:=DataSet.Fields[i].AsSTring
            end;

            if (IncI>0) and (Resulttable.TableType<>ttparadox)
            then begin
               ResultTable.fields[0].asInteger:=k;
            end;

            ResultTable.Post;
            DataSet.Next;
          end;

          bvFlush(ResultTable);
          ResultTable.Close;
       end
       else if (uppercase(ExtractFileExt(ResultTable.TableName))='.TXT')
               or (uppercase(ExtractFileExt(ResultTable.TableName))='.CSV')
       then begin
           SaveTableToTxt(ResultTable.tableName,nil,dataset,thWF);

{          AssignFile(ThFile,ResultTable.TableName);
          Rewrite(ThFile);

          Str:='';
          for i:=0 to DataSet.FieldCount-1 do begin
               if Str>'' then Str:=Str+#9;
               Str:=Str+DataSet.Fields[i].FieldName;
            end;

          Writeln(ThFile,Str);

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=DataSet.RecordCount;

          DatasEt.First;

          k:=0;
          while not Dataset.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            Str:='';

            for i:=0 to DataSet.FieldCount-1 do begin
                 if Str>'' then Str:=Str+#9;
                 Str:=Str+DataSet.fields[i].AsString;
              end;

            Writeln(ThFile,Str);
            DataSet.Next;
          end;
          CloseFile(Thfile);
}
       end
       else if  (uppercase(ExtractFileExt(ResultTable.TableName))='.XLS')
       then begin
          begin // Excel 8.0
              SaveTableToExcel8( ResultTable.TableName,nil,DataSet,ThWF,False);
          end
       end
       else if  (uppercase(ExtractFileExt(ResultTable.TableName))='.DOC')
       then begin
          MSWord:=CreateOleObject('Word.Basic');

          Str:='';

          RoWC:=0;

          for i:=0 to dataSet.FieldCount-1 do begin
               inc(RowC);
               if Str<>'' then   Str:=Str+#9;
               Str:=Str+DataSet.Fields[i].FieldName;
            end;


          str:=Str+#13;

          thWF.ShowProgress;
          thwf.ProgressBarPos:=0;

          ThRecC:=DatasET.RecordCount;

          DataSet.First;

          k:=0;
          while not DataSet.eof do begin
            inc(k);
            thWF.ProgressBarPos:=k * 100 div ThRecC;

            Str1:='';

            for i:=0 to DataSet.FieldCount-1 do begin
                 if Str1<>'' then Str1:=Str1+#9;
                 Str1:=Str1+DataSet.Fields[i].AsString;
            end;

            Str:=Str+Str1+#13;

            DataSet.Next;
          end;

//          if CheckShowResult.Checked then  MsWord.AppShow;
          MSWord.FileNew;
          MSWord.Insert(Str);
//          MSWord.LineUp(1, 0);
          MSWord.EditSelectAll;
//          MSWord.TextToTable(ConvertFrom := 2,NumColumns := FormGrid.RowCount);
          MSWord.TextToTable(NumColumns := RowC);
          MSWord.FileSaveAs(ResultTable.TableName);
          {if not checkShowResult.Checked then} msword.AppClose;
       end
       else begin
         bvMessageError(StrErrorCannotWriteToThisFormat+': '+ExtractFileExt(ResultTable.TableName));
         exit;
       end;

     except
       on e:exception do begin
         bvMessageError(StrErrorCannotSaveData+#13+#13+
                    strMessage+': '+e.message);
         exit;
       end
       else raise;
     end;
//     Close;
   finally
     if WF=nil then  thWF.free
     else begin
        WF.doptext:=OldText;
     end;
     if not IsControlsDisabled then DataSet.EnableControls;
     ResultTable.Active:=false;
     //ResultTable.Free;
     DAtaset.active:=DSOpened;
     REsultTable.active:=ResOpened;

   end;
end;  ///

function BDENumberStr(Value:extended):string;
var fmt: FMTNumber;
begin
  Result:=floattostr(Value);
  if (DbiGetNumberFormat(fmt)=DBIERR_NONE)
     and
     (fmt.cDecimalSeparator<>decimalseparator)
     then
       Result:=StringReplace( Result,DecimalSeparator,fmt.cDecimalSeparator,[rfReplaceAll])
end;

function BDEDateStr(Value:TDateTime;Quoted:boolean=true;TypeCastString:boolean=true;BDESEPARATOR:boolean=false):string;
var fmt: FMTDate;
begin
  Result:=FormatDateTime('dd/mm/yyyy',Value); // need 4 digits in year //datetostr(Value);

  if bdeseparator then begin
  if (DbiGetdateFormat(fmt)=DBIERR_NONE)  // В SQL Глюк - работает только с точкой! А вот Filter - требует bdeseparator
     and
     (fmt.szDateSeparator[0]<>DateSeparator)
     then
       Result:=StringReplace( Result,DateSeparator,fmt.szDateSeparator[0],[rfReplaceAll]);
  end
  else Result:=StringReplace( Result,DateSeparator,'.',[rfReplaceAll]);

  if Quoted then Result:=QuotedStr(result);
  if TypeCastString then Result:='cast('+Result+' as Date)';
  //DateTosql(Value)
end;

function  BatchMoveToTMP(pDATABASEName:string;
                         pSQL:string;
                         p_tableName:string='';
                         pSessionName:string='';
                         WaitPrompt:boolean=false):string;
var Query:tQuery;
begin
  Query:=TQuery.create(Application);
  with Query do
  try
     if pSessionName<>'' then SessionName:=pSessionname;
    if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
     DatabaseName:=pDATABASENAME;
     sql.clear;
     sql.add(psql);
     result:=BatchMoveToTMP(Query,p_tableName,WaitPrompt);
  finally
     free
  end;
end;


function  BatchMoveToTMP(DATAset:TDBDataSet;p_tableName:string='';WaitPrompt:boolean=false):string;
var WF:TMyWait;
begin
   if WaitPrompt then WF:=TMyWait.Create(StrCopying+'..')
   else WF:=nil;

   try
     with TTable.Create(Application) do
     try
        SessionName:=Dataset.SessionName;
        DatabaseName:=defaultdatabasename;//Dataset.DatabaseName;
        tableName:=p_tableName;
        if trim(tableName)='' then tableName:=UniqueTableName();
        BatchMove(DATASET,batCopy);
        REsult:=TableName;
     finally
        Free;
     end;
   finally
     if assigned(WF) then WF.Free
   end;
end;

procedure bvFlushAllBuffers;
var
  I, L: Integer;
  Session: TSession;
  J: Integer;
begin
  for J := 0 to Sessions.Count - 1 do begin
    Session := Sessions[J];
    if not Session.Active then Continue;
    for I := 0 to Session.DatabaseCount - 1 do begin
      with Session.Databases[I] do
        if Connected and not IsSQLBased then begin
          for L := 0 to DataSetCount - 1 do begin
            if DataSets[L].Active
               and
               (
               dataSets[l] is TTAble
               or (DAtaSets[l] is TQuery) and
                  (DataSets[l] as TQuery).RequestLive
               )
            then
              DbiSaveChanges(DataSets[L].Handle);
          end;
        end;
    end;
  end;
end;

procedure bvRefresh;
begin
   bvRefreshAll;
end;

procedure bvREfreshAll;
var
  I, L: Integer;
  Session: TSession;
  J: Integer;
begin
  for J := 0 to Sessions.Count - 1 do begin
    Session := Sessions[J];
    if not Session.Active then Continue;
    for I := 0 to Session.DatabaseCount - 1 do begin
      with Session.Databases[I] do
        if Connected then begin
          for L := 0 to DataSetCount - 1 do begin
            if DataSets[L].Active
               and
               (
               dataSets[l] is TTAble
               or (DAtaSets[l] is TQuery) and
                  (DataSets[l] as TQuery).RequestLive
               )
               and not (Datasets[l].State in [dsEdit,dsInsert])
            then
              DAtaSets[l].Refresh;
          end;
        end;
    end;
  end;
end;

function RoundSQL(Value:string;n:integer=0):string;
             // возврацает выражение, округляющее Value до n
var xN,xdivN:string;
begin
 xN:=StringReplace(floattostrf( IntPower(10,n),ffFixed,100,abs(n)+1),',','.',[]);
 xdivN:=StringReplace(floattostrf( Intpower(10,-n),ffFixed,100,abs(n)+1),',','.',[]);

 { access violation on bde ?????
 Result:='cast('+
            'cast( (cast('+value+' as float)+cast('+xdivn+'*0.499999999 as float))*'+xn+' as integer)'+
         '*'+xdivn+' as float)';
 }

 { OK variant, but very long }
 Result:='('+
            'cast(('+value+')*'+xn+' as integer)'+

            ' + '+
            'cast(cast('+
                 '(('+Value +')*'+xn+')'+
                 '-' +
                 'cast(('+vAlue+')*'+xn+' as integer)'+
                 ' + 0.499999999'+
            ' as integer) as float)'+
         ')*'+xdivn;
end;

(*
function Frac1SQL(Value:string;n:integer=0):string;
var xN,xdivN:string;
begin
 xN:=StringReplace(floattostr(Power(10,n)),',','.',[]);
 xdivN:=StringReplace(floattostr(power(10,-n)),',','.',[]);

 Result:='('+
            'cast('+
               '('+
                    '('+Value +')*'+xn+
                    '-' +
                    'cast(('+vAlue+')*'+xn+' as integer)'+

               ')*10'+

             ' as integer)/5'+
         ')*'+xdivn;

end;
*)

procedure SaveTableToTxt(FileName:string;FormGrid:TStringGrid;ThData:TDBDataSet;WF:TMyWait{;ShowResult:boolean});
var
    ThFile:TextFile;
    str:string;
    i:integer;
    threcC:integer;
    k:integer;
    thDelimiter:char;

    function quot(Str :string) :string;
    begin
      if pos(thdelimiter,str)>0 then result := '"'+str+'"'
      else result := str;
    end;
begin
    AssignFile(ThFile,FileName);
    Rewrite(ThFile);

    Str:='';

    if uppercase(extractfileext(FileName))='.CSV'
    then thDelimiter := ';'
    else thdelimiter := #9;


    if Assigned(FormGrid) then begin
      for i:=1 to FormGrid.RowCount-1 do
        if (FormGrid.Cells[0,i]=PlusStr)
           and Assigned(FormGrid.objects[0,i])
        then begin
           if Str>'' then Str:=Str+thDelimiter;
           Str:=Str+quot(FormGrid.Cells[1,i]);
        end
    end
    else begin
      for i:=0 to thData.FieldCount-1 do begin
           if Str>'' then Str:=Str+thdelimiter;
           Str:=Str+quot(thData.Fields[i].FieldName);
        end;
    end;

    if pos('ID',str)=1 then begin  // В Excel вылезет Exception, исли оставить ID
       str[1]:='i';
    end;


    Writeln(ThFile,Str);

    if Assigned(WF) then begin
      wf.ShowProgress;
      wf.ProgressBarPos:=0;
    end;

    ThRecC:=ThData.RecordCount;

    ThData.First;

    k:=0;
    while not ThData.eof do begin
      inc(k);

      if assigned(WF) then  WF.ProgressBarPos:=k * 100 div ThRecC;

      Str:='';

      if Assigned(FormGrid) then begin
        for i:=1 to FormGrid.RowCount-1 do
          if (FormGrid.Cells[0,i]=PlusStr)
             and Assigned(FormGrid.objects[0,i])
          then begin
             if i>1 {Str>''} then Str:=Str+thDelimiter;
             Str:=Str+quot(TField(FormGrid.objects[0,i]).AsString);
          end
      end
      else begin
        for i:=0 to thDAta.FieldCount-1 do begin
             if Str>'' then Str:=Str+thDelimiter;
             Str:=Str+quot(thData.fields[i].AsString);
          end;
      end;

      Writeln(ThFile,Str);
      thData.Next;
    end;
    CloseFile(Thfile);
end;

function bvBatchMove(pDestination:TTAble;pSource:TBDEDataSet;AMode:TBAtchMode):integer;
begin
  try
    Result:=pDestination.batchmove(pSource,AMode);
  except
    //Result:=0;
    raise;
  end;
end;

function bvBatchMove(pDestination:string;pSource:TBDEDataSet;AMode:TBAtchMode;pDescdatabaseName:string):integer;
var Tab:TTAble;
begin
  try
    tab:=TTable.create(Application) ;
    try
      tab.tableName:=pDestination;
      if trim(PDescDatabaseName)='' then pDescDatabaseName:=defaultdatabaseName;
      tab.databaseName:=pDescDatabaseName;
      result:=bvBatchmove(tab,pSource,amode);
    finally
      tab.free;
    end;
  except
    //Result:=0;
    raise
  end;
end;

function bvBatchMove(pDestination:TTable;pSource:string;AMode:TBAtchMode;pSRCdatabaseName:string):integer;
var Tab:TTAble;
begin
  try
    tab:=TTable.create(Application) ;
    try
      tab.tableName:=pSource;
      if trim(PSRCDatabaseName)='' then pSRCDatabaseName:=defaultdatabaseName;
      tab.databaseName:=pSRCDatabaseName;
      result:=bvBatchmove(pDestination,tab,amode);
    finally
      tab.free;
    end;
  except
    //Result:=0;
    raise
  end;
end;

function bvBatchAppend(pDestination:TTable;pSource:string;pSrcDatabaseName:string=''):integer; overload;
var Qu:TQuery;
begin
  try
    Qu:=TQuery.create(Application) ;
    try
      Qu.sql.Text:=pSource;
      if trim(PSRCDatabaseName)='' then pSRCDatabaseName:=defaultdatabaseName;
      Qu.databaseName:=pSRCDatabaseName;
      result:=bvBatchmove(pDestination,Qu,batAppend);
    finally
      Qu.free;
    end;
  except
    //Result:=0;
    raise
  end;
end;

function bvBatchMove(pDestination:string;pSource:string;AMode:TBAtchMode;pdatabaseName:string):integer;
var Tab:TTable;
begin
  try
    Tab:=TTable.create(Application);
    try
      Tab.tableName:=pSource;
      if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
      tab.databasename:=pDatabaseName;
      result:=bvBatchMove(pDestination,Tab,AMode,pdatabaseName);
    finally
      Tab.free;
    end;
  except
    //Result:=0;
    raise;
  end;
end;


function GetRecordCount(pTableName:string;pDatabaseName:string):integer;
begin

  with TTable.create(Application) do
  try
  try
    if trim(PDatabaseName)='' then pDatabaseName:=defaultdatabaseName;
    databaseName:=pDatabaseName;
    tableName:=pTableName;
    open;
    Result:=RecordCount;
    Close;
  finally
    Free
  end;
  except
    //Result:=0;
    raise;
  end;

end;

procedure bvFlush(DataSet:TdbDataSet=nil);
begin
  if Dataset=nil then bvFlushAllBuffers
  else
  if dataset.Active
     and not (dataset.state in [dsEdit,dsInsert])
     and Assigned(DAtASEt.database)
     and not dataset.database.IsSQLBased
  then begin
    dataset.FlushBuffers;
  end;
end;

procedure DeleteNULLs(Table:TTable);
var i,FC:integer;
begin
  table.open;
  FC:=table.fieldcount;
  table.first;
  while not table.eof do begin
    for i:=0 to FC-1 do begin
      if table.fields[i].isNULL and (table.fields[i].datatype in [ftSmallint, ftInteger, ftWord,
       ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
       ftBytes, ftVarBytes,ftLargeint])
      then begin
           if not (table.state in [dsEdit,dsInsert])
           then table.edit;
           table.fields[i].value:=0
      end;
    end;
    if table.state in [dsEdit,dsInsert] then table.post;
    table.next;
  end;
end;

function getDoubleValue(dSet:TDataSet;FName:string):double;
var F:TField;
begin
  Result := 0;
  if not dSet.active then exit;

  f := DSet.Findfield(fname);
  if assigned(f)
  then result := f.asfloat;

end;

function getStringValue(dSet:TDataSet;FName:string):string;
var F:TField;
begin
  Result := '';
  if not dSet.active then exit;

  f := DSet.Findfield(fname);
  if assigned(f)
  then result := f.asString;

end;

function getDateValue(dSet:TDataSet;FName:string):TDateTime;
var F:TField;
begin
  Result := 0;
  if not dSet.active then exit;

  f := DSet.Findfield(fname);
  if assigned(f)
  then result := f.AsDateTime;

end;

function getIntegerValue(dSet:TDataSet;FName:string):integer;
var F:TField;
begin
  Result := 0;
  if not dSet.active then exit;

  f := DSet.Findfield(fname);
  if assigned(f)
  then result := f.asinteger;

end;

function getNextKey(
                  Sessionname:string;
                  databaseName:string;
                  TableName:string;
                  IDName:string):integer;
var Query:TQuery;
begin
  Result := 1;

  if sessionname='' then Sessionname := Session.SessionName;
  if databaseName='' then Exception.Create('getnextkey: Databasename - ?')
  else if Tablename='' then exception.create('getnextkey: Tablename - ?')
  else if IDNAME='' then exception.create('getnextkey: IDNAME - ?');

  Query := tQuery.create(application);
  try
     Query.SessionName := sessionname;
     Query.DatabaseName := databasename;
     Query.sql.Clear;
     Query.sql.add('select max(a."'+IDName+'") from "'+tablename+'" a');
     Query.open;
     Query.first;
     if Query.Recordcount>0 then REsult := Query.Fields[0].asinteger +1;
     Query.close;
  finally
    Query.free;
  end;

end;

function getNextKey(
                  Table:TTAble;
                  IDName:string):integer;
begin
   if not assigned(table) then Exception.Create('getnextkey: not assigned Table')
   else begin
     table.open;
     bvRefresh;
     Result := getNextKey(table.sessionname,table.databasename,table.tablename,idname)
   end;
end;

end.

