{$DEFINE UserScriptCmdList}
unit ScriptUtils;
{*********************************************************}
{*                     ScriptUtil.pas                    *}
{*             Common Archiver Kit Experinment           *}
{*   Copyright (c) 2002 - 2004 Joseph Leung Yat Chun     *}
{*                 All rights reserved.                  *}
{*********************************************************}
interface
uses SysUtils, Dialogs, Forms, Classes, CakUtils2, Cakdir2, Windows, Controls, QzWildCardLister, CakDefs2,
     CakDelphiZip, CakZipForge; //For Zip64 support
type   TScriptCmdRec = Record
                    CommandName : string;
                    ImgIndex : Integer;
                    CommandHelp : string;
                    HtmlDesc : string;
                    TParam1, TParam2 : string;
                    Param1, Param2 : string;
                    Param1Question : string;
                    Param2Question : string;
                    AkpCommand : string;
                  end;


const p1 = '<dynamic id="param1">';
      p2 = '<dynamic id="param2">';
      Def_Archive   = 'c:\QuickZip.zip';
      Def_Content   = '*.*';
      Def_Directory = '%ProgramFiles%'; //%USERPROFILE%%Windir%
      Def_File      = 'c:\folder\file.ext';
      Def_String    = '';
      Def_Bool      = 'TRUE';
      Def_Type      = 'ZIP';
      MaxScript = 32;
      ScriptCmdList : Array[-1..MaxScript-1] of TScriptCmdRec = (
  (CommandName:'Unknown'       ;ImgIndex:-1                  ;
   CommandHelp:'Unknown Command'                             ;
   HtmlDesc:'Unknown Command!!'                              ;
   TParam1:''                  ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:''                                         ;
   Param2Question:''                                         ;
   AkpCommand:''                                             ),
  ///////////////////////////////////////////////////////////
  (CommandName:'New'           ;ImgIndex:0                   ;
   CommandHelp:'Create a New Archive'                        ;
   HtmlDesc:'Create an archive named : ' + p1 + '.'          ;
   TParam1:'%Archive%'         ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'ArchiveName:'                             ;
   Param2Question:''                                         ;
   AkpCommand:'New(ArchiveName : string);'                   ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Open'          ;ImgIndex:1                   ;
   CommandHelp:'Open an Existing Archive'                    ;
   HtmlDesc:'Open an archive named : '+p1+'.'                ;
   TParam1:'%Archive%'         ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'ArchiveName:'                             ;
   Param2Question:''                                         ;
   AkpCommand:'Open(ArchiveName : string);'                  ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Close'         ;ImgIndex:4                   ;
   CommandHelp:'Close Opened Archive'                        ;
   HtmlDesc:'Close Opened Archive'                           ;
   TParam1:''                  ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:''                                         ;
   Param2Question:''                                         ;
   AkpCommand:'Close;'                                       ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Extr'          ;ImgIndex:14                  ;
   CommandHelp:'Extract / Decompress file(s) from archive.'  ;
   HtmlDesc:'Extract ' +p1+' from archive to '+p2+'.'        ;
   TParam1:'%Content%'         ;TParam2:'%Directory%'        ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'What to Extract:'                         ;
   Param2Question:'Extract to where:'                        ;
   AkpCommand:'Extract(what, too : string);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Extract'       ;ImgIndex:14                  ;
   CommandHelp:'Extract / Decompress file(s) from archive.'  ;
   HtmlDesc:'Extract ' +p1+' from archive to '+p2+'.'        ;
   TParam1:'%Content%'         ;TParam2:'%Directory%'        ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'What to Extract:'                         ;
   Param2Question:'Extract to where:'                        ;
   AkpCommand:'Extract(what, too : string);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Add'           ;ImgIndex:13                  ;
   CommandHelp:'Add / Compress file into opened archive.'    ;
   HtmlDesc:'Add '+p1+' to current archive. '                ;
   TParam1:'%File%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'What to Add:'                             ;
   Param2Question:''                                         ;
   AkpCommand:'Add(name : string);'                          ),
  ///////////////////////////////////////////////////////////
  (CommandName:'DoAdd'         ;ImgIndex:7                   ;
   CommandHelp:'Start Adding files.'                         ;
   HtmlDesc:'Add files.'                                     ;
   TParam1:''                  ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:''                                         ;
   Param2Question:''                                         ;
   AkpCommand:'DoAdd;'                                       ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Password'      ;ImgIndex:6                   ;
   CommandHelp:'Configure Add/Extract Password.'             ;
   HtmlDesc:'Set current Add/Extract Password to '+p1+'.'    ;
   TParam1:'%String%'          ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Password:'                                ;
   Param2Question:''                                         ;
   AkpCommand:'Password(value: string);'                     ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Delete'        ;ImgIndex:4                   ;
   CommandHelp:'Remove File from Opened Archive.'            ;
   HtmlDesc:'Remove '+p1+' from Current Archive.'            ;
   TParam1:'%Content%'          ;TParam2:''                  ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Delete Which file:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'Delete(filename: string);'                    ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Del'          ;ImgIndex:4                    ;
   CommandHelp:'Remove File from Opened Archive.'            ;
   HtmlDesc:'Remove '+p1+' from Current Archive.'            ;
   TParam1:'%Content%'          ;TParam2:''                  ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Delete Which file:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'Delete(filename: string);'                    ),
  ///////////////////////////////////////////////////////////
  (CommandName:'UseExtrPath'        ;ImgIndex:2              ;
   CommandHelp:'Toggle use Path information when Extract.'   ;
   HtmlDesc:'Use Path when Extract : '+p1+'.';
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Use Path:'                                ;
   Param2Question:''                                         ;
   AkpCommand:'UseExtrPath(toggle:Bollean);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'UseExtrOverwrite'   ;ImgIndex:2              ;
   CommandHelp:'Toggle to overwrite existing files.'         ;
   HtmlDesc:'Overwrite existing Files :'+p1+'.'              ;
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Overwrite Existing files:'                ;
   Param2Question:''                                         ;
   AkpCommand:'UseExtrOverwrite(toggle:Bollean);'            ),
  ///////////////////////////////////////////////////////////
  (CommandName:'UseAddPath'         ;ImgIndex:2              ;
   CommandHelp:'Toggle Save Path Information.'               ;
   HtmlDesc:'Save Add Path : '+p1+'.'                        ;
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Use Path:'                                ;
   Param2Question:''                                         ;
   AkpCommand:'UseAddPath(toggle:Bollean);'                  ),
  ///////////////////////////////////////////////////////////
  (CommandName:'UseRelativePath'    ;ImgIndex:2              ;
   CommandHelp:'Toggle Save Relative Path Information.'      ;
   HtmlDesc:'Add Relative Path : '+p1+'.'                    ;
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Use Relative Path:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'UseRelativePath(toggle:Bollean);'             ),
  ///////////////////////////////////////////////////////////
  (CommandName:'UseSubDir'          ;ImgIndex:2              ;
   CommandHelp:'Toggle Add file in Sub-Directory.'           ;
   HtmlDesc:'Include SubDir : '+p1+'.'                       ;
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Include SubDir:'                          ;
   Param2Question:''                                         ;
   AkpCommand:'UseSubDir(toggle:Bollean);'                   ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Zip64'              ;ImgIndex:2              ;
   CommandHelp:'Toggle Large Zip support.'                   ;
   HtmlDesc:'Use Large Zip support : '+p1+'.'                ;
   TParam1:'%Bool%'            ;TParam2:''                   ;
   Param1 :''                  ;Param2 :''                   ;
   Param1Question:'Use Large Zip support:'                   ;
   Param2Question:''                                         ;
   AkpCommand:'n/a'                                          ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Convert'       ;ImgIndex:26                  ;
   CommandHelp:'Convert an archive to other type.'           ;
   HtmlDesc:'Convert '+p1+' to '+p2+' archive.'              ;
   TParam1:'%Archive%'            ;TParam2:'%Type%'          ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Archive to Convert:'                      ;
   Param2Question:'Type:'                                    ;
   AkpCommand:'Convert(from,totype : string);'               ),
  ///////////////////////////////////////////////////////////
  (CommandName:'RunFile'       ;ImgIndex:0                   ;
   CommandHelp:'Execute another file (e.g. exe).'            ;
   HtmlDesc:'Execute '+p1+' with param = '+p2+'.'            ;
   TParam1:'%File%'               ;TParam2:'%string%'        ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'File to run:'                             ;
   Param2Question:'Parameter:'                               ;
   AkpCommand:'Run(name : string);'                          ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Movefile'      ;ImgIndex:0                   ;
   CommandHelp:'Move a file to another location.'            ;
   HtmlDesc:'Move '+p1+' to '+p2+'.'                         ;
   TParam1:'%File%'               ;TParam2:'%File%'          ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'File to Move:'                            ;
   Param2Question:'Move File to:'                            ;
   AkpCommand:'Movefile(from,to : string);'                  ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Copyfile'      ;ImgIndex:0                   ;
   CommandHelp:'Copy a file to another location.'            ;
   HtmlDesc:'Copy '+p1+' to '+p2+'.'                         ;
   TParam1:'%File%'               ;TParam2:'%File%'          ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'File to Copy:'                            ;
   Param2Question:'Copy File to:'                            ;
   AkpCommand:'Copyfile(from,to : string);'                  ),
  ///////////////////////////////////////////////////////////
  (CommandName:'Renfile'       ;ImgIndex:0                   ;
   CommandHelp:'Rename a file.'                              ;
   HtmlDesc:'Move '+p1+' to '+p2+'.'                         ;
   TParam1:'%File%'               ;TParam2:'%File%'          ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'File to Rename:'                          ;
   Param2Question:'Rename to:'                               ;
   AkpCommand:'Renamefile(from,to : string);'                ),
  ///////////////////////////////////////////////////////////
  (CommandName:'MakeDir'       ;ImgIndex:1                   ;
   CommandHelp:'Create a directory.'                         ;
   HtmlDesc:'Create an directory named: '+p1+'.'             ;
   TParam1:'%Directory%'          ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'New Folder:'                              ;
   Param2Question:''                                         ;
   AkpCommand:'MakeDir(name : string);'                      ),
  ///////////////////////////////////////////////////////////
  (CommandName:'OpenDir'       ;ImgIndex:1                   ;
   CommandHelp:'Open a directory.'                           ;
   HtmlDesc:'Open an directory named: '+p1+'.'               ;
   TParam1:'%Directory%'          ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Folder:'                                  ;
   Param2Question:''                                         ;
   AkpCommand:'OpenDir(name : string);'                      ),
  ///////////////////////////////////////////////////////////
  (CommandName:'TxtFileList'   ;ImgIndex:30                  ;
   CommandHelp:'Text file list.'                             ;
   HtmlDesc:'Create a text file list named: '+p1+'.'         ;
   TParam1:'%File%'               ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Filelist filename:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'Textfilelist(name : string);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'HtmFileList'   ;ImgIndex:31                  ;
   CommandHelp:'Html file list.'                             ;
   HtmlDesc:'Create a html file list named: '+p1+'.'         ;
   TParam1:'%File%'               ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Filelist filename:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'Htmlfilelist(name : string);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'CsvFileList'   ;ImgIndex:30                  ;
   CommandHelp:'Csv file list.'                              ;
   HtmlDesc:'Create a csv file list named: '+p1+'.'          ;
   TParam1:'%File%'               ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Filelist filename:'                       ;
   Param2Question:''                                         ;
   AkpCommand:'Csvfilelist(name : string);'                  ),
  ///////////////////////////////////////////////////////////
  (CommandName:'DateStr'       ;ImgIndex:16                  ;
   CommandHelp:'Configure text of %Date%.'                   ;
   HtmlDesc:'Change datestr format to : '+p1+'.'             ;
   TParam1:'%String%'             ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Datestr format:'                          ;
   Param2Question:''                                         ;
   AkpCommand:'n/a'                                          ),
  ///////////////////////////////////////////////////////////
  (CommandName:'RenameFileList';ImgIndex:26                  ;
   CommandHelp:'Rename files using special wildcard. (Details: Tools\MassRenamer)';
   HtmlDesc:'Rename from '+p1+' to '+p2+'.'                  ;
   TParam1:'%String%'             ;TParam2:'%String%'        ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Source Format:'                           ;
   Param2Question:'Target Format:'                           ;
   AkpCommand:'n/a'                                          ),
  ///////////////////////////////////////////////////////////
  (CommandName:'CopyFileList'  ;ImgIndex:26                  ;
   CommandHelp:'Copy files using special wildcard. (Details: Tools\MassRenamer)';
   HtmlDesc:'Copy from '+p1+' to '+p2+'.'                  ;
   TParam1:'%String%'             ;TParam2:'%String%'        ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Source Format:'                           ;
   Param2Question:'Target Format:'                           ;
   AkpCommand:'n/a'                                          ),
  /////////////////////////////////////////////////////////// 
  (CommandName:'BatchZipFileList';ImgIndex:26                ;
   CommandHelp:'BatchZip files using special wildcard. (Details: Tools\MassRenamer)';
   HtmlDesc:'BatchZip from '+p1+' to '+p2+'.'                ;
   TParam1:'%String%'             ;TParam2:'%String%'        ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'Source Format:'                           ;
   Param2Question:'Target Format:'                           ;
   AkpCommand:'n/a'                                          ),  
  ///////////////////////////////////////////////////////////
  (CommandName:'Msg'           ;ImgIndex:16                  ;
   CommandHelp:'Show message on progress.'                   ;
   HtmlDesc:'Show message: '+p1+'.'                          ;
   TParam1:'%String%'             ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:'String to show:'                          ;
   Param2Question:''                                         ;
   AkpCommand:'Writeln(something : string);'                 ),
  ///////////////////////////////////////////////////////////
  (CommandName:'CloseARC'      ;ImgIndex:6                   ;
   CommandHelp:'Close Archiver.'                             ;
   HtmlDesc:'Close Archiver.'                                ;
   TParam1:''                     ;TParam2:''                ;
   Param1 :''                     ;Param2 :''                ;
   Param1Question:''                                         ;
   Param2Question:''                                         ;
   AkpCommand:'Application.terminate;';                      )
  );

var Cakdir2 : TCakdir2;
    ParamList : TStringList;
    DATESTR : string;
    Executing : boolean = false;
function isExistsin(text2find, textstring : string) : boolean;
function EncodeAksCommand(AksCommand : TScriptCmdRec) : string;
function LocateID(CmdName : string) : Integer;
function LocateCmdRec(CmdName : string) : TScriptCmdRec;
function DecodeAksCommand(AksCommand : string) : TScriptCmdRec;
function GetDefaultParam(ParamType : string) : string;
function DecodeEnvVar(EnvVar : string; index : integer) : string;
function EnvPath2RealPath(EnvPath : string; index : integer) : string;
procedure Execute_command(scriptcommand : String);
procedure Execute_commandEx(scriptcommand : TScriptCmdRec; index : integer);
procedure Load_Script(Script : Tstrings);

function AskForParam(AksCommand : TScriptCmdRec; default : string; variable : integer) : string;

implementation
{$IFDEF UserScriptCmdList}uses QzScriptCommandList;{$ENDIF}
var currentParam1 : string;

function decodetimestr(input : string) : string;
var i: integer;
    hh,mm,ss,ms,yy,nn,dd : word;
    Date: TDateTime;
const day : array[1..7] of string =
        ('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday');
      month : array[1..12] of string =
        ('Janurary','Feburary','March','April','May','June','July','August',
         'September','October','November','December');
function ret_dddd : string;
begin
        result := day[dayofweek(now)];
end;
function ret_yyyy : string;
begin
        result := inttostr(yy);
end;
function ret_mmmm : string;
begin
        result := month[mm];
end;
function ret_ddd : string;
begin
        result := copy(day[dayofweek(now)],0,3);
end;
function ret_mmm : string;
begin
        result := copy(month[dayofweek(now)],0,3);
end;
function ret_hh2 : string;
begin
        result := inttostr(hh);
        if hh < 10 then result := '0' + result;
end;
function ret_hh : string;
begin
        if hh > 12 then
        result := inttostr(hh- 12)  else
        result := inttostr(hh);

        if length(result) = 1 then result := '0' + result;
end;
function ret_dd : string;
begin
        result := inttostr(dd);
        if dd < 10 then result := '0' + result;
end;
function ret_yy : string;
begin
        result := copy(inttostr(yy),3,2);
end;

function ret_mm : string;
begin
        result := inttostr(mm);
        if mm < 10 then result := '0' + result;
end;

function ret_nn : string;
begin
        result := inttostr(nn);
        if nn < 10 then result := '0' + result;
end;

function ret_ss : string;
begin
        result := inttostr(ss);
        if ss < 10 then result := '0' + result;
end;


function ret_h : string;
begin
        result := inttostr(hh);
end;
function ret_d : string;
begin
        result := inttostr(dd);
end;

function ret_m : string;
begin
        result := inttostr(mm);
end;

function ret_n : string;
begin
        result := inttostr(nn);
end;

function ret_s : string;
begin
        result := inttostr(ss);
end;

function ret_p : string;
begin
        if hh > 12 then
        result := 'PM' else
        result := 'AM';
end;

function replace(source : string; fromm, too : string) : string;
var i : integer;
begin
        i := pos(fromm,' ' + source);
        if i <> 0 then
        begin
        dec(i);
        if i = 0 then
        result := too + copy(source,length(fromm),length(source)-length(fromm)+1);
        result := copy(source,0,i-1) + too + copy(source,i + length(fromm),length(source)-i-length(fromm)+1);
        end else
        result := source;
end;

begin
        Date := now;
        Result := '';
        if pos('%',' ' + input) = 0 then
                for i := 1 to length(input) do
                Case input[i] of
                'y' : result := result + '%YYYY%';
                'n' : result := result + '%M%';
                'd' : result := result + '%D%';
                'h' : result := result + '%H%';
                'm' : result := result + '%M%';
                's' : result := result + '%S%';
                else result := result + input[i];
                end else
                result := input;
        Decodetime(date,hh,nn,ss,ms);
        Decodedate(date,yy,mm,dd);

        result := replace(result,'%YYYY%',ret_yyyy);
        result := replace(result,'%MMMM%',ret_mmmm);
        result := replace(result,'%DDDD%',ret_dddd);

        result := replace(result,'%HHH%',ret_hh2);
        result := replace(result,'%MMM%',ret_mmm);
        result := replace(result,'%DDD%',ret_ddd);

        result := replace(result,'%YY%',ret_yy);
        result := replace(result,'%MM%',ret_mm);
        result := replace(result,'%DD%',ret_dd);
        result := replace(result,'%HH%',ret_hh);
        result := replace(result,'%NN%',ret_nn);
        result := replace(result,'%SS%',ret_ss);

        result := replace(result,'%M%',ret_m);
        result := replace(result,'%D%',ret_d);
        result := replace(result,'%H%',ret_h);
        result := replace(result,'%N%',ret_n);
        result := replace(result,'%P%',ret_p);
        result := replace(result,'%S%',ret_s);
end;

function isExistsin(text2find, textstring : string) : boolean;
begin
  Result := (pos(lowercase(Text2find),lowercase(textstring)) <> 0);
end;

function SimpleHash(input : string) : Longint;
var Key : Longint;
    i : integer;
begin
    key := 1;
    for i := 1 to length(Input) do
      Key := Key Xor (Ord(Input[i])*i);
    Result := ((2654435769*Key) and 1023) mod 2048;
end;

function LocateID(CmdName : string) : integer;
var i : integer;
begin
  Result := -1;

  {$IFDEF UserScriptCmdList}
  for i := 0 to MaxScript+UserMaxScript-1 do
  if Lowercase(NewCmdList(i).CommandName) = Lowercase(Cmdname) then
  {$ELSE}
  for i := 0 to MaxScript-1 do
  if Lowercase(ScriptCmdList[i].CommandName) = Lowercase(Cmdname) then
  {$ENDIF}
      begin
      Result := i;
      exit;
      end;
end;

function LocateCmdRec(CmdName : string) : TScriptCmdRec;
begin
  Result := ScriptCmdList[LocateID(CmdName)];
end;

function EncodeAksCommand(AksCommand : TScriptCmdRec) : string;
var r : TScriptCmdRec;
begin
  r := AksCommand;
  if r.Param1 = '' then r.Param1 := GetDefaultParam(r.TParam1);
  if r.Param2 = '' then r.Param2 := GetDefaultParam(r.TParam2);
  with r do Param1 := EnvPath2RealPath(Param1,-1);
  with r do Param2 := EnvPath2RealPath(Param2,-1);

  if r.Tparam2 <> '' then
    Result := Format('%s "%s" "%s"',[r.CommandName,r.Param1, r.Param2]) else
  if r.Tparam1 <> '' then
    Result := Format('%s "%s"',[r.CommandName,r.Param1]) else
  Result := r.CommandName;
end;

function DecodeAksCommand(AksCommand : string) : TScriptCmdRec;
var k : string;
    loc : integer;
    CommandName : string;
function DecodeParam : string;
begin
  Result := '';
  k := trim(k);
  if k <> '' then
    begin
     loc := Pos('"',k);
     if loc <> 0 then
      begin
      k := Copy(k,loc+1,length(k)-loc);
      loc := Pos('"',k);
      if loc <> 0 then
        begin
        Result := Copy(k,1,loc-1);
        k := Copy(k,loc+1,length(k)-loc);
        end;
      end;
    end;
end;
begin
  loc := Pos(' ',AksCommand);
  if loc = 0 then
    begin
    CommandName := AksCommand;
    k := '';
    end else
    begin
    CommandName := Copy(AksCommand,1,loc-1);
    k := Trim(Copy(AksCommand,loc+1,length(AksCommand)-loc));
    end;
    
  {$IFDEF UserScriptCmdList}
  Result := NewCmdList(LocateId(CommandName));
  {$ELSE}
  Result := ScriptCmdList[LocateId(CommandName)];
  {$ENDIF}

  Result.Param1 := DecodeParam;
  Result.Param2 := DecodeParam;
  CurrentParam1 := Result.Param1;
  with Result do if Param1 = '' then Param1 := GetDefaultParam(TParam1);
  with Result do if Param2 = '' then Param2 := GetDefaultParam(TParam2);

  if Executing then
  with Result do Case SimpleHash(lowercase(Commandname)) of
  -1 : begin end;
  else begin
        Param1 := EnvPath2RealPath(Param1,-1);
        Param2 := EnvPath2RealPath(Param2,-1);
       end;
  end;

end;

function GetDefaultParam(ParamType : string) : string;
begin
  if Lowercase(ParamType) = '%archive%' then
    Result := Def_Archive else
  if Lowercase(ParamType) = '%content%' then
    Result := Def_Content else
  if Lowercase(ParamType) = '%directory%' then
    Result := Def_Directory else
  if Lowercase(ParamType) = '%file%' then
    Result := Def_File else
  if Lowercase(ParamType) = '%string%' then
    Result := Def_String else
  if Lowercase(ParamType) = '%bool%' then
    Result := Def_Bool else
  if Lowercase(ParamType) = '%type%' then
    Result := Def_Type;
end;

function DecodeEnvVar(EnvVar : string; index : integer) : string;
var def_cak, def_par : boolean;
    len : integer;
begin
  def_cak := Assigned(Cakdir2);
  def_par := Assigned(ParamList) and (ParamList.Count > 0);
  len := Length(EnvVar);
  Case SimpleHash(Lowercase(Trim(EnvVar))) of
   595{First  } : if (len=5) and Def_Par then Result := ParamList.Strings[0];
   688{1      } : if (len=1) and Def_Par then Result := ParamList.Strings[0];

   270{Name   } : if (len=4) then if(index<>-1) and Def_Par then Result := RemoveFileExt(Extractfilename(ParamList.Strings[index])) else
                                                                 Result := RemoveFileExt(Extractfilename(ParamList.Strings[0]));
   392{Ext    } : if (len=3) then if(index<>-1) and Def_Par then Result := Extractfileext(ParamList.Strings[index]) else
                                                                 Result := Extractfileext(ParamList.Strings[0]);
   23 {Path   } : if (len=4) then if(index<>-1) and Def_Par then Result := Extractfilepath(ParamList.Strings[index]) else
                                                                 Result := Extractfilepath(ParamList.Strings[0]); 

   823{Date   } : if (len=4) then Result := DateStr;
   120{Temp   } : if (len=4) then Result := GrabTempPath;
   874{Windows} : if (len=7) then Result := GrabWindowPath;
   500{Desktop} : if (len=7) then Result := GrabDesktopPath;
   527{Archive} : if (len=7) and Def_cak then Result := Cakdir2.GrabArchivePath;
   679{Mydocu } : if (len=6) then Result := GrabMydocupath;
  else
   if decodetimestr('%'+EnvVar+'%') <> '%'+EnvVar+'%' then Result := decodetimestr('%'+EnvVar+'%') else
    begin
      SetLength(Result, MAX_PATH);
      Result := GetEnvironmentVariable(EnvVar);
    end;
  end;
end;

function EnvPath2RealPath(EnvPath : string; index : integer) : string;
var loc : integer;
    k,l : string;
begin
  loc := pos('%',EnvPath);
  Result := EnvPath;
  if loc <> 0 then
  begin
    k := Copy(EnvPath,loc+1,length(EnvPath)-loc);
    loc := pos('%',k);
    if loc <> 0 then
      begin
        k := Copy(k,0,loc-1);
        l := DecodeEnvVar(k,index);
        Result := EnvPath2RealPath(Replace(EnvPath,'%' + k + '%',l),index);
      end;
    end;
end;

procedure Execute_command(scriptcommand : string);
var r : TScriptCmdRec;
    commands : Tstrings;
begin
  if not assigned(Cakdir2) then exit;
  r := DecodeAksCommand(ScriptCommand);
  Execute_commandEx(r,-1);
end;

procedure Execute_commandEx(scriptcommand : TScriptCmdRec; index : integer);
var len : integer;
    i : integer;
    sf,tf : TStrings;
    commands : Tstrings;
begin
  with scriptcommand do
  begin
  len := length(commandName);
  Case SimpleHash(Lowercase(CommandName)) of
   960{New        }:if (len= 3) then begin
                      MakeDirectory(Extractfilepath(Param1));
                      Cakdir2.ArchiveName := Param1; end;
   785{Open       }:if (len= 4) then begin
                      MakeDirectory(Extractfilepath(Param1));
                      Cakdir2.ArchiveName := Param1; end;
   818{Close      }:if (len= 5) then Cakdir2.ArchiveName := '';
   000{Extr       }:if (len= 4) then begin
                      Cakdir2.Select(Param1,'');
                      Cakdir2.Extractoptions.Extr_to := Param2;
                      Cakdir2.Extract; end;
   771{Extract    }:if (len= 7) then begin
                      Cakdir2.Select(Param1,'');
                      Cakdir2.Extractoptions.Extr_to := Param2;
                      Cakdir2.Extract; end;
   100{Add        }:if (len= 3) then //if fileexists(Param1) then
                      for i := 0 to ParamList.Count - 1 do
                      Cakdir2.AddOptions.Add_Files.Add(EnvPath2RealPath(CurrentParam1,i));
   796{DoAdd      }:if (len= 5) then
                      begin
                        Cakdir2.AddOptions.Add_Relative := false;
                        //Cakdir2.AddOptions.Add_UsePath  := false;
                        Cakdir2.Add;
                      end;
   009{Password   }:if (len= 8) then begin
                      Cakdir2.Password := (Param1);
                      Cakdir2.AddOptions.add_encrypt := Param1;
                      Cakdir2.AddOptions.add_useencrypt := (Param1 <> '');
                      end;
   467{Del        }:if (len= 3) then begin
                      Cakdir2.Select(Param1,'');
                      Cakdir2.Delete; end;
   509{Delete     }:if (len= 6) then begin
                      Cakdir2.Select(Param1,'');
                      Cakdir2.Delete; end;
   478{UseExtrPath}:if (len=11) then Cakdir2.Extractoptions.Extr_DirNames  := (lowercase(Param1) <> 'false');
   409{UseExtrOver}:if (len=16) then Cakdir2.Extractoptions.Extr_Overwrite := (lowercase(Param1) <> 'false');
   769{UseAddPath }:if (len=10) then Cakdir2.AddOptions.Add_UsePath := (lowercase(Param1) <> 'false');
   088{UseRelPath }:if (len=10) then Cakdir2.AddOptions.Add_Relative := (lowercase(Param1) <> 'false');
   242{UseSubDir  }:if (len= 9) then Cakdir2.AddOptions.Add_SubDir := (lowercase(Param1) <> 'false');
   479{Convert    }:if (len= 7) then begin end;
   101{RunFile    }:if (len= 7) then Run(Param1,Param2);
   400{MoveFile   }:if (len= 8) then MoveFile(PChar(Param1),PChar(Param2));
   124{CopyFile   }:if (len= 8) then CopyFile(PChar(Param1),PChar(Param2),true);
   901{RenFile    }:if (len= 7) then RenameFile(PChar(Param1),PChar(Param2));
   495{MakeDir    }:if (len= 7) then MakeDirectory(Param1);
   285{OpenDir    }:if (len= 7) then OpenFolder(Param1);
   710{DateStr    }:if (len= 7) then DateStr := DecodetimeStr(Param1);
   957{Zip64      }:if (len= 5) then if lowercase(param1) <> 'true' then
                                      Cakdir2.Enable_Archiver(TCakDelphiZip,_ZIP) else    
                                      Cakdir2.Enable_Archiver(TCakZipForge ,_ZIP);
   692{TxtFileList}: begin end;
   819{HtmFileList}: begin end;
   595{CsvFileList}: begin end;
   27 {RenameFileList }, 755{CopyFileList   }, 809{BatchZipFileList}
                   :begin
                     ProcessWildcard(Extractfilepath(Param1),Extractfilename(Param1),
                        ExtractFilename(Param2),sf,tf);
                     for i := 0 to tf.count -1 do
                      tf.Strings[i] := Appendslash(ExtractFilePath(Param2))+tf.Strings[i];
                     for i := 0 to tf.count -1 do
                      sf.Strings[i] := Appendslash(ExtractFilePath(Param1))+sf.Strings[i];
                     for i := 0 to sf.count -1 do
                        Case SimpleHash(Lowercase(CommandName)) of
                          27 : if (len=14) then RenameFile(sf.Strings[i],tf.Strings[i]);
                          755: if (len=12) then CopyFile(PChar(sf.Strings[i]),PChar(tf.Strings[i]),true);
                          809: if (len=16) then begin
                                                CakDir2.ArchiveName := tf.Strings[i];
                                                CakDir2.AddOptions.Add_Files.Add(sf.Strings[i]);
                                                CakDir2.Add;
                                                end;
                        end;
                    end;
   519{Msg        }:if Assigned(Cakdir2.FOnMsg) then Cakdir2.FOnMsg(nil,-1,-1,Msg_Warning,Param1);
   754{CloseARC   }:if (len= 8) then Application.Terminate;
  else
   if assigned(Cakdir2.FOnUnkCmd) then
    begin
    commands := TStringList.Create;
    commands.add(Param1);
    commands.add(Param2);
    Cakdir2.FOnUnkCmd(nil,CommandName,commands);
    commands.free;
    end else
   if Assigned(Cakdir2.FOnMsg) then
    Cakdir2.FOnMsg(nil,-1,-1,msg_error,'Command '+CommandName+' not found!');
  end;
  end;
end;

procedure Load_Script(Script : Tstrings);
var i : integer;
begin
  Datestr := Decodetimestr('yyyy-mm-dd');
  Executing := true;
  try
  for i := 0 to Script.Count -1 do
    Execute_Command(Script.Strings[i]);
  finally
    Executing := false;
  end;
end;

function AskForParam(AksCommand : TScriptCmdRec; default : string; variable : integer) : string;
function RequestVariable(VarType : String; DefQuestion : string) : string;
const savefilter = 'Zip archive|*.zip|7z archive|*.7z|Sqx archive|*.sqx|Lzh archive|*.lzh|Lha archive|*.lha|Bza archive|*.bza|Gza archive|*.gza|Bz2 archive|*.bz2|Cab archive|*.cab|Tar archive|*.tar|Tgz archive|*.tgz|Gz archive|*.gz|z archive|*.z|Rs archive|*.rs';
begin
  if {(Vartype = '%file%') or} (Vartype = '%archive%') then
    with TSaveDialog.Create(nil) do
      begin
        DefaultExt := '.zip';
        Filename := Extractfilename(Default);
        Result := Default;
        Options := [ofHideReadOnly,ofExtensionDifferent,ofEnableSizing];
        InitialDir := Extractfilepath(Default);
        if Execute then result := Filename;
      end else
  if vartype = '%bool%' then
     if lowercase(default) = 'false' then Result := 'TRUE' else Result := 'FALSE'


  else Result := InputBox('Please Input',DefQuestion,Default);
end;
begin
  if variable = 1 then
  Result := RequestVariable(Lowercase(AksCommand.TParam1),AksCommand.Param1Question) else
  Result := RequestVariable(Lowercase(AksCommand.TParam2),AksCommand.Param2Question);
end;


end.
