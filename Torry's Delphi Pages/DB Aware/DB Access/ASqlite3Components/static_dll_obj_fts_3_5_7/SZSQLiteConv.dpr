{
SZSQLiteConv ver 1.0.6, 2007-02-24

SQLite file conversion to create only one SQLite.OBJ file
Author: Sasa Zeman, public@szutils.net, www.szutils.net

Thanks to LesNes and his original guide (please read LesNes.txt).
on which bases is created this simple coversion utility. It adapt
necessary SQLite files in order to create only one OBJ file which
will be used to link in Delphi application, as well it produce
necessary batch file.

All you need is to have installed Borland free commandline C++ compiler (BCC55) in
C:\borland\bcc55 directory (it can be changed, please see creating batch file).

Usage:

1. Simply copy SZSQLiteConv.exe to folder where are extracted SQLite sources for windows.

2. Start SZSQLiteConv.exe to create files for specific version.

    Please use SZSQLiteConv <SQLite Version> [Parameters]

    Allowed parameters:
      -a  Allows creating batches for all supported C models
      -t  Allows compiling threadsafe version
      -d  Creates obj version for debugging
      -f  Creates obj version enabling FST1

   Parameters are not necessary. Default C model is cdecl.
   Parameters  can be existed alon or combined freely
   For example: -t, -d, -at, -ta, -td, -da, -adt

   An example:

   SZSQLiteConv 337 -atd

   This will modify/creates .c files for specific SQLite version
   (3.3.7 in example), creates batches for all supported C models,
   allow debugging SQLite C sources from Delphi and make it threadsafe.

3. Utility also adapt sqlite3.def in order to be compiled with
   Borland free commandline C++ compiler (BCC55)

4. To create obj file, execute created batch file.

5. Created obj file can be linked with Delphi, with additional
   necessary library modules

-------------------------------------
History
-------------------------------------
Ver 1.0.6, 2007-02-24
  - Added support to SQLite 3.3.13
  - No need to change 'vdbeint.h' anymore since author put necessary code
  - Parameter version changes. Instead of for example 337, now in necessary
    to write 3.3.7. Reason - build version number > 9.

Ver 1.0.5, 2007-01-29
  - Support from SQLite 3.3.1 up to SQLite 3.3.10
  - Fixed bug modifying 'tokenize.c'
    Thanks to Thomas

Ver 1.0.4, 2006-11-20
  - Added support for SQLite 338
  - Prepared code to create FTS1 version.
    Unfortunately, current changes I made are still not enought!
    Please do not use -f parameter, yet!

Ver 1.0.3, 2006-09-30
  - Added support for SQLite 337
  - Parameter improvements
  - Abbility to create obj for cdecl, stdcall and register C models
  - Abbility to create batches for all supported C modes
  - Abbility to create batches which creates debug version of obj
  - Abbility to create batches which creates threadsafe version of SQLite,
    thanks to Miha Vrhovnik

Ver 1.0.2, 2006-05-31
  - Small bug fixed in CreateBuild, thanks to Noeska

Ver 1.0.1, 2006-04-29
  - Code optimization
  - Report enchancements

Ver 1.0.0, 2006-04-24
  - The first release
-------------------------------------
              

You may use and change this utility by following conditions:

-------------------------------------
1. The origin of this software must not be misrepresented,
 you must not claim that you wrote the original software.
 If you use this software in a product, an acknowledgment
 in the product documentation would be appreciated but is
 not required.

2. You may use this software for any purpose you like, even in
 commercial applications, you may also change source and freely
 redistribute.

3. Changed source must be plainly marked as such, and
 must not be misrepresented as being the original software.

4. If you make changes which improves the software you must
 mail these to original author with complete documentation.

5. This notice may not be removed or changed from any source
 distribution.

6. YOU USE THIS SOFTWARE AT YOUR OWN RISK!
THIS SOFTWARE IS PROVIDED 'AS IS' WITHOUT ANY WARRANTIES.
AUTHOR DENY ANY RESPONSIBILITY FOR ANY DAMAGE MAY ARISE IN
ANY WAY USING THIS SOFTWARE.
-------------------------------------

I hope this little utility will be helpful to automatize
creation of sqlite3.OBJ file in order to include it in Delphi.

Author: Sasa Zeman, public@szutils.net, www.szutils.net

Thanks to:
LesNes, www.lesnes.net, for original guide to create sqlite3.obj
Albert Drent, www.aducom.com, author of ASQLite3 components for Delphi
}

{$APPTYPE CONSOLE}

uses SysUtils, Classes,StrUtils;

type
  //DW
  TParams = (Threaded, AllModels, Debugged, FTS1, FTS3);
  // Threaded  - Creates threadsafe version of SQLite
  // AllModels - Creates batches for cdecl, standard and parameter C models
  // Debugged  - Creates obj version for debugging
  // FTS1      - Creates obj version enabling FTS1
  // FTS3      - Creates obj version enabling FTS3 

  TCModels = (cmCdecl,cmStdcall, cmRegister);

var
  Version: integer;
  Parameters,Filename: string;

  Params: set of TParams;

function ReplaceText(orig: TStrings; i: integer; const s1,s2: string): Boolean;
var
  p: integer;
begin
  result:=false;
  p:=pos(s1,orig[i]);
  if p>0 then
    if pos(s2,orig[i])=0 then
    begin
      orig[i]:= StringReplace(orig[i],s1,s2,[rfReplaceAll, rfIgnoreCase]);
      result:=true;
    end;
end;

function Change_Text_Into_File( F,s1,s2: String): Boolean;
var
  i: integer;
  Basic:TStrings;
begin
  Result:=true;

  Basic:=TStringList.Create;
  try
    Basic.Clear;
    if FileExists(F) then
      Basic.LoadFromFile(F)
    else
    begin
      writeln('Error: File ',F,' does not exists!');
      halt(1);
    end;

    for i:=0 to Basic.Count-1 do
      ReplaceText(Basic,i,s1,s2);

    Basic.SaveToFile(F);

  except
    Result:=false;
  end;
  Basic.free

end;

/////////////////////////////////
// Changing specific SQLite file
/////////////////////////////////

function Change_vdbefifo_c: Boolean;
// Base start from 331
begin
  Result:=Change_Text_Into_File('vdbefifo.c','allocatePage','vdallocatePage');
end;

function Change_tokenize_c: Boolean;
// Base start from 331
begin
  Result:= false;
  Result:= Change_Text_Into_File('tokenize.c','extern','//extern');
  Result:= Change_Text_Into_File('tokenize.c','void*(*)(int)','void*(*)(size_t)');
end;

function Change_vdbeint_h: Boolean;
// Base start from 331
var
  i,p: integer;
  Basic:TStrings;
  F,s1,s2: String;
begin
  F:='vdbeint.h';

  Result:=true;

  Basic:=TStringList.Create;
  try
    Basic.Clear;
    if FileExists(F) then
      Basic.LoadFromFile(F)
    else
    begin
      writeln('Error: File ',F,' does not exists!');
      halt(1);
    end;

//    case Version of
//      3003001..3003005:

    //if version is <3.3.13
    if version<3003013 then

      begin

        s1:='#ifndef _VDBEINT_H_';
        s2:='#define _VDBEINT_H_';

        p:=basic.IndexOf(s1);
        if p<0 then
          Basic.Insert(0,s1);

        if basic.IndexOf(s2)<0 then
          Basic.Insert(1,s2);

        if basic[Basic.Count-1]<>'#endif' then
           basic.Add('#endif');
      end;

      s1:='extern char *sqlite3OpcodeNames[]';
      s2:='#include "opcodes.c"';

      for i:=0 to Basic.Count-1 do
      begin
        ReplaceText(Basic,i,s1,s2);
      end;

    //end;

    Basic.SaveToFile(F);

  except
    Result:=false;
  end;
  Basic.free
end;

function Change_FTS1: Boolean;
begin
  // Following changes are currently not enough!!!
  // Please do not use -f parameter, yet.

  Result:= false;
{
  Result:= Change_Text_Into_File('fts1.c','putVarint','FTS1putVarint');
  Result:= Change_Text_Into_File('fts1.c','getVarint','FTS1getVarint');
  Result:= Change_Text_Into_File('fts1.c','getToken','FTS1getToken');
  Result:= Change_Text_Into_File('fts1_hash.c','strHash','FTS1strHash');
}
end;

function Change_sqlite3_def: Boolean;
// Base start from 331
var
  i,p: integer;
  Basic:TStrings;
  F: String;
begin
  F:='sqlite3.def';

  Result:=true;

  Basic:=TStringList.Create;
  try
    Basic.Clear;
    if FileExists(F) then
      Basic.LoadFromFile(F)
    else
    begin
      writeln('Warning: File ',F,' does not exists!');
      Result:=false;
      Basic.Free;
      exit
    end;

//    case Version of
//      3003001..3003005:
      begin

        for i:=0 to Basic.Count-1 do
        begin

          p:=pos('=',Basic[i]);

          if p<=0 then
          if pos('sqlite3_',Basic[i])>0 then
            Basic[i]:= Basic[i]+' = _'+Basic[i];

        end

      end;
    //end;

    Basic.SaveToFile(F);

  except
    Result:=false;
  end;
  Basic.free
end;

procedure CreateFileName(model: TCModels);
begin
  Filename:='SQLite'+inttostr(Version);

   case model of
      cmCdecl   : Filename:=Filename+'-cdecl';
      cmStdcall : Filename:=Filename+'-stdcall';
      cmRegister: Filename:=Filename+'-register';
   end;

   if Threaded in Params then
      Filename:=Filename+'-threadsafe';


   if FTS1 in Params then
      Filename:=Filename+'-fts1';
   
   //DW
   if FTS3 in Params then
      Filename:=Filename+'-fts3';      

   if Debugged in Params then
      Filename:=Filename+'-debug';
end;

function PatchBtreeIntH:boolean;
var
  lines : TStringList;
begin
  result := true;
  lines := TStringList.Create;
  try
    lines.LoadFromFile('btreeint.h');
    if pos('#define _SQLITE_BTREEINTH_',lines.Text)=0 then begin
      lines.Insert(0,'#ifndef _SQLITE_BTREEINTH_');
      lines.Insert(1,'#define _SQLITE_BTREEINTH_');
      lines.Insert(2,'');
      lines.Append('');
      lines.Append('#endif');
      lines.SaveToFile('btreeint.h');
    end;
  except
    result := false;
  end;
  lines.Free;
end;

function CreateSQLiteCCode(model: TCModels): Boolean;
// Base start from 331
var
  Basic:TStrings;
begin

  CreateFileName(model);

  Result:=true;
  Basic:=TStringList.Create;
  try
    Basic.Clear;

  with Basic do
  begin

    add('#include "pager.c"');
    add('#include "btree.c"');    
    add('#include "vdbefifo.c"');
    add('#include "alter.c"');
    add('#include "analyze.c"');
    add('#include "attach.c"');
    add('#include "auth.c"');
    add('#include "build.c"');
    add('#include "callback.c"');
    add('#include "complete.c"');
    add('#include "date.c"');
    add('#include "delete.c"');
    add('#include "expr.c"');
    add('#include "func.c"');
    add('#include "hash.c"');
    add('#include "insert.c"');
    add('#include "legacy.c"');
    add('#include "main.c"');
    add('#include "os.c"');
    add('#include "os_win.c"');
    add('#include "parse.c"');
    add('#include "pragma.c"');
    add('#include "prepare.c"');
    add('#include "printf.c"');
    add('#include "random.c"');
    add('#include "select.c"');
    add('#include "table.c"');
    add('#include "tokenize.c"');
    add('#include "trigger.c"');
    add('#include "update.c"');
    add('#include "utf.c"');
    add('#include "util.c"');
    add('#include "vacuum.c"');
    add('#include "vdbe.c"');
    add('#include "vdbeapi.c"');
    add('#include "vdbeaux.c"');
    add('#include "vdbemem.c"');   
    add('#include "where.c"');

    if version >= 3003007 then
    begin
      add('#include "loadext.c"');
      add('#include "vtab.c"');
    end;

    {
    if version >= 3003008 then
    begin
      add('#include "fts1.c"');
      add('#include "fts1_porter.c"');
      add('#include "fts1_tokenizer1.c"');
      add('#include "fts1_hash.c"');
    end;
    }
    
    if version >= 3004000 then 
    begin
      add('#include "malloc.c"'); 
      add('#include "vdbeblob.c"');
      add('#include "opcodes.c"');    
    end;

    //DW - btreeint.h
    if version >= 3005000 then
    begin
      PatchBtreeIntH;
    end;

    //DW - include bitvec.c (3.5.6 cvs or 3.5.7 or higher)
    if version >= 3005007 then
    begin
      add('#include "bitvec.c"');
    end;

    //DW - 3.5.1 or higher    
    if version >= 3005001 then 
    begin
      add('#include "btmutex.c"'); 
      add('#include "mutex_w32.c"');
      add('#include "mem1.c"');    
    end;

    //DW - include fault.c > 3.5.5
    if version >= 3005005 then
    begin
      add('#include "fault.c"');
    end;
    
    //DW - include FTS3
    if version >= 3005006 then
    begin
      add('#include "fts3.c"');
      add('#include "fts3_hash.c"');
      add('#include "fts3_porter.c"');
      add('#include "fts3_tokenizer.c"');
      add('#include "fts3_tokenizer1.c"');    
    end;
    

  end;

  Basic.SaveToFile(Filename+'.c');

  except
    Writeln('Error: '+Filename+'.c not produced!');
    Result:=false;
  end;
  Basic.free

end;

function CreateBuild(model: TCModels): Boolean;
var
  Basic:TStrings;
  BorlandDir,s: string;
begin

  Result:=true;
  Basic:=TStringList.Create;
  try
    Basic.Clear;

    Basic.Add('rem Build for '+inttostr(Version));

    CreateFileName(Model);

    BorlandDir:='c:\borland\bcc55';

    s:=BorlandDir+'\bin\bcc32';

    case model of
        cmCdecl   : s:=s+' -pc';
        cmStdcall : s:=s+' -ps';
        cmRegister: s:=s+' -pr';
    end;

    if Threaded in Params then
      s:=s+ ' -tWM -DTHREADSAFE=1';

    if (FTS1 in Params) then
      s:=s+' -DSQLITE_CORE=1 -DSQLITE_ENABLE_FTS1=1';
      
    if (FTS3 in Params) then
      s:=s+' -DSQLITE_ENABLE_FTS3'; 

    if Debugged in Params then
      s:=s+ ' -v';

    s:=s+ ' -RT- -O -w- -6 -I'+BorlandDir+'\include -c';

    Basic.Add( s +' '+Filename+'.c');

    Basic.SaveToFile('Build'+Filename+'.bat');

    Writeln(Filename+' produced');
  except
    Writeln('Error: '+Filename+' not produced!');
    Result:=false;
  end;
  Basic.free

end;

procedure ParamsIdent;
begin
  Params:=[];

  if pos('a',Parameters) >0 then Params:=Params+ [AllModels];
  if pos('t',Parameters) >0 then Params:=Params+ [Threaded];
  if pos('d',Parameters) >0 then Params:=Params+ [Debugged];
  if pos('f',Parameters) >0 then Params:=Params+ [FTS1];
  //DW
  if pos('f3',Parameters) >0 then Params:=Params+ [FTS3] - [FTS1];

end;

procedure CreateAll;
var
  i,p1,p2: TCModels;
begin
  // identify parameters
  ParamsIdent;

  p1:=cmCdecl;

  if AllModels in Params then
    p2:=cmRegister
  else
    p2:=cmCdecl;

  // change vdbefifo.c
  Change_vdbefifo_c;

  // change tokenize.c
  Change_tokenize_c;

  // change vdbeint.h
    Change_vdbeint_h;

  // change fts1.c
  if version>=3003008 then
    Change_fts1;

  for i:=p1 to p2 do
  begin
    //Create adequat sqlite c file
    CreateSQLiteCCode(i);

    // Create build for obj file
    CreateBuild(i);
  end;

  // change sqlite3.def
  Change_sqlite3_def;

end;

var
  s:string;
  i1,i2: integer;
begin

  writeln('SZSQLiteConv ver 1.0.6 DW #03, 2007-11-29');
  writeln('Author: Sasa Zeman, public@szutils.net, www.szutils.net');
  writeln('SQLite file conversion to create only one OBJ file');
  writeln('');
  if paramcount<1 then
  begin
    writeln('Please use SZSQLiteConv <SQLite Version> [Parameters]');
    writeln('Allowed parameters:');
    writeln('  -a  Allows creating batches for all supported C models ');
    writeln('  -t  Allows compiling threadsafe version');
    writeln('  -d  Creates obj version for debugging');
    writeln('  -f  Creates obj version enabling FTS1');
    //DW
    writeln('  -f3  Creates obj version enabling FTS3');    

    writeln;
    writeln('Parameters can be combiend freely, as follows: -at' );
    writeln;
    writeln('For example, for SQLite version 3.3.1:');
    writeln('SZSQLiteConv.exe 3.3.1');
    writeln('  This create batchs for cdecl C model only');
    writeln('SZSQLiteConv.exe 3.3.7 -a');
    writeln('  This create batches for all supported C models');
    writeln('SZSQLiteConv.exe 3.3.7 -at');
    writeln('  This create batches for all supported C models');
    writeln('  which as well compile threadsafe version of SQLite');

    halt(0);
  end;

  try
    s:=paramstr(1);

    //major
    i1:=1;
    i2:=PosEx('.',s,1);
    version:=strtoint(copy(s,i1,i2-i1));


    // minor
    i1:=i2+1;
    i2:=PosEx('.',s,i1);

    version:=version*1000+strtoint(copy(s,i1,i2-i1));

    // build
    i1:=i2+1;
    i2:=length(s)+1;

    version:=version*1000+strtoint(copy(s,i1,i2-i1));

    Parameters:=paramstr(2);
    writeln('Files for SQLite version ',version);
    writeln('---------------------------------------------------');

    CreateAll;
    writeln('---------------------------------------------------');

    writeln('All files are changed and produced successfully.');
    writeln('Please execute desired build');
  except
    writeln('Error: insufficient number of parameters!');
  end;

end.
