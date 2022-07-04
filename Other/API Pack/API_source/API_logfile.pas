unit API_logfile;

//------------------------------------------------------------------------------
// API_logfile, component for threadsafe logging 
// copyright (c) 2004-2008 ari pikivirta
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// 20052009, buffer r1.00, ari pikivirta
// 20032009, logfile r1.15, ari pikivirta
//  * added new component to handle external logging systems TAPI_logbuffer
//
// 20032009, r1.15, ari pikivirta
//  * rewrote whole shit
//
// 08012009, r1.14, ari pikivirta
//  * lots simpler structure with always using memory log
//  * removed unneeded variables and functions
//  * added use of Loaded procedure to load existing log into memory on start
//  * whole file access uses file stream instead of textfile handle
//
// 07012009, r1.13, ari pikivirta
//  * added onlogfull event to allow change file on maxlines happen
//
// 20062008, r1.12, ari pikivirta
//  * changed lines function to procedure (to avoid easily cause memory leaks
//    on misuse that compiler doesn't regognize)
//
// 11012008, r1.11, ari pikivirta
//  * fixed bug in adding log lines if no maximum log lines were defined
//
// 08012008, r1.10, ari pikivirta
//  * removed onlogfull event, is not really needed
//  * added memory log listing possibility via enabling from property (faster access to list)
//  * on appending log file, file will remain open until closed or changed (much faster)
//
// 01082007, r1.09, ari pikivirta
//  * replaced criticalsections with mrews
//  * removed unnecessary lockings on get and clear logfile
//  * added missing error(s) to clear and getlist
//
// 31012007, r1.08, ari pikivirta
//  * fixed bug on addmsg, did not place any message unless empty string!!!!
//
// 14012007, r1.07, ari pikivirta
//  * removed lines property, now the log file is loaded in that function
//  * removed loadonstartup property
//
// 30102006, r1.06, ari pikivirta
//  * changed some of the properties to functions for easier understanding
//  * added after log event
//  * removed timestamp from the beginning (as an option now)
//
// 18102006, r1.05, ari pikivirta
//  * added critical section to allow this to be used directly from the threads also
//  * added load lines on startup property
//
// 02042006, r1.04, ari pikivirta
//  * added some events on critical points of component work
//  * changed saving of limited log to use stringlists internal saving function
//
// 12102004, r1.03, ari pikivirta
// * changed msg property to addmsg for easier understanding
//
// r1.02, 17082004, ari pikivirta
// * fixed the default maxlines property to 0 (does not clear the file
//   when logging lines next time)
//
// r1.01, ari pikivirta
// * added lines property to get log file visible easily into listbox for example
// * added linecount property (<1 count logs all lines to file >0 logs only set amount of lines
//
// r1.00, ari pikivirta

interface

uses
  Windows, SysUtils, Classes, syncobjs, API_base, Dialogs;

const
  MAXLOGLINES = high(SMALLINT); // 0..32k

type
  TAPI_logfile_OnLog = procedure (sender: tcomponent; var Txt: string ) of object;
  TAPI_logfile_AfterLog = procedure (sender: tcomponent; const Txt: string) of object;
  TAPI_logfile_OnFull = procedure (sender: tcomponent; var DontRemoveLines: Boolean) of object;

  TAPI_logfile = class(TAPI_Custom_Component)
  private
    flock: tcriticalsection;                // critical sections to make it threadsafe
    ffilename: string;                      // log file name
    ftextfile: textfile;                    // handle to text file
    ffileopen: boolean;                     // text file opened memory
    flinecount: integer;                    // line count memory
    flasttime: double;                      // last stored message time stamp
    flastmessage: string;                   // last stored message content
    fmaxlines: integer;                     // max lines to store into file
    fonlog: TAPI_logfile_OnLog;             // onlog event
    fafterlog: tapi_logfile_afterlog;       // afterlog event
    fonlogfull: TAPI_logfile_OnFull;        // log full (before) anything is removed
    ftimestamp: boolean;                    // enable add timestamp as prefix
    foncloselog: TNotifyEvent;
    fonopenfilefailed: TNotifyEvent;
    procedure int_Clear;
    procedure setfilename(s: string);
    function  getfilename: string;
    procedure setmaxlines(i: integer);
    function  getmaxlines: integer;
    procedure settimestamp(b: boolean);
    function  gettimestamp: boolean;
    procedure int_closelogfile;
    procedure DoOnLogFull(var DoNotRemove: Boolean);
    procedure Int_Openlogfile;
  protected
    procedure Loaded; override;
  public
    constructor Create(aowner: tcomponent); Override;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Lines(List: tstrings);
    function    LineCount: integer;
    function    LastMessage: string;
    function    LastTime: tdatetime;
    procedure   Add(Const TexttoLog: string);
    function    LogFileOpen: boolean;
    procedure   CloseLogFile;
  published
    property Filename: string read getfilename write setfilename;               // log file name
    property MaxLines: integer read getmaxlines write setmaxlines;              // 0 = no limitation
    property TimeStamp: boolean read gettimestamp write settimestamp;
    property OnLog: TAPI_logfile_OnLog read fonlog write fonlog;                // on new message event
    property AfterLog: TAPI_logfile_Afterlog read fafterlog write fafterlog;
    property OnLogFull: TAPI_logfile_OnFull read fonlogfull write fonlogfull;
    property OnCloseLog: TNotifyEvent read foncloselog write foncloselog;
    property OnOpenFailed: TNotifyEvent read fonopenfilefailed write fonopenfilefailed;
  end;

  // log buffer component to use for external logging purposes
  TAPI_logbuffer = class(TAPI_Custom_Component)
    private
      ALock: TCriticalSection;            // critical section to access buffer
      ACount: Integer;                    // buffer size
      AItem: array of String;             // internal buffer
      AMaxCount: Integer;
      procedure SetSize(const I: Integer);
      function GetSize: Integer;
      procedure int_maintainbuffer;          // do maintaining (amaxcount)
      procedure int_deletefirst;
    public
      constructor Create(AOWner: TComponent); Override;
      destructor Destroy; Override;       // free critical section etc.
      procedure Clear;                    // clear buffer
      function Count: Integer;            // count of items in buffer
      function FirstLine: string;         // returns last line without clearing it
      function GetFirstLine: string;      // returns and removes last line
      procedure DeleteFirst;              // delete last line of buffer
      procedure Add(const S: String);     // add message to buffer
    published
      property Size: integer read GetSize write SetSize;
  end;

procedure Register;

implementation

uses
  api_strings;

const
  versioninfostring_logfile = 'r1.15/ari.pikivirta(at)kolumbus.fi';
  versioninfostring_buffer = 'r1.00/ari.pikivirta(at)kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_logfile.create(aowner: tcomponent);
begin
  inherited create(aowner);
  flock:= tcriticalsection.create;
  version:= versioninfostring_logfile;
  ffilename:= '';
  ffileopen:= FALSE;
  flinecount:= 0;
  fmaxlines:= 0;
  ftimestamp:= TRUE;
end;

//------------------------------------------------------------------------------
destructor TAPI_logfile.destroy;
begin
  closelogfile;
  freeandnil(flock);
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.LogFileOpen: boolean;
begin
  flock.acquire;
  try
    result:= ffileopen;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.int_closelogfile;
begin
  if (ffileopen) then
  begin
    // close file
    flush(ftextfile); // ensure all was written to log
    closefile(ftextfile); // close text file handle
    ffileopen:= FALSE; // set open flag to false
    flinecount:= 0; // no lines anymore
    // fire event
    if assigned(foncloselog) then foncloselog(self);
  end;
end;

procedure TAPI_logfile.closelogfile;
begin
  flock.acquire;
  try
    int_closelogfile;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.setfilename(s: string);
begin
  flock.Acquire;
  try
    // if name is changed
    if (s<>ffilename) then
    begin
      int_closelogfile;
      ffilename:= s;
      int_openlogfile;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.getfilename: string;
begin
  flock.Acquire;
  try
    result:= ffilename;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.settimestamp(b: boolean);
begin
  flock.Acquire;
  try
    ftimestamp:= b;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.gettimestamp: boolean;
begin
  flock.Acquire;
  try
    result:= ftimestamp;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.setmaxlines(i: integer);
begin
  flock.Acquire;
  try
    if (i>-1) and (i<>fmaxlines) then
    begin
      fmaxlines:= i;
      //int_closelogfile; // close & flush
      //int_openlogfile;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.getmaxlines: integer;
begin
  flock.Acquire;
  try
    result:= fmaxlines;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.Lines(List: tstrings);
begin
  // loads all lines from log file to list given
  // as parameter, in case log file was open -
  // it will be first closed for the reading operation
  flock.acquire;
  try
    int_closelogfile;
    api_strings.OpenFileToStrings(ffilename, list);
    int_openlogfile;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.LineCount: integer;
begin
  flock.acquire;
  try
    result:= flinecount;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.LastMessage: string;
begin
  flock.Acquire;
  try
    result:= flastmessage;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.int_Clear;
begin
  int_closelogfile; // close file if open (also clear memory log)
  if fileexists(ffilename) then
  try
    deletefile(ffilename); // delete existing log file
  except
    raise Exception.create('Failed to delete '+ffilename);
  end;
  int_openlogfile;
end;

procedure TAPI_logfile.Clear;
begin
  flock.Acquire;
  try
    int_Clear;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_logfile.LastTime: tdatetime;
begin
  flock.Acquire;
  try
    result:= flasttime;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.DoOnLogFull(var DoNotRemove: Boolean);
begin
  donotremove:= FALSE; // by default, we allow roll over
  if assigned(fonlogfull) then
  begin
    fonlogfull(self, donotremove);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.Int_Openlogfile;
var
  lines: tstringlist;
begin
  if (csDesigning in Componentstate) then exit;

  // open (reserve file) in case not done yet, and in case
  // file exists, retrieve current line count from it to
  // allow logging to roll over if needed (load previous,
  // contents etc.)
  if (not ffileopen) and (ffilename<>'') then
  begin
    if fileexists(ffilename) then
    begin
      // retrieve current line count
      lines:= tstringlist.create;
      try
        api_strings.OpenFileToStrings(ffilename, lines);
        flinecount:= lines.count;
      finally
        lines.free;
      end;
      {$i-}
      assignfile(ftextfile, ffilename);
      append(ftextfile);
      {$i+}
      ffileopen:= (ioresult=0);
    end else
    begin
      {$i-}
      assignfile(ftextfile, ffilename);
      rewrite(ftextfile);
      {$i+}
      ffileopen:= (ioresult=0);
      flinecount:= 0;
    end;
    // launch event on log file open failed in case assigned
    if (assigned(fonopenfilefailed)) and (not ffileopen) then
    begin
      fonopenfilefailed(self);
      flinecount:= 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.Loaded;
begin
  // on loading component, we'll check if name was already
  // defined before this and open the file (and retrieve
  // current line count)
  flock.acquire;
  try
    int_openlogfile;
  finally
    flock.release;
  end;

  // continue..
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_logfile.Add(Const TexttoLog: string);
var
  s: string;
  dontremove: boolean;
  ml: integer;
begin
  if (TextToLog='') then exit;

  // max line(s) check
  ml:= getmaxlines;
  if (ml>0) and (linecount>ml) then
    begin
      DoOnLogFull(dontremove);
      if (not dontremove) then Clear; // start over from line 0
      // << on TRUE allow change filename on the event
      // << or to continue to same file even max line is gone over
    end;

  // enter critical section
  flock.Acquire;
  try
    // make sure we've log file open
    int_openlogfile;

    // prepare line to log
    s:= TexttoLog;
    if ftimestamp then s:= datetimetostr(now)+'> '+s;         // add time stamp
    flasttime:= now;                                          // store as last time stamp
    flastmessage:= s;                                         // store as last message

    // add to log (note: called inside lock)
    if assigned(fonlog) then fonlog(self, s);

    // save to file
    if (ffileopen) then
    begin
      writeln(ftextfile, s);
      flinecount:= flinecount + 1; // only increase on writing file
    end;

    // after logging line (note: inside lock)
    if assigned(fafterlog) then fafterlog(self, s);

  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
(*
  TAPI_logbuffer = class(TAPI_Custom_Component)
    private
      ALock: TCriticalSection;            // critical section to access buffer
      ACount: Integer;                    // buffer size
      AItem: array of String;             // internal buffer
      AMaxCount: Integer;
      procedure SetSize(const I: Integer);
      function GetSize: Integer;
    public
      constructor Create(AOWner: TComponent); Override;
      destructor Destroy; Override;       // free critical section etc.
      procedure Clear;                    // clear buffer
      function Count: Integer;            // count of items in buffer
      function LastLine: string;          // returns last line without clearing it
      function GetLastLine: string;       // returns and removes last line
      procedure DeleteLast;               // delete last line of buffer
      procedure Add(const S: String);     // add message to buffer
    published
      property Size: integer read SetSize write GetSize;
  end;
*)

constructor TAPI_logbuffer.Create(AOWner: TComponent);
begin
  inherited Create(AOwner);
  Version:= versioninfostring_buffer;
  Alock:= tcriticalsection.create;
  AMaxCount:= 256;
  Clear;
end;

destructor TAPI_logbuffer.Destroy;
begin
  Clear;
  Alock.free;
  inherited Destroy;
end;

procedure TAPI_logbuffer.Clear;
begin
  Alock.Acquire;
  try
    Acount:= 0;
    setlength(aitem, acount);
  finally
    Alock.release;
  end;
end;

procedure TAPI_logbuffer.int_maintainbuffer;
begin
  // THIS MUST BE CALLED INSIDE LOCK!
  while acount>=amaxcount do
    int_deletefirst;
end;

procedure TAPI_logbuffer.SetSize(const I: Integer);
begin
  alock.Acquire;
  try
    if i<>amaxcount then
    begin
      amaxcount:= i;
      int_maintainbuffer;
    end;
  finally
    alock.Release;
  end;
end;

procedure TAPI_logbuffer.int_deletefirst;
var
  i: integer;
begin
  // THIS MUST BE CALLED INSIDE LOCK
  for i:=0 to acount-2 do aitem[i]:= aitem[i+1];
  acount:= acount - 1;
  setlength(aitem, acount);
end;

procedure TAPI_logbuffer.DeleteFirst;
begin
  alock.Acquire;
  try
    int_deletefirst;
  finally
    alock.release;
  end;
end;

procedure TAPI_logbuffer.Add(const S: string);
begin
  alock.Acquire;
  try
    acount:= acount + 1;
    setlength(aitem, acount);
    aitem[acount-1]:= S;
    int_maintainbuffer;
  finally
    alock.Release;
  end;
end;

function TAPI_logbuffer.GetSize;
begin
  alock.acquire;
  try
    result:= amaxcount;
  finally
    alock.release;
  end;
end;

function TAPI_logbuffer.Count;
begin
  alock.Acquire;
  try
    result:= acount;
  finally
    alock.release;
  end;
end;

function TAPI_logbuffer.FirstLine;
begin
  alock.Acquire;
  try
    if acount>0 then result:= aitem[0]
      else result:= '';
  finally
    alock.release;
  end;
end;

function TAPI_logbuffer.GetFirstLine;
begin
  alock.Acquire;
  try
    if acount>0 then
    begin
      result:= aitem[0];
      int_deletefirst;
    end else
      result:= '';
  finally
    alock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_logFile, TAPI_LogBuffer]);
end;

end.
