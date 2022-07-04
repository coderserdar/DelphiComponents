unit API_globals;

interface

// files
function HaveBackSlash(const Path: string): boolean;
function AddBackSlash(const FileOrDir: string): string;

// datetime
function WeekDay(const DayNum: integer): string;
function DateTimeToGMTStr(const TimeToConvert: tdatetime; OffsetHours: integer): string;

implementation

uses
  windows, sysutils;

const
  daystr: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  monthstr: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

//------------------------------------------------------------------------------
function HaveBackSlash(const Path: string): boolean;
begin
  result:= false;
  if path<>'' then
    result:= (path[length(path)]='\');
end;

//------------------------------------------------------------------------------
function AddBackSlash(const FileOrDir: string): string;
begin
  result:= fileordir;
  if result<>'' then
    if result[length(result)]<>'\' then
      result:= result + '\';  
end;

//------------------------------------------------------------------------------
function WeekDay(const DayNum: integer): string;
begin
  if (daynum>0) and (daynum<8) then
    result:= daystr[daynum]
    else result:='';
end;

//------------------------------------------------------------------------------
function DateTimeToGMTStr(const TimeToConvert: tdatetime; OffsetHours: integer): string;
var
  wDay, wMonth, wYear: Word;
begin
  decodedate(TimeToConvert, wYear, wMonth, wDay);
  result:= format('%s, %d %s %d %s %s',
    [daystr[DayOfWeek(TimeToConvert)],
    wDay,
    monthstr[wMonth],
    wYear,
    FormatDateTime('HH":"NN":"SS', TimeToConvert),
    (inttostr(OffsetHours)+'GMT')]);
end;

end.
