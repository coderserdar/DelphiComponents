unit WinCC;

interface

// Parametreler
// Path        : Çalýþtýrýlacak dosya
// Visibility  : True ise çalýþtýrýlan program show edilir.
// TimeOut     : Programýn çalýþmasýný bekleme süresi (Milisaniye olarak) $FFFFFFFF verilirse
//              iþlem bitene kadar bekler.
function WinExecAndWait(Path: String; Visibility: Boolean;Timeout : DWORD): integer;

implementation

function WinExecAndWait(Path: String; Visibility: Boolean;Timeout : DWORD): integer;
var
  WaitResult        : integer;
  StartupInfo       : TStartupInfo;
  ProcessInfo       : TProcessInformation;
  iResult           : integer;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
       begin
         cb := SizeOf(TStartupInfo);
         dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;

         if visibility then
            wShowWindow := 5
         else
            wShowWindow :=0;
       end;  // with StartupInfo do

  if CreateProcess(nil,
                   PChar(Path),
                   nil,
                   nil,
                   False,
		   NORMAL_PRIORITY_CLASS,
                   nil,
                   nil,
		   StartupInfo,
                   ProcessInfo) then
     begin
       WaitResult := WaitForSingleObject(ProcessInfo.hProcess, timeout);  //
       result := WaitResult;
     end
  else
     result:=GetLastError;
end;

end.

