unit readme;

interface

uses
  windows,classes, sysutils, toolsapi,dialogs;

  {$EXTERNALSYM ShellExecute}
function ShellExecute(hWnd: HWND; Operation, FileName, Parameters, Directory:
  PChar; ShowCmd: Integer): HINST; stdcall; external 'shell32.dll' name
'ShellExecuteA';

type
  TReadme = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    { Private declarations }
  protected
    { Protected declarations }
        procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
          procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation
var
  Index: Integer;


procedure Treadme.AfterCompile(Succeeded: Boolean);
begin
  { Not interested in AfterCompile at this time }
end;

procedure Treadme.BeforeCompile(const Project: IOTAProject;
var Cancel: Boolean);
begin
  { Not interested in BeforeCompile at this time }
end;
procedure TReadme.FileNotification(
NotifyCode: TOTAFileNotification; const FileName: string;
var Cancel: Boolean);
var
  contents: Tstrings;
    lineno : integer;
    astring : string;
begin
  case NotifyCode of
    ofnFileOpened:
      begin
// When modules are opened
         if (extractfileext(Filename)='.dpr') or (extractfileext(Filename)='.dpk') then
         begin
	   if fileexists(filename) then
	   begin
              try
                contents:=tstringlist.create;
                try
                  contents.loadfromFile(Filename);
                  for lineno := 0 to contents.Count - 1 do
                  begin
                     if pos('%readme',contents[lineno])>0 then
                     begin
                       if pos('''',contents[lineno])>0 then
                       begin
                          astring:=contents[lineno];
                          delete(astring,1,pos('''',astring));
                          delete(astring,pos('''',astring),9999);
                          if (extractfileext(astring)='.html') or (extractfileext(astring)='.txt') or (extractfileext(astring)='.htm') or (extractfileext(astring)='.html') or (extractfileext(astring)='.rtf') or (extractfileext(astring)='.doc') then
                          if fileexists(astring) then shellexecute(0,'open',pchar(astring),'','',SW_SHOWNORMAL);
                       end;
                     end;
                  end;
                finally
                  contents.free;
                end;
              except
                showmessage('Unable to load project file');
              end;
	   end;
         end;
      end;
  end;
end;


procedure Register;
begin
  Index := (BorlandIDEServices as IOTAServices).AddNotifier(TReadme.Create);
end;

initialization
finalization
  (BorlandIDEServices as IOTAServices).RemoveNotifier(Index);
end.


