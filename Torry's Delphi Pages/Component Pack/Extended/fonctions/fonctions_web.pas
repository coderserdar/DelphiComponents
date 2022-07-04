unit fonctions_web;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\extends.inc}


interface
{$IFDEF VERSIONS}
uses fonctions_version ;

const
  gVer_fonctions_web : T_Version = ( Component : 'Fonctions Web' ; FileUnit : 'fonctions_web' ;
                        			                 Owner : 'Matthieu Giroux' ;
                        			                 Comment : 'Fonctions de lien web.' ;
                        			                 BugsStory : '0.0.4.1 : On peut aller sur FIREFOX sur FPC.'
                                                         + '0.0.4.0 : Ne compile pas sur DELPHI.';
                        			                 UnitType : 1 ;
                        			                 Major : 0 ; Minor : 0 ; Release : 4 ; Build : 1 );

{$ENDIF}


{$IFDEF FPC}
procedure OpenURL(const URL: string);
function SearchExecutable(const ShortFilename: string; var Filename: string ): boolean;
procedure GetBrowser(var i:longint;out BrowserName, BrowserFilename, StartScriptFilename: string);
{$ENDIF}

const ga_browsers : Array [ 0..7 ] of string = ( 'xdg-open', 'Firefox', 'Galeon', 'Konqueror', 'Mozilla', 'Netscape', 'Opera', 'Iexplorer' );
implementation

uses
  SysUtils,
{$IFDEF FPC}
  LCLProc, FileUtil, AsyncProcess, process,
{$ELSE}
  Windows,
{$ENDIF}
   Classes, Forms, Dialogs;

{$IFDEF FPC}
function SearchExecutable(const ShortFilename: string; var Filename: string
  ): boolean;
begin
  Filename:=SearchFileInPath(ShortFilename,'',
                      GetEnvironmentVariableUTF8('PATH'),PathSeparator,[]);
  Result:=Filename<>'';
end;
procedure OpenURL(const URL: string);
var
  TheProcess: TAsyncProcess;
  BrowserFilename: string;
  StartScriptFilename: string;
  BrowserName: string;
  i: Longint ;
begin
  i:= 0 ;
  BrowserFilename:='';
  while (( i <= high ( ga_browsers )) and ( BrowserFilename='' )) do GetBrowser(i, BrowserName,BrowserFilename,StartScriptFilename);
  if BrowserFilename='' then begin
    DebugLn('OpenURL unable to find browser "',BrowserName,'"');
    MessageDlg('Invalid browser',
           'Unable to find browser executable "'+BrowserName+'"',
           mtError,[mbCancel],0);
    exit;
  end;

  TheProcess:=TAsyncProcess.Create(nil);
  try
    TheProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutput];
    TheProcess.ShowWindow := swoNone;
    TheProcess.CommandLine:=BrowserFilename+' '+URL;
    try
      TheProcess.Execute;
      TheProcess.WaitOnExit;
      if TheProcess.ExitStatus<>0 then begin
        MessageDlg('Error',
          'Error executing browser script '+StartScriptFilename+#13
          +'Error code: '+IntToStr(TheProcess.ExitStatus),
          mtError,[mbCancel],0);
      end;
    finally
      TheProcess.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TForm1.OpenURL ERROR: ',E.Message);
    end;
  end;
end;

procedure GetBrowser(var i:longint;out BrowserName, BrowserFilename, StartScriptFilename: string);
begin
  if (i<low(ga_browsers)) or ( i>high (ga_browsers)) then i:=0;
  BrowserName:=ga_browsers[i];

  if not SearchExecutable(lowercase(BrowserName),BrowserFilename) then
    BrowserFilename:='';
  inc ( i );
end;

{$ENDIF}


initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_web );
{$ENDIF}
end.

