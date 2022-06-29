unit ATxLnk;

interface
function FLinkInfo(const fn: WideString): WideString;

implementation

uses
  Windows,
  ShlObj, ComObj, ActiveX;

function FLinkInfo(const fn: WideString): WideString;
var
  ShellLink: IShellLinkW;
  PersistFile: IPersistFile;
  AnObj: IUnknown;
  buf: array[0 .. Pred(MAX_PATH)] of WideChar;
  fd: array[0 .. 1] of TWin32FindData; //FindDataW doesn't match
begin
  try
    //Access to the two interfaces of the object
    AnObj := CreateComObject(CLSID_ShellLink);
    ShellLink := AnObj as IShellLinkW;
    PersistFile := AnObj as IPersistFile;
    FillChar(buf, Sizeof(buf), 0);

    //Opens the specified file and initializes an object from the file contents
    PersistFile.Load(PWideChar(fn), 0);
    ShellLink.GetPath(buf, MAX_PATH, fd[0], SLGP_UNCPRIORITY);
    Result := buf;
  except
    Result := ''
  end;
end;

end.
