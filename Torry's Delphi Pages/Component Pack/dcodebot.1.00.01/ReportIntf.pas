
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ReportIntf;

interface

{$I STD.INC}

uses
  Classes, Messages, SysUtils, Windows, ComObj;

{ IReportParameters interface }

const
  pkText     = 0;
  pkInteger  = 1;
  pkFloat    = 2;
  pkDate     = 3;
  pkChoice   = 4;

type
  IReportParameters = interface(IUnknown)
    ['{290188AC-505C-4BA2-B0B7-BA74BF426E71}']
    function GetCount(out Count: Integer): HResult; stdcall;
    function GetName(Index: Integer; Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetKind(Index: Integer; out Kind: Integer): HResult; stdcall;
    function GetChoices(Index: Integer; Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetValue(Index: Integer; Buf: PChar; BufSize: Integer): HResult; stdcall;
    function SetValue(Index: Integer; Text: PChar): HResult; stdcall;
  end;

{ IReport interface }

  IReport = interface(IUnknown)
    ['{290188AD-505C-4BA2-B0B7-BA74BF426E71}']
    function GetTitle(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetGroup(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetAuthor(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetDescription(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetLastModified(out Date: TDateTime): HResult; stdcall;
    function GetIcon(out Icon: HICON): HResult; stdcall;
    function GetVersion(out Version: Double): HResult; stdcall;
    function Print: HResult; stdcall;
  end;

{ Report registration support }

type
  TEnumReportsProc = procedure(Report: IUnknown; Data: Pointer); stdcall;
  TEnumReports = procedure(Proc: TEnumReportsProc; Data: Pointer); stdcall;

implementation

end.
