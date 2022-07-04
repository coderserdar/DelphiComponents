
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ReportTools;

interface

{$I STD.INC}

uses
  Classes, Messages, SysUtils, Windows;

{ IEnumReportParams interface }

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
    function SetConnectionInfo(const Info: OleVariant): HResult; stdcall;
  end;

{ IReport interface }

  IReport = interface(IUnknown)
    ['{290188AD-505C-4BA2-B0B7-BA74BF426E71}']
    function GetIdentity(out Identity: TGUID): HResult; stdcall;
    function GetTitle(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetGroup(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetAuthor(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetDescription(Buf: PChar; BufSize: Integer): HResult; stdcall;
    function GetLastModified(out Date: TDateTime): HResult; stdcall;
    function GetIcon(out Icon: HICON): HResult; stdcall;
    function GetVersion(out Version: Double): HResult; stdcall;
    function Print: HResult; stdcall;
    function Preview: HResult; stdcall;
  end;

{ TReport }

  TReport = class(TInterfacedObject, IReportParameters, IReport)
  protected
    { IReportParameters }
    function GetCount(out Count: Integer): HResult; virtual; stdcall;
    function GetName(Index: Integer; Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetKind(Index: Integer; out Kind: Integer): HResult; virtual; stdcall;
    function GetChoices(Index: Integer; Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetValue(Index: Integer; Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function SetValue(Index: Integer; Text: PChar): HResult; virtual; stdcall;
    function SetConnectionInfo(const Info: OleVariant): HResult; virtual; stdcall;
    { IReport }
    function GetIdentity(out Identity: TGUID): HResult; virtual; stdcall;
    function GetTitle(Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetGroup(Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetAuthor(Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetDescription(Buf: PChar; BufSize: Integer): HResult; virtual; stdcall;
    function GetLastModified(out Date: TDateTime): HResult; virtual; stdcall;
    function GetIcon(out Icon: HICON): HResult; virtual; stdcall;
    function GetVersion(out Version: Double): HResult; virtual; stdcall;
    function Print: HResult; virtual; stdcall;
    function Preview: HResult; virtual; stdcall;
  end;

{ Report registration support }

type
  TEnumReportsProc = procedure(Report: IUnknown; Data: Pointer); stdcall;
  TEnumReports = procedure(Proc: TEnumReportsProc; Data: Pointer); stdcall;

{ Report parameter helper routines }

function GetParameterIndex(Report: IUnknown; const Param: string): Integer;
procedure SetParameterValue(Report: IUnknown; Index: Integer; const Value: string); overload;
procedure SetParameterValue(Report: IUnknown; const Param, Value: string); overload;
function GetParameterValue(Report: IUnknown; Index: Integer): string; overload;
function GetParameterValue(Report: IUnknown; const Param: string): string; overload;
function GetParameterChoices(Report: IUnknown; const Param: string): string; overload;
procedure GetParameterChoices(Report: IUnknown; const Param: string; Strings: TStrings); overload;

implementation

{ TReport.IReportParameters }

function TReport.GetCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TReport.GetName(Index: Integer; Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetKind(Index: Integer; out Kind: Integer): HResult;
begin
  Result := E_FAIL;
end;

function TReport.GetChoices(Index: Integer; Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetValue(Index: Integer; Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.SetValue(Index: Integer; Text: PChar): HResult;
begin
  Result := S_OK;
end;

function TReport.SetConnectionInfo(const Info: OleVariant): HResult;
begin
  Result := S_OK;
end;

{ TReport.IReport }

function TReport.GetIdentity(out Identity: TGUID): HResult;
begin
  FillChar(Identity, SizeOf(TGUID), #0);
  Result := S_OK;
end;

function TReport.GetTitle(Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetGroup(Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetAuthor(Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetDescription(Buf: PChar; BufSize: Integer): HResult;
begin
  if BufSize > 0 then
    Buf^ := #0;
  Result := S_OK;
end;

function TReport.GetLastModified(out Date: TDateTime): HResult;
begin
  Date := 0;
  Result := S_OK;
end;

function TReport.GetIcon(out Icon: HICON): HResult;
begin
  Icon := 0;
  Result := S_OK;
end;

function TReport.GetVersion(out Version: Double): HResult;
begin
  Version := 0;
  Result := S_OK;
end;

function TReport.Print: HResult;
begin
  Result := S_OK;
end;

function TReport.Preview: HResult;
begin
  Result := S_OK;
end;

{ Report parameter helper routines }

function GetParameterIndex(Report: IUnknown; const Param: string): Integer;
var
  ReportParameters: IReportParameters;
  Buffer: array[0..1024] of Char;
  S: string;
  I: Integer;
begin
  Result := -1;
  if Supports(Report, IReportParameters, ReportParameters) then
  begin
    S := UpperCase(Param);
    ReportParameters.GetCount(I);
    while I > 0 do
    begin
      Dec(I);
      ReportParameters.GetName(I, Buffer, SizeOf(Buffer));
      if S = UpperCase(Buffer) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure SetParameterValue(Report: IUnknown; Index: Integer; const Value: string);
var
  ReportParameters: IReportParameters;
  I: Integer;
begin
  if Index < 0 then Exit;
  if Supports(Report, IReportParameters, ReportParameters) then
  begin
    ReportParameters.GetCount(I);
    if Index < I then
      ReportParameters.SetValue(Index, PChar(Value));
  end;
end;

procedure SetParameterValue(Report: IUnknown; const Param, Value: string);
var
  ReportParameters: IReportParameters;
  I: Integer;
begin
  I := GetParameterIndex(Report, Param);
  if I > -1 then
  begin
    ReportParameters := Report as IReportParameters;
    ReportParameters.SetValue(I, PChar(Value));
  end;
end;

function GetParameterValue(Report: IUnknown; Index: Integer): string;
var
  ReportParameters: IReportParameters;
  Buffer: array[0..1024] of Char;
  I: Integer;
begin
  Result := '';
  if (Index < -1) and (Supports(Report, IReportParameters, ReportParameters)) then
  begin
    ReportParameters.GetCount(I);
    if Index < I then
    begin
      ReportParameters.GetValue(I, Buffer, SizeOf(Buffer));
      Result := Buffer;
    end
  end;
end;

function GetParameterValue(Report: IUnknown; const Param: string): string;
var
  ReportParameters: IReportParameters;
  Buffer: array[0..1024] of Char;
  I: Integer;
begin
  I := GetParameterIndex(Report, Param);
  if I > -1 then
  begin
    ReportParameters := Report as IReportParameters;
    ReportParameters.GetValue(I, Buffer, SizeOf(Buffer));
    Result := Buffer;
  end
  else
    Result := '';
end;

function GetParameterChoices(Report: IUnknown; const Param: string): string;
var
  ReportParameters: IReportParameters;
  Kind: Integer;
  Buffer: array[0..1024] of Char;
  I: Integer;
begin
  Result := '';
  I := GetParameterIndex(Report, Param);
  if I > -1 then
  begin
    ReportParameters := Report as IReportParameters;
    ReportParameters.GetKind(I, Kind);
    if Kind = pkChoice then
    begin
      ReportParameters.GetChoices(I, Buffer, SizeOf(Buffer));
      Result := Buffer;
    end;
  end;
end;

procedure GetParameterChoices(Report: IUnknown; const Param: string; Strings: TStrings);
begin
  Strings.CommaText := GetParameterChoices(Report, Param);
end;

end.
