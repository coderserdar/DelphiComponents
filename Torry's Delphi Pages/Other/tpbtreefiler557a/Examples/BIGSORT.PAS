{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$S-,R-,V-,I-,B-,F-}

program BigSort;
  {-DOS filter to sort text files}

uses
  Dos,
  MSort;

const
  BufSize  = 8192;
  TempPath : PathName = '';
  StdInHandle = 0;           {handle numbers for OpenStdDev}
  StdOutHandle = 1;
  StdErrHandle = 2;
  StdPrnHandle = 4;

type
  TextBuffer = array[1..BufSize] of Char;

var
  Status : MSortStatus;      {Success of sort}
  StdErr : Text;             {For status reporting}
  TextBuf : TextBuffer;      {Speeds input and output}
  GetProc : Pointer;         {Points to Input procedure to use}
  PutProc : Pointer;         {Points to Output procedure to use}
  LessFunc : Pointer;        {Points to Less function to use}
  IgnoreCase : Boolean;      {True to sort ignoring case}
  Reverse : Boolean;         {True to sort in reverse order}
  StdErrBuf : Char;          {Forces StdErr to write a char at a time}


  procedure Error(msg : string);
    {-Report error through StdErr and halt}
  begin
    WriteLn(StdErr, ^M^J, msg);
    Close(StdErr);
    Halt(1);
  end;

  function OpenStdDev(var F : Text; StdHandle : Word) : Boolean;
    {-Assign the text file to the specified standard DOS device}
  begin
    OpenStdDev := False;
    case StdHandle of
      StdInHandle,
      StdOutHandle,
      StdErrHandle,
      StdPrnHandle :
        begin
          {Initialize the file variable}
          Assign(F, '');
          Rewrite(F);
          if IoResult = 0 then begin
            TextRec(F).Handle := StdHandle;
            if StdHandle = StdErrHandle then
              TextRec(F).BufSize := 1;
            OpenStdDev := True;
          end;
        end;
    end;
  end;

  function HandleIsConsole(Handle : Word) : Boolean;
    {-Return true if handle is the console device (input or output)}

  var
    Regs           : Registers;

  begin
    with Regs do begin
      AX := $4400;
      BX := Handle;
      MsDos(Regs);
      if (DX and $80) = 0 then
        HandleIsConsole := False
      else
        HandleIsConsole := (DX and $02 <> 0) or (DX and $01 <> 0);
    end;
  end;

  function StUpcase(S : String) : String;

  var
    I              : Word;

  begin
    for I := 1 to Length(S) do
      S[I] := UpCase(S[I]);
    StUpCase := S;
  end;

  {$F+}

  procedure GetStr;
    {-Read all of the strings from the standard input}
  var
    s : string;
  begin
    SetTextBuf(Input, TextBuf, BufSize);
    while not(eof) do begin
      FillChar(S,SizeOf(String),0);
      ReadLn(s);
      if IoResult <> 0 then
        Error('Error reading input');
      {Add the element to the sort set}
      if not(PutElement(s)) then
        Exit;
    end;
  end;

  procedure PutStr;
    {-Write the sorted strings to the standard output}
  var
    s : string;
  begin
    SetTextBuf(Output, TextBuf, BufSize);
    while GetElement(s) do begin
      WriteLn(s);
      if IoResult <> 0 then
        Error('Error writing output');
    end;
    Write(^Z);
  end;

  function Less(var X, Y) : Boolean;
    {-Compare two strings}
  var
    Xs : String absolute X;
    Ys : String absolute Y;

  begin
    if IgnoreCase then begin
      if Reverse then
        Less := (StUpcase(Ys) < StUpcase(Xs))
      else
        Less := (StUpcase(Xs) < StUpcase(Ys));
    end else begin
      if Reverse then
        Less := (Ys < Xs)
      else
        Less := (Xs < Ys);
    end;
  end;
  {$F-}

  procedure Unrecognized(msg : string);
    {-Report error for unrecognized command line option}
  begin
    Error('Unrecognized option: '+msg);
  end;

  function GetArgNumber(var I : Integer) : Integer;
    {-Return number following argument i}
  var
    Code : Word;
    Num : Integer;
  begin
    if I >= ParamCount then
      Error('Numeric value does not follow '+ParamStr(I));
    Inc(I);
    Val(ParamStr(I), Num, Code);
    if Code <> 0 then
      Error('Invalid numeric value '+ParamStr(I));
    GetArgNumber := Num;
  end;

  procedure GetOptions;
    {-Analyze the command line for options}
  var
    Arg : string;
    I : Integer;
  begin

    {Default state}
    IgnoreCase := False;
    Reverse := False;

    I := 1;
    while I <= ParamCount do begin

      Arg := ParamStr(I);
      if Length(Arg) <> 2 then
        Unrecognized(Arg);
      if (Arg[1] <> '/') and (Arg[1] <> '-') then
        Unrecognized(Arg);

      case Upcase(Arg[2]) of
        'R' : Reverse := True;
        'I' : IgnoreCase := True;
        'T' : begin
                Inc(I);
                TempPath := ParamStr(I);
              end;
      else
        Unrecognized(Arg);
      end;

      Inc(I);
    end;

    GetProc := @GetStr;
    PutProc := @PutStr;
    LessFunc := @Less;
  end;

 procedure GetBufferSizes(var RunLen, BufLen : Word); {!!.07}
 var
   L : LongInt;
 begin
   {try to calculate a big run length}
   BufLen   := $2000;
   L := (MaxAvail - (BufLen + $8000)) div SizeOf(String);
   {Limit number of run buffers} {!!.40}
   if L > 512 then               {!!.40}
     L := 512;                   {!!.40}
   RunLen := L;
 end;

var
  RunLength,BufLength
                   : Word;

begin

  {Open StdErr for status reporting}
  if not(OpenStdDev(StdErr, 2)) then begin
    WriteLn('Error opening StdErr');
    Halt(1);
  end else
    {Force buffer flush every character}
    SetTextBuf(StdErr, StdErrBuf, 1);
  WriteLn(StdErr, 'Big Sort II. Copyright (c) 1989,1994 by TurboPower Software. Version 5.55');

  {Make sure input was redirected}
  if HandleIsConsole(0) then begin
    {It wasn't, write some help}
    WriteLn(StdErr);
    WriteLn(StdErr, 'Usage: BIGSORT [Options] <InputFile >OutputFile');
    WriteLn(StdErr);
    WriteLn(StdErr, 'Options:');
    WriteLn(StdErr, '  /R     Sort in reverse order');
    WriteLn(StdErr, '  /I     Sort ignoring case');
    WriteLn(StdErr, '  /T dir Use directory dir to store temporary sort files');
    Error('');
  end;

  {Analyze the command line and set defaults}
  GetOptions;

  {Sort strings}
  GetBufferSizes(RunLength,BufLength);
  Status := DoSort(RunLength, SizeOf(String)-1, BufSize, BufSize, TempPath,
                   GetProc, LessFunc, PutProc);

  {Were we successful?}
  case Status of
    MSortSuccess          : WriteLn(StdErr,'File sorted');
    MSortOutOfMemory      : Error('Insufficient memory');
    MSortDiskError        : Error('Disk Error');
    MSortOutOfDisk        : Error('Insufficient disk space for merge');
    MSortEMSError         : Error('EMS error');
    else Error('Unknown return status');
  end;

  Close(StdErr);
end.
