{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1999,2000 Alexey Popov          }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Adopted from Alex Ghost Library by Alexey Popov       }
{*******************************************************}
unit RxMailBox;

{$I RX.INC}

interface

uses Classes;

{$A-}

type

  { TRxCustomMailBox }

  TRxCustomMailBox = class(TObject)
  private
    FHeaderSize, irec_size: integer;
    FMsg: string;
    FMsgStart, FMsgSize: cardinal;
    FDir, FFullDir, FFileName, FName, FFN: string;
    FMsgNum: integer;
    procedure SetDir(Dir: string);
    procedure SetFileName(AFileName: string);
    procedure SetName(AName: string);
    function GetMsgCount: integer;
    function GetMailBoxSize: cardinal;
    function GetMailBoxTrash: cardinal;
    function GetMsgSize: cardinal;
  protected
    FHeader: pointer;
    procedure GetHeader; virtual;
    procedure SetHeader; virtual;
  public
    constructor Create(HeaderSize: integer);
    destructor Destroy; override;
    procedure AddMessage; virtual;
    function SetToMessage(Number: integer): boolean; virtual;
    function LoadMessage: boolean;
    function DeleteMessage: boolean;
    function CompressMailBox: boolean;
    function EmptyMailBox: boolean;
    procedure CreateMailBoxFiles;
    procedure UpdateHeader; virtual;
    property Directory: string read FDir write SetDir;
    property FileName: string read FFileName write SetFileName;
    property Name: string read FName write SetName;
    property MsgCount: integer read GetMsgCount;
    property MsgSize: cardinal read GetMsgSize;
    property MsgNumber: integer read FMsgNum;
    property Message: string read FMsg write FMsg;
    property MailBoxSize: cardinal read GetMailBoxSize;
    property MailBoxTrash: cardinal read GetMailBoxTrash;
  end;

  { TRxMailBox }

  TMailHeader = record
    FromName, ToName: string;
    Subject: string;
    DateSend, DateRec: TDateTime;
    ReadFlag: boolean;
  end;

  TRxMailBox = class(TRxCustomMailBox)
  protected
    procedure GetHeader; override;
    procedure SetHeader; override;
  public
    Header: TMailHeader;
    Data: pointer;
    constructor Create;
  end;

  { TRxMailBoxManager }

  TRxMailBoxList = class(TList)
  private
    function Get(const Index: integer): TRxMailBox;
  public
    destructor Destroy; override;
    property Items[const Index: integer]: TRxMailBox read Get; default;
    function IndexOfFileName(FileName: string): integer;
  end;

  TChangeMailBoxEvent = procedure(Sender: TObject; Index: integer) of object;

  TRxMailBoxManager = class(TComponent)
  private
    FDir: string;
    FMailBoxes: TRxMailBoxList;
    FMBDesign: TStrings;
    FMBIndex: integer;
    FChangeMBEvent: TChangeMailBoxEvent;
    procedure SetDir(Dir: string);
    procedure SetMBDesign(Value: TStrings);
    function GetCurrentMB: TRxMailBox;
    procedure SetCurrentMB(Value: TRxMailBox);
    procedure SetMBIndex(Value: integer);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MailBoxes: TRxMailBoxList read FMailBoxes;
    function AddMailBox(FileName, Name: string): TRxMailBox;
    procedure DeleteMailBox(Index: integer);
    property MailBox: TRxMailBox read GetCurrentMB write SetCurrentMB;
    property MailBoxIndex: integer read FMBIndex write SetMBIndex;
  published
    property MailBoxDirectory: string read FDir write SetDir;
    property MailBoxesDesign: TStrings read FMBDesign write SetMBDesign;
    property OnChangeMailBox: TChangeMailBoxEvent read FChangeMBEvent
      write FChangeMBEvent;
  end;

implementation

uses Windows, SysUtils, RxFileUtil;

resourcestring
  StrIdx = '.idx';
  StrTmpIdx = '.id$';
  StrMbx = '.mbx';
  StrTmpMbx = '.mb$';

type
  Tirec = array [0 .. 2] of cardinal;

  THeaderRec = packed record
    FromName, ToName: string[80];
    Subject: string[80];
    DateSend, DateRec: string[40];
    ReadFlag: boolean;
  end;

{ TRxCustomMailBox }

constructor TRxCustomMailBox.Create(HeaderSize: integer);
begin
  FDir := ExtractFilePath(ParamStr(0));
  FFileName := 'noname';
  FFN := FDir + FFileName;
  FName := 'Noname';
  FHeaderSize := HeaderSize;
  irec_size := SizeOf(cardinal) * 2 + FHeaderSize;
  FMsg := '';
  FMsgStart := 0;
  FMsgSize := 0;
  FMsgNum := -1;
  GetMem(FHeader, FHeaderSize);
  FillChar(FHeader^, FHeaderSize, 0);
end;

destructor TRxCustomMailBox.Destroy;
begin
  FreeMem(FHeader);
  inherited Destroy;
end;

procedure TRxCustomMailBox.SetDir(Dir: string);

  function ExpandRelativePath(const Path, RelPath: string): string;
  var
    s: string;
  begin
    s := Path;
    if length(ExtractFileDrive(s)) = 0 then
    begin
      while (Copy(s, 1, 1) =
        {$IFDEF RX_D6}SysUtils.PathDelim{$ELSE}'\'{$ENDIF}) do
        Delete(s, 1, 1);
      s := NormalDir(RelPath) + s;
    end;
    Result := s;
  end;

begin
  FDir := Dir;
  FFullDir := ExpandRelativePath(Dir, ExtractFilePath(ParamStr(0)));
  ForceDirectories(FFullDir);
  FFN := FFullDir + FFileName;
end;

procedure TRxCustomMailBox.SetFileName(AFileName: string);
begin
  if AFileName = '' then
    AFileName := 'noname';
  FFileName := ChangeFileExt(ExtractFileName(AFileName), '');
  FFN := FFullDir + FFileName;
end;

procedure TRxCustomMailBox.GetHeader;
begin
end;

procedure TRxCustomMailBox.SetHeader;
begin
end;

function TRxCustomMailBox.GetMsgCount: integer;
var
  f: file;
  fn: string;
begin
  fn := FFN + stridx;
  if FileExists(fn) then
  begin
    AssignFile(f, fn);
    Reset(f, irec_size);
    Result := FileSize(f);
    CloseFile(f);
  end
  else
    Result := 0;
end;

procedure TRxCustomMailBox.AddMessage;
var
  f: file;
  fn: string;
  irec: pointer;
begin
  SetHeader;
  // add message to mailbox file
  fn := FFN + StrMbx;
  AssignFile(f, fn);
  if FileExists(fn) then
    Reset(f, 1)
  else
    Rewrite(f, 1);
  FMsgStart := FileSize(f);
  FMsgSize := length(FMsg);
  Seek(f, FMsgStart);
  BlockWrite(f, FMsg[1], FMsgSize);
  CloseFile(f);
  // add header record to index file
  GetMem(irec, irec_size);
  try
    Tirec(irec^)[0] := FMsgStart;
    Tirec(irec^)[1] := FMsgSize;
    Move(FHeader^, Tirec(irec^)[2], FHeaderSize);
    fn := FFN + StrIDX;
    AssignFile(f, fn);
    if FileExists(fn) then
      Reset(f, irec_size)
    else
      Rewrite(f, irec_size);
    Seek(f, FileSize(f));
    BlockWrite(f, irec^, 1);
    CloseFile(f);
  finally
    FreeMem(irec);
  end;
end;

function TRxCustomMailBox.SetToMessage(Number: integer): boolean;
var
  f: file;
  fn: string;
  irec: pointer;
begin
  Result := false;
  FMsg := '';
  FMsgStart := 0;
  FMsgSize := 0;
  FMsgNum := -1;
  FillChar(FHeader^, FHeaderSize, 0);
  fn := FFN + StrIDX;
  if FileExists(fn) then
  begin
    GetMem(irec, irec_size);
    try
      AssignFile(f, fn);
      Reset(f, irec_size);
      Seek(f, Number);
      try
        BlockRead(f, irec^, 1);
        FMsgStart := Tirec(irec^)[0];
        FMsgSize := Tirec(irec^)[1];
        Move(Tirec(irec^)[2], FHeader^, FHeaderSize);
        FMsgNum := Number;
        GetHeader;
        Result := true;
      except
      end;
    finally
      CloseFile(f);
      FreeMem(irec);
    end;
  end;
end;

function TRxCustomMailBox.LoadMessage: boolean;
var
  f: file;
  fn: string;
begin
  Result := false;
  SetLength(FMsg, 0);
  if FMsgSize = 0 then
    exit;
  fn := FFN + StrMbx;
  if FileExists(fn) then
  begin
    SetLength(FMsg, FMsgSize);
    AssignFile(f, fn);
    Reset(f, 1);
    try
      Seek(f, FMsgStart);
      try
        BlockRead(f, FMsg[1], FMsgSize);
        Result := true;
      except
        SetLength(FMsg, 0);
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

function TRxCustomMailBox.DeleteMessage: boolean;
var
  f, f_tmp: file;
  fn: string;
  irec: pointer;
  i, cnt: Integer;
begin
  Result := false;
  cnt := MsgCount;
  if (cnt = 0) or (FMsgNum = -1) then
    Exit;

  fn := NormalDir(GetTempDir) + 'temp.idx';
  AssignFile(f_tmp, fn);
  Rewrite(f_tmp, irec_size);
  AssignFile(f, FFN + StrIdx);
  Reset(f, irec_size);
  GetMem(irec, irec_size);
  try
    for i := 0 to cnt - 1 do
    begin
      BlockRead(f, irec^, 1);
      if i <> FMsgNum then
        BlockWrite(f_tmp, irec^, 1);
    end;
  finally
    FreeMem(irec);
    CloseFile(f);
    CloseFile(f_tmp);
  end;

  DeleteFile(FFN + StrTmpIdx);
  if not RenameFile(FFN + StrIDX, FFN + StrTmpIdx) then
    exit;
  if not RenameFile(fn, FFN + StrIDX) then
  begin
    RenameFile(FFN + StrTmpIdx, FFN + StrIDX);
    exit;
  end;
  DeleteFile(fn);
  DeleteFile(FFN + StrTmpIdx);

  Result := true;
end;

function TRxCustomMailBox.CompressMailBox: boolean;
var
  f, fi, f_tmp, fi_tmp: file;
  fn, fni: string;
  irec, msg: Pointer;
  I, cnt: Integer;
  size: Cardinal;
begin
  Result := false;
  cnt := MsgCount;

  fn := FFN + StrMbx;
  AssignFile(f, fn);
  if FileExists(fn) then
    Reset(f, 1)
  else
    Rewrite(f, 1);
  if (cnt = 0) and (FileSize(f) > 0) then
  begin
    CloseFile(f);
    Result := EmptyMailBox;
    exit;
  end;
  fn := FFN + StrIDX;
  AssignFile(fi, fn);
  if FileExists(fn) then
    Reset(fi, irec_size)
  else
    Rewrite(fi, irec_size);

  fn := GetTempDir;
  fni := NormalDir(fn) + 'temp.idx';
  fn := NormalDir(fn) + 'temp.mbx';
  AssignFile(f_tmp, fn);
  Rewrite(f_tmp, 1);
  AssignFile(fi_tmp, fni);
  Rewrite(fi_tmp, irec_size);
  GetMem(irec, irec_size);
  try
    for I := 0 to cnt - 1 do
    begin
      BlockRead(fi, irec^, 1);
      size := Tirec(irec^)[1];
      Seek(f, Tirec(irec^)[0]);
      Tirec(irec^)[0] := FilePos(f_tmp);
      GetMem(msg, size);
      try
        BlockRead(f, msg^, size);
        BlockWrite(f_tmp, msg^, size);
      finally
        FreeMem(msg);
      end;
      BlockWrite(fi_tmp, irec^, 1);
    end;
  finally
    FreeMem(irec);
    CloseFile(f);
    CloseFile(fi);
    CloseFile(f_tmp);
    CloseFile(fi_tmp);
  end;

  DeleteFile(FFN + StrTmpMbx);
  DeleteFile(FFN + StrTmpIdx);
  if not RenameFile(FFN + StrMbx, FFN + StrTmpMbx) then
    exit;
  if not RenameFile(FFN + StrIDX, FFN + StrTmpIdx) then
    exit;
  if not RenameFile(fn, FFN + StrMbx) then
  begin
    RenameFile(FFN + StrTmpMbx, FFN + StrMbx);
    RenameFile(FFN + StrTmpIdx, FFN + StrIDX);
    exit;
  end;
  if not RenameFile(fni, FFN + StrIDX) then
  begin
    RenameFile(FFN + StrTmpMbx, FFN + StrMbx);
    RenameFile(FFN + StrTmpIdx, FFN + StrIDX);
    exit;
  end;
  DeleteFile(fn);
  DeleteFile(fni);
  DeleteFile(FFN + StrTmpMbx);
  DeleteFile(FFN + StrTmpIdx);

  Result := true;
end;

function TRxCustomMailBox.EmptyMailBox: boolean;
var
  f: file;
begin
  try
    AssignFile(f, FFN + StrIDX);
    Rewrite(f, 1);
    CloseFile(f);
    AssignFile(f, FFN + StrMbx);
    Rewrite(f, 1);
    CloseFile(f);
    Result := true;
  except
    Result := false;
  end;
end;

function TRxCustomMailBox.GetMailBoxSize: cardinal;
var
  f: file;
  fn: string;
begin
  fn := FFN + StrMbx;
  AssignFile(f, fn);
  if FileExists(fn) then
    Reset(f, 1)
  else
    Rewrite(f, 1);
  Result := FileSize(f);
  CloseFile(f);
end;

function TRxCustomMailBox.GetMailBoxTrash: cardinal;
var
  f: file;
  irec: Pointer;
  I, Cnt: Integer;
  size: Cardinal;
  fn: string;
begin
  Cnt := MsgCount;
  size := 0;

  fn := FFN + StrIDX;
  AssignFile(f, fn);
  if FileExists(fn) then
    Reset(f, irec_size)
  else
    Rewrite(f, irec_size);
  GetMem(irec, irec_size);
  try
    for I := 0 to Cnt - 1 do
    begin
      BlockRead(f, irec^, 1);
      inc(size, Tirec(irec^)[1]);
    end;
  finally
    FreeMem(irec);
    CloseFile(f);
  end;
  Result := MailBoxSize - size;
end;

function TRxCustomMailBox.GetMsgSize: cardinal;
begin
  if FMsgNum = -1 then
    Result := 0
  else
    Result := FMsgSize;
end;

procedure TRxCustomMailBox.CreateMailBoxFiles;

  procedure _CreateFile(fn: string);
  var
    f: file;
  begin
    AssignFile(f, fn);
    if not FileExists(fn) then
    begin
      Rewrite(f, 1);
      CloseFile(f);
    end;
  end;

begin
  _CreateFile(FFN + StrMbx);
  _CreateFile(FFN + StrIDX);
end;

procedure TRxCustomMailBox.SetName(AName: string);
begin
  if AName = '' then
    AName := 'Noname';
  FName := AName;
end;

procedure TRxCustomMailBox.UpdateHeader;
var
  f: file;
  fn: string;
  irec: pointer;
begin
  if FMsgNum < 0 then
    exit;
  fn := FFN + StrIDX;
  if FileExists(fn) then
    Reset(f, irec_size)
  else
    Rewrite(f, irec_size);
  GetMem(irec, irec_size);
  try
    if FMsgNum >= FileSize(f) then
      exit;
    SetHeader;
    Seek(f, FMsgNum);
    BlockRead(f, irec^, 1);
    Move(FHeader^, Tirec(irec^)[2], FHeaderSize);
    Seek(f, FMsgNum);
    BlockWrite(f, irec^, 1);
  finally
    FreeMem(irec);
    CloseFile(f);
  end;
end;

{ TRxMailBox }

constructor TRxMailBox.Create;
begin
  inherited Create(SizeOf(THeaderRec));
end;

procedure TRxMailBox.GetHeader;
begin
  with THeaderRec(FHeader^) do
  begin
    Header.FromName := string(FromName);
    Header.ToName := string(ToName);
    Header.Subject := string(Subject);
    Header.DateSend := StrToDate(string(DateSend));
    Header.DateRec := StrToDate(string(DateRec));
    Header.ReadFlag := ReadFlag;
  end;
end;

procedure TRxMailBox.SetHeader;
begin
  with THeaderRec(FHeader^) do
  begin
    FromName := AnsiString(Header.FromName);
    ToName := AnsiString(Header.ToName);
    Subject := AnsiString(Header.Subject);
    DateSend := AnsiString(DateToStr(Header.DateSend));
    DateRec := AnsiString(DateToStr(Header.DateRec));
    ReadFlag := Header.ReadFlag;
  end;
end;

{ TRxMailBoxList }

destructor TRxMailBoxList.Destroy;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited Destroy;
end;

function TRxMailBoxList.Get(const Index: integer): TRxMailBox;
begin
  Result := inherited Items[Index];
end;

function TRxMailBoxList.IndexOfFileName(FileName: string): integer;
var
  i: Integer;
  s: string;
begin
  s := AnsiLowerCase(FileName);
  Result := -1;
  for i := 0 to Count - 1 do
    if AnsiLowerCase(Items[i].FileName) = s then
    begin
      Result := i;
      break;
    end;
end;

{ TRxMailBoxManager }

function TRxMailBoxManager.AddMailBox(FileName, Name: string): TRxMailBox;
begin
  Result := TRxMailBox.Create;
  Result.Directory := FDir;
  Result.FileName := FileName;
  Result.Name := Name;
  FMailBoxes.Add(Result);
end;

constructor TRxMailBoxManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMailBoxes := TRxMailBoxList.Create;
  FMBDesign := TStringList.Create;
  FMBIndex := -1;
end;

procedure TRxMailBoxManager.DeleteMailBox(Index: integer);
begin
  FMailBoxes[Index].Free;
  FMailBoxes.Delete(Index);
end;

destructor TRxMailBoxManager.Destroy;
begin
  FMailBoxes.Free;
  FMBDesign.Free;
  inherited Destroy;
end;

function TRxMailBoxManager.GetCurrentMB: TRxMailBox;
begin
  if FMBIndex = -1 then
    Result := nil
  else
    Result := FMailBoxes[FMBIndex];
end;

procedure TRxMailBoxManager.Loaded;
var
  i, j: Integer;
  s: string;
  MB: TRxMailBox;
begin
  inherited Loaded;
  if not(csDesigning in ComponentState) then
    for i := 0 to FMBDesign.Count - 1 do
    begin
      s := FMBDesign[i];
      if length(s) > 0 then
      begin
        j := Pos('=', s);
        if j = 0 then
          j := length(s) + 1;
        MB := AddMailBox(Trim(Copy(s, 1, j - 1)),
          Trim(Copy(s, j + 1, length(s) - j)));
        MB.CreateMailBoxFiles;
      end;
    end;
end;

procedure TRxMailBoxManager.SetCurrentMB(Value: TRxMailBox);
begin
  MailBoxIndex := FMailBoxes.IndexOf(Value);
end;

procedure TRxMailBoxManager.SetDir(Dir: string);
var
  i: integer;
begin
  FDir := Dir;
  for i := 0 to FMailBoxes.Count - 1 do
    FMailBoxes[i].Directory := FDir;
end;

procedure TRxMailBoxManager.SetMBDesign(Value: TStrings);
begin
  FMBDesign.Assign(Value);
end;

procedure TRxMailBoxManager.SetMBIndex(Value: integer);
begin
  if Value <> FMBIndex then
  begin
    if Value < 0 then
      Value := -1;
    if Value >= FMailBoxes.Count then
      Value := -1;
    FMBIndex := Value;
    if Assigned(FChangeMBEvent) then
      FChangeMBEvent(Self, FMBIndex);
  end;
end;

end.
