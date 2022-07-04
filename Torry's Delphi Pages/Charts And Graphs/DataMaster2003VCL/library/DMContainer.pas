///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMContainer;

{$I+,B-}

interface

uses
  Windows, Classes, Dialogs, Forms, SysUtils;

{$I DMData.inc}

type
  TData = class (TObject)
  protected
    function GetData: string; virtual; abstract;
    procedure SetData(const S: string); virtual; abstract;
    procedure FreeData; virtual;
    procedure LoadData(var F: text); virtual;
    procedure SaveData(var F: text); virtual;
    procedure ReadData(S: TStream); virtual; abstract;
    procedure WriteData(S: TStream); virtual; abstract;
  public
    property Data: string read GetData write SetData;
    destructor Destroy; override;
    procedure Copy(D: TData); virtual;
  end;
  
  ERealDataError = class (Exception)
  end;
  
  TRealData = class (TData)
  private
    FNumCol: Integer;
    FData: PRealArray;
  protected
    function GetData: string; override;
    procedure SetData(const S: string); override;
    procedure FreeData; override;
    procedure ReadData(S: TStream); override;
    procedure WriteData(S: TStream); override;
  public
    Format: PFormatArray;
    class function GetClipboardFormat: Word;
    function GetItemText(N: integer): string;
    function GetRData(var R: TRealArray): Integer;
    procedure SetRData(N: integer; R: TRealArray);
    function GetItem(N:integer): TReal;
    procedure SetItem(N:integer; R: TReal);
    procedure DelItem(N: integer);
    procedure InsItem(R: TReal);
    procedure Copy(D: TData); override;
    property Size: Integer read FNumCol;
    property RData[N:integer]: TReal read GetItem write SetItem; default;
  end;
  
  TDataType=(dtCustom, dtRealData);
  TCompareResult=(crGreater, crLess, crEqual);
  TCompareEvent = function (Sender: TObject; I1,I2: TData): TCompareResult of object;
  TInitItemEvent = function (Sender: TObject): TData of object;
  TProgressEvent = procedure (Sender: TObject; P: integer) of object;
  TDataClass=class of TData;

  TContainer = class (TComponent)
  private
    FFileName: string;
    FDataClass: TDataClass;
    FUpdateCaption: Boolean;
    FAutoLoad: Boolean;
    FList: TList;
    FInitItem: TInitItemEvent;
    FProgress: TProgressEvent;
    FModified: Boolean;
    FChanged: TNotifyEvent;
    FCompare: TCompareEvent;
    FDataCache: TReal;
    FRowCache: TRealArray;
    procedure SetDataType(T: TDataType);
    function GetDataType: TDataType;
    procedure SetFileName(const FN: string);
    procedure SetModified(M: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    property Items: TList read FList;
    property Modified: Boolean read FModified write SetModified;
    property DataClass: TDataClass read FDataClass write FDataClass;
    procedure ShowProgress(P: integer); virtual;
    function InitItem: TData; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile;
    procedure SaveToFile(Backup: boolean);
    procedure LoadFromStream(S: TStream);
    procedure SaveToStream(S: TStream);
    procedure Sort(BegLine,EndLine: integer; Descend: boolean);
    procedure Assign(Source: TPersistent); override;
    function Calibrate(R:TReal; Index,Key: integer): TReal;
  published
    property FileName: string read FFileName write SetFileName;
    property DataType: TDataType read GetDataType write SetDataType;
    property UpdateCaption: Boolean read FUpdateCaption write FUpdateCaption;
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad;
    property OnInitItem: TInitItemEvent read FInitItem write FInitItem;
    property OnProgress: TProgressEvent read FProgress write FProgress;
    property OnChanged: TNotifyEvent read FChanged write FChanged;
    property OnCompare: TCompareEvent read FCompare write FCompare;
  end;
  
procedure Register;
function Str2Real(S: string; var R: TRealArray): byte;

{const
  UseTabs: boolean=true;}

resourcestring
  strItemIndex='Item index %d is not valid!';
  strInsertItem='Cannot insert number: array full!';
  strInitializeItem='Cannot initialize item!';
  strSetCustomType='You need to define %s.OnInitItem or DataClass!';
  strSortError='Unable to sort lines!';

implementation

uses Controls{crXX};

procedure Register;
begin
  RegisterComponents('DM2003', [TContainer]);
end;

{Converts string to TRealArray. Returns size of the array. Numbers may be
delimited by spaces, tabstops or commas}
function Str2Real(S: string; var R: TRealArray): byte;
var
  Ss: string;
  I,J: integer;
  Flag: integer;
  Rr: TReal;
  Separators: set of char;
begin
  I:=1;
  J:=0;
  if DecimalSeparator=',' 
  then Separators:=[' ', #9] 
  else Separators:=[' ', #9, ','];
  while(I<=length(S)) and (J<MaxCols) do
  begin
    if S[I] in Separators then Inc(I) else
    begin
      SS:='';
      while (I<=length(S)) and (not (S[I] in Separators)) do
      begin
        Ss:=Ss+S[I]; Inc(I);
      end;
      Flag:=0;
      try
        Rr:=StrToFloat(Ss); // uses decimal separator
      except
        Val(Ss, Rr, Flag);
      end;
      // Val(Ss, Rr, Flag);
      Str2Real:=J;
      if Flag<>0 then Exit;
      Inc(J);
      R[J]:=Rr;
    end;
  end;
  Str2Real:=J;
end;

{---------- Class: TData ----------}
procedure TData.Copy(D: TData);
begin
  Data:=D.Data;
end;

destructor TData.Destroy;
begin
  FreeData;
  inherited;
end;

procedure TData.FreeData;
begin
end;

procedure TData.LoadData(var F: text);
var
  S: string;
begin
  readln(F, S);
  Data:=S;
end;

//: WARNING: force '.' decimal separator for numbers in file! 
procedure TData.SaveData(var F: text);
var
  C: Char;
begin
  C:=DecimalSeparator;
  if C='.'
  then writeln(F, Data) else
  try
    DecimalSeparator:='.';
    writeln(F, Data);
  finally
    DecimalSeparator:=C;
  end;
end;

{---------- Class: TRealData ----------}
procedure TRealData.Copy(D: TData);
var
  R: TRealArray;
  N: Byte;
begin
  if D is TRealData then // avoid precision losses
  begin
    N:=(D as TRealData).GetRData(R);
    {$ifdef CopyDataFormat}
    Format:=(D as TRealData).Format;
    {$endif}
    SetRData(N,R);
  end else inherited;
end;

procedure TRealData.DelItem(N: integer);
var
  I, J, K: Integer;
  R: TRealArray;
begin
  if (N<1) or (N>FNumCol) then
  begin
    raise ERealDataError.CreateFmt(strItemIndex, [N]);
    Exit;
  end;
  K:=FNumCol;
  for I:=1 to K do
  R[I]:=FData^[I];
  FreeData;
  GetMem(FData, (K-1)*SizeOf(TReal));
  FNumCol:=K-1;
  J:=1;
  for I:=1 to K do
  if I<>N then
  begin
    FData^[J]:=R[I];
    Inc(J);
  end;
end;

procedure TRealData.FreeData;
begin
  if Assigned(FData)
  then FreeMem(FData, FNumCol*SizeOf(TReal));
  FNumCol:=0;
  FData:=nil;
end;

class function TRealData.GetClipboardFormat: Word;
begin
  Result:=RegisterClipboardFormat('TRealData Array');
end;

function TRealData.GetData: string;
var
  I: Integer;
  Tab: Char;
begin
  Result:='';
  {if UseTabs
  then Tab:=#9
  else} Tab:=' ';
  for I:=1 to FNumCol do
  Result:=Result+GetItemText(I)+Tab;
end;

function TRealData.GetItem(N:integer): TReal;
begin
  if (N<1) or (N>FNumCol)
  then raise ERealDataError.CreateFmt(strItemIndex, [N])
  else GetItem:=FData^[N];
end;

function TRealData.GetItemText(N: integer): string;
var
  W, D: integer;
  F: TFloatFormat;
begin
  if N>FNumCol then
  begin
    Result:='';
    Exit;
  end;
  if Assigned(Format) then with Format^[N] do
  begin
    W:=Width;
    D:=Decimals;
    F:=FType;
  end else
  begin
    W:=15;
    D:=7;
    F:=ffGeneral;
  end;
  Result:=FloatToStrF(GetItem(N), F, W, D);
end;

function TRealData.GetRData(var R: TRealArray): Integer;
var
  I: Integer;
begin
  for I:=1 to FNumCol do
  R[I]:=FData^[I];
  GetRData:=FNumCol;
end;

procedure TRealData.InsItem(R: TReal);
var
  I, N: Integer;
  Rr: TRealArray;
begin
  if FNumCol=MaxCols
  then raise ERealDataError.Create(strInsertItem) else
  begin
    N:=FNumCol;
    for I:=1 to N do
    Rr[I]:=FData^[I];
    FreeData;
    GetMem(FData, (N+1)*SizeOf(TReal));
    FNumCol:=N+1;
    for I:=1 to N do
    FData^[I]:=Rr[I];
    FData^[N+1]:=R;
  end;
end;

procedure TRealData.ReadData(S: TStream);
var
  I, N: Byte;
  R: TRealArray;
  Ex: Extended;
begin
  N:=FNumCol;
  S.ReadBuffer(N, SizeOf(N));
  for I:=1 to N do
  {$ifdef doublefloat}
  begin
    S.ReadBuffer(Ex, SizeOf(Ex));
    R[I]:=Ex; // always Extended!!! (for DMWs)
  end;
  {$else}
  S.ReadBuffer(R[I], SizeOf(TReal));
  {$endif}
  SetRData(N,R);
end;

procedure TRealData.SetData(const S: string);
var
  R: TRealArray;
  N: Integer;
begin
  N:=Str2Real(S, R);
  SetRData(N,R);
end;

procedure TRealData.SetItem(N:integer; R: TReal);
begin
  if (N<1) or (N>FNumCol)
  then raise ERealDataError.CreateFmt(strItemIndex, [N])
  else FData^[N]:=R;
end;

procedure TRealData.SetRData(N: integer; R: TRealArray);
var
  I: Integer;
begin
  FreeData;
  if N<1 then Exit;
  FNumCol:=N;
  GetMem(FData, N*SizeOf(TReal));
  for I:=1 to N do
  FData^[I]:=R[I];
end;

procedure TRealData.WriteData(S: TStream);
var
  I: Byte;
  N: Byte;
  Ex: Extended;
begin
  N:=FNumCol;
  S.WriteBuffer(N, SizeOf(N));
  for I:=1 to N do
  {$ifdef doublefloat}
  begin
    Ex:=FData^[I];
    S.WriteBuffer(Ex, SizeOf(Ex)); // always Extended!!! (for DMWs)
  end;
  {$else}
  S.WriteBuffer(FData^[I], SizeOf(TReal));
  {$endif}
end;

{---------- Class: TContainer ----------}
procedure TContainer.Assign(Source: TPersistent);
var
  I, N: Integer;
  D: TData;
begin
  if not Assigned(FList)
  then Exit;
  if (Source is TContainer) or (Source is TStrings) then
  begin
    Screen.Cursor:=crHourGlass;
    try  // not Clear() - else OnChanged called twice (what is not good)
      for I:=0 to Items.Count-1 do
      begin
        D:=Items[I];
        D.Free;
      end;
      Items.Clear;
      if Source is TContainer
      then N:=(Source as TContainer).Items.Count-1
      else N:=(Source as TStrings).Count-1;
      for I:=0 to N do
      begin
        D:=InitItem;
        if Assigned(D) then
          if Source is TContainer then
          begin
            D.Copy((Source as TContainer).Items[I]);
            Items.Add(D);
          end else
          begin
            D.Data:=(Source as TStrings)[I];
            Items.Add(D);
          end;
        if N<>0
        then ShowProgress(Round(I/N*100.0));
      end;
    finally
      Screen.Cursor:=crDefault;
      Modified:=true;
    end;
  end else inherited;
end;

function TContainer.Calibrate(R:TReal; Index,Key: integer): TReal;
  
    function LineInterpolate(X1, X2, Y1, Y2, X: TReal): TReal;
    begin // linear interpolation: Y:=Yk+(X-Xk)/(Xk+1-Xk)*(Yk+1-Yk)
      if X1=X2 // NOTE! process zero interval
      then LineInterpolate:=Y1+Y2/2
      else LineInterpolate:=Y1+(X-X1)/(X2-X1)*(Y2-Y1);
    end;
  var
    D1,D2: TRealData;
    I,J,K: integer;
  
begin
  if R=FDataCache then
  begin
    Result:=FRowCache[Index];
    Exit;
  end;
  Result:=R;
  if Items.Count<6
  then Exit;
  D1:=Items.First;
  D2:=Items.Last;
  if R<D1.RData[Key] then
  begin
    Result:=D1.RData[Index];
    Exit;
  end;
  if R>D2.RData[Key] then
  begin
    Result:=D2.RData[Index];
    Exit;
  end;
  I:=0;
  J:=Items.Count-1;
  repeat
    D1:=Items[I+((J-I) div 2)];
    if R<D1.RData[Key]
    then J:=I+(J-I) div 2
    else I:=I+(J-I) div 2;
  until J-I<2;
  D1:=Items[I];
  D2:=Items[J];
  if D1.Size=D2.Size then
  for K:=1 to D1.Size do
  FRowCache[K]:=LineInterpolate(D1.RData[Key], D2.RData[Key],
    D1.RData[K], D2.RData[K], R);
  Result:=FRowCache[Index];
  FDataCache:=R;
end;

procedure TContainer.Clear;
var
  I: Integer;
  P: TData;
begin
  if not Assigned(FList) or (FList.Count=0)
  then Exit;
  for I:=0 to FList.Count-1 do
  begin
    P:=FList[I];
    P.Free;
  end;
  FList.Clear;
  Modified:=true;
end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateCaption:=false;
  FAutoLoad:=false;
  FDataClass:=TRealData;
  FList:=TList.Create;
  FModified:=false;
end;

procedure TContainer.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', LoadFromStream, SaveToStream, True);
end;

destructor TContainer.Destroy;
begin
  Clear;
  if Assigned(FList)
  then Flist.Free;
  inherited;
end;

function TContainer.GetDataType: TDataType;
begin
  Result:=dtCustom;
  if FDataClass=TRealData
  then Result:=dtRealData;
end;

function TContainer.InitItem: TData;
begin
  if Assigned(FInitItem)
  then Result:=FInitItem(Self) else
    if Assigned(FDataClass)
    then Result:=FDataClass.Create else
    begin
      MessageDlg(strInitializeItem, mtError, [mbCancel], 0);
      Result:=nil;
    end;
end;

procedure TContainer.LoadFromFile;
var
  F: system.text;
  P: TData;
begin
  Clear;
  system.Assign(F, FileName);
  system.Reset(F);
  Screen.Cursor:=crHourGlass;
  try
    while not eof(F) do
    begin
      P:=InitItem;
      if not assigned(P)
      then Break;
      P.LoadData(F);
      FList.Add(P);
      ShowProgress(FList.Count);
    end;
  finally
    Screen.Cursor:=crDefault;
    system.Close(F);
    Modified:=false;
  end;
end;

procedure TContainer.LoadFromStream(S: TStream);
var
  I, N: LongInt;
  P: TData;
  T: TDataType;
begin
  Screen.Cursor:=crHourGlass;
  try
    S.ReadBuffer(T, SizeOf(T));
    DataType:=T;
    S.ReadBuffer(N, SizeOf(N));
    Clear;
    for I:=1 to N do
    begin
      P:=InitItem;
      if not Assigned(P)
      then Break;
      P.ReadData(S);
      FList.Add(P);
      if N<>0 then ShowProgress(Round(I/N*100.0));
    end;
  finally
    Screen.Cursor:=crDefault;
    Modified:=false;
  end;
end;

procedure TContainer.SaveToFile(Backup: boolean);
var
  F: system.text;
  I: Integer;
  P: TData;
  BakName: string;
begin
  if Backup then
  begin
    BakName:=ChangeFileExt(FileName, '.BAK');
    if FileExists(BakName)
    then DeleteFile(BakName);
    RenameFile(FileName, BakName);
  end;
  system.Assign(F, FileName);
  system.ReWrite(F);
  Screen.Cursor:=crHourGlass;
  try
    for I:=0 to FList.Count-1 do
    begin
      P:=FList[I];
      P.SaveData(F);
      if FList.Count>1 // check for division by zero at next line
      then ShowProgress(Round(I/(FList.Count-1)*100.0));
    end;
  finally
    Screen.Cursor:=crDefault;
    system.Close(F);
    Modified:=false;
  end;
end;

procedure TContainer.SaveToStream(S: TStream);
var
  I: LongInt;
  P: TData;
  D: TDataType;
begin
  Screen.Cursor:=crHourGlass;
  try
    D:=DataType;
    S.WriteBuffer(D, SizeOf(TDataType));
    I:=FList.Count;
    S.WriteBuffer(I, SizeOf(I));
    for I:=0 to FList.Count-1 do
    begin
      P:=FList[I];
      P.WriteData(S);
      if FList.Count>1
      then ShowProgress(Round(I/(FList.Count-1)*100.0));
    end;
  finally
    Screen.Cursor:=crDefault;
    Modified:=false;
  end;
end;

procedure TContainer.SetDataType(T: TDataType);
begin
  if T=dtCustom then
  begin
    if csDesigning in ComponentState then
    MessageDlg(Format(strSetCustomType, [Name]), mtWarning, [mbOk], 0);
    FDataClass:=nil;
  end;
  if T=dtRealData
  then FDataClass:=TRealData;
end;

procedure TContainer.SetFileName(const FN: string);
begin
  if FUpdateCaption and (Owner is TForm)
  then (Owner as TForm).Caption:=ExtractFileName(FN);
  if FAutoLoad and (FFileName<>FN) then
  begin
    FFileName:=FN;
    LoadFromFile;
  end
  else FFileName:=FN;
end;

procedure TContainer.SetModified(M: Boolean);
begin
  FModified:=M;
  if Assigned(FChanged) and not (csDestroying in ComponentState)
  then FChanged(Self);
end;

procedure TContainer.ShowProgress(P: integer);
begin
  if Assigned(FProgress)
  then FProgress(Self, P);
end;

procedure TContainer.Sort(BegLine,EndLine: integer; Descend: boolean);
  
  procedure DoSort(BegLine,EndLine: integer; Descend: boolean);
  var
    I,J: Integer;
    D: TData;
  begin
    I:=BegLine;
    J:=EndLine;
    D:=FList[(BegLine+EndLine) shr 1];
    repeat
      if Descend then
      begin
        while FCompare(Self, FList[I], D)=crGreater
        do Inc(I);
        while FCompare(Self, FList[J], D)=crLess
        do Dec(J);
      end else
      begin
        while FCompare(Self, FList[I], D)=crLess
        do Inc(I);
        while FCompare(Self, FList[J], D)=crGreater
        do Dec(J);
      end;
      if I<=J then
      begin
        Flist.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I>J;
    if BegLine<J
    then DoSort(BegLine, J, Descend);
    if I<EndLine
    then DoSort(I, EndLine, Descend);
  end;
  
begin
  if (BegLine>=EndLine) or (not Assigned(FCompare)) then
  begin
    MessageDlg(strSortError, mtError, [mbCancel], 0);
    Exit;
  end;
  Screen.Cursor:=crHourGlass;
  try
    DoSort(BegLine, EndLine, Descend);
  finally
    Screen.Cursor:=crDefault;
    Modified:=true;
  end;
end;

end.

