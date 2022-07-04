{------------------------------------------------------------------------------}
{                                                                              }
{  WaveRecorders - Wave recorder components                                    }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveRecorders;

interface

uses
  Windows, Messages, Classes, MMSystem, scWaveUtils, scWaveStorage, scWaveIn;

type

  // Records wave into a wave stream
  TAudioRecorder = class(TWaveAudioIn)
  private
    fWave: TWaveStreamAdapter;
  protected
    function CreateWave: TWaveStreamAdapter; virtual;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    procedure WaveDataReady(const Buffer: Pointer; BufferSize: DWORD;
      var FreeIt: Boolean); override;
    procedure DoWaveInDeviceClose; override;
    procedure DoError; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
    property Wave: TWaveStreamAdapter read fWave;
  published
    property PCMFormat;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
    property OnFormat;
  end;

  // Records wave into user defined buffers
  TLiveAudioRecorder = class(TWaveAudioIn)
  private
    fOnData: TWaveAudioDataReadyEvent;
  protected
    procedure WaveDataReady(const Buffer: Pointer; BufferSize: DWORD;
      var FreeIt: Boolean); override;
  public
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
  published
    property PCMFormat;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
    property OnFormat;
    property OnData: TWaveAudioDataReadyEvent read fOnData write fOnData;
  end;

  // Records wave into a file, or stream
  TStockAudioRecorder = class(TWaveAudioIn)
  private
    Wave: TWaveStreamAdapter;
    fStock: TCustomWaveStorage;
    procedure SetStock(Value: TCustomWaveStorage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean); override;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    procedure WaveDataReady(const Buffer: Pointer; BufferSize: DWORD;
      var FreeIt: Boolean); override;
    procedure DoWaveInDeviceClose; override;
    procedure DoError; override;
    function InternalRecord(AStream: TStream; Ownership: TStreamOwnership): Boolean; virtual;
  public
    function RecordToStream(AStream: TStream): Boolean;
    function RecordToFile(const FileName: String): Boolean;
    function RecordToStock(Index: Integer {$IFDEF COMPILER4_UP} = 0 {$ENDIF}): Boolean;
    function Stop: Boolean;
    property NumDevs;
    property DeviceName;
    property DeviceFormats;
    property LastError;
    property LastErrorText;
    property Position;          // Milliseconds
    property DeviceID;
    property Paused;
    property Active;
  published
    property Stock: TCustomWaveStorage read fStock write SetStock;
    property PCMFormat;
    property BufferLength;      // Milliseconds
    property BufferCount;
    property Async;
    property OnActivate;
    property OnDeactivate;
    property OnPause;
    property OnResume;
    property OnError;
    property OnLevel;
    property OnFormat;
  end;


implementation

uses
  SysUtils;

{ TAudioRecorder }

constructor TAudioRecorder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWave := CreateWave;
end;

destructor TAudioRecorder.Destroy;
begin
  inherited Destroy;
  fWave.Free;
end;

function TAudioRecorder.CreateWave: TWaveStreamAdapter;
begin
  Result := TWave.Create;
end;

procedure TAudioRecorder.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  inherited GetWaveFormat(pWaveFormat, FreeIt);
  if Assigned(pWaveFormat) then
    fWave.BeginRewrite(pWaveFormat);
end;

procedure TAudioRecorder.WaveDataReady(const Buffer: Pointer;
  BufferSize: DWORD; var FreeIt: Boolean);
begin
  if fWave.Write(Buffer^, BufferSize) <> Integer(BufferSize) then
    Success(MMSYSERR_ERROR); // Raises an OnError event
end;

procedure TAudioRecorder.DoWaveInDeviceClose;
begin
  try
    fWave.EndRewrite;
  finally
    inherited DoWaveInDeviceClose;
  end;
end;

procedure TAudioRecorder.DoError;
begin
  try
    if not Active then
      fWave.EndRewrite;
  finally
    inherited DoError;
  end;
end;

{ TLiveAudioRecorder }

procedure TLiveAudioRecorder.WaveDataReady(const Buffer: Pointer;
  BufferSize: DWORD; var FreeIt: Boolean);
begin
  if Assigned(fOnData) then
    fOnData(Self, Buffer, BufferSize, FreeIt);
end;

{ TStockAudioRecorder }

procedure TStockAudioRecorder.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Stock) then
    Stock := nil;
end;

procedure TStockAudioRecorder.SetStock(Value: TCustomWaveStorage);
begin
  if Stock <> Value then
  begin
    {$IFDEF COMPILER5_UP}
    if Assigned(Stock) then
      Stock.RemoveFreeNotification(Self);
    {$ENDIF}
    fStock := Value;
    if Assigned(Stock) then
      Stock.FreeNotification(Self);
  end;
end;

procedure TStockAudioRecorder.SetActive(Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      RecordToStock(0)
    else
      Stop;
  end;
end;

procedure TStockAudioRecorder.WaveDataReady(const Buffer: Pointer;
  BufferSize: DWORD; var FreeIt: Boolean);
begin
  if Wave.Write(Buffer^, BufferSize) <> Integer(BufferSize) then
    Success(MMSYSERR_ERROR); // Raises an OnError event
end;

procedure TStockAudioRecorder.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  inherited GetWaveFormat(pWaveFormat, FreeIt);
  if Assigned(pWaveFormat) then
    Wave.BeginRewrite(pWaveFormat)
end;

procedure TStockAudioRecorder.DoWaveInDeviceClose;
begin
  try
    Wave.EndRewrite;
    Wave.Free;
    Wave := nil;
  finally
    inherited DoWaveInDeviceClose;
  end;
end;

procedure TStockAudioRecorder.DoError;
begin
  try
    if not Active and Assigned(Wave) then
    begin
      Wave.EndRewrite;
      Wave.Free;
      Wave := nil;
    end;
  finally
    inherited DoError;
  end;
end;

function TStockAudioRecorder.InternalRecord(AStream: TStream;
  Ownership: TStreamOwnership): Boolean;
begin
  Result := False;
  if Active then
  begin
    inherited Active := False;
    Sleep(0);
  end;
  Wave := TWaveStreamAdapter.Create(AStream, Ownership);
  try
    Result := InternalOpen;
  finally
    if (Result = False) and Assigned(Wave) then
    begin
      Wave.Free;
      Wave := nil;
    end;
  end;
end;

function TStockAudioRecorder.RecordToStream(AStream: TStream): Boolean;
begin
  Result := InternalRecord(AStream, soReference);
end;

function TStockAudioRecorder.RecordToFile(const FileName: String): Boolean;
begin
  Result := InternalRecord(TFileStream.Create(FileName, fmCreate or fmShareDenyWrite), soOwned);
end;

function TStockAudioRecorder.RecordToStock(Index: Integer): Boolean;
begin
  if not Assigned(Stock) then
    raise EWaveAudioInvalidOperation.Create('Stock property is not assigned');
  Result := InternalRecord(Stock.WaveStream[Index], soReference);
end;

function TStockAudioRecorder.Stop: Boolean;
begin
  Result := InternalClose;
end;

end.
