{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
 Testers   : Dominique Louis
             Ivan Blecic
}
unit SXMovie;

{$INCLUDE DelphiXcfg.inc}

interface

uses
  Windows, Classes, SysUtils, ActiveX, Math, Dialogs,
  DXSounds, DXDraws,
  {$IFDEF StandardDX}
  DirectDraw, DirectSound, DirectShow9,
    {$IFDEF DX7}
      {$IFDEF D3DRM}
  Direct3DRM,
      {$ENDIF}
  Direct3D;
    {$ENDIF}
    {$IFDEF DX9}
  Direct3D9, Direct3D, D3DX9, {Direct3D8,} DX7toDX8;
    {$ENDIF}
  {$ELSE}
  DShow, DirectX;
  {$ENDIF}

type
  TMovieOptions = (VideoAndSound, VideoOnly);
  TDisplay = (FullScreen, WideScreen, OriginalSize);

  TScreenRect = class(TPersistent)
  private
    FLeft: integer;
    FRight: integer;
    FTop: integer;
    FBottom: integer;
    procedure SetLeft(Value: integer);
    procedure SetRight(Value: integer);
    procedure SetTop(Value: integer);
    procedure SetBottom(Value: integer);
  protected
  public
    procedure Assign(Value: TScreenRect);
  published
    property Left: integer read FLeft write SetLeft;
    property Right: integer read FRight write SetRight;
    property Top: integer read FTop write SetTop;
    property Bottom: integer read FBottom write SetBottom;
  end;


  TSXMovie = class;

  TMovieThread = class(TThread)
  private
    { Private declarations }
    FSXMovie: TSXMovie;
  protected
    procedure Execute; override;
  public
    property SXMovie: TSXMovie read FSXMovie write FSXMovie;
  end;

  TSXMovie = class(TComponent)
  private
    FMovieThread: TMovieThread;
    FDXDraw: TDXDraw;
    FDXSound: TDXSound;
    FMMStream: IMultiMediaStream;
    FPrimaryVidStream: IMediaStream;
    FDDStream: IDirectDrawMediaStream;
    FSample: IDirectDrawStreamSample;
    FSurface: IDirectDrawSurface;
    FDXSurface: TDirectDrawSurface;
    FRect: TRect;
    FDestRect: TRect;
    FScreenRect: TScreenRect;
    FFilename: TFilename;
    FPlaying: Boolean;
    FDoFlip: Boolean;
    {Event}
    FOnMovieEnd: TNotifyEvent;
    FOnBeforeRender: TNotifyEvent;
    FOnAfterRender: TNotifyEvent;
    procedure SetFilename(Value: TFilename);
    procedure SetScreenRect(Value: TScreenRect);
    procedure DoMovieEnd;
    procedure DoBeforeRender;
    procedure DoAfterRender;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMovieStateRun;
    procedure SetMovieStateStop;
    procedure CreateMediaStream; virtual;
    procedure SetupMediaSample; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpDate; virtual;
//    function SetDisplay(Value:TDisplay):Boolean;
    procedure DisplayRect(Top, Left, Right, Bottom: integer);
    procedure Play;
    procedure Stop;
    {}
  published
    property DXDraw: TDXDraw read FDXDraw write FDXDraw;
    property DXSound: TDXSound read FDXSound write FDXSound;
    property Filename: TFilename read FFilename write SetFilename;
    property Playing: Boolean read FPlaying;
    property DoFlip: Boolean read FDoFlip write FDoFlip;
    property DestinationRectangle: TScreenRect read FScreenRect write SetScreenRect;
    property OnMovieEnd: TNotifyEvent read FOnMovieEnd write FOnMovieEnd;
    property OnBeforeRender: TNotifyEvent read FOnBeforeRender write FOnBeforeRender;
    property OnAfterRender: TNotifyEvent read FOnAfterRender write FOnAfterRender;
  end;

implementation

uses Graphics;

procedure TScreenRect.SetLeft(Value: integer);
begin
  FLeft := Value;
end;

procedure TScreenRect.SetRight(Value: integer);
begin
  FRight := Value;
end;

procedure TScreenRect.SetTop(Value: integer);
begin
  FTop := Value;
end;

procedure TScreenRect.SetBottom(Value: integer);
begin
  FBottom := Value;
end;

procedure TScreenRect.Assign(Value: TScreenRect);
begin
  Left := Value.Left;
  Right := Value.Right;
  Top := Value.Top;
  Bottom := Value.Bottom;
end;

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TMovieThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TMovieThread }

procedure TMovieThread.Execute;
begin
  { Place thread code here }
  if Assigned(SXMovie) then
    SXMovie.SetMovieStateRun;
  while (not Terminated) and Assigned(SXMovie) do
  begin
    Synchronize(SXMovie.UpDate)
  end;
  if Assigned(SXMovie) then
    SXMovie.SetMovieStateStop;
end;

{SXMovie}

procedure TSXMovie.SetFilename(Value: TFilename);
begin
  if Value <> '' then
    FFilename := Value;
end;

procedure TSXMovie.SetScreenRect(Value: TScreenRect);
begin
  FScreenRect.Assign(Value);
end;

procedure TSXMovie.DoMovieEnd;
begin
  if Assigned(FOnMovieEnd) then FOnMovieEnd(Self);
end;

procedure TSXMovie.DoBeforeRender;
begin
  if Assigned(FOnBeforeRender) then FOnBeforeRender(Self);
end;

procedure TSXMovie.DoAfterRender;
begin
  if Assigned(FOnAfterRender) then FOnAfterRender(Self);
end;

procedure TSXMovie.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent is TDXDraw then FDXDraw := nil;
    if AComponent is TDXSound then FDXSound := nil;
  end;
  inherited;
end;

procedure TSXMovie.SetMovieStateRun;
begin
  try
    if FMMStream.SetState(STREAMSTATE_RUN) <> S_OK then
      Exception.Create('Set Movie State Run Exception');
  except
  end;
end;

procedure TSXMovie.SetMovieStateStop;
begin
  try
    FMMStream.SetState(STREAMSTATE_STOP);
  finally
    FPlaying := False;
  end;
end;

procedure TSXMovie.CreateMediaStream;
var
  wPath:{$IFDEF UNICODE}array of Char{$ELSE}array[0..MAX_PATH] of WChar{$ELSE}{$ENDIF};
  AMStream:IAMMultiMediaStream;
  Media:IMediaStream;
begin
   Media := nil;
   AMStream := nil;
   try
      CoCreateinstance(CLSID_AMMULTIMEDIASTREAM,nil,CLSCTX_INPROC_SERVER,IID_IAMMULTIMEDIASTREAM,AMStream);
      {$IFDEF UNICODE}
      SetLength(wPath, Length(Filename) + 1);
      StrPCopy(@wPath, Filename);
      {$ELSE}
      MultiByteToWideChar(CP_ACP,0,PAnsiChar(Filename),-1,wPath,Sizeof(wPAth) div sizeof(wPath[0]));
      {$ENDIF}
      AMStream.Initialize(STREAMTYPE_READ,AMMSF_NOGRAPHTHREAD,nil);
      if (DXSound <> nil) and (DXSound.DSound <> nil) and (DXSound.DSound.ISound <> nil) then
         AMStream.AddMediaStream(DXSound.DSound.ISound,{$IFDEF UNICODE}@{$ENDIF}MSPID_PrimaryAudio,AMMSF_ADDDEFAULTRENDERER,IMediaStream(nil^));
      AMStream.AddMediaStream(DXDraw.DDraw.IDraw,{$IFDEF UNICODE}@{$ENDIF}MSPID_PrimaryVideo,0,Media);
      AMStream.OpenFile({$IFDEF UNICODE}@{$ENDIF}wPAth,0);
      FMMStream := AMStream;
   except
      FMMStream := nil;
   end;
end;

procedure TSXMovie.SetupMediaSample;
begin
  try
    FSample := nil;
    FDDStream := nil;
    FPrimaryVidStream := nil;
    if FMMStream.GetMediaStream(MSPID_PrimaryVideo, FPrimaryVidStream) <> S_OK then Exit;
    if FPrimaryVidStream.QueryInterface(IID_IDirectDrawMediaStream, FDDStream) <> S_OK then Exit;
    if FDDStream.CreateSample(nil, PRect(nil)^, 0, FSample) <> S_OK then Exit;
    FDXSurface := TDirectDrawSurface.Create(FDXDraw.DDraw);
    if FSample.GetSurface(FSurface, FRect) <> S_OK then Exit;
    FDXSurface.IDDSurface := FSurface;
  except

  end;
end;

procedure TSXMovie.UpDate;
  function AspectRatio(SourceRect: TRect; var DestRect: TRect): Boolean;
  var
    SourceWidth, SourceHeight, DestWidth, DestHeight: Integer;
    SourceRatio, DestRatio: Double;
  begin
    Result := False;
    SourceWidth := SourceRect.Right - SourceRect.Left;
    SourceHeight := SourceRect.Bottom - SourceRect.Top;
    SourceRatio := SourceWidth/SourceHeight;

    DestWidth := DestRect.Right - DestRect.Left;
    DestHeight := DestRect.Bottom - DestRect.Top;
    DestRatio := DestWidth/DestHeight;
    if SourceRatio <> DestRatio  then
    begin
      if DestWidth > DestHeight then
        DestRect.Bottom := DestRect.Top + Round(DestWidth / SourceRatio)
      else
        DestRect.Right := DestRect.Left + Round(SourceRatio * DestHeight);
      Result := True;
    end;
  end;
var
  R: TRect;
  Q: HResult;
begin
  try
    Q := FSample.Update(0, 0, nil, 0);
    case Q of
      HResult(MS_S_PENDING):;
      HResult(MS_S_NOUPDATE):;
      HResult(MS_S_ENDOFSTREAM):;
    end;
    if Q <> S_OK then
    begin
      FMovieThread.Terminate;
      SetMovieStateStop;
      FPlaying := False;
      DoMovieEnd;
      Exit;
    end;
    if (FSurface <> nil) and DXDraw.CanDraw then
    begin
      DoBeforeRender;
      R := FDestRect;
      if AspectRatio(FRect, R) then
        FDestRect := R;
      DXDraw.Surface.StretchDraw(FDestRect, FRect, FDXSurface, False);
      with DXDraw.Surface.Canvas do
      begin
        Brush.Style := bsClear;
        Font.Color := clWhite;
        Font.Size := 8;
        Textout(5, 5, 'SilleX Media - Beta 1');
        Release;
      end;
      DoAfterRender;
      if DoFlip then
        DXDraw.Flip;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TSXMovie.DisplayRect(Top, Left, Right, Bottom: integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    FScreenRect.Top := Top;
    FSCreenRect.Left := Left;
    FScreenRect.Right := Right;
    FScreenRect.Bottom := Bottom;
  end;
end;

procedure TSXMovie.Play;
begin
  if not (csDesigning in ComponentState) then
  begin
    FDestRect.Left := FScreenRect.Left;
    FDestRect.Right := FScreenRect.Right;
    FDestRect.Top := FScreenRect.Top;
    FDestRect.Bottom := FScreenRect.Bottom;
    FPlaying := True;
    CreateMediaStream;
    SetupMediaSample;
    if FSample <> nil then
    begin
      FMovieThread := TMovieThread.Create(True);
      if Assigned(FMovieThread) then
      begin
        FMovieThread.FreeOnTerminate := False;
        FMovieThread.SXMovie := Self;
        FMovieThread.Resume;
      end;
    end;
  end;
end;

procedure TSXMovie.Stop;
begin
  if not (csDesigning in ComponentState) then
  begin
    FPlaying := False;
    if Assigned(FMovieThread) then
    begin
      if not FMovieThread.Terminated then
      begin
        FMovieThread.Terminate;
        DoMovieEnd;
      end;
      FMovieThread.Free;
      FMovieThread := nil;
    end;
    if FDXSurface <> nil then
    begin
      FDXSurface.Free;
      FSurface := nil;
    end;
  end;
end;

constructor TSXMovie.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScreenRect := TScreenRect.Create;
  CoInitialize(nil);
end;

destructor TSXMovie.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    FPlaying := False;
    if Assigned(FMovieThread) then
    begin
      if not FMovieThread.Terminated then
        FMovieThread.Terminate;
      FMovieThread.Free;
      FMovieThread := nil;
    end;
    if Assigned(FSample) then
      FSample := nil;
    if Assigned(FDDStream) then
      FDDStream := nil;
    if Assigned(FPrimaryVidStream) then
      FPrimaryVidStream := nil;
    if Assigned(FMMStream) then
      FMMStream := nil;
    if Assigned(FSurface) then
      FSurface := nil;
  end;
  FScreenRect.Free;
  CoUnInitialize;
  inherited;
end;

end.

