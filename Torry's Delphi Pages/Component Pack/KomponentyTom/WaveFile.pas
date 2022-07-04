unit WaveFile;

interface

uses
  SysUtils, Classes;

type

  TWaveFileString = type string;

  EWaveError = class(Exception);

  TWavePause = (wpAsync, wpSync);
  TWaveLoop  = (wlNoLoop, wlLoop);


  TWaveFile = class(TComponent)
  private
    { Private declarations }
    FData: Pointer;
    FDataSize: Integer;
    FWaveName: TWaveFileString;
    FWavePause: TWavePause;
    FWaveLoop: TWaveLoop;
    FOnPlay: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FWaveFileName: TWaveFileString;
    procedure SetWaveFileName(const Value: TWaveFileString);
    procedure WriteData(Stream: TStream);
    procedure ReadData(Stream: TStream);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Public declarations }
    destructor Destroy; override;
    function Empty: Boolean;
    function Equal(Wav: TWaveFile): Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(S: TStream);
    procedure Play;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(S: TStream);
    procedure Stop;
  published
    { Published declarations }
    property WavePause: TWavePause read FWavePause write FWavePause;
    property WaveLoop: TWaveLoop read FWaveLoop write FWaveLoop;
    property WaveName: TWaveFileString read FWaveFileName write SetWaveFileName;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

procedure Register;

implementation
uses TypInfo, Dialogs, Forms, Windows, MMSystem;

type

  TWaveFileStringProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TWaveEditor = class(TComponentEditor)
  private
    procedure EditProp(PropertyEditor: TPropertyEditor);
  protected
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

const
  VerbCount = 3;
  ArrayVerb : array[0..VerbCount - 1] of string[10] = ('Odtwarzaj','Stop',
                 'O ...');


{ TWaveFile }

function StreamEqual(S1, S2: TMemoryStream): Boolean;
begin
  Result:=(S1.Size = S2.Size) and
          CompareMem(S1.Memory, S2.Memory, S1.Size); 
end;

procedure TWaveFile.DefineProperties(Filer: TFiler);
function DoWrite: Boolean;
begin
  if Filer.Ancestor <> nil then
    Result:= not (Filer.Ancestor is TWaveFile) or
      not Equal(TWaveFile(Filer.Ancestor))
  else
    Result:= not Empty;
end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

destructor TWaveFile.Destroy;
begin
  if not Empty then
    FreeMem(FData, FDataSize);
  inherited Destroy;
end;

function TWaveFile.Empty: Boolean;
begin
  Result:= FDataSize = 0;
end;

function TWaveFile.Equal(Wav: TWaveFile): Boolean;
var
  MyImage, WavImage: TMemoryStream;
begin
  Result:= (Wav <> nil) and (ClassType = Wav.ClassType);
  if Empty or Wav.Empty then
  begin
    Result:= Empty and Wav.Empty;
    Exit;
  end;
  if Result then
  begin
    MyImage:= TMemoryStream.Create;
    try
      SaveToStream(MyImage);
      WavImage:=TMemoryStream.Create;
      try
        Wav.SaveToStream(WavImage);
        Result:= StreamEqual(MyImage, WavImage);
      finally
        WavImage.Free;
      end;
    finally
      MyImage.Free;
    end;
  end;
end;

procedure TWaveFile.LoadFromFile(const FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName,fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TWaveFile.LoadFromStream(S: TStream);
begin
  if not Empty then
    FreeMem(FData, FDataSize);
  FData:= AllocMem(S.Size);
  FDataSize:= S.Size;
  S.Read(FData^, FDataSize);
end;

procedure TWaveFile.Play;
const
  LoopArray: array[TWaveLoop] of DWORD = (0, SND_LOOP);
  PauseArray: array[TWavePause] of DWORD = (SND_ASYNC, SND_SYNC);
begin
  if Empty then
    raise EWaveError.Create('Nie ma danych wave');
  if Assigned(FOnPlay) then FOnPlay(Self);
  if not PlaySound(FData, 0, SND_MEMORY or PauseArray[FWavePause] or
            LoopArray[FWaveLoop]) then
    raise EWaveError.Create('B³¹d odtwarzania dzwiêku');
end;

procedure TWaveFile.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TWaveFile.SaveToFile(const FileName: string);
var F: TFileStream;
begin
  F:= TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.free;
  end;
end;

procedure TWaveFile.SaveToStream(S: TStream);
begin
  if not Empty then
    S.Write(FData^, FDataSize);
end;

procedure TWaveFile.SetWaveFileName(const Value: TWaveFileString);
begin
  if Value <> '' then
  begin
    FWaveFileName := ExtractFileName(Value);
    if (not (csLoading in ComponentState)) and FileExists(Value) then
      LoadFromFile(Value);
  end else
  begin
    FWaveName:='';
    if not Empty then
      FreeMem(FData, FDataSize);
    FDataSize:= 0;
  end;
end;

procedure TWaveFile.Stop;
begin
  if Assigned(FOnStop) then FOnStop(Self);
  PlaySound(nil, 0, SND_PURGE);
end;

procedure TWaveFile.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;


{ TWaveFileStringProperty }

procedure TWaveFileStringProperty.Edit;
var OpenDialog: TOpendialog;
begin
  OpenDialog:= TOpenDialog.Create(Application);
  try
    with OpenDialog do
    begin
      Filter:='Pliki WAV|*.wav|Wszystkie plik|*.*';
      DefaultExt:='*.wav';
      FileName:=GetStrValue;
      if Execute then SetStrValue(FileName);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TWaveFileStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

{ TWaveEditor }

procedure TWaveEditor.Edit;
var Components: TComponentList;
begin
  Components:= TComponentList.Create;
  try
    Components.Add(Component);
    GetComponentProperties(Components,tkAny, Designer, EditProp);
  finally
    Components.Free;
  end;
end;

procedure TWaveEditor.EditProp(PropertyEditor: TPropertyEditor);
begin
  if PropertyEditor is TWaveFileStringProperty then
  begin
    TWaveFileStringProperty(PropertyEditor).Edit;
    Designer.Modified;
  end;
end;

procedure TWaveEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: TWaveFile(Component).Play;
    1: TWaveFile(Component).Stop;
    2: MessageDlg('TWaveFile'^M +
     'Komponent odtwarzaj¹cy dzwiêk zapisany w strumieniu DFM'^M^M +
     'Copyright (c) 1999 Tomasz Bojara',mtInformation,[mbOK],0);    
  end;
end;

function TWaveEditor.GetVerb(Index: Integer): string;
begin
  Result:= ArrayVerb[Index];
end;

function TWaveEditor.GetVerbCount: Integer;
begin
  Result:= VerbCount;
end;

procedure Register;
begin
  RegisterComponents('TOM', [TWaveFile]);              
  RegisterPropertyEditor(TypeInfo(TWaveFileString),TWaveFile,'WaveName',
       TWaveFileStringProperty);
  RegisterComponentEditor(TWaveFile, TWaveEditor);
end;

end.
