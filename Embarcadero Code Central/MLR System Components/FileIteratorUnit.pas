unit FileIteratorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TFileAttribute = (faReadOnly, faHidden, faSysFile, faVolumeID, faDirectory,
    faArchive);
  TFileAttributes = set of TFileAttribute;
  TBeforeIteratingEvent = procedure (Sender :TObject; Files :TStrings) of object;
  TFileFoundEvent = procedure (Sender :TObject; FileName :string;
    var Continue :Boolean) of object;

  TFileIterator = class(TComponent)
  private
    FUseAttributes: Boolean;
    FFilter: string;
    FFolder: string;
    FAttributes: TFileAttributes;
    FFileFoundEvent: TFileFoundEvent;
    FBeforeIterating: TBeforeIteratingEvent;
    FIncludeFolder: Boolean;
    function GetFullFilter: string;
    procedure SetFilter(const Value: string);
    procedure SetFolder(const Value: string);
    procedure SetFullFilter(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    procedure Execute;
    procedure ListFiles(Strings :TStrings);
    property FullFilter :string read GetFullFilter write SetFullFilter;
  published
    { Published declarations }
    property Filter :string read FFilter write SetFilter;
    property Folder :string read FFolder write SetFolder;
    property IncludeFolder    :Boolean read FIncludeFolder write FIncludeFolder default True;
    property Attributes       :TFileAttributes read FAttributes write FAttributes default [];
    property UseAttributes    :Boolean read FUseAttributes write FUseAttributes default False;
    property OnFileFound      :TFileFoundEvent read FFileFoundEvent write FFileFoundEvent;
    property OnBeforeIterating:TBeforeIteratingEvent read FBeforeIterating write FBeforeIterating;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TFileIterator]);
end;

{ TFileIterator }

function Slash(const s :string) :string;
begin
  Result := s;
  if s = '' then exit;
  if s[Length(s)] = '\' then exit;
  Result := s + '\';
end;

constructor TFileIterator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := '*.*';
  FAttributes := [];
  FUseAttributes := False;
  FIncludeFolder := True;
end;

procedure TFileIterator.Execute;
var FileNames :TStrings; i :Integer; Continue :Boolean;
begin
  if not Assigned(FFileFoundEvent) then exit;
  FileNames := TStringList.Create;
  try
    ListFiles(FileNames);
    if Assigned(FBeforeIterating) then
      FBeforeIterating(Self, FileNames);
    Continue := True;
    i := 0;
    while (i < FileNames.Count) and Continue do begin
      FFileFoundEvent(Self, FileNames[i], Continue);
      Inc(i);
    end;
  finally
    FileNames.Free;
  end;
end;

function TFileIterator.GetFullFilter: string;
begin
  Result := Slash(FFolder) + FFilter;
end;

procedure TFileIterator.ListFiles(Strings: TStrings);
var SearchRec :TSearchRec; Attr, Found :Integer;
begin
  Strings.Clear;
  Strings.BeginUpdate;
  try
    if FUseAttributes then begin
      Attr := 0;
      if faReadOnly in FAttributes  then Attr := Attr + SysUtils.faReadOnly;
      if faHidden in FAttributes    then Attr := Attr + SysUtils.faHidden;
      if faSysFile in FAttributes   then Attr := Attr + SysUtils.faSysFile;
      if faVolumeID in FAttributes  then Attr := Attr + SysUtils.faVolumeID;
      if faDirectory in FAttributes then Attr := Attr + SysUtils.faDirectory;
      if faArchive in FAttributes   then Attr := Attr + SysUtils.faArchive;
    end else
      Attr := faAnyFile;
    Found := FindFirst(FullFilter, Attr, SearchRec);
    try
      while Found = 0 do begin
        if FIncludeFolder then
          Strings.Add(Slash(FFolder) + SearchRec.Name) else
          Strings.Add(SearchRec.Name);
        Found := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TFileIterator.SetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TFileIterator.SetFolder(const Value: string);
begin
  FFolder := Value;
end;

procedure TFileIterator.SetFullFilter(const Value: string);
var i :Integer;
begin
  if Value = '' then begin
    FFilter := '';
    FFolder := '';
    exit;
  end;
  i := Length(Value);
  while (i > 0) and (Value[i] <> '\') do Dec(i);
  if Value[i] = '\' then begin
    FFilter := Copy(Value, i + 1, Length(Value));
    FFolder := Copy(Value, i, 1);
  end else begin
    FFilter := Value;
    FFolder := '';
  end;
end;

end.
 