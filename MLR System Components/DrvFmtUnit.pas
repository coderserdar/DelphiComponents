unit DrvFmtUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TFormatOption = (foFull, foSysOnly);
  TFormatOptions = set of TFormatOption;
  TDriveFormatter = class(TComponent)
  private
    FErrorWhileFormatting: Boolean;
    FCancelled: Boolean;
    FCancelOnDiskette: Boolean;
    FSucceeded: Boolean;
    FDriveNotFormatable: Boolean;
    FDriveLetter: char;
    FFormatOptions: TFormatOptions;
    FLastFormatIDValid: Boolean;
    FRepeatFormat: Boolean;
    FLastFormatID: Word;
    FParent: TWinControl;
    procedure SetParent(const Value: TWinControl);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    procedure Execute;
    property Suceeded :Boolean read FSucceeded;
    property Cancelled :Boolean read FCancelled;
    property ErrorWhileFormatting :Boolean read FErrorWhileFormatting;
    property DriveNotFormatable :Boolean read FDriveNotFormatable;
    property LastFormatID :Word read FLastFormatID;
    property LastFormatIDValid :Boolean read FLastFormatIDValid;
  published
    { Published declarations }
    property DriveLetter :char read FDriveLetter write FDriveLetter default 'A';
    property FormatOptions :TFormatOptions read FFormatOptions write FFormatOptions default [];
    property CancelOnNoDiskette :Boolean read FCancelOnDiskette write FCancelOnDiskette default False;
    property RepeatFormat :Boolean read FRepeatFormat write FRepeatFormat default False;
    property Parent :TWinControl read FParent write SetParent;
  end;

procedure Register;

const
  { Special value of fmtID which means "use the default format". }
  SHFMT_ID_DEFAULT  = $FFFF;

  { Option bits for options parameter. }
  SHFMT_OPT_FULL    = $0001;
  SHFMT_OPT_SYSONLY = $0002;

  { Special return values. }
  SHFMT_ERROR       : DWord = $FFFFFFFF;  { Error on last format, drive may be formatable. }
  SHFMT_CANCEL      : DWord = $FFFFFFFE;  { Last format was canceled. }
  SHFMT_NOFORMAT    : DWord = $FFFFFFFD;  { Drive is not formatable. }
  
function SHFormatDrive(hWnd: HWND; Drive, FormatID, Options :Word) :DWord; stdcall;

implementation

const
  shell32 = 'shell32.dll';

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TDriveFormatter]);
end;

{ TDriveFormatter }

constructor TDriveFormatter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDriveLetter := 'A';
end;

procedure TDriveFormatter.Execute;
var
  Answer    :DWord; { The result of the operation. }
  Options   :Word;  { The operation options. }
  FormatID  :Word;  { The format type. }
  DriveNo   :Word;  { The driver number, where A = 1. }
  ParentHand: THandle;
begin
  Options := 0;
  if foFull in FFormatOptions then Options := Options or SHFMT_OPT_FULL;
  if foSysOnly in FFormatOptions then Options := Options or SHFMT_OPT_SYSONLY;
  DriveNo := Ord(UpCase(DriveLetter)) - Ord('A');
  if FRepeatFormat and FLastFormatIDValid then
    FormatID := FLastFormatID
  else
    FormatID := SHFMT_ID_DEFAULT;
  if Assigned(Parent) then
    ParentHand := Parent.Handle else
    ParentHand := 0;
  Answer := SHFormatDrive(ParentHand, DriveNo, FormatID, Options);
  FSucceeded            := not (Answer in [SHFMT_ERROR, SHFMT_CANCEL]);
  FErrorWhileFormatting := Answer = SHFMT_ERROR;
  FCancelled            := Answer = SHFMT_CANCEL;
  FDriveNotFormatable   := Answer = SHFMT_NOFORMAT;
  if FSucceeded then begin
    FLastFormatIDValid := True;
    FLastFormatID := LoWord(Answer);
  end;
end;

function SHFormatDrive; external shell32 name 'SHFormatDrive';

procedure TDriveFormatter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FParent) then
    FParent := nil;
end;

procedure TDriveFormatter.SetParent(const Value: TWinControl);
begin
  FParent := Value;
  if Assigned(FParent) then FParent.FreeNotification(Self);
end;

end.

