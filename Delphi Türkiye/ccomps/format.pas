unit Format;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TCapacity  = (capDefault, capHigh, capLow);
  TFmtType   = (fmtQuick, fmtFull, fmtBoot);
  TDriveType = (dtADrive, dtBDrive);

  TFormat   = class(TComponent)
  private
    { Private declarations }
    FCapacity  : TCapacity;
    FFmtType   : TFmtType;
    FDriveType : TDriveType;
    FParent    : HWND;
    FWinNT     : Boolean;

    procedure SetDriveCapacity(Value : TCapacity);
    procedure SetFormatType(Value : TFmtType);

  protected
    { Protected declarations }

  public
    { Public declarations }
    constructor Create(Parent : TComponent); override;
    destructor  Destroy; override;

    function    FormatDrive : LongInt;

  published
    { Published declarations }
    property Capacity   : TCapacity  read FCapacity  write SetDriveCapacity
default capDefault;
    property FormatType : TFmtType   read FFmtType   write SetFormatType   
default fmtQuick;
    property Drive      : TDriveType read FDriveType write FDriveType      
default dtADrive;
  end;

{ Non object functions }
procedure Register;
function  SHFormatDrive(hWnd : HWND; iDriveID, iCapacity, iFormatType :
Integer) : longint; stdcall;

implementation

const
   T_SHELLDLL = 'shell32.dll';
   T_FMTDRV   = 'SHFormatDrive';
   T_WIN32    = 'Win32';

{ Non object functions }
function SHFormatDrive;	Stdcall; external T_SHELLDLL name T_FMTDRV;

procedure Register;
begin
  RegisterComponents('Cc', [TFormat]);
end;

{ Property Set and Get Methods }

{ SetDriveCapacity - set's the drive capacity during
      format. }
procedure TFormat.SetDriveCapacity(Value : TCapacity);
begin

   FCapacity := Value;
end; { SetDriveCapacity }

{ SetFormatType - set's the format type - some types aren't
      available under Windows NT 4.0 }
procedure TFormat.SetFormatType(Value : TFmtType);
begin

   FFmtType := Value;

end; { SetFormatType }
{ Constructor and Destructor }

{ Create }
constructor TFormat.Create(Parent : TComponent);
var os : TOSVERSIONINFO;
begin
   inherited Create(Parent);

   if Parent is TWinControl then
      FParent := TWinControl(Parent).Handle
   else
      FParent := Application.Handle;

   { The running OS must be determined }
   os.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
   GetVersionEx(os);
   if os.dwPlatformId=VER_PLATFORM_WIN32_NT then
      FWinNT := TRUE
   else
      FWinNT := FALSE;
end; { Create }

{ Destroy }
destructor TFormat.Destroy;
begin
   inherited Destroy;
end; { Destroy }

{ Public methods }

{ FormatDrive - this routine actually calls the API function to
      format the drive }
function TFormat.FormatDrive : LongInt;
var iDriveID, iCapacity, iFormatType : integer;
begin
   { set the drive ID }
   iDriveID := Ord(FDriveType);

   { set the capacity }
   if FWinNT or (FCapacity=capDefault)then
      { If Windows NT - then capacity is always default }
      iCapacity := 0
   else
   begin
   // TODO this needs some validation on what
   // type of drive it is.  Right now A = 3.5; B = 5.25
      if FCapacity=capLow then
      begin
         if iDriveID=0 then
            iCapacity := 5
         else
            iCapacity := 3;
      end
      else
         iCapacity := 0;
   end;
   { finally set the format type }
   if FWinNT then
   begin
      // NT doesn't support the making of a bootable floppy
      if (FFmtType=fmtFull) or (FFmtType=fmtBoot) then
         iFormatType := 0
      else
         iFormatType := 1;
   end
   else
   begin
      // Windows 95 (Maybe Windows 98?)
      if FFmtType=fmtQuick then
         iFormatType := 0
      else if FFmtType=fmtFull then
         iFormatType := 1
      else
         iFormatType := 2;
   end;
   Result := SHFormatDrive(FParent, iDriveID, iCapacity, iFormatType);
end; { FormatDrive }
end.


