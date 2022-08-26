unit Fileprops;

{ A Visual Component for Delphi V3 }
{ Provide an easy method of accessing or displaying the properties of a file }
{ Author:  Howard Harvey }
{ Contact: hharvey@dove.net.au }
{ Dated:   21/FEB/2000 }
{ Version: 1.1 }
{ Copyright: Freeware to those who do not charge for any application
             using this component. }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ShellApi ;

type
  ThhFileProps = class (Tcomponent)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
    fFileName : string ;
    fMSDOSname: string ;
    fFilePath : string ;
    fFileSize : longint ;
    fFileAttr : word ;
    fFileCreated  : tDateTime ;
    fFileModified : tDateTime ;
    fFileAccessed : tDateTime ;
  public
    { Public Declarations }
    constructor Create( AOwner : Tcomponent) ; override ;
    destructor Destroy ; override ;
    function ShowFileProperties( myFileName : string ) : boolean ;
    function GetFileProperties ( myFileName : string ) : boolean ;
    function BytesConvert( value : longint ): string ;
  published
    property FileName : string  read fFileName ;
    property MSDOSname: string  read fMSDOSname ;
    property FilePath : string  read fFilePath ;
    property FileSize : longint read fFileSize default 0 ;
    property FileAttr : word    read fFileAttr default 0 ;
    property FileCreated  : tDateTime read fFileCreated  ;
    property FileModified : tDateTime read fFileModified ;
    property FileAccessed : tDateTime read fFileAccessed ;
  end ;

procedure Register ;

implementation

type
  TFilePropsForm = class(TForm)
    BitBtn1: TBitBtn;
    Bevel2: TBevel;
    Bevel3: TBevel;
    labelLOC: TLabel;
    labelSIZE: TLabel;
    labelDOS: TLabel;
    labelCREATED: TLabel;
    labelMOD: TLabel;
    labelACC: TLabel;
    labelATTR: TLabel;
    CheckRO: TCheckBox;
    CheckArchive: TCheckBox;
    CheckHidden: TCheckBox;
    CheckSystem: TCheckBox;
    LocationData: TLabel;
    SizeData: TLabel;
    MSDOSdata: TLabel;
    CreatedData: TLabel;
    ModifiedData: TLabel;
    AccessedData: TLabel;
    EditFilename: TEdit;
    Label1: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    Label2: TLabel;
    UserData: TLabel;
    DateLabel: TLabel;
    LabelDate: TLabel;
    Label3: TLabel;
    TimeLabel: TLabel;
    Panel2: TPanel;
    Image1: TImage;
    procedure CheckClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fAttributes : word ;
  public
    { Public declarations }
  end;

{$R *.DFM}

var
  FilePropsForm: TFilePropsForm;

{ -------------------------------------------------------------------- }

constructor ThhFileProps.Create(AOwner:Tcomponent) ;

{ Create the form and initialise the parameters }

begin
  inherited Create(AOwner) ;
  fFileName := '' ;
  fMSDOSname:= '' ;
  fFilePath := '' ;
  fFileSize := 0 ;
  fFileAttr := 0 ;
  fFileCreated  := 0 ;
  fFileModified := 0 ;
  fFileAccessed := 0 ;
end;
{ -------------------------------------------------------------------- }

destructor ThhFileProps.Destroy ;
begin
  inherited Destroy ;
end ;

{ -------------------------------------------------------------------- }

function UserName : string ;

{ Retrieve the user name from the O/S }

var
  UName : String;
  NameSize : DWORD;
begin
  NameSize := 255;
  SetLength(UName, 254);
  if GetUserName(pChar(UName), NameSize) then
  begin
    SetLength(UName, NameSize);
    UserName := Uname;
  end;
end;

{ -------------------------------------------------------------------- }

function ThhFileProps.BytesConvert( value : longint ) : string ;

{ Convert a longint value to Kilobytes or Megabytes }
{ No number will contain more than 5 digits }
{ Conversion rounds to the nearest Kbyte or Mbyte }
{ 1 Kbyte = 1024 bytes }
{ 1 Mbyte = 1024 Kbytes = 1024*1024 or 1,048,576 bytes }

var
  sValue : string ;
begin
  if value < 99999
  then begin
    sValue := IntToStr( value ) ;
  end
  else begin
    value := (value+1023) SHR 10 ;
    if value < 99999
    then begin
      sValue := IntToStr( value ) + 'K' ;
    end
    else begin
      value := (value+1023) SHR 10 ;
      sValue := IntToStr( value ) + 'M' ;
    end ;
  end ;
  BytesConvert := sValue ;
end ;

{ -------------------------------------------------------------------- }

function ThhFileProps.GetFileProperties( myFileName : string ) : boolean ;

{ Extract file properties for the specified file }

var
  SearchRec : tSearchRec ;
  FileTime  : TFileTime ;
  DOSdate   : longint ;
  FATdate   : word ;
  FATtime   : word ;

begin
  fFileName := '' ;
  fMSDOSname:= '' ;
  fFilePath := '' ;
  fFileSize := 0 ;
  fFileAttr := 0 ;
  fFileCreated  := 0 ;
  fFileModified := 0 ;
  fFileAccessed := 0 ;

{ First make sure the file exists }

  Result := FileExists( myFileName ) ;
  if NOT Result then exit ;

{ Extract the FileName and FilePath }

  fFileName := ExtractFilename( myFileName ) ;
  fFilePath := ExtractFilePath( myFileName ) ;

{ Get the SearchRec data }

  FindFirst(myFilename, faAnyFile, SearchRec);

{ Extract FileSize and FileAttr values }

  fFileSize := SearchRec.Size ;
  fFileAttr := SearchRec.Attr ;

{ Extract the MSDOS filename }

  fMSDOSname := String(SearchRec.FindData.cAlternateFileName) ;
  if fMSDOSname = '' then fMSDOSname := Filename ;

{ Extract Creation Date/Time }

  FileTimeToLocalFileTime( SearchRec.FindData.ftcreationtime , FileTime) ;
  if FileTimeToDosDateTime( FileTime , FATdate , FATtime ) AND (FATdate <> 0)
  then begin
    DOSdate := (FATdate SHL 16) OR FATtime ;
    fFileCreated := FileDateToDateTime( DOSdate ) ;
  end
  else fFileCreated := 0 ;

{ Extract Modification Date/Time }

  FileTimeToLocalFileTime( SearchRec.Finddata.ftlastwritetime , FileTime) ;
  if FileTimeToDosDateTime( FileTime , FATdate , FATtime ) AND (FATdate <> 0)
  then begin
    DOSdate := (FATdate SHL 16) OR FATtime ;
    fFileModified := FileDateToDateTime( DOSdate ) ;
  end
  else fFileModified := 0 ;

{ Extract Access Date/Time }

  FileTimeToLocalFileTime( SearchRec.Finddata.ftlastaccesstime , FileTime) ;
  if FileTimeToDosDateTime( FileTime , FATdate , FATtime ) AND (FATdate <> 0)
  then begin
    DOSdate := (FATdate SHL 16) OR FATtime ;
    fFileAccessed := FileDateToDateTime( DOSdate ) ;
  end
  else fFileAccessed := 0 ;

{ Close the Search }

  FindClose( SearchRec ) ;
end ;

{ -------------------------------------------------------------------- }

function ThhFileProps.ShowFileProperties( myFileName : string ) : boolean ;
var
  fString1  : string ;
  fString2  : string ;
  IconX     : TIcon;
  IconIndex : word;

begin
  Result := GetFileProperties( myFileName ) ;

{ Make sure the file exists }

  if NOT Result then exit ;

{ Create the form }

  FilePropsForm := TFilePropsForm.Create( self ) ;
  with FilePropsForm do
  begin

{ Extract data }

    UserData.Caption := UserName ;
    EditFilename.Text := fFilename ;
    MSDOSdata.Caption := fMSDOSname ;
    LocationData.Caption := fFilePath ;

{ Convert filesize if required }

    fString1 := Format('%12.0n', [INT(FileSize)]) + ' bytes' ;
    while fString1[1] = ' ' do DELETE( fString1 , 1 , 1 ) ;
    fString2 := BytesConvert(FileSize) ;
    while fString2[1] = ' ' do DELETE( fString2 , 1 , 1 ) ;
    fString2 := ' ('+fString2+')' ;
    if fFileSize < 100000
    then SizeData.Caption := fstring1
    else SizeData.Caption := fString1+fString2;

{ Set up attributes checkboxes }

    fAttributes := fFileAttr ;
    CheckClick( Self ) ;

{ Set up dates - check for invalid dates }

    if (fFileCreated <> 0)
    then CreatedData.Caption := FormatDateTime('dddddd  tt', fFileCreated)
    else CreatedData.Caption := '(unknown)' ;

    if (fFileModified <> 0)
    then ModifiedData.Caption := FormatDateTime('dddddd  tt', fFileModified)
    else ModifiedData.Caption := '(unknown)' ;

    if (fFileAccessed <> 0)
    then AccessedData.Caption := FormatDateTime('dddddd', fFileAccessed)
    else AccessedData.Caption := '(unknown)' ;

    IconX:=TIcon.Create;
    Image1.Refresh ;
    IconIndex:=0;
    IconX.handle := ExtractAssociatedIcon(hInstance,
                    Pchar(myFilename),IconIndex);
    Image1.Width:= IconX.Width;
    Image1.Height:= IconX.Height;
//    Image1.Canvas.Draw(0,0,IconX);
    Image1.Picture.Icon := IconX ;

{ Show form and wait for the user response }

    ShowModal ;
    IconX.Free;

{ Destroy the form }

    Destroy ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure TFilePropsForm.CheckClick(Sender: TObject);

{ CheckClick sets the checkboxes - restoring them if they are clicked! }

begin
    CheckRO.Checked := (fAttributes and faReadOnly) <> 0 ;
    CheckArchive.Checked := (fAttributes and faArchive) <> 0 ;
    CheckHidden.Checked := (fAttributes and faHidden) <> 0 ;
    CheckSystem.Checked := (fAttributes and faSysFile) <> 0 ;
end;

{ -------------------------------------------------------------------- }

procedure TFilePropsForm.Timer1Timer(Sender: TObject);

{ Timer to give a 1-second resolution for date/time display }

var
  dString : string ;
begin
  Timer1.Interval := 1000 ;
  DateTimeToString( dString , 'dddddd' , Date ) ;
  DateLabel.Caption := dString ;
  DateTimeToString( dString , 'tt' , Time ) ;
  TimeLabel.Caption := dString ;
end;

procedure Register;
begin
  RegisterComponents('Howie', [ThhFileProps]);
end;

end.
