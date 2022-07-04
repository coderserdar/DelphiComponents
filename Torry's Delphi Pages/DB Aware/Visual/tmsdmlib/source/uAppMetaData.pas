{***************************************************************************}
{ TMS Data Modeler Library for Delphi & C++Builder                          }
{ version 1.7                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1997 - 2010                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such.                             }
{***************************************************************************}

unit uAppMetaData;

interface

uses
  Classes, SysUtils, uDBProperties, dgBase, uGDAO;

type
  EDMException = class(Exception);

  TAppMetaData = class;

  TVersionControl = class;
  TVersion = class;

  TGDAODiagramsObject = class;
  TGDAODiagrams       = class;
  TGDAODiagram        = class;

  {TGDAODigramsObject is a container for the list of diagrams defined in the DM project}
  TGDAODiagramsObject = class(TPersistent)
  private
    FGDD      : TGDD;
    FDiagrams : TGDAODiagrams;
    procedure SetDiagrams(const Value: TGDAODiagrams);
  public
    constructor Create;
    destructor Destroy; override;
  published
    {Diagrams property contains a collection of the diagrams defined in the DM project}
    property Diagrams: TGDAODiagrams read FDiagrams write SetDiagrams;
  end;

  {TGDAODiagrams is a collection that holds all the diagams defined in the DM project}
  TGDAODiagrams = class(TCollection)
  private
    function GetItem(i: integer): TGDAODiagram;
  public
    {Items property contains a collection of TGDAODiagram objects. Each TGDAODiagram
    corresponds to a diagram defined in the DM project}
    property Items[i: integer]: TGDAODiagram read GetItem; default;
  end;

  {TGDAODiagram is an object that holds information about a diagram in the DM project}
  TGDAODiagram = class(TBaseGDAODiagram)
  end;

  {TAppMetaData is the base class that holds information about the DM project.
  Basically it has three types of information:
  a) the data dictionary model;
  b) the model versions (only date, number, description, etc., not the actually entire model)
  c) diagrams}
  TAppMetaData = class(TBaseAppMetaData)
  private
    FDataDictionary           : TGDAODatabase;
    FVersionControl           : TVersionControl;
    FDiagramObj               : TGDAODiagramsObject;
    FFileNAme: string;
    FSavingVersionArchive: boolean;
    procedure SetDataDictionary(const Value: TGDAODatabase);
    procedure SetDiagramObj(const Value: TGDAODiagramsObject);
    procedure SetVersionControl(const Value: TVersionControl);
    function IsPropertyStored: Boolean;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    {LoadFromFile method is the main method of TAppMetaData. You must provide a file name for the TMS Data Modeler
     project (.dgp file). This method is a class method and will *create* a new TAppMetaData instance}
    class function LoadFromFile(AFileName: string): TAppMetaData;
    {Holds the name of the file which was loaded by the last call of LoadFromFile}
    property FileName: string read FFileName write FFileName;
 published
    {VersionControl property holds information about the versions of the DM project.}
    property VersionControl: TVersionControl read FVersionControl write SetVersionControl stored IsPropertyStored;
    {DataDictionary property provides a TGDAODatabase object that holds information about the whole database model, like tables, fields,
    relationships, procedures, etc.}
    property DataDictionary: TGDAODatabase read FDataDictionary write SetDataDictionary;
    {DiagramObj provides a TGDAODiagramsObject that holds information about the diagrams defined in the DM project}
    property DiagramObj: TGDAODiagramsObject read FDiagramObj write SetDiagramObj;
 end;

 {TVersionControl object holds information about all the versions of the DM project.
  It doesn't hold the model for each version itself, only basic information (date, number, etc.)}
  TVersionControl = class(TBaseVersionControl)
  private
    FAMD: TAppMetaData;
    function GetItem(i: integer): TVersion;
    procedure SetItem(i: integer; const Value: TVersion);
  public
    constructor Create(AAMD: TAppMetaData);
    {Not implemented}
    function Add: TVersion;
    {Retrieves a TVersion object that holds information about the last version archived}
    function GetLastVersion : TVersion;
    {HasVersions returns true if the DM project has one or more versions archived}
    function HasVersions: Boolean;
    {Returns the index of the AVersion object in the versions collection}
    function IndexOf(AVersion: TVersion): integer;
    {Returns the reference to the TAppMetaData object which this version object belongs to}  
    property AMD: TAppMetaData read FAMD;
    {Holds a collection of TVersion objects that holds information for all versions archived}
    property Items[i: integer]: TVersion read GetItem write SetItem; default;
  end;

  {TVersion object contains information about a DM project version archived. Note that no information
   about the database structure is provided (tables, fields, etc.), only basic information like
   version number, date, etc.}
  TVersion = class(TBaseVersion)
  private
    function GetVersionControl: TVersionControl;
  public
    {Returns true if this version is actually the current version of the DM project}
    function IsCurrentVersion: boolean;
    {Returns a reference to the TVersionControl object which this TVersion object belongs to}
    property VersionControl: TVersionControl read GetVersionControl;
  end;

implementation

procedure WriteComponentResFileText(AFileName:string;AInstance:TComponent);
var BinStream : TMemoryStream;
    FileStream : TFileStream;
begin
   BinStream := TMemoryStream.Create;
   try
      FileStream := TFileStream.Create(AFileName,fmCreate);
      try
         BinStream.WriteComponent(AInstance);
         BinStream.Seek(0, soFromBeginning);
         ObjectBinaryToText(BinStream, FileStream);
      finally
         FileStream.Free;
      end;
   finally
      BinStream.Free
   end;
end;

function ReadComponentResFileText(const FileName: string; Instance: TComponent): TComponent;
var FileStream : TFileStream;
    BinStream  : TMemoryStream;
begin
   FileStream := TFileStream.Create(FileName,fmOpenRead);
   try
      BinStream := TMemoryStream.Create;
      try
         ObjectTextToBinary(FileStream, BinStream);
         BinStream.Seek(0, soFromBeginning);
         Result := BinStream.ReadComponent(Instance);
      finally
         BinStream.Free;
      end;
   finally
      FileStream.Free;
   end;
end;

{ TAppMetaData }

constructor TAppMetaData.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Name := 'AppMetaData';
   FDataDictionary := TGDAODatabase.Create(self);
   FVersionControl := TVersionControl.Create(Self);
   FDiagramObj := TGDAODiagramsObject.Create;
   FDiagramObj.FGDD := DataDictionary;
end;

destructor TAppMetaData.Destroy;
begin
   if Assigned(FDataDictionary) then FDataDictionary.Free;
   if Assigned(FVersionControl) then FVersionControl.Free;
   if Assigned(FDiagramObj) then FDiagramObj.Free;
   inherited;
end;

class function TAppMetaData.LoadFromFile(AFileName: string): TAppMetaData;
begin
  result := TAppMetaData.Create(nil);
  try
    try
      ReadComponentResFileText(AFileName, result);
      result.FileName := AFileName;
    except
      on e: Exception do
        raise EDMException.Create(
          Format('The file "%s" is not a valid Data Modeler project file.'#13#10+
          'Internal error: %s',
            [ExtractFileName(AFileName), e.Message]));
    end;
    TDBProperties.LoadAll(result.DataDictionary);
  except
    result.Free;
    raise;
  end;
end;

procedure TAppMetaData.Loaded;
begin
  inherited;
  DataDictionary.Loaded;
end;

procedure TAppMetaData.SetDataDictionary(const Value: TGDAODatabase);
begin
  TDBProperties.CopyDictionary(Value, FDataDictionary);
end;

procedure TAppMetaData.SetDiagramObj(const Value: TGDAODiagramsObject);
begin
  FDiagramObj.Assign(Value);
end;

procedure TAppMetaData.SetVersionControl(const Value: TVersionControl);
begin
  FVersionControl.Assign(Value);
end;

function TAppMetaData.IsPropertyStored: Boolean;
begin
  result := not FSavingVersionArchive;
end;

{ TVersionControl }

function TVersionControl.HasVersions: Boolean;
begin
  Result := (Count > 0);
end;

function TVersionControl.IndexOf(AVersion: TVersion): integer;
begin
  for result := 0 to Count - 1 do
    if AVersion = Items[result] then
      exit;
  result := -1;
end;

function TVersionControl.Add: TVersion;
begin
  Result := TVersion(inherited Add);
end;

constructor TVersionControl.Create(AAMD: TAppMetaData);
begin
  FAMD := AAMD;
  inherited Create(TVersion);
end;

function TVersionControl.GetItem(i: integer): TVersion;
begin
  Result := TVersion(inherited Items[i]);
end;

function TVersionControl.GetLastVersion: TVersion;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[Count - 1];
end;

procedure TVersionControl.SetItem(i: integer; const Value: TVersion);
begin
  Items[i].Assign(Value);
end;

{ TVersion }

function TVersion.GetVersionControl: TVersionControl;
begin
  result := TVersionControl(Collection);
end;

function TVersion.IsCurrentVersion: Boolean;
begin
  result := (TVersionControl(Collection).GetLastVersion = self);
end;

{ TGDAODiagramsObject }

constructor TGDAODiagramsObject.Create;
begin
  inherited;
  FDiagrams := TGDAODiagrams.Create(TGDAODiagram);
end;

destructor TGDAODiagramsObject.Destroy;
begin
  FDiagrams.Free;
  inherited;
end;

procedure TGDAODiagramsObject.SetDiagrams(const Value: TGDAODiagrams);
begin
  FDiagrams.Assign(Value);
end;

{ TGDAODiagrams }

function TGDAODiagrams.GetItem(i: integer): TGDAODiagram;
begin
  result := TGDAODiagram(inherited Items[i]);
end;

end.

