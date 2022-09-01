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

unit dgBase;

interface

uses
  Classes;

type
  {TBaseGDAODiagram is an object that holds information about a diagram in the DM project}
  TBaseGDAODiagram = class(TCollectionItem)
  private
    FDiagramName   : String;
    FDiagramString : string;
  protected
    function GetDiagramString: String; virtual;
    procedure SetDiagramString(const Value: String); virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    {DiagramName contains the name of the diagram}
    property DiagramName   : String read FDiagramName write FDiagramName;
    {DiagramString contains meta information about the diagram. This is for internal use only}
    property DiagramString : String read GetDiagramString write SetDiagramString;
  end;


 {TBaseVersionControl object holds information about all the versions of the DM project.
  It doesn't hold the model for each version itself, only basic information (date, number, etc.)}
  TBaseVersionControl = class(TCollection)
  end;

  {TBaseVersion object contains information about a DM project version archived. Note that no information
   about the database structure is provided (tables, fields, etc.), only basic information like
   version number, date, etc.}
  TBaseVersion = class(TCollectionItem)
  private
    FVersionID   : Integer;
    FFileName    : String;
    FAuthor      : String;
    FInformation : String;
    FDateTime    : TDateTime;
    FCloseDate: TDateTime;
  public
    procedure Assign(Source: TPersistent); override;
  published
    {Contains the version number}
    property VersionID   : Integer read FVersionID write FVersionID;
    {Contains the TDateTime value that refers to the date the version was open}
    property DateTime    : TDateTime read FDateTime write FDateTime;
    {Contains the TDateTime value that refers to the date the version was closed (arhived)}
    property CloseDate: TDateTime read FCloseDate write FCloseDate;
    {Contains the name of the file that holds information about the database model (tables, fields, etc.)
    related to this version} 
    property FileName    : string read FFileName write FFileName;
    {Contains the description / comments of this version}
    property Information : string read FInformation write FInformation;
  end;

  {TBaseAppMetaData is the ancestor class for TAppMetaData, which is the base class that holds information about the DM project.
  Basically it has three types of information:
  a) the data dictionary model;
  b) the model versions (only date, number, description, etc., not the actually entire model)
  c) diagrams}
  TBaseAppMetaData = class(TComponent)
  private
    FPrjName: string;
    FPrjAuthor: string;
    FPrjDescription: string;
    FVersionControlPath: string;
    FUserOptions: TStringList;
    FPrjDBName: string;
    procedure SetUserOptions(const Value: TStringList);
  protected
    function GetPrjDBName: string; virtual;
    procedure SetPrjDBName(const Value: string); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    {Contains the project name of the DM project}
    property PrjName            : string read FPrjName write FPrjName;
    {Contains the name of the author of the DM project}
    property PrjAuthor          : string read FPrjAuthor write FPrjAuthor;
    {Contains the description of the DM project}
    property PrjDescription     : string read FPrjDescription write FPrjDescription;
    {Contains the directory that holds all archived versions of the DM project}
    property VersionControlPath : string read FVersionControlPath write FVersionControlPath;
    {Contains custom user options. Used only by the TMS Data Modeler application}
    property UserOptions: TStringList read FUserOptions write SetUserOptions;
  end;

implementation

{ TBaseVersion }

procedure TBaseVersion.Assign(Source: TPersistent);
begin
  VersionID := TBaseVersion(Source).VersionID;
  DateTime := TBaseVersion(Source).DateTime;
  FileName := TBaseVersion(Source).FileName;
  FAuthor := TBaseVersion(Source).FAuthor;
  Information := TBaseVersion(Source).Information;
  CloseDate := TBaseVersion(Source).CloseDate;
end;

{ TBaseAppMetaData }

constructor TBaseAppMetaData.Create(Owner: TComponent);
begin
  inherited;
  FUserOptions := TStringList.Create;
end;

destructor TBaseAppMetaData.Destroy;
begin
  FUserOptions.Free;
  inherited;
end;

function TBaseAppMetaData.GetPrjDBName: string;
begin
  result := FPrjDBName;
end;

procedure TBaseAppMetaData.SetPrjDBName(const Value: string);
begin
  FPrjDBName := Value;
end;

procedure TBaseAppMetaData.SetUserOptions(const Value: TStringList);
begin
  FUserOptions.Assign(Value);
end;

{ TBaseGDAODiagram }

procedure TBaseGDAODiagram.Assign(Source: TPersistent);
begin
  DiagramName := TBaseGDAODiagram(Source).DiagramName;
  DiagramString := TBaseGDAODiagram(Source).DiagramString;
end;

function TBaseGDAODiagram.GetDiagramString: String;
begin
  result := FDiagramString;
end;

procedure TBaseGDAODiagram.SetDiagramString(const Value: String);
begin
  FDiagramString := Value;
end;

end.

