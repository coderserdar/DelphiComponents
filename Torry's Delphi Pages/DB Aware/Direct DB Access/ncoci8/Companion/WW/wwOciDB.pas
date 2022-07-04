unit wwOciDB;

interface

uses
  SysUtils, Classes, DB, wwTypes, NCOCi, NCOciWrapper, NCOciDB;

type
  TwwOCIQuery = class(TOCIQuery)
  private
    FControlType: TStrings;
    FPictureMasks: TStrings;
    FOnInvalidValue: TwwInvalidValueEvent;
    FUsePictureMask: Boolean;

    function  GetControlType: TStrings;
    procedure SetControlType(Value: TStrings);
    function  GetPictureMasks: TStrings;
    procedure SetPictureMasks(Value: TStrings);
  protected
    procedure DoBeforePost; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ControlType: TStrings
             read GetControlType
             write SetControltype;
    property PictureMasks: TStrings
             read GetPictureMasks
             write SetPictureMasks;
    property ValidateWithMask: Boolean
             read FUsePictureMask
             write FUsePictureMask;
    property OnInvalidValue: TwwInvalidValueEvent
             read FOnInvalidValue
             write FOnInvalidValue;
  end;

  TwwOCIStoredProc = class(TOCIStoredProc)
  private
    FControlType: TStrings;
    FPictureMasks: TStrings;
    FOnInvalidValue: TwwInvalidValueEvent;
    FUsePictureMask: Boolean;

    function  GetControlType: TStrings;
    procedure SetControlType(Value: TStrings);
    function  GetPictureMasks: TStrings;
    procedure SetPictureMasks(Value: TStrings);
  protected
    procedure DoBeforePost; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ControlType: TStrings
             read GetControlType
             write SetControltype;
    property PictureMasks: TStrings
             read GetPictureMasks
             write SetPictureMasks;
    property ValidateWithMask: Boolean
             read FUsePictureMask
             write FUsePictureMask;
    property OnInvalidValue: TwwInvalidValueEvent
             read FOnInvalidValue
             write FOnInvalidValue;
  end;

procedure Register;

implementation

uses wwCommon;

{TwwOCIQuery}
constructor TwwOCIQuery.Create(AOwner: TComponent);
begin
  inherited;
  FControlType := TStringList.Create;
  FPictureMasks := TStringList.create;
  FUsePictureMask := True;
end;

destructor TwwOCIQuery.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  FPictureMasks := nil;
  inherited;
end;

function TwwOCIQuery.GetControltype: TStrings;
begin
  Result := FControlType;
end;

procedure TwwOCIQuery.SetControlType(Value : TStrings);
begin
  FControlType.Assign(Value);
end;

function TwwOCIQuery.GetPictureMasks: TStrings;
begin
  Result := FPictureMasks;
end;

procedure TwwOCIQuery.SetPictureMasks(Value : TStrings);
begin
  FPictureMasks.Assign(Value);
end;

procedure TwwOCIQuery.DoBeforePost;
begin
  inherited DoBeforePost;
  if FUsePictureMask then
    wwValidatePictureFields(Self, FOnInvalidValue);
end;

{TwwOCIStoredProc}
constructor TwwOCIStoredProc.Create(AOwner: TComponent);
begin
  inherited;
  FControlType := TStringList.Create;
  FPictureMasks := TStringList.create;
  FUsePictureMask := True;
end;

destructor TwwOCIStoredProc.Destroy;
begin
  FControlType.Free;
  FPictureMasks.Free;
  FPictureMasks := nil;
  inherited;
end;

function TwwOCIStoredProc.GetControltype: TStrings;
begin
  Result := FControlType;
end;

procedure TwwOCIStoredProc.SetControlType(Value : TStrings);
begin
  FControlType.Assign(Value);
end;

function TwwOCIStoredProc.GetPictureMasks: TStrings;
begin
  Result := FPictureMasks;
end;

procedure TwwOCIStoredProc.SetPictureMasks(Value : TStrings);
begin
  FPictureMasks.Assign(Value);
end;

procedure TwwOCIStoredProc.DoBeforePost;
begin
  inherited DoBeforePost;
  if FUsePictureMask then
    wwValidatePictureFields(Self, FOnInvalidValue);
end;

procedure Register;
begin
  RegisterComponents('IP Access', [TwwOCIQuery,
                                   TwwOCIStoredProc]);
end;

end.
