unit TextConvertor;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TTextConvertor = class(tcomponent)
  private
    FInputFiles: TStrings;
    FOutputFiles: TStrings;
    procedure SetInputFiles(Value: TStrings);
    procedure SetOutputFiles(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property InputFiles: TStrings read FInputFiles write SetInputFiles;
    property OutputFiles: TStrings read FOutputFiles write SetOutputFiles;
  end;

procedure Register;

implementation

uses DesignEditors, DesignIntf, TextConvertorEditor;

constructor TTextConvertor.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FInputFiles := TStringList.Create;
   FOutputFiles := TStringList.Create;
end;

destructor TTextConvertor.Destroy;
begin
   FInputFiles.Free;
   FOutputFiles.Free;
   inherited Destroy;
end;

type
  TTextConvertorFilesProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTextConvertorInputProperty = class(TTextConvertorFilesProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
  end;

  TTextConvertorOutputProperty = class(TTextConvertorFilesProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
  end;

function TTextConvertorFilesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TTextConvertorInputProperty.Edit;
var
  List: TStringList;
  TextConvertor: TTextConvertor;
begin
  TextConvertor := GetComponent(0) as TTextConvertor;
  List := TStringList.Create;
  try
    List.Assign(TextConvertor.FInputFiles);
    if EditTextConvertorFiles(List, True) then
    begin
       TextConvertor.FInputFiles.Assign(List);
       Modified;
    end;
  finally
    List.Free;
  end;
end;

function TTextConvertorInputProperty.GetValue: string;
begin
  Result := strCaption[True];
end;

procedure TTextConvertorOutputProperty.Edit;
var
  List: TStringList;
  TextConvertor: TTextConvertor;
begin
  TextConvertor := GetComponent(0) as TTextConvertor;
  List := TStringList.Create;
  try
    List.Assign(TextConvertor.FOutputFiles);
    if EditTextConvertorFiles(List, False) then
    begin
       TextConvertor.FOutputFiles.Assign(List);
       Modified;
    end;
  finally
    List.Free;
  end;
end;

function TTextConvertorOutputProperty.GetValue: string;
begin
  Result := strCaption[False];
end;

function TTextConvertor.Execute: Boolean;
begin
   Result := False;
   try
   finally
   end;
end;

procedure TTextConvertor.SetInputFiles(Value: TStrings);
begin
  FInputFiles.Assign(Value);
end;

procedure TTextConvertor.SetOutputFiles(Value: TStrings);
begin
  FOutputFiles.Assign(Value);
end;

procedure Register;
begin
   RegisterPropertyEditor(TypeInfo(TStrings), TTextConvertor, 'InputFiles', TTextConvertorInputProperty);
   RegisterPropertyEditor(TypeInfo(TStrings), TTextConvertor, 'OutputFiles', TTextConvertorOutputProperty);

   RegisterComponents('Samples', [TTextConvertor]);
end;

end.
