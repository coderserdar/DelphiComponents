unit DEditors;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  ComObj, ActiveX, DFilters,
  ADOApi, ADOEditorApi;

type
  TDConnectionEditor = class(TComponent)
  private
    { Private declarations }
    FConnect  : String;
    FDefTable : String;
  protected
    { Protected declarations }
    function PromptEditor(const ConnectStr: String): String;
  public
    { Public declarations }
    function Execute: Boolean;
  published
    { Published declarations }
    property Connection   : String read FConnect  write FConnect;
    property DefaultTable : String read FDefTable write FDefTable;
  end;

{******************************************************************************}

  TDFilterEditor = class(TComponent)
  private
    { Private declarations }
    FField    : TField;
    FDataSet  : TDataSet;
    FMarker   : String;
    FFilter   : TDSubFilters;

    // Handle properties
    procedure SetFilter(Value: TDSubFilters);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    { Published declarations }
    property Field     : TField       read FField   write FField;
    property DataSet   : TDataSet     read FDataSet write FDataSet;
    property Marker    : String       read FMarker  write FMarker;
    property SubFilter : TDSubFilters read FFilter  write SetFilter;
  end;

{******************************************************************************}

  TDColorEditor = class(TComponent)
  private
    { Private declarations }
    FDataSet  : TDataSet;
    FColors   : TDFilters;
    FMarker   : String;

  protected
    { Protected declarations }
  public
    { Public declarations }
    function Execute: Boolean;
  published
    { Published declarations }
    property DataSet : TDataSet  read FDataSet write FDataSet;
    property Colors  : TDFilters read FColors  write FColors;
    property Marker  : String    read FMarker  write FMarker;
  end;

{******************************************************************************}

  TDExportEditor = class(TComponent)
  private
    { Private declarations }
    FDataSet : TDataSet;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Execute;
  published
    { Published declarations }
    property DataSet : TDataSet read FDataSet write FDataSet;
  end;

{******************************************************************************}

  TDFillUpMode = (fmNone, fmConst, fmLower, fmUpper, fmSQL);
  TDFillUpEditor = class(TComponent)
  private
    { Private declarations }
    FField    : TField;
    FDataSet  : TDataSet;
    FMode     : TDFillUpMode;
    FText     : String;
    FLines    : TStrings;

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    { Published declarations }
    property Field     : TField       read FField   write FField;
    property DataSet   : TDataSet     read FDataSet write FDataSet;
    property Mode      : TDFillUpMode read FMode    write FMode default fmNone;
    property Lines     : TStrings     read FLines;
    property Text      : String       read FText;

  end;

implementation

uses DlgFilter,
     DlgExport,
     DlgFillUp,
     DlgColor;

{ TDConnection Editor }

function TDConnectionEditor.Execute: Boolean;
begin
     FConnect := PromptEditor(FConnect);
     Result := (FConnect <> '');
end;

function TDConnectionEditor.PromptEditor(const ConnectStr: String): String;
var
   oEdit : DataLinks;
   oConn : _Connection;
begin
     Result := ConnectStr;
     OleCheck(CoCreateInstance(CLASS_DataLinks, nil, CLSCTX_ALL, IID_IDataSourceLocator, oEdit));
     if Assigned(oEdit) then
     begin
          if ConnectStr = '' then
          begin
               IDispatch(oConn) := oEdit.PromptNew();
               try
                  Result := oConn.ConnectionString;
               except
               end;
          end
          else
          begin
               OleCheck(CoCreateInstance(CLASS_Connection, nil, CLSCTX_ALL, IID__Connection, oConn));
               oConn.ConnectionString := ConnectStr;
               if oEdit.PromptEdit(IDispatch(oConn)) then Result := oConn.ConnectionString;
          end;
     end;
     oConn := nil;
     oEdit := nil;
end;


{ TDFilterEditor }

constructor TDFilterEditor.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FFilter := TDSubFilters.Create(Self);
end;


destructor TDFilterEditor.Destroy;
begin
     FFilter.Free;
     inherited Destroy;
end;


function TDFilterEditor.Execute: Boolean;
var
   oDialog : TFilterDialog;
   i       : Integer;
begin
     Result := False;
     if not Assigned(Field) or not Assigned(DataSet) then Exit;
     oDialog := TFilterDialog.Create(Application);
     try
     begin
          oDialog.Field     := FField;
          oDialog.Marker    := FMarker;
          oDialog.DataSet   := FDataSet;
          oDialog.UpperCase := (foCaseInsensitive in FDataSet.FilterOptions);
          oDialog.Fields.Clear;
          oDialog.cbxFields.Items.Clear;
          for i := 0 to FDataSet.Fields.Count-1 do
          begin
               oDialog.Fields.Add(FDataSet.Fields[i].FieldName);
               oDialog.cbxFields.Items.Add(Trim(FDataSet.Fields[i].DisplayLabel));
          end;
          Result := (oDialog.ShowModal = mrOk);
          if Result then SubFilter := oDialog.Filters;
     end;
     finally
          oDialog.Free;
     end;
end;


procedure TDFilterEditor.SetFilter(Value: TDSubFilters);
begin
     FFilter.Assign(Value);
end;


{ TDColorEditor }


function TDColorEditor.Execute: Boolean;
var
  oColFlt : TColorFltDialog;
begin
     oColFlt := TColorFltDialog.Create(Application);
     try
     begin
          oColFlt.DataSet     := FDataSet;
          oColFlt.Colors      := FColors;
          oColFlt.MarkerField := FMarker;
          Result := (oColFlt.ShowModal = mrOk);
     end;
     finally
          oColFlt.Free;
     end;
end;


{ TDExportEditor }

procedure TDExportEditor.Execute;
var
   oExport: TExportDialog;
begin
     oExport := TExportDialog.Create(Application);
     try
     begin
          oExport.DataSet := FDataSet;
          oExport.ShowModal;
     end;
     finally
          oExport.Free;
     end;
end;


{ TDFillUpEditor }

constructor TDFillUpEditor.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     FLines := TStringList.Create;
end;


destructor TDFillUpEditor.Destroy;
begin
     FLines.Free;
     FLines := nil;
     inherited Destroy;
end;


function TDFillUpEditor.Execute: Boolean;
var
   oFillUp: TFillUpDialog;
begin
     Result := False;
     if Assigned(FField) then
     begin
          if FField.FieldKind = fkData then
          begin
               oFillUp := TFillUpDialog.Create(Application);
               try
               begin
                    oFillUp.Field := FField;
                    FMode  := fmNone;
                    Result := (oFillUp.ShowModal = mrOk);
                    if Result then
                    begin
                         FText := oFillUp.edTolt.Text;
                         FLines.Assign(oFillUp.mSQL.Lines);
                         case oFillUp.rgpKonvert.ItemIndex of
                              cniToUpper    : FMode := fmUpper;
                              cniToLower    : FMode := fmLower;
                              cniFillConst  : FMode := fmConst;
                              cniSQL        : FMode := fmSQL;
                         end;
                    end;
               end;
               finally
                    oFillUp.Free;
               end;
          end;
     end;
end;


end.
