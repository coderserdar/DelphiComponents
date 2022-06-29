{   Component(s):
    TcyDBClientLookupComboBox

    Description:
    A DBLookupComboBox with :
    - DropDownControl property that can replace Drop down listbox.
    - Internal Dataset for custom order / filter

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyBaseDBClientLookupComboBox;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses Classes, Windows, Messages, SysUtils, Controls, StdCtrls, Dialogs, Db, DbClient, Provider, DBCtrls, Variants, cyDb, cyFlyingContainer, cyBaseDBlookupComboBox;

type
  TcyBaseDBClientLookupComboBox = class;

  TcyListSourceLink = class(TDataLink)
  private
    FDBLookupControl: TcyBaseDBClientLookupComboBox;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
    procedure EditingChanged; override;
  public
    constructor Create;
  end;

  TUpdateInternalDatasetEvent = procedure (Sender: TObject; var WithDataset: TDataset)  of object;

  TcyBaseDBClientLookupComboBox = class(TcyBaseDBlookupComboBox)
    FInternalListSource: TDataSource;
    FInternalDataset: TClientDataset;
    FDatasetProvider: TDatasetProvider;
    FcyListLink: TcyListSourceLink;
  private
    FUpdatingInternalDataset: Boolean;
    FDropDownRefreshList: Boolean;
    FListFilter: String;
    FInternalFilter: String;
    FOnUpdateInternalDataset: TUpdateInternalDatasetEvent;
    function GetListFilter: String;
    function GetListOrderFieldNames: String;
    procedure SetListFilter(const Value: String);
    procedure SetListOrderFieldNames(const Value: String);
    function GetListSource: TDataSource;
    procedure SetListSource(const Value: TDataSource);
    procedure FInternalDatasetBeforeClose(DataSet: TDataSet);
    function GetListFilterOptions: TFilterOptions;
    procedure SetListFilterOptions(const Value: TFilterOptions);
    procedure SetFInternalFilter(const Value: String);
  protected
    procedure DropDown; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure cyListLinkDataChanged;
    procedure cyListLinkEditingChanged;
    procedure cyUpdateListFields;
    procedure UpdateInternalDataset; virtual;   // Only called when user Drop down ...
    procedure UpdateInternalDatasetFilter;
    property cyListLink: TcyListSourceLink read FcyListLink;
    property DropDownControl;
    property InternalFilter: String read FInternalFilter write SetFInternalFilter;
    property DropDownRefreshList: Boolean read FDropDownRefreshList write FDropDownRefreshList default false;
    property ListOrderFieldNames: String read GetListOrderFieldNames write SetListOrderFieldNames;
    property ListFilter: String read FListFilter write SetListFilter;
    property ListFilterOptions: TFilterOptions read GetListFilterOptions write SetListFilterOptions;
    property ListSource: TDataSource read GetListSource write SetListSource;  // New ListSource property !
    property OnUpdateInternalDataset: TUpdateInternalDatasetEvent read FOnUpdateInternalDataset write FOnUpdateInternalDataset;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp(Accept: Boolean); override;
    function GetDropDownControlDefs: TcyFlyingContainer;
    function GetInternalListSource: TDataSource;
    function GetInternalDataSet: TClientDataset;
    function GetInternalDatasetCurrentKeyValue: Variant;
    procedure SelectCurrentKeyValue(const CloseDropDown: Boolean = true);
  published
  end;



  TcyDBClientLookupComboBox = class(TcyBaseDBClientLookupComboBox)
  private
  protected
  public
  published
    property DropDownControl;
    property DropDownRefreshList;
    property ListOrderFieldNames;
    property ListFilter;
    property ListFilterOptions;
    property ListSource;  // New ListSource property !
    property OnUpdateInternalDataset;
  end;

implementation

{ TcyListSourceLink }
constructor TcyListSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TcyListSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.cyUpdateListFields;
end;

procedure TcyListSourceLink.DataSetChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.cyListLinkDataChanged;
end;

procedure TcyListSourceLink.EditingChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.cyListLinkEditingChanged;
end;

procedure TcyListSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.cyUpdateListFields;
end;


{ TcyBaseDBClientLookupComboBox }
constructor TcyBaseDBClientLookupComboBox.Create(AOwner: TComponent);
begin
  FDropDownRefreshList := false;
  FUpdatingInternalDataset := false;
  FInternalFilter := '';

  FDatasetProvider := TDatasetProvider.Create(Self);
  FDatasetProvider.Name := 'InternalLookupProvider';

  FInternalDataset := TClientDataset.Create(Self);
  FInternalDataset.ProviderName := FDatasetProvider.Name;
  FInternalDataset.FilterOptions := [foCaseInsensitive];
  FInternalDataset.BeforeClose := FInternalDatasetBeforeClose;

  // FDatasetProvider.Options := [poNoReset,poUseQuoteChar];
  // FDatasetProvider.SetSubComponent(True);
  // FInternalDataset.SetProvider(FDatasetProvider);



  FInternalListSource := TDataSource.Create(Self);
  FInternalListSource.DataSet := FInternalDataset;

  inherited;

  FcyListLink := TcyListSourceLink.Create;
  FcyListLink.FDBLookupControl := Self;
end;

destructor TcyBaseDBClientLookupComboBox.Destroy;
begin
  FInternalListSource.Free;
  FInternalDataset.Free;
  FDatasetProvider.Free;

  inherited Destroy;

  if FcyListLink <> nil then
    FcyListLink.FDBLookupControl := nil;
  FcyListLink.Free;
  FcyListLink := nil;
end;

procedure TcyBaseDBClientLookupComboBox.FInternalDatasetBeforeClose(DataSet: TDataSet);
begin
  // Delete any index added to the dataset to avoid error when trying to re-open it:
  if FInternalDataset.IndexName <> '' then
    try
      FInternalDataset.DeleteIndex(FInternalDataset.IndexName);
      FInternalDataset.IndexName := '';
    except
    end;
end;

procedure TcyBaseDBClientLookupComboBox.DropDown;
var
  SavKeyValue: Variant;
begin
  SavKeyValue := KeyValue;

  if FDropDownRefreshList then
    if FInternalDataset.Active then
      FInternalDataset.Active := false;    // !!! 2106-05-05 ... Key value set to null here if no property DataField and no property Datasource specified !!!

  if not FInternalDataset.Active then
    UpdateInternalDataset;

  Inherited ListSource := FInternalListSource;

  if KeyValue <> savKeyValue then          // !!! 2106-05-05 ... Reassign Key value if no property DataField and no property Datasource specified !!!
    KeyValue := savKeyValue;

  inherited;
end;

procedure TcyBaseDBClientLookupComboBox.CloseUp(Accept: Boolean);
begin
  inherited CloseUp(True);

  Inherited ListSource := FcyListLink.DataSource;  // Because of custom filter ...
end;

procedure TcyBaseDBClientLookupComboBox.UpdateInternalDataset;
var
  f: Integer;
  WithDataset: TDataset;
  // savKeyValue: Variant;
begin
  if not Assigned(cyListLink.DataSource) then Exit;

  if FInternalDataset.Active then
    FInternalDataset.Active := false;

  WithDataset := cyListLink.DataSource.DataSet;   // -> BUG : Remove any Range on BDE dataset !

  if Assigned(FOnUpdateInternalDataset) then
    FOnUpdateInternalDataset(Self, WithDataset);

  FDatasetProvider.DataSet := WithDataset;

  if Assigned(FDatasetProvider.DataSet) then
    if FDatasetProvider.DataSet.Active then
    begin
      // 2016-05-05 ... SavKeyValue := KeyValue;

      FUpdatingInternalDataset := true;
      // FDatasetProvider.DataSet.DisableControls;

      try
        if not FDatasetProvider.DataSet.IsUniDirectional then
        begin
          if not FDatasetProvider.DataSet.Bof then
            FDatasetProvider.DataSet.First;
        end;

        {$IFDEF UNICODE}
        // FInternalDataset.PacketRecords := -1; // Import all records -> Bad char importing WideString field values in DELPHI 2009 !!!
        FInternalDataset.PacketRecords :=  0; // Do not import records ...
        FInternalDataset.Active := true;

        // 2016-04-26 Avoid error when ReadOnly set on imported dataset :
        for f := 0 to FInternalDataset.FieldCount-1 do
        begin
          FInternalDataset.Fields[f].ReadOnly := false;
          FInternalDataset.Fields[f].Required := false;    // 2017-07-28 ...
        end;

        FInternalDataset.DisableControls;

        while not FDatasetProvider.DataSet.Eof do
        begin
          FInternalDataset.Append;
          FInternalDataset.CopyFields(FDatasetProvider.DataSet);
          // Also works ... cyDb.CloneFieldValues(FDatasetProvider.DataSet, FInternalDataset);
          FInternalDataset.Post;

          FDatasetProvider.DataSet.Next;
        end;

        FInternalDataset.First; // 2017-05-09 ...
        FInternalDataset.EnableControls;
        {$ELSE}
        FInternalDataset.PacketRecords := -1;
        FInternalDataset.Active := true;
        {$ENDIF}
      finally
        // FDatasetProvider.DataSet.EnableControls;
        FUpdatingInternalDataset := false;
      end;

      (*// 2016-05-05 ... if KeyValue <> savKeyValue then
      begin
        KeyValue := savKeyValue;
        ShowMessage('Cindy Component debug: TcyBaseDBClientLookupComboBox May not enter here !');
      end;*)
    end;
end;

procedure TcyBaseDBClientLookupComboBox.SetListFilterOptions(const Value: TFilterOptions);
begin
  FInternalDataset.FilterOptions := Value;
end;

procedure TcyBaseDBClientLookupComboBox.SetFInternalFilter(const Value: String);
begin
  FInternalFilter := Value;
  UpdateInternalDatasetFilter;
end;

procedure TcyBaseDBClientLookupComboBox.SetListFilter(const Value: String);
begin
  FListFilter := value;
  UpdateInternalDatasetFilter;
end;

procedure TcyBaseDBClientLookupComboBox.UpdateInternalDatasetFilter;
var
  Filter: String;
begin
  // Apply filters :
  Filter := Self.ListFilter;

  if FInternalFilter <> '' then
  begin
    if Filter <> '' then
      Filter := '(' + Filter + ') AND ';
    Filter := Filter + FInternalFilter;
  end;

  FInternalDataset.Filtered := false;
  FInternalDataset.Filter   := Filter;

  try
    FInternalDataset.Filtered := FInternalDataset.Filter <> '';
  except
  end;
end;

procedure TcyBaseDBClientLookupComboBox.SetListOrderFieldNames(const Value: String);
begin
  FInternalDataset.IndexFieldNames := Value;
end;

procedure TcyBaseDBClientLookupComboBox.SetListSource(const Value: TDataSource);
begin
  if FInternalDataset.Active then
    FInternalDataset.Active := false;

  Inherited ListSource := Value;
  FcyListLink.DataSource := Value;

  if Value <> nil then Value.FreeNotification(Self);
end;

function TcyBaseDBClientLookupComboBox.GetDropDownControlDefs: TcyFlyingContainer;
begin
  Result := FFlyingContainer;
end;

function TcyBaseDBClientLookupComboBox.GetListFilterOptions: TFilterOptions;
begin
  Result := FInternalDataset.FilterOptions;
end;

function TcyBaseDBClientLookupComboBox.GetInternalDataSet: TClientDataset;
begin
  Result := FInternalDataset;
end;

function TcyBaseDBClientLookupComboBox.GetInternalListSource: TDataSource;
begin
  Result := FInternalListSource;
end;

function TcyBaseDBClientLookupComboBox.GetListFilter: String;
begin
  Result := FInternalDataset.Filter;
end;

function TcyBaseDBClientLookupComboBox.GetListOrderFieldNames: String;
begin
  Result := FInternalDataset.IndexFieldNames;
end;

function TcyBaseDBClientLookupComboBox.GetListSource: TDataSource;
begin
  Result := FcyListLink.DataSource;
end;

procedure TcyBaseDBClientLookupComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

// Scrolling/adding/deleting or post record on new ListSource property ...
procedure TcyBaseDBClientLookupComboBox.cyListLinkDataChanged;
begin
  //
end;

// Editing/canceling record
procedure TcyBaseDBClientLookupComboBox.cyListLinkEditingChanged;
begin
  // Can' t close because raise error trying changing field after Edit ...
  // if FInternalDataset.Active then
  //  FInternalDataset.Active := false;
end;

// Assigning new ListSource property ...
procedure TcyBaseDBClientLookupComboBox.cyUpdateListFields;
begin
  if FInternalDataset.Active then
    FInternalDataset.Active := false;

//  if not Self.Focused then
//  begin
    if Inherited ListSource <> FcyListLink.DataSource then
      Inherited ListSource := FcyListLink.DataSource;
//  end
//  else begin
//    UpdateInternalDataset;
//    Inherited ListSource := FInternalListSource;
//  end;
end;


procedure TcyBaseDBClientLookupComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
    if ListSource <> Nil then
      if AComponent = ListSource then ListSource := nil;
end;

function TcyBaseDBClientLookupComboBox.GetInternalDatasetCurrentKeyValue: Variant;
begin
  Result := Null;

  if FInternalDataset.Active then
    if not (FInternalDataset.Bof and FInternalDataset.Eof) then
      if FInternalDataset.FindField(KeyField) <> Nil then
        Result := FInternalDataset.FindField(KeyField).Value;
end;

procedure TcyBaseDBClientLookupComboBox.SelectCurrentKeyValue(const CloseDropDown: Boolean = true);
var
  ListValue: Variant;
begin
  if CloseDropDown then
    if GetDropDownControlDefs.Active then
      GetDropDownControlDefs.Close;

  ListValue := GetInternalDatasetCurrentKeyValue;
  SelectKeyValue(ListValue);
end;

end.
