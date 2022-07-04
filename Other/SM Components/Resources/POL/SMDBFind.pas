{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  The TSMDBFind component allow to display a visual dialog for
  record search in any dataset or activate this search even
  without visual dialog (just as extended Locate method of dataset).
  You can use this component in any form where you need a search possibility.
  For example, together with TSMDBNavigator component
  (see a FIND button).

  You can use the next properties:
   - Caption: the desired caption of dialog
   - ShowDialog: activate/deactivate a visual dialog with search attributes
   - CaptionsFields: list of field names which will see a user
     (if field is not included in this list, then field will be hidden from user)
   - FindOption: search settings
   - FindByField: field name for search.
     (if FindByField is empty, then search will be for EACH field)
   - FindMode: at a beginning of a field/exactly coincidence/any position
   - FindScore: search from first record or current record
   - FindValue: value which we want to find
   - SayResult: to show a message after search (success/not found/error etc) or not
}

unit SMDBFind;

interface

{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER100}
      {$DEFINE SMC_D4} { Delphi 4.0 or higher }
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DB;

type
  TSMFindResult = (frFound, frNotFound, frError);
  TSMFindMode = (fmBeginPart, fmRegularExpression, fmAnyPart);
  TSMFindScore = (fsFromFirst, fsFromCurrent);

  TSMDBFindDialog = class(TComponent)
  private
    { Private declarations }
    FCaption: TCaption;
    FShowDialog: Boolean;
    FDataSource: TDataSource;
    FCaptionsFields: TStrings;
    FFieldNames: TStrings;

    FFindOption: TLocateOptions;
    FFindByField: string;
    FFindMode: TSMFindMode;
    FFindScore: TSMFindScore;
    FFindValue: string;
    FSayResult: Boolean;
    FPosition: TPosition;

    FOnExecuteStart: TNotifyEvent;
    FOnExecuteFinish: TNotifyEvent;
    FOnShow: TNotifyEvent;

    procedure SetFieldNames(Value: TStrings);
    procedure SetCaptionsFields(Value: TStrings);
    function CompareStrings(S1, S2: string; CaseSensitive: Boolean; intMode: Integer): Boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: TSMFindResult;
  published
    { Published declarations }
    property Caption: TCaption read FCaption write FCaption;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
    property DataSource: TDataSource read FDataSource write FDataSource;

    property FieldNames: TStrings read FFieldNames write SetFieldNames;
    property CaptionsFields: TStrings read FCaptionsFields write SetCaptionsFields;

    property FindOption: TLocateOptions read FFindOption write FFindOption;
    property FindByField: string read FFindByField write FFindByField;
    property FindMode: TSMFindMode read FFindMode write FFindMode;
    property FindScore: TSMFindScore read FFindScore write FFindScore;
    property FindValue: string read FFindValue write FFindValue;
    property SayResult: Boolean read FSayResult write FSayResult;
    property Position: TPosition read FPosition write FPosition default poScreenCenter;

    property OnExecuteStart: TNotifyEvent read FOnExecuteStart write FOnExecuteStart;
    property OnExecuteFinish: TNotifyEvent read FOnExecuteFinish write FOnExecuteFinish;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TfrmFind = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    gbOptions: TGroupBox;
    cbCaseSensitive: TCheckBox;
    rgPosition: TRadioGroup;
    lblValue: TLabel;
    edValue: TEdit;
    bvlButton: TBevel;
    gbFields: TGroupBox;
    cbFields: TComboBox;
    rbOneField: TRadioButton;
    rbAllFields: TRadioButton;
    cbMode: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cbFieldsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure Register;

implementation
{$R *.DFM}
{$R *.DCR}

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBFindDialog]);
end;


{ TSMDBFindDialog }
constructor TSMDBFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if FCaption = EmptyStr then
  	FCaption := 'Znajdü rekord';
  FDataSource := nil;
  FCaptionsFields := TStringList.Create;
  FFieldNames := TStringList.Create;
  FShowDialog := True;
  FPosition := poScreenCenter;
end;

destructor TSMDBFindDialog.Destroy;
begin
  FCaptionsFields.Free;
  FFieldNames.Free;

  inherited Destroy;
end;

procedure TSMDBFindDialog.SetFieldNames(Value: TStrings);
begin
  FFieldNames.Assign(Value);
end;

procedure TSMDBFindDialog.SetCaptionsFields(Value: TStrings);
begin
  FCaptionsFields.Assign(Value);
end;

function TSMDBFindDialog.Execute: TSMFindResult;
var
  i: Integer;
  boolFound: Boolean;
  strField, strFieldCaption, strFieldName: string;
  frmFind: TfrmFind;
begin
  if Assigned(FOnExecuteStart) then
    FOnExecuteStart(Self);

  Result := frError;

  if FShowDialog then
  begin
    frmFind := TfrmFind.Create(Application);
    with frmFind do
      try
        Position := Self.Position;

        {fill a field list}
        if (FFieldNames.Count = 0) then
          for i := 0 to DataSource.DataSet.FieldCount - 1 do
            if (DataSource.DataSet.Fields[i].DataType in
                [ftString, ftSmallint, ftInteger, ftWord, ftBoolean,
                 ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
                 ftBytes, ftVarBytes, ftAutoInc, ftMemo, ftFmtMemo
                 {$IFDEF SMC_D4}
                 , ftWideString, ftLargeint
                 {$ENDIF}
                 ]) then
             FFieldNames.Add(DataSource.DataSet.Fields[i].FieldName);

        strField := '';
        for i := 0 to FFieldNames.Count - 1 do
        begin
          strFieldName := FFieldNames[i];//.DisplayName;
          strFieldCaption := FCaptionsFields.Values[strFieldName];

          { user has not listed any Captionfields
            OR
            user has listed at least 1 captionfield
            and we must check if current field is required for list
          }
          if (FCaptionsFields.Count = 0) or
             ((FCaptionsFields.Count <> 0) and
              (FCaptionsFields.IndexOfName(strFieldName) > -1)) then
          begin
            if (strFieldCaption = '') then
              strFieldCaption := strFieldName;

            cbFields.Items.AddObject(strFieldCaption, TObject(LongInt(NewStr(FFieldNames[i]))));
            if (FFindByField = FFieldNames[i]) then
              strField := strFieldCaption;
          end;
        end;

        if (strField = '') then
          cbFields.ItemIndex := 0
        else
          cbFields.ItemIndex := cbFields.Items.IndexOf(strField);

        rbAllFields.Checked := (FFindByField = '');
        rbOneField.Checked := not rbAllFields.Checked;
        cbMode.ItemIndex := Ord(FFindMode);
        rgPosition.ItemIndex := Ord(FFindScore);

        cbCaseSensitive.Checked := not (loCaseInsensitive in FindOption);
        if (loPartialKey in FindOption) then
          cbMode.ItemIndex := 0;
//        else
//          cbMode.ItemIndex := 1;

        edValue.Text := FFindValue;
        Caption := FCaption;

        if Assigned(FOnShow) then
          FOnShow(Self);

        if (ShowModal = mrOk) then
        begin
          FindOption := [];
          if cbCaseSensitive.Checked then
            FindOption := FindOption - [loCaseInsensitive]
          else
            FindOption := FindOption + [loCaseInsensitive];

          case cbMode.ItemIndex of
            0: FindOption := FindOption + [loPartialKey];
            1: FindOption := FindOption - [loPartialKey];
          end;

          FFindMode := TSMFindMode(cbMode.ItemIndex);
          FFindScore := TSMFindScore(rgPosition.ItemIndex);

          if rbAllFields.Checked then
            FFindByField := ''
          else
            FFindByField := PString(cbFields.Items.Objects[cbFields.ItemIndex])^;

          FFindValue := edValue.Text;
        end
        else
          Exit;
      finally
        for i := 0 to cbFields.Items.Count-1 do
          DisposeStr(PString(cbFields.Items.Objects[i]));
        Free
      end;
  end;

  {search process}

  {if search is by one field from at beginning of a string or not any position, then LOCATE}
   boolFound := False;
   with DataSource.DataSet do
   begin
     if (FFindByField <> '') and
        (FFindScore = fsFromFirst) and
        (FFindMode <> fmAnyPart) then
       boolFound := Locate(FFindByField, FFindValue, FindOption)
     else
     begin
       DisableControls;

       if (FindScore = fsFromFirst) then
         First
       else
         Next;

       while not EOF and (not boolFound) do
       begin
         if (FFindByField <> '') then
           boolFound := CompareStrings(FindValue,
                          FieldByName(FindByField).AsString,
                          (loCaseInsensitive in FindOption),
                          Ord(FindMode))
         else
         begin
           i := 0;
           while (i < FieldCount) and (not boolFound) do
           begin
             boolFound := CompareStrings(FindValue,
                           Fields[i].AsString,
                           (loCaseInsensitive in FindOption),
                           Ord(FindMode));
             Inc(i);
           end;
         end;

         if not boolFound then
           Next;
       end;

       EnableControls;
     end;

     if boolFound then
       Result := frFound
     else
       Result := frNotFound;
   end;
   if FSayResult then
     case Result of
//       frFound: MessageDlg('WartoúÊ znaleziona!', mtInformation, [mbOk], 0);
       frNotFound: MessageDlg('Podana wartoúÊ nie zosta≥a znaleziona!', mtInformation, [mbOk], 0);
       frError: MessageDlg('B≥πd podczas wyszukiwania!', mtError, [mbOk], 0);
     end;

  if Assigned(FOnExecuteFinish) then
    FOnExecuteFinish(Self);
end;

function TSMDBFindDialog.CompareStrings(S1, S2: string; CaseSensitive: Boolean; intMode: Integer): Boolean;
begin
  Result := False;
  try
    if CaseSensitive then
       case intMode of
         0: Result := (AnsiCompareText(S1, Copy(S2, 1, Length(S1))) = 0);
         1: Result := (AnsiCompareText(S1, S2) = 0);
         2: Result := (AnsiPos(AnsiUpperCase(S1), AnsiUpperCase(S2)) > 0);
       end
    else
       case intMode of
         0: Result := (AnsiCompareStr(S1, Copy(S2, 1, Length(S1))) = 0);
         1: Result := (AnsiCompareStr(S1, S2) = 0);
         2: Result := (AnsiPos(S1, S2) > 0);
       end;
  except
  end;
end;

{ TfrmFind }
procedure TfrmFind.FormCreate(Sender: TObject);
begin
  cbFields.ItemIndex := 0;
  cbMode.ItemIndex := 0;
end;

procedure TfrmFind.cbFieldsClick(Sender: TObject);
begin
  rbOneField.Checked := True
end;

end.
