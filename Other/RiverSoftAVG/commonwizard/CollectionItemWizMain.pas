unit CollectionItemWizMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList, CollectionComponent,
  ComCtrls, Grids;

const
  NewLineString = #13#10;

  cGETFIELDDECLARATION = '##GETFIELDDECLARATION##';
  cSETFIELDDECLARATION = '##SETFIELDDECLARATION##';
  cFIELDDECLARATION = '##FIELDDECLARATION##';
  cPROPERTYDECLARATION = '##PROPERTYDECLARATION##';
  cPROPERTYFIELDNAME = '##PROPERTYFIELDNAME##';
  cPROPERTYNAME = '##PROPERTYNAME##';
  cPROPERTYTYPE = '##PROPERTYTYPE##';
  cITEMPROPERTY = '##ITEMPROPERTY##';
  cGETITEMPROPERTY = '##GETITEMPROPERTY##';
  cSETITEMPROPERTY = '##SETITEMPROPERTY##';
  cTPROPERTYTYPE = '##TPROPERTYTYPE##';
  cCOLLECTIONPROPERTY = '##COLLECTIONPROPERTY##';
  cTITEMCOLLECTIONDESCENDANT = '##TITEMCOLLECTIONDESCENDANT##';
  cTITEMCOLLECTIONOWNER = '##TITEMCOLLECTIONOWNER##';
  cTITEMNAME = '##TITEMNAME##';
  cTITEMDESCENDANT = '##TITEMDESCENDANT##';
  cTITEMCOLLECTION = '##TITEMCOLLECTION##';
  cPROPERTYASSIGNIMPLEMENTATION = '##PROPERTYASSIGNIMPLEMENTATION##';
  cGETFIELDIMPLEMENTATION = '##GETFIELDIMPLEMENTATION##';
  cSETFIELDIMPLEMENTATION = '##SETFIELDIMPLEMENTATION##';
  cFIELDIMPLEMENTATION = '##FIELDIMPLEMENTATION##';
  cPROPERTYIMPLEMENTATION = '##PROPERTYIMPLEMENTATION##';
  cGETDISPLAYNAMETYPE = '##GETDISPLAYNAMETYPE##';
  cGETDISPLAYNAMEIMPL = '##GETDISPLAYNAMEIMPL##';
  cASSIGNDECLARATION = '##ASSIGNDECLARATION##';
  cASSIGNIMPLEMENTATION = '##ASSIGNIMPLEMENTATION##';
  cSAFECONSTRUCTORTYPE = '##SAFECONSTRUCTORTYPE##';
  cSAFECONSTRUCTORIMPL = '##SAFECONSTRUCTORIMPL##';
  cCOLLECTIONOWNERREFTYPE = '##COLLECTIONOWNERREFTYPE##';
  cCOLLECTIONOWNERREFIMPL = '##COLLECTIONOWNERREFIMPL##';
  cGETSETCOLLECTIONREFTYPE = '##GETSETCOLLECTIONREFTYPE##';
  cGETSETCOLLECTIONREFIMPL = '##GETSETCOLLECTIONREFIMPL##';
  cHASREADACCESSOR = '##READACCESSORKEYWORD##';
  cHASWRITEACCESSOR = '##WRITEACCESSORKEYWORD##';
  cCOLLECTIONFORWARDDECL = '##COLLECTIONFORWARDDECL##';

  AssignDeclStr = '    procedure Assign(Source: TPersistent); override;'+NewLineString;
  GetDisplayNameTypeStr = '    function GetDisplayName: String; override;'+NewLineString;
  GetSetCollectionRefTypeStr =
  '    function GetCollection: '+cTITEMCOLLECTION+';'+NewLineString+
  '    procedure SetCollection(const Value: '+cTITEMCOLLECTION+'); reintroduce;'+NewLineString;
  CollectionPropStr =
  '    property Collection: '+cTITEMCOLLECTION+' read GetCollection write SetCollection;'+NewLineString;

  ForwardDeclStr =
  '  '+cTITEMCOLLECTION+' = class;'+NewLineString;

  FieldDeclStr = '    '+cPROPERTYFIELDNAME+': '+cTPROPERTYTYPE+';'+NewLineString;
  GetFieldDeclStr = '    function '+cGETITEMPROPERTY+': '+cTPROPERTYTYPE+';'+NewLineString;
  SetFieldDeclStr = '    procedure '+cSETITEMPROPERTY+'(const Value: '+cTPROPERTYTYPE+');'+NewLineString;
  PropertyDeclStr = '    property '+cITEMPROPERTY+': '+cTPROPERTYTYPE+cHASREADACCESSOR+cGETITEMPROPERTY+cHASWRITEACCESSOR+cSETITEMPROPERTY+';'+NewLineString;
  ItemTypeStr =
cCOLLECTIONFORWARDDECL+
'  '+cTITEMNAME+' = class('+cTITEMDESCENDANT+')'+NewLineString+
'  { Purpose: }'+NewLineString+
'  private'+NewLineString+
'    { Private declarations }'+NewLineString+
cFIELDDECLARATION+
cGETSETCOLLECTIONREFTYPE+
cGETFIELDDECLARATION+
cSETFIELDDECLARATION+
'  protected'+NewLineString+
'    { Protected declarations }'+NewLineString+
cGETDISPLAYNAMETYPE+
'  public'+NewLineString+
'    { Public declarations }'+NewLineString+
cASSIGNDECLARATION+
cCOLLECTIONPROPERTY+
'  published'+NewLineString+
'    { Published declarations }'+NewLineString+
cPROPERTYDECLARATION+
'  end; { '+cTITEMNAME +' }'+NewLineString+
NewLineString;

  SafeConstructorTypeStr = '    constructor Create(AOwner: '+cTITEMCOLLECTIONOWNER+');'+NewLineString;
  CollectionOwnerRefTypeStr = '    function Owner: '+cTITEMCOLLECTIONOWNER+'; reintroduce;'+NewLineString;

  CollectionTypeStr =
'  '+cTITEMCOLLECTION+' = class('+cTITEMCOLLECTIONDESCENDANT+')'+NewLineString+
'  { Purpose: }'+NewLineString+
'  private'+NewLineString+
'    { Private declarations }'+NewLineString+
'    function GetItem(Index: Integer): '+cTITEMNAME+';'+NewLineString+
'    procedure SetItem(Index: Integer; const Value: '+cTITEMNAME+');'+NewLineString+
'  protected'+NewLineString+
'    { Protected declarations }'+NewLineString+
'  public'+NewLineString+
'    { Public declarations }'+NewLineString+
cSAFECONSTRUCTORTYPE+
'    function Add: '+cTITEMNAME+';'+NewLineString+
'    function FindItemID(ID: Integer): '+cTITEMNAME+';'+NewLineString+
'    function Insert(Index: Integer): '+cTITEMNAME+';'+NewLineString+
'    property Items[Index: Integer]: '+cTITEMNAME+' read GetItem write SetItem; default;'+NewLineString+
cCOLLECTIONOWNERREFTYPE+
'  published'+NewLineString+
'    { Published declarations }'+NewLineString+
'  end; { '+cTITEMCOLLECTION+' }'+NewLineString+
NewLineString;

  ProperyAssignImplStr = '          F'+cITEMPROPERTY+' := '+cTITEMNAME+'(Source).'+cITEMPROPERTY+';'+NewLineString;
  AssignImplStr =
'procedure '+cTITEMNAME+'.Assign(Source: TPersistent);'+NewLineString+
'begin'+NewLineString+
'     if Source is '+cTITEMNAME+' then'+NewLineString+
'     begin'+NewLineString+
'          // Copy properties here'+NewLineString+
cPROPERTYASSIGNIMPLEMENTATION+
'          Changed(False);'+NewLineString+
'     end'+NewLineString+
'     else'+NewLineString+
'         inherited Assign(Source);'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  GetDisplayNameImplStr =
'function '+cTITEMNAME+'.GetDisplayName: String;'+NewLineString+
'begin'+NewLineString+
'     result := '''';  {TODO: Update code for Display Name based on your properties}'+NewLineString+
'     if result = '''' then'+NewLineString+
'        result := inherited GetDisplayName;'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  GetSetCollectionRefImplStr =
'function '+cTITEMNAME+'.GetCollection: '+cTITEMCOLLECTION+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cTITEMCOLLECTION+'(inherited Collection);'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
'procedure '+cTITEMNAME+'.SetCollection(const Value: '+cTITEMCOLLECTION+');'+NewLineString+
'begin'+NewLineString+
'     inherited Collection := Value;'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  GetFieldImplStr =
'function '+cTITEMNAME+'.'+cGETITEMPROPERTY+': '+cTPROPERTYTYPE+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cPROPERTYFIELDNAME+';'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  SetFieldImplStr =
'procedure '+cTITEMNAME+'.'+cSETITEMPROPERTY+'(const Value: '+cTPROPERTYTYPE+');'+NewLineString+
'begin'+NewLineString+
'     if Value <> '+cITEMPROPERTY+' then'+NewLineString+
'     begin'+NewLineString+
'          '+cPROPERTYFIELDNAME+' := Value;'+NewLineString+
'          Changed(False);'+NewLineString+
'     end;'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  ItemImplStr =
'{ '+cTITEMNAME+' }'+NewLineString+
''+NewLineString+
cASSIGNIMPLEMENTATION+
cGETSETCOLLECTIONREFIMPL+
cGETDISPLAYNAMEIMPL+
cGETFIELDIMPLEMENTATION+
cSETFIELDIMPLEMENTATION+
NewLineString;

  SafeConstructorImplStr =
'constructor '+cTITEMCOLLECTION+'.Create(AOwner: '+cTITEMCOLLECTIONOWNER+');'+NewLineString+
'begin'+NewLineString+
'     inherited Create(AOwner, '+cTITEMNAME+');'+NewLineString+
'end;';

  CollectionOwnerRefImplStr =
'function '+cTITEMCOLLECTION+'.Owner: '+cTITEMCOLLECTIONOWNER+';'+NewLineString+
'var'+NewLineString+
'   AOwner: TPersistent;'+NewLineString+
'begin'+NewLineString+
'     AOwner := inherited Owner;'+NewLineString+
'     if AOwner is '+cTITEMCOLLECTIONOWNER+' then'+NewLineString+
'        result := '+cTITEMCOLLECTIONOWNER+'(AOwner)'+NewLineString+
'     else'+NewLineString+
'         result := nil;'+NewLineString+
'end;'+NewLineString+
''+NewLineString;

  CollectionImplStr =
'{ '+cTITEMCOLLECTION+' }'+NewLineString+
''+NewLineString+
'function '+cTITEMCOLLECTION+'.Add: '+cTITEMNAME+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cTITEMNAME+'(inherited Add);'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
'function '+cTITEMCOLLECTION+'.FindItemID(ID: Integer): '+cTITEMNAME+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cTITEMNAME+'(inherited FindItemID(ID));'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
'function '+cTITEMCOLLECTION+'.GetItem(Index: Integer): '+cTITEMNAME+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cTITEMNAME+'(inherited Items[Index]);'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
cCOLLECTIONOWNERREFIMPL+
'function '+cTITEMCOLLECTION+'.Insert(Index: Integer): '+cTITEMNAME+';'+NewLineString+
'begin'+NewLineString+
'     result := '+cTITEMNAME+'(inherited Insert(Index));'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
'procedure '+cTITEMCOLLECTION+'.SetItem(Index: Integer; const Value: '+cTITEMNAME+');'+NewLineString+
'begin'+NewLineString+
'     inherited Items[Index] := Value;'+NewLineString+
'end;'+NewLineString+
''+NewLineString+
cSAFECONSTRUCTORIMPL+NewLineString;

type
  TWizOption = (woCreateItem, woCreateCollection, woItemHasRef, woCollectionHasRef,
               woSafeConstructor, woAssignMethod, woGetDisplayName);
  TWizOptions = set of TWizOption;
  TfrmNewCollection = class(TForm)
    Panel1: TPanel;
    lblOwner: TLabel;
    cbOwner: TComboBox;
    eCollectionItemName: TEdit;
    lblCollectionDesc: TLabel;
    lblCollectionItemName: TLabel;
    cbCollectionDesc: TComboBox;
    bbOk: TBitBtn;
    bbCancel: TBitBtn;
    lblCollectionName: TLabel;
    eCollectionName: TEdit;
    ActionList1: TActionList;
    actCreateItem: TAction;
    actCreateCollection: TAction;
    actItemHasRef: TAction;
    actCollectionHasRef: TAction;
    actSafeConstructor: TAction;
    actAssignMethod: TAction;
    pcOptions: TPageControl;
    tsOptions: TTabSheet;
    cbCreateCollectionItem: TCheckBox;
    cbCreateCollection: TCheckBox;
    cbCollectionHasRef: TCheckBox;
    cbItemHasRef: TCheckBox;
    cbSafeConstructor: TCheckBox;
    cbAddAssign: TCheckBox;
    tsItemProperties: TTabSheet;
    tsPreview: TTabSheet;
    mmPreview: TMemo;
    sgItemProps: TStringGrid;
    cbGetDisplayName: TCheckBox;
    actGetDisplayName: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actCollectionHasRefExecute(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure sgItemPropsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure sgItemPropsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pcOptionsChange(Sender: TObject);
    procedure eCollectionItemNameChange(Sender: TObject);
    procedure sgItemPropsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure sgItemPropsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
  private
    { Private declarations }
    FPropName: String;
    FOptions: TWizOptions;
    FImplementationStr: String;
    FInterfaceStr: String;
    procedure SetOptions(Value: TWizOptions);
    function GetImplementationStr: String;
    function GetInterfaceStr: String;
    property PropName: String read FPropName write FPropName;
  public
    { Public declarations }
    function ToString: String;
    property ImplementationStr: String read GetImplementationStr;
    property InterfaceStr: String read GetInterfaceStr;
    property Options: TWizOptions read FOptions write SetOptions;
  end;

var
  frmNewCollection: TfrmNewCollection;

implementation

{$R *.dfm}

procedure TfrmNewCollection.FormCreate(Sender: TObject);
   procedure AddNames( Comp: TComponent );
   var
      i: Integer;
      ClassRef: TClass;
   begin
        ClassRef := Comp.ClassType;
        while ClassRef <> nil do
        begin
             if cbOwner.Items.IndexOf( ClassRef.ClassName ) = -1 then
                cbOwner.Items.Add( ClassRef.ClassName );
             ClassRef := ClassRef.ClassParent;
             if ClassRef = TPersistent then
                ClassRef := nil;
        end;
        for i := 0 to Comp.ComponentCount - 1 do
            AddNames( Comp.Components[i] );
   end;
var
   i: Integer;
begin
     FOptions := [Low(TWizOption)..High(TWizOption)];
     for i := 0 to Application.ComponentCount - 1 do
         AddNames( Application.Components[i] );
     sgItemProps.Rows[0].Text := 'Property Name'+NewLineString+'Field Name'+
                                 NewLineString+'Type'+
                                 NewLineString+'Read Accessor'+
                                 NewLineString+'Write Accessor';
     pcOptions.ActivePageIndex := 0;
end;

procedure TfrmNewCollection.SetOptions(Value: TWizOptions);
begin
     FOptions := Value;
     actCreateItem.Checked := woCreateItem in Options;
     actCreateCollection.Checked := woCreateCollection in Options;
     actItemHasRef.Checked := woItemHasRef in Options;
     actCollectionHasRef.Checked := woCollectionHasRef in Options;
     actSafeConstructor.Checked := woSafeConstructor in Options;
     actAssignMethod.Checked := woAssignMethod in Options;
     actItemHasRef.Enabled := woCreateItem in Options;
     actAssignMethod.Enabled := woCreateItem in Options;
     actGetDisplayName.Enabled := woCreateItem in Options;
     actCollectionHasRef.Enabled := woCreateCollection in Options;
     actSafeConstructor.Enabled := woCreateCollection in Options;
end;

procedure TfrmNewCollection.actCollectionHasRefExecute(Sender: TObject);
begin
     with Sender as TAction do
          if Checked then
             Options := Options + [TWizOption(Tag)]
          else
             Options := Options - [TWizOption(Tag)];
end;


function TfrmNewCollection.ToString: String;
   function ReplacePropValues(InitialStr, FieldName, PropType, Prop, GetProp, SetProp: String): String;
   begin
        result := InitialStr;
        result := StringReplace( result, cPROPERTYFIELDNAME, FieldName, [rfReplaceAll] );
        result := StringReplace( result, cTPROPERTYTYPE, PropType, [rfReplaceAll] );
        result := StringReplace( result, cITEMPROPERTY, Prop, [rfReplaceAll] );
        result := StringReplace( result, cGETITEMPROPERTY, GetProp, [rfReplaceAll] );
        result := StringReplace( result, cSETITEMPROPERTY, SetProp, [rfReplaceAll] );
   end;
var
   TypeStr: String;
   ImplStr: String;
   i: Integer;
   FieldStr, GetFieldStr, SetFieldStr, PropStr, AssignStr: String;
   Temp: String;
   GetFieldStrImpl, SetFieldStrImpl: String;
begin
     result := '';
     TypeStr := '';
     ImplStr := '';
     if woCreateItem in Options then
     begin
          TypeStr := TypeStr + ItemTypeStr;
          ImplStr := ImplStr + ItemImplStr;
          if woAssignMethod in Options then
          begin
               TypeStr := StringReplace( TypeStr, cASSIGNDECLARATION, AssignDeclStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cASSIGNIMPLEMENTATION, AssignImplStr, [rfReplaceAll] );
          end
          else
          begin
               TypeStr := StringReplace( TypeStr, cASSIGNDECLARATION, '', [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cASSIGNIMPLEMENTATION, '', [rfReplaceAll] );
          end;
          if woItemHasRef in Options then
          begin
               TypeStr := StringReplace( TypeStr, cGETSETCOLLECTIONREFTYPE, GetSetCollectionRefTypeStr, [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cCOLLECTIONPROPERTY, CollectionPropStr, [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cCOLLECTIONFORWARDDECL, ForwardDeclStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cGETSETCOLLECTIONREFIMPL, GetSetCollectionRefImplStr, [rfReplaceAll] );
          end
          else
          begin
               TypeStr := StringReplace( TypeStr, cGETSETCOLLECTIONREFTYPE, '', [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cCOLLECTIONPROPERTY, '', [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cCOLLECTIONFORWARDDECL, '', [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cGETSETCOLLECTIONREFIMPL, '', [rfReplaceAll] );
          end;
          if woGetDisplayName in Options then
          begin
               TypeStr := StringReplace( TypeStr, cGETDISPLAYNAMETYPE, GetDisplayNameTypeStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cGETDISPLAYNAMEIMPL, GetDisplayNameImplStr, [rfReplaceAll] );
          end
          else
          begin
               TypeStr := StringReplace( TypeStr, cGETDISPLAYNAMETYPE, '', [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cGETDISPLAYNAMEIMPL, '', [rfReplaceAll] );
          end;
          // Now the properties
          FieldStr := '';
          GetFieldStr := '';
          SetFieldStr := '';
          PropStr := '';
          AssignStr := '';
          GetFieldStrImpl := '';
          SetFieldStrImpl := '';
          with sgItemProps do
          begin
               for i := 1 to RowCount - 1 do
               begin
                    if (Cells[0,i] <> '') and
                       (Cells[2,i] <> '') then
                    begin
                         if (Cells[1,i] <> '') then
                         begin
                            FieldStr := FieldStr +
                                     ReplacePropValues( FieldDeclStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                            AssignStr := AssignStr +
                                     ReplacePropValues( ProperyAssignImplStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                         end;
                         if (Cells[3,i] <> '') and
                            (Cells[3,i] <> Cells[1,i]) then
                         begin
                            GetFieldStr := GetFieldStr +
                                     ReplacePropValues( GetFieldDeclStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                            GetFieldStrImpl := GetFieldStrImpl +
                                     ReplacePropValues( GetFieldImplStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                         end;
                         if (Cells[4,i] <> '') and
                            (Cells[4,i] <> Cells[1,i]) then
                         begin
                            SetFieldStr := SetFieldStr +
                                     ReplacePropValues( SetFieldDeclStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                            SetFieldStrImpl := SetFieldStrImpl +
                                     ReplacePropValues( SetFieldImplStr,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                         end;
                         Temp := PropertyDeclStr;
                         if Cells[3,i] <> '' then
                            Temp := StringReplace( Temp, cHASREADACCESSOR, ' read ', [rfReplaceAll] )
                         else
                            Temp := StringReplace( Temp, cHASREADACCESSOR, '', [rfReplaceAll] );
                         if Cells[4,i] <> '' then
                            Temp := StringReplace( Temp, cHASWRITEACCESSOR, ' write ', [rfReplaceAll] )
                         else
                            Temp := StringReplace( Temp, cHASWRITEACCESSOR, '', [rfReplaceAll] );
                         PropStr := PropStr +
                                     ReplacePropValues( Temp,
                                                        Cells[1,i], Cells[2,i],
                                                        Cells[0,i], Cells[3,i],
                                                        Cells[4,i] );
                    end;
               end;
               TypeStr := StringReplace( TypeStr, cFIELDDECLARATION, FieldStr, [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cGETFIELDDECLARATION, GetFieldStr, [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cSETFIELDDECLARATION, SetFieldStr, [rfReplaceAll] );
               TypeStr := StringReplace( TypeStr, cPROPERTYDECLARATION, PropStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cPROPERTYASSIGNIMPLEMENTATION, AssignStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cGETFIELDIMPLEMENTATION, GetFieldStrImpl, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cSETFIELDIMPLEMENTATION, SetFieldStrImpl, [rfReplaceAll] );
          end;
     end;
     if woCreateCollection in Options then
     begin
          TypeStr := TypeStr + CollectionTypeStr;
          ImplStr := ImplStr + CollectionImplStr;
          if woCollectionHasRef in Options then
          begin
               TypeStr := StringReplace( TypeStr, cCOLLECTIONOWNERREFTYPE, CollectionOwnerRefTypeStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cCOLLECTIONOWNERREFIMPL, CollectionOwnerRefImplStr, [rfReplaceAll] );
          end
          else
          begin
               TypeStr := StringReplace( TypeStr, cCOLLECTIONOWNERREFTYPE, '', [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cCOLLECTIONOWNERREFIMPL, '', [rfReplaceAll] );
          end;

          if woSafeConstructor in Options then
          begin
               TypeStr := StringReplace( TypeStr, cSAFECONSTRUCTORTYPE, SafeConstructorTypeStr, [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cSAFECONSTRUCTORIMPL, SafeConstructorImplStr, [rfReplaceAll] );
          end
          else
          begin
               TypeStr := StringReplace( TypeStr, cSAFECONSTRUCTORTYPE, '', [rfReplaceAll] );
               ImplStr := StringReplace( ImplStr, cSAFECONSTRUCTORIMPL, '', [rfReplaceAll] );
          end;

     end;
     TypeStr := StringReplace( TypeStr, cSETFIELDDECLARATION, '', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cFIELDDECLARATION, '', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cPROPERTYDECLARATION, '', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cITEMPROPERTY, '', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTPROPERTYTYPE, '', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTITEMCOLLECTIONDESCENDANT, cbCollectionDesc.Text, [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTITEMNAME, eCollectionItemName.Text, [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTITEMDESCENDANT, 'TCollectionItem', [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTITEMCOLLECTIONOWNER, cbOwner.Text, [rfReplaceAll] );
     TypeStr := StringReplace( TypeStr, cTITEMCOLLECTION, eCollectionName.Text, [rfReplaceAll] );

     ImplStr := StringReplace( ImplStr, cPROPERTYASSIGNIMPLEMENTATION, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cGETFIELDIMPLEMENTATION, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cSETFIELDIMPLEMENTATION, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cFIELDIMPLEMENTATION, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cPROPERTYIMPLEMENTATION, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTPROPERTYTYPE, '', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTITEMCOLLECTIONDESCENDANT, cbCollectionDesc.Text, [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTITEMNAME, eCollectionItemName.Text, [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTITEMDESCENDANT, 'TCollectionItem', [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTITEMCOLLECTIONOWNER, cbOwner.Text, [rfReplaceAll] );
     ImplStr := StringReplace( ImplStr, cTITEMCOLLECTION, eCollectionName.Text, [rfReplaceAll] );
     result := TypeStr + NewLineString + ImplStr;
     FImplementationStr := ImplStr;
     FInterfaceStr := TypeStr;
end;

procedure TfrmNewCollection.bbOkClick(Sender: TObject);
begin
     mmPreview.Lines.Text := ToString;
end;

procedure TfrmNewCollection.sgItemPropsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
     with Sender as TStringGrid do
     begin
         if Cells[0,ARow] = '' then Exit;
         case ACol of
              1: if Cells[ACol,ARow] = '' then
                    Cells[ACol,ARow] := 'F'+Cells[0,ARow];
              2: if (Cells[ACol,ARow] = '') and
                    (ARow > 1) then Cells[ACol,ARow] := Cells[ACol,ARow-1];
              3: if Cells[ACol,ARow] = '' then
                    Cells[ACol,ARow] := Cells[1,ARow];
              4: if Cells[ACol,ARow] = '' then Cells[ACol,ARow] := 'Set'+Cells[0,ARow];
         end;
     end;
end;

procedure TfrmNewCollection.sgItemPropsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
     with Sender as TStringGrid do
     if (Key = vk_Tab) and (Shift = []) and
        (Col = (ColCount-1)) and (Row = (RowCount - 1)) and
        ((Cells[0,Row] <> '') and
         (Cells[2,Row] <> '') and
         ((Cells[1,Row] <> '') or (Cells[3,Row] <> '') or (Cells[4,Row] <> ''))) then
     begin
          RowCount := RowCount + 1;
     end;
end;

procedure TfrmNewCollection.sgItemPropsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
     with Sender as TStringGrid do
     if (ACol = 0) and (Value <> '') then
     begin
          if Cells[1,ARow] = ('F'+PropName) then
             Cells[1,ARow] := 'F'+Value;
          if Cells[3,ARow] = ('F'+PropName) then
             Cells[3,ARow] := 'F'+Value
          else if Cells[3,ARow] = ('Is'+PropName) then
             Cells[3,ARow] := 'Is'+Value
          else if Cells[3,ARow] = ('Get'+PropName) then
             Cells[3,ARow] := 'Get'+Value;
          if Cells[4,ARow] = ('Set'+PropName) then
             Cells[4,ARow] := 'Set'+Value;
          PropName := Value;
     end;
end;

procedure TfrmNewCollection.pcOptionsChange(Sender: TObject);
begin
     if (Sender as TPageControl).ActivePage = tsPreview then
        mmPreview.Lines.Text := ToString;
end;

procedure TfrmNewCollection.eCollectionItemNameChange(Sender: TObject);
begin
     if pcOptions.ActivePage = tsPreview then
        mmPreview.Lines.Text := ToString;
end;

function TfrmNewCollection.GetImplementationStr: String;
begin
     ToString;
     result := FImplementationStr;
end;

function TfrmNewCollection.GetInterfaceStr: String;
begin
     ToString;
     result := FInterfaceStr;
end;

procedure TfrmNewCollection.sgItemPropsGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
begin
     if ACol = 0 then
        PropName := Value;
end;

end.
