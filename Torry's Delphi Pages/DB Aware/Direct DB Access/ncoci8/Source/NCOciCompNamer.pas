{***********************************************}
{File:      NCOciCompNamer.pas                  }
{Revision:  2.01 / 04.02.2002                   }
{Comment:   Component namer class               }
{Copyright: (c) 1997-2002, Dmitry Arefiev       }
{Author:    Dmitry Arefiev, darefiev@da-soft.com}
{***********************************************}
{$I NCOciDef.inc}

// ===========================================
// Name formats:
// <fmt1>;...;<fmtN>[;*]
// -------------------------------------------
// Format commands:
// -------------------------------------------
// %C - class name
// %C(<prop name>) - class name of class property value
// %T - user defined text
// %T(<prompt>) - user defined text with enter prompt
// %V(<prop name>) - <prop name> text
// -------------------------------------------
// Format modifiers:
// -------------------------------------------
// L - lower case
// U - UPPER CASE
// P - Proper Case
// S - short
// ===========================================
// Strip prefixes:
// {<pref1>;...;<prefN>} ....
// -------------------------------------------

unit NCOciCompNamer;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Registry;

type
    TOciNameStrip = (nsRemoveUCa, nsRemoveLCa, nsRemoveUCb, nsRemoveLCb,
        nsRemoveOver, nsPrefixsNoCase);
    TOciNameStrips = set of TOciNameStrip;
    TOciCase = (csNone, csLower, csUpper, csProper);
    TOciShort = (shNone, shClassName, shNormal);

    TOciCompNamer = class(TComponent)
    private
        FStripPrefixs: String;
        FOptions: TOciNameStrips;
        FNameFormats: TStrings;
        FComponent: TComponent;
        function GetPropStrValue(APropName: String): String;
        function FormWord(AWord: String; ACase: TOciCase; AShort: TOciShort;
            AFirstWord: Boolean): String;
        function FormatName(AFormat: String): String;
        procedure SetNameFormats(AValue: TStrings);
        procedure SetComponent(const Value: TComponent);
    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function CreateName: String;
        procedure Rename;
        procedure LoadFromReg(AReg: TRegistry);
        procedure SaveToReg(AReg: TRegistry);
    published
        property StripPrefixs: String read FStripPrefixs write FStripPrefixs;
        property Options: TOciNameStrips read FOptions write FOptions
            default [nsRemoveLCa, nsRemoveLCb, nsPrefixsNoCase];
        property NameFormats: TStrings read FNameFormats write SetNameFormats;
        property Component: TComponent read FComponent write SetComponent;
    end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

implementation

Uses TypInfo, DB, NCOciUtil, NcOciMsg, NCOciDB;

constructor TOciCompNamer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FNameFormats := TStringList.Create;
    FNameFormats.Add('*=%Csl%Vp(DataField);%Csl%Vp(DataSource.DataSet.TableName);%Csl');
    FNameFormats.Add('TOCIDATABASE=%Csl%Vp(DatabaseName)');
    FNameFormats.Add('TOCITABLE=%Csl%Vp(TableName)');
    FNameFormats.Add('TOCIQUERY=%Csl%Vp(TableName)');
    FNameFormats.Add('TOCISTOREDPROC=%Cls%Vp(OPackageName)%Vp(OProcedureName)');
    FNameFormats.Add('TOCIUPDATESQL=%Csl%Vp(TableName)');
    FNameFormats.Add('TOCISEQUENCE=%Csl%Vp(SequenceName)');
    FNameFormats.Add('TOCINESTEDDATASET=%Csl%Vp(DataSetField.FieldName)%Vp(ParamName)');
    FNameFormats.Add('TOCIIMPHNDLDATABASE=%Csl%Vp(DatabaseName)');
    FNameFormats.Add('TOCIBDEDATABASE=%Csl%Vp(DatabaseName)');
    FNameFormats.Add('TOCITRANSACTIONMANAGER=%Cls%Vp(DatabaseName)');
    FNameFormats.Add('TDATASOURCE=%Csl%Vp(DataSet.TableName);%Csl%V(DataSet.Name)');
    FNameFormats.Add('TFIELD=%V(DataSet.Name)%V(FieldName)');
    FStripPrefixs := '{T}{OCI;NC;QR;*}{DB;ARR;IMP*}';
    FOptions := [nsRemoveLCa, nsPrefixsNoCase];
end;

destructor TOciCompNamer.Destroy;
begin
    FNameFormats.Free;
    inherited Destroy;
end;

procedure TOciCompNamer.SetNameFormats(AValue: TStrings);
begin
    FNameFormats.Assign(AValue);
end;

procedure TOciCompNamer.SetComponent(const Value: TComponent);
begin
    if FComponent <> Value then begin
        FComponent := Value;
        if FComponent <> nil then
            FComponent.FreeNotification(Self);
    end;
end;

procedure TOciCompNamer.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then
        if AComponent = FComponent then
            FComponent := nil;
end;

procedure TOciCompNamer.SaveToReg(AReg: TRegistry);
begin
    with AReg do begin
        WriteString('StripPrefixs', StripPrefixs);
        WriteInteger('Options', PInteger(@FOptions)^);
        WriteString('NameFormats', NameFormats.Text);
    end;
end;

procedure TOciCompNamer.LoadFromReg(AReg: TRegistry);
begin
    with AReg do begin
        StripPrefixs := ReadString('StripPrefixs');
        PInteger(@FOptions)^ := ReadInteger('Options');
        NameFormats.Text := ReadString('NameFormats');
    end;
end;

// Owner, Parent, DataSet - special cases. IOW, they are not
// published, but we know - how to get them !
function TOciCompNamer.GetPropStrValue(APropName: String): String;
var
    i: Integer;
    obj: TObject;
    pProp: PPropInfo;
    ucPropName: String;
begin
    i := 1;
    obj := Component;
    Result := '';
    while (i <= Length(APropName)) and (Result = '') and (obj <> nil) do begin
        ucPropName := UpperCase(StrToken(APropName, ['.'], i));
        if (ucPropName = 'OWNER') and (obj is TComponent) then begin
            obj := TComponent(obj).Owner;
            if (obj <> nil) and (i > Length(APropName)) then
                Result := obj.ClassName;
        end
        else if (ucPropName = 'PARENT') and (obj is TControl) then begin
            obj := TControl(obj).Parent;
            if (obj <> nil) and (i > Length(APropName)) then
                Result := obj.ClassName;
        end
        else if (ucPropName = 'DATASET') and (obj is TField) then begin
            obj := TField(obj).DataSet;
            if (obj <> nil) and (i > Length(APropName)) then
                Result := obj.ClassName;
        end
        else if (ucPropName = 'TABLENAME') and (obj is TOCIDataSet) then begin
            if i > Length(APropName) then
                Result := TOCIDataSet(obj).TableName;
        end
        else begin
            pProp := GetPropInfo(obj.ClassInfo, ucPropName);
            if (pProp = nil) or not ((pProp^.PropType^.Kind = tkClass) or
               (pProp^.PropType^.Kind in [tkChar, tkString, tkWChar, tkLString, tkWString]) and
               (i > Length(APropName))) then
                raise Exception.CreateFmt(SCNBadPropName, [APropName])
            else if pProp^.PropType^.Kind = tkClass then begin
                obj := TObject(GetOrdProp(obj, pProp));
                if (obj <> nil) and (i > Length(APropName)) then
                    Result := obj.ClassName;
            end
            else
                Result := GetStrProp(obj, pProp);
        end;
    end;
end;

function TOciCompNamer.FormWord(AWord: String; ACase: TOciCase; AShort: TOciShort;
    AFirstWord: Boolean): String;
var
    Group, Item, Prefixs: String;
    i, iPref, iTo, iFromWord: Integer;
    GroupMatched, sogl, glasn, lc: Boolean;
    StartWord: Boolean;
begin
    Result := AWord;
    if (StripPrefixs <> '') and (AShort = shClassName) then begin
        Prefixs := StripPrefixs;
        if nsPrefixsNoCase in Options then begin
            Prefixs := AnsiUpperCase(Prefixs);
            Result := AnsiUpperCase(Result);
        end;
        iPref := 1;
        iFromWord := 1;
        GroupMatched := True;
        while (iPref <= Length(Prefixs)) and GroupMatched do
            if Prefixs[iPref] = '{' then begin
                iTo := StrIPos('}', Prefixs, iPref + 1);
                if iTo = 0 then
                    raise Exception.Create(SCNBadStripPref);
                Group := Copy(Prefixs, iPref + 1, iTo - iPref - 1);
                i := 1;
                GroupMatched := False;
                while i <= Length(Group) do begin
                    Item := StrToken(Group, [';'], i);
                    if Item = '*' then
                        GroupMatched := True
                    else if StrLComp(PChar(Item), PChar(Result) + iFromWord - 1, Length(Item)) = 0 then begin
                        Inc(iFromWord, Length(Item));
                        GroupMatched := GroupMatched or True;
                        Break;
                    end;
                end;
                iPref := iTo + 1;
            end
            else
                Inc(iPref);
        Result := AWord;
        Result := Copy(Result, iFromWord, Length(Result));
    end;
    if (AShort <> shNone) and
       ([nsRemoveUCa, nsRemoveLCa, nsRemoveUCb, nsRemoveLCb,
         nsRemoveOver] * Options <> []) then begin
        i := 1;
        while i <= LengtH(Result) do begin
            glasn := Result[i] in ['A', 'E', 'I', 'J', 'O', 'U', 'Y',
                'a', 'e', 'i', 'j', 'o', 'u', 'y'];
            sogl := not glasn and (Result[i] in ['A'..'Z', 'a'..'z']);
            lc := AnsiUpperCase(Result[i]) <> Result[i];
            if glasn and
                ((nsRemoveUCa in Options) and not lc or
                 (nsRemoveLCa in Options) and lc) or
               sogl and
                ((nsRemoveUCb in Options) and not lc or
                 (nsRemoveLCb in Options) and lc) or
               not glasn and not sogl and (nsRemoveOver in Options) then
                Delete(Result, i, 1)
            else
                Inc(i);
        end;
    end;
    if (ACase = csUpper) then
        Result := AnsiUpperCase(Result)
    else if (ACase = csLower) then
        Result := AnsiLowerCase(Result)
    else if (ACase = csProper) then begin
        i := 1;
        StartWord := True;
        while i <= Length(Result) do
            if AFirstWord and (i = 1) and not (Result[i] in ['A'..'Z', 'a'..'z', '_']) or
               (i > 1) and not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then begin
                Delete(Result, i, 1);
                StartWord := True;
            end
            else begin
                if StartWord then begin
                    CharUpperBuff(@Result[i], 1);
                    StartWord := False;
                end
                else
                    CharLowerBuff(@Result[i], 1);
                Inc(i);
            end;
    end;
end;

function TOciCompNamer.FormatName(AFormat: String): String;
var
    i, iTo: Integer;
    capt, s: String;
    ACase: TOciCase;
    AShort: TOciShort;

    procedure GetOpt;
    begin
        ACase := csNone;
        AShort := shNone;
        Inc(i);
        while i <= Length(AFormat) do begin
            case AFormat[i] of
            'L', 'l': ACase := csLower;
            'U', 'u': ACase := csUpper;
            'P', 'p': ACase := csProper;
            'S', 's': AShort := shNormal;
            '.':
                begin
                    Inc(i);
                    Break;
                end;
            else
                Break;
            end;
            Inc(i);
        end;
    end;

    function GetArg: String;
    begin
        Result := '';
        iTo := StrIPos(')', AFormat, i + 1);
        if iTo <> 0 then begin
            Result := Copy(AFormat, i + 1, iTo - i - 1);
            i := iTo + 1;
        end;
    end;

begin
    Result := '';
    i := 1;
    while True do begin
        Result := Result + FormWord(StrToken(AFormat, ['%'], i), csNone, shNone,
            Length(Result) = 0);
        if i > Length(AFormat) then
            Break;
        case AFormat[i] of
        '%':
            begin
                Inc(i);
                s := '%';
            end;
        'C', 'c':
            begin
                GetOpt;
                if AFormat[i] = '(' then
                    s := GetPropStrValue(GetArg)
                else
                    s := Component.ClassName;
                if AShort <> shNone then
                    AShort := shClassName;
            end;
        'T', 't':
            begin
                GetOpt;
                if AFormat[i] = '(' then
                    capt := GetArg
                else
                    capt := SCNEnterText;
                s := '';
                if not InputQuery('Component Namer',  capt, s) then
                    Abort;
            end;
        'V', 'v':
            begin
                GetOpt;
                s := GetArg;
                if s = '' then
                    raise Exception.CreateFmt(SCNBadVFormat, [AFormat]);
                s := GetPropStrValue(s);
            end;
        else
            raise Exception.CreateFmt(SCNUnknownCommand, [AFormat]);
        end;
        Result := Result + FormWord(s, ACase, AShort, Length(Result) = 0);
    end;
end;

function TOciCompNamer.CreateName: String;
var
    j, i: Integer;
    msg, allFormats, itemFormat: String;
    comp: TComponent;
    AClass: TClass;
begin
    if Component = nil then
        raise Exception.Create(SCNNilComp);
    Result := '';
    msg := '';
    AClass := Component.ClassType;
    repeat
        if AClass = TComponent then
            allFormats := '*'
        else
            allFormats := AnsiUpperCase(AClass.ClassName);
        allFormats := NameFormats.Values[allFormats];
        AClass := AClass.ClassParent;
    until (allFormats <> '') or (AClass = TPersistent);
    if allFormats = '' then
        raise Exception.CreateFmt(SCNUnknownFormat, [Component.ClassName]);
    j := 1;
    repeat
        itemFormat := StrToken(allFormats, [';'], j);
        if itemFormat <> '*' then
            try
                itemFormat := FormatName(itemFormat);
                i := -1;
                repeat
                    Inc(i);
                    if i = 0 then
                        Result := itemFormat
                    else
                        Result := itemFormat + IntToStr(i);
                    comp := Component.Owner.FindComponent(Result);
                until (comp = nil) or (comp = Component);
            except
                on E: Exception do
                    msg := Exception(E).Message;
                on E: EAbort do begin
                    Result := '';
                    Break;
                end;
            end;
    until (j > Length(allFormats)) or (itemFormat = '*') or (Result <> '');
    if (Result = '') and (itemFormat <> '*') and (msg <> '') then
        raise Exception.Create(msg);
end;

procedure TOciCompNamer.Rename;
var
    s: String;
begin
    s := CreateName;
    if s <> '' then
        Component.Name := s;
end;

end.
