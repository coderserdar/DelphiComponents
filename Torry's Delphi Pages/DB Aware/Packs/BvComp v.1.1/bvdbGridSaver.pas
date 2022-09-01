unit bvdbGridSaver;

interface

uses
  {$ifndef LINUX}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  DBGrids,
  Registry,
  {$else}
  QGraphics,
  QControls,
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QMenus,
  Qt,
  {$endif}
  SysUtils, Classes,
  bvGridPopupUnit,bvDBGridDrawType,bvLocalization,

  {$ifndef VER140} //Delphi 6
  //{$ifndef BCB}

    FileCtrl,

  //{$endif}
  {$endif}

  bvConst;

const //MaxColCount=100;
      DEfaultWidth=50;

const GridSaverSubDir:string='Grids';
      GridsaverExt:string='.grd';

type
    TbvSaverMenuItem = class(tbvCommonGridmenuitem);
    TbvSaverPopupMenu = class(TbvCommonGridPopupMenu);


type
  TDBGridSaver = class(TComponent)
  private
    { Private declarations }
    IsLoaded:boolean;
    FThisGrid:TDBGrid;
    FEnabled:boolean;
    FSaveWidthOnly:boolean;
//    ThisIniFile:TIniFile;
    FOldOnFormDestroy:TNotifyEvent;
    FThVariant:string;
    FAutoPopup:boolean;
    FCannotAddColumns:boolean;

    FDefaultReadOnly:boolean;

    FValues:TStringList;
    OldSectionName:string;

    procedure SetThisGrid(Value:TDBGrid);
//    function GetSectionName:string;
    procedure CheckList;
    procedure OnDestroyForm(Sender:TObject);
    procedure MenuClick(Sender:TObject);
    function GetFixedCols:integer;
    procedure SetFixedCols(Value:integer);

    function GetIndicator:boolean;
    function GetTabStop:boolean;
    function GetAlwaysShowEditor:boolean;
    function GetAlwaysShowSelection:boolean;
    function GetRowLines:Boolean;
    function GetColLines:boolean;
    function GetCheckTitle:boolean;
    function GetSelectRows:Boolean;
    function GetMultiSelect:boolean;

    function GetStrippedRows:integer;
    procedure SetStrippedRows(Value:integer);
    function GetStrippedColor:TColor;
    procedure SetStrippedColor(Value:TColor);

    function GetTitleMinHeight:integer;
    procedure SetTitleMinHeight(Value:integer);

    function GetCELLROWS:integer;
    procedure SetCELLROWS(Value:integer);

    function GetEnter2Tab:boolean;
    procedure SetEnter2Tab(Value:boolean);

    function GetCellHint:boolean;
    procedure SetCellHint(Value:boolean);

    function GetLines3dH:Lines3dType;
    procedure SetLines3dH(Value:Lines3dType);

    function GetLines3dV:Lines3dType;
    procedure SetLines3dV(Value:Lines3dType);

    procedure SetIndicator(Value:boolean);
    procedure SetTabStop(Value:boolean);
    procedure SetAlwaysShowEditor(Value:boolean);
    procedure SetAlwaysShowSelection(Value:boolean);
    procedure setRowLines(Value:Boolean);
    procedure setColLines(Value:boolean);
    procedure setCheckTitle(Value:boolean);
    procedure setSelectRows(Value:Boolean);
    procedure setMultiSelect(Value:boolean);

    procedure SetColor(Value:TColor);
    function GetColor:TColor;

    procedure SetFixedColor(Value:TColor);
    function GetFixedColor:TColor;

    procedure SetReadOnly(Value:boolean);
    function GetReadOnly:boolean;

    procedure SetFont(Value:TFont);
    function GetFont:TFont;

    procedure SetTitleFont(Value:TFont);
    function GetTitleFont:TFont;

    function GetDefaultReadOnly:boolean;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;Operation: TOperation); override;
  public
    { Public declarations }
    Inserted:Array of integer;

    function IsInserted(Value:integer):boolean;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded;override;
    procedure SaveGrid;
    procedure RestoreGrid;
    procedure SetDefault;
    procedure DoEditor;
    procedure EndValues(Value:integer);
    procedure SetValues(ID:integer;Nomer:integer;FieldName:string;ColumnName:String;
                                  Width:integer;pVisible:boolean;AlignMent:TAlignMent;ReadOnly:boolean;
                                  Title_AlignMent:TAlignMent;Color:TColor;ParentColor:TColor;Font:TFont;ParentFont:TFont;FInserted:boolean);

    function GetValues(Nomer:integer;var ID:integer;var FieldName:string;var ColumnName:String;
                                  var Width:integer;var pVisible:boolean;
                                  var AlignMent:TAlignMent;var ReadOnly:boolean;
                                  var Title_AlignMent:TAlignMent;var thColor:TColor;var IsColorDefault:boolean;
                                  var DefFont:boolean;
                                  var Font_CharSet:TFontCharset;
                                  var Font_Color:TColor;
                                  var Font_Size:integer;
                                  var Font_Name:TFontName;
                                  var Font_Style_Bold:boolean;
                                  var Font_Style_Underline:boolean;
                                  var Font_Style_Italic:boolean;
                                  var Font_style_StrikeOut:boolean;
                                  var FInserted:boolean
             ):boolean;


    function RestoreColumn(ThIndex:integer):boolean;

//    procedure ClearBuffer;
    property FixedCols:integer read GetFixedCols write SetFixedCols;
    property Indicator:boolean read GetIndicator write SetIndicator;
    property TabStop:boolean read GetTabStop write SetTabStop;
    property AlwaysShowEditor:boolean read GetAlwaysShowEditor write SetAlwaysShowEditor;
    property AlwaysShowSelection:boolean read GetAlwaysShowSelection write SetAlwaysShowSelection;
    property RowLines:Boolean read GEtRowLines write SetRowLines;

    property StrippedRows:integer read GetStrippedRows write SetStrippedRows;
    property StrippedColor:TColor read GetStrippedColor write SetStrippedColor;

    property TitleMinHeight:integer read GetTitleMinHeight write SetTitleMinHeight;
    property CELLROWS:integer read GetCELLROWS write SetCELLROWS;

    property Enter2Tab:boolean read Getenter2Tab write SetEnter2Tab;
    property CellHint:boolean read GetCellHint write SetCellHint;

    property Lines3dV:Lines3dType read GetLines3dV write SetLines3dV;
    property Lines3dH:Lines3dType read GetLines3dH write SetLines3dH;

    property ColLines:boolean read GetColLines write SetColLines;
    property CheckTitle:boolean read GetcheckTitle write setCheckTitle;
    property SelectRows:Boolean read GetSelectRows write SetSelectRows;
    property Color:TColor read GetColor write SetColor;
    property FixedColor:TColor read GetFixedColor write SetFixedColor;
    property ReadOnly:boolean read GetReadOnly write SetReadOnly;
    property DefaultReadOnly:boolean read GetDefaultReadOnly;
    property Font:TFont read GetFont write SetFont;
    property TitleFont:TFont read GetTitleFont write SetTitleFont;
    property MultiSelect:boolean read getMultiSelect write SetMultiSelect;

  published
    { Published declarations }
    property ThisGrid:TDBGrid read FThisGrid write SetThisGrid;
    property Enabled:boolean read FEnabled write FEnabled;
    property SaveWidthOnly:boolean read FSaveWidthOnly write FSaveWidthOnly;
//    property MaxColCount:word read FMaxColCount write FMaxColCount;
    property ThVariant:string read FthVariant write FThVariant;
    property AutoPopup:boolean read FAutoPopup write FAutoPopup default true;
    property CannotAddColumns:boolean read FCannotAddColumns write FCannotAddColumns;
  end;

{$ifndef LINUX}
function CheckOldIniFormat:boolean;
{$endif}
function GetIniFileName(Saver:TDBGridSaver;const CompName:string=''):string;
procedure CheckIniDir;


var AllSaversEnabled:boolean;

implementation

uses bvColumsEditor,bvDBGrid,math,{rxdbCtrl,}bvUtils,bvcursorUnit,bvMessageUnit; //,Forms;

{$R bvgridsav.res}

function TrueString(BStr:string):boolean;
begin
  bstr:=trim(bstr);
  result:=(BStr='1') or (BStr=bvLocalization.BoolTrueText);
end;

function SetIniStr(Value:boolean):string; overload;
begin
  if Value then Result:='1'
  else Result:='0'
end;

function SetIniStr(Value:integer):string; overLoad;
begin
  Result:=inttostr(value);
end;

function GetParam(const List:tstrings; const Param:string;Default:Boolean):boolean; overload;
var thStr:string;
begin
  thstr:= List.Values[Param];
  if thstr='' then Result:=default
  else begin
    if (thstr='1') or (Uppercase(thsTr)='TRUE')
    then Result:=true
    else Result:=false;
  end;
end;

function GetParam(const List:tstrings; const Param:string;Default:integer):integer; overload;
var thStr:string;
begin
  thstr:= List.Values[Param];
  if thstr='' then Result:=default
  else begin
    Result:=strtoint(thstr);
  end;
end;

function GetParam(const List:tstrings; const Param:string;Default:string):string; overload;
var thStr:string;
begin
  thstr:= List.Values[Param];
  if thstr='' then Result:=default
  else begin
     Result:=thstr;
  end;
end;

type
     TColumnArray=record
        ID:integer;
        Name :String;
        ColumnName:string;
        Width :integer;
        index :integer;
        Visible:boolean;
        AlignMent:TAlignment;
        ReadOnly:boolean;
        Title_AlignMent:TAlignment;
        Color:TColor;
        DefColor:boolean;
        DefFont:boolean;

        Font_CharSet:TFontCharset;
        Font_Color:TColor;
        Font_Size:integer;
        Font_Name:TFontName;
        Font_Style_Bold:boolean;
        Font_Style_Underline:boolean;
        Font_Style_Italic:boolean;
        Font_style_StrikeOut:boolean;
     end;



// TListSortCompare = function (Item1, Item2: Pointer): Integer;

{ // что-то как-то не так
procedure bvQuickSort(SortArray:array of TColumnArray;L, R: Integer);
var
  I, J: Integer;
  P, T: TColumnArray;
begin
  repeat
    I := L;
    J := R;
    P := sortArray[(L+R) shr 1];
    repeat
      while SortArray[I].index<P.index do Inc(I);
      while SortArray[J].index<P.index do Dec(J);
      if I <= J then
      begin
        T := SortArray[I];
        SortArray[I] := SortArray[J];
        SortArray[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then bvQuickSort(SortArray, L, J);
    L := I;
  until I >= R;
end;
}



procedure TDBGridSaver.SetThisGrid(Value:TDBGrid);
var ThItem:TbvSaverMenuItem;
    i:integer;
begin
  if (Value is TDBGrid) then begin
    if (Value<>FThisGrid) and not (Owner=Value) then begin
////
        if AllSaversEnabled and FAutoPopup and  isLoaded and not (csdesigning in componentState) then begin
            if Assigned(FThisGrid) and Assigned(FThisGrid.PopupMenu) then begin
               if FThisGrid.PopupMenu is TbvSaverPopupMenu then begin
                 FThisGrid.PopupMenu.Free;
                 FThisGrid.Popupmenu:=nil;
               end
               else begin
                 i:=0;
                 while i<FThisGrid.PopupMenu.Items.Count do
                   if (FThisGrid.PopupMenu.Items[i] is TbvSaverMenuItem)
                      and (FThisGrid.PopupMenu.Items[i].Owner=Self)
                   then FThisGrid.PopupMenu.Items.Delete(i)
                   else inc(i);
               end;
            end;
            if {(Self.Enabled) and} Assigned(Value) then begin
              if (Value.PopupMenu=nil) then begin
                  Value.PopupMenu:=TbvSaverPopupMenu.create(Self);
//                  (Value.PopupMenu as tbvSaverPopupMenu).DrawText:='“аблица';
              end
              else if
                   not (Value.PopupMenu is TbvCommonGridPopupMenu)
                   and (Value.popupmenu.items.count>0) and
                   not ((Value.popupmenu.Items[Value.popupmenu.items.count-1] is TbvCommonGridMenuItem)
                       )
              then begin
                ThItem:=tbvSaverMenuItem.Create(self);
                ThItem.Caption:='-';
                Value.PopupMenu.items.Add(ThItem);
              end;

              ThItem:=tbvSaverMenuItem.Create(self);
              ThItem.Caption:=StrEditorCaption;
              ThItem.OnClick:=MenuClick;
              ThItem.Bitmap.LoadFromResourceName(Hinstance,'bvGridsaver');
              ThItem.HelpContext:=10000;
//              thItem.Enabled:= AllSaversEnabled;
              Value.PopupMenu.items.Add(ThItem);
//              SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bvGridSaver'),LoadBitmap(Hinstance,'bvGridsaver'));
            end;
        end;

/////
        FThisGrid:=Value;
    end;
  end
  else {if Value=nil then} FThisGrid:=nil
//  else ShowMessage('“олько дл€ Tdbgrid');
end;

constructor TDBGridSaver.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  IsLoaded:=false;
  FAutoPopup:=true;
  if AOwner is TbvDBGrid then FThisGrid:=TbvdbGrid(AOwner)
  else FThisGrid:=nil;
  Enabled:=true;
  FSaveWidthOnly:=false;
  FOldOnFormDestroy:=nil;
  FThVariant:='';
  FValues:=nil;
  Inserted:=nil;
  {if aowner is tbvdbgrid then FCannotAddColumns:=(Aowner as TbvdbGrid).savercannotaddcolumns
  else}
  FCannotAddColumns:=false;
end;

destructor TDBGridSaver.destroy;
var i:integer;
begin
    if Assigned(FThisGrid)
       and Assigned(Owner)
       and ( (Owner=FThisGrid)
             or
             (Owner is tForm)
             and (Owner.FindComponent(FThisGrid.Name)<>nil)
           )
       and Assigned(FThisGrid.PopupMenu)
    then  begin
      if FThisGrid.PopupMenu is TbvSaverPopupMenu
      then FThisGrid.PopupMenu.free
      else begin
         i:=0;
         while i<FThisGrid.PopupMenu.Items.Count do
           if (FThisGrid.PopupMenu.Items[i] is TbvSaverMenuItem)
              and (FThisGrid.PopupMenu.Items[i].Owner=Self)
           then FThisGrid.PopupMenu.Items.Delete(i)
           else inc(i);
      end;
    end;
    if Assigned(FValues) then FValues.Free;

    Inserted:=nil;
    inherited Destroy;
end;

procedure TDBGridSaver.MenuClick(Sender:tobject);
begin
  DoEditor;
end;

procedure TDBGridSaver.DoEditor;
var ThisColEdit:tcoleditForm;
begin
  ThisColEdit:=TColeditForm.Create(Self);
  try                   
    ThisColEdit.BitBtnInsert.enabled:=not FCannotAddColumns;
    ThisColEdit.NInsert.enabled:=not FCannotAddColumns;
    ThisColEdit.ShowModal;
  finally
    ThisColEdit.Free;
  end;
end;

procedure TDBGridSaver.Loaded;
var ThItem:TbvSaverMenuItem;
begin
  inherited;
//  FOldOnFormDestroy:=nil;
  FDefaultReadOnly:=ReadOnly;

  if not IsLoaded then begin
      IsLoaded:=true;
      if Self.Enabled
         and Assigned(FThisGrid)
         and (FThisGrid is tbvDBGrid)
         and (Owner<>FThisGrid)
      then Self.Enabled:=false;

      if (Self.Enabled)
         and not (Owner is TbvDBGrid)
         and (not assigned(FOldonFormDestroy))
         and  (not ( csDesigning in ComponentState))
         and (Self.Owner is TForm)
      then begin
        FOldOnFormDestroy:=(Self.Owner as TForm).OnDestroy;
        (Self.Owner as TForm).OnDestroy:=OnDestroyForm;
      end;

      if {(Self.Enabled) and} Assigned(ThisGrid) then begin
        if AllSaversEnabled and FAutoPopup and not (csDesigning in ComponentState) then  begin
            if (ThisGrid.PopupMenu=nil) then begin
               ThisGrid.PopupMenu:=tbvSaverpopupmenu.Create(Self);
//               (ThisGrid.PopupMenu as tbvSaverPopupMenu).DrawText:='“аблица';
            end
            else if
                 not (ThisGrid.PopupMenu is TbvCommonGridPopupMenu)
                 and (ThisGrid.popupmenu.items.count>0) and
                 not ((ThisGrid.popupmenu.items[ThisGrid.popupmenu.items.count-1] is TbvCommonGridMenuItem)
                     )
            then begin
               ThItem:=tbvSaverMenuItem.Create(self);
               ThItem.Caption:='-';
               ThisGrid.PopupMenu.items.Add(ThItem);
            end;
            ThItem:=tbvSaverMenuItem.Create(self);
            ThItem.Caption:=StrEditorCaption;
            ThItem.OnClick:=MenuClick;
            ThItem.Bitmap.LoadFromResourceName(Hinstance,'bvGridsaver');
            ThItem.HelpContext:=10000;
//            thItem.Enabled:= AllSaversEnabled;
            // Handle:=LoadBitmap(Hinstance,'SPAGENTS');
            ThisGrid.PopupMenu.items.Add(ThItem);   //resource discarded
//            SetMenuItemBitmaps(FThisGrid.popupmenu.Handle,ThItem.Command,MF_BYCOMMAND,LoadBitmap(Hinstance,'bvGridSaver'),LoadBitmap(Hinstance,'bvGridsaver'));
        end;
        RestoreGrid;
      end;
  end;
  OldSectionName:=GetIniFileName(Self);
end;

procedure TDBGridSaver.OnDestroyForm(Sender:TObject);
var ThEvent:TNotifyEvent;
begin
  SaveGrid;
  Enabled:=false;
  if assigned(FOldOnFormDestroy) then begin
    ThEvent:=FOldOnFormDestroy;
    FOldOnFormDestroy:=nil;
    ThEvent(sender);
  end;
end;


{
function TDBGridSaver.GetSectionName:string;
begin
  Result:=ThisGrid.Owner.ClassName+'.'+ThisGrid.Name;
//  Result:=Result+'.MySettings';
  if ThVariant<>'' then Result:=Result+'.'+ThVariant;
end;
}

procedure CheckIniDir;
var str:string;
begin
  str:=
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}
          ( extractFilePath(Application.ExeName)+GridsaverSubDir);
  if not directoryexists(str)
  then CreateDir(str);
end;

procedure TdbGridSaver.CheckList;
begin
  if not Assigned(fValues) then begin
    FValues:=TStringList.Create;
  end;

  if OldSectionName<>GetIniFileName(Self)
  then begin
    if FileExists(GetIniFileName(Self))
    then FValues.loadfromFile(GEtInifileName(Self))
    else fValues.Clear;
//    ThisIniFile.ReadSectionValues(GetSectionName,FValues);
    OldSectionName:=GetIniFileName(Self);
  end;
end;

function GetIniFileName(Saver:TDBGridSaver;const CompName:string=''):string;
begin
  REsult:=
     {$ifndef VER140}
        includetrailingbackslash
     {$else}
        includetrailingpathdelimiter
     {$endif}
          ( extractFilePath(Application.ExeName)+GridsaverSubDir);

  if (trim(CompName)='') and Assigned(saver) and Assigned(Saver.ThisGrid) then begin
     Result:=Result+saver.ThisGrid.Owner.ClassName+'.'+saver.ThisGrid.Name;
     if trim(Saver.thvariant)<>'' then Result:=REsult+'.'+Saver.thVariant;
  end
  else Result:=Result+CompName;

  REsult:=REsult+GridSaverExt;
end;

procedure TDBGridSaver.SetDefault;
begin
  CheckIniDir;
  if FileExists(GetIniFileName(Self)) then DeleteFile(GetIniFileName(Self));
//  ThisIniFile.EraseSection(GetSectionName);
//  ThisGrid.Columns.RestoreDefaults;
  Enabled:=false;
end;

//procedure TDBGridSaver.SetValues(Nomer:integer;FieldName:string;ColumnName:String;
//                                  Width:integer;pVisible:boolean);

procedure tdbGridSaver.EndValues(Value:integer);
var thInd:integer;
begin
  if assigned(FValues) then begin
    CheckIniDir;
    while true
    do begin
       thInd:=fValues.IndexOfName('Column'+StringReplace( format('%3d',[Value]),' ','0',[rfReplaceAll]));
       if thInd>=0
       then begin
            FValues.Delete(thInd);
            inc(Value);
       end
       else break;
    end;
    FValues.SaveToFile(GetIniFileName(Self));
  end;

end;

procedure tdbgridsaver.SetValues(ID:integer;Nomer:integer;FieldName:string;ColumnName:String;
                                  Width:integer;pVisible:boolean;
                                  AlignMent:TAlignMent;ReadOnly:boolean;
                                  Title_AlignMent:TAlignMent;Color:TColor;ParentColor:TColor;Font:TFont;ParentFont:TFont;FInserted:boolean);

   function Boolstr(Value:boolean):string;
   begin
     if Value
     then Result:='1' //bvLocalization.BoolTrueText
     else Result:='0' //bvLocalization.BoolFalseText;
   end;

var
    thStr:string;
begin

  thStr:=' Field='+trim(fieldName)+
         ', ID='+inttostr(ID)+
         ', Column='+trim(ColumnName)+', Width='+
         trim(inttostr(width))+', Visible='+BoolStr(PVisible)+', AlignMent='+
         inttostr(ord(AlignMent))+
         ', ReadOnly='+BoolStr(ReadOnly)+', Title_AlignMent='+
         inttostr(ord(Title_AlignMent));
  if not (Color=ParentColor) then thStr:=thStr+', Color='+inttostr(Color);

  {if (Font.Charset<>ParentFont.CharSet)
     or
     (Font.Color<>ParentFont.Color)
     or
     (Font.Size<>ParentFont.Size)
     or
     (Font.Name<>ParentFont.Name)
     or
     (Font.Style<>ParentFont.Style)
  then}
  begin
     thStr:=thStr+', Font_Charset='+inttostr(integer(font.Charset))
                 +', Font_Color='+inttostr(font.Color)
                 +', Font_Size='+inttostr(font.size)
                 +', Font_Name='+font.name
                 +', Font_Style_Bold='+BoolStr(fsBold in Font.Style)
                 +', Font_Style_Italic='+BoolStr(fsItalic in Font.Style)
                 +', Font_Style_Underline='+BoolStr(fsUnderline in Font.Style)
                 +', Font_Style_StrikeOut='+BoolStr(fsStrikeOut in Font.Style)
  end;

  if FInserted then  thStr:=thStr+', Inserted=1';


//  if Assigned(ThisIniFile)
//  then  ThisIniFile.WriteString(GetSectionName,'Column'+StringReplace( format('%3d',[nomer]),' ','0',[rfReplaceAll]) { trim(inttostr(nomer))},thStr);
  if Assigned(FValues) then FValues.Values['Column'+StringReplace( format('%3d',[nomer]),' ','0',[rfReplaceAll])]:=THstr;

end;

function tdbGridSaver.GetValues(Nomer:integer; var ID:integer;var FieldName:string;var ColumnName:String;
                                  var Width:integer;var pVisible:boolean;
                                  var AlignMent:TAlignMent;var ReadOnly:boolean;
                                  var Title_AlignMent:TAlignMent;var thColor:TColor;var IsColorDefault:boolean;
                                  var DefFont:boolean;
                                  var Font_CharSet:TFontCharset;
                                  var Font_Color:TColor;
                                  var Font_Size:integer;
                                  var Font_Name:TFontName;
                                  var Font_Style_Bold:boolean;
                                  var Font_Style_Underline:boolean;
                                  var Font_Style_Italic:boolean;
                                  var Font_style_StrikeOut:boolean;
                                  var FInserted:boolean
                                  ):boolean;
var ResStr:string;
    i,Ip:integer;
    Len:integer;
begin

  Result:=false;

  CheckList;

//  ResStr:=ThisIniFile.ReadString(GetSectionName,'Column'+trim(inttostr(nomer)),'');

  ResStr:=FValues.Values['Column'+StringReplace( format('%3d',[nomer]),' ','0',[rfReplaceAll])];



  if ResStr='' then exit;

  FInserted:=Pos(' Inserted=1',ResStr)>0;

  ip:=Pos('Field=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+6 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  FieldName:=trim(copy(ResStr,ip+6,Len));


  ip:=Pos('ID=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+3 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  try
    ID:=max(0,strtoint(trim(copy(ResStr,ip+3,len))));
  except
    exit;
  end;


////////
  ip:=Pos('AlignMent=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+10 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  try
    AlignMent:=TAlignMent(max(0,strtoint(trim(copy(ResStr,ip+10,len)))));
  except
    exit;
  end;

  ip:=Pos('ReadOnly=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+9 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  try
//    ReadOnly:=boolean(max(0,strtoint(trim(copy(ResStr,ip+9,len)))));
    ReadOnly:=TrueString(trim(copy(ResStr,ip+9,len)));
  except
    exit;
  end;

  ip:=Pos('Title_AlignMent=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+16 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  try
    Title_Alignment:=TAlignment(max(0,strtoint(trim(copy(ResStr,ip+16,len)))));
  except
    exit;
  end;

  ip:=Pos(' Color=',ResStr);


//  thColor:=low(Tcolor);
  IsColorDefault:=true;

  if ip>0 then begin
//  exit;
    Len:=0;
    for i:=ip+7 to length(resStr) do
      if not (ResStr[i]=',') then inc(Len)
      else break;

    if Len>0 then
    try
      thColor:=TColor(strtoint(copy(ResStr,ip+7,len)));
      IsColorDefault:=false;
    except
      exit;
    end;
  end;


////////

  ip:=Pos('Column=',ResStr);
  if ip=0 then exit;

  Len:=0;
  for i:=ip+7 to length(resStr) do
    if not (ResStr[i]=',') then inc(Len)
    else break;

  if Len=0 then exit;

  ColumnName:=trim(copy(ResStr,ip+7,Len));

  ip:=Pos('Width=',ResStr);

  if ip=0 then exit;

  Len:=0;
  for i:=ip+6 to length(resStr) do
    if (ResStr[i] in ['0'..'9','-','+']) then inc(Len)
    else break;

  if Len=0 then exit;

  try
    Width:=max(0,strtoint(trim(copy(ResStr,ip+6,len))));
  except
    exit;
  end;

/////////////////

  DefFont:=true;
  ip:=Pos('Font_Charset=',ResStr);
  if ip>0 then begin
//  exit;
    Len:=0;
    for i:=ip+13 to length(resStr) do
      if not (ResStr[i]=',') then inc(Len)
      else break;

    if Len>0 then
    try
      Font_CharSet:=TFontCharSet(strtoint(trim(copy(ResStr,ip+13,len))));

      ip:=Pos('Font_Color=',ResStr);
      if ip>0 then begin
    //  exit;
        Len:=0;
        for i:=ip+11 to length(resStr) do
          if not (ResStr[i]=',') then inc(Len)
          else break;

        if Len>0 then
        try
          Font_color:=strtoint(trim(copy(ResStr,ip+11,len)));

//

          ip:=Pos('Font_Size=',ResStr);
          if ip>0 then begin
        //  exit;
            Len:=0;
            for i:=ip+10 to length(resStr) do
              if not (ResStr[i]=',') then inc(Len)
              else break;

            if Len>0 then
            try
              Font_Size:=strtoint(trim(copy(ResStr,ip+10,len)));

              ip:=Pos('Font_Name=',ResStr);
              if ip>0 then begin
            //  exit;
                Len:=0;
                for i:=ip+10 to length(resStr) do
                  if not (ResStr[i]=',') then inc(Len)
                  else break;

                if Len>0 then
                try
                  Font_Name:=trim(copy(ResStr,ip+10,len));

//                  DefFont:=false;

                  ip:=Pos('Font_Style_Bold=',ResStr);
                  if ip>0 then begin
                //  exit;
                    Len:=0;
                    for i:=ip+16 to length(resStr) do
                      if not (ResStr[i]=',') then inc(Len)
                      else break;

                    if Len>0 then
                    try
                      Font_Style_Bold:=trueString(trim(copy(ResStr,ip+16,len)));


                      ip:=Pos('Font_Style_Italic=',ResStr);
                      if ip>0 then begin
                    //  exit;
                        Len:=0;
                        for i:=ip+18 to length(resStr) do
                          if not (ResStr[i]=',') then inc(Len)
                          else break;

                        if Len>0 then
                        try
                          Font_Style_Italic:=trueString(trim(copy(ResStr,ip+18,len)));

//
                          ip:=Pos('Font_Style_Underline=',ResStr);
                          if ip>0 then begin
                        //  exit;
                            Len:=0;
                            for i:=ip+21 to length(resStr) do
                              if not (ResStr[i]=',') then inc(Len)
                              else break;

                            if Len>0 then
                            try
                              Font_Style_Underline:=trueString(trim(copy(ResStr,ip+21,len)));

                              ip:=Pos('Font_Style_StrikeOut=',ResStr);
                              if ip>0 then begin
                            //  exit;
                                Len:=0;
                                for i:=ip+21 to length(resStr) do
                                  if not (ResStr[i]=',') then inc(Len)
                                  else break;

                                if Len>0 then
                                try
                                  Font_Style_StrikeOut:=TrueString(trim(copy(ResStr,ip+21,len)));

                                  DefFont:=false;
                                except
                                  exit;
                                end;
                              end;


//
                            except
                              exit;
                            end;
                          end;


//
                        except
                          exit;
                        end;
                      end;

                    except
                      exit;
                    end;
                  end;

//
                except
                  exit;
                end;
              end;

//
            except
              exit;
            end;
          end;

//          DefFont:=false;
        except
          exit;
        end;
      end;
    except
      exit;
    end;
  end;


/////////////////

  ip:=Pos('Visible=',ResStr);

  if ip>0 then begin

      Len:=0;
      for i:=ip+8 to length(resStr) do
        if (pos(ResStr[i],bvLocalization.BoolTrueText)>0)
           or
           (pos(ResStr[i],bvLocalization.BoolFalseText)>0)
        then inc(Len)
        else break;

      pVisible:=not ((trim(copy(ResStr,ip+8,len))=bvLocalization.BoolFalseText)
                     or (copy(resstr,ip+8,1)='0')
                    );

  end
  else pVisible:=True;


  Result:=true;
end;


procedure TDBGridSaver.SaveGrid;
var  i:integer;
    ThisColumns:TDBGridColumns;
begin

  if csDesigning in ComponentState then Exit;

  if not (Self.enabled) then Exit;

  with TWaitCursor.Create do
  try

    ThisColumns:=ThisGrid.Columns;


    if Assigned(ThisColumns) then begin
      for i:=0 to ThisColumns.Count-1 do
      if Assigned(ThisColumns[i]) then
      begin
        SetValues( ThisColumns[i].ID, i,ThisColumns[i].FieldName,ThisColumns[i].Title.Caption,
                   ThisColumns[i].Width,ThisColumns[i].Visible,
                   ThisColumns[i].AlignMent,thisColumns[i].ReadOnly,ThisColumns[i].title.Alignment,
                   ThisColumns[i].Color,ThisGrid.Color,ThisColumns[i].Font,ThisGrid.Font,IsInserted(ThisColumns[i].ID));
      end;
      EndValues(ThisColumns.Count);
    end;
  finally
    free;
  end
end;

function TDBGridSaver.RestoreColumn(ThIndex:integer):boolean;
var
    ThisColumns:TDBGridColumns;
    i:integer;
    ColName,ColField:string;
    ColWidth:integer;
    ColVisible:boolean;
    ColID:integer;
    ColAlignMent:TAlignMent;
    ColReadOnly:boolean;
    ColTitle_AlignMent:TAlignMent;
    ColColor:TColor;
    DefColor:boolean;
    DefFont:boolean;

    Font_CharSet:TFontCharset;
    Font_Color:TColor;
    Font_Size:integer;
    Font_Name:TFontName;
    Font_Style_Bold:boolean;
    Font_Style_Underline:boolean;
    Font_Style_Italic:boolean;
    Font_style_StrikeOut:boolean;
    FInserted:boolean;

begin

  Result:=false;

  if csDesigning in ComponentState then Exit;

  if not (Self.enabled) then Exit;

  ThisColumns:=ThisGrid.Columns;



   i:=0;
   while GetValues( i ,ColID,ColField,ColName,Colwidth,ColVisible,ColAlignMent,ColReadOnly,ColTitle_AlignMent,ColColor,DefColor,
          DefFont,
          Font_CharSet,
          Font_Color,
          Font_Size,
          Font_Name,
          Font_Style_Bold,
          Font_Style_Underline,
          Font_Style_Italic,
          Font_style_StrikeOut,
          FInserted
          )
    do begin

         if  (ColID=ThisColumns[ThIndex].ID) then begin
//           if FInserted then begin
//              ThisColumns[thIndex].ID:=ColID
//           end;

           ThisColumns[ThIndex].Visible:=ColVisible;

           if ColWidth>0 then  ThisColumns[ThIndex].Width:=ColWidth;

           if ColVisible and (ThisColumns[thIndex].width<=0)
           then ThisColumns[ThIndex].width:=DefaultWidth;

           ThisColumns[ThIndex].title.Caption:=ColName;

           ThisColumns[ThIndex].Alignment:=ColAlignMent;
           ThisColumns[thIndex].ReadOnly:=ColREadOnly;
           ThisColumns[thIndex].Title.Alignment:=ColTitle_AlignMent;
           if not DefColor
           then ThisColumns[thIndex].Color:=TColor(round(ColColor))
           else ThisColumns[thIndex].Color:=ThisGrid.Color;


           if not DefFont then begin
              ThisColumns[thIndex].Font.CharSet:=Font_CharSet;
              ThisColumns[thIndex].Font.Color:=Font_Color;
              ThisColumns[thIndex].Font.Size:=Font_Size;
              ThisColumns[thIndex].Font.Name:=Font_Name;
              if Font_style_Bold then
                ThisColumns[thIndex].Font.Style:=ThisColumns[thIndex].Font.Style+[fsBold];
              if Font_style_Underline then
                ThisColumns[thIndex].Font.Style:=ThisColumns[thIndex].Font.Style+[fsUnderline];

              if Font_style_Italic then
                ThisColumns[thIndex].Font.Style:=ThisColumns[thIndex].Font.Style+[fsItalic];
              if Font_style_StrikeOut then
                ThisColumns[thIndex].Font.Style:=ThisColumns[thIndex].Font.Style+[fsStrikeOut];
           end;

           if not SaveWidthOnly then  ThisColumns[ThIndex].Index:=i;
           Result:=true;
           exit;
         end;
         inc(i);                      
   end;
end;

procedure TDBGridSaver.RestoreGrid;
var
    ColumnArray:array of TcolumnArray;  //[0..MaxColCount]
//    thColumn:TColumnArray;
    ThisColumn:TColumnArray;
    NewColumn:TColumn;

    i,k,ColCount,ResColCount:integer;
    ThisColumns:TDBGridColumns;

    thFS:TFontstyles;
    fIsInserted:boolean;
    IsVisible:boolean;
    FCheckTitle:boolean;

    NeedRestoreControl:boolean;
begin

  if csDesigning in ComponentState then Exit;

  if not (Self.enabled) then Exit;


  try
    if ThisGrid is tbvdbGrid then begin
      FCheckTitle:=(ThisGrid as tbvdbgrid).checktitle;
      (ThisGrid as tbvdbgrid).checktitle:=false;
    end
    else
      FCheckTitle:=false;

    try


    //  ThisGrid.Visible:=false;
    //  ThisGrid.Columns.State:=csCustomized;
      ThisGrid.Columns.BeginUpdate;
      //ThisGrid.beginLayout;
    //  ThisGrid.Columns.Items[0].Expandabl;
      IsVisible:=ThisGrid.Visible;
      NeedRestoreControl:=assigned(ThisGrid.Owner)
                          and (ThisGrid.Owner is TCustomForm)
                          and (TCustomForm(ThisGrid.Owner).ActiveControl=ThisGrid);
      ThisGrid.Visible:=false;



      with TWaitCursor.Create do
      try
        ThisColumns:=ThisGrid.Columns;

      //  if ThisColumns.State<>csCustomized then ThisColumns.State:=csCustomized;

        ColCount:=ThisColumns.Count;

        (*
         for i:=0 to ColCount-1 do begin      // чтобы не глючил с размерами, как например
      //                                        // при открытии движени€ по артикулу - поле упак/ем
           //include(ThisColumns[i].AssignedValues,cvWidth);
           //ThisColumns[i].AssignedValues:=ThisColumns[i].AssignedValues+[cvWidth];

           {
           ThisColumns[i].Width:=ThisColumns[i].Width+1;
           ThisColumns[i].Width:=ThisColumns[i].Width-1;
           }
    //         if assigned(ThisColumns[i].Field) then
    //         ThisColumns[i].Field.DisplayWidth:=1;
    //         include(ThisColumns[i].AssignedValues,cvWidth);
      //     ThisColumns[i].Width:=ThisColumns[i].Width;
         end;
        *)



        Setlength(ColumnArray,ColCount);

      //  ThisGrid.beginLayout;
        try
          CheckIniDir;
          CheckList;
          if Assigned(thisGrid) then begin
            if ThisGrid is tbvdbGrid then (ThisGrid as tbvdbGrid).FixedCols:=GetParam(FValues,'FixedCols',(ThisGrid as tbvdbGrid).FixedCols)
            ;{else if ThisGrid is trxdbGrid then (ThisGrid as trxdbGrid).FixedCols:=GetParam(FValues,'FixedCols',(ThisGrid as trxdbGrid).FixedCols);}

            if GetParam(FValues,'Indicator',dgIndicator in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgIndicator]
            else ThisGrid.Options:=ThisGrid.Options-[dgIndicator];

            thisGrid.tabstop:=GetParam(FValues,'TabStop',ThisGrid.tabstop);

            if GetParam(FValues,'AlwaysShowEditor',dgAlwaysShowEditor in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgAlwaysShowEditor]
            else ThisGrid.Options:=ThisGrid.Options-[dgAlwaysShowEditor];

            if GetParam(FValues,'AlwaysShowSelection',dgAlwaysShowSelection in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgAlwaysShowSelection]
            else ThisGrid.Options:=ThisGrid.Options-[dgAlwaysShowSelection];

            if GetParam(FValues,'RowLine',dgRowLines in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgRowLines]
            else ThisGrid.Options:=ThisGrid.Options-[dgRowLines];

            if GetParam(FValues,'ColLines',dgColLines in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgColLines]
            else ThisGrid.Options:=ThisGrid.Options-[dgColLines];

            if GetParam(FValues,'MultiSelect',dgMultiSelect in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgMultiSelect]
            else ThisGrid.Options:=ThisGrid.Options-[dgMultiSelect];

            if GetParam(FValues,'Title',dgTitles in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgtitles]
            else ThisGrid.Options:=ThisGrid.Options-[dgTitles];

            if GetParam(FValues,'SelectRows',dgRowselect in ThisGrid.Options)
            then ThisGrid.Options:=ThisGrid.Options+[dgRowselect]
            else ThisGrid.Options:=ThisGrid.Options-[dgrowSelect];


            if ThisGrid is TbvdbGrid then begin
               (ThisGrid as tbvdbGrid).StrippedRows:=GetParam(FValues,'StrippedRows',(ThisGrid as tbvdbGrid).StrippedRows);
               (ThisGrid as tbvdbGrid).StrippedColor:=GetParam(FValues,'StrippedColor',(ThisGrid as tbvdbGrid).StrippedColor);

               (ThisGrid as tbvdbGrid).TitleMinHeight:=GetParam(FValues,'TitleMinHeight',(ThisGrid as tbvdbGrid).TitleMinHeight);
               (ThisGrid as tbvdbGrid).cELLHeights:=GetParam(FValues,'CELLHeights',(ThisGrid as tbvdbGrid).CELLHeights);
               (ThisGrid as tbvdbGrid).Enter2Tab:=getParam(FValues,'Enter2Tab',(ThisGrid as tbvdbGrid).Enter2Tab);
               (ThisGrid as tbvdbGrid).HintCells:=getParam(FValues,'HIntCells',(ThisGrid as tbvdbGrid).HintCells);
               (ThisGrid as tbvdbGrid).Lines3dV:=Lines3dType( getParam(FValues,'Lines3dV',integer((ThisGrid as tbvdbGrid).Lines3dV)));
               (ThisGrid as tbvdbGrid).Lines3dH:=Lines3dType( getParam(FValues,'Lines3dH',integer((ThisGrid as tbvdbGrid).Lines3dH)));
            end;

            ThisGrid.Color:=TColor(GetParam(FValues,'Color',Color));
            ThisGrid.fixedColor:=TColor(GetParam(FValues,'FixedColor',FixedColor));

            thisGrid.Font.Charset:=TFontCharset(GetParam(FValues,'Font.CharSet',integer(Font.Charset)));
            thisGrid.Font.Color:=TColor(GetParam(FValues,'Font.Color',Font.Color));
            thisGrid.Font.Size:=GetParam(FValues,'Font.Size',Font.Size);


            thisGrid.Font.Size:=GetParam(FValues,'Font.Size',Font.Size);

            thFS:=[];
            if GetParam(FValues,'Font.Style.Bold',fsBold in Font.Style)
            then thFs:=thFs+[fsBold];
            if GetParam(FValues,'Font.Style.Underline',fsUnderline in Font.Style)
            then thFs:=thFs+[fsUnderLine];
            if GetParam(FValues,'Font.Style.Italic',fsItalic in Font.Style)
            then thFs:=thFs+[fsItalic];
            if GetParam(FValues,'Font.Style.StrikeOut',fsStrikeOut in Font.Style)
            then thFs:=thFs+[fsStrikeOut];

            ThisGrid.Font.Style:=thFS;

            ThisGrid.Font.Name:=GetParam(FValues,'Font.Name',Font.Name);

            thisGrid.TitleFont.Charset:=TFontCharset(GetParam(FValues,'TitleFont.CharSet',integer(TitleFont.Charset)));
            thisGrid.TitleFont.Color:=TColor(GetParam(FValues,'TitleFont.Color',TitleFont.Color));
            thisGrid.TitleFont.Size:=GetParam(FValues,'TitleFont.Size',TitleFont.Size);

            thFS:=[];
            if GetParam(FValues,'titleFont.Style.Bold',fsBold in titleFont.Style)
            then thFs:=thFs+[fsBold];
            if GetParam(FValues,'titleFont.Style.Underline',fsUnderline in titleFont.Style)
            then thFs:=thFs+[fsUnderLine];
            if GetParam(FValues,'titleFont.Style.Italic',fsItalic in titleFont.Style)
            then thFs:=thFs+[fsItalic];
            if GetParam(FValues,'titleFont.Style.StrikeOut',fsStrikeOut in titleFont.Style)
            then thFs:=thFs+[fsStrikeOut];

            ThisGrid.titleFont.Style:=thFS;

            ThisGrid.TitleFont.Name:=GetParam(FValues,'TitleFont.Name',TitleFont.Name);


            if not DefaultReadOnly
            then ThisGrid.ReadOnly:=GetParam(FValues,'ReadOnly',ReadOnly);

          end;

           ResColCount:=0;
           while true do
           {for ResColCount:=0 to ColCount-1 do}
           begin
              thisColumn.Index:=ResColCount;
              thisColumn.Color:=low(TColor);

      //        ThisColumn.ID:=ThisGrid.Columns[ResColCount]

      //        ColumnArray[ResColCount].Index:=ResColCount;

      //        ColumnArray[ResColCount].Color:=low(tcolor);// COLOR_ENDCOLORS;

              if not GetValues( ResColCount,thisColumn.ID ,thisColumn.Name,thisColumn.ColumnName,thisColumn.width,
                    thisColumn.Visible,
                    thisColumn.AlignMent,
                    thisColumn.ReadOnly,
                    thisColumn.Title_AlignMent,
                    ThisColumn.Color,
                    ThisColumn.DefColor,
                    ThisColumn.DefFont,
                    ThisColumn.Font_CharSet,
                    ThisColumn.Font_Color,
                    ThisColumn.Font_Size,
                    ThisColumn.Font_Name,
                    ThisColumn.Font_Style_Bold,
                    ThisColumn.Font_Style_Underline,
                    ThisColumn.Font_Style_Italic,
                    ThisColumn.Font_style_StrikeOut,
                    fIsInserted
                    )
              then break
              else begin
                 if fIsInserted then begin
                    for k:=0 to ThisColumns.Count-1 do begin
                       if AnsiUppercase(thisColumns[k].FieldName)=AnsiUppercase(ThisColumn.Name)
                       then begin
                          break;
                       end;
                    end;
                    if k>=ThisColumns.count then begin // не найдено такое поле, значит можно создавать колонку
                       NewColumn:=TColumn.Create(ThisColumns);

                       ThisColumn.ID:=NewColumn.ID;
                       NewColumn.FieldName:=ThisColumn.Name;

                       if not Assigned(Inserted) then begin
                         setLength(Inserted,1);
                         Inserted[0]:=NewColumn.ID
                       end
                       else begin
                         i:=high(inserted);
                         SetLength(Inserted,i+2);
                         Inserted[i+1]:=NewColumn.ID;
                       end;
                    end
                    else begin
                       //inc(ResColCount);  пусть вставитс€
                       //continue
                    end;
                 end;

                 if not Assigned(ColumnArray)
                 then begin
                    SetLength(ColumnArray,1);
                    ColumnArray[0]:=ThisColumn;
                 end
                 else begin
                   if High(ColumnArray)<ResColCount
                   then SetLength(ColumnArray,ResColCount+1);
                   ColumnArray[ResColCount]:=ThisColumn;
                 end;
              end;
              inc(ResColCount);
           end;

           ColCount:=min(ThisColumns.Count,rescolcount);

           setLength(ColumnArray,ColCount);


           if ResColCount>0 then begin

              // ѕо индексу вроде как уже отсортировано!
             //if not SaveWidthOnly
             //then bvQuickSort(ColumnArray,low(ColumnArray),High(ColumnArray));
      {
           // —ортировка методом всплывающих пузырьков
             if not SaveWidthOnly then
             for i:=0 to ResColCount-1 do begin
               for k:=0 to ResColCount-1 do if ColumnArray[k].index=i then break;
               if k<ResColCount then begin
                 ThisColumn:=ColumnArray[i];
                 ColumnArray[i]:=ColumnArray[k];
                 ColumnArray[k]:=ThisColumn;
               end;
             end;
      }

             for i:=0 to ColCount-1 do begin
                NewColumn:=nil;

                for k:=i to ColCount-1 do begin
                   if AnsiUppercase(thisColumns[k].FieldName)=AnsiUppercase(ColumnArray[i].Name)
                   then begin
                      NewColumn:=ThisColumns[k];
                      break;
                   end;
                end;

                //if not Assigned(NewColumn)
                //then NewColumn:=TColumn(ThisColumns.Finditemid(ColumnArray[i].ID));

                if Assigned(NewColumn)
                then with Newcolumn do begin
                    if ColumnArray[i].Index>=ColCount then
                      index:=ColCount-1
                    else
                      if not SaveWidthOnly then index:=ColumnArray[i].index;

                    Visible:=ColumnArray[i].Visible;

                    if (FieldName='') then FieldName:=ColumnArray[i].Name;

                    if ColumnArray[i].Width>0 then width:=ColumnArray[i].Width;

                    if Visible and (width<=0)
                    then width:=DefaultWidth;


                    AlignMent:=ColumnArray[i].AlignMent;
                    ReadOnly:=ColumnArray[i].ReadOnly;
                    title.Alignment:=ColumnArray[i].Title_AlignMent;

                    if not ColumnArray[i].DefColor  //COLOR_ENDCOLORS
                    then  Color:=round(ColumnArray[i].Color)
                    else Color:=ThisGrid.Color;


                    title.Caption:=ColumnArray[i].ColumnName;

                    if not ColumnArray[i].DefFont then begin
                       Font.CharSet:=ColumnArray[i].Font_CharSet;
                       Font.Color:=ColumnArray[i].Font_Color;
                       Font.Size:=ColumnArray[i].Font_Size;
                       Font.Name:=ColumnArray[i].Font_Name;
                       Font.Style:=[];
                       if ColumnArray[i].Font_style_Bold then
                         Font.Style:=Font.Style+[fsBold];
                       if ColumnArray[i].Font_style_Underline then
                         Font.Style:=Font.Style+[fsUnderline];

                       if ColumnArray[i].Font_style_Italic then
                         Font.Style:=Font.Style+[fsItalic];
                       if ColumnArray[i].Font_style_StrikeOut then
                         Font.Style:=Font.Style+[fsStrikeOut];
                    end;

      //              break;
                end;
             end;
           end;
        finally
      //     ThisGrid.Endlayout;
           ColumnArray:=nil;
        end;
      finally
        Free;
        ThisGrid.Columns.EndUpdate;
        //ThisGrid.Columns.endLayout;
        ThisGrid.Visible:=IsVisible;
        if IsVisible and NeedRestoreControl and Assigned(ThisGrid.Owner)
           and (ThisGrid.Owner is TCustomForm)
           and (TCustomForm(ThisGrid.Owner).ActiveControl<>ThisGrid)
        then   TCustomForm(ThisGrid.Owner).activeControl:=ThisGrid;

      end
    finally
      if FCheckTitle
      then (ThisGrid as tbvdbgrid).checktitle:=FCheckTitle
    end;
  finally
    if Assigned(ThisGrid)
       and (ThisGrid is tbvDBGrid)
       and assigned((ThisGrid as TbvDBGrid).Afterrestore)
    then (ThisGrid as TbvDBGrid).Afterrestore(SElf);
  end;

end;

procedure TDBGridSaver.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent=ThisGrid) then ThisGrid:=nil;
  end;
end;

procedure tdbGridsaver.SetFixedCols(value:integer);
begin
  if Assigned(thisGrid) and (Value>-1) then begin
    if ThisGrid is tbvdbGrid then (ThisGrid as tbvdbGrid).FixedCols:=Value
    ;//else if ThisGrid is trxdbGrid then (ThisGrid as trxdbGrid).FixedCols:=Value;

    CheckList;
    FValues.Values['FixedCols']:=SetIniStr(Value);
  end;
end;


procedure tdbGridsaver.SetIndicator(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgIndicator]
    else ThisGrid.Options:=ThisGrid.Options-[dgIndicator];
    CheckList;
    FValues.Values['Indicator']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.SetTabStop(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    ThisGrid.tabstop:=Value;
    CheckList;
    FValues.Values['TabStop']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.SetAlwaysShowEditor(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgAlwaysShowEditor]
    else ThisGrid.Options:=ThisGrid.Options-[dgAlwaysShowEditor];
    CheckList;
    FValues.Values['AlwaysShowEditor']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.SetAlwaysShowSelection(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgAlwaysShowSelection]
    else ThisGrid.Options:=ThisGrid.Options-[dgAlwaysShowSelection];
    CheckList;
    FValues.Values['AlwaysShowSelection']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setRowLines(Value:Boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgRowLines]
    else ThisGrid.Options:=ThisGrid.Options-[dgRowLines];
    CheckList;
    FValues.Values['RowLine']:=setIniStr(Value);
  end;
end;

function tdbGridSaver.GetStrippedRows:integer;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).strippedRows
  else Result:=0;
end;

function tdbGridSaver.GetTitleMinHeight:integer;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).TitleMinHeight
  else Result:=1;
end;

function tdbGridSaver.GetCELLROWS:integer;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).CELLHeights
  else Result:=1;
end;

function tdbGridSaver.GetEnter2TAb:boolean;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).Enter2Tab
  else Result:=false;
end;

function tdbGridSaver.GetCellHint:boolean;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).HintCells
  else Result:=false;
end;

function tdbGridSaver.GetLines3dV:Lines3dType;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).Lines3dV
  else Result:=c3dNONE;
end;

function tdbGridSaver.GetLines3dH:Lines3dType;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).Lines3dH
  else Result:=c3dNONE;
end;

procedure tdbGridsaver.setStrippedRows(Value:integer);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).strippedRows:=Value;

    CheckList;
    FValues.Values['StrippedRows']:=SetIniStr((ThisGrid as TbvdbGrid).strippedRows);
  end;
end;

procedure tdbGridsaver.setTitleMinHeight(Value:integer);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).TitleMinHeight:=Value;

    CheckList;
    FValues.Values['TitleMinHeight']:=SetIniStr((ThisGrid as TbvdbGrid).TitleMinHeight);
  end;
end;

procedure tdbGridsaver.setCELLROWS(Value:integer);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).CELLHeights:=Value;

    CheckList;
    FValues.Values['CELLHeights']:=SetIniStr((ThisGrid as TbvdbGrid).CELLHeights);
  end;
end;

procedure tdbGridsaver.setEnter2Tab(Value:boolean);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).Enter2Tab:=Value;

    CheckList;
    FValues.Values['Enter2Tab']:=SetIniStr((ThisGrid as TbvdbGrid).Enter2TAb);
  end;
end;

procedure tdbGridsaver.setCellHint(Value:boolean);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).HintCells:=Value;

    CheckList;
    FValues.Values['Hintcells']:=SetIniStr((ThisGrid as TbvdbGrid).HintCells);
  end;
end;

procedure tdbGridsaver.SetLines3dH(Value:Lines3dtype);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).Lines3dH:=Value;

    CheckList;
    FValues.Values['Lines3dH']:=SetIniStr(integer((ThisGrid as TbvdbGrid).Lines3dH));
  end;
end;

procedure tdbGridsaver.SetLines3dV(Value:Lines3dtype);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).Lines3dV:=Value;

    CheckList;
    FValues.Values['Lines3dV']:=SetIniStr(integer((ThisGrid as TbvdbGrid).Lines3dV));
  end;
end;

function tdbGridSaver.GetStrippedColor:TColor;
begin
  if Assigned(ThisGrid)  and (ThisGrid is tbvdbGrid)
  then Result:=(ThisGrid as tbvdbGrid).strippedColor
  else Result:=clWhite;
end;

procedure tdbGridsaver.setStrippedColor(Value:TColor);
begin
  if Assigned(thisGrid)
     and (ThisGrid is TbvdbGrid)
  then begin
    (ThisGrid as TbvdbGrid).strippedColor:=Value;

    CheckList;
    FValues.Values['Strippedcolor']:=SetIniStr((ThisGrid as TbvdbGrid).strippedColor);
  end;
end;

procedure tdbGridsaver.setColLines(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgColLines]
    else ThisGrid.Options:=ThisGrid.Options-[dgColLines];
    CheckList;
    FValues.Values['ColLines']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setMultiSelect(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgMultiSelect]
    else ThisGrid.Options:=ThisGrid.Options-[dgMultiSelect];
    CheckList;
    FValues.Values['MultiSelect']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setCheckTitle(Value:boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgTitles]
    else ThisGrid.Options:=ThisGrid.Options-[dgTitles];
    CheckList;
    FValues.Values['title']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setSelectRows(Value:Boolean);
begin
  if Assigned(thisGrid) then begin
    if Value then ThisGrid.Options:=ThisGrid.Options+[dgRowselect]
    else ThisGrid.Options:=ThisGrid.Options-[dgRowSelect];
    CheckList;
    FValues.Values['SelectRows']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setColor(Value:TColor);
begin
  if Assigned(thisGrid) then begin
    ThisGrid.Color:=Value;
    CheckList;
    FValues.Values['Color']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setFixedColor(Value:TColor);
begin
  if Assigned(thisGrid) then begin
    ThisGrid.FixedColor:=Value;
    CheckList;
    FValues.Values['FixedColor']:=setIniStr(Value);
  end;
end;

procedure tdbGridsaver.setFont(Value:TFont);
begin
  if Assigned(thisGrid) then begin
    ThisGrid.Font:=Value;

    CheckList;
    FValues.Values['Font.CharSet']:=SetIniStr(integer(Value.Charset));
    FValues.Values['Font.Color']:=SetIniStr(Value.Color);
    FValues.Values['Font.Size']:=SetIniStr(value.Size);
    FValues.Values['Font.Name']:=Value.Name;

    FValues.Values['Font.Style.Bold']:=SetIniStr(fsBold in Value.Style);
    FValues.Values['Font.Style.Underline']:=setIniStr(fsUnderline in Value.Style);
    FValues.Values['Font.Style.Italic']:=SetIniStr(fsItalic in value.Style);
    FValues.Values['Font.Style.StrikeOut']:=SetIniStr(fsStrikeOut in Value.Style);
  end;
end;

procedure tdbGridsaver.settitleFont(Value:TFont);
begin
  if Assigned(thisGrid) then begin
    ThisGrid.TitleFont:=Value;

    CheckList;
    FValues.Values['TitleFont.CharSet']:=setIniStr(integer(Value.Charset));
    FValues.Values['TitleFont.Color']:=setIniStr(Value.Color);
    FValues.Values['TitleFont.Size']:=setIniStr(value.Size);
    FValues.Values['TitleFont.Name']:=Value.Name;

    FValues.Values['titleFont.Style.Bold']:=setIniStr(fsBold in Value.Style);
    FValues.Values['titleFont.Style.Underline']:=setIniStr(fsUnderline in Value.Style);
    FValues.Values['titleFont.Style.Italic']:=setIniStr(fsItalic in Value.Style);
    FValues.Values['titleFont.Style.StrikeOut']:=setIniStr(fsStrikeOut in Value.Style);
  end;
end;

procedure tdbGridsaver.setReadOnly(Value:boolean);
begin
  if Assigned(thisGrid) and not DefaultReadOnly
  then begin
    ThisGrid.ReadOnly:=Value;
    CheckList;
    FValues.values['ReadOnly']:=SetIniStr(Value);
  end;
end;

function tdbGridSaver.GetIndicator:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgIndicator in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetTabStop:boolean;
begin
  if Assigned(ThisGrid) then Result:=thisGrid.tabstop
  else Result:=false;
end;

function tdbGridSaver.GetAlwaysShowEditor:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgAlwaysShowEditor in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetAlwaysShowSelection:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgAlwaysShowSelection in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetRowLines:Boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgRowLines in Thisgrid.Options)
  else Result:=false;
end;


function tdbGridSaver.GetColLines:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgColLines in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetMultiSelect:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgMultiSelect in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetCheckTitle:boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgTitles in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.GetSelectRows:Boolean;
begin
  if Assigned(ThisGrid) then Result:=(dgRowSelect in Thisgrid.Options)
  else Result:=false;
end;

function tdbGridSaver.Getcolor:TColor;
begin
  if Assigned(ThisGrid) then Result:=ThisGrid.color
  else Result:=0;
end;

function tdbGridSaver.GetFixedcolor:TColor;
begin
  if Assigned(ThisGrid) then Result:=ThisGrid.Fixedcolor
  else Result:=0;
end;

function tdbGridSaver.GetFont:TFont;
begin
  if Assigned(thisGrid) then result:=Thisgrid.Font
  else Result:=nil;
end;

function tdbGridSaver.GetTitleFont:TFont;
begin
  if Assigned(thisGrid) then result:=Thisgrid.TitleFont
  else Result:=nil;
end;

function tdbGridSaver.GetReadOnly:boolean;
begin
  if Assigned(ThisGrid) then Result:=ThisGrid.readOnly
  else Result:=false;
end;

function tdbGridSaver.GetFixedCols:integer;
begin
  if Assigned(thisGrid) then begin
    if ThisGrid is tbvdbGrid then Result:=(ThisGrid as tbvdbGrid).FixedCols
    //else if ThisGrid is trxdbGrid then Result:=(ThisGrid as trxdbGrid).FixedCols
    else Result:=-1
  end
  else Result:=-1;
end;

function tdbGridSaver.GetDefaultReadOnly:boolean;
begin
  Result:=FDefaultReadOnly;
end;

function tdbGridSaver.IsInserted(Value:integer):boolean;
var i:integer;
    Lo,Hi:integer;
begin
  if (Inserted=nil) then Result:=false
  else begin
    Lo:=Low(Inserted);
    Hi:=High(Inserted);
//    if Hi=Lo then Result:=false
    {else}
    begin
      for i:=Lo to Hi
        do if Inserted[i]=Value then break;
      if i>hi then Result:=false
      else Result:=true;
    end;
  end
end;

{$ifndef LINUX}
function CheckOldIniFormat:boolean;

var
    ThisIniFile:TRegInifile;
    ThDir,thStr:string;
    Sectionnames,SectionValues:TStringList;
    i:integer;
begin
  Result:=true;
  with TWaitCursor.create do
  try
  try
    ThDir:=Application.ExeName;
    thDir:=StringReplace(thDir,'\','/',[rfReplaceAll]);
    thDir:='Software\BV\'+thDir+'\grids';

    ThisIniFile:=TRegIniFile.Create(thDir);
//    if ThisIniFile.OpenKey(thdir,false) then begin

      CheckIniDir;

      SEctionNames:=TStringList.Create;
      SEctionValues:=TstringList.create;
      try
        ThisIniFile.ReadSections(SectionNames);

        if SEctionnames.count>0 then
        for i:=0 to SectionNames.Count-1 do begin
           thStr:=SectionNames.Strings[i];
           SEctionValues.clear;
           ThisIniFile.ReadSectionValues(thstr,SectionValues);
           SectionValues.SaveToFile(GetINIFilename(nil,thstr));
        end;

        if SEctionNames.count>0 then ThisIniFile.EraseSection('' {thDir});
      finally
        ThisIniFile.Free;
        SEctionNames.free;
        SectionValues.Free;
      end;
//    end;
  finally
    Free;
  end
  except
    on e:exception do begin
       REsult:=false;
       bvMessageError(StrErrorReadProperties+#13
                                     +StrMessage+': '+e.Message);
    end
  end;
end;
{$endif}

initialization
  AllSaversEnabled:=true;

end.
