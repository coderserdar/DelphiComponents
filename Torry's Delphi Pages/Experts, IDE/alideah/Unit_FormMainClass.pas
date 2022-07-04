unit Unit_FormMainClass;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;


type
  TFormResult = ( frCancel, frLoad, frUnload, frUpdate, frDoNothing );

  TFormAutoHide = class(TForm)
    PageControl1: TPageControl;
    TabInclude: TTabSheet;
    PanelRight: TPanel;
    ButtonLoad: TButton;
    ButtonCancel: TButton;
    ButtonUpdate: TButton;
    TabSheetOptions: TTabSheet;
    Label1: TLabel;
    CBoxCaptionButton: TCheckBox;
    CBoxShutdown: TCheckBox;
    Label2: TLabel;
    CBoxModal: TCheckBox;
    CBoxSound: TCheckBox;
    Panel1: TPanel;
    ButtonAbout: TButton;
    Panel2: TPanel;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDel: TButton;
    Panel3: TPanel;
    LBoxClassname: TListBox;
    CBoxInit: TCheckBox;
    EditInterval: TEdit;
    UpDownInterval: TUpDown;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure LBoxClassnameDblClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure LBoxClassnameClick(Sender: TObject);
    procedure LBoxClassnameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAboutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBoxClassnameDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure OnChangeAnything(Sender: TObject);
    procedure EditIntervalExit(Sender: TObject);
    procedure EditIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure UpDownIntervalChanging(Sender: TObject;
      var AllowChange: Boolean);
  private
    { Private declarations }
    FEdited: boolean;
    FBitmap: TBitmap;

    procedure UpdateButtons;
    procedure UpdateChanges;
    procedure RegistryFirstTime;
    procedure RegistryClear;
    procedure RegistryUpdate ( aItems: TStrings );
    procedure RegistryUpdateDefault;
    procedure RegistryRead ( aItems: TStrings );
    procedure RegistryReadDefault;
  public
    { Public declarations }
    FResult: TFormResult;
    procedure AutohideLoaded;
  end;

var
  FormAutoHide: TFormAutoHide;

// -----------------------------------------------------------------------------

implementation

uses
    Unit_FormAddClass, Unit_FormAbout, Unit_Constants, Unit_Misc, Registry, Math;

{$R *.dfm}
{$R AutoHide.Res}

// -----------------------------------------------------------------------------


procedure TFormAutoHide.RegistryFirstTime;
var
    aRegistry: TRegistry;
    aBuffer: string;
    aItems, aDefTempID, aIDList: TStringList;
    aID: integer;
    count, counter: integer;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, FALSE ) ) then begin
        count := aRegistry.ReadInteger ( REG_WINDOWS_COUNT );
        aRegistry.CloseKey;

        if ( count > 0 ) then
            exit;
        end;
finally
    aRegistry.Free;
    end;

    
aItems := TStringList.Create;
try
    try
        aIDList := TStringList.Create;
        try
            Template_GetIDList ( aIDList );
            try
                aDefTempID := TStringList.Create;
                try
                    SetLength ( aBuffer, 512 );
                    LoadString ( HInstance, RES_TEMPLATE_DEFAULT, PChar(aBuffer), 511 );
                    SetLength ( aBuffer, StrLen( PChar( aBuffer ) ) );
                    aDefTempID.CommaText := aBuffer;

                    count := aDefTempID.count - 1;
                    for counter := 0 to count do begin
                        try
                            aID := strtoint ( aDefTempID[counter] );
                            if ( aID < 0 ) or ( aID >= aIDList.Count ) then continue;
                            Template_GetItems ( aIDList[aID], aIDList, aItems );
                        except
                            continue;
                            end;
                        end; // for

                    RegistryUpdate ( aItems );

                finally
                    aDefTempID.Free;
                    end;
            finally
                Template_FreeIDList ( aIDList );
                end;
        finally
            aIDList.Free;
            end;
    finally
        Template_FreeItems ( aItems );
        end;
finally
    aItems.Free;
    end;

end;



procedure TFormAutoHide.RegistryClear;
var
    count, counter: integer;
    aRegistry: TRegistry;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, FALSE ) ) then begin
        try
            count := aRegistry.ReadInteger ( REG_WINDOWS_COUNT ) - 1;
            for counter := 0 to count do
                aRegistry.DeleteValue ( inttostr(counter) );

            aRegistry.DeleteValue ( REG_WINDOWS_COUNT );
        except
            end;
        aRegistry.CloseKey;
        end; // if

finally
    aRegistry.Free;
    end;
end;


procedure TFormAutoHide.RegistryUpdate ( aItems: TStrings );
var
    count, counter: integer;
    aRegistry: TRegistry;
    aAHProp: PWndPropAH;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, TRUE ) ) then begin
        try
            aRegistry.WriteInteger ( REG_WINDOWS_COUNT, aItems.Count );
            count := aItems.Count - 1;
            for counter := 0 to count do begin
                aAHProp := PWndPropAH ( aItems.Objects[counter] );
                aRegistry.WriteString ( inttostr ( counter ),
                                        aItems[counter] + AHWndPropToStr ( aAHProp ) );
                end; // for
        except
            end;
        aRegistry.CloseKey;
        end; // if

finally
    aRegistry.Free;
    end;
end;



procedure TFormAutoHide.RegistryRead ( aItems: TStrings );
var
    count, counter: integer;
    aCor: integer;
    aItem: string;
    aRegistry: TRegistry;
    aAHProp: PWndPropAH;
begin
LBoxClassname.Items.Clear;

aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEYINCLUDE, FALSE ) ) then begin
        try
           count := aRegistry.ReadInteger ( REG_WINDOWS_COUNT ) - 1;
           for counter := 0 to count do begin
               aItem := aRegistry.ReadString ( inttostr(counter) );
               aCor := Pos ( SIGNATURE_HEAD, aItem );
               if ( Length ( aItem ) = 0 ) or ( aCor = 1 ) then continue;

               new ( aAHProp );
               StrToAHWndProp ( aItem, aAHProp^ );

               if ( aCor <> 0 ) then
                   delete ( aItem, aCor, MAXINT );

               aItems.AddObject ( aItem, TObject(aAHProp) );
               end;
        except
            end;
        aRegistry.CloseKey;
        end; // if

finally
    aRegistry.Free;
    end;

UpdateButtons;
end;


procedure TFormAutoHide.RegistryUpdateDefault;
var
    aRegistry: TRegistry;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEY, TRUE ) ) then begin
        aRegistry.WriteInteger ( REG_INTERVAL, UpDownInterval.Position );
        aRegistry.WriteBool ( REG_MODALRESTORE, CBoxModal.Checked );
        aRegistry.WriteBool ( REG_CAPTIONBUTTON, CBoxCaptionButton.Checked );
        aRegistry.WriteBool ( REG_DELPHILOADINIT, CBoxInit.Checked );
        aRegistry.WriteBool ( REG_DELPHICLOSEKILL, CBoxShutdown.Checked );
        //aRegistry.WriteBool ( REG_XPSTYLE, CBoxWinXP.Checked );
        aRegistry.WriteBool ( REG_SOUND, CBoxSound.Checked );

        aRegistry.CloseKey;
        end;

finally
    aRegistry.Free;
    end;

end;



procedure TFormAutoHide.RegistryReadDefault;
var
    aRegistry: TRegistry;
begin
aRegistry := TRegistry.Create;
try
    aRegistry.RootKey := HKEY_CURRENT_USER;

    if ( aRegistry.OpenKey ( ROOT_KEY, FALSE ) ) then begin
        UpDownInterval.Position := Max ( RegReadIntegerDef ( aRegistry,
                                    REG_INTERVAL, REG_DEFVAL_INTERVAL ),
                                    TIMER_MIN_VALUE );

        CBoxModal.Checked := RegReadBoolDef ( aRegistry,
                                    REG_MODALRESTORE, REG_DEFVAL_MODALRESTORE );
        CBoxCaptionButton.Checked := RegReadBoolDef ( aRegistry,
                                    REG_CAPTIONBUTTON, REG_DEFVAL_CAPTIONBUTTON );
        CBoxInit.Checked := RegReadBoolDef ( aRegistry,
                                    REG_DELPHILOADINIT, REG_DEFVAL_DELPHILOADINIT );
        CBoxShutdown.Checked := RegReadBoolDef ( aRegistry,
                                    REG_DELPHICLOSEKILL, REG_DEFVAL_DELPHICLOSEKILL );
        //CBoxWinXP.Checked := RegReadBoolDef ( aRegistry,
        //                            REG_XPSTYLE, REG_DEFVAL_XPSTYLE );
        CBoxSound.Checked := RegReadBoolDef ( aRegistry,
                                    REG_SOUND, REG_DEFVAL_SOUND );

        aRegistry.CloseKey;
        end
    else begin
        UpDownInterval.Position := REG_DEFVAL_INTERVAL;
        CBoxModal.Checked := REG_DEFVAL_MODALRESTORE;
        CBoxCaptionButton.Checked := REG_DEFVAL_CAPTIONBUTTON;
        CBoxInit.Checked := REG_DEFVAL_DELPHILOADINIT;
        CBoxShutdown.Checked := REG_DEFVAL_DELPHICLOSEKILL;
        //CBoxWinXP.Checked := REG_DEFVAL_XPSTYLE;
        CBoxSound.Checked := REG_DEFVAL_SOUND;
        end;

finally
    aRegistry.Free;
    end;
end;

// -----------------------------------------------------------------------------


procedure TFormAutoHide.AutohideLoaded;
begin
ButtonLoad.Caption := '&Unload';
ButtonUpdate.Enabled := FALSE;
end;


procedure TFormAutoHide.UpdateButtons;
begin
ButtonUpdate.Enabled := FEdited;

ButtonAdd.Enabled := TRUE;
ButtonEdit.Enabled := ( LBoxClassname.Items.Count > 0 )
                       and ( LBoxClassname.ItemIndex >= 0 )
                       and ( LBoxClassname.Selected[LBoxClassname.ItemIndex] );
ButtonDel.Enabled := ( LBoxClassname.SelCount > 0 );
end;


procedure TFormAutoHide.UpdateChanges;
begin
if ( FEdited ) then begin
    FEdited := FALSE;
    UpdateButtons;

    RegistryClear;
    RegistryUpdate ( LBoxClassname.Items );
    RegistryUpdateDefault;
    end;
end;


// -----------------------------------------------------------------------------


procedure TFormAutoHide.FormCreate(Sender: TObject);
begin
LBoxClassname.DoubleBuffered := TRUE;

FBitmap := TBitmap.Create;
FBitmap.LoadFromResourceName ( HInstance, 'IMG_BMP_STATES' );

RegistryReadDefault;
RegistryFirstTime;
RegistryRead ( LBoxClassname.Items );

FEdited := FALSE;
UpdateButtons;
end;


procedure TFormAutoHide.FormDestroy(Sender: TObject);
begin
Template_FreeItems ( LBoxClassname.Items );
FBitmap.Free;
end;


procedure TFormAutoHide.ButtonCancelClick(Sender: TObject);
begin
FResult := frCancel;
Close;
end;


procedure TFormAutoHide.ButtonLoadClick(Sender: TObject);
begin
if ( ButtonLoad.Caption = '&Load' ) then begin
    if ( FEdited ) then begin
        case ( MessageDlg ( 'Save changes to the AutoHide Properties?',
                                mtConfirmation, mbYesNoCancel, 0 ) ) of
            mrYes: UpdateChanges;
            mrNo: begin { do nothing } end;
            mrCancel: exit;
            end; // case
        end; // if

    FResult := frLoad;
    end
else
    FResult := frUnload;

Close;
end;


procedure TFormAutoHide.ButtonUpdateClick(Sender: TObject);
begin
UpdateChanges;

FResult := frUpdate;
if ( ButtonLoad.Caption <> '&Load' ) then
    Close;
end;


procedure TFormAutoHide.ButtonAddClick(Sender: TObject);
var
    aForm: TFormAddClass;
    aAHProp: TWndPropAH;
    aNewAHProp: PWndPropAH;
    aClassname: string;
    aItems, aIDList: TStrings;
begin
aItems := LBoxClassname.Items;

aForm := TFormAddClass.Create ( Self );
try
    aForm.InitTemplate;
    if ( aForm.ShowModal = mrOK ) then begin
        FEdited := TRUE;
        aClassname := Trim ( aForm.CBoxClassName.Text );
        aForm.GetDescriptions ( aAHProp );
        aIDList := aForm.CBoxClassname.Items;

        if ( aIDList.IndexOf ( aClassname ) <> - 1 ) then
            Template_GetItems ( aClassname, aIDList, aItems, @aAHProp )
        else begin
            new ( aNewAHProp );
            Move ( aAHProp, aNewAHProp^, sizeof(aNewAHProp^) );
            aItems.AddObject ( aClassname, TObject(aNewAHProp) );
            end;
        end; // if

finally
    aForm.Free;
    end;

UpdateButtons;
end;


procedure TFormAutoHide.ButtonEditClick(Sender: TObject);
var
    aForm: TFormAddClass;
    aListBox: TListBox;
    aIndex: integer;
    aAHProp: PWndPropAH;
begin
aListBox := LBoxClassname;

if ( aListBox.ItemIndex <> -1 ) then begin
    aForm := TFormAddClass.Create ( Self );
    try
        aForm.Caption := 'Edit Class';
        aIndex := aListBox.ItemIndex;
        aForm.CBoxClassName.Text := aListBox.Items[aIndex];
        aAHProp := PWndPropAH ( aListBox.Items.Objects[aIndex] );
        aForm.SetDescriptions ( aAHProp );

        if ( aForm.ShowModal <> mrCancel ) then begin
            FEdited := TRUE;
            aListBox.Items[aIndex] := aForm.CBoxClassName.Text;
            if ( aAHProp = NIL ) then
                new ( aAHProp );

            aForm.GetDescriptions ( aAHProp^ );
            aListBox.Items.Objects[aIndex] := TObject(aAHProp);
            aListBox.Selected[aIndex] := TRUE;

            UpdateButtons;
            end; // if
    finally
        aForm.Free;
        end;
    end; // if
end;


procedure TFormAutoHide.LBoxClassnameDblClick(Sender: TObject);
var
    aListBox: TListBox;
begin
aListBox := TListBox ( Sender );
if ( aListBox.ItemIndex <> -1 ) and ( aListBox.Selected[aListBox.ItemIndex] ) then 
    ButtonEditClick ( Self );
end;


procedure TFormAutoHide.ButtonDelClick(Sender: TObject);
var
    aListBox: TListBox;
    aCount: integer;
    count, counter: integer;
    aAHProp: PWndPropAH;
begin
aListBox := LBoxClassname;

if ( aListBox.SelCount > 0 ) then begin
    aCount := aListBox.SelCount;
    count := aListBox.Items.Count - 1;
    for counter := count downto 0 do begin
        if ( aListBox.Selected[counter] ) then begin
            aAHProp := PWndPropAH ( aListBox.Items.Objects[counter] );
            dispose ( aAHProp );
            aListBox.Items.Delete ( counter );
            FEdited := TRUE;
            dec ( aCount );
            if ( aCount = 0 ) then break;
            end;
        end; // for
    end;

UpdateButtons;
end;


procedure TFormAutoHide.LBoxClassnameClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TFormAutoHide.LBoxClassnameKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
UpdateButtons;
end;

procedure TFormAutoHide.FormShow(Sender: TObject);
begin
UpdateButtons;
ClientHeight := 221;
end;

procedure TFormAutoHide.PageControl1Change(Sender: TObject);
begin
UpdateButtons;
end;


procedure TFormAutoHide.ButtonAboutClick(Sender: TObject);
var
    aForm: TFormAbout;
begin
aForm := TFormAbout.Create ( Self );
try
    aForm.ShowModal;
finally
    aForm.Free;
    end;
end;


procedure TFormAutoHide.LBoxClassnameDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
  procedure GetRect ( var aRect: TRect; aPos, aSize: integer );
  begin
  aRect.Left := aPos*aSize;
  aRect.Top := 0;
  aRect.Right := aRect.Left+aSize;
  aRect.Bottom := aRect.Top + aSize;
  end;
var
    aAHProp: PWndPropAH;
    aCanvas: TCanvas;
    aDRect, aSRect: TRect;
    aString: string;
    aIndex, aWidth: integer;
begin
aCanvas := LBoxClassname.Canvas;

if ( odSelected in State ) then begin
    aCanvas.Brush.Color := clHighLight;
    aCanvas.Font.Color := clHighLightText;
    end
else begin
    aCanvas.Brush.Color := clWindow;
    aCanvas.Font.Color := clWindowText;
    end;

aCanvas.FillRect ( Rect );

SetRect ( aDRect, Rect.Left+2, Rect.Top+3, Rect.Left+13, Rect.Top+14 );
aAHProp := PWndPropAH ( LBoxClassname.Items.Objects[Index] );

if ( aAHProp <> NIL ) then begin
    aIndex := 0;
    case ( aAHProp^.FLockType ) of
        ltUnLock:     aIndex := 0;
        ltLockExpand: aIndex := 1;
        ltLockShrink: aIndex := 2;
        end; // case
    GetRect ( aSRect, aIndex, 11 );
    aCanvas.BrushCopy ( aDRect, FBitmap, aSRect, clTeal );
    OffsetRect ( aDRect, 13, 0 );

    case ( aAHProp^.FShrinkType ) of
        stCaption:      aIndex := 3;
        stShortCaption: begin
            if ( aAHProp^.FDirection in [sdirTop, sdirRight] ) then
                aIndex := 4
            else
                aIndex := 5;
            end;
        stUltraThin: begin
            case ( aAHProp^.FDirection ) of
                sdirTop:     aIndex := 6;
                sdirLeft:    aIndex := 7;
                sdirBottom:  aIndex := 8;
                sdirRight:   aIndex := 9;
                end;
            end;
        end;
    GetRect ( aSRect, aIndex, 11 );
    aCanvas.BrushCopy ( aDRect, FBitmap, aSRect, clTeal );
    OffsetRect ( aDRect, 11, 0 );

    case ( aAHProp^.FCaptionOnShrink ) of
        saNone:     aIndex := 10;
        saTop:      aIndex := 11;
        saTopMost:  aIndex := 12;
        end;
    GetRect ( aSRect, aIndex, 11 );
    aCanvas.BrushCopy ( aDRect, FBitmap, aSRect, clTeal );
    OffsetRect ( aDRect, 14, 0 );

    aString := '[';
    aWidth := aCanvas.TextWidth ( aString );
    aCanvas.TextOut ( aDRect.Left, aDRect.Top-1, aString );
    OffsetRect ( aDRect, aWidth+1, 0 );

    aString := inttostr ( aAHProp^.FUTWeight );
    aWidth := aCanvas.TextWidth ( aString );
    aCanvas.TextOut ( aDRect.Left, aDRect.Top-1, aString );
    OffsetRect ( aDRect, aWidth, 0 );


    case ( aAHProp^.FUTOnShrink ) of
        saNone:     aIndex := 10;
        saTop:      aIndex := 13;
        saTopMost:  aIndex := 14;
        end;
    GetRect ( aSRect, aIndex, 11 );
    aCanvas.BrushCopy ( aDRect, FBitmap, aSRect, clTeal );
    OffsetRect ( aDRect, 11, 0 );

    aString := ']';
    aWidth := aCanvas.TextWidth ( aString ) + 4;
    aCanvas.TextOut ( aDRect.Left, aDRect.Top-1, aString );
    OffsetRect ( aDRect, aWidth, 0 );
    end;

aCanvas.TextOut ( aDRect.Left, aDRect.Top-1, LBoxClassname.Items[Index] );
SetTextColor ( aCanvas.Handle, $ffffff );
end;

procedure TFormAutoHide.OnChangeAnything(Sender: TObject);
begin
FEdited := TRUE;
UpdateButtons;
end;

procedure TFormAutoHide.EditIntervalExit(Sender: TObject);
begin
EditInterval.Text := inttostr ( UpDownInterval.Position );
end;

procedure TFormAutoHide.EditIntervalKeyPress(Sender: TObject;
  var Key: Char);
begin
if ( Key in [#8, '0'..'9'] ) then
    OnChangeAnything ( Self )
else    
    Key := #10;
end;

procedure TFormAutoHide.UpDownIntervalChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
OnChangeAnything ( Self );
end;

end.
