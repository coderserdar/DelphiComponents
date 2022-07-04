unit Unit_FormAddClass;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Unit_Misc, ComCtrls;

type
  TFormAddClass = class(TForm)
    CBoxClassName: TComboBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Label2: TLabel;
    Label3: TLabel;
    CBoxShrinkType: TComboBox;
    PanelCapture: TPanel;
    ImageCapture: TImage;
    Label1: TLabel;
    CBoxLockType: TComboBox;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    CBoxCaptionOnShrink: TComboBox;
    Label9: TLabel;
    CBoxUTOnShrink: TComboBox;
    Label10: TLabel;
    Label7: TLabel;
    CBoxDirection: TComboBox;
    EditUTWidth: TEdit;
    UpDownUTWidth: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageCaptureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageCaptureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageCaptureMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CBoxClassNameChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBoxClassNameDropDown(Sender: TObject);
    procedure EditUTWidthExit(Sender: TObject);
    procedure EditUTWidthKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FSearchWnd: THandle;
    FMouseDown: boolean;
    FDroppedDown: boolean;
  public
    { Public declarations }
    procedure InitTemplate;
    procedure GetDescriptions ( var aAHProp: TWndPropAH );
    procedure SetDescriptions ( aAHProp: PWndPropAH );
    procedure ResetDescriptions;
  end;

var
  FormAddClass: TFormAddClass;

// -----------------------------------------------------------------------------

implementation

uses
    Unit_Constants, Math;
    
{$R *.dfm}


// -----------------------------------------------------------------------------


procedure TFormAddClass.FormCreate(Sender: TObject);
begin
ClientHeight := 200;
ImageCapture.Picture.Bitmap.LoadFromResourceName ( HInstance, 'IMG_BMP_SEARCHWND' );
CBoxClassName.Text := '';
ResetDescriptions;   
end;


procedure TFormAddClass.FormDestroy(Sender: TObject);
begin
Template_FreeIDList ( CBoxClassName.Items );
end;


procedure TFormAddClass.ResetDescriptions;
begin
CBoxShrinkType.ItemIndex := 0;
CBoxLockType.ItemIndex := 0;
CBoxCaptionOnShrink.ItemIndex := 1; // Top
CBoxDirection.ItemIndex := 0;
CBoxUTOnShrink.ItemIndex := 2; // TopMost
end;


procedure TFormAddClass.InitTemplate;
begin
Template_GetIDList ( CBoxClassName.Items );
end;


procedure TFormAddClass.FormShow(Sender: TObject);
begin
ButtonOK.Enabled := Trim ( CBoxClassName.Text ) <> '';
end;


procedure TFormAddClass.CBoxClassNameChange(Sender: TObject);
var
    aIndex: integer;
    aAHProp: PWndPropAH;
begin
ButtonOK.Enabled := Trim ( CBoxClassName.Text ) <> '';
if ( FDroppedDown ) then begin
    FDroppedDown := FALSE;
    aIndex := CBoxClassName.Items.IndexOf ( CBoxClassName.Text );
    if ( aIndex <> - 1 ) then begin
        aAHProp := PWndPropAH ( CBoxClassName.Items.Objects[aIndex] );
        SetDescriptions ( aAHProp );
        end;
    end;
end;


procedure TFormAddClass.ImageCaptureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
SetCapture ( PanelCapture.Handle );
FMouseDown := TRUE;
Screen.Cursor := crHandPoint;
ImageCapture.Hide;
end;


procedure TFormAddClass.ImageCaptureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if ( FSearchWnd <> 0 ) then
    HighlightWindow ( FSearchWnd, FALSE );

ImageCapture.Show;
Screen.Cursor := crDefault;
FMouseDown := FALSE;
ReleaseCapture;
end;


procedure TFormAddClass.ImageCaptureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
    aWnd: THandle;
begin
if ( FMouseDown ) then begin
    aWnd := WndFromPoint;
    if ( FSearchWnd = aWnd ) then exit;

    if ( aWnd <> 0 ) and ( aWnd <> Handle ) then begin
        if ( FSearchWnd <> 0 ) then
            HighlightWindow ( FSearchWnd, FALSE );

        FSearchWnd := aWnd;
        HighlightWindow ( aWnd, TRUE );
        CBoxClassName.Text := GetWindowClassName ( aWnd );
        end
    else begin
         if ( FSearchWnd <> 0 ) then
            HighlightWindow ( FSearchWnd, FALSE );

         FSearchWnd := 0;
         CBoxClassName.Text := '';
         end;

    CBoxClassNameChange ( Self );
    end;
end;


procedure TFormAddClass.EditUTWidthExit(Sender: TObject);
begin
EditUTWidth.Text := inttostr ( UpDownUTWidth.Position );
end;


procedure TFormAddClass.EditUTWidthKeyPress(Sender: TObject;
  var Key: Char);
begin
if not ( Key in [#8, '0'..'9'] ) then
    Key := #0;
end;


procedure TFormAddClass.GetDescriptions ( var aAHProp: TWndPropAH );
const
    CLockType: array [0..2] of TLockType = ( ltUnlock, ltLockExpand, ltLockShrink );
    CShrinkTypes: array [0..2] of TShrinkType = ( stCaption, stShortCaption, stUltraThin );
    CDirection: array [0..3] of TShrinkDirection = ( sdirTop, sdirLeft, sdirBottom, sdirRight );
    COnShrink: array [0..2] of TShrinkAction = ( saNone, saTop, saTopMost );
    COnShrink2: array [0..2] of TShrinkAction = ( saNone, saTop, saTopMost );
begin
with aAHProp do begin
    FLockType := CLockType[CBoxLockType.ItemIndex];
    FShrinkType := CShrinkTypes[CBoxShrinkType.ItemIndex];
    FCaptionOnShrink := COnShrink[CBoxCaptionOnShrink.ItemIndex];
    FDirection := CDirection[CBoxDirection.ItemIndex];
    FUTOnShrink := COnShrink2[CBoxUTOnShrink.ItemIndex];
    FUTWeight := UpDownUTWidth.Position;
    end; // with
end;


procedure TFormAddClass.SetDescriptions ( aAHProp: PWndPropAH );
const
    CLockType: array [TLockType] of integer = ( 0, 1, 2 );
    CShrinkTypes: array [TShrinkType] of integer = ( 0, 1, 2, 0 );
    CDirection: array [TShrinkDirection] of integer = ( 0, 1, 2, 3, 0 );
    COnShrink: array [TShrinkAction] of integer = ( 0, 1, 2 );
begin
if ( aAHProp = NIL ) then exit;

CBoxLockType.ItemIndex := CLockType[aAHProp.FLockType];
CBoxShrinkType.ItemIndex := CShrinkTypes[aAHProp.FShrinkType];
CBoxCaptionOnShrink.ItemIndex := COnShrink[aAHProp.FCaptionOnShrink];
UpDownUTWidth.Position := Max ( 1, aAHProp.FUTWeight );
CBoxDirection.ItemIndex := CDirection[aAHProp.FDirection];
CBoxUTOnShrink.ItemIndex := COnShrink[aAHProp.FUTOnShrink];
end;


procedure TFormAddClass.CBoxClassNameDropDown(Sender: TObject);
begin
FDroppedDown := TRUE;
end;


procedure TFormAddClass.ButtonOKClick(Sender: TObject);
begin
if ( CBoxShrinkType.ItemIndex = 2 ) and ( CBoxLockType.ItemIndex = 2 ) then begin
    CBoxShrinkType.SetFocus;
    MessageDlg ( ULTRATHINSHRINKLOCK_ERROR, mtError, [mbOK], 0 );
    exit;
    end;

Close;
ModalResult := mrOK;
end;



end.
