unit MainFrm;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls, Grids, TypInfo, Menus, ELPropInsp;

type
    TfrmMain = class(TForm)
        ELPropertyInspector1: TELPropertyInspector;
        Label3: TLabel;
        pnlContainer: TPanel;
        Panel1: TPanel;
        CheckBox1: TCheckBox;
        Memo1: TMemo;
        Label2: TLabel;
        GroupBox1: TGroupBox;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Button7: TButton;
        Button10: TButton;
        Shape1: TShape;
        GroupBox2: TGroupBox;
        CheckBox2: TCheckBox;
        Memo3: TMemo;
        CheckBox3: TCheckBox;
        Label1: TLabel;
        Memo2: TMemo;
        Label4: TLabel;
        GroupBox3: TGroupBox;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        RadioButton3: TRadioButton;
        RadioButton4: TRadioButton;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure ELPropertyInspector1FilterProp(Sender: TObject;
            AInstance: TPersistent; APropInfo: PPropInfo;
            var AIncludeProp: Boolean);
        procedure Button7Click(Sender: TObject);
        procedure Button10Click(Sender: TObject);
        procedure ELPropertyInspector1Click(Sender: TObject);
        procedure ELPropertyInspector1Change(Sender: TObject);
        procedure CheckBox2Click(Sender: TObject);
        procedure CheckBox3Click(Sender: TObject);
        procedure ELPropertyInspector1GetComponents(Sender: TObject;
            AClass: TComponentClass; AResult: TList);
        procedure ELPropertyInspector1GetComponentName(Sender: TObject;
            AComponent: TComponent; var AName: string);
        procedure ELPropertyInspector1GetComponent(Sender: TObject;
            const AComponentName: string; var AComponent: TComponent);
        procedure RadioButton1Click(Sender: TObject);
        procedure ELPropertyInspector1GetCaptionColor(Sender: TObject;
            APropTypeInfo: PTypeInfo; const APropName: string;
            var AColor: TColor);
    private
        { Private declarations }
        FEditorClass: TElPropEditorClass;
        procedure UpdateObjectsMemo;
    public
        { Public declarations }
    end;

var
    frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
    BevelCutFrm;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Panel1);
    UpdateObjectsMemo;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(CheckBox1);
    UpdateObjectsMemo;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Memo1);
    UpdateObjectsMemo;
end;

procedure TfrmMain.UpdateObjectsMemo;
var
    LI: Integer;
begin
    Memo2.Lines.Clear;
    for LI := 0 to ELPropertyInspector1.ObjectCount - 1 do
        Memo2.Lines.Add(ELPropertyInspector1.Objects[LI].GetNamePath);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Memo1.Font);
    UpdateObjectsMemo;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Shape1.Brush);
    UpdateObjectsMemo;
end;

procedure TfrmMain.ELPropertyInspector1FilterProp(Sender: TObject;
    AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
    // Example filter properties
    if CheckBox2.Checked and (Memo3.Lines.IndexOf(APropInfo^.Name) <> -1) then
        AIncludeProp := False;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Memo1);
    ELPropertyInspector1.Add(Panel1);
    ELPropertyInspector1.Add(CheckBox1);
    ELPropertyInspector1.Add(Label2);
    UpdateObjectsMemo;
end;

procedure TfrmMain.Button10Click(Sender: TObject);
begin
    ELPropertyInspector1.Clear;
    ELPropertyInspector1.Add(Label2);
    UpdateObjectsMemo;
end;

procedure TfrmMain.ELPropertyInspector1Click(Sender: TObject);
begin
    Label3.Caption := 'Active Property: ' + ELPropertyInspector1.ActiveItem.Caption;
end;

procedure TfrmMain.ELPropertyInspector1Change(Sender: TObject);
begin
    ELPropertyInspector1Click(nil);
end;

procedure TfrmMain.CheckBox2Click(Sender: TObject);
begin
    ELPropertyInspector1.UpdateItems;
end;

procedure TfrmMain.CheckBox3Click(Sender: TObject);
begin
    ELPropertyInspector1.UpdateItems;
end;

procedure TfrmMain.ELPropertyInspector1GetComponents(Sender: TObject;
    AClass: TComponentClass; AResult: TList);
var
    LI: Integer;
begin
    // Add to AResult list only controls you want to appear
    // in the drop-down list of the component reference property editor
    // (for examle in PopupMenu or FocusControl property). This event is used
    // only for generating drop-down list, but does not restrict user to
    // type any other component name directly in the property inspector.
    // Real restriction mast be done in the OnGetComponent event handler.
    // In this examle only proper children of pnlContainer will be added.
    //
    // See also: OnGetComponentName, OnGetComponent
    //
    // This events can be used to flexible control component reference
    // property editors behavior.

    for LI := 0 to pnlContainer.ControlCount - 1 do
        { Check class compability }
        if (pnlContainer.Controls[LI].ClassType = AClass) or
            (pnlContainer.Controls[LI].InheritsFrom(AClass)) then
            AResult.Add(pnlContainer.Controls[LI]);
end;

procedure TfrmMain.ELPropertyInspector1GetComponentName(
    Sender: TObject; AComponent: TComponent; var AName: string);
begin
    // Provide string that will be used as the name of the
    // component in the values column and drop-down list of the
    // component reference property editors. You can return string, that
    // differs from TComponent.Name, but you mast be available to find
    // component reference (pointer to component) using this string in the
    // OnGetComponent event handler.
    //
    // See also: OnGetGetComponents, OnGetComponent
    //
    // This events can be used to flexible control component reference
    // property editors behavior.

    AName := AComponent.Name;
end;

procedure TfrmMain.ELPropertyInspector1GetComponent(Sender: TObject;
    const AComponentName: string; var AComponent: TComponent);
begin
    // Search for component using string provided in the OnGetComponentName
    // event hadler. In this event you can restrict number of components used by
    // property inspector as component reference targets.
    // In this examle only children of pnlContainer can be used (by chousing from
    // drop-down list or typing directly) as component reference targets.
    //
    // See also: OnGetGetComponents, OnGetComponentName
    //
    // This events can be used to flexible control component reference
    // property editors behavior.

    AComponent := pnlContainer.FindChildControl(AComponentName);
end;

procedure TfrmMain.RadioButton1Click(Sender: TObject);
begin
    if FEditorClass <> nil then
        ELPropertyInspector1.UnregisterPropEditor(TypeInfo(TBevelCut),
            nil, 'BevelInner', FEditorClass);
    if RadioButton2.Checked then
        begin
            FEditorClass := TBevelCutProperty;
            ELPropertyInspector1.RegisterPropEditor(TypeInfo(TBevelCut), nil, 'BevelInner', FEditorClass);
        end
    else
        if RadioButton3.Checked then
            begin
                FEditorClass := TBevelCutDlgProperty;
                ELPropertyInspector1.RegisterPropEditor(TypeInfo(TBevelCut), nil, 'BevelInner', FEditorClass);
            end
        else
            if RadioButton4.Checked then
                begin
                    FEditorClass := TBevelCutDrawProperty;
                    ELPropertyInspector1.RegisterPropEditor(TypeInfo(TBevelCut), nil, 'BevelInner', FEditorClass);
                end;
end;

procedure TfrmMain.ELPropertyInspector1GetCaptionColor(
    Sender: TObject; APropTypeInfo: PTypeInfo; const APropName: string;
    var AColor: TColor);
begin
    if CheckBox3.Checked and (Memo3.Lines.IndexOf(APropName) <> -1) then AColor := clRed;
end;

end.

