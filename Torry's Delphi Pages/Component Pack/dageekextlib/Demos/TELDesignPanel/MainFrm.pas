unit MainFrm;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ExtCtrls, Buttons, Menus, Grids, ELDsgnr, ELPropInsp;

type

    // dummy class which is used as DesignControl
    // demonstrates in DesignPanel how controls which are larger than
    // client area are handled.
    TTestPanel = class(TPanel)
        constructor Create(AOwner: TComponent); override;
    end;

    TfrmMain = class(TForm)
        LMDDesignPanel1: TELDesignPanel;
        Button1: TButton;
        SpeedButton1: TSpeedButton;
        SpeedButton2: TSpeedButton;
        SpeedButton3: TSpeedButton;
        ELDesigner1: TELDesigner;
        SpeedButton4: TSpeedButton;
        SpeedButton5: TSpeedButton;
        ELPropertyInspector1: TELPropertyInspector;
        procedure Button1Click(Sender: TObject);
        procedure ELDesigner1ControlInserting(Sender: TObject;
            var AControlClass: TControlClass);
        procedure ELDesigner1ControlInserted(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure ELDesigner1ChangeSelection(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        Panel: TPanel;
        TestPanel: TTestPanel;
    end;


var
    frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
    ELDesigner1.Active := False;
    if Button1.Tag = 0 then
        begin
            Button1.Tag := 1;
            Button1.Caption := 'Use simple Panel';
            ELDesigner1.DesignControl := TestPanel;
        end
    else
        begin
            Button1.Tag := 0;
            Button1.Caption := 'Use extended Panel (see code)';
            ELDesigner1.DesignControl := Panel;
            Panel.Align := alClient;
        end;
    ELDesigner1.Active := True;
end;

procedure TfrmMain.ELDesigner1ControlInserting(Sender: TObject;
    var AControlClass: TControlClass);
begin
    if SpeedButton1.Down then
        AControlClass := TLabel
    else
        if SpeedButton2.Down then
            AControlClass := TButton
        else
            if SpeedButton4.Down then
                AControlClass := TMemo
            else
                if SpeedButton5.Down then
                    AControlClass := TCheckBox;
end;

procedure TfrmMain.ELDesigner1ControlInserted(Sender: TObject);
begin
    SpeedButton3.Down := True;
end;

constructor TTestPanel.Create(AOwner: TComponent);
begin
    inherited Create(aOwner);
    Width := 400;
    Height := 500;
    Color := clWhite;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
    Panel := TPanel.Create(nil); // Owner=nil!!
    TestPanel := TTestPanel.Create(nil);
    Button1Click(nil);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
    ELDesigner1.Active := False;
    if Button1.Tag = 0 then
		TestPanel.Free
    else
		Panel.Free;
end;

procedure TfrmMain.ELDesigner1ChangeSelection(Sender: TObject);
var
    LObjects: TList;
begin
    LObjects := TList.Create;
    try
        ELDesigner1.SelectedControls.GetControls(LObjects);
        ELPropertyInspector1.AssignObjects(LObjects);
    finally
        LObjects.Free;
    end;
end;

end.

