unit XPInformationBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TXPInformationBar = class(TPanel)
  private
   FHeader: TPanel;

   function GetHeaderVisibility: Boolean;
   procedure SetHeaderVisibility(const Value:Boolean);
   function GetHeaderCaption: String;
   procedure SetHeaderCaption(const Value:String);
   function GetHeaderColor: TColor;
   procedure SetHeaderColor(const Value:TColor);
   function GetHeaderFont: TFont;
   procedure SetHeaderFont(const Value:TFont);
   function GetHeaderHeight: Integer;
   procedure SetHeaderHeight(const Value:Integer);
    { Private-Deklarationen }
  protected
    { Protected-Deklarationen }
  public
   constructor Create (AOwner: TComponent); override;
   destructor Destroy; override;
    { Public-Deklarationen }
  published
   property HeaderVisible: boolean read GetHeaderVisibility write SetHeaderVisibility;
   property HeaderCaption: string read GetHeaderCaption write SetHeaderCaption;
   property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
   property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
   property HeaderHeight: Integer read GetHeaderHeight write SetHeaderHeight;
    { Published-Deklarationen }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XPlorer', [TXPInformationBar]);
end;



constructor TXPInformationBar.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  Align :=AlLeft;
  BorderWidth :=1;
  Font.Name:='Tahoma';
  Color:=$00EFA27B;
  BevelOuter:=BvNone;
  width:=250;
  Font.Color:=ClGray;

  FHeader:=TPanel.create(Self);
  with FHeader do
  begin
   Parent:=self;
   Visible:=true;
   BorderWidth:=3;
   BevelOuter:=BvRaised;
   Color:=ClBtnFace;
   Alignment:=TaLeftJustify;
   Font.Color:=ClBlack;
   Caption:='Header';
   Height:=22;
   SetBounds(0, 0, width, height);
   Align:=AlTop;
  end;

  FHeader.Font.Color:=ClBlack;
end;



destructor TXPInformationBar.Destroy;
begin

 FHeader.Destroy;

 inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////// Prozeduren und Funktionen der Komponente
///////////////////////////////////////////////////////////////////////////////////////////////////////

function TXPInformationBar.GetHeaderVisibility: boolean;
begin
 result:=FHeader.Visible;
end;



procedure TXPInformationBar.SetHeaderVisibility(const Value:boolean);
begin
 FHeader.Visible:=Value;
end;



function TXPInformationBar.GetHeaderCaption: String;
begin
 result:= FHeader.caption;
end;



procedure TXPInformationBar.SetHeaderCaption(const Value:String);
begin
 FHeader.Caption:=Value;
end;



function TXPInformationBar.GetHeaderColor: TColor;
begin
 result:=FHeader.Color;
end;



procedure TXPInformationBar.SetHeaderColor(const Value:TColor);
begin
 FHeader.color:=Value;
end;



function TXPInformationBar.GetHeaderFont: TFont;
begin
 result:= FHeader.Font;
end;



procedure TXPInformationBar.SetHeaderFont(const Value:TFont);
begin
 FHeader.Font:= Value;
end;



function TXPInformationBar.GetHeaderHeight: Integer;
begin
 result:= FHeader.Height;
end;



procedure TXPInformationBar.SetHeaderHeight(const Value:Integer);
begin
 FHeader.Height:= Value;
end;

end.
