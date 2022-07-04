unit iFormEditEdReg;


// dopracovat
// Brush.BitmapIndex
// Pen.Brush_BitmapIndex;
// Pen.UserStyle
//


interface

{$i iformedit.inc}

uses
        {$ifdef D6+}
                designeditors,designintf,
        {$else}
                DsgnIntf,
        {$endif}
     ShellAPI, Windows, Classes, Forms, Controls,
     iLabelStringEditor,
     ipsLabel, iProppsLabel, ipsBrush,
     ipsShape, iProppsShape;

const
     const_fe_web = 'http://www.psoft.sk/products.asp';
type

   TpsBrushIndexEditor= class(TIntegerProperty)
   public
         function GetAttributes:TPropertyAttributes; override;
         procedure Edit; override;
   end;


   TpsLabelCaptionProperty=class(TStringProperty)
   public
         function GetAttributes:TPropertyAttributes; override;
         procedure Edit; override;
   end;

   TpsLabelEditor = class(TDefaultEditor)
   public
   	procedure ExecuteVerb(Index: Integer); override;
   	function  GetVerb(Index: Integer): string; override;
   	function  GetVerbCount: Integer; override;
        procedure Edit;override;
   end;

   TpsShapeEditor = class(TDefaultEditor)
   public
   	procedure ExecuteVerb(Index: Integer); override;
   	function  GetVerb(Index: Integer): string; override;
   	function  GetVerbCount: Integer; override;
        procedure Edit;override;
   end;




   procedure Register;

implementation

uses ipsBrushEd;

procedure TpsLabelEditor.Edit;
begin
  if Component is TpsLabel then
     TpsLabelFmt.Execute(Point(-1,-1), [TpsLabel(Component)]);
end;

procedure TpsLabelEditor.ExecuteVerb(Index: Integer);
var L:TpsLabel;
begin
   L:=nil;
   if Component is TpsLabel then L:=TpsLabel(Component);

   if L<>nil then
	case Index of
                0 : TpsLabelFmt.Execute(Point(-1,-1), [L]);
                1 : ; // EditpsFont(E.psFont);
                2 : ShellExecute(GetDesktopWindow(), 'open',
                  PChar(const_fe_web), nil, nil, SW_SHOWNORMAL);

	end;
end;

function TpsLabelEditor.GetVerb(Index: Integer): String;
begin
	case Index of
		0 : Result := 'FormEdit library - PSOFT company © 2000-2001';
                1 : Result := 'FormEdit font editor';
                2 : Result := 'Connect FormEdit library homepage';
	end;
end;

function TpsLabelEditor.GetVerbCount: Integer;
begin
	Result := 3;
end;


procedure Register;
begin
     RegisterPropertyEditor(TypeInfo(String), TpsLabel, 'Caption', TpsLabelCaptionProperty);


     RegisterPropertyEditor(TypeInfo(Integer), TpsBrush, 'BitmapIndex',
          TpsBrushIndexEditor);
     RegisterPropertyEditor(TypeInfo(Integer), TpsPen,   'Brush_BitmapIndex',
          TpsBrushIndexEditor);

     RegisterComponentEditor(TpsLabel, TpsLabelEditor);
     RegisterComponentEditor(TpsShape, TpsShapeEditor);
end;



{ TpsBrushIndexEditor }

procedure TpsBrushIndexEditor.Edit;
var i:Integer;
    b:TpsBrush;
    p:TpsPen;
begin
    if GetComponent(0) is TpsPen then begin
       p:= getComponent(0) as TpsPen;
       i:=p.Brush_BitmapIndex;
       p.Brush_BitmapIndex:=EditBrushIndex(i);
    end;
    if GetComponent(0) is TpsBrush then begin
       b:= getComponent(0) as TpsBrush;
       i:=b.BitmapIndex;
       b.BitmapIndex:=EditBrushIndex(i);
    end;
end;

function TpsBrushIndexEditor.GetAttributes: TPropertyAttributes;
begin
     Result := [paDialog];
end;

{ TpsLabelCaptionProperty }

procedure TpsLabelCaptionProperty.Edit;
var s:String;
    p:TpsLabel;
begin
    if GetComponent(0) is TpsLabel then begin
       p:= getComponent(0) as TpsLabel;
       with TStringEditorFmt.Create(Application) do
       try
                MEMO.Text := p.Caption;
                if ShowModal=mrOK then
                        p.Caption := MEMO.Text;
       finally
                Free;
       end;
    end;
end;

function TpsLabelCaptionProperty.GetAttributes: TPropertyAttributes;
begin
     Result := [paDialog];
end;

{ TpsShapeEditor }

procedure TpsShapeEditor.Edit;
begin
  if Component is TpsShape then
     TpsShapeFmt.Execute(Point(0,0), [TpsShape(Component)]);
end;

procedure TpsShapeEditor.ExecuteVerb(Index: Integer);
var L:TpsShape;
begin
   L:=nil;
   if Component is TpsShape then L:=TpsShape(Component);

   if L<>nil then
	case Index of
                0 : TpsShapeFmt.Execute(Point(0,0), [L]);
                1 : ShellExecute(GetDesktopWindow(), 'open',
                  PChar(const_fe_web), nil, nil, SW_SHOWNORMAL);

	end;
end;

function TpsShapeEditor.GetVerb(Index: Integer): string;
begin
	case Index of
		0 : Result := 'TpsShape editor(FormEdit library)';
                1 : Result := 'Connect FormEdit library homepage';
	end;
end;

function TpsShapeEditor.GetVerbCount: Integer;
begin
        Result := 2;
end;

end.
