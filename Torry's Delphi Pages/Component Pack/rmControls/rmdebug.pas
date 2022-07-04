unit rmDebug;

interface

procedure SetDebugMessage(Str:string);

implementation

uses Forms, rmCollectionListBox, Controls;

var
   wForm : TForm;
   wCLB : TrmCollectionListBox;

procedure SetDebugMessage(Str:string);
begin
   if (wForm <> nil) and (wCLB <> nil) then
      wCLB.Insert(0, Str, -1, nil);
end;

initialization

  wForm := TForm.create(nil);
  wForm.BorderIcons := []; 
  wform.Show;


  wCLB := TrmCollectionListBox.Create(wForm);
  wCLB.Parent := wForm;
  wclb.Top := 0;
  wclb.left := 0;
  wCLB.Align := alClient;
  wclb.show;


finalization

  wForm.free;
  wForm := nil;

end.
