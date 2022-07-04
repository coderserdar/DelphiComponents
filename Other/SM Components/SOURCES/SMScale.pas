{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  This component allow rescale all controls on form
  according a PixelsPerInch change between design- and run-time
  of you application.
}
unit SMScale;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TSMScaler = class(TComponent)
  private
    FDesignedPPI: Integer;
    FScaleFactor: Integer;

    FOnBeforeRescale: TNotifyEvent;
    FOnAfterRescale: TNotifyEvent;
  protected
  public
    constructor Create (AOwner: TComponent); override;
    procedure Loaded; override;
    procedure Execute;
  published
    property DesignedPPI: Integer read FDesignedPPI write FDesignedPPI;
    property ScaleFactor: Integer read FScaleFactor write FScaleFactor;

    property OnBeforeRescale: TNotifyEvent read FOnBeforeRescale write FOnBeforeRescale;
    property OnAfterRescale: TNotifyEvent read FOnAfterRescale write FOnAfterRescale;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMScaler]);
end;

constructor TSMScaler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDesignedPPI := Screen.PixelsPerInch;
  FScaleFactor := 100;
end;

procedure TSMScaler.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
    Execute
end;

type THackForm = class(TCustomForm);

procedure TSMScaler.Execute;
var frmParent: THackForm;
    curPPI: Integer;
begin
  curPPI := Screen.PixelsPerInch;
  if (curPPI > DesignedPPI) or (FScaleFactor <> 100) then
  begin
    frmParent := THackForm(Owner);
    if Assigned(frmParent) then
    begin
      if Assigned(FOnBeforeRescale) then
        FOnBeforeRescale(Self);

      with frmParent do
      begin
        AutoScroll := False;
        ChangeScale(Trunc((DesignedPPI/curPPI)*FScaleFactor), 100);
      end;

      if Assigned(FOnAfterRescale) then
        FOnAfterRescale(Self);
    end;
  end;
end;

end.
