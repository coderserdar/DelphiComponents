{Author:	Poul Bak}
{}
{Copyright © 2002-2003 : BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.50.00.00}
{}
{PBEditEx is a PBEdit descendant that can show an image in the Edit-window.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}

unit PBEditEx;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, StdCtrls, PBEdit, ImgList,
	Graphics,	Forms, ExtCtrls;

type

	TImageIndex = type integer;
	
{Author:	Poul Bak}
{}
{Copyright © 2002-2003 : BakSoft-Denmark (Poul Bak). All rights reserved.}
{}
{http://home11.inet.tele.dk/BakSoft/}
{Mailto: baksoft.denmark@tiscali.dk}
{NOTE: Be sure to include my name in the mail-body to get pass my filters.}
{}
{Component Version: 8.50.00.00}
{}
{PBEditEx is a PBEdit descendant that can show an image in the Edit-window.}

{Supports Windows 95, 98 and NT.}
{Supports Default-Button click.}
{Supports Cancel-button click.}
	TPBEditEx = class(TPBEdit)
	private
		{ Private declarations }
		FImage : TImage;
		FImages : TImageList;
		FImageIndex : TImageIndex;
		function GetColor : TColor;
		function GetDisabledColor : TColor;
		procedure SetColor(Value : TColor);
		procedure SetDisabledColor(Value : TColor);
		procedure SetImageIndex(Value : TImageIndex);
		procedure SetImages(Value : TImageList);
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
	protected
		{ Protected declarations }
		procedure CreateWnd; override;
		procedure Notification(AComponent: TComponent;
			Operation: TOperation); override;
		procedure SetEnabled(Value : Boolean); override;
	public
		{ Public declarations }
		constructor Create(AOwner : TComponent); override;
		destructor Destroy; override;
		procedure Invalidate; override;
	published
		{ Published declarations }
{The Color property sets only the enabled-color. When you disable the
edit-control the Disabled color will be used.}
		property Color : TColor read GetColor write SetColor default clWindow;
{DisabledColor is the background color used when the edit-control is disabled.}
{To set the color when Enabled - use the Color property.}
		property DisabledColor : TColor read GetDisabledColor
			write SetDisabledColor default clBtnFace;
{Besides standard enabling/disabling - this property also changes the background
color to either the Color or the DisabledColor property.}
		property Enabled : Boolean read GetEnabled
			write SetEnabled default True;
{The ImageList used to display the edit-image. See also ImageIndex.}
		property Images : TImageList read FImages write SetImages;
{The Index in Images that is shown in the editbox.}
		property ImageIndex : TImageIndex read FImageIndex
			write SetImageIndex default -1;
	end;

implementation

constructor TPBEditEx.Create(AOwner : TComponent);
var
	t : integer;
begin
	inherited;
	FImage := TImage.Create(Self);
	FImageIndex := -1;
	FImages := nil;
	for t := 0 to AOwner.ComponentCount - 1 do
	begin
		if AOwner.Components[t] is TImageList then
		begin
			FImages := TImageList(AOwner.Components[t]);
			if FImages.Count > 0 then FImageIndex := 0;
			Break;
		end;
	end;
	SetColor(clWindow);
	SetDisabledColor(clBtnFace);
end;

procedure TPBEditEx.CreateWnd;
begin
	inherited;
	FImage.Parent := Self;
	FImage.BoundsRect := Bounds(2, 2, 0, 16);
	Invalidate;
end;

destructor TPBEditEx.Destroy;
begin
	FImage.Free;
	FImage := nil;
	inherited;
end;

procedure TPBEditEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited;
	if (AComponent = FImages) and (Operation = opRemove) then
	begin
		FImages := nil;
		Invalidate;
	end;
end;

procedure TPBEditEx.SetImageIndex(Value : TImageIndex);
begin
	FImageIndex := Value;
	Invalidate;
end;

procedure TPBEditEx.SetImages(Value : TImageList);
begin
	FImages := Value;
	Invalidate;
end;

procedure TPBEditEx.Invalidate;
var
	Loc, Rect0 : TRect;
begin
	if Parent <> nil then
	begin
		Loc := Bounds(2, 2, ClientWidth - 4, ClientHeight - 2);
		if (FImages = nil) or (FImageIndex < 0) or (FImageIndex >= FImages.Count) then
		begin
			Loc.Left := 2;
			if FImage <> nil then FImage.BoundsRect := Bounds(2, 2, 0, FImage.Height);
		end
		else
		begin
			Loc.Left := 4 + FImages.Width;
			FImage.BoundsRect := Bounds(2, 2, FImages.Width, FImages.Height);
			if Enabled then FImage.Canvas.Brush.Color := Self.Color
			else FImage.Canvas.Brush.Color := Self.DisabledColor;
			FImage.Canvas.FillRect(FImage.ClientRect);
			FImages.GetBitmap(FImageIndex, FImage.Picture.Bitmap);
		end;
		SendMessage(Handle, EM_SETRECT, 0, LongInt(@Loc));
		Rect0 := ClientRect;
		InvalidateRect(Handle, @Rect0, True);
	end;
	inherited;
end;

procedure TPBEditEx.WMSize(var Message: TWMSize);
begin
	inherited;
	Invalidate;
end;

function TPBEditEx.GetColor : TColor;
begin
	Result := inherited Color;
end;

function TPBEditEx.GetDisabledColor : TColor;
begin
	Result := inherited DisabledColor;
end;

procedure TPBEditEx.SetDisabledColor(Value : TColor);
begin
	if inherited DisabledColor <> Value then
	begin
		inherited DisabledColor := Value;
		Invalidate;
	end;
end;

procedure TPBEditEx.SetColor(Value : TColor);
begin
	if inherited Color <> Value then
	begin
		inherited Color := Value;
		Invalidate;
	end;
end;

procedure TPBEditEx.SetEnabled(Value : Boolean);
begin
	inherited;
	Invalidate;
end;

end.
