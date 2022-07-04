{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukrdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{------------------------------ Navigator Comments -----------------------------}

{
	TKCustomDBNavigator.AfterAction: TKAfterActionEvent = procedure( Sender: TObject;
		Button: TNavIndex ) of object;
}
	sComDBNavAfterAction  =
 '» Parameters: Sender = TKDBNavigator; Button = clicked button Index.'#13#10 +
 '» Invoked after the navigator action has already taken place, but is NOT '#13#10 +
 '    invoked for the niPrint and niSearch buttons.'#13#10 +
 '» Use this event to process navigator button actions.';

{
	TKCustomDBNavigator.BeforeAction: TKBeforeActionEvent = procedure( Sender: TObject;
		Button: TNavIndex; var Confirm: Boolean ) of object;
}

	sComDBNavBeforeAction =
 '» Parameters: Sender = TKDBNavigator; Button = clicked button Index;'#13#10 +
 '                var Confirm = allow event to be processed (default = true).'#13#10 +
 '» Invoked for every navigator button, before a navigator action takes place.'#13#10 +
 '» Use this event to allow the navigator button action to be processed.';

{
	TKCustomDBNavigator.OnPrint: TNotifyEvent;
}
	sComDBNavOnPrint      =
 '» Parameters: Sender = TKDBNavigator.'#13#10 +
 '» Invoked after the navigator print button is clicked, but after the '#13#10 +
 '    BeforeAction event has occurred. This event will only be called if '#13#10 +
 '    no TKDBNavigator.NavEvents.OnPrint event is called.'#13#10 +
 '» Use this event to process generic navigator print action.';

{
	TKCustomDBNavigator.OnSearch: TNotifyEvent;
}
	sComDBNavOnSearch     =
 '» Parameters: Sender = TKDBNavigator.'#13#10 +
 '» Invoked after the navigator search button is clicked, but after the '#13#10 +
 '    BeforeAction event has occurred. This event will only be called if '#13#10 +
 '    no TKDBNavigator.NavEvents.OnSearch event is called.'#13#10 +
 '» Use this event to process generic navigator search action.';

{
	TKNavCollectionItem.OnPrint: TKFormNavEvent = procedure( Sender: TObject;
		Form: TCustomForm ) of object;
}
	sComDBNavItemOnPrint  =
 '» Parameters: Sender = TKDBNavigator; Form = NavEvents.FormName instance.'#13#10 +
 '» Invoked after the navigator print button is clicked, but after the '#13#10 +
 '    BeforeAction event has occurred. This event will only be called if '#13#10 +
 '    a FormName instance can be found; if such an instance is found, but it is '#13#10 +
 '    not a Form (ie, QuickRep, DataModule, ...), an exception will be raised.'#13#10 +
 '» Use this event to process form specific navigator print action.';

{
	TKNavCollectionItem.OnSearch: TKFormNavEvent = procedure( Sender: TObject;
		Form: TCustomForm ) of object;
}
	sComDBNavItemOnSearch =
 '» Parameters: Sender = TKDBNavigator; Form = NavEvents.FormName instance.'#13#10 +
 '» Invoked after the navigator search button is clicked, but after the '#13#10 +
 '    BeforeAction event has occurred. This event will only be called if '#13#10 +
 '    a FormName instance can be found; if such an instance is found, but it is '#13#10 +
 '    not a Form (ie, QuickRep, DataModule, ...), an exception will be raised.'#13#10 +
 '» Use this event to process form specific navigator search action.';


{--------------------------- TKCaptionButton Comments ---------------------------}

{
	TKCaptionButton.OnCustomButtonDraw: TKDrawCaptionButtonEvent = procedure( Sender:
		TKCaptionButton; dc: HDC; rt: TRect ) of object;

}
	sComCapBtnCustomBtnDraw = '';

{
	TKCaptionButton.OnClick: TNotifyEvent

}
	sComCapBtnClick = '';

{
	TKCaptionButton.OnChange: TNotifyEvent

}
	sComCapBtnChange = '';

{---------------------------- TKFormPainter Comments ---------------------------}

{
	TKFormPainter.OnCloseQuery: TCloseQueryEvent

}
	sComFrmPCloseQuery = '';


{------------------------- TKCustomSpeedButton Comments ------------------------}

{
	TKCustomSpeedButton.OnDownClick: TNotifyEvent

}
	sComSpBtnDownClick = '';

{
	TKCustomSpeedButton.OnUpClick: TNotifyEvent

}
	sComSpBtnUpClick = '';

{
	TKCustomSpeedButton.OnDrawButton: TKDrawButtonEvent = procedure( Sender: TObject;
		ACanvas: TCanvas; ARect: TRect ) of object;
}
	sComSpBtnDrawBtn = '';

{------------------------ TKCustomSpeedControl Comments ------------------------}

{
	TKCustomSpeedControl.OnButtonClick: TNotifyEvent

}
	sComSpCtrlBtnClick = '';

{
	TKCustomSpeedControl.OnDownClick: TNotifyEvent

}
	sComSpCtrlDownClick = '';

{
	TKCustomSpeedControl.OnUpClick: TNotifyEvent

}
	sComSpCtrlUpClick = '';

{
	TKCustomSpeedControl.OnDrawButton: TKDrawButtonEvent = procedure( Sender: TObject;
		ACanvas: TCanvas; ARect: TRect ) of object;
}
	sComSpCtrlDrawBtn = '';

{-------------------------- TKCustomSpeedText Comments -------------------------}

{
	TKCustomSpeedText.OnCheckValue: TKCheckTextEvent = procedure( Sender: TObject;
		var NewValue: string ) of object;

}
	sComSpTxtCheckValue = '';

{----------------------- TKFormattedSpeedControl Comments ----------------------}

{
	TKFormattedSpeedControl.OnFormatDisplayText: TKFormatFloatTextEvent = procedure(
		Sender: TObject; const AValue: Extended; const AFormat: string; var AText:
		string ) of object;

}
	sComSpFltFmtDisp = '';

{
	TKFormattedSpeedControl.OnFormatEditText: TKFormatFloatTextEvent = procedure(
		Sender: TObject; const AValue: Extended; const AFormat: string; var AText:
		string ) of object;

}
	sComSpFltFmtEdt = '';

{------------------------- TKCustomSpeedInteger Comments -----------------------}

{
	TKCustomSpeedInteger.OnFormatDisplayText: TKFormatIntegerTextEvent = procedure(
		Sender: TObject; const AValue: Integer; const AFormat: string; var AText:
		string ) of object;

}
	sComSpIntFmtDisp = '';

{
	TKCustomSpeedInteger.OnFormatEditText: TKFormatIntegerTextEvent = procedure(
		Sender: TObject; const AValue: Integer; const AFormat: string; var AText:
		string ) of object;

}
	sComSpIntFmtEdt = '';

{----- TKCustomSpeedDateTime/TKCustomSpeedDate/TKCustomSpeedTime Comments -----}

{
	TKCustomSpeedDateTime.OnFormatDisplayText: TKFormatDateTimeTextEvent = procedure(
		Sender: TObject; const AValue: TDateTime; const AFormat: string; var AText:
		string ) of object;

}
	sComSpDTFmtDisp = '';

{
	TKCustomSpeedDateTime.OnFormatEditText: TKFormatDateTimeTextEvent = procedure(
		Sender: TObject; const AValue: TDateTime; const AFormat: string; var AText:
		string ) of object;

}
	sComSpDTFmtEdt = '';

implementation

end.
