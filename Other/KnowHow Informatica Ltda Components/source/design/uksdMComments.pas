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

unit uksdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{----------------------------- TKWebLabel Comments -----------------------------}

{
	TKWebLabel.BeforeWebAction: TKWebActionEvent = procedure( Sender: TObject;
		var Continue: Boolean ) of object;
}

	sComBeforeWebAction =
	'» Parameters: Sender = TKWebLabel; var Continue = set continue to'#13#10 +
	'    false in order to avoid the Web Action to occur ( default = true ).'#13#10 +
	'» Invoked right before a Web Action occurs.'#13#10 +
	'» Use this event to examine Web Actions before they actually occur.';

{
	TKWebLabel.AfterWebAction: TNotifyEvent
}

	sComAfterWebAction =
	'» Parameters: Sender = TKWebLabel.'#13#10 +
	'» Invoked right after a Web Action has occured.'#13#10 +
	'» Use this event to take proper action after a Web Action has occured.';

{
	TKWebLabel.OnCustomWebAction: TNotifyEvent
}
	sComCustomWebAction =
	'» Parameters: Sender = TKWebLabel.'#13#10 +
	'» Invoked .... :) ';

implementation

end.
