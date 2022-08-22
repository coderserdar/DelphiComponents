TDragListBox - (ver 1.2 - March 2002)
--------------------------------------------------------

TDragListBox is an extension to TListBox to support moving items by dragging.

Its main features are:
  - Allow dragging and moving items within the control.
  - Accept dragged items from any TCustomListBox descendants.
  - Draw a focus rect where the dragged item will drop.

Properties:
  AutoDrag: Boolean;
    This option determines whether user can displace items in the list box.
  CrossDrag: Boolean;
    This option determines whether user can drop items from other list boxes.

Methods:
  There are some utility methods:
    Displace current item in the list:
      procedure MoveUp;
      procedure MoveDown;
      procedure MoveTop;
      procedure MoveBottom;
    Move selected item to another list:
      procedure MoveTo( DestList: TCustomListBox); 
      procedure MoveAll( DestList: TCustomListBox);

--------------------------------------------------------
This component is as is, without warranty of any kind, 
And you may use it at your own risk.
--------------------------------------------------------


Let us know if you have used this in your applications.

--------------------------------------------------------
Farsi Components Co.
Email: support@farsicomponents.com
Website: http://www.farsicomponents.com/
--------------------------------------------------------
