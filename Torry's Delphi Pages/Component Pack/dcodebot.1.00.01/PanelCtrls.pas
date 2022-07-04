unit PanelCtrls;

interface

uses
  Classes, Controls, Graphics, SysUtils, Messages, Windows, GraphTools;

{ TPanelBox }

type
  TPanelBox = class(TCustomControl)
  private
  protected
  public
  end;

{ TCheckPanelBox }

  TCheckPanelBox = class(TPanelBox)
  private
  published
    property Caption;
    property Checked: Boolean read FChecked write SetChecked;
    property OnClick;
  end;

{ TNodePanelBox }

  TNodePanelBox = class(TPanelBox)
  private
  public
    procedure Expand;
    procedure Collapse;
  published
    property Caption;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ExpandHeight: Integer;
    property OnExpand;
  end;

implementation

end.
 