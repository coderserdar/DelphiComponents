{
 * Macro Toolbar Display Wizard for Delphi 7
 *
 * DDabMacroTBarDisplayWiz.pas
 *
 * Implements a Delphi 7 Wizard that provides a View menu item to toggle the
 * macro recorder toolbar in editor windows.
 *
 * v1.0 of 10 Jun 2007 - Original version.
 *
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is DDabMacroTBarDisplayWiz.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit DDabMacroTBarDisplayWiz;


// Ensure wizard will only compile on Delphi 7
{$IFNDEF VER150}
  {$MESSAGE FATAL 'This wizard requires Delphi 7'}
{$ENDIF}


interface


uses
  // Delphi
  Classes, ToolsAPI, ComCtrls, Menus, ActnList, Forms;


type

  {
  TDDabMacroTBarSwitchWiz:
    Implements Delphi Wizard that provides a View menu item that toggles
    visibility of the Macro recording toolbar in editor windows.
  }
  TDDabMacroTBarSwitchWiz = class(TNotifierObject,
    IOTAWizard
  )
  private
    fAction: TAction;
      {Action associated with menu item}
    fMenuItem: TMenuItem;
      {Menu item added to view menu}
    fMenuSpacer: TMenuItem;
      {Spacer to appear above new menu item}
    fOldOnActiveFormChange: TNotifyEvent;
      {Stores reference to any previous TScreen's OnActiveFormChange event
      handler}
    procedure ActionExecute(Sender: TObject);
      {Handles action's OnExecute event. Toggles state of macro recording
      toolbar.
        @param Sender [in] Not used.
      }
    procedure ActionUpdate(Sender: TObject);
      {Handles action's OnUpdate event. Enables / disables action according to
      whether an editor window is displayed.
        @param Sender [in] Not used.
      }
    procedure ActiveFormChange(Sender: TObject);
      {Handles TScreen's OnActiveFormChange event. Updates visibility of macro
      toolbars in all editor windows.
        @param Sender [in] Not used.
      }
    function GetNTAServices: INTAServices;
      {Gets reference to Delphi's NTA services.
        @return Required services object.
      }
    function EditWindowExists: Boolean;
      {Checks if an edit window is currently displayed.
        @return True if an edit window is displayed, False otherwise.
      }
    function IsEditWindow(const Form: TForm): Boolean;
      {Checks if a form is an edit window.
        @param Form [in] Form to be checked.
        @return True if form is an edit window, False otherwise.
      }
    function FindToolBar(const EditWindow: TForm): TToolBar;
      {Finds an editor window's macro recording toolbar.
        @return Reference to required toolbar or nil if toolbar can't be found.
      }
    function FindViewMenuItem: TMenuItem;
      {Finds Delphi's top level View menu item.
        @return Reference to required menu item or nil if menu item can't be
          found.
      }
    procedure UpdateToolBar;
      {Updates visibility of macro recording toolbar in each editor window.
      }
  protected
    { IOTAWizard }
    function GetIDString: string;
      {Gets the wizard's unique identification string.
        @return Required ID.
      }
    function GetName: string;
      {Gets user-friendly name of wizard.
        @return Required name.
      }
    function GetState: TWizardState;
      {Gets menu state. Only called for menu wizards, i.e. not for IOTAWizard
      implementations.
        @return Empty set.
      }
    procedure Execute;
      {Never called for IOTAWizard implementations.
      }
  public
    constructor Create;
      {Class constructor. Sets up the wizard.
      }
    destructor Destroy; override;
      {Class destructor. Tears down wizard.
      }
  end;


procedure Register;
  {Registers wizard with Delphi.
  }


implementation


uses
  // Delphi
  SysUtils, Controls, Windows;


const
  cViewMenuName = 'ViewsMenu';      // Name of top level Delphi View menu item
  cEditWindowName = 'TEditWindow';  // Class name of editor window
  cToolBarName = 'RecordToolBar';   // Name of record toolbar in edit window
  cActionCategory = 'View';         // Category to which action belongs
  cWizardId = 'DelphiDabbler.MacroTBarDisplayWizard'; // Unique ID of wizard

resourcestring
  sMenuCaption = 'D&isplay Macro Toolbar';                    // menu caption
  sWizardName = 'DelphiDabbler Macro Toolbar Display Wizard'; // name of wizard


procedure Register;
  {Registers wizard with Delphi.
  }
begin
  RegisterPackageWizard(TDDabMacroTBarSwitchWiz.Create);
end;


{ TDDabMacroTBarSwitchWiz }

procedure TDDabMacroTBarSwitchWiz.ActionExecute(Sender: TObject);
  {Handles action's OnExecute event. Toggles state of macro recording toolbar.
    @param Sender [in] Not used.
  }
begin
  fAction.Checked := not fAction.Checked;
  UpdateToolBar;
end;

procedure TDDabMacroTBarSwitchWiz.ActionUpdate(Sender: TObject);
  {Handles action's OnUpdate event. Enables / disables action according to
  whether an editor window is displayed.
    @param Sender [in] Not used.
  }
begin
  fAction.Enabled := EditWindowExists;
end;

procedure TDDabMacroTBarSwitchWiz.ActiveFormChange(Sender: TObject);
  {Handles TScreen's OnActiveFormChange event. Updates visibility of macro
  toolbars in all editor windows.
    @param Sender [in] Not used.
  }
begin
  if Assigned(fOldOnActiveFormChange) then
    fOldOnActiveFormChange(Sender); // call any pre-existing handler first
  UpdateToolBar;
end;

constructor TDDabMacroTBarSwitchWiz.Create;
  {Class constructor. Sets up the wizard.
  }
var
  ViewMenu: TMenuItem;        // reference to Delphi View menu item
begin
  inherited;

  // Handle screen's OnActiveFormChange event
  fOldOnActiveFormChange := Screen.OnActiveFormChange;  // store old handler
  Screen.OnActiveFormChange := ActiveFormChange;

  // Create action used to update macro toolbar visibility and add it to
  // Delphi's action list
  fAction := TAction.Create(Application.MainForm);
  fAction.ActionList := GetNTAServices.ActionList;
  fAction.Caption := sMenuCaption;
  fAction.Enabled := False;
  fAction.Checked := True;
  fAction.OnExecute := ActionExecute;
  fAction.OnUpdate := ActionUpdate;
  fAction.Category := cActionCategory;

  // Create menu item and spacer
  fMenuItem := TMenuItem.Create(Application.MainForm);
  fMenuItem.Action := fAction;
  fMenuSpacer := TMenuItem.Create(Application.MainForm);
  fMenuSpacer.Caption := '-';

  // Add spacer and menu item to view menu
  ViewMenu := FindViewMenuItem;
  if Assigned(ViewMenu) then
    ViewMenu.Add([fMenuSpacer, fMenuItem]);

  // Try to display macro toolbar in editor
  UpdateToolBar;
end;

destructor TDDabMacroTBarSwitchWiz.Destroy;
  {Class destructor. Tears down wizard.
  }

  // ---------------------------------------------------------------------------
  procedure RemoveAction(ToolBar: TToolBar);
    {Removes any tool buttons associated with the wizard from a toolbar.
      @param ToolBar [in] Reference to toolbar from which buttons may need to be
        removed.
    }
  var
    I: Integer;       // loops through buttons in toolbar
    Btn: TToolButton; // reference to each button in toolbar
  begin
    // Check toolbar is valid
    if not Assigned(ToolBar) then
      Exit;
    // Loop though each button in toolbar
    for I := Pred(ToolBar.ButtonCount) downto 0 do
    begin
      Btn := ToolBar.Buttons[I];
      if Btn.Action = fAction then
      begin
        // Toolbar references wizard's action so remove and free it
        // (we use method recommended by Delphi help)
        Toolbar.Perform(CM_CONTROLCHANGE, WPARAM(Btn), 0);
        Btn.Free;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

const
  // Array of Delphi toolbar names
  cToolbars: array[1..7] of string = (
    sCustomToolBar, sDesktopToolBar, sStandardToolBar, sDebugToolBar,
    sViewToolBar, sInternetToolBar, sCORBAToolBar
  );
var
  NTAServices: INTAServices;  // reference to Delphi's tool services
  TBIdx: Integer;             // loops through array of toolbar names
begin
  // Remove macro toolbars
  fAction.Checked := False;
  UpdateToolBar;
  // Restore TScreen's original OnActiveFormChange event handler
  Screen.OnActiveFormChange := fOldOnActiveFormChange;
  // Remove any custom toolbuttons set up to trigger wizard's action
  NTAServices := GetNTAServices;
  for TBIdx := Low(cToolbars) to High(cToolbars) do
    RemoveAction(NTAServices.ToolBar[cToolbars[TBIdx]]);
  // Free menu items and action
  fMenuSpacer.Free;
  fMenuItem.Free;
  fAction.Free;
  inherited;
end;

function TDDabMacroTBarSwitchWiz.EditWindowExists: Boolean;
  {Checks if an edit window is currently displayed.
    @return True if an edit window is displayed, False otherwise.
  }
var
  Idx: Integer; // loops through all Delphi's forms
begin
  Result := False;
  for Idx := 0 to Pred(Screen.FormCount) do
  begin
    if IsEditWindow(Screen.Forms[Idx]) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TDDabMacroTBarSwitchWiz.Execute;
  {Never called for IOTAWizard implementations.
  }
begin
  // Do nothing: not called
end;

function TDDabMacroTBarSwitchWiz.FindToolBar(const EditWindow: TForm): TToolBar;
  {Finds an editor window's macro recording toolbar.
    @return Reference to required toolbar or nil if toolbar can't be found.
  }
begin
  Assert(Assigned(EditWindow));
  Result := EditWindow.FindComponent(cToolBarName) as TToolBar;
end;

function TDDabMacroTBarSwitchWiz.FindViewMenuItem: TMenuItem;
  {Finds Delphi's top level View menu item.
    @return Reference to required menu item or nil if menu item can't be found.
  }
var
  Idx: Integer;               // loops through all main menu items
  NTAServices: INTAServices;  // reference to Delphi's tool services
begin
  NTAServices := GetNTAServices;
  Result := nil;
  for Idx := 0 to Pred(NTAServices.MainMenu.Items.Count) do
  begin
    if SameText(NTAServices.MainMenu.Items[Idx].Name, cViewMenuName) then
    begin
      Result := NTAServices.MainMenu.Items[Idx];
      Break;
    end;
  end;
end;

function TDDabMacroTBarSwitchWiz.GetIDString: string;
  {Gets the wizard's unique identification string.
    @return Required ID.
  }
begin
  Result := cWizardId;
end;

function TDDabMacroTBarSwitchWiz.GetName: string;
  {Gets user-friendly name of wizard.
    @return Required name.
  }
begin
  Result := sWizardName;
end;

function TDDabMacroTBarSwitchWiz.GetNTAServices: INTAServices;
  {Gets reference to Delphi's NTA services.
    @return Required services object.
  }
begin
  Supports(BorlandIDEServices, INTAServices, Result);
end;

function TDDabMacroTBarSwitchWiz.GetState: TWizardState;
  {Gets menu state. Only called for menu wizards, i.e. not for IOTAWizard
  implementations.
    @return Empty set.
  }
begin
  Result := []; // Not called
end;

function TDDabMacroTBarSwitchWiz.IsEditWindow(const Form: TForm): Boolean;
  {Checks if a form is an edit window.
    @param Form [in] Form to be checked.
    @return True if form is an edit window, False otherwise.
  }
begin
  Result := SameText(Form.ClassName, cEditWindowName);
end;

procedure TDDabMacroTBarSwitchWiz.UpdateToolBar;
  {Updates visibility of macro recording toolbar in each editor window.
  }
var
  Form: TForm;        // reference to a Delphi form
  Toolbar: TToolBar;  // reference to macro recording toolbar
  Idx: Integer;       // loops through all Delphi's forms
begin
  // *** Based on code suggested by Brian Long in The Delphi Magazine issue 82.
  // *** Modified to find all open editors rather than just one.
  
  // Scan all of Delphi's forms, looking for edit window
  for Idx := 0 to Pred(Screen.FormCount) do
  begin
    Form := Screen.Forms[Idx];
    if IsEditWindow(Form) then
    begin
      // We have an edit window: find toolbar and update visibility
      Toolbar := FindToolBar(Form);
      if Assigned(Toolbar) then
        Toolbar.Visible := fAction.Checked;
    end;
  end;
end;

end.
