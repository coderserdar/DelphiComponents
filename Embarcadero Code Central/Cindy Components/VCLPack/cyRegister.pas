{   * ***** BEGIN LICENSE BLOCK *****
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
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyRegister;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  Classes,

  {$IFDEF DELPHI6_OR_ABOVE}
  DesignEditors, DesignIntf,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}

  // Visual components :
  {$IFDEF UNICODE}
  cyFlowPanel, cyGridPanel, cyAdvFlowPanel, cyAdvGridPanel, cyButtonedEdit,
  {$ENDIF}
  cyPanel, cyAdvPanel, cyNavPanel, cyBevel, cyPaintBox, cyAdvPaintBox, cyCustomImage, cyScrollBox, cyLabel, cyHotLabel,
  cySpeedButton, cyAdvSpeedButton, cyButton, cyBitBtn, cyAdvButton, cySkinButton, cySkinArea, cyLed, cyAdvLed, cySimpleGauge, cySplitter,
  cyAttract, cyColorMatrix, cyColorGrid, cyStatusBar, cyBook, cyPageControl, cyTabControl, cyStaticText, cyAdvStaticText,
  cyEdit, cyEditInteger, cyEditFloat, cyEditDate, cyEditTime, cyEditFilename, cyEditMail, cyEditWebsite, cyMaskEdit,
  cyRadioGroup, cyCheckBox, cyBaseComboBox, cyBaseFilterComboBox,

  // Non visual components :
  cyIniForm, cySearchFiles, cyCopyFiles, cyFlyingContainer, cyModalContainer, cyResizer, cyVirtualGrid, cyVirtualChart, cyProgressionPanel,
  cyAdvProgressionPanel, cyRunTimeResize, cyCommRoomConnector, cyCommunicate, cyAppInstances, cyClipboard, cyDDECmd, cyMathParser, cyDebug;


type
  // Add additional units dropping component :
  TcyEditSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // Design-time component editors:
  TcyDefaultEditor = class(TDefaultEditor) // class(TComponentEditor)
  public
    { Public declarations }
    procedure Edit; override;  // On double click the component ...
    procedure ExecuteVerb(Index: Integer); override;  // Called when MenuItem was clicked by developper ...
    function GetVerb(Index: Integer): string; override;  // Inform Delphi the captions to appear in the context menu.
    function GetVerbCount: Integer; override;  // Inform Delphi how many items we want to add to the context menu ...
//    procedure PrepareItem(Index: Integer; const AItem: TMenuItem); override;  // before appearing the contextMenu, we can Show/hide menuItems but we can't destroy them ...
  end;


procedure Register;

implementation

uses cyformAbout, cyDsnResource;

procedure TcyDefaultEditor.Edit;
var Handled: Boolean;
begin
  Handled := false;

  if Component is TcyLed
  then
    with TcyLed(Component) do
    begin
      LedValue := not LedValue;
      Handled := true;
    end;

  if Component is TcyAdvLed
  then
    with TcyAdvLed(Component) do
    begin
      LedValue := not LedValue;
      Handled := true;
    end;

  if not Handled
  then Inherited Edit;
end;

procedure TcyDefaultEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1   // Last one ...
  then begin
    CindyDesignAboutForm;
    // Tell to the IDE that something changed!
    //  Designer.Modified;
  end
  else
    Inherited ExecuteVerb(Index);
end;

function TcyDefaultEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1   // Last one ...
  then Result := RSAboutName
  else Result := Inherited GetVerb(Index);
end;

function TcyDefaultEditor.GetVerbCount: Integer;
begin
  Result := Inherited GetVerbCount + 1;  // We add one MenuItem ...
end;

{procedure TcyDefaultEditor.PrepareItem(Index: Integer; const AItem: TMenuItem);
begin
  case Index of
    0: AItem.Enabled := true;
  end;

  // Tell to the IDE that something changed!
  Designer.Modified;
end;}

{procedure TcyDBGridEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1   // Last one ...
  then begin
    // Edit columns :

  end
  else
    Inherited ExecuteVerb(Index);
end;

function TcyDBGridEditor.GetVerb(Index: Integer): string;
begin
  if Index <> GetVerbCount - 1   // Last one ...
  then Result := 'Columns ...'
  else Result := Inherited GetVerb(Index);
end;

function TcyDBGridEditor.GetVerbCount: Integer;
begin
  Result := Inherited GetVerbCount + 1;  // We add one MenuItem ...
end;            }

{ TcyEditSelectionEditor }
procedure TcyEditSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('cyEdit');
end;


procedure Register;
begin
  // Register Visual components :
  RegisterComponents(RSVCLCompsPalette, [
    TcyBevel,
    TcyPanel,
    TcyAdvPanel,
    TcyNavPanel,
    {$IFDEF UNICODE}
    TcyFlowPanel,
    TcyGridPanel,
    TcyAdvFlowPanel,
    TcyAdvGridPanel,
    TcyButtonedEdit,
    {$ENDIF}
    TcyPaintBox,
    TcyAdvPaintBox,
    TcyImage,
    TcyScrollBox,
    TcyLabel,
    TcyHotLabel,
    TcyEdit,
    TcyEditInteger,
    TcyEditFloat,
    TcyEditDate,
    TcyEditTime,
    TcyEditFilename,
    TcyEditMail,
    TcyEditWebsite,
    TcyMaskEdit,
    TcyCheckBox,
    TcySpeedButton,
    TcyAdvSpeedButton,
    TcyButton,
    TcyBitBtn,
    TcyAdvButton,
    TcySkinButton,
    TcySkinArea,
    TcyLed,
    TcyAdvLed,
    TcySimpleGauge,
    TcySplitter,
    TcyAttract,
    TcyColorMatrix,
    TcyColorGrid,
    TcyStatusBar,
    TcyBook,
    TcyPageControl,
    TcyTabControl,
    TcyStaticText,
    TcyAdvStaticText,
    TcyRadioGroup,
    TcyRadioButton,
    TcyComboBox,
    TcyFilterComboBox
    ]);

    RegisterComponentEditor(TcyBevel, TcyDefaultEditor);
    RegisterComponentEditor(TcyPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyNavPanel, TcyDefaultEditor);
    {$IFDEF UNICODE}
    RegisterComponentEditor(TcyFlowPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyGridPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvFlowPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvGridPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyButtonedEdit, TcyDefaultEditor);
    {$ENDIF}
    RegisterComponentEditor(TcyPaintBox, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvPaintBox, TcyDefaultEditor);
    RegisterComponentEditor(TcyImage, TcyDefaultEditor);
    RegisterComponentEditor(TcyScrollbox, TcyDefaultEditor);
    RegisterComponentEditor(TcyLabel, TcyDefaultEditor);
    RegisterComponentEditor(TcyHotLabel, TcyDefaultEditor);
    RegisterComponentEditor(TcyEdit, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditInteger, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditFloat, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditDate, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditTime, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditFilename, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditMail, TcyDefaultEditor);
    RegisterComponentEditor(TcyEditWebsite, TcyDefaultEditor);
    RegisterComponentEditor(TcyMaskEdit, TcyDefaultEditor);
    RegisterComponentEditor(TcyCheckbox, TcyDefaultEditor);
    RegisterComponentEditor(TcySpeedButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvSpeedButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyBitBtn, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvButton, TcyDefaultEditor);
    RegisterComponentEditor(TcySkinButton, TcyDefaultEditor);
    RegisterComponentEditor(tcySkinArea, TcyDefaultEditor);
    RegisterComponentEditor(TcyLed, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvLed, TcyDefaultEditor);
    RegisterComponentEditor(TcySimpleGauge, TcyDefaultEditor);
    RegisterComponentEditor(TcySplitter, TcyDefaultEditor);
    RegisterComponentEditor(TcyAttract, TcyDefaultEditor);
    RegisterComponentEditor(TcyColorMatrix, TcyDefaultEditor);
    RegisterComponentEditor(TcyColorGrid, TcyDefaultEditor);
    RegisterComponentEditor(TcyStatusBar, TcyDefaultEditor);
    RegisterComponentEditor(TcyBook, TcyDefaultEditor);
//    RegisterComponentEditor(TcyPageControl, TcyDefaultEditor);
    RegisterComponentEditor(TcyTabControl, TcyDefaultEditor);
    RegisterComponentEditor(TcyStaticText, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvStaticText, TcyDefaultEditor);
    RegisterComponentEditor(TcyRadioGroup, TcyDefaultEditor);
    RegisterComponentEditor(TcyRadioButton, TcyDefaultEditor);
    RegisterComponentEditor(TcyComboBox, TcyDefaultEditor);
    RegisterComponentEditor(TcyFilterComboBox, TcyDefaultEditor);


  // Register Non-Visual components :
  RegisterComponents(RSVCLCompsPalette, [
    TcyIniForm,
    TcySearchFiles,
    TcyCopyFiles,
    TcyFlyingContainer,
    TcyModalContainer,
    TcyResizer,
    TcyVirtualChart,
    TcyVirtualGrid,
    TcyProgressionPanel,
    TcyAdvProgressionPanel,
    TcyCommRoomConnector,
    TcyCommunicate,
    TcyAppInstances,
    TcyRunTimeResize,
    TcyClipBoard,
    TcyDDECmd,
    TcyMathParser,
    TcyDebug
    ]);

    RegisterComponentEditor(TcyIniForm, TcyDefaultEditor);
    RegisterComponentEditor(TcySearchFiles, TcyDefaultEditor);
    RegisterComponentEditor(TcyCopyFiles, TcyDefaultEditor);
    RegisterComponentEditor(TcyFlyingContainer, TcyDefaultEditor);
    RegisterComponentEditor(TcyModalContainer, TcyDefaultEditor);
    RegisterComponentEditor(TcyResizer, TcyDefaultEditor);
    RegisterComponentEditor(TcyVirtualChart, TcyDefaultEditor);
    RegisterComponentEditor(TcyVirtualGrid, TcyDefaultEditor);
    RegisterComponentEditor(TcyProgressionPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyAdvProgressionPanel, TcyDefaultEditor);
    RegisterComponentEditor(TcyCommRoomConnector, TcyDefaultEditor);
    RegisterComponentEditor(TcyCommunicate, TcyDefaultEditor);
    RegisterComponentEditor(TcyAppInstances, TcyDefaultEditor);
    RegisterComponentEditor(TcyRunTimeResize, TcyDefaultEditor);
    RegisterComponentEditor(TcyClipBoard, TcyDefaultEditor);
    RegisterComponentEditor(TcyDDECmd, TcyDefaultEditor);
    RegisterComponentEditor(TcyMathParser, TcyDefaultEditor);
    RegisterComponentEditor(TcyDebug, TcyDefaultEditor);


    RegisterSelectionEditor(TcyEditInteger, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditFloat, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditDate, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditTime, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditFilename, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditMail, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyEditWebsite, TcyEditSelectionEditor);
    RegisterSelectionEditor(TcyMaskEdit, TcyEditSelectionEditor);
end;

end.
