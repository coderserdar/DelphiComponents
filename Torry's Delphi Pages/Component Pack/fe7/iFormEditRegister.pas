unit iFormEditRegister;


interface

uses Classes, Controls, TypInfo,
     iFormEditor, iFormEditorRes, iFormEditorAction,
     iPaperModel,
     ipsConst, iGrCombo, ipsPen,
     iPropEditor,iPropTree, ipsImage,
     ipsShape,
     ipsLabel,
     iGammaPanel,iAngleSelector,ipsFormPrinter,
     iCharMap,
     ipsSlider,
     iFormEditorPreview,
     ipsBrushPanel,
     ipsObjectsCombo,

     fmpsPen, fmpsFont, fmpsBrush, fmpsShadow,

     ActnList;


procedure Register;

implementation

{$R febmp.res}

procedure Register;
begin
        // register all FormEdit library components
        RegisterComponents(psoft_palette,
           [TpsFormEditor,  TpsFormEditorActionList,  TpsFeImageList,
            TpsObjectsCombo,
            TpsFormEditorPreview,TpsFeOpenDialog,

            
            TpsPaperModel,
            TpsColorComboBox,     TpsColorListBox,
            TpsPenBrushEditor,
            TpsPropertyEditor,    TpsPropTree,
            TpsImage,
            TpsShape,
            TpsLabel,
            TpsGammaPanel,
            TpsAngleSelector,
            TpsFormPrinter,
            TpsCharMap,
            TpsSlider,
            TpsBrushPanel{,

            TFRAME_PSBRUSH,
            TFRPSFONT,
            TFRAME_PEN,
            TFR_SHADOW}
            ]);

        // unregister all form editor actions
        TpsFormEditorActionList.UnregisterFeActions;

        // register new actions
        RegisterActions(rsFeClipboard,
           [TpsFeCopy,TpsFePaste,TpsFeCut], nil);

        RegisterActions(rsFeFiles,
           [TpsFeNew, TpsFeLoad, TpsFeSave, TpsFeSaveAs,
            TpsFePrint, TpsFePreview, TpsFePrinterSetup], nil);

        RegisterActions(rsFeObject,
           [TpsFeInsert, TpsFeDelete,
            TpsFeAlign,
            TpsFeAlignLeft,TpsFeAlignRight,TpsFeAlignTop,TpsFeAlignBottom,
            TpsFeAlignCenterX, TpsFeAlignCenterY,
            TpsFeAlignCenterXWindow, TpsFeAlignCenterYWindow,
            TpsFeAlignToGrid,
            TpsFeAlignStackXTop, TpsFeAlignStackXCenter,TpsFeAlignStackXBottom,
            TpsFeAlignStackYLeft,TpsFeAlignStackYCenter,TpsFeAlignStackYRight,

            TpsFeSize,
            TpsFeSizeMinWidth,  TpsFeSizeMaxWidth,
            TpsFeSizeMinHeight, TpsFeSizeMaxHeight,

            TpsFeBringToFront,
            TpsFeSendToBack,
            TpsFeSelectAll, TpsFeUnSelectAll,
            TpsFeUndo, TpsFeUndoMultiple,
            TpsFeRedo
            ], nil);


        RegisterActions(rsFeOptions,
           [TpsFeSwitchEditMode,
            TpsFeEditorProperties,
            TpsFeControlProperties,
            TpsFeDefaultPropertyEditor,
            TpsFeDocumentProperties,
            TpsFeRuler,
            TpsFeRulerSwitchVisible,
            TpsFeRulerSwitchNumbers,
            TpsFeObjectOptions,
            TpsFeSwitchOptionLocked,
            TpsFeSwitchOptionNoPrintable,
            TpsFeSwitchOptionNoSelectable
           ],
           nil);

end;


end.
