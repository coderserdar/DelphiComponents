unit EkTypes;

interface

uses Graphics;

//==============================================================================

type TEkLEDValClip = ( elcNone, elcMax, elcZero );
type TEkLEDTypes = ( eltSingle, eltDual, eltTriple, eltQuad );
type TEkCheckStates = (ecCheckOver, ecCheck, ecCheckDown,
ecUnCheckOver, ecUnCheck, ecUnCheckDown);
type TEkMouseStates = (emUp, emDown, emEnter, emLeave, emDisabled);
type TEkBorders = (ebNone, ebRound, ebSingle, ebSunken, ebDeepSunken,
                      ebRaised, ebFatRaised);
type TEkFillStyles = (efDashed, efSolid);
type TEkGradientStyles = (egHorizontal, egVertical);
type TEkDirection = (edForward, edReverse);
type TEkAnimateFx = (efxNone, efx1, efx2, efx3, efx4, efx5);
type TEkArrangement = (eaHorizontal, eaVertical);
type TEkTextEllipsis = (etlNone, etlEndEllipsis, etlPathEllipsis);
type TEkFadeFx =
(efxfTween, efxfTweenFade, efxfFade1, efxfFade2,
efxfInvert, efxfGreyscale, efxfMess, efxfFlash,
efxfBuild, efxfColour1, efxfColour2);

//==============================================================================

implementation

end.
