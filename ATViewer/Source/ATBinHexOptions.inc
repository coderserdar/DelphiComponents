//ATBinHex options:

{$define NOTIF} //Enable file change notification code.
                //Should be enabled.

{$define PRINT} //Enable printing code.
                //Should be enabled.

//{$define PREVIEW} //Enable ATPrintPreview usage.
                    //Should be disabled.

{$define SEARCH} //Enable searching code.
                 //Should be enabled.

{$define HSCROLLBAR_WORKAROUND}
                //Enable workaround for Windows bug: when horiz scrollbar temporary hides,
                //this may cause both horiz+vertical scrollbars + window border to disappear
                //completely. This workaround rises users' question: why horiz scrollbar
                //doesn't hide when lines become short enough.
                //Should be enabled.

//{$define REGEX} //Enable DIRegEx usage. Used for URL hilighting.
                  //Should be enabled (with DIRegEx).

//{$define DEBUG_FORM}
                //Show debug form.
                //Must be commented in release.
