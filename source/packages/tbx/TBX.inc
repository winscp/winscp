{$IFDEF JR_D7}
  {$WARN SYMBOL_PLATFORM OFF} // TBX only supports Win32
{$ENDIF}

{$DEFINE TBX_UNICODE}          // experimental
{*$DEFINE COMPATIBLE_CTL}      // register duplicate controls with older names
{*$DEFINE COMPATIBLE_GFX}      // compatibility with third-party themes
{$DEFINE WIN4_SHADOW}          // enable shadow simulation in Win98/ME

{$DEFINE NARROWCOMBOBUTTON}    // removes gap after a combo item (default theme only)
{*$DEFINE OFFICE2K_COMBOS}     // alternative border painting in popups of combo boxes

{$DEFINE NEWCOLORPROPERTY}     // register alternative color property editor

{$DEFINE NARROWCOMBOBUTTON}

