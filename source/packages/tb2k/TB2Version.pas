unit TB2Version;

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    https://jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2Version.pas,v 1.61 2005/07/30 18:34:13 jr Exp $
}

interface

{$I TB2Ver.inc}

const
  Toolbar2000Version = '2.1.6';
  Toolbar2000VersionPropText = 'Toolbar2000 version ' + Toolbar2000Version;

type
  TToolbar2000Version = type string;

const
  Sig: PChar = '- ' + Toolbar2000VersionPropText +
    {$IFDEF VER90}  '/D2'+ {$ENDIF} {$IFDEF VER93}  '/CB1'+ {$ENDIF}
    {$IFDEF VER100} '/D3'+ {$ENDIF} {$IFDEF VER110} '/CB3'+ {$ENDIF}
    {$IFDEF VER120} '/D4'+ {$ENDIF} {$IFDEF VER125} '/CB4'+ {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER130} '/D5'+ {$ENDIF} {$ELSE} {$IFDEF VER130} '/CB5'+ {$ENDIF} {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER140} '/D6'+ {$ENDIF} {$ELSE} {$IFDEF VER140} '/CB6'+ {$ENDIF} {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER150} '/D7'+ {$ENDIF} {$ELSE} {$IFDEF VER150} '/CB7'+ {$ENDIF} {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER170} '/D9'+ {$ENDIF} {$ELSE} {$IFDEF VER170} '/CB9'+ {$ENDIF} {$ENDIF}
    ', Copyright (C) 1998-2005 by Jordan Russell -';

implementation

initialization
  Sig := Sig;
end.
