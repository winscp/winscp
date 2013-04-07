unit TB2DsgnConvertOptions;

{
  Toolbar2000
  Copyright (C) 1998-2005 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2DsgnConvertOptions.pas,v 1.6 2005/01/06 03:56:50 jr Exp $
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TTBConvertOptionsForm = class(TForm)
    MenuCombo: TComboBox;
    Label1: TLabel;
    ConvertButton: TButton;
    HelpButton: TButton;
    Button1: TButton;
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TTBConvertOptionsForm.HelpButtonClick(Sender: TObject);
const
  SMsg1 = 'This will import the contents of a TMainMenu or TPopupMenu ' +
    'component on the form.'#13#10#13#10 +
    'The new items will take the names of the old menu ' +
    'items. The old menu items will have "_OLD" appended to the end of ' +
    'their names.'#13#10#13#10 +
    'After the conversion process completes, you should verify that ' +
    'everything was copied correctly. Afterward, you may delete the ' +
    'old menu component.';
begin
  Application.MessageBox(SMsg1, 'Convert Help', MB_OK or MB_ICONINFORMATION);
end;

end.
