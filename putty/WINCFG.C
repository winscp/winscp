/*
 * wincfg.c - the Windows-specific parts of the PuTTY configuration
 * box.
 */

#include <assert.h>
#include <stdlib.h>

#include "putty.h"
#include "dialog.h"
#include "storage.h"

static void about_handler(union control *ctrl, void *dlg,
			  void *data, int event)
{
    HWND *hwndp = (HWND *)ctrl->generic.context.p;

    if (event == EVENT_ACTION) {
	modal_about_box(*hwndp);
    }
}

static void help_handler(union control *ctrl, void *dlg,
			 void *data, int event)
{
    HWND *hwndp = (HWND *)ctrl->generic.context.p;

    if (event == EVENT_ACTION) {
	show_help(*hwndp);
    }
}

void win_setup_config_box(struct controlbox *b, HWND *hwndp, int has_help,
			  int midsession)
{
    struct controlset *s;
    union control *c;
    char *str;

    if (!midsession) {
	/*
	 * Add the About and Help buttons to the standard panel.
	 */
	s = ctrl_getset(b, "", "", "");
	c = ctrl_pushbutton(s, "About", 'a', HELPCTX(no_help),
			    about_handler, P(hwndp));
	c->generic.column = 0;
	if (has_help) {
	    c = ctrl_pushbutton(s, "Help", 'h', HELPCTX(no_help),
				help_handler, P(hwndp));
	    c->generic.column = 1;
	}
    }

    /*
     * Full-screen mode is a Windows peculiarity; hence
     * scrollbar_in_fullscreen is as well.
     */
    s = ctrl_getset(b, "Window", "scrollback",
		    "Control the scrollback in the window");
    ctrl_checkbox(s, "Display scrollbar in full screen mode", 'i',
		  HELPCTX(window_scrollback),
		  dlg_stdcheckbox_handler,
		  I(offsetof(Config,scrollbar_in_fullscreen)));
    /*
     * Really this wants to go just after `Display scrollbar'. See
     * if we can find that control, and do some shuffling.
     */
    {
        int i;
        for (i = 0; i < s->ncontrols; i++) {
            c = s->ctrls[i];
            if (c->generic.type == CTRL_CHECKBOX &&
                c->generic.context.i == offsetof(Config,scrollbar)) {
                /*
                 * Control i is the scrollbar checkbox.
                 * Control s->ncontrols-1 is the scrollbar-in-FS one.
                 */
                if (i < s->ncontrols-2) {
                    c = s->ctrls[s->ncontrols-1];
                    memmove(s->ctrls+i+2, s->ctrls+i+1,
                            (s->ncontrols-i-2)*sizeof(union control *));
                    s->ctrls[i+1] = c;
                }
                break;
            }
        }
    }

    /*
     * Windows has the AltGr key, which has various Windows-
     * specific options.
     */
    s = ctrl_getset(b, "Terminal/Keyboard", "features",
		    "Enable extra keyboard features:");
    ctrl_checkbox(s, "AltGr acts as Compose key", 't',
		  HELPCTX(keyboard_compose),
		  dlg_stdcheckbox_handler, I(offsetof(Config,compose_key)));
    ctrl_checkbox(s, "Control-Alt is different from AltGr", 'd',
		  HELPCTX(keyboard_ctrlalt),
		  dlg_stdcheckbox_handler, I(offsetof(Config,ctrlaltkeys)));

    /*
     * Windows allows an arbitrary .WAV to be played as a bell, and
     * also the use of the PC speaker. For this we must search the
     * existing controlset for the radio-button set controlling the
     * `beep' option, and add extra buttons to it.
     * 
     * Note that although this _looks_ like a hideous hack, it's
     * actually all above board. The well-defined interface to the
     * per-platform dialog box code is the _data structures_ `union
     * control', `struct controlset' and so on; so code like this
     * that reaches into those data structures and changes bits of
     * them is perfectly legitimate and crosses no boundaries. All
     * the ctrl_* routines that create most of the controls are
     * convenient shortcuts provided on the cross-platform side of
     * the interface, and template creation code is under no actual
     * obligation to use them.
     */
    s = ctrl_getset(b, "Terminal/Bell", "style", "Set the style of bell");
    {
	int i;
	for (i = 0; i < s->ncontrols; i++) {
	    c = s->ctrls[i];
	    if (c->generic.type == CTRL_RADIO &&
		c->generic.context.i == offsetof(Config, beep)) {
		assert(c->generic.handler == dlg_stdradiobutton_handler);
		c->radio.nbuttons += 2;
		c->radio.buttons =
		    sresize(c->radio.buttons, c->radio.nbuttons, char *);
		c->radio.buttons[c->radio.nbuttons-1] =
		    dupstr("Play a custom sound file");
		c->radio.buttons[c->radio.nbuttons-2] =
		    dupstr("Beep using the PC speaker");
		c->radio.buttondata =
		    sresize(c->radio.buttondata, c->radio.nbuttons, intorptr);
		c->radio.buttondata[c->radio.nbuttons-1] = I(BELL_WAVEFILE);
		c->radio.buttondata[c->radio.nbuttons-2] = I(BELL_PCSPEAKER);
		if (c->radio.shortcuts) {
		    c->radio.shortcuts =
			sresize(c->radio.shortcuts, c->radio.nbuttons, char);
		    c->radio.shortcuts[c->radio.nbuttons-1] = NO_SHORTCUT;
		    c->radio.shortcuts[c->radio.nbuttons-2] = NO_SHORTCUT;
		}
		break;
	    }
	}
    }
    ctrl_filesel(s, "Custom sound file to play as a bell:", NO_SHORTCUT,
		 FILTER_WAVE_FILES, FALSE, "Select bell sound file",
		 HELPCTX(bell_style),
		 dlg_stdfilesel_handler, I(offsetof(Config, bell_wavefile)));

    /*
     * While we've got this box open, taskbar flashing on a bell is
     * also Windows-specific.
     */
    ctrl_radiobuttons(s, "Taskbar/caption indication on bell:", 'i', 3,
		      HELPCTX(bell_taskbar),
		      dlg_stdradiobutton_handler,
		      I(offsetof(Config, beep_ind)),
		      "Disabled", I(B_IND_DISABLED),
		      "Flashing", I(B_IND_FLASH),
		      "Steady", I(B_IND_STEADY), NULL);

    /*
     * The sunken-edge border is a Windows GUI feature.
     */
    s = ctrl_getset(b, "Window/Appearance", "border",
		    "Adjust the window border");
    ctrl_checkbox(s, "Sunken-edge border (slightly thicker)", 's',
		  HELPCTX(appearance_border),
		  dlg_stdcheckbox_handler, I(offsetof(Config,sunken_edge)));

    /*
     * Cyrillic Lock is a horrid misfeature even on Windows, and
     * the least we can do is ensure it never makes it to any other
     * platform (at least unless someone fixes it!).
     */
    s = ctrl_getset(b, "Window/Translation", "input",
		    "Enable character set translation on input data");
    ctrl_checkbox(s, "Caps Lock acts as Cyrillic switch", 's',
		  HELPCTX(translation_cyrillic),
		  dlg_stdcheckbox_handler,
		  I(offsetof(Config,xlat_capslockcyr)));

    /*
     * Windows has the weird OEM font mode, which gives us some
     * additional options when working with line-drawing
     * characters.
     */
    str = dupprintf("Adjust how %s displays line drawing characters", appname);
    s = ctrl_getset(b, "Window/Translation", "linedraw", str);
    sfree(str);
    {
	int i;
	for (i = 0; i < s->ncontrols; i++) {
	    c = s->ctrls[i];
	    if (c->generic.type == CTRL_RADIO &&
		c->generic.context.i == offsetof(Config, vtmode)) {
		assert(c->generic.handler == dlg_stdradiobutton_handler);
		c->radio.nbuttons += 3;
		c->radio.buttons =
		    sresize(c->radio.buttons, c->radio.nbuttons, char *);
		c->radio.buttons[c->radio.nbuttons-3] =
		    dupstr("Font has XWindows encoding");
		c->radio.buttons[c->radio.nbuttons-2] =
		    dupstr("Use font in both ANSI and OEM modes");
		c->radio.buttons[c->radio.nbuttons-1] =
		    dupstr("Use font in OEM mode only");
		c->radio.buttondata =
		    sresize(c->radio.buttondata, c->radio.nbuttons, intorptr);
		c->radio.buttondata[c->radio.nbuttons-3] = I(VT_XWINDOWS);
		c->radio.buttondata[c->radio.nbuttons-2] = I(VT_OEMANSI);
		c->radio.buttondata[c->radio.nbuttons-1] = I(VT_OEMONLY);
		if (!c->radio.shortcuts) {
		    int j;
		    c->radio.shortcuts = snewn(c->radio.nbuttons, char);
		    for (j = 0; j < c->radio.nbuttons; j++)
			c->radio.shortcuts[j] = NO_SHORTCUT;
		} else {
		    c->radio.shortcuts = sresize(c->radio.shortcuts,
						 c->radio.nbuttons, char);
		}
		c->radio.shortcuts[c->radio.nbuttons-3] = 'x';
		c->radio.shortcuts[c->radio.nbuttons-2] = 'b';
		c->radio.shortcuts[c->radio.nbuttons-1] = 'e';
		break;
	    }
	}
    }

    /*
     * RTF paste is Windows-specific.
     */
    s = ctrl_getset(b, "Window/Selection", "format",
		    "Formatting of pasted characters");
    ctrl_checkbox(s, "Paste to clipboard in RTF as well as plain text", 'f',
		  HELPCTX(selection_rtf),
		  dlg_stdcheckbox_handler, I(offsetof(Config,rtf_paste)));

    /*
     * Windows often has no middle button, so we supply a selection
     * mode in which the more critical Paste action is available on
     * the right button instead.
     */
    s = ctrl_getset(b, "Window/Selection", "mouse",
		    "Control use of mouse");
    ctrl_radiobuttons(s, "Action of mouse buttons:", 'm', 1,
		      HELPCTX(selection_buttons),
		      dlg_stdradiobutton_handler,
		      I(offsetof(Config, mouse_is_xterm)),
		      "Windows (Middle extends, Right brings up menu)", I(2),
		      "Compromise (Middle extends, Right pastes)", I(0),
		      "xterm (Right extends, Middle pastes)", I(1), NULL);
    /*
     * This really ought to go at the _top_ of its box, not the
     * bottom, so we'll just do some shuffling now we've set it
     * up...
     */
    c = s->ctrls[s->ncontrols-1];      /* this should be the new control */
    memmove(s->ctrls+1, s->ctrls, (s->ncontrols-1)*sizeof(union control *));
    s->ctrls[0] = c;

    /*
     * Logical palettes don't even make sense anywhere except Windows.
     */
    s = ctrl_getset(b, "Window/Colours", "general",
		    "General options for colour usage");
    ctrl_checkbox(s, "Attempt to use logical palettes", 'l',
		  HELPCTX(colours_logpal),
		  dlg_stdcheckbox_handler, I(offsetof(Config,try_palette)));
    ctrl_checkbox(s, "Use system colours", 's',
                  HELPCTX(colours_system),
                  dlg_stdcheckbox_handler, I(offsetof(Config,system_colour)));


    /*
     * Resize-by-changing-font is a Windows insanity.
     */
    s = ctrl_getset(b, "Window", "size", "Set the size of the window");
    ctrl_radiobuttons(s, "When window is resized:", 'z', 1,
		      HELPCTX(window_resize),
		      dlg_stdradiobutton_handler,
		      I(offsetof(Config, resize_action)),
		      "Change the number of rows and columns", I(RESIZE_TERM),
		      "Change the size of the font", I(RESIZE_FONT),
		      "Change font size only when maximised", I(RESIZE_EITHER),
		      "Forbid resizing completely", I(RESIZE_DISABLED), NULL);

    /*
     * Most of the Window/Behaviour stuff is there to mimic Windows
     * conventions which PuTTY can optionally disregard. Hence,
     * most of these options are Windows-specific.
     */
    s = ctrl_getset(b, "Window/Behaviour", "main", NULL);
    ctrl_checkbox(s, "Window closes on ALT-F4", '4',
		  HELPCTX(behaviour_altf4),
		  dlg_stdcheckbox_handler, I(offsetof(Config,alt_f4)));
    ctrl_checkbox(s, "System menu appears on ALT-Space", 'y',
		  HELPCTX(behaviour_altspace),
		  dlg_stdcheckbox_handler, I(offsetof(Config,alt_space)));
    ctrl_checkbox(s, "System menu appears on ALT alone", 'l',
		  HELPCTX(behaviour_altonly),
		  dlg_stdcheckbox_handler, I(offsetof(Config,alt_only)));
    ctrl_checkbox(s, "Ensure window is always on top", 'e',
		  HELPCTX(behaviour_alwaysontop),
		  dlg_stdcheckbox_handler, I(offsetof(Config,alwaysontop)));
    ctrl_checkbox(s, "Full screen on Alt-Enter", 'f',
		  HELPCTX(behaviour_altenter),
		  dlg_stdcheckbox_handler,
		  I(offsetof(Config,fullscreenonaltenter)));
}
