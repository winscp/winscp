/*
 * Exports and types from dialog.c.
 */

/*
 * This will come in handy for generic control handlers. Anyone
 * knows how to make this more portable, let me know :-)
 */
#define ATOFFSET(data, offset) ( (void *) ( (char *)(data) + (offset) ) )

/*
 * This is the big union which defines a single control, of any
 * type.
 * 
 * General principles:
 *  - _All_ pointers in this structure are expected to point to
 *    dynamically allocated things, unless otherwise indicated.
 *  - `char' fields giving keyboard shortcuts are expected to be
 *    NO_SHORTCUT if no shortcut is desired for a particular control.
 *  - The `label' field can often be NULL, which will cause the
 *    control to not have a label at all. This doesn't apply to
 *    checkboxes and push buttons, in which the label is not
 *    separate from the control.
 */

#define NO_SHORTCUT '\0'

enum {
    CTRL_TEXT,			       /* just a static line of text */
    CTRL_EDITBOX,		       /* label plus edit box */
    CTRL_RADIO,			       /* label plus radio buttons */
    CTRL_CHECKBOX,		       /* checkbox (contains own label) */
    CTRL_BUTTON,		       /* simple push button (no label) */
    CTRL_LISTBOX,		       /* label plus list box */
    CTRL_COLUMNS,		       /* divide window into columns */
    CTRL_FILESELECT,		       /* label plus filename selector */
    CTRL_FONTSELECT,		       /* label plus font selector */
    CTRL_TABDELAY		       /* see `tabdelay' below */
};

/*
 * Many controls have `intorptr' unions for storing user data,
 * since the user might reasonably want to store either an integer
 * or a void * pointer. Here I define a union, and two convenience
 * functions to create that union from actual integers or pointers.
 * 
 * The convenience functions are declared as inline if possible.
 * Otherwise, they're declared here and defined when this header is
 * included with DEFINE_INTORPTR_FNS defined. This is a total pain,
 * but such is life.
 */
typedef union { void *p; int i; } intorptr;

#ifndef INLINE
intorptr I(int i);
intorptr P(void *p);
#endif

#if defined DEFINE_INTORPTR_FNS || defined INLINE
#ifdef INLINE
#define PREFIX INLINE
#else
#define PREFIX
#endif
PREFIX intorptr I(int i) { intorptr ret; ret.i = i; return ret; }
PREFIX intorptr P(void *p) { intorptr ret; ret.p = p; return ret; }
#undef PREFIX
#endif

/*
 * Each control has an `int' field specifying which columns it
 * occupies in a multi-column part of the dialog box. These macros
 * pack and unpack that field.
 * 
 * If a control belongs in exactly one column, just specifying the
 * column number is perfectly adequate.
 */
#define COLUMN_FIELD(start, span) ( (((span)-1) << 16) + (start) )
#define COLUMN_START(field) ( (field) & 0xFFFF )
#define COLUMN_SPAN(field) ( (((field) >> 16) & 0xFFFF) + 1 )

union control;

/*
 * The number of event types is being deliberately kept small, on
 * the grounds that not all platforms might be able to report a
 * large number of subtle events. We have:
 *  - the special REFRESH event, called when a control's value
 *    needs setting
 *  - the ACTION event, called when the user does something that
 *    positively requests action (double-clicking a list box item,
 *    or pushing a push-button)
 *  - the VALCHANGE event, called when the user alters the setting
 *    of the control in a way that is usually considered to alter
 *    the underlying data (toggling a checkbox or radio button,
 *    moving the items around in a drag-list, editing an edit
 *    control)
 *  - the SELCHANGE event, called when the user alters the setting
 *    of the control in a more minor way (changing the selected
 *    item in a list box).
 *  - the CALLBACK event, which happens after the handler routine
 *    has requested a subdialog (file selector, font selector,
 *    colour selector) and it has come back with information.
 */
enum {
    EVENT_REFRESH,
    EVENT_ACTION,
    EVENT_VALCHANGE,
    EVENT_SELCHANGE,
    EVENT_CALLBACK
};
typedef void (*handler_fn)(union control *ctrl, void *dlg,
			   void *data, int event);

#define STANDARD_PREFIX \
	int type; \
	char *label; \
	int tabdelay; \
	int column; \
        handler_fn handler; \
	intorptr context; \
        intorptr helpctx

union control {
    /*
     * The first possibility in this union is the generic header
     * shared by all the structures, which we are therefore allowed
     * to access through any one of them.
     */
    struct {
	int type;
	/*
	 * Every control except CTRL_COLUMNS has _some_ sort of
	 * label. By putting it in the `generic' union as well as
	 * everywhere else, we avoid having to have an irritating
	 * switch statement when we go through and deallocate all
	 * the memory in a config-box structure.
	 * 
	 * Yes, this does mean that any non-NULL value in this
	 * field is expected to be dynamically allocated and
	 * freeable.
	 * 
	 * For CTRL_COLUMNS, this field MUST be NULL.
	 */
	char *label;
	/*
	 * If `tabdelay' is non-zero, it indicates that this
	 * particular control should not yet appear in the tab
	 * order. A subsequent CTRL_TABDELAY entry will place it.
	 */
	int tabdelay;
	/*
	 * Indicate which column(s) this control occupies. This can
	 * be unpacked into starting column and column span by the
	 * COLUMN macros above.
	 */
	int column;
	/*
	 * Most controls need to provide a function which gets
	 * called when that control's setting is changed, or when
	 * the control's setting needs initialising.
	 * 
	 * The `data' parameter points to the writable data being
	 * modified as a result of the configuration activity; for
	 * example, the PuTTY `Config' structure, although not
	 * necessarily.
	 * 
	 * The `dlg' parameter is passed back to the platform-
	 * specific routines to read and write the actual control
	 * state.
	 */
	handler_fn handler;
	/*
	 * Almost all of the above functions will find it useful to
	 * be able to store a piece of `void *' or `int' data.
	 */
	intorptr context;
	/*
	 * For any control, we also allow the storage of a piece of
	 * data for use by context-sensitive help. For example, on
	 * Windows you can click the magic question mark and then
	 * click a control, and help for that control should spring
	 * up. Hence, here is a slot in which to store per-control
	 * data that a particular platform-specific driver can use
	 * to ensure it brings up the right piece of help text.
	 */
	intorptr helpctx;
    } generic;
    struct {
	STANDARD_PREFIX;
	union control *ctrl;
    } tabdelay;
    struct {
	STANDARD_PREFIX;
    } text;
    struct {
	STANDARD_PREFIX;
	char shortcut;		       /* keyboard shortcut */
	/*
	 * Percentage of the dialog-box width used by the edit box.
	 * If this is set to 100, the label is on its own line;
	 * otherwise the label is on the same line as the box
	 * itself.
	 */
	int percentwidth;
	int password;		       /* details of input are hidden */
	/*
	 * A special case of the edit box is the combo box, which
	 * has a drop-down list built in. (Note that a _non_-
	 * editable drop-down list is done as a special case of a
	 * list box.)
	 */
	int has_list;
	/*
	 * Edit boxes tend to need two items of context, so here's
	 * a spare.
	 */
	intorptr context2;
    } editbox;
    struct {
	STANDARD_PREFIX;
	/*
	 * `shortcut' here is a single keyboard shortcut which is
	 * expected to select the whole group of radio buttons. It
	 * can be NO_SHORTCUT if required, and there is also a way
	 * to place individual shortcuts on each button; see below.
	 */
	char shortcut;
	/*
	 * There are separate fields for `ncolumns' and `nbuttons'
	 * for several reasons.
	 * 
	 * Firstly, we sometimes want the last of a set of buttons
	 * to have a longer label than the rest; we achieve this by
	 * setting `ncolumns' higher than `nbuttons', and the
	 * layout code is expected to understand that the final
	 * button should be given all the remaining space on the
	 * line. This sounds like a ludicrously specific special
	 * case (if we're doing this sort of thing, why not have
	 * the general ability to have a particular button span
	 * more than one column whether it's the last one or not?)
	 * but actually it's reasonably common for the sort of
	 * three-way control you get a lot of in PuTTY: `yes'
	 * versus `no' versus `some more complex way to decide'.
	 * 
	 * Secondly, setting `nbuttons' higher than `ncolumns' lets
	 * us have more than one line of radio buttons for a single
	 * setting. A very important special case of this is
	 * setting `ncolumns' to 1, so that each button is on its
	 * own line.
	 */
	int ncolumns;
	int nbuttons;
	/*
	 * This points to a dynamically allocated array of `char *'
	 * pointers, each of which points to a dynamically
	 * allocated string.
	 */
	char **buttons;		       /* `nbuttons' button labels */
	/*
	 * This points to a dynamically allocated array of `char'
	 * giving the individual keyboard shortcuts for each radio
	 * button. The array may be NULL if none are required.
	 */
	char *shortcuts;	       /* `nbuttons' shortcuts; may be NULL */
	/*
	 * This points to a dynamically allocated array of
	 * intorptr, giving helpful data for each button.
	 */
	intorptr *buttondata;	       /* `nbuttons' entries; may be NULL */
    } radio;
    struct {
	STANDARD_PREFIX;
	char shortcut;
    } checkbox;
    struct {
	STANDARD_PREFIX;
	char shortcut;
	/*
	 * At least Windows has the concept of a `default push
	 * button', which gets implicitly pressed when you hit
	 * Return even if it doesn't have the input focus.
	 */
	int isdefault;
	/*
	 * Also, the reverse of this: a default cancel-type button,
	 * which is implicitly pressed when you hit Escape.
	 */
	int iscancel;
    } button;
    struct {
	STANDARD_PREFIX;
	char shortcut;		       /* keyboard shortcut */
	/*
	 * Height of the list box, in approximate number of lines.
	 * If this is zero, the list is a drop-down list.
	 */
	int height;		       /* height in lines */
	/*
	 * If this is set, the list elements can be reordered by
	 * the user (by drag-and-drop or by Up and Down buttons,
	 * whatever the per-platform implementation feels
	 * comfortable with). This is not guaranteed to work on a
	 * drop-down list, so don't try it!
	 */
	int draglist;
	/*
	 * If this is non-zero, the list can have more than one
	 * element selected at a time. This is not guaranteed to
	 * work on a drop-down list, so don't try it!
	 * 
	 * Different non-zero values request slightly different
	 * types of multi-selection (this may well be meaningful
	 * only in GTK, so everyone else can ignore it if they
	 * want). 1 means the list box expects to have individual
	 * items selected, whereas 2 means it expects the user to
	 * want to select a large contiguous range at a time.
	 */
	int multisel;
	/*
	 * Percentage of the dialog-box width used by the list box.
	 * If this is set to 100, the label is on its own line;
	 * otherwise the label is on the same line as the box
	 * itself. Setting this to anything other than 100 is not
	 * guaranteed to work on a _non_-drop-down list, so don't
	 * try it!
	 */
	int percentwidth;
	/*
	 * Some list boxes contain strings that contain tab
	 * characters. If `ncols' is greater than 0, then
	 * `percentages' is expected to be non-zero and to contain
	 * the respective widths of `ncols' columns, which together
	 * will exactly fit the width of the list box. Otherwise
	 * `percentages' must be NULL.
	 */
	int ncols;		       /* number of columns */
	int *percentages;	       /* % width of each column */
    } listbox;
    struct {
	STANDARD_PREFIX;
	char shortcut;
	/*
	 * `filter' dictates what type of files will be selected by
	 * default; for example, when selecting private key files
	 * the file selector would do well to only show .PPK files
	 * (on those systems where this is the chosen extension).
	 * 
	 * The precise contents of `filter' are platform-defined,
	 * unfortunately. The special value NULL means `all files'
	 * and is always a valid fallback.
	 * 
	 * Unlike almost all strings in this structure, this value
	 * is NOT expected to require freeing (although of course
	 * you can always use ctrl_alloc if you do need to create
	 * one on the fly). This is because the likely mode of use
	 * is to define string constants in a platform-specific
	 * header file, and directly reference those. Or worse, a
	 * particular platform might choose to cast integers into
	 * this pointer type...
	 */
	char const *filter;
	/*
	 * Some systems like to know whether a file selector is
	 * choosing a file to read or one to write (and possibly
	 * create).
	 */
	int for_writing;
	/*
	 * On at least some platforms, the file selector is a
	 * separate dialog box, and contains a user-settable title.
	 * 
	 * This value _is_ expected to require freeing.
	 */
	char *title;
    } fileselect;
    struct {
	/* In this variant, `label' MUST be NULL. */
	STANDARD_PREFIX;
	int ncols;		       /* number of columns */
	int *percentages;	       /* % width of each column */
	/*
	 * Every time this control type appears, exactly one of
	 * `ncols' and the previous number of columns MUST be one.
	 * Attempting to allow a seamless transition from a four-
	 * to a five-column layout, for example, would be way more
	 * trouble than it was worth. If you must lay things out
	 * like that, define eight unevenly sized columns and use
	 * column-spanning a lot. But better still, just don't.
	 * 
	 * `percentages' may be NULL if ncols==1, to save space.
	 */
    } columns;
    struct {
	STANDARD_PREFIX;
	char shortcut;
    } fontselect;
};

#undef STANDARD_PREFIX

/*
 * `controlset' is a container holding an array of `union control'
 * structures, together with a panel name and a title for the whole
 * set. In Windows and any similar-looking GUI, each `controlset'
 * in the config will be a container box within a panel.
 * 
 * Special case: if `boxname' is NULL, the control set gives an
 * overall title for an entire panel of controls.
 */
struct controlset {
    char *pathname;		       /* panel path, e.g. "SSH/Tunnels" */
    char *boxname;		       /* internal short name of controlset */
    char *boxtitle;		       /* title of container box */
    int ncolumns;		       /* current no. of columns at bottom */
    int ncontrols;		       /* number of `union control' in array */
    int ctrlsize;		       /* allocated size of array */
    union control **ctrls;	       /* actual array */
};

/*
 * This is the container structure which holds a complete set of
 * controls.
 */
struct controlbox {
    int nctrlsets;		       /* number of ctrlsets */
    int ctrlsetsize;		       /* ctrlset size */
    struct controlset **ctrlsets;      /* actual array of ctrlsets */
    int nfrees;
    int freesize;
    void **frees;		       /* array of aux data areas to free */
};

struct controlbox *ctrl_new_box(void);
void ctrl_free_box(struct controlbox *);

/*
 * Standard functions used for populating a controlbox structure.
 */

/* Set up a panel title. */
struct controlset *ctrl_settitle(struct controlbox *,
				 char *path, char *title);
/* Retrieve a pointer to a controlset, creating it if absent. */
struct controlset *ctrl_getset(struct controlbox *,
			       char *path, char *name, char *boxtitle);
void ctrl_free_set(struct controlset *);

void ctrl_free(union control *);

/*
 * This function works like `malloc', but the memory it returns
 * will be automatically freed when the controlbox is freed. Note
 * that a controlbox is a dialog-box _template_, not an instance,
 * and so data allocated through this function is better not used
 * to hold modifiable per-instance things. It's mostly here for
 * allocating structures to be passed as control handler params.
 */
void *ctrl_alloc(struct controlbox *b, size_t size);

/*
 * Individual routines to create `union control' structures in a controlset.
 * 
 * Most of these routines allow the most common fields to be set
 * directly, and put default values in the rest. Each one returns a
 * pointer to the `union control' it created, so that final tweaks
 * can be made.
 */

/* `ncolumns' is followed by that many percentages, as integers. */
union control *ctrl_columns(struct controlset *, int ncolumns, ...);
union control *ctrl_editbox(struct controlset *, char *label, char shortcut,
			    int percentage, intorptr helpctx,
			    handler_fn handler,
			    intorptr context, intorptr context2);
union control *ctrl_combobox(struct controlset *, char *label, char shortcut,
			     int percentage, intorptr helpctx,
			     handler_fn handler,
			     intorptr context, intorptr context2);
/*
 * `ncolumns' is followed by (alternately) radio button titles and
 * intorptrs, until a NULL in place of a title string is seen. Each
 * title is expected to be followed by a shortcut _iff_ `shortcut'
 * is NO_SHORTCUT.
 */
union control *ctrl_radiobuttons(struct controlset *, char *label,
				 char shortcut, int ncolumns,
				 intorptr helpctx,
				 handler_fn handler, intorptr context, ...);
union control *ctrl_pushbutton(struct controlset *,char *label,char shortcut,
			       intorptr helpctx,
			       handler_fn handler, intorptr context);
union control *ctrl_listbox(struct controlset *,char *label,char shortcut,
			    intorptr helpctx,
			    handler_fn handler, intorptr context);
union control *ctrl_droplist(struct controlset *, char *label, char shortcut,
			     int percentage, intorptr helpctx,
			     handler_fn handler, intorptr context);
union control *ctrl_draglist(struct controlset *,char *label,char shortcut,
			     intorptr helpctx,
			     handler_fn handler, intorptr context);
union control *ctrl_filesel(struct controlset *,char *label,char shortcut,
			    char const *filter, int write, char *title,
			    intorptr helpctx,
			    handler_fn handler, intorptr context);
union control *ctrl_fontsel(struct controlset *,char *label,char shortcut,
			    intorptr helpctx,
			    handler_fn handler, intorptr context);
union control *ctrl_text(struct controlset *, char *text, intorptr helpctx);
union control *ctrl_checkbox(struct controlset *, char *label, char shortcut,
			     intorptr helpctx,
			     handler_fn handler, intorptr context);
union control *ctrl_tabdelay(struct controlset *, union control *);

/*
 * Standard handler routines to cover most of the common cases in
 * the config box.
 */
/*
 * The standard radio-button handler expects the main `context'
 * field to contain the `offsetof' of an int field in the structure
 * pointed to by `data', and expects each of the individual button
 * data to give a value for that int field.
 */
void dlg_stdradiobutton_handler(union control *ctrl, void *dlg,
				void *data, int event);
/*
 * The standard checkbox handler expects the main `context' field
 * to contain the `offsetof' an int field in the structure pointed
 * to by `data', optionally ORed with CHECKBOX_INVERT to indicate
 * that the sense of the datum is opposite to the sense of the
 * checkbox.
 */
#define CHECKBOX_INVERT (1<<30)
void dlg_stdcheckbox_handler(union control *ctrl, void *dlg,
			     void *data, int event);
/*
 * The standard edit-box handler expects the main `context' field
 * to contain the `offsetof' a field in the structure pointed to by
 * `data'. The secondary `context2' field indicates the type of
 * this field:
 * 
 *  - if context2 > 0, the field is a char array and context2 gives
 *    its size.
 *  - if context2 == -1, the field is an int and the edit box is
 *    numeric.
 *  - if context2 < -1, the field is an int and the edit box is
 *    _floating_, and (-context2) gives the scale. (E.g. if
 *    context2 == -1000, then typing 1.2 into the box will set the
 *    field to 1200.)
 */
void dlg_stdeditbox_handler(union control *ctrl, void *dlg,
			    void *data, int event);
/*
 * The standard file-selector handler expects the main `context'
 * field to contain the `offsetof' a Filename field in the
 * structure pointed to by `data'.
 */
void dlg_stdfilesel_handler(union control *ctrl, void *dlg,
			    void *data, int event);
/*
 * The standard font-selector handler expects the main `context'
 * field to contain the `offsetof' a Font field in the structure
 * pointed to by `data'.
 */
void dlg_stdfontsel_handler(union control *ctrl, void *dlg,
			    void *data, int event);

/*
 * Routines the platform-independent dialog code can call to read
 * and write the values of controls.
 */
void dlg_radiobutton_set(union control *ctrl, void *dlg, int whichbutton);
int dlg_radiobutton_get(union control *ctrl, void *dlg);
void dlg_checkbox_set(union control *ctrl, void *dlg, int checked);
int dlg_checkbox_get(union control *ctrl, void *dlg);
void dlg_editbox_set(union control *ctrl, void *dlg, char const *text);
void dlg_editbox_get(union control *ctrl, void *dlg, char *buffer, int length);
/* The `listbox' functions can also apply to combo boxes. */
void dlg_listbox_clear(union control *ctrl, void *dlg);
void dlg_listbox_del(union control *ctrl, void *dlg, int index);
void dlg_listbox_add(union control *ctrl, void *dlg, char const *text);
/*
 * Each listbox entry may have a numeric id associated with it.
 * Note that some front ends only permit a string to be stored at
 * each position, which means that _if_ you put two identical
 * strings in any listbox then you MUST not assign them different
 * IDs and expect to get meaningful results back.
 */
void dlg_listbox_addwithid(union control *ctrl, void *dlg,
			   char const *text, int id);
int dlg_listbox_getid(union control *ctrl, void *dlg, int index);
/* dlg_listbox_index returns <0 if no single element is selected. */
int dlg_listbox_index(union control *ctrl, void *dlg);
int dlg_listbox_issel(union control *ctrl, void *dlg, int index);
void dlg_listbox_select(union control *ctrl, void *dlg, int index);
void dlg_text_set(union control *ctrl, void *dlg, char const *text);
void dlg_filesel_set(union control *ctrl, void *dlg, Filename fn);
void dlg_filesel_get(union control *ctrl, void *dlg, Filename *fn);
void dlg_fontsel_set(union control *ctrl, void *dlg, FontSpec fn);
void dlg_fontsel_get(union control *ctrl, void *dlg, FontSpec *fn);
/*
 * Bracketing a large set of updates in these two functions will
 * cause the front end (if possible) to delay updating the screen
 * until it's all complete, thus avoiding flicker.
 */
void dlg_update_start(union control *ctrl, void *dlg);
void dlg_update_done(union control *ctrl, void *dlg);
/*
 * Set input focus into a particular control.
 */
void dlg_set_focus(union control *ctrl, void *dlg);
/*
 * Return the `ctrl' structure for the most recent control that had
 * the input focus apart from the one mentioned. This is NOT
 * GUARANTEED to work on all platforms, so don't base any critical
 * functionality on it!
 */
union control *dlg_last_focused(union control *ctrl, void *dlg);
/*
 * During event processing, you might well want to give an error
 * indication to the user. dlg_beep() is a quick and easy generic
 * error; dlg_error() puts up a message-box or equivalent.
 */
void dlg_beep(void *dlg);
void dlg_error_msg(void *dlg, char *msg);
/*
 * This function signals to the front end that the dialog's
 * processing is completed, and passes an integer value (typically
 * a success status).
 */
void dlg_end(void *dlg, int value);

/*
 * Routines to manage a (per-platform) colour selector.
 * dlg_coloursel_start() is called in an event handler, and
 * schedules the running of a colour selector after the event
 * handler returns. The colour selector will send EVENT_CALLBACK to
 * the control that spawned it, when it's finished;
 * dlg_coloursel_results() fetches the results, as integers from 0
 * to 255; it returns nonzero on success, or zero if the colour
 * selector was dismissed by hitting Cancel or similar.
 * 
 * dlg_coloursel_start() accepts an RGB triple which is used to
 * initialise the colour selector to its starting value.
 */
void dlg_coloursel_start(union control *ctrl, void *dlg,
			 int r, int g, int b);
int dlg_coloursel_results(union control *ctrl, void *dlg,
			  int *r, int *g, int *b);

/*
 * This routine is used by the platform-independent code to
 * indicate that the value of a particular control is likely to
 * have changed. It triggers a call of the handler for that control
 * with `event' set to EVENT_REFRESH.
 * 
 * If `ctrl' is NULL, _all_ controls in the dialog get refreshed
 * (for loading or saving entire sets of settings).
 */
void dlg_refresh(union control *ctrl, void *dlg);

/*
 * It's perfectly possible that individual controls might need to
 * allocate or store per-dialog-instance data, so here's a
 * mechanism.
 * 
 * `dlg_get_privdata' and `dlg_set_privdata' allow the user to get
 * and set a void * pointer associated with the control in
 * question. `dlg_alloc_privdata' will allocate memory, store a
 * pointer to that memory in the private data field, and arrange
 * for it to be automatically deallocated on dialog cleanup.
 */
void *dlg_get_privdata(union control *ctrl, void *dlg);
void dlg_set_privdata(union control *ctrl, void *dlg, void *ptr);
void *dlg_alloc_privdata(union control *ctrl, void *dlg, size_t size);

/*
 * Standard helper functions for reading a controlbox structure.
 */

/*
 * Find the index of next controlset in a controlbox for a given
 * path, or -1 if no such controlset exists. If -1 is passed as
 * input, finds the first. Intended usage is something like
 * 
 * 	for (index=-1; (index=ctrl_find_path(ctrlbox, index, path)) >= 0 ;) {
 *          ... process this controlset ...
 *      }
 */
int ctrl_find_path(struct controlbox *b, char *path, int index);
int ctrl_path_elements(char *path);
/* Return the number of matching path elements at the starts of p1 and p2,
 * or INT_MAX if the paths are identical. */
int ctrl_path_compare(char *p1, char *p2);
