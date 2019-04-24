/*
 * storage.h: interface defining functions for storage and recovery
 * of PuTTY's persistent data.
 */

#ifndef PUTTY_STORAGE_H
#define PUTTY_STORAGE_H

/* ----------------------------------------------------------------------
 * Functions to save and restore PuTTY sessions. Note that this is
 * only the low-level code to do the reading and writing. The
 * higher-level code that translates an internal Conf structure into
 * a set of (key,value) pairs in their external storage format is
 * elsewhere, since it doesn't (mostly) change between platforms.
 */

/*
 * Write a saved session. The caller is expected to call
 * open_setting_w() to get a `void *' handle, then pass that to a
 * number of calls to write_setting_s() and write_setting_i(), and
 * then close it using close_settings_w(). At the end of this call
 * sequence the settings should have been written to the PuTTY
 * persistent storage area.
 *
 * A given key will be written at most once while saving a session.
 * Keys may be up to 255 characters long.  String values have no length
 * limit.
 * 
 * Any returned error message must be freed after use.
 */
settings_w *open_settings_w(const char *sessionname, char **errmsg);
void write_setting_s(settings_w *handle, const char *key, const char *value);
void write_setting_i(settings_w *handle, const char *key, int value);
void write_setting_filename(settings_w *handle,
                            const char *key, Filename *value);
void write_setting_fontspec(settings_w *handle,
                            const char *key, FontSpec *font);
void close_settings_w(settings_w *handle);

/*
 * Read a saved session. The caller is expected to call
 * open_setting_r() to get a `void *' handle, then pass that to a
 * number of calls to read_setting_s() and read_setting_i(), and
 * then close it using close_settings_r().
 * 
 * read_setting_s() returns a dynamically allocated string which the
 * caller must free. read_setting_filename() and
 * read_setting_fontspec() likewise return dynamically allocated
 * structures.
 * 
 * If a particular string setting is not present in the session,
 * read_setting_s() can return NULL, in which case the caller
 * should invent a sensible default. If an integer setting is not
 * present, read_setting_i() returns its provided default.
 */
settings_r *open_settings_r(const char *sessionname);
char *read_setting_s(settings_r *handle, const char *key);
int read_setting_i(settings_r *handle, const char *key, int defvalue);
Filename *read_setting_filename(settings_r *handle, const char *key);
FontSpec *read_setting_fontspec(settings_r *handle, const char *key);
void close_settings_r(settings_r *handle);

/*
 * Delete a whole saved session.
 */
void del_settings(const char *sessionname);

/*
 * Enumerate all saved sessions.
 */
settings_e *enum_settings_start(void);
bool enum_settings_next(settings_e *handle, strbuf *out);
void enum_settings_finish(settings_e *handle);

/* ----------------------------------------------------------------------
 * Functions to access PuTTY's host key database.
 */

/*
 * See if a host key matches the database entry. Return values can
 * be 0 (entry matches database), 1 (entry is absent in database),
 * or 2 (entry exists in database and is different).
 */
int verify_host_key(const char *hostname, int port,
		    const char *keytype, const char *key);

/*
 * Write a host key into the database, overwriting any previous
 * entry that might have been there.
 */
void store_host_key(const char *hostname, int port,
		    const char *keytype, const char *key);

/* ----------------------------------------------------------------------
 * Functions to access PuTTY's random number seed file.
 */

typedef void (*noise_consumer_t) (void *data, int len);

/*
 * Read PuTTY's random seed file and pass its contents to a noise
 * consumer function.
 */
void read_random_seed(noise_consumer_t consumer);

/*
 * Write PuTTY's random seed file from a given chunk of noise.
 */
void write_random_seed(void *data, int len);

/* ----------------------------------------------------------------------
 * Cleanup function: remove all of PuTTY's persistent state.
 */
void cleanup_all(void);

#endif
