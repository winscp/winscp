/*
 * storage.h: interface defining functions for storage and recovery
 * of PuTTY's persistent data.
 */

#ifndef PUTTY_STORAGE_H
#define PUTTY_STORAGE_H

/* ----------------------------------------------------------------------
 * Functions to save and restore PuTTY sessions. Note that this is
 * only the low-level code to do the reading and writing. The
 * higher-level code that translates a Config structure into a set
 * of (key,value) pairs is elsewhere, since it doesn't (mostly)
 * change between platforms.
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
void *open_settings_w(const char *sessionname, char **errmsg);
void write_setting_s(void *handle, const char *key, const char *value);
void write_setting_i(void *handle, const char *key, int value);
void write_setting_filename(void *handle, const char *key, Filename value);
void write_setting_fontspec(void *handle, const char *key, FontSpec font);
void close_settings_w(void *handle);

/*
 * Read a saved session. The caller is expected to call
 * open_setting_r() to get a `void *' handle, then pass that to a
 * number of calls to read_setting_s() and read_setting_i(), and
 * then close it using close_settings_r().
 * 
 * read_setting_s() writes into the provided buffer and returns a
 * pointer to the same buffer.
 * 
 * If a particular string setting is not present in the session,
 * read_setting_s() can return NULL, in which case the caller
 * should invent a sensible default. If an integer setting is not
 * present, read_setting_i() returns its provided default.
 * 
 * read_setting_filename() and read_setting_fontspec() each read into
 * the provided buffer, and return zero if they failed to.
 */
void *open_settings_r(const char *sessionname);
char *read_setting_s(void *handle, const char *key, char *buffer, int buflen);
int read_setting_i(void *handle, const char *key, int defvalue);
int read_setting_filename(void *handle, const char *key, Filename *value);
int read_setting_fontspec(void *handle, const char *key, FontSpec *font);
void close_settings_r(void *handle);

/*
 * Delete a whole saved session.
 */
void del_settings(const char *sessionname);

/*
 * Enumerate all saved sessions.
 */
void *enum_settings_start(void);
char *enum_settings_next(void *handle, char *buffer, int buflen);
void enum_settings_finish(void *handle);

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
