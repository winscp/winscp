/*
 * Implementation of open_for_write_would_lose_data for Windows.
 */

#include "putty.h"

bool open_for_write_would_lose_data(const Filename *fn)
{
    WIN32_FILE_ATTRIBUTE_DATA attrs;
    if (!GetFileAttributesEx(fn->path, GetFileExInfoStandard, &attrs)) {
        /*
         * Generally, if we don't identify a specific reason why we
         * should return true from this function, we return false, and
         * let the subsequent attempt to open the file for real give a
         * more useful error message.
         */
        return false;
    }
    if (attrs.dwFileAttributes & (FILE_ATTRIBUTE_DEVICE |
                                  FILE_ATTRIBUTE_DIRECTORY)) {
        /*
         * File is something other than an ordinary disk file, so
         * opening it for writing will not cause truncation. (It may
         * not _succeed_ either, but that's not our problem here!)
         */
        return false;
    }
    if (attrs.nFileSizeHigh == 0 && attrs.nFileSizeLow == 0) {
        /*
         * File is zero-length (or may be a named pipe, which
         * dwFileAttributes can't tell apart from a regular file), so
         * opening it for writing won't truncate any data away because
         * there's nothing to truncate anyway.
         */
        return false;
    }
    return true;
}
