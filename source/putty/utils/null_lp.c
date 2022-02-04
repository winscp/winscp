/*
 * Stub methods usable by LogPolicy implementations.
 */

#include "putty.h"

bool null_lp_verbose_no(LogPolicy *lp) { return false; }
bool null_lp_verbose_yes(LogPolicy *lp) { return true; }
