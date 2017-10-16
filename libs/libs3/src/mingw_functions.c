/** **************************************************************************
 * mingw_functions.c
 * 
 * Copyright 2008 Bryan Ischo <bryan@ischo.com>
 * 
 * This file is part of libs3.
 * 
 * libs3 is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, version 3 of the License.
 *
 * In addition, as a special exception, the copyright holders give
 * permission to link the code of this library and its programs with the
 * OpenSSL library, and distribute linked combinations including the two.
 *
 * libs3 is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * version 3 along with libs3, in a file named COPYING.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 ************************************************************************** **/

#include <pthread.h>
#include <sys/utsname.h>

unsigned long pthread_self()
{
    return (unsigned long) GetCurrentThreadId();
}


int pthread_mutex_init(pthread_mutex_t *mutex, void *v)
{
    (void) v;

    InitializeCriticalSection(&(mutex->criticalSection));

    return 0;
}


int pthread_mutex_lock(pthread_mutex_t *mutex)
{
    EnterCriticalSection(&(mutex->criticalSection));

    return 0;
}


int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
    LeaveCriticalSection(&(mutex->criticalSection));

    return 0;
}


int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
    DeleteCriticalSection(&(mutex->criticalSection));

    return 0;
}


int uname(struct utsname *u)
{
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(info);

    if (!GetVersionEx(&info)) {
        return -1;
    }

    u->machine = "";

    switch (info.dwMajorVersion) {
    case 4:
        switch (info.dwMinorVersion) {
        case 0:
            u->sysname = "Microsoft Windows NT 4.0";
            break;
        case 10:
            u->sysname = "Microsoft Windows 98";
            break;
        case 90:
            u->sysname = "Microsoft Windows Me";
            break;
        default:
            return -1;
        }
        break;

    case 5:
        switch (info.dwMinorVersion) {
        case 0:
            u->sysname = "Microsoft Windows 2000";
            break;
        case 1:
            u->sysname = "Microsoft Windows XP";
            break;
        case 2:
            u->sysname = "Microsoft Server 2003";
            break;
        default:
            return -1;
        }
        break;

    default:
        return -1;
    }

    return 0;
}
