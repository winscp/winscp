/** **************************************************************************
 * request_context.c
 * 
 * Copyright 2008 Bryan Ischo <bryan@ischo.com>
 *
 * This file is part of libs3.
 *
 * libs3 is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, version 3 or above of the License.  You can also
 * redistribute and/or modify it under the terms of the GNU General Public
 * License, version 2 or above of the License.
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
 * You should also have received a copy of the GNU General Public License
 * version 2 along with libs3, in a file named COPYING-GPLv2.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 ************************************************************************** **/

#include <curl/curl.h>
#include <stdlib.h>
#include <sys/select.h>
#include "request.h"
#include "request_context.h"


S3Status S3_create_request_context_ex(S3RequestContext **requestContextReturn,
                                      CURLM *curlm,
                                      S3SetupCurlCallback setupCurlCallback,
                                      void *setupCurlCallbackData)
{
    *requestContextReturn = 
        (S3RequestContext *) malloc(sizeof(S3RequestContext));
    
    if (!*requestContextReturn) {
        return S3StatusOutOfMemory;
    }
    
    if (curlm) {
        (*requestContextReturn)->curlm = curlm;
        (*requestContextReturn)->curl_mode = S3CurlModeMultiSocket;
    }
    else {
        if (!((*requestContextReturn)->curlm = curl_multi_init())) {
            free(*requestContextReturn);
            return S3StatusOutOfMemory;
        }

        (*requestContextReturn)->curl_mode = S3CurlModeMultiPerform;
    }

    (*requestContextReturn)->requests = 0;
    (*requestContextReturn)->verifyPeer = 0;
    (*requestContextReturn)->verifyPeerSet = 0;
    (*requestContextReturn)->setupCurlCallback = setupCurlCallback;
    (*requestContextReturn)->setupCurlCallbackData = setupCurlCallbackData;

    return S3StatusOK;
}


S3Status S3_create_request_context(S3RequestContext **requestContextReturn)
{
    return S3_create_request_context_ex(requestContextReturn, NULL, NULL, NULL);
}


void S3_destroy_request_context(S3RequestContext *requestContext)
{
    // For each request in the context, remove curl handle, call back its done
    // method with 'interrupted' status
    Request *r = requestContext->requests, *rFirst = r;
    
    if (r) do {
        r->status = S3StatusInterrupted;
        // remove easy handle from a multi session
        curl_multi_remove_handle(requestContext->curlm, r->curl);
        Request *rNext = r->next;
        request_finish(r);
        r = rNext;
    } while (r != rFirst);

    if (requestContext->curl_mode == S3CurlModeMultiPerform)
        curl_multi_cleanup(requestContext->curlm);

    free(requestContext);
}


S3Status S3_runall_request_context(S3RequestContext *requestContext)
{
    int requestsRemaining;
    do {
        fd_set readfds, writefds, exceptfds;
        FD_ZERO(&readfds);
        FD_ZERO(&writefds);
        FD_ZERO(&exceptfds);
        int maxfd;
        S3Status status = S3_get_request_context_fdsets
            (requestContext, &readfds, &writefds, &exceptfds, &maxfd);
        if (status != S3StatusOK) {
            return status;
        }
        // curl will return -1 if it hasn't even created any fds yet because
        // none of the connections have started yet.  In this case, don't
        // do the select at all, because it will wait forever; instead, just
        // skip it and go straight to running the underlying CURL handles
        if (maxfd != -1) {
            int64_t timeout = S3_get_request_context_timeout(requestContext);
            struct timeval tv = { timeout / 1000, (timeout % 1000) * 1000 };
            select(maxfd + 1, &readfds, &writefds, &exceptfds,
                   (timeout == -1) ? 0 : &tv);
        }
        status = S3_runonce_request_context(requestContext,
                                            &requestsRemaining);
        if (status != S3StatusOK) {
            return status;
        }
    } while (requestsRemaining);
    
    return S3StatusOK;
}


static S3Status process_request_context(S3RequestContext *requestContext, int *retry)
{
    CURLMsg *msg;
    int junk;

    *retry = 0;

    while ((msg = curl_multi_info_read(requestContext->curlm, &junk))) {
        if (msg->msg != CURLMSG_DONE) {
            return S3StatusInternalError;
        }
        Request *request;
        if (curl_easy_getinfo(msg->easy_handle, CURLINFO_PRIVATE,
                              (char **) (char *) &request) != CURLE_OK) {
            return S3StatusInternalError;
        }
        // Remove the request from the list of requests
        if (request->prev == request->next) {
            // It was the only one on the list
            requestContext->requests = 0;
        }
        else {
            // It doesn't matter what the order of them are, so just in
            // case request was at the head of the list, put the one after
            // request to the head of the list
            requestContext->requests = request->next;
            request->prev->next = request->next;
            request->next->prev = request->prev;
        }
        if ((msg->data.result != CURLE_OK) &&
            (request->status == S3StatusOK)) {
            request->status = request_curl_code_to_status(
                msg->data.result);
        }
        if (curl_multi_remove_handle(requestContext->curlm,
                                     msg->easy_handle) != CURLM_OK) {
            return S3StatusInternalError;
        }
        // Finish the request, ensuring that all callbacks have been made,
        // and also releases the request
        request_finish(request);
        // Now, since a callback was made, there may be new requests
        // queued up to be performed immediately, so do so
        *retry = 1;
    }

    return S3StatusOK;
}


S3Status S3_runonce_request_context(S3RequestContext *requestContext, 
                                    int *requestsRemainingReturn)
{
    S3Status s3_status;
    CURLMcode status;
    int retry;

    do {
        status = curl_multi_perform(requestContext->curlm,
                                    requestsRemainingReturn);

        switch (status) {
        case CURLM_OK:
        case CURLM_CALL_MULTI_PERFORM:
            break;
        case CURLM_OUT_OF_MEMORY:
            return S3StatusOutOfMemory;
        default:
            return S3StatusInternalError;
        }

        s3_status = process_request_context(requestContext, &retry);
    } while (s3_status == S3StatusOK &&
             (status == CURLM_CALL_MULTI_PERFORM || retry));

    return s3_status;
}


S3Status S3_process_request_context(S3RequestContext *requestContext)
{
    int retry;
    /* In curl_multi_socket_action mode any new requests created during
       the following call will have already started associated socket
       operations, so no need to retry here */
    return process_request_context(requestContext, &retry);
}


S3Status S3_get_request_context_fdsets(S3RequestContext *requestContext,
                                       fd_set *readFdSet, fd_set *writeFdSet,
                                       fd_set *exceptFdSet, int *maxFd)
{
    return ((curl_multi_fdset(requestContext->curlm, readFdSet, writeFdSet,
                              exceptFdSet, maxFd) == CURLM_OK) ?
            S3StatusOK : S3StatusInternalError);
}

int64_t S3_get_request_context_timeout(S3RequestContext *requestContext)
{
    long timeout;

    if (curl_multi_timeout(requestContext->curlm, &timeout) != CURLM_OK) {
        timeout = 0;
    }
    
    return timeout;
}

void S3_set_request_context_verify_peer(S3RequestContext *requestContext,
                                        int verifyPeer)
{
    requestContext->verifyPeerSet = 1;
    requestContext->verifyPeer = (verifyPeer != 0);
}
