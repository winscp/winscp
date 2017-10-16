/** **************************************************************************
 * simplexml.c
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

#include <libxml/parser.h>
#include <string.h>
#include "simplexml.h"

// Use libxml2 for parsing XML.  XML is severely overused in modern
// computing.  It is useful for only a very small subset of tasks, but
// software developers who don't know better and are afraid to go against the
// grain use it for everything, and in most cases, it is completely
// inappropriate.  Usually, the document structure is severely under-specified
// as well, as is the case with S3.  We do our best by just caring about the
// most important aspects of the S3 "XML document" responses: the elements and
// their values.  The SAX API (just about the lamest API ever devised and
// proof that XML sucks - well, the real proof is how crappy all of the XML
// parsing libraries are, including libxml2 - but I digress) is used here
// because we don't need much from the parser and SAX is fast and low memory.
//
// Note that for simplicity we assume all ASCII here.  No attempts are made to
// detect non-ASCII sequences in utf-8 and convert them into ASCII in any way.
// S3 appears to only use ASCII anyway.


static xmlEntityPtr saxGetEntity(void *user_data, const xmlChar *name)
{
    (void) user_data;

    return xmlGetPredefinedEntity(name);
}


static void saxStartElement(void *user_data, const xmlChar *nameUtf8,
                            const xmlChar **attr)
{
    (void) attr;

    SimpleXml *simpleXml = (SimpleXml *) user_data;

    if (simpleXml->status != S3StatusOK) {
        return;
    }
    
    // Assume that name has no non-ASCII in it
    char *name = (char *) nameUtf8;

    // Append the element to the element path
    int len = strlen(name);

    if ((simpleXml->elementPathLen + len + 1) >= 
        (int) sizeof(simpleXml->elementPath)) {
        // Cannot handle this element, stop!
        simpleXml->status = S3StatusXmlParseFailure;
        return;
    }

    if (simpleXml->elementPathLen) {
        simpleXml->elementPath[simpleXml->elementPathLen++] = '/';
    }
    strcpy(&(simpleXml->elementPath[simpleXml->elementPathLen]), name);
    simpleXml->elementPathLen += len;
}


static void saxEndElement(void *user_data, const xmlChar *name)
{
    (void) name;

    SimpleXml *simpleXml = (SimpleXml *) user_data;

    if (simpleXml->status != S3StatusOK) {
        return;
    }

    // Call back with 0 data
    simpleXml->status = (*(simpleXml->callback))
        (simpleXml->elementPath, 0, 0, simpleXml->callbackData);

    while ((simpleXml->elementPathLen > 0) &&
           (simpleXml->elementPath[simpleXml->elementPathLen] != '/')) {
        simpleXml->elementPathLen--;
    }

    simpleXml->elementPath[simpleXml->elementPathLen] = 0;
}


static void saxCharacters(void *user_data, const xmlChar *ch, int len)
{
    SimpleXml *simpleXml = (SimpleXml *) user_data;

    if (simpleXml->status != S3StatusOK) {
        return;
    }

    simpleXml->status = (*(simpleXml->callback))
        (simpleXml->elementPath, (char *) ch, len, simpleXml->callbackData);
}


static void saxError(void *user_data, const char *msg, ...)
{
    (void) msg;

    SimpleXml *simpleXml = (SimpleXml *) user_data;

    if (simpleXml->status != S3StatusOK) {
        return;
    }

    simpleXml->status = S3StatusXmlParseFailure;
}


static struct _xmlSAXHandler saxHandlerG =
{
    0, // internalSubsetSAXFunc
    0, // isStandaloneSAXFunc
    0, // hasInternalSubsetSAXFunc
    0, // hasExternalSubsetSAXFunc
    0, // resolveEntitySAXFunc
    &saxGetEntity, // getEntitySAXFunc
    0, // entityDeclSAXFunc
    0, // notationDeclSAXFunc
    0, // attributeDeclSAXFunc
    0, // elementDeclSAXFunc
    0, // unparsedEntityDeclSAXFunc
    0, // setDocumentLocatorSAXFunc
    0, // startDocumentSAXFunc
    0, // endDocumentSAXFunc
    &saxStartElement, // startElementSAXFunc
    &saxEndElement, // endElementSAXFunc
    0, // referenceSAXFunc
    &saxCharacters, // charactersSAXFunc
    0, // ignorableWhitespaceSAXFunc
    0, // processingInstructionSAXFunc
    0, // commentSAXFunc
    0, // warningSAXFunc
    &saxError, // errorSAXFunc
    &saxError, // fatalErrorSAXFunc
    0, // getParameterEntitySAXFunc
    &saxCharacters, // cdataBlockSAXFunc
    0, // externalSubsetSAXFunc
    0, // initialized
    0, // _private
    0, // startElementNsSAX2Func
    0, // endElementNsSAX2Func
    0 // xmlStructuredErrorFunc serror;
};

void simplexml_initialize(SimpleXml *simpleXml, 
                          SimpleXmlCallback *callback, void *callbackData)
{
    simpleXml->callback = callback;
    simpleXml->callbackData = callbackData;
    simpleXml->elementPathLen = 0;
    simpleXml->status = S3StatusOK;
    simpleXml->xmlParser = 0;
}


void simplexml_deinitialize(SimpleXml *simpleXml)
{
    if (simpleXml->xmlParser) {
        xmlFreeParserCtxt(simpleXml->xmlParser);
    }
}


S3Status simplexml_add(SimpleXml *simpleXml, const char *data, int dataLen)
{
    if (!simpleXml->xmlParser &&
        (!(simpleXml->xmlParser = xmlCreatePushParserCtxt
           (&saxHandlerG, simpleXml, 0, 0, 0)))) {
        return S3StatusInternalError;
    }

    if (xmlParseChunk((xmlParserCtxtPtr) simpleXml->xmlParser, 
                      data, dataLen, 0)) {
        return S3StatusXmlParseFailure;
    }

    return simpleXml->status;
}
