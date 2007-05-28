#include "stdafx.h"
#include "CBase64Coding.hpp"
#pragma hdrstop

#ifdef MPEXT
#pragma warn -inl
#endif

#define CARRIAGE_RETURN (13)
#define LINE_FEED       (10)

/*
** Author: Samuel R. Blackburn
** Internet: wfc@pobox.com
**
** You can use it any way you like as long as you don't try to sell it.
**
** Any attempt to sell WFC in source code form must have the permission
** of the original author. You can produce commercial executables with
** WFC but you can't sell WFC.
**
** Copyright, 2000, Samuel R. Blackburn
**
** Workfile: CBase64Coding.cpp
** Revision: 1.3
** Modtime: 5/12/00 3:39p
** Reuse Tracing Code: 1
*/

//Modified for use with CAsyncProxySocket, removed tracing code

#if defined( _DEBUG ) && ! defined( WFC_STL )
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#define new DEBUG_NEW
#endif // _DEBUG

#define END_OF_BASE64_ENCODED_DATA           ('=')
#define BASE64_END_OF_BUFFER                 (0xFD)
#define BASE64_IGNORABLE_CHARACTER           (0xFE)
#define BASE64_UNKNOWN_VALUE                 (0xFF)
#define BASE64_NUMBER_OF_CHARACTERS_PER_LINE (72)

static inline BYTE __get_character( const BYTE * buffer, const BYTE * decoder_table, int& index, int size_of_buffer )
{
   BYTE return_value = 0;

   do
   {
      if ( index >= size_of_buffer )
      {
         return( BASE64_END_OF_BUFFER );
      }

      return_value = buffer[ index ];
      index++;
   }
   while( return_value != END_OF_BASE64_ENCODED_DATA &&
          decoder_table[ return_value ] == BASE64_IGNORABLE_CHARACTER );

   return( return_value );
}

CBase64Coding::CBase64Coding()
{
}

CBase64Coding::~CBase64Coding()
{
}

BOOL CBase64Coding::Encode( const char * source, int len, char * destination_string )
{

   const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   int loop_index                = 0;
   int number_of_bytes_to_encode = len;

   BYTE byte_to_add = 0;
   BYTE byte_1      = 0;
   BYTE byte_2      = 0;
   BYTE byte_3      = 0;

   DWORD number_of_bytes_encoded = (DWORD) ( (double) number_of_bytes_to_encode / (double) 0.75 ) + 1;

   // Now add in the CR/LF pairs, each line is truncated at 72 characters

   // 2000-05-12
   // Thanks go to Ilia Golubev (ilia@varicom.co.il) for finding a bug here.
   // I was using number_of_bytes_to_encode rather than number_of_bytes_encoded.

   number_of_bytes_encoded += (DWORD)( ( ( number_of_bytes_encoded / BASE64_NUMBER_OF_CHARACTERS_PER_LINE ) + 1 ) * 2 );

   char * destination = destination_string;

   number_of_bytes_encoded = 0;

   while( loop_index < number_of_bytes_to_encode )
   {
      // Output the first byte

      byte_1 = source[ loop_index ];
      byte_to_add = alphabet[ ( byte_1 >> 2 ) ];

      destination[number_of_bytes_encoded ] = static_cast<char>( byte_to_add );
      number_of_bytes_encoded++;

      loop_index++;

      if ( loop_index >= number_of_bytes_to_encode )
      {
         // We're at the end of the data to encode

         byte_2 = 0;
         byte_to_add = alphabet[ ( ( ( byte_1 & 0x03 ) << 4 ) | ( ( byte_2 & 0xF0 ) >> 4 ) ) ];

         destination[ number_of_bytes_encoded ] = byte_to_add;
         number_of_bytes_encoded++;

         destination[ number_of_bytes_encoded ] =  END_OF_BASE64_ENCODED_DATA;
         number_of_bytes_encoded++;

         destination[ number_of_bytes_encoded ] =  END_OF_BASE64_ENCODED_DATA;

         // 1999-09-01
         // Thanks go to Yurong Lin (ylin@dial.pipex.com) for finding a bug here.
         // We must NULL terminate the string before letting CString have the buffer back.

         destination[ number_of_bytes_encoded + 1 ] = 0;

         return( TRUE );
      }
      else
      {
         byte_2 = source[ loop_index ];
      }

      byte_to_add = alphabet[ ( ( ( byte_1 & 0x03 ) << 4 ) | ( ( byte_2 & 0xF0 ) >> 4 ) ) ];

      destination[ number_of_bytes_encoded ] = byte_to_add;
      number_of_bytes_encoded++;

      loop_index++;

      if ( loop_index >= number_of_bytes_to_encode )
      {
         // We ran out of bytes, we need to add the last half of byte_2 and pad
         byte_3 = 0;

         byte_to_add = alphabet[ ( ( ( byte_2 & 0x0F ) << 2 ) | ( ( byte_3 & 0xC0 ) >> 6 ) ) ];

         destination[ number_of_bytes_encoded ] = byte_to_add;
         number_of_bytes_encoded++;

         destination[ number_of_bytes_encoded ] = END_OF_BASE64_ENCODED_DATA;

         // 1999-09-01
         // Thanks go to Yurong Lin (ylin@dial.pipex.com) for finding a bug here.
         // We must NULL terminate the string before letting CString have the buffer back.

         destination[ number_of_bytes_encoded + 1 ] = 0;

         return( TRUE );
      }
      else
      {
         byte_3 = source[ loop_index ];
      }

      loop_index++;

      byte_to_add = alphabet[ ( ( ( byte_2 & 0x0F ) << 2 ) | ( ( byte_3 & 0xC0 ) >> 6 ) ) ];

      destination[ number_of_bytes_encoded ] = byte_to_add;
      number_of_bytes_encoded++;

      byte_to_add = alphabet[ ( byte_3 & 0x3F ) ];

      destination[ number_of_bytes_encoded ] = byte_to_add;
      number_of_bytes_encoded++;

      if ( ( number_of_bytes_encoded % BASE64_NUMBER_OF_CHARACTERS_PER_LINE ) == 0 )
      {
         destination[ number_of_bytes_encoded ] = CARRIAGE_RETURN;
         number_of_bytes_encoded++;

         destination[ number_of_bytes_encoded ] = LINE_FEED;
         number_of_bytes_encoded++;
      }
   }

   destination[ number_of_bytes_encoded ] = END_OF_BASE64_ENCODED_DATA;

   // 1999-09-01
   // Thanks go to Yurong Lin (ylin@dial.pipex.com) for finding a bug here.
   // We must NULL terminate the string before letting CString have the buffer back.

   destination[ number_of_bytes_encoded + 1 ] = 0;

   return( TRUE );
}

// End of source

#if 0
<HTML>

<HEAD>
<TITLE>WFC - CBase64Coding</TITLE>
<META name="keywords" content="WFC, MFC extension library, freeware class library, Win32, MIME encoding, base 64, source code">
<META name="description" content="This C++ class let's you MIME encode bytes to text using base64.">
</HEAD>

<BODY>

<H1>CBase64Coding</H1>

$Revision: 1.3 $<BR><HR>

<H2>Description</H2>

This class gives you the ability to encode/decode data using base64.

<H2>Constructors</H2>

<DL COMPACT>

<DT><PRE><B>CBase64Coding</B>()<DD>
Constructs this object.

</DL>

<H2>Methods</H2>

<DL COMPACT>

<DT><PRE>BOOL <B><A NAME="Decode">Decode</A></B>( const CByteArray&amp; source, CByteArray&amp; destination )
BOOL <B>Decode</B>( const CString&amp;    source, CByteArray&amp; destination )</PRE><DD>
This method takes base64 encoded text and produces the bytes. It decodes
the base64 encoding.

<DT><PRE>BOOL <B><A NAME="Encode">Encode</A></B>( const CByteArray&amp; source, CByteArray&amp; destination )
BOOL <B>Encode</B>( const CByteArray&amp; source, CString&amp;    destination )</PRE><DD>
This method takes bytes and turns them into base64 text.

</DL>

<H2>Example</H2>
<PRE><CODE>#include &lt;wfc.h&gt;

int _tmain( int number_of_command_line_arguments, LPCTSTR command_line_arguments[] )
{
   <A HREF="WfcTrace.htm">WFCTRACEINIT</A>( TEXT( &quot;_tmain()&quot; ) );

   CByteArray bytes;

   get_file_contents( command_line_arguments[ 0 ], bytes );

   <B>CBase64Coding</B> encoder;

   CString encoded_data;

   if ( encoder.Encode( bytes, encoded_data ) != FALSE )
   {
      _tprintf( TEXT( &quot;%s\n&quot;, (LPCTSTR) encoded_data );
   }
}</CODE></PRE>
<HR><I>Copyright, 2000, <A HREF="mailto:wfc@pobox.com">Samuel R. Blackburn</A></I><BR>
$Workfile: CBase64Coding.cpp $<BR>
$Modtime: 5/12/00 3:39p $
</BODY>

</HTML>
#endif
