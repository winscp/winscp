/************************************************************************
 * $Id: minibidi.c,v 1.1 2004/05/22 10:36:50 simon Exp $
 *
 * ------------
 * Description:
 * ------------
 * This is an implemention of Unicode's Bidirectional Algorithm
 * (known as UAX #9).
 *
 *   http://www.unicode.org/reports/tr9/
 * 
 * Author: Ahmad Khalifa
 *
 * -----------------
 * Revision Details:    (Updated by Revision Control System)
 * -----------------
 *  $Date: 2004/05/22 10:36:50 $
 *  $Author: simon $
 *  $Revision: 1.1 $
 *  $Source: /home/cvs/putty/minibidi.c,v $
 *
 * (www.arabeyes.org - under MIT license)
 *
 ************************************************************************/

/*
 * TODO:
 * =====
 * - Explicit marks need to be handled (they are not 100% now)
 * - Ligatures
 */

#include "minibidi.h"

/*
 * Flips the text buffer, according to max level, and
 * all higher levels
 * 
 * Input:
 * from: text buffer, on which to apply flipping
 * level: resolved levels buffer
 * max: the maximum level found in this line (should be unsigned char)
 * count: line size in bidi_char
 */
void flipThisRun(bidi_char *from, unsigned char *level, int max, int count)
{
    int i, j, rcount, tlevel;
    bidi_char temp;

    j = i = 0;
    while(i<count && j<count)
    {

	/* find the start of the run of level=max */
	tlevel = max;
	i = j = findIndexOfRun(level, i, count, max);
	/* find the end of the run */
	while(tlevel <= level[i] && i<count)
	{
	    i++;
	}
	rcount = i-j;
	for(; rcount>((i-j)/2); rcount--)
	{
	    temp = from[j+rcount-1];
	    from[j+rcount-1] = from[i-rcount];
	    from[i-rcount] = temp;
	}
    }
}

/*
 * Finds the index of a run with level equals tlevel
 */
int findIndexOfRun(unsigned char* level , int start, int count, int tlevel)
{
    int i;
    for(i=start; i<count; i++)
    {
	if(tlevel == level[i])
	{
	    return i;
	}
    }
    return count;
}

/*
 * Returns character type of ch, by calling RLE table lookup
 * function
 */
unsigned char getType(wchar_t ch)
{
    return getRLE(ch);
}

/*
 * The most significant 2 bits of each level are used to store
 * Override status of each character
 * This function sets the override bits of level according
 * to the value in override, and reurns the new byte.
 */
unsigned char setOverrideBits(unsigned char level, unsigned char override)
{
    if(override == ON)
	return level;
    else if(override == R)
	return level | OISR;
    else if(override == L)
	return level | OISL;
    return level;
}

/* Dont remember what this was used for :-) */
unsigned char getPreviousLevel(unsigned char* level, int from)
{
    unsigned char current;
    from--;
    current = level[from];
    while(from>0 && level[from] == current)
    {
	from--;
    }
    return level[++from];
}

/*
 * Returns the first odd value greater than x
 */
unsigned char leastGreaterOdd(unsigned char x)
{
    if((x % 2) == 0)
	return x+1;
    else
	return x+2;
}

/*
 * Returns the first even value greater than x
 */
unsigned char leastGreaterEven(unsigned char x)
{
    if((x % 2) == 0)
	return x+2;
    else
	return x+1;
}

/*
 * Loops over the RLE_table array looking for the
 * type of ch
 */
unsigned char getRLE(wchar_t ch)
{
    int offset, i, freq;

    freq = offset = 0;
    for(i=0; i<0xFFFF; i++)
    {
	freq = ((RLENode*)RLE_table)[i].f;
	offset += freq;
	if(offset == ch)
	    return ((RLENode*)RLE_table)[i].d;
	else if(offset > ch)
	    return ((RLENode*)RLE_table)[i-1].d;
    }
    /* this is here to stop compiler nagging */
    return ON;
}

/* The Main shaping function, and the only one to be used
 * by the outside world.
 *
 * line: buffer to apply shaping to. this must be passed by doBidi() first
 * to: output buffer for the shaped data
 * count: number of characters in line
 */
int do_shape(bidi_char *line, bidi_char *to, int count)
{
    int i, tempShape, ligFlag;

    for(ligFlag=i=0; i<count; i++)
    {
	to[i] = line[i];
	tempShape = STYPE(line[i].wc);
	switch(tempShape )
	{
	  case SC:
	    break;

	  case SU:
	    break;

	  case SR:
	    tempShape = STYPE(line[i+1].wc);
	    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
		to[i].wc = SFINAL((SISOLATED(line[i].wc)));
	    else
		to[i].wc = SISOLATED(line[i].wc);
	    break;


	  case SD:
	    /* Make Ligatures */
	    tempShape = STYPE(line[i+1].wc);
	    if(line[i].wc == 0x644)
	    {
		switch(line[i-1].wc)
		{
		  case 0x622:
		    ligFlag = 1;
		    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
			to[i].wc = 0xFEF6;
		    else
			to[i].wc = 0xFEF5;
		    break;
		  case 0x623:
		    ligFlag = 1;
		    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
			to[i].wc = 0xFEF8;
		    else
			to[i].wc = 0xFEF7;
		    break;
		  case 0x625:
		    ligFlag = 1;
		    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
			to[i].wc = 0xFEFA;
		    else
			to[i].wc = 0xFEF9;
		    break;
		  case 0x627:
		    ligFlag = 1;
		    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
			to[i].wc = 0xFEFC;
		    else
			to[i].wc = 0xFEFB;
		    break;
		}
		if(ligFlag)
		{
		    to[i-1].wc = 0x20;
		    ligFlag = 0;
		    break;
		}
	    }

	    if((tempShape == SL) || (tempShape == SD) || (tempShape == SC))
	    {
		tempShape = STYPE(line[i-1].wc);
		if((tempShape == SR) || (tempShape == SD) || (tempShape == SC))
		    to[i].wc = SMEDIAL( (SISOLATED(line[i].wc)) );
		else
		    to[i].wc = SFINAL((SISOLATED(line[i].wc)));
		break;
	    }

	    tempShape = STYPE(line[i-1].wc);
	    if((tempShape == SR) || (tempShape == SD) || (tempShape == SC))
		to[i].wc = SINITIAL((SISOLATED(line[i].wc)));
	    else
		to[i].wc = SISOLATED(line[i].wc);
	    break;


	}
    }
    return 1;
}

/*
 * The Main Bidi Function, and the only function that should
 * be used by the outside world.
 *
 * line: a buffer of size count containing text to apply
 * the Bidirectional algorithm to.
 */

int do_bidi(bidi_char *line, int count)
{
    unsigned char* types;
    unsigned char* levels;
    unsigned char paragraphLevel;
    unsigned char currentEmbedding;
    unsigned char currentOverride;
    unsigned char tempType;
    int i, j, imax, yes, bover;

    /* Check the presence of R or AL types as optimization */
    yes = 0;
    for(i=0; i<count; i++)
    {
	if(getType(line[i].wc) == R || getType(line[i].wc) == AL)
	{
	    yes = 1;
	    break;
	}
    }
    if(yes == 0)
	return L;

    /* Initialize types, levels */
    types = malloc(sizeof(unsigned char) * count);
    levels = malloc(sizeof(unsigned char) * count);

    /* Rule (P1)  NOT IMPLEMENTED
     * P1. Split the text into separate paragraphs. A paragraph separator is
     * kept with the previous paragraph. Within each paragraph, apply all the
     * other rules of this algorithm.
     */

    /* Rule (P2), (P3)
     * P2. In each paragraph, find the first character of type L, AL, or R.
     * P3. If a character is found in P2 and it is of type AL or R, then set
     * the paragraph embedding level to one; otherwise, set it to zero.
     */
    paragraphLevel = 0;
    for( i=0; i<count ; i++)
    {
	if(getType(line[i].wc) == R || getType(line[i].wc) == AL)
	{
	    paragraphLevel = 1;
	    break;
	}
	else if(getType(line[i].wc) == L)
	    break;
    }

    /* Rule (X1)
     * X1. Begin by setting the current embedding level to the paragraph
     * embedding level. Set the directional override status to neutral.
     */
    currentEmbedding = paragraphLevel;
    currentOverride = ON;

    /* Rule (X2), (X3), (X4), (X5), (X6), (X7), (X8)
     * X2. With each RLE, compute the least greater odd embedding level.
     * X3. With each LRE, compute the least greater even embedding level.
     * X4. With each RLO, compute the least greater odd embedding level.
     * X5. With each LRO, compute the least greater even embedding level.
     * X6. For all types besides RLE, LRE, RLO, LRO, and PDF:
     *		a. Set the level of the current character to the current
     *		    embedding level.
     *		b.  Whenever the directional override status is not neutral,
     *               reset the current character type to the directional
     *               override status.
     * X7. With each PDF, determine the matching embedding or override code.
     * If there was a valid matching code, restore (pop) the last
     * remembered (pushed) embedding level and directional override.
     * X8. All explicit directional embeddings and overrides are completely
     * terminated at the end of each paragraph. Paragraph separators are not
     * included in the embedding. (Useless here) NOT IMPLEMENTED
     */
    bover = 0;
    for( i=0; i<count; i++)
    {
	tempType = getType(line[i].wc);
	switch(tempType)
	{
	  case RLE:
	    currentEmbedding = levels[i] = leastGreaterOdd(currentEmbedding);
	    levels[i] = setOverrideBits(levels[i], currentOverride);
	    currentOverride = ON;
	    break;

	  case LRE:
	    currentEmbedding = levels[i] = leastGreaterEven(currentEmbedding);
	    levels[i] = setOverrideBits(levels[i], currentOverride);
	    currentOverride = ON;
	    break;

	  case RLO:
	    currentEmbedding = levels[i] = leastGreaterOdd(currentEmbedding);
	    tempType = currentOverride = R;
	    bover = 1;
	    break;

	  case LRO:
	    currentEmbedding = levels[i] = leastGreaterEven(currentEmbedding);
	    tempType = currentOverride = L;
	    bover = 1;
	    break;

	  case PDF:
	    currentEmbedding = getPreviousLevel(levels, i);
	    currentOverride = currentEmbedding & OMASK;
	    currentEmbedding = currentEmbedding & ~OMASK;
	    levels[i] = currentEmbedding;
	    break;

	    /* Whitespace is treated as neutral for now */
	  case WS:
	  case S:
	    levels[i] = currentEmbedding;
	    tempType = ON;
	    if(currentOverride != ON)
		tempType = currentOverride;
	    break;

	  default:
	    levels[i] = currentEmbedding;
	    if(currentOverride != ON)
		tempType = currentOverride;
	    break;

	}
	types[i] = tempType;
    }
    /* this clears out all overrides, so we can use levels safely... */
    /* checks bover first */
    if(bover)
	for( i=0; i<count; i++)
	    levels[i] = levels[i] & LMASK;

    /* Rule (X9)
     * X9. Remove all RLE, LRE, RLO, LRO, PDF, and BN codes.
     * Here, they're converted to BN.
     */
    for(i=0; i<count; i++)
    {
	switch(types[i])
	{
	  case RLE:
	  case LRE:
	  case RLO:
	  case LRO:
	  case PDF:
	    types[i] = BN;
	    break;
	}
    }

    /* Rule (W1)
     * W1. Examine each non-spacing mark (NSM) in the level run, and change
     * the type of the NSM to the type of the previous character. If the NSM
     * is at the start of the level run, it will get the type of sor.
     */
    if(types[0] == NSM)
	types[0] = paragraphLevel;

    for(i=1; i<count; i++)
    {
	if(types[i] == NSM)
	    types[i] = types[i-1];
	/* Is this a safe assumption?
	 * I assumed the previous, IS a character.
	 */
    }

    /* Rule (W2)
     * W2. Search backwards from each instance of a European number until the
     * first strong type (R, L, AL, or sor) is found.  If an AL is found,
     * change the type of the European number to Arabic number.
     */
    for(i=0; i<count; i++)
    {
	if(types[i] == EN)
	{
	    j=i;
	    while(j >= 0)
	    {
		if(types[j] == AL)
		{
		    types[i] = AN;
		    break;
		}else if(types[j] == R || types[j] == L)
		    {
			break;
		    }
		j--;
	    }
	}
    }

    /* Rule (W3)
     * W3. Change all ALs to R.
     *
     * Optimization: on Rule Xn, we might set a flag on AL type
     * to prevent this loop in L R lines only...
     */
    for(i=0; i<count; i++)
    {
	if(types[i] == AL)
	    types[i] = R;
    }

    /* Rule (W4)
     * W4. A single European separator between two European numbers changes
     * to a European number. A single common separator between two numbers
     * of the same type changes to that type.
     */
    for( i=0; i<(count-1); i++)
    {
	if(types[i] == ES)
	{
	    if(types[i-1] == EN && types[i+1] == EN)
		types[i] = EN;
	}else if(types[i] == CS)
	    {
		if(types[i-1] == EN && types[i+1] == EN)
		    types[i] = EN;
		else if(types[i-1] == AN && types[i+1] == AN)
		    types[i] = AN;
	    }
    }

    /* Rule (W5)
     * W5. A sequence of European terminators adjacent to European numbers
     * changes to all European numbers.
     *
     * Optimization: lots here... else ifs need rearrangement
     */
    for(i=0; i<count; i++)
    {
	if(types[i] == ET)
	{
	    if(types[i-1] == EN)
	    {
		types[i] = EN;
		continue;
	    }else if(types[i+1] == EN)
		{
		    types[i] = EN;
		    continue;
		}else if(types[i+1] == ET)
		    {
			j=i;
			while(j <count && types[j] == ET)
			{
			    j++;
			}
			if(types[j] == EN)
			    types[i] = EN;
		    }
	}
    }

    /* Rule (W6)
     * W6. Otherwise, separators and terminators change to Other Neutral:
     */
    for(i=0; i<count; i++)
    {
	switch(types[i])
	{
	  case ES:
	  case ET:
	  case CS:
	    types[i] = ON;
	    break;
	}
    }

    /* Rule (W7)
     * W7. Search backwards from each instance of a European number until
     * the first strong type (R, L, or sor) is found. If an L is found,
     * then change the type of the European number to L.
     */
    for(i=0; i<count; i++)
    {
	if(types[i] == EN)
	{
	    j=i;
	    while(j >= 0)
	    {
		if(types[j] == L)
		{
		    types[i] = L;
		    break;
		}
		else if(types[j] == R || types[j] == AL)
		{
		    break;
		}
		j--;
	    }
	}
    }

    /* Rule (N1)
     * N1. A sequence of neutrals takes the direction of the surrounding
     * strong text if the text on both sides has the same direction. European
     * and Arabic numbers are treated as though they were R.
     */
    if(types[0] == ON)
    {
	if((types[1] == R) || (types[1] == EN) || (types[1] == AN))
	    types[0] = R;
	else if(types[1] == L)
	    types[0] = L;
    }
    for(i=1; i<(count-1); i++)
    {
	if(types[i] == ON)
	{
	    if(types[i-1] == L)
	    {
		j=i;
		while(j<(count-1) && types[j] == ON)
		{
		    j++;
		}
		if(types[j] == L)
		{
		    while(i<j)
		    {
			types[i] = L;
			i++;
		    }
		}

	    }else if((types[i-1] == R)  ||
		     (types[i-1] == EN) ||
		     (types[i-1] == AN))
		{
		    j=i;
		    while(j<(count-1) && types[j] == ON)
		    {
			j++;
		    }
		    if((types[j] == R)  ||
		       (types[j] == EN) ||
		       (types[j] == AN))
		    {
			while(i<j)
			{
			    types[i] = R;
			    i++;
			}
		    }
		}
	}
    }
    if(types[count-1] == ON)
    {
	if(types[count-2] == R || types[count-2] == EN || types[count-2] == AN)
	    types[count-1] = R;
	else if(types[count-2] == L)
	    types[count-1] = L;
    }

    /* Rule (N2)
     * N2. Any remaining neutrals take the embedding direction.
     */
    for(i=0; i<count; i++)
    {
	if(types[i] == ON)
	{
	    if((levels[i] % 2) == 0)
		types[i] = L;
	    else
		types[i] = R;
	}
    }

    /* Rule (I1)
     * I1. For all characters with an even (left-to-right) embedding
     * direction, those of type R go up one level and those of type AN or
     * EN go up two levels.
     */
    for(i=0; i<count; i++)
    {
	if((levels[i] % 2) == 0)
	{
	    if(types[i] == R)
		levels[i] += 1;
	    else if(types[i] == AN || types[i] == EN)
		levels[i] += 2;
	}
    }

    /* Rule (I2)
     * I2. For all characters with an odd (right-to-left) embedding direction,
     * those of type L, EN or AN go up one level.
     */
    for(i=0; i<count; i++)
    {
	if((levels[i] % 2) == 1)
	{
	    if(types[i] == L || types[i] == EN || types[i] == AN)
		levels[i] += 1;
	}
    }

    /* Rule (L1)
     * L1. On each line, reset the embedding level of the following characters
     * to the paragraph embedding level:
     *		(1)segment separators, (2)paragraph separators,
     *           (3)any sequence of whitespace characters preceding
     *           a segment separator or paragraph separator,
     *           (4)and any sequence of white space characters
     *           at the end of the line.
     * The types of characters used here are the original types, not those
     * modified by the previous phase.
     */
    j=count-1;
    while(j>0 && (getType(line[j].wc) == WS))
    {
	j--;
    }
    if(j < (count-1))
    {
	for(j++; j<count; j++)
	    levels[j] = paragraphLevel;
    }
    for(i=0; i<count; i++)
    {
	tempType = getType(line[i].wc);
	if(tempType == WS)
	{
	    j=i;
	    while(j<count && (getType(line[j].wc) == WS))
	    {
		j++;
	    }
	    if(getType(line[j].wc) == B || getType(line[j].wc) == S)
	    {
		for(j--; j>=i ; j--)
		{
		    levels[j] = paragraphLevel;
		}
	    }
	}else if(tempType == B || tempType == S)
		levels[i] = paragraphLevel;
    }

    /* Rule (L4) NOT IMPLEMENTED
     * L4. A character that possesses the mirrored property as specified by
     * Section 4.7, Mirrored, must be depicted by a mirrored glyph if the
     * resolved directionality of that character is R.
     */
    /* Note: this is implemented before L2 for efficiency */
    for(i=0; i<count; i++)
	if((levels[i] % 2) == 1)
	    doMirror(&line[i].wc);

    /* Rule (L2)
     * L2. From the highest level found in the text to the lowest odd level on
     * each line, including intermediate levels not actually present in the
     * text, reverse any contiguous sequence of characters that are at that
     * level or higher
     */
    /* we flip the character string and leave the level array */
    imax = 0;
    i=0;
    tempType = levels[0];
    while(i < count)
    {
	if(levels[i] > tempType)
	{
	    tempType = levels[i];
	    imax=i;
	}
	i++;
    }
    /* maximum level in tempType, its index in imax. */
    while(tempType > 0)		/* loop from highest level to the least odd, */
    {				/* which i assume is 1 */
	flipThisRun(line, levels, tempType, count);
	tempType--;
    }

    /* Rule (L3) NOT IMPLEMENTED
     * L3. Combining marks applied to a right-to-left base character will at
     * this point precede their base character. If the rendering engine
     * expects them to follow the base characters in the final display
     * process, then the ordering of the marks and the base character must
     * be reversed.
     */
    free(types);
    free(levels);
    return R;
}


/*
 * Bad, Horrible funtion
 * takes a pointer to a character that is checked for
 * having a mirror glyph.
 */
void doMirror(wchar_t* ch)
{
    if((*ch & 0xFF00) == 0)
    {
	switch(*ch)
	{
	  case 0x0028:
	    *ch = 0x0029;
	    break;
	  case 0x0029:
	    *ch = 0x0028;
	    break;
	  case 0x003C:
	    *ch = 0x003E;
	    break;
	  case 0x003E:
	    *ch = 0x003C;
	    break;
	  case 0x005B:
	    *ch = 0x005D;
	    break;
	  case 0x005D:
	    *ch = 0x005B;
	    break;
	  case 0x007B:
	    *ch = 0x007D;
	    break;
	  case 0x007D:
	    *ch = 0x007B;
	    break;
	  case 0x00AB:
	    *ch = 0x00BB;
	    break;
	  case 0x00BB:
	    *ch = 0x00AB;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0x2000)
    {
	switch(*ch)
	{
	  case 0x2039:
	    *ch = 0x203A;
	    break;
	  case 0x203A:
	    *ch = 0x2039;
	    break;
	  case 0x2045:
	    *ch = 0x2046;
	    break;
	  case 0x2046:
	    *ch = 0x2045;
	    break;
	  case 0x207D:
	    *ch = 0x207E;
	    break;
	  case 0x207E:
	    *ch = 0x207D;
	    break;
	  case 0x208D:
	    *ch = 0x208E;
	    break;
	  case 0x208E:
	    *ch = 0x208D;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0x2200)
    {
	switch(*ch)
	{
	  case 0x2208:
	    *ch = 0x220B;
	    break;
	  case 0x2209:
	    *ch = 0x220C;
	    break;
	  case 0x220A:
	    *ch = 0x220D;
	    break;
	  case 0x220B:
	    *ch = 0x2208;
	    break;
	  case 0x220C:
	    *ch = 0x2209;
	    break;
	  case 0x220D:
	    *ch = 0x220A;
	    break;
	  case 0x2215:
	    *ch = 0x29F5;
	    break;
	  case 0x223C:
	    *ch = 0x223D;
	    break;
	  case 0x223D:
	    *ch = 0x223C;
	    break;
	  case 0x2243:
	    *ch = 0x22CD;
	    break;
	  case 0x2252:
	    *ch = 0x2253;
	    break;
	  case 0x2253:
	    *ch = 0x2252;
	    break;
	  case 0x2254:
	    *ch = 0x2255;
	    break;
	  case 0x2255:
	    *ch = 0x2254;
	    break;
	  case 0x2264:
	    *ch = 0x2265;
	    break;
	  case 0x2265:
	    *ch = 0x2264;
	    break;
	  case 0x2266:
	    *ch = 0x2267;
	    break;
	  case 0x2267:
	    *ch = 0x2266;
	    break;
	  case 0x2268:
	    *ch = 0x2269;
	    break;
	  case 0x2269:
	    *ch = 0x2268;
	    break;
	  case 0x226A:
	    *ch = 0x226B;
	    break;
	  case 0x226B:
	    *ch = 0x226A;
	    break;
	  case 0x226E:
	    *ch = 0x226F;
	    break;
	  case 0x226F:
	    *ch = 0x226E;
	    break;
	  case 0x2270:
	    *ch = 0x2271;
	    break;
	  case 0x2271:
	    *ch = 0x2270;
	    break;
	  case 0x2272:
	    *ch = 0x2273;
	    break;
	  case 0x2273:
	    *ch = 0x2272;
	    break;
	  case 0x2274:
	    *ch = 0x2275;
	    break;
	  case 0x2275:
	    *ch = 0x2274;
	    break;
	  case 0x2276:
	    *ch = 0x2277;
	    break;
	  case 0x2277:
	    *ch = 0x2276;
	    break;
	  case 0x2278:
	    *ch = 0x2279;
	    break;
	  case 0x2279:
	    *ch = 0x2278;
	    break;
	  case 0x227A:
	    *ch = 0x227B;
	    break;
	  case 0x227B:
	    *ch = 0x227A;
	    break;
	  case 0x227C:
	    *ch = 0x227D;
	    break;
	  case 0x227D:
	    *ch = 0x227C;
	    break;
	  case 0x227E:
	    *ch = 0x227F;
	    break;
	  case 0x227F:
	    *ch = 0x227E;
	    break;
	  case 0x2280:
	    *ch = 0x2281;
	    break;
	  case 0x2281:
	    *ch = 0x2280;
	    break;
	  case 0x2282:
	    *ch = 0x2283;
	    break;
	  case 0x2283:
	    *ch = 0x2282;
	    break;
	  case 0x2284:
	    *ch = 0x2285;
	    break;
	  case 0x2285:
	    *ch = 0x2284;
	    break;
	  case 0x2286:
	    *ch = 0x2287;
	    break;
	  case 0x2287:
	    *ch = 0x2286;
	    break;
	  case 0x2288:
	    *ch = 0x2289;
	    break;
	  case 0x2289:
	    *ch = 0x2288;
	    break;
	  case 0x228A:
	    *ch = 0x228B;
	    break;
	  case 0x228B:
	    *ch = 0x228A;
	    break;
	  case 0x228F:
	    *ch = 0x2290;
	    break;
	  case 0x2290:
	    *ch = 0x228F;
	    break;
	  case 0x2291:
	    *ch = 0x2292;
	    break;
	  case 0x2292:
	    *ch = 0x2291;
	    break;
	  case 0x2298:
	    *ch = 0x29B8;
	    break;
	  case 0x22A2:
	    *ch = 0x22A3;
	    break;
	  case 0x22A3:
	    *ch = 0x22A2;
	    break;
	  case 0x22A6:
	    *ch = 0x2ADE;
	    break;
	  case 0x22A8:
	    *ch = 0x2AE4;
	    break;
	  case 0x22A9:
	    *ch = 0x2AE3;
	    break;
	  case 0x22AB:
	    *ch = 0x2AE5;
	    break;
	  case 0x22B0:
	    *ch = 0x22B1;
	    break;
	  case 0x22B1:
	    *ch = 0x22B0;
	    break;
	  case 0x22B2:
	    *ch = 0x22B3;
	    break;
	  case 0x22B3:
	    *ch = 0x22B2;
	    break;
	  case 0x22B4:
	    *ch = 0x22B5;
	    break;
	  case 0x22B5:
	    *ch = 0x22B4;
	    break;
	  case 0x22B6:
	    *ch = 0x22B7;
	    break;
	  case 0x22B7:
	    *ch = 0x22B6;
	    break;
	  case 0x22C9:
	    *ch = 0x22CA;
	    break;
	  case 0x22CA:
	    *ch = 0x22C9;
	    break;
	  case 0x22CB:
	    *ch = 0x22CC;
	    break;
	  case 0x22CC:
	    *ch = 0x22CB;
	    break;
	  case 0x22CD:
	    *ch = 0x2243;
	    break;
	  case 0x22D0:
	    *ch = 0x22D1;
	    break;
	  case 0x22D1:
	    *ch = 0x22D0;
	    break;
	  case 0x22D6:
	    *ch = 0x22D7;
	    break;
	  case 0x22D7:
	    *ch = 0x22D6;
	    break;
	  case 0x22D8:
	    *ch = 0x22D9;
	    break;
	  case 0x22D9:
	    *ch = 0x22D8;
	    break;
	  case 0x22DA:
	    *ch = 0x22DB;
	    break;
	  case 0x22DB:
	    *ch = 0x22DA;
	    break;
	  case 0x22DC:
	    *ch = 0x22DD;
	    break;
	  case 0x22DD:
	    *ch = 0x22DC;
	    break;
	  case 0x22DE:
	    *ch = 0x22DF;
	    break;
	  case 0x22DF:
	    *ch = 0x22DE;
	    break;
	  case 0x22E0:
	    *ch = 0x22E1;
	    break;
	  case 0x22E1:
	    *ch = 0x22E0;
	    break;
	  case 0x22E2:
	    *ch = 0x22E3;
	    break;
	  case 0x22E3:
	    *ch = 0x22E2;
	    break;
	  case 0x22E4:
	    *ch = 0x22E5;
	    break;
	  case 0x22E5:
	    *ch = 0x22E4;
	    break;
	  case 0x22E6:
	    *ch = 0x22E7;
	    break;
	  case 0x22E7:
	    *ch = 0x22E6;
	    break;
	  case 0x22E8:
	    *ch = 0x22E9;
	    break;
	  case 0x22E9:
	    *ch = 0x22E8;
	    break;
	  case 0x22EA:
	    *ch = 0x22EB;
	    break;
	  case 0x22EB:
	    *ch = 0x22EA;
	    break;
	  case 0x22EC:
	    *ch = 0x22ED;
	    break;
	  case 0x22ED:
	    *ch = 0x22EC;
	    break;
	  case 0x22F0:
	    *ch = 0x22F1;
	    break;
	  case 0x22F1:
	    *ch = 0x22F0;
	    break;
	  case 0x22F2:
	    *ch = 0x22FA;
	    break;
	  case 0x22F3:
	    *ch = 0x22FB;
	    break;
	  case 0x22F4:
	    *ch = 0x22FC;
	    break;
	  case 0x22F6:
	    *ch = 0x22FD;
	    break;
	  case 0x22F7:
	    *ch = 0x22FE;
	    break;
	  case 0x22FA:
	    *ch = 0x22F2;
	    break;
	  case 0x22FB:
	    *ch = 0x22F3;
	    break;
	  case 0x22FC:
	    *ch = 0x22F4;
	    break;
	  case 0x22FD:
	    *ch = 0x22F6;
	    break;
	  case 0x22FE:
	    *ch = 0x22F7;
	    break;
	}
    }else if((*ch & 0xFF00) == 0x2300)
	{
	    switch(*ch)
	    {
	      case 0x2308:
		*ch = 0x2309;
		break;
	      case 0x2309:
		*ch = 0x2308;
		break;
	      case 0x230A:
		*ch = 0x230B;
		break;
	      case 0x230B:
		*ch = 0x230A;
		break;
	      case 0x2329:
		*ch = 0x232A;
		break;
	      case 0x232A:
		*ch = 0x2329;
		break;
	    }
	}
    else if((*ch & 0xFF00) == 0x2700)
    {
	switch(*ch)
	{
	  case 0x2768:
	    *ch = 0x2769;
	    break;
	  case 0x2769:
	    *ch = 0x2768;
	    break;
	  case 0x276A:
	    *ch = 0x276B;
	    break;
	  case 0x276B:
	    *ch = 0x276A;
	    break;
	  case 0x276C:
	    *ch = 0x276D;
	    break;
	  case 0x276D:
	    *ch = 0x276C;
	    break;
	  case 0x276E:
	    *ch = 0x276F;
	    break;
	  case 0x276F:
	    *ch = 0x276E;
	    break;
	  case 0x2770:
	    *ch = 0x2771;
	    break;
	  case 0x2771:
	    *ch = 0x2770;
	    break;
	  case 0x2772:
	    *ch = 0x2773;
	    break;
	  case 0x2773:
	    *ch = 0x2772;
	    break;
	  case 0x2774:
	    *ch = 0x2775;
	    break;
	  case 0x2775:
	    *ch = 0x2774;
	    break;
	  case 0x27D5:
	    *ch = 0x27D6;
	    break;
	  case 0x27D6:
	    *ch = 0x27D5;
	    break;
	  case 0x27DD:
	    *ch = 0x27DE;
	    break;
	  case 0x27DE:
	    *ch = 0x27DD;
	    break;
	  case 0x27E2:
	    *ch = 0x27E3;
	    break;
	  case 0x27E3:
	    *ch = 0x27E2;
	    break;
	  case 0x27E4:
	    *ch = 0x27E5;
	    break;
	  case 0x27E5:
	    *ch = 0x27E4;
	    break;
	  case 0x27E6:
	    *ch = 0x27E7;
	    break;
	  case 0x27E7:
	    *ch = 0x27E6;
	    break;
	  case 0x27E8:
	    *ch = 0x27E9;
	    break;
	  case 0x27E9:
	    *ch = 0x27E8;
	    break;
	  case 0x27EA:
	    *ch = 0x27EB;
	    break;
	  case 0x27EB:
	    *ch = 0x27EA;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0x2900)
    {
	switch(*ch)
	{
	  case 0x2983:
	    *ch = 0x2984;
	    break;
	  case 0x2984:
	    *ch = 0x2983;
	    break;
	  case 0x2985:
	    *ch = 0x2986;
	    break;
	  case 0x2986:
	    *ch = 0x2985;
	    break;
	  case 0x2987:
	    *ch = 0x2988;
	    break;
	  case 0x2988:
	    *ch = 0x2987;
	    break;
	  case 0x2989:
	    *ch = 0x298A;
	    break;
	  case 0x298A:
	    *ch = 0x2989;
	    break;
	  case 0x298B:
	    *ch = 0x298C;
	    break;
	  case 0x298C:
	    *ch = 0x298B;
	    break;
	  case 0x298D:
	    *ch = 0x2990;
	    break;
	  case 0x298E:
	    *ch = 0x298F;
	    break;
	  case 0x298F:
	    *ch = 0x298E;
	    break;
	  case 0x2990:
	    *ch = 0x298D;
	    break;
	  case 0x2991:
	    *ch = 0x2992;
	    break;
	  case 0x2992:
	    *ch = 0x2991;
	    break;
	  case 0x2993:
	    *ch = 0x2994;
	    break;
	  case 0x2994:
	    *ch = 0x2993;
	    break;
	  case 0x2995:
	    *ch = 0x2996;
	    break;
	  case 0x2996:
	    *ch = 0x2995;
	    break;
	  case 0x2997:
	    *ch = 0x2998;
	    break;
	  case 0x2998:
	    *ch = 0x2997;
	    break;
	  case 0x29B8:
	    *ch = 0x2298;
	    break;
	  case 0x29C0:
	    *ch = 0x29C1;
	    break;
	  case 0x29C1:
	    *ch = 0x29C0;
	    break;
	  case 0x29C4:
	    *ch = 0x29C5;
	    break;
	  case 0x29C5:
	    *ch = 0x29C4;
	    break;
	  case 0x29CF:
	    *ch = 0x29D0;
	    break;
	  case 0x29D0:
	    *ch = 0x29CF;
	    break;
	  case 0x29D1:
	    *ch = 0x29D2;
	    break;
	  case 0x29D2:
	    *ch = 0x29D1;
	    break;
	  case 0x29D4:
	    *ch = 0x29D5;
	    break;
	  case 0x29D5:
	    *ch = 0x29D4;
	    break;
	  case 0x29D8:
	    *ch = 0x29D9;
	    break;
	  case 0x29D9:
	    *ch = 0x29D8;
	    break;
	  case 0x29DA:
	    *ch = 0x29DB;
	    break;
	  case 0x29DB:
	    *ch = 0x29DA;
	    break;
	  case 0x29F5:
	    *ch = 0x2215;
	    break;
	  case 0x29F8:
	    *ch = 0x29F9;
	    break;
	  case 0x29F9:
	    *ch = 0x29F8;
	    break;
	  case 0x29FC:
	    *ch = 0x29FD;
	    break;
	  case 0x29FD:
	    *ch = 0x29FC;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0x2A00)
    {
	switch(*ch)
	{
	  case 0x2A2B:
	    *ch = 0x2A2C;
	    break;
	  case 0x2A2C:
	    *ch = 0x2A2B;
	    break;
	  case 0x2A2D:
	    *ch = 0x2A2C;
	    break;
	  case 0x2A2E:
	    *ch = 0x2A2D;
	    break;
	  case 0x2A34:
	    *ch = 0x2A35;
	    break;
	  case 0x2A35:
	    *ch = 0x2A34;
	    break;
	  case 0x2A3C:
	    *ch = 0x2A3D;
	    break;
	  case 0x2A3D:
	    *ch = 0x2A3C;
	    break;
	  case 0x2A64:
	    *ch = 0x2A65;
	    break;
	  case 0x2A65:
	    *ch = 0x2A64;
	    break;
	  case 0x2A79:
	    *ch = 0x2A7A;
	    break;
	  case 0x2A7A:
	    *ch = 0x2A79;
	    break;
	  case 0x2A7D:
	    *ch = 0x2A7E;
	    break;
	  case 0x2A7E:
	    *ch = 0x2A7D;
	    break;
	  case 0x2A7F:
	    *ch = 0x2A80;
	    break;
	  case 0x2A80:
	    *ch = 0x2A7F;
	    break;
	  case 0x2A81:
	    *ch = 0x2A82;
	    break;
	  case 0x2A82:
	    *ch = 0x2A81;
	    break;
	  case 0x2A83:
	    *ch = 0x2A84;
	    break;
	  case 0x2A84:
	    *ch = 0x2A83;
	    break;
	  case 0x2A8B:
	    *ch = 0x2A8C;
	    break;
	  case 0x2A8C:
	    *ch = 0x2A8B;
	    break;
	  case 0x2A91:
	    *ch = 0x2A92;
	    break;
	  case 0x2A92:
	    *ch = 0x2A91;
	    break;
	  case 0x2A93:
	    *ch = 0x2A94;
	    break;
	  case 0x2A94:
	    *ch = 0x2A93;
	    break;
	  case 0x2A95:
	    *ch = 0x2A96;
	    break;
	  case 0x2A96:
	    *ch = 0x2A95;
	    break;
	  case 0x2A97:
	    *ch = 0x2A98;
	    break;
	  case 0x2A98:
	    *ch = 0x2A97;
	    break;
	  case 0x2A99:
	    *ch = 0x2A9A;
	    break;
	  case 0x2A9A:
	    *ch = 0x2A99;
	    break;
	  case 0x2A9B:
	    *ch = 0x2A9C;
	    break;
	  case 0x2A9C:
	    *ch = 0x2A9B;
	    break;
	  case 0x2AA1:
	    *ch = 0x2AA2;
	    break;
	  case 0x2AA2:
	    *ch = 0x2AA1;
	    break;
	  case 0x2AA6:
	    *ch = 0x2AA7;
	    break;
	  case 0x2AA7:
	    *ch = 0x2AA6;
	    break;
	  case 0x2AA8:
	    *ch = 0x2AA9;
	    break;
	  case 0x2AA9:
	    *ch = 0x2AA8;
	    break;
	  case 0x2AAA:
	    *ch = 0x2AAB;
	    break;
	  case 0x2AAB:
	    *ch = 0x2AAA;
	    break;
	  case 0x2AAC:
	    *ch = 0x2AAD;
	    break;
	  case 0x2AAD:
	    *ch = 0x2AAC;
	    break;
	  case 0x2AAF:
	    *ch = 0x2AB0;
	    break;
	  case 0x2AB0:
	    *ch = 0x2AAF;
	    break;
	  case 0x2AB3:
	    *ch = 0x2AB4;
	    break;
	  case 0x2AB4:
	    *ch = 0x2AB3;
	    break;
	  case 0x2ABB:
	    *ch = 0x2ABC;
	    break;
	  case 0x2ABC:
	    *ch = 0x2ABB;
	    break;
	  case 0x2ABD:
	    *ch = 0x2ABE;
	    break;
	  case 0x2ABE:
	    *ch = 0x2ABD;
	    break;
	  case 0x2ABF:
	    *ch = 0x2AC0;
	    break;
	  case 0x2AC0:
	    *ch = 0x2ABF;
	    break;
	  case 0x2AC1:
	    *ch = 0x2AC2;
	    break;
	  case 0x2AC2:
	    *ch = 0x2AC1;
	    break;
	  case 0x2AC3:
	    *ch = 0x2AC4;
	    break;
	  case 0x2AC4:
	    *ch = 0x2AC3;
	    break;
	  case 0x2AC5:
	    *ch = 0x2AC6;
	    break;
	  case 0x2AC6:
	    *ch = 0x2AC5;
	    break;
	  case 0x2ACD:
	    *ch = 0x2ACE;
	    break;
	  case 0x2ACE:
	    *ch = 0x2ACD;
	    break;
	  case 0x2ACF:
	    *ch = 0x2AD0;
	    break;
	  case 0x2AD0:
	    *ch = 0x2ACF;
	    break;
	  case 0x2AD1:
	    *ch = 0x2AD2;
	    break;
	  case 0x2AD2:
	    *ch = 0x2AD1;
	    break;
	  case 0x2AD3:
	    *ch = 0x2AD4;
	    break;
	  case 0x2AD4:
	    *ch = 0x2AD3;
	    break;
	  case 0x2AD5:
	    *ch = 0x2AD6;
	    break;
	  case 0x2AD6:
	    *ch = 0x2AD5;
	    break;
	  case 0x2ADE:
	    *ch = 0x22A6;
	    break;
	  case 0x2AE3:
	    *ch = 0x22A9;
	    break;
	  case 0x2AE4:
	    *ch = 0x22A8;
	    break;
	  case 0x2AE5:
	    *ch = 0x22AB;
	    break;
	  case 0x2AEC:
	    *ch = 0x2AED;
	    break;
	  case 0x2AED:
	    *ch = 0x2AEC;
	    break;
	  case 0x2AF7:
	    *ch = 0x2AF8;
	    break;
	  case 0x2AF8:
	    *ch = 0x2AF7;
	    break;
	  case 0x2AF9:
	    *ch = 0x2AFA;
	    break;
	  case 0x2AFA:
	    *ch = 0x2AF9;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0x3000)
    {
	switch(*ch)
	{
	  case 0x3008:
	    *ch = 0x3009;
	    break;
	  case 0x3009:
	    *ch = 0x3008;
	    break;
	  case 0x300A:
	    *ch = 0x300B;
	    break;
	  case 0x300B:
	    *ch = 0x300A;
	    break;
	  case 0x300C:
	    *ch = 0x300D;
	    break;
	  case 0x300D:
	    *ch = 0x300C;
	    break;
	  case 0x300E:
	    *ch = 0x300F;
	    break;
	  case 0x300F:
	    *ch = 0x300E;
	    break;
	  case 0x3010:
	    *ch = 0x3011;
	    break;
	  case 0x3011:
	    *ch = 0x3010;
	    break;
	  case 0x3014:
	    *ch = 0x3015;
	    break;
	  case 0x3015:
	    *ch = 0x3014;
	    break;
	  case 0x3016:
	    *ch = 0x3017;
	    break;
	  case 0x3017:
	    *ch = 0x3016;
	    break;
	  case 0x3018:
	    *ch = 0x3019;
	    break;
	  case 0x3019:
	    *ch = 0x3018;
	    break;
	  case 0x301A:
	    *ch = 0x301B;
	    break;
	  case 0x301B:
	    *ch = 0x301A;
	    break;
	}
    }
    else if((*ch & 0xFF00) == 0xFF00)
    {
	switch(*ch)
	{
	  case 0xFF08:
	    *ch = 0xFF09;
	    break;
	  case 0xFF09:
	    *ch = 0xFF08;
	    break;
	  case 0xFF1C:
	    *ch = 0xFF1E;
	    break;
	  case 0xFF1E:
	    *ch = 0xFF1C;
	    break;
	  case 0xFF3B:
	    *ch = 0xFF3D;
	    break;
	  case 0xFF3D:
	    *ch = 0xFF3B;
	    break;
	  case 0xFF5B:
	    *ch = 0xFF5D;
	    break;
	  case 0xFF5D:
	    *ch = 0xFF5B;
	    break;
	  case 0xFF5F:
	    *ch = 0xFF60;
	    break;
	  case 0xFF60:
	    *ch = 0xFF5F;
	    break;
	  case 0xFF62:
	    *ch = 0xFF63;
	    break;
	  case 0xFF63:
	    *ch = 0xFF62;
	    break;
	}
    }
}
