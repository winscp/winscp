/*
 * tree234.h: header defining functions in tree234.c.
 * 
 * This file is copyright 1999-2001 Simon Tatham.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL SIMON TATHAM BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef TREE234_H
#define TREE234_H

/*
 * This typedef is opaque outside tree234.c itself.
 */
typedef struct tree234_Tag tree234;

typedef int (*cmpfn234) (void *, void *);

/*
 * Create a 2-3-4 tree. If `cmp' is NULL, the tree is unsorted, and
 * lookups by key will fail: you can only look things up by numeric
 * index, and you have to use addpos234() and delpos234().
 */
tree234 *newtree234(cmpfn234 cmp);

/*
 * Free a 2-3-4 tree (not including freeing the elements).
 */
void freetree234(tree234 * t);

/*
 * Add an element e to a sorted 2-3-4 tree t. Returns e on success,
 * or if an existing element compares equal, returns that.
 */
void *add234(tree234 * t, void *e);

/*
 * Add an element e to an unsorted 2-3-4 tree t. Returns e on
 * success, NULL on failure. (Failure should only occur if the
 * index is out of range or the tree is sorted.)
 * 
 * Index range can be from 0 to the tree's current element count,
 * inclusive.
 */
void *addpos234(tree234 * t, void *e, int index);

/*
 * Look up the element at a given numeric index in a 2-3-4 tree.
 * Returns NULL if the index is out of range.
 * 
 * One obvious use for this function is in iterating over the whole
 * of a tree (sorted or unsorted):
 * 
 *   for (i = 0; (p = index234(tree, i)) != NULL; i++) consume(p);
 * 
 * or
 * 
 *   int maxcount = count234(tree);
 *   for (i = 0; i < maxcount; i++) {
 *       p = index234(tree, i);
 *       assert(p != NULL);
 *       consume(p);
 *   }
 */
void *index234(tree234 * t, int index);

/*
 * Find an element e in a sorted 2-3-4 tree t. Returns NULL if not
 * found. e is always passed as the first argument to cmp, so cmp
 * can be an asymmetric function if desired. cmp can also be passed
 * as NULL, in which case the compare function from the tree proper
 * will be used.
 * 
 * Three of these functions are special cases of findrelpos234. The
 * non-`pos' variants lack the `index' parameter: if the parameter
 * is present and non-NULL, it must point to an integer variable
 * which will be filled with the numeric index of the returned
 * element.
 * 
 * The non-`rel' variants lack the `relation' parameter. This
 * parameter allows you to specify what relation the element you
 * provide has to the element you're looking for. This parameter
 * can be:
 * 
 *   REL234_EQ     - find only an element that compares equal to e
 *   REL234_LT     - find the greatest element that compares < e
 *   REL234_LE     - find the greatest element that compares <= e
 *   REL234_GT     - find the smallest element that compares > e
 *   REL234_GE     - find the smallest element that compares >= e
 * 
 * Non-`rel' variants assume REL234_EQ.
 * 
 * If `rel' is REL234_GT or REL234_LT, the `e' parameter may be
 * NULL. In this case, REL234_GT will return the smallest element
 * in the tree, and REL234_LT will return the greatest. This gives
 * an alternative means of iterating over a sorted tree, instead of
 * using index234:
 * 
 *   // to loop forwards
 *   for (p = NULL; (p = findrel234(tree, p, NULL, REL234_GT)) != NULL ;)
 *       consume(p);
 * 
 *   // to loop backwards
 *   for (p = NULL; (p = findrel234(tree, p, NULL, REL234_LT)) != NULL ;)
 *       consume(p);
 */
enum {
    REL234_EQ, REL234_LT, REL234_LE, REL234_GT, REL234_GE
};
void *find234(tree234 * t, void *e, cmpfn234 cmp);
void *findrel234(tree234 * t, void *e, cmpfn234 cmp, int relation);
void *findpos234(tree234 * t, void *e, cmpfn234 cmp, int *index);
void *findrelpos234(tree234 * t, void *e, cmpfn234 cmp, int relation,
		    int *index);

/*
 * Delete an element e in a 2-3-4 tree. Does not free the element,
 * merely removes all links to it from the tree nodes.
 * 
 * delpos234 deletes the element at a particular tree index: it
 * works on both sorted and unsorted trees.
 * 
 * del234 deletes the element passed to it, so it only works on
 * sorted trees. (It's equivalent to using findpos234 to determine
 * the index of an element, and then passing that index to
 * delpos234.)
 * 
 * Both functions return a pointer to the element they delete, for
 * the user to free or pass on elsewhere or whatever. If the index
 * is out of range (delpos234) or the element is already not in the
 * tree (del234) then they return NULL.
 */
void *del234(tree234 * t, void *e);
void *delpos234(tree234 * t, int index);

/*
 * Return the total element count of a tree234.
 */
int count234(tree234 * t);

#endif				/* TREE234_H */
