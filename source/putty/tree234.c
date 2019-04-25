/*
 * tree234.c: reasonably generic counted 2-3-4 tree routines.
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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "defs.h"
#include "tree234.h"

#ifdef TEST
#define LOG(x) (printf x)
#define snew(type) ((type *)malloc(sizeof(type)))
#define snewn(n, type) ((type *)malloc((n) * sizeof(type)))
#define sresize(ptr, n, type)                                         \
    ((type *)realloc(sizeof((type *)0 == (ptr)) ? (ptr) : (ptr),      \
                     (n) * sizeof(type)))
#define sfree(ptr) free(ptr)
#else
#include "puttymem.h"
#define LOG(x)
#endif

typedef struct node234_Tag node234;

struct tree234_Tag {
    node234 *root;
    cmpfn234 cmp;
};

struct node234_Tag {
    node234 *parent;
    node234 *kids[4];
    int counts[4];
    void *elems[3];
};

/*
 * Create a 2-3-4 tree.
 */
tree234 *newtree234(cmpfn234 cmp)
{
    tree234 *ret = snew(tree234);
    LOG(("created tree %p\n", ret));
    ret->root = NULL;
    ret->cmp = cmp;
    return ret;
}

/*
 * Free a 2-3-4 tree (not including freeing the elements).
 */
static void freenode234(node234 * n)
{
    if (!n)
	return;
    freenode234(n->kids[0]);
    freenode234(n->kids[1]);
    freenode234(n->kids[2]);
    freenode234(n->kids[3]);
    sfree(n);
}

void freetree234(tree234 * t)
{
    freenode234(t->root);
    sfree(t);
}

/*
 * Internal function to count a node.
 */
static int countnode234(node234 * n)
{
    int count = 0;
    int i;
    if (!n)
	return 0;
    for (i = 0; i < 4; i++)
	count += n->counts[i];
    for (i = 0; i < 3; i++)
	if (n->elems[i])
	    count++;
    return count;
}

/*
 * Internal function to return the number of elements in a node.
 */
static int elements234(node234 *n)
{
    int i;
    for (i = 0; i < 3; i++)
        if (!n->elems[i])
            break;
    return i;
}

/*
 * Count the elements in a tree.
 */
int count234(tree234 * t)
{
    if (t->root)
	return countnode234(t->root);
    else
	return 0;
}

/*
 * Add an element e to a 2-3-4 tree t. Returns e on success, or if
 * an existing element compares equal, returns that.
 */
static void *add234_internal(tree234 * t, void *e, int index)
{
    node234 *n, **np, *left, *right;
    void *orig_e = e;
    int c, lcount, rcount;

    LOG(("adding node %p to tree %p\n", e, t));
    if (t->root == NULL) {
	t->root = snew(node234);
	t->root->elems[1] = t->root->elems[2] = NULL;
	t->root->kids[0] = t->root->kids[1] = NULL;
	t->root->kids[2] = t->root->kids[3] = NULL;
	t->root->counts[0] = t->root->counts[1] = 0;
	t->root->counts[2] = t->root->counts[3] = 0;
	t->root->parent = NULL;
	t->root->elems[0] = e;
	LOG(("  created root %p\n", t->root));
	return orig_e;
    }

    n = NULL; /* placate gcc; will always be set below since t->root != NULL */
    np = &t->root;
    while (*np) {
	int childnum;
	n = *np;
	LOG(("  node %p: %p/%d [%p] %p/%d [%p] %p/%d [%p] %p/%d\n",
	     n,
	     n->kids[0], n->counts[0], n->elems[0],
	     n->kids[1], n->counts[1], n->elems[1],
	     n->kids[2], n->counts[2], n->elems[2],
	     n->kids[3], n->counts[3]));
	if (index >= 0) {
	    if (!n->kids[0]) {
		/*
		 * Leaf node. We want to insert at kid position
		 * equal to the index:
		 * 
		 *   0 A 1 B 2 C 3
		 */
		childnum = index;
	    } else {
		/*
		 * Internal node. We always descend through it (add
		 * always starts at the bottom, never in the
		 * middle).
		 */
		do {		       /* this is a do ... while (0) to allow `break' */
		    if (index <= n->counts[0]) {
			childnum = 0;
			break;
		    }
		    index -= n->counts[0] + 1;
		    if (index <= n->counts[1]) {
			childnum = 1;
			break;
		    }
		    index -= n->counts[1] + 1;
		    if (index <= n->counts[2]) {
			childnum = 2;
			break;
		    }
		    index -= n->counts[2] + 1;
		    if (index <= n->counts[3]) {
			childnum = 3;
			break;
		    }
		    return NULL;       /* error: index out of range */
		} while (0);
	    }
	} else {
	    if ((c = t->cmp(e, n->elems[0])) < 0)
		childnum = 0;
	    else if (c == 0)
		return n->elems[0];    /* already exists */
	    else if (n->elems[1] == NULL
		     || (c = t->cmp(e, n->elems[1])) < 0) childnum = 1;
	    else if (c == 0)
		return n->elems[1];    /* already exists */
	    else if (n->elems[2] == NULL
		     || (c = t->cmp(e, n->elems[2])) < 0) childnum = 2;
	    else if (c == 0)
		return n->elems[2];    /* already exists */
	    else
		childnum = 3;
	}
	np = &n->kids[childnum];
	LOG(("  moving to child %d (%p)\n", childnum, *np));
    }

    /*
     * We need to insert the new element in n at position np.
     */
    left = NULL;
    lcount = 0;
    right = NULL;
    rcount = 0;
    while (n) {
	LOG(("  at %p: %p/%d [%p] %p/%d [%p] %p/%d [%p] %p/%d\n",
	     n,
	     n->kids[0], n->counts[0], n->elems[0],
	     n->kids[1], n->counts[1], n->elems[1],
	     n->kids[2], n->counts[2], n->elems[2],
	     n->kids[3], n->counts[3]));
	LOG(("  need to insert %p/%d [%p] %p/%d at position %d\n",
	     left, lcount, e, right, rcount, (int)(np - n->kids)));
	if (n->elems[1] == NULL) {
	    /*
	     * Insert in a 2-node; simple.
	     */
	    if (np == &n->kids[0]) {
		LOG(("  inserting on left of 2-node\n"));
		n->kids[2] = n->kids[1];
		n->counts[2] = n->counts[1];
		n->elems[1] = n->elems[0];
		n->kids[1] = right;
		n->counts[1] = rcount;
		n->elems[0] = e;
		n->kids[0] = left;
		n->counts[0] = lcount;
	    } else {		       /* np == &n->kids[1] */
		LOG(("  inserting on right of 2-node\n"));
		n->kids[2] = right;
		n->counts[2] = rcount;
		n->elems[1] = e;
		n->kids[1] = left;
		n->counts[1] = lcount;
	    }
	    if (n->kids[0])
		n->kids[0]->parent = n;
	    if (n->kids[1])
		n->kids[1]->parent = n;
	    if (n->kids[2])
		n->kids[2]->parent = n;
	    LOG(("  done\n"));
	    break;
	} else if (n->elems[2] == NULL) {
	    /*
	     * Insert in a 3-node; simple.
	     */
	    if (np == &n->kids[0]) {
		LOG(("  inserting on left of 3-node\n"));
		n->kids[3] = n->kids[2];
		n->counts[3] = n->counts[2];
		n->elems[2] = n->elems[1];
		n->kids[2] = n->kids[1];
		n->counts[2] = n->counts[1];
		n->elems[1] = n->elems[0];
		n->kids[1] = right;
		n->counts[1] = rcount;
		n->elems[0] = e;
		n->kids[0] = left;
		n->counts[0] = lcount;
	    } else if (np == &n->kids[1]) {
		LOG(("  inserting in middle of 3-node\n"));
		n->kids[3] = n->kids[2];
		n->counts[3] = n->counts[2];
		n->elems[2] = n->elems[1];
		n->kids[2] = right;
		n->counts[2] = rcount;
		n->elems[1] = e;
		n->kids[1] = left;
		n->counts[1] = lcount;
	    } else {		       /* np == &n->kids[2] */
		LOG(("  inserting on right of 3-node\n"));
		n->kids[3] = right;
		n->counts[3] = rcount;
		n->elems[2] = e;
		n->kids[2] = left;
		n->counts[2] = lcount;
	    }
	    if (n->kids[0])
		n->kids[0]->parent = n;
	    if (n->kids[1])
		n->kids[1]->parent = n;
	    if (n->kids[2])
		n->kids[2]->parent = n;
	    if (n->kids[3])
		n->kids[3]->parent = n;
	    LOG(("  done\n"));
	    break;
	} else {
	    node234 *m = snew(node234);
	    m->parent = n->parent;
	    LOG(("  splitting a 4-node; created new node %p\n", m));
	    /*
	     * Insert in a 4-node; split into a 2-node and a
	     * 3-node, and move focus up a level.
	     * 
	     * I don't think it matters which way round we put the
	     * 2 and the 3. For simplicity, we'll put the 3 first
	     * always.
	     */
	    if (np == &n->kids[0]) {
		m->kids[0] = left;
		m->counts[0] = lcount;
		m->elems[0] = e;
		m->kids[1] = right;
		m->counts[1] = rcount;
		m->elems[1] = n->elems[0];
		m->kids[2] = n->kids[1];
		m->counts[2] = n->counts[1];
		e = n->elems[1];
		n->kids[0] = n->kids[2];
		n->counts[0] = n->counts[2];
		n->elems[0] = n->elems[2];
		n->kids[1] = n->kids[3];
		n->counts[1] = n->counts[3];
	    } else if (np == &n->kids[1]) {
		m->kids[0] = n->kids[0];
		m->counts[0] = n->counts[0];
		m->elems[0] = n->elems[0];
		m->kids[1] = left;
		m->counts[1] = lcount;
		m->elems[1] = e;
		m->kids[2] = right;
		m->counts[2] = rcount;
		e = n->elems[1];
		n->kids[0] = n->kids[2];
		n->counts[0] = n->counts[2];
		n->elems[0] = n->elems[2];
		n->kids[1] = n->kids[3];
		n->counts[1] = n->counts[3];
	    } else if (np == &n->kids[2]) {
		m->kids[0] = n->kids[0];
		m->counts[0] = n->counts[0];
		m->elems[0] = n->elems[0];
		m->kids[1] = n->kids[1];
		m->counts[1] = n->counts[1];
		m->elems[1] = n->elems[1];
		m->kids[2] = left;
		m->counts[2] = lcount;
		/* e = e; */
		n->kids[0] = right;
		n->counts[0] = rcount;
		n->elems[0] = n->elems[2];
		n->kids[1] = n->kids[3];
		n->counts[1] = n->counts[3];
	    } else {		       /* np == &n->kids[3] */
		m->kids[0] = n->kids[0];
		m->counts[0] = n->counts[0];
		m->elems[0] = n->elems[0];
		m->kids[1] = n->kids[1];
		m->counts[1] = n->counts[1];
		m->elems[1] = n->elems[1];
		m->kids[2] = n->kids[2];
		m->counts[2] = n->counts[2];
		n->kids[0] = left;
		n->counts[0] = lcount;
		n->elems[0] = e;
		n->kids[1] = right;
		n->counts[1] = rcount;
		e = n->elems[2];
	    }
	    m->kids[3] = n->kids[3] = n->kids[2] = NULL;
	    m->counts[3] = n->counts[3] = n->counts[2] = 0;
	    m->elems[2] = n->elems[2] = n->elems[1] = NULL;
	    if (m->kids[0])
		m->kids[0]->parent = m;
	    if (m->kids[1])
		m->kids[1]->parent = m;
	    if (m->kids[2])
		m->kids[2]->parent = m;
	    if (n->kids[0])
		n->kids[0]->parent = n;
	    if (n->kids[1])
		n->kids[1]->parent = n;
	    LOG(("  left (%p): %p/%d [%p] %p/%d [%p] %p/%d\n", m,
		 m->kids[0], m->counts[0], m->elems[0],
		 m->kids[1], m->counts[1], m->elems[1],
		 m->kids[2], m->counts[2]));
	    LOG(("  right (%p): %p/%d [%p] %p/%d\n", n,
		 n->kids[0], n->counts[0], n->elems[0],
		 n->kids[1], n->counts[1]));
	    left = m;
	    lcount = countnode234(left);
	    right = n;
	    rcount = countnode234(right);
	}
	if (n->parent)
	    np = (n->parent->kids[0] == n ? &n->parent->kids[0] :
		  n->parent->kids[1] == n ? &n->parent->kids[1] :
		  n->parent->kids[2] == n ? &n->parent->kids[2] :
		  &n->parent->kids[3]);
	n = n->parent;
    }

    /*
     * If we've come out of here by `break', n will still be
     * non-NULL and all we need to do is go back up the tree
     * updating counts. If we've come here because n is NULL, we
     * need to create a new root for the tree because the old one
     * has just split into two. */
    if (n) {
	while (n->parent) {
	    int count = countnode234(n);
	    int childnum;
	    childnum = (n->parent->kids[0] == n ? 0 :
			n->parent->kids[1] == n ? 1 :
			n->parent->kids[2] == n ? 2 : 3);
	    n->parent->counts[childnum] = count;
	    n = n->parent;
	}
    } else {
	LOG(("  root is overloaded, split into two\n"));
	t->root = snew(node234);
	t->root->kids[0] = left;
	t->root->counts[0] = lcount;
	t->root->elems[0] = e;
	t->root->kids[1] = right;
	t->root->counts[1] = rcount;
	t->root->elems[1] = NULL;
	t->root->kids[2] = NULL;
	t->root->counts[2] = 0;
	t->root->elems[2] = NULL;
	t->root->kids[3] = NULL;
	t->root->counts[3] = 0;
	t->root->parent = NULL;
	if (t->root->kids[0])
	    t->root->kids[0]->parent = t->root;
	if (t->root->kids[1])
	    t->root->kids[1]->parent = t->root;
	LOG(("  new root is %p/%d [%p] %p/%d\n",
	     t->root->kids[0], t->root->counts[0],
	     t->root->elems[0], t->root->kids[1], t->root->counts[1]));
    }

    return orig_e;
}

void *add234(tree234 * t, void *e)
{
    if (!t->cmp)		       /* tree is unsorted */
	return NULL;

    return add234_internal(t, e, -1);
}
void *addpos234(tree234 * t, void *e, int index)
{
    if (index < 0 ||		       /* index out of range */
	t->cmp)			       /* tree is sorted */
	return NULL;		       /* return failure */

    return add234_internal(t, e, index);	/* this checks the upper bound */
}

/*
 * Look up the element at a given numeric index in a 2-3-4 tree.
 * Returns NULL if the index is out of range.
 */
void *index234(tree234 * t, int index)
{
    node234 *n;

    if (!t->root)
	return NULL;		       /* tree is empty */

    if (index < 0 || index >= countnode234(t->root))
	return NULL;		       /* out of range */

    n = t->root;

    while (n) {
	if (index < n->counts[0])
	    n = n->kids[0];
	else if (index -= n->counts[0] + 1, index < 0)
	    return n->elems[0];
	else if (index < n->counts[1])
	    n = n->kids[1];
	else if (index -= n->counts[1] + 1, index < 0)
	    return n->elems[1];
	else if (index < n->counts[2])
	    n = n->kids[2];
	else if (index -= n->counts[2] + 1, index < 0)
	    return n->elems[2];
	else
	    n = n->kids[3];
    }

    /* We shouldn't ever get here. I wonder how we did. */
    return NULL;
}

/*
 * Find an element e in a sorted 2-3-4 tree t. Returns NULL if not
 * found. e is always passed as the first argument to cmp, so cmp
 * can be an asymmetric function if desired. cmp can also be passed
 * as NULL, in which case the compare function from the tree proper
 * will be used.
 */
void *findrelpos234(tree234 * t, void *e, cmpfn234 cmp,
		    int relation, int *index)
{
    search234_state ss;
    int reldir = (relation == REL234_LT || relation == REL234_LE ? -1 :
                  relation == REL234_GT || relation == REL234_GE ? +1 : 0);
    bool equal_permitted = (relation != REL234_LT && relation != REL234_GT);
    void *toret;

    /* Only LT / GT relations are permitted with a null query element. */
    assert(!(equal_permitted && !e));

    if (cmp == NULL)
	cmp = t->cmp;

    search234_start(&ss, t);
    while (ss.element) {
        int cmpret;

        if (e) {
            cmpret = cmp(e, ss.element);
        } else {
            cmpret = -reldir;          /* invent a fixed compare result */
        }

        if (cmpret == 0) {
            /*
             * We've found an element that compares exactly equal to
             * the query element.
             */
            if (equal_permitted) {
                /* If our search relation permits equality, we've
                 * finished already. */
                if (index)
                    *index = ss.index;
                return ss.element;
            } else {
                /* Otherwise, pretend this element was slightly too
                 * big/small, according to the direction of search. */
                cmpret = reldir;
            }
        }

        search234_step(&ss, cmpret);
    }

    /*
     * No element compares equal to the one we were after, but
     * ss.index indicates the index that element would have if it were
     * inserted.
     *
     * So if our search relation is EQ, we must simply return failure.
     */
    if (relation == REL234_EQ)
        return NULL;

    /*
     * Otherwise, we must do an index lookup for the previous index
     * (if we're going left - LE or LT) or this index (if we're going
     * right - GE or GT).
     */
    if (relation == REL234_LT || relation == REL234_LE) {
        ss.index--;
    }

    /*
     * We know the index of the element we want; just call index234
     * to do the rest. This will return NULL if the index is out of
     * bounds, which is exactly what we want.
     */
    toret = index234(t, ss.index);
    if (toret && index)
        *index = ss.index;
    return toret;
}
void *find234(tree234 * t, void *e, cmpfn234 cmp)
{
    return findrelpos234(t, e, cmp, REL234_EQ, NULL);
}
void *findrel234(tree234 * t, void *e, cmpfn234 cmp, int relation)
{
    return findrelpos234(t, e, cmp, relation, NULL);
}
void *findpos234(tree234 * t, void *e, cmpfn234 cmp, int *index)
{
    return findrelpos234(t, e, cmp, REL234_EQ, index);
}

void search234_start(search234_state *state, tree234 *t)
{
    state->_node = t->root;
    state->_base = 0; /* index of first element in this node's subtree */
    state->_last = -1; /* indicate that this node is not previously visted */
    search234_step(state, 0);
}
void search234_step(search234_state *state, int direction)
{
    node234 *node = state->_node;
    int i;

    if (!node) {
        state->element = NULL;
        state->index = 0;
        return;
    }

    if (state->_last != -1) {
        /*
         * We're already pointing at some element of a node, so we
         * should restrict to the elements left or right of it,
         * depending on the requested search direction.
         */
        assert(direction);
        assert(node);

        if (direction > 0)
            state->_lo = state->_last + 1;
        else
            state->_hi = state->_last - 1;

        if (state->_lo > state->_hi) {
            /*
             * We've run out of elements in this node, i.e. we've
             * narrowed to nothing but a child pointer. Descend to
             * that child, and update _base to the leftmost index of
             * its subtree.
             */
            for (i = 0; i < state->_lo; i++)
                state->_base += 1 + node->counts[i];
            state->_node = node = node->kids[state->_lo];
            state->_last = -1;
        }
    }

    if (state->_last == -1) {
        /*
         * We've just entered a new node - either because of the above
         * code, or because we were called from search234_start - and
         * anything in that node is a viable answer.
         */
        state->_lo = 0;
        state->_hi = node ? elements234(node)-1 : 0;
    }

    /*
     * Now we've got something we can return.
     */
    if (!node) {
        state->element = NULL;
        state->index = state->_base;
    } else {
        state->_last = (state->_lo + state->_hi) / 2;
        state->element = node->elems[state->_last];
        state->index = state->_base + state->_last;
        for (i = 0; i <= state->_last; i++)
            state->index += node->counts[i];
    }
}

/*
 * Delete an element e in a 2-3-4 tree. Does not free the element,
 * merely removes all links to it from the tree nodes.
 */
static void *delpos234_internal(tree234 * t, int index)
{
    node234 *n;
    void *retval;
    int ei = -1;

    retval = 0;

    n = t->root;
    LOG(("deleting item %d from tree %p\n", index, t));
    while (1) {
	while (n) {
	    int ki;
	    node234 *sub;

	    LOG(
		("  node %p: %p/%d [%p] %p/%d [%p] %p/%d [%p] %p/%d index=%d\n",
		 n, n->kids[0], n->counts[0], n->elems[0], n->kids[1],
		 n->counts[1], n->elems[1], n->kids[2], n->counts[2],
		 n->elems[2], n->kids[3], n->counts[3], index));
	    if (index < n->counts[0]) {
		ki = 0;
	    } else if (index -= n->counts[0] + 1, index < 0) {
		ei = 0;
		break;
	    } else if (index < n->counts[1]) {
		ki = 1;
	    } else if (index -= n->counts[1] + 1, index < 0) {
		ei = 1;
		break;
	    } else if (index < n->counts[2]) {
		ki = 2;
	    } else if (index -= n->counts[2] + 1, index < 0) {
		ei = 2;
		break;
	    } else {
		ki = 3;
	    }
	    /*
	     * Recurse down to subtree ki. If it has only one element,
	     * we have to do some transformation to start with.
	     */
	    LOG(("  moving to subtree %d\n", ki));
	    sub = n->kids[ki];
	    if (!sub->elems[1]) {
		LOG(("  subtree has only one element!\n"));
		if (ki > 0 && n->kids[ki - 1]->elems[1]) {
		    /*
		     * Case 3a, left-handed variant. Child ki has
		     * only one element, but child ki-1 has two or
		     * more. So we need to move a subtree from ki-1
		     * to ki.
		     * 
		     *                . C .                     . B .
		     *               /     \     ->            /     \
		     * [more] a A b B c   d D e      [more] a A b   c C d D e
		     */
		    node234 *sib = n->kids[ki - 1];
		    int lastelem = (sib->elems[2] ? 2 :
				    sib->elems[1] ? 1 : 0);
		    sub->kids[2] = sub->kids[1];
		    sub->counts[2] = sub->counts[1];
		    sub->elems[1] = sub->elems[0];
		    sub->kids[1] = sub->kids[0];
		    sub->counts[1] = sub->counts[0];
		    sub->elems[0] = n->elems[ki - 1];
		    sub->kids[0] = sib->kids[lastelem + 1];
		    sub->counts[0] = sib->counts[lastelem + 1];
		    if (sub->kids[0])
			sub->kids[0]->parent = sub;
		    n->elems[ki - 1] = sib->elems[lastelem];
		    sib->kids[lastelem + 1] = NULL;
		    sib->counts[lastelem + 1] = 0;
		    sib->elems[lastelem] = NULL;
		    n->counts[ki] = countnode234(sub);
		    LOG(("  case 3a left\n"));
		    LOG(
			("  index and left subtree count before adjustment: %d, %d\n",
			 index, n->counts[ki - 1]));
		    index += n->counts[ki - 1];
		    n->counts[ki - 1] = countnode234(sib);
		    index -= n->counts[ki - 1];
		    LOG(
			("  index and left subtree count after adjustment: %d, %d\n",
			 index, n->counts[ki - 1]));
		} else if (ki < 3 && n->kids[ki + 1]
			   && n->kids[ki + 1]->elems[1]) {
		    /*
		     * Case 3a, right-handed variant. ki has only
		     * one element but ki+1 has two or more. Move a
		     * subtree from ki+1 to ki.
		     * 
		     *      . B .                             . C .
		     *     /     \                ->         /     \
		     *  a A b   c C d D e [more]      a A b B c   d D e [more]
		     */
		    node234 *sib = n->kids[ki + 1];
		    int j;
		    sub->elems[1] = n->elems[ki];
		    sub->kids[2] = sib->kids[0];
		    sub->counts[2] = sib->counts[0];
		    if (sub->kids[2])
			sub->kids[2]->parent = sub;
		    n->elems[ki] = sib->elems[0];
		    sib->kids[0] = sib->kids[1];
		    sib->counts[0] = sib->counts[1];
		    for (j = 0; j < 2 && sib->elems[j + 1]; j++) {
			sib->kids[j + 1] = sib->kids[j + 2];
			sib->counts[j + 1] = sib->counts[j + 2];
			sib->elems[j] = sib->elems[j + 1];
		    }
		    sib->kids[j + 1] = NULL;
		    sib->counts[j + 1] = 0;
		    sib->elems[j] = NULL;
		    n->counts[ki] = countnode234(sub);
		    n->counts[ki + 1] = countnode234(sib);
		    LOG(("  case 3a right\n"));
		} else {
		    /*
		     * Case 3b. ki has only one element, and has no
		     * neighbour with more than one. So pick a
		     * neighbour and merge it with ki, taking an
		     * element down from n to go in the middle.
		     *
		     *      . B .                .
		     *     /     \     ->        |
		     *  a A b   c C d      a A b B c C d
		     * 
		     * (Since at all points we have avoided
		     * descending to a node with only one element,
		     * we can be sure that n is not reduced to
		     * nothingness by this move, _unless_ it was
		     * the very first node, ie the root of the
		     * tree. In that case we remove the now-empty
		     * root and replace it with its single large
		     * child as shown.)
		     */
		    node234 *sib;
		    int j;

		    if (ki > 0) {
			ki--;
			index += n->counts[ki] + 1;
		    }
		    sib = n->kids[ki];
		    sub = n->kids[ki + 1];

		    sub->kids[3] = sub->kids[1];
		    sub->counts[3] = sub->counts[1];
		    sub->elems[2] = sub->elems[0];
		    sub->kids[2] = sub->kids[0];
		    sub->counts[2] = sub->counts[0];
		    sub->elems[1] = n->elems[ki];
		    sub->kids[1] = sib->kids[1];
		    sub->counts[1] = sib->counts[1];
		    if (sub->kids[1])
			sub->kids[1]->parent = sub;
		    sub->elems[0] = sib->elems[0];
		    sub->kids[0] = sib->kids[0];
		    sub->counts[0] = sib->counts[0];
		    if (sub->kids[0])
			sub->kids[0]->parent = sub;

		    n->counts[ki + 1] = countnode234(sub);

		    sfree(sib);

		    /*
		     * That's built the big node in sub. Now we
		     * need to remove the reference to sib in n.
		     */
		    for (j = ki; j < 3 && n->kids[j + 1]; j++) {
			n->kids[j] = n->kids[j + 1];
			n->counts[j] = n->counts[j + 1];
			n->elems[j] = j < 2 ? n->elems[j + 1] : NULL;
		    }
		    n->kids[j] = NULL;
		    n->counts[j] = 0;
		    if (j < 3)
			n->elems[j] = NULL;
		    LOG(("  case 3b ki=%d\n", ki));

		    if (!n->elems[0]) {
			/*
			 * The root is empty and needs to be
			 * removed.
			 */
			LOG(("  shifting root!\n"));
			t->root = sub;
			sub->parent = NULL;
			sfree(n);
		    }
		}
	    }
	    n = sub;
	}
	if (!retval)
	    retval = n->elems[ei];

	if (ei == -1)
	    return NULL;	       /* although this shouldn't happen */

	/*
	 * Treat special case: this is the one remaining item in
	 * the tree. n is the tree root (no parent), has one
	 * element (no elems[1]), and has no kids (no kids[0]).
	 */
	if (!n->parent && !n->elems[1] && !n->kids[0]) {
	    LOG(("  removed last element in tree\n"));
	    sfree(n);
	    t->root = NULL;
	    return retval;
	}

	/*
	 * Now we have the element we want, as n->elems[ei], and we
	 * have also arranged for that element not to be the only
	 * one in its node. So...
	 */

	if (!n->kids[0] && n->elems[1]) {
	    /*
	     * Case 1. n is a leaf node with more than one element,
	     * so it's _really easy_. Just delete the thing and
	     * we're done.
	     */
	    int i;
	    LOG(("  case 1\n"));
	    for (i = ei; i < 2 && n->elems[i + 1]; i++)
		n->elems[i] = n->elems[i + 1];
	    n->elems[i] = NULL;
	    /*
	     * Having done that to the leaf node, we now go back up
	     * the tree fixing the counts.
	     */
	    while (n->parent) {
		int childnum;
		childnum = (n->parent->kids[0] == n ? 0 :
			    n->parent->kids[1] == n ? 1 :
			    n->parent->kids[2] == n ? 2 : 3);
		n->parent->counts[childnum]--;
		n = n->parent;
	    }
	    return retval;	       /* finished! */
	} else if (n->kids[ei]->elems[1]) {
	    /*
	     * Case 2a. n is an internal node, and the root of the
	     * subtree to the left of e has more than one element.
	     * So find the predecessor p to e (ie the largest node
	     * in that subtree), place it where e currently is, and
	     * then start the deletion process over again on the
	     * subtree with p as target.
	     */
	    node234 *m = n->kids[ei];
	    void *target;
	    LOG(("  case 2a\n"));
	    while (m->kids[0]) {
		m = (m->kids[3] ? m->kids[3] :
		     m->kids[2] ? m->kids[2] :
		     m->kids[1] ? m->kids[1] : m->kids[0]);
	    }
	    target = (m->elems[2] ? m->elems[2] :
		      m->elems[1] ? m->elems[1] : m->elems[0]);
	    n->elems[ei] = target;
	    index = n->counts[ei] - 1;
	    n = n->kids[ei];
	} else if (n->kids[ei + 1]->elems[1]) {
	    /*
	     * Case 2b, symmetric to 2a but s/left/right/ and
	     * s/predecessor/successor/. (And s/largest/smallest/).
	     */
	    node234 *m = n->kids[ei + 1];
	    void *target;
	    LOG(("  case 2b\n"));
	    while (m->kids[0]) {
		m = m->kids[0];
	    }
	    target = m->elems[0];
	    n->elems[ei] = target;
	    n = n->kids[ei + 1];
	    index = 0;
	} else {
	    /*
	     * Case 2c. n is an internal node, and the subtrees to
	     * the left and right of e both have only one element.
	     * So combine the two subnodes into a single big node
	     * with their own elements on the left and right and e
	     * in the middle, then restart the deletion process on
	     * that subtree, with e still as target.
	     */
	    node234 *a = n->kids[ei], *b = n->kids[ei + 1];
	    int j;

	    LOG(("  case 2c\n"));
	    a->elems[1] = n->elems[ei];
	    a->kids[2] = b->kids[0];
	    a->counts[2] = b->counts[0];
	    if (a->kids[2])
		a->kids[2]->parent = a;
	    a->elems[2] = b->elems[0];
	    a->kids[3] = b->kids[1];
	    a->counts[3] = b->counts[1];
	    if (a->kids[3])
		a->kids[3]->parent = a;
	    sfree(b);
	    n->counts[ei] = countnode234(a);
	    /*
	     * That's built the big node in a, and destroyed b. Now
	     * remove the reference to b (and e) in n.
	     */
	    for (j = ei; j < 2 && n->elems[j + 1]; j++) {
		n->elems[j] = n->elems[j + 1];
		n->kids[j + 1] = n->kids[j + 2];
		n->counts[j + 1] = n->counts[j + 2];
	    }
	    n->elems[j] = NULL;
	    n->kids[j + 1] = NULL;
	    n->counts[j + 1] = 0;
	    /*
	     * It's possible, in this case, that we've just removed
	     * the only element in the root of the tree. If so,
	     * shift the root.
	     */
	    if (n->elems[0] == NULL) {
		LOG(("  shifting root!\n"));
		t->root = a;
		a->parent = NULL;
		sfree(n);
	    }
	    /*
	     * Now go round the deletion process again, with n
	     * pointing at the new big node and e still the same.
	     */
	    n = a;
	    index = a->counts[0] + a->counts[1] + 1;
	}
    }
}
void *delpos234(tree234 * t, int index)
{
    if (index < 0 || index >= countnode234(t->root))
	return NULL;
    return delpos234_internal(t, index);
}
void *del234(tree234 * t, void *e)
{
    int index;
    if (!findrelpos234(t, e, NULL, REL234_EQ, &index))
	return NULL;		       /* it wasn't in there anyway */
    return delpos234_internal(t, index);	/* it's there; delete it. */
}

#ifdef TEST

/*
 * Test code for the 2-3-4 tree. This code maintains an alternative
 * representation of the data in the tree, in an array (using the
 * obvious and slow insert and delete functions). After each tree
 * operation, the verify() function is called, which ensures all
 * the tree properties are preserved:
 *  - node->child->parent always equals node
 *  - tree->root->parent always equals NULL
 *  - number of kids == 0 or number of elements + 1;
 *  - tree has the same depth everywhere
 *  - every node has at least one element
 *  - subtree element counts are accurate
 *  - any NULL kid pointer is accompanied by a zero count
 *  - in a sorted tree: ordering property between elements of a
 *    node and elements of its children is preserved
 * and also ensures the list represented by the tree is the same
 * list it should be. (This last check also doubly verifies the
 * ordering properties, because the `same list it should be' is by
 * definition correctly ordered. It also ensures all nodes are
 * distinct, because the enum functions would get caught in a loop
 * if not.)
 */

#include <stdarg.h>
#include <string.h>

int n_errors = 0;

/*
 * Error reporting function.
 */
void error(char *fmt, ...)
{
    va_list ap;
    printf("ERROR: ");
    va_start(ap, fmt);
    vfprintf(stdout, fmt, ap);
    va_end(ap);
    printf("\n");
    n_errors++;
}

/* The array representation of the data. */
void **array;
int arraylen, arraysize;
cmpfn234 cmp;

/* The tree representation of the same data. */
tree234 *tree;

typedef struct {
    int treedepth;
    int elemcount;
} chkctx;

int chknode(chkctx * ctx, int level, node234 * node,
	    void *lowbound, void *highbound)
{
    int nkids, nelems;
    int i;
    int count;

    /* Count the non-NULL kids. */
    for (nkids = 0; nkids < 4 && node->kids[nkids]; nkids++);
    /* Ensure no kids beyond the first NULL are non-NULL. */
    for (i = nkids; i < 4; i++)
	if (node->kids[i]) {
	    error("node %p: nkids=%d but kids[%d] non-NULL",
		  node, nkids, i);
	} else if (node->counts[i]) {
	    error("node %p: kids[%d] NULL but count[%d]=%d nonzero",
		  node, i, i, node->counts[i]);
	}

    /* Count the non-NULL elements. */
    for (nelems = 0; nelems < 3 && node->elems[nelems]; nelems++);
    /* Ensure no elements beyond the first NULL are non-NULL. */
    for (i = nelems; i < 3; i++)
	if (node->elems[i]) {
	    error("node %p: nelems=%d but elems[%d] non-NULL",
		  node, nelems, i);
	}

    if (nkids == 0) {
	/*
	 * If nkids==0, this is a leaf node; verify that the tree
	 * depth is the same everywhere.
	 */
	if (ctx->treedepth < 0)
	    ctx->treedepth = level;    /* we didn't know the depth yet */
	else if (ctx->treedepth != level)
	    error("node %p: leaf at depth %d, previously seen depth %d",
		  node, level, ctx->treedepth);
    } else {
	/*
	 * If nkids != 0, then it should be nelems+1, unless nelems
	 * is 0 in which case nkids should also be 0 (and so we
	 * shouldn't be in this condition at all).
	 */
	int shouldkids = (nelems ? nelems + 1 : 0);
	if (nkids != shouldkids) {
	    error("node %p: %d elems should mean %d kids but has %d",
		  node, nelems, shouldkids, nkids);
	}
    }

    /*
     * nelems should be at least 1.
     */
    if (nelems == 0) {
	error("node %p: no elems", node, nkids);
    }

    /*
     * Add nelems to the running element count of the whole tree.
     */
    ctx->elemcount += nelems;

    /*
     * Check ordering property: all elements should be strictly >
     * lowbound, strictly < highbound, and strictly < each other in
     * sequence. (lowbound and highbound are NULL at edges of tree
     * - both NULL at root node - and NULL is considered to be <
     * everything and > everything. IYSWIM.)
     */
    if (cmp) {
	for (i = -1; i < nelems; i++) {
	    void *lower = (i == -1 ? lowbound : node->elems[i]);
	    void *higher =
		(i + 1 == nelems ? highbound : node->elems[i + 1]);
	    if (lower && higher && cmp(lower, higher) >= 0) {
		error("node %p: kid comparison [%d=%s,%d=%s] failed",
		      node, i, lower, i + 1, higher);
	    }
	}
    }

    /*
     * Check parent pointers: all non-NULL kids should have a
     * parent pointer coming back to this node.
     */
    for (i = 0; i < nkids; i++)
	if (node->kids[i]->parent != node) {
	    error("node %p kid %d: parent ptr is %p not %p",
		  node, i, node->kids[i]->parent, node);
	}


    /*
     * Now (finally!) recurse into subtrees.
     */
    count = nelems;

    for (i = 0; i < nkids; i++) {
	void *lower = (i == 0 ? lowbound : node->elems[i - 1]);
	void *higher = (i >= nelems ? highbound : node->elems[i]);
	int subcount =
	    chknode(ctx, level + 1, node->kids[i], lower, higher);
	if (node->counts[i] != subcount) {
	    error("node %p kid %d: count says %d, subtree really has %d",
		  node, i, node->counts[i], subcount);
	}
	count += subcount;
    }

    return count;
}

void verify(void)
{
    chkctx ctx;
    int i;
    void *p;

    ctx.treedepth = -1;		       /* depth unknown yet */
    ctx.elemcount = 0;		       /* no elements seen yet */
    /*
     * Verify validity of tree properties.
     */
    if (tree->root) {
	if (tree->root->parent != NULL)
	    error("root->parent is %p should be null", tree->root->parent);
	chknode(&ctx, 0, tree->root, NULL, NULL);
    }
    printf("tree depth: %d\n", ctx.treedepth);
    /*
     * Enumerate the tree and ensure it matches up to the array.
     */
    for (i = 0; NULL != (p = index234(tree, i)); i++) {
	if (i >= arraylen)
	    error("tree contains more than %d elements", arraylen);
	if (array[i] != p)
	    error("enum at position %d: array says %s, tree says %s",
		  i, array[i], p);
    }
    if (ctx.elemcount != i) {
	error("tree really contains %d elements, enum gave %d",
	      ctx.elemcount, i);
    }
    if (i < arraylen) {
	error("enum gave only %d elements, array has %d", i, arraylen);
    }
    i = count234(tree);
    if (ctx.elemcount != i) {
	error("tree really contains %d elements, count234 gave %d",
	      ctx.elemcount, i);
    }
}

void internal_addtest(void *elem, int index, void *realret)
{
    int i, j;
    void *retval;

    if (arraysize < arraylen + 1) {
	arraysize = arraylen + 1 + 256;
	array = sresize(array, arraysize, void *);
    }

    i = index;
    /* now i points to the first element >= elem */
    retval = elem;		       /* expect elem returned (success) */
    for (j = arraylen; j > i; j--)
	array[j] = array[j - 1];
    array[i] = elem;		       /* add elem to array */
    arraylen++;

    if (realret != retval) {
	error("add: retval was %p expected %p", realret, retval);
    }

    verify();
}

void addtest(void *elem)
{
    int i;
    void *realret;

    realret = add234(tree, elem);

    i = 0;
    while (i < arraylen && cmp(elem, array[i]) > 0)
	i++;
    if (i < arraylen && !cmp(elem, array[i])) {
	void *retval = array[i];       /* expect that returned not elem */
	if (realret != retval) {
	    error("add: retval was %p expected %p", realret, retval);
	}
    } else
	internal_addtest(elem, i, realret);
}

void addpostest(void *elem, int i)
{
    void *realret;

    realret = addpos234(tree, elem, i);

    internal_addtest(elem, i, realret);
}

void delpostest(int i)
{
    int index = i;
    void *elem = array[i], *ret;

    /* i points to the right element */
    while (i < arraylen - 1) {
	array[i] = array[i + 1];
	i++;
    }
    arraylen--;			       /* delete elem from array */

    if (tree->cmp)
	ret = del234(tree, elem);
    else
	ret = delpos234(tree, index);

    if (ret != elem) {
	error("del returned %p, expected %p", ret, elem);
    }

    verify();
}

void deltest(void *elem)
{
    int i;

    i = 0;
    while (i < arraylen && cmp(elem, array[i]) > 0)
	i++;
    if (i >= arraylen || cmp(elem, array[i]) != 0)
	return;			       /* don't do it! */
    delpostest(i);
}

/* A sample data set and test utility. Designed for pseudo-randomness,
 * and yet repeatability. */

/*
 * This random number generator uses the `portable implementation'
 * given in ANSI C99 draft N869. It assumes `unsigned' is 32 bits;
 * change it if not.
 */
int randomnumber(unsigned *seed)
{
    *seed *= 1103515245;
    *seed += 12345;
    return ((*seed) / 65536) % 32768;
}

int mycmp(void *av, void *bv)
{
    char const *a = (char const *) av;
    char const *b = (char const *) bv;
    return strcmp(a, b);
}

#define lenof(x) ( sizeof((x)) / sizeof(*(x)) )

char *strings[] = {
    "a", "ab", "absque", "coram", "de",
    "palam", "clam", "cum", "ex", "e",
    "sine", "tenus", "pro", "prae",
    "banana", "carrot", "cabbage", "broccoli", "onion", "zebra",
    "penguin", "blancmange", "pangolin", "whale", "hedgehog",
    "giraffe", "peanut", "bungee", "foo", "bar", "baz", "quux",
    "murfl", "spoo", "breen", "flarn", "octothorpe",
    "snail", "tiger", "elephant", "octopus", "warthog", "armadillo",
    "aardvark", "wyvern", "dragon", "elf", "dwarf", "orc", "goblin",
    "pixie", "basilisk", "warg", "ape", "lizard", "newt", "shopkeeper",
    "wand", "ring", "amulet"
};

#define NSTR lenof(strings)

int findtest(void)
{
    const static int rels[] = {
	REL234_EQ, REL234_GE, REL234_LE, REL234_LT, REL234_GT
    };
    const static char *const relnames[] = {
	"EQ", "GE", "LE", "LT", "GT"
    };
    int i, j, rel, index;
    char *p, *ret, *realret, *realret2;
    int lo, hi, mid, c;

    for (i = 0; i < NSTR; i++) {
	p = strings[i];
	for (j = 0; j < sizeof(rels) / sizeof(*rels); j++) {
	    rel = rels[j];

	    lo = 0;
	    hi = arraylen - 1;
	    while (lo <= hi) {
		mid = (lo + hi) / 2;
		c = strcmp(p, array[mid]);
		if (c < 0)
		    hi = mid - 1;
		else if (c > 0)
		    lo = mid + 1;
		else
		    break;
	    }

	    if (c == 0) {
		if (rel == REL234_LT)
		    ret = (mid > 0 ? array[--mid] : NULL);
		else if (rel == REL234_GT)
		    ret = (mid < arraylen - 1 ? array[++mid] : NULL);
		else
		    ret = array[mid];
	    } else {
		assert(lo == hi + 1);
		if (rel == REL234_LT || rel == REL234_LE) {
		    mid = hi;
		    ret = (hi >= 0 ? array[hi] : NULL);
		} else if (rel == REL234_GT || rel == REL234_GE) {
		    mid = lo;
		    ret = (lo < arraylen ? array[lo] : NULL);
		} else
		    ret = NULL;
	    }

	    realret = findrelpos234(tree, p, NULL, rel, &index);
	    if (realret != ret) {
		error("find(\"%s\",%s) gave %s should be %s",
		      p, relnames[j], realret, ret);
	    }
	    if (realret && index != mid) {
		error("find(\"%s\",%s) gave %d should be %d",
		      p, relnames[j], index, mid);
	    }
	    if (realret && rel == REL234_EQ) {
		realret2 = index234(tree, index);
		if (realret2 != realret) {
		    error("find(\"%s\",%s) gave %s(%d) but %d -> %s",
			  p, relnames[j], realret, index, index, realret2);
		}
	    }
#if 0
	    printf("find(\"%s\",%s) gave %s(%d)\n", p, relnames[j],
		   realret, index);
#endif
	}
    }

    realret = findrelpos234(tree, NULL, NULL, REL234_GT, &index);
    if (arraylen && (realret != array[0] || index != 0)) {
	error("find(NULL,GT) gave %s(%d) should be %s(0)",
	      realret, index, array[0]);
    } else if (!arraylen && (realret != NULL)) {
	error("find(NULL,GT) gave %s(%d) should be NULL", realret, index);
    }

    realret = findrelpos234(tree, NULL, NULL, REL234_LT, &index);
    if (arraylen
	&& (realret != array[arraylen - 1] || index != arraylen - 1)) {
	error("find(NULL,LT) gave %s(%d) should be %s(0)", realret, index,
	      array[arraylen - 1]);
    } else if (!arraylen && (realret != NULL)) {
	error("find(NULL,LT) gave %s(%d) should be NULL", realret, index);
    }
}

void searchtest_recurse(search234_state ss, int lo, int hi,
                        char **expected, char *directionbuf,
                        char *directionptr)
{
    *directionptr = '\0';

    if (!ss.element) {
        if (lo != hi) {
            error("search234(%s) gave NULL for non-empty interval [%d,%d)",
                  directionbuf, lo, hi);
        } else if (ss.index != lo) {
            error("search234(%s) gave index %d should be %d",
                  directionbuf, ss.index, lo);
        } else {
            printf("%*ssearch234(%s) gave NULL,%d\n",
                   (int)(directionptr-directionbuf) * 2, "", directionbuf,
                   ss.index);
        }
    } else if (lo == hi) {
        error("search234(%s) gave %s for empty interval [%d,%d)",
              directionbuf, (char *)ss.element, lo, hi);
    } else if (ss.element != expected[ss.index]) {
        error("search234(%s) gave element %s should be %s",
              directionbuf, (char *)ss.element, expected[ss.index]);
    } else if (ss.index < lo || ss.index >= hi) {
        error("search234(%s) gave index %d should be in [%d,%d)",
              directionbuf, ss.index, lo, hi);
        return;
    } else {
        search234_state next;

        printf("%*ssearch234(%s) gave %s,%d\n",
               (int)(directionptr-directionbuf) * 2, "", directionbuf,
               (char *)ss.element, ss.index);

        next = ss;
        search234_step(&next, -1);
        *directionptr = '-';
        searchtest_recurse(next, lo, ss.index,
                           expected, directionbuf, directionptr+1);

        next = ss;
        search234_step(&next, +1);
        *directionptr = '+';
        searchtest_recurse(next, ss.index+1, hi,
                           expected, directionbuf, directionptr+1);
    }
}

void searchtest(void)
{
    char *expected[NSTR], *p;
    char directionbuf[NSTR * 10];
    int n;
    search234_state ss;

    printf("beginning searchtest:");
    for (n = 0; (p = index234(tree, n)) != NULL; n++) {
        expected[n] = p;
        printf(" %d=%s", n, p);
    }
    printf(" count=%d\n", n);

    search234_start(&ss, tree);
    searchtest_recurse(ss, 0, n, expected, directionbuf, directionbuf);
}

int main(void)
{
    int in[NSTR];
    int i, j, k;
    unsigned seed = 0;

    for (i = 0; i < NSTR; i++)
	in[i] = 0;
    array = NULL;
    arraylen = arraysize = 0;
    tree = newtree234(mycmp);
    cmp = mycmp;

    verify();
    searchtest();
    for (i = 0; i < 10000; i++) {
	j = randomnumber(&seed);
	j %= NSTR;
	printf("trial: %d\n", i);
	if (in[j]) {
	    printf("deleting %s (%d)\n", strings[j], j);
	    deltest(strings[j]);
	    in[j] = 0;
	} else {
	    printf("adding %s (%d)\n", strings[j], j);
	    addtest(strings[j]);
	    in[j] = 1;
	}
	findtest();
        searchtest();
    }

    while (arraylen > 0) {
	j = randomnumber(&seed);
	j %= arraylen;
	deltest(array[j]);
    }

    freetree234(tree);

    /*
     * Now try an unsorted tree. We don't really need to test
     * delpos234 because we know del234 is based on it, so it's
     * already been tested in the above sorted-tree code; but for
     * completeness we'll use it to tear down our unsorted tree
     * once we've built it.
     */
    tree = newtree234(NULL);
    cmp = NULL;
    verify();
    for (i = 0; i < 1000; i++) {
	printf("trial: %d\n", i);
	j = randomnumber(&seed);
	j %= NSTR;
	k = randomnumber(&seed);
	k %= count234(tree) + 1;
	printf("adding string %s at index %d\n", strings[j], k);
	addpostest(strings[j], k);
    }
    while (count234(tree) > 0) {
	printf("cleanup: tree size %d\n", count234(tree));
	j = randomnumber(&seed);
	j %= count234(tree);
	printf("deleting string %s from index %d\n",
               (const char *)array[j], j);
	delpostest(j);
    }

    printf("%d errors found\n", n_errors);
    return (n_errors != 0);
}

#endif
