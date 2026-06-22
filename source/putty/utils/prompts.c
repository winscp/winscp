/*
 * Functions for making, destroying, and manipulating prompts_t
 * structures.
 */

#include "putty.h"

prompts_t *new_prompts(void)
{
    prompts_t *p = snew(prompts_t);
    p->prompts = NULL;
    p->n_prompts = p->prompts_size = 0;
    p->data = NULL;
    p->spr = SPR_INCOMPLETE;
    p->to_server = true; /* to be on the safe side */
    p->name = p->instruction = NULL;
    p->name_reqd = p->instr_reqd = false;
    p->callback = NULL;
    p->callback_ctx = NULL;
    p->ldisc_ptr_to_us = NULL;
    p->utf8 = false;
    return p;
}

void add_prompt(prompts_t *p, char *promptstr, bool echo)
{
    prompt_t *pr = snew(prompt_t);
    pr->prompt = promptstr;
    pr->echo = echo;
    pr->result = strbuf_new_nm();
    sgrowarray(p->prompts, p->prompts_size, p->n_prompts);
    p->prompts[p->n_prompts++] = pr;
}

void prompt_set_result(prompt_t *pr, const char *newstr)
{
    strbuf_clear(pr->result);
    put_dataz(pr->result, newstr);
}

const char *prompt_get_result_ref(prompt_t *pr)
{
    return pr->result->s;
}

char *prompt_get_result(prompt_t *pr)
{
    return dupstr(pr->result->s);
}

void free_prompts(prompts_t *p)
{
    size_t i;

    /* If an Ldisc currently knows about us, tell it to forget us, so
     * it won't dereference a stale pointer later. */
    if (p->ldisc_ptr_to_us)
        *p->ldisc_ptr_to_us = NULL;

    for (i=0; i < p->n_prompts; i++) {
        prompt_t *pr = p->prompts[i];
        strbuf_free(pr->result);
        sfree(pr->prompt);
        sfree(pr);
    }
    sfree(p->prompts);
    sfree(p->name);
    sfree(p->instruction);
    sfree(p);
}
