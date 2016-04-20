/*************************************************************************
 *
 *  file:  backtrace.cpp
 *
 * =======================================================================
 *  Backtracing structures and routines.  See also explain.c
 * =======================================================================
 */

/* ====================================================================
                        Backtracing routines
   ==================================================================== */

#include "ebc.h"

#include "agent.h"
#include "condition.h"
#include "debug.h"
#include "explain.h"
#include "instantiation.h"
#include "mem.h"
#include "memory_manager.h"
#include "preference.h"
#include "print.h"
#include "production.h"
#include "instantiation.h"
#include "soar_TraceNames.h"
#include "symbol.h"
#include "test.h"
#include "working_memory.h"
#include "xml.h"

#include <stdlib.h>


using namespace soar_TraceNames;

/* ====================================================================

                            Backtracing

   Four sets of conditions are maintained during backtracing:  locals,
   grounds, positive potentials, and negateds.  Negateds are really
   potentials, but we keep them separately throughout backtracing, and
   ground them at the very end.  Note that this means during backtracing,
   the grounds, positive potentials, and locals are all instantiated
   top-level positive conditions, so they all have a bt.wme_ on them.

   In order to avoid backtracing through the same instantiation twice,
   we mark each instantiation as we BT it, by setting
   inst->backtrace_number = backtrace_number (this is a global variable
   which gets incremented each time we build a chunk).

   Locals, grounds, and positive potentials are kept on lists (see the
   global variables below).  These are consed lists of the conditions
   (that is, the original instantiated conditions).  Furthermore,
   we mark the bt.wme_'s on each condition so we can quickly determine
   whether a given condition is already in a given set.  The "grounds_tc",
   "potentials_tc", "locals_tc", and "chunker_bt_pref" fields on wme's
   are used for this.  Wmes are marked as "in the grounds" by setting
   wme->grounds_tc = grounds_tc.  For potentials and locals, we also
   must set wme->chunker_bt_pref:  if the same wme was tested by two
   instantiations created at different times--times at which the wme
   was supported by two different preferences--then we really need to
   BT through *both* preferences.  Marking the wmes with just "locals_tc"
   or "potentials_tc" alone would prevent the second preference from
   being BT'd.

   The add_to_grounds(), add_to_potentials(), and add_to_locals()
   macros below are used to add conditions to these sets.  The negated
   conditions are maintained in the chunk_cond_set "negated_set."

   As we backtrace, each instantiation that has some Nots is added to
   the list instantiations_with_nots.  We have to go back afterwards
   and figure out which Nots are between identifiers that ended up in
   the grounds.
==================================================================== */

void Explanation_Based_Chunker::add_to_grounds(condition* cond)
{
    if ((cond)->bt.wme_->grounds_tc != grounds_tc)
    {
        (cond)->bt.wme_->grounds_tc = grounds_tc;
        cond->bt.wme_->chunker_bt_last_ground_cond = cond;
    }
    if (cond->bt.wme_->chunker_bt_last_ground_cond != cond)
    {
        add_singleton_unification_if_needed(cond);
    }
    push(thisAgent, (cond), grounds);
}

void Explanation_Based_Chunker::add_to_potentials(condition* cond)
{
    if ((cond)->bt.wme_->potentials_tc != potentials_tc)
    {
        (cond)->bt.wme_->potentials_tc = potentials_tc;
        (cond)->bt.wme_->chunker_bt_pref = (cond)->bt.trace;
    }
    push(thisAgent, (cond), positive_potentials);
}

void Explanation_Based_Chunker::add_to_locals(condition* cond)
{
    if ((cond)->bt.wme_->locals_tc != locals_tc)
    {
        (cond)->bt.wme_->locals_tc = locals_tc;
        (cond)->bt.wme_->chunker_bt_pref = (cond)->bt.trace;
    }
    add_local_singleton_unification_if_needed(cond);
    push(thisAgent, (cond), locals);
}

/* -------------------------------------------------------------------
                     Backtrace Through Instantiation

   This routine BT's through a given instantiation.  The general method
   is as follows:

     1. If we've already BT'd this instantiation, then skip it.
     2. Mark the TC (in the instantiated conditions) of all higher goal
        ids tested in top-level positive conditions
     3. Scan through the instantiated conditions; add each one to the
        appropriate set (locals, positive_potentials, grounds, negated_set).
     4. If the instantiation has any Nots, add this instantiation to
        the list of instantiations_with_nots.
------------------------------------------------------------------- */

/* mvp 5-17-94 */
void print_consed_list_of_conditions(agent* thisAgent, list* c, int indent)
{
    for (; c != NIL; c = c->rest)
    {
        if (get_printer_output_column(thisAgent) >= COLUMNS_PER_LINE - 20)
        {
            print(thisAgent,  "\n      ");
        }

        /* mvp 5-17-94 */
        print_spaces(thisAgent, indent);
        print_condition(thisAgent, static_cast<condition_struct*>(c->first));
    }
}

/* mvp 5-17-94 */
void print_consed_list_of_condition_wmes(agent* thisAgent, list* c, int indent)
{
    for (; c != NIL; c = c->rest)
    {
        if (get_printer_output_column(thisAgent) >= COLUMNS_PER_LINE - 20)
        {
            print(thisAgent,  "\n      ");
        }

        /* mvp 5-17-94 */
        print_spaces(thisAgent, indent);
        print(thisAgent,  "     ");
        print_wme(thisAgent, (static_cast<condition*>(c->first))->bt.wme_);
    }
}

/* This is the wme which is causing this production to be backtraced through.
   It is NULL when backtracing for a result preference.                   */

void Explanation_Based_Chunker::backtrace_through_instantiation(instantiation* inst,
                                     goal_stack_level grounds_level,
                                     condition* trace_cond,
                                     const identity_triple o_ids_to_replace,
                                     const rhs_triple rhs_funcs,
                                     uint64_t bt_depth,
                                     BTSourceType bt_type)
{

    tc_number tc;   /* use this to mark ids in the ground set */
    tc_number tc2;  /* use this to mark other ids we see */
    condition* c;
    list* grounds_to_print, *pots_to_print, *locals_to_print, *negateds_to_print;
    bool need_another_pass;
    dprint_header(DT_BACKTRACE, PrintBefore, "Backtracing instantiation i%u (matched %y at level %d) with RHS preference\n", inst->i_id, inst->prod_name, grounds_level);
    dprint(DT_BACKTRACE, "(%y [o%u] ^%y [o%u] %y [o%u]) that matched condition %l\n",
        get_ovar_for_o_id(o_ids_to_replace.id),o_ids_to_replace.id,
        get_ovar_for_o_id(o_ids_to_replace.attr),o_ids_to_replace.attr,
        get_ovar_for_o_id(o_ids_to_replace.value), o_ids_to_replace.value, trace_cond);
    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {

        /* mvp 5-17-94 */
        print(thisAgent,  "... BT through instantiation of ");
        if (inst->prod)
        {
            print_with_symbols(thisAgent, "%y\n", inst->prod_name);
        }
        else
        {
            print_string(thisAgent, "[Architectural Fake Instantiation]\n");
        }

        xml_begin_tag(thisAgent, kTagBacktrace);
        if (inst->prod)
        {
            xml_att_val(thisAgent, kProduction_Name, inst->prod_name);
        }
        else
        {
            xml_att_val(thisAgent, kProduction_Name, "[Architectural Fake Instantiation]");
        }

    }

    if (trace_cond)
    {
        unify_backtraced_conditions(trace_cond, o_ids_to_replace, rhs_funcs);
    }

    ++bt_depth;
    if (inst->explain_depth > bt_depth)
    {
        inst->explain_depth = bt_depth;
    }
    /* --- if the instantiation has already been BT'd, don't repeat it --- */
    if (inst->backtrace_number == backtrace_number)
    {
        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {

            /* mvp 5-17-94 */
            print_string(thisAgent, "(We already backtraced through this instantiation.)\n");
            xml_att_val(thisAgent, kBacktracedAlready, "true");
            xml_end_tag(thisAgent, kTagBacktrace);
        }
        #ifdef BUILD_WITH_EXPLAINER
        thisAgent->explanationLogger->increment_stat_seen_instantations_backtraced();
        #endif
        dprint(DT_BACKTRACE, "backtrace_through_instantiation returning b/c this instantiation already backtraced through.\n");
        return;
    }
    dprint(DT_EXPLAIN, "backtrace_through_instantiation setting backtrace number of i%u (%y) of to %d", inst->i_id, inst->prod_name, backtrace_number);
    inst->backtrace_number = backtrace_number;
    #ifdef BUILD_WITH_EXPLAINER
    thisAgent->explanationLogger->add_bt_instantiation(inst, bt_type);
    thisAgent->explanationLogger->increment_stat_instantations_backtraced();
    #endif
    if (!inst->reliable)
    {
        m_reliable = false;
    }

    /* --- mark transitive closure of each higher goal id that was tested in
       the id field of a top-level positive condition --- */
    tc = get_new_tc_number(thisAgent);
    tc2 = get_new_tc_number(thisAgent);
    need_another_pass = false;
    Symbol* thisID, *value;

    for (c = inst->top_of_instantiated_conditions; c != NIL; c = c->next)
    {

        if (c->type != POSITIVE_CONDITION)
        {
            continue;
        }

//        dprint(DT_BACKTRACE, "Backtracing marking tc of condition: %l\n", c);
        /* -- We copy any relational constraints found in this condition into a temporary map.
         *    When backtracing is complete and we are building the chunk conditions, we will
         *    add all of the relational constraints found while backtracing into the final
         *    chunk, whether the original condition the constraint came from made it into
         *    the chunk or not.  Since the constraint was necessary for the problem-solving
         *    -- */
        cache_constraints_in_cond(c);

        thisID = c->data.tests.id_test->eq_test->data.referent;

        if (thisID->tc_num == tc)
        {
            /* --- id is already in the TC, so add in the value --- */
            value = c->data.tests.value_test->eq_test->data.referent;
            if (value->symbol_type == IDENTIFIER_SYMBOL_TYPE)
            {
                /* --- if we already saw it before, we're going to have to go back
                   and make another pass to get the complete TC --- */
                if (value->tc_num == tc2)
                {
                    need_another_pass = true;
                }
                value->tc_num = tc;
            }
        }
        else if ((thisID->id->isa_goal) && (c->bt.level <= grounds_level))
        {
            /* --- id is a higher goal id that was tested: so add id to the TC --- */
            thisID->tc_num = tc;
            value = c->data.tests.value_test->eq_test->data.referent;
            if (value->symbol_type == IDENTIFIER_SYMBOL_TYPE)
            {
                /* --- if we already saw it before, we're going to have to go back
                   and make another pass to get the complete TC --- */
                if (value->tc_num == tc2)
                {
                    need_another_pass = true;
                }
                value->tc_num = tc;
            }
        }
        else
        {
            /* --- as far as we know so far, id shouldn't be in the tc: so mark it
               with number "tc2" to indicate that it's been seen already --- */
            thisID->tc_num = tc2;
        }
    }

    /* --- if necessary, make more passes to get the complete TC through the
       top-level positive conditions (recall that they're all super-simple
       wme tests--all three fields are equality tests --- */
    while (need_another_pass)
    {
        need_another_pass = false;
        for (c = inst->top_of_instantiated_conditions; c != NIL; c = c->next)
        {
            if (c->type != POSITIVE_CONDITION)
            {
                continue;
            }
            thisID = c->data.tests.id_test->eq_test->data.referent;
            if (thisID->tc_num != tc)
            {
                continue;
            }
            value = c->data.tests.value_test->eq_test->data.referent;
            if (value->symbol_type == IDENTIFIER_SYMBOL_TYPE)
                if (value->tc_num != tc)
                {
                    value->tc_num = tc;
                    need_another_pass = true;
                }
        } /* end of for loop */
    } /* end of while loop */

    /* --- scan through conditions, collect grounds, potentials, & locals --- */
    grounds_to_print = NIL;
    pots_to_print = NIL;
    locals_to_print = NIL;
    negateds_to_print = NIL;

    dprint(DT_BACKTRACE, "Backtracing collecting grounds, potentials and locals...\n");

    for (c = inst->top_of_instantiated_conditions; c != NIL; c = c->next)
    {
        if (c->type == POSITIVE_CONDITION)
        {
            thisID = c->data.tests.id_test->eq_test->data.referent;

            /* --- positive cond's are grounds, potentials, or locals --- */
            if (thisID->tc_num == tc)
            {
                dprint(DT_BACKTRACE, "Backtracing adding ground condition... %l (i%u)\n", c, c->inst->i_id);
                add_to_grounds(c);
                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    push(thisAgent, c, grounds_to_print);
                }
            }
            else if (c->bt.level <= grounds_level)
            {
                dprint(DT_BACKTRACE, "Backtracing adding potential condition... %l (i%u)\n", c, c->inst->i_id);
                add_to_potentials(c);
                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    push(thisAgent, c, pots_to_print);
                }
            }
            else
            {
                dprint(DT_BACKTRACE, "Backtracing adding local condition... %l (i%u)\n", c, c->inst->i_id);
                add_to_locals(c);
                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    push(thisAgent, c, locals_to_print);
                }
            }
        }
        else
        {
            dprint(DT_BACKTRACE, "Backtracing adding negated condition...%l (i%u)\n", c, c->inst->i_id);
            /* --- negative or nc cond's are either grounds or potentials --- */
            add_to_chunk_cond_set(&negated_set,
                                  make_chunk_cond_for_negated_condition(c));
            if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
            {
                push(thisAgent, c, negateds_to_print);
            }
        }
    } /* end of for loop */

//    dprint(DT_BACKTRACE, "Grounds:\n%3", grounds);
//    dprint(DT_BACKTRACE, "Potentials:\n%3", positive_potentials);
//    dprint(DT_BACKTRACE, "Locals:\n%3", locals);

    /* Now record the sets of conditions.  Note that these are not necessarily */
    /* the final resting place for these wmes.  In particular potentials may   */
    /* move over to become grounds, but since all we really need for explain is*/
    /* the list of wmes, this will do as a place to record them.               */

    /* --- if tracing BT, print the resulting conditions, etc. --- */
    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        /* mvp 5-17-94 */
        print_string(thisAgent, "  -->Grounds:\n");
        xml_begin_tag(thisAgent, kTagGrounds);
        print_consed_list_of_condition_wmes(thisAgent, grounds_to_print, 0);
        xml_end_tag(thisAgent, kTagGrounds);
        print(thisAgent,  "\n");
        print_string(thisAgent, "\n  -->Potentials:\n");
        xml_begin_tag(thisAgent, kTagPotentials);
        print_consed_list_of_condition_wmes(thisAgent, pots_to_print, 0);
        xml_end_tag(thisAgent, kTagPotentials);
        print(thisAgent,  "\n");
        print_string(thisAgent, "  -->Locals:\n");
        xml_begin_tag(thisAgent, kTagLocals);
        print_consed_list_of_condition_wmes(thisAgent, locals_to_print, 0);
        xml_end_tag(thisAgent, kTagLocals);
        print(thisAgent,  "\n");
        print_string(thisAgent, "  -->Negated:\n");
        xml_begin_tag(thisAgent, kTagNegated);
        print_consed_list_of_conditions(thisAgent, negateds_to_print, 0);
        xml_end_tag(thisAgent, kTagNegated);
        print(thisAgent,  "\n");
        /* mvp done */

        xml_begin_tag(thisAgent, kTagNots);
        xml_begin_tag(thisAgent, kTagNot);
        xml_end_tag(thisAgent, kTagNot);
        xml_end_tag(thisAgent, kTagNots);
        xml_end_tag(thisAgent, kTagBacktrace);
    }

    /* Moved these free's down to here, to ensure they are cleared even if we're
       not printing these lists     */

    free_list(thisAgent, grounds_to_print);
    free_list(thisAgent, pots_to_print);
    free_list(thisAgent, locals_to_print);
    free_list(thisAgent, negateds_to_print);
}

/* ---------------------------------------------------------------
                             Trace Locals

   This routine backtraces through locals, and keeps doing so until
   there are no more locals to BT.
--------------------------------------------------------------- */

void Explanation_Based_Chunker::trace_locals(goal_stack_level grounds_level)
{

    /* mvp 5-17-94 */
    cons* c, *CDPS;
    condition* cond;
    preference* bt_pref, *p;

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        print_string(thisAgent, "\n\n*** Tracing Locals ***\n");
        xml_begin_tag(thisAgent, kTagLocals);
    }

    while (locals)
    {
        c = locals;
        locals = locals->rest;
        cond = static_cast<condition_struct*>(c->first);
        free_cons(thisAgent, c);

        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            print_string(thisAgent, "\nFor local ");
            xml_begin_tag(thisAgent, kTagLocal);
            print_wme(thisAgent, cond->bt.wme_);
            print_string(thisAgent, " ");
        }

        bt_pref = find_clone_for_level(cond->bt.trace,
                                       static_cast<goal_stack_level>(grounds_level + 1));
        /* --- if it has a trace at this level, backtrace through it --- */
        if (bt_pref)
        {
            backtrace_through_instantiation(bt_pref->inst, grounds_level, cond, bt_pref->o_ids, bt_pref->rhs_funcs, cond->inst->explain_depth, BT_Normal);

            /* Check for any CDPS prefs and backtrace through them */
            if (cond->bt.CDPS)
            {
                for (CDPS = cond->bt.CDPS; CDPS != NIL; CDPS = CDPS->rest)
                {
                    p = static_cast<preference_struct*>(CDPS->first);
                    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                    {
                        print_string(thisAgent, "     Backtracing through CDPS preference: ");
                        xml_begin_tag(thisAgent, kTagCDPSPreference);
                        print_preference(thisAgent, p);
                    }
                    /* This used to pass in cond instead of NULL, but I think CDPS prefs are
                     * essentially like results in this context, which get NULL in that parameter */
                    backtrace_through_instantiation(p->inst, grounds_level, NULL, p->o_ids, p->rhs_funcs, cond->inst->explain_depth, BT_CDPS);

                    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                    {
                        xml_end_tag(thisAgent, kTagCDPSPreference);
                    }
                }
            }

            if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
            {
                xml_end_tag(thisAgent, kTagLocal);
            }
            continue;
        }

        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            print_string(thisAgent, "...no trace, can't BT");
            // add an empty <backtrace> tag to make parsing XML easier
            xml_begin_tag(thisAgent, kTagBacktrace);
            xml_end_tag(thisAgent, kTagBacktrace);
        }
        /* --- for augmentations of the local goal id, either handle the
           "^quiescence t" test or discard it --- */
        Symbol* thisID = cond->data.tests.id_test->eq_test->data.referent;
        Symbol* thisAttr = cond->data.tests.attr_test->eq_test->data.referent;
        Symbol* thisValue = cond->data.tests.value_test->eq_test->data.referent;
        if (thisID->id->isa_goal)
        {
            if ((thisAttr == thisAgent->quiescence_symbol) &&
                (thisValue == thisAgent->t_symbol) &&
                (! cond->test_for_acceptable_preference))
            {
                m_reliable = false;
            }
            if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
            {
                xml_end_tag(thisAgent, kTagLocal);
            }
            continue;
        }

        /* --- otherwise add it to the potential set --- */
        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            print_string(thisAgent, " --> make it a potential.");
            xml_begin_tag(thisAgent, kTagAddToPotentials);
            xml_end_tag(thisAgent, kTagAddToPotentials);
        }
        add_to_potentials(cond);

        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            xml_end_tag(thisAgent, kTagLocal);
        }

    } /* end of while locals loop */

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        xml_end_tag(thisAgent, kTagLocals);
    }
}

/* ---------------------------------------------------------------
                       Trace Grounded Potentials

   This routine looks for positive potentials that are in the TC
   of the ground set, and moves them over to the ground set.  This
   process is repeated until no more positive potentials are in
   the TC of the grounds.
--------------------------------------------------------------- */

void Explanation_Based_Chunker::trace_grounded_potentials()
{
    tc_number tc;
    cons* c, *next_c, *prev_c;
    condition* pot;
    bool need_another_pass;

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        print_string(thisAgent, "\n\n*** Tracing Grounded Potentials ***\n");
        xml_begin_tag(thisAgent, kTagGroundedPotentials);
    }

    /* --- setup the tc of the ground set --- */
    tc = get_new_tc_number(thisAgent);
    for (c = grounds; c != NIL; c = c->rest)
    {
        add_cond_to_tc(thisAgent, static_cast<condition_struct*>(c->first), tc, NIL, NIL);
    }

    need_another_pass = true;
    while (need_another_pass)
    {
        need_another_pass = false;
        /* --- look for any potentials that are in the tc now --- */
        prev_c = NIL;
        for (c = positive_potentials; c != NIL; c = next_c)
        {
            next_c = c->rest;
            pot = static_cast<condition_struct*>(c->first);
            if (cond_is_in_tc(thisAgent, pot, tc))
            {
                /* --- pot is a grounded potential, move it over to ground set --- */
                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    print_string(thisAgent, "\n-->Moving to grounds: ");
                    print_wme(thisAgent, pot->bt.wme_);
                }
                if (prev_c)
                {
                    prev_c->rest = next_c;
                }
                else
                {
                    positive_potentials = next_c;
                }
                if (pot->bt.wme_->grounds_tc != grounds_tc)   /* add pot to grounds */
                {
                    dprint(DT_BACKTRACE, "Moving potential to grounds: %l\n", pot);
                    pot->bt.wme_->grounds_tc = grounds_tc;
                    c->rest = grounds;
                    grounds = c;
                    pot->bt.wme_->chunker_bt_last_ground_cond = pot;
                    add_cond_to_tc(thisAgent, pot, tc, NIL, NIL);

                    need_another_pass = true;
                }
                else     /* pot was already in the grounds */
                {
                    dprint(DT_UNIFY_SINGLETONS, "Moving potential to grounds. (note wme already marked in grounds): %l\n", pot);
                    c->rest = grounds;
                    grounds = c;
                    add_cond_to_tc(thisAgent, pot, tc, NIL, NIL);
                    add_singleton_unification_if_needed(pot);
                    /* This used to delete the pot.  We keep it now amd add to tc, but I don't think we need another pass because
                     * symbols in bt_wme already had the right tc, and this potential should have the same symbols in its
                     * equality tests. */
                }
            }
            else
            {
                dprint(DT_BACKTRACE, "Not moving potential to grounds b/c not marked: %l\n", pot);
                prev_c = c;
            }
        } /* end of for c */
    } /* end of while need_another_pass */

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        xml_end_tag(thisAgent, kTagGroundedPotentials);
    }
}

/* Requires: pCond is a local condition */
void Explanation_Based_Chunker::add_local_singleton_unification_if_needed(condition* pCond)
{
    if (pCond->bt.wme_->id->id->isa_goal)
    {
        if (pCond->bt.wme_->attr == thisAgent->superstate_symbol)
        {
            if (!local_singleton_superstate_identity)
            {
                dprint(DT_UNIFY_SINGLETONS, "Storing identities for local singleton wme: %l\n", pCond);
                local_singleton_superstate_identity = new identity_triple(pCond->data.tests.id_test->eq_test->identity,
                    pCond->data.tests.attr_test->eq_test->identity, pCond->data.tests.value_test->eq_test->identity);
            } else {
                dprint(DT_UNIFY_SINGLETONS, "Unifying local singleton wme: %l\n", pCond);
                if (pCond->data.tests.id_test->eq_test->identity || local_singleton_superstate_identity->id)
                {
                    dprint(DT_UNIFY_SINGLETONS, "Unifying identity element %u -> %u\n", pCond->data.tests.id_test->eq_test->identity, local_singleton_superstate_identity->id);
                    add_identity_unification(pCond->data.tests.id_test->eq_test->identity, local_singleton_superstate_identity->id);
                }
                if (pCond->data.tests.attr_test->eq_test->identity || local_singleton_superstate_identity->attr)
                {
                    dprint(DT_UNIFY_SINGLETONS, "Unifying attr element %u -> %u\n", pCond->data.tests.attr_test->eq_test->identity, local_singleton_superstate_identity->attr);
                    add_identity_unification(pCond->data.tests.attr_test->eq_test->identity, local_singleton_superstate_identity->attr);
                }
                if (pCond->data.tests.value_test->eq_test->identity || local_singleton_superstate_identity->value)
                {
                    dprint(DT_UNIFY_SINGLETONS, "Unifying value element %u -> %u\n", pCond->data.tests.value_test->eq_test->identity, local_singleton_superstate_identity->value);
                    add_identity_unification(pCond->data.tests.value_test->eq_test->identity, local_singleton_superstate_identity->value);
                }
            }
        }
    }
}

/* Requires: pCond is being added to grounds and is the second condition being added to grounds
 *           that matched a given wme, which guarantees chunker_bt_last_ground_cond points to the
 *           first condition that matched. */
void Explanation_Based_Chunker::add_singleton_unification_if_needed(condition* pCond)
{
    /* MToDo:  Do we need to check if not a proposal?  This seems to already not unify proposals. */
    if (pCond->bt.wme_->id->id->isa_goal)
    {
        if ((pCond->bt.wme_->attr == thisAgent->operator_symbol) ||
            (pCond->bt.wme_->attr == thisAgent->superstate_symbol))
        {
            condition* last_cond = pCond->bt.wme_->chunker_bt_last_ground_cond;
            assert(last_cond);
            dprint(DT_UNIFY_SINGLETONS, "Unifying singleton wme already marked: %l\n", pCond);
            dprint(DT_UNIFY_SINGLETONS, " Other cond val: %l\n", pCond->bt.wme_->chunker_bt_last_ground_cond);
            if (pCond->data.tests.id_test->eq_test->identity || last_cond->data.tests.id_test->eq_test->identity)
            {
                dprint(DT_UNIFY_SINGLETONS, "Unifying identity element %u -> %u\n", pCond->data.tests.id_test->eq_test->identity, last_cond->data.tests.id_test->eq_test->identity);
                add_identity_unification(pCond->data.tests.id_test->eq_test->identity, last_cond->data.tests.id_test->eq_test->identity);
            }
            if (pCond->data.tests.attr_test->eq_test->identity || last_cond->data.tests.attr_test->eq_test->identity)
            {
                dprint(DT_UNIFY_SINGLETONS, "Unifying attr element %u -> %u\n", pCond->data.tests.attr_test->eq_test->identity, last_cond->data.tests.attr_test->eq_test->identity);
                add_identity_unification(pCond->data.tests.attr_test->eq_test->identity, last_cond->data.tests.attr_test->eq_test->identity);
            }
            if (pCond->data.tests.value_test->eq_test->identity || last_cond->data.tests.value_test->eq_test->identity)
            {
                dprint(DT_UNIFY_SINGLETONS, "Unifying value element %u -> %u\n", pCond->data.tests.value_test->eq_test->identity, last_cond->data.tests.value_test->eq_test->identity);
                add_identity_unification(pCond->data.tests.value_test->eq_test->identity, last_cond->data.tests.value_test->eq_test->identity);
            }
        }
    }
}
/* ---------------------------------------------------------------
                     Trace Ungrounded Potentials

   This routine backtraces through ungrounded potentials.  At entry,
   all potentials must be ungrounded.  This BT's through each
   potential that has some trace (at the right level) that we can
   BT through.  Other potentials are left alone.  true is returned
   if anything was BT'd; false if nothing changed.
--------------------------------------------------------------- */

bool Explanation_Based_Chunker::trace_ungrounded_potentials(goal_stack_level grounds_level)
{

    /* mvp 5-17-94 */
    cons* c, *next_c, *prev_c, *CDPS;
    cons* pots_to_bt;
    condition* potential;
    preference* bt_pref, *p;

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        print_string(thisAgent, "\n\n*** Tracing Ungrounded Potentials ***\n");
        xml_begin_tag(thisAgent, kTagUngroundedPotentials);
    }

    /* --- scan through positive potentials, pick out the ones that have
       a preference we can backtrace through --- */
    pots_to_bt = NIL;
    prev_c = NIL;
    for (c = positive_potentials; c != NIL; c = next_c)
    {
        next_c = c->rest;
        potential = static_cast<condition_struct*>(c->first);
        bt_pref = find_clone_for_level(potential->bt.trace,
                                       static_cast<goal_stack_level>(grounds_level + 1));
        if (bt_pref)
        {
            if (prev_c)
            {
                prev_c->rest = next_c;
            }
            else
            {
                positive_potentials = next_c;
            }
            c->rest = pots_to_bt;
            pots_to_bt = c;
        }
        else
        {
            prev_c = c;
        }
    }

    /* --- if none to BT, exit --- */
    if (!pots_to_bt)
    {
        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            xml_end_tag(thisAgent, kTagUngroundedPotentials);
        }
        return false;
    }

    /* --- backtrace through each one --- */
    while (pots_to_bt)
    {
        c = pots_to_bt;
        pots_to_bt = pots_to_bt->rest;
        potential = static_cast<condition_struct*>(c->first);
        free_cons(thisAgent, c);
        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            print_string(thisAgent, "\nFor ungrounded potential ");
            xml_begin_tag(thisAgent, kTagUngroundedPotential);
            print_wme(thisAgent, potential->bt.wme_);
            print_string(thisAgent, " ");
        }
        bt_pref = find_clone_for_level(potential->bt.trace,
                                       static_cast<goal_stack_level>(grounds_level + 1));

        backtrace_through_instantiation(bt_pref->inst, grounds_level, potential, bt_pref->o_ids, bt_pref->rhs_funcs, potential->inst->explain_depth, BT_Normal);

        if (potential->bt.CDPS)
        {
            for (CDPS = potential->bt.CDPS; CDPS != NIL; CDPS = CDPS->rest)
            {
                p = static_cast<preference_struct*>(CDPS->first);
                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    print_string(thisAgent, "     Backtracing through CDPS preference: ");
                    xml_begin_tag(thisAgent, kTagCDPSPreference);
                    print_preference(thisAgent, p);
                }

                /* This used to pass in potential instead of NULL, but I think CDPS prefs are
                 * essentially like results in this context, which get NULL in that parameter */
                backtrace_through_instantiation(p->inst, grounds_level, NULL, p->o_ids, p->rhs_funcs, potential->inst->explain_depth, BT_CDPS);

                if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
                {
                    xml_end_tag(thisAgent, kTagCDPSPreference);
                }
            }
        }

        if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
        {
            xml_end_tag(thisAgent, kTagUngroundedPotential);
        }
    }

    if (thisAgent->sysparams[TRACE_BACKTRACING_SYSPARAM])
    {
        xml_end_tag(thisAgent, kTagUngroundedPotentials);
    }

    return true;
}

void Explanation_Based_Chunker::report_local_negation(condition* c)
{
    if (thisAgent->sysparams[TRACE_CHUNK_NAMES_SYSPARAM])
    {
        // use the same code as the backtracing above
        list* negated_to_print = NIL;
        push(thisAgent, c, negated_to_print);

        print_string(thisAgent, "\n*** Chunk won't be formed due to local negation in backtrace ***\n");
        xml_begin_tag(thisAgent, kTagLocalNegation);
        print_consed_list_of_conditions(thisAgent, negated_to_print, 2);
        xml_end_tag(thisAgent, kTagLocalNegation);

        free_list(thisAgent, negated_to_print);
    }
}
