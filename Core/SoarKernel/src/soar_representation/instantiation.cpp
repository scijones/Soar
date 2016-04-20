/*************************************************************************
 * PLEASE SEE THE FILE "license.txt" (INCLUDED WITH THIS SOFTWARE PACKAGE)
 * FOR LICENSE AND COPYRIGHT INFORMATION.
 *************************************************************************/

/*************************************************************************
 *
 *  file:  instantiation.cpp
 *
 * =======================================================================
 *
 * Init_firer() and init_chunker() should be called at startup time, to
 * do initialization.
 *
 * Do_preference_phase() runs the entire preference phase.  This is called
 * from the top-level control in main.c.
 *
 * Possibly_deallocate_instantiation() checks whether an instantiation
 * can be deallocated yet, and does so if possible.  This is used whenever
 * the (implicit) reference count on the instantiation decreases.
 * =======================================================================
 */

#include "instantiation.h"

#include "agent.h"
#include "assert.h"
#include "callback.h"
#include "condition.h"
#include "debug.h"
#include "decide.h"
#include "ebc.h"
#include "instantiation.h"
#include "mem.h"
#include "misc.h"
#include "osupport.h"
#include "preference.h"
#include "print.h"
#include "production.h"
#include "reinforcement_learning.h"
#include "rete.h"
#include "rhs_functions.h"
#include "rhs.h"
#include "run_soar.h"
#include "slot.h"
#include "soar_module.h"
#include "soar_TraceNames.h"
#include "symbol.h"
#include "test.h"
#include "working_memory_activation.h"
#include "working_memory.h"
#include "xml.h"

#include <stdlib.h>
#include <string> // SBW 8/4/08
#include <list>

using namespace soar_TraceNames;

#ifdef USE_MEM_POOL_ALLOCATORS
typedef std::list<instantiation*,
        soar_module::soar_memory_pool_allocator<instantiation*> > inst_mpool_list;
typedef std::list<condition*,
        soar_module::soar_memory_pool_allocator<condition*> > cond_mpool_list;
#else
typedef std::list< instantiation* > inst_mpool_list;
typedef std::list< condition* > cond_mpool_list;
#endif

/* TEMPORARY HACK (Ideally this should be doable through
 the external kernel interface but for now using a
 couple of global STL lists to get this information
 from the rhs function to this preference adding code)*/
wme* glbDeepCopyWMEs = NULL;


void init_instantiation_pool(agent* thisAgent)
{
    thisAgent->memoryManager->init_memory_pool(MP_instantiation, sizeof(instantiation), "instantiation");
}

/* --------------------------------------------------------------------------
                 Build context-dependent preference set

  This function will copy the CDPS from a slot to the backtrace info for the
  corresponding condition.  The copied CDPS will later be backtraced through.

  Note: Until prohibits are included explicitly as part of the CDPS, we will
  just copy them directly from the prohibits list so that there is no
  additional overhead.  Once the CDPS is on by default, we can eliminate the
  second half of the big else (and not call this function at all if the
  sysparam is not set.

 --------------------------------------------------------------------------*/

void build_CDPS(agent* thisAgent, instantiation* inst)
{
    condition* cond;
    preference* pref, *new_pref;
    cons* CDPS;

    for (cond = inst->top_of_instantiated_conditions; cond != NIL;
            cond = cond->next)
    {
        cond->bt.CDPS = NIL;
        if (cond->type == POSITIVE_CONDITION && cond->bt.trace && cond->bt.trace->slot)
        {
            if (thisAgent->sysparams[CHUNK_THROUGH_EVALUATION_RULES_SYSPARAM])
            {
                if (cond->bt.trace->slot->CDPS)
                {
                    for (CDPS = cond->bt.trace->slot->CDPS; CDPS != NIL; CDPS = CDPS->rest)
                    {
                        new_pref = NIL;
                        pref = static_cast<preference*>(CDPS->first);
                        if (pref->inst->match_goal_level == inst->match_goal_level
                                && pref->in_tm)
                        {
                            push(thisAgent, pref, cond->bt.CDPS);
                            preference_add_ref(pref);
                        }
                        else
                        {
                            new_pref = find_clone_for_level(pref, inst->match_goal_level);
                            if (new_pref)
                            {
                                if (new_pref->in_tm)
                                {
                                    push(thisAgent, new_pref, cond->bt.CDPS);
                                    preference_add_ref(new_pref);
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                pref = cond->bt.trace->slot->preferences[PROHIBIT_PREFERENCE_TYPE];
                while (pref)
                {
                    new_pref = NIL;
                    if (pref->inst->match_goal_level == inst->match_goal_level && pref->in_tm)
                    {
                        push(thisAgent, pref, cond->bt.CDPS);
                        preference_add_ref(pref);
                    }
                    else
                    {
                        new_pref = find_clone_for_level(pref, inst->match_goal_level);
                        if (new_pref)
                        {
                            if (new_pref->in_tm)
                            {
                                push(thisAgent, new_pref, cond->bt.CDPS);
                                preference_add_ref(new_pref);
                            }
                        }
                    }
                    pref = pref->next;
                }
            }
        }
    }
}

/* -----------------------------------------------------------------------
 Find Clone For Level

 This routines take a given preference and finds the clone of it whose
 match goal is at the given goal_stack_level.  (This is used to find the
 proper preference to backtrace through.)  If the given preference
 itself is at the right level, it is returned.  If there is no clone at
 the right level, NIL is returned.
 ----------------------------------------------------------------------- */

preference* find_clone_for_level(preference* p, goal_stack_level level)
{
    preference* clone;

    if (!p)
    {
        /* --- if the wme doesn't even have a preference on it, we can't backtrace
         at all (this happens with I/O and some architecture-created wmes --- */
        return NIL;
    }

    /* --- look at pref and all of its clones, find one at the right level --- */

    if (p->inst->match_goal_level == level)
    {
        return p;
    }

    for (clone = p->next_clone; clone != NIL; clone = clone->next_clone)
        if (clone->inst->match_goal_level == level)
        {
            return clone;
        }

    for (clone = p->prev_clone; clone != NIL; clone = clone->prev_clone)
        if (clone->inst->match_goal_level == level)
        {
            return clone;
        }

    /* --- if none was at the right level, we can't backtrace at all --- */
    return NIL;
}

/* =======================================================================

 Firer Utilities

 ======================================================================= */

/* -----------------------------------------------------------------------
 Find Match Goal

 Given an instantiation, this routines looks at the instantiated
 conditions to find its match goal.  It fills in inst->match_goal and
 inst->match_goal_level.  If there is a match goal, match_goal is set
 to point to the goal identifier.  If no goal was matched, match_goal
 is set to NIL and match_goal_level is set to ATTRIBUTE_IMPASSE_LEVEL.
 ----------------------------------------------------------------------- */

void find_match_goal(instantiation* inst)
{
    Symbol* lowest_goal_so_far;
    goal_stack_level lowest_level_so_far;
    condition* cond;
    Symbol* id;

    lowest_goal_so_far = NIL;
    lowest_level_so_far = -1;
    for (cond = inst->top_of_instantiated_conditions; cond != NIL;
            cond = cond->next)
        if (cond->type == POSITIVE_CONDITION)
        {
            id = cond->bt.wme_->id;
            if (id->id->isa_goal)
                if (cond->bt.level > lowest_level_so_far)
                {
                    lowest_goal_so_far = id;
                    lowest_level_so_far = cond->bt.level;
                }
        }

    inst->match_goal = lowest_goal_so_far;
    if (lowest_goal_so_far)
    {
        inst->match_goal_level = lowest_level_so_far;
    }
    else
    {
        inst->match_goal_level = ATTRIBUTE_IMPASSE_LEVEL;
    }
}


goal_stack_level get_match_goal(condition* top_cond)
{
    goal_stack_level lowest_level_so_far;
    condition* cond;
    Symbol* id;

    lowest_level_so_far = -1;
    for (cond = top_cond; cond != NIL; cond = cond->next)
    {
        if (cond->type == POSITIVE_CONDITION)
        {
            id = cond->bt.wme_->id;
            if (id->id->isa_goal)
                if (cond->bt.level > lowest_level_so_far)
                {
                    lowest_level_so_far = cond->bt.level;
                }
        }
    }
    if (lowest_level_so_far != -1)
    {
        return lowest_level_so_far;
    }
    else
    {
        return ATTRIBUTE_IMPASSE_LEVEL;
    }
}
/* -----------------------------------------------------------------------

 Executing the RHS Actions of an Instantiation

 Execute_action() executes a given RHS action.  For MAKE_ACTION's, it
 returns the created preference structure, or NIL if an error occurs.
 For FUNCALL_ACTION's, it returns NIL.

 Instantiate_rhs_value() returns the (symbol) instantiation of an
 rhs_value, or NIL if an error occurs.  It takes a new_id_level
 argument indicating what goal_stack_level a new id is to be created
 at, in case a gensym is needed for the instantiation of a variable.
 (although I'm not sure this is really needed.)

 As rhs unbound variables are encountered, they are instantiated with
 new gensyms.  These gensyms are then stored in the rhs_variable_bindings
 array, so if the same unbound variable is encountered a second time
 it will be instantiated with the same gensym.
 ----------------------------------------------------------------------- */

Symbol* instantiate_rhs_value(agent* thisAgent, rhs_value rv,
                              goal_stack_level new_id_level, char new_id_letter,
                              struct token_struct* tok, wme* w)
{
    list* fl;
    list* arglist;
    cons* c, *prev_c, *arg_cons;
    rhs_function* rf;
    Symbol* result;
    bool nil_arg_found;

    if (rhs_value_is_symbol(rv))
    {

        result = rhs_value_to_symbol(rv);
        /*
         Long-Winded Case-by-Case [Hopeful] Explanation

         This has to do with long-term identifiers (LTIs) that exist within productions (including chunks/justifications).
         The real issue is that identifiers, upon creation, require a goal level (used for promotion/demotion/garbage collection).
         At the time of parsing a rule, we don't have this information, so we give it an invalid "unknown" value.
         This is OK on the condition side of a rule, since the rete (we think) will just consider it another symbol used for matching.
         However, it becomes hairy when LTIs are on the action side of a rule, with respect to the state of the LTI in working memory and the rule LHS.
         Consider the following cases:

         1. Identifier is LTI, does NOT exist as a LHS symbol
         - we do NOT support this!!!  bad things will likely happen due to potential for adding an identifier to working memory
         with an unknown goal level.
         - Note:  The re-orderer has been changed so that we can allow LTIs in the identifier element if it is indirectly
                  linked to an identifier with a level

         2. Attribute/Value is LTI, does NOT exist as a LHS symbol (!!!!!IMPORTANT CASE!!!!!)
         - the caller of this function will supply new_id_level (probably based upon the level of the id).
         - if this is valid (i.e. greater than 0), we use it.  else, ignore.
         - we have a huge assert on add_wme_to_wm that will kill soar if we try to add an identifier to working memory with an invalid level.

         3. Identifier/Attribute/Value is LTI, DOES exist as LHS symbol
         - in this situation, we are *guaranteed* that the resulting LTI (since it is in WM) has a valid goal level.
         - it should be noted that if a value, the level of the LTI may change during promotion/demotion/garbage collection,
         but this is natural Soar behavior and outside our purvue.

         */
        if ((result->is_lti()) &&
                (result->id->level == SMEM_LTI_UNKNOWN_LEVEL) &&
                (new_id_level > 0))
        {
            dprint(DT_UNKNOWN_LEVEL, "Setting level for LTI %y from SMEM_LTI_UNKNOWN_LEVEL to %d.\n", result, new_id_level);
            result->id->level = new_id_level;
            result->id->promotion_level = new_id_level;
        }

        symbol_add_ref(thisAgent, result);
        return result;
    }

    if (rhs_value_is_unboundvar(rv))
    {
        int64_t index;
        Symbol* sym;

        index = static_cast<int64_t>(rhs_value_to_unboundvar(rv));
        if (thisAgent->firer_highest_rhs_unboundvar_index < index)
        {
            thisAgent->firer_highest_rhs_unboundvar_index = index;
        }
        sym = *(thisAgent->rhs_variable_bindings + index);

        if (!sym)
        {
            sym = make_new_identifier(thisAgent, new_id_letter, new_id_level);
            *(thisAgent->rhs_variable_bindings + index) = sym;
            return sym;
        }
        else if (sym->is_variable())
        {
            new_id_letter = *(sym->var->name + 1);
            sym = make_new_identifier(thisAgent, new_id_letter, new_id_level);
            *(thisAgent->rhs_variable_bindings + index) = sym;
            return sym;
        }
        else
        {
            symbol_add_ref(thisAgent, sym);
            return sym;
        }
    }

    if (rhs_value_is_reteloc(rv))
    {
        result = get_symbol_from_rete_loc(rhs_value_to_reteloc_levels_up(rv),
                                          rhs_value_to_reteloc_field_num(rv), tok, w);
        symbol_add_ref(thisAgent, result);
        return result;
    }

    fl = rhs_value_to_funcall_list(rv);
    rf = static_cast<rhs_function_struct*>(fl->first);

    /* --- build up a list of the argument values --- */
    prev_c = NIL;
    nil_arg_found = false;
    arglist = NIL; /* unnecessary, but gcc -Wall warns without it */
    for (arg_cons = fl->rest; arg_cons != NIL; arg_cons = arg_cons->rest)
    {
        allocate_cons(thisAgent, &c);
        c->first = instantiate_rhs_value(thisAgent,
                                         static_cast<char*>(arg_cons->first), new_id_level,
                                         new_id_letter, tok, w);
        if (!c->first)
        {
            nil_arg_found = true;
        }
        if (prev_c)
        {
            prev_c->rest = c;
        }
        else
        {
            arglist = c;
        }
        prev_c = c;
    }
    if (prev_c)
    {
        prev_c->rest = NIL;
    }
    else
    {
        arglist = NIL;
    }

    /* --- if all args were ok, call the function --- */

    if (!nil_arg_found)
    {
        // stop the kernel timer while doing RHS funcalls  KJC 11/04
        // the total_cpu timer needs to be updated in case RHS fun is statsCmd
#ifndef NO_TIMING_STUFF
        thisAgent->timers_kernel.stop();
        thisAgent->timers_cpu.stop();
        thisAgent->timers_total_kernel_time.update(thisAgent->timers_kernel);
        thisAgent->timers_total_cpu_time.update(thisAgent->timers_cpu);
        thisAgent->timers_cpu.start();
#endif

        result = (*(rf->f))(thisAgent, arglist, rf->user_data);

#ifndef NO_TIMING_STUFF  // restart the kernel timer
        thisAgent->timers_kernel.start();
#endif

    }
    else
    {
        result = NIL;
    }

    /* --- scan through arglist, dereference symbols and deallocate conses --- */
    for (c = arglist; c != NIL; c = c->rest)
        if (c->first)
        {
            symbol_remove_ref(thisAgent, static_cast<Symbol*>(c->first));
        }
    free_list(thisAgent, arglist);

    return result;
}

preference* execute_action(agent* thisAgent, action* a, struct token_struct* tok, wme* w,
                           action* rule_action,
                           condition* cond)
{
    Symbol* id, *attr, *value, *referent;
    char first_letter;

    if (a->type == FUNCALL_ACTION)
    {
        value = instantiate_rhs_value(thisAgent, a->value, -1, 'v', tok, w);
        if (value)
        {
            symbol_remove_ref(thisAgent, value);
        }
        return NIL;
    }

    attr = NIL;
    value = NIL;
    referent = NIL;
    id = instantiate_rhs_value(thisAgent, a->id, -1, 's', tok, w);
    if (!id)
    {
        goto abort_execute_action;
    }
    if (!id->is_identifier())
    {
        print_with_symbols(thisAgent,
                           "Error: RHS makes a preference for %y (not an identifier)\n",
                           id);
        goto abort_execute_action;
    }

    attr = instantiate_rhs_value(thisAgent, a->attr, id->id->level, 'a', tok, w);
    if (!attr)
    {
        goto abort_execute_action;
    }

    first_letter = first_letter_from_symbol(attr);

    value = instantiate_rhs_value(thisAgent, a->value, id->id->level,
                                  first_letter, tok, w);
    if (!value)
    {
        goto abort_execute_action;
    }

    if (preference_is_binary(a->preference_type))
    {
        referent = instantiate_rhs_value(thisAgent, a->referent, id->id->level,
                                         first_letter, tok, w);
        if (!referent)
        {
            goto abort_execute_action;
        }
    }

    if (((a->preference_type != ACCEPTABLE_PREFERENCE_TYPE)
            && (a->preference_type != REJECT_PREFERENCE_TYPE))
            && (!(id->id->isa_goal && (attr == thisAgent->operator_symbol))))
    {
        print_with_symbols(thisAgent,
                           "\nError: attribute preference other than +/- for %y ^%y -- ignoring it.",
                           id, attr);
        goto abort_execute_action;
    }
    /* Populate identity and rhs_function stuff */
    uint64_t oid_id, oid_attr, oid_value, oid_referent;
    rhs_value f_id, f_attr, f_value;
    if (rule_action)
    {
        if (rule_action->id)
        {
            if (rhs_value_is_funcall(rule_action->id))
            {
                oid_id = 0;
                f_id = rule_action->id;
                /* rule_action will get deallocated in create_instantiation, but we want it
                 * in the preference for learning, so we just steal this copy and set
                 * rule_action's to null */
                rule_action->id = NULL;
            } else {
                oid_id = rhs_value_to_o_id(rule_action->id);
                f_id = 0;
            }
        } else {
            oid_id = 0;
            f_id = 0;
        }
        if (rule_action->attr)
        {
            if (rhs_value_is_funcall(rule_action->attr))
            {
                oid_attr = 0;
                f_attr = rule_action->attr;
                rule_action->attr = NULL;
            } else {
                oid_attr = rhs_value_to_o_id(rule_action->attr);
                f_attr = 0;
            }
        } else {
            oid_attr = 0;
            f_attr = 0;
        }
        if (rule_action->value)
        {
            if (rhs_value_is_funcall(rule_action->value))
            {
                oid_value = 0;
                f_value = rule_action->value;
                rule_action->value = NULL;
            } else {
                oid_value = rhs_value_to_o_id(rule_action->value);
                f_value = 0;
            }
        } else {
            oid_value = 0;
            f_value = 0;
        }
        if (rule_action->referent)
        {
            assert(!rhs_value_is_funcall(rule_action->referent));
            oid_referent = rhs_value_to_o_id(rule_action->referent);
        } else {
            oid_referent = 0;
        }
    }
    return make_preference(thisAgent, a->preference_type, id, attr, value, referent,
                           identity_triple(oid_id, oid_attr, oid_value, oid_referent),
                           rhs_triple(f_id, f_attr, f_value));

abort_execute_action: /* control comes here when some error occurred */
    if (id)
    {
        symbol_remove_ref(thisAgent, id);
    }
    if (attr)
    {
        symbol_remove_ref(thisAgent, attr);
    }
    if (value)
    {
        symbol_remove_ref(thisAgent, value);
    }
    if (referent)
    {
        symbol_remove_ref(thisAgent, referent);
    }
    return NIL;
}

/* -----------------------------------------------------------------------
 Fill In New Instantiation Stuff

 This routine fills in a newly created instantiation structure with
 various information.   At input, the instantiation should have:
 - preferences_generated filled in;
 - instantiated conditions filled in;
 - top-level positive conditions should have bt.wme_, bt.level, and
 bt.trace filled in, but bt.wme_ and bt.trace shouldn't have their
 reference counts incremented yet.

 This routine does the following:
 - increments reference count on production;
 - fills in match_goal and match_goal_level;
 - for each top-level positive cond:
 replaces bt.trace with the preference for the correct level,
 updates reference counts on bt.pref and bt.wmetraces and wmes
 - for each preference_generated, adds that pref to the list of all
 pref's for the match goal
 - fills in backtrace_number;
 - if "need_to_do_support_calculations" is true, calculates o-support
 for preferences_generated;
 ----------------------------------------------------------------------- */

void init_instantiation(agent*          thisAgent,
                        instantiation*  inst,
                        bool            need_to_do_support_calculations,
                        instantiation*  original_inst)
{
    condition* cond;
    preference* p;
    goal_stack_level level;

    production_add_ref(inst->prod);

    find_match_goal(inst);

    level = inst->match_goal_level;

    /* Note: since we'll never backtrace through instantiations at the top
     level, it might make sense to not increment the reference counts
     on the wmes and preferences here if the instantiation is at the top
     level.  As it stands now, we could gradually accumulate garbage at
     the top level if we have a never-ending sequence of production
     firings at the top level that chain on each other's results.  (E.g.,
     incrementing a counter on every decision cycle.)  I'm leaving it this
     way for now, because if we go to S-Support, we'll (I think) need to
     save these around (maybe). */

    /* KJC 6/00:  maintaining all the top level ref cts does have a big
     impact on memory pool usage and also performance (due to malloc).
     (See tests done by Scott Wallace Fall 99.)  Therefore added
     preprocessor macro so that by unsetting macro the top level ref cts are not
     incremented.  It's possible that in some systems, these ref cts may
     be desireable: they can be added by defining DO_TOP_LEVEL_REF_CTS
     */

    for (cond = inst->top_of_instantiated_conditions; cond != NIL; cond = cond->next)
    {
        if (cond->type == POSITIVE_CONDITION)
        {
#ifdef DO_TOP_LEVEL_REF_CTS
            wme_add_ref(cond->bt.wme_);
#else
            if (level > TOP_GOAL_LEVEL)
            {
                wme_add_ref(cond->bt.wme_);
            }
#endif
            /* --- if trace is for a lower level, find one for this level --- */
            if (cond->bt.trace)
            {
                if (cond->bt.trace->inst->match_goal_level > level)
                {
                    cond->bt.trace = find_clone_for_level(cond->bt.trace,
                                                          level);
                }
#ifdef DO_TOP_LEVEL_REF_CTS
                if (cond->bt.trace)
                {
                    preference_add_ref(cond->bt.trace);
                }
#else
                if ((cond->bt.trace) && (level > TOP_GOAL_LEVEL))
                {
                    preference_add_ref(cond->bt.trace);
                }
#endif
            }
        }
        cond->inst = inst;
    }

    if (inst->match_goal)
    {
        for (p = inst->preferences_generated; p != NIL; p = p->inst_next)
        {
            insert_at_head_of_dll(inst->match_goal->id->preferences_from_goal, p,
                                  all_of_goal_next, all_of_goal_prev);
            p->on_goal_list = true;
        }
    }
    inst->backtrace_number = 0;

    if ((thisAgent->o_support_calculation_type == 0)
            || (thisAgent->o_support_calculation_type == 3)
            || (thisAgent->o_support_calculation_type == 4))
    {
        /* --- do calc's the normal Soar 6 way --- */
        if (need_to_do_support_calculations)
        {
            calculate_support_for_instantiation_preferences(thisAgent, inst, original_inst);
        }
    }
    else if (thisAgent->o_support_calculation_type == 1)
    {
        if (need_to_do_support_calculations)
        {
            calculate_support_for_instantiation_preferences(thisAgent, inst, original_inst);
        }
        /* --- do calc's both ways, warn on differences --- */
        if ((inst->prod->declared_support != DECLARED_O_SUPPORT)
                && (inst->prod->declared_support != DECLARED_I_SUPPORT))
        {
            /* --- At this point, we've done them the normal way.  To look for
             differences, save o-support flags on a list, then do Doug's
             calculations, then compare and restore saved flags. --- */
            list* saved_flags;
            preference* pref;
            bool difference_found;
            saved_flags = NIL;
            for (pref = inst->preferences_generated; pref != NIL;
                    pref = pref->inst_next)
            {
                push(thisAgent, (pref->o_supported ? pref : NIL), saved_flags);
            }
            saved_flags = destructively_reverse_list(saved_flags);
            dougs_calculate_support_for_instantiation_preferences(thisAgent,
                    inst);
            difference_found = false;
            for (pref = inst->preferences_generated; pref != NIL;
                    pref = pref->inst_next)
            {
                cons* c;
                bool b;
                c = saved_flags;
                saved_flags = c->rest;
                b = (c->first ? true : false);
                free_cons(thisAgent, c);
                if (pref->o_supported != b)
                {
                    difference_found = true;
                }
                pref->o_supported = b;
            }
            if (difference_found)
            {
                print_with_symbols(thisAgent,
                                   "\n*** O-support difference found in production %y",
                                   inst->prod_name);
            }
        }
    }
    else
    {
        /* --- do calc's Doug's way --- */
        if ((inst->prod->declared_support != DECLARED_O_SUPPORT)
                && (inst->prod->declared_support != DECLARED_I_SUPPORT))
        {
            dougs_calculate_support_for_instantiation_preferences(thisAgent,
                    inst);
        }
    }
}

inline bool trace_firings_of_inst(agent* thisAgent, instantiation* inst)
{
    return ((inst)->prod
            && (thisAgent->sysparams[TRACE_FIRINGS_OF_USER_PRODS_SYSPARAM
                                     + (inst)->prod->type] || ((inst)->prod->trace_firings)));
}

/* -----------------------------------------------------------------------
 Create Instantiation

 This builds the instantiation for a new match, and adds it to
 newly_created_instantiations.  It also calls chunk_instantiation() to
 do any necessary chunk or justification building.
 ----------------------------------------------------------------------- */
void create_instantiation(agent* thisAgent, production* prod,
                          struct token_struct* tok, wme* w)
{
    instantiation* inst;
    condition* cond;
    preference* pref;
    action* a, *a2, *rhs_vars = NULL;
    cons* c;
    bool need_to_do_support_calculations;
    bool trace_it;
    int64_t index;
    Symbol** cell;

#ifdef BUG_139_WORKAROUND
    /* New waterfall model: this is now checked for before we call this function */
    assert(prod->type != JUSTIFICATION_PRODUCTION_TYPE);
    /* RPM workaround for bug #139: don't fire justifications */
    //if (prod->type == JUSTIFICATION_PRODUCTION_TYPE) {
    //    return;
    //}
#endif

    thisAgent->memoryManager->allocate_with_pool(MP_instantiation, &inst);
    inst->next = thisAgent->newly_created_instantiations;
    thisAgent->newly_created_instantiations = inst;
    inst->prod = prod;
    inst->rete_token = tok;
    inst->rete_wme = w;
    inst->reliable = true;
    inst->in_ms = true;
    inst->i_id = thisAgent->ebChunker->get_new_inst_id();
    inst->explain_status = explain_unrecorded;
    inst->explain_depth = 0;
    inst->explain_tc_num = 0;
    inst->GDS_evaluated_already = false;
    inst->prod_name = prod ? prod->name : thisAgent->architecture_inst_symbol;
    symbol_add_ref(thisAgent, inst->prod_name);
    dprint_header(DT_MILESTONES, PrintBefore,
        "create_instantiation() for instance of %y (id=%u) begun.\n",
        inst->prod_name, inst->i_id);
//    if ((inst->i_id == 3) || (inst->i_id == 9))
//    {
//        dprint(DT_DEBUG, "Found.\n");
//    }
    if (thisAgent->soar_verbose_flag == true)
    {
        print_with_symbols(thisAgent,
            "\n   In create_instantiation for instance of rule %y",
            inst->prod_name);
        char buf[256];
        SNPRINTF(buf, 254, "in create_instantiation: %s",
            inst->prod_name->to_string(true));
        xml_generate_verbose(thisAgent, buf);
    }

    thisAgent->production_being_fired = inst->prod;
    prod->firing_count++;
    thisAgent->production_firing_count++;

    AddAdditionalTestsMode additional_test_mode;
    if (prod->type == TEMPLATE_PRODUCTION_TYPE) {
        additional_test_mode = JUST_INEQUALITIES;
    } else if (thisAgent->sysparams[LEARNING_ON_SYSPARAM])
    {
        additional_test_mode = ALL_ORIGINALS;
    } else  {
        additional_test_mode = DONT_EXPLAIN;
    }
    /* --- build the instantiated conditions, and bind LHS variables --- */
//    if (additional_test_mode != DONT_EXPLAIN)
//    {
        p_node_to_conditions_and_rhs(thisAgent, prod->p_node, tok, w,
            &(inst->top_of_instantiated_conditions),
            &(inst->bottom_of_instantiated_conditions), &(rhs_vars),
            inst->i_id, additional_test_mode);
//    } else {
//        p_node_to_conditions_and_rhs(thisAgent, prod->p_node, tok, w,
//            &(inst->top_of_instantiated_conditions),
//            &(inst->bottom_of_instantiated_conditions), NULL,
//            inst->i_id, additional_test_mode);
//    }
    /* --- record the level of each of the wmes that was positively tested --- */
    for (cond = inst->top_of_instantiated_conditions; cond != NIL; cond = cond->next)
    {
        cond->inst = inst;
        if (cond->type == POSITIVE_CONDITION)
        {
            cond->bt.level = cond->bt.wme_->id->id->level;
            cond->bt.trace = cond->bt.wme_->preference;
        }
    }
    /* --- print trace info --- */
    trace_it = trace_firings_of_inst(thisAgent, inst);
    if (trace_it)
    {
        start_fresh_line(thisAgent);
        print(thisAgent,  "Firing ");
        print_instantiation_with_wmes(thisAgent, inst,
                                      static_cast<wme_trace_type>(thisAgent->sysparams[TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM]),
                                      0);
    }

    /* --- initialize rhs_variable_bindings array with names of variables
     (if there are any stored on the production -- for chunks there won't
     be any) --- */
    index = 0;
    cell = thisAgent->rhs_variable_bindings;
    for (c = prod->rhs_unbound_variables; c != NIL; c = c->rest)
    {
        *(cell++) = static_cast<symbol_struct*>(c->first);
        index++;
    }
    thisAgent->firer_highest_rhs_unboundvar_index = index - 1;

    /* 7.1/8 merge: Not sure about this.  This code in 704, but not in either 7.1 or 703/soar8 */
    /* --- Before executing the RHS actions, tell the user that the -- */
    /* --- phase has changed to output by printing the arrow --- */
    if (trace_it && thisAgent->sysparams[TRACE_FIRINGS_PREFERENCES_SYSPARAM])
    {
        print(thisAgent,  " -->\n");
        xml_object(thisAgent, kTagActionSideMarker);
    }

    /* --- execute the RHS actions, collect the results --- */
    inst->preferences_generated = NIL;
    need_to_do_support_calculations = false;
    a2 = rhs_vars;
    goal_stack_level glbDeepCopyWMELevel = 0;

    for (a = prod->action_list; a != NIL; a = a->next)
    {
        if (prod->type != TEMPLATE_PRODUCTION_TYPE)
        {
            dprint(DT_RL_VARIABLIZATION, "Executing action for non-template production.\n");
            if (a2)
            {
//                dprint(DT_RHS_VARIABLIZATION, "Executing action:\n%a\n[%a]\n", a, a2);
                pref = execute_action(thisAgent, a, tok, w, a2, inst->top_of_instantiated_conditions);
            } else {
//                dprint(DT_RHS_VARIABLIZATION, "Executing action:\n%a\n", a);
                pref = execute_action(thisAgent, a, tok, w, NULL, inst->top_of_instantiated_conditions);
            }
        }
        else
        {
            dprint(DT_RL_VARIABLIZATION, "Executing action for template production.  (building template instantiation)\n");
            pref = NIL;
            rl_build_template_instantiation(thisAgent, inst, tok, w, a2);

        }

        /* If glbDeepCopyWMEs exists it must have been the rhs function executed, so
         * save the goal stack level for preferences that it generates. */
        if (pref && glbDeepCopyWMEs)
        {
            glbDeepCopyWMELevel = pref->id->id->level;
        }
        /* SoarTech changed from an IF stmt to a WHILE loop to support GlobalDeepCpy */
        while (pref)
        {
            /* The parser assumes that any rhs preference of the form
             *
             * (<s> ^operator <o> = <x>)
             *
             * is a binary indifferent preference, because it assumes <x> is an
             * operator. However, it could be the case that <x> is actually bound to
             * a number, which would make this a numeric indifferent preference. The
             * parser had no way of easily figuring this out, but it's easy to check
             * here.
             *
             * jzxu April 22, 2009
             */
            if ((pref->type == BINARY_INDIFFERENT_PREFERENCE_TYPE)
                    && ((pref->referent->symbol_type
                         == FLOAT_CONSTANT_SYMBOL_TYPE)
                        || (pref->referent->symbol_type
                            == INT_CONSTANT_SYMBOL_TYPE)))
            {
                pref->type = NUMERIC_INDIFFERENT_PREFERENCE_TYPE;
            }

            pref->inst = inst;
            insert_at_head_of_dll(inst->preferences_generated, pref, inst_next,
                                  inst_prev);
            if (inst->prod->declared_support == DECLARED_O_SUPPORT)
            {
                pref->o_supported = true;
            }
            else if (inst->prod->declared_support == DECLARED_I_SUPPORT)
            {
                pref->o_supported = false;
            }
            else
            {

                pref->o_supported =
                    (thisAgent->FIRING_TYPE == PE_PRODS) ? true : false;
                /* REW: end   09.15.96 */
            }

            /* TEMPORARY HACK (Ideally this should be doable through
             the external kernel interface but for now using a
             couple of global STL lists to get this information
             from the rhs function to this preference adding code)

             Getting the next pref from the set of possible prefs
             added by the deep copy rhs function */
            if (glbDeepCopyWMEs != 0)
            {
                wme* tempwme = glbDeepCopyWMEs;
//                pref = make_preference(thisAgent, a->preference_type,
//                    tempwme->id, tempwme->attr, tempwme->value, NULL, tempwme->preference->o_ids, tempwme->preference->rhs_funcs);
                if (tempwme->id->id->level == 0)
                {
                    tempwme->id->id->level = glbDeepCopyWMELevel;
                }
                if (tempwme->attr->is_identifier() && tempwme->attr->id->level == 0)
                {
                    tempwme->attr->id->level = glbDeepCopyWMELevel;
                }
                if (tempwme->value->is_identifier() && tempwme->value->id->level == 0)
                {
                    tempwme->value->id->level = glbDeepCopyWMELevel;
                }

                pref = make_preference(thisAgent, a->preference_type, tempwme->id, tempwme->attr, tempwme->value, NULL);
                glbDeepCopyWMEs = tempwme->next;
                deallocate_wme(thisAgent, tempwme);
            }
            else
            {
                pref = 0;
            }
        }
        if (a2)
        {
            a2 = a2->next;
        }
    }

    /* --- reset rhs_variable_bindings array to all zeros --- */
    index = 0;
    cell = thisAgent->rhs_variable_bindings;
    while (index++ <= thisAgent->firer_highest_rhs_unboundvar_index)
    {
        *(cell++) = NIL;
    }

    /* --- fill in lots of other stuff --- */
    init_instantiation(thisAgent, inst,
                                    need_to_do_support_calculations, NIL);

    /* --- print trace info: printing preferences --- */
    /* Note: can't move this up, since fill_in_new_instantiation_stuff gives
     the o-support info for the preferences we're about to print */
    if (trace_it && thisAgent->sysparams[TRACE_FIRINGS_PREFERENCES_SYSPARAM])
    {
        for (pref = inst->preferences_generated; pref != NIL;
                pref = pref->inst_next)
        {
            print(thisAgent,  " ");
            print_preference(thisAgent, pref);
        }
    }

    thisAgent->ebChunker->set_learning_for_instantiation(inst);

    /* Copy any context-dependent preferences for conditions of this instantiation */
    build_CDPS(thisAgent, inst);

    thisAgent->production_being_fired = NIL;

    dprint(DT_PRINT_INSTANTIATIONS,  "%fcreate_instantiation for %y created: \n%5", inst->prod_name, inst->top_of_instantiated_conditions, inst->preferences_generated);

    /* --- build chunks/justifications if necessary --- */
    thisAgent->ebChunker->build_chunk_or_justification(inst, &(thisAgent->newly_created_instantiations));

    thisAgent->ebChunker->cleanup_for_instantiation(inst->i_id);
    deallocate_action_list(thisAgent, rhs_vars);

    dprint_header(DT_MILESTONES, PrintAfter, "create_instantiation() for instance of %y (id=%u) finished.\n", inst->prod_name, inst->i_id);

    if (!thisAgent->system_halted)
    {
        /* --- invoke callback function --- */
        soar_invoke_callbacks(thisAgent, FIRING_CALLBACK,
                              static_cast<soar_call_data>(inst));

    }
}

/* -----------------------------------------------------------------------
 Deallocate Instantiation

 This deallocates the given instantiation.  This should only be invoked
 via the possibly_deallocate_instantiation() macro.
 ----------------------------------------------------------------------- */

void deallocate_instantiation(agent* thisAgent, instantiation*& inst)
{
    condition* cond;

    /* mvp 5-17-94 */
    list* c, *c_old;
    preference* pref;
    goal_stack_level level;

#ifdef USE_MEM_POOL_ALLOCATORS
    cond_mpool_list cond_stack = cond_mpool_list(
                                     soar_module::soar_memory_pool_allocator<condition*>(thisAgent));
    inst_mpool_list inst_list = inst_mpool_list(
                                    soar_module::soar_memory_pool_allocator<instantiation*>(thisAgent));
#else
    cond_mpool_list cond_stack;
    inst_mpool_list inst_list;
#endif

    inst_list.push_back(inst);
    inst_mpool_list::iterator next_iter = inst_list.begin();

    while (next_iter != inst_list.end())
    {
        inst = *next_iter;
        assert(inst);
        ++next_iter;

        dprint(DT_DEALLOCATES, "Deallocating instantiation of %y\n", inst->prod_name);

        level = inst->match_goal_level;

        /* The following cleans up some structures used by explanation-based learning.  Note that
         * clean up of  the inst/ovar to identity map moved to end of instantiation and chunk
         * creation functions.  I don't think they're needed after that and could build up when
         * we have a long sequence of persistent instantiations firing. The following function
         * cleans up the identity->rule variable mapping that are only used for debugging in a
         * non-release build. */
        thisAgent->ebChunker->cleanup_for_instantiation_deallocation(inst->i_id);

        for (cond = inst->top_of_instantiated_conditions; cond != NIL; cond =
                    cond->next)
        {
            if (cond->type == POSITIVE_CONDITION)
            {

                if (cond->bt.CDPS)
                {
                    c_old = c = cond->bt.CDPS;
                    cond->bt.CDPS = NIL;
                    for (; c != NIL; c = c->rest)
                    {
                        pref = static_cast<preference*>(c->first);
#ifdef DO_TOP_LEVEL_REF_CTS
                        if (level > TOP_GOAL_LEVEL)
#endif
                        {
                            preference_remove_ref(thisAgent, pref);
                        }
                    }
                    free_list(thisAgent, c_old);
                }

                /*  voigtjr, nlderbin:
                 We flattened out the following recursive loop in order to prevent a stack
                 overflow that happens when the chain of backtrace instantiations is very long:

                 retract_instantiation
                 possibly_deallocate_instantiation
                 loop start:
                 deallocate_instantiation (here)
                 preference_remove_ref
                 possibly_deallocate_preferences_and_clones
                 deallocate_preference
                 possibly_deallocate_instantiation
                 goto loop start
                 */
#ifndef DO_TOP_LEVEL_REF_CTS
                if (level > TOP_GOAL_LEVEL)
#endif
                {
                    wme_remove_ref(thisAgent, cond->bt.wme_);
                    if (cond->bt.trace)
                    {
                        cond->bt.trace->reference_count--;
                        if (cond->bt.trace->reference_count == 0)
                        {
                            preference* clone;

                            if (cond->bt.trace->reference_count)
                            {
                                continue;
                            }
                            bool has_active_clones = false;
                            for (clone = cond->bt.trace->next_clone;
                                    clone != NIL; clone = clone->next_clone)
                            {
                                if (clone->reference_count)
                                {
                                    has_active_clones = true;
                                }
                            }
                            if (has_active_clones)
                            {
                                continue;
                            }
                            for (clone = cond->bt.trace->prev_clone;
                                    clone != NIL; clone = clone->prev_clone)
                            {
                                if (clone->reference_count)
                                {
                                    has_active_clones = true;
                                }
                            }
                            if (has_active_clones)
                            {
                                continue;
                            }

                            // The clones are hopefully a simple case so we just call deallocate_preference on them.
                            // Someone needs to create a test case to push this boundary...
                            {
                                preference* clone = cond->bt.trace->next_clone;
                                preference* next;
                                while (clone)
                                {
                                    next = clone->next_clone;
                                    deallocate_preference(thisAgent, clone);
                                    clone = next;
                                }
                                clone = cond->bt.trace->prev_clone;
                                while (clone)
                                {
                                    next = clone->prev_clone;
                                    deallocate_preference(thisAgent, clone);
                                    clone = next;
                                }
                            }

                            /* --- deallocate pref --- */
                            /* --- remove it from the list of bt.trace's for its match goal --- */
                            if (cond->bt.trace->on_goal_list)
                            {
                                remove_from_dll(
                                    cond->bt.trace->inst->match_goal->id->preferences_from_goal,
                                    cond->bt.trace, all_of_goal_next,
                                    all_of_goal_prev);
                            }

                            /* --- remove it from the list of bt.trace's from that instantiation --- */
                            remove_from_dll(
                                cond->bt.trace->inst->preferences_generated,
                                cond->bt.trace, inst_next, inst_prev);
                            if ((!cond->bt.trace->inst->preferences_generated)
                                    && (!cond->bt.trace->inst->in_ms))
                            {
                                next_iter = inst_list.insert(next_iter,
                                                             cond->bt.trace->inst);
                            }

                            cond_stack.push_back(cond);
                        } // if
                    } // if
                } // if
                /* voigtjr, nlderbin end */
            } // if
        } // for
    } // while

    // free condition symbols and pref
    while (!cond_stack.empty())
    {
        condition* temp = cond_stack.back();
        cond_stack.pop_back();

        /* --- dereference component symbols --- */
        symbol_remove_ref(thisAgent, temp->bt.trace->id);
        symbol_remove_ref(thisAgent, temp->bt.trace->attr);
        symbol_remove_ref(thisAgent, temp->bt.trace->value);
        if (preference_is_binary(temp->bt.trace->type))
        {
            symbol_remove_ref(thisAgent, temp->bt.trace->referent);
        }

        if (temp->bt.trace->wma_o_set)
        {
            wma_remove_pref_o_set(thisAgent, temp->bt.trace);
        }

        /* --- free the memory --- */
        thisAgent->memoryManager->free_with_pool(MP_preference, temp->bt.trace);
    }

    symbol_remove_ref(thisAgent, inst->prod_name);

    // free instantiations in the reverse order
    inst_mpool_list::reverse_iterator riter = inst_list.rbegin();
    while (riter != inst_list.rend())
    {
        instantiation* temp = *riter;
        ++riter;

        deallocate_condition_list(thisAgent,
                                  temp->top_of_instantiated_conditions);
        if (temp->prod)
        {
            production_remove_ref(thisAgent, temp->prod);
        }
        thisAgent->memoryManager->free_with_pool(MP_instantiation, temp);
    }
    inst = NULL;
}

/* -----------------------------------------------------------------------
 Retract Instantiation

 This retracts the given instantiation.
 ----------------------------------------------------------------------- */

void retract_instantiation(agent* thisAgent, instantiation* inst)
{
    preference* pref, *next;
    bool retracted_a_preference;
    bool trace_it;

    /* --- invoke callback function --- */
    soar_invoke_callbacks(thisAgent, RETRACTION_CALLBACK,
                          static_cast<soar_call_data>(inst));

    retracted_a_preference = false;

    trace_it = trace_firings_of_inst(thisAgent, inst);

    /* --- retract any preferences that are in TM and aren't o-supported --- */
    pref = inst->preferences_generated;

    while (pref != NIL)
    {
        next = pref->inst_next;
        if (pref->in_tm && (!pref->o_supported))
        {

            if (trace_it)
            {
                if (!retracted_a_preference)
                {
                    start_fresh_line(thisAgent);
                    print(thisAgent,  "Retracting ");
                    print_instantiation_with_wmes(thisAgent, inst,
                                                  static_cast<wme_trace_type>(thisAgent->sysparams[TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM]),
                                                  1);
                    if (thisAgent->sysparams[TRACE_FIRINGS_PREFERENCES_SYSPARAM])
                    {
                        print(thisAgent,  " -->\n");
                        xml_object(thisAgent, kTagActionSideMarker);
                    }
                }
                if (thisAgent->sysparams[TRACE_FIRINGS_PREFERENCES_SYSPARAM])
                {
                    print(thisAgent,  " ");
                    print_preference(thisAgent, pref);
                }
            }

            remove_preference_from_tm(thisAgent, pref);
            retracted_a_preference = true;
        }
        pref = next;
    }

    /* --- remove inst from list of instantiations of this production --- */
    remove_from_dll(inst->prod->instantiations, inst, next, prev);

    /* --- if retracting a justification, excise it --- */
    /*
     * if the reference_count on the production is 1 (or less) then the
     * only thing supporting this justification is the instantiation, hence
     * it has already been excised, and doing it again is wrong.
     */
    production* prod = inst->prod;
    if ((prod->type == JUSTIFICATION_PRODUCTION_TYPE) &&
        (prod->reference_count > 1))
    {
        excise_production(thisAgent, prod, false);
    }
    else if (prod->type == CHUNK_PRODUCTION_TYPE)
    {
        rl_param_container::apoptosis_choices apoptosis =
            thisAgent->rl_params->apoptosis->get_value();

        // we care about production history of chunks if...
        // - we are dealing with a non-RL rule and all chunks are subject to apoptosis OR
        // - we are dealing with an RL rule that...
        //   - has not been updated by RL AND
        //   - is not in line to be updated by RL
        if (apoptosis != rl_param_container::apoptosis_none)
        {
            if ((!prod->rl_rule
                    && (apoptosis == rl_param_container::apoptosis_chunks))
                    || (prod->rl_rule
                        && (static_cast<int64_t>(prod->rl_update_count) == 0)
                        && (prod->rl_ref_count == 0)))
            {
                thisAgent->rl_prods->reference_object(prod, 1);
            }
        }
    }

    /* --- mark as no longer in MS, and possibly deallocate  --- */
    inst->in_ms = false;
    possibly_deallocate_instantiation(thisAgent, inst);
}

instantiation* make_architectural_instantiation(agent* thisAgent, Symbol* state, wme_set* conditions, symbol_triple_list* actions)
{
    dprint_header(DT_MILESTONES, PrintBoth, "make_fake_instantiation() called.\n");

    // make fake instantiation
    instantiation* inst;
    thisAgent->memoryManager->allocate_with_pool(MP_instantiation, &inst);
    inst->prod = NULL;
    inst->next = inst->prev = NULL;
    inst->rete_token = NULL;
    inst->rete_wme = NULL;
    inst->match_goal = state;
    inst->match_goal_level = state->id->level;
    inst->reliable = true;
    inst->backtrace_number = 0;
    inst->in_ms = false;
    inst->i_id = thisAgent->ebChunker->get_new_inst_id();
    inst->GDS_evaluated_already = false;
    inst->top_of_instantiated_conditions = NULL;
    inst->bottom_of_instantiated_conditions = NULL;
    inst->explain_status = explain_unrecorded;
    inst->explain_depth = 0;
    inst->explain_tc_num = 0;
    inst->prod_name = thisAgent->fake_instantiation_symbol;
    symbol_add_ref(thisAgent, inst->prod_name);

    // create preferences
    inst->preferences_generated = NULL;
    {
        preference* pref;

        for (symbol_triple_list::iterator a_it = actions->begin(); a_it != actions->end(); a_it++)
        {
            pref = make_preference(thisAgent, ACCEPTABLE_PREFERENCE_TYPE, (*a_it)->id, (*a_it)->attr, (*a_it)->value, NIL);
            pref->o_supported = true;
            symbol_add_ref(thisAgent, pref->id);
            symbol_add_ref(thisAgent, pref->attr);
            symbol_add_ref(thisAgent, pref->value);

            pref->inst = inst;
            pref->inst_next = pref->inst_prev = NULL;

            insert_at_head_of_dll(inst->preferences_generated, pref, inst_next, inst_prev);
        }
    }

    // create conditions
    {
        condition* cond = NULL;
        condition* prev_cond = NULL;

        for (wme_set::iterator c_it = conditions->begin(); c_it != conditions->end(); c_it++)
        {
            // construct the condition
            cond = make_condition(thisAgent,
                make_test(thisAgent, (*c_it)->id, EQUALITY_TEST),
                make_test(thisAgent, (*c_it)->attr, EQUALITY_TEST),
                make_test(thisAgent, (*c_it)->value, EQUALITY_TEST));
            cond->prev = prev_cond;
            cond->next = NULL;
            if (prev_cond != NULL)
            {
                prev_cond->next = cond;
            }
            else
            {
                inst->top_of_instantiated_conditions = cond;
                inst->bottom_of_instantiated_conditions = cond;
            }
            cond->test_for_acceptable_preference = (*c_it)->acceptable;
            cond->bt.wme_ = (*c_it);
            cond->inst = inst;

#ifndef DO_TOP_LEVEL_REF_CTS
            if (inst->match_goal_level > TOP_GOAL_LEVEL)
#endif
            {
                wme_add_ref((*c_it));
            }

            cond->bt.level = (*c_it)->id->id->level;
            cond->bt.trace = (*c_it)->preference;

            if (cond->bt.trace)
            {
#ifndef DO_TOP_LEVEL_REF_CTS
                if (inst->match_goal_level > TOP_GOAL_LEVEL)
#endif
                {
                    preference_add_ref(cond->bt.trace);
                }
            }

            cond->bt.CDPS = NULL;
            assert(cond->bt.wme_->preference = cond->bt.trace);
            prev_cond = cond;
        }
    }

    /* Might not be needed yet, but could be if we add identity information to fake instantiation */
    thisAgent->ebChunker->cleanup_for_instantiation(inst->i_id);

    return inst;
}

/* ------------------------------------------------------------------
            Fake Preferences for Goal ^Item Augmentations

   When we backtrace through a (goal ^item) augmentation, we want
   to backtrace to the acceptable preference wme in the supercontext
   corresponding to that ^item.  A slick way to do this automagically
   is to set the backtracing preference pointer on the (goal ^item)
   wme to be a "fake" preference for a "fake" instantiation.  The
   instantiation has as its LHS a list of one condition, which matched
   the acceptable preference wme in the supercontext.

   make_fake_instantiation_for_impasse_item() builds such a fake preference
   and instantiation, given a pointer to the supergoal and the
   acceptable/require preference for the value, and returns a pointer
   to the fake preference.  *** for Soar 8.3, we changed the fake
   preference to be ACCEPTABLE instead of REQUIRE.  This could
   potentially break some code, but it avoids the BUGBUG condition
   that can occur when you have a REQUIRE lower in the stack than an
   ACCEPTABLE but the goal stack gets popped while the WME backtrace
   still points to the REQUIRE, instead of the higher ACCEPTABLE.
   See the section above on Preference Semantics.  It also allows
   the GDS to backtrace through ^items properly.

------------------------------------------------------------------ */

preference* make_architectural_instantiation_for_impasse_item(agent* thisAgent, Symbol* goal, preference* cand)
{
    slot* s;
    wme* ap_wme;
    instantiation* inst;
    preference* pref;
    condition* cond;

    /* --- find the acceptable preference wme we want to backtrace to --- */
    s = cand->slot;
    for (ap_wme = s->acceptable_preference_wmes; ap_wme != NIL; ap_wme = ap_wme->next)
        if (ap_wme->value == cand->value)
        {
            break;
        }
    if (!ap_wme)
    {
        char msg[BUFFER_MSG_SIZE];
        strncpy(msg,
                "decide.c: Internal error: couldn't find acceptable pref wme\n", BUFFER_MSG_SIZE);
        msg[BUFFER_MSG_SIZE - 1] = 0; /* ensure null termination */
        abort_with_fatal_error(thisAgent, msg);
    }

    /* --- make the fake instantiation --- */
    thisAgent->memoryManager->allocate_with_pool(MP_instantiation, &inst);
    inst->i_id = thisAgent->ebChunker->get_new_inst_id();

    /* --- make the fake condition --- */
    cond = make_condition(thisAgent);
    cond->data.tests.id_test = make_test(thisAgent, ap_wme->id, EQUALITY_TEST);
    cond->data.tests.id_test->identity = thisAgent->ebChunker->get_or_create_o_id(thisAgent->ss_context_variable, inst->i_id);
    cond->data.tests.attr_test = make_test(thisAgent, ap_wme->attr, EQUALITY_TEST);
    cond->data.tests.value_test = make_test(thisAgent, ap_wme->value, EQUALITY_TEST);
    cond->data.tests.value_test->identity = thisAgent->ebChunker->get_or_create_o_id(thisAgent->o_context_variable, inst->i_id);

    /* --- make the fake preference --- */
    pref = make_preference(thisAgent, ACCEPTABLE_PREFERENCE_TYPE, goal, thisAgent->item_symbol,
                           cand->value, NIL,
                           identity_triple(
                               thisAgent->ebChunker->get_or_create_o_id(thisAgent->s_context_variable, inst->i_id),
                               0,
                               cond->data.tests.value_test->identity));
    symbol_add_ref(thisAgent, pref->id);
    symbol_add_ref(thisAgent, pref->attr);
    symbol_add_ref(thisAgent, pref->value);
    insert_at_head_of_dll(goal->id->preferences_from_goal, pref,
                          all_of_goal_next, all_of_goal_prev);
    pref->on_goal_list = true;
    preference_add_ref(pref);

    pref->inst = inst;
    pref->inst_next = pref->inst_prev = NIL;

    /* -- Fill in the fake instantiation info -- */
    inst->preferences_generated = pref;
    inst->prod = NIL;
    inst->next = inst->prev = NIL;
    inst->rete_token = NIL;
    inst->rete_wme = NIL;
    inst->match_goal = goal;
    inst->match_goal_level = goal->id->level;
    inst->reliable = true;
    inst->backtrace_number = 0;
    inst->in_ms = false;
    inst->explain_status = explain_unrecorded;
    inst->explain_depth = 0;
    inst->explain_tc_num = 0;
    inst->prod_name = thisAgent->fake_instantiation_symbol;
    symbol_add_ref(thisAgent, inst->prod_name);

    /* -- Fill in fake condition info -- */
    cond->type = POSITIVE_CONDITION;
    inst->top_of_instantiated_conditions = cond;
    inst->bottom_of_instantiated_conditions = cond;

    cond->test_for_acceptable_preference = true;
    cond->bt.wme_ = ap_wme;
    cond->inst = inst;
#ifdef DO_TOP_LEVEL_REF_CTS
    wme_add_ref(ap_wme);
#else
    if (inst->match_goal_level > TOP_GOAL_LEVEL)
    {
        wme_add_ref(ap_wme);
    }
#endif
    cond->bt.level = ap_wme->id->id->level;

    thisAgent->ebChunker->cleanup_for_instantiation_deallocation(inst->i_id);

    /* --- return the fake preference --- */
    return pref;
}
