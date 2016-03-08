/*************************************************************************
 *
 *  file:  production.cpp
 *
 * ====================================================================
 *                    Production Utilities
 *
 * This file contains various utility routines for manipulating
 * Init_production_utilities() should be called before anything else here.
 * =======================================================================
 */

#include "production.h"

#include "agent.h"
#include "condition.h"
#include "debug.h"
#include "ebc.h"
#include "explain.h"
#include "instantiation.h"
#include "mem.h"
#include "print.h"
#include "production_reorder.h"
#include "rete.h"
#include "reinforcement_learning.h"
#include "rhs.h"
#include "symbol.h"
#include "test.h"

#include <ctype.h>
#include <run_soar.h>
#include <stdlib.h>

void init_production_utilities(agent* thisAgent)
{
    thisAgent->memoryManager->init_memory_pool(MP_test, sizeof(test_info), "test");
    thisAgent->memoryManager->init_memory_pool(MP_condition, sizeof(condition), "condition");
    thisAgent->memoryManager->init_memory_pool(MP_production, sizeof(production), "production");
    thisAgent->memoryManager->init_memory_pool(MP_action, sizeof(action), "action");
    thisAgent->memoryManager->init_memory_pool(MP_rhs_symbol, sizeof(rhs_info), "rhs symbol");
    init_reorderer(thisAgent);
}

/* *********************************************************************

                    Transitive Closure Utilities

********************************************************************* */

/* =====================================================================

              Increment TC Counter and Return New TC Number

   Get_new_tc_number() is called from lots of places.  Any time we need
   to mark a set of identifiers and/or variables, we get a new tc_number
   by calling this routine, then proceed to mark various ids or vars
   by setting the sym->tc_num or sym->tc_num fields.

   A global tc number counter is maintained and incremented by this
   routine in order to generate a different tc_number each time.  If
   the counter ever wraps around back to 0, we bump it up to 1 and
   reset the the tc_num fields on all existing identifiers and variables
   to 0.
===================================================================== */

tc_number get_new_tc_number(agent* thisAgent)
{
    /* This was originally a global variable. For the present I'll move it here,
       but it probably belongs in kernel_struct. */

    thisAgent->current_tc_number++;
    if (thisAgent->current_tc_number == 0)
    {
        reset_id_and_variable_tc_numbers(thisAgent);
        thisAgent->current_tc_number = 1;
    }
    return thisAgent->current_tc_number;
}

/* =====================================================================

               Marking, Unmarking, and Collecting Symbols

   Sometimes in addition to marking symbols using their tc_num fields,
   we also want to build up a list of the symbols we've marked.  So,
   many routines in this file take an "id_list" or "var_list" argument.
   This argument should be NIL if no such list is desired.  If non-NIL,
   it should point to the header of the linked list being built.

   Unmark_identifiers_and_free_list() unmarks all the id's in the given
   list, and deallocates the list.  Unmark_variables_and_free_list()
   is similar, only the list should be a list of variables rather than
   identifiers.

   Symbol_is_constant_or_marked_variable() tests whether the given symbol
   is either a constant (non-variable) or a variable marked with the
   given tc number.
===================================================================== */

void unmark_identifiers_and_free_list(agent* thisAgent, list* id_list)
{
    cons* next;
    Symbol* sym;

    while (id_list)
    {
        sym = static_cast<symbol_struct*>(id_list->first);
        next = id_list->rest;
        free_cons(thisAgent, id_list);
        sym->tc_num = 0;
        id_list = next;
    }
}

void unmark_variables_and_free_list(agent* thisAgent, list* var_list)
{
    cons* next;
    Symbol* sym;

    while (var_list)
    {
        sym = static_cast<symbol_struct*>(var_list->first);
        next = var_list->rest;
        free_cons(thisAgent, var_list);
        sym->tc_num = 0;
        var_list = next;
    }
}

/* =====================================================================

   Finding the variables bound in tests, conditions, and condition lists

   These routines collect the variables that are bound in equality tests.
   Their "var_list" arguments should either be NIL or else should point
   to the header of the list of marked variables being constructed.

   The add_LTI parameter is available so that when Soar is marking symbols for
   action ordering based on whether the levels of the symbols would be known,
   it also consider whether the LTIs level can be determined by being linked
   to a LHS element or a RHS action that has already been executed.

===================================================================== */

void add_bound_variables_in_condition(agent* thisAgent, condition* c, tc_number tc,
                                      list** var_list, bool add_LTIs)
{
    if (c->type != POSITIVE_CONDITION)
    {
        return;
    }
    add_bound_variables_in_test(thisAgent, c->data.tests.id_test, tc, var_list, add_LTIs);
    add_bound_variables_in_test(thisAgent, c->data.tests.attr_test, tc, var_list, add_LTIs);
    add_bound_variables_in_test(thisAgent, c->data.tests.value_test, tc, var_list, add_LTIs);
}

void add_bound_variables_in_condition_list(agent* thisAgent, condition* cond_list,
        tc_number tc, list** var_list, bool add_LTIs = false)
{
    condition* c;

    for (c = cond_list; c != NIL; c = c->next)
    {
        add_bound_variables_in_condition(thisAgent, c, tc, var_list, add_LTIs);
    }
}

void add_all_variables_in_condition_list(agent* thisAgent, condition* cond_list,
        tc_number tc, list** var_list);

void add_all_variables_in_condition(agent* thisAgent,
                                    condition* c, tc_number tc,
                                    list** var_list)
{
    if (c->type == CONJUNCTIVE_NEGATION_CONDITION)
    {
        add_all_variables_in_condition_list(thisAgent, c->data.ncc.top, tc, var_list);
    }
    else
    {
        add_all_variables_in_test(thisAgent, c->data.tests.id_test, tc, var_list);
        add_all_variables_in_test(thisAgent, c->data.tests.attr_test, tc, var_list);
        add_all_variables_in_test(thisAgent, c->data.tests.value_test, tc, var_list);
    }
}

void add_all_variables_in_condition_list(agent* thisAgent, condition* cond_list,
        tc_number tc, list** var_list)
{
    condition* c;

    for (c = cond_list; c != NIL; c = c->next)
    {
        add_all_variables_in_condition(thisAgent, c, tc, var_list);
    }
}

/* ====================================================================

              Transitive Closure for Conditions and Actions

   These routines do transitive closure calculations for tests,
   conditions, actions, etc.

   Usage:
     1. Set my_tc = get_new_tc_number() to start a new TC
     2. (optional) If you want linked lists of symbols in the TC, initialize
        id_list=NIL and var_list=NIL.
        If you're not using id_list and/or var_list, give NIL for "&id_list"
        and/or "&var_list" in the function calls below.
     3. (optional) setup any id's or var's that you want to include in the
        initial TC, by calling
           add_symbol_to_tc (sym, my_tc, &id_list, &var_list)
        (If not using id_list or var_list, you can just mark
         sym->{id,var}.tc_num = my_tc instead.)
     4. To do the work you want, use any of the following any number of times:
            add_cond_to_tc (cond, my_tc, &id_list, &var_list);
            add_action_to_tc (cond, my_tc, &id_list, &var_list);
            result = cond_is_in_tc (cond, my_tc);
            result = action_is_in_tc (action, my_tc);
     5. When finished, free the cons cells in id_list and var_list (but
        don't call symbol_remove_ref() on the symbols in them).

  Warning:  actions must not contain reteloc's or rhs unbound variables here.
==================================================================== */

void add_symbol_to_tc(agent* thisAgent, Symbol* sym, tc_number tc,
                      list** id_list, list** var_list)
{
    if ((sym->symbol_type == VARIABLE_SYMBOL_TYPE) || (sym->symbol_type == IDENTIFIER_SYMBOL_TYPE))
    {
        sym->mark_if_unmarked(thisAgent, tc, id_list);
    }
}

void add_test_to_tc(agent* thisAgent, test t, tc_number tc,
                    list** id_list, list** var_list)
{
    cons* c;

    if (!t)
    {
        return;
    }

    if (t->type == EQUALITY_TEST)
    {
        add_symbol_to_tc(thisAgent, t->data.referent, tc, id_list, var_list);
        return;
    }
    else if (t->type == CONJUNCTIVE_TEST)
    {
        for (c = t->data.conjunct_list; c != NIL; c = c->rest)
        {
            add_test_to_tc(thisAgent, static_cast<test>(c->first), tc, id_list, var_list);
        }
    }
}

void add_cond_to_tc(agent* thisAgent, condition* c, tc_number tc,
                    list** id_list, list** var_list)
{
    if (c->type == POSITIVE_CONDITION)
    {
        add_test_to_tc(thisAgent, c->data.tests.id_test, tc, id_list, var_list);
        add_test_to_tc(thisAgent, c->data.tests.value_test, tc, id_list, var_list);
    }
}

void add_action_to_tc(agent* thisAgent, action* a, tc_number tc,
                      list** id_list, list** var_list)
{
    if (a->type != MAKE_ACTION)
    {
        return;
    }
    add_symbol_to_tc(thisAgent, rhs_value_to_symbol(a->id), tc, id_list, var_list);
    if (rhs_value_is_symbol(a->value))
    {
        add_symbol_to_tc(thisAgent, rhs_value_to_symbol(a->value), tc, id_list, var_list);
    }
    if (preference_is_binary(a->preference_type))
        if (rhs_value_is_symbol(a->referent))
        {
            add_symbol_to_tc(thisAgent, rhs_value_to_symbol(a->referent), tc, id_list, var_list);
        }
}

bool test_is_in_tc(test t, tc_number tc)
{
    cons* c;

    if (!t)
    {
        return false;
    }
    if (t->type == EQUALITY_TEST)
    {
        return t->data.referent->is_in_tc(tc);
    }
    else if (t->type == CONJUNCTIVE_TEST)
    {
        for (c = t->data.conjunct_list; c != NIL; c = c->rest)
            if (test_is_in_tc(static_cast<test>(c->first), tc))
            {
                return true;
            }
        return false;
    }
    return false;
}

bool cond_is_in_tc(agent* thisAgent, condition* cond, tc_number tc)
{
    condition* c;
    bool anything_changed;
    bool result;
    list* new_ids, *new_vars;

    if (cond->type != CONJUNCTIVE_NEGATION_CONDITION)
    {
        return test_is_in_tc(cond->data.tests.id_test, tc);
    }

    /* --- conjunctive negations:  keep trying to add stuff to the TC --- */
    new_ids = NIL;
    new_vars = NIL;
    for (c = cond->data.ncc.top; c != NIL; c = c->next)
    {
        c->already_in_tc = false;
    }
    while (true)
    {
        anything_changed = false;
        for (c = cond->data.ncc.top; c != NIL; c = c->next)
            if (! c->already_in_tc)
                if (cond_is_in_tc(thisAgent, c, tc))
                {
                    add_cond_to_tc(thisAgent, c, tc, &new_ids, &new_vars);
                    c->already_in_tc = true;
                    anything_changed = true;
                }
        if (! anything_changed)
        {
            break;
        }
    }

    /* --- complete TC found, look for anything that didn't get hit --- */
    result = true;
    for (c = cond->data.ncc.top; c != NIL; c = c->next)
        if (! c->already_in_tc)
        {
            result = false;
        }

    /* --- unmark identifiers and variables that we just marked --- */
    unmark_identifiers_and_free_list(thisAgent, new_ids);
    unmark_variables_and_free_list(thisAgent, new_vars);

    return result;
}

bool action_is_in_tc(action* a, tc_number tc)
{
    if (a->type != MAKE_ACTION)
    {
        return false;
    }
    return rhs_value_to_symbol(a->id)->is_in_tc(tc);
}

/* *********************************************************************

                         Variable Generator

   These routines are used for generating new variables.  The variables
   aren't necessarily "completely" new--they might occur in some existing
   production.  But we usually need to make sure the new variables don't
   overlap with those already used in a *certain* production--for instance,
   when variablizing a chunk, we don't want to introduce a new variable that
   conincides with the name of a variable already in an NCC in the chunk.

   To use these routines, first call reset_variable_generator(), giving
   it lists of conditions and actions whose variables should not be
   used.  Then call generate_new_variable() any number of times; each
   time, you give it a string to use as the prefix for the new variable's
   name.  The prefix string should not include the opening "<".
********************************************************************* */


void reset_variable_generator(agent* thisAgent,
                              condition* conds_with_vars_to_avoid,
                              action* actions_with_vars_to_avoid)
{
    tc_number tc;
    list* var_list;
    cons* c;
    int i;

    /* --- reset counts, and increment the gensym number --- */
    for (i = 0; i < 26; i++)
    {
        thisAgent->gensymed_variable_count[i] = 1;
    }
    thisAgent->current_variable_gensym_number++;
    if (thisAgent->current_variable_gensym_number == 0)
    {
        reset_variable_gensym_numbers(thisAgent);
        thisAgent->current_variable_gensym_number = 1;
    }

    /* --- mark all variables in the given conds and actions --- */
    tc = get_new_tc_number(thisAgent);
    var_list = NIL;
    add_all_variables_in_condition_list(thisAgent, conds_with_vars_to_avoid, tc, &var_list);
    add_all_variables_in_action_list(thisAgent, actions_with_vars_to_avoid, tc, &var_list);
    for (c = var_list; c != NIL; c = c->rest)
    {
        static_cast<Symbol*>(c->first)->var->gensym_number = thisAgent->current_variable_gensym_number;
    }
    free_list(thisAgent, var_list);
}

Symbol* generate_new_variable(agent* thisAgent, const char* prefix)
{
#define GENERATE_NEW_VARIABLE_BUFFER_SIZE 200 /* that ought to be long enough! */
    char name[GENERATE_NEW_VARIABLE_BUFFER_SIZE];
    Symbol* New;
    char first_letter;

    first_letter = *prefix;
    if (isalpha(first_letter))
    {
        if (isupper(first_letter))
        {
            first_letter = static_cast<char>(tolower(first_letter));
        }
    }
    else
    {
        first_letter = 'v';
    }

    while (true)
    {
        SNPRINTF(name, GENERATE_NEW_VARIABLE_BUFFER_SIZE, "<%s%lu>", prefix,
                 static_cast<long unsigned int>(thisAgent->gensymed_variable_count[first_letter - 'a']++));
        name[GENERATE_NEW_VARIABLE_BUFFER_SIZE - 1] = 0; /* ensure null termination */

        New = make_variable(thisAgent, name);
        if (New->var->gensym_number != thisAgent->current_variable_gensym_number)
        {
            break;
        }
        /* -- A variable with that name already existed.  make_variable just returned it and
         *    incremented its refcount, so reverse that refcount addition and try again. -- */
        symbol_remove_ref(thisAgent, New);
    }

    New->var->current_binding_value = NIL;
    New->var->gensym_number = thisAgent->current_variable_gensym_number;
    return New;
}

/* *********************************************************************

                         Production Management

    Make_production() does reordering, compile-time o-support calc's,
    and builds and returns a production structure for a new production.
    It does not enter the production into the Rete net, however.
    The "type" argument should be one of USER_PRODUCTION_TYPE, etc.
    The flag "reorder_nccs" tells whether to recursively reorder
    the subconditions of NCC's--this is not necessary for newly
    built chunks, as their NCC's are copies of other NCC's in SP's that
    have already been reordered.  If any error occurs, make_production()
    returns NIL.

    Deallocate_production() and excise_production() do just what they
    say.  Normally deallocate_production() should be invoked only via
    the production_remove_ref() macro.
********************************************************************* */

/* Before calling make_production, caller is responsible for using this
 * function to make sure the production is valid.  This was separated out
 * so EBC can first try to fix unconnected conditions before creating
 * the production. */

EBCFailureType reorder_and_validate_lhs_and_rhs(agent*        thisAgent,
                                                condition**   lhs_top,
                                                action**      rhs_top,
                                                bool          reorder_nccs,
                                                bool          collect_ungroundeds,
                                                symbol_list*  ungrounded_syms)
{
    tc_number tc;

    reset_variable_generator(thisAgent, *lhs_top, *rhs_top);
    tc = get_new_tc_number(thisAgent);
    add_bound_variables_in_condition_list(thisAgent, *lhs_top, tc, NIL, true);

    if (! reorder_action_list(thisAgent, rhs_top, tc))
    {
        return ebc_failed_reordering_rhs;
    }
    if (! reorder_lhs(thisAgent, lhs_top, reorder_nccs, collect_ungroundeds, ungrounded_syms))
    {
        return ebc_failed_unconnected_conditions;
    }

    return ebc_success;
}

production* make_production(agent*          thisAgent,
                            ProductionType  type,
                            Symbol*         name,
                            char*           original_rule_name,
                            condition**     lhs_top,
                            action**        rhs_top,
                            bool            reorder_nccs,
                            preference*     results)
{
    production* p;
    action* a;

    if (type != JUSTIFICATION_PRODUCTION_TYPE)
    {
#ifdef DO_COMPILE_TIME_O_SUPPORT_CALCS
        calculate_compile_time_o_support(*lhs_top, *rhs_top);
#ifdef DEBUG_CT_OSUPPORT
        for (a = *rhs_top; a != NIL; a = a->next)
            if ((a->type == MAKE_ACTION) && (a->support == UNKNOWN_SUPPORT))
            {
                break;
            }
        if (a)
        {
            print_with_symbols(thisAgent, "\nCan't classify %y\n", name);
        }
#endif
#else
        for (a = *rhs_top; a != NIL; a = a->next)
        {
            a->support = UNKNOWN_SUPPORT;
        }
#endif
    }
    else
    {
        /* --- for justifications --- */
        /* force run-time o-support (it'll only be done once) */
        for (a = *rhs_top; a != NIL; a = a->next)
        {
            a->support = UNKNOWN_SUPPORT;
        }
    }

    thisAgent->memoryManager->allocate_with_pool(MP_production, &p);
    p->name = name;
    p->original_rule_name = make_memory_block_for_string(thisAgent, original_rule_name);

    if (name->sc->production)
    {
        print(thisAgent,  "Internal error: make_production called with name %s\n",
              thisAgent->name_of_production_being_reordered);
        print(thisAgent,  "for which a production already exists\n");
    }
    name->sc->production = p;
    p->documentation = NIL;
    p->filename = NIL;
    p->firing_count = 0;
    p->reference_count = 1;
    insert_at_head_of_dll(thisAgent->all_productions_of_type[type], p, next, prev);
    thisAgent->num_productions_of_type[type]++;
    p->type = type;
    p->declared_support = UNDECLARED_SUPPORT;
    p->trace_firings = false;
    p->p_node = NIL;               /* it's not in the Rete yet */
    p->action_list = *rhs_top;
    p->rhs_unbound_variables = NIL; /* the Rete fills this in */
    p->instantiations = NIL;
    p->interrupt = false;
    p->explain_its_chunks = false;
    p->save_for_justification_explanation = false;

    // Soar-RL stuff
    p->rl_update_count = 0.0;
    p->rl_delta_bar_delta_beta = -3.0;
    p->rl_delta_bar_delta_h = 0.0;
    p->rl_rule = false;
    p->rl_update_count = 0;
    p->rl_ref_count = 0;
    p->rl_ecr = 0.0;
    p->rl_efr = 0.0;
    p->rl_gql = 0.0;
    if ((type != JUSTIFICATION_PRODUCTION_TYPE) && (type != TEMPLATE_PRODUCTION_TYPE))
    {
        p->rl_rule = rl_valid_rule(p);
        if (p->rl_rule)
        {
            p->rl_efr = get_number_from_symbol(rhs_value_to_symbol(p->action_list->referent));
        }
    }
    p->rl_template_conds = NIL;
    p->rl_template_instantiations = NIL;

    rl_update_template_tracking(thisAgent, name->sc->name);

    return p;
}

void deallocate_production(agent* thisAgent, production* prod)
{
    if (prod->instantiations)
    {
        char msg[BUFFER_MSG_SIZE];
        strncpy(msg, "Internal error: deallocating prod. that still has inst's\n", BUFFER_MSG_SIZE);
        msg[BUFFER_MSG_SIZE - 1] = 0; /* ensure null termination */
        abort_with_fatal_error(thisAgent, msg);
    }
    deallocate_action_list(thisAgent, prod->action_list);
    /* RBD 3/28/95 the following line used to use free_list(), leaked memory */
    deallocate_symbol_list_removing_references(thisAgent, prod->rhs_unbound_variables);
    symbol_remove_ref(thisAgent, prod->name);
    free_memory_block_for_string(thisAgent, prod->original_rule_name);
    if (prod->documentation)
    {
        free_memory_block_for_string(thisAgent, prod->documentation);
    }
    /* next line, kjh CUSP(B11) */
    if (prod->filename)
    {
        free_memory_block_for_string(thisAgent, prod->filename);
    }

    if (prod->rl_template_conds)
    {
        deallocate_condition_list(thisAgent, prod->rl_template_conds);
    }
    if (prod->rl_template_instantiations)
    {
        delete prod->rl_template_instantiations;
    }

    thisAgent->memoryManager->free_with_pool(MP_production, prod);
}

void excise_production(agent* thisAgent, production* prod, bool print_sharp_sign)
{
    dprint_header(DT_DEALLOCATES, PrintBoth, "Excising production %y.\n", prod->name);
#ifdef BUILD_WITH_EXPLAINER
    if (prod->save_for_justification_explanation && thisAgent->explanationLogger)
    {
        thisAgent->explanationLogger->save_excised_production(prod);
    }
#endif
    if (prod->trace_firings)
    {
        remove_pwatch(thisAgent, prod);
    }
    remove_from_dll(thisAgent->all_productions_of_type[prod->type], prod, next, prev);

    // Remove reference from apoptosis object store
    if ((prod->type == CHUNK_PRODUCTION_TYPE) && (thisAgent->rl_params) && (thisAgent->rl_params->apoptosis->get_value() != rl_param_container::apoptosis_none))
    {
        thisAgent->rl_prods->remove_object(prod);
    }

    // Remove RL-related pointers to this production
    if (prod->rl_rule)
    {
        rl_remove_refs_for_prod(thisAgent, prod);
    }

    thisAgent->num_productions_of_type[prod->type]--;
    if (print_sharp_sign)
    {
        print(thisAgent,  "#");
    }
    if (prod->p_node)
    {
        excise_production_from_rete(thisAgent, prod);
    }
    prod->name->sc->production = NIL;
    production_remove_ref(thisAgent, prod);
    dprint_header(DT_DEALLOCATES, PrintAfter, "");
}

void excise_all_productions_of_type(agent* thisAgent,
                                    byte type,
                                    bool print_sharp_sign)
{

    // Iterating through the productions of the appropriate type and excising them
    while (thisAgent->all_productions_of_type[type])
    {
        excise_production(thisAgent,
                          thisAgent->all_productions_of_type[type],
                          print_sharp_sign && thisAgent->sysparams[TRACE_LOADING_SYSPARAM]);
    }
}

void excise_all_productions(agent* thisAgent,
                            bool print_sharp_sign)
{

    // Excise all the productions of the four different types
    for (int i = 0; i < NUM_PRODUCTION_TYPES; i++)
    {
        excise_all_productions_of_type(thisAgent,
                                       static_cast<byte>(i),
                                       print_sharp_sign && thisAgent->sysparams[TRACE_LOADING_SYSPARAM]);
    }
}

/****************************/
/* ----------------------------------------------------------------
 This returns a boolean that indicates that one condition is
 greater than another in some ordering of the conditions. The ordering
 is dependent upon the hash-value of each of the tests in the
 condition.
------------------------------------------------------------------*/

#define NON_EQUAL_TEST_RETURN_VAL 0  /* some unusual number */

uint32_t canonical_test(test t)
{
    Symbol* sym;

    if (!t)
    {
        return NON_EQUAL_TEST_RETURN_VAL;
    }

    if (t->type == EQUALITY_TEST)
    {
        sym = t->data.referent;
        if (sym->symbol_type == STR_CONSTANT_SYMBOL_TYPE ||
                sym->symbol_type == INT_CONSTANT_SYMBOL_TYPE ||
                sym->symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
        {
            return sym->hash_id;
        }
        else
        {
            return NON_EQUAL_TEST_RETURN_VAL;
        }
    }
    return NON_EQUAL_TEST_RETURN_VAL;
}

#define CANONICAL_TEST_ORDER canonical_test

/*
#define CANONICAL_TEST_ORDER hash_test
*/

bool canonical_cond_greater(condition* c1, condition* c2)
/*

 Original:  676,362 total rete nodes (1 dummy + 560045 positive + 4
            unhashed positive + 2374 negative + 113938 p_nodes)

The following notation describes the order of tests and the relation
of the hash_test that was used. IAV< means test the (I)d field first
then the (A)ttribute field, then the (V)alue field. and use less than
as the ordering constraint. The actual ordering constraint should not
make any difference.

 IAV<:  737,605 total rete nodes (1 dummy + 617394 positive + 3
            unhashed positive + 6269 negative + 113938 p_nodes)

Realized that the identifier will always be a variable and thus
shouldn't be part of the ordering.

Changed to put all negative tests in front of cost 1 tests list.
   That is always break ties of cost 1 with a negative test if
   it exists.

Changed so that canonical_test_order returns a negative -1 when
comparing anything but constants. Also fixed a bug.

Consistency checks:

 Original:  676,362 total rete nodes (1 dummy + 560045 positive + 4
            unhashed positive + 2374 negative + 113938 p_nodes)
    Still holds with 1 optimization in and always returning False

 Remove 1:  720,126 total rete nodes (1 dummy + 605760 positive + 4
            unhashed positive +  423 negative + 113938 p_nodes)
    Always returning False causes the first item in the tie list to
       be picked.

 Surprise:  637,482 total rete nodes (1 dummy + 523251 positive + 3
            unhashed positive +  289 negative + 113938 p_nodes)
    Without 1 optimization and always returning True. Causes the
      last item in 1-tie list to be picked.

 In the following tests ht means hash test provided the canonical
 value. ct means that the routine constant test provided the canonical
 value. ct provides a value for non constant equality tests. I tried
 both 0 and a big number (B)  with no difference noted.

  ht_AV<:   714,427 total rete nodes (1 dummy + 600197 positive + 2
            unhashed positive +  289 negative + 113938 p_nodes)

  ht_AV>:   709,637 total rete nodes (1 dummy + 595305 positive + 3
            unhashed positive +  390 negative + 113938 p_nodes)

  ct0_AV>:  709,960 total rete nodes (1 dummy + 595628 positive + 3
            unhashed positive +  390 negative + 113938 p_nodes)

  ct0_AV<:  714,162 total rete nodes (1 dummy + 599932 positive + 2
            unhashed positive +  289 negative + 113938 p_nodes)

  ctB_AV>:  709,960 total rete nodes (1 dummy + 595628 positive + 3
            unhashed positive +  390 negative + 113938 p_nodes)

  ctB_AV<:  714,162 total rete nodes (1 dummy + 599932 positive + 2
            unhashed positive +  289 negative + 113938 p_nodes)

  ctB_VA>:  691,193 total rete nodes (1 dummy + 576861 positive + 3
            unhashed positive +  390 negative + 113938 p_nodes)

  ctB_VA<:  704,539 total rete nodes (1 dummy + 590309 positive + 2
            unhashed positive +  289 negative + 113938 p_nodes)

  ct0_VA<:  744,604 total rete nodes (1 dummy + 630374 positive + 2
            unhashed positive +  289 negative + 113938 p_nodes)

  ct0_VA>:  672,367 total rete nodes (1 dummy + 558035 positive + 3
            unhashed positive +  390 negative + 113938 p_nodes)

   ht_VA>:  727,742 total rete nodes (1 dummy + 613517 positive + 3
            unhashed positive +  283 negative + 113938 p_nodes)

   ht_VA<:  582,559 total rete nodes (1 dummy + 468328 positive + 3
            unhashed positive +  289 negative + 113938 p_nodes)

Changed  < to > 10/5/92*/
{
    uint32_t test_order_1, test_order_2;

    if ((test_order_1 = CANONICAL_TEST_ORDER(c1->data.tests.attr_test)) <
            (test_order_2 = CANONICAL_TEST_ORDER(c2->data.tests.attr_test)))
    {
        return true;
    }
    else if (test_order_1 == test_order_2 &&
             CANONICAL_TEST_ORDER(c1->data.tests.value_test) <
             CANONICAL_TEST_ORDER(c2->data.tests.value_test))
    {
        return true;
    }
    return false;
}
