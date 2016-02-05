/*************************************************************************
 * PLEASE SEE THE FILE "license.txt" (INCLUDED WITH THIS SOFTWARE PACKAGE)
 * FOR LICENSE AND COPYRIGHT INFORMATION.
 *************************************************************************/

/*************************************************************************
 *
 *  file:  soar_module.cpp
 *
 * =======================================================================
 * Description  :  Useful functions for Soar modules
 * =======================================================================
 */

#include "kernel.h"
#include "soar_module.h"

#include "agent.h"
#include "condition.h"
#include "debug.h"
#include "decide.h"
#include "ebc.h"
#include "instantiation.h"
#include "slot.h"
#include "mem.h"
#include "preference.h"
#include "print.h"
#include "soar_TraceNames.h"
#include "test.h"
#include "xml.h"
#include "working_memory_activation.h"
#include "working_memory.h"

namespace soar_module
{
    timer::timer(const char* new_name, agent* new_agent, timer_level new_level, predicate<timer_level>* new_pred, bool soar_control): named_object(new_name), thisAgent(new_agent), level(new_level), pred(new_pred)
    {
        stopwatch.set_enabled(((soar_control) ? (&(new_agent->sysparams[ TIMERS_ENABLED ])) : (NULL)));
        reset();
    }

    /////////////////////////////////////////////////////////////
    // Utility functions
    /////////////////////////////////////////////////////////////

    wme* add_module_wme(agent* thisAgent, Symbol* id, Symbol* attr, Symbol* value)
    {
        slot* my_slot = make_slot(thisAgent, id, attr);
        wme* w = make_wme(thisAgent, id, attr, value, false);

        insert_at_head_of_dll(my_slot->wmes, w, next, prev);
        add_wme_to_wm(thisAgent, w);

        return w;
    }

    void remove_module_wme(agent* thisAgent, wme* w)
    {
        slot* my_slot = find_slot(w->id, w->attr);

        if (my_slot)
        {
            remove_from_dll(my_slot->wmes, w, next, prev);

            if (w->gds)
            {
                if (w->gds->goal != NIL)
                {
                    gds_invalid_so_remove_goal(thisAgent, w);

                    /* NOTE: the call to remove_wme_from_wm will take care of checking if GDS should be removed */
                }
            }

            remove_wme_from_wm(thisAgent, w);
        }
    }

    instantiation* make_fake_instantiation(agent* thisAgent, Symbol* state, wme_set* conditions, symbol_triple_list* actions)
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
                thisAgent->memoryManager->allocate_with_pool(MP_condition, &cond);
                init_condition(cond);
                cond->type = POSITIVE_CONDITION;
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
                cond->data.tests.id_test = make_test(thisAgent, (*c_it)->id, EQUALITY_TEST);
                cond->data.tests.attr_test = make_test(thisAgent, (*c_it)->attr, EQUALITY_TEST);
                cond->data.tests.value_test = make_test(thisAgent, (*c_it)->value, EQUALITY_TEST);

                cond->test_for_acceptable_preference = (*c_it)->acceptable;
                cond->bt.wme_ = (*c_it);

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

        return inst;
    }


}
