#include "explain.h"
#include "agent.h"
#include "condition.h"
#include "debug.h"
#include "instantiation.h"
#include "preference.h"
#include "production.h"
#include "rhs.h"
#include "symbol.h"
#include "test.h"
#include "output_manager.h"
#include "working_memory.h"

void condition_record::connect_to_action()
{
    if (parent_instantiation && cached_pref)
    {
        assert(cached_pref);
        parent_action = parent_instantiation->find_rhs_action(cached_pref);
        assert(parent_action);
        dprint(DT_EXPLAIN_CONNECT, "   Linked condition %u (%t ^%t %t) to a%u in i%u.\n", conditionID, condition_tests.id, condition_tests.attr, condition_tests.value, parent_action->get_actionID(), parent_instantiation->get_instantiationID());
    } else {
        dprint(DT_EXPLAIN, "   Did not link condition %u (%t ^%t %t) because no parent instantiation.\n", conditionID, condition_tests.id, condition_tests.attr, condition_tests.value);
    }
//    cached_pref = NULL;
}

void condition_record::update_condition(condition* pCond, instantiation_record* pInst)
{
    //dprint(DT_EXPLAIN_UPDATE, "   Updating condition c%u for %l.\n", conditionID, pCond);
    if (!matched_wme)
    {
        set_matched_wme_for_cond(pCond);
    }
    cached_pref = pCond->bt.trace;
    cached_wme = pCond->bt.wme_;
    if (pCond->bt.trace)
    {
        parent_instantiation = thisAgent->explanationLogger->get_instantiation(pCond->bt.trace->inst);
    } else {
        parent_instantiation = NULL;
    }
    parent_action = NULL;
    if (path_to_base) {
        delete path_to_base;
    }
    path_to_base = NULL;
}

void condition_record::set_matched_wme_for_cond(condition* pCond)
{
    /* bt info wme doesn't seem to always exist (maybe just for terminal nodes), so
     * we use actual tests if we know it's a literal condition because identifier is STI */
    if (condition_tests.id->eq_test->data.referent->is_identifier() &&
        !condition_tests.attr->eq_test->data.referent->is_variable() &&
        !condition_tests.attr->eq_test->data.referent->is_variable())
    {
        matched_wme = new symbol_triple(condition_tests.id->eq_test->data.referent, condition_tests.attr->eq_test->data.referent, condition_tests.value->eq_test->data.referent);
        symbol_add_ref(thisAgent, matched_wme->id);
        symbol_add_ref(thisAgent, matched_wme->attr);
        symbol_add_ref(thisAgent, matched_wme->value);
    } else {
        if (pCond->bt.wme_)
        {
            matched_wme = new symbol_triple(pCond->bt.wme_->id, pCond->bt.wme_->attr, pCond->bt.wme_->value);
            symbol_add_ref(thisAgent, matched_wme->id);
            symbol_add_ref(thisAgent, matched_wme->attr);
            symbol_add_ref(thisAgent, matched_wme->value);
        } else {
            matched_wme = NULL;
        }
    }
}

condition_record::condition_record(agent* myAgent, condition* pCond, uint64_t pCondID)
{
    thisAgent = myAgent;
    conditionID = pCondID;
    type = pCond->type;
    parent_action = NULL;
    path_to_base = NULL;
    my_instantiation = NULL;

    dprint(DT_EXPLAIN_CONDS, "   Creating condition %u for %l.\n", conditionID, pCond);

    condition_tests.id = copy_test(thisAgent, pCond->data.tests.id_test);
    condition_tests.attr = copy_test(thisAgent, pCond->data.tests.attr_test);
    condition_tests.value = copy_test(thisAgent, pCond->data.tests.value_test);

    set_matched_wme_for_cond(pCond);

    if (pCond->bt.level)
    {
        wme_level_at_firing = pCond->bt.level;
    } else if (condition_tests.id->eq_test->data.referent->is_identifier())
    {
        assert (condition_tests.id->eq_test->data.referent->id->level);
        wme_level_at_firing = condition_tests.id->eq_test->data.referent->id->level;
        dprint(DT_EXPLAIN_CONDS, "   No backtrace level found.  Setting condition level to id's current level.\n", wme_level_at_firing);
    } else {
        wme_level_at_firing = 0;
        dprint(DT_EXPLAIN_CONDS, "   No backtrace level or sti identifier found.  Setting condition level to 0.\n", wme_level_at_firing);
    }

    /* Cache the pref to make it easier to connect this condition to the action that created
     * the preference later. Tricky because NCs and NCCs have neither and architectural
     * may have neither */
    cached_pref = pCond->bt.trace;
    cached_wme = pCond->bt.wme_;
    if (pCond->bt.trace)
    {
        parent_instantiation = thisAgent->explanationLogger->get_instantiation(pCond->bt.trace->inst);
    } else {
        parent_instantiation = NULL;
    }
    dprint(DT_EXPLAIN_CONDS, "   Done creating condition %u.\n", conditionID);
}

condition_record::~condition_record()
{
    dprint(DT_EXPLAIN_CONDS, "   Deleting condition record c%u for: (%t ^%t %t)\n", conditionID, condition_tests.id, condition_tests.attr, condition_tests.value);

    deallocate_test(thisAgent, condition_tests.id);
    deallocate_test(thisAgent, condition_tests.attr);
    deallocate_test(thisAgent, condition_tests.value);
    if (matched_wme)
    {
        dprint(DT_EXPLAIN_CONDS, "   Removing references for matched wme: (%y ^%y %y)\n", matched_wme->id, matched_wme->attr, matched_wme->value);
        symbol_remove_ref(thisAgent, matched_wme->id);
        symbol_remove_ref(thisAgent, matched_wme->attr);
        symbol_remove_ref(thisAgent, matched_wme->value);
        delete matched_wme;
    }
    if (path_to_base)
    {
        delete path_to_base;
    }
    dprint(DT_EXPLAIN_CONDS, "   Done deleting condition record c%u\n", conditionID);
}

bool test_contains_identity_in_set(agent* thisAgent, test t, const id_set* pIDSet)
{
    cons* c;

    switch (t->type)
    {
        case EQUALITY_TEST:
            if (t->identity)
            {
                id_set::const_iterator it;
                it = pIDSet->find(t->identity);
                if (it != pIDSet->end())
                {
                    return true;
                }
            }

            return false;
            break;
        case CONJUNCTIVE_TEST:
            for (c = t->data.conjunct_list; c != NIL; c = c->rest)
            {
                if (test_contains_identity_in_set(thisAgent, static_cast<test>(c->first), pIDSet))
                {
                    return true;
                }
            }

        default:  /* relational tests other than equality */
            return false;
    }
}

bool condition_record::contains_identity_from_set(const id_set* pIDSet)
{
    bool returnVal = (test_contains_identity_in_set(thisAgent, condition_tests.value, pIDSet) ||
        test_contains_identity_in_set(thisAgent, condition_tests.id, pIDSet) ||
        test_contains_identity_in_set(thisAgent, condition_tests.attr, pIDSet));

    dprint(DT_EXPLAIN_PATHS, "condition_record::contains_identity_from_set returning %s.\n", returnVal ? "TRUE" : "FALSE");

    return returnVal;
}

void condition_record::create_identity_paths(const inst_record_list* pInstPath)
{
    if (path_to_base)
    {
        dprint(DT_EXPLAIN_PATHS, "      Condition already has a path to base.  Skipping (%t ^%t %t).\n", condition_tests.id, condition_tests.attr, condition_tests.value);
        return;
    } else
    {
        assert(!path_to_base);
        path_to_base = new inst_record_list();
        (*path_to_base) = (*pInstPath);
        dprint(DT_EXPLAIN_PATHS, "      Condition record copied path_to_base %d = %d.\n", path_to_base->size(), pInstPath->size());
    }

}
