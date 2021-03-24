#include "episodic_memory.h"

#include "agent.h"
#include "decide.h"
#include "ebc.h"
#include "instantiation.h"
#include "preference.h"
#include "semantic_memory.h"
#include "slot.h"
#include "symbol.h"
#include "output_manager.h"
#include "print.h"
#include "production.h"
#include "working_memory.h"
#include "working_memory_activation.h"
#include "xml.h"

#include <cmath>
#include <algorithm>
#include <iterator>
#include <iostream>
#include <fstream>
#include <set>
#include <climits>


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Bookmark strings to help navigate the code
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

// parameters                   epmem::param
// stats                        epmem::stats
// timers                       epmem::timers
// statements                   epmem::statements

// wme-related                  epmem::wmes

// variable abstraction         epmem::var

// relational interval tree     epmem::rit

// cleaning up                  epmem::clean
// initialization               epmem::init

// temporal hash                epmem::hash

// storing new episodes         epmem::storage
// non-cue-based queries        epmem::ncb
// cue-based queries            epmem::cbr

// vizualization                epmem::viz

// high-level api               epmem::api


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Parameter Functions (epmem::params)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

epmem_param_container::epmem_param_container(agent* new_agent): soar_module::param_container(new_agent)
{
    // learning
    learning = new soar_module::boolean_param("learning", off, new soar_module::f_predicate<boolean>());
    add(learning);

    ////////////////////
    // Encoding
    ////////////////////

    // phase
    phase = new soar_module::constant_param<phase_choices>("phase", phase_output, new soar_module::f_predicate<phase_choices>());
    phase->add_mapping(phase_output, "output");
    phase->add_mapping(phase_selection, "selection");
    add(phase);

    // trigger
    trigger = new soar_module::constant_param<trigger_choices>("trigger", dc, new soar_module::f_predicate<trigger_choices>());
    trigger->add_mapping(none, "none");
    trigger->add_mapping(output, "output");
    trigger->add_mapping(dc, "dc");
    add(trigger);

    // force
    force = new soar_module::constant_param<force_choices>("force", force_off, new soar_module::f_predicate<force_choices>());
    force->add_mapping(remember, "remember");
    force->add_mapping(ignore, "ignore");
    force->add_mapping(force_off, "off");
    add(force);

    // exclusions - this is initialized with "epmem" directly after hash tables
    exclusions = new soar_module::sym_set_param("exclusions", new soar_module::f_predicate<const char*>, thisAgent);
    add(exclusions);


    ////////////////////
    // Storage
    ////////////////////

    // database
    database = new soar_module::constant_param<db_choices>("database", memory, new soar_module::f_predicate<db_choices>());
    database->add_mapping(memory, "memory");
    database->add_mapping(file, "file");
    add(database);

    // append database or dump data on init
    append_db = new soar_module::boolean_param("append", off, new soar_module::f_predicate<boolean>());
    add(append_db);

    // path
    path = new epmem_path_param("path", "", new soar_module::predicate<const char*>(), new soar_module::f_predicate<const char*>(), thisAgent);
    add(path);

    // auto-commit
    lazy_commit = new soar_module::boolean_param("lazy-commit", on, new epmem_db_predicate<boolean>(thisAgent));
    add(lazy_commit);

    ////////////////////
    // Retrieval
    ////////////////////

    // graph-match
    graph_match = new soar_module::boolean_param("graph-match", on, new soar_module::f_predicate<boolean>());
    add(graph_match);

    // balance
    balance = new soar_module::decimal_param("balance", 1, new soar_module::btw_predicate<double>(0, 1, true), new soar_module::f_predicate<double>());
    add(balance);


    ////////////////////
    // Performance
    ////////////////////

    // timers
    timers = new soar_module::constant_param<soar_module::timer::timer_level>("timers", soar_module::timer::zero, new soar_module::f_predicate<soar_module::timer::timer_level>());
    timers->add_mapping(soar_module::timer::zero, "off");
    timers->add_mapping(soar_module::timer::one, "one");
    timers->add_mapping(soar_module::timer::two, "two");
    timers->add_mapping(soar_module::timer::three, "three");
    add(timers);

    // page_size
    page_size = new soar_module::constant_param<page_choices>("page-size", page_8k, new epmem_db_predicate<page_choices>(thisAgent));
    page_size->add_mapping(epmem_param_container::page_1k, "1k");
    page_size->add_mapping(epmem_param_container::page_2k, "2k");
    page_size->add_mapping(epmem_param_container::page_4k, "4k");
    page_size->add_mapping(epmem_param_container::page_8k, "8k");
    page_size->add_mapping(epmem_param_container::page_16k, "16k");
    page_size->add_mapping(epmem_param_container::page_32k, "32k");
    page_size->add_mapping(epmem_param_container::page_64k, "64k");
    add(page_size);

    // cache_size
    cache_size = new soar_module::integer_param("cache-size", 10000, new soar_module::gt_predicate<int64_t>(1, true), new epmem_db_predicate<int64_t>(thisAgent));
    add(cache_size);

    // opt
    opt = new soar_module::constant_param<opt_choices>("optimization", epmem_param_container::opt_speed, new epmem_db_predicate<opt_choices>(thisAgent));
    opt->add_mapping(epmem_param_container::opt_safety, "safety");
    opt->add_mapping(epmem_param_container::opt_speed, "performance");
    add(opt);


    ////////////////////
    // Experimental
    ////////////////////

    gm_ordering = new soar_module::constant_param<gm_ordering_choices>("graph-match-ordering", gm_order_undefined, new soar_module::f_predicate<gm_ordering_choices>());
    gm_ordering->add_mapping(gm_order_undefined, "undefined");
    gm_ordering->add_mapping(gm_order_dfs, "dfs");
    gm_ordering->add_mapping(gm_order_mcv, "mcv");
    add(gm_ordering);

    // merge
    merge = new soar_module::constant_param<merge_choices>("merge", merge_none, new soar_module::f_predicate<merge_choices>());
    merge->add_mapping(merge_none, "none");
    merge->add_mapping(merge_add, "add");
    add(merge);

    //smem surprise
    smem_surprise = new soar_module::boolean_param("smem-surprise", off, new soar_module::f_predicate<boolean>());
    add(smem_surprise);

    // surprise
    surprise_method = new soar_module::constant_param<surprise_method_choices>("surprise-method", bla, new soar_module::f_predicate<surprise_method_choices>());
    surprise_method->add_mapping(bla,"bla");
    surprise_method->add_mapping(hebbian,"hebbian");
    add(surprise_method);

    default_surprise_threshold = new soar_module::decimal_param("default-surprise-threshold", 0.5, new soar_module::gt_predicate<double>(0, false), new soar_module::f_predicate<double>());
    add(default_surprise_threshold);// No idea what a good parameter value is here. probably differs by method. -- for simplicity, going to make surprise a value between 0 and 1 always. may change later, but going with this now.

    surprise_exclusions = new soar_module::sym_set_param("surprise-exclusions", new soar_module::f_predicate<const char*>, thisAgent);
    add(surprise_exclusions); //For practicality, things like an always incrementing timer should be ignored unless they have timing significance. even then, still maybe. the point is that you want to be able to cross parts of the same river twice.
//Compression rate would make a good surprise method -- can come back to that with element-wise sequitur later, maybe, (not encessarily that method for compression in general, anything that makes longer timescale blocks with symbolic interpretation.
}

//

epmem_path_param::epmem_path_param(const char* new_name, const char* new_value, soar_module::predicate<const char*>* new_val_pred, soar_module::predicate<const char*>* new_prot_pred, agent* new_agent): soar_module::string_param(new_name, new_value, new_val_pred, new_prot_pred), thisAgent(new_agent) {}

void epmem_path_param::set_value(const char* new_value)
{
    /* Removed automatic switching to disk database mode when first setting path.  Now
       that switching databases and database modes on the fly seems to work, there's
       no need to attach special significance to the first time the path is set.
       MMA 2013 */

    value->assign(new_value);
}

//

template <typename T>
epmem_db_predicate<T>::epmem_db_predicate(agent* new_agent): soar_module::agent_predicate<T>(new_agent) {}

template <typename T>
bool epmem_db_predicate<T>::operator()(T /*val*/)
{
    return (this->thisAgent->EpMem->epmem_db->get_status() == soar_module::connected);
}


/***************************************************************************
 * Function     : epmem_enabled
 * Author       : Nate Derbinsky
 * Notes        : Shortcut function to system parameter
 **************************************************************************/
bool epmem_enabled(agent* thisAgent)
{
    return (thisAgent->EpMem->epmem_params->learning->get_value() == on);
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Temporal Hash Functions (epmem::hash)
//
// The rete has symbol hashing, but the values are
// reliable only for the lifetime of a symbol.  This
// isn't good for epmem.  Hence, we implement a simple
// lookup table.
//
// Note the hashing functions for the symbol types are
// very similar, but with enough differences that I
// separated them out for clarity.
//
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

inline epmem_hash_id epmem_temporal_hash_add_type(agent* thisAgent, byte sym_type)
{
    thisAgent->EpMem->epmem_stmts_common->hash_add_type->bind_int(1, sym_type);
    thisAgent->EpMem->epmem_stmts_common->hash_add_type->execute(soar_module::op_reinit);
    return static_cast<epmem_hash_id>(thisAgent->EpMem->epmem_db->last_insert_rowid());
}

inline epmem_hash_id epmem_temporal_hash_int(agent* thisAgent, int64_t val, bool add_on_fail = true)
{
    epmem_hash_id return_val = NIL;

    // search first
    thisAgent->EpMem->epmem_stmts_common->hash_get_int->bind_int(1, val);
    if (thisAgent->EpMem->epmem_stmts_common->hash_get_int->execute() == soar_module::row)
    {
        return_val = static_cast<epmem_hash_id>(thisAgent->EpMem->epmem_stmts_common->hash_get_int->column_int(0));
    }
    thisAgent->EpMem->epmem_stmts_common->hash_get_int->reinitialize();

    // if fail and supposed to add
    if (!return_val && add_on_fail)
    {
        // type first
        return_val = epmem_temporal_hash_add_type(thisAgent, INT_CONSTANT_SYMBOL_TYPE);

        // then content
        thisAgent->EpMem->epmem_stmts_common->hash_add_int->bind_int(1, return_val);
        thisAgent->EpMem->epmem_stmts_common->hash_add_int->bind_int(2, val);
        thisAgent->EpMem->epmem_stmts_common->hash_add_int->execute(soar_module::op_reinit);
    }

    return return_val;
}

inline epmem_hash_id epmem_temporal_hash_float(agent* thisAgent, double val, bool add_on_fail = true)
{
    epmem_hash_id return_val = NIL;

    // search first
    thisAgent->EpMem->epmem_stmts_common->hash_get_float->bind_double(1, val);
    if (thisAgent->EpMem->epmem_stmts_common->hash_get_float->execute() == soar_module::row)
    {
        return_val = static_cast<epmem_hash_id>(thisAgent->EpMem->epmem_stmts_common->hash_get_float->column_int(0));
    }
    thisAgent->EpMem->epmem_stmts_common->hash_get_float->reinitialize();

    // if fail and supposed to add
    if (!return_val && add_on_fail)
    {
        // type first
        return_val = epmem_temporal_hash_add_type(thisAgent, FLOAT_CONSTANT_SYMBOL_TYPE);

        // then content
        thisAgent->EpMem->epmem_stmts_common->hash_add_float->bind_int(1, return_val);
        thisAgent->EpMem->epmem_stmts_common->hash_add_float->bind_double(2, val);
        thisAgent->EpMem->epmem_stmts_common->hash_add_float->execute(soar_module::op_reinit);
    }

    return return_val;
}

inline epmem_hash_id epmem_temporal_hash_str(agent* thisAgent, char* val, bool add_on_fail = true)
{
    epmem_hash_id return_val = NIL;

    // search first
    thisAgent->EpMem->epmem_stmts_common->hash_get_str->bind_text(1, static_cast<const char*>(val));
    if (thisAgent->EpMem->epmem_stmts_common->hash_get_str->execute() == soar_module::row)
    {
        return_val = static_cast<epmem_hash_id>(thisAgent->EpMem->epmem_stmts_common->hash_get_str->column_int(0));
    }
    thisAgent->EpMem->epmem_stmts_common->hash_get_str->reinitialize();

    // if fail and supposed to add
    if (!return_val && add_on_fail)
    {
        // type first
        return_val = epmem_temporal_hash_add_type(thisAgent, STR_CONSTANT_SYMBOL_TYPE);

        // then content
        thisAgent->EpMem->epmem_stmts_common->hash_add_str->bind_int(1, return_val);
        thisAgent->EpMem->epmem_stmts_common->hash_add_str->bind_text(2, static_cast<const char*>(val));
        thisAgent->EpMem->epmem_stmts_common->hash_add_str->execute(soar_module::op_reinit);
    }

    return return_val;
}


inline int64_t epmem_reverse_hash_int(agent* thisAgent, epmem_hash_id s_id_lookup)
{
    int64_t return_val = NIL;

    thisAgent->EpMem->epmem_stmts_common->hash_rev_int->bind_int(1, s_id_lookup);
    soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_rev_int->execute();
    (void)res; // quells compiler warning
    assert(res == soar_module::row);
    return_val = thisAgent->EpMem->epmem_stmts_common->hash_rev_int->column_int(0);
    thisAgent->EpMem->epmem_stmts_common->hash_rev_int->reinitialize();

    return return_val;
}

inline double epmem_reverse_hash_float(agent* thisAgent, epmem_hash_id s_id_lookup)
{
    double return_val = NIL;

    thisAgent->EpMem->epmem_stmts_common->hash_rev_float->bind_int(1, s_id_lookup);
    soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_rev_float->execute();
    (void)res; // quells compiler warning
    assert(res == soar_module::row);
    return_val = thisAgent->EpMem->epmem_stmts_common->hash_rev_float->column_double(0);
    thisAgent->EpMem->epmem_stmts_common->hash_rev_float->reinitialize();

    return return_val;
}

inline void epmem_reverse_hash_str(agent* thisAgent, epmem_hash_id s_id_lookup, std::string& dest)
{
    soar_module::exec_result res;
    soar_module::sqlite_statement* sql_hash_rev_str = thisAgent->EpMem->epmem_stmts_common->hash_rev_str;

    sql_hash_rev_str->bind_int(1, s_id_lookup);
    res = sql_hash_rev_str->execute();
    (void)res; // quells compiler warning
    if (res != soar_module::row)
    {
        epmem_close(thisAgent);
    }
    assert(res == soar_module::row);
    dest.assign(sql_hash_rev_str->column_text(0));
    sql_hash_rev_str->reinitialize();
}

/* **************************************************************************

                         epmem_reverse_hash

  This function will take an s_id and return a symbol whose contents match those
  stored in the epmem database.  If no sym_type is passed in, this function will
  look up the type in the symbol type database.

  How type id is handled is changed somewhat  from how smem does it and epmem
  previously did it;  that code retrieves the symbol types within the original
  large query, while this one does another retrieve as needed. Hopefully, we'll
  gain more from removing a join from the big, more computationally intensive
  query than we'll lose from the overhead of a second query.  This leverages
  that we always know the symbol type for id's and attributes and don't even
  need to join with the type table for those.

  Will want to verify later.  If confirmed, we should check if we could do it
  for smem too.  We could also remove the LTI join from the big query too and
  do those retrieves as needed.

************************************************************************** */

inline Symbol* epmem_reverse_hash(agent* thisAgent, epmem_hash_id s_id_lookup, byte sym_type = 255)
{
    Symbol* return_val = NULL;
    std::string dest;

    if (sym_type == 255)
    {
        thisAgent->EpMem->epmem_stmts_common->hash_get_type->bind_int(1, s_id_lookup);
        soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_get_type->execute();
        (void)res; // quells compiler warning
        assert(res == soar_module::row);
        sym_type = static_cast<byte>(thisAgent->EpMem->epmem_stmts_common->hash_get_type->column_int(0));
        thisAgent->EpMem->epmem_stmts_common->hash_get_type->reinitialize();
    }

    switch (sym_type)
    {
        case STR_CONSTANT_SYMBOL_TYPE:
            epmem_reverse_hash_str(thisAgent, s_id_lookup, dest);
            return_val = thisAgent->symbolManager->make_str_constant(const_cast<char*>(dest.c_str()));
            break;

        case INT_CONSTANT_SYMBOL_TYPE:
            return_val = thisAgent->symbolManager->make_int_constant(epmem_reverse_hash_int(thisAgent, s_id_lookup));
            break;

        case FLOAT_CONSTANT_SYMBOL_TYPE:
            return_val = thisAgent->symbolManager->make_float_constant(epmem_reverse_hash_float(thisAgent, s_id_lookup));
            break;

        default:
            return_val = NULL;
            break;
    }

    return return_val;
}

/* **************************************************************************

                         epmem_reverse_hash_print

  This function will take an s_id and stores a printable string version of the
  content of that symbol stored in the epmem database into the dest parameter.
  If no sym_type is passed in, this function will look up the type in the
  symbol type database.


************************************************************************** */

inline void epmem_reverse_hash_print(agent* thisAgent, epmem_hash_id s_id_lookup, std::string& dest, byte sym_type = 255)
{
    Symbol* return_val = NULL;

    // This may be faster than including type lookup in edges?  Might want to check later.

    if (sym_type == 255)
    {
        thisAgent->EpMem->epmem_stmts_common->hash_get_type->bind_int(1, s_id_lookup);
        soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_get_type->execute();
        (void)res; // quells compiler warning
        assert(res == soar_module::row);
        // check if should be column_int
        sym_type = static_cast<byte>(thisAgent->EpMem->epmem_stmts_common->hash_get_type->column_int(0));
        thisAgent->EpMem->epmem_stmts_common->hash_get_type->reinitialize();
    }

    switch (sym_type)
    {
        case STR_CONSTANT_SYMBOL_TYPE:
            epmem_reverse_hash_str(thisAgent, s_id_lookup, dest);
            break;

        case INT_CONSTANT_SYMBOL_TYPE:
            to_string(epmem_reverse_hash_int(thisAgent, s_id_lookup), dest);
            break;

        case FLOAT_CONSTANT_SYMBOL_TYPE:
            to_string(epmem_reverse_hash_float(thisAgent, s_id_lookup), dest);
            break;

        default:
            return_val = NULL;
            break;
    }
}

/* **************************************************************************

                         epmem_temporal_hash

    This function returns an s_id (symbol id) representing a symbol constant
    stored in the epmem database. The individual hash functions will first
    check if there already exists an identical entry in the epmem database
    and return it if found.  If not, it will add new entries to the both the
    type table and one of the typed constant tables.

************************************************************************** */

epmem_hash_id epmem_temporal_hash(agent* thisAgent, Symbol* sym, bool add_on_fail)
{
    epmem_hash_id return_val = NIL;

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->hash->start();
    ////////////////////////////////////////////////////////////////////////////

    if (sym->is_constant())
    {
        if ((!sym->epmem_hash) || (sym->epmem_valid != thisAgent->EpMem->epmem_validation))
        {
            sym->epmem_hash = NIL;
            sym->epmem_valid = thisAgent->EpMem->epmem_validation;

            switch (sym->symbol_type)
            {
                case STR_CONSTANT_SYMBOL_TYPE:
                    return_val = epmem_temporal_hash_str(thisAgent, sym->sc->name, add_on_fail);
                    break;

                case INT_CONSTANT_SYMBOL_TYPE:
                    return_val = epmem_temporal_hash_int(thisAgent, sym->ic->value, add_on_fail);
                    break;

                case FLOAT_CONSTANT_SYMBOL_TYPE:
                    return_val = epmem_temporal_hash_float(thisAgent, sym->fc->value, add_on_fail);
                    break;
            }

            // cache results for later re-use
            sym->epmem_hash = return_val;
            sym->epmem_valid = thisAgent->EpMem->epmem_validation;
        }

        return_val = sym->epmem_hash;
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->hash->stop();
    ////////////////////////////////////////////////////////////////////////////

    return return_val;
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Statistic Functions (epmem::stats)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

epmem_stat_container::epmem_stat_container(agent* new_agent): soar_module::stat_container(new_agent)
{
    // time
    time = new epmem_time_id_stat("time", 0, new epmem_db_predicate<epmem_time_id>(thisAgent));
    add(time);

    // db-lib-version
    db_lib_version = new epmem_db_lib_version_stat(thisAgent, "db-lib-version", NULL, new soar_module::predicate< const char* >());
    add(db_lib_version);

    // mem-usage
    mem_usage = new epmem_mem_usage_stat(thisAgent, "mem-usage", 0, new soar_module::predicate<int64_t>());
    add(mem_usage);

    // mem-high
    mem_high = new epmem_mem_high_stat(thisAgent, "mem-high", 0, new soar_module::predicate<int64_t>());
    add(mem_high);

    // non-cue-based-retrievals
    ncbr = new soar_module::integer_stat("retrievals", 0, new soar_module::f_predicate<int64_t>());
    add(ncbr);

    // cue-based-retrievals
    cbr = new soar_module::integer_stat("queries", 0, new soar_module::f_predicate<int64_t>());
    add(cbr);

    // nexts
    nexts = new soar_module::integer_stat("nexts", 0, new soar_module::f_predicate<int64_t>());
    add(nexts);

    // prev's
    prevs = new soar_module::integer_stat("prevs", 0, new soar_module::f_predicate<int64_t>());
    add(prevs);

    // ncb-wmes
    ncb_wmes = new soar_module::integer_stat("ncb-wmes", 0, new soar_module::f_predicate<int64_t>());
    add(ncb_wmes);

    // qry-pos
    qry_pos = new soar_module::integer_stat("qry-pos", 0, new soar_module::f_predicate<int64_t>());
    add(qry_pos);

    // qry-neg
    qry_neg = new soar_module::integer_stat("qry-neg", 0, new soar_module::f_predicate<int64_t>());
    add(qry_neg);

    // qry-ret
    qry_ret = new epmem_time_id_stat("qry-ret", 0, new soar_module::f_predicate<epmem_time_id>());
    add(qry_ret);

    // qry-card
    qry_card = new soar_module::integer_stat("qry-card", 0, new soar_module::f_predicate<int64_t>());
    add(qry_card);

    // qry-lits
    qry_lits = new soar_module::integer_stat("qry-lits", 0, new soar_module::f_predicate<int64_t>());
    add(qry_lits);

    // next-id
    next_id = new epmem_node_id_stat("next-id", 0, new epmem_db_predicate<epmem_node_id>(thisAgent));
    add(next_id);

    // rit-offset-1
    rit_offset_1 = new soar_module::integer_stat("rit-offset-1", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_offset_1);

    // rit-left-root-1
    rit_left_root_1 = new soar_module::integer_stat("rit-left-root-1", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_left_root_1);

    // rit-right-root-1
    rit_right_root_1 = new soar_module::integer_stat("rit-right-root-1", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_right_root_1);

    // rit-min-step-1
    rit_min_step_1 = new soar_module::integer_stat("rit-min-step-1", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_min_step_1);

    // rit-offset-2
    rit_offset_2 = new soar_module::integer_stat("rit-offset-2", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_offset_2);

    // rit-left-root-2
    rit_left_root_2 = new soar_module::integer_stat("rit-left-root-2", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_left_root_2);

    // rit-right-root-2
    rit_right_root_2 = new soar_module::integer_stat("rit-right-root-2", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_right_root_2);

    // rit-min-step-2
    rit_min_step_2 = new soar_module::integer_stat("rit-min-step-2", 0, new epmem_db_predicate<int64_t>(thisAgent));
    add(rit_min_step_2);


    /////////////////////////////
    // connect to rit state
    /////////////////////////////

    // graph
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].offset.stat = rit_offset_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].offset.var_key = var_rit_offset_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].leftroot.stat = rit_left_root_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].leftroot.var_key = var_rit_leftroot_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].rightroot.stat = rit_right_root_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].rightroot.var_key = var_rit_rightroot_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].minstep.stat = rit_min_step_1;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].minstep.var_key = var_rit_minstep_1;

    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].offset.stat = rit_offset_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].offset.var_key = var_rit_offset_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].leftroot.stat = rit_left_root_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].leftroot.var_key = var_rit_leftroot_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].rightroot.stat = rit_right_root_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].rightroot.var_key = var_rit_rightroot_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].minstep.stat = rit_min_step_2;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].minstep.var_key = var_rit_minstep_2;
}

//

epmem_db_lib_version_stat::epmem_db_lib_version_stat(agent* new_agent, const char* new_name, const char* new_value, soar_module::predicate< const char* >* new_prot_pred): soar_module::primitive_stat< const char* >(new_name, new_value, new_prot_pred), thisAgent(new_agent) {}

const char* epmem_db_lib_version_stat::get_value()
{
    return thisAgent->EpMem->epmem_db->lib_version();
}

//

epmem_mem_usage_stat::epmem_mem_usage_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred): soar_module::integer_stat(new_name, new_value, new_prot_pred), thisAgent(new_agent) {}

int64_t epmem_mem_usage_stat::get_value()
{
    return thisAgent->EpMem->epmem_db->memory_usage();
}

//

epmem_mem_high_stat::epmem_mem_high_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred): soar_module::integer_stat(new_name, new_value, new_prot_pred), thisAgent(new_agent) {}

int64_t epmem_mem_high_stat::get_value()
{
    return thisAgent->EpMem->epmem_db->memory_highwater();
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Timer Functions (epmem::timers)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

epmem_timer_container::epmem_timer_container(agent* new_agent): soar_module::timer_container(new_agent)
{
    // one

    total = new epmem_timer("_total", thisAgent, soar_module::timer::one);
    add(total);

    // two

    storage = new epmem_timer("epmem_storage", thisAgent, soar_module::timer::two);
    add(storage);

    ncb_retrieval = new epmem_timer("epmem_ncb_retrieval", thisAgent, soar_module::timer::two);
    add(ncb_retrieval);

    query = new epmem_timer("epmem_query", thisAgent, soar_module::timer::two);
    add(query);

    api = new epmem_timer("epmem_api", thisAgent, soar_module::timer::two);
    add(api);

    trigger = new epmem_timer("epmem_trigger", thisAgent, soar_module::timer::two);
    add(trigger);

    init = new epmem_timer("epmem_init", thisAgent, soar_module::timer::two);
    add(init);

    next = new epmem_timer("epmem_next", thisAgent, soar_module::timer::two);
    add(next);

    prev = new epmem_timer("epmem_prev", thisAgent, soar_module::timer::two);
    add(prev);

    hash = new epmem_timer("epmem_hash", thisAgent, soar_module::timer::two);
    add(hash);

    wm_phase = new epmem_timer("epmem_wm_phase", thisAgent, soar_module::timer::two);
    add(wm_phase);

    // three

    storage_store_levels = new epmem_timer("epmem_storage_store_levels", thisAgent, soar_module::timer::three);
    add(storage_store_levels);
    storage_insert_constants = new epmem_timer("epmem_storage_insert_constants", thisAgent, soar_module::timer::three);
    add(storage_insert_constants);
    storage_insert_ids = new epmem_timer("epmem_storage_insert_ids", thisAgent, soar_module::timer::three);
    add(storage_insert_ids);
    storage_remove_constants = new epmem_timer("epmem_storage_remove_constants", thisAgent, soar_module::timer::three);
    add(storage_remove_constants);
    storage_remove_ids = new epmem_timer("epmem_storage_remove_ids", thisAgent, soar_module::timer::three);
    add(storage_remove_ids);
    storage_do_surprise = new epmem_timer("epmem_storage_do_surprise", thisAgent, soar_module::timer::three);
    add(storage_do_surprise);
    storage_do_edge_updates = new epmem_timer("epmem_storage_do_edge_updates", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates);
    storage_do_edge_updates_1 = new epmem_timer("epmem_storage_do_edge_updates_1", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_1);
    storage_do_edge_updates_2 = new epmem_timer("epmem_storage_do_edge_updates_2", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_2);
    storage_do_edge_updates_3 = new epmem_timer("epmem_storage_do_edge_updates_3", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_3);
    storage_do_edge_updates_4 = new epmem_timer("epmem_storage_do_edge_updates_4", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_4);
    storage_do_edge_updates_4_1 = new epmem_timer("epmem_storage_do_edge_updates_4_1", thisAgent, soar_module::timer::three);
        add(storage_do_edge_updates_4_1);
        storage_do_edge_updates_4_2 = new epmem_timer("epmem_storage_do_edge_updates_4_2", thisAgent, soar_module::timer::three);
            add(storage_do_edge_updates_4_2);
            storage_do_edge_updates_4_3 = new epmem_timer("epmem_storage_do_edge_updates_4_3", thisAgent, soar_module::timer::three);
                add(storage_do_edge_updates_4_3);

    storage_do_edge_updates_5 = new epmem_timer("epmem_storage_do_edge_updates_5", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_5);
    storage_do_edge_updates_6 = new epmem_timer("epmem_storage_do_edge_updates_6", thisAgent, soar_module::timer::three);
    add(storage_do_edge_updates_6);
    storage_do_edge_updates_4_2_1 = new epmem_timer("epmem_storage_do_edge_updates_4_2_1", thisAgent, soar_module::timer::three);
                add(storage_do_edge_updates_4_2_1);

    ncb_edge = new epmem_timer("ncb_edge", thisAgent, soar_module::timer::three);
    add(ncb_edge);

    ncb_edge_rit = new epmem_timer("ncb_edge_rit", thisAgent, soar_module::timer::three);
    add(ncb_edge_rit);

    ncb_node = new epmem_timer("ncb_node", thisAgent, soar_module::timer::three);
    add(ncb_node);

    ncb_node_rit = new epmem_timer("ncb_node_rit", thisAgent, soar_module::timer::three);
    add(ncb_node_rit);

    query_dnf = new epmem_timer("query_dnf", thisAgent, soar_module::timer::three);
    add(query_dnf);

    query_walk = new epmem_timer("query_walk", thisAgent, soar_module::timer::three);
    add(query_walk);

    query_walk_edge = new epmem_timer("query_walk_edge", thisAgent, soar_module::timer::three);
    add(query_walk_edge);

    query_walk_interval = new epmem_timer("query_walk_interval", thisAgent, soar_module::timer::three);
    add(query_walk_interval);

    query_graph_match = new epmem_timer("query_graph_match", thisAgent, soar_module::timer::three);
    add(query_graph_match);

    query_result = new epmem_timer("query_result", thisAgent, soar_module::timer::three);
    add(query_result);

    query_cleanup = new epmem_timer("query_cleanup", thisAgent, soar_module::timer::three);
    add(query_cleanup);

    query_sql_edge = new epmem_timer("query_sql_edge", thisAgent, soar_module::timer::three);
    add(query_sql_edge);

    query_sql_start_ep = new epmem_timer("query_sql_start_ep", thisAgent, soar_module::timer::three);
    add(query_sql_start_ep);

    query_sql_start_now = new epmem_timer("query_sql_start_now", thisAgent, soar_module::timer::three);
    add(query_sql_start_now);

    query_sql_start_point = new epmem_timer("query_sql_start_point", thisAgent, soar_module::timer::three);
    add(query_sql_start_point);

    query_sql_end_ep = new epmem_timer("query_sql_end_ep", thisAgent, soar_module::timer::three);
    add(query_sql_end_ep);

    query_sql_end_now = new epmem_timer("query_sql_end_now", thisAgent, soar_module::timer::three);
    add(query_sql_end_now);

    query_sql_end_point = new epmem_timer("query_sql_end_point", thisAgent, soar_module::timer::three);
    add(query_sql_end_point);

    /////////////////////////////
    // connect to rit state
    /////////////////////////////

    // graph
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].timer = ncb_node_rit;
    thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].timer = ncb_edge_rit;
}

//

epmem_timer_level_predicate::epmem_timer_level_predicate(agent* new_agent): soar_module::agent_predicate<soar_module::timer::timer_level>(new_agent) {}

bool epmem_timer_level_predicate::operator()(soar_module::timer::timer_level val)
{
    return (thisAgent->EpMem->epmem_params->timers->get_value() >= val);
}

//

epmem_timer::epmem_timer(const char* new_name, agent* new_agent, soar_module::timer::timer_level new_level): soar_module::timer(new_name, new_agent, new_level, new epmem_timer_level_predicate(new_agent)) {}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Statement Functions (epmem::statements)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

void epmem_common_statement_container::create_graph_tables()
{

    add_structure("CREATE TABLE IF NOT EXISTS versions (system TEXT PRIMARY KEY,version_number TEXT)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_persistent_variables (variable_id INTEGER PRIMARY KEY,variable_value NONE)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_rit_left_nodes (rit_min INTEGER, rit_max INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_rit_right_nodes (rit_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_symbols_type (s_id INTEGER PRIMARY KEY, symbol_type INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_symbols_integer (s_id INTEGER PRIMARY KEY, symbol_value INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_symbols_float (s_id INTEGER PRIMARY KEY, symbol_value REAL)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_symbols_string (s_id INTEGER PRIMARY KEY, symbol_value TEXT)");

}

void epmem_common_statement_container::create_graph_indices()
{

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS symbols_int_const ON epmem_symbols_integer (symbol_value)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS symbols_float_const ON epmem_symbols_float (symbol_value)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS symbols_str_const ON epmem_symbols_string (symbol_value)");

}

void epmem_common_statement_container::drop_graph_tables()
{

    // Note: We don't want to dump versions database because it might also contain other version information
    // if we ever combine epmem and smem into one database, which is something that has been discussed

    add_structure("DROP TABLE IF EXISTS epmem_persistent_variables");
    add_structure("DROP TABLE IF EXISTS epmem_rit_left_nodes");
    add_structure("DROP TABLE IF EXISTS epmem_rit_right_nodes");
    add_structure("DROP TABLE IF EXISTS epmem_symbols_type");
    add_structure("DROP TABLE IF EXISTS epmem_symbols_integer");
    add_structure("DROP TABLE IF EXISTS epmem_symbols_float");
    add_structure("DROP TABLE IF EXISTS epmem_symbols_string");

}

epmem_common_statement_container::epmem_common_statement_container(agent* new_agent): soar_module::sqlite_statement_container(new_agent->EpMem->epmem_db)
{
    soar_module::sqlite_database* new_db = new_agent->EpMem->epmem_db;

    // Drop tables in the database if append setting is off.  (Tried DELETE before, but it had problems.)
    if ((new_agent->EpMem->epmem_params->database->get_value() != epmem_param_container::memory) &&
            (new_agent->EpMem->epmem_params->append_db->get_value() == off))
    {
        drop_graph_tables();
    }

    create_graph_tables();
    create_graph_indices();

    // Update the schema version number
    add_structure("INSERT OR REPLACE INTO versions (system, version_number) VALUES ('epmem_schema'," EPMEM_SCHEMA_VERSION ")");

    // Add symbol lookups for special cases

    // Root node of tree
    // Note:  I don't think root node string is ever actually looked up.  Set to root instead of
    //        previous NULL for compatibility with other db systems.
    add_structure("INSERT OR IGNORE INTO epmem_symbols_type (s_id,symbol_type) VALUES (0,2)");
    add_structure("INSERT OR IGNORE INTO epmem_symbols_string (s_id,symbol_value) VALUES (0,'root')");

    // Acceptable preference wmes: id 1 = "operator+"
    add_structure("INSERT OR IGNORE INTO epmem_symbols_type (s_id,symbol_type) VALUES (1,2)");
    add_structure("INSERT OR IGNORE INTO epmem_symbols_string (s_id,symbol_value) VALUES (1,'operator*')");

    //

    begin = new soar_module::sqlite_statement(new_db, "BEGIN");
    add(begin);

    commit = new soar_module::sqlite_statement(new_db, "COMMIT");
    add(commit);

    rollback = new soar_module::sqlite_statement(new_db, "ROLLBACK");
    add(rollback);

    //

    var_get = new soar_module::sqlite_statement(new_db, "SELECT variable_value FROM epmem_persistent_variables WHERE variable_id=?");
    add(var_get);

    var_set = new soar_module::sqlite_statement(new_db, "REPLACE INTO epmem_persistent_variables (variable_id,variable_value) VALUES (?,?)");
    add(var_set);

    //

    rit_add_left = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_rit_left_nodes (rit_min,rit_max) VALUES (?,?)");
    add(rit_add_left);

    rit_truncate_left = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_rit_left_nodes");
    add(rit_truncate_left);

    rit_add_right = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_rit_right_nodes (rit_id) VALUES (?)");
    add(rit_add_right);

    rit_truncate_right = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_rit_right_nodes");
    add(rit_truncate_right);

    //

    hash_rev_int = new soar_module::sqlite_statement(new_db, "SELECT symbol_value FROM epmem_symbols_integer WHERE s_id=?");
    add(hash_rev_int);

    hash_rev_float = new soar_module::sqlite_statement(new_db, "SELECT symbol_value FROM epmem_symbols_float WHERE s_id=?");
    add(hash_rev_float);

    hash_rev_str = new soar_module::sqlite_statement(new_db, "SELECT symbol_value FROM epmem_symbols_string WHERE s_id=?");
    add(hash_rev_str);

    hash_get_int = new soar_module::sqlite_statement(new_db, "SELECT s_id FROM epmem_symbols_integer WHERE symbol_value=?");
    add(hash_get_int);

    hash_get_float = new soar_module::sqlite_statement(new_db, "SELECT s_id FROM epmem_symbols_float WHERE symbol_value=?");
    add(hash_get_float);

    hash_get_str = new soar_module::sqlite_statement(new_db, "SELECT s_id FROM epmem_symbols_string WHERE symbol_value=?");
    add(hash_get_str);

    hash_get_type = new soar_module::sqlite_statement(new_db, "SELECT symbol_type FROM epmem_symbols_type WHERE s_id=?");
    add(hash_get_type);

    hash_add_type = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_symbols_type (symbol_type) VALUES (?)");
    add(hash_add_type);

    hash_add_int = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_symbols_integer (s_id,symbol_value) VALUES (?,?)");
    add(hash_add_int);

    hash_add_float = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_symbols_float (s_id,symbol_value) VALUES (?,?)");
    add(hash_add_float);

    hash_add_str = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_symbols_string (s_id,symbol_value) VALUES (?,?)");
    add(hash_add_str);

}

void epmem_graph_statement_container::create_graph_tables()
{
//The new tables for floats are not used for existing query and memory access. Floats are redundantly represented two ways. The old way is maintained without change within the constant table.
    //It is merely in addition that I also keep separate tables that are meant to store qualitative symbols grounded to floats.2
    add_structure("CREATE TABLE IF NOT EXISTS epmem_nodes (n_id INTEGER, lti_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_episodes (episode_id INTEGER PRIMARY KEY)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_times (time_id INTEGER PRIMARY KEY AUTOINCREMENT,start_episode_id INTEGER, end_episode_id INTEGER)");
//    add_structure("CREATE TABLE IF NOT EXISTS epmem_time_time_weight (time_id_left INTEGER, time_id_right INTEGER, weight REAL)");//insert here after first calculation, always check to see if it's there.
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_constant_now (wc_id INTEGER,start_episode_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_identifier_now (wi_id INTEGER,start_episode_id INTEGER, lti_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_float_now (wf_id INTEGER, start_episode_id INTEGER)");// The new float tables such as this one do *not* replace functionality provided by the constant tables.
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_constant_point (wc_id INTEGER,episode_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_identifier_point (wi_id INTEGER,episode_id INTEGER, lti_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_float_point (wf_id INTEGER,episode_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_constant_range (rit_id INTEGER,start_episode_id INTEGER,end_episode_id INTEGER,wc_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_identifier_range (rit_id INTEGER,start_episode_id INTEGER,end_episode_id INTEGER,wi_id INTEGER, lti_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_float_range (start_episode_id INTEGER,end_episode_id INTEGER,wf_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_constant (wc_id INTEGER PRIMARY KEY AUTOINCREMENT,parent_n_id INTEGER,attribute_s_id INTEGER, value_s_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_identifier (wi_id INTEGER PRIMARY KEY AUTOINCREMENT,parent_n_id INTEGER,attribute_s_id INTEGER,child_n_id INTEGER, last_episode_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_float (wf_id INTEGER PRIMARY KEY AUTOINCREMENT, parent_n_id INTEGER, attribute_s_id INTEGER, direction INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_index (w_id INTEGER PRIMARY KEY AUTOINCREMENT, type INTEGER, w_type_based_id)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_ascii (ascii_num INTEGER PRIMARY KEY, ascii_chr TEXT)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_intervals (interval_id INTEGER PRIMARY KEY AUTOINCREMENT, w_id INTEGER, time_id INTEGER, surprise REAL, is_now BOOLEAN, start_episode_id INTEGER)");//An interval is something at a time, and it may be surprising.
    add_structure("CREATE TABLE IF NOT EXISTS epmem_interval_relations (w_id_left INTEGER, w_id_right INTEGER, relation INTEGER, weight REAL, total REAL, weight_norm REAL, PRIMARY KEY(w_id_left,w_id_right,relation)) WITHOUT ROWID");//I have enums for the relation type.
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_index_now (w_id INTEGER PRIMARY KEY, start_episode_id INTEGER) WITHOUT ROWID");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_temp (w_id INTEGER PRIMARY KEY, time_id INTEGER, start_episode_id INTEGER)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_wmes_just_added (w_id INTEGER PRIMARY KEY) WITHOUT ROWID");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_relations_just_added (w_id_left INTEGER, w_id_right INTEGER, relation INTEGER, weight REAL)");
    add_structure("CREATE TABLE IF NOT EXISTS epmem_potential_interval_updates (w_id_left INTEGER, w_id_right INTEGER, time_id_left INTEGER, time_id_right INTEGER, relation INTEGER, finished_right BOOLEAN)");//Make this include some form of time data as well (to help provide update magnitude).
    add_structure("CREATE TABLE IF NOT EXISTS epmem_w_id_to_lti_id (w_id INTEGER, lti_id INTEGER, PRIMARY KEY(w_id,lti_id)) WITHOUT ROWID");//the lti identity for any interval associated with w_id.
}

void epmem_graph_statement_container::create_graph_indices()
{//todo organize in two ways -- by the table(s) referenced and by the aspect of memory, the functionality they support
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_index_now_time_id ON epmem_wmes_index_now (start_episode_id DESC, w_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_w_id_to_lti_index ON epmem_w_id_to_lti_id (lti_id,w_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_intervals_w_id_now ON epmem_intervals (is_now,w_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_interval_relations_left_right ON epmem_interval_relations (w_id_left,w_id_right,relation,weight_norm)");//, relation, weight)");//when adding more relations, this should only be unique per relation type.
    add_structure("CREATE INDEX IF NOT EXISTS epmem_intervals_time ON epmem_intervals (time_id)");
    //add_structure("CREATE INDEX IF NOT EXISTS epmem_interval_relations_right_left ON epmem_interval_relations (w_id_right,w_id_left)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_find_update_index ON epmem_potential_interval_updates (w_id_left,w_id_right,relation) WHERE finished_right=1");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_update_hold_up ON epmem_potential_interval_updates (w_id_right) WHERE finished_right=0");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_potential_update_grouping ON epmem_potential_interval_updates (w_id_left) WHERE finished_right=1");

    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_insert_add_to_new_relations AFTER INSERT ON epmem_interval_relations FOR EACH ROW BEGIN INSERT INTO epmem_relations_just_added (w_id_left, w_id_right, relation, weight) VALUES (NEW.w_id_left, NEW.w_id_right, NEW.relation, NEW.weight_norm); END");//needs weight

    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_insert_add_to_now AFTER INSERT ON epmem_intervals FOR EACH ROW BEGIN INSERT INTO epmem_wmes_index_now (w_id,start_episode_id) VALUES (NEW.w_id,NEW.start_episode_id); END");
    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_update_move_to_temp AFTER UPDATE OF time_id ON epmem_intervals FOR EACH ROW BEGIN INSERT INTO epmem_wmes_temp (w_id,time_id,start_episode_id) VALUES (NEW.w_id,NEW.time_id,NEW.start_episode_id); END");
    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_update_move_from_now AFTER UPDATE OF time_id ON epmem_intervals FOR EACH ROW BEGIN DELETE FROM epmem_wmes_index_now WHERE epmem_wmes_index_now.w_id=OLD.w_id; END");
    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_update_find_weight_updates AFTER UPDATE ON epmem_intervals FOR EACH ROW BEGIN UPDATE epmem_potential_interval_updates SET finished_right=1,time_id_right=NEW.time_id WHERE w_id_right=OLD.w_id AND finished_right=0; END");
    //when these triggers have fired and after the addition/removal loops of epmem processing at the end of the DC, there will now be a table of the contents of the intervals that have ended and a table of the things those could have before-relations to.
    //cartesian product is the thing we want from that.

    add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_insert_add_to_new AFTER INSERT ON epmem_wmes_index FOR EACH ROW BEGIN INSERT INTO epmem_wmes_just_added (w_id) VALUES (NEW.w_id); END");

    //and, because of that equivalence, not going to make this table. Probably can make the subqueries work by index without making table.

    //add_structure("CREATE TRIGGER IF NOT EXISTS epmem_on_time_update_do_weight AFTER UPDATE ON epmem_potential_interval_updates FOR EACH ROW BEGIN INSERT INTO epmem_interval_relations (w_id_left, w_id_right, relation, weight, total) SELECT () FROM  ON CONFLICT(w_id_left,w_id_right) DO UPDATE SET ; END");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_times_time_from_bounds ON epmem_times (start_episode_id,end_episode_id,time_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_float_parent_value_direction ON epmem_wmes_float (parent_n_id,attribute_s_id,direction)");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_index_type_id ON epmem_wmes_index (type,w_type_based_id,w_id)");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_times_start_end ON epmem_times (start_episode_id,end_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_times_end_start ON epmem_times (end_episode_id,start_episode_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_intervals_id_start_end ON epmem_intervals (time_id,surprise DESC)");
    //add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_intervals_data_and_time ON epmem_intervals (w_id,time_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_intervals_id_now ON epmem_intervals (w_id,interval_id) WHERE is_now");
    //add_structure("CREATE INDEX IF NOT EXISTS epmem_intervals_id_end_start ON epmem_intervals (end_episode_id,start_episode_id,surprise DESC)");
    //add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_intervals_");
    //A hash index would be nice here for id, start, end, without ordering. For now, can just retrieve by interval and filter at retrieval time by surprise.

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_node_lti ON epmem_nodes (n_id, lti_id)");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_lti ON epmem_nodes (lti_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_constant_now_start ON epmem_wmes_constant_now (start_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_constant_now_id_start ON epmem_wmes_constant_now (wc_id,start_episode_id DESC)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_float_now_start ON epmem_wmes_float_now (start_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_float_now_id ON epmem_wmes_float_now (wf_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_identifier_now_start ON epmem_wmes_identifier_now (start_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_identifier_now_id_start ON epmem_wmes_identifier_now (wi_id,start_episode_id DESC)");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_constant_point_id_start ON epmem_wmes_constant_point (wc_id,episode_id DESC)");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_constant_point_start ON epmem_wmes_constant_point (episode_id)");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_identifier_point_id_start ON epmem_wmes_identifier_point (wi_id,episode_id DESC)");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_identifier_point_start ON epmem_wmes_identifier_point (episode_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_constant_range_lower ON epmem_wmes_constant_range (rit_id,start_episode_id)");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_constant_range_upper ON epmem_wmes_constant_range (rit_id,end_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_constant_range_id_start ON epmem_wmes_constant_range (wc_id,start_episode_id DESC)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_constant_range_id_end_start ON epmem_wmes_constant_range (wc_id,end_episode_id DESC,start_episode_id)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_identifier_range_lower ON epmem_wmes_identifier_range (rit_id,start_episode_id)");
    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_identifier_range_upper ON epmem_wmes_identifier_range (rit_id,end_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_identifier_range_id_start ON epmem_wmes_identifier_range (wi_id,start_episode_id DESC)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_identifier_range_id_end_start ON epmem_wmes_identifier_range (wi_id,end_episode_id DESC,start_episode_id)");

    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_constant_parent_attribute_value ON epmem_wmes_constant (parent_n_id,attribute_s_id,value_s_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_float_parent_attribute_value ON epmem_wmes_float (parent_n_id,attribute_s_id,direction)");

    add_structure("CREATE INDEX IF NOT EXISTS epmem_wmes_identifier_parent_attribute_last ON epmem_wmes_identifier (parent_n_id,attribute_s_id,last_episode_id)");
    add_structure("CREATE UNIQUE INDEX IF NOT EXISTS epmem_wmes_identifier_parent_attribute_child ON epmem_wmes_identifier (parent_n_id,attribute_s_id,child_n_id)");



}

void epmem_graph_statement_container::drop_graph_tables()
{
    add_structure("DROP TABLE IF EXISTS epmem_nodes");
    add_structure("DROP TABLE IF EXISTS epmem_episodes");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_constant_now");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_float_now");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_identifier_now");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_constant_point");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_identifier_point");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_constant_range");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_identifier_range");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_constant");
    add_structure("DROP TABLE IF EXISTS epmem_wmes_identifier");
}

epmem_graph_statement_container::epmem_graph_statement_container(agent* new_agent): soar_module::sqlite_statement_container(new_agent->EpMem->epmem_db)
{
    soar_module::sqlite_database* new_db = new_agent->EpMem->epmem_db;

    // Delete all entries from the tables in the database if append setting is off
    if (new_agent->EpMem->epmem_params->append_db->get_value() == off)
    {
        print_sysparam_trace(new_agent, 0, "Erasing contents of episodic memory database. (append = off)\n");
        drop_graph_tables();
    }

    create_graph_tables();
    create_graph_indices();

    // workaround for tree: type 1 = IDENTIFIER_SYMBOL_TYPE
    add_structure("INSERT OR IGNORE INTO epmem_nodes (n_id) VALUES (0)");
    {
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (65,'A')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (66,'B')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (67,'C')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (68,'D')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (69,'E')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (70,'F')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (71,'G')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (72,'H')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (73,'I')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (74,'J')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (75,'K')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (76,'L')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (77,'M')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (78,'N')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (79,'O')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (80,'P')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (81,'Q')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (82,'R')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (83,'S')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (84,'T')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (85,'U')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (86,'V')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (87,'W')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (88,'X')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (89,'Y')");
        add_structure("INSERT OR IGNORE INTO epmem_ascii (ascii_num, ascii_chr) VALUES (90,'Z')");
    }

    //generally, anything that needs the "OR IGNORE" to actually work is being inefficient, at the least.

    add_time = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_episodes (episode_id) VALUES (?)");
    add(add_time);

    add_interval_time = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_times (start_episode_id,end_episode_id) VALUES (?,?)");
    add(add_interval_time);

    insert_potential_before_relations = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_potential_interval_updates (w_id_left,w_id_right,time_id_left,time_id_right,relation,finished_right) SELECT l.w_id, r.w_id, l.time_id, -1, 0, 0 FROM epmem_wmes_temp l,epmem_wmes_index_now r WHERE l.start_episode_id < r.start_episode_id");
    add(insert_potential_before_relations);

    //After that insertion, there will be many transitions which were not observed, and these can be depreciated in a batch.
    //for all lefts in epmem_wmes_temp, for all rights in epmem_interval_relations not within epmem_potential_interval_updates from those corresponding lefts, depreciate within epmem_interval_relations
    //what form of update? -- start simple with nothing but an "n"-based average. adding a zero to the average in depreciation. adding to sum with a weight when actually observing a transition
    //so, first, depreciation based on a join, performed by incrementing the total.
    update_unobserved_before_relations = new soar_module::sqlite_statement(new_db, "REPLACE INTO epmem_interval_relations (w_id_left, w_id_right, relation, weight, total, weight_norm) SELECT r.w_id_left, r.w_id_right, r.relation, r.weight, r.total+1.0, r.weight/(total+1.0) FROM "
            "(epmem_interval_relations r INNER JOIN epmem_wmes_temp t ON r.w_id_left=t.w_id LEFT JOIN epmem_wmes_index_now n ON n.w_id=r.w_id_right) WHERE n.w_id IS NULL");//(epmem_interval_relations r LEFT JOIN epmem_wmes_temp t ON r.w_id_left=t.w_id) WHERE t.w_id IS NULL");
    add(update_unobserved_before_relations);//The logic on this update is to look for relations with left_w_ids that just finished (are in temp), but without corresponding right_w_ids in now.

    select_removed_nows = new soar_module::sqlite_statement(new_db, "SELECT wl.lti_id FROM epmem_wmes_temp t INNER JOIN epmem_w_id_to_lti_id wl ON t.w_id=wl.w_id");//needs to be the lti_id.
    add(select_removed_nows);

    delete_removed_nows = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_wmes_temp");
    add(delete_removed_nows);

    select_new_nows = new soar_module::sqlite_statement(new_db, "SELECT wl.lti_id FROM epmem_wmes_index_now i INNER JOIN epmem_w_id_to_lti_id wl ON i.w_id=wl.w_id WHERE start_episode_id=?");//needs to be the lti_id
    add(select_new_nows);

    count_old_nows = new soar_module::sqlite_statement(new_db, "SELECT COUNT(*) FROM epmem_wmes_index_now WHERE start_episode_id<?");
    add(count_old_nows);

    get_interval_time = new soar_module::sqlite_statement(new_db, "SELECT time_id FROM epmem_times WHERE start_episode_id=? AND end_episode_id=?");
    add(get_interval_time);

    //so, this really becomes a different query //could do a bla-style thing (so that it's not all frequency) or even a maturation constant. would have boost on observation of a relation and time increment on observation of a left. could have a confidence/stickiness param. a million ways to go
    select_updates = new soar_module::sqlite_statement(new_db, "SELECT * FROM epmem_potential_interval_updates WHERE finished_right=1 ORDER BY w_id_left");
    add(select_updates);//when this is true, can calculate once per time_id pair the weight. also, can avoid doing a "select" and within the database update the weight and total counts.
    update_observed_before_relations = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_interval_relations (w_id_left,w_id_right,relation,weight,total, weight_norm) "
            "SELECT p.w_id_left, p.w_id_right, p.relation, "
            "(CAST((tr.start_episode_id-tl.start_episode_id) AS REAL)/(tl.end_episode_id-tl.start_episode_id+1.0)+CAST((tr.end_episode_id-tl.end_episode_id) AS REAL)/(tr.end_episode_id-tr.start_episode_id+1.0))/2.0,1.0,(CAST((tr.start_episode_id-tl.start_episode_id) AS REAL)/(tl.end_episode_id-tl.start_episode_id+1.0)+CAST((tr.end_episode_id-tl.end_episode_id) AS REAL)/(tr.end_episode_id-tr.start_episode_id+1.0))/2.0 "
            "FROM epmem_potential_interval_updates p LEFT JOIN epmem_times tl ON p.time_id_left=tl.time_id LEFT JOIN epmem_times tr ON p.time_id_right=tr.time_id WHERE p.finished_right=1 "
            "ON CONFLICT (w_id_left,w_id_right,relation) DO UPDATE SET weight=weight+excluded.weight,total=total+1.0, weight_norm=(weight+excluded.weight)/(total+1.0)");
    add(update_observed_before_relations);//We need a query that takes the weight associated with a given pair of time intervals and increments the weight in the interval relation between grounded elements
    //We also need a query or trigger before this that, for all of the finished_right=TRUE elements calculates the weight update for that pair of time_ids
    //may instead keep a time id characterization in ram (left, middle, right -> weight)-map and instead of time_ids, keep interval bounds explicitly (as they are already integers).

    /*The below can't be initialized here and then called by the usual "prepare" loop because it won't always be the case that smem_db exists. Instead, these must be initialized when smem_db is attached.*/

    //make_adds_into_ltis = new soar_module::sqlite_statement(new_db, "INSERT INTO smem_db.smem_lti (lti_id, total_augmentations, activation_base_level, activations_total, activations_last, activations_first, activation_spread, activation_value, lti_augmentations) SELECT -ja.w_id,0,0,0,0,0,0,0,0 FROM epmem_wmes_just_added ja");
    make_adds_into_ltis = new soar_module::sqlite_statement(new_db, "SELECT w_id FROM epmem_wmes_just_added");
    add(make_adds_into_ltis);//I don't know if negative lti_ids will break everything, but if they don't, it's a very easy way to give epmem a special space of lti numbers.

    record_lti_id_for_w_id = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_w_id_to_lti_id (w_id, lti_id) VALUES (?,?)");
    add(record_lti_id_for_w_id);

    //todo: is slow because sa version of smem_augmentations doesn't index properly within the join -- needed to index on the value_constant_s_id within the join. should be good now. still slow, but not horrible.
    update_smem_edges = new soar_module::sqlite_statement(new_db,"UPDATE smem_augmentations SET edge_weight=ir.weight_norm FROM "//right now, we can identify the row for epmem_interval_relations by index, but the weight_norm comes from the real row, not an index.
            "epmem_potential_interval_updates iu INNER JOIN epmem_interval_relations ir INNER JOIN epmem_w_id_to_lti_id wll INNER JOIN epmem_w_id_to_lti_id wl INNER JOIN smem_augmentations sa ON iu.w_id_left=ir.w_id_left AND iu.w_id_right=ir.w_id_right AND iu.finished_right=1 AND sa.lti_id=wl.lti_id AND wl.lti_id=ir.w_id_left AND sa.value_lti_id=wll.lti_id AND wll.lti_id=ir.w_id_right AND sa.value_constant_s_id=" SMEM_AUGMENTATIONS_NULL_STR " WHERE smem_augmentations.lti_id=sa.lti_id AND smem_augmentations.value_constant_s_id=" SMEM_AUGMENTATIONS_NULL_STR " AND sa.value_lti_id=smem_augmentations.value_lti_id");
    add(update_smem_edges);//updates all of the changed edge weights within the smem store.//udpated to use the lti_id_stored in the epmem_w_id_to_lti table.


    select_new_relations = new soar_module::sqlite_statement(new_db, "SELECT left_lti.lti_id, right_lti.lti_id, ja.weight FROM epmem_relations_just_added ja INNER JOIN epmem_w_id_to_lti_id left_lti INNER JOIN epmem_w_id_to_lti_id right_lti ON left_lti.w_id=ja.w_id_left AND right_lti.w_id=ja.w_id_right");
    add(select_new_relations);//since the point of this one is to later insert the relevant edge into the smem database, it should now return the lti_ids instead of the w_ids.

    delete_brand_new_relation_adds = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_relations_just_added");
    add(delete_brand_new_relation_adds);

    delete_brand_new_adds = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_wmes_just_added");
    add(delete_brand_new_adds);

    finish_updates = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_potential_interval_updates WHERE finished_right=1");
    add(finish_updates);

    //add_interval_data = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_intervals (w_id,time_id,surprise) VALUES (?,?,?)");
    //add(add_interval_data);
    //epmem_wmes_index (w_id INTEGER PRIMARY KEY AUTOINCREMENT, type INTEGER, w_type_based_id)");
    add_interval_data = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_intervals (w_id,time_id,surprise,is_now,start_episode_id) SELECT i.w_id,?,?,1,? FROM epmem_wmes_index i WHERE i.type=? AND i.w_type_based_id=?");
    add(add_interval_data);

    find_now_interval = new soar_module::sqlite_statement(new_db, "SELECT i.interval_id FROM epmem_intervals i INNER JOIN (SELECT x.w_id FROM epmem_wmes_index x WHERE x.type=? AND x.w_type_based_id=?) w ON i.w_id=w.w_id WHERE i.is_now");
    add(find_now_interval);

    update_interval_data = new soar_module::sqlite_statement(new_db, "UPDATE epmem_intervals SET time_id=?,is_now=0 WHERE interval_id=?");
    add(update_interval_data); //I'm gonna make an in-memory map between those things which are current in "now" tables and their existing interval_ids.
    //also going to make an in-memory map that is updated each cycle that associates this-cycle-used time intervals with their time_ids (so that they don't need to be looked up each instance during a cycle, since many elements with share time_ids).

    update_interval_surprise = new soar_module::sqlite_statement(new_db, "UPDATE epmem_intervals SET surprise=(1.0-(1.0*sc.kinda_spread/(?*1.0))) FROM epmem_w_id_to_lti_id AS wl INNER JOIN (SELECT sum(num_appearances_i_j/num_appearances) AS kinda_spread, lti_id FROM smem_current_spread GROUP BY lti_id) AS sc ON sc.lti_id=wl.lti_id WHERE epmem_intervals.w_id=wl.w_id");//"UPDATE epmem_intervals SET surprise=? WHERE interval_id=?");//can instead make into a select from now with start_id=actually_just_now joined with smem spread.
    add(update_interval_surprise);//basically, an update from join between "now" and smem spread.//todo is also kinda slow, maybe compare to something that loops externally instead of being wholly in-database.
    //probably want to transform data that's within the smem_current_spread table because it has the raw numbers used in any spread calculation without the weirdness.


    add_node = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_nodes (n_id,lti_id) VALUES (?,?)");
    add(add_node);

    update_node = new soar_module::sqlite_statement(new_db, "INSERT OR REPLACE INTO epmem_nodes (n_id, lti_id) VALUES (?,?)");//"UPDATE epmem_nodes SET lti_id=? where n_id=?");
    add(update_node);

    //epmem_wmes_index (w_id INTEGER PRIMARY KEY AUTOINCREMENT, type INTEGER, w_type_based_id)
    add_epmem_wmes = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_index (type, w_type_based_id) VALUES (?,?)");
    add(add_epmem_wmes);


    add_epmem_wmes_constant_now = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_constant_now (wc_id,start_episode_id) VALUES (?,?)");
    add(add_epmem_wmes_constant_now);

    add_epmem_wmes_float_now = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_float_now (wf_id,start_episode_id) VALUES (?,?)");
    add(add_epmem_wmes_float_now);

    find_time_epmem_wmes_float_now = new soar_module::sqlite_statement(new_db, "SELECT start_episode_id FROM epmem_wmes_float_now WHERE wf_id=?");
    add(find_time_epmem_wmes_float_now);

    delete_epmem_wmes_constant_now = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_wmes_constant_now WHERE wc_id=?");
    add(delete_epmem_wmes_constant_now);

    delete_epmem_wmes_float_now = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_wmes_float_now WHERE wf_id=?");
    add(delete_epmem_wmes_float_now);

    add_epmem_wmes_constant_point = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_constant_point (wc_id,episode_id) VALUES (?,?)");
    add(add_epmem_wmes_constant_point);

    add_epmem_wmes_float_point = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_float_point (wf_id,episode_id) VALUES (?,?)");
    add(add_epmem_wmes_float_point);

    add_epmem_wmes_constant_range = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_constant_range (rit_id,start_episode_id,end_episode_id,wc_id) VALUES (?,?,?,?)");
    add(add_epmem_wmes_constant_range);

    add_epmem_wmes_float_range = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_float_range (start_episode_id,end_episode_id,wf_id) VALUES (?,?,?)");
    add(add_epmem_wmes_float_range);

    add_epmem_wmes_float = new soar_module::sqlite_statement(new_db, "INSERT or IGNORE INTO epmem_wmes_float (parent_n_id,attribute_s_id,direction) VALUES (?,?,?)");
    add(add_epmem_wmes_float);

    find_epmem_wmes_float = new soar_module::sqlite_statement(new_db, "SELECT wf_id FROM epmem_wmes_float WHERE parent_n_id=? AND attribute_s_id=? AND direction=?");
    add(find_epmem_wmes_float);

    add_epmem_wmes_constant = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_constant (parent_n_id,attribute_s_id,value_s_id) VALUES (?,?,?)");
    add(add_epmem_wmes_constant);

    find_epmem_wmes_constant = new soar_module::sqlite_statement(new_db, "SELECT wc_id FROM epmem_wmes_constant WHERE parent_n_id=? AND attribute_s_id=? AND value_s_id=?");
    add(find_epmem_wmes_constant);

    add_epmem_wmes_identifier_now = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_identifier_now (wi_id,start_episode_id,lti_id) VALUES (?,?,?)");
    add(add_epmem_wmes_identifier_now);

    delete_epmem_wmes_identifier_now = new soar_module::sqlite_statement(new_db, "DELETE FROM epmem_wmes_identifier_now WHERE wi_id=?");
    add(delete_epmem_wmes_identifier_now);

    add_epmem_wmes_identifier_point = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_identifier_point (wi_id,episode_id,lti_id) VALUES (?,?,?)");
    add(add_epmem_wmes_identifier_point);

    add_epmem_wmes_identifier_range = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_identifier_range (rit_id,start_episode_id,end_episode_id,wi_id,lti_id) VALUES (?,?,?,?,?)");
    add(add_epmem_wmes_identifier_range);

    add_epmem_wmes_identifier = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_identifier (parent_n_id,attribute_s_id,child_n_id,last_episode_id) VALUES (?,?,?,?)");
    add(add_epmem_wmes_identifier);

    find_epmem_wmes_identifier = new soar_module::sqlite_statement(new_db, "SELECT wi_id, child_n_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=?");
    add(find_epmem_wmes_identifier);

    find_epmem_wmes_identifier_shared = new soar_module::sqlite_statement(new_db, "SELECT wi_id, child_n_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=? AND child_n_id=?");
    add(find_epmem_wmes_identifier_shared);

    valid_episode = new soar_module::sqlite_statement(new_db, "SELECT COUNT(*) AS ct FROM epmem_episodes WHERE episode_id=?");
    add(valid_episode);

    next_episode = new soar_module::sqlite_statement(new_db, "SELECT episode_id FROM epmem_episodes WHERE episode_id>? ORDER BY episode_id ASC LIMIT 1");
    add(next_episode);

    prev_episode = new soar_module::sqlite_statement(new_db, "SELECT episode_id FROM epmem_episodes WHERE episode_id<? ORDER BY episode_id DESC LIMIT 1");
    add(prev_episode);

    get_wmes_with_constant_values = new soar_module::sqlite_statement(new_db,
            "SELECT f.wc_id, f.parent_n_id, f.attribute_s_id, f.value_s_id "
            "FROM epmem_wmes_constant f "
            "WHERE f.wc_id IN "
            "(SELECT n.wc_id FROM epmem_wmes_constant_now n WHERE n.start_episode_id<= ? UNION ALL "
            "SELECT p.wc_id FROM epmem_wmes_constant_point p WHERE p.episode_id=? UNION ALL "
            "SELECT e1.wc_id FROM epmem_wmes_constant_range e1, epmem_rit_left_nodes lt WHERE e1.rit_id=lt.rit_min AND e1.end_episode_id >= ? UNION ALL "
            "SELECT e2.wc_id FROM epmem_wmes_constant_range e2, epmem_rit_right_nodes rt WHERE e2.rit_id = rt.rit_id AND e2.start_episode_id <= ?) "
            "ORDER BY f.wc_id ASC", new_agent->EpMem->epmem_timers->ncb_node);
    add(get_wmes_with_constant_values);


    //constant to float in this. I haven't done the conversion. //TODO I may not ever replicate because the changes to floats are about a different kind of query, so leaving the old get_wmes query as-is may be fine.
    /*get_wmes_with_constant_values = new soar_module::sqlite_statement(new_db,
                "SELECT f.wc_id, f.parent_n_id, f.attribute_s_id, f.value_s_id "
                "FROM epmem_wmes_constant f "
                "WHERE f.wc_id IN "
                "(SELECT n.wc_id FROM epmem_wmes_constant_now n WHERE n.start_episode_id<= ? UNION ALL "
                "SELECT p.wc_id FROM epmem_wmes_constant_point p WHERE p.episode_id=? UNION ALL "
                "SELECT e1.wc_id FROM epmem_wmes_constant_range e1, epmem_rit_left_nodes lt WHERE e1.rit_id=lt.rit_min AND e1.end_episode_id >= ? UNION ALL "
                "SELECT e2.wc_id FROM epmem_wmes_constant_range e2, epmem_rit_right_nodes rt WHERE e2.rit_id = rt.rit_id AND e2.start_episode_id <= ?) "
                "ORDER BY f.wc_id ASC", new_agent->EpMem->epmem_timers->ncb_node);
    add(get_wmes_with_constant_values);*/

    get_wmes_with_identifier_values = new soar_module::sqlite_statement(new_db,
            "WITH timetables AS ( "
            "SELECT n.wi_id, n.lti_id FROM epmem_wmes_identifier_now n WHERE n.start_episode_id<= ? UNION ALL "
            "SELECT p.wi_id, p.lti_id FROM epmem_wmes_identifier_point p WHERE p.episode_id = ? UNION ALL "
            "SELECT e1.wi_id, e1.lti_id FROM epmem_wmes_identifier_range e1, epmem_rit_left_nodes lt WHERE e1.rit_id=lt.rit_min AND e1.end_episode_id >= ? UNION ALL "
            "SELECT e2.wi_id, e2.lti_id FROM epmem_wmes_identifier_range e2, epmem_rit_right_nodes rt WHERE e2.rit_id = rt.rit_id AND e2.start_episode_id <= ?) "
            "SELECT f.parent_n_id, f.attribute_s_id, f.child_n_id, n.lti_id, f.wi_id FROM epmem_wmes_identifier f, timetables n WHERE f.wi_id=n.wi_id "
            "ORDER BY f.parent_n_id ASC, f.child_n_id ASC", new_agent->EpMem->epmem_timers->ncb_edge);
            /*"SELECT f.parent_n_id, f.attribute_s_id, f.child_n_id, n.lti_id "
            "FROM epmem_wmes_identifier f, epmem_nodes n "
            "WHERE (f.child_n_id=n.n_id) AND f.wi_id IN "
            "(SELECT n.wi_id FROM epmem_wmes_identifier_now n WHERE n.start_episode_id<= ? UNION ALL "
            "SELECT p.wi_id FROM epmem_wmes_identifier_point p WHERE p.episode_id = ? UNION ALL "
            "SELECT e1.wi_id FROM epmem_wmes_identifier_range e1, epmem_rit_left_nodes lt WHERE e1.rit_id=lt.rit_min AND e1.end_episode_id >= ? UNION ALL "
            "SELECT e2.wi_id FROM epmem_wmes_identifier_range e2, epmem_rit_right_nodes rt WHERE e2.rit_id = rt.rit_id AND e2.start_episode_id <= ?) "
            "ORDER BY f.parent_n_id ASC, f.child_n_id ASC", new_agent->EpMem->epmem_timers->ncb_edge);*/
    add(get_wmes_with_identifier_values);

    update_epmem_wmes_identifier_last_episode_id = new soar_module::sqlite_statement(new_db, "UPDATE epmem_wmes_identifier SET last_episode_id=? WHERE wi_id=?");
    add(update_epmem_wmes_identifier_last_episode_id);

    //

    get_single_wcid_info = new soar_module::sqlite_statement(new_db, "SELECT parent_n_id,attribute_s_id,value_s_id FROM epmem_wmes_constant WHERE wc_id=?");
	add(get_single_wcid_info);
	//actually, want to join with the different tables that store the values for these int labels/pointers and return the real values.
	//Actually, screw that guy (still me), let's just do it the lazy way because it's late and I can't think and this is a user-initiated print function, so
	//who cares about performance. (I will regret that.)

	get_single_wiid_info = new soar_module::sqlite_statement(new_db, "SELECT parent_n_id,attribute_s_id,child_n_id FROM epmem_wmes_identifier WHERE wi_id=?");
	add(get_single_wiid_info);

	get_constant = new soar_module::sqlite_statement(new_db, "SELECT cast(symbol_value as text) FROM epmem_symbols_float f WHERE f.s_id=? UNION SELECT cast(symbol_value as text) FROM epmem_symbols_string s WHERE s.s_id=? UNION SELECT cast(symbol_value as text) FROM epmem_symbols_integer i WHERE i.s_id=?");
	add(get_constant);


    // init statement pools
    {
        int j, k, m;

        const char* epmem_find_edge_queries[2][2] =
        {
            {
                "SELECT wc_id, value_s_id, ? FROM epmem_wmes_constant WHERE parent_n_id=? AND attribute_s_id=?",
                "SELECT wc_id, value_s_id, ? FROM epmem_wmes_constant WHERE parent_n_id=? AND attribute_s_id=? AND value_s_id=?"
            },
            {
                "SELECT wi_id, child_n_id, last_episode_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=? AND ?<last_episode_id ORDER BY last_episode_id DESC",
                "SELECT wi_id, child_n_id, last_episode_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=? AND child_n_id=? AND ?<last_episode_id"
            }
        };

        for (j = EPMEM_RIT_STATE_NODE; j <= EPMEM_RIT_STATE_EDGE; j++)
        {
            for (k = 0; k <= 1; k++)
            {
                pool_find_edge_queries[ j ][ k ] = new soar_module::sqlite_statement_pool(new_agent, new_db, epmem_find_edge_queries[ j ][ k ]);
            }
        }

        //

        // Because the DB records when things are /inserted/, we need to offset
        // the start by 1 to /remove/ them at the right time. Ditto to even
        // include those intervals correctly
        const char* epmem_find_interval_queries[2][2][3] =
        {
            {
                {
                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_constant_range e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_constant_now e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT (e.episode_id - 1) AS start FROM epmem_wmes_constant_point e WHERE e.wc_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                },
                {
                    "SELECT e.end_episode_id AS end FROM epmem_wmes_constant_range e WHERE e.wc_id=? AND e.end_episode_id>0 AND e.start_episode_id<=? ORDER BY e.end_episode_id DESC",
                    "SELECT ? AS end FROM epmem_wmes_constant_now e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT e.episode_id AS end FROM epmem_wmes_constant_point e WHERE e.wc_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                }
            },
            {
                {
                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_identifier_range e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_identifier_now e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT (e.episode_id - 1) AS start FROM epmem_wmes_identifier_point e WHERE e.wi_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                },
                {
                    "SELECT e.end_episode_id AS end FROM epmem_wmes_identifier_range e WHERE e.wi_id=? AND e.end_episode_id>0 AND e.start_episode_id<=? ORDER BY e.end_episode_id DESC",
                    "SELECT ? AS end FROM epmem_wmes_identifier_now e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                    "SELECT e.episode_id AS end FROM epmem_wmes_identifier_point e WHERE e.wi_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                }
            },
        };

        for (j = EPMEM_RIT_STATE_NODE; j <= EPMEM_RIT_STATE_EDGE; j++)
        {
            for (k = EPMEM_RANGE_START; k <= EPMEM_RANGE_END; k++)
            {
                for (m = EPMEM_RANGE_EP; m <= EPMEM_RANGE_POINT; m++)
                {
                    pool_find_interval_queries[ j ][ k ][ m ] = new soar_module::sqlite_statement_pool(new_agent, new_db, epmem_find_interval_queries[ j ][ k ][ m ]);
                }
            }
        }

        //

        pool_dummy = new soar_module::sqlite_statement_pool(new_agent, new_db, "SELECT ? as start");
    }
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// WME Functions (epmem::wmes)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/***************************************************************************
 * Function     : epmem_get_augs_of_id
 * Author       : Nate Derbinsky
 * Notes        : This routine gets all wmes rooted at an id.
 **************************************************************************/
epmem_wme_list* epmem_get_augs_of_id(Symbol* id, tc_number tc)
{
    slot* s;
    wme* w;
    epmem_wme_list* return_val = new epmem_wme_list;

    // augs only exist for identifiers
    if ((id->is_sti()) &&
            (id->tc_num != tc))
    {
        id->tc_num = tc;

        // impasse wmes
        for (w = id->id->impasse_wmes; w != NIL; w = w->next)
        {
            return_val->push_back(w);
        }

        // input wmes
        for (w = id->id->input_wmes; w != NIL; w = w->next)
        {
            return_val->push_back(w);
        }

        // regular wmes
        for (s = id->id->slots; s != NIL; s = s->next)
        {
            for (w = s->wmes; w != NIL; w = w->next)
            {
                return_val->push_back(w);
            }

            for (w = s->acceptable_preference_wmes; w != NIL; w = w->next)
            {
                return_val->push_back(w);
            }
        }
    }

    return return_val;
}

inline void _epmem_process_buffered_wme_list(agent* thisAgent, Symbol* state, wme_set& cue_wmes, symbol_triple_list& my_list, preference_list* epmem_wmes)
{
    if (my_list.empty())
    {
        return;
    }

    instantiation* inst = make_architectural_instantiation_for_memory_system(thisAgent, state, &cue_wmes, &my_list, false);

    for (preference* pref = inst->preferences_generated; pref;)
    {
        // add the preference to temporary memory
        if (add_preference_to_tm(thisAgent, pref))
        {
            // add to the list of preferences to be removed
            // when the goal is removed
            insert_at_head_of_dll(state->id->preferences_from_goal, pref, all_of_goal_next, all_of_goal_prev);
            pref->on_goal_list = true;

            if (epmem_wmes)
            {
                // if this is a meta wme, then it is completely local
                // to the state and thus we will manually remove it
                // (via preference removal) when the time comes
                epmem_wmes->push_back(pref);
            }
        }
        else
        {
			if (pref->reference_count == 0)
			{
				preference* previous = pref;
				pref = pref->inst_next;
				possibly_deallocate_preference_and_clones(thisAgent, previous, true);
				continue;
			}
        }

		pref = pref->inst_next;
    }

    if (!epmem_wmes)
    {
        instantiation* my_justification_list = NIL;
        if (my_justification_list != NIL)
        {
            preference* just_pref = NIL;
            instantiation* next_justification = NIL;

            for (instantiation* my_justification = my_justification_list;
                    my_justification != NIL;
                    my_justification = next_justification)
            {
                next_justification = my_justification->next;

                if (my_justification->in_ms)
                {
                    insert_at_head_of_dll(my_justification->prod->instantiations, my_justification, next, prev);
                }

                for (just_pref = my_justification->preferences_generated; just_pref != NIL; just_pref = just_pref->inst_next)
                {
                    if (add_preference_to_tm(thisAgent, just_pref))
                    {
                        if (wma_enabled(thisAgent))
                        {
                            wma_activate_wmes_in_pref(thisAgent, just_pref);
                        }
                    }
                    else
                    {
                        preference_add_ref(just_pref);
                        preference_remove_ref(thisAgent, just_pref);
                    }
                }
            }
        }
    }
}

inline void epmem_process_buffered_wmes(agent* thisAgent, Symbol* state, wme_set& cue_wmes, symbol_triple_list& meta_wmes, symbol_triple_list& retrieval_wmes)
{
    _epmem_process_buffered_wme_list(thisAgent, state, cue_wmes, meta_wmes, state->id->epmem_info->epmem_wmes);
    _epmem_process_buffered_wme_list(thisAgent, state, cue_wmes, retrieval_wmes, NULL);
}

inline void epmem_buffer_add_wme(agent* thisAgent, symbol_triple_list& my_list, Symbol* id, Symbol* attr, Symbol* value)
{
    my_list.push_back(new symbol_triple(id, attr, value));

    thisAgent->symbolManager->symbol_add_ref(id);
    thisAgent->symbolManager->symbol_add_ref(attr);
    thisAgent->symbolManager->symbol_add_ref(value);
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Variable Functions (epmem::var)
//
// Variables are key-value pairs stored in the database
// that are necessary to maintain a store between
// multiple runs of Soar.
//
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/***************************************************************************
 * Function     : epmem_get_variable
 * Author       : Nate Derbinsky
 * Notes        : Gets an EpMem variable from the database
 **************************************************************************/
bool epmem_get_variable(agent* thisAgent, epmem_variable_key variable_id, int64_t* variable_value)
{
    soar_module::exec_result status;
    soar_module::sqlite_statement* var_get = thisAgent->EpMem->epmem_stmts_common->var_get;

    var_get->bind_int(1, variable_id);
    status = var_get->execute();

    if (status == soar_module::row)
    {
        (*variable_value) = var_get->column_int(0);
    }

    var_get->reinitialize();

    return (status == soar_module::row);
}

/***************************************************************************
 * Function     : epmem_set_variable
 * Author       : Nate Derbinsky
 * Notes        : Sets an EpMem variable in the database
 **************************************************************************/
void epmem_set_variable(agent* thisAgent, epmem_variable_key variable_id, int64_t variable_value)
{
    soar_module::sqlite_statement* var_set = thisAgent->EpMem->epmem_stmts_common->var_set;

    var_set->bind_int(1, variable_id);
    var_set->bind_int(2, variable_value);

    var_set->execute(soar_module::op_reinit);
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// RIT Functions (epmem::rit)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/***************************************************************************
 * Function     : epmem_rit_fork_node
 * Author       : Nate Derbinsky
 * Notes        : Implements the forkNode function of RIT
 **************************************************************************/
int64_t epmem_rit_fork_node(int64_t lower, int64_t upper, bool /*bounds_offset*/, int64_t* step_return, epmem_rit_state* rit_state)
{
    // never called
    /*if ( !bounds_offset )
      {
      int64_t offset = rit_state->offset.stat->get_value();

      lower = ( lower - offset );
      upper = ( upper - offset );
      }*/

    // descend the tree down to the fork node
    int64_t node = EPMEM_RIT_ROOT;
    if (upper < EPMEM_RIT_ROOT)
    {
        node = rit_state->leftroot.stat->get_value();
    }
    else if (lower > EPMEM_RIT_ROOT)
    {
        node = rit_state->rightroot.stat->get_value();
    }

    int64_t step;
    for (step = (((node >= 0) ? (node) : (-1 * node)) / 2); step >= 1; step /= 2)
    {
        if (upper < node)
        {
            node -= step;
        }
        else if (node < lower)
        {
            node += step;
        }
        else
        {
            break;
        }
    }

    // never used
    // if ( step_return != NULL )
    {
        (*step_return) = step;
    }

    return node;
}

/***************************************************************************
 * Function     : epmem_rit_clear_left_right
 * Author       : Nate Derbinsky
 * Notes        : Clears the left/right relations populated during prep
 **************************************************************************/
void epmem_rit_clear_left_right(agent* thisAgent)
{
    thisAgent->EpMem->epmem_stmts_common->rit_truncate_left->execute(soar_module::op_reinit);
    thisAgent->EpMem->epmem_stmts_common->rit_truncate_right->execute(soar_module::op_reinit);
}

/***************************************************************************
 * Function     : epmem_rit_add_left
 * Author       : Nate Derbinsky
 * Notes        : Adds a range to the left relation
 **************************************************************************/
void epmem_rit_add_left(agent* thisAgent, epmem_time_id min, epmem_time_id max)
{
    thisAgent->EpMem->epmem_stmts_common->rit_add_left->bind_int(1, min);
    thisAgent->EpMem->epmem_stmts_common->rit_add_left->bind_int(2, max);
    thisAgent->EpMem->epmem_stmts_common->rit_add_left->execute(soar_module::op_reinit);
}

/***************************************************************************
 * Function     : epmem_rit_add_right
 * Author       : Nate Derbinsky
 * Notes        : Adds a node to the to the right relation
 **************************************************************************/
void epmem_rit_add_right(agent* thisAgent, epmem_time_id id)
{
    thisAgent->EpMem->epmem_stmts_common->rit_add_right->bind_int(1, id);
    thisAgent->EpMem->epmem_stmts_common->rit_add_right->execute(soar_module::op_reinit);
}

/***************************************************************************
 * Function     : epmem_rit_prep_left_right
 * Author       : Nate Derbinsky
 * Notes        : Implements the computational components of the RIT
 *                query algorithm
 **************************************************************************/
void epmem_rit_prep_left_right(agent* thisAgent, int64_t lower, int64_t upper, epmem_rit_state* rit_state)
{
    ////////////////////////////////////////////////////////////////////////////
    rit_state->timer->start();
    ////////////////////////////////////////////////////////////////////////////

    int64_t offset = rit_state->offset.stat->get_value();
    int64_t node, step;
    int64_t left_node, left_step;
    int64_t right_node, right_step;

    lower = (lower - offset);
    upper = (upper - offset);

    // auto add good range
    epmem_rit_add_left(thisAgent, lower, upper);

    // go to fork
    node = EPMEM_RIT_ROOT;
    step = 0;
    if ((lower > node) || (upper < node))
    {
        if (lower > node)
        {
            node = rit_state->rightroot.stat->get_value();
            epmem_rit_add_left(thisAgent, EPMEM_RIT_ROOT, EPMEM_RIT_ROOT);
        }
        else
        {
            node = rit_state->leftroot.stat->get_value();
            epmem_rit_add_right(thisAgent, EPMEM_RIT_ROOT);
        }

        for (step = (((node >= 0) ? (node) : (-1 * node)) / 2); step >= 1; step /= 2)
        {
            if (lower > node)
            {
                epmem_rit_add_left(thisAgent, node, node);
                node += step;
            }
            else if (upper < node)
            {
                epmem_rit_add_right(thisAgent, node);
                node -= step;
            }
            else
            {
                break;
            }
        }
    }

    // go left
    left_node = node - step;
    for (left_step = (step / 2); left_step >= 1; left_step /= 2)
    {
        if (lower == left_node)
        {
            break;
        }
        else if (lower > left_node)
        {
            epmem_rit_add_left(thisAgent, left_node, left_node);
            left_node += left_step;
        }
        else
        {
            left_node -= left_step;
        }
    }

    // go right
    right_node = node + step;
    for (right_step = (step / 2); right_step >= 1; right_step /= 2)
    {
        if (upper == right_node)
        {
            break;
        }
        else if (upper < right_node)
        {
            epmem_rit_add_right(thisAgent, right_node);
            right_node -= right_step;
        }
        else
        {
            right_node += right_step;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    rit_state->timer->stop();
    ////////////////////////////////////////////////////////////////////////////
}

/***************************************************************************
 * Function     : epmem_rit_insert_interval
 * Author       : Nate Derbinsky
 * Notes        : Inserts an interval in the RIT
 **************************************************************************/
void epmem_rit_insert_interval(agent* thisAgent, int64_t lower, int64_t upper, epmem_node_id id, epmem_rit_state* rit_state, int64_t lti_id = 0)
{
    // initialize offset
    int64_t offset = rit_state->offset.stat->get_value();
    if (offset == EPMEM_RIT_OFFSET_INIT)
    {
        offset = lower;

        // update database
        epmem_set_variable(thisAgent, rit_state->offset.var_key, offset);

        // update stat
        rit_state->offset.stat->set_value(offset);
    }

    // get node
    int64_t node;
    {
        int64_t left_root = rit_state->leftroot.stat->get_value();
        int64_t right_root = rit_state->rightroot.stat->get_value();
        int64_t min_step = rit_state->minstep.stat->get_value();

        // shift interval
        int64_t l = (lower - offset);
        int64_t u = (upper - offset);

        // update left_root
        if ((u < EPMEM_RIT_ROOT) && (l <= (2 * left_root)))
        {
            left_root = static_cast<int64_t>(pow(-2.0, floor(log(static_cast<double>(-l)) / EPMEM_LN_2)));

            // update database
            epmem_set_variable(thisAgent, rit_state->leftroot.var_key, left_root);

            // update stat
            rit_state->leftroot.stat->set_value(left_root);
        }

        // update right_root
        if ((l > EPMEM_RIT_ROOT) && (u >= (2 * right_root)))
        {
            right_root = static_cast<int64_t>(pow(2.0, floor(log(static_cast<double>(u)) / EPMEM_LN_2)));

            // update database
            epmem_set_variable(thisAgent, rit_state->rightroot.var_key, right_root);

            // update stat
            rit_state->rightroot.stat->set_value(right_root);
        }

        // update min_step
        int64_t step;
        node = epmem_rit_fork_node(l, u, true, &step, rit_state);

        if ((node != EPMEM_RIT_ROOT) && (step < min_step))
        {
            min_step = step;

            // update database
            epmem_set_variable(thisAgent, rit_state->minstep.var_key, min_step);

            // update stat
            rit_state->minstep.stat->set_value(min_step);
        }
    }

    // perform insert

    /*std::ostringstream temp;
    temp << "\nInserting element with id: ";
    temp << id;
    temp << ", and lti_id: ";
    temp << static_cast<int64_t>(lti_id);//lti_id;
    temp << ".\n";
    std::string temp2 = temp.str();
    thisAgent->outputManager->print(temp2.c_str());*/

    rit_state->add_query->bind_int(1, node);
    rit_state->add_query->bind_int(2, lower);
    rit_state->add_query->bind_int(3, upper);
    rit_state->add_query->bind_int(4, id);
    //if (lti_id)
    //A horrible error was occuring where instead of defaulting to null in the absense of a given
    //lti_id, another value was used, which ended up effectively assigning lti status to absurd things
    //when they were recorded into the history for episodic memory.
    {
        rit_state->add_query->bind_int(5, (lti_id));
    }
    rit_state->add_query->execute(soar_module::op_reinit);
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Clean-Up Functions (epmem::clean)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

void epmem_clear_transient_structures(agent* thisAgent)
{
    epmem_parent_id_pool::iterator p;
    epmem_hashed_id_pool::iterator p_p;

    // de-allocate statement pools
    {
        int j, k, m;

        for (j = EPMEM_RIT_STATE_NODE; j <= EPMEM_RIT_STATE_EDGE; j++)
        {
            for (k = 0; k <= 1; k++)
            {
                delete thisAgent->EpMem->epmem_stmts_graph->pool_find_edge_queries[ j ][ k ];
            }
        }

        for (j = EPMEM_RIT_STATE_NODE; j <= EPMEM_RIT_STATE_EDGE; j++)
        {
            for (k = EPMEM_RANGE_START; k <= EPMEM_RANGE_END; k++)
            {
                for (m = EPMEM_RANGE_EP; m <= EPMEM_RANGE_POINT; m++)
                {
                    delete thisAgent->EpMem->epmem_stmts_graph->pool_find_interval_queries[ j ][ k ][ m ];
                }
            }
        }

        delete thisAgent->EpMem->epmem_stmts_graph->pool_dummy;
    }

    // de-allocate statements
    delete thisAgent->EpMem->epmem_stmts_common;
    delete thisAgent->EpMem->epmem_stmts_graph;

    // de-allocate id repository
    for (p = thisAgent->EpMem->epmem_id_repository->begin(); p != thisAgent->EpMem->epmem_id_repository->end(); p++)
    {
        for (p_p = p->second->begin(); p_p != p->second->end(); p_p++)
        {
            delete p_p->second;
        }

        delete p->second;
    }
    thisAgent->EpMem->epmem_id_repository->clear();
    thisAgent->EpMem->epmem_id_replacement->clear();

    // de-allocate id ref counts
    for (epmem_id_ref_counter::iterator rf_it = thisAgent->EpMem->epmem_id_ref_counts->begin(); rf_it != thisAgent->EpMem->epmem_id_ref_counts->end(); rf_it++)
    {
        delete rf_it->second;
    }
    thisAgent->EpMem->epmem_id_ref_counts->clear();
    thisAgent->EpMem->epmem_wme_adds->clear();

}

/***************************************************************************
 * Function     : epmem_close
 * Author       : Nate Derbinsky
 * Notes        : Performs cleanup operations when the database needs
 *                to be closed (end soar, manual close, etc)
 **************************************************************************/
void epmem_close(agent* thisAgent)
{
    if (thisAgent->EpMem->epmem_db->get_status() == soar_module::connected)
    {
        print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Closing episodic memory database %s.\n", thisAgent->EpMem->epmem_params->path->get_value());
        // if lazy, commit
        if (thisAgent->EpMem->epmem_params->lazy_commit->get_value() == on)
        {
            thisAgent->EpMem->epmem_stmts_common->commit->execute(soar_module::op_reinit);
        }

        epmem_clear_transient_structures(thisAgent);

        // close the database
        thisAgent->EpMem->epmem_db->disconnect();
    }
}

void epmem_attach(agent* thisAgent)
{
    if (thisAgent->EpMem->epmem_db->get_status() == soar_module::disconnected)
    {
        epmem_init_db(thisAgent);
    }
}

/**
 * @name    epmem_reinit
 * @param   thisAgent
 * @brief   The function closes and then intializes the episodic memory
 *          database.  All data structures should be cleaned up and
 *          re-initialized properly, so this can be used for other database
 *          setting changes
 */
void epmem_reinit_cmd(agent* thisAgent)
{
    epmem_close(thisAgent);
    epmem_init_db(thisAgent);
}


void epmem_reinit(agent* thisAgent)
{
    if (thisAgent->EpMem->epmem_db->get_status() == soar_module::connected)
    {
        if ((thisAgent->EpMem->epmem_params->database->get_value() == epmem_param_container::memory))
        {
            if (thisAgent->EpMem->epmem_params->append_db->get_value() == off)
            {
                print_sysparam_trace(thisAgent, 0, "Episodic memory re-initializing.\n");
            }
            else
            {
                print_sysparam_trace(thisAgent, 0, "Note: Episodic memory can currently only append to an an on-disk database.  Ignoring append = on.\n");
                print_sysparam_trace(thisAgent, 0, "Episodic memory re-initializing.\n");
            }
        }
        else
        {
            print_sysparam_trace(thisAgent, 0, "Episodic memory re-initializing.\n");
        }
        epmem_close(thisAgent);
    }
}
/***************************************************************************
 * Function     : epmem_clear_result
 * Author       : Nate Derbinsky
 * Notes        : Removes any WMEs produced by EpMem resulting from
 *                a command
 **************************************************************************/
void epmem_clear_result(agent* thisAgent, Symbol* state)
{
    preference* pref;

    while (!state->id->epmem_info->epmem_wmes->empty())
    {
        pref = state->id->epmem_info->epmem_wmes->back();
        state->id->epmem_info->epmem_wmes->pop_back();

        if (pref->in_tm)
        {
            remove_preference_from_tm(thisAgent, pref);
        }
    }
}

/***************************************************************************
 * Function     : epmem_reset
 * Author       : Nate Derbinsky
 * Notes        : Performs cleanup when a state is removed
 **************************************************************************/
void epmem_reset(agent* thisAgent, Symbol* state)
{
    if (state == NULL)
    {
        state = thisAgent->top_goal;
    }

    while (state)
    {
        epmem_data* data = state->id->epmem_info;

        data->last_ol_time = 0;

        data->last_cmd_time = 0;
        data->last_cmd_count = 0;

        data->last_memory = EPMEM_MEMID_NONE;

        // this will be called after prefs from goal are already removed,
        // so just clear out result stack
        data->epmem_wmes->clear();

        state = state->id->lower_goal;
    }
}

void epmem_switch_db_mode(agent* thisAgent, std::string& buf, bool readonly)
{
    print_sysparam_trace(thisAgent, 0, buf.c_str());
    thisAgent->EpMem->epmem_db->disconnect();
    thisAgent->EpMem->epmem_params->database->set_value(epmem_param_container::memory);
    epmem_init_db(thisAgent, readonly);
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Initialization Functions (epmem::init)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

void epmem_attach_smem(agent* thisAgent)
{


    if (!thisAgent->EpMem->smem_connected && thisAgent->EpMem->epmem_params->smem_surprise->get_value() == on)
    {
        thisAgent->SMem->attach();//if smem not initialized, this will do it.
        //This is where we actually have epmem connect to the smem database
        {
            std::string sql_to_execute;
            std::string path_str;
            const char* smem_db_path;
            if (thisAgent->SMem->settings->database->get_value() == smem_param_container::memory)
            {
                path_str = "smem_";
                path_str.append(thisAgent->name);
                path_str.append("_db");
                smem_db_path = path_str.c_str();
            }
            else
            {
                smem_db_path = thisAgent->SMem->settings->path->get_value();
            }
            sql_to_execute = "ATTACH DATABASE '";
            sql_to_execute+= smem_db_path;
            sql_to_execute+= "' AS smem_db";
            //bool result = thisAgent->EpMem->epmem_db->sql_execute(sql_to_execute.c_str());
            soar_module::sqlite_statement* test_attach = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db,sql_to_execute.c_str());
            test_attach->prepare();
            test_attach->execute();
            test_attach->reinitialize();
            delete test_attach;
            soar_module::sqlite_statement* test_it_works = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, "SELECT * FROM smem_db.sqlite_master WHERE type='table'");
            test_it_works->prepare();
            bool whatever = false;
            while(test_it_works->execute() == soar_module::row)
            {
                std::string what_is_it = test_it_works->column_text(0);
                whatever= true;
            }
            assert(whatever);
            test_it_works->reinitialize();
            delete test_it_works;
        }
        thisAgent->EpMem->smem_connected = true;

        /*make_adds_into_ltis = new soar_module::sqlite_statement(new_db, "INSERT INTO smem_db.smem_lti (lti_id, total_augmentations, activation_base_level, activations_total, activations_last, activations_first, activation_spread, activation_value, lti_augmentations) SELECT (-ja.w_id,0,0,0,0,0,0,0,0) FROM epmem_wmes_just_added ja");
    add(make_adds_into_ltis);//I don't know if negative lti_ids will break everything, but if they don't, it's a very easy way to give epmem a special space of lti numbers.


    update_smem_edges = new soar_module::sqlite_statement(new_db,"UPDATE smem_db.smem_augmentations AS sa SET sa.edge_weight=ir.weight FROM "
            "sa INNER JOIN epmem_interval_relations ir INNER JOIN epmem_potential_interval_updates iu ON sa.lti_id=-ir.w_id_left AND sa.value_lti_id=-ir.w_id_right AND iu.w_id_left=ir.w_id_left AND iu.w_id_right=ir.w_id_right WHERE iu.finished_right");
    add(update_smem_edges);//updates all of the changed edge weights within the smem store.*/
       /* thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, "INSERT INTO smem_db.smem_lti (lti_id, total_augmentations, activation_base_level, activations_total, activations_last, activations_first, activation_spread, activation_value, lti_augmentations) SELECT (-ja.w_id,0,0,0,0,0,0,0,0) FROM epmem_wmes_just_added ja");
        thisAgent->EpMem->epmem_stmts_graph->add(thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis);
        thisAgent->EpMem->epmem_stmts_graph->update_smem_edges = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db,"UPDATE smem_db.smem_augmentations AS sa SET sa.edge_weight=ir.weight FROM "
                "sa INNER JOIN epmem_interval_relations ir INNER JOIN epmem_potential_interval_updates iu ON sa.lti_id=-ir.w_id_left AND sa.value_lti_id=-ir.w_id_right AND iu.w_id_left=ir.w_id_left AND iu.w_id_right=ir.w_id_right WHERE iu.finished_right");
        thisAgent->EpMem->epmem_stmts_graph->add(thisAgent->EpMem->epmem_stmts_graph->update_smem_edges);
        thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis->prepare();
        thisAgent->EpMem->epmem_stmts_graph->update_smem_edges->prepare();*/
    }
}


/***************************************************************************
 * Function     : epmem_init_db
 * Author       : Nate Derbinsky
 * Notes        : Opens the SQLite database and performs all
 *                initialization required for the current mode
 *
 *                The readonly param should only be used in
 *                experimentation where you don't want to alter
 *                previous database state.
 **************************************************************************/

void epmem_init_db(agent* thisAgent, bool readonly)
{
    if (thisAgent->EpMem->epmem_db->get_status() != soar_module::disconnected)
    {
        print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Cannot initialize episodic memory database.  It is already connected!");
        return;
    }


    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->init->start();
    ////////////////////////////////////////////////////////////////////////////

    const char* db_path;
    std::string filename_string;
    if (thisAgent->EpMem->epmem_params->database->get_value() == epmem_param_container::memory)
    {
        filename_string.append("file:epmem_");
        filename_string.append(thisAgent->name);
        filename_string.append("_db?mode=memory&cache=shared");
        db_path = filename_string.c_str();//Without making a big mess and ruining backwards compatibility,
        //I can have smem and epmem efficiently communicate with each other even when they are both in-memory databases by having them each be named in-memory databases,
        //allowing the separate epmem and smem sqlite instances containing each independent in-memory database to separately attach to the other's in-memory database.
        //This change requires that the same process run both smem and epmem's sqlite instances.
        print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Initializing episodic memory database in cpu memory.\n");
    }
    else
    {
        db_path = thisAgent->EpMem->epmem_params->path->get_value();
        print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Initializing episodic memory database at %s\n", db_path);
    }

    // attempt connection
    thisAgent->EpMem->epmem_db->connect(db_path,SQLITE_OPEN_URI | SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE);

    if (thisAgent->EpMem->epmem_db->get_status() == soar_module::problem)
    {
        print_sysparam_trace(thisAgent, 0, "Episodic memory database error: %s\n", thisAgent->EpMem->epmem_db->get_errmsg());
    }
    else
    {
        //need a statement that basically says "if not already attached, attach". Also, the database that opens second does all the lifting.
        //so, logic is to check that smem is already open and running and totally happy.
        //then, attach to that totally happy smem.
        //dumbdumb, can just do the attach when it is needed waaaaaay later.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
        //for that matter, literally call the other database's attach when you need it.

        epmem_time_id time_max;
        soar_module::sqlite_statement* temp_q = NULL;
        soar_module::sqlite_statement* temp_q2 = NULL;

        // If the database is on file, make sure the database contents use the current schema
        // If it does not, switch to memory-based database

        if (strcmp(db_path, "file:epmem_db?mode=memory&cache=shared")) // Only worry about database version if writing to disk
        {
            bool switch_to_memory, sql_is_new;
            std::string schema_version, version_error_message;

            switch_to_memory = true;

            if (thisAgent->EpMem->epmem_db->sql_is_new_db(sql_is_new))
            {
                if (sql_is_new)
                {
                    switch_to_memory = false;
                    print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "...episodic memory database is new.\n");
                }
                else
                {
                    // Check if table exists already
                    temp_q = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, "CREATE TABLE IF NOT EXISTS versions (system TEXT PRIMARY KEY,version_number TEXT)");
                    temp_q->prepare();
                    if (temp_q->get_status() == soar_module::ready)
                    {
                        if (thisAgent->EpMem->epmem_db->sql_simple_get_string("SELECT version_number FROM versions WHERE system = 'epmem_schema'", schema_version))
                        {
                            if (schema_version != EPMEM_SCHEMA_VERSION)   // Incompatible version
                            {
                                version_error_message.assign("...Error:  Cannot load episodic memory database with schema version ");
                                version_error_message.append(schema_version.c_str());
                                version_error_message.append(".\n...Please convert old database or start a new database by "
                                                             "setting a new database file path.\n...Switching to memory-based database.\n");
                            }
                            else     // Version is OK
                            {
                                print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "...version of episodic memory database ok.\n");
                                switch_to_memory = false;
                            }

                        }
                        else     // Some sort of error reading version info from version database
                        {
                            version_error_message.assign("...Error:  Cannot read version number from file-based episodic memory database.\n"
                                                         "...Switching to memory-based database.\n");
                        }
                    }
                    else     // Non-empty database exists with no version table.  Probably schema 1.0
                    {
                        version_error_message.assign("...Error:  Cannot load an episodic memory database with an old schema version.\n...Please convert "
                                                     "old database or start a new database by setting a new database file path.\n...Switching "
                                                     "to memory-based database.\n");
                    }
                    delete temp_q;
                    temp_q = NULL;
                }
            }
            else
            {
                version_error_message.assign("...Error:  Cannot read database meta info from file-based episodic memory database.\n"
                                             "...Switching to memory-based database.\n");
            }
            if (switch_to_memory)
            {
                // Memory mode will be set on, database will be disconnected to and then init_db
                // will be called again to reinitialize database.
                epmem_switch_db_mode(thisAgent, version_error_message, readonly);
                return;
            }
        }
        // apply performance options
        {
            // page_size
            {
                switch (thisAgent->EpMem->epmem_params->page_size->get_value())
                {
                    case (epmem_param_container::page_1k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 1024");
                        break;

                    case (epmem_param_container::page_2k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 2048");
                        break;

                    case (epmem_param_container::page_4k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 4096");
                        break;

                    case (epmem_param_container::page_8k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 8192");
                        break;

                    case (epmem_param_container::page_16k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 16384");
                        break;

                    case (epmem_param_container::page_32k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 32768");
                        break;

                    case (epmem_param_container::page_64k):
                        thisAgent->EpMem->epmem_db->sql_execute("PRAGMA page_size = 65536");
                        break;
                }
            }

            // cache_size
            {
                std::string cache_sql("PRAGMA cache_size = ");
                char* str = thisAgent->EpMem->epmem_params->cache_size->get_cstring();
                cache_sql.append(str);
                free(str);
                str = NULL;

                thisAgent->EpMem->epmem_db->sql_execute(cache_sql.c_str());
            }

            // optimization
            if (thisAgent->EpMem->epmem_params->opt->get_value() == epmem_param_container::opt_speed)
            {
                // synchronous - don't wait for writes to complete (can corrupt the db in case unexpected crash during transaction)
                thisAgent->EpMem->epmem_db->sql_execute("PRAGMA synchronous = OFF");

                // journal_mode - no atomic transactions (can result in database corruption if crash during transaction)
                thisAgent->EpMem->epmem_db->sql_execute("PRAGMA journal_mode = OFF");

                // locking_mode - no one else can view the database after our first write
                //thisAgent->EpMem->epmem_db->sql_execute("PRAGMA locking_mode = EXCLUSIVE");
            }
        }

        // point stuff
        epmem_time_id range_start;
        epmem_time_id time_last;

        // update validation count
        thisAgent->EpMem->epmem_validation++;

        // setup common structures/queries
        thisAgent->EpMem->epmem_stmts_common = new epmem_common_statement_container(thisAgent);
        thisAgent->EpMem->epmem_stmts_common->structure();
        epmem_attach_smem(thisAgent);
        thisAgent->EpMem->epmem_stmts_common->prepare();

        {
            // setup graph structures/queries
            thisAgent->EpMem->epmem_stmts_graph = new epmem_graph_statement_container(thisAgent);
            thisAgent->EpMem->epmem_stmts_graph->structure();
            thisAgent->EpMem->epmem_stmts_graph->prepare();

            // initialize range tracking
            thisAgent->EpMem->epmem_node_mins->clear();
            thisAgent->EpMem->epmem_node_maxes->clear();
            thisAgent->EpMem->epmem_node_removals->clear();

            thisAgent->EpMem->epmem_edge_mins->clear();
            thisAgent->EpMem->epmem_edge_maxes->clear();
            thisAgent->EpMem->epmem_edge_removals->clear();

            (*thisAgent->EpMem->epmem_id_repository)[ EPMEM_NODEID_ROOT ] = new epmem_hashed_id_pool;
            {
#ifdef USE_MEM_POOL_ALLOCATORS
                epmem_wme_set* wms_temp = new epmem_wme_set(std::less< wme* >(), soar_module::soar_memory_pool_allocator< wme* >());
#else
                epmem_wme_set* wms_temp = new epmem_wme_set();
#endif

                // voigtjr: Cast to wme* is necessary for compilation in VS10
                // Without it, it picks insert(int) instead of insert(wme*) and does not compile.
                wms_temp->insert(static_cast<wme*>(NULL));

                (*thisAgent->EpMem->epmem_id_ref_counts)[ EPMEM_NODEID_ROOT ] = wms_temp;
            }

            // initialize time
            thisAgent->EpMem->epmem_stats->time->set_value(1);

            // initialize next_id
            thisAgent->EpMem->epmem_stats->next_id->set_value(1);
            {
                int64_t stored_id = NIL;
                if (epmem_get_variable(thisAgent, var_next_id, &stored_id))
                {
                    thisAgent->EpMem->epmem_stats->next_id->set_value(stored_id);
                }
                else
                {
                    epmem_set_variable(thisAgent, var_next_id, thisAgent->EpMem->epmem_stats->next_id->get_value());
                }
            }

            // initialize rit state
            for (int i = EPMEM_RIT_STATE_NODE; i <= EPMEM_RIT_STATE_EDGE; i++)
            {
                thisAgent->EpMem->epmem_rit_state_graph[ i ].offset.stat->set_value(EPMEM_RIT_OFFSET_INIT);
                thisAgent->EpMem->epmem_rit_state_graph[ i ].leftroot.stat->set_value(0);
                thisAgent->EpMem->epmem_rit_state_graph[ i ].rightroot.stat->set_value(1);
                thisAgent->EpMem->epmem_rit_state_graph[ i ].minstep.stat->set_value(LONG_MAX);
            }
            thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ].add_query = thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_range;
            thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ].add_query = thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_range;

            ////

            // get/set RIT variables
            {
                int64_t var_val = NIL;

                for (int i = EPMEM_RIT_STATE_NODE; i <= EPMEM_RIT_STATE_EDGE; i++)
                {
                    // offset
                    if (epmem_get_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].offset.var_key, &var_val))
                    {
                        thisAgent->EpMem->epmem_rit_state_graph[ i ].offset.stat->set_value(var_val);
                    }
                    else
                    {
                        epmem_set_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].offset.var_key, thisAgent->EpMem->epmem_rit_state_graph[ i ].offset.stat->get_value());
                    }

                    // leftroot
                    if (epmem_get_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].leftroot.var_key, &var_val))
                    {
                        thisAgent->EpMem->epmem_rit_state_graph[ i ].leftroot.stat->set_value(var_val);
                    }
                    else
                    {
                        epmem_set_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].leftroot.var_key, thisAgent->EpMem->epmem_rit_state_graph[ i ].leftroot.stat->get_value());
                    }

                    // rightroot
                    if (epmem_get_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].rightroot.var_key, &var_val))
                    {
                        thisAgent->EpMem->epmem_rit_state_graph[ i ].rightroot.stat->set_value(var_val);
                    }
                    else
                    {
                        epmem_set_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].rightroot.var_key, thisAgent->EpMem->epmem_rit_state_graph[ i ].rightroot.stat->get_value());
                    }

                    // minstep
                    if (epmem_get_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].minstep.var_key, &var_val))
                    {
                        thisAgent->EpMem->epmem_rit_state_graph[ i ].minstep.stat->set_value(var_val);
                    }
                    else
                    {
                        epmem_set_variable(thisAgent, thisAgent->EpMem->epmem_rit_state_graph[ i ].minstep.var_key, thisAgent->EpMem->epmem_rit_state_graph[ i ].minstep.stat->get_value());
                    }
                }
            }

            ////

            // get max time
            {
                temp_q = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, "SELECT MAX(episode_id) FROM epmem_episodes");
                temp_q->prepare();
                if (temp_q->execute() == soar_module::row)
                {
                    thisAgent->EpMem->epmem_stats->time->set_value(temp_q->column_int(0) + 1);
                }

                delete temp_q;
                temp_q = NULL;
            }
            time_max = thisAgent->EpMem->epmem_stats->time->get_value();

            // insert non-NOW intervals for all current NOW's
            // remove NOW's
            if (!readonly)
            {
                time_last = (time_max - 1);

                const char* now_select[] = { "SELECT wc_id,start_episode_id FROM epmem_wmes_constant_now", "SELECT wi_id,start_episode_id,lti_id FROM epmem_wmes_identifier_now" };
                soar_module::sqlite_statement* now_add[] = { thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_point, thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_point };
                const char* now_delete[] = { "DELETE FROM epmem_wmes_constant_now", "DELETE FROM epmem_wmes_identifier_now" };

                for (int i = EPMEM_RIT_STATE_NODE; i <= EPMEM_RIT_STATE_EDGE; i++)
                {
                    temp_q = now_add[i];
                    temp_q->bind_int(2, time_last);

                    temp_q2 = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, now_select[i]);
                    temp_q2->prepare();
                    while (temp_q2->execute() == soar_module::row)
                    {
                        range_start = temp_q2->column_int(1);

                        // point
                        if (range_start == time_last)
                        {
                            temp_q->bind_int(1, temp_q2->column_int(0));
                            if (i == EPMEM_RIT_STATE_EDGE)
                            {
                                temp_q->bind_int(3, temp_q2->column_int(2));
                            }
                            temp_q->execute(soar_module::op_reinit);
                        }
                        else
                        {
                            epmem_rit_insert_interval(thisAgent, range_start, time_last, temp_q2->column_int(0), &(thisAgent->EpMem->epmem_rit_state_graph[i]), temp_q2->column_int(2));
                        }

                        if (i == EPMEM_RIT_STATE_EDGE)
                        {
                            thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(1, time_last);
                            thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(2, temp_q2->column_int(0));
                            thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->execute(soar_module::op_reinit);
                        }
                    }
                    delete temp_q2;
                    temp_q2 = NULL;
                    temp_q = NULL;


                    // remove all NOW intervals
                    temp_q = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, now_delete[i]);
                    temp_q->prepare();
                    temp_q->execute();
                    delete temp_q;
                    temp_q = NULL;
                }
            }

            // get max id + max list
            {
                const char* minmax_select[] = { "SELECT MAX(wc_id) FROM epmem_wmes_constant", "SELECT MAX(wi_id) FROM epmem_wmes_identifier" };
                std::vector<bool>* minmax_max[] = { thisAgent->EpMem->epmem_node_maxes, thisAgent->EpMem->epmem_edge_maxes };
                std::vector<epmem_time_id>* minmax_min[] = { thisAgent->EpMem->epmem_node_mins, thisAgent->EpMem->epmem_edge_mins };

                for (int i = EPMEM_RIT_STATE_NODE; i <= EPMEM_RIT_STATE_EDGE; i++)
                {
                    temp_q = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, minmax_select[i]);
                    temp_q->prepare();
                    temp_q->execute();
                    if (temp_q->column_type(0) != soar_module::null_t)
                    {
                        std::vector<bool>::size_type num_ids = static_cast<size_t>(temp_q->column_int(0));

                        minmax_max[i]->resize(num_ids, true);
                        minmax_min[i]->resize(num_ids, time_max);
                    }

                    delete temp_q;
                    temp_q = NULL;
                }
            }

            // get id pools
            {
                epmem_node_id parent_n_id;
                int64_t attribute_s_id;
                epmem_node_id child_n_id;
                epmem_node_id wi_id;

                epmem_hashed_id_pool** hp;
                epmem_id_pool** ip;

                temp_q = new soar_module::sqlite_statement(thisAgent->EpMem->epmem_db, "SELECT parent_n_id, attribute_s_id, child_n_id, wi_id FROM epmem_wmes_identifier");
                temp_q->prepare();

                while (temp_q->execute() == soar_module::row)
                {
                    parent_n_id = temp_q->column_int(0);
                    attribute_s_id = temp_q->column_int(1);
                    child_n_id = temp_q->column_int(2);
                    wi_id = temp_q->column_int(3);

                    hp = & (*thisAgent->EpMem->epmem_id_repository)[ parent_n_id ];
                    if (!(*hp))
                    {
                        (*hp) = new epmem_hashed_id_pool;
                    }

                    ip = & (*(*hp))[ attribute_s_id ];
                    if (!(*ip))
                    {
                        (*ip) = new epmem_id_pool;
                    }

                    (*ip)->push_front(std::make_pair(child_n_id, wi_id));

                    hp = & (*thisAgent->EpMem->epmem_id_repository)[ child_n_id ];
                    if (!(*hp))
                    {
                        (*hp) = new epmem_hashed_id_pool;
                    }
                }

                delete temp_q;
                temp_q = NULL;
            }

            // at init, top-state is considered the only known identifier
            thisAgent->top_goal->id->epmem_id = EPMEM_NODEID_ROOT;
            thisAgent->top_goal->id->epmem_valid = thisAgent->EpMem->epmem_validation;

            // capture augmentations of top-state as the sole set of adds,
            // which catches up to what would have been incremental encoding
            // to this point
            {
                thisAgent->EpMem->epmem_wme_adds->insert(thisAgent->top_state);
            }
        }

        // if lazy commit, then we encapsulate the entire lifetime of the agent in a single transaction
        if (thisAgent->EpMem->epmem_params->lazy_commit->get_value() == on)
        {
            thisAgent->EpMem->epmem_stmts_common->begin->execute(soar_module::op_reinit);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->init->stop();
    ////////////////////////////////////////////////////////////////////////////
}



//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Storage Functions (epmem::storage)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/*
 * As part of ongoing implementation associated with event cognition, Soar
 * will feature different methods for estimating "surprise" as a form of metadata
 * associated with encoded episodic memory elements.
 *
 * The first and simplest of these is based on inverted base-level activation.
 */


double epmem_surprise_bla(agent* thisAgent, epmem_time_id time_counter, bool is_a_constant, epmem_node_id parent_id, epmem_hash_id attribute_id, epmem_node_id triple_id, bool is_a_float)//Needs to be the case that where I call this, I can tell if it's for a float or not.
{
    //First, check for if an element has ever been added before. if not initialize. could make parameter for whether or not first appearance allows "surprise".
    //If it already exists, just update blas and add to surprise list.
    double bla_before = -999999.0;

    //This needs to be changed so that search_pair is only for floats. Meanwhile, the remainder use triple_ids for the actual location.
    assert(!(is_a_float && !is_a_constant));//Yeah, I know I could distribute it, but this is easier to read.

    if (is_a_float)
    {//!!!!!!!!!!!!!triple_id has a different meaning for floats.!!!!!!!!!!!!!!!!!!!!!1
        std::pair<epmem_node_id,epmem_hash_id> search_pair = std::make_pair(parent_id,attribute_id);
        if (thisAgent->EpMem->float_change_counter->find(search_pair) == thisAgent->EpMem->float_change_counter->end())
        { // wholly novel case
            thisAgent->EpMem->float_change_counter->emplace(std::make_pair(std::make_pair(parent_id,attribute_id),1));//)[std::make_pair(parent_id,attribute_id)] = 1;
            thisAgent->EpMem->float_change_time_recent->emplace(std::make_pair(std::make_pair(parent_id,attribute_id),time_counter));//)[std::make_pair(parent_id,attribute_id)] = time_counter;
            thisAgent->EpMem->float_change_time_first->emplace(std::make_pair(std::make_pair(parent_id,attribute_id),time_counter));//)[std::make_pair(parent_id,attribute_id)] = time_counter;
            if (time_counter > 1)//May remove that condition. depends on whether or not wholly novel is to be paid attention.
            {
                //Not sure how we're gonna use surprise yet, so for now, I'm associating surprise with each interval. This means using the triple_id and time_counter as the keys and the surprise as the value.
                //This will let me look for the surprise associated with something, but does not allow for easy indexing by surprise.
                //database table index by interval time and then surprise might be the most useful for later integration with queries.
                    //otherwise, end up with perhaps a very large priority queue in ram


                //thisAgent->EpMem->epmem_node_id_reverse_bla->emplace(std::make_tuple(triple_id,bla_before,time_counter));

            }
        }
        else
        { // seen it
            double count = static_cast<double>((*thisAgent->EpMem->float_change_counter)[search_pair]);
            double recent = static_cast<double>((*thisAgent->EpMem->float_change_time_recent)[search_pair]);
            double first = static_cast<double>((*thisAgent->EpMem->float_change_time_first)[search_pair]);
            (*thisAgent->EpMem->float_change_counter)[search_pair] = 1 + (*thisAgent->EpMem->float_change_counter)[search_pair];
            (*thisAgent->EpMem->float_change_time_recent)[search_pair] = time_counter;
            double bla_odds = (1.0/(sqrt(time_counter-recent)) + (2.0*(count - 1))/(sqrt(time_counter-first)+sqrt(time_counter-recent)));// If I took this value right now and just slapped a log around it, I'd have a bla for just before the element was accessed/changed.
            bla_odds = 1.0/bla_odds;
            //Alright, so look, bla was always considered as "log-odds". Now, all it really means to me is that there is a nice decaying curve where before you take the log, the range of the function is from 0 to infinity, which you'd
            //want out of "odds". Now, it does well as a heuristic for recency and frequency based probability. However, what "surprise" should basically measure is the "odds" of *not* happening, given some recency and frequency.
            //Thus, I just plain do the normal odds calculation, don't do the superfluous log, and invert the odds, and then use o/(1+o)=p to get a surprise value between 0 and 1, because that's an easier range to deal with, and can be more easily
            //interpretted as a probability. No, I didn't do any of the algebra to simplify the expression -- I'll bother with that sort of thing after I see whether this is a decent heuristic or not.
            //As far as I can tell, it's basically out of weird tradition and dubious algebraic simplifications that we ever ended up wanting log-odds in the first place. It's not like it was originally motivated by SLAM-style grid filtering where
            //we actually use the convenience of a log-odds formulation for efficient computation reasons.
            double surprise = bla_odds/(1+bla_odds);//A number between 0 and 1 and that should be big when it's been a long time since something infrequently change has changed.
            if (time_counter > 1)
            {
                return surprise;
                //thisAgent->EpMem->epmem_node_id_reverse_bla->emplace(std::make_tuple(triple_id,bla_before,time_counter));
            }
        }
    }
    else
    {
        std::pair<bool,epmem_hash_id> search_pair = std::make_pair(is_a_constant,triple_id);
        if (thisAgent->EpMem->change_counter->find(search_pair) == thisAgent->EpMem->change_counter->end())
        { // wholly novel case
            thisAgent->EpMem->change_counter->emplace(std::make_pair(std::make_pair(is_a_constant,triple_id),1));//)[std::make_pair(parent_id,attribute_id)] = 1;
            thisAgent->EpMem->change_time_recent->emplace(std::make_pair(std::make_pair(is_a_constant,triple_id),time_counter));//)[std::make_pair(parent_id,attribute_id)] = time_counter;
            thisAgent->EpMem->change_time_first->emplace(std::make_pair(std::make_pair(is_a_constant,triple_id),time_counter));//)[std::make_pair(parent_id,attribute_id)] = time_counter;
            if (time_counter > 1)//May remove that condition. depends on whether or not wholly novel is to be paid attention.
            {
                //Not sure how we're gonna use surprise yet, so for now, I'm associating surprise with each interval. This means using the triple_id and time_counter as the keys and the surprise as the value.
                //This will let me look for the surprise associated with something, but does not allow for easy indexing by surprise.
                //database table index by interval time and then surprise might be the most useful for later integration with queries.
                    //otherwise, end up with perhaps a very large priority queue in ram

                //thisAgent->EpMem->epmem_node_id_reverse_bla->emplace(std::make_tuple(triple_id,bla_before,time_counter));
            }
        }
        else
        { // seen it
            double count = static_cast<double>((*thisAgent->EpMem->change_counter)[search_pair]);
            double recent = static_cast<double>((*thisAgent->EpMem->change_time_recent)[search_pair]);
            double first = static_cast<double>((*thisAgent->EpMem->change_time_first)[search_pair]);
            (*thisAgent->EpMem->change_counter)[search_pair] = 1 + (*thisAgent->EpMem->change_counter)[search_pair];
            (*thisAgent->EpMem->change_time_recent)[search_pair] = time_counter;
            double bla_odds = (1.0/(sqrt(time_counter-recent)) + (2.0*(count - 1))/(sqrt(time_counter-first)+sqrt(time_counter-recent)));// If I took this value right now and just slapped a log around it, I'd have a bla for just before the element was accessed/changed.
            bla_odds = 1.0/bla_odds;
            //Alright, so look, bla was always considered as "log-odds". Now, all it really means to me is that there is a nice decaying curve where before you take the log, the range of the function is from 0 to infinity, which you'd
            //want out of "odds". Now, it does well as a heuristic for recency and frequency based probability. However, what "surprise" should basically measure is the "odds" of *not* happening, given some recency and frequency.
            //Thus, I just plain do the normal odds calculation, don't do the superfluous log, and invert the odds, and then use o/(1+o)=p to get a surprise value between 0 and 1, because that's an easier range to deal with, and can be more easily
            //interpretted as a probability. No, I didn't do any of the algebra to simplify the expression -- I'll bother with that sort of thing after I see whether this is a decent heuristic or not.
            //As far as I can tell, it's basically out of weird tradition and dubious algebraic simplifications that we ever ended up wanting log-odds in the first place. It's not like it was originally motivated by SLAM-style grid filtering where
            //we actually use the convenience of a log-odds formulation for efficient computation reasons.
            double surprise = bla_odds/(1+bla_odds);//A number between 0 and 1 and that should be big when it's been a long time since something infrequently change has changed.
            if (time_counter > 1)
            {
                return surprise;
                //thisAgent->EpMem->epmem_node_id_reverse_bla->emplace(std::make_tuple(triple_id,bla_before,time_counter));
            }
        }
    }
    return bla_before;
}

double epmem_surprise_hebbian_batch(agent* thisAgent)
{
    //using the current contents of smem's spreading, updates the surprise of epmem_intervals.
    return 0.0;
}

double epmem_surprise_hebbian(agent* thisAgent, epmem_time_id time_counter, bool is_a_constant, epmem_node_id parent_id, epmem_hash_id attribute_id, epmem_node_id triple_id, bool is_a_float)
{//The way this function works is you get as input the time at which the new thing just showed up, and the new thing, just like epmem_surprise_bla, but here, we instead look into the database to see what
    //the expectation would have been for that element to show up. The magnitude of expectation violation is surprise. Then, we update the expectations for all pairwise associations defined by "fire-together-wire-together" hebbian update.
    //The idea is to learn pairwise successions/associations (they are the same thing, when viewed in spacetime, just succession/association is more general than mere association).
    //So, first, note that we already have intervals within the database and they have both a timeslot (interval of time) and a space/entry in the WMG (egospace value).
    //All of the relations between all of the timeslots that are adjacent to each other (not all in general) can be efficiently computed. Think of a chain of befores/afters, but not including "way before", instead things like "overlapping before"
    //These efficiently-computable temporal interval relations are then instantiated by different groundings to egospace values.
    //pairwise relations between groundings are the ones that have a set of associated metadata. (instances)
    //so, as an update rule, all of the groundings that share the same underlying temporal interval will likely share an increment update (won't be the same in general, but will be helpful for efficiency when incrementally changing the metadata)

    //For the first implementation, ignoring "begins" "exact" "contains" as candidates for causal temporal relation. "before-overlap" as canonical case, "before-meets" as edge-case.
    //current idea is that for "contains", you want to "and" with something else to turn it into a traditional before. That "and" candidate can be an explicit timer with
    //for exact and begins, better to treat something else as a mutual cause.
    //for "ends", similar.
    //easily-perceptible causality will have temporal succession structure. Other structures will require additional reasoning or learning. The simple cases of before-overlap and before-meets are all I consider at first.
    //can only update metadata once both intervals leave?
        //measure of surprise as -- degree of expectation violation of thing showing up or ... metadata update magnitude? -- can't be the second because that doesn't exist when element shows up unless the update is like a filter -- (degree of mismatch *is* magnitude of update)
        //hebbian update at least partially based on before/after ratios.
    //question -- to allow self-relations or not? (for same-location interval succession, which is always a before-meets) -- will start with yes.
    //when start of interval goes in, is a potential "before" for anything after, and a potential "after" for anything before.
    //when end of interval goes in, all potential "afters" for that interval (those that started at the same time or that already started, but after this one that just ended began) have the type of relation clarified.
    //when end of interval goes in, all potential "befores" (those that have ended during/just-at-start) to this interval have metadata updated  -- this is the one to figure out how to do efficiently. The rest is "to fit".


    //restating for how i'll do the algorithm
    //basically, keep a "version" that logs what "was now" at start of processing (leftover from last cycle)
    //time+value interval gets added (with "now" end of interval). This interval now has a potential for relations from all the previous cycle's nows.
    //The benefit of this method is that I can keep ram tables without ever referencing database file

    //now, what if I don't keep a record of "potential befores"? (Currently, i'll waste processing when many "contains" happen)
    //means that one the end of an interval, you look for other intervals with before lefts and before/at rights.
    //Do I include "ends"? Could be as a subcase of "before" or "contains" (technically is a contains).

    //definitely doing the "keep track of nows" version.
    //contains should not have any boost. should instead have something that "ands" with the contains to make the relation happen.
    //So, being practical, think about entering a room and expecting objects in that room. There is a temporal "contains" there, for sure, but is it just because of some other form of context-based spatial contains?
    //I think an important form of "anding" that will make a lot of this work is "anding" with "timer since x less than y" as an explicit wme (log-based real time, so that perfectly-timed DCs line up with 1,2,4,8,16,etc).
    //This allows an agent time to scan a room, then when you are surprised by the room not containing something because enough time has passed without you seeing it, you'll be surprised and perhaps prompted to look for it.

    //thisAgent->EpMem->nows
    //for efficiency, hebbian updating should be a batch job at the end of the cycle's processing and not a per-element update, I think.

    epmem_attach_smem(thisAgent);
    //The "could have been expected" version of surprise is what we'll be doing. Basically, smem's job with spreading is going to be to predict, sure, but also to compute the predictability of events that weren't predicted.
    //That seems to be how things usually go anyways, unpredicted, but not unusual.

    //Alright, smem should be attached. The next thing to do is to have smem create a table of spreading activation values for the elements just put into the now table within epmem. the "context" is things that were in the now table or that only just left.
    //In a hardware version, absolutely abstractified better through matrix operations where I'm throwing in a slightly different sparse matrix every cycle. could even use this kernel still with matrix operations made into virtual tables to not have to butcher ltm code.
    //Currently, smem spreads based on smem_current_spread, which is fingerprints (spread ~~p(i|j) for sources j) incrementally added each cycle. Could add something similar within epmem. should end up being the same in the end, but for backwards compatibility not yet.

    //should make another spread function using almost the same function as the one in smem, but going to reference the epmem table for determining context instead. could even implement as just a "epmem_current_spread", really.

    //done and good is better than perfect -- make the epmem_current_spread table format (even just as a view on stuff already in epmem).

    //can do as a batch-like thing. select by context of epmem now and just-previous and only candidates are those that can be spread to by these and also those which were only just added (joins should make this small).
    //then, only for those elements, need the edges to calculate spread. //I suppose in the work that I do that I am capable of putting what I am not conscious of into a string of symbols. I am perhaps similarly not conscious of the ball hidden in the box, but can maintain object permamence as an inference.
    //really, spread could be done much more efficiently in this manner if we didn't have a base-level decay thing to do.

    //This particular function will be the one that calculates the surprise to associate with a given element, given the existing spread so that it fills the same role as the bla version. a different function will actually do the metadata update stuff.
//How do we keep track of what all was the "previous now" without including in that the "just-this-cycle-added" nows?
    //answer -- select from now based on !=startthiscycle.
    //basically, do an insert or ignore fingerprint for all things added to "now",

    //I'm going to have a value between 0 and 1 for surprise. maximum surprise is when nothing could have predicted what happened. (post-hoc)
    //minimum surprise is when everything pointed to exactly 1 and only 1 potential successor that happened.
    //basically, this thing here updates the surprise values in the table by interpreting the spread values.

    return .77;

}

void epmem_update_spread(agent* thisAgent)
{
    //epmem_wmes_index_now is the table that should serve as the "context" for doing spread.
    //Since surprise is calcuated every cycle, don't need to try efficiency from never processing things that go in and then out. instead, worth processing w.r.t. all changes.
    epmem_attach_smem(thisAgent);
//

    //make a newbies table when things are added to the epmem index, but have yet to be moved to smem
    //make a new relations table when relations are added to epmem relations, but have yet to be moved to smem.
    //all ids should be set up before any relations are set up
    //modelling after cli_add
        //for all new ids, gonna add_specific_LTI -- want to map to epmem index somehow -- gonna use negatives for now.
        //for all new relations, gonna populate slots for those  new ids
        //for all ids with non-nil slots, ltm_to_db
            //need to make the children ltm slot map handle the addition so that ltm_to_db handles processing automatically (such as spread invalidation)
//    ltm_set newbies;
//    //thing that populates newbies
//    //just pull rows from table of newly-added-to-epmem-index
//
//    ltm_set::iterator c_new;
//    for (c_new = newbies.begin(); c_new != newbies.end(); c_new++)
//    {
//
//        //add_specific_LTI((*c_new)->lti_id);//don't loop, can just call for each row in table, given that it's all lti_add and no logic.
//    }

    //thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis->execute(soar_module::op_reinit);//take the newly-added wmemgraph elements and make them into ltis
    //doing it the slow way for now -- going to loop through the adds, add ltis, get those ltis, make a table of epmem_id and lti_id because they are two different address spaces.
    uint64_t epmem_w_id = 0;
    uint64_t smem_lti_for_w_id = 0;
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_1->start();
    while (thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis->execute() == soar_module::row)
    {
        epmem_w_id = thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis->column_int(0);
        smem_lti_for_w_id = thisAgent->SMem->add_new_LTI();
        //Now that I have a w_id and some lti, I can add the pair to a table in epmem that keeps track of which ltis are associated with which w_ids. //todo: I have no idea how I'm going to juggle epmem ids with ltis associated with them.
        thisAgent->EpMem->epmem_stmts_graph->record_lti_id_for_w_id->bind_int(1,epmem_w_id);
        thisAgent->EpMem->epmem_stmts_graph->record_lti_id_for_w_id->bind_int(2,smem_lti_for_w_id);
        thisAgent->EpMem->epmem_stmts_graph->record_lti_id_for_w_id->execute(soar_module::op_reinit);
    }
    thisAgent->EpMem->epmem_stmts_graph->make_adds_into_ltis->reinitialize();
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_1->stop();
    thisAgent->EpMem->epmem_stmts_graph->delete_brand_new_adds->execute(soar_module::op_reinit);//delete the temp table that kept track of them.

    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2->start();
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2_1->start();
    while (thisAgent->EpMem->epmem_stmts_graph->select_new_relations->execute() == soar_module::row)
    {//for each new relation, add that relation and invalidate spread that came from the left of that relation
        thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2_1->stop();
        uint64_t existing_edges = 0;
        uint64_t existing_lti_edges = 0;
        //need to make sure this function is timed such that the *weights* have been updated before this happens. %need to make sure this initial addition is the weight_norm. also need to make later updating use the weight_norm.


        thisAgent->SMem->EpMem_to_DB(thisAgent->EpMem->epmem_stmts_graph->select_new_relations->column_int(0), thisAgent->EpMem->epmem_stmts_graph->select_new_relations->column_int(1),thisAgent->EpMem->epmem_stmts_graph->select_new_relations->column_int(2));//just doing each relation

        //todo one way to make the above line more efficient is to take each sql statement used inside that function and instead of looping here, do them as a batch table-level change. need to check whether some modifications require line-by-line calculation, but likely not necessary, or can merely loop over *ONLY* those.
        //first glance says it's doable -- basically requires using SUM and maybe boolean logic, (I think sqlite lets that), in a few places.
        thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2_1->start();
        //thisAgent->SMem->invalidate_from_lti(thisAgent->EpMem->epmem_stmts_graph->select_new_relations->column_int(0));
        thisAgent->SMem->add_to_invalidate_from_lti_table(thisAgent->EpMem->epmem_stmts_graph->select_new_relations->column_int(0));
    }
    thisAgent->EpMem->epmem_stmts_graph->select_new_relations->reinitialize();
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2_1->stop();
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_2->stop();
    thisAgent->EpMem->epmem_stmts_graph->delete_brand_new_relation_adds->execute(soar_module::op_reinit);
    //here, put the join update that makes the smem augmentations weights equal to the weights from epmem.
    //requires that "finish_updates" *not* happen *before* this.
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_3->start();
//        std::string err;
//
//        bool result = epmem_backup_db(thisAgent, "mid_process_error.db", &(err));
    thisAgent->EpMem->epmem_stmts_graph->update_smem_edges->execute(soar_module::op_reinit);//This should make it so that smem now will do spread over "nexts" that came from epmem. //todo gets slow.
    thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4_3->stop();

    //the below would be easier to do by directly manipulating the smem structures than by reusing the code and faking intermediate ltms.
//    //the slots should be the new relations. -- pull rows again.
//    //each attribute is "before" and each value is some lti.
//    //need a slot map per lti parent and a slot in that map for the attr, and values for that attr.
//    ltm_slot_map same_slots_every_time;
//    ltm_slot same_slot_attr_every_time;
//    same_slot_attr_every_time.insert();
//    same_slots_every_time.insert();
//    for (c_new = newbies.begin(); c_new != newbies.end(); c_new++)
//    {
//        if ((*c_new)->slots != NIL)
//        {
//           LTM_to_DB((*c_new)->lti_id, (*c_new)->slots, false, false);
//        }
//    }

    //find the attR_hash for "before".
    //then do web_add, and invalidate_trajectories. --  web_add acccepts an edge weight.
    //do wmes_lti_frequency changes.

    //do need the same act_set stuff about numbber of augmentations.

    //so, basically, this is a partial rewrite of ltm_to_db.


}//Much of this can be optimized to be in-database instead of looped here.




/* **************************************************************************

                         _epmem_store_level

   This function process the addition of "one level" of wme's rooted at
   the n_id specified by parent_id into the working memory graph.  It does
   not process any temporal information.

   - w_b is the list of wme's it will process (until it hits w_e)

   - can add syms and ids of augmentations to parent_syms and parent_ids
   queues so that they can be processed during the next "level"

   - The identifier symbol of each wme isn't set and is ignored. parent_id
     is used to specify the root.


     three cases for sharing ids amongst identifiers in two passes:
     1. value known in phase one (try reservation)
     2. value unknown in phase one, but known at phase two (try assignment adhering to constraint)
     3. value unknown in phase one/two (if anything is left, unconstrained assignment)

************************************************************************** */


inline void _epmem_store_level(agent* thisAgent,
                               std::queue< Symbol* >& parent_syms,
                               std::queue< epmem_node_id >& parent_ids,
                               tc_number tc,
                               epmem_wme_list::iterator w_b,
                               epmem_wme_list::iterator w_e,
                               epmem_node_id parent_id,
                               epmem_time_id time_counter,
                               std::map< wme*, epmem_id_reservation* >& id_reservations,
                               std::set< Symbol* >& new_identifiers,
                               std::queue< epmem_node_id >& epmem_node,
							   std::queue<std::pair<epmem_node_id,int64_t>>& epmem_edge,
							   std::map<std::pair<int64_t,int64_t>,std::pair<int64_t,double>>& float_deltas,
							   std::set<epmem_node_id>& delta_ids,
							   std::map<std::pair<bool,int64_t>,std::pair<int64_t,int64_t>>& additions_epmem_id_to_parent_attr)
{
    epmem_wme_list::iterator w_p;
    bool value_known_apriori = false;

    // temporal hash
    epmem_hash_id my_hash;  // attribute
    epmem_hash_id my_hash2; // value

    // id repository
    epmem_id_pool** my_id_repo;
    epmem_id_pool* my_id_repo2;
    epmem_id_pool::iterator pool_p;
    std::map<wme*, epmem_id_reservation*>::iterator r_p;
    epmem_id_reservation* new_id_reservation;

    // identifier recursion
    epmem_wme_list::iterator w_p2;

#ifdef DEBUG_EPMEM_WME_ADD
    fprintf(stderr, "==================================================\nDEBUG _epmem_store_level called for parent_id %d\n==================================================\n", (unsigned int) parent_id);
#endif

    // find WME ID for WMEs whose value is an identifier and has a known epmem id (prevents ordering issues with unknown children)
    for (w_p = w_b; w_p != w_e; w_p++)
    {
//      #ifdef DEBUG_EPMEM_WME_ADD
//      fprintf(stderr, "DEBUG epmem.2132: _epmem_store_level processing wme (types: %d %d %d)\n",
//              (*w_p)->id->symbol_type,  (*w_p)->attr->var->symbol_type,  (*w_p)->value->symbol_type);
//      #endif
        // skip over WMEs already in the system
        if (((*w_p)->epmem_id != EPMEM_NODEID_BAD) && ((*w_p)->epmem_valid == thisAgent->EpMem->epmem_validation))
        {
            continue;
        }
        /* Not sure why this is excluding LTIs or whether that makes sense any more. */
        if (((*w_p)->value->symbol_type     == IDENTIFIER_SYMBOL_TYPE) &&
           (((*w_p)->value->id->epmem_id    != EPMEM_NODEID_BAD) &&
            ((*w_p)->value->id->epmem_valid == thisAgent->EpMem->epmem_validation)))
        {
            // prevent exclusions from being recorded
            if (thisAgent->EpMem->epmem_params->exclusions->in_set((*w_p)->attr))
            {
                continue;
            }

#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "--------------------------------------------\nReserving WME: %d ^%s %s\n",
                    (unsigned int) parent_id, symbol_to_string(thisAgent, (*w_p)->attr, true, NIL, 0), symbol_to_string(thisAgent, (*w_p)->value, true, NIL, 0));
#endif

            // if still here, create reservation (case 1)
#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "   wme is known.  creating reservation.\n");
#endif
            new_id_reservation = new epmem_id_reservation;
            new_id_reservation->my_id = EPMEM_NODEID_BAD;
            new_id_reservation->my_pool = NULL;

            if ((*w_p)->acceptable)
            {
                new_id_reservation->my_hash = EPMEM_HASH_ACCEPTABLE;
            }
            else
            {
                new_id_reservation->my_hash = epmem_temporal_hash(thisAgent, (*w_p)->attr);
            }

            // try to find appropriate reservation
            my_id_repo = & (*(*thisAgent->EpMem->epmem_id_repository)[ parent_id ])[ new_id_reservation->my_hash ];
            if ((*my_id_repo))
            {
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   id repository exists.  reserving id\n");
#endif
                for (pool_p = (*my_id_repo)->begin(); pool_p != (*my_id_repo)->end(); pool_p++)
                {
                    if (pool_p->first == (*w_p)->value->id->epmem_id)
                    {
#ifdef DEBUG_EPMEM_WME_ADD
                        fprintf(stderr, "   reserved id %d\n", (unsigned int) pool_p->second);
#endif
                        new_id_reservation->my_id = pool_p->second;
                        (*my_id_repo)->erase(pool_p);
                        break;
                    }
                }
            }
            else
            {
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   no id repository found.  creating new id repository.\n");
#endif
                // add repository
                (*my_id_repo) = new epmem_id_pool;
            }

            new_id_reservation->my_pool = (*my_id_repo);
            id_reservations[(*w_p) ] = new_id_reservation;
            new_id_reservation = NULL;
        }
    }

    for (w_p = w_b; w_p != w_e; w_p++)
    {
#ifdef DEBUG_EPMEM_WME_ADD
        fprintf(stderr, "--------------------------------------------\nProcessing WME: %d ^%s %s\n",
                (unsigned int) parent_id, symbol_to_string(thisAgent, (*w_p)->attr, true, NIL, 0), symbol_to_string(thisAgent, (*w_p)->value, true, NIL, 0));
#endif
        // skip over WMEs already in the system
        if (((*w_p)->epmem_id != EPMEM_NODEID_BAD) && ((*w_p)->epmem_valid == thisAgent->EpMem->epmem_validation))
        {
#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "   WME already in system with id %d.\n", (unsigned int)(*w_p)->epmem_id);
#endif
            continue;
        }

        // prevent exclusions from being recorded
        if (thisAgent->EpMem->epmem_params->exclusions->in_set((*w_p)->attr))
        {
#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "   WME excluded.  Skipping.\n");
#endif
            continue;
        }

        if ((*w_p)->value->symbol_type == IDENTIFIER_SYMBOL_TYPE)
        {
#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "   WME value is IDENTIFIER.\n");
#endif
            (*w_p)->epmem_valid = thisAgent->EpMem->epmem_validation;
            (*w_p)->epmem_id = EPMEM_NODEID_BAD;

            my_hash = NIL;
            my_id_repo2 = NIL;

            // if the value already has an epmem_id, the WME ID would have already been assigned above (ie. the epmem_id of the VALUE is KNOWN APRIORI [sic])
            // however, it's also possible that the value is known but no WME ID is given (eg. (<s> ^foo <a> ^bar <a>)); this is case 2
            value_known_apriori = (((*w_p)->value->id->epmem_id != EPMEM_NODEID_BAD) && ((*w_p)->value->id->epmem_valid == thisAgent->EpMem->epmem_validation));

            // if long-term identifier as value, special processing
            if ((*w_p)->value->id->LTI_ID && ((*w_p)->value->id->LTI_epmem_valid != thisAgent->EpMem->epmem_validation) && ((*w_p)->value->id->epmem_id != EPMEM_NODEID_BAD))
            {
                // Update the node database with the new lti_id
                thisAgent->EpMem->epmem_stmts_graph->update_node->bind_int(2, (*w_p)->value->id->LTI_ID);
                thisAgent->EpMem->epmem_stmts_graph->update_node->bind_int(1, (*w_p)->value->id->epmem_id);
                thisAgent->EpMem->epmem_stmts_graph->update_node->execute(soar_module::op_reinit);
                (*w_p)->value->id->LTI_epmem_valid = thisAgent->EpMem->epmem_validation;
            }// Steven - This is an important point - It's where we know we're adding an lti for which there wasn't already a known epmem id correspondence assigned to it.
            {
                // in the case of a known value, we already have a reservation (case 1)
                if (value_known_apriori)
                {
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   WME is known.  Looking for reservation.\n");
#endif
                    r_p = id_reservations.find((*w_p));

                    if (r_p != id_reservations.end())
                    {
#ifdef DEBUG_EPMEM_WME_ADD
                        fprintf(stderr, "   Found existing reservation.\n");
#endif
                        // restore reservation info
                        my_hash = r_p->second->my_hash;
                        my_id_repo2 = r_p->second->my_pool;

                        if (r_p->second->my_id != EPMEM_NODEID_BAD)
                        {
                            (*w_p)->epmem_id = r_p->second->my_id;
                            (*thisAgent->EpMem->epmem_id_replacement)[(*w_p)->epmem_id ] = my_id_repo2;
#ifdef DEBUG_EPMEM_WME_ADD
                            fprintf(stderr, "   Assigning id from existing pool: %d\n", (unsigned int)(*w_p)->epmem_id);
#endif
                        }

                        // delete reservation and map entry
                        delete r_p->second;
                        id_reservations.erase(r_p);
                    }
                    // OR a shared identifier at the same level, in which case we need an exact match (case 2)
                    else
                    {
#ifdef DEBUG_EPMEM_WME_ADD
                        fprintf(stderr, "   No reservation found.  Looking for shared identifier at same level.\n");
#endif
                        // get temporal hash
                        if ((*w_p)->acceptable)
                        {
                            my_hash = EPMEM_HASH_ACCEPTABLE;
                        }
                        else
                        {
                            my_hash = epmem_temporal_hash(thisAgent, (*w_p)->attr);
                        }

                        // try to get an id that matches new information
                        my_id_repo = & (*(*thisAgent->EpMem->epmem_id_repository)[ parent_id ])[ my_hash ];
                        if ((*my_id_repo))
                        {
                            if (!(*my_id_repo)->empty())
                            {
                                for (pool_p = (*my_id_repo)->begin(); pool_p != (*my_id_repo)->end(); pool_p++)
                                {
                                    if (pool_p->first == (*w_p)->value->id->epmem_id)
                                    {
                                        (*w_p)->epmem_id = pool_p->second;
                                        (*my_id_repo)->erase(pool_p);
                                        (*thisAgent->EpMem->epmem_id_replacement)[(*w_p)->epmem_id ] = (*my_id_repo);
#ifdef DEBUG_EPMEM_WME_ADD
                                        fprintf(stderr, "   Assigning id from existing pool: %d\n", (unsigned int)(*w_p)->epmem_id);
#endif
                                        break;
                                    }
                                }
                            }
                        }
                        else
                        {
#ifdef DEBUG_EPMEM_WME_ADD
                            fprintf(stderr, "   No pool.  Creating a new one.\n");
#endif
                            // add repository
                            (*my_id_repo) = new epmem_id_pool;
                        }

                        // keep the address for later (used if w->epmem_id was not assigned)
                        my_id_repo2 = (*my_id_repo);
                    }
                }
                // case 3
                else
                {
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   WME is unknown.  Looking for id in repo pool.\n");
#endif
                    // UNKNOWN identifier
                    new_identifiers.insert((*w_p)->value);

                    // get temporal hash
                    if ((*w_p)->acceptable)
                    {
                        my_hash = EPMEM_HASH_ACCEPTABLE;
                    }
                    else
                    {
                        my_hash = epmem_temporal_hash(thisAgent, (*w_p)->attr);
                    }

                    // try to find node
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   Trying to find node with parent=%d and attr=%d\n", (unsigned int) parent_id, (unsigned int) my_hash);
#endif
                    my_id_repo = & (*(*thisAgent->EpMem->epmem_id_repository)[ parent_id ])[ my_hash ];
                    if ((*my_id_repo))
                    {
                        // if something leftover, try to use it
                        if (!(*my_id_repo)->empty())
                        {
                            for (pool_p = (*my_id_repo)->begin(); pool_p != (*my_id_repo)->end(); pool_p++)
                            {
                                // the ref set for this epmem_id may not be there if the pools were regenerated from a previous DB
                                // a non-existant ref set is the equivalent of a ref count of 0 (ie. an empty ref set)
                                // so we allow the identifier from the pool to be used
                                if ((thisAgent->EpMem->epmem_id_ref_counts->count(pool_p->first) == 0) ||
                                        ((*thisAgent->EpMem->epmem_id_ref_counts)[ pool_p->first ]->empty()))

                                {
                                    (*w_p)->epmem_id = pool_p->second;
                                    (*w_p)->value->id->epmem_id = pool_p->first;
#ifdef DEBUG_EPMEM_WME_ADD
                                    fprintf(stderr, "   Found unused id. Setting wme id for VALUE to %d\n", (unsigned int)(*w_p)->value->id->epmem_id);
#endif
                                    (*w_p)->value->id->epmem_valid = thisAgent->EpMem->epmem_validation;
                                    (*my_id_repo)->erase(pool_p);
                                    (*thisAgent->EpMem->epmem_id_replacement)[(*w_p)->epmem_id ] = (*my_id_repo);

#ifdef DEBUG_EPMEM_WME_ADD
                                    fprintf(stderr, "   Assigning id from existing pool %d.\n", (unsigned int)(*w_p)->epmem_id);
#endif
                                    break;
                                }
                            }
                        }
                    }
                    else
                    {
#ifdef DEBUG_EPMEM_WME_ADD
                        fprintf(stderr, "   No pool.  Creating a new one.\n");
#endif
                        // add repository
                        (*my_id_repo) = new epmem_id_pool;
                    }

                    // keep the address for later (used if w->epmem_id was not assgined)
                    my_id_repo2 = (*my_id_repo);
                }
            }

            // add wme if no success above
            if ((*w_p)->epmem_id == EPMEM_NODEID_BAD)
            {
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   No success, adding wme to database.");
#endif
                // can't use value_known_apriori, since value may have been assigned (lti, id repository via case 3)
                if (((*w_p)->value->id->epmem_id == EPMEM_NODEID_BAD) || ((*w_p)->value->id->epmem_valid != thisAgent->EpMem->epmem_validation))
                {
                    // update next id
                    (*w_p)->value->id->epmem_id = thisAgent->EpMem->epmem_stats->next_id->get_value();
                    (*w_p)->value->id->epmem_valid = thisAgent->EpMem->epmem_validation;
                    thisAgent->EpMem->epmem_stats->next_id->set_value((*w_p)->value->id->epmem_id + 1);
                    epmem_set_variable(thisAgent, var_next_id, (*w_p)->value->id->epmem_id + 1);

#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   Adding new n_id and setting wme id for VALUE to %d\n", (unsigned int)(*w_p)->value->id->epmem_id);
#endif
                    // Update the node database with the new n_id
                    thisAgent->EpMem->epmem_stmts_graph->update_node->bind_int(1, (*w_p)->value->id->epmem_id);
                    if (!(*w_p)->value->id->LTI_ID)
                    {
                        thisAgent->EpMem->epmem_stmts_graph->update_node->bind_int(2, 0);
                    } else {
                        thisAgent->EpMem->epmem_stmts_graph->update_node->bind_int(2, (*w_p)->value->id->LTI_ID);
                        (*w_p)->value->id->LTI_epmem_valid = thisAgent->EpMem->epmem_validation;
                    }
                    thisAgent->EpMem->epmem_stmts_graph->update_node->execute(soar_module::op_reinit);

                    // add repository for possible future children
                    (*thisAgent->EpMem->epmem_id_repository)[(*w_p)->value->id->epmem_id ] = new epmem_hashed_id_pool;

                    // add ref set
#ifdef USE_MEM_POOL_ALLOCATORS
                    (*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ] = new epmem_wme_set(std::less< wme* >(), soar_module::soar_memory_pool_allocator< wme* >());
#else
                    (*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ] = new epmem_wme_set();
#endif
                }

                // insert (parent_n_id,attribute_s_id,child_n_id)
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   Performing database insertion: %d %d %d\n",
                        (unsigned int) parent_id, (unsigned int) my_hash, (unsigned int)(*w_p)->value->id->epmem_id);

                fprintf(stderr, "   Adding wme to epmem_wmes_identifier table.\n");
#endif
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier->bind_int(1, parent_id);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier->bind_int(2, my_hash);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier->bind_int(3, (*w_p)->value->id->epmem_id);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier->bind_int(4, LLONG_MAX);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier->execute(soar_module::op_reinit);

                (*w_p)->epmem_id = static_cast<epmem_node_id>(thisAgent->EpMem->epmem_db->last_insert_rowid());
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(1,IDENTIFIER_SYMBOL_TYPE);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(2,(*w_p)->epmem_id);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->execute(soar_module::op_reinit);
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   Incrementing and setting wme id to %d\n", (unsigned int)(*w_p)->epmem_id);
#endif
                // replace the epmem_id and wme id in the right place
                (*thisAgent->EpMem->epmem_id_replacement)[(*w_p)->epmem_id ] = my_id_repo2;

                // new nodes definitely start
                epmem_edge.emplace((*w_p)->epmem_id,static_cast<int64_t>((*w_p)->value->id->is_lti() ? (*w_p)->value->id->LTI_ID : 0));
                thisAgent->EpMem->epmem_edge_mins->push_back(time_counter);
                thisAgent->EpMem->epmem_edge_maxes->push_back(false);
                //epmem_surprise_hebbian(thisAgent, time_counter, false, parent_id, my_hash, (*w_p)->epmem_id, false);// surprise for an identifier interval.
                additions_epmem_id_to_parent_attr.emplace(std::pair<bool,int64_t>(true,(*w_p)->epmem_id),std::pair<int64_t,int64_t>(parent_id,my_hash));
            }
            else
            {
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   No success but already has id, so don't remove.\n");
#endif
                // definitely don't remove
                (*thisAgent->EpMem->epmem_edge_removals)[std::make_pair((*w_p)->epmem_id, static_cast<int64_t>((*w_p)->value->id->is_lti() ? (*w_p)->value->id->LTI_ID : 0)) ] = false;

                // we add ONLY if the last thing we did was remove
                if ((*thisAgent->EpMem->epmem_edge_maxes)[static_cast<size_t>((*w_p)->epmem_id - 1)])
                {
                    epmem_edge.emplace((*w_p)->epmem_id,static_cast<int64_t>((*w_p)->value->id->is_lti() ? (*w_p)->value->id->LTI_ID : 0));
                    (*thisAgent->EpMem->epmem_edge_maxes)[static_cast<size_t>((*w_p)->epmem_id - 1)] = false;
                    //epmem_surprise_hebbian(thisAgent, time_counter, false, parent_id, my_hash, (*w_p)->epmem_id, false);// surprise for an identifier interval.
                    additions_epmem_id_to_parent_attr.emplace(std::pair<bool,int64_t>(true,(*w_p)->epmem_id),std::pair<int64_t,int64_t>(parent_id,my_hash));
                }
            }

            // at this point we have successfully added a new wme
            // whose value is an identifier.  If the value was
            // unknown at the beginning of this episode, then we need
            // to update its ref count for each WME added (thereby catching
            // up with ref counts that would have been accumulated via wme adds)
            if (new_identifiers.find((*w_p)->value) != new_identifiers.end())
            {
                // because we could have bypassed the ref set before, we need to create it here
                if (thisAgent->EpMem->epmem_id_ref_counts->count((*w_p)->value->id->epmem_id) == 0)
                {
#ifdef USE_MEM_POOL_ALLOCATORS
                    (*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ] = new epmem_wme_set(std::less< wme* >(), soar_module::soar_memory_pool_allocator< wme* >(thisAgent));
#else
                    (*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ] = new epmem_wme_set;
#endif
                }
                (*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ]->insert((*w_p));
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   increasing ref_count of value in %d %d %d; new ref_count is %d\n",
                        (unsigned int)(*w_p)->id->id->epmem_id, (unsigned int) epmem_temporal_hash(thisAgent, (*w_p)->attr), (unsigned int)(*w_p)->value->id->epmem_id, (unsigned int)(*thisAgent->EpMem->epmem_id_ref_counts)[(*w_p)->value->id->epmem_id ]->size());
#endif
            }

            // if the value has not been iterated over, continue to augmentations
            if ((*w_p)->value->tc_num != tc)
            {
                parent_syms.push((*w_p)->value);
                parent_ids.push((*w_p)->value->id->epmem_id);
            }
        }
        else
        {
#ifdef DEBUG_EPMEM_WME_ADD
            fprintf(stderr, "   WME value is a CONSTANT.\n");
#endif

            // have we seen this node in this database?
            if (((*w_p)->epmem_id == EPMEM_NODEID_BAD) || ((*w_p)->epmem_valid != thisAgent->EpMem->epmem_validation))
            {
#ifdef DEBUG_EPMEM_WME_ADD
                fprintf(stderr, "   This is a new wme.\n");
#endif

                (*w_p)->epmem_id = EPMEM_NODEID_BAD;
                (*w_p)->epmem_valid = thisAgent->EpMem->epmem_validation;

                my_hash = epmem_temporal_hash(thisAgent, (*w_p)->attr);
                my_hash2 = epmem_temporal_hash(thisAgent, (*w_p)->value);

                // try to get node id
                {
                    // parent_n_id=? AND attribute_s_id=? AND value_s_id=?
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   Looking for id of a duplicate entry in epmem_wmes_constant.\n");
#endif
                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->bind_int(1, parent_id);
                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->bind_int(2, my_hash);
                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->bind_int(3, my_hash2);

                    if (thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->execute() == soar_module::row)
                    {
                        (*w_p)->epmem_id = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->column_int(0);
                    }

                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_constant->reinitialize();
                }

                // act depending on new/existing feature
                if ((*w_p)->epmem_id == EPMEM_NODEID_BAD)
                {
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   No duplicate wme found in epmem_wmes_constant.  Adding wme to table!!!!\n");
                    fprintf(stderr, "   Performing database insertion: %d %d %d\n",
                            (unsigned int) parent_id, (unsigned int) my_hash, (unsigned int) my_hash2);
#endif
                    // insert (parent_n_id, attribute_s_id, value_s_id)
                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant->bind_int(1, parent_id);
                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant->bind_int(2, my_hash);
                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant->bind_int(3, my_hash2);
                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant->execute(soar_module::op_reinit);

                    (*w_p)->epmem_id = (epmem_node_id) thisAgent->EpMem->epmem_db->last_insert_rowid();
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   Setting wme id from last row to %d\n", (unsigned int)(*w_p)->epmem_id);
#endif
                    // new nodes definitely start
                    epmem_node.push((*w_p)->epmem_id);


                    //void epmem_surprise_hebbian(agent* thisAgent, epmem_time_id time_counter, bool is_a_constant, epmem_node_id triple_id, bool is_a_float)

					if ((*w_p)->value->symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
					{
						//epmem_surprise_hebbian(thisAgent, time_counter, true, parent_id, my_hash, (*w_p)->epmem_id, true);
						float_deltas.emplace(std::pair<int64_t,int64_t>(parent_id,my_hash),std::pair<int64_t,double>((*w_p)->epmem_id,(*w_p)->value->fc->value));
						delta_ids.insert((*w_p)->epmem_id);
						thisAgent->EpMem->val_at_last_change.insert(std::pair<std::pair<int64_t,int64_t>,double>(std::pair<int64_t,int64_t>(parent_id,my_hash),(*w_p)->value->fc->value));
					}
					else
					{
					    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(1,(*w_p)->value->symbol_type);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(2,(*w_p)->epmem_id);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->execute(soar_module::op_reinit);
					    //epmem_surprise_hebbian(thisAgent, time_counter, true, parent_id, my_hash, (*w_p)->epmem_id, false);
                        additions_epmem_id_to_parent_attr.emplace(std::pair<bool,int64_t>(false,(*w_p)->epmem_id),std::pair<int64_t,int64_t>(parent_id,my_hash));
					}

                    thisAgent->EpMem->epmem_node_mins->push_back(time_counter);
                    thisAgent->EpMem->epmem_node_maxes->push_back(false);
                }
                else
                {
#ifdef DEBUG_EPMEM_WME_ADD
                    fprintf(stderr, "   Node found in database, definitely don't remove.\n");
                    fprintf(stderr, "   Setting wme id from existing node to %d\n", (unsigned int)(*w_p)->epmem_id);
#endif
                    // definitely don't remove
                    (*thisAgent->EpMem->epmem_node_removals)[(*w_p)->epmem_id ] = false;

                    // add ONLY if the last thing we did was add
                    if ((*thisAgent->EpMem->epmem_node_maxes)[static_cast<size_t>((*w_p)->epmem_id - 1)])
                    {
                        epmem_node.push((*w_p)->epmem_id);

                        if ((*w_p)->value->symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
						{
                        	float_deltas.emplace(std::pair<int64_t,int64_t>(parent_id,my_hash),std::pair<int64_t,double>((*w_p)->epmem_id,(*w_p)->value->fc->value));
							delta_ids.insert((*w_p)->epmem_id);
							thisAgent->EpMem->val_at_last_change.insert(std::pair<std::pair<int64_t,int64_t>,double>(std::pair<int64_t,int64_t>(parent_id,my_hash),(*w_p)->value->fc->value));
						}
                        else
                        {
                            //epmem_surprise_hebbian(thisAgent, time_counter, true, parent_id, my_hash, (*w_p)->epmem_id, false);
                            additions_epmem_id_to_parent_attr.emplace(std::pair<bool,int64_t>(false,(*w_p)->epmem_id),std::pair<int64_t,int64_t>(parent_id,my_hash));
                        }

                        (*thisAgent->EpMem->epmem_node_maxes)[static_cast<size_t>((*w_p)->epmem_id - 1)] = false;
                    }
                }
            }
        }
    }
}

void epmem_new_episode(agent* thisAgent)
{

    epmem_attach(thisAgent);

    // add the episode only if db is properly initialized
    if (thisAgent->EpMem->epmem_db->get_status() != soar_module::connected)
    {
        return;
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->storage->start();
    ////////////////////////////////////////////////////////////////////////////

    epmem_time_id time_counter = thisAgent->EpMem->epmem_stats->time->get_value();

    // provide trace output
    print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM,  "New episodic memory recorded for time %u.\n", static_cast<long int>(time_counter));

    // perform storage
    {
        // seen nodes (non-identifiers) and edges (identifiers)
        std::queue<epmem_node_id> epmem_node;
        std::queue<std::pair<epmem_node_id,int64_t>> epmem_edge;//epmem_edge now needs to keep track of the lti status/identity of the wmenode/epmemedge
        std::map<std::pair<int64_t,int64_t>,std::pair<int64_t,double>> potential_float_deltas;//should likely be unordered, but just for now, I want this written. -- sjj - TODO
		std::set<epmem_node_id> potential_delta_ids;
		std::map<std::pair<bool,int64_t>,std::pair<int64_t,int64_t>> additions_epmem_id_to_parent_attr;

		thisAgent->EpMem->epmem_timers->storage_store_levels->start();
        // walk appropriate levels
        {
            // prevents infinite loops
            tc_number tc = get_new_tc_number(thisAgent);

            // children of the current identifier
            epmem_wme_list* wmes = NULL;

            // breadth first search state
            std::queue< Symbol* > parent_syms;
            Symbol* parent_sym = NULL;
            std::queue< epmem_node_id > parent_ids;
            epmem_node_id parent_id;

            // cross-level information
            std::map< wme*, epmem_id_reservation* > id_reservations;
            std::set< Symbol* > new_identifiers;

            // start with new WMEs attached to known identifiers
            for (epmem_symbol_set::iterator id_p = thisAgent->EpMem->epmem_wme_adds->begin(); id_p != thisAgent->EpMem->epmem_wme_adds->end(); id_p++)
            {
                // make sure the WME is valid
                // it can be invalid if a child WME was added, but then the parent was removed, setting the epmem_id to EPMEM_NODEID_BAD
                if ((*id_p)->id->epmem_id != EPMEM_NODEID_BAD)
                {
                    parent_syms.push((*id_p));
                    parent_ids.push((*id_p)->id->epmem_id);
                    while (!parent_syms.empty())
                    {
                        parent_sym = parent_syms.front();
                        parent_syms.pop();
                        parent_id = parent_ids.front();
                        parent_ids.pop();
                        wmes = epmem_get_augs_of_id(parent_sym, tc);
                        if (! wmes->empty())
                        {
                            //_epmem_store_level(thisAgent, parent_syms, parent_ids, tc, wmes->begin(), wmes->end(), parent_id, time_counter, id_reservations, new_identifiers, epmem_node, epmem_edge);
                        	_epmem_store_level(thisAgent, parent_syms, parent_ids, tc, wmes->begin(), wmes->end(), parent_id, time_counter, id_reservations, new_identifiers, epmem_node, epmem_edge, potential_float_deltas, potential_delta_ids, additions_epmem_id_to_parent_attr);
                        }
                        delete wmes;
                    }
                }
            }
        }
        thisAgent->EpMem->epmem_timers->storage_store_levels->stop();
        // Before we process inserts, we need to initialize the data structure which keeps track of them
        EpMem_Id_Delta* some_delta = NIL;
        some_delta = new EpMem_Id_Delta(thisAgent);//may end up wrapping this in an "if" and using the nil as a check later.

        uint64_t working_memory_delta_size = 0;

	    bool was_nothing = true;
	    bool c_change_only = true;
	    bool created_interval_time = false;
        epmem_time_id now_interval_time_id = -1;
        // all inserts
        {
            epmem_node_id* temp_node;
            int64_t* lti_id;

            thisAgent->EpMem->epmem_timers->storage_insert_constants->start();
            // nodes
            while (!epmem_node.empty())
            {
                if (!created_interval_time)
                {
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_counter);
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,-1);
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                    now_interval_time_id = thisAgent->EpMem->epmem_db->last_insert_rowid();
                    created_interval_time = true;
                }
                temp_node = & epmem_node.front();

                // add NOW entry
                // id = ?, start_episode_id = ?
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_now->bind_int(1, (*temp_node));
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_now->bind_int(2, time_counter);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_now->execute(soar_module::op_reinit);


                //This is where to add surprise if this isn't a float.
                if (potential_delta_ids.find((*temp_node)) == potential_delta_ids.end())
                {
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->bind_int(1,(*temp_node));//gets parent, attr, value from wc_id.
                        //Note that this just gives the hashes, which is fine for the parent and attr, but not the value.
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->execute();
                    int64_t value_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(2);
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->reinitialize();
                    thisAgent->EpMem->epmem_stmts_common->hash_get_type->bind_int(1, value_hash);
                    soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_get_type->execute();
                    (void)res; // quells compiler warning
                    assert(res == soar_module::row);
                    byte sym_type = static_cast<byte>(thisAgent->EpMem->epmem_stmts_common->hash_get_type->column_int(0));
                    thisAgent->EpMem->epmem_stmts_common->hash_get_type->reinitialize();
                    /* todo Don't actually do surprise here. do altogether at end of cycle, but before weight updates. */ //double temp_surprise = epmem_surprise_hebbian(thisAgent, time_counter, true, additions_epmem_id_to_parent_attr[std::pair<bool,int64_t>(false,(*temp_node))].first, additions_epmem_id_to_parent_attr[std::pair<bool,int64_t>(false,(*temp_node))].second, (*temp_node), false);// surprise for an identifier interval.                    //armed with a surprise value and a freshly-started interval, we can insert a row into the epmem_intervals table.
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(1,now_interval_time_id);//time_id from time index
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_double(2,1.0);//surprise value
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(3,time_counter);//start_time
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(4,sym_type);//w_id from master index comes from the type and the type-specific id. The query handles it.
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(5,(*temp_node));
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->execute(soar_module::op_reinit);

                }
                /*
                 * This is where we know we've added something to working memory.
                 */
                thisAgent->EpMem->total_wme_changes++;
                some_delta->add_addition(*temp_node);
                c_change_only = false;
                was_nothing = false;
                ++working_memory_delta_size;


                // update min
                (*thisAgent->EpMem->epmem_node_mins)[static_cast<size_t>((*temp_node) - 1)] = time_counter;

                epmem_node.pop();
            }
            thisAgent->EpMem->epmem_timers->storage_insert_constants->stop();

            thisAgent->EpMem->epmem_timers->storage_insert_ids->start();
            // edges
            while (!epmem_edge.empty())
            {//For the identifiers that are lti instances and for which they previously were not stored with the lti metadata they currently possess,
             //we need to make a new interval. The lti instance metadata will be stored as a field on the interval. The most recent lti will be
             //treated as the "default" and splitting of intervals will happen only if a change from the most recent is detected.
             //It will function similarly to a removal and replacement with respect to the relational interval tree.
                temp_node = & epmem_edge.front().first;
                lti_id = & epmem_edge.front().second;

                if (!created_interval_time)
                {
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_counter);
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,-1);
                    thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                    now_interval_time_id = thisAgent->EpMem->epmem_db->last_insert_rowid();
                    created_interval_time = true;
                }

                // add NOW entry
                // id = ?, start_episode_id = ?
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_now->bind_int(1, (*temp_node));
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_now->bind_int(2, time_counter);
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_now->bind_int(3, (*lti_id));
                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_now->execute(soar_module::op_reinit);
                /*
                 * This is where we know we've added something to working memory.
                 */
                thisAgent->EpMem->total_wme_changes++;

                //This is where to add surprise.
                /* todo Don't actually do surprise here. do altogether at end of cycle, but before weight updates. */ //double temp_surprise = epmem_surprise_hebbian(thisAgent, time_counter, false, additions_epmem_id_to_parent_attr[std::pair<bool,int64_t>(true,(*temp_node))].first, additions_epmem_id_to_parent_attr[std::pair<bool,int64_t>(true,(*temp_node))].second, (*temp_node), false);// surprise for an identifier interval.
                //armed with a surprise value and a freshly-started interval, we can insert a row into the epmem_intervals table.
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(1,now_interval_time_id);//time_id from time index
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_double(2,1.0);//surprise value
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(3,time_counter);//start_time
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(4,IDENTIFIER_SYMBOL_TYPE);//w_id from master index comes from the type and the type-specific id. The query handles it.
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(5,(*temp_node));
                thisAgent->EpMem->epmem_stmts_graph->add_interval_data->execute(soar_module::op_reinit);

                some_delta->add_addition(*temp_node);
                c_change_only = false;
                was_nothing = false;
                ++working_memory_delta_size;

                // update min
                (*thisAgent->EpMem->epmem_edge_mins)[static_cast<size_t>((*temp_node) - 1)] = time_counter;

                thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(1, LLONG_MAX);
                thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(2, *temp_node);
                thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->execute(soar_module::op_reinit);

                epmem_edge.pop();
            }
        }
        thisAgent->EpMem->epmem_timers->storage_insert_ids->stop();

        // all removals
        {

            epmem_time_id range_start;
            epmem_time_id range_end;

            thisAgent->EpMem->epmem_timers->storage_remove_constants->start();
            // wme's with constant values
            {
                epmem_id_removal_map::iterator r;
                r = thisAgent->EpMem->epmem_node_removals->begin();
                while (r != thisAgent->EpMem->epmem_node_removals->end())
                {//so, basically, if we encounter a float here, it takes one of x paths. It may turn out we have a direction of change change -- in which case it's a proper float_now table removal.
                    //It may be the case that the float is removed altogether, which is also a proper float table removal.
                    //when the direction of change remains the same, it's not a real removal. That means the float was a potential delta, but then turned out not to be a delta.
                    //
                    bool did_number_change = false;
                    if (r->second)
                    {

                        //To check for removal where there is also addition with the same parent and attribute
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->bind_int(1,r->first);//gets parent, attr, value from wc_id.
                        //Note that this just gives the hashes, which is fine for the parent and attr, but not the value.
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->execute();
                        int64_t parent_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(0);
                        int64_t attr_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(1);
                        int64_t value_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(2);
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->reinitialize();
                        Symbol* value = epmem_reverse_hash(thisAgent, value_hash);

                        bool did_already = false;
                        //If i'm going todo change my +/- thing into bins instead, need some kind of easy way to make bins, and ideally shouldn't be too horribly-sensitive to values that are truly close to bin boundaries.

                        //solution aspect 1 -- have multiple out-of-phase bins, pick best bin.
                        //solution aspect 2 -- have automatic creation of max bin size and min bin size
                        //solution aspect 3 -- have multiple simultaneous bins (hierarchy as if hierarchical clustering)
                        //2.1 -- minimum bin size should be bigger than smallest difference between successive values, also bigger than noise magnitude.
                        //2.2 -- biggest bin size should be smaller than delta between biggest value and smallest value, and as a rule-of-thumb -- divided by 2.
                        //2.3 -- intermediate bin sizes should be exponential/log.
                        //noise estimation is equivalent to comparing a given binning to compression of random values (must be distinguishable)



                        //First, we can check if there exists a parent,attr in either potential delta map before doing further processing.
                        if (potential_float_deltas.find(std::pair<int64_t,int64_t>(parent_hash,attr_hash)) != potential_float_deltas.end() && value->symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
                        {//I just want to emphasize that the spaghetti here for how to handle floats comes down to "implement some form of discretization/feature-recognition" before storing to epmem.
                            std::map<std::pair<int64_t,int64_t>,std::pair<int64_t,double>>::iterator delta_it = potential_float_deltas.find(std::pair<int64_t,int64_t>(parent_hash,attr_hash));

                            //If we have a change, then we can make sure not to treat as an addition or a subtraction and here add to the change table, but also prevent from being added to the remove table.
                            //Things which were added, but not also removed, those will be later treated as final additions for sequitur.
                            double change = delta_it->second.second - value->fc->value; //newly-added value minus the being removed value.
                            //instead of doing a minus, will do a "are these in the same bin?"
                            //two ways to proceed -- custom hand-make each bin (should instead do that in roomsworld code)
                            // or -- automatic through Soar. If automatic, how?
                            //

                            //need to have a check for whether something was an unchanged float for a duration. basically, no "points" for nonchange, but can have ranges of nonchange.
                            if (change > 0.001 || change < -0.001)//need a characterization of sensor noise.//independently from that, in practicality, this also is used to "if" out the case of the val at last change being *exactly* the same.
                            {//will likely do something like hysteresis binning eventually. -- adaptive hysteresis gap based on what appears more informative than noise.
                                //double change = thisAgent->EpMem->val_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)] - value->fc->value;


                                if (thisAgent->EpMem->prev_delta->number_changes_find(parent_hash, attr_hash, change > 0.0) == thisAgent->EpMem->prev_delta->number_changes_end() && (thisAgent->EpMem->change_at_last_change.find(std::pair<int64_t,int64_t>(parent_hash, attr_hash)) == thisAgent->EpMem->change_at_last_change.end() || change > 0.0 != thisAgent->EpMem->change_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)]))
                                {//add_epmem_wmes_float = new soar_module::sqlite_statement(new_db, "INSERT INTO epmem_wmes_float (parent_n_id,attribute_s_id,direction) VALUES (?,?,?)");
                                    epmem_node_id f_id;
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(1,parent_hash);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(2,attr_hash);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(3,(change>0.0 ? 1 : -1));//can basically use exactly the same code for most of this when I have binning, but just more "values" than +1, -1.
                                    bool found = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->execute() == soar_module::row;
                                    if (found)
                                    {
                                        f_id = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->column_int(0);
                                        thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();
                                    }
                                    else
                                    {
                                        thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(1, parent_hash);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(2, attr_hash);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(3, (change>0.0 ? 1 : -1));
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->execute(soar_module::op_reinit); //gonna have to special case the very very first insert at a given location.
                                        //At the creation of the first float at a location, can do an update with a dummy direction, then use an update statement on the next d.c.
                                        //also do changing from now to point or range and insertion into now.
                                        //for the "now" and other tables -- usually assumed that we have the id for the constant associated with the symbol passed through working memory. here, we
                                        //might have to look it up instead.
                                        f_id = thisAgent->EpMem->epmem_db->last_insert_rowid();
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(1,FLOAT_CONSTANT_SYMBOL_TYPE);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(2,f_id);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->execute(soar_module::op_reinit);
                                    }

                                    //this gives the wf_id of the previous interval that's still in the now table.
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(1,parent_hash);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(2,attr_hash);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(3,(thisAgent->EpMem->change_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)]) ? 1 : -1);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->execute();
                                    epmem_node_id previous_float_id = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->column_int(0);
                                    thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();


                                    //We need the start_time of that float.
                                    thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->bind_int(1,previous_float_id);
                                    thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->execute();
                                    int64_t time_start = thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->column_int(0);
                                    thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->reinitialize();

                                    if (!created_interval_time)
                                    {
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_counter);
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,-1);
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                                        now_interval_time_id = thisAgent->EpMem->epmem_db->last_insert_rowid();
                                        created_interval_time = true;
                                    }

                                    //We take that id out of the float_now table
                                    thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_float_now->bind_int(1,previous_float_id);
                                    thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_float_now->execute(soar_module::op_reinit);

                                    //Now, that id gets added to the range or point table.
                                    if (time_counter-time_start > 1)
                                    {//range
//                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(1,);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(1,time_start);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(2,time_counter-1);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(3,previous_float_id);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->execute(soar_module::op_reinit);
                                        //Now, there's a big deal to how I've done this here. The rit_state_graph approach basically assumes in its structure (distributed throughout episodic_memory.cpp and episodic_memory.h)
                                        //that we have unique indexing for contants, but I've gone and overloaded floats, basically, so that the individual uniquely-valued float isn't the thing I refer to in the independent float table.
                                        //Anyways, my point is that I'm intentionally not making this compatible with the existing rit approach until there is sufficient justification to treat my weird version of floats as
                                        //being the actual data structures the agent uses. If I get there, I'd also want to then not treat floats the way they are now, meaning I could instead more naturally integrate my floats
                                        //back within the constant tables, not needing new tables.
                                        //So, long story short, I'd be doing unnecessary work and I won't do that until I see a good reason.
                                    }
                                    else
                                    {//point
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->bind_int(1,previous_float_id);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->bind_int(2,time_start);
                                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->execute(soar_module::op_reinit);
                                    }


                                    some_delta->add_number_change(parent_hash, attr_hash, change > 0.0);//std::pair<std::pair<int64_t,int64_t>, bool>
                                    was_nothing = false;
                                    //thisAgent->EpMem->val_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)] = delta_it->second.second;
                                    thisAgent->EpMem->change_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)] = change > 0.0;
                                    /* todo Don't actually do surprise here. do altogether at end of cycle, but before weight updates. */ //double temp_surprise = epmem_surprise_hebbian(thisAgent, time_counter, true, parent_hash, attr_hash, f_id, true);//Often, a float is really just a change in an existing value, for which I treat calculation of surprise as qualitatively distinct from noncontinuous constant types.
                                    //We add the new id to the float_now table
                                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->bind_int(1,f_id);
                                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->bind_int(2,time_counter);
                                    thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->execute(soar_module::op_reinit);


                                    //armed with a surprise value and a freshly-started interval, we can insert a row into the epmem_intervals table.
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(1,now_interval_time_id);//time_id from time index
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_double(2,1.0);//surprise value
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(3,time_counter);//start_time
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(4,FLOAT_CONSTANT_SYMBOL_TYPE);//w_id from master index comes from the type and the type-specific id. The query handles it.
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(5,f_id);
                                    thisAgent->EpMem->epmem_stmts_graph->add_interval_data->execute(soar_module::op_reinit);

                                    //Meanwhile, the old interval has data in the interval table that needs updating because now it has a stop point.
                                    //First, we put the new time interval for that old interval into the time table, and get a time id from that.
                                    thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(1,time_start);
                                    thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(2,time_counter-1);
                                    bool has_time_id = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->execute() == soar_module::row;
                                    epmem_time_id new_interval_time;
                                    if (has_time_id)
                                    {
                                        new_interval_time = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->column_int(0);
                                        thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                                    }
                                    else
                                    {
                                        thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_start);
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,time_counter-1);
                                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                                        new_interval_time = thisAgent->EpMem->epmem_db->last_insert_rowid();
                                    }
                                    //Then, we can update the existing record of that interval with the updated interval id.
                                    //So, first we find the existing record.
                                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(1,FLOAT_CONSTANT_SYMBOL_TYPE);
                                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(2,previous_float_id);
                                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->execute();
                                    int64_t existing_interval_id = thisAgent->EpMem->epmem_stmts_graph->find_now_interval->column_int(0);
                                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->reinitialize();
                                    //Then, we update it. // These two could be made more efficient with an "update from join", but I think I would have to update Soar's packaged sqlite version. sqlite is unfathomably backwards compatible, so it should be fine.
                                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(1,new_interval_time);
                                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(2,existing_interval_id);
                                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->execute(soar_module::op_reinit);
                                    //Now that this grounded interval instance has ended, there are potential before relations to store/update.


                                }
                            }
                            thisAgent->EpMem->val_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)] = delta_it->second.second;//I don't think this should ever matter... not commenting it out yet.
                            //potential_float_deltas.erase(delta_it);
                            potential_delta_ids.erase(delta_it->second.first);

                            did_already = true;

                        }
                        else if (value->symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
                        {
                            //This means that if we don't have a match, we simply have a removal.
                            //here, we move from now to point or range.


                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(1,parent_hash);
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(2,attr_hash);
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(3,thisAgent->EpMem->change_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)]);
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->execute();
                            epmem_node_id previous_float_id = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->column_int(0);
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();




                            //We need the start_time of that float.
                            thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->bind_int(1,previous_float_id);
                            thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->execute();
                            int64_t time_start = thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->column_int(0);
                            thisAgent->EpMem->epmem_stmts_graph->find_time_epmem_wmes_float_now->reinitialize();

                            //We take that id out of the float_now table
                            thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_float_now->bind_int(1,previous_float_id);
                            thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_float_now->execute(soar_module::op_reinit);
                            if (time_counter-time_start > 1)
                            {//range

                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(1,time_start);
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(2,time_counter-1);
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->bind_int(3,previous_float_id);
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_range->execute(soar_module::op_reinit);
                            }
                            else
                            {//point
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->bind_int(1,previous_float_id);
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->bind_int(2,time_start);
                                thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_point->execute(soar_module::op_reinit);
                            }

                            //First, we put the new time interval for that old interval into the time table, and get a time id from that.
                            thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(1,time_start);
                            thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(2,time_counter-1);
                            bool has_time_id = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->execute() == soar_module::row;
                            epmem_time_id new_interval_time;
                            if (has_time_id)
                            {
                                new_interval_time = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->column_int(0);
                                thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                            }
                            else
                            {
                                thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_start);
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,time_counter-1);
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                                new_interval_time = thisAgent->EpMem->epmem_db->last_insert_rowid();
                            }
                            //Then, we can update the existing record of that interval with the updated interval id.
                            //So, first we find the existing record.
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(1,FLOAT_CONSTANT_SYMBOL_TYPE);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(2,previous_float_id);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->execute();
                            int64_t existing_interval_id = thisAgent->EpMem->epmem_stmts_graph->find_now_interval->column_int(0);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->reinitialize();
                            //Then, we update it.
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(1,new_interval_time);
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(2,existing_interval_id);
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->execute(soar_module::op_reinit);


                            some_delta->add_removal_constant(r->first);
                            was_nothing = false;
                            c_change_only = false;
                            thisAgent->EpMem->change_at_last_change.erase(std::pair<int64_t,int64_t>(parent_hash, attr_hash));
                            thisAgent->EpMem->val_at_last_change.erase(std::pair<int64_t,int64_t>(parent_hash, attr_hash));
                            did_already = true;
                            //end an interval here.
                        }




                        // remove NOW entry
                        // id = ?
                        thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_constant_now->bind_int(1, r->first);
                        thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_constant_now->execute(soar_module::op_reinit);
                        /*
                         * This is where we know we've taken something out of working memory.
                         */
                        if (!did_already)
                        {
                            some_delta->add_removal_constant(r->first);
                            c_change_only = false;
                            was_nothing = false;
                            //thisAgent->EpMem->change_at_last_change.erase(std::pair<int64_t,int64_t>(parent_hash, attr_hash));
                            //thisAgent->EpMem->val_at_last_change.erase(std::pair<int64_t,int64_t>(parent_hash, attr_hash));
                        }
                        ++working_memory_delta_size;

                        range_start = (*thisAgent->EpMem->epmem_node_mins)[static_cast<size_t>(r->first - 1)];
                        range_end = (time_counter - 1);

                        // point (id, start_episode_id)
                        if (range_start == range_end)
                        {
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_point->bind_int(1, r->first);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_point->bind_int(2, range_start);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_constant_point->execute(soar_module::op_reinit);
                        }
                        // node
                        else
                        {
                            epmem_rit_insert_interval(thisAgent, range_start, range_end, r->first, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ]));
                        }
                        if (!did_already)
                        {
                            //First, we put the new time interval for that old interval into the time table, and get a time id from that.
                            thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(1,range_start);
                            thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(2,range_end);
                            bool has_time_id = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->execute() == soar_module::row;
                            epmem_time_id new_interval_time;
                            if (has_time_id)
                            {
                                new_interval_time = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->column_int(0);
                                thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                            }
                            else
                            {
                                thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,range_start);
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,range_end);
                                thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                                new_interval_time = thisAgent->EpMem->epmem_db->last_insert_rowid();
                            }
                            //Then, we can update the existing record of that interval with the updated interval id.
                            //So, first we find the existing record.

                            thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->bind_int(1,r->first);//gets parent, attr, value from wc_id.
                            //Note that this just gives the hashes, which is fine for the parent and attr, but not the value.
                            thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->execute();
                            int64_t value_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(2);
                            thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->reinitialize();
                            thisAgent->EpMem->epmem_stmts_common->hash_get_type->bind_int(1, value_hash);
                            soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_get_type->execute();
                            (void)res; // quells compiler warning
                            assert(res == soar_module::row);
                            byte sym_type = static_cast<byte>(thisAgent->EpMem->epmem_stmts_common->hash_get_type->column_int(0));
                            thisAgent->EpMem->epmem_stmts_common->hash_get_type->reinitialize();
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(1,sym_type);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(2,r->first);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->execute();
                            int64_t existing_interval_id = thisAgent->EpMem->epmem_stmts_graph->find_now_interval->column_int(0);
                            thisAgent->EpMem->epmem_stmts_graph->find_now_interval->reinitialize();
                            //Then, we update it.
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(1,new_interval_time);
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(2,existing_interval_id);
                            thisAgent->EpMem->epmem_stmts_graph->update_interval_data->execute(soar_module::op_reinit);
                        }
                        // update max
                        (*thisAgent->EpMem->epmem_node_maxes)[static_cast<size_t>(r->first - 1)] = true;
                    }

                    r++;
                }
                thisAgent->EpMem->epmem_node_removals->clear();
                // At this point, we can iterate through the remaining potential deltas and treat them as additions.
                //loop over remaining potential delta ids.
                //wc_ids *are* epmem_ids.
                std::set<epmem_node_id>::iterator potential_delta_ids_it;
                std::set<epmem_node_id>::iterator potential_delta_ids_begin = potential_delta_ids.begin();
                std::set<epmem_node_id>::iterator potential_delta_ids_end = potential_delta_ids.end();




                for (potential_delta_ids_it = potential_delta_ids_begin; potential_delta_ids_it != potential_delta_ids_end; ++potential_delta_ids_it)
                {// actual additions


                    some_delta->add_addition_constant(*potential_delta_ids_it);
                    c_change_only = false;
                    was_nothing = false;
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->bind_int(1,*potential_delta_ids_it);//gets parent, attr, value from wc_id.
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->execute();
                    int64_t value_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(2);
                    thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->reinitialize();
                    thisAgent->EpMem->epmem_stmts_common->hash_get_type->bind_int(1,value_hash);
                    soar_module::exec_result res = thisAgent->EpMem->epmem_stmts_common->hash_get_type->execute();
                    (void)res; // quells compiler warning
                    assert(res == soar_module::row);
                    byte sym_type = static_cast<byte>(thisAgent->EpMem->epmem_stmts_common->hash_get_type->column_int(0));// pretty sure I don't need this anymore -- was from when delta_ids wasn't just for floats. not the case in this branch (surprise-based encoding) (is always floats).
                    thisAgent->EpMem->epmem_stmts_common->hash_get_type->reinitialize();
                    if(sym_type == FLOAT_CONSTANT_SYMBOL_TYPE)//todo this is another place to do a float insert
                    {//Some floats are altogether fresh, not deltas. Those get their surprise calculated here.
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->bind_int(1,*potential_delta_ids_it);//gets parent, attr, value from wc_id.
                        //Note that this just gives the hashes, which is fine for the parent and attr, but not the value.
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->execute();
                        int64_t parent_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(0);
                        int64_t attr_hash = thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->column_int(1);
                        thisAgent->EpMem->epmem_stmts_graph->get_single_wcid_info->reinitialize();

                        epmem_node_id f_id;
                        thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(1,parent_hash);
                        thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(2,attr_hash);
                        thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->bind_int(3,0);
                        bool found = thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->execute() == soar_module::row;
                        if (found)
                        {
                            f_id = static_cast<epmem_node_id>(thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->column_int(0));
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();
                        }
                        else
                        {
                            thisAgent->EpMem->epmem_stmts_graph->find_epmem_wmes_float->reinitialize();
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(1, parent_hash);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(2, attr_hash);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->bind_int(3, 0);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float->execute(soar_module::op_reinit); //gonna have to special case the very very first insert at a given location.
                            //At the creation of the first float at a location, can do an update with a dummy direction, then use an update statement on the next d.c.
                            //also do changing from now to point or range and insertion into now.
                            //for the "now" and other tables -- usually assumed that we have the id for the constant associated with the symbol passed through working memory. here, we
                            //might have to look it up instead.
                            f_id = static_cast<epmem_node_id>(thisAgent->EpMem->epmem_db->last_insert_rowid());
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(1,FLOAT_CONSTANT_SYMBOL_TYPE);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->bind_int(2,f_id);
                            thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes->execute(soar_module::op_reinit);
                        }

                        /* todo Don't actually do surprise here. do altogether at end of cycle, but before weight updates. */ //double temp_surprise = epmem_surprise_hebbian(thisAgent, time_counter, true, parent_hash, attr_hash, f_id, true);

                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->bind_int(1,f_id);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->bind_int(2,time_counter);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_float_now->execute(soar_module::op_reinit);

                        if (!created_interval_time)
                        {
                            thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,time_counter);
                            thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,-1);
                            thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                            now_interval_time_id = thisAgent->EpMem->epmem_db->last_insert_rowid();
                            created_interval_time = true;
                        }

                        //armed with a surprise value and a freshly-started interval, we can insert a row into the epmem_intervals table.
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(1,now_interval_time_id);//time_id from time index
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_double(2,1.0);//surprise value
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(3,time_counter);//start_time
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(4,FLOAT_CONSTANT_SYMBOL_TYPE);//w_id from master index comes from the type and the type-specific id. The query handles it.
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->bind_int(5,f_id);
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_data->execute(soar_module::op_reinit);


                        //Note that this call is passing the constant id, not the float id. However, in the case of floats, it's actually unused altogether, which is confusing, but there you go.
                        thisAgent->EpMem->change_at_last_change[std::pair<int64_t,int64_t>(parent_hash, attr_hash)] = 0.0;
                    }
                }
            }
            thisAgent->EpMem->epmem_timers->storage_remove_constants->stop();

            thisAgent->EpMem->epmem_timers->storage_remove_ids->start();
            // wme's with identifier values
            epmem_edge_removal_map::iterator r = thisAgent->EpMem->epmem_edge_removals->begin();
            while (r != thisAgent->EpMem->epmem_edge_removals->end())
            {
                if (r->second)
                {
                    // remove NOW entry
                    // id = ?
                    thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_identifier_now->bind_int(1, r->first.first);
                    thisAgent->EpMem->epmem_stmts_graph->delete_epmem_wmes_identifier_now->execute(soar_module::op_reinit);

                    /*
                     * This is where we know we've taken something out of working memory.
                     */
                    some_delta->add_removal(r->first.first);
                    was_nothing = false;
                    c_change_only = false;
                    ++working_memory_delta_size;

                    range_start = (*thisAgent->EpMem->epmem_edge_mins)[static_cast<size_t>(r->first.first - 1)];
                    range_end = (time_counter - 1);

                    thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(1, range_end);
                    thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->bind_int(2, r->first.first);
                    thisAgent->EpMem->epmem_stmts_graph->update_epmem_wmes_identifier_last_episode_id->execute(soar_module::op_reinit);
                    // point (id, start_episode_id)
                    if (range_start == range_end)
                    {
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_point->bind_int(1, r->first.first);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_point->bind_int(2, range_start);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_point->bind_int(3, r->first.second);
                        thisAgent->EpMem->epmem_stmts_graph->add_epmem_wmes_identifier_point->execute(soar_module::op_reinit);
                    }
                    // node
                    else
                    {
                        epmem_rit_insert_interval(thisAgent, range_start, range_end, r->first.first, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ]), r->first.second);
                    }
                    //Meanwhile, the old interval has data in the interval table that needs updating because now it has a stop point.
                    //First, we put the new time interval for that old interval into the time table, and get a time id from that.
                    thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(1,range_start);
                    thisAgent->EpMem->epmem_stmts_graph->get_interval_time->bind_int(2,range_end);
                    bool has_time_id = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->execute() == soar_module::row;
                    epmem_time_id new_interval_time;
                    if (has_time_id)
                    {
                        new_interval_time = thisAgent->EpMem->epmem_stmts_graph->get_interval_time->column_int(0);
                        thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                    }
                    else
                    {
                        thisAgent->EpMem->epmem_stmts_graph->get_interval_time->reinitialize();
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(1,range_start);
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->bind_int(2,range_end);
                        thisAgent->EpMem->epmem_stmts_graph->add_interval_time->execute(soar_module::op_reinit);
                        new_interval_time = thisAgent->EpMem->epmem_db->last_insert_rowid();
                    }
                    //Then, we can update the existing record of that interval with the updated interval id.
                    //So, first we find the existing record.
                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(1,IDENTIFIER_SYMBOL_TYPE);
                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->bind_int(2,r->first.first);
                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->execute();
                    int64_t existing_interval_id = thisAgent->EpMem->epmem_stmts_graph->find_now_interval->column_int(0);
                    thisAgent->EpMem->epmem_stmts_graph->find_now_interval->reinitialize();
                    //Then, we update it.
                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(1,new_interval_time);
                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->bind_int(2,existing_interval_id);
                    thisAgent->EpMem->epmem_stmts_graph->update_interval_data->execute(soar_module::op_reinit);

                    // update max
                    (*thisAgent->EpMem->epmem_edge_maxes)[static_cast<size_t>(r->first.first - 1)] = true;
                }

                r++;
            }
            thisAgent->EpMem->epmem_edge_removals->clear();
            thisAgent->EpMem->epmem_timers->storage_remove_ids->stop();
        }
        //The following block of code calculates the surprise for the things that happened this cycle based on what the weights were before. It's a post-hoc "how unexpected was that given what I knew" surprise using smem spread.
        thisAgent->EpMem->epmem_timers->storage_do_surprise->start();
        {// Let's say there's a maximum of 1 unit of surprise for an unaccounted for transition. This means that something altogether unexpected doesn't have infinite surprise, but O(size-of-context) surprise.
            //First things first, need the collection of things for which there is to be a calculation of surprise -- things that showed up this cycle.

            std::set<uint64_t> current_candidates;//need to populate with things that showed up this cycle. (the targets for spread from sources that were things present last cycle).
            bool do_manual_crawl = false;
            //for each row in the now table that has a start_episode id equal to right actually now.
            thisAgent->EpMem->epmem_stmts_graph->select_new_nows->bind_int(1,(time_counter));//could make more efficient by tracking them in the cpp code here. doing this for now for simplicity.
            while (thisAgent->EpMem->epmem_stmts_graph->select_new_nows->execute() == soar_module::row)
            {
                current_candidates.insert(thisAgent->EpMem->epmem_stmts_graph->select_new_nows->column_int(0));
            }
            thisAgent->EpMem->epmem_stmts_graph->select_new_nows->reinitialize();

            thisAgent->SMem->calc_spread(&current_candidates,do_manual_crawl);

            //Now that spread has been calculated, we want to association something like "1-spread" to the newly-added elements.
            //epmem_surprise_hebbian_batch(thisAgent);
            thisAgent->EpMem->epmem_stmts_graph->count_old_nows->bind_int(1,(time_counter));
            thisAgent->EpMem->epmem_stmts_graph->count_old_nows->execute();
            uint64_t total_number_of_sources = thisAgent->EpMem->epmem_stmts_graph->count_old_nows->column_int(0);
            thisAgent->EpMem->epmem_stmts_graph->count_old_nows->reinitialize();

            thisAgent->EpMem->epmem_stmts_graph->update_interval_surprise->bind_double(1,static_cast<double>(total_number_of_sources));//size of the now table before this cycle's additions.
            thisAgent->EpMem->epmem_stmts_graph->update_interval_surprise->execute(soar_module::op_reinit);
//UPDATE epmem_intervals SET surprise=(1.0-(sc.kinda_spread/?)) FROM epmem_w_id_to_lti_id AS wl INNER JOIN (SELECT sum(num_appearances_i_j/num_appearances) AS kinda_spread, lti_id FROM smem_current_spread GROUP BY lti_id) AS sc ON sc.lti_id=wl.lti_id WHERE epmem_intervals.w_id=wl.w_id");


            //to add to smem_context_additions, we select from the "now" table in epmem those which have a start_episode_id equal to the current time index within epmem. (they started this cycle)
            //to add to smem_context_removals, we select from the "epmem_wmes_temp" table -- things that were just stopped, and so are no longer now.
            //BUT, we only do this AFTER spread because things that were present last cycle are suitable for doing this spread.
            //so, the calculation of spread here uses last cycle's smem_context_removals and smem_context_additions. We accomplish this by updating after we call calc_spread.

            //for each row in the now table that has a start episode id equal to right actually now.
            thisAgent->EpMem->epmem_stmts_graph->select_new_nows->bind_int(1,(time_counter));//could make more efficient by tracking them in the cpp code here. doing this for now for simplicity.
            while (thisAgent->EpMem->epmem_stmts_graph->select_new_nows->execute() == soar_module::row)//the only way this is correct is if the nows that only just stopped aren't yet removed. (they arne't until the next code block).
            {
                thisAgent->SMem->smem_context_additions->insert(thisAgent->EpMem->epmem_stmts_graph->select_new_nows->column_int(0));
            }
            thisAgent->EpMem->epmem_stmts_graph->select_new_nows->reinitialize();

            //now we can add removals:
            //for each row in the epmem_wmes_temp table.
            while (thisAgent->EpMem->epmem_stmts_graph->select_removed_nows->execute() == soar_module::row)
            {
                thisAgent->SMem->smem_context_removals->insert(thisAgent->EpMem->epmem_stmts_graph->select_removed_nows->column_int(0));// Currently a set of unsigned ints. need to recheck code //TODO everywhere! for ltis being forced into unsigned, since I'm using signed ones now. (half of almost infinity is almostish infinity -- hoping 64 bits hold out).
            }
            thisAgent->EpMem->epmem_stmts_graph->select_removed_nows->reinitialize();
        }
        thisAgent->EpMem->epmem_timers->storage_do_surprise->stop();
        thisAgent->EpMem->epmem_timers->storage_do_edge_updates->start();//expensive//todo
        // The next block of code updates the weights based on what happened this cycle.
        {//Now that the additions and then removals are done, we can update the relation strengths (right now only "before" relations)
            //the logic -- for everything that we do a positive update for, do a negative update for the things that don't show up -- basically copying the existing smem code for this
            //we have a log of the timeids and w_ids for updates to do
            //The "select_updates" query orders by w_id_left, so we can collect all of the things w_id_left goes to before committing to depreciating the rest.
            //also, we defer depreciation until it's clear remaining "nows" aren't potentially going to actually increase the weight (but just haven't left yet). alternatively, we do a count-based update and don't worry about specific timings of pre-post, just that a form of "before" was satisfied.
            //for candidate "ands" in later reasoning that will want to make composite symbols, would help to keep track of "not together often, but when together, followed by y" as selective features.
            //when a given left element leaves, you know for sure which potential updates it has. when the right element leaves, you know for sure the update.
            //so, can immediately depreciate all that aren't potential, then mark as having done this (on first right element for a given left). then, for remaining left, just get to them eventually.
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_1->start();
            thisAgent->EpMem->epmem_stmts_graph->insert_potential_before_relations->execute(soar_module::op_reinit);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_1->stop();
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_2->start();
            thisAgent->EpMem->epmem_stmts_graph->update_unobserved_before_relations->execute(soar_module::op_reinit);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_2->stop();
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_3->start();
            thisAgent->EpMem->epmem_stmts_graph->update_observed_before_relations->execute(soar_module::op_reinit);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_3->stop();
            //Before doing these deletes, could use these tables to update smem edges. (don't want to always update all edges, just those changed here.)
            //todo -- this is the place to update edge weights.

            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4->start();
            epmem_update_spread(thisAgent);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_4->stop();
            //epmem_update_spread is written such that "finish_updates" needs to happen after, not before.

            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_5->start();
            thisAgent->EpMem->epmem_stmts_graph->finish_updates->execute(soar_module::op_reinit);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_5->stop();
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_6->start();
            thisAgent->EpMem->epmem_stmts_graph->delete_removed_nows->execute(soar_module::op_reinit);
            thisAgent->EpMem->epmem_timers->storage_do_edge_updates_6->stop();
            //importantly, the calculation of surprise before these updates depended on the state of metadata before these updates.
        }
        thisAgent->EpMem->epmem_timers->storage_do_edge_updates->stop();

        // add the time id to the epmem_episodes table
        thisAgent->EpMem->epmem_stmts_graph->add_time->bind_int(1, time_counter);
        thisAgent->EpMem->epmem_stmts_graph->add_time->execute(soar_module::op_reinit);

        thisAgent->EpMem->epmem_stats->time->set_value(time_counter + 1);

        // update time wme on all states
        {
            Symbol* state = thisAgent->bottom_goal;
            Symbol* my_time_sym = thisAgent->symbolManager->make_int_constant(time_counter + 1);

            while (state != NULL)
            {
                if (state->id->epmem_info->epmem_time_wme != NIL)
                {
                    soar_module::remove_module_wme(thisAgent, state->id->epmem_info->epmem_time_wme);
                }

                state->id->epmem_info->epmem_time_wme = soar_module::add_module_wme(thisAgent, state->id->epmem_info->epmem_link_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_present_id, my_time_sym);

                state = state->id->higher_goal;
            }

            thisAgent->symbolManager->symbol_remove_ref(&my_time_sym);
        }

        // clear add/remove maps
        {
            thisAgent->EpMem->epmem_wme_adds->clear();
        }


        if (!(was_nothing && thisAgent->EpMem->no_immediately_previous_change) && !(c_change_only  && (*some_delta) == (*(thisAgent->EpMem->prev_delta))))//TODO: sjj -- It may be that cycle-specific timings are important to encode and this should be turned into an optional parameter for epmem compression.
        {
            if (!was_nothing)
            {
                delete thisAgent->EpMem->prev_delta;
                thisAgent->EpMem->prev_delta = new EpMem_Id_Delta(*some_delta);
                //Don't need to do surprise stuff here. Can do at time of insert into the delta structure.
            }
        }

        thisAgent->EpMem->no_immediately_previous_change = was_nothing;
    }
//    std::string err;
//
//    bool result = epmem_backup_db(thisAgent, "mid_process_error.db", &(err));
    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->storage->stop();
    ////////////////////////////////////////////////////////////////////////////
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Non-Cue-Based Retrieval Functions (epmem::ncb)
//
// NCB retrievals occur when you know the episode you
// want to retrieve.  It is the process of converting
// the database representation to WMEs in working
// memory.
//
// This occurs at the end of a cue-based query, or
// in response to a retrieve/next/previous command.
//
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/***************************************************************************
 * Function     : epmem_valid_episode
 * Author       : Nate Derbinsky
 * Notes        : Returns true if the temporal id is valid
 **************************************************************************/
bool epmem_valid_episode(agent* thisAgent, epmem_time_id memory_id)
{
    bool return_val = false;

    {
        soar_module::sqlite_statement* my_q = thisAgent->EpMem->epmem_stmts_graph->valid_episode;

        my_q->bind_int(1, memory_id);
        my_q->execute();
        return_val = (my_q->column_int(0) > 0);
        my_q->reinitialize();
    }

    return return_val;
}

inline void _epmem_install_id_wme(agent* thisAgent, Symbol* parent, Symbol* attr, std::map< epmem_node_id, std::pair< Symbol*, bool > >* ids, epmem_node_id child_n_id, uint64_t val_num, epmem_id_mapping* id_record, symbol_triple_list& retrieval_wmes)
{
    std::map< epmem_node_id, std::pair< Symbol*, bool > >::iterator id_p = ids->find(child_n_id);
    bool existing_identifier = (id_p != ids->end());

        if (!existing_identifier)
        {
            if (val_num)
            {
                id_p = ids->insert(std::make_pair(child_n_id, std::make_pair(thisAgent->symbolManager->make_new_identifier(((attr->symbol_type == STR_CONSTANT_SYMBOL_TYPE) ? (attr->sc->name[0]) : ('L')), parent->id->level), true))).first;
                if (thisAgent->SMem->lti_exists(val_num))
                {
                    id_p->second.first->id->LTI_ID = val_num;
                    id_p->second.first->update_cached_lti_print_str();
                    id_p->second.first->id->LTI_epmem_valid = thisAgent->EpMem->epmem_validation;
                }
            } else {
                id_p = ids->insert(std::make_pair(child_n_id, std::make_pair(thisAgent->symbolManager->make_new_identifier(((attr->symbol_type == STR_CONSTANT_SYMBOL_TYPE) ? (attr->sc->name[0]) : ('E')), parent->id->level), true))).first;
            }
            if (id_record)
            {
                epmem_id_mapping::iterator rec_p = id_record->find(child_n_id);
                if (rec_p != id_record->end())
                {
                    rec_p->second = id_p->second.first;
                }
            }
        }

        epmem_buffer_add_wme(thisAgent, retrieval_wmes, parent, attr, id_p->second.first);

        if (!existing_identifier)
        {
            thisAgent->symbolManager->symbol_remove_ref(&id_p->second.first);
        }
}

/***************************************************************************
 * Function     : epmem_install_memory
 * Author       : Nate Derbinsky
 * Notes        : Reconstructs an episode in working memory.
 *
 *                Use RIT to collect appropriate ranges.  Then
 *                combine with NOW and POINT.  Merge with unique
 *                to get all the data necessary to create WMEs.
 *
 *                The id_record parameter is only used in the case
 *                that the graph-match has a match and creates
 *                a mapping of identifiers that should be recorded
 *                during reconstruction.
 **************************************************************************/
void epmem_install_memory(agent* thisAgent, Symbol* state, epmem_time_id memory_id, symbol_triple_list& meta_wmes, symbol_triple_list& retrieval_wmes, epmem_id_mapping* id_record = NULL)
{
    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->ncb_retrieval->start();
    ////////////////////////////////////////////////////////////////////////////

    // get the ^result header for this state
    Symbol* result_header = state->id->epmem_info->result_wme->value;

    // initialize stat
    int64_t num_wmes = 0;
    thisAgent->EpMem->epmem_stats->ncb_wmes->set_value(num_wmes);

    // if no memory, say so
    if ((memory_id == EPMEM_MEMID_NONE) ||
            !epmem_valid_episode(thisAgent, memory_id))
    {
        epmem_buffer_add_wme(thisAgent, meta_wmes, result_header, thisAgent->symbolManager->soarSymbols.epmem_sym_retrieved, thisAgent->symbolManager->soarSymbols.epmem_sym_no_memory);
        state->id->epmem_info->last_memory = EPMEM_MEMID_NONE;

        ////////////////////////////////////////////////////////////////////////////
        thisAgent->EpMem->epmem_timers->ncb_retrieval->stop();
        ////////////////////////////////////////////////////////////////////////////

        return;
    }

    // remember this as the last memory installed
    state->id->epmem_info->last_memory = memory_id;

    // create a new ^retrieved header for this result
    Symbol* retrieved_header;
    retrieved_header = thisAgent->symbolManager->make_new_identifier('R', result_header->id->level);
    if (id_record)
    {
        (*id_record)[ EPMEM_NODEID_ROOT ] = retrieved_header;
    }

    epmem_buffer_add_wme(thisAgent, meta_wmes, result_header, thisAgent->symbolManager->soarSymbols.epmem_sym_retrieved, retrieved_header);
    thisAgent->symbolManager->symbol_remove_ref(&retrieved_header);

    // add *-id wme's
    {
        Symbol* my_meta;

        my_meta = thisAgent->symbolManager->make_int_constant(static_cast<int64_t>(memory_id));
        epmem_buffer_add_wme(thisAgent, meta_wmes, result_header, thisAgent->symbolManager->soarSymbols.epmem_sym_memory_id, my_meta);
        thisAgent->symbolManager->symbol_remove_ref(&my_meta);

        my_meta = thisAgent->symbolManager->make_int_constant(static_cast<int64_t>(thisAgent->EpMem->epmem_stats->time->get_value()));
        epmem_buffer_add_wme(thisAgent, meta_wmes, result_header, thisAgent->symbolManager->soarSymbols.epmem_sym_present_id, my_meta);
        thisAgent->symbolManager->symbol_remove_ref(&my_meta);
    }

    // install memory
    {
        // Big picture: create identifier skeleton, then hang non-identifers
        //
        // Because of shared WMEs at different levels of the storage breadth-first search,
        // there is the possibility that the unique database id of an identifier can be
        // greater than that of its parent.  Because the retrieval query sorts by
        // unique id ascending, it is thus possible to have an "orphan" - a child with
        // no current parent.  We keep track of orphans and add them later, hoping their
        // parents have shown up.  I *suppose* there could be a really evil case in which
        // the ordering of the unique ids is exactly opposite of their insertion order.
        // I just hope this isn't a common case...

        // shared identifier lookup table
        std::map< epmem_node_id, std::pair< Symbol*, bool > > ids;
        bool dont_abide_by_ids_second = (thisAgent->EpMem->epmem_params->merge->get_value() == epmem_param_container::merge_add);

        // symbols used to create WMEs
        Symbol* attr = NULL;

        // lookup query
        soar_module::sqlite_statement* my_q;

        // initialize the lookup table
        ids[ EPMEM_NODEID_ROOT ] = std::make_pair(retrieved_header, true);

        // first identifiers (i.e. reconstruct)
        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_identifier_values;
        {
            // relates to finite automata: child_n_id = d(parent_n_id, attribute_s_id)
            epmem_node_id parent_n_id; // id
            epmem_node_id child_n_id; // attribute

            uint64_t val_lti_id = NIL;

            // used to lookup shared identifiers
            // the bool in the pair refers to if children are allowed on this id (re: lti)
            std::map< epmem_node_id, std::pair< Symbol*, bool> >::iterator id_p;

            // orphaned children
            std::queue< epmem_edge* > orphans;
            epmem_edge* orphan;

            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ]));

            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            my_q->bind_int(5, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                // parent_n_id, attribute_s_id, child_n_id, epmem_node.lti_id
                parent_n_id = my_q->column_int(0);
                child_n_id = my_q->column_int(2);
                val_lti_id = (my_q->column_type(3) == soar_module::null_t ? 0 : static_cast<uint64_t>(my_q->column_int(3)));
                attr = epmem_reverse_hash(thisAgent, my_q->column_int(1));

                // get a reference to the parent
                id_p = ids.find(parent_n_id);
                if (id_p != ids.end())
                {
                    // if existing lti with kids don't touch
                    if (dont_abide_by_ids_second || id_p->second.second)
                    {
                        _epmem_install_id_wme(thisAgent, id_p->second.first, attr, &(ids), child_n_id, val_lti_id, id_record, retrieval_wmes);
                        num_wmes++;
                    }

                    thisAgent->symbolManager->symbol_remove_ref(&attr);
                }
                else
                {
                    // out of order
                    orphan = new epmem_edge;
                    orphan->parent_n_id = parent_n_id;
                    orphan->attribute = attr;
                    orphan->child_n_id = child_n_id;
                    orphan->child_lti_id = val_lti_id;

                    orphans.push(orphan);
                }
            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);

            // take care of any orphans
            if (!orphans.empty())
            {
                std::queue<epmem_edge*>::size_type orphans_left;
                std::queue<epmem_edge*> still_orphans;

                do
                {
                    orphans_left = orphans.size();

                    while (!orphans.empty())
                    {
                        orphan = orphans.front();
                        orphans.pop();

                        // get a reference to the parent
                        id_p = ids.find(orphan->parent_n_id);
                        if (id_p != ids.end())
                        {
                            if (dont_abide_by_ids_second || id_p->second.second)
                            {
                                _epmem_install_id_wme(thisAgent, id_p->second.first, orphan->attribute, &(ids), orphan->child_n_id, orphan->child_lti_id, id_record, retrieval_wmes);
                                num_wmes++;
                            }

                            thisAgent->symbolManager->symbol_remove_ref(&orphan->attribute);

                            delete orphan;
                        }
                        else
                        {
                            still_orphans.push(orphan);
                        }
                    }

                    orphans = still_orphans;
                    while (!still_orphans.empty())
                    {
                        still_orphans.pop();
                    }

                }
                while ((!orphans.empty()) && (orphans_left != orphans.size()));

                while (!orphans.empty())
                {
                    orphan = orphans.front();
                    orphans.pop();

                    thisAgent->symbolManager->symbol_remove_ref(&orphan->attribute);

                    delete orphan;
                }
            }
        }

        // then epmem_wmes_constant
        // f.wc_id, f.parent_n_id, f.attribute_s_id, f.value_s_id
        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_constant_values;
        {
            epmem_node_id parent_n_id;
            std::pair< Symbol*, bool > parent;
            Symbol* value = NULL;

            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ]));

            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                parent_n_id = my_q->column_int(1);

                // get a reference to the parent
                parent = ids[ parent_n_id ];

                if (dont_abide_by_ids_second || parent.second)
                {
                    // make a symbol to represent the attribute
                    attr = epmem_reverse_hash(thisAgent, my_q->column_int(2));

                    // make a symbol to represent the value
                    value = epmem_reverse_hash(thisAgent, my_q->column_int(3));

                    epmem_buffer_add_wme(thisAgent, retrieval_wmes, parent.first, attr, value);
                    num_wmes++;

                    thisAgent->symbolManager->symbol_remove_ref(&attr);
                    thisAgent->symbolManager->symbol_remove_ref(&value);
                }
            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);
        }
    }

    // adjust stat
    thisAgent->EpMem->epmem_stats->ncb_wmes->set_value(num_wmes);

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->ncb_retrieval->stop();
    ////////////////////////////////////////////////////////////////////////////
}

/***************************************************************************
 * Function     : epmem_next_episode
 * Author       : Nate Derbinsky
 * Notes        : Returns the next valid temporal id.  This is really
 *                only an issue if you implement episode dynamics like
 *                forgetting.
 **************************************************************************/
epmem_time_id epmem_next_episode(agent* thisAgent, epmem_time_id memory_id)
{
    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->next->start();
    ////////////////////////////////////////////////////////////////////////////

    epmem_time_id return_val = EPMEM_MEMID_NONE;

    if (memory_id != EPMEM_MEMID_NONE)
    {
        soar_module::sqlite_statement* my_q = thisAgent->EpMem->epmem_stmts_graph->next_episode;
        my_q->bind_int(1, memory_id);
        if (my_q->execute() == soar_module::row)
        {
            return_val = (epmem_time_id) my_q->column_int(0);
        }

        my_q->reinitialize();
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->next->stop();
    ////////////////////////////////////////////////////////////////////////////

    return return_val;
}

/***************************************************************************
 * Function     : epmem_previous_episode
 * Author       : Nate Derbinsky
 * Notes        : Returns the last valid temporal id.  This is really
 *                only an issue if you implement episode dynamics like
 *                forgetting.
 **************************************************************************/
epmem_time_id epmem_previous_episode(agent* thisAgent, epmem_time_id memory_id)
{
    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->prev->start();
    ////////////////////////////////////////////////////////////////////////////

    epmem_time_id return_val = EPMEM_MEMID_NONE;

    if (memory_id != EPMEM_MEMID_NONE)
    {
        soar_module::sqlite_statement* my_q = thisAgent->EpMem->epmem_stmts_graph->prev_episode;
        my_q->bind_int(1, memory_id);
        if (my_q->execute() == soar_module::row)
        {
            return_val = (epmem_time_id) my_q->column_int(0);
        }

        my_q->reinitialize();
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->prev->stop();
    ////////////////////////////////////////////////////////////////////////////

    return return_val;
}


//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Cue-Based Retrieval (epmem::cbr)
//
// Cue-based retrievals are searches in response to
// queries and/or neg-queries.
//
// All functions below implement John/Andy's range search
// algorithm (see Andy's thesis).  The primary insight
// is to only deal with changes.  In this case, change
// occurs at the end points of ranges of node occurrence.
//
// The implementations below share a common theme:
// 1) identify wmes in the cue
// 2) get pointers to ALL b-tree leaves
//    associated with sorted occurrence-endpoints
//    of these wmes (ranges, points, now)
// 3) step through the leaves according to the range
//    search algorithm
//
// In the Working Memory Tree, the occurrence of a leaf
// node is tantamount to the occurrence of the path to
// the leaf node (since there are no shared identifiers).
// However, when we add graph functionality, path is
// important.  Moreover, identifiers that "blink" have
// ambiguous identities over time.  Thus I introduced
// the Disjunctive Normal Form (DNF) graph.
//
// The primary insight of the DNF graph is that paths to
// leaf nodes can be written as the disjunction of the
// conjunction of paths.
//
// Metaphor: consider that a human child's lineage is
// in question (perhaps for purposes of estate).  We
// are unsure as to the child's grandfather.  The grand-
// father can be either gA or gB.  If it is gA, then the
// father is absolutely fA (otherwise fB).  So the child
// could exist as (where cX is child with lineage X):
//
// (gA ^ fA ^ cA) \/ (gB ^ fB ^ cB)
//
// Note that due to family... irregularities
// (i.e. men sleeping around), a parent might contribute
// to multiple family lines.  Thus gX could exist in
// multiple clauses.  However, due to well-enforced
// incest laws (i.e. we only support acyclic graph cues),
// an individual can only occur once within a lineage/clause.
//
// We have a "match" (i.e. identify the child's lineage)
// only if all literals are "on" in a path of
// lineage.  Thus, our task is to efficiently track DNF
// satisfaction while flipping on/off a single literal
// (which may exist in multiple clauses).
//
// The DNF graph implements this intuition efficiently by
// (say it with me) only processing changes!  First we
// construct the graph by creating "literals" (gA, fA, etc)
// and maintaining parent-child relationships (gA connects
// to fA which connects to cA).  Leaf literals have no
// children, but are associated with a "match."  Each match
// has a cardinality count (positive/negative 1 depending on
// query vs. neg-query) and a WMA value (weighting).
//
// We then connect the b-tree pointers from above with
// each of the literals.  It is possible that a query
// can serve multiple literals, so we gain from sharing.
//
// Nodes within the DNF Graph need only save a "count":
// zero means neither it nor its lineage to date is
// satisfied, one means either its lineage or it is
// satisfied, two means it and its lineage is satisfied.
//
// The range search algorithm is simply walking (in-order)
// the parallel b-tree pointers.  When we get to an endpoint,
// we appropriately turn on/off all directly associated
// literals.  Based upon the current state of the literals,
// we may need to propagate changes to children.
//
// If propogation reaches and changes a match, we alter the
// current episode's cardinality/score.  Thus we achieve
// the Soar mantra of only processing changes!
//
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
// Justin's Stuff
//////////////////////////////////////////////////////////

#define QUERY_DEBUG 0

void epmem_print_retrieval_state(epmem_wme_literal_map& literals, epmem_triple_pedge_map pedge_caches[], epmem_triple_uedge_map uedge_caches[])
{
    //std::map<epmem_node_id, std::string> tsh;
    std::cout << std::endl;
    std::cout << "digraph {" << std::endl;
    std::cout << "node [style=\"filled\"];" << std::endl;
    // LITERALS
    std::cout << "subgraph cluster_literals {" << std::endl;
    std::cout << "node [fillcolor=\"#0084D1\"];" << std::endl;
    for (epmem_wme_literal_map::iterator lit_iter = literals.begin(); lit_iter != literals.end(); lit_iter++)
    {
        epmem_literal* literal = (*lit_iter).second;
        if (literal->id_sym)
        {
            std::cout << "\"" << literal->value_sym << "\" [";
            if (literal->child_n_id == EPMEM_NODEID_BAD)
            {
                std::cout << "label=\"" << literal->value_sym << "\"";
            }
            else
            {
                std::cout << "label=\"" << literal->child_n_id << "\"";
            }
            if (!literal->value_is_id)
            {
                std::cout << ", shape=\"rect\"";
            }
            if (literal->matches.size() == 0)
            {
                std::cout << ", penwidth=\"2.0\"";
            }
            if (literal->is_neg_q)
            {
                std::cout << ", fillcolor=\"#C5000B\"";
            }
            std::cout << "];" << std::endl;
            std::cout << "\"" << literal->id_sym << "\" -> \"" << literal->value_sym << "\" [label=\"";
            if (literal->attribute_s_id == EPMEM_NODEID_BAD)
            {
                std::cout << "?";
            }
            else
            {
                std::cout << literal->attribute_s_id;
            }
            std::cout << "\\n" << literal << "\"];" << std::endl;
        }
    }
    std::cout << "};" << std::endl;
    // NODES / NODE->NODE
    std::cout << "subgraph cluster_uedges{" << std::endl;
    std::cout << "node [fillcolor=\"#FFD320\"];" << std::endl;
    for (int type = EPMEM_RIT_STATE_NODE; type <= EPMEM_RIT_STATE_EDGE; type++)
    {
        epmem_triple_uedge_map* uedge_cache = &uedge_caches[type];
        for (epmem_triple_uedge_map::iterator uedge_iter = uedge_cache->begin(); uedge_iter != uedge_cache->end(); uedge_iter++)
        {
            epmem_triple triple = (*uedge_iter).first;
            if (triple.child_n_id != EPMEM_NODEID_ROOT)
            {
                if (type == EPMEM_RIT_STATE_NODE)
                {
                    std::cout << "\"n" << triple.child_n_id << "\" [shape=\"rect\"];" << std::endl;
                }
                std::cout << "\"e" << triple.parent_n_id << "\" -> \"" << (type == EPMEM_RIT_STATE_NODE ? "n" : "e") << triple.child_n_id << "\" [label=\"" << triple.attribute_s_id << "\"];" << std::endl;
            }
        }
    }
    std::cout << "};" << std::endl;
    // PEDGES / LITERAL->PEDGE
    std::cout << "subgraph cluster_pedges {" << std::endl;
    std::cout << "node [fillcolor=\"#008000\"];" << std::endl;
    std::multimap<epmem_node_id, epmem_pedge*> parent_pedge_map;
    for (int type = EPMEM_RIT_STATE_NODE; type <= EPMEM_RIT_STATE_EDGE; type++)
    {
        for (epmem_triple_pedge_map::iterator pedge_iter = pedge_caches[type].begin(); pedge_iter != pedge_caches[type].end(); pedge_iter++)
        {
            epmem_triple triple = (*pedge_iter).first;
            epmem_pedge* pedge = (*pedge_iter).second;
            if (triple.attribute_s_id != EPMEM_NODEID_BAD)
            {
                std::cout << "\"" << pedge << "\" [label=\"" << pedge << "\\n(" << triple.parent_n_id << ", " << triple.attribute_s_id << ", ";
                if (triple.child_n_id == EPMEM_NODEID_BAD)
                {
                    std::cout << "?";
                }
                else
                {
                    std::cout << triple.child_n_id;
                }
                std::cout << ")\"";
                if (!pedge->value_is_id)
                {
                    std::cout << ", shape=\"rect\"";
                }
                std::cout << "];" << std::endl;
                for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                {
                    epmem_literal* literal = *lit_iter;
                    std::cout << "\"" << literal->value_sym << "\" -> \"" << pedge << "\";" << std::endl;
                }
                parent_pedge_map.insert(std::make_pair(triple.parent_n_id, pedge));
            }
        }
    }
    std::cout << "};" << std::endl;
    // PEDGE->PEDGE / PEDGE->NODE
    std::set<std::pair<epmem_pedge*, epmem_node_id> > drawn;
    for (int type = EPMEM_RIT_STATE_NODE; type <= EPMEM_RIT_STATE_EDGE; type++)
    {
        epmem_triple_uedge_map* uedge_cache = &uedge_caches[type];
        for (epmem_triple_uedge_map::iterator uedge_iter = uedge_cache->begin(); uedge_iter != uedge_cache->end(); uedge_iter++)
        {
            epmem_triple triple = (*uedge_iter).first;
            epmem_uedge* uedge = (*uedge_iter).second;
            if (triple.attribute_s_id != EPMEM_NODEID_BAD)
            {
                for (epmem_pedge_set::iterator pedge_iter = uedge->pedges.begin(); pedge_iter != uedge->pedges.end(); pedge_iter++)
                {
                    epmem_pedge* pedge = *pedge_iter;
                    std::pair<epmem_pedge*, epmem_node_id> pair = std::make_pair(pedge, triple.parent_n_id);
                    if (!drawn.count(pair))
                    {
                        drawn.insert(pair);
                        std::cout << "\"" << pedge << "\" -> \"e" << triple.parent_n_id << "\";" << std::endl;
                    }
                    std::cout << "\"" << pedge << "\" -> \"" << (pedge->value_is_id ? "e" : "n") << triple.child_n_id << "\" [style=\"dashed\"];" << std::endl;
                    std::pair<std::multimap<epmem_node_id, epmem_pedge*>::iterator, std::multimap<epmem_node_id, epmem_pedge*>::iterator> pedge_iters = parent_pedge_map.equal_range(triple.child_n_id);
                    for (std::multimap<epmem_node_id, epmem_pedge*>::iterator pedge_iter = pedge_iters.first; pedge_iter != pedge_iters.second; pedge_iter++)
                    {
                        std::cout << "\"" << pedge << "\" -> \"" << (*pedge_iter).second << "\";" << std::endl;
                    }
                }
            }
        }
    }
    std::cout << "}" << std::endl;
}

bool epmem_gm_mcv_comparator(const epmem_literal* a, const epmem_literal* b)
{
    return (a->matches.size() < b->matches.size());
}

epmem_literal* epmem_build_dnf(wme* cue_wme, epmem_wme_literal_map& literal_cache, epmem_literal_set& leaf_literals, epmem_symbol_int_map& symbol_num_incoming, epmem_literal_deque& gm_ordering, int query_type, std::set<Symbol*>& visiting, wme_set& cue_wmes, agent* thisAgent, Symbol* cue_root = NULL)
{
    // if the value is being visited, this is part of a loop; return NULL
    // remove this check (and in fact, the entire visiting parameter) if cyclic cues are allowed
    if (visiting.count(cue_wme->value))
    {
        return NULL;
    }
    // if the value is an identifier and we've been here before, we can return the previous literal
    if (literal_cache.count(cue_wme))
    {
        return literal_cache[cue_wme];
    }

    cue_wmes.insert(cue_wme);
    Symbol* value = cue_wme->value;
    epmem_literal* literal;
    thisAgent->memoryManager->allocate_with_pool(MP_epmem_literal, &literal);
    literal->cue = cue_root;
    new(&(literal->parents)) epmem_literal_set();
    new(&(literal->children)) epmem_literal_set();

    if (value->symbol_type != IDENTIFIER_SYMBOL_TYPE)   // WME is a value
    {
        literal->value_is_id = EPMEM_RIT_STATE_NODE;
        literal->is_leaf = true;
        literal->child_n_id = epmem_temporal_hash(thisAgent, value);
        leaf_literals.insert(literal);
    }
    //else if (value->id->is_lti() && value->id->LTI_ID)
    //{
        // This is an attempt to reintegrate matching of ltis into epmem queries despite the
        // change to make ltis into instances. The idea is that ltis require both a structure match
        // like normal identifiers, and also a direct match of the lti in question.
        // Treating ltis purely like normal identifiers (lacking this "else", essentially)
        // is done with an additional command when the query is issued to epmem.
        // TODO: Actually implement that extra (agent-initiated epmem-link) command.
        /*
         * scijones - May 2 2017 My first try at implementing this is just to copy the old code
         * that was here in the first place before we changed ltis to be instance-based.
         */
        // The first step is to find the LTI at all in the first place. If it's not present,
        // we can just return failure.

    //}
    else     // WME is a normal identifier
    {
        // we determine whether it is a leaf by checking for children
        epmem_wme_list* children = epmem_get_augs_of_id(value, get_new_tc_number(thisAgent));
        literal->value_is_id = EPMEM_RIT_STATE_EDGE;
        literal->child_n_id = EPMEM_NODEID_BAD;

        // if the WME has no children, then it's a leaf
        // otherwise, we recurse for all children
        if (children->empty())
        {
            literal->is_leaf = true;
            leaf_literals.insert(literal);
            delete children;
        }
        else
        {
            bool cycle = false;
            visiting.insert(cue_wme->value);
            for (epmem_wme_list::iterator wme_iter = children->begin(); wme_iter != children->end(); wme_iter++)
            {
                // check to see if this child forms a cycle
                // if it does, we skip over it
                epmem_literal* child = epmem_build_dnf(*wme_iter, literal_cache, leaf_literals, symbol_num_incoming, gm_ordering, query_type, visiting, cue_wmes, thisAgent, cue_root);
                if (child)
                {
                    child->parents.insert(literal);
                    literal->children.insert(child);
                }
                else
                {
                    cycle = true;
                }
            }
            delete children;
            visiting.erase(cue_wme->value);
            // if all children of this WME lead to cycles, then we don't need to walk this path
            // in essence, this forces the DNF graph to be acyclic
            // this results in savings in not walking edges and intervals
            if (cycle && literal->children.empty())
            {
                literal->parents.~epmem_literal_set();
                literal->children.~epmem_literal_set();
                thisAgent->memoryManager->free_with_pool(MP_epmem_literal, literal);
                return NULL;
            }
            literal->is_leaf = false;
            epmem_symbol_int_map::iterator rem_iter = symbol_num_incoming.find(value);
            if (rem_iter == symbol_num_incoming.end())
            {
                symbol_num_incoming[value] = 1;
            }
            else
            {
                (*rem_iter).second++;
            }
        }
    }

    if (query_type == EPMEM_NODE_POS)
    {
        gm_ordering.push_front(literal);
        thisAgent->EpMem->epmem_stats->qry_pos->set_value(thisAgent->EpMem->epmem_stats->qry_pos->get_value() + 1);
    }
    else
    {
        thisAgent->EpMem->epmem_stats->qry_neg->set_value(thisAgent->EpMem->epmem_stats->qry_neg->get_value() + 1);
    }

    literal->id_sym = cue_wme->id;
    literal->value_sym = cue_wme->value;
    literal->attribute_s_id = epmem_temporal_hash(thisAgent, cue_wme->attr);
    literal->is_neg_q = query_type;
    literal->weight = (literal->is_neg_q ? -1 : 1) * (thisAgent->EpMem->epmem_params->balance->get_value() >= 1.0 - 1.0e-8 ? 1.0 : wma_get_wme_activation(thisAgent, cue_wme, true));
#ifdef USE_MEM_POOL_ALLOCATORS
    new(&(literal->matches)) epmem_node_pair_set(std::less<epmem_node_pair>(), soar_module::soar_memory_pool_allocator<epmem_node_pair>());
#else
    new(&(literal->matches)) epmem_node_pair_set();
#endif
    new(&(literal->values)) epmem_node_int_map();

    literal_cache[cue_wme] = literal;
    return literal;
}

bool epmem_register_pedges(epmem_node_id parent, epmem_literal* literal, epmem_pedge_pq& pedge_pq, epmem_time_id after, epmem_triple_pedge_map pedge_caches[], epmem_triple_uedge_map uedge_caches[], agent* thisAgent)
{
    // we don't need to keep track of visited literals/nodes because the literals are guaranteed to be acyclic
    // that is, the expansion to the literal's children will eventually bottom out
    // select the query
    epmem_triple triple = {parent, literal->attribute_s_id, literal->child_n_id};
    int is_edge = literal->value_is_id;
    if (QUERY_DEBUG >= 1)
    {
        std::cout << "		RECURSING ON " << parent << " " << literal << std::endl;
    }
    // if the unique edge does not exist, create a new unique edge query
    // otherwse, if the pedge has not been registered with this literal
    epmem_triple_pedge_map* pedge_cache = &(pedge_caches[is_edge]);
    epmem_triple_pedge_map::iterator pedge_iter = pedge_cache->find(triple);
    epmem_pedge* child_pedge = NULL;
    if (pedge_iter == pedge_cache->end() || (*pedge_iter).second == NULL)
    {
        int has_value = (literal->child_n_id != EPMEM_NODEID_BAD ? 1 : 0);
        /*
         * {
                "SELECT wc_id, value_s_id, ? FROM epmem_wmes_constant WHERE parent_n_id=? AND attribute_s_id=?",
                "SELECT wc_id, value_s_id, ? FROM epmem_wmes_constant WHERE parent_n_id=? AND attribute_s_id=? AND value_s_id=?"
            },
            {
                "SELECT wi_id, child_n_id, last_episode_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=? AND ?<last_episode_id ORDER BY last_episode_id DESC",
                "SELECT wi_id, child_n_id, last_episode_id FROM epmem_wmes_identifier WHERE parent_n_id=? AND attribute_s_id=? AND child_n_id=? AND ?<last_episode_id"
            }
         */
        soar_module::pooled_sqlite_statement* pedge_sql = thisAgent->EpMem->epmem_stmts_graph->pool_find_edge_queries[is_edge][has_value]->request(thisAgent->EpMem->epmem_timers->query_sql_edge);
        //don't confuse with pool_find_interval_queries. anyways, this reports the wi_id or the wc_id based on whether the pedge refers to a constant (is_edge false) and whether it's a leaf w.r.t. the query (has_value=false)
        //is_edge means in the WM graph sense -- is identifier.
        int bind_pos = 1;
        if (!is_edge)
        {
            pedge_sql->bind_int(bind_pos++, LLONG_MAX);//for some reason, we select llongmax as the third column when these are constants, so that the last_episode_id becomes that.
        }
        pedge_sql->bind_int(bind_pos++, triple.parent_n_id);
        pedge_sql->bind_int(bind_pos++, triple.attribute_s_id);
        if (has_value)
        {
            pedge_sql->bind_int(bind_pos++, triple.child_n_id);
        }
        if (is_edge)
        {
            pedge_sql->bind_int(bind_pos++, after);//want the instance such that the last_episode_id is greater than the after.
        }
        if (pedge_sql->execute() == soar_module::row)
        {
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_pedge, &child_pedge);
            child_pedge->triple = triple;
            child_pedge->value_is_id = literal->value_is_id;
            child_pedge->sql = pedge_sql;
            new(&(child_pedge->literals)) epmem_literal_set();
            child_pedge->literals.insert(literal);
            child_pedge->time = child_pedge->sql->column_int(2);//the last_episode_id, ordered descending, so they'll go backwards in time as this is called.
            pedge_pq.push(child_pedge);
            (*pedge_cache)[triple] = child_pedge;
            return true;
        }
        else
        {
            pedge_sql->get_pool()->release(pedge_sql);
            return false;
        }
    }
    else
    {//we've run into this pedge before and it has a child, so we don't have to do the creation process for it, but maybe we will for a child.
        child_pedge = (*pedge_iter).second;
        if (!child_pedge->literals.count(literal))
        {
            child_pedge->literals.insert(literal);
            // if the literal is an identifier and not a leaf
            if (!literal->is_leaf && literal->child_n_id == EPMEM_NODEID_BAD)
            {
                bool created = false;
                epmem_triple_uedge_map* uedge_cache = &uedge_caches[is_edge];
                for (epmem_triple_uedge_map::iterator uedge_iter = uedge_cache->lower_bound(triple); uedge_iter != uedge_cache->end(); uedge_iter++)
                {
                    epmem_triple child_triple = (*uedge_iter).first;
                    // make sure we're still looking at the right edge(s)
                    if (child_triple.parent_n_id != triple.parent_n_id || child_triple.attribute_s_id != triple.attribute_s_id)
                    {
                        break;//seems ugly. is there such a thing as "upper bound"? weird.
                    }
                    epmem_uedge* child_uedge = (*uedge_iter).second;
                    if (child_triple.child_n_id != EPMEM_NODEID_BAD && child_uedge->value_is_id)
                    {
                        for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                        {
                            created |= epmem_register_pedges(child_triple.child_n_id, *child_iter, pedge_pq, after, pedge_caches, uedge_caches, thisAgent);
                        }
                    }
                }
                return created;
            }
        }
    }
    return true;
}

bool epmem_satisfy_literal(epmem_literal* literal, epmem_node_id parent, epmem_node_id child, double& current_score, long int& current_cardinality, epmem_symbol_node_pair_int_map& symbol_node_count, epmem_triple_uedge_map uedge_caches[], epmem_symbol_int_map& symbol_num_incoming)
{
    epmem_symbol_node_pair_int_map::iterator match_iter;
    if (QUERY_DEBUG >= 1)
    {
        std::cout << "		RECURSING ON " << parent << " " << child << " " << literal << std::endl;
    }
    // check if the ancestors of this literal are satisfied
    bool parents_satisfied = (literal->id_sym == NULL);
    if (!parents_satisfied)
    {
        // ancestors are satisfied if:
        // 1. all incoming literals are satisfied
        // 2. all incoming literals have this particular node satisfying it
        int num_incoming = symbol_num_incoming[literal->id_sym];
        match_iter = symbol_node_count.find(std::make_pair(literal->id_sym, parent));
        // since, by definition, if a node satisfies all incoming literals, all incoming literals are satisfied
        parents_satisfied = (match_iter != symbol_node_count.end()) && ((*match_iter).second == num_incoming);
    }
    // if yes
    if (parents_satisfied)
    {
        // add the edge as a match
        literal->matches.insert(std::make_pair(parent, child));
        epmem_node_int_map::iterator values_iter = literal->values.find(child);
        if (values_iter == literal->values.end())
        {
            literal->values[child] = 1;
            if (QUERY_DEBUG >= 1)
            {
                std::cout << "		LITERAL SATISFIED: " << literal << std::endl;
            }
            if (literal->is_leaf)
            {
                if (literal->matches.size() == 1)
                {
                    current_score += literal->weight;
                    current_cardinality += (literal->is_neg_q ? -1 : 1);
                    if (QUERY_DEBUG >= 1)
                    {
                        std::cout << "			NEW SCORE: " << current_score << ", " << current_cardinality << std::endl;
                    }
                    return true;
                }
            }
            else
            {
                bool changed_score = false;
                // change bookkeeping information about ancestry
                epmem_symbol_node_pair match = std::make_pair(literal->value_sym, child);
                match_iter = symbol_node_count.find(match);
                if (match_iter == symbol_node_count.end())
                {
                    symbol_node_count[match] = 1;
                }
                else
                {
                    symbol_node_count[match]++;
                }
                // recurse over child literals
                for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                {
                    epmem_literal* child_lit = *child_iter;
                    epmem_triple_uedge_map* uedge_cache = &uedge_caches[child_lit->value_is_id];
                    epmem_triple child_triple = {child, child_lit->attribute_s_id, child_lit->child_n_id};
                    epmem_triple_uedge_map::iterator uedge_iter;
                    epmem_uedge* child_uedge = NULL;
                    if (child_lit->child_n_id == EPMEM_NODEID_BAD)
                    {
                        uedge_iter = uedge_cache->lower_bound(child_triple);
                        while (uedge_iter != uedge_cache->end())
                        {
                            child_triple = (*uedge_iter).first;
                            child_uedge = (*uedge_iter).second;
                            if (child_triple.parent_n_id != child || child_triple.attribute_s_id != child_lit->attribute_s_id)
                            {
                                break;
                            }
                            if (child_uedge->activated && child_uedge->activation_count == 1)
                            {
                                changed_score |= epmem_satisfy_literal(child_lit, child_triple.parent_n_id, child_triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                            }
                            uedge_iter++;
                        }
                    }
                    else
                    {
                        uedge_iter = uedge_cache->find(child_triple);
                        if (uedge_iter != uedge_cache->end() && (*uedge_iter).second != NULL)
                        {
                            child_uedge = (*uedge_iter).second;
                            if (child_uedge->activated && child_uedge->activation_count == 1)
                            {
                                changed_score |= epmem_satisfy_literal(child_lit, child_triple.parent_n_id, child_triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                            }
                        }
                    }
                }
                return changed_score;
            }
        }
        else
        {
            (*values_iter).second++;
        }
    }
    return false;
}

bool epmem_unsatisfy_literal(epmem_literal* literal, epmem_node_id parent, epmem_node_id child, double& current_score, long int& current_cardinality, epmem_symbol_node_pair_int_map& symbol_node_count)
{
    epmem_symbol_int_map::iterator count_iter;
    if (literal->matches.size() == 0)
    {
        return false;
    }
    if (QUERY_DEBUG >= 1)
    {
        std::cout << "		RECURSING ON " << parent << " " << child << " " << literal << std::endl;
    }
    // we only need things if this parent-child pair is matching the literal
    epmem_node_pair_set::iterator lit_match_iter = literal->matches.find(std::make_pair(parent, child));
    if (lit_match_iter != literal->matches.end())
    {
        // erase the edge from this literal's matches
        literal->matches.erase(lit_match_iter);
        epmem_node_int_map::iterator values_iter = literal->values.find(child);
        (*values_iter).second--;
        if ((*values_iter).second == 0)
        {
            literal->values.erase(values_iter);
            if (QUERY_DEBUG >= 1)
            {
                std::cout << "		LITERAL UNSATISFIED: " << literal << std::endl;
            }
            if (literal->is_leaf)
            {
                if (literal->matches.size() == 0)
                {
                    current_score -= literal->weight;
                    current_cardinality -= (literal->is_neg_q ? -1 : 1);
                    if (QUERY_DEBUG >= 1)
                    {
                        std::cout << "			NEW SCORE: " << current_score << ", " << current_cardinality << std::endl;
                    }
                    return true;
                }
            }
            else
            {
                bool changed_score = false;
                epmem_symbol_node_pair match = std::make_pair(literal->value_sym, child);
                epmem_symbol_node_pair_int_map::iterator match_iter = symbol_node_count.find(match);
                (*match_iter).second--;
                if ((*match_iter).second == 0)
                {
                    symbol_node_count.erase(match_iter);
                }
                // if this literal is no longer satisfied, recurse on all children
                // if this literal is still satisfied, recurse on children who is matching on descendants of this edge
                if (literal->matches.size() == 0)
                {
                    for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                    {
                        epmem_literal* child_lit = *child_iter;
                        epmem_node_pair_set::iterator node_iter = child_lit->matches.begin();
                        while (node_iter != child_lit->matches.end())
                        {
                            changed_score |= epmem_unsatisfy_literal(child_lit, (*node_iter).first, (*node_iter).second, current_score, current_cardinality, symbol_node_count);
                            if (child_lit->matches.empty())
                            {
                                break;
                            }
                            else
                            {
                                node_iter++;
                            }
                        }
                    }
                }
                else
                {
                    epmem_node_pair node_pair = std::make_pair(child, EPMEM_NODEID_BAD);
                    for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                    {
                        epmem_literal* child_lit = *child_iter;
                        epmem_node_pair_set::iterator node_iter = child_lit->matches.lower_bound(node_pair);
                        if (node_iter != child_lit->matches.end() && (*node_iter).first == child)
                        {
                            changed_score |= epmem_unsatisfy_literal(child_lit, (*node_iter).first, (*node_iter).second, current_score, current_cardinality, symbol_node_count);
                        }
                    }
                }
                return changed_score;
            }
        }
    }
    return false;
}

bool epmem_graph_match(epmem_literal_deque::iterator& dnf_iter, epmem_literal_deque::iterator& iter_end, epmem_literal_node_pair_map& bindings, epmem_node_symbol_map bound_nodes[], agent* thisAgent, int depth = 0)
{
    if (dnf_iter == iter_end)
    {
        return true;
    }
    epmem_literal* literal = *dnf_iter;
    if (bindings.count(literal))
    {
        return false;
    }
    epmem_literal_deque::iterator next_iter = dnf_iter;
    next_iter++;
#ifdef USE_MEM_POOL_ALLOCATORS
    epmem_node_set failed_parents = epmem_node_set(std::less<epmem_node_id>(), soar_module::soar_memory_pool_allocator<epmem_node_id>());
    epmem_node_set failed_children = epmem_node_set(std::less<epmem_node_id>(), soar_module::soar_memory_pool_allocator<epmem_node_id>());
#else
    epmem_node_set failed_parents;
    epmem_node_set failed_children;
#endif
    // go through the list of matches, binding each one to this literal in turn
    for (epmem_node_pair_set::iterator match_iter = literal->matches.begin(); match_iter != literal->matches.end(); match_iter++)
    {
        epmem_node_id parent_n_id = (*match_iter).first;
        epmem_node_id child_n_id = (*match_iter).second;
        if (failed_parents.count(parent_n_id))
        {
            continue;
        }
        if (QUERY_DEBUG >= 2)
        {
            for (int i = 0; i < depth; i++)
            {
                std::cout << "\t";
            }
            std::cout << "TRYING " << literal << " " << parent_n_id << std::endl;
        }
        bool relations_okay = true;
        // for all parents
        for (epmem_literal_set::iterator parent_iter = literal->parents.begin(); relations_okay && parent_iter != literal->parents.end(); parent_iter++)
        {
            epmem_literal* parent = *parent_iter;
            epmem_literal_node_pair_map::iterator bind_iter = bindings.find(parent);
            if (bind_iter != bindings.end() && (*bind_iter).second.second != parent_n_id)
            {
                relations_okay = false;
            }
        }
        if (!relations_okay)
        {
            if (QUERY_DEBUG >= 2)
            {
                for (int i = 0; i < depth; i++)
                {
                    std::cout << "\t";
                }
                std::cout << "PARENT CONSTRAINT FAIL" << std::endl;
            }
            failed_parents.insert(parent_n_id);
            continue;
        }
        // if the node has already been bound, make sure it's bound to the same thing
        epmem_node_symbol_map::iterator binder = bound_nodes[literal->value_is_id].find(child_n_id);
        if (binder != bound_nodes[literal->value_is_id].end() && (*binder).second != literal->value_sym)
        {
            failed_children.insert(child_n_id);
            continue;
        }
        if (QUERY_DEBUG >= 2)
        {
            for (int i = 0; i < depth; i++)
            {
                std::cout << "\t";
            }
            std::cout << "TRYING " << literal << " " << parent_n_id << " " << child_n_id << std::endl;
        }
        if (literal->child_n_id != EPMEM_NODEID_BAD && literal->child_n_id != child_n_id)
        {
            relations_okay = false;
        }
        // for all children
        for (epmem_literal_set::iterator child_iter = literal->children.begin(); relations_okay && child_iter != literal->children.end(); child_iter++)
        {
            epmem_literal* child = *child_iter;
            epmem_literal_node_pair_map::iterator bind_iter = bindings.find(child);
            if (bind_iter != bindings.end() && (*bind_iter).second.first != child_n_id)
            {
                relations_okay = false;
            }
        }
        if (!relations_okay)
        {
            if (QUERY_DEBUG >= 2)
            {
                for (int i = 0; i < depth; i++)
                {
                    std::cout << "\t";
                }
                std::cout << "CHILD CONSTRAINT FAIL" << std::endl;
            }
            failed_children.insert(child_n_id);
            continue;
        }
        if (QUERY_DEBUG >= 2)
        {
            for (int i = 0; i < depth; i++)
            {
                std::cout << "\t";
            }
            std::cout << literal << " " << parent_n_id << " " << child_n_id << std::endl;
        }
        // temporarily modify the bindings and bound nodes
        bindings[literal] = std::make_pair(parent_n_id, child_n_id);
        bound_nodes[literal->value_is_id][child_n_id] = literal->value_sym;
        // recurse on the rest of the list
        bool list_satisfied = epmem_graph_match(next_iter, iter_end, bindings, bound_nodes, thisAgent, depth + 1);
        // if the rest of the list matched, we've succeeded
        // otherwise, undo the temporarily modifications and try again
        if (list_satisfied)
        {
            return true;
        }
        else
        {
            bindings.erase(literal);
            bound_nodes[literal->value_is_id].erase(child_n_id);
        }
    }
    // this means we've tried everything and this whole exercise was a waste of time
    // EPIC FAIL
    if (QUERY_DEBUG >= 2)
    {
        for (int i = 0; i < depth; i++)
        {
            std::cout << "\t";
        }
        std::cout << "EPIC FAIL" << std::endl;
    }
    return false;
}

void epmem_process_query(agent* thisAgent, Symbol* state, Symbol* pos_query, Symbol* neg_query, epmem_time_list& prohibits, epmem_time_id before, epmem_time_id after, wme_set& cue_wmes, symbol_triple_list& meta_wmes, symbol_triple_list& retrieval_wmes, int level = 3)
{//the form of interval_query is a list of changes in order, expressed as partially-ordered allen's temporal interval relations.
    //has to be a graph match, doing a match to the interval query basically in backwards order.
    // a query must contain a positive cue
    if (pos_query == NULL)
    {
        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_status, thisAgent->symbolManager->soarSymbols.epmem_sym_bad_cmd);
        return;
    }

    // before and after, if specified, must be valid relative to each other
    if (before != EPMEM_MEMID_NONE && after != EPMEM_MEMID_NONE && before <= after)
    {
        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_status, thisAgent->symbolManager->soarSymbols.epmem_sym_bad_cmd);
        return;
    }

    if (QUERY_DEBUG >= 1)
    {
        std::cout << std::endl << "==========================" << std::endl << std::endl;
    }

    thisAgent->EpMem->epmem_timers->query->start();

    // sort probibit's
    if (!prohibits.empty())
    {
        std::sort(prohibits.begin(), prohibits.end());
    }

    // epmem options
    bool do_graph_match = (thisAgent->EpMem->epmem_params->graph_match->get_value() == on);
    epmem_param_container::gm_ordering_choices gm_order = thisAgent->EpMem->epmem_params->gm_ordering->get_value();

    // variables needed for cleanup
    epmem_wme_literal_map literal_cache;
    epmem_triple_pedge_map pedge_caches[2];
#ifdef USE_MEM_POOL_ALLOCATORS
    epmem_triple_uedge_map uedge_caches[2] =
    {
        epmem_triple_uedge_map(std::less<epmem_triple>(), soar_module::soar_memory_pool_allocator<std::pair<const epmem_triple, epmem_uedge*> >()),
        epmem_triple_uedge_map(std::less<epmem_triple>(), soar_module::soar_memory_pool_allocator<std::pair<const epmem_triple, epmem_uedge*> >())
    };
    epmem_interval_set interval_cleanup = epmem_interval_set(std::less<epmem_interval*>(), soar_module::soar_memory_pool_allocator<epmem_interval*>());
#else
    epmem_triple_uedge_map uedge_caches[2] = {epmem_triple_uedge_map(), epmem_triple_uedge_map()};
    epmem_interval_set interval_cleanup = epmem_interval_set();
#endif

    // TODO JUSTIN additional indices

    // variables needed for building the DNF
    epmem_literal* root_literal;
    thisAgent->memoryManager->allocate_with_pool(MP_epmem_literal, &root_literal);
    epmem_literal_set leaf_literals;

    // priority queues for interval walk
    epmem_pedge_pq pedge_pq;
    epmem_interval_pq interval_pq;

    // variables needed to track satisfiability
    epmem_symbol_int_map symbol_num_incoming;                 // number of literals with a certain symbol as its value
    epmem_symbol_node_pair_int_map symbol_node_count;         // number of times a symbol is matched by a node

    // various things about the current and the best episodes
    epmem_time_id best_episode = EPMEM_MEMID_NONE;
    double best_score = 0;
    bool best_graph_matched = false;
    long int best_cardinality = 0;
    epmem_literal_node_pair_map best_bindings;
    double current_score = 0;
    long int current_cardinality = 0;

    // variables needed for graphmatch
    epmem_literal_deque gm_ordering;

    if (level > 1)
    {
        // build the DNF graph while checking for leaf WMEs
        {
            thisAgent->EpMem->epmem_stats->qry_pos->set_value(0);
            thisAgent->EpMem->epmem_stats->qry_neg->set_value(0);
            thisAgent->EpMem->epmem_timers->query_dnf->start();
            root_literal->id_sym = NULL;
            root_literal->value_sym = pos_query;
            root_literal->is_neg_q = EPMEM_NODE_POS;
            root_literal->value_is_id = EPMEM_RIT_STATE_EDGE;
            root_literal->is_leaf = false;
            root_literal->attribute_s_id = EPMEM_NODEID_BAD;
            root_literal->child_n_id = EPMEM_NODEID_ROOT;
            root_literal->weight = 0.0;
            new(&(root_literal->parents)) epmem_literal_set();
            new(&(root_literal->children)) epmem_literal_set();
#ifdef USE_MEM_POOL_ALLOCATORS
            new(&(root_literal->matches)) epmem_node_pair_set(std::less<epmem_node_pair>(), soar_module::soar_memory_pool_allocator<epmem_node_pair>(thisAgent));
#else
            new(&(root_literal->matches)) epmem_node_pair_set();
#endif
            new(&(root_literal->values)) epmem_node_int_map();
            symbol_num_incoming[pos_query] = 1;
            literal_cache[NULL] = root_literal;

            std::set<Symbol*> visiting;
            visiting.insert(pos_query);
            visiting.insert(neg_query);
            for (int query_type = EPMEM_NODE_POS; query_type <= EPMEM_NODE_NEG; query_type++)
            {
                Symbol* query_root = NULL;
                switch (query_type)
                {
                    case EPMEM_NODE_POS:
                        query_root = pos_query;
                        break;
                    case EPMEM_NODE_NEG:
                        query_root = neg_query;
                        break;
                }
                if (!query_root)
                {
                    continue;
                }
                epmem_wme_list* children = epmem_get_augs_of_id(query_root, get_new_tc_number(thisAgent));
                // for each first level WME, build up a DNF
                for (epmem_wme_list::iterator wme_iter = children->begin(); wme_iter != children->end(); wme_iter++)
                {
                    epmem_literal* child = epmem_build_dnf(*wme_iter, literal_cache, leaf_literals, symbol_num_incoming, gm_ordering, query_type, visiting, cue_wmes, thisAgent);
                    if (child)
                    {
                        // force all first level literals to have the same id symbol
                        child->id_sym = pos_query;
                        child->parents.insert(root_literal);
                        root_literal->children.insert(child);
                    }
                }
                delete children;
            }
            thisAgent->EpMem->epmem_timers->query_dnf->stop();
            thisAgent->EpMem->epmem_stats->qry_lits->set_value(thisAgent->EpMem->epmem_stats->qry_pos->get_value() + thisAgent->EpMem->epmem_stats->qry_neg->get_value());
        }

        // calculate the highest possible score and cardinality score
        double perfect_score = 0;
        int perfect_cardinality = 0;
        for (epmem_literal_set::iterator iter = leaf_literals.begin(); iter != leaf_literals.end(); iter++)
        {
            if (!(*iter)->is_neg_q)
            {
                perfect_score += (*iter)->weight;
                perfect_cardinality++;
            }
        }

        // set default values for before and after
        if (before == EPMEM_MEMID_NONE)
        {
            before = thisAgent->EpMem->epmem_stats->time->get_value() - 1;
        }
        else
        {
            before = before - 1; // since before's are strict
        }
        if (after == EPMEM_MEMID_NONE)
        {
            after = EPMEM_MEMID_NONE;
        }
        epmem_time_id current_episode = before;
        epmem_time_id next_episode;

        // create dummy edges and intervals
        {
            // insert dummy unique edge and interval end point queries for DNF root
            // we make an SQL statement just so we don't have to do anything special at cleanup
            epmem_triple triple = {EPMEM_NODEID_BAD, EPMEM_NODEID_BAD, EPMEM_NODEID_ROOT};
            epmem_pedge* root_pedge;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_pedge, &root_pedge);
            root_pedge->triple = triple;
            root_pedge->value_is_id = EPMEM_RIT_STATE_EDGE;
            new(&(root_pedge->literals)) epmem_literal_set();
            root_pedge->literals.insert(root_literal);
            root_pedge->sql = thisAgent->EpMem->epmem_stmts_graph->pool_dummy->request();
            root_pedge->sql->prepare();
            root_pedge->sql->bind_int(1, LLONG_MAX);
            root_pedge->sql->execute(soar_module::op_reinit);
            root_pedge->time = LLONG_MAX;
            pedge_pq.push(root_pedge);
            pedge_caches[EPMEM_RIT_STATE_EDGE][triple] = root_pedge;

            epmem_uedge* root_uedge;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_uedge, &root_uedge);
            root_uedge->triple = triple;
            root_uedge->value_is_id = EPMEM_RIT_STATE_EDGE;
            root_uedge->activation_count = 0;
            new(&(root_uedge->pedges)) epmem_pedge_set();
            root_uedge->intervals = 1;
            root_uedge->activated = false;
            uedge_caches[EPMEM_RIT_STATE_EDGE][triple] = root_uedge;

            epmem_interval* root_interval;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_interval, &root_interval);
            root_interval->uedge = root_uedge;
            root_interval->is_end_point = true;
            root_interval->sql = thisAgent->EpMem->epmem_stmts_graph->pool_dummy->request();
            root_interval->sql->prepare();
            root_interval->sql->bind_int(1, before);
            root_interval->sql->execute(soar_module::op_reinit);
            root_interval->time = before;
            interval_pq.push(root_interval);
            interval_cleanup.insert(root_interval);
        }

        if (QUERY_DEBUG >= 1)
        {
            epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
        }

        // main loop of interval walk
        thisAgent->EpMem->epmem_timers->query_walk->start();
        while (pedge_pq.size() && current_episode > after)
        {
            epmem_time_id next_edge;
            epmem_time_id next_interval;

            bool changed_score = false;

            thisAgent->EpMem->epmem_timers->query_walk_edge->start();
            next_edge = pedge_pq.top()->time;

            // process all edges which were last used at this time point
            while (pedge_pq.size() && (pedge_pq.top()->time == next_edge || pedge_pq.top()->time >= current_episode))
            {
                epmem_pedge* pedge = pedge_pq.top();
                pedge_pq.pop();
                epmem_triple triple = pedge->triple;
                triple.child_n_id = pedge->sql->column_int(1);

                if (QUERY_DEBUG >= 1)
                {
                    std::cout << "	EDGE " << triple.parent_n_id << "-" << triple.attribute_s_id << "-" << triple.child_n_id << std::endl;
                }

                // create queries for the unique edge children of this partial edge
                if (pedge->value_is_id)
                {
                    bool created = false;
                    for (epmem_literal_set::iterator literal_iter = pedge->literals.begin(); literal_iter != pedge->literals.end(); literal_iter++)
                    {
                        epmem_literal* literal = *literal_iter;
                        for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                        {
                            created |= epmem_register_pedges(triple.child_n_id, *child_iter, pedge_pq, after, pedge_caches, uedge_caches, thisAgent);
                        }
                    }
                }
                // TODO JUSTIN what I want to do here is, if there is no children which leads to a leaf, retract everything
                // I'm not sure how to properly test for this though

                // look for uedge with triple; if none exist, create one
                // otherwise, link up the uedge with the pedge and consider score changes
                epmem_triple_uedge_map* uedge_cache = &uedge_caches[pedge->value_is_id];
                epmem_triple_uedge_map::iterator uedge_iter = uedge_cache->find(triple);
                if (uedge_iter == uedge_cache->end())
                {
                    // create a uedge for this
                    epmem_uedge* uedge;
                    thisAgent->memoryManager->allocate_with_pool(MP_epmem_uedge, &uedge);
                    uedge->triple = triple;
                    uedge->value_is_id = pedge->value_is_id;
                    uedge->activation_count = 0;
                    new(&(uedge->pedges)) epmem_pedge_set();
                    uedge->intervals = 0;
                    uedge->activated = false;
                    // create interval queries for this partial edge
                    bool created = false;
                    int64_t edge_id = pedge->sql->column_int(0);
                    for (int interval_type = EPMEM_RANGE_EP; interval_type <= EPMEM_RANGE_POINT; interval_type++)
                    {
                        for (int point_type = EPMEM_RANGE_START; point_type <= EPMEM_RANGE_END; point_type++)
                        {
                            // pick a timer (any timer)
                            soar_module::timer* sql_timer = NULL;
                            switch (interval_type)
                            {
                                case EPMEM_RANGE_EP:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_ep;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_ep;
                                    }
                                    break;
                                case EPMEM_RANGE_NOW:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_now;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_now;
                                    }
                                    break;
                                case EPMEM_RANGE_POINT:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_point;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_point;
                                    }
                                    break;
                            }
                            // create the SQL query and bind it
                            // try to find an existing query first; if none exist, allocate a new one from the memory pools
                            soar_module::pooled_sqlite_statement* interval_sql = NULL;
                            interval_sql = thisAgent->EpMem->epmem_stmts_graph->pool_find_interval_queries[pedge->value_is_id][point_type][interval_type]->request(sql_timer);
                            int bind_pos = 1;
                            if (point_type == EPMEM_RANGE_END && interval_type == EPMEM_RANGE_NOW)
                            {
                                interval_sql->bind_int(bind_pos++, current_episode);
                            }
                            interval_sql->bind_int(bind_pos++, edge_id);
                            interval_sql->bind_int(bind_pos++, current_episode);
                            if (interval_sql->execute() == soar_module::row)
                            {
                                epmem_interval* interval;
                                thisAgent->memoryManager->allocate_with_pool(MP_epmem_interval, &interval);
                                interval->is_end_point = point_type;
                                interval->uedge = uedge;
                                // If it's an start point of a range (ie. not a point) and it's before the promo time
                                // (this is possible if a the promotion is in the middle of a range)
                                // trim it to the promo time.
                                // This will only happen if the LTI is promoted in the last interval it appeared in
                                // (since otherwise the start point would not be before its promotion).
                                // We don't care about the remaining results of the query
                                interval->time = interval_sql->column_int(0);
                                interval->sql = interval_sql;
                                interval_pq.push(interval);
                                interval_cleanup.insert(interval);
                                uedge->intervals++;
                                created = true;
                            }
                            else
                            {
                                interval_sql->get_pool()->release(interval_sql);
                            }
                        }
                    }
                    if (created)
                    {
                        uedge->pedges.insert(pedge);
                        uedge_cache->insert(std::make_pair(triple, uedge));
                    }
                    else
                    {
                        uedge->pedges.~epmem_pedge_set();
                        thisAgent->memoryManager->free_with_pool(MP_epmem_uedge, uedge);
                    }
                }
                else
                {
                    epmem_uedge* uedge = (*uedge_iter).second;
                    uedge->pedges.insert(pedge);
                    if (uedge->activated && uedge->activation_count == 1)
                    {
                        for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                        {
                            epmem_literal* literal = (*lit_iter);
                            changed_score |= epmem_satisfy_literal(literal, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                        }
                    }
                }

                // put the partial edge query back into the queue if there's more
                // otherwise, reinitialize the query and put it in a pool
                if (pedge->sql && pedge->sql->execute() == soar_module::row)
                {
                    pedge->time = pedge->sql->column_int(2);
                    pedge_pq.push(pedge);
                }
                else if (pedge->sql)
                {
                    pedge->sql->get_pool()->release(pedge->sql);
                    pedge->sql = NULL;
                }
            }
            next_edge = (pedge_pq.empty() ? after : pedge_pq.top()->time);
            thisAgent->EpMem->epmem_timers->query_walk_edge->stop();

            // process all intervals before the next edge arrives
            thisAgent->EpMem->epmem_timers->query_walk_interval->start();
            while (interval_pq.size() && interval_pq.top()->time > next_edge && current_episode > after)
            {
                if (QUERY_DEBUG >= 1)
                {
                    std::cout << "EPISODE " << current_episode << std::endl;
                }
                // process all interval endpoints at this time step
                while (interval_pq.size() && interval_pq.top()->time >= current_episode)
                {
                    epmem_interval* interval = interval_pq.top();
                    interval_pq.pop();
                    epmem_uedge* uedge = interval->uedge;
                    epmem_triple triple = uedge->triple;
                    if (QUERY_DEBUG >= 1)
                    {
                        std::cout << "	INTERVAL (" << (interval->is_end_point ? "end" : "start") << " at time " << interval->time << "): " << triple.parent_n_id << "-" << triple.attribute_s_id << "-" << triple.child_n_id << std::endl;
                    }
                    if (interval->is_end_point)
                    {
                        uedge->activated = true;
                        uedge->activation_count++;
                        if (uedge->activation_count == 1)
                        {
                            for (epmem_pedge_set::iterator pedge_iter = uedge->pedges.begin(); pedge_iter != uedge->pedges.end(); pedge_iter++)
                            {
                                epmem_pedge* pedge = *pedge_iter;
                                for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                                {
                                    epmem_literal* literal = *lit_iter;
                                    changed_score |= epmem_satisfy_literal(literal, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                                }
                            }
                        }
                    }
                    else
                    {
                        uedge->activated = false;
                        uedge->activation_count--;
                        for (epmem_pedge_set::iterator pedge_iter = uedge->pedges.begin(); pedge_iter != uedge->pedges.end(); pedge_iter++)
                        {
                            epmem_pedge* pedge = *pedge_iter;
                            for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                            {
                                changed_score |= epmem_unsatisfy_literal(*lit_iter, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count);
                            }
                        }
                    }
                    // put the interval query back into the queue if there's more and some literal cares
                    // otherwise, reinitialize the query and put it in a pool
                    if (interval->sql && interval->sql->execute() == soar_module::row)
                    {
                        interval->time = interval->sql->column_int(0);
                        interval_pq.push(interval);
                    }
                    else if (interval->sql)
                    {
                        interval->sql->get_pool()->release(interval->sql);
                        interval->sql = NULL;
                        uedge->intervals--;
                        if (uedge->intervals)
                        {
                            interval_cleanup.erase(interval);
                            thisAgent->memoryManager->free_with_pool(MP_epmem_interval, interval);
                        }
                        else
                        {
                            // TODO JUSTIN retract intervals
                        }
                    }
                }
                next_interval = (interval_pq.empty() ? after : interval_pq.top()->time);
                next_episode = (next_edge > next_interval ? next_edge : next_interval);

                // update the prohibits list to catch up
                while (prohibits.size() && prohibits.back() > current_episode)
                {
                    prohibits.pop_back();
                }
                // ignore the episode if it is prohibited
                while (prohibits.size() && current_episode > next_episode && current_episode == prohibits.back())
                {
                    current_episode--;
                    prohibits.pop_back();
                }

                if (QUERY_DEBUG >= 2)
                {
                    epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
                }

                print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Considering episode (time, cardinality, score) (%u, %u, %f)\n", static_cast<long long int>(current_episode), current_cardinality, current_score);

                // if
                // * the current time is still before any new intervals
                // * and the score was changed in this period
                // * and the new score is higher than the best score
                // then save the current time as the best one
                if (current_episode > next_episode && changed_score && (best_episode == EPMEM_MEMID_NONE || current_score > best_score || (do_graph_match && current_score == best_score && !best_graph_matched)))
                {
                    bool new_king = false;
                    if (best_episode == EPMEM_MEMID_NONE || current_score > best_score)
                    {
                        best_episode = current_episode;
                        best_score = current_score;
                        best_cardinality = current_cardinality;
                        new_king = true;
                    }
                    // we should graph match if the option is set and all leaf literals are satisfied
                    if (current_cardinality == perfect_cardinality)
                    {
                        bool graph_matched = false;
                        if (do_graph_match)
                        {
                            if (gm_order == epmem_param_container::gm_order_undefined)
                            {
                                std::sort(gm_ordering.begin(), gm_ordering.end());
                            }
                            else if (gm_order == epmem_param_container::gm_order_mcv)
                            {
                                std::sort(gm_ordering.begin(), gm_ordering.end(), epmem_gm_mcv_comparator);
                            }
                            epmem_literal_deque::iterator begin = gm_ordering.begin();
                            epmem_literal_deque::iterator end = gm_ordering.end();
                            best_bindings.clear();
                            epmem_node_symbol_map bound_nodes[2];
                            if (QUERY_DEBUG >= 1)
                            {
                                std::cout << "	GRAPH MATCH" << std::endl;
                                epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
                            }
                            thisAgent->EpMem->epmem_timers->query_graph_match->start();
                            graph_matched = epmem_graph_match(begin, end, best_bindings, bound_nodes, thisAgent, 2);
                            thisAgent->EpMem->epmem_timers->query_graph_match->stop();
                        }
                        if (!do_graph_match || graph_matched)
                        {
                            best_episode = current_episode;
                            best_graph_matched = true;
                            current_episode = EPMEM_MEMID_NONE;
                            new_king = true;
                        }
                    }
                    if (new_king && thisAgent->trace_settings[TRACE_EPMEM_SYSPARAM])
                    {
                        char buf[256];
                        SNPRINTF(buf, 254, "NEW KING (perfect, graph-match): (%s, %s)\n", (current_cardinality == perfect_cardinality ? "true" : "false"), (best_graph_matched ? "true" : "false"));
                        thisAgent->outputManager->printa_sf(thisAgent, buf);
                        xml_generate_warning(thisAgent, buf);
                    }
                }

                if (current_episode == EPMEM_MEMID_NONE)
                {
                    break;
                }
                else
                {
                    current_episode = next_episode;
                }
            }
            thisAgent->EpMem->epmem_timers->query_walk_interval->stop();
        }
        thisAgent->EpMem->epmem_timers->query_walk->stop();

        // if the best episode is the default, fail
        // otherwise, put the episode in working memory
        if (best_episode == EPMEM_MEMID_NONE)
        {
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_failure, pos_query);
            if (neg_query)
            {
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_failure, neg_query);
            }
        }
        else
        {
            thisAgent->EpMem->epmem_stats->qry_ret->set_value(best_episode);
            thisAgent->EpMem->epmem_stats->qry_card->set_value(best_cardinality);
            thisAgent->EpMem->epmem_timers->query_result->start();
            Symbol* temp_sym;
            epmem_id_mapping node_map_map;
            epmem_id_mapping node_mem_map;
            // cue size
            temp_sym = thisAgent->symbolManager->make_int_constant(leaf_literals.size());
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_cue_size, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // match cardinality
            temp_sym = thisAgent->symbolManager->make_int_constant(best_cardinality);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_match_cardinality, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // match score
            temp_sym = thisAgent->symbolManager->make_float_constant(best_score);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_match_score, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // normalized match score
            temp_sym = thisAgent->symbolManager->make_float_constant(best_score / perfect_score);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_normalized_match_score, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // status
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_success, pos_query);
            if (neg_query)
            {
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_success, neg_query);
            }
            // give more metadata if graph match is turned on
            if (do_graph_match)
            {
                // graph match
                temp_sym = thisAgent->symbolManager->make_int_constant((best_graph_matched ? 1 : 0));
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match, temp_sym);
                thisAgent->symbolManager->symbol_remove_ref(&temp_sym);

                // mapping
                if (best_graph_matched)
                {
                    goal_stack_level level = state->id->epmem_info->result_wme->value->id->level;
                    // mapping identifier
                    Symbol* mapping = thisAgent->symbolManager->make_new_identifier('M', level);
                    epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping, mapping);
                    thisAgent->symbolManager->symbol_remove_ref(&mapping);

                    for (epmem_literal_node_pair_map::iterator iter = best_bindings.begin(); iter != best_bindings.end(); iter++)
                    {
                        if ((*iter).first->value_is_id)
                        {
                            // create the node
                            temp_sym = thisAgent->symbolManager->make_new_identifier('N', level);
                            epmem_buffer_add_wme(thisAgent, meta_wmes, mapping, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping_node, temp_sym);
                            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
                            // point to the cue identifier
                            epmem_buffer_add_wme(thisAgent, meta_wmes, temp_sym, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping_cue, (*iter).first->value_sym);
                            // save the mapping point for the episode
                            node_map_map[(*iter).second.second] = temp_sym;
                            node_mem_map[(*iter).second.second] = NULL;
                        }
                    }
                }
            }
            // reconstruct the actual episode
            if (level > 2)
            {
                epmem_install_memory(thisAgent, state, best_episode, meta_wmes, retrieval_wmes, &node_mem_map);
            }
            if (best_graph_matched)
            {
                for (epmem_id_mapping::iterator iter = node_mem_map.begin(); iter != node_mem_map.end(); iter++)
                {
                    epmem_id_mapping::iterator map_iter = node_map_map.find((*iter).first);
                    if (map_iter != node_map_map.end() && (*iter).second)
                    {
                        epmem_buffer_add_wme(thisAgent, meta_wmes, (*map_iter).second, thisAgent->symbolManager->soarSymbols.epmem_sym_retrieved, (*iter).second);
                    }
                }
            }
            thisAgent->EpMem->epmem_timers->query_result->stop();
        }
    }

    // cleanup
    thisAgent->EpMem->epmem_timers->query_cleanup->start();
    for (epmem_interval_set::iterator iter = interval_cleanup.begin(); iter != interval_cleanup.end(); iter++)
    {
        epmem_interval* interval = *iter;
        if (interval->sql)
        {
            interval->sql->get_pool()->release(interval->sql);
        }
        thisAgent->memoryManager->free_with_pool(MP_epmem_interval, interval);
    }
    for (int type = EPMEM_RIT_STATE_NODE; type <= EPMEM_RIT_STATE_EDGE; type++)
    {
        for (epmem_triple_pedge_map::iterator iter = pedge_caches[type].begin(); iter != pedge_caches[type].end(); iter++)
        {
            epmem_pedge* pedge = (*iter).second;
            if (pedge->sql)
            {
                pedge->sql->get_pool()->release(pedge->sql);
            }
            pedge->literals.~epmem_literal_set();
            thisAgent->memoryManager->free_with_pool(MP_epmem_pedge, pedge);
        }
        for (epmem_triple_uedge_map::iterator iter = uedge_caches[type].begin(); iter != uedge_caches[type].end(); iter++)
        {
            epmem_uedge* uedge = (*iter).second;
            uedge->pedges.~epmem_pedge_set();
            thisAgent->memoryManager->free_with_pool(MP_epmem_uedge, uedge);
        }
    }
    for (epmem_wme_literal_map::iterator iter = literal_cache.begin(); iter != literal_cache.end(); iter++)
    {
        epmem_literal* literal = (*iter).second;
        literal->parents.~epmem_literal_set();
        literal->children.~epmem_literal_set();
        literal->matches.~epmem_node_pair_set();
        literal->values.~epmem_node_int_map();
        thisAgent->memoryManager->free_with_pool(MP_epmem_literal, literal);
    }
    thisAgent->EpMem->epmem_timers->query_cleanup->stop();

    thisAgent->EpMem->epmem_timers->query->stop();
}


//the other way to do this is to completely ignore interval specification for the query itself and directly query for the start and end states of an interval. -- another is to recurse process_query using the temporal interval constraints to set before/afters. (don't bother with savings)

//the left and right symbols are the bounds of the interval. the left is either a "starts" or a "starts-before" (where "starts" means that the supplied cue is the first and "starts-before" wants the memory system to reach before a bit).
//the right is similar -- the right is either a "ends" or a "ends-after" (where "ends" means that the supplied cue it the last and "ends-after" wants the memory system to reach after a bit).
//the cue is twofold. First, there is a specification of the state graph, like with an ordinary epmem query. However, instead of matching to an instant, we match to a specification of how it changes over time (which implicitly will also require some matching just to the state)
//one way to do this -- provide several states, offering a great deal of redundant information
//another way to do this -- provide a single interval-subsuming state, then provide a separate interval specification that points into this.
//going to do that second way. So, first, there should be a normal "pos_query" like a normal epmem query, but that should span more than one state.
    //then, the interval specification is that "starts" "ends" business. just any partial ordering made of allen's temporal interval relations should be valid, but i'm going to stick with either "afters" or "befores", and treat overlaps the same as meets.
//using that specification, the query can progress very similarly to the existing query code
// -- find the relevant parts of the state, scan backwards, find leaf matches, but where leaf now also includes things that are pointed at by interval specifications, satisfy literals when things match, unsatisfy when they don't, graph match at end.
// -- big difference: literals aren't just "a thing exists", but "a thing exists at the right time". (literal unsatisfaction doesn't happen when something disappears, only when the next thing doesn't have the right relation (doesn't show up before thing disappears).
// -- there's going to be a "current literal set" which is baically the front of the partial ordering. when something goes away, but its successors showed up, that thing is no longer in the current literal set.
//something to keep in my back pocket for efficiency -- can do a graph match up front on the WMgraph if I know my query is pretty selecting in terms of which portions of the WM graph could likely match. ESPECIALLY if I anticipate a nearly-always-uniquely-constraining subset of state.
// (instead of subgraph isomorphism, it's really just indexing using treelike addresses) -- depends on query load.

//to simplify, gonna start only allowing perfect graph matches.
void epmem_process_interval_query(agent* thisAgent, Symbol* state, std::list<Symbol*>* pos_queries,  std::list<Symbol*>* interval_relations, epmem_time_list& prohibits, epmem_time_id before, epmem_time_id after, wme_set& cue_wmes, symbol_triple_list& meta_wmes, symbol_triple_list& retrieval_wmes, int level = 3) //gonna work on neg_queries later.
{//interval_relations have constrained structure epmem command -> interval-relation r
    // r-left-> cue_root, r-right-> cue_root, r -type-> (before/after/etc. a binary relation from left to right, such as "left before right")
    // a query must contain a positive cue
    if (pos_queries->size() == 0)
    {
        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_status, thisAgent->symbolManager->soarSymbols.epmem_sym_bad_cmd);
        return;
    }

    // before and after, if specified, must be valid relative to each other
    if (before != EPMEM_MEMID_NONE && after != EPMEM_MEMID_NONE && before <= after)
    {
        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_status, thisAgent->symbolManager->soarSymbols.epmem_sym_bad_cmd);
        return;
    }

    if (QUERY_DEBUG >= 1)
    {
        std::cout << std::endl << "==========================" << std::endl << std::endl;
    }

    thisAgent->EpMem->epmem_timers->query->start();

    // sort probibit's
    if (!prohibits.empty())
    {
        std::sort(prohibits.begin(), prohibits.end());
    }

    // epmem options
    bool do_graph_match = (thisAgent->EpMem->epmem_params->graph_match->get_value() == on);
    epmem_param_container::gm_ordering_choices gm_order = thisAgent->EpMem->epmem_params->gm_ordering->get_value();

    // variables needed for cleanup
    epmem_wme_literal_map literal_cache;
    epmem_triple_pedge_map pedge_caches[2];
#ifdef USE_MEM_POOL_ALLOCATORS
    epmem_triple_uedge_map uedge_caches[2] =
    {
        epmem_triple_uedge_map(std::less<epmem_triple>(), soar_module::soar_memory_pool_allocator<std::pair<const epmem_triple, epmem_uedge*> >()),
        epmem_triple_uedge_map(std::less<epmem_triple>(), soar_module::soar_memory_pool_allocator<std::pair<const epmem_triple, epmem_uedge*> >())
    };
    epmem_interval_set interval_cleanup = epmem_interval_set(std::less<epmem_interval*>(), soar_module::soar_memory_pool_allocator<epmem_interval*>());
#else
    epmem_triple_uedge_map uedge_caches[2] = {epmem_triple_uedge_map(), epmem_triple_uedge_map()};
    epmem_interval_set interval_cleanup = epmem_interval_set();
#endif

    // TODO JUSTIN additional indices



    // priority queues for interval walk
    epmem_pedge_pq pedge_pq;
    epmem_interval_pq interval_pq;

    // variables needed to track satisfiability
    epmem_symbol_int_map symbol_num_incoming;                 // number of literals with a certain symbol as its value
    epmem_symbol_node_pair_int_map symbol_node_count;         // number of times a symbol is matched by a node

    // various things about the current and the best episodes
    epmem_time_id best_interval_end = EPMEM_MEMID_NONE;
    epmem_time_id best_interval_start = EPMEM_MEMID_NONE;
    /*double best_score = 0;
    bool best_graph_matched = false;
    long int best_cardinality = 0;*/
    //epmem_literal_node_pair_map best_bindings;
    /*double current_score = 0;
    long int current_cardinality = 0;*/

    // To accomodate temporal interval queries, the idea is to expand the semantics associated with cues.
    // Typically, a cue is "these structures must all be simultaneously present"
    // I'll continue to hold that as the specification for a single cue, but now we'll have multiple cues and look for an interval satisfying all of those cues.
    // So, we could have just 3 cues with no temporal intervals and we'll just find the first interval that ends up containing all of those, could even be a single timestep.
    // but, we can also have a temporal orderings associated with the cues. then, for a given cue, it's not enough to just check satisfiability at a timestamp, but also the relation of that satisfaction to other satisfied cues.
    // we can do this by scanning backwards, looking for cue satisfaction, and when it happens, then looking for relations involving that cue.
    // first cue to match marks the end of the interval. last cue to match marks the beginning of the interval. (going backwards in time)
    // when a cue is first satisfied, first check if that alone violates existing temporal relations
    // when a cue is unsatisfied, check if that violates existing temporal relations
    // when the satisfaction of all cues are unchanged, there can be no change in violation/satisfaction of temporal relations.
    // if all of the cues becomes satisfied and then unsatisfied without ever having violated a relation, then you've done it.
    // i'm starting with "after" relations only to begin, but any kind of relation could be translated into booleans that trigger when to consider a given potential interval as failed
    // one exception type -- probably very difficult to do strict "befores" and "afters" instead of "overlaps" and "meets" just because of the potential width of intervals.
    // how to do a match with "afters" -- when something becomes satisfied, you check two things
        // 1 - Is this thing supposed to be before something? If so, is that something already satisfied? Or, was it satisfied on the immediately preceeding cycle? Either is fine.
        // 2 - Is this thing supposed to be after something? If so, take note. That thing better show up.
    // when something becomes unsatisfied
    //  // 1 - Is this supposed to be before something? If so, is that thing already unsatisfied? it better be.
    // //  2 - Is this thing supposed to be after something? If so, that thing better already be here or show up next cycle.
    // // One thing to help math here -- keep a "last satisfying interval" data for each previous binding -- lets you know what to skip "can't have another before bind to this before/after until another after shows up"
    // When a query fails -- something that is supposed to have something just before it goes away and after the preceeding cycle is checked -- it still didn't show up.
    // (A cue is not "really" satisfied until it also satisfies temporal relations that point to the same value_sym as the cue itself.)
    /*
     * Problems:
     *  -- can have something show up multiple times, and only one of those times is the one that fits the temporal interval description -- don't want to invalidate the whole interval until you know it's for sure to-be-scrapped.
     *   --> for example, the only time you know that a cue that could have satisfied being before something is actually unsatisfied isn't if it goes away while the post is still present, but if another doesn't then show up in preceeding cycles
     *
     *   easy way --> have falsification criterea for temporal interval literals where a given use of a satisfying graph match as the satisfaction of the temporal interval relation becomes invalidated -- like a before/after where there is a time gap and the before side wasn't filled -- means that the after side is wrong too.
     */
    /* one algorithm, maybe not the right one, but to think about -- find the first match for each supplied cue. return the interval over which that match is true.
     * bind those interval to each interval relations, see what works.
     * For those that fail, query for previous instance of problematic intervals (for a before/after, occurs if instead it's a "contains", if there's a gap, or if the order is wrong.
     * only do one at a time, replacing the "recentmost" interval, defined as
     *
     * on before/afters, there's a dependency, where it might be the case that a given after is bad for a given before, but a different earlier after might work. basically, you get a nested for loop, very limited, but still.
     * could alternatively do a query for all things not on the LHS of any before/after, then have anything with a LHS do a query for each match it achieves. -- for a chain, do a single match, then look for prev, then for prev, then for prev..., then restart with a new "before" at the top if it fails at any point.
     *
     * could be more clever about it and keep simultaneous matches and just do a single scan backwards. -- need to keep a "running matches" structure.
     * yup -- gonna keep a "running matches" pq with most recent matching cue as the sort.
     * first of the "running matches" to achieve a complete match while scanning backwards wins. rest are discarded.
     * how to make the running matches structure -- pick an arbitrary "on-a-RHS followed by "not on a LHS" to be the cue that is used for ordering. on each new instance of that cue while scanning back, make a new potential-interval-match structure to put into "running matches".
     * should make a separate pedge_pq, interval_pq, literals for each cue. -- nah, can have them all pooled, but need to keep score differently, perhaps by having an "instance of cue" score associated with each running match.
     */

    /*
     * So, initially, need to create a map from cues to temporal interval relation literals using that cue.
     * Also, initially create a list of ongoing matches that initially is empty, but that will store a copy of that map for each partial match.
     * The data structure within "ongoing matches" is not just the map, but the map paired with a score. When that score is at max, that's a complete match. (score is basically a count of satisfied temporal interval relation bindings. will create a dummy binding for things that aren't explicitly temporally related.)
     */


    //should loop over cues first before making interval relation literals:

    std::set<epmem_temporal_literal*> temporal_literals;

    std::multimap<Symbol*,epmem_temporal_literal*> cues_to_temporal_literals;
    std::set<epmem_ongoing_match*> ongoing_matches;//gonna just have a map pointer, i'll need to manage the deletion. (loop over every match every time a score could change) -- alternatively, could keep master map that spans matches
    //this map would contain pairings of cues to intervals and a particular ongoing match would just have pointers to that master map. that master map would have a counter for deletion.
    int max_match_score = 0;//1 for every relation and 1 for cues that aren't involved in a relation.
            // each appearance of the same arbitrarily selected for this instance of process_interval_query rightmost interval spawns a new ongoing match.

    //loop over the temporal interval relation constraints and create temporal literals for them. for those cues not touched, create dummy temporal intervals.
    std::list<Symbol*>::iterator interval_relations_it;
    epmem_wme_list::iterator wme_it;
    epmem_wme_list* children;


    std::set<Symbol*> afters;
    std::set<Symbol*> befores;

    for (interval_relations_it = interval_relations->begin(); interval_relations_it != interval_relations->end(); interval_relations_it++)
    {
        Symbol* left;
        Symbol* right;
        int type;
        children = epmem_get_augs_of_id(*interval_relations_it, get_new_tc_number(thisAgent));
        epmem_temporal_literal* temporal_literal = new epmem_temporal_literal();
        //thisAgent->memoryManager->allocate_with_pool(MP_epmem_temporal_literal, &temporal_literal);//probably should just "new" things instead.

        temporal_literals.insert(temporal_literal);

        for (wme_it = children->begin(); wme_it != children->end(); wme_it++)
        {
            switch ((*wme_it)->attr)//need to read that this is "left", "right", or "type".
            {//thisAgent->symbolManager->soarSymbols.epmem_sym_    left/right/type
                case thisAgent->symbolManager->soarSymbols.epmem_sym_left:
                    temporal_literal->value_sym_1 = (*wme_it)->value;
                    befores.insert((*wme_it)->value);
                    if (afters.find((*wme_it)->value))
                    {
                        afters.erase((*wme_it)->value);
                    }
                    break;
                case thisAgent->symbolManager->soarSymbols.epmem_sym_right:
                    temporal_literal->value_sym_2 = (*wme_it)->value;//need to have checks that these actually point to a valid cue.
                    if (befores.find((*wme_it)->value) == befores.end())
                    {
                        afters.insert((*wme_it)->value);
                    }
                    break;
                case thisAgent->symbolManager->soarSymbols.epmem_sym_type:
                    //(*wme_it)->value;
                    temporal_literal->type = (*wme_it)->value->ic->value;//need to check to make this safe later. -- TODO basically there are a lot of bad things that will happen if i don't throw in some "bad_cmd" checks.
                    assert(temporal_literal->type == BEFORE); //(before == 0 in enums.h, is actually "means" before-overlaps || before-meets, and I can't handle other types yet.)
                    break;
            }
        }
        temporal_literal->is_exists = false;
        temporal_literal->satisfied_1 = false;
        temporal_literal->satisfied_2 = false;
        temporal_literal->just_deleted = 0;
        delete children;
        cues_to_temporal_literals.insert(std::make_pair(temporal_literal->value_sym_1,temporal_literal));
        cues_to_temporal_literals.insert(std::make_pair(temporal_literal->value_sym_2,temporal_literal));
    }
    //for each cue, if it's not already in the cues_to_temporal_literals map, add a dummy literal;
    std::list<Symbol*>::iterator pos_queries_it;

    std::map<Symbol*,int> individual_cues_current_scores;//and initialize a score of 0.

    for (pos_queries_it = pos_queries->begin(); pos_queries_it != pos_queries->end(); pos_queries_it++)
    {
        individual_cues_current_scores[*pos_queries_it] = 0;
        if (cues_to_temporal_literals.find(*pos_queries_it) == cues_to_temporal_literals.end())
        {
            epmem_temporal_literal* temporal_literal = new epmem_temporal_literal();
            temporal_literal->is_exists = true;
            temporal_literal->interval_1_right = -1;
            temporal_literal->interval_1_left = -1;
            temporal_literal->interval_2_left = -1;
            temporal_literal->interval_2_right = -1;
            temporal_literal->satisfied = false;
            temporal_literal->match_using_this = NULL;
            temporal_literal->value_sym_2 = NULL;
            temporal_literal->type = NONE_TYPE;
            temporal_literal->value_sym_1 = *pos_queries_it;
            cues_to_temporal_literals.insert(std::make_pair(*pos_queries_it,temporal_literal));
            temporal_literals.insert(temporal_literal);
        }
    }

    max_match_score = temporal_literals.size();


    //when a cue shows up, either it can fit into some ongoing match(es) or
    // -- it can fit into some ongoing match by replacing itself (forking an existing match) -- (especially if I begin to allow distant before/after instead of only meets/overlaps)
    // -- it can make a new match altogether.
    //for any of those, subject to further "if it doesn't violate things to do so" (so, for example, if it's a LHS of a before and the RHS isn't satisfied, can't possible work). -- means temporal chains will be efficient because they can't easily fork as easily. -- also means that uniquely-constraining cues are awesome.
        //(whenever attempting to bind a given instance of a cue to a temporal relation, always going to check that it actually works)



//The pedge and uedge caches can remain completely shared.

    //however, each cue needs its own set of literals.
    //cue to root_literal map:
    std::map<Symbol*,epmem_literal*> cue_to_root_literal_map;

    //cue to literal cache map:

    //cue to visiting map:
    //with the above three, should be able to build DNF graphs for each cue, separately.
    //cue to perfect score (for the cue) map
    //cue to perfect cardinality (for the cue) map

    //literals should know which cues they belong to and so should pedges.


//scoring should be associated with ongoing matches



    //the real question is when to get rid of an overall ongoing match -- and that's when it has some "after" that can no longer be satisfied


    //big simplifying assumption that makes this not horrible -- only one instance of a cue can exist at a given time (by defn)

//this means that, when scanning backwards, when a new instance shows up, its old one has already gone. since i'm only doing before/after in a specific way, that means
    /*
     * 1 - the new instance that just showed up could be a different after. if so, it should just plain be a new match. do we need to consider preexisting befores? no. they'd either actually be before this or they'd contain this.
     * this, if it's an after, it's just plain a new potential ongoing match. the other after has its own ongoing match structure that was created.
     * 2 - the new instance that just showed up is just an exists. it can fork any and all existing matches, but also it's not very informative. one option -- i'll take an implicit semantics of "should be contained within the temporal specification" and thus if there isn't any appearance by the time the "real" interval relations are satisfied, consider it a failure
     * 3 - the new instance that just showed up is a before. if so, should match to an existing after already in ongoing matches. can't take over the spot for some other before because that before can't still exist and if it's valid, that means its after also finished.
     *
     * so, only "exists" and "afters" can potentially make new ongoing matches. specifically, befores cannot (so an instance that is both a before and an after cannot?
     * the weird case is when an exists show up multiple times during an interval. -- initially going to be arbitrary and just pick the temporally last (first found) (probably has least surprise anyways, naturally will retrieve the rest).
     * there, so only "afters" can create *new* ongoing matches.
     */



    ////just came up with a weird heuristic for retrieval -- when retrieving, retrieve things with a surprise bigger than or equal to the things that matched -- presumably you'd be able to remember those details as well.

    // variables needed for graphmatch
    //epmem_literal_deque gm_ordering;
    // variables needed for building the DNF
    //epmem_literal_set leaf_literals;
    std::map<Symbol*, epmem_literal_set*> cue_to_leaf_literals;
    std::map<Symbol*, epmem_literal_deque*> cue_to_gm_ordering;

    if (level > 1)
    {
        // build the DNF graph while checking for leaf WMEs
        std::list<Symbol*>::iterator pos_queries_it;
        for (pos_queries_it = pos_queries->begin(); pos_queries_it != pos_queries->end(); pos_queries_it++)
        {//need a root literal for each cue, gonna check to make sure that does the pedge and uedge stuff right.

            epmem_literal_set* set_for_this_cue = new epmem_literal_set();
            cue_to_leaf_literals[*pos_queries_it] = set_for_this_cue;//todo have to delete later.

            epmem_literal_deque* gm_ordering = new epmem_literal_deque();
            cue_to_gm_ordering[*pos_queries_it] = gm_ordering;

            epmem_literal* root_literal;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_literal, &root_literal);
            cue_to_root_literal_map[*pos_queries_it] = root_literal;

            root_literal->cue = *pos_queries_it;
            root_literal->id_sym = NULL;
            root_literal->value_sym = *pos_queries_it;
            root_literal->is_neg_q = EPMEM_NODE_POS;
            root_literal->value_is_id = EPMEM_RIT_STATE_EDGE;
            root_literal->is_leaf = false;
            root_literal->attribute_s_id = EPMEM_NODEID_BAD;
            root_literal->child_n_id = EPMEM_NODEID_ROOT;
            root_literal->weight = 0.0;
            new(&(root_literal->parents)) epmem_literal_set();
            new(&(root_literal->children)) epmem_literal_set();
#ifdef USE_MEM_POOL_ALLOCATORS
            new(&(root_literal->matches)) epmem_node_pair_set(std::less<epmem_node_pair>(), soar_module::soar_memory_pool_allocator<epmem_node_pair>(thisAgent));
#else
            new(&(root_literal->matches)) epmem_node_pair_set();
#endif
            new(&(root_literal->values)) epmem_node_int_map();
            symbol_num_incoming[*pos_queries_it] = 1;
            literal_cache[NULL] = root_literal;

            std::set<Symbol*> visiting;
            visiting.insert(*pos_queries_it);
            //for (int query_type = EPMEM_NODE_POS; query_type <= EPMEM_NODE_NEG; query_type++)
            {
                Symbol* query_root = NULL;
                //switch (query_type)
                {
                //    case EPMEM_NODE_POS:
                        query_root = *pos_queries_it;
                //        break;
//                    case EPMEM_NODE_NEG:
//                        query_root = neg_query;
//                        break;
                }
                /*if (!query_root)
                {
                    continue;
                }*/
                epmem_wme_list* children = epmem_get_augs_of_id(query_root, get_new_tc_number(thisAgent));
                // for each first level WME, build up a DNF
                for (epmem_wme_list::iterator wme_iter = children->begin(); wme_iter != children->end(); wme_iter++)
                {//would have to dredge up old cpp knowledge, but i think this will actually modify the relevant stored-the-pointer-in-the-map epmem_literals set by reference. (leaf literals)
                    epmem_literal* child = epmem_build_dnf(*wme_iter, literal_cache, *(cue_to_leaf_literals[*pos_queries_it]), symbol_num_incoming, *(cue_to_gm_ordering[*pos_queries_it]), EPMEM_NODE_POS, visiting, cue_wmes, thisAgent, *pos_queries_it);//all the literals are formed here, based on the cue structure and treating it as treelike.
                    //need a different gm_ordering per cue.
                    //this implicitly assumes that doing all graph matches to the atemporal WM graph is a bad thing to do. -- that it would be more expensive than saving on how many timesteps we inspect while scanning backwards.
                    //basically, it depends on how aliased the state is over time vs within an instant w.r.t. cues.
                    if (child)
                    {
                        // force all first level literals to have the same id symbol
                        child->id_sym = *pos_queries_it;
                        child->parents.insert(root_literal);
                        root_literal->children.insert(child);
                    }
                }
                delete children;
            }
        }
        //so now, at the end of this, we have the same literals, but they all share a literal cache, but the root literals and leaf literals are indirectly accessible through cues.


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /// the less hacky version is to do a perfect graph match to the final timestep, then to walk backwards through the interval changes. (still hacky, but less)
        /// then, if anything fails, do a new perfect graph match with a "before" set to whenever the first perfect match would have no longer applied. (the "next-edge" for one of its bindings)
        /// potential weirdness -- what if multiple bindings could have matched at that initial match? -- worry about that if it becomes a problem.
        /// in the meantime, going to accept the skeleton+temporal specification, noting that things without a temporal interval are taken to mean "throughout". (could mean "exists", could be ignored, whatever)
        /// alternatively, leave the satisfaction the way it is, but change the score business so that a perfect score is only achieved once the whole interval is accounted for.
        ////////
        // calculate the highest possible score and cardinality score
        //double perfect_score = 0;
        //int perfect_cardinality = 0;

        //std::map<Symbol*,double> cues_to_perfect_scores;
        std::map<Symbol*,int> cues_to_perfect_cardinalities;
        std::list<Symbol*>::iterator pos_queries_it;
        for (pos_queries_it = pos_queries->begin(); pos_queries_it != pos_queries->end(); pos_queries_it++)
        {
            for (epmem_literal_set::iterator iter = cue_to_leaf_literals[*pos_queries_it]->begin(); iter != cue_to_leaf_literals[*pos_queries_it]->end(); iter++)
            {
                if (!(*iter)->is_neg_q)
                {
                    //perfect_score += (*iter)->weight;
                    if (cues_to_perfect_cardinalities.find(*pos_queries_it) == cues_to_perfect_cardinalities.end())
                    {
                        cues_to_perfect_cardinalities[*pos_queries_it] = 1;//pretty sure for my immediate purposes, i can through out one of these. just do cardinality probably.
                    }
                    else
                    {
                        cues_to_perfect_cardinalities[*pos_queries_it] = cues_to_perfect_cardinalities[*pos_queries_it] + 1;
                    }
                }
            }
        }

        // set default values for before and after
        if (before == EPMEM_MEMID_NONE)
        {
            before = thisAgent->EpMem->epmem_stats->time->get_value() - 1;
        }
        else
        {
            before = before - 1; // since before's are strict
        }
        if (after == EPMEM_MEMID_NONE)
        {
            after = EPMEM_MEMID_NONE;
        }
        epmem_time_id current_episode = before;
        epmem_time_id next_episode;

        // create dummy edges and intervals
        std::list<Symbol*>::iterator pos_queries_it;
        for (pos_queries_it = pos_queries->begin(); pos_queries_it != pos_queries->end(); pos_queries_it++)
        {
            // insert dummy unique edge and interval end point queries for DNF root
            // we make an SQL statement just so we don't have to do anything special at cleanup
            epmem_triple triple = {EPMEM_NODEID_BAD, EPMEM_NODEID_BAD, EPMEM_NODEID_ROOT};
            epmem_pedge* root_pedge;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_pedge, &root_pedge);
            root_pedge->triple = triple;
            root_pedge->value_is_id = EPMEM_RIT_STATE_EDGE;
            new(&(root_pedge->literals)) epmem_literal_set();
            root_pedge->literals.insert(cue_to_root_literal_map[*pos_queries_it]);//Do I put every root literal here? If so, then it's up to tracking literals to make sure individual cues' satisfaction are tallied correctly. will try that first.
            root_pedge->sql = thisAgent->EpMem->epmem_stmts_graph->pool_dummy->request();
            root_pedge->sql->prepare();
            root_pedge->sql->bind_int(1, LLONG_MAX);
            root_pedge->sql->execute(soar_module::op_reinit);
            root_pedge->time = LLONG_MAX;
            pedge_pq.push(root_pedge);
            pedge_caches[EPMEM_RIT_STATE_EDGE][triple] = root_pedge;

            epmem_uedge* root_uedge;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_uedge, &root_uedge);
            root_uedge->triple = triple;
            root_uedge->value_is_id = EPMEM_RIT_STATE_EDGE;
            root_uedge->activation_count = 0;
            new(&(root_uedge->pedges)) epmem_pedge_set();
            root_uedge->intervals = 1;
            root_uedge->activated = false;
            uedge_caches[EPMEM_RIT_STATE_EDGE][triple] = root_uedge;

            epmem_interval* root_interval;
            thisAgent->memoryManager->allocate_with_pool(MP_epmem_interval, &root_interval);
            root_interval->uedge = root_uedge;
            root_interval->is_end_point = true;
            root_interval->sql = thisAgent->EpMem->epmem_stmts_graph->pool_dummy->request();
            root_interval->sql->prepare();
            root_interval->sql->bind_int(1, before);
            root_interval->sql->execute(soar_module::op_reinit);
            root_interval->time = before;
            interval_pq.push(root_interval);
            interval_cleanup.insert(root_interval);
        }

        if (QUERY_DEBUG >= 1)
        {
            epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
        }


        std::map<Symbol*, std::set<epmem_temporal_literal*>*> cues_to_open_temporal_literal_instances;//just make a new set for each cue and clean up later.
        std::map<Symbol*, std::set<epmem_temporal_literal*>*> cues_to_temporal_literal_instances;
        std::map<Symbol*, std::set<epmem_temporal_literal*>*> active_cues_to_active_temporal_literal_instances;
        std::list<Symbol*>::iterator pos_queries_it;
        for (pos_queries_it = pos_queries->begin(); pos_queries_it != pos_queries->end(); pos_queries_it++)
        {
            cues_to_open_temporal_literal_instances[*pos_queries_it] = new std::set<epmem_temporal_literal*>();
            cues_to_temporal_literal_instances[*pos_queries_it] = new std::set<epmem_temporal_literal*>();
            active_cues_to_active_temporal_literal_instances[*pos_queries_it] = new std::set<epmem_temporal_literal*>();
        }
        std::set<Symbol*> cues_that_hit_perfect;
        std::set<Symbol*> cues_that_just_went_away;
        std::set<Symbol*> cues_that_have_been_perfect;
        // main loop of interval walk
        thisAgent->EpMem->epmem_timers->query_walk->start();
        while (pedge_pq.size() && current_episode > after)//basically, current_episode is decremented in gaps, based on when some aspect of matching to the cue changes (not intermediate cue-agnostic changes).
        {

            epmem_time_id next_edge;
            epmem_time_id next_interval;

            bool changed_score = false;

            thisAgent->EpMem->epmem_timers->query_walk_edge->start();
            next_edge = pedge_pq.top()->time;//the top is the pedge with the highest time. constants have llongmax.

            // process all edges which were last used at this time point
            while (pedge_pq.size() && (pedge_pq.top()->time == next_edge || pedge_pq.top()->time >= current_episode))//all constant pedges will have time greater than or equal to current episode.
            {//current episode walks backwards, so "time" being "greater than"
                epmem_pedge* pedge = pedge_pq.top();
                pedge_pq.pop();
                epmem_triple triple = pedge->triple;
                triple.child_n_id = pedge->sql->column_int(1);

                if (QUERY_DEBUG >= 1)
                {
                    std::cout << "  EDGE " << triple.parent_n_id << "-" << triple.attribute_s_id << "-" << triple.child_n_id << std::endl;
                }

                // create queries for the unique edge children of this partial edge
                if (pedge->value_is_id)
                {
                    bool created = false;
                    for (epmem_literal_set::iterator literal_iter = pedge->literals.begin(); literal_iter != pedge->literals.end(); literal_iter++)
                    {
                        epmem_literal* literal = *literal_iter;
                        for (epmem_literal_set::iterator child_iter = literal->children.begin(); child_iter != literal->children.end(); child_iter++)
                        {
                            created |= epmem_register_pedges(triple.child_n_id, *child_iter, pedge_pq, after, pedge_caches, uedge_caches, thisAgent);
                        }
                    }
                }
                // TODO JUSTIN what I want to do here is, if there is no children which leads to a leaf, retract everything
                // I'm not sure how to properly test for this though

                // look for uedge with triple; if none exist, create one
                // otherwise, link up the uedge with the pedge and consider score changes
                epmem_triple_uedge_map* uedge_cache = &uedge_caches[pedge->value_is_id];
                epmem_triple_uedge_map::iterator uedge_iter = uedge_cache->find(triple);
                if (uedge_iter == uedge_cache->end())
                {//the pedges are concerned with potentially satisfying a literal using a triple. the uedges are concerned with finding intervals of time corresponding to a triple.
                    // create a uedge for this
                    epmem_uedge* uedge;
                    thisAgent->memoryManager->allocate_with_pool(MP_epmem_uedge, &uedge);
                    uedge->triple = triple;
                    uedge->value_is_id = pedge->value_is_id;
                    uedge->activation_count = 0;
                    new(&(uedge->pedges)) epmem_pedge_set();
                    uedge->intervals = 0;
                    uedge->activated = false;
                    // create interval queries for this partial edge
                    bool created = false;
                    int64_t edge_id = pedge->sql->column_int(0);
                    for (int interval_type = EPMEM_RANGE_EP; interval_type <= EPMEM_RANGE_POINT; interval_type++)
                    {
                        for (int point_type = EPMEM_RANGE_START; point_type <= EPMEM_RANGE_END; point_type++)
                        {
                            // pick a timer (any timer)
                            soar_module::timer* sql_timer = NULL;
                            switch (interval_type)
                            {
                                case EPMEM_RANGE_EP:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_ep;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_ep;
                                    }
                                    break;
                                case EPMEM_RANGE_NOW:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_now;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_now;
                                    }
                                    break;
                                case EPMEM_RANGE_POINT:
                                    if (point_type == EPMEM_RANGE_START)
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_start_point;
                                    }
                                    else
                                    {
                                        sql_timer = thisAgent->EpMem->epmem_timers->query_sql_end_point;
                                    }
                                    break;
                            }
                            // create the SQL query and bind it
                            // try to find an existing query first; if none exist, allocate a new one from the memory pools
                            soar_module::pooled_sqlite_statement* interval_sql = NULL;
                            /*
                             *  {
                                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_constant_range e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_constant_now e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT (e.episode_id - 1) AS start FROM epmem_wmes_constant_point e WHERE e.wc_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                                },
                                {
                                    "SELECT e.end_episode_id AS end FROM epmem_wmes_constant_range e WHERE e.wc_id=? AND e.end_episode_id>0 AND e.start_episode_id<=? ORDER BY e.end_episode_id DESC",
                                    "SELECT ? AS end FROM epmem_wmes_constant_now e WHERE e.wc_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT e.episode_id AS end FROM epmem_wmes_constant_point e WHERE e.wc_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                                }
                            },
                            {
                                {
                                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_identifier_range e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT (e.start_episode_id - 1) AS start FROM epmem_wmes_identifier_now e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT (e.episode_id - 1) AS start FROM epmem_wmes_identifier_point e WHERE e.wi_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                                },
                                {
                                    "SELECT e.end_episode_id AS end FROM epmem_wmes_identifier_range e WHERE e.wi_id=? AND e.end_episode_id>0 AND e.start_episode_id<=? ORDER BY e.end_episode_id DESC",
                                    "SELECT ? AS end FROM epmem_wmes_identifier_now e WHERE e.wi_id=? AND e.start_episode_id<=? ORDER BY e.start_episode_id DESC",
                                    "SELECT e.episode_id AS end FROM epmem_wmes_identifier_point e WHERE e.wi_id=? AND e.episode_id<=? ORDER BY e.episode_id DESC"
                                }
                             */
                            interval_sql = thisAgent->EpMem->epmem_stmts_graph->pool_find_interval_queries[pedge->value_is_id][point_type][interval_type]->request(sql_timer);
                            int bind_pos = 1;
                            if (point_type == EPMEM_RANGE_END && interval_type == EPMEM_RANGE_NOW)
                            {
                                interval_sql->bind_int(bind_pos++, current_episode);
                            }
                            interval_sql->bind_int(bind_pos++, edge_id);
                            interval_sql->bind_int(bind_pos++, current_episode);
                            if (interval_sql->execute() == soar_module::row)
                            {
                                epmem_interval* interval;
                                thisAgent->memoryManager->allocate_with_pool(MP_epmem_interval, &interval);
                                interval->is_end_point = point_type;
                                interval->uedge = uedge;
                                // If it's an start point of a range (ie. not a point) and it's before the promo time
                                // (this is possible if a the promotion is in the middle of a range)
                                // trim it to the promo time.
                                // This will only happen if the LTI is promoted in the last interval it appeared in
                                // (since otherwise the start point would not be before its promotion).
                                // We don't care about the remaining results of the query
                                interval->time = interval_sql->column_int(0);
                                interval->sql = interval_sql;
                                interval_pq.push(interval);
                                interval_cleanup.insert(interval);
                                uedge->intervals++;
                                created = true;
                            }
                            else
                            {
                                interval_sql->get_pool()->release(interval_sql);
                            }
                        }
                    }
                    if (created)
                    {
                        uedge->pedges.insert(pedge);
                        uedge_cache->insert(std::make_pair(triple, uedge));
                    }
                    else
                    {
                        uedge->pedges.~epmem_pedge_set();
                        thisAgent->memoryManager->free_with_pool(MP_epmem_uedge, uedge);
                    }
                }
                else
                {//the uedge has already been created. all that needs doing is associating this pedge.
                    epmem_uedge* uedge = (*uedge_iter).second;
                    uedge->pedges.insert(pedge);
                    if (uedge->activated && uedge->activation_count == 1)
                    {//whenever a new pedge is associated with a given uedge, time to satisfy the literals associated with those pedges.
                        for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                        {
                            epmem_literal* literal = (*lit_iter);//cues_to_perfect_cardinalities[*pos_queries_it]
                            double current_score = 0;
                            long int current_cardinality = 0;
                            changed_score |= epmem_satisfy_literal(literal, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                            individual_cues_current_scores[literal->cue] = individual_cues_current_scores[literal->cue] + current_cardinality;
                            if (individual_cues_current_scores[literal->cue] == cues_to_perfect_cardinalities[literal->cue])
                            {
                                cues_that_hit_perfect.insert(literal->cue);
                            }
                            //todo probably want to make current score and cardinality instead reference individual_cues_current_scores.
                        }
                    }
                }

                // put the partial edge query back into the queue if there's more
                // otherwise, reinitialize the query and put it in a pool
                if (pedge->sql && pedge->sql->execute() == soar_module::row)
                {
                    pedge->time = pedge->sql->column_int(2);
                    pedge_pq.push(pedge);//there's only going to be a limited number of these constants with llongmax in the epmem_wmes_constant table, so they'll end up no longer pushing back at some point.
                    //since we care about leaf matches first, they all must be processed, but then not so sure about the remaining "edges".
                }
                else if (pedge->sql)
                {
                    pedge->sql->get_pool()->release(pedge->sql);
                    pedge->sql = NULL;
                }
            }
            next_edge = (pedge_pq.empty() ? after : pedge_pq.top()->time);
            thisAgent->EpMem->epmem_timers->query_walk_edge->stop();

            // process all intervals before the next edge arrives //in other words, the next pedge won't show up for awhile, but we created some uedges with intervals that could show up before then.
            thisAgent->EpMem->epmem_timers->query_walk_interval->start();
            while (interval_pq.size() && interval_pq.top()->time > next_edge && current_episode > after)
            {//the next time any uedge's time shows up before the next pedge does, we have a score to settle first. //basically, the idea is that maybe we won't ever need to look at intervals for some pedges, is why it looks like these are split.
                if (QUERY_DEBUG >= 1)
                {
                    std::cout << "EPISODE " << current_episode << std::endl;
                }
                // process all interval endpoints at this time step
                while (interval_pq.size() && interval_pq.top()->time >= current_episode)
                {
                    epmem_interval* interval = interval_pq.top();
                    interval_pq.pop();
                    epmem_uedge* uedge = interval->uedge;
                    epmem_triple triple = uedge->triple;
                    if (QUERY_DEBUG >= 1)
                    {
                        std::cout << "  INTERVAL (" << (interval->is_end_point ? "end" : "start") << " at time " << interval->time << "): " << triple.parent_n_id << "-" << triple.attribute_s_id << "-" << triple.child_n_id << std::endl;
                    }
                    if (interval->is_end_point)
                    {//uedge's triple shows up (scanning backwards)
                        uedge->activated = true;
                        uedge->activation_count++;
                        if (uedge->activation_count == 1)
                        {
                            for (epmem_pedge_set::iterator pedge_iter = uedge->pedges.begin(); pedge_iter != uedge->pedges.end(); pedge_iter++)
                            {
                                epmem_pedge* pedge = *pedge_iter;
                                for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                                {
                                    epmem_literal* literal = *lit_iter;
                                    double current_score = 0;
                                    long int current_cardinality = 0;
                                    changed_score |= epmem_satisfy_literal(literal, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count, uedge_caches, symbol_num_incoming);
                                    individual_cues_current_scores[literal->cue] = individual_cues_current_scores[literal->cue] + current_cardinality;
                                    if (individual_cues_current_scores[literal->cue] == cues_to_perfect_cardinalities[literal->cue])
                                    {
                                        cues_that_hit_perfect.insert(literal->cue);
                                    }
                                }
                            }
                        }
                    }
                    else
                    {//uedge's start time showed up, time to remove that triple. that means unsatisfying all literals (and thus looking through all pedges using that triple to satisfy a literal.
                        uedge->activated = false;
                        uedge->activation_count--;
                        for (epmem_pedge_set::iterator pedge_iter = uedge->pedges.begin(); pedge_iter != uedge->pedges.end(); pedge_iter++)
                        {
                            epmem_pedge* pedge = *pedge_iter;
                            for (epmem_literal_set::iterator lit_iter = pedge->literals.begin(); lit_iter != pedge->literals.end(); lit_iter++)
                            {
                                double current_score = 0;
                                long int current_cardinality = 0;
                                changed_score |= epmem_unsatisfy_literal(*lit_iter, triple.parent_n_id, triple.child_n_id, current_score, current_cardinality, symbol_node_count);//todo make this also set the start-time for the cue in any temporal literals the root for this literal satisfies.
                                individual_cues_current_scores[(*lit_iter)->cue] = individual_cues_current_scores[(*lit_iter)->cue] + current_cardinality;
                                //if it's in the perfect score list, take it out!
                                std::set<Symbol*>::iterator cue_it = cues_that_hit_perfect.find((*lit_iter)->cue);
                                if (cues_that_have_been_perfect.find((*lit_iter)->cue)!=cues_that_have_been_perfect.end())
                                {
                                    cues_that_just_went_away.insert((*lit_iter)->cue);
                                    cues_that_have_been_perfect.erase((*lit_iter)->cue);
                                }
                                if (cue_it != cues_that_hit_perfect.end())
                                {
                                    cues_that_hit_perfect.erase(cue_it);
                                }
                            }
                        }
                    }
                    // put the interval query back into the queue if there's more and some literal cares
                    // otherwise, reinitialize the query and put it in a pool
                    if (interval->sql && interval->sql->execute() == soar_module::row)
                    {
                        interval->time = interval->sql->column_int(0);
                        interval_pq.push(interval);
                    }
                    else if (interval->sql)
                    {
                        interval->sql->get_pool()->release(interval->sql);
                        interval->sql = NULL;
                        uedge->intervals--;
                        if (uedge->intervals)
                        {
                            interval_cleanup.erase(interval);
                            thisAgent->memoryManager->free_with_pool(MP_epmem_interval, interval);
                        }
                        else
                        {
                            // TODO JUSTIN retract intervals
                        }
                    }
                }
                next_interval = (interval_pq.empty() ? after : interval_pq.top()->time);
                next_episode = (next_edge > next_interval ? next_edge : next_interval);//either an altogether as-yet-unseen satisfaction is gonna happen next or some existing uedge is gonna happen next.

                // update the prohibits list to catch up
                while (prohibits.size() && prohibits.back() > current_episode)
                {
                    prohibits.pop_back();
                }
                // ignore the episode if it is prohibited
                while (prohibits.size() && current_episode > next_episode && current_episode == prohibits.back())
                {
                    current_episode--;
                    prohibits.pop_back();
                }

                if (QUERY_DEBUG >= 2)
                {
                    epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
                }

                print_sysparam_trace(thisAgent, TRACE_EPMEM_SYSPARAM, "Considering episode (time, cardinality, score) (%u, %u, %f)\n", static_cast<long long int>(current_episode), current_cardinality, current_score);

                // if
                // * the current time is still before any new intervals
                // * and the score was changed in this period
                // * and the new score is higher than the best score
                // then save the current time as the best one
                std::set<epmem_ongoing_match> matches_to_trash;
                if (current_episode > next_episode && changed_score && (cues_that_hit_perfect.size() > 0 || cues_that_just_went_away.size() > 0))
                {//now, this will be a for loop. for all of the cues that have a best possible score, try graph matching.

                    //cues_that_just_went_away.insert((*lit_iter)->cue);// gonna make it so that when a cue goes away, pretty much just record keeping. all the interesting stuff happens when it shows up. OTHER THAN when an after has yet to disappear, then a before shows up for it, then than before goes away, but the after didn't go away yet -- that's a big deal.
                    //means that the before wasn't good and should act like the before never happened.
                    std::set<Symbol*>::iterator away_it;
                    for (away_it = cues_that_just_went_away.begin(); away_it != cues_that_just_went_away.end(); away_it++)
                    {//these are all cues that were graph matches that have had an unsatisfy literal happen.
                        //need to find the literals in ongoing matches that use this particular match...
                        ////////only one instance of cue available at a given time -> the ones where the interval has a -1 for the left.//todo is that a reasonable assumption? What about when there are multiple ways to graph match at a given time? answer -- we're not going to worry about it yet.


                        //std::multimap<Symbol*, epmem_temporal_literal*> cues_to_temporal_literal_instances
                        std::set<epmem_temporal_literal*>::iterator active_it;
                        std::set<epmem_temporal_literal*>::iterator active_begin = active_cues_to_active_temporal_literal_instances[*away_it]->begin();
                        std::set<epmem_temporal_literal*>::iterator active_end = active_cues_to_active_temporal_literal_instances[*away_it]->end();
                        for (active_it = active_begin; active_it != active_end; active_it++)
                        {//all of the literals inside of ongoing matches currently using this cue to satisfy an interval and for which that interval hasn't closed (using the active instance of the cue)
                            epmem_temporal_literal* active_literal = *active_it;
                            if (active_literal->value_sym_1 == *away_it)
                            {//the interval that was active and that closed here was for the before of something.
                                assert(active_literal->interval_1_left < 0);//it better be the case that if this was active and it was bound to the before, the before doesn't already have a left.
                                active_literal->interval_1_left = current_episode;
                                if (active_literal->interval_1_left < active_literal->interval_2_left && active_literal->interval_1_right < active_literal->interval_2_right && active_literal->interval_1_right+1 >= active_literal->interval_2_left)
                                {//a satisfied temporal literal!
                                    if (!active_literal->satisfied)
                                    {//we may have already satisfied this without it having concluded just when it showed up. If we didn't, then we can do it here.//todo I don't think this should actually ever happen.
                                        active_literal->satisfied = true;
                                        int score = ++active_literal->match_using_this->match_score;
                                        if (score == max_match_score)
                                        {//winner winner chicken dinner!

                                        }
                                    }
                                }
                                else
                                {//IF AND ONLY IF the before that stopped here didn't work because the after has yet to close can this continue to be an ongoing match. otherwise, this is trash.
                                    if (active_literal->interval_2_left < 0)
                                    {//just pretend that before never happened. propogate through all literals using that before within this match.//overall, the match is still fine

                                    } else {//trash the match

                                    }

                                }
                            }
                            else if (active_literal->value_sym_2 == *away_it)
                            {
                                assert(active_literal->interval_2_left < 0);
                                //this is an after that just finished. need to make sure it wasn't the case that there was some before that already finished for it. that would be bad.
                                if (active_literal->interval_1_left != -1)
                                {//badbad, this is trash now.

                                }
                                else//the before is still going on, so now this is actually a satisfaction if there is a valid before.
                                {
                                    if (active_literal->interval_1_left < active_literal->interval_2_left && active_literal->interval_1_right < active_literal->interval_2_right && active_literal->interval_1_right+1 >= active_literal->interval_2_left)
                                    {//there was a valid before
                                        active_literal->satisfied = true;
                                        int score = ++active_literal->match_using_this->match_score;
                                        if (score == max_match_score)
                                        {//winner winner chicken dinner!

                                        }
                                    }//no else here, could still be fine if a before shows up on the very next timestep.
                                }

                            }
                        }

//                        for (ongoing_matches_it = ongoing_matches.begin(); ongoing_matches_it != ongoing_matches.end(); ongoing_matches_it++)//todo instead of a double-for, keep a map directly from cues to set of temporal literals (ptrs) they satisfy (across all ongoing matches) when they have yet to go away. then, have the temporal literals point to the ongoing match they're within.
//                        {//for each match, find the temporal literals that were using this cue. They are the ones that have a right interval bound, but not a left.
//                            std::multimap<Symbol*,epmem_temporal_literal*>::iterator temporal_literal_it;
//                            for (temporal_literal_it = ongoing_matches_it->cues_to_temporal_literals.lower_bound(*away_it); temporal_literal_it = ongoing_matches_it->cues_to_temporal_literals.upper_bound(*away_it); temporal_literal_it++)
//                            {
//
//                            }
//                        }

                    }//perhaps the biggest thing to figure out is whether the "appearance" of episodic memory is because of architecture changes (literally growing) or because of knowledge changes (crossing a threshold of semantic knowledge of one's history, perhaps).


                    // we should graph match if the option is set and all leaf literals are satisfied

                    //basically, this is where we graph match the thing that finally hit all of its leaves and make sure it actually is a graph match. then, if it is, we integrate it into an ongoing match if a suitable one exists or make one if this is a suitable candidate match for starting a new ongoing match.
                    //then, we stop if this is the final interval for an ongoing match.
                    std::set<Symbol*>::iterator potential_cue_match_it;
                    for (potential_cue_match_it = cues_that_hit_perfect.begin(); potential_cue_match_it != cues_that_hit_perfect.end(); potential_cue_match_it++)
                    {
                        bool graph_matched = false;

                        {
                            epmem_literal_deque* gm_ordering = cue_to_gm_ordering[*potential_cue_match_it];
                            if (gm_order == epmem_param_container::gm_order_undefined)
                            {
                                std::sort(gm_ordering->begin(), gm_ordering->end());
                            }
                            else if (gm_order == epmem_param_container::gm_order_mcv)
                            {
                                std::sort(gm_ordering->begin(), gm_ordering->end(), epmem_gm_mcv_comparator);
                            }
                            epmem_literal_deque::iterator begin = gm_ordering->begin();
                            epmem_literal_deque::iterator end = gm_ordering->end();
                            epmem_literal_node_pair_map* best_bindings = new epmem_literal_node_pair_map();
                            //best_bindings.clear();
                            epmem_node_symbol_map bound_nodes[2];
                            if (QUERY_DEBUG >= 1)
                            {
                                std::cout << "  GRAPH MATCH" << std::endl;
                                epmem_print_retrieval_state(literal_cache, pedge_caches, uedge_caches);
                            }
                            thisAgent->EpMem->epmem_timers->query_graph_match->start();
                            graph_matched = epmem_graph_match(begin, end, *best_bindings, bound_nodes, thisAgent, 2);
                            //this is where we can check against temporal interval literals.
                            if (graph_matched)
                            {//We actually have a match for one of the cues in this temporal query. Let's start an ongoing match if this doesn't fit in any of the existing ongoing matches.


                                cues_that_have_been_perfect.insert(*potential_cue_match_it);
                                //let's stop and think a second. We have an interval that now actually matches at least for an instant.
                                //the way it works -- either this fits into an existing match without this interval having already been satisfied or it's an after and can make a new match.   (two exhaustive cases to consider -- there's not already an after present->this just fills it in for now. -- there's already this after present->this must be an alternative match,
                                //within that second case -- either there was a before associated with that other after and thus this is just a real split or there was not a before associated with that other after and this can't happen because on cessation without a before having filled in that after would have been deleted. thus, only matters in the cases that we get an after and the associated literal was completely satisfied. in those cases, we split, unbinding all literals with the same cues as that literal and also cascading depending on implications (like a before no longer being able to apply because its after was taken away for another literal that was not initially being inpected).
                                //so, basically, the only hard case is when we encounter an after that doesn't have a free spot in an existing ongoing match. why? because it doesn't always equate to starting fresh, but sometimes requires borrowing what can be borrowed from existing matches.
                                //########################################easy mode -- all "rightmost" (do not exist as befores) "afters" both fit into existing matches where appropriate and create new ongoing matches even if they fit.#########################################################

                                //so, first, no matter what it is, we look to see if there are valid matches in the existing ongoing matches.
                                //just be happy and plug it in if so.

                                //std::set<epmem_ongoing_match*>::iterator ongoing_matches_it;
                                //for (ongoing_matches_it = ongoing_matches.begin(); ongoing_matches_it != ongoing_matches.end(); ongoing_matches_it++)
                                std::set<epmem_temporal_literal*>::iterator open_literals_it;
                                std::set<epmem_temporal_literal*> literals_to_remove_from_open;//because the cue filled them in.
                                for (open_literals_it = cues_to_open_temporal_literal_instances[*potential_cue_match_it]->begin(); cues_to_open_temporal_literal_instances[*potential_cue_match_it]->end(); open_literals_it++)
                                {//can keep track of just the temporal literals that have open spots and loop over that, then inspect the match after literal satisfaction. -- rather than loop over all ongoing matches
                                    epmem_temporal_literal* literal_to_match = *open_literals_it;
                                    literals_to_remove_from_open.insert(literal_to_match); //no matter what, this literal will no longer be able to match to this cue until something else changes that. just don't want to mess up iterators until loop is done.
                                    if (*potential_cue_match_it == literal_to_match->value_sym_1)//the big mess here could be encapsulated with a "temporal_literal_satisfaction" function that differentially processed by type of interval relation.
                                    {//it's a before or an exists. if it's an exists, this is easy.
                                        assert(literal_to_match->interval_1_right == -1);
                                        literal_to_match->interval_1_right = current_episode;
                                        if (literal_to_match->is_exists)
                                        {
                                            literal_to_match->satisfied = true;
                                            int score = ++literal_to_match->match_using_this->match_score;
                                            if (score == max_match_score)
                                            {
                                                //We have a winner winner chicken dinner!!!
                                            }
                                        }
                                        else
                                        {//it's a before, so it's not trivial.
                                            if (literal_to_match->interval_2_left != -1)
                                            {//the after part of this is not ongoing. now we just check that indeed, this before that just showed up actually fits.
                                                if (literal_to_match->interval_2_left - 1 == literal_to_match->interval_1_right)
                                                {//if the after interval is already fully specified, the only way for this match to work now is if this is a before-meets.
                                                    //yay, it works, and so this is actually a satisfaction (via before-meets)
                                                    literal_to_match->satisfied = true;
                                                    int score = ++literal_to_match->match_using_this->match_score;
                                                    if (score == max_match_score)
                                                    {
                                                        //We have a winner winner chicken dinner!!!
                                                    }
                                                }
                                                else
                                                {//otherwise there's a gap and this no longer works. have to trash the ongoing.
                                                    matches_to_trash.insert(literal_to_match->match_using_this);
                                                }
                                            }
                                            else
                                            {//the after is ongoing and the before just started to overlap. basically, there's no free room in this literal, but it's not satisfied yet either.

                                            }
                                        }

                                    }
                                    else
                                    {//like when an after that didn't start the cue shows up.
                                        assert(*potential_cue_match_it == literal_to_match->value_sym_2);
                                        assert(literal_to_match->interval_2_right == -1);
                                        literal_to_match->interval_2_right = current_episode;

                                    }




                                    //immediately can stop if it completes an ongoing-match. (before for which the after just started/finished a cycle ago)

                                }

                                //we can check if it's a pure after and if so, create a new match for it regardless of whether it fit into any existing matches.
                                if (afters.find(*potential_cue_match_it) != afters.end())//the only source of new matches
                                {//is a pure after, make a new ongoing-match as well.
                                    //temporal_literals is basically a pure untouched set of literals initialized from the cues before I do the walk here. These can be copied for each new ongoing match.
                                    epmem_ongoing_match* new_match = new epmem_ongoing_match();// needs to keep track of the bindings for the graph matches for later reconstruction. //needs to
                                    //should have a match score.
                                    new_match->match_score = 0;//don't actually satisfy any temporal literals with only the after of a before/after relation.
                                    //std::set<epmem_temporal_literal*>* temporal_literal_copy = new std::set<epmem_temporal_literal*>();//need to actually make real copies of the original ones, so I can't just copy the pointers.

                                    std::set<epmem_temporal_literal*>::iterator temporal_literals_it;
                                    for (temporal_literals_it = temporal_literals.begin(); temporal_literals_it != temporal_literals.end(); temporal_literals_it++)
                                    {//if not an is_exists and not the case that the after here is the second sym
                                        epmem_temporal_literal* literal_copy = new epmem_temporal_literal();
                                        literal_copy->value_sym_1 = (*temporal_literals_it)->value_sym_1;
                                        literal_copy->is_exists = (*temporal_literals_it)->is_exists;
                                        literal_copy->value_sym_2 = (*temporal_literals_it)->value_sym_2;
                                        literal_copy->satisfied = false;
                                        literal_copy->type = (*temporal_literals_it)->type;
                                        literal_copy->match_using_this = new_match;
                                        new_match->temporal_literals.insert(literal_copy);
                                        new_match->cues_to_temporal_literals.insert(std::make_pair(literal_copy->value_sym_1,literal_copy));
                                        new_match->unbound_cues.insert(literal_copy->value_sym_1);
                                        cues_to_open_temporal_literal_instances[literal_copy->value_sym_1]->insert(literal_copy);
                                        if (!literal_copy->is_exists)
                                        {
                                            new_match->cues_to_temporal_literals.insert(std::make_pair(literal_copy->value_sym_2,literal_copy));
                                            new_match->unbound_cues.insert(literal_copy->value_sym_2);
                                            cues_to_open_temporal_literal_instances[literal_copy->value_sym_2]->insert(literal_copy);
                                        }


                                    }
                                    new_match->unbound_cues.erase(*potential_cue_match_it);
                                    new_match->best_bindings.insert(std::make_pair(*potential_cue_match_it,best_bindings));
                                    new_match->match_end = current_episode;
                                    new_match->match_start = -1;//to indicate it's not yet bound.
                                    //since this is a pure after, any temporal_literals that this is used in, it's the second symbol. Can fill out information accordingly.
                                    std::multimap<Symbol*,epmem_temporal_literal*>::iterator this_match_temporal_literals_it;
                                    for (this_match_temporal_literals_it = new_match->cues_to_temporal_literals.lower_bound(*potential_cue_match_it); this_match_temporal_literals_it != new_match->cues_to_temporal_literals.upper_bound(*potential_cue_match_it); this_match_temporal_literals_it++)
                                    {
                                        epmem_temporal_literal* literal_for_which_cue_is_the_after = this_match_temporal_literals_it->second;
                                        literal_for_which_cue_is_the_after->satisfied = false;
                                        literal_for_which_cue_is_the_after->interval_2_right = current_episode;
                                        literal_for_which_cue_is_the_after->interval_2_left = -1;
                                        literal_for_which_cue_is_the_after->interval_1_left = -1;
                                        literal_for_which_cue_is_the_after->interval_1_right = -1;
                                        cues_to_open_temporal_literal_instances[*potential_cue_match_it]->erase(literal_for_which_cue_is_the_after);//the literal is no longer open to this after, given that the after is currently attempting to satisfy it.
                                        cues_to_temporal_literal_instances[*potential_cue_match_it]->insert(literal_for_which_cue_is_the_after);
                                        active_cues_to_active_temporal_literal_instances[*potential_cue_match_it]->insert(literal_for_which_cue_is_the_after);
                                    }

                                }


                                //We check which ongoing match this could fit into //could make this into something other than a for-loop if it actually becomes necessary. -- multimap from unmatched cues to potential matches
//                                bool found_a_fit = false;
//                                bool is_an_after = false;
//                                //cues_to_temporal_literals;
//                                cues_to_temporal_literals;
//                                std::multimap<Symbol*,epmem_temporal_literal*>::iterator temporal_literals_it;
//                                for (temporal_literals_it = cues_to_temporal_literals.lower_bound(*potential_cue_match_it); temporal_literals_it != cues_to_temporal_literals.upper_bound(*potential_cue_match_it); temporal_literals_it++)
//                                {//need to separately iterate over the potentiall-after cues first. basically, double this code. have two "cues_to_temporal_literals", one for afters, one for the rest.
//                                    if (*potential_cue_match_it == temporal_literals_it->second->value_sym_1)
//                                    {//matches to a before. go ahead and try it.
//                                        if (temporal_literals_it->second->satisfied_1)
//                                        {//already have a match and this is a before, so we do nothing here.
//
//                                        }
//                                        else
//                                        {
//                                            if ()
//                                            {
//
//                                            }
//                                            temporal_literals_it->second->satisfied_1 = true;
//                                        }
//                                    }
//
//                                }
//                                std::set<epmem_ongoing_match>::iterator ongoing_matches_it;
//                                for (ongoing_matches_it = ongoing_matches.begin(); ongoing_matches_it != ongoing_matches.end(); ongoing_matches_it++)
//                                {
//
//                                }
                            }
                            thisAgent->EpMem->epmem_timers->query_graph_match->stop();
                        }
//                        if (!true || graph_matched)
//                        {
//                            best_episode = current_episode;
//                            best_graph_matched = true;
//                            current_episode = EPMEM_MEMID_NONE;//todo remember as trigger for making this stop.
//                            new_king = true;
//                        }
                    }
                }
                cues_that_hit_perfect.clear();
                cues_that_just_went_away.clear();

                if (current_episode == EPMEM_MEMID_NONE)
                {
                    break;
                }
                else
                {
                    current_episode = next_episode;
                }
            }
            thisAgent->EpMem->epmem_timers->query_walk_interval->stop();
        }
        thisAgent->EpMem->epmem_timers->query_walk->stop();

        // if the best episode is the default, fail
        // otherwise, put the episode in working memory
        if (best_episode == EPMEM_MEMID_NONE)
        {
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_failure, pos_query);
            if (neg_query)
            {
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_failure, neg_query);
            }
        }
        else
        {
            thisAgent->EpMem->epmem_timers->query_result->start();
            Symbol* temp_sym;
            epmem_id_mapping node_map_map;
            epmem_id_mapping node_mem_map;
            // cue size
            temp_sym = thisAgent->symbolManager->make_int_constant(leaf_literals.size());
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_cue_size, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // match cardinality
            temp_sym = thisAgent->symbolManager->make_int_constant(best_cardinality);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_match_cardinality, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // match score
            temp_sym = thisAgent->symbolManager->make_float_constant(best_score);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_match_score, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // normalized match score
            temp_sym = thisAgent->symbolManager->make_float_constant(best_score / perfect_score);
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_normalized_match_score, temp_sym);
            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
            // status
            epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_success, pos_query);
            if (neg_query)
            {
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_success, neg_query);
            }
            // give more metadata if graph match is turned on
            if (true)
            {
                // graph match
                temp_sym = thisAgent->symbolManager->make_int_constant((best_graph_matched ? 1 : 0));
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match, temp_sym);
                thisAgent->symbolManager->symbol_remove_ref(&temp_sym);

                // mapping
                if (best_graph_matched)
                {
                    goal_stack_level level = state->id->epmem_info->result_wme->value->id->level;
                    // mapping identifier
                    Symbol* mapping = thisAgent->symbolManager->make_new_identifier('M', level);
                    epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping, mapping);
                    thisAgent->symbolManager->symbol_remove_ref(&mapping);

                    for (epmem_literal_node_pair_map::iterator iter = best_bindings.begin(); iter != best_bindings.end(); iter++)
                    {
                        if ((*iter).first->value_is_id)
                        {
                            // create the node
                            temp_sym = thisAgent->symbolManager->make_new_identifier('N', level);
                            epmem_buffer_add_wme(thisAgent, meta_wmes, mapping, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping_node, temp_sym);
                            thisAgent->symbolManager->symbol_remove_ref(&temp_sym);
                            // point to the cue identifier
                            epmem_buffer_add_wme(thisAgent, meta_wmes, temp_sym, thisAgent->symbolManager->soarSymbols.epmem_sym_graph_match_mapping_cue, (*iter).first->value_sym);
                            // save the mapping point for the episode
                            node_map_map[(*iter).second.second] = temp_sym;
                            node_mem_map[(*iter).second.second] = NULL;
                        }
                    }
                }
            }
            // reconstruct the actual episode
            if (level > 2)
            {
                epmem_install_memory(thisAgent, state, best_episode, meta_wmes, retrieval_wmes, &node_mem_map);
            }
            if (best_graph_matched)
            {
                for (epmem_id_mapping::iterator iter = node_mem_map.begin(); iter != node_mem_map.end(); iter++)
                {
                    epmem_id_mapping::iterator map_iter = node_map_map.find((*iter).first);
                    if (map_iter != node_map_map.end() && (*iter).second)
                    {
                        epmem_buffer_add_wme(thisAgent, meta_wmes, (*map_iter).second, thisAgent->symbolManager->soarSymbols.epmem_sym_retrieved, (*iter).second);
                    }
                }
            }
            thisAgent->EpMem->epmem_timers->query_result->stop();
        }
    }

    // cleanup
    thisAgent->EpMem->epmem_timers->query_cleanup->start();

    for (epmem_interval_set::iterator iter = interval_cleanup.begin(); iter != interval_cleanup.end(); iter++)
    {
        epmem_interval* interval = *iter;
        if (interval->sql)
        {
            interval->sql->get_pool()->release(interval->sql);
        }
        thisAgent->memoryManager->free_with_pool(MP_epmem_interval, interval);
    }
    for (int type = EPMEM_RIT_STATE_NODE; type <= EPMEM_RIT_STATE_EDGE; type++)
    {
        for (epmem_triple_pedge_map::iterator iter = pedge_caches[type].begin(); iter != pedge_caches[type].end(); iter++)
        {
            epmem_pedge* pedge = (*iter).second;
            if (pedge->sql)
            {
                pedge->sql->get_pool()->release(pedge->sql);
            }
            pedge->literals.~epmem_literal_set();
            thisAgent->memoryManager->free_with_pool(MP_epmem_pedge, pedge);
        }
        for (epmem_triple_uedge_map::iterator iter = uedge_caches[type].begin(); iter != uedge_caches[type].end(); iter++)
        {
            epmem_uedge* uedge = (*iter).second;
            uedge->pedges.~epmem_pedge_set();
            thisAgent->memoryManager->free_with_pool(MP_epmem_uedge, uedge);
        }
    }
    for (epmem_wme_literal_map::iterator iter = literal_cache.begin(); iter != literal_cache.end(); iter++)
    {
        epmem_literal* literal = (*iter).second;
        literal->parents.~epmem_literal_set();
        literal->children.~epmem_literal_set();
        literal->matches.~epmem_node_pair_set();
        literal->values.~epmem_node_int_map();
        thisAgent->memoryManager->free_with_pool(MP_epmem_literal, literal);
    }
    thisAgent->EpMem->epmem_timers->query_cleanup->stop();

    thisAgent->EpMem->epmem_timers->query->stop();
}





//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// Visualization (epmem::viz)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

inline std::string _epmem_print_sti(epmem_node_id id)
{
    std::string t1, t2;

    t1.assign("<id");

    to_string(id, t2);
    t1.append(t2);
    t1.append(">");

    return t1;
}

void epmem_print_episode(agent* thisAgent, epmem_time_id memory_id, std::string* buf)
{
    epmem_attach(thisAgent);

    // if bad memory, bail
    buf->clear();
    if ((memory_id == EPMEM_MEMID_NONE) ||
            !epmem_valid_episode(thisAgent, memory_id))
    {
        return;
    }

    // fill episode map
    std::map< epmem_node_id, std::string > ltis;
    std::map< epmem_node_id, std::map< std::string, std::list< std::string > > > ep;
    {
        soar_module::sqlite_statement* my_q;
        std::string temp_s, temp_s2, temp_s3;
        int64_t temp_i;

        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_identifier_values;
        {
            epmem_node_id parent_n_id;
            epmem_node_id child_n_id;
            bool val_is_short_term;

            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ]));

            // query for edges
            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            my_q->bind_int(5, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                // parent_n_id, attribute_s_id, child_n_id, epmem_node.lti_id
                parent_n_id = my_q->column_int(0);
                child_n_id = my_q->column_int(2);

                epmem_reverse_hash_print(thisAgent, my_q->column_int(1), temp_s);

                val_is_short_term = (my_q->column_type(3) == soar_module::null_t || my_q->column_int(3) == 0);
                if (val_is_short_term)
                {
                    temp_s2 = _epmem_print_sti(child_n_id);
                }
                else
                {
                    temp_s2.assign("@");
                    temp_i = static_cast< uint64_t >(my_q->column_int(3));
                    to_string(temp_i, temp_s3);
                    temp_s2.append(temp_s3);

                    ltis[ child_n_id ] = temp_s2;
                }

                ep[ parent_n_id ][ temp_s ].push_back(temp_s2);
            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);
        }

        // f.wc_id, f.parent_n_id, f.attribute_s_id, f.value_s_id
        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_constant_values;
        {
            epmem_node_id parent_n_id;

            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ]));

            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                parent_n_id = my_q->column_int(1);
//              fprintf(stderr, "PRINTING %d %d %d\n", (unsigned int) parent_n_id, (unsigned int) my_q->column_int( 2 ), (unsigned int) my_q->column_int( 3 ));
                epmem_reverse_hash_print(thisAgent, my_q->column_int(2), temp_s);
//              fprintf(stderr, "  - Attribute is %s\n", temp_s.data());
                epmem_reverse_hash_print(thisAgent, my_q->column_int(3), temp_s2);
//              fprintf(stderr, "  - Value is %s\n", temp_s2.data());

                ep[ parent_n_id ][ temp_s ].push_back(temp_s2);
            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);
        }
    }

    // output
    {
        std::map< epmem_node_id, std::string >::iterator lti_it;
        std::map< epmem_node_id, std::map< std::string, std::list< std::string > > >::iterator ep_it;
        std::map< std::string, std::list< std::string > >::iterator slot_it;
        std::list< std::string >::iterator val_it;

        for (ep_it = ep.begin(); ep_it != ep.end(); ep_it++)
        {
            buf->append("(");

            // id
            lti_it = ltis.find(ep_it->first);
            if (lti_it == ltis.end())
            {
                buf->append(_epmem_print_sti(ep_it->first));
            }
            else
            {
                buf->append(lti_it->second);
            }

            // attr
            for (slot_it = ep_it->second.begin(); slot_it != ep_it->second.end(); slot_it++)
            {
                buf->append(" ^");
                buf->append(slot_it->first);

                for (val_it = slot_it->second.begin(); val_it != slot_it->second.end(); val_it++)
                {
                    buf->append(" ");
                    buf->append(*val_it);
                }
            }

            buf->append(")\n");
        }
    }
}

void epmem_visualize_episode(agent* thisAgent, epmem_time_id memory_id, std::string* buf)
{
    epmem_attach(thisAgent);

    // if bad memory, bail
    buf->clear();
    if ((memory_id == EPMEM_MEMID_NONE) ||
            !epmem_valid_episode(thisAgent, memory_id))
    {
        return;
    }

    // init
    {
        buf->append("digraph epmem {\n");
    }

    // taken heavily from install
    {
        soar_module::sqlite_statement* my_q;

        // first identifiers (i.e. reconstruct)
        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_identifier_values;
        {
            // for printing
            std::map< epmem_node_id, std::string > stis;
            std::map< epmem_node_id, std::pair< std::string, std::string > > ltis;
            std::list< std::string > edges;
            std::map< epmem_node_id, std::string >::iterator sti_p;
            std::map< epmem_node_id, std::pair< std::string, std::string > >::iterator lti_p;

            // relates to finite automata: child_n_id = d(parent_n_id, w)
            epmem_node_id parent_n_id; // id
            epmem_node_id child_n_id; // attribute
            std::string temp, temp2, temp3, temp4;

            bool val_is_short_term;
            uint64_t val_num;

            // 0 is magic
            temp.assign("ID_0");
            stis.insert(std::make_pair(epmem_node_id(0), temp));

            // prep rit
            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_EDGE ]));

            // query for edges
            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            my_q->bind_int(5, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                // parent_n_id, attribute_s_id, child_n_id, epmem_node.lti_id
                parent_n_id = my_q->column_int(0);
                child_n_id = my_q->column_int(2);

                // "ID_parent_n_id"
                temp.assign("ID_");
                to_string(parent_n_id, temp2);
                temp.append(temp2);

                // "ID_child_n_id"
                temp3.assign("ID_");
                to_string(child_n_id, temp2);
                temp3.append(temp2);

                val_is_short_term = (my_q->column_type(3) == soar_module::null_t || my_q->column_int(3) == 0);
                if (val_is_short_term)
                {
                    sti_p = stis.find(child_n_id);
                    if (sti_p == stis.end())
                    {
                        stis.insert(std::make_pair(child_n_id, temp3));
                    }
                }
                else
                {
                    lti_p = ltis.find(child_n_id);

                    if (lti_p == ltis.end())
                    {
                        // "L#"
                        temp4 = "@";
                        val_num = static_cast<uint64_t>(my_q->column_int(3));
                        to_string(val_num, temp2);
                        temp4.append(temp2);

                        ltis.insert(std::make_pair(child_n_id, std::make_pair(temp3, temp4)));
                    }
                }

                // " -> ID_child_n_id"
                temp.append(" -> ");
                temp.append(temp3);

                // " [ label="attribute_s_id" ];\n"
                temp.append(" [ label=\"");
                epmem_reverse_hash_print(thisAgent, my_q->column_int(1), temp2);
                temp.append(temp2);
                temp.append("\" ];\n");

                edges.push_back(temp);
            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);

            // identifiers
            {
                // short-term
                {
                    buf->append("node [ shape = circle ];\n");

                    for (sti_p = stis.begin(); sti_p != stis.end(); sti_p++)
                    {
                        buf->append(sti_p->second);
                        buf->append(" ");
                    }

                    buf->append(";\n");
                }

                // long-term
                {
                    buf->append("node [ shape = doublecircle ];\n");

                    for (lti_p = ltis.begin(); lti_p != ltis.end(); lti_p++)
                    {
                        buf->append(lti_p->second.first);
                        buf->append(" [ label=\"");
                        buf->append(lti_p->second.second);
                        buf->append("\" ];\n");
                    }

                    buf->append("\n");
                }
            }

            // edges
            {
                std::list< std::string >::iterator e_p;

                for (e_p = edges.begin(); e_p != edges.end(); e_p++)
                {
                    buf->append((*e_p));
                }
            }
        }

        // then epmem_wmes_constant

        my_q = thisAgent->EpMem->epmem_stmts_graph->get_wmes_with_constant_values;
        {
            epmem_node_id wc_id;
            epmem_node_id parent_n_id;

            std::list< std::string > edges;
            std::list< std::string > consts;

            std::string temp, temp2;

            epmem_rit_prep_left_right(thisAgent, memory_id, memory_id, &(thisAgent->EpMem->epmem_rit_state_graph[ EPMEM_RIT_STATE_NODE ]));

            my_q->bind_int(1, memory_id);
            my_q->bind_int(2, memory_id);
            my_q->bind_int(3, memory_id);
            my_q->bind_int(4, memory_id);
            while (my_q->execute() == soar_module::row)
            {
                // f.wc_id, f.parent_n_id, f.attribute_s_id, f.value_s_id
                wc_id = my_q->column_int(0);
                parent_n_id = my_q->column_int(1);

                temp.assign("ID_");
                to_string(parent_n_id, temp2);
                temp.append(temp2);
                temp.append(" -> C_");
                to_string(wc_id, temp2);
                temp.append(temp2);
                temp.append(" [ label=\"");
                epmem_reverse_hash_print(thisAgent, my_q->column_int(2), temp2);
                temp.append(temp2);
                temp.append("\" ];\n");
                edges.push_back(temp);

                temp.assign("C_");
                to_string(wc_id, temp2);
                temp.append(temp2);
                temp.append(" [ label=\"");
                epmem_reverse_hash_print(thisAgent, my_q->column_int(3), temp2);
                temp.append(temp2);
                temp.append("\" ];\n");

                consts.push_back(temp);

            }
            my_q->reinitialize();
            epmem_rit_clear_left_right(thisAgent);

            // constant nodes
            {
                std::list< std::string >::iterator e_p;

                buf->append("node [ shape = plaintext ];\n");

                for (e_p = consts.begin(); e_p != consts.end(); e_p++)
                {
                    buf->append((*e_p));
                }
            }

            // edges
            {
                std::list< std::string >::iterator e_p;

                for (e_p = edges.begin(); e_p != edges.end(); e_p++)
                {
                    buf->append((*e_p));
                }
            }
        }
    }

    // close
    {
        buf->append("\n}\n");
    }
}

//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
// API Implementation (epmem::api)
//////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////

/***************************************************************************
 * Function     : epmem_consider_new_episode
 * Author       : Nate Derbinsky
 * Notes        : Based upon trigger/force parameter settings, potentially
 *                records a new episode
 **************************************************************************/
bool epmem_consider_new_episode(agent* thisAgent)
{
    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->trigger->start();
    ////////////////////////////////////////////////////////////////////////////

    const int64_t force = thisAgent->EpMem->epmem_params->force->get_value();
    bool new_memory = false;

    if (force == epmem_param_container::force_off)
    {
        const int64_t trigger = thisAgent->EpMem->epmem_params->trigger->get_value();

        if (trigger == epmem_param_container::output)
        {
            slot* s;
            wme* w;
            Symbol* ol = thisAgent->io_header_output;

            // examine all commands on the output-link for any
            // that appeared since last memory was recorded
            for (s = ol->id->slots; s != NIL; s = s->next)
            {
                for (w = s->wmes; w != NIL; w = w->next)
                {
                    if (w->timetag > thisAgent->top_goal->id->epmem_info->last_ol_time)
                    {
                        new_memory = true;
                        thisAgent->top_goal->id->epmem_info->last_ol_time = w->timetag;
                    }
                }
            }
        }
        else if (trigger == epmem_param_container::dc)
        {
            new_memory = true;
        }
        else if (trigger == epmem_param_container::none)
        {
            new_memory = false;
        }
    }
    else
    {
        new_memory = (force == epmem_param_container::remember);

        thisAgent->EpMem->epmem_params->force->set_value(epmem_param_container::force_off);
    }

    ////////////////////////////////////////////////////////////////////////////
    thisAgent->EpMem->epmem_timers->trigger->stop();
    ////////////////////////////////////////////////////////////////////////////

    if (new_memory)
    {
        epmem_new_episode(thisAgent);
    }

    return new_memory;
}

void inline _epmem_respond_to_cmd_parse(agent* thisAgent, epmem_wme_list* cmds, bool& good_cue, int& path, epmem_time_id& retrieve, Symbol*& next, Symbol*& previous, Symbol*& query, Symbol*& neg_query, epmem_time_list& prohibit, epmem_time_id& before, epmem_time_id& after, wme_set& cue_wmes, std::list<Symbol*>& temporal_queries = NULL, std::list<Symbol*>& interval_relations = NULL)
{
    cue_wmes.clear();

    retrieve = EPMEM_MEMID_NONE;
    next = NULL;
    previous = NULL;
    temporal_queries = NULL;
    interval_relations = NULL;
    query = NULL;
    neg_query = NULL;
    prohibit.clear();
    before = EPMEM_MEMID_NONE;
    after = EPMEM_MEMID_NONE;
    good_cue = true;
    path = 0;

    for (epmem_wme_list::iterator w_p = cmds->begin(); w_p != cmds->end(); w_p++)
    {
        cue_wmes.insert((*w_p));

        if (good_cue)
        {
            // collect information about known commands
            if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_retrieve)
            {
                if (((*w_p)->value->symbol_type == INT_CONSTANT_SYMBOL_TYPE) &&
                        (path == 0) &&
                        ((*w_p)->value->ic->value > 0))
                {
                    retrieve = (*w_p)->value->ic->value;
                    path = 1;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_next)
            {
                if (((*w_p)->value->is_sti()) &&
                        (path == 0))
                {
                    next = (*w_p)->value;
                    path = 2;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_prev)
            {
                if (((*w_p)->value->is_sti()) &&
                        (path == 0))
                {
                    previous = (*w_p)->value;
                    path = 2;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_query)
            {
                if (((*w_p)->value->is_sti()) &&
                        ((path == 0) || (path == 3)) &&
                        (query == NULL))

                {
                    query = (*w_p)->value;
                    path = 3;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_negquery)
            {
                if (((*w_p)->value->is_sti()) &&
                        ((path == 0) || (path == 3)) &&
                        (neg_query == NULL))

                {
                    neg_query = (*w_p)->value;
                    path = 3;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_before)
            {
                if (((*w_p)->value->symbol_type == INT_CONSTANT_SYMBOL_TYPE) &&
                        ((path == 0) || (path == 3)))
                {
                    if ((before == EPMEM_MEMID_NONE) || (static_cast<epmem_time_id>((*w_p)->value->ic->value) < before))
                    {
                        before = (*w_p)->value->ic->value;
                    }
                    path = 3;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_after)
            {
                if (((*w_p)->value->symbol_type == INT_CONSTANT_SYMBOL_TYPE) &&
                        ((path == 0) || (path == 3)))
                {
                    if (after < static_cast<epmem_time_id>((*w_p)->value->ic->value))
                    {
                        after = (*w_p)->value->ic->value;
                    }
                    path = 3;
                }
                else
                {
                    good_cue = false;
                }
            }
            else if ((*w_p)->attr == thisAgent->symbolManager->soarSymbols.epmem_sym_prohibit)
            {
                if (((*w_p)->value->symbol_type == INT_CONSTANT_SYMBOL_TYPE) &&
                        ((path == 0) || (path == 3)))
                {
                    prohibit.push_back((*w_p)->value->ic->value);
                    path = 3;
                }
                else
                {
                    good_cue = false;
                }
            }
            else
            {
                good_cue = false;
            }
        }
    }

    // if on path 3 must have query
    if ((path == 3) && (query == NULL))
    {
        good_cue = false;
    }

    // must be on a path
    if (path == 0)
    {
        good_cue = false;
    }
}

/***************************************************************************
 * Function     : epmem_respond_to_cmd
 * Author       : Nate Derbinsky
 * Notes        : Implements the Soar-EpMem API
 **************************************************************************/
void epmem_respond_to_cmd(agent* thisAgent)
{
    epmem_attach(thisAgent);

    // respond to query only if db is properly initialized
    if (thisAgent->EpMem->epmem_db->get_status() != soar_module::connected)
    {
        return;
    }

    // start at the bottom and work our way up
    // (could go in the opposite direction as well)
    Symbol* state = thisAgent->bottom_goal;

    epmem_wme_list* wmes;
    epmem_wme_list* cmds;
    epmem_wme_list::iterator w_p;

    wme_set cue_wmes;
    symbol_triple_list meta_wmes;
    symbol_triple_list retrieval_wmes;

    epmem_time_id retrieve;
    Symbol* next;
    Symbol* previous;
    Symbol* query;
    Symbol* neg_query;
    epmem_time_list prohibit;
    epmem_time_id before, after;
    std::list<Symbol*> pos_queries;
    std::list<Symbol*> interval_relations;
    bool good_cue;
    int path;

    uint64_t wme_count;
    bool new_cue;

    bool do_wm_phase = false;

    while (state != NULL)
    {
        ////////////////////////////////////////////////////////////////////////////
        thisAgent->EpMem->epmem_timers->api->start();
        ////////////////////////////////////////////////////////////////////////////
        // make sure this state has had some sort of change to the cmd
        new_cue = false;
        wme_count = 0;
        cmds = NULL;
        {
            tc_number tc = get_new_tc_number(thisAgent);
            std::queue<Symbol*> syms;
            Symbol* parent_sym;

            // initialize BFS at command
            syms.push(state->id->epmem_info->cmd_wme->value);

            while (!syms.empty())
            {
                // get state
                parent_sym = syms.front();
                syms.pop();

                // get children of the current identifier
                wmes = epmem_get_augs_of_id(parent_sym, tc);
                {
                    for (w_p = wmes->begin(); w_p != wmes->end(); w_p++)
                    {
                        wme_count++;

                        if ((*w_p)->timetag > state->id->epmem_info->last_cmd_time)
                        {
                            new_cue = true;
                            state->id->epmem_info->last_cmd_time = (*w_p)->timetag;
                        }

                        if ((*w_p)->value->symbol_type == IDENTIFIER_SYMBOL_TYPE)
                        {
                            syms.push((*w_p)->value);
                        }
                    }

                    // free space from aug list
                    if (cmds == NIL)
                    {
                        cmds = wmes;
                    }
                    else
                    {
                        delete wmes;
                    }
                }
            }

            // see if any WMEs were removed
            if (state->id->epmem_info->last_cmd_count != wme_count)
            {
                new_cue = true;
                state->id->epmem_info->last_cmd_count = wme_count;
            }

            if (new_cue)
            {
                // clear old results
                epmem_clear_result(thisAgent, state);

                do_wm_phase = true;
            }
        }

        // a command is issued if the cue is new
        // and there is something on the cue
        if (new_cue && wme_count)
        {
            _epmem_respond_to_cmd_parse(thisAgent, cmds, good_cue, path, retrieve, next, previous, query, neg_query, prohibit, before, after, cue_wmes, pos_queries);

            ////////////////////////////////////////////////////////////////////////////
            thisAgent->EpMem->epmem_timers->api->stop();
            ////////////////////////////////////////////////////////////////////////////

            retrieval_wmes.clear();
            meta_wmes.clear();

            // process command
            if (good_cue)
            {
                thisAgent->explanationBasedChunker->clear_symbol_identity_map();

                // retrieve
                if (path == 1)
                {
                    epmem_install_memory(thisAgent, state, retrieve, meta_wmes, retrieval_wmes);

                    // add one to the ncbr stat
                    thisAgent->EpMem->epmem_stats->ncbr->set_value(thisAgent->EpMem->epmem_stats->ncbr->get_value() + 1);
                }
                // previous or next
                else if (path == 2)
                {
                    if (next)
                    {
                        epmem_install_memory(thisAgent, state, epmem_next_episode(thisAgent, state->id->epmem_info->last_memory), meta_wmes, retrieval_wmes);

                        // add one to the next stat
                        thisAgent->EpMem->epmem_stats->nexts->set_value(thisAgent->EpMem->epmem_stats->nexts->get_value() + 1);
                    }
                    else
                    {
                        epmem_install_memory(thisAgent, state, epmem_previous_episode(thisAgent, state->id->epmem_info->last_memory), meta_wmes, retrieval_wmes);

                        // add one to the prev stat
                        thisAgent->EpMem->epmem_stats->prevs->set_value(thisAgent->EpMem->epmem_stats->prevs->get_value() + 1);
                    }

                    if (state->id->epmem_info->last_memory == EPMEM_MEMID_NONE)
                    {
                        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_failure, ((next) ? (next) : (previous)));
                    }
                    else
                    {
                        epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_success, ((next) ? (next) : (previous)));
                    }
                }
                // query
                else if (path == 3)
                {
                    epmem_process_query(thisAgent, state, query, neg_query, prohibit, before, after, cue_wmes, meta_wmes, retrieval_wmes);

                    // add one to the cbr stat
                    thisAgent->EpMem->epmem_stats->cbr->set_value(thisAgent->EpMem->epmem_stats->cbr->get_value() + 1);
                }
                else if (path=4)//todo might be higher, have to check.
                {
                    epmem_process_interval_query(thisAgent, state, &pos_queries, &interval_relations, prohibit, before, after, cue_wmes, meta_wmes, retrieval_wmes);
                }
            }
            else
            {
                epmem_buffer_add_wme(thisAgent, meta_wmes, state->id->epmem_info->result_wme->value, thisAgent->symbolManager->soarSymbols.epmem_sym_status, thisAgent->symbolManager->soarSymbols.epmem_sym_bad_cmd);
            }

            // clear prohibit list
            prohibit.clear();

            if (!retrieval_wmes.empty() || !meta_wmes.empty())
            {
                // process preference assertion en masse
                epmem_process_buffered_wmes(thisAgent, state, cue_wmes, meta_wmes, retrieval_wmes);

                // clear cache
                {
                    symbol_triple_list::iterator mw_it;

                    for (mw_it = retrieval_wmes.begin(); mw_it != retrieval_wmes.end(); mw_it++)
                    {
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->id);
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->attr);
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->value);

                        delete(*mw_it);
                    }
                    retrieval_wmes.clear();

                    for (mw_it = meta_wmes.begin(); mw_it != meta_wmes.end(); mw_it++)
                    {
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->id);
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->attr);
                        thisAgent->symbolManager->symbol_remove_ref(&(*mw_it)->value);

                        delete(*mw_it);
                    }
                    meta_wmes.clear();
                }

                // process wm changes on this state
                do_wm_phase = true;
            }

            // clear cue wmes
            cue_wmes.clear();
        }
        else
        {
            ////////////////////////////////////////////////////////////////////////////
            thisAgent->EpMem->epmem_timers->api->stop();
            ////////////////////////////////////////////////////////////////////////////
        }

        // free space from command aug list
        if (cmds)
        {
            delete cmds;
        }

        state = state->id->higher_goal;
    }

    if (do_wm_phase)
    {
        ////////////////////////////////////////////////////////////////////////////
        thisAgent->EpMem->epmem_timers->wm_phase->start();
        ////////////////////////////////////////////////////////////////////////////

        do_working_memory_phase(thisAgent);

        ////////////////////////////////////////////////////////////////////////////
        thisAgent->EpMem->epmem_timers->wm_phase->stop();
        ////////////////////////////////////////////////////////////////////////////
    }
}

/***************************************************************************
 * Function     : epmem_go
 * Author       : Nate Derbinsky
 * Notes        : The kernel calls this function to implement Soar-EpMem:
 *                consider new storage and respond to any commands
 **************************************************************************/
void epmem_go(agent* thisAgent, bool allow_store)
{

    thisAgent->EpMem->epmem_timers->total->start();

    if (allow_store)
    {
        epmem_consider_new_episode(thisAgent);
    }
    epmem_respond_to_cmd(thisAgent);


    thisAgent->EpMem->epmem_timers->total->stop();

}

bool epmem_backup_db(agent* thisAgent, const char* file_name, std::string* err)
{
    bool return_val = false;

    if (thisAgent->EpMem->epmem_db->get_status() == soar_module::connected)
    {
        if (thisAgent->EpMem->epmem_params->lazy_commit->get_value() == on)
        {
            thisAgent->EpMem->epmem_stmts_common->commit->execute(soar_module::op_reinit);
        }

        return_val = thisAgent->EpMem->epmem_db->backup(file_name, err);

        if (thisAgent->EpMem->epmem_params->lazy_commit->get_value() == on)
        {
            thisAgent->EpMem->epmem_stmts_common->begin->execute(soar_module::op_reinit);
        }
    }
    else
    {
        err->assign("Episodic database is not currently connected.");
    }

    return return_val;
}

void EpMem_Id_Delta::add_addition(int64_t newly_added_epmem_id)
{
    this->additions->insert(newly_added_epmem_id);
}
void EpMem_Id_Delta::add_removal(int64_t newly_removed_epmem_id)
{
    this->removals->insert(newly_removed_epmem_id);
}
void EpMem_Id_Delta::add_addition_constant(int64_t newly_added_epmem_id)
{
    this->additions_constant->insert(newly_added_epmem_id);
}
void EpMem_Id_Delta::add_removal_constant(int64_t newly_removed_epmem_id)
{
    this->removals_constant->insert(newly_removed_epmem_id);
}
void EpMem_Id_Delta::add_number_change(int64_t parent, int64_t attr, bool value_change)
{
    this->number_changes->emplace(std::make_pair(std::make_pair(parent,attr),value_change));
}
epmem_id_num_delta_set::const_iterator EpMem_Id_Delta::number_changes_find(int64_t parent, int64_t attr, bool value_change) const
{
    return this->number_changes->find(std::make_pair(std::make_pair(parent,attr),value_change));
}
uint64_t EpMem_Id_Delta::additions_size() const
{
    return additions->size();
}
uint64_t EpMem_Id_Delta::removals_size() const
{
    return removals->size();
}
uint64_t EpMem_Id_Delta::additions_constant_size() const
{
    return additions_constant->size();
}
uint64_t EpMem_Id_Delta::removals_constant_size() const
{
    return removals_constant->size();
}
uint64_t EpMem_Id_Delta::number_changes_size() const
{
    return number_changes->size();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::additions_begin() const
{
    return additions->cbegin();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::additions_end() const
{
    return additions->cend();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::removals_begin() const
{
    return removals->cbegin();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::removals_end() const
{
    return removals->cend();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::additions_constant_begin() const
{
    return additions_constant->cbegin();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::additions_constant_end() const
{
    return additions_constant->cend();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::removals_constant_begin() const
{
    return removals_constant->cbegin();
}
epmem_id_delta_set::const_iterator EpMem_Id_Delta::removals_constant_end() const
{
    return removals_constant->cend();
}
epmem_id_num_delta_set::const_iterator EpMem_Id_Delta::number_changes_begin() const
{
    return number_changes->cbegin();
}
epmem_id_num_delta_set::const_iterator EpMem_Id_Delta::number_changes_end() const
{
    return number_changes->cend();
}
bool EpMem_Id_Delta::operator ==(const EpMem_Id_Delta &other) const
{
    epmem_id_delta_set::const_iterator self_additions_begin = additions_begin();
    epmem_id_delta_set::const_iterator self_removals_begin = removals_begin();
    epmem_id_delta_set::const_iterator self_additions_end = additions_end();
    epmem_id_delta_set::const_iterator self_removals_end = removals_end();
    epmem_id_delta_set::const_iterator other_additions_begin = other.additions_begin();
    epmem_id_delta_set::const_iterator other_removals_begin = other.removals_begin();
    epmem_id_delta_set::const_iterator other_additions_end = other.additions_end();
    epmem_id_delta_set::const_iterator other_removals_end = other.removals_end();
    epmem_id_delta_set::const_iterator self_additions_it;
    epmem_id_delta_set::const_iterator self_removals_it;
    epmem_id_delta_set::const_iterator other_additions_it;
    epmem_id_delta_set::const_iterator other_removals_it;
    epmem_id_delta_set::const_iterator self_additions_constant_begin = additions_constant_begin();
    epmem_id_delta_set::const_iterator self_removals_constant_begin = removals_constant_begin();
    epmem_id_delta_set::const_iterator self_additions_constant_end = additions_constant_end();
    epmem_id_delta_set::const_iterator self_removals_constant_end = removals_constant_end();
    epmem_id_delta_set::const_iterator other_additions_constant_begin = other.additions_constant_begin();
    epmem_id_delta_set::const_iterator other_removals_constant_begin = other.removals_constant_begin();
    epmem_id_delta_set::const_iterator other_additions_constant_end = other.additions_constant_end();
    epmem_id_delta_set::const_iterator other_removals_constant_end = other.removals_constant_end();
    epmem_id_delta_set::const_iterator self_additions_constant_it;
    epmem_id_delta_set::const_iterator self_removals_constant_it;
    epmem_id_delta_set::const_iterator other_additions_constant_it;
    epmem_id_delta_set::const_iterator other_removals_constant_it;
    epmem_id_num_delta_set::const_iterator self_number_changes_begin = number_changes_begin();
    epmem_id_num_delta_set::const_iterator self_number_changes_end = number_changes_end();
    epmem_id_num_delta_set::const_iterator self_number_changes_it;
    epmem_id_num_delta_set::const_iterator other_number_changes_begin = other.number_changes_begin();
    epmem_id_num_delta_set::const_iterator other_number_changes_end = other.number_changes_end();
    epmem_id_num_delta_set::const_iterator other_number_changes_it;
    if (additions_size() != other.additions_size())
    {
        return false;
    }
    if (removals_size() != other.removals_size())
    {
        return false;
    }
    if (additions_constant_size() != other.additions_constant_size())
    {
        return false;
    }
    if (removals_constant_size() != other.removals_constant_size())
    {
        return false;
    }
    if (number_changes_size() != other.number_changes_size())
    {
        return false;
    }
    //Supposing i switch the underlying set to unordered set, this will have to be changed to iteration
    //over one and manual checking in the other instead of iteration over both.
    other_additions_it = other_additions_begin;
    for (self_additions_it = self_additions_begin; self_additions_it != self_additions_end; ++self_additions_it)
    {
        if (*self_additions_it != *other_additions_it)
        {
            return false;
        }
        ++other_additions_it;
    }

    other_additions_constant_it = other_additions_constant_begin;
    for (self_additions_constant_it = self_additions_constant_begin; self_additions_constant_it != self_additions_constant_end; ++self_additions_constant_it)
    {
        if (*self_additions_constant_it != *other_additions_constant_it)
        {
            return false;
        }
        ++other_additions_constant_it;
    }

    other_removals_it = other_removals_begin;
    for (self_removals_it = self_removals_begin; self_removals_it != self_removals_end; ++self_removals_it)
    {
        if (*self_removals_it != *other_removals_it)
        {
            return false;
        }
        ++other_removals_it;
    }

    other_removals_constant_it = other_removals_constant_begin;
    for (self_removals_constant_it = self_removals_constant_begin; self_removals_constant_it != self_removals_constant_end; ++self_removals_constant_it)
    {
        if (*self_removals_constant_it != *other_removals_constant_it)
        {
            return false;
        }
        ++other_removals_constant_it;
    }

    other_number_changes_it = other_number_changes_begin;
    for (self_number_changes_it = self_number_changes_begin; self_number_changes_it != self_number_changes_end; ++self_number_changes_it)
    {//std::set<std::pair<std::pair<int64_t,int64_t>, bool>> is the type of the set which is being iterated over.
        if (self_number_changes_it->first.first != other_number_changes_it->first.first || self_number_changes_it->first.second != other_number_changes_it->first.second || self_number_changes_it->second != other_number_changes_it->second)
        {
            return false;
        }
        ++other_number_changes_it;
    }

    return true;
}

bool EpMem_Id_Delta::operator !=(const EpMem_Id_Delta &other) const
{
    return !(*this == other);
}

EpMem_Id_Delta EpMem_Id_Delta::operator +(const EpMem_Id_Delta &other) const
{
    EpMem_Id_Delta sum(other);
    //The logic for the sum is to add all of the first delta to the sum. then, the second delta will either also add or change the sum. Additions that are removed must be cancelled out. Similarly, removals that are added must be cancelled out.
    //Additionally, number_changes that do not agree must be given recalculated. Finally, number_changes that do not exist in either the first or the second must be turned into either additions or removals.
    //This all allows a rule to be represented as a single delta.
    return sum; //TODO
    //Combined with a concrete-style heuristic for old rules in Sequitur and old sequence memory in EpMem, could allow s-rules to be stored in SMem, and turn top-level EpMem interval representation into shorter version that only stores rule summary.
    //Allows rules to compress EpMem, loses individual values for items in compressed subsequences.
    //*Could* maintain in SMem additional instances-level data.
}

inline void insert_i_info(agent* myAgent, std::set<uint64_t>* value_bool, std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>* i_information,
        epmem_id_delta_set::iterator it, std::map<uint64_t,std::string>* translation, std::map<std::tuple<uint64_t,uint64_t,uint64_t,bool>,std::pair<uint64_t,bool>>* raw_triple_to_wid, bool add)
{
    soar_module::sqlite_statement* get_constant = myAgent->EpMem->epmem_stmts_graph->get_constant;
    soar_module::sqlite_statement* get_single_wiid_info = myAgent->EpMem->epmem_stmts_graph->get_single_wiid_info;
    get_single_wiid_info->bind_int(1,*it);
    get_single_wiid_info->execute();
    uint64_t parent_id = get_single_wiid_info->column_int(0);
    uint64_t attribute_id = get_single_wiid_info->column_int(1);
    uint64_t value_id = get_single_wiid_info->column_int(2);
    get_single_wiid_info->reinitialize();
    raw_triple_to_wid->emplace(std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent_id,attribute_id,value_id,false),std::pair<uint64_t,bool>(*it,add));
    //We now have the triple. We need to harvest printable representations from the table, though.
    get_constant->bind_int(1,attribute_id);
    get_constant->bind_int(2,attribute_id);
    get_constant->bind_int(3,attribute_id);
    get_constant->execute();
    std::string attribute(get_constant->column_text(0));
    get_constant->reinitialize();
    translation->emplace(attribute_id,attribute);
    value_bool->insert(value_id);
    i_information->emplace(parent_id,std::pair<uint64_t,uint64_t>(attribute_id,value_id));
}
inline void insert_c_info(agent* myAgent, std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>* c_information,
        epmem_id_delta_set::iterator it, std::map<uint64_t,std::string>* translation, std::map<std::tuple<uint64_t,uint64_t,uint64_t,bool>,std::pair<uint64_t,bool>>* raw_triple_to_wid, bool add, std::map<uint64_t,uint64_t>* wc_id_to_value_id)
{
    soar_module::sqlite_statement* get_constant = myAgent->EpMem->epmem_stmts_graph->get_constant;
    soar_module::sqlite_statement* get_single_wcid_info = myAgent->EpMem->epmem_stmts_graph->get_single_wcid_info;
    get_single_wcid_info->bind_int(1,*it);
    get_single_wcid_info->execute();
    uint64_t parent_id = get_single_wcid_info->column_int(0);
    uint64_t attribute_id = get_single_wcid_info->column_int(1);
    uint64_t value_id = get_single_wcid_info->column_int(2);
    wc_id_to_value_id->emplace(*it, value_id);
    get_single_wcid_info->reinitialize();
    raw_triple_to_wid->emplace(std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent_id,attribute_id,value_id,true),std::pair<uint64_t,bool>(*it,add));
    //We now have the triple. We need to harvest printable representations from the table, though.
    get_constant->bind_int(1,attribute_id);
    get_constant->bind_int(2,attribute_id);
    get_constant->bind_int(3,attribute_id);
    get_constant->execute();
    std::string attribute(get_constant->column_text(0));
    get_constant->reinitialize();
    translation->emplace(attribute_id,attribute);
    get_constant->bind_int(1,value_id);
    get_constant->bind_int(2,value_id);
    get_constant->bind_int(3,value_id);
    get_constant->execute();
    std::string value(get_constant->column_text(0));
    translation->emplace(value_id,value);
    c_information->emplace(parent_id,std::pair<uint64_t,uint64_t>(attribute_id,value_id));
    get_constant->reinitialize();
}

std::ostream& operator<< (std::ostream &out, const EpMem_Id_Delta &delta)
{//"wid" means triple, referring to either wiid or wcid.
    out << "[";
    auto it = delta.additions->begin();
    agent* thisAgent = delta.myAgent;
    //get_single_wiid_info will be used to get the local structural information, which will be fed into a map.
        //This map will then be used for printing in a more hierarchical manner. A separate map will record how often
        //a given identifier is a value. Those with a value of zero will be printed recursively. (which really means use a set...)
    std::set<uint64_t> value_bool;
    std::map<uint64_t,std::string> translation;
    std::map<std::tuple<uint64_t,uint64_t,uint64_t,bool>,std::pair<uint64_t,bool>> raw_triple_to_wid;//The pair key bool == true indicates identifier (instead of constant) valued wme.
    //lol, nope. that becomes obvious when printing, so now the bool instead represents being addition. False represents removal. This lets the printing be way more clear on what is happenning with the delta.
    //Also, I'm actually including the constant bit in the front end. My logic was all wrong the first time. The tuple bool represents being a constant. The value bool represents addition.
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>> i_information;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>> c_information;
    std::map<uint64_t,uint64_t> wc_id_to_value_id;
    if (it != delta.additions->end())
    {
        out << "na: " << *it;

        insert_i_info(thisAgent, &value_bool, &i_information, it, &translation, &raw_triple_to_wid, true);

        ++it;
    }
    while (it != delta.additions->end())
    {
        out << ", " << *it;

        insert_i_info(thisAgent, &value_bool, &i_information, it, &translation, &raw_triple_to_wid, true);

        ++it;
    }
    out << ";";

    it = delta.removals->begin();
    if (it != delta.removals->end())
    {
        out << " nr: " << *it;

        insert_i_info(thisAgent, &value_bool, &i_information, it, &translation, &raw_triple_to_wid, false);

        ++it;
    }
    while (it != delta.removals->end())
    {
        out << ", " << *it;

        insert_i_info(thisAgent, &value_bool, &i_information, it, &translation, &raw_triple_to_wid, false);

        ++it;
    }
    out << ";";

    it = delta.additions_constant->begin();
    if (it != delta.additions_constant->end())
    {
        out << " ca: " << *it;

        insert_c_info(thisAgent, &c_information, it, &translation, &raw_triple_to_wid, true,&wc_id_to_value_id);

        ++it;
    }
    while (it != delta.additions_constant->end())
    {
        out << ", " << *it;

        insert_c_info(thisAgent, &c_information, it, &translation, &raw_triple_to_wid, true,&wc_id_to_value_id);

        ++it;
    }
    out << ";";

    it = delta.removals_constant->begin();
    if (it != delta.removals_constant->end())
    {
        out << " cr: " << *it;

        insert_c_info(thisAgent, &c_information, it, &translation, &raw_triple_to_wid,false,&wc_id_to_value_id);

        ++it;
    }
    while (it != delta.removals_constant->end())
    {
        out << ", " << *it;

        insert_c_info(thisAgent, &c_information, it, &translation, &raw_triple_to_wid,false,&wc_id_to_value_id);

        ++it;
    }
    out << ";";

    soar_module::sqlite_statement* get_constant_1 = thisAgent->EpMem->epmem_stmts_graph->get_constant;
    auto it2 = delta.number_changes->begin();
    if (it2 != delta.number_changes->end())
    {//std::set<std::pair<std::pair<int64_t,int64_t>, bool>>
        get_constant_1->bind_int(1,it2->first.second);
        get_constant_1->bind_int(2,it2->first.second);
        get_constant_1->bind_int(3,it2->first.second);
        get_constant_1->execute();
        std::string attribute(get_constant_1->column_text(0));
        get_constant_1->reinitialize();
        out << " c_change: (" << it2->first.first << " | " << attribute << " | " << (it2->second ? "+" : "-") << ")";

        //insert_c_change_info(thisAgent, &c_change_information, it2, &translation, &raw_triple_to_wid,false,&wc_id_to_value_id);

        ++it2;
    }
    while (it2 != delta.number_changes->end())
    {
        get_constant_1->bind_int(1,it2->first.second);
        get_constant_1->bind_int(2,it2->first.second);
        get_constant_1->bind_int(3,it2->first.second);
        get_constant_1->execute();
        std::string attribute(get_constant_1->column_text(0));
        get_constant_1->reinitialize();
        out << ", (" << it2->first.first << " | " << attribute << " | " << (it2->second ? "+" : "-") << ")";

        //insert_c_change_info(thisAgent, &c_change_information, it2, &translation, &raw_triple_to_wid,false,&wc_id_to_value_id);

        ++it2;
    }
    out << "]";

    //This is special case printing for when there were only constant changes, and no node changes.
    /*if (delta.additions->empty() && delta.removals->empty() && !(delta.additions_constant->empty() && delta.removals_constant->empty()))
    {//In this case, we won't trigger the more network-style printing that happens below this if block and instead we'll have
        //to just print out manually the particular wmes involved for these additions and removals. Of note, we will have to list
        //the node ids even though they aren't involved, just to it makes sense to look at.

        for (auto c_additions_it = delta.additions_constant->begin(); c_additions_it != delta.additions_constant->end(); ++c_additions_it)
        {

        }
    }*/
    std::set<uint64_t> value_c_bool;

    //At this point, we have a lot of graphical information we need to print that is associated with the rule.
    //We loop through the i_information keys, checking that they do not contain membership in the value_bool set.
    //We also check at each item (that is now established to not be member of the value_bool set) for if there is membership as a key
    //in c_information. If so, we print all of that as well and mark the value_bool as true for that item.
    //For every i_information value that is also an i_information key, when we print the value, we will also
    //indent and print the child structure.
    //For any item which is currently the i_information of interest, we first print all constants from c_information.
    //Only after all i_information has been printed (either at the top level or nested), do we look for c_information
    //that has yet to be printed (doesn't fall into value_bool).
    auto i_it = i_information.begin();//Just to reiterate. This is the first loop and itself also an outer loop.
    auto i_it_end = i_information.end();
    std::set<uint64_t> printed_already;
    std::queue<uint64_t> child_queue;
    std::pair<std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator, std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator> c_child_range;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_c_it_begin;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_c_it_end;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_c_it;
    std::pair<std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator, std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator> i_child_range;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_i_it_begin;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_i_it_end;
    std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator child_i_it;
    uint64_t current_depth = 0;
    std::map<uint64_t,uint64_t> depth_map;
    std::set<uint64_t> added_to_child_queue_already;
    while(i_it!=i_it_end)
    {
        //First, we check for membership in the value_bool set. (Is this someone's child?)
        //In order to check for membership, we need the id of the parent for this wid, first.
        uint64_t parent = i_it->first;
        if (value_bool.find(parent) != value_bool.end())
        {
            ++i_it;
            continue;//We'll get to it as a child, not as a parent.
        }
        value_bool.insert(parent);
        out << std::endl << "    {<" << parent << ">";
        //We know this is a top level parent, now. Next, we check for constant children and print them.

        c_child_range = c_information.equal_range(parent);
        child_c_it_begin = c_child_range.first;
        child_c_it_end = c_child_range.second;
        child_c_it = child_c_it_begin;
        uint64_t attribute_id;
        uint64_t value_id;

        while(child_c_it != child_c_it_end)
        {
            //We now loop over the constant attribute/value pairs for this parent.
            attribute_id = (*child_c_it).second.first;
            value_id = (*child_c_it).second.second;
            std::pair<uint64_t,bool>* epmem_id = &(raw_triple_to_wid[std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent,attribute_id,value_id,true)]);
            out << " ^" << translation[attribute_id] << " " << translation[value_id] << " [" << epmem_id->first << " " << (epmem_id->second ? "+" : "-") << "]";
            value_c_bool.insert(value_id);
            ++child_c_it;
        }
        //We have now looped through this parent's constant children. Next are the variable/identifier ones.
        std::pair<std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator, std::multimap<uint64_t,std::pair<uint64_t,uint64_t>>::iterator> i_child_range;
        i_child_range = i_information.equal_range(parent);
        child_i_it_begin = i_child_range.first;
        child_i_it_end = i_child_range.second;
        child_i_it = child_i_it_begin;
        while(child_i_it != child_i_it_end)
        {
            attribute_id = (*child_i_it).second.first;
            value_id = (*child_i_it).second.second;
            std::pair<uint64_t,bool>* epmem_id = &(raw_triple_to_wid[std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent,attribute_id,value_id,false)]);
            out << " ^" << translation[attribute_id] << " <" << value_id << ">" << " [" << epmem_id->first << " " << (epmem_id->second ? "+" : "-") << "]";
            //I will do a "recursive" (queued) traversal through children *after* doing the top of this parent.
            if (added_to_child_queue_already.find(value_id) == added_to_child_queue_already.end())
            {
                child_queue.push(value_id);
                added_to_child_queue_already.insert(value_id);
                depth_map[value_id] = current_depth + 1;
            }
            ++child_i_it;
        }
        //At this point, we can close off the top level of that parent.
        out << "}";// << std::endl;
        printed_already.insert(parent);

        ++i_it;
    }
    std::string current_indentation = "    ";//We will keep extending this by four spaces for every depth.
    while(!child_queue.empty())
    {
        uint64_t parent = child_queue.front();
        child_queue.pop();
        if (i_information.find(parent) == i_information.end() && c_information.find(parent) == c_information.end())
        {
            continue;
        }
        printed_already.insert(parent);
        if (depth_map[parent] > current_depth)
        {
            ++current_depth;
            current_indentation.append("    ");
        }
        //We now look for any structure corresponding to this child. We indent, start a line, and go through the same process of constants, then children.
        out << std::endl << current_indentation << "{<" << parent << ">";

        c_child_range = c_information.equal_range(parent);
        child_c_it_begin = c_child_range.first;
        child_c_it_end = c_child_range.second;
        child_c_it = child_c_it_begin;
        while(child_c_it != child_c_it_end)
        {
            std::pair<uint64_t,bool>* epmem_id = &(raw_triple_to_wid[std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent,child_c_it->second.first,child_c_it->second.second,true)]);
            out << " ^" << translation[child_c_it->second.first] << " " << translation[child_c_it->second.second] << " [" << epmem_id->first << " " << (epmem_id->second ? "+" : "-") << "]";
            value_c_bool.insert(child_c_it->second.second);
            ++child_c_it;
        }
        i_child_range = i_information.equal_range(parent);
        child_i_it_begin = i_child_range.first;
        child_i_it_end = i_child_range.second;
        child_i_it = child_i_it_begin;
        while(child_i_it != child_i_it_end)
        {
            std::pair<uint64_t,bool>* epmem_id = &(raw_triple_to_wid[std::tuple<uint64_t,uint64_t,uint64_t,bool>(parent,child_i_it->second.first,child_i_it->second.second,false)]);
            out << " ^" << translation[child_i_it->second.first] << " <" << child_i_it->second.second << ">" << " [" << epmem_id->first << " " << (epmem_id->second ? "+" : "-") << "]";
            if (added_to_child_queue_already.find(child_i_it->second.second) == added_to_child_queue_already.end())
            {
                child_queue.push(child_i_it->second.second);
                added_to_child_queue_already.insert(child_i_it->second.second);
                depth_map[child_i_it->second.second] = current_depth + 1;
            }
            ++child_i_it;
        }
        out << "}";
    }
    auto c_additions_it = delta.additions_constant->begin();
    bool did_co = false;
    if (c_additions_it != delta.additions_constant->end())
    {
        did_co = false;
        if (value_c_bool.find(wc_id_to_value_id[*c_additions_it]) == value_c_bool.end())
        {
            out << std::endl << "    {constant-only-additions: ";
            out << translation[wc_id_to_value_id[*c_additions_it]];
            did_co = true;
        }
        ++c_additions_it;
        while (c_additions_it != delta.additions_constant->end())
        {
            if (value_c_bool.find(wc_id_to_value_id[*c_additions_it]) == value_c_bool.end())
            {
                if (!did_co)
                    out << std::endl << "    {constant-only-additions: ";
                out << ", " << translation[wc_id_to_value_id[*c_additions_it]];
                did_co = true;
            }
            ++c_additions_it;
        }
        if (did_co)
            out << "}";
    }
    auto c_removals_it = delta.removals_constant->begin();
    if (c_removals_it != delta.removals_constant->end())
    {
        did_co = false;
        if (value_c_bool.find(wc_id_to_value_id[*c_removals_it]) == value_c_bool.end())
        {
            out << std::endl << "    {constant-only-removals: ";
            out << translation[wc_id_to_value_id[*c_removals_it]];
            did_co = true;
        }
        ++c_removals_it;
        while (c_removals_it != delta.removals_constant->end())
        {
            if (value_c_bool.find(wc_id_to_value_id[*c_removals_it]) == value_c_bool.end())
            {
                if (!did_co)
                    out << std::endl << "    {constant-only-removals: ";
                out << ", " << translation[wc_id_to_value_id[*c_removals_it]];
                did_co = true;
            }
            ++c_removals_it;
        }
        if (did_co)
            out << "}";
    }
    return out;
}

EpMem_Id_Delta::EpMem_Id_Delta(EpMem_Id_Delta&& other)
    : additions(NULL)
    , removals(NULL)
    , additions_constant(NULL)
    , removals_constant(NULL)
    , number_changes(NULL)
    , myAgent(NULL)
{
    myAgent = other.myAgent;
    additions = other.additions;
    removals = other.removals;
    additions_constant = other.additions_constant;
    removals_constant = other.removals_constant;
    number_changes = other.number_changes;
    other.additions = NULL;
    other.removals = NULL;
    other.additions_constant = NULL;
    other.removals_constant = NULL;
    other.number_changes = NULL;
}
EpMem_Id_Delta::EpMem_Id_Delta(const EpMem_Id_Delta& other)
{
    additions = new epmem_id_delta_set(*(other.additions));
    removals = new epmem_id_delta_set(*(other.removals));
    additions_constant = new epmem_id_delta_set(*(other.additions_constant));
    removals_constant = new epmem_id_delta_set(*(other.removals_constant));
    number_changes = new epmem_id_num_delta_set(*(other.number_changes));
    myAgent = other.myAgent;
}
EpMem_Id_Delta::EpMem_Id_Delta(agent* someAgent)
{
    additions = new epmem_id_delta_set;
    removals = new epmem_id_delta_set;
    additions_constant = new epmem_id_delta_set;
    removals_constant = new epmem_id_delta_set;
    number_changes = new epmem_id_num_delta_set;
    myAgent = someAgent;
}
EpMem_Id_Delta::~EpMem_Id_Delta()
{
    bool null_present = false;
    null_present = (additions == NULL || removals == NULL || additions_constant == NULL || removals_constant == NULL || number_changes == NULL);
    assert( (!null_present) || (additions == NULL && removals == NULL && additions_constant == NULL && removals_constant == NULL && number_changes == NULL) );
    //If one is null, all better be null. This is from the move constructor.
    if (!null_present)
    {
        delete this->additions;
        delete this->removals;
        delete this->additions_constant;
        delete this->removals_constant;
        delete this->number_changes;
    }
}


EpMem_Manager::EpMem_Manager(agent* myAgent)
{
    thisAgent = myAgent;

    thisAgent->EpMem = this;

    // epmem initialization
    epmem_params = new epmem_param_container(thisAgent);
    epmem_stats = new epmem_stat_container(thisAgent);
    epmem_timers = new epmem_timer_container(thisAgent);

    epmem_db = new soar_module::sqlite_database();
    epmem_stmts_common = NULL;
    epmem_stmts_graph = NULL;

    epmem_node_mins = new std::vector<epmem_time_id>();
    epmem_node_maxes = new std::vector<bool>();

    epmem_edge_mins = new std::vector<epmem_time_id>();
    epmem_edge_maxes = new std::vector<bool>();
    epmem_id_repository = new epmem_parent_id_pool();
    epmem_id_replacement = new epmem_return_id_pool();
    epmem_id_ref_counts = new epmem_id_ref_counter();

#ifdef USE_MEM_POOL_ALLOCATORS
    epmem_node_removals = new epmem_id_removal_map(std::less< epmem_node_id >(), soar_module::soar_memory_pool_allocator< std::pair< std::pair<epmem_node_id const,int64_t> const, bool > >(thisAgent));
    epmem_edge_removals = new epmem_edge_removal_map(std::less< std::pair<epmem_node_id const,int64_t> >(), soar_module::soar_memory_pool_allocator< std::pair< epmem_node_id const, bool > >(thisAgent));
    epmem_wme_adds = new epmem_symbol_set(std::less< Symbol* >(), soar_module::soar_memory_pool_allocator< Symbol* >(thisAgent));
    epmem_id_removes = new epmem_symbol_stack(soar_module::soar_memory_pool_allocator< Symbol* >(thisAgent));
#else
    epmem_node_removals = new epmem_id_removal_map();
    epmem_edge_removals = new epmem_edge_removal_map();
    epmem_wme_adds = new epmem_symbol_set();
    epmem_id_removes = new epmem_symbol_stack();
#endif

    epmem_validation = 0;

    //The below three implement petrov approximation base-level, but that will be used for surprise.
    change_counter = new std::map<std::pair<bool,epmem_node_id>,uint64_t>();
    change_time_recent = new std::map<std::pair<bool,epmem_node_id>,uint64_t>();
    change_time_first = new std::map<std::pair<bool,epmem_node_id>,uint64_t>();

    float_change_counter = new std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>();
    float_change_time_recent = new std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>();
    float_change_time_first = new std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>();

    total_wme_changes = 0;
    no_immediately_previous_change = false;
    prev_delta = NULL;
    smem_connected = false;
};

void EpMem_Manager::clean_up_for_agent_deletion()
{
    /* This is not in destructor because it may be called before other
     * deletion code that may need params, stats or timers to exist */
    // cleanup exploration

    epmem_close(thisAgent);
    delete epmem_params;
    delete epmem_stats;
    delete epmem_timers;

    delete epmem_node_removals;
    delete epmem_node_mins;
    delete epmem_node_maxes;
    delete epmem_edge_removals;
    delete epmem_edge_mins;
    delete epmem_edge_maxes;
    delete epmem_id_repository;
    delete epmem_id_replacement;
    delete epmem_id_ref_counts;
    delete epmem_id_removes;

    delete epmem_wme_adds;
    delete change_counter;
    delete change_time_recent;
	delete change_time_first;
	delete float_change_counter;
    delete float_change_time_recent;
    delete float_change_time_first;

    delete epmem_db;
}
void epmem_param_container::print_settings(agent* thisAgent)
{
    std::string tempString;
    Output_Manager* outputManager = thisAgent->outputManager;

//    outputManager->reset_column_indents();
//    outputManager->set_column_indent(0, 25);
//    outputManager->set_column_indent(1, 58);
//    outputManager->printa(thisAgent, "=======================================================\n");
//    outputManager->printa(thisAgent, "-      Episodic Memory Sub-Commands and Options       -\n");
//    outputManager->printa(thisAgent, "=======================================================\n");
//    outputManager->printa_sf(thisAgent, "%s   %-\n", concatJustified("enabled",learning->get_string(), 55).c_str());
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("database", database->get_string(), 55).c_str(), "Whether to store database in memory or file");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("append", append_db->get_string(), 55).c_str(), "Whether to append or overwrite database");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("path", path->get_string(), 55).c_str(), "Path of database file");
//    outputManager->printa(thisAgent, "-------------------------------------------------------\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem", "[? | help]", 55).c_str(), "Print this help screen");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem", "[--enable | --disable ]", 55).c_str(), "Enable/disable semantic memory");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem [--get | --set] ","<option> [<value>]", 55).c_str(), "Print or set value of an SMem parameter");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --add","{ (id ^attr value)* }", 55).c_str(), "Add concepts to semantic memory");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --backup","<filename>", 55).c_str(), "Creates a backup of the semantic database on disk");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --init ","", 55).c_str(), "Reinitialize ALL memories");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --query ","{(<cue>*} [<num>]}", 55).c_str(), "Query for concepts in semantic store matching some cue");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --remove","{ (id [^attr [value]])* }", 55).c_str(), "Remove concepts from semantic memory");
//    outputManager->printa(thisAgent, "------------------------ Printing ---------------------\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("print","@", 55).c_str(), "Print semantic memory store");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("print","<LTI>", 55).c_str(), "Print specific semantic memory");
//    outputManager->printa(thisAgent, "---------------------- Activation --------------------\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --history","<LTI>", 55).c_str(), "Print activation history for some LTM");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("activation-mode", activation_mode->get_string(), 55).c_str(), "recency, frequency, base-level");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("activate-on-query", activate_on_query->get_string(), 55).c_str(), "on, off");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("base-decay", base_decay->get_string(), 55).c_str(), "Decay parameter for base-level activation computation");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("base-update-policy", base_update->get_string(), 55).c_str(), "stable, naive, incremental");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("base-incremental-threshes", base_incremental_threshes->get_string(), 55).c_str(), "integer > 0");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("thresh", thresh->get_string(), 55).c_str(), "integer >= 0");
//    outputManager->printa(thisAgent, "------------- Database Optimization Settings ----------\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("lazy-commit", lazy_commit->get_string(), 55).c_str(), "Delay writing semantic store until exit");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("optimization", opt->get_string(), 55).c_str(), "safety, performance");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("cache-size", cache_size->get_string(), 55).c_str(), "Number of memory pages used for SQLite cache");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("page-size", page_size->get_string(), 55).c_str(), "Size of each memory page used");
//    outputManager->printa(thisAgent, "----------------- Timers and Statistics ---------------\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("timers", timers->get_string(), 55).c_str(), "Timer granularity (off, one, two, three)");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --timers ","[<timer>]", 55).c_str(), "Print timer summary or specific statistic:");
//    outputManager->printa_sf(thisAgent, "%-%- (_total, smem_api, smem_hash, smem_init, smem_query)\n");
//    outputManager->printa_sf(thisAgent, "%-%- (smem_ncb_retrieval, smem_storage, three_activation)\n");
//    outputManager->printa_sf(thisAgent, "%s   %-%s\n", concatJustified("smem --stats","[<stat>]", 55).c_str(), "Print statistic summary or specific statistic:");
//    outputManager->printa_sf(thisAgent, "%-%- (act_updates, db-lib-version, edges, mem-usage)\n");
//    outputManager->printa_sf(thisAgent, "%-%- (mem-high, nodes, queries, retrieves, stores)\n");
//    outputManager->printa(thisAgent, "-------------------------------------------------------\n\n");
//    outputManager->printa_sf(thisAgent, "For a detailed explanation of these settings:  %-%- help output\n");

    // Print Epmem Settings
//    PrintCLIMessage_Header("Episodic Memory Settings", 40);
//    PrintCLIMessage_Item("learning:", thisAgent->EpMem->epmem_params->learning, 40);
//    PrintCLIMessage_Section("Encoding", 40);
//    PrintCLIMessage_Item("phase:", thisAgent->EpMem->epmem_params->phase, 40);
//    PrintCLIMessage_Item("trigger:", thisAgent->EpMem->epmem_params->trigger, 40);
//    PrintCLIMessage_Item("force:", thisAgent->EpMem->epmem_params->force, 40);
//    PrintCLIMessage_Item("exclusions:", thisAgent->EpMem->epmem_params->exclusions, 40);
//    PrintCLIMessage_Section("Storage", 40);
//    PrintCLIMessage_Item("database:", thisAgent->EpMem->epmem_params->database, 40);
//    PrintCLIMessage_Item("append:", thisAgent->EpMem->epmem_params->append_db, 40);
//    PrintCLIMessage_Item("path:", thisAgent->EpMem->epmem_params->path, 40);
//    PrintCLIMessage_Item("lazy-commit:", thisAgent->EpMem->epmem_params->lazy_commit, 40);
//    PrintCLIMessage_Section("Retrieval", 40);
//    PrintCLIMessage_Item("balance:", thisAgent->EpMem->epmem_params->balance, 40);
//    PrintCLIMessage_Item("graph-match:", thisAgent->EpMem->epmem_params->graph_match, 40);
//    PrintCLIMessage_Item("graph-match-ordering:", thisAgent->EpMem->epmem_params->gm_ordering, 40);
//    PrintCLIMessage_Section("Performance", 40);
//    PrintCLIMessage_Item("page-size:", thisAgent->EpMem->epmem_params->page_size, 40);
//    PrintCLIMessage_Item("cache-size:", thisAgent->EpMem->epmem_params->cache_size, 40);
//    PrintCLIMessage_Item("optimization:", thisAgent->EpMem->epmem_params->opt, 40);
//    PrintCLIMessage_Item("timers:", thisAgent->EpMem->epmem_params->timers, 40);
//    PrintCLIMessage_Section("Experimental", 40);
//    PrintCLIMessage_Item("merge:", thisAgent->EpMem->epmem_params->merge, 40);
//    PrintCLIMessage("");
}

void epmem_param_container::print_summary(agent* thisAgent)
{
    std::string tempString;
    Output_Manager* outputManager = thisAgent->outputManager;

    std::string lStr("Episodic memory is ");
    lStr.append(thisAgent->EpMem->epmem_params->learning->get_value() ? "enabled." : "not enabled.");
//    PrintCLIMessage(lStr.c_str());
//    PrintCLIMessage("Use 'epmem ?' to see EpMem setting and 'help epmem' to learn more about the epmem command.");
}
