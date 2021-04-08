/*************************************************************************
 * PLEASE SEE THE FILE "COPYING" (INCLUDED WITH THIS SOFTWARE PACKAGE)
 * FOR LICENSE AND COPYRIGHT INFORMATION.
 *************************************************************************/

/*************************************************************************
 *
 *  file:  episodic_memory.h
 *
 * =======================================================================
 */

#ifndef EPISODIC_MEMORY_H
#define EPISODIC_MEMORY_H

#include "kernel.h"

#include "soar_module.h"
#include "soar_db.h"

#include <map>
#include <list>
#include <stack>
#include <set>
#include <queue>

//////////////////////////////////////////////////////////
// EpMem Parameters
//////////////////////////////////////////////////////////
class EpMem_Id_Delta;//I'll wrap a timer around the code to maintain this thing -- it seems like it may be a nice kind of data structure to create each cycle, but maybe it's just not worth it.

class epmem_path_param;

class epmem_param_container: public soar_module::param_container
{
    public:

        // storage
        enum db_choices { memory, file };

        // encoding
        enum phase_choices { phase_output, phase_selection };
        enum trigger_choices { none, output, dc };
        enum force_choices { remember, ignore, force_off };

        // performance
        enum page_choices { page_1k, page_2k, page_4k, page_8k, page_16k, page_32k, page_64k };
        enum opt_choices { opt_safety, opt_speed };

        // experimental
        enum gm_ordering_choices { gm_order_undefined, gm_order_dfs, gm_order_mcv };
        enum merge_choices { merge_none, merge_add };

        // surprise
        enum surprise_method_choices { bla, hebbian };

        ////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////

        soar_module::boolean_param* learning;

        // encoding
        soar_module::constant_param<phase_choices>* phase;
        soar_module::constant_param<trigger_choices>* trigger;
        soar_module::constant_param<force_choices>* force;
        soar_module::sym_set_param* exclusions;

        // storage
        soar_module::constant_param<db_choices>* database;
        epmem_path_param* path;
        soar_module::boolean_param* lazy_commit;
        soar_module::boolean_param* append_db;

        // retrieval
        soar_module::boolean_param* graph_match;
        soar_module::decimal_param* balance;

        // performance
        soar_module::constant_param<page_choices>* page_size;
        soar_module::integer_param* cache_size;
        soar_module::constant_param<opt_choices>* opt;
        soar_module::constant_param<soar_module::timer::timer_level>* timers;

        // experimental
        soar_module::constant_param<gm_ordering_choices>* gm_ordering;
        soar_module::constant_param<merge_choices>* merge;

        // surprise
        soar_module::boolean_param* smem_surprise;
        soar_module::constant_param<surprise_method_choices>* surprise_method;
        soar_module::decimal_param* default_surprise_threshold;
        soar_module::sym_set_param* surprise_exclusions;//Not clear we really should want this to be distinct from exclusions in general, but I don't want to begin making theoretical claims about the sorts of things agents should be able to have as graph structures, such as "no floats"



        void print_settings(agent* thisAgent);
        void print_summary(agent* thisAgent);

        epmem_param_container(agent* new_agent);
};

class epmem_path_param: public soar_module::string_param
{
    protected:
        agent* thisAgent;

    public:
        epmem_path_param(const char* new_name, const char* new_value, soar_module::predicate<const char*>* new_val_pred, soar_module::predicate<const char*>* new_prot_pred, agent* new_agent);
        virtual void set_value(const char* new_value);
};

template <typename T>
class epmem_db_predicate: public soar_module::agent_predicate<T>
{
    public:
        epmem_db_predicate(agent* new_agent);
        bool operator()(T val);
};


//////////////////////////////////////////////////////////
// EpMem Statistics
//////////////////////////////////////////////////////////

typedef soar_module::primitive_stat<epmem_time_id> epmem_time_id_stat;
typedef soar_module::primitive_stat<epmem_node_id> epmem_node_id_stat;

class epmem_db_lib_version_stat;
class epmem_mem_usage_stat;
class epmem_mem_high_stat;

class epmem_stat_container: public soar_module::stat_container
{
    public:
        epmem_time_id_stat* time;
        epmem_db_lib_version_stat* db_lib_version;
        epmem_mem_usage_stat* mem_usage;
        epmem_mem_high_stat* mem_high;
        soar_module::integer_stat* ncbr;
        soar_module::integer_stat* cbr;
        soar_module::integer_stat* nexts;
        soar_module::integer_stat* prevs;
        soar_module::integer_stat* ncb_wmes;

        soar_module::integer_stat* qry_pos;
        soar_module::integer_stat* qry_neg;
        epmem_time_id_stat* qry_ret;
        soar_module::integer_stat* qry_card;
        soar_module::integer_stat* qry_lits;

        epmem_node_id_stat* next_id;

        soar_module::integer_stat* rit_offset_1;
        soar_module::integer_stat* rit_left_root_1;
        soar_module::integer_stat* rit_right_root_1;
        soar_module::integer_stat* rit_min_step_1;

        soar_module::integer_stat* rit_offset_2;
        soar_module::integer_stat* rit_left_root_2;
        soar_module::integer_stat* rit_right_root_2;
        soar_module::integer_stat* rit_min_step_2;

        epmem_stat_container(agent* thisAgent);
};

//

class epmem_db_lib_version_stat: public soar_module::primitive_stat< const char* >
{
    protected:
        agent* thisAgent;

    public:
        epmem_db_lib_version_stat(agent* new_agent, const char* new_name, const char* new_value, soar_module::predicate< const char* >* new_prot_pred);
        const char* get_value();
};

//

class epmem_mem_usage_stat: public soar_module::integer_stat
{
    protected:
        agent* thisAgent;

    public:
        epmem_mem_usage_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred);
        int64_t get_value();
};

//

class epmem_mem_high_stat: public soar_module::integer_stat
{
    protected:
        agent* thisAgent;

    public:
        epmem_mem_high_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred);
        int64_t get_value();
};


//////////////////////////////////////////////////////////
// EpMem Timers
//////////////////////////////////////////////////////////

class epmem_timer_container: public soar_module::timer_container
{
    public:
        soar_module::timer* total;
        soar_module::timer* storage;
        soar_module::timer* storage_store_levels;
        soar_module::timer* storage_insert_constants;
        soar_module::timer* storage_insert_ids;
        soar_module::timer* storage_remove_constants;
        soar_module::timer* storage_remove_ids;
        soar_module::timer* storage_do_surprise;
        soar_module::timer* storage_do_edge_updates;
        soar_module::timer* storage_do_edge_updates_1;
        soar_module::timer* storage_do_edge_updates_2;
        soar_module::timer* storage_do_edge_updates_3;
        soar_module::timer* storage_do_edge_updates_4;
        soar_module::timer* storage_do_edge_updates_4_1;
        soar_module::timer* storage_do_edge_updates_4_2;
        soar_module::timer* storage_do_edge_updates_4_2_1;
        soar_module::timer* storage_do_edge_updates_4_3;
        soar_module::timer* storage_do_edge_updates_5;
        soar_module::timer* storage_do_edge_updates_6;
        soar_module::timer* ncb_retrieval;
        soar_module::timer* query;
        soar_module::timer* api;
        soar_module::timer* trigger;
        soar_module::timer* init;
        soar_module::timer* next;
        soar_module::timer* prev;
        soar_module::timer* hash;
        soar_module::timer* wm_phase;

        soar_module::timer* ncb_edge;
        soar_module::timer* ncb_edge_rit;
        soar_module::timer* ncb_node;
        soar_module::timer* ncb_node_rit;

        soar_module::timer* query_dnf;
        soar_module::timer* query_walk;
        soar_module::timer* query_walk_edge;
        soar_module::timer* query_walk_interval;
        soar_module::timer* query_graph_match;
        soar_module::timer* query_result;
        soar_module::timer* query_cleanup;

        soar_module::timer* query_sql_edge;
        soar_module::timer* query_sql_start_ep;
        soar_module::timer* query_sql_start_now;
        soar_module::timer* query_sql_start_point;
        soar_module::timer* query_sql_end_ep;
        soar_module::timer* query_sql_end_now;
        soar_module::timer* query_sql_end_point;

        epmem_timer_container(agent* thisAgent);
};

class epmem_timer_level_predicate: public soar_module::agent_predicate<soar_module::timer::timer_level>
{
    public:
        epmem_timer_level_predicate(agent* new_agent);
        bool operator()(soar_module::timer::timer_level val);
};

class epmem_timer: public soar_module::timer
{
    public:
        epmem_timer(const char* new_name, agent* new_agent, timer_level new_level);
};


//////////////////////////////////////////////////////////
// EpMem Statements
//////////////////////////////////////////////////////////

class epmem_common_statement_container: public soar_module::sqlite_statement_container
{
    public:
        soar_module::sqlite_statement* begin;
        soar_module::sqlite_statement* commit;
        soar_module::sqlite_statement* rollback;

        soar_module::sqlite_statement* var_get;
        soar_module::sqlite_statement* var_set;

        soar_module::sqlite_statement* rit_add_left;
        soar_module::sqlite_statement* rit_truncate_left;
        soar_module::sqlite_statement* rit_add_right;
        soar_module::sqlite_statement* rit_truncate_right;

        soar_module::sqlite_statement* hash_rev_int;
        soar_module::sqlite_statement* hash_rev_float;
        soar_module::sqlite_statement* hash_rev_str;
        soar_module::sqlite_statement* hash_get_int;
        soar_module::sqlite_statement* hash_get_float;
        soar_module::sqlite_statement* hash_get_str;
        soar_module::sqlite_statement* hash_get_type;
        soar_module::sqlite_statement* hash_add_type;
        soar_module::sqlite_statement* hash_add_int;
        soar_module::sqlite_statement* hash_add_float;
        soar_module::sqlite_statement* hash_add_str;

        epmem_common_statement_container(agent* new_agent);

    private:

        void create_graph_tables();
        void drop_graph_tables();
        void create_graph_indices();

};

class epmem_graph_statement_container: public soar_module::sqlite_statement_container
{
    public:
        soar_module::sqlite_statement* add_node;
        soar_module::sqlite_statement* update_node;
        soar_module::sqlite_statement* add_time;
        soar_module::sqlite_statement* insert_potential_before_relations;
        soar_module::sqlite_statement* update_unobserved_before_relations;
        soar_module::sqlite_statement* update_observed_before_relations;
        soar_module::sqlite_statement* select_removed_nows;
        soar_module::sqlite_statement* delete_removed_nows;
        soar_module::sqlite_statement* select_new_nows;
        soar_module::sqlite_statement* count_old_nows;
        soar_module::sqlite_statement* select_updates;
        soar_module::sqlite_statement* select_new_relations;
        soar_module::sqlite_statement* update_smem_edges;
        soar_module::sqlite_statement* make_adds_into_ltis;
        soar_module::sqlite_statement* record_lti_id_for_w_id;
        soar_module::sqlite_statement* delete_brand_new_adds;
        soar_module::sqlite_statement* delete_brand_new_relation_adds;
        soar_module::sqlite_statement* finish_updates;

        //

        soar_module::sqlite_statement* add_interval_time;
        soar_module::sqlite_statement* get_interval_time;
        soar_module::sqlite_statement* add_interval_data;
        soar_module::sqlite_statement* update_interval_data;
        soar_module::sqlite_statement* update_interval_surprise;
        soar_module::sqlite_statement* find_now_interval;

        //

        soar_module::sqlite_statement* add_epmem_wmes;
        soar_module::sqlite_statement* add_epmem_wmes_constant_now;
        soar_module::sqlite_statement* find_time_epmem_wmes_float_now;
        soar_module::sqlite_statement* add_epmem_wmes_float_now;
        soar_module::sqlite_statement* delete_epmem_wmes_constant_now;
        soar_module::sqlite_statement* delete_epmem_wmes_float_now;
        soar_module::sqlite_statement* add_epmem_wmes_constant_point;
        soar_module::sqlite_statement* add_epmem_wmes_float_point;
        soar_module::sqlite_statement* add_epmem_wmes_constant_range;
        soar_module::sqlite_statement* add_epmem_wmes_float_range;

        soar_module::sqlite_statement* add_epmem_wmes_float;
        soar_module::sqlite_statement* add_epmem_wmes_constant;
        soar_module::sqlite_statement* find_epmem_wmes_float;
        soar_module::sqlite_statement* find_epmem_wmes_constant;

        //

        soar_module::sqlite_statement* add_epmem_wmes_identifier_now;
        soar_module::sqlite_statement* delete_epmem_wmes_identifier_now;
        soar_module::sqlite_statement* add_epmem_wmes_identifier_point;
        soar_module::sqlite_statement* add_epmem_wmes_identifier_range;

        soar_module::sqlite_statement* add_epmem_wmes_identifier;
        soar_module::sqlite_statement* find_epmem_wmes_identifier;
        soar_module::sqlite_statement* find_epmem_wmes_identifier_shared;

        //

        soar_module::sqlite_statement* valid_episode;
        soar_module::sqlite_statement* next_episode;
        //soar_module::sqlite_statement* next_query_episode;
        soar_module::sqlite_statement* prev_episode;

        soar_module::sqlite_statement* get_wmes_with_identifier_values;
        soar_module::sqlite_statement* get_wmes_with_constant_values;
        soar_module::sqlite_statement* get_single_wcid_info;
		soar_module::sqlite_statement* get_single_wiid_info;
		soar_module::sqlite_statement* get_constant;

//        //
//
//        soar_module::sqlite_statement* find_lti;
//        soar_module::sqlite_statement* find_lti_promotion_time;
//
        //

        soar_module::sqlite_statement* update_epmem_wmes_identifier_last_episode_id;

        //

        soar_module::sqlite_statement_pool* pool_find_edge_queries[2][2];
        soar_module::sqlite_statement_pool* pool_find_interval_queries[2][2][3];
//        soar_module::sqlite_statement_pool* pool_find_lti_queries[2][3];
        soar_module::sqlite_statement_pool* pool_dummy;

        //

        epmem_graph_statement_container(agent* new_agent);

    private:
        void create_graph_tables();
        void drop_graph_tables();
        void create_graph_indices();

};


//////////////////////////////////////////////////////////
// Common Types
//////////////////////////////////////////////////////////

// represents a vector of times
typedef std::vector<epmem_time_id> epmem_time_list;

// represents a list of wmes
typedef std::list<wme*> epmem_wme_list;

// keeping state for multiple RIT's
typedef struct epmem_rit_state_param_struct
{
    soar_module::integer_stat* stat;
    epmem_variable_key var_key;
} epmem_rit_state_param;

typedef struct epmem_rit_state_struct
{
    epmem_rit_state_param offset;
    epmem_rit_state_param leftroot;
    epmem_rit_state_param rightroot;
    epmem_rit_state_param minstep;

    soar_module::timer* timer;
    soar_module::sqlite_statement* add_query;
} epmem_rit_state;

//////////////////////////////////////////////////////////
// Soar Integration Types
//////////////////////////////////////////////////////////

// data associated with each state
typedef struct epmem_data_struct
{
    uint64_t last_ol_time;                                  // last update to output-link
    uint64_t last_ol_count;                                 // last count of output-link

    uint64_t last_cmd_time;                                 // last update to epmem.command
    uint64_t last_cmd_count;                                // last update to epmem.command

    epmem_time_id last_memory;                              // last retrieved memory

    wme* epmem_link_wme;
    wme* cmd_wme;
    wme* result_wme;
    wme* epmem_time_wme;

    preference_list* epmem_wmes;                            // preferences generated in last epmem
} epmem_data;

// lookup tables to facilitate shared identifiers
typedef std::map<epmem_node_id, Symbol*> epmem_id_mapping;

// types/structures to facilitate re-use of identifiers
typedef std::pair<epmem_node_id, epmem_node_id> epmem_id_pair;
typedef std::list<epmem_id_pair> epmem_id_pool;
typedef std::map<epmem_node_id, epmem_id_pool*> epmem_hashed_id_pool;
typedef std::map<epmem_node_id, epmem_hashed_id_pool*> epmem_parent_id_pool;
typedef std::map<epmem_node_id, epmem_id_pool*> epmem_return_id_pool;

#ifdef USE_MEM_POOL_ALLOCATORS
typedef std::set< wme*, std::less< wme* >, soar_module::soar_memory_pool_allocator< wme* > > epmem_wme_set;
typedef std::list< Symbol*, soar_module::soar_memory_pool_allocator< Symbol* > > epmem_symbol_stack;

// types/structures to facilitate incremental storage
typedef std::map< epmem_node_id, bool, std::less< epmem_node_id >, soar_module::soar_memory_pool_allocator< std::pair< epmem_node_id const, bool > > > epmem_id_removal_map;
typedef std::map< std::pair<epmem_node_id const,int64_t>, bool, std::less< std::pair<epmem_node_id const,int64_t> >, soar_module::soar_memory_pool_allocator< std::pair< std::pair<epmem_node_id const,int64_t>  const, bool > > > epmem_edge_removal_map;
typedef std::set< Symbol*, std::less< Symbol* >, soar_module::soar_memory_pool_allocator< Symbol* > > epmem_symbol_set;

#else
typedef std::set< wme* > epmem_wme_set;
typedef std::list< Symbol* > epmem_symbol_stack;

// types/structures to facilitate incremental storage
typedef std::map<epmem_node_id, bool> epmem_id_removal_map;
typedef std::map< std::pair<epmem_node_id const,int64_t>, bool> epmem_edge_removal_map;
typedef std::set< Symbol* > epmem_symbol_set;
#endif
typedef std::map<epmem_node_id, epmem_wme_set*> epmem_id_ref_counter;

typedef struct epmem_id_reservation_struct
{
    epmem_node_id my_id;
    epmem_hash_id my_hash;
    epmem_id_pool* my_pool;
} epmem_id_reservation;

// represents a graph edge (i.e. identifier)
// follows cs theory notation of finite automata: q1 = d( q0, w )
typedef struct epmem_edge_struct
{

    epmem_node_id   parent_n_id;
    Symbol*         attribute;
    epmem_node_id   child_n_id;
    epmem_node_id   triple_id;
    uint64_t        child_lti_id;

} epmem_edge;

//////////////////////////////////////////////////////////
// Parameter Functions (see cpp for comments)
//////////////////////////////////////////////////////////

// shortcut for determining if EpMem is enabled
extern bool epmem_enabled(agent* thisAgent);

//////////////////////////////////////////////////////////
// Soar Functions (see cpp for comments)
//////////////////////////////////////////////////////////

// init, end
extern void epmem_attach(agent* thisAgent);
extern void epmem_reset(agent* thisAgent, Symbol* state = NULL);
extern void epmem_close(agent* thisAgent);
extern void epmem_reinit(agent* thisAgent);
extern void epmem_reinit_cmd(agent* thisAgent);

extern void epmem_clear_transient_structures(agent* thisAgent);

// perform epmem actions
extern void epmem_go(agent* thisAgent, bool allow_store = true);
extern bool epmem_backup_db(agent* thisAgent, const char* file_name, std::string* err);
extern void epmem_init_db(agent* thisAgent, bool readonly = false);
// visualization
extern void epmem_visualize_episode(agent* thisAgent, epmem_time_id memory_id, std::string* buf);
extern void epmem_print_episode(agent* thisAgent, epmem_time_id memory_id, std::string* buf);

extern epmem_hash_id epmem_temporal_hash(agent* thisAgent, Symbol* sym, bool add_on_fail = true);
//////////////////////////////////////////////////////////
// Episodic Memory Search
//////////////////////////////////////////////////////////

// defined below
typedef struct epmem_triple_struct epmem_triple;
typedef struct epmem_literal_struct epmem_literal;
typedef struct epmem_pedge_struct epmem_pedge;
typedef struct epmem_uedge_struct epmem_uedge;
typedef struct epmem_interval_struct epmem_interval;
typedef struct epmem_temporal_interval_relation_literal_struct epmem_temporal_literal;
typedef struct epmem_ongoing_match_struct epmem_ongoing_match;

// pairs
typedef struct std::pair<Symbol*, epmem_literal*> epmem_symbol_literal_pair;
typedef struct std::pair<Symbol*, epmem_node_id> epmem_symbol_node_pair;
typedef struct std::pair<epmem_literal*, epmem_node_id> epmem_literal_node_pair;
typedef struct std::pair<epmem_node_id, epmem_node_id> epmem_node_pair;

// collection classes
typedef std::deque<epmem_literal*> epmem_literal_deque;
typedef std::deque<epmem_node_id> epmem_node_deque;
typedef std::map<Symbol*, int> epmem_symbol_int_map;
typedef std::map<epmem_literal*, epmem_node_pair> epmem_literal_node_pair_map;
typedef std::map<epmem_literal_node_pair, int> epmem_literal_node_pair_int_map;
typedef std::map<epmem_node_id, Symbol*> epmem_node_symbol_map;
typedef std::map<epmem_node_id, int> epmem_node_int_map;
typedef std::map<epmem_symbol_literal_pair, int> epmem_symbol_literal_pair_int_map;
typedef std::map<epmem_symbol_node_pair, int> epmem_symbol_node_pair_int_map;
typedef std::map<epmem_triple, epmem_pedge*> epmem_triple_pedge_map;
typedef std::map<wme*, epmem_literal*> epmem_wme_literal_map;
typedef std::set<epmem_literal*> epmem_literal_set;
typedef std::set<epmem_pedge*> epmem_pedge_set;

#ifdef USE_MEM_POOL_ALLOCATORS
typedef std::map<epmem_triple, epmem_uedge*, std::less<epmem_triple>, soar_module::soar_memory_pool_allocator<std::pair<epmem_triple const, epmem_uedge*> > > epmem_triple_uedge_map;
typedef std::set<epmem_interval*, std::less<epmem_interval*>, soar_module::soar_memory_pool_allocator<epmem_interval*> > epmem_interval_set;
typedef std::set<epmem_node_id, std::less<epmem_node_id>, soar_module::soar_memory_pool_allocator<epmem_node_id> > epmem_node_set;
typedef std::set<epmem_node_pair, std::less<epmem_node_pair>, soar_module::soar_memory_pool_allocator<epmem_node_pair> > epmem_node_pair_set;
#else
typedef std::map<epmem_triple, epmem_uedge*> epmem_triple_uedge_map;
typedef std::set<epmem_interval*> epmem_interval_set;
typedef std::set<epmem_node_id> epmem_node_set;
typedef std::set<epmem_node_pair> epmem_node_pair_set;
#endif

// structs
struct epmem_triple_struct
{
    epmem_node_id parent_n_id;
    epmem_node_id attribute_s_id;
    epmem_node_id child_n_id;
    bool operator<(const epmem_triple& other) const
    {
        if (parent_n_id != other.parent_n_id)
        {
            return (parent_n_id < other.parent_n_id);
        }
        else if (attribute_s_id != other.attribute_s_id)
        {
            return (attribute_s_id < other.attribute_s_id);
        }
        else
        {
            return (child_n_id < other.child_n_id);
        }
    }
};

struct epmem_literal_struct
{
    Symbol* id_sym;
    Symbol* value_sym;//the symbol on the cue matching to this literal. (e.g. state root to the positive cue root, positive cue root is value_sym)
    int is_neg_q;
    int value_is_id;//basically a boolean. 1 if yes. ("1" = EPMEM_RIT_STATE_EDGE) //all constants are leaves, but some identifiers are leaves as well.
    bool is_leaf;
    epmem_node_id attribute_s_id;
    epmem_node_id child_n_id; //  -- is set to -1 (epmem_nodeid_bad) if an identifier. if a typical value -- double check - i think i'm wrong.
    //(not to be confused with leaf, as leaf is a superset of value, since an identifier can be a leaf), then this is set to the epmem hash for that value (constant).
    double weight;
    epmem_literal_set parents;
    epmem_literal_set children;
    epmem_node_pair_set matches;//which pedges could satisfy this?
    epmem_node_int_map values;
    /*epmem_literal_set befores;//the literals that are supposed to come (directly) before this one.//when this literal becomes unsatisfied, if there are befores,
    //then the score doesn't actually decrease if those befores are satisfied on the -1 cycle or already satisfied.
    //when a literal is already satisfied, but it is supposed to be before another literal, is bad (even though it's "satisfied")
    epmem_literal_set afters;*/
    Symbol* cue;//nice to know when a literal is satisfied, which cue it's satisfying.
};

struct epmem_temporal_interval_relation_literal_struct
{//a binding should be associated with a the left or right of a literal.
    Symbol* value_sym_1;//the cue root for this temporal relation literal
    bool is_exists;//doesn't bind to any explicitly-supplied temporal relation constraints
    Symbol* value_sym_2;//the other cue root for this temporal relation literal//will be null for the is_exists literals.
    int interval_1_left;
    int interval_1_right;
    int interval_2_left;//todo lol, i think i'm off-by-one on all of the lefts throughout the interval matching function.
    int interval_2_right;
    //epmem_literal_node_pair_map* binding_1;
    //epmem_literal_node_pair_map* binding_2;
    bool satisfied;
    //int just_deleted;//which symbol only just no longer matches?
    int type;
    epmem_ongoing_match* match_using_this;
};//epmem_temporal_literal

struct epmem_ongoing_match_struct
{//an "ongoing match" is when at least one cue has a particular binding to an interval during which that cue is present.
    std::map<Symbol*,epmem_literal_node_pair_map*> best_bindings;//for a given cue, the bindings for a graph match.
    //std::map<Symbol*,int> individual_cue_scores;
    int match_score;// When this is equal to the number of temporal relations to satisfy, we did it!
    //std::map<Symbol*,std::pair<uint64_t,uint64_t>>* cues_to_satisfying_intervals;
    int match_end;//filled in first, the temporally last timestep of this match.
    int match_start;//filled in last, the temporally first timestep of this match.
    std::multimap<Symbol*,epmem_temporal_literal*> cues_to_temporal_literals;
    std::set<Symbol*> unbound_cues;
    std::set<epmem_temporal_literal*> temporal_literals;
};//maybe "episodic memory" is when we become able to conceive of and directly represent the passage of time declaratively as just another relation in semantic memory.


struct epmem_pedge_struct
{
    epmem_triple triple;//what part of the WMG does this correspond to?
    int value_is_id;//is the value an identifier?
    epmem_literal_set literals;//what aspects of the cue structure does this potentially correspond to?
    soar_module::pooled_sqlite_statement* sql;//sql statements asking for the last_episode_id and w*_id
    epmem_time_id time;
    Symbol* cue;//unused in typical process_query usage, only to keep track of which cue// as long as all of the literals keep track of things, may not need this.
};

struct epmem_uedge_struct
{
    epmem_triple triple;//what part of the WMG does this correspond to?
    int value_is_id;//is the value an identifier?
    int activation_count;//How many pedges are currently active (present at the current timestep being swept) using this?
    epmem_pedge_set pedges;//Which pedges use this?
    int intervals;//goes up when the first interval shows up, doesn't go back down until last interval goes away (start time of first interval in relevant history)
    bool activated;//not sure, but I think: activation_count, but bool
};

struct epmem_interval_struct
{
    epmem_uedge* uedge;//a given temporal interval is associated with a given uedge -- this struct is used as "current interval"
    int is_end_point;//the end_episode_id, so it tells you (when sweeping back) when a given uedge's triple becomes present in memory.
    soar_module::pooled_sqlite_statement* sql;//look for the next interval and make it current by changing time
    epmem_time_id time;//the time at which this interval expires
};

// priority queues and comparison functions
struct epmem_pedge_comparator
{
    bool operator()(const epmem_pedge* a, const epmem_pedge* b) const
    {
        if (a->time != b->time)
        {
            return (a->time < b->time);
        }
        else
        {
            return (a < b);
        }
    }
};
typedef std::priority_queue<epmem_pedge*, std::vector<epmem_pedge*>, epmem_pedge_comparator> epmem_pedge_pq;
struct epmem_interval_comparator
{
    bool operator()(const epmem_interval* a, const epmem_interval* b) const
    {
        if (a->time != b->time)
        {
            return (a->time < b->time);
        }
        else if (a->is_end_point == b->is_end_point)
        {
            return (a < b);
        }
        else
        {
            // put ends before starts so intervals are closed first
            return (a->is_end_point == EPMEM_RANGE_END);
        }
    }
};

typedef std::priority_queue<epmem_interval*, std::vector<epmem_interval*>, epmem_interval_comparator> epmem_interval_pq;

typedef std::set<int64_t> epmem_id_delta_set;//These should probably be int64_t's given sqlite's inherent int64_t limitation.
typedef std::set<std::pair<std::pair<int64_t,int64_t>, bool>> epmem_id_num_delta_set;//Keeping same-address number changes as sign-of-the-delta-only.
//Purely for consistency, I may make two sets, one for greater-than, one for less-than. (Equality would be intentionally omitted.)

//One of these is created every epmem timestep.
class EpMem_Id_Delta
{
    public:
        EpMem_Id_Delta(agent* myAgent);
        EpMem_Id_Delta(EpMem_Id_Delta&& other);
        EpMem_Id_Delta(const EpMem_Id_Delta& other);
        ~EpMem_Id_Delta();
        void add_addition(int64_t);
        void add_removal(int64_t);
        void add_addition_constant(int64_t);
        void add_removal_constant(int64_t);
        void add_number_change(int64_t,int64_t, bool);
        bool operator==(const EpMem_Id_Delta &other) const;
        bool operator!=(const EpMem_Id_Delta &other) const;
        EpMem_Id_Delta operator+(const EpMem_Id_Delta &other) const;
        std::size_t hash() const;
        epmem_id_delta_set::const_iterator additions_begin() const;
        epmem_id_delta_set::const_iterator removals_begin() const;
        epmem_id_delta_set::const_iterator additions_end() const;
        epmem_id_delta_set::const_iterator removals_end() const;
        epmem_id_num_delta_set::const_iterator number_changes_begin() const;
        epmem_id_num_delta_set::const_iterator number_changes_end() const;
        epmem_id_delta_set::const_iterator additions_constant_begin() const;
        epmem_id_delta_set::const_iterator removals_constant_begin() const;
        epmem_id_delta_set::const_iterator additions_constant_end() const;
        epmem_id_delta_set::const_iterator removals_constant_end() const;
        uint64_t additions_size() const;
        uint64_t removals_size() const;
        uint64_t additions_constant_size() const;
        uint64_t removals_constant_size() const;
        uint64_t number_changes_size() const;
        epmem_id_num_delta_set::const_iterator number_changes_find(int64_t parent, int64_t attr, bool value_change) const;
        friend std::ostream& operator<< (std::ostream &out, const EpMem_Id_Delta &delta);
    private:
        epmem_id_delta_set* additions;
        epmem_id_delta_set* removals;
        epmem_id_delta_set* additions_constant;
        epmem_id_delta_set* removals_constant;
        epmem_id_num_delta_set* number_changes;
        agent* myAgent;
};

class EpMem_Manager
{
    public:
        EpMem_Manager(agent* myAgent);
        ~EpMem_Manager() {};

        void clean_up_for_agent_deletion();

        // epmem
        epmem_param_container* epmem_params;
        epmem_stat_container* epmem_stats;
        epmem_timer_container* epmem_timers;

        soar_module::sqlite_database* epmem_db;
        epmem_common_statement_container* epmem_stmts_common;
        epmem_graph_statement_container* epmem_stmts_graph;

        epmem_id_removal_map* epmem_node_removals;
        std::vector<epmem_time_id>* epmem_node_mins;
        std::vector<bool>* epmem_node_maxes;

        epmem_edge_removal_map* epmem_edge_removals;
        std::vector<epmem_time_id>* epmem_edge_mins;
        std::vector<bool>* epmem_edge_maxes;

        epmem_parent_id_pool* epmem_id_repository;
        epmem_return_id_pool* epmem_id_replacement;
        epmem_id_ref_counter* epmem_id_ref_counts;
        epmem_symbol_stack* epmem_id_removes;

        std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>* float_change_counter;//just keeping track of how often something has changed and the most recent change time.
		std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>* float_change_time_recent;
		std::map<std::pair<epmem_node_id,epmem_hash_id>,uint64_t>* float_change_time_first;
		//The above three give a petrov approx bla for float intervals. the below three do the same for everything else.
		std::map<std::pair<bool,epmem_node_id>,uint64_t>* change_counter;//just keeping track of how often something has changed and the most recent change time.
		std::map<std::pair<bool,epmem_node_id>,uint64_t>* change_time_recent;
		std::map<std::pair<bool,epmem_node_id>,uint64_t>* change_time_first;

		//std::unordered_map<uint64_t>* nows;//do all additions first. Then do removals, but keep track of them.
		//when something exits, can make it a potential "before left" w.r.t. all "nows"//can do by updating all things that are leaving first, removing them, but keeping them in a cache/set/list/whatever
		//then, loop through them again, double-for, making new before-relations to existing nows (ones that weren't removed.)
		//std::map<std::pair<uint64_t,uint64_t>, std::pair<bool,bool>>* potential_relations;//When the second in a pair is finished, time to update relation metadata and clarify relation.
		//on insert, can add to nows table.
		//on update, move to temp location
		//then batch through temp location. (sqlite query based on join)
		//surprise based on existing metadata, (note, surprise distinct from amount of later update) -- object permanence is learned... is it?, or else i'd care more about the notion that there has to be something else that "causes the negation".
		//

        uint64_t total_wme_changes;

        epmem_symbol_set* epmem_wme_adds;

        epmem_rit_state epmem_rit_state_graph[2];

        uint64_t epmem_validation;

        EpMem_Id_Delta* prev_delta;

        std::map<std::pair<int64_t,int64_t>, double> val_at_last_change;
		std::map<std::pair<int64_t,int64_t>, bool> change_at_last_change;
		bool no_immediately_previous_change;
		bool smem_connected;


    private:

        agent* thisAgent;

};

// suppress long "decorated name length exceeded" warning;
// applies for the rest of the TU, which is where templates are expanded
// and the warning is generated.
#ifdef _MSC_VER
  #pragma warning(disable : 4503)
#endif

#endif // EPISODIC_MEMORY_H
