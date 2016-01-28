/*************************************************************************
 * PLEASE SEE THE FILE "COPYING" (INCLUDED WITH THIS SOFTWARE PACKAGE)
 * FOR LICENSE AND COPYRIGHT INFORMATION.
 *************************************************************************/

/*************************************************************************
 *
 *  file:  semantic_memory.h
 *
 * =======================================================================
 */

#ifndef SEMANTIC_MEMORY_H
#define SEMANTIC_MEMORY_H

#include "portability.h"

#include <stack>
#include <set>
#include <list>
#include <vector>
#include <queue>

#include "soar_module.h"
#include "soar_db.h"

#include "semantic_memory_math_queries.h"

//////////////////////////////////////////////////////////
// SMem Experimentation
//
// If defined, we hijack the main SMem function
// for tight-loop experimentation/timing.
//
//////////////////////////////////////////////////////////

//#define SMEM_EXPERIMENT


//////////////////////////////////////////////////////////
// SMem Parameters
//////////////////////////////////////////////////////////

class smem_path_param;

class smem_param_container: public soar_module::param_container
{
    public:
        // This determines what math is done to combine the spreading and base-level components when using the likelihood probability model.
        enum spreading_types { ppr, actr };
        // The network traversal used to calculate spread can go along edges, reverse edge direction, or treat all edges as bidirectional.
        enum spreading_directions { forwards, backwards, both };
        // Precalculate - The calculation of a fingerprint is done when spreading is turned on.
            // When the long-term store changes, the fingerprint will need to be recalculated. This is done at query-time.
        // query-time - The calculation of a fingerprint isn't done until query-time.
            // When the long-term store changes, nothing is done.
        // context-change - The calculation of a fingerprint is done when a LTI enters working memory.
            // When the long-term store changes, nothing is done.
        enum spreading_times { query_time, context_change };

        enum spreading_crawl_times { on_demand, precalculate };

        // Currently, only the likelihood option is supported. The way spreading is calculated is only as an additional number combined with BLA
        // The belief-update model would modify the base-level activation of nodes according to spread isntead of keeping the numbers separate.
            //For example, when using context-change spreading, a LTI no longer in working memory (because it was recently removed) may still
            // have an effect on cued-retrieval (queries) because it modified the base-level activation of other elements and that has yet to decay away enough.
        enum spreading_models { likelihood, belief_update };
        // Random walks versus limited breadth-first traversal
        enum spreading_traversals { random, deterministic };
        enum db_choices { memory, file };
        enum cache_choices { cache_S, cache_M, cache_L };
        enum page_choices { page_1k, page_2k, page_4k, page_8k, page_16k, page_32k, page_64k };
        enum opt_choices { opt_safety, opt_speed };
        
        enum merge_choices { merge_none, merge_add };
        enum act_choices { act_recency, act_frequency, act_base };

        soar_module::boolean_param* learning;
        soar_module::boolean_param* spreading;//clearly, for spreading.
        soar_module::boolean_param* spontaneous_retrieval;
        soar_module::boolean_param* spreading_normalization;
        soar_module::constant_param<db_choices>* database;
        soar_module::constant_param<spreading_types>* spreading_type;
        soar_module::constant_param<spreading_directions>* spreading_direction;
        soar_module::constant_param<spreading_times>* spreading_time;
        soar_module::constant_param<spreading_crawl_times>* spreading_crawl_time;
        soar_module::constant_param<spreading_models>* spreading_model;
        soar_module::constant_param<spreading_traversals>* spreading_traversal;
        smem_path_param* path;
        soar_module::boolean_param* lazy_commit;
        soar_module::boolean_param* append_db;

        soar_module::constant_param<soar_module::timer::timer_level>* timers;

        soar_module::constant_param<page_choices>* page_size;
        soar_module::integer_param* cache_size;
        soar_module::constant_param<opt_choices>* opt;

        soar_module::integer_param* thresh;

        soar_module::decimal_param* number_trajectories;
        soar_module::constant_param<merge_choices>* merge;
        soar_module::boolean_param* activate_on_query;
        soar_module::boolean_param* activate_on_add;
        soar_module::constant_param<act_choices>* activation_mode;
        soar_module::decimal_param* base_decay;

        soar_module::decimal_param* spreading_limit;
        soar_module::decimal_param* spreading_depth_limit;
        soar_module::decimal_param* spreading_baseline;
        soar_module::decimal_param* continue_probability;
        soar_module::boolean_param* spreading_loop_avoidance;
        enum base_update_choices { bupt_stable, bupt_naive, bupt_incremental };
        soar_module::constant_param<base_update_choices>* base_update;

        soar_module::int_set_param* base_incremental_threshes;

        soar_module::boolean_param* mirroring;

        smem_param_container(agent* new_agent);
};
class smem_path_param: public soar_module::string_param

{
    protected:
        agent* thisAgent;
        
    public:
        smem_path_param(const char* new_name, const char* new_value, soar_module::predicate<const char*>* new_val_pred, soar_module::predicate<const char*>* new_prot_pred, agent* new_agent);
        virtual void set_value(const char* new_value);
};

template <typename T>
class smem_db_predicate: public soar_module::agent_predicate<T>
{
    public:
        smem_db_predicate(agent* new_agent);
        bool operator()(T val);
};


//////////////////////////////////////////////////////////
// SMem Statistics
//////////////////////////////////////////////////////////

class smem_db_lib_version_stat;
class smem_mem_usage_stat;
class smem_mem_high_stat;

class smem_stat_container: public soar_module::stat_container
{
    public:
        smem_db_lib_version_stat* db_lib_version;
        smem_mem_usage_stat* mem_usage;
        smem_mem_high_stat* mem_high;

        soar_module::integer_stat* expansions;
        soar_module::integer_stat* cbr;
        soar_module::integer_stat* stores;
        soar_module::integer_stat* act_updates;
        soar_module::integer_stat* mirrors;

        soar_module::integer_stat* chunks;
        soar_module::integer_stat* slots;

        smem_stat_container(agent* thisAgent);
};

//

class smem_db_lib_version_stat: public soar_module::primitive_stat< const char* >
{
    protected:
        agent* thisAgent;

    public:
        smem_db_lib_version_stat(agent* new_agent, const char* new_name, const char* new_value, soar_module::predicate< const char* >* new_prot_pred);
        const char* get_value();
};

//

class smem_mem_usage_stat: public soar_module::integer_stat
{
    protected:
        agent* thisAgent;

    public:
        smem_mem_usage_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred);
        int64_t get_value();
};

//

class smem_mem_high_stat: public soar_module::integer_stat
{
    protected:
        agent* thisAgent;

    public:
        smem_mem_high_stat(agent* new_agent, const char* new_name, int64_t new_value, soar_module::predicate<int64_t>* new_prot_pred);
        int64_t get_value();
};


//////////////////////////////////////////////////////////
// SMem Timers
//////////////////////////////////////////////////////////

class smem_timer_container: public soar_module::timer_container
{
    public:
        soar_module::timer* total;
        soar_module::timer* storage;
        soar_module::timer* ncb_retrieval;
        soar_module::timer* query;
        soar_module::timer* api;
        soar_module::timer* init;
        soar_module::timer* hash;
        soar_module::timer* act;
        soar_module::timer* spreading_act;
        soar_module::timer* spontaneous_retrieval;
        soar_module::timer* spontaneous_retrieval_1;
        soar_module::timer* spontaneous_retrieval_2;
        soar_module::timer* spreading_fix_1;
        soar_module::timer* spreading_fix_1_1;
        soar_module::timer* spreading_fix_1_1_1;
        soar_module::timer* spreading_fix_1_1_2;
        soar_module::timer* spreading_fix_1_2;
        soar_module::timer* spreading_fix_2;
        soar_module::timer* spreading_calc_1;
        soar_module::timer* spreading_calc_2;
        soar_module::timer* spreading_calc_2_1;
        soar_module::timer* spreading_calc_2_2;
        soar_module::timer* spreading_calc_2_2_1;
        soar_module::timer* spreading_calc_2_2_2;
        soar_module::timer* spreading_calc_2_2_3;
        soar_module::timer* spreading_store_1;
        soar_module::timer* spreading_store_2;
        soar_module::timer* spreading_store_3;
        soar_module::timer* spreading_store_3_1;
        soar_module::timer* spreading_store_3_2;
        soar_module::timer* spreading_store_3_2_1;
        soar_module::timer* spreading_store_3_2_2;
        soar_module::timer* spreading_store_4;

        smem_timer_container(agent* thisAgent);
};

class smem_timer_level_predicate: public soar_module::agent_predicate<soar_module::timer::timer_level>
{
    public:
        smem_timer_level_predicate(agent* new_agent);
        bool operator()(soar_module::timer::timer_level val);
};

class smem_timer: public soar_module::timer
{
    public:
        smem_timer(const char* new_name, agent* new_agent, soar_module::timer::timer_level new_level);
};


//////////////////////////////////////////////////////////
// SMem Statements
//////////////////////////////////////////////////////////

class smem_statement_container: public soar_module::sqlite_statement_container
{
    public:
        soar_module::sqlite_statement* begin;
        soar_module::sqlite_statement* commit;
        soar_module::sqlite_statement* rollback;

        soar_module::sqlite_statement* var_get;
        soar_module::sqlite_statement* var_set;
        soar_module::sqlite_statement* var_create;

        soar_module::sqlite_statement* hash_rev_int;
        soar_module::sqlite_statement* hash_rev_float;
        soar_module::sqlite_statement* hash_rev_str;
        soar_module::sqlite_statement* hash_rev_type;
        soar_module::sqlite_statement* hash_get_int;
        soar_module::sqlite_statement* hash_get_float;
        soar_module::sqlite_statement* hash_get_str;
        soar_module::sqlite_statement* hash_add_type;
        soar_module::sqlite_statement* hash_add_int;
        soar_module::sqlite_statement* hash_add_float;
        soar_module::sqlite_statement* hash_add_str;

        soar_module::sqlite_statement* lti_add;
        soar_module::sqlite_statement* lti_get;
        soar_module::sqlite_statement* lti_letter_num;
        soar_module::sqlite_statement* lti_max;
        soar_module::sqlite_statement* lti_access_get;
        soar_module::sqlite_statement* lti_access_set;
        soar_module::sqlite_statement* lti_get_t;

        soar_module::sqlite_statement* web_add;
        soar_module::sqlite_statement* web_truncate;
        soar_module::sqlite_statement* web_expand;

        soar_module::sqlite_statement* web_all;

        soar_module::sqlite_statement* web_attr_all;
        soar_module::sqlite_statement* web_const_all;
        soar_module::sqlite_statement* web_lti_all;

        soar_module::sqlite_statement* web_attr_child;
        soar_module::sqlite_statement* web_const_child;
        soar_module::sqlite_statement* web_lti_child;

        soar_module::sqlite_statement* attribute_frequency_check;
        soar_module::sqlite_statement* wmes_constant_frequency_check;
        soar_module::sqlite_statement* wmes_lti_frequency_check;

        soar_module::sqlite_statement* attribute_frequency_add;
        soar_module::sqlite_statement* wmes_constant_frequency_add;
        soar_module::sqlite_statement* wmes_lti_frequency_add;

        soar_module::sqlite_statement* attribute_frequency_update;
        soar_module::sqlite_statement* wmes_constant_frequency_update;
        soar_module::sqlite_statement* wmes_lti_frequency_update;

        soar_module::sqlite_statement* attribute_frequency_get;
        soar_module::sqlite_statement* wmes_constant_frequency_get;
        soar_module::sqlite_statement* wmes_lti_frequency_get;

        soar_module::sqlite_statement* act_set;
        soar_module::sqlite_statement* act_lti_child_ct_set;
        soar_module::sqlite_statement* act_lti_child_ct_get;
        soar_module::sqlite_statement* act_lti_set;
        soar_module::sqlite_statement* act_lti_get;

        soar_module::sqlite_statement* history_get;
        soar_module::sqlite_statement* history_push;
        soar_module::sqlite_statement* history_add;

        soar_module::sqlite_statement* prohibit_set;
        soar_module::sqlite_statement* prohibit_add;
        soar_module::sqlite_statement* prohibit_check;
        soar_module::sqlite_statement* prohibit_reset;
        soar_module::sqlite_statement* prohibit_clean;
        soar_module::sqlite_statement* prohibit_remove;
        soar_module::sqlite_statement* history_remove;
        soar_module::sqlite_statement* vis_lti;
        soar_module::sqlite_statement* vis_lti_act;
        soar_module::sqlite_statement* vis_act;
        soar_module::sqlite_statement* vis_value_const;
        soar_module::sqlite_statement* vis_value_lti;

        soar_module::sqlite_statement* lti_get_high_act;

        
        //The ones below are for spreading
        soar_module::sqlite_statement* web_val_parent;
        soar_module::sqlite_statement* web_val_parent_2;
        soar_module::sqlite_statement* web_val_child;
        soar_module::sqlite_statement* web_val_both;
        soar_module::sqlite_statement* lti_all;
        soar_module::sqlite_statement* trajectory_add;
        soar_module::sqlite_statement* trajectory_remove;
        soar_module::sqlite_statement* trajectory_remove_lti;
        soar_module::sqlite_statement* trajectory_check_invalid;
        soar_module::sqlite_statement* trajectory_remove_invalid;
        soar_module::sqlite_statement* trajectory_remove_all;
        soar_module::sqlite_statement* trajectory_find_invalid;
        soar_module::sqlite_statement* trajectory_get;
        soar_module::sqlite_statement* trajectory_invalidate_from_lti;
        soar_module::sqlite_statement* trajectory_invalidate_edge;
        soar_module::sqlite_statement* trajectory_size_debug_cmd;

        soar_module::sqlite_statement* likelihood_cond_count_remove;
        soar_module::sqlite_statement* lti_count_num_appearances_remove;
        soar_module::sqlite_statement* likelihood_cond_count_insert;
        soar_module::sqlite_statement* likelihood_cond_count_insert_deterministic;
        soar_module::sqlite_statement* lti_count_num_appearances_insert;
        soar_module::sqlite_statement* calc_spread;
        soar_module::sqlite_statement* calc_spread_size_debug_cmd;
        soar_module::sqlite_statement* delete_old_context;
        soar_module::sqlite_statement* delete_old_spread;
        soar_module::sqlite_statement* add_new_context;
        soar_module::sqlite_statement* add_fingerprint;

        smem_statement_container(agent* new_agent);

    private:

        void create_tables();
        void create_indices();
        void drop_tables(agent* new_agent);
};

//////////////////////////////////////////////////////////
// Soar Constants
//////////////////////////////////////////////////////////

enum smem_variable_key
{
    var_max_cycle, var_num_nodes, var_num_edges, var_act_thresh, var_act_mode
};

#define SMEM_ACT_MAX static_cast<uint64_t>( static_cast<uint64_t>( 0 - 1 ) / static_cast<uint64_t>(2) )

#define SMEM_LTI_UNKNOWN_LEVEL 0

#define SMEM_AUGMENTATIONS_NULL 0
#define SMEM_AUGMENTATIONS_NULL_STR "0"

#define SMEM_ACT_HISTORY_ENTRIES 10
#define SMEM_ACT_LOW -1000000000

#define SMEM_SCHEMA_VERSION "2.0"

//////////////////////////////////////////////////////////
// Soar Integration Types
//////////////////////////////////////////////////////////

// represents the unique identification of a
// long-term identifier
typedef uint64_t smem_lti_id;

// represents a temporal hash
typedef uint64_t smem_hash_id;

// represents a collection of long-term identifiers
typedef std::list<smem_lti_id> smem_lti_list;
typedef std::set<smem_lti_id> smem_lti_set;
typedef std::map<smem_lti_id, uint64_t> smem_lti_map;

// a list of symbols
typedef std::list<Symbol*> smem_sym_list;

// ways to store an identifier
enum smem_storage_type { store_level, store_recursive, store_mirrored };

// represents a list of wmes
typedef std::list<wme*> smem_wme_list;

// represents a set of symbols
#ifdef USE_MEM_POOL_ALLOCATORS
typedef std::set< Symbol*, std::less< Symbol* >, soar_module::soar_memory_pool_allocator< Symbol* > > smem_pooled_symbol_set;
#else
typedef std::set< Symbol* > smem_pooled_symbol_set;
#endif

// list used primarily like a stack
#ifdef USE_MEM_POOL_ALLOCATORS
typedef std::list< preference*, soar_module::soar_memory_pool_allocator< preference* > > smem_wme_stack;
#else
typedef std::list< preference* > smem_wme_stack;
#endif

// data associated with each state
typedef struct smem_data_struct
{
    uint64_t last_cmd_time[2];          // last update to smem.command
    uint64_t last_cmd_count[2];         // last update to smem.command

    smem_wme_stack* smem_wmes;          // wmes in last smem
} smem_data;

//

enum smem_cue_element_type { attr_t, value_const_t, value_lti_t, smem_cue_element_type_none };

typedef struct smem_weighted_cue_element_struct
{
    uint64_t weight;

    struct wme_struct* cue_element;
    smem_hash_id attr_hash;
    smem_hash_id value_hash;
    smem_lti_id value_lti;

    smem_cue_element_type element_type;
    bool pos_element;
    MathQuery* mathElement;

} smem_weighted_cue_element;

struct smem_compare_weighted_cue_elements
{
    bool operator()(const smem_weighted_cue_element* a, const smem_weighted_cue_element* b) const
    {
        return (a->weight > b->weight);
    }
};

typedef std::priority_queue<smem_weighted_cue_element*, std::vector<smem_weighted_cue_element*>, smem_compare_weighted_cue_elements> smem_prioritized_weighted_cue;
typedef std::list<smem_weighted_cue_element*> smem_weighted_cue_list;

typedef std::pair< double, smem_lti_id > smem_activated_lti;

struct smem_compare_activated_lti
{
    bool operator()(const smem_activated_lti a, const smem_activated_lti b) const
    {
        return (b.first > a.first);
    }
};

typedef std::priority_queue< smem_activated_lti, std::vector<smem_activated_lti>, smem_compare_activated_lti> smem_prioritized_activated_lti_queue;

//

typedef struct smem_chunk_struct smem_chunk;
typedef std::set<smem_chunk*> smem_chunk_set;
typedef union smem_chunk_value_union smem_chunk_value;
typedef std::list<smem_chunk_value*> smem_slot;
typedef std::map<Symbol*, smem_slot*> smem_slot_map;

struct smem_chunk_struct
{
    Symbol* soar_id;
    smem_lti_id lti_id;

    char lti_letter;
    uint64_t lti_number;

    smem_slot_map* slots;
};

struct smem_chunk_value_constant
{
    smem_cue_element_type val_type;
    Symbol* val_value;
};

struct smem_chunk_value_lti
{
    smem_cue_element_type val_type;
    smem_chunk* val_value;
};

union smem_chunk_value_union
{
    struct smem_chunk_value_constant val_const;
    struct smem_chunk_value_lti val_lti;
};

typedef std::map<std::string, smem_chunk*> smem_str_to_chunk_map;
typedef std::map<Symbol*, smem_chunk*> smem_sym_to_chunk_map;

//

typedef struct smem_vis_lti_struct
{
    public:
        smem_lti_id lti_id;
        std::string lti_name;
        unsigned int level;
} smem_vis_lti;

//

enum smem_query_levels { qry_search, qry_full };
enum smem_install_type { wm_install, fake_install };

//////////////////////////////////////////////////////////
// Soar Functions (see cpp for comments)
//////////////////////////////////////////////////////////

extern bool smem_enabled(agent* thisAgent);
extern void smem_attach(agent* thisAgent);

extern bool smem_calc_spread_trajectory_actr(agent* thisAgent);
extern bool smem_calc_spread_trajectories(agent* thisAgent);
extern bool smem_calc_spread_trajectories_deterministic(agent* thisAgent);

extern bool smem_parse_chunks(agent* thisAgent, const char* chunks, std::string** err_msg);
extern bool smem_parse_cues(agent* thisAgent, const char* chunks, std::string** err_msg, std::string** result_message, uint64_t number_to_retrieve);
extern bool smem_parse_remove(agent* thisAgent, const char* chunks, std::string** err_msg, std::string** result_message, bool force = false);

extern void smem_visualize_store(agent* thisAgent, std::string* return_val);
extern void smem_visualize_lti(agent* thisAgent, smem_lti_id lti_id, unsigned int depth, std::string* return_val);
extern void smem_print_store(agent* thisAgent, std::string* return_val);
extern void smem_print_lti(agent* thisAgent, smem_lti_id lti_id, uint64_t depth, std::string* return_val, bool history = false);

typedef struct condition_struct condition;
typedef struct action_struct action;

extern bool smem_count_ltis(agent* thisAgent, void* item, void* userdata);
extern bool smem_valid_production(condition* lhs_top, action* rhs_top);

extern smem_lti_id smem_lti_get_id(agent* thisAgent, char name_letter, uint64_t name_number);
extern Symbol* smem_lti_soar_make(agent* thisAgent, smem_lti_id lti, char name_letter, uint64_t name_number, goal_stack_level level);

extern void smem_reset(agent* thisAgent, Symbol* state);
extern void smem_reset_id_counters(agent* thisAgent);
extern void smem_close(agent* thisAgent);
extern void smem_reinit(agent* thisAgent);
extern void smem_reinit_cmd(agent* thisAgent);

// perform smem actions
extern void smem_go(agent* thisAgent, bool store_only);
extern bool smem_backup_db(agent* thisAgent, const char* file_name, std::string* err);

void smem_init_db(agent* thisAgent);

#endif
