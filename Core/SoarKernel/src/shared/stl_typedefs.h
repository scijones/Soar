#ifndef STL_TYPEDEFS_H_
#define STL_TYPEDEFS_H_

#include "kernel.h"
#include "triples.h"
#include "mempool_allocator.h"

#include <map>
#include <string>
#include <set>
#include <unordered_set>
#include <list>
#include <functional>
#include <assert.h>
#include <cmath>


typedef std::list< symbol_triple* > symbol_triple_list;
typedef std::list< test_triple* > test_triple_list;
typedef std::list< identity_triple* > identity_triple_list;

typedef std::map< Symbol*, Symbol* > rl_symbol_map;
typedef std::set< rl_symbol_map > rl_symbol_map_set;

class sym_grounding_path;
class condition_record;
class action_record;
class instantiation_record;

#ifdef USE_MEM_POOL_ALLOCATORS
    typedef std::list< condition*, soar_module::soar_memory_pool_allocator< condition* > >                  condition_list;
    typedef std::list< Symbol*, soar_module::soar_memory_pool_allocator< Symbol* > >                        symbol_list;
    typedef std::set< Symbol*, std::less< Symbol* >, soar_module::soar_memory_pool_allocator< Symbol* > >   symbol_set;

    /* Windows does not like the memory pool allocators for wme's for some reason. Not sure why yet, so using
     * without allocator for now. */
    //typedef std::list< wme*, soar_module::soar_memory_pool_allocator< wme* > > wme_list;
    typedef std::list< wme* >                                                                               wme_list;

    typedef std::set< wme*, std::less< wme* >, soar_module::soar_memory_pool_allocator< wme* > >            wme_set;
    typedef std::list<sym_grounding_path*, soar_module::soar_memory_pool_allocator<sym_grounding_path*> >   sym_grounding_path_list;

    typedef std::set< wma_decay_element*, std::less< wma_decay_element* >, soar_module::soar_memory_pool_allocator< wma_decay_element* > >                          wma_decay_set;
    typedef std::map< wma_d_cycle, wma_decay_set*, std::less< wma_d_cycle >, soar_module::soar_memory_pool_allocator< std::pair< wma_d_cycle, wma_decay_set* > > >  wma_forget_p_queue;
    typedef std::set< wma_d_cycle, std::less< wma_d_cycle >, soar_module::soar_memory_pool_allocator< wma_d_cycle > >                                               wma_decay_cycle_set;

    typedef std::set< wme*, std::less< wme* >, soar_module::soar_memory_pool_allocator< wme* > >                                                                    wma_pooled_wme_set;
    typedef std::map< Symbol*, uint64_t, std::less< Symbol* >, soar_module::soar_memory_pool_allocator< std::pair< Symbol*, uint64_t > > >                          wma_sym_reference_map;
    typedef std::map< production*, double, std::less< production* >, soar_module::soar_memory_pool_allocator< std::pair< production*, double > > >                  rl_et_map;
    typedef std::list< production*, soar_module::soar_memory_pool_allocator< production* > >                                                                        rl_rule_list;
#else
    typedef std::list< condition* >                 condition_list;
    typedef std::list< Symbol* >                    symbol_list;
    typedef std::set< Symbol* >                     symbol_set;
    typedef std::list< wme* >                       wme_list;
    typedef std::set< wme* >                        wme_set;
    typedef std::list< sym_grounding_path* >        sym_grounding_path_list;

    typedef std::set< wma_decay_element* >          wma_decay_set;
    typedef std::map< wma_d_cycle, wma_decay_set* > wma_forget_p_queue;
    typedef std::set< wma_d_cycle >                 wma_decay_cycle_set;

    typedef std::set< wme* >                        wma_pooled_wme_set;
    typedef std::map< Symbol*, uint64_t >           wma_sym_reference_map;
    typedef std::map< production*, double >         rl_et_map;
    typedef std::list< production* >                rl_rule_list;
#endif

    //#ifdef USE_MEM_POOL_ALLOCATORS
    //#include "soar_module.h"
    //typedef std::list< condition_record*, soar_module::soar_memory_pool_allocator< condition_record* > > condition_record_list;
    //typedef std::list< action_record*, soar_module::soar_memory_pool_allocator< action_record* > > action_record_list;
    //typedef std::list< uint64_t, soar_module::soar_memory_pool_allocator< uint64_t > > id_list;
    //#else
    //typedef std::list< condition_record* > condition_record_list;
    //typedef std::list< action_record* > action_record_list;
    //typedef std::list< uint64_t > id_list;
    //#endif

    /* MToDo | Need to make allocator versions for these */
    typedef std::set< instantiation* >                                  inst_set;
    typedef std::set< instantiation_record* >                           inst_record_set;
    typedef std::list< instantiation_record* >                          inst_record_list;
    typedef std::list< inst_record_list* >                              inst_path_list;
    typedef std::list< condition_record* >                              condition_record_list;
    typedef std::list< action_record* >                                 action_record_list;
    typedef std::list< uint64_t >                                       id_list;
    typedef std::unordered_set< uint64_t >                              id_set;
    typedef std::unordered_map< uint64_t, uint64_t >                    id_to_id_map_type;

    typedef struct identity_set_struct identity_set_info;
    typedef std::unordered_map< uint64_t, identity_set_info* >                    id_to_idset_map_type;
    typedef std::unordered_map< uint64_t, identity_set_info* >::iterator          id_to_idset_map_iter_type;

#endif /* STL_TYPEDEFS_H_ */
