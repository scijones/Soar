/////////////////////////////////////////////////////////////////
// smem command file.
//
// Author: Jonathan Voigt, voigtjr@gmail.com,
//         Nate Derbinsky, nlderbin@umich.edu
// Date  : 2009
//
/////////////////////////////////////////////////////////////////

#include "portability.h"

#include "cli_CommandLineInterface.h"

#include "cli_Commands.h"

#include "sml_Names.h"
#include "sml_AgentSML.h"
#include "slot.h"

#include "semantic_memory.h"
#include "agent.h"
#include "misc.h"

using namespace cli;
using namespace sml;

bool CommandLineInterface::DoSMem(const char pOp, const std::string* pAttr, const std::string* pVal)
{
    agent* thisAgent = m_pAgentSML->GetSoarAgent();
    std::ostringstream tempString;

    if (!pOp)
    {
        // Print SMem Settings
        PrintCLIMessage_Header("Semantic Memory Settings", 40);
        PrintCLIMessage_Item("learning:", thisAgent->smem_params->learning, 40);
        PrintCLIMessage_Section("Storage", 40);
        PrintCLIMessage_Item("database:", thisAgent->smem_params->database, 40);
        PrintCLIMessage_Item("append:", thisAgent->smem_params->append_db, 40);
        PrintCLIMessage_Item("path:", thisAgent->smem_params->path, 40);
        PrintCLIMessage_Item("lazy-commit:", thisAgent->smem_params->lazy_commit, 40);
        PrintCLIMessage_Section("Activation", 40);
        PrintCLIMessage_Item("activation-mode:", thisAgent->smem_params->activation_mode, 40);
        PrintCLIMessage_Item("activate-on-query:", thisAgent->smem_params->activate_on_query, 40);
        PrintCLIMessage_Item("activate-on-add:", thisAgent->smem_params->activate_on_add, 40);
        PrintCLIMessage_Item("base-decay:", thisAgent->smem_params->base_decay, 40);
        PrintCLIMessage_Item("base-update-policy:", thisAgent->smem_params->base_update, 40);
        PrintCLIMessage_Item("base-incremental-threshes:", thisAgent->smem_params->base_incremental_threshes, 40);
        PrintCLIMessage_Item("thresh:", thisAgent->smem_params->thresh, 40);
        PrintCLIMessage_Section("Performance", 40);
        PrintCLIMessage_Item("page-size:", thisAgent->smem_params->page_size, 40);
        PrintCLIMessage_Item("cache-size:", thisAgent->smem_params->cache_size, 40);
        PrintCLIMessage_Item("optimization:", thisAgent->smem_params->opt, 40);
        PrintCLIMessage_Item("timers:", thisAgent->smem_params->timers, 40);
        PrintCLIMessage_Section("Experimental", 40);
        PrintCLIMessage_Item("merge:", thisAgent->smem_params->merge, 40);
        PrintCLIMessage_Item("mirroring:", thisAgent->smem_params->mirroring, 40);
        PrintCLIMessage_Item("spreading:", thisAgent->smem_params->spreading, 40);
        PrintCLIMessage_Item("spontaneous-retrieval:", thisAgent->smem_params->spontaneous_retrieval, 40);
        PrintCLIMessage_Item("spreading-baseline:", thisAgent->smem_params->spreading_baseline, 40);
        PrintCLIMessage_Item("spreading-type:", thisAgent->smem_params->spreading_type, 40);
        PrintCLIMessage_Item("spreading-direction:", thisAgent->smem_params->spreading_direction, 40);
        PrintCLIMessage_Item("spreading-normalization:", thisAgent->smem_params->spreading_normalization, 40);
        PrintCLIMessage_Item("spreading-depth-limit:", thisAgent->smem_params->spreading_depth_limit, 40);
        PrintCLIMessage_Item("spreading-limit:", thisAgent->smem_params->spreading_limit, 40);
        PrintCLIMessage_Item("spreading-time:", thisAgent->smem_params->spreading_time, 40);
        PrintCLIMessage_Item("spreading-crawl-time:", thisAgent->smem_params->spreading_crawl_time, 40);
        PrintCLIMessage_Item("spreading-model:", thisAgent->smem_params->spreading_model, 40);
        PrintCLIMessage_Item("spreading-traversal:", thisAgent->smem_params->spreading_traversal, 40);
        PrintCLIMessage_Item("spreading-loop-avoidance:", thisAgent->smem_params->spreading_loop_avoidance, 40);
        PrintCLIMessage_Item("number-trajectories:", thisAgent->smem_params->number_trajectories, 40);
        PrintCLIMessage_Item("continue-probability:", thisAgent->smem_params->continue_probability, 40);
        PrintCLIMessage("");

        return true;
    }
    else if (pOp == 'a')
    {
        std::string* err = new std::string("");
        bool result = smem_parse_chunks(thisAgent, pAttr->c_str(), &(err));

        if (!result)
        {
            SetError(*err);
        }
        else
        {
            PrintCLIMessage("Knowledge added to semantic memory.");
        }
        delete err;
        return result;
    }
    else if (pOp == 'b')
    {
        std::string err;
        bool result = smem_backup_db(thisAgent, pAttr->c_str(), &(err));

        if (!result)
        {
            SetError("Error while backing up database: " + err);
        }
        else
        {
            tempString << "Semantic memory database backed up to " << pAttr->c_str();
            PrintCLIMessage(&tempString);
        }

        return result;
    }
    else if (pOp == 'e')
    {
        bool result = thisAgent->smem_params->learning->set_string("on");

        if (!result)
        {
            SetError("This parameter is protected while the semantic memory database is open.");
        }
        else
        {
            PrintCLIMessage("Semantic memory enabled.");
        }

        return result;
    }
    else if (pOp == 'd')
    {
        bool result = thisAgent->smem_params->learning->set_string("off");

        if (!result)
        {
            SetError("This parameter is protected while the semantic memory database is open.");
        }
        else
        {
            PrintCLIMessage("Semantic memory disabled.");
        }

        return result;
    }
    else if (pOp == 'g')
    {
        soar_module::param* my_param = thisAgent->smem_params->get(pAttr->c_str());
        if (!my_param)
        {
            return SetError("Invalid semantic memory parameter.  Use 'help smem' to see list of valid settings.");
        }

        PrintCLIMessage_Item("", my_param, 0);
        return true;
    }
    else if (pOp == 'h')
    {
        smem_lti_id lti_id = NIL;
        uint64_t depth = 1;
        bool history = true;
        smem_attach(thisAgent);

        if (thisAgent->smem_db->get_status() != soar_module::connected)
        {
            return SetError("Semantic memory database not connected.");
        }

        if (pAttr)
        {
            soar::Lexeme lexeme = soar::Lexer::get_lexeme_from_string(thisAgent, pAttr->c_str());
            if (lexeme.type == IDENTIFIER_LEXEME)
            {
                lti_id = smem_lti_get_id(thisAgent, lexeme.id_letter, lexeme.id_number);
            }
            if (lti_id == NIL)
            {
                return SetError("LTI not found");
            }
        }

        std::string viz;

        smem_print_lti(thisAgent, lti_id, depth, &(viz), history);

        if (viz.empty())
        {
            return SetError("Could not find information on LTI.");
        }

        PrintCLIMessage_Header("Semantic Memory", 40);
        PrintCLIMessage(&viz);
        return true;
    }
    else if (pOp == 'i')
    {
        // Because of LTIs, re-initializing requires all other memories to be reinitialized.
        // epmem - close before working/production memories to get re-init benefits
        // smem - close before working/production memories to prevent id counter mess-ups
        // production memory (automatic init-soar clears working memory as a result)

        epmem_reinit_cmd(thisAgent);
        smem_reinit_cmd(thisAgent);

        ExciseBitset options(0);
        options.set(EXCISE_ALL, true);
        DoExcise(options, 0);

        PrintCLIMessage("Semantic memory system re-initialized.");
        return true;
    }
    else if (pOp == 'p')
    {
        smem_lti_id lti_id = NIL;
        unsigned int depth = 1;

        smem_attach(thisAgent);
        if (thisAgent->smem_db->get_status() != soar_module::connected)
        {
            return SetError("Semantic memory database not connected.");
        }

        if (pAttr)
        {
            const char* pAttr_c_str = pAttr->c_str();
            soar::Lexer lexer(thisAgent, pAttr_c_str);
            lexer.get_lexeme();
            if (lexer.current_lexeme.type == AT_LEXEME)
            {
                lexer.get_lexeme();
            }
            if (lexer.current_lexeme.type == IDENTIFIER_LEXEME)
            {
                if (thisAgent->smem_db->get_status() == soar_module::connected)
                {
                    lti_id = smem_lti_get_id(thisAgent, lexer.current_lexeme.id_letter, lexer.current_lexeme.id_number);

                    if ((lti_id != NIL) && pVal)
                    {
                        from_c_string(depth, pVal->c_str());
                    }
                }
            }

            if (lti_id == NIL)
            {
                return SetError("LTI not found.");
            }
        }

        std::string viz;

        if (lti_id == NIL)
        {
            smem_print_store(thisAgent, &(viz));
            if (!viz.empty())
            {
                PrintCLIMessage_Header("Semantic Memory", 40);
        }
        }
        else
        {
            smem_print_lti(thisAgent, lti_id, depth, &(viz));
        }
        if (viz.empty())
        {
            return SetError("Semantic memory is empty.");
        }

        PrintCLIMessage(&viz);
        return true;
    }
    else if (pOp == 'q')
    {
        std::string* err = new std::string;
        std::string* retrieved = new std::string;
        uint64_t number_to_retrieve = 1;

        if (pVal)
        {
            from_c_string(number_to_retrieve, pVal->c_str());
        }

        bool result = smem_parse_cues(thisAgent, pAttr->c_str(), &(err), &(retrieved), number_to_retrieve);

        if (!result)
        {
            SetError("Error while parsing query\n" + *err);
        }
        else
        {
            PrintCLIMessage(retrieved);
            PrintCLIMessage("SMem| Query complete.");
        }
        delete err;
        delete retrieved;
        return result;
    }
    else if (pOp == 'r')
    {
        std::string* err = new std::string;
        std::string* retrieved = new std::string;
        bool force = false;
        if (pVal)
        {
            force = (!strcmp(pVal->c_str(), "f") || (!strcmp(pVal->c_str(), "force")));
        }

        bool result = smem_parse_remove(thisAgent, pAttr->c_str(), &(err), &(retrieved), force);

        if (!result)
        {
            SetError("Error while attempting removal.\n" + *err);
        }
        else
        {
            PrintCLIMessage(retrieved);
            PrintCLIMessage(err);
            PrintCLIMessage("SMem| Removal complete.");
        }
        delete err;
        delete retrieved;
        return result;
    }
    else if (pOp == 's')
    {
        soar_module::param* my_param = thisAgent->smem_params->get(pAttr->c_str());
        if (!my_param)
        {
            return SetError("Invalid SMem parameter.");
        }

        if (!my_param->validate_string(pVal->c_str()))
        {
            return SetError("Invalid setting for SMem parameter.");
        }
        if (!strcmp(pAttr->c_str(), "spontaneous-retrieval"))
        {
            if (thisAgent->smem_params->spontaneous_retrieval->get_value() == on)
            {
                soar_module::sqlite_statement* lti_back_index_create = new soar_module::sqlite_statement(thisAgent->smem_db,
                    "CREATE INDEX smem_lti_act ON smem_lti (activation_value, lti_id)");
                lti_back_index_create->execute(soar_module::op_reinit);
                delete lti_back_index_create;
                PrintCLIMessage("If spontaneous retrieval is on, there is a large cost during activation updates.\n"
                    "If you want to avoid this cost, turn it back off.");
            }
            else
            {
                soar_module::sqlite_statement* lti_back_index_create = new soar_module::sqlite_statement(thisAgent->smem_db,
                    "DROP INDEX smem_lti_act");
                lti_back_index_create->execute(soar_module::op_reinit);
                delete lti_back_index_create;
            }
        }
        
        if (!(strcmp(pAttr->c_str(), "spreading-baseline") &&
            strcmp(pAttr->c_str(), "spreading-type") &&
            strcmp(pAttr->c_str(), "spreading-direction") &&
            strcmp(pAttr->c_str(), "spreading-normalization") &&
            strcmp(pAttr->c_str(), "spreading-depth-limit") &&
            strcmp(pAttr->c_str(), "spreading-limit") &&
            strcmp(pAttr->c_str(), "spreading-time") &&
            strcmp(pAttr->c_str(), "spreading-model") &&
            strcmp(pAttr->c_str(), "spreading-traversal") &&
            strcmp(pAttr->c_str(), "spreading-loop-avoidance") &&
            strcmp(pAttr->c_str(), "number-trajectories") &&
            strcmp(pAttr->c_str(), "continue-probability")) && thisAgent->smem_params->spreading->get_value() == on)
        {
            return SetError("Some spreading activation parameters cannot be changed once spreading activation has been turned on.");
        }
        if (!strcmp(pAttr->c_str(), "spreading-crawl-time") && !strcmp(pVal->c_str(), "precalculate"))
        {
            if (thisAgent->smem_params->spreading->get_value() != on)
            {
                PrintCLIMessage("If spreading is turned on while precalculate is on,\n"
                        "a large batch calculation for spreading activation will occur.");
            }
        }
        smem_param_container::db_choices last_db_mode = thisAgent->smem_params->database->get_value();
        bool result = my_param->set_string(pVal->c_str());

        if (!result)
        {
            SetError("This parameter is protected while the semantic memory database is open.");
        }
        else
        {
            tempString << pAttr->c_str() << " = " << pVal->c_str();
            PrintCLIMessage(&tempString);
            if (thisAgent->smem_db->get_status() == soar_module::connected)
            {
                if (((!strcmp(pAttr->c_str(), "database")) && (thisAgent->smem_params->database->get_value() != last_db_mode)) ||
                        (!strcmp(pAttr->c_str(), "path")))
                {
                    PrintCLIMessage("To finalize database switch, issue an smem --init command.\n");
                }
            }
            if (!strcmp(pAttr->c_str(), "append"))
            {
                if (thisAgent->smem_params->append_db->get_value() == off)
                {
                    PrintCLIMessage("Warning: Since append mode is off, starting/reinitializing,\n"
                                    "         Soar will erase the semantic memory database.\n");
                }

            }
            if (!strcmp(pAttr->c_str(), "spreading-type"))
            {
                //fragile - I'm assuming no typo (but just in case, I'm defaulting to ppr.)
                if (pVal)
                {
                    if (strcmp(pVal->c_str(), "actr") && strcmp(pVal->c_str(), "ppr"))
                    {
                        return SetError("Invalid semantic memory parameter.  Spreading type must be actr or ppr.");
                        //assert(false); //This shouldn't happen while I'm testing.
                    }
                }
            }
            if (!strcmp(pAttr->c_str(), "spreading"))
            {
                if (thisAgent->smem_params->spreading->get_value() == on)
                {
                    PrintCLIMessage("This might take a long while.\n");
                    //This is where a huge batch processing of all of SMem can be run.

                    if (thisAgent->smem_params->spreading_crawl_time->get_value() == smem_param_container::precalculate)
                    {
                        if  (thisAgent->smem_params->spreading_traversal->get_value() == smem_param_container::random)
                        {
                            smem_calc_spread_trajectories(thisAgent);
                        }
                        else if (thisAgent->smem_params->spreading_traversal->get_value() == smem_param_container::deterministic)
                        {
                            if (thisAgent->smem_params->spreading_type->get_value() == smem_param_container::actr)
                            {
                                smem_calc_spread_trajectory_actr(thisAgent);
                            }
                            else
                            {
    thisAgent->smem_timers->total->start();
                                smem_calc_spread_trajectories_deterministic(thisAgent);
    thisAgent->smem_timers->total->stop();
                            }
                        }
                    }
                }
                else
                {
                    return SetError("Spreading activation has undefined behavior when turned off.\n"
                            "If you wish to reset spreading activation,\n"
                            "type 'smem --reset spreading-activation'. Then, you can then change\n"
                            "the spreading activation parameters and turn it back on afterwards.");
                }
            }
        }
        return result;
    }
    else if (pOp == 'S')
    {
        smem_attach(thisAgent);
        if (!pAttr)
        {
            // Print SMem Settings
            PrintCLIMessage_Header("Semantic Memory Statistics", 40);
            PrintCLIMessage_Item("SQLite Version:", thisAgent->smem_stats->db_lib_version, 40);
            PrintCLIMessage_Item("Memory Usage:", thisAgent->smem_stats->mem_usage, 40);
            PrintCLIMessage_Item("Memory Highwater:", thisAgent->smem_stats->mem_high, 40);
            PrintCLIMessage_Item("Retrieves:", thisAgent->smem_stats->expansions, 40);
            PrintCLIMessage_Item("Queries:", thisAgent->smem_stats->cbr, 40);
            PrintCLIMessage_Item("Stores:", thisAgent->smem_stats->stores, 40);
            PrintCLIMessage_Item("Activation Updates:", thisAgent->smem_stats->act_updates, 40);
            PrintCLIMessage_Item("Mirrors:", thisAgent->smem_stats->mirrors, 40);
            PrintCLIMessage_Item("Nodes:", thisAgent->smem_stats->chunks, 40);
            PrintCLIMessage_Item("Edges:", thisAgent->smem_stats->slots, 40);
            thisAgent->smem_stmts->calc_spread_size_debug_cmd->execute();
            uint64_t number_spread_elements = thisAgent->smem_stmts->calc_spread_size_debug_cmd->column_int(0);
            std::ostringstream s_spread_output_string;
            s_spread_output_string << number_spread_elements;
            std::string spread_output_string = s_spread_output_string.str();
            PrintCLIMessage_Justify("Spread Size:",spread_output_string.c_str(), 40);
            thisAgent->smem_stmts->calc_spread_size_debug_cmd->reinitialize();
            thisAgent->smem_stmts->trajectory_size_debug_cmd->execute();
            uint64_t number_fingerprint_elements = thisAgent->smem_stmts->trajectory_size_debug_cmd->column_int(0);
            std::ostringstream s_trajectory_output_string;
            s_trajectory_output_string << number_fingerprint_elements;
            std::string trajectory_output_string = s_trajectory_output_string.str();
            PrintCLIMessage_Justify("Fingerprint Entries:",trajectory_output_string.c_str(), 40);
            thisAgent->smem_stmts->trajectory_size_debug_cmd->reinitialize();
        }
        else
        {
            soar_module::statistic* my_stat = thisAgent->smem_stats->get(pAttr->c_str());
            if (!my_stat)
            {
                return SetError("Invalid statistic.");
            }

            PrintCLIMessage_Item("", my_stat, 0);
        }

        return true;
    }
    else if (pOp == 't')
    {
        if (!pAttr)
        {
            struct foo: public soar_module::accumulator< soar_module::timer* >
            {
                private:
                    bool raw;
                    cli::CommandLineInterface* this_cli;
                    std::ostringstream& m_Result;

                    foo& operator=(const foo&)
                    {
                        return *this;
                    }

                public:
                    foo(bool m_RawOutput, cli::CommandLineInterface* new_cli, std::ostringstream& m_Result): raw(m_RawOutput), this_cli(new_cli), m_Result(m_Result) {};

                    void operator()(soar_module::timer* t)
                    {
                        std::string output(t->get_name());
                        output += ":";
                        this_cli->PrintCLIMessage_Item(output.c_str(), t, 40);
                    }
            } bar(m_RawOutput, this, m_Result);

            PrintCLIMessage_Header("Semantic Memory Timers", 40);
            thisAgent->smem_timers->for_each(bar);
        }
        else
        {
            soar_module::timer* my_timer = thisAgent->smem_timers->get(pAttr->c_str());
            if (!my_timer)
            {
                return SetError("Invalid timer.");
            }

            PrintCLIMessage_Item("", my_timer, 0);
        }

        return true;
    }
    else if (pOp == 'v')
    {
        smem_lti_id lti_id = NIL;
        unsigned int depth = 1;

        // visualizing the store requires an open semantic database
        smem_attach(thisAgent);

        if (pAttr)
        {
            soar::Lexeme lexeme = soar::Lexer::get_lexeme_from_string(thisAgent, pAttr->c_str());
            if (lexeme.type == IDENTIFIER_LEXEME)
            {
                if (thisAgent->smem_db->get_status() == soar_module::connected)
                {
                    lti_id = smem_lti_get_id(thisAgent, lexeme.id_letter, lexeme.id_number);

                    if ((lti_id != NIL) && pVal)
                    {
                        from_c_string(depth, pVal->c_str());
                    }
                }
            }

            if (lti_id == NIL)
            {
                return SetError("Invalid long-term identifier.");
            }
        }

        std::string viz;

        if (lti_id == NIL)
        {
            smem_visualize_store(thisAgent, &(viz));
        }
        else
        {
            smem_visualize_lti(thisAgent, lti_id, depth, &(viz));
        }

        if (viz.empty())
        {
            return SetError("Nothing to visualize.");
        }
        PrintCLIMessage(&viz);

        return true;
    }

    return SetError("Unknown option.");
}
