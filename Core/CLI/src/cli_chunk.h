/*
 * cli_explain.h
 *
 *  Created on: Dec 22, 2015
 *      Author: mazzin
 */

#ifndef CLI_CHUNK_H_
#define CLI_CHUNK_H_

#include "cli_Parser.h"
#include "cli_Options.h"
#include "cli_Cli.h"
#include "misc.h"
#include "kernel.h"
#include "sml_Events.h"

namespace cli
{

    class ChunkCommand : public cli::ParserCommand
    {
        public:
            ChunkCommand(cli::Cli& cli) : cli(cli), ParserCommand() {}
            virtual ~ChunkCommand() {}
            virtual const char* GetString() const
            {
                return "chunk";
            }
            virtual const char* GetSyntax() const
            {
                return  "Syntax:  chunk [--set <option> <value> | --get <option> | --help | --history | --stats]\n"
                        "                      learn  [ always | never | only | except ]\n"
                        "                      bottom-only                     [ on | off ]\n"
                        "                      interrupt                       [ on | off ]\n"
                        "                      ignore-dnb                      [ on | off ]\n"
                        "                      variablize-identity             [ on | off ]\n"
                        "                      enforce-constraints             [ on | off ]\n"
                        "                      add-osk                         [ on | off ]\n"
                        "                      variablize-rhs-funcs            [ on | off ]\n"
                        "                      repair-rhs                      [ on | off ]\n"
                        "                      repair-lhs                      [ on | off ]\n"
                        "                      repair-rhs-promotion            [ on | off ]\n"
                        "                      merge                           [ on | off ]\n"
                        "                      user-singletons                 [ on | off ]\n"
                        "                      allow-local-negative-reasoning  [ on | off ]\n"
                        "                      allow-missing-osk               [ on | off ]\n"
                        "                      allow-smem                      [ on | off ]\n"
                        "                      allow-uncertain-operators       [ on | off ]\n"
                        "                      allow-multiple-prefs            [ on | off ]\n"
                        "                      allow-pre-existing-ltm          [ on | off ]\n"
                        "                      allow-local-promotion           [ on | off ]\n";
            }

            virtual bool Parse(std::vector< std::string >& argv)
            {
                cli::Options opt;
                OptionsData optionsData[] =
                {
                    {'g', "get",        OPTARG_NONE},
                    {'h', "history",    OPTARG_NONE},
                    {'s', "set",        OPTARG_NONE},
                    {'S', "stats",      OPTARG_NONE},
                    {'?', "help",      OPTARG_NONE},
                    {0, 0, OPTARG_NONE}
                };

                char option = 0;

                for (;;)
                {
                    if (!opt.ProcessOptions(argv, optionsData))
                    {
                        cli.SetError(opt.GetError().c_str());
                        return cli.AppendError(GetSyntax());
                    }

                    if (opt.GetOption() == -1)
                    {
                        break;
                    }

                    if (option != 0)
                    {
                        cli.SetError("chunk takes only one option at a time.");
                        return cli.AppendError(GetSyntax());
                    }

                    option = static_cast<char>(opt.GetOption());
                }

                switch (option)
                {
                    case 0:
                    default:
                        // no options
                        break;
                    case 'g':
                    {
                        // case: get requires one non-option argument
                        if (!opt.CheckNumNonOptArgs(1, 1))
                        {
                            cli.SetError(opt.GetError().c_str());
                            return cli.AppendError(GetSyntax());
                        }

                        return cli.DoChunk(option, &(argv[2]));
                    }
                    case 'h':
                    case 'S':
                        if (!opt.CheckNumNonOptArgs(0, 0))
                        {
                            cli.SetError(opt.GetError().c_str());
                            return cli.AppendError(GetSyntax());
                        }

                        return cli.DoChunk(option);
                    case 's':
                    {
                        // case: set requires two non-option arguments
                        if (!opt.CheckNumNonOptArgs(2, 2))
                        {
                            cli.SetError(opt.GetError().c_str());
                            return cli.AppendError(GetSyntax());
                        }

                        return cli.DoChunk(option, &(argv[2]), &(argv[3]));
                    }
                    case '?':
                    {
                        return cli.SetError(GetSyntax());
                    }
                }

                // bad: no option, but more than one argument
                if (argv.size() > 1)
                {
                    return cli.SetError("Too many arguments.");
                }

                // case: nothing = full configuration information
                return cli.DoChunk();
            }

        private:
            cli::Cli& cli;

            ChunkCommand& operator=(const ChunkCommand&);
    };
}



#endif /* CLI_CHUNK_H_ */
