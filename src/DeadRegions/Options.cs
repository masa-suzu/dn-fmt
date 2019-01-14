using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Microsoft.DotNet.DeadRegionAnalysis;

namespace DeadRegions
{
    internal class Options
    {
        private static readonly char[] s_symbolSeparatorChars = new[] { ';', ',' };

        private OptionParser m_parser;
        private List<string> m_ignoredSymbols;
        private List<string> m_definedSymbols;
        private List<string> m_disabledSymbols;
        private List<IEnumerable<string>> m_symbolConfigurations;

        public string Usage { get { return m_parser.Usage; } }

        public ImmutableArray<string> FilePaths { get; private set; }

        public IEnumerable<string> IgnoredSymbols { get { return m_ignoredSymbols; } }

        public IEnumerable<string> DefinedSymbols { get { return m_definedSymbols; } }

        public IEnumerable<string> DisabledSymbols { get { return m_disabledSymbols; } }

        public IEnumerable<IEnumerable<string>> SymbolConfigurations { get { return m_symbolConfigurations; } }

        public Tristate UndefinedSymbolValue { get; private set; }

        public bool PrintDisabled { get; private set; }

        public bool PrintEnabled { get; private set; }

        public bool PrintVarying { get; private set; }

        public bool PrintSymbolInfo { get; private set; }

        public bool Edit { get; private set; }

        public Options()
        {
            m_parser = new OptionParser();

            m_parser.Add(
                "config",
                arg => m_symbolConfigurations.Add(ParseSymbolList(arg)),
                parameterUsage: "<symbol list>",
                description: "Specify a complete symbol configuration",
                allowMultiple: true);

            m_parser.Add(
                "ignore",
                arg => m_ignoredSymbols.AddRange(ParseSymbolList(arg)),
                parameterUsage: "<symbol list>",
                description: "Ignore a list of symbols (treat as varying)",
                allowMultiple: true);

            m_parser.Add(
                "define",
                arg => m_definedSymbols.AddRange(ParseSymbolList(arg)),
                parameterUsage: "<symbol list>",
                description: "Define a list of symbols (treat as always true)",
                allowMultiple: true);

            m_parser.Add(
                "disable",
                arg => m_disabledSymbols.AddRange(ParseSymbolList(arg)),
                parameterUsage: "<symbol list>",
                description: "Disable a list of symbols (treat as always disabled)",
                allowMultiple: true);

            m_parser.Add(
                "default",
                arg => UndefinedSymbolValue = Tristate.Parse(arg),
                parameterUsage: "<false|true|varying>",
                description: "Set the default value for symbols which do not have a specified value (defaults to varying)");

            m_parser.Add(
                "printdisabled",
                () => PrintDisabled = true,
                description: "Print the list of always disabled conditional regions");

            m_parser.Add(
                "printenabled",
                () => PrintEnabled = true,
                description: "Print the list of always enabled conditional regions");

            m_parser.Add(
                "printvarying",
                () => PrintVarying = true,
                description: "Print the list of varying conditional regions");

            m_parser.Add(
                "printsymbols",
                () => PrintSymbolInfo = true,
                description: "Print the lists of uniquely specified preprocessor symbols, symbols visited during analysis, and symbols not encountered during analysis");

            m_parser.Add(
                "print",
                () => PrintDisabled = PrintEnabled = PrintVarying = PrintSymbolInfo = true,
                description: "Print the entire list of conditional regions and the lists of preprocessor symbols (combination of printenabled, printdisabled, printvarying, and printsymbols)");

            m_parser.Add(
                "edit",
                () => Edit = true,
                "Perform edits to remove always enabled and always disabled conditional regions from source files, and simplify preprocessor expressions which evaluate to 'varying'");
        }

        public bool Parse()
        {
            try
            {
                m_ignoredSymbols = new List<string>();
                m_definedSymbols = new List<string>();
                m_disabledSymbols = new List<string>();
                m_symbolConfigurations = new List<IEnumerable<string>>();
                UndefinedSymbolValue = Tristate.Varying;
                FilePaths = m_parser.Parse(Environment.CommandLine);
            }
            catch (OptionParseException e)
            {
                Console.WriteLine("error: " + e.Message);
                return false;
            }

            return FilePaths.Length > 0;
        }

        private static string[] ParseSymbolList(string s)
        {
            if (string.IsNullOrWhiteSpace(s))
            {
                throw new FormatException("Symbol list must not be empty");
            }

            return s.Split(s_symbolSeparatorChars);
        }
    }
}
