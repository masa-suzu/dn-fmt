// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Microsoft.DotNet.DeadRegionAnalysis
{
    internal class PreprocessorSymbolTracker : CSharpSyntaxVisitor
    {
        private readonly IEnumerable<string> m_specifiedSymbols;
        private readonly HashSet<string> m_unvisitedSymbols;
        private readonly HashSet<string> m_visitedSymbols;

        public IEnumerable<string> SpecifiedSymbols { get { return m_specifiedSymbols; } }
        public IEnumerable<string> UnvisitedSymbols { get { return m_unvisitedSymbols; } }
        public IEnumerable<string> VisitedSymbols { get { return m_visitedSymbols; } }

        public PreprocessorSymbolTracker(IEnumerable<string> specifiedSymbols)
        {
            m_specifiedSymbols = specifiedSymbols;
            m_unvisitedSymbols = new HashSet<string>(specifiedSymbols);
            m_visitedSymbols = new HashSet<string>();
        }

        private void VisitSymbol(string symbol)
        {
            m_unvisitedSymbols.Remove(symbol);
            m_visitedSymbols.Add(symbol);
        }

        public override void VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.TrueLiteralExpression:
                    VisitSymbol("true");
                    break;
                case SyntaxKind.FalseLiteralExpression:
                    VisitSymbol("false");
                    break;
                default:
                    throw new InvalidPreprocessorExpressionException("Expected true or false literal expression");
            }
        }

        public override void VisitIdentifierName(IdentifierNameSyntax node)
        {
            VisitSymbol(node.ToString());
        }

        public override void VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            if (node.Kind() != SyntaxKind.LogicalNotExpression)
            {
                throw new InvalidPreprocessorExpressionException("Expected logical not expression");
            }

            node.Operand.Accept(this);
        }

        public override void VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            node.Left.Accept(this);
            node.Right.Accept(this);

            switch (node.Kind())
            {
                case SyntaxKind.LogicalAndExpression:
                case SyntaxKind.LogicalOrExpression:
                    break;
                default:
                    throw new InvalidPreprocessorExpressionException("Expected logical and/or expression");
            }
        }

        public override void VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
        {
            node.Expression.Accept(this);
        }
    }
}
