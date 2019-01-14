// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Microsoft.CodeAnalysis;
using Xunit;

namespace Microsoft.DotNet.CodeFormatting.Tests
{
    public class PrivateFieldNamingRuleTests : GlobalSemanticRuleTestBase
    {
        internal override IGlobalSemanticFormattingRule Rule
        {
            get { return new Rules.PrivateFieldNamingRule(); }
        }

        public sealed class CSharpFields : PrivateFieldNamingRuleTests
        {
            [Fact]
            public void TestUnderScoreInPrivateFields()
            {
                var text = @"
using System;
class T
{
    private static int x;
    private static int s_y;
    // some trivia
    private static int m_z;
    // some trivia
    private int k = 1, m_s = 2, rsk_yz = 3, x_y_z;
    // some trivia
    [ThreadStatic] static int r;
    [ThreadStaticAttribute] static int b_r;
}";
                var expected = @"
using System;
class T
{
    private static int s_x;
    private static int s_y;
    // some trivia
    private static int s_z;
    // some trivia
    private int m_k = 1, m_s = 2, m_rsk_yz = 3, m_y_z;
    // some trivia
    [ThreadStatic] static int t_r;
    [ThreadStaticAttribute] static int t_r;
}";
                Verify(text, expected);
            }

            [Fact]
            public void CornerCaseNames()
            {
                var text = @"
class C
{
    private int x_;
    private int _;
    private int __;
    private int m_field1;
    private int field2_;
";

                var expected = @"
class C
{
    private int m_x;
    private int _;
    private int __;
    private int m_field1;
    private int m_field2;
";

                Verify(text, expected, runFormatter: false);
            }

            [Fact]
            public void MultipleDeclarators()
            {
                var text = @"
class C1
{
    private int field1, field2, field3;
}

class C2
{
    private static int field1, field2, field3;
}

class C3
{
    internal int field1, field2, field3;
}
";

                var expected = @"
class C1
{
    private int m_field1, m_field2, m_field3;
}

class C2
{
    private static int s_field1, s_field2, s_field3;
}

class C3
{
    internal int field1, field2, field3;
}
";

                Verify(text, expected, runFormatter: true);
            }

            /// <summary>
            /// If the name is pascal cased make it camel cased during the rewrite.  If it is not
            /// pascal cased then do not change the casing.
            /// </summary>
            [Fact]
            public void NameCasingField()
            {
                var text = @"
class C
{
    int Field;
    static int Other;
    int GCField;
    static int GCOther;
}
";

                var expected = @"
class C
{
    int m_field;
    static int s_other;
    int m_GCField;
    static int s_GCOther;
}
";

                Verify(text, expected, runFormatter: false);
            }

            [Fact]
            public void Issue68()
            {
                var text = @"
delegate void Action();
class C
{
    Action someAction;
    void M(C p)
    {
        someAction();
        this.someAction();
        p.someAction();
    }
}";

                var expected = @"
delegate void Action();
class C
{
    Action m_someAction;
    void M(C p)
    {
        m_someAction();
        this.m_someAction();
        p.m_someAction();
    }
}";

                Verify(text, expected);
            }

            /// <summary>
            /// Ensure that Roslyn properly renames private fields when accessed through a non-this
            /// instance within the same type.
            /// </summary>
            [Fact]
            public void Issue69()
            {
                var text = @"
class C
{
    int field;

    int M(C p)
    {
        int x = p.field;
        return x;
    }
}";

                var expected = @"
class C
{
    int m_field;

    int M(C p)
    {
        int x = p.m_field;
        return x;
    }
}";

                Verify(text, expected);
            }

            [Fact]
            public void Issue258()
            {
                var text = @"
class C
{
    private (string name, string value) myTuple;
}
";
                var expected = @"
class C
{
    private (string name, string value) m_myTuple;
}
";

                Verify(text, expected, runFormatter: false);
            }

            [Fact]
            public void Issue241()
            {
                var text = @"
class C
{
        private bool streamObjects;

        /// <summary>
        /// A collection in which objects that are written using the WriteError
        /// method are accumulated if <see cref=""streamObjects"" /> is false.
        /// </summary>
        private List<string> errors;
        
}
";
                var expected = @"
class C
{
        private bool m_streamObjects;

        /// <summary>
        /// A collection in which objects that are written using the WriteError
        /// method are accumulated if <see cref=""m_streamObjects"" /> is false.
        /// </summary>
        private List<string> m_errors;
        
}
";

                Verify(text, expected, runFormatter: false);
            }
        }

        public sealed class VisualBasicFields : PrivateFieldNamingRuleTests
        {
            [Fact]
            public void Simple()
            {
                var text = @"
Class C 
    Private Field As Integer
End Class";

                var expected = @"
Class C 
    Private m_field As Integer
End Class";

                Verify(text, expected, runFormatter: false, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void ModuleFieldsAreShared()
            {
                var text = @"
Module C
    Private Field As Integer
End Module";

                var expected = @"
Module C
    Private s_field As Integer
End Module";

                Verify(text, expected, runFormatter: false, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void MultipleDeclarations()
            {
                var text = @"
Class C 
    Private Field1, Field2 As Integer
End Class";

                var expected = @"
Class C 
    Private m_field1,m_field2 As Integer
End Class";

                Verify(text, expected, runFormatter: false, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void FieldAndUse()
            {
                var text = @"
Class C 
    Private Field As Integer

    Sub M()
        Console.WriteLine(Field)
    End Sub
End Class";

                var expected = @"
Class C 
    Private m_field As Integer

    Sub M()
        Console.WriteLine(m_field)
    End Sub
End Class";

                Verify(text, expected, runFormatter: false, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void Issue69()
            {
                var text = @"
Class C1
    Private Field As Integer

    Function M(p As C1) As Integer
        Dim x = p.Field
        Return x
    End Function
End Class";

                var expected = @"
Class C1
    Private m_field As Integer

    Function M(p As C1) As Integer
        Dim x = p.m_field
        Return x
    End Function
End Class";

                Verify(text, expected, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void FieldMarkedWithEvents()
            {   // See:  https://github.com/dotnet/codeformatter/issues/216

                var text = @"
Class C1
    Private Field WithEvents As Integer
End Class";

                var expected = @"
Class C1
    Private m_field WithEvents As Integer
End Class";

                Verify(text, expected, languageName: LanguageNames.VisualBasic);
            }

            [Fact]
            public void RemoveTwoLetterThreadStaticPrefix()
            {
                var text = @"
class C
{
    int m_instancem;
    int ts_instancets;
    static int ts_Static;
    [System.ThreadStatic]static int ts_ThreadStatic;
}
";

                var expected = @"
class C
{
    int m_instancem;
    int m_instancets;
    static int s_static;
    [System.ThreadStatic]static int t_threadStatic;
}
";

                Verify(text, expected, runFormatter: false);
            }
        }
    }
}
