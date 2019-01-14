// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Microsoft.DotNet.DeadRegionAnalysis
{
    public struct ConditionalRegionChain : IComparable<ConditionalRegionChain>
    {
        private ImmutableArray<ConditionalRegion> m_regions;

        public bool IsDefault { get { return m_regions == null; } }

        public ImmutableArray<ConditionalRegion> Regions { get { return m_regions; } }

        public int SpanStart { get { return m_regions != null ? m_regions[0].SpanStart : -1; } }

        public int SpanEnd { get { return m_regions != null ? m_regions[m_regions.Length - 1].SpanEnd : -1; } }

        internal ConditionalRegionChain(ImmutableArray<ConditionalRegion> regions)
        {
            if (regions.IsDefaultOrEmpty)
            {
                throw new ArgumentException("regions");
            }

            m_regions = regions;
        }

        public int CompareTo(ConditionalRegionChain other)
        {
            int result = IsDefault.CompareTo(other.IsDefault);

            if (result == 0)
            {
                result = Regions.Length - other.Regions.Length;
                if (result == 0)
                {
                    result = SpanStart - other.SpanStart;
                    if (result == 0)
                    {
                        return SpanEnd - other.SpanEnd;
                    }
                }
            }

            return result;
        }
    }
}
