/*
 * Copyright (C) 2016-2023 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include "DisplayListItems.h"
#include "RenderingResource.h"
#include <wtf/Noncopyable.h>
#include <wtf/TZoneMalloc.h>
#include <wtf/ThreadSafeRefCounted.h>
#include <wtf/Vector.h>
#include <wtf/text/WTFString.h>

namespace WTF {
class TextStream;
}

namespace WebCore {
namespace DisplayList {

// Note: currently this class is not usable from multiple threads due to the underlying objects, such
// Font instances, not being thread-safe.
class DisplayList final : public ThreadSafeRefCounted<DisplayList> {
    WTF_MAKE_TZONE_ALLOCATED(DisplayList);
    WTF_MAKE_NONCOPYABLE(DisplayList);
public:
    static Ref<const DisplayList> create(Vector<Item>&& items)
    {
        return adoptRef(*new DisplayList(WTFMove(items)));
    }
    WEBCORE_EXPORT ~DisplayList();

    std::span<const Item> items() const LIFETIME_BOUND { return m_items.span(); }

    WEBCORE_EXPORT String asText(OptionSet<AsTextFlag>) const;
    void dump(WTF::TextStream&) const;

private:
    WEBCORE_EXPORT DisplayList(Vector<Item>&& items);

    Vector<Item> m_items;
};

class RemoteDisplayList final : public ThreadSafeRefCounted<RemoteDisplayList, WTF::DestructionThread::Main> {
    WTF_MAKE_TZONE_ALLOCATED_EXPORT(RemoteDisplayList, WEBCORE_EXPORT);
    WTF_MAKE_NONCOPYABLE(RemoteDisplayList);
public:
    RemoteDisplayList(unsigned hash)
      : m_hash(hash)
    { }
    WEBCORE_EXPORT ~RemoteDisplayList();

    RenderingResourceIdentifier renderingResourceIdentifier() const
    {
        return m_identifier;
    }

    void addObserver(WeakRef<RenderingResourceObserver>&& observer)
    {
        m_observers.add(WTFMove(observer));
    }

    void dump(WTF::TextStream&) const;

    unsigned hash() { return m_hash; }

private:
    WeakHashSet<RenderingResourceObserver> m_observers;
    RenderingResourceIdentifier m_identifier { RenderingResourceIdentifier::generate() };
    unsigned m_hash { 0 };
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const DisplayList&);
WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const RemoteDisplayList&);

} // DisplayList
} // WebCore
