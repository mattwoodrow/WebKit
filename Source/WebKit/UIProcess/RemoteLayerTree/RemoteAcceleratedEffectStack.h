/*
 * Copyright (C) 2023 Apple Inc. All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#if ENABLE(THREADED_ANIMATION_RESOLUTION)
#include <WebCore/AcceleratedEffect.h>
#include <WebCore/AcceleratedEffectStack.h>
#include <WebCore/AcceleratedEffectValues.h>
#endif

namespace WebKit {

#if ENABLE(THREADED_ANIMATION_RESOLUTION)
class RemoteAcceleratedEffectStack : public WebCore::AcceleratedEffectStack
{
public:
    RemoteAcceleratedEffectStack(const WebCore::FloatRect& bounds, Seconds acceleratedTimelineTimeOrigin)
    : m_bounds(bounds)
    , m_acceleratedTimelineTimeOrigin(acceleratedTimelineTimeOrigin)
    { }
    
    void applyEffectsFromScrollingThread(Seconds secondsSinceEpoch) const;
    void initEffectsFromMainThread(PlatformLayer *layer, Seconds currentTime);

    void clear(PlatformLayer *);
    
private:
    WebCore::AcceleratedEffectValues computeValues(Seconds, const WebCore::FloatRect&) const;

    WebCore::FloatRect m_bounds;
    Seconds m_acceleratedTimelineTimeOrigin;

    RetainPtr<CAPresentationModifierGroup> m_presentationModifierGroup;
    RetainPtr<CAPresentationModifier> m_opacityPresentationModifier;
    RetainPtr<CAPresentationModifier> m_transformPresentationModifier;

    RetainPtr<CAPresentationModifierGroup> m_filterPresentationModifierGroup;
    Vector<RetainPtr<CAPresentationModifier>> m_filterPresentationModifiers;
};
#endif

}
