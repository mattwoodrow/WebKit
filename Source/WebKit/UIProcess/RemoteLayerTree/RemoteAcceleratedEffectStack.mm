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

#import "config.h"
#import "RemoteAcceleratedEffectStack.h"

#import <WebCore/PlatformCAFilters.h>
#import <pal/spi/cocoa/QuartzCoreSPI.h>

namespace WebKit {

#if ENABLE(THREADED_ANIMATION_RESOLUTION)
void RemoteAcceleratedEffectStack::applyEffectsFromScrollingThread(Seconds secondsSinceEpoch) const
{
    auto computedValues = computeValues(secondsSinceEpoch - m_acceleratedTimelineTimeOrigin, m_bounds);
    auto computedTransform = computedValues.computedTransformationMatrix(m_bounds);

    auto *opacity = @(computedValues.opacity);
    auto *transform = [NSValue valueWithCATransform3D:computedTransform];

    [m_opacityPresentationModifier setValue:opacity];
    [m_transformPresentationModifier setValue:transform];
    [m_presentationModifierGroup flush];

    if (m_filterPresentationModifierGroup) {
        PlatformCAFilters::updatePresentationModifiersForFilters(computedValues.filter, m_filterPresentationModifiers);
        [m_filterPresentationModifierGroup flush];
    }
}

void RemoteAcceleratedEffectStack::initEffectsFromMainThread(PlatformLayer *layer, Seconds currentTime)
{
    ASSERT(!m_opacityPresentationModifier);
    ASSERT(!m_transformPresentationModifier);

    auto bounds = FloatRect(layer.bounds);
    auto computedValues = computeValues(currentTime, bounds);
    auto computedTransform = computedValues.computedTransformationMatrix(bounds);

    auto *opacity = @(computedValues.opacity);
    auto *transform = [NSValue valueWithCATransform3D:computedTransform];

    // FIXME: Only set transform and opacity if actually present.
    m_presentationModifierGroup = [CAPresentationModifierGroup groupWithCapacity:2];
    m_opacityPresentationModifier = adoptNS([[CAPresentationModifier alloc] initWithKeyPath:@"opacity" initialValue:opacity additive:NO group:m_presentationModifierGroup.get()]);
    m_transformPresentationModifier = adoptNS([[CAPresentationModifier alloc] initWithKeyPath:@"transform" initialValue:transform additive:NO group:m_presentationModifierGroup.get()]);

    [layer addPresentationModifier:m_opacityPresentationModifier.get()];
    [layer addPresentationModifier:m_transformPresentationModifier.get()];

    PlatformCAFilters::setFiltersOnLayer(layer, computedValues.filter);
    m_filterPresentationModifierGroup = PlatformCAFilters::presentationModifiersForFilters(computedValues.filter, m_filterPresentationModifiers);

    for (auto& filterPresentationModifier : m_filterPresentationModifiers)
        [layer addPresentationModifier:filterPresentationModifier.get()];

    [m_presentationModifierGroup flushWithTransaction];
    if (m_filterPresentationModifierGroup)
        [m_filterPresentationModifierGroup flushWithTransaction];
}

AcceleratedEffectValues RemoteAcceleratedEffectStack::computeValues(Seconds currentTime, const FloatRect& bounds) const
{
    auto values = m_baseValues;
    for (auto& effect : m_backdropLayerEffects.isEmpty() ? m_primaryLayerEffects : m_backdropLayerEffects)
        effect->apply(currentTime, values, bounds);
    return values;
}

void RemoteAcceleratedEffectStack::clear(PlatformLayer *layer)
{
    if (!m_presentationModifierGroup) {
        ASSERT(!m_opacityPresentationModifier);
        ASSERT(!m_transformPresentationModifier);
        return;
    }

    ASSERT(m_opacityPresentationModifier);
    ASSERT(m_transformPresentationModifier);

    [layer removePresentationModifier:m_opacityPresentationModifier.get()];
    [layer removePresentationModifier:m_transformPresentationModifier.get()];
    [m_presentationModifierGroup flushWithTransaction];

    m_opacityPresentationModifier = nil;
    m_transformPresentationModifier = nil;
    m_presentationModifierGroup = nil;

    for (auto& filterPresentationModifier : m_filterPresentationModifiers)
        [layer removePresentationModifier:filterPresentationModifier.get()];
    [m_filterPresentationModifierGroup flushWithTransaction];

    m_filterPresentationModifiers.clear();
    m_filterPresentationModifierGroup = nil;
}

#endif

}
