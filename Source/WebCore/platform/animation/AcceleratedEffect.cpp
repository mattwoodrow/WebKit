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

#include "config.h"
#include "AcceleratedEffect.h"

#if ENABLE(THREADED_ANIMATION_RESOLUTION)

#include "AnimationEffect.h"
#include "CSSPropertyAnimation.h"
#include "CSSPropertyNames.h"
#include "DeclarativeAnimation.h"
#include "Document.h"
#include "LayoutSize.h"
#include "KeyframeEffect.h"
#include "KeyframeList.h"
#include "LayoutSize.h"
#include "WebAnimation.h"
#include "WebAnimationTypes.h"
#include <wtf/IsoMallocInlines.h>

namespace WebCore {

AcceleratedEffectKeyframe AcceleratedEffectKeyframe::clone() const
{
    RefPtr<TimingFunction> clonedTimingFunction;
    if (auto* srcTimingFunction = timingFunction.get())
        clonedTimingFunction = srcTimingFunction->clone();

    return {
        offset,
        values.clone(),
        WTFMove(clonedTimingFunction),
        compositeOperation,
        animatedProperties
    };
};

WTF_MAKE_ISO_ALLOCATED_IMPL(AcceleratedEffect);

static AcceleratedEffectProperty acceleratedPropertyFromCSSProperty(AnimatableProperty property, const Settings& settings)
{
#if ASSERT_ENABLED
    ASSERT(CSSPropertyAnimation::animationOfPropertyIsAccelerated(property, settings));
#else
    UNUSED_PARAM(settings);
#endif
    ASSERT(std::holds_alternative<CSSPropertyID>(property));

    switch (std::get<CSSPropertyID>(property)) {
    case CSSPropertyOpacity:
        return AcceleratedEffectProperty::Opacity;
    case CSSPropertyTransform:
        return AcceleratedEffectProperty::Transform;
    case CSSPropertyTranslate:
        return AcceleratedEffectProperty::Translate;
    case CSSPropertyRotate:
        return AcceleratedEffectProperty::Rotate;
    case CSSPropertyScale:
        return AcceleratedEffectProperty::Scale;
    case CSSPropertyOffsetPath:
        return AcceleratedEffectProperty::OffsetPath;
    case CSSPropertyOffsetDistance:
        return AcceleratedEffectProperty::OffsetDistance;
    case CSSPropertyOffsetPosition:
        return AcceleratedEffectProperty::OffsetPosition;
    case CSSPropertyOffsetAnchor:
        return AcceleratedEffectProperty::OffsetAnchor;
    case CSSPropertyOffsetRotate:
        return AcceleratedEffectProperty::OffsetRotate;
    case CSSPropertyFilter:
        return AcceleratedEffectProperty::Filter;
#if ENABLE(FILTERS_LEVEL_2)
    case CSSPropertyBackdropFilter:
    case CSSPropertyWebkitBackdropFilter:
        return AcceleratedEffectProperty::BackdropFilter;
#endif
    default:
        ASSERT_NOT_REACHED();
        return AcceleratedEffectProperty::Invalid;
    }
}

static CSSPropertyID cssPropertyFromAcceleratedProperty(AcceleratedEffectProperty property)
{
    switch (property) {
    case AcceleratedEffectProperty::Opacity:
        return CSSPropertyOpacity;
    case AcceleratedEffectProperty::Transform:
        return CSSPropertyTransform;
    case AcceleratedEffectProperty::Translate:
        return CSSPropertyTranslate;
    case AcceleratedEffectProperty::Rotate:
        return CSSPropertyRotate;
    case AcceleratedEffectProperty::Scale:
        return CSSPropertyScale;
    case AcceleratedEffectProperty::OffsetPath:
        return CSSPropertyOffsetPath;
    case AcceleratedEffectProperty::OffsetDistance:
        return CSSPropertyOffsetDistance;
    case AcceleratedEffectProperty::OffsetPosition:
        return CSSPropertyOffsetPosition;
    case AcceleratedEffectProperty::OffsetAnchor:
        return CSSPropertyOffsetAnchor;
    case AcceleratedEffectProperty::OffsetRotate:
        return CSSPropertyOffsetRotate;
    case AcceleratedEffectProperty::Filter:
        return CSSPropertyFilter;
#if ENABLE(FILTERS_LEVEL_2)
    case AcceleratedEffectProperty::BackdropFilter:
        return CSSPropertyWebkitBackdropFilter;
#endif
    default:
        ASSERT_NOT_REACHED();
        return CSSPropertyInvalid;
    }
}

Ref<AcceleratedEffect> AcceleratedEffect::create(const KeyframeEffect& effect, const IntRect& borderBoxRect)
{
    return adoptRef(*new AcceleratedEffect(effect, borderBoxRect));
}

Ref<AcceleratedEffect> AcceleratedEffect::create(Vector<AcceleratedEffectKeyframe>&& keyframes, WebAnimationType type, FillMode fill, PlaybackDirection direction, CompositeOperation composite, RefPtr<TimingFunction>&& timingFunction, RefPtr<TimingFunction>&& defaultKeyframeTimingFunction, OptionSet<WebCore::AcceleratedEffectProperty>&& animatedProperties, bool paused, double iterationStart, double iterations, double playbackRate, Seconds delay, Seconds endDelay, Seconds iterationDuration, Seconds activeDuration, Seconds endTime, std::optional<Seconds> startTime, std::optional<Seconds> holdTime)
{
    return adoptRef(*new AcceleratedEffect(WTFMove(keyframes), type, fill, direction, composite, WTFMove(timingFunction), WTFMove(defaultKeyframeTimingFunction), WTFMove(animatedProperties), paused, iterationStart, iterations, playbackRate, delay, endDelay, iterationDuration, activeDuration, endTime, startTime, holdTime));
}

Ref<AcceleratedEffect> AcceleratedEffect::clone() const
{
    auto clonedKeyframes = m_keyframes.map([](const auto& keyframe) {
        return keyframe.clone();
    });

    RefPtr<TimingFunction> clonedTimingFunction;
    if (auto* timingFunction = m_timingFunction.get())
        clonedTimingFunction = timingFunction->clone();

    RefPtr<TimingFunction> clonedDefaultKeyframeTimingFunction;
    if (auto* defaultKeyframeTimingFunction = m_defaultKeyframeTimingFunction.get())
        clonedDefaultKeyframeTimingFunction = defaultKeyframeTimingFunction->clone();

    auto clonedAnimatedProperties = m_animatedProperties;

    return AcceleratedEffect::create(WTFMove(clonedKeyframes), m_animationType, m_fill, m_direction, m_compositeOperation, WTFMove(clonedTimingFunction), WTFMove(clonedDefaultKeyframeTimingFunction), WTFMove(clonedAnimatedProperties), m_paused, m_iterationStart, m_iterations, m_playbackRate, m_delay, m_endDelay, m_iterationDuration, m_activeDuration, m_endTime, m_startTime, m_holdTime);
}

Ref<AcceleratedEffect> AcceleratedEffect::copyWithProperties(OptionSet<AcceleratedEffectProperty>& propertyFilter) const
{
    return adoptRef(*new AcceleratedEffect(*this, propertyFilter));
}

AcceleratedEffect::AcceleratedEffect(const KeyframeEffect& effect, const IntRect& borderBoxRect)
{
    m_fill = effect.fill();
    m_direction = effect.direction();
    m_compositeOperation = effect.composite();
    m_iterationStart = effect.iterationStart();
    m_iterations = effect.iterations();
    m_delay = effect.delay();
    m_endDelay = effect.endDelay();
    m_iterationDuration = effect.iterationDuration();
    m_animationType = effect.animationType();

    ASSERT(effect.animation());
    if (auto* animation = effect.animation()) {
        m_paused = animation->playState() == WebAnimation::PlayState::Paused;
        m_playbackRate = animation->playbackRate();
        ASSERT(animation->holdTime() || animation->startTime());
        m_holdTime = animation->holdTime();
        m_startTime = animation->startTime();
        if (is<DeclarativeAnimation>(animation)) {
            if (auto* defaultKeyframeTimingFunction = downcast<DeclarativeAnimation>(*animation).backingAnimation().timingFunction())
                m_defaultKeyframeTimingFunction = defaultKeyframeTimingFunction;
        }
    }

    m_timingFunction = effect.timingFunction();

    if (m_iterationDuration && m_iterations)
        m_activeDuration = m_iterationDuration * m_iterations;

    m_endTime = std::max(0_s, m_delay + m_activeDuration + m_endDelay);

    ASSERT(effect.document());
    auto& settings = effect.document()->settings();

    for (auto& srcKeyframe : effect.blendingKeyframes()) {
        OptionSet<AcceleratedEffectProperty> animatedProperties;
        for (auto animatedCSSProperty : srcKeyframe.properties()) {
            if (CSSPropertyAnimation::animationOfPropertyIsAccelerated(animatedCSSProperty, settings)) {
                auto acceleratedProperty = acceleratedPropertyFromCSSProperty(animatedCSSProperty, settings);
                animatedProperties.add(acceleratedProperty);
                m_animatedProperties.add(acceleratedProperty);
            }
        }

        if (animatedProperties.isEmpty())
            continue;

        AcceleratedEffectKeyframe acceleratedKeyframe;
        acceleratedKeyframe.offset = srcKeyframe.key();
        acceleratedKeyframe.compositeOperation = srcKeyframe.compositeOperation();
        acceleratedKeyframe.animatedProperties = WTFMove(animatedProperties);

        acceleratedKeyframe.values = [&]() -> AcceleratedEffectValues {
            if (auto* style = srcKeyframe.style())
                return { *style, borderBoxRect };
            return { };
        }();

        acceleratedKeyframe.timingFunction = srcKeyframe.timingFunction();

        m_keyframes.append(WTFMove(acceleratedKeyframe));
    }
}

AcceleratedEffect::AcceleratedEffect(Vector<AcceleratedEffectKeyframe>&& keyframes, WebAnimationType type, FillMode fill, PlaybackDirection direction, CompositeOperation composite, RefPtr<TimingFunction>&& timingFunction, RefPtr<TimingFunction>&& defaultKeyframeTimingFunction, OptionSet<WebCore::AcceleratedEffectProperty>&& animatedProperties, bool paused, double iterationStart, double iterations, double playbackRate, Seconds delay, Seconds endDelay, Seconds iterationDuration, Seconds activeDuration, Seconds endTime, std::optional<Seconds> startTime, std::optional<Seconds> holdTime)
    : m_keyframes(WTFMove(keyframes))
    , m_animationType(type)
    , m_fill(fill)
    , m_direction(direction)
    , m_compositeOperation(composite)
    , m_timingFunction(WTFMove(timingFunction))
    , m_defaultKeyframeTimingFunction(WTFMove(defaultKeyframeTimingFunction))
    , m_animatedProperties(WTFMove(animatedProperties))
    , m_paused(paused)
    , m_iterationStart(iterationStart)
    , m_iterations(iterations)
    , m_playbackRate(playbackRate)
    , m_delay(delay)
    , m_endDelay(endDelay)
    , m_iterationDuration(iterationDuration)
    , m_activeDuration(activeDuration)
    , m_endTime(endTime)
    , m_startTime(startTime)
    , m_holdTime(holdTime)
{
}

AcceleratedEffect::AcceleratedEffect(const AcceleratedEffect& source, OptionSet<AcceleratedEffectProperty>& propertyFilter)
{
    m_animationType = source.m_animationType;
    m_fill = source.m_fill;
    m_direction = source.m_direction;
    m_compositeOperation = source.m_compositeOperation;
    m_paused = source.m_paused;
    m_iterationStart = source.m_iterationStart;
    m_iterations = source.m_iterations;
    m_playbackRate = source.m_playbackRate;
    m_delay = source.m_delay;
    m_endDelay = source.m_endDelay;
    m_iterationDuration = source.m_iterationDuration;
    m_activeDuration = source.m_activeDuration;
    m_endTime = source.m_endTime;
    m_startTime = source.m_startTime;
    m_holdTime = source.m_holdTime;

    m_timingFunction = source.m_timingFunction.copyRef();
    m_defaultKeyframeTimingFunction = source.m_defaultKeyframeTimingFunction.copyRef();

    for (auto& srcKeyframe : source.m_keyframes) {
        auto& animatedProperties = srcKeyframe.animatedProperties;
        if (!animatedProperties.containsAny(propertyFilter))
            continue;

        AcceleratedEffectKeyframe keyframe;
        keyframe.offset = srcKeyframe.offset;
        keyframe.values = srcKeyframe.values;
        keyframe.compositeOperation = srcKeyframe.compositeOperation;
        keyframe.animatedProperties = srcKeyframe.animatedProperties & propertyFilter;
        keyframe.timingFunction = srcKeyframe.timingFunction.copyRef();

        m_animatedProperties.add(keyframe.animatedProperties);
        m_keyframes.append(WTFMove(keyframe));
    }
}

std::optional<double> AcceleratedEffect::computeIterationProgress(Seconds currentTime) const
{
    auto localTime = [&]() -> Seconds {
        ASSERT(m_holdTime || m_startTime);
        if (m_holdTime)
            return *m_holdTime;
        return (currentTime - *m_startTime) * m_playbackRate;
    }();

    auto phase = [this, localTime]() -> AnimationEffectPhase {
        // 3.5.5. Animation effect phases and states
        // https://drafts.csswg.org/web-animations-1/#animation-effect-phases-and-states

        bool animationIsBackwards = m_playbackRate < 0;
        auto beforeActiveBoundaryTime = std::max(std::min(m_delay, m_endTime), 0_s);
        auto activeAfterBoundaryTime = std::max(std::min(m_delay + m_activeDuration, m_endTime), 0_s);

        // An animation effect is in the before phase if the animation effect’s local time is not unresolved and
        // either of the following conditions are met:
        //     1. the local time is less than the before-active boundary time, or
        //     2. the animation direction is ‘backwards’ and the local time is equal to the before-active boundary time.
        if ((localTime + timeEpsilon) < beforeActiveBoundaryTime || (animationIsBackwards && std::abs(localTime.microseconds() - beforeActiveBoundaryTime.microseconds()) < timeEpsilon.microseconds()))
            return AnimationEffectPhase::Before;

        // An animation effect is in the after phase if the animation effect’s local time is not unresolved and
        // either of the following conditions are met:
        //     1. the local time is greater than the active-after boundary time, or
        //     2. the animation direction is ‘forwards’ and the local time is equal to the active-after boundary time.
        if ((localTime - timeEpsilon) > activeAfterBoundaryTime || (!animationIsBackwards && std::abs(localTime.microseconds() - activeAfterBoundaryTime.microseconds()) < timeEpsilon.microseconds()))
            return AnimationEffectPhase::After;

        // An animation effect is in the active phase if the animation effect’s local time is not unresolved and it is not
        // in either the before phase nor the after phase.
        // (No need to check, we've already established that local time was resolved).
        return AnimationEffectPhase::Active;
    }();

    auto activeTime = [this, localTime, phase]() -> std::optional<Seconds> {
        // 3.8.3.1. Calculating the active time
        // https://drafts.csswg.org/web-animations-1/#calculating-the-active-time

        // The active time is based on the local time and start delay. However, it is only defined
        // when the animation effect should produce an output and hence depends on its fill mode
        // and phase as follows,

        // If the animation effect is in the before phase, the result depends on the first matching
        // condition from the following,
        if (phase == AnimationEffectPhase::Before) {
            // If the fill mode is backwards or both, return the result of evaluating
            // max(local time - start delay, 0).
            if (m_fill == FillMode::Backwards || m_fill == FillMode::Both)
                return std::max(localTime - m_delay, 0_s);
            // Otherwise, return an unresolved time value.
            return std::nullopt;
        }

        // If the animation effect is in the active phase, return the result of evaluating local time - start delay.
        if (phase == AnimationEffectPhase::Active)
            return localTime - m_delay;

        // If the animation effect is in the after phase, the result depends on the first matching
        // condition from the following,
        if (phase == AnimationEffectPhase::After) {
            // If the fill mode is forwards or both, return the result of evaluating
            // max(min(local time - start delay, active duration), 0).
            if (m_fill == FillMode::Forwards || m_fill == FillMode::Both)
                return std::max(std::min(localTime - m_delay, m_activeDuration), 0_s);
            // Otherwise, return an unresolved time value.
            return std::nullopt;
        }

        // Otherwise (the local time is unresolved), return an unresolved time value.
        return std::nullopt;
    }();

    auto overallProgress = [this, phase, activeTime]() -> std::optional<double> {
        // 3.8.3.2. Calculating the overall progress
        // https://drafts.csswg.org/web-animations-1/#calculating-the-overall-progress

        // The overall progress describes the number of iterations that have completed (including partial iterations) and is defined as follows:

        // 1. If the active time is unresolved, return unresolved.
        if (!activeTime)
            return std::nullopt;

        // 2. Calculate an initial value for overall progress based on the first matching condition from below,
        double overallProgress;

        if (!m_iterationDuration) {
            // If the iteration duration is zero, if the animation effect is in the before phase, let overall progress be zero,
            // otherwise, let it be equal to the iteration count.
            overallProgress = phase == AnimationEffectPhase::Before ? 0 : m_iterations;
        } else {
            // Otherwise, let overall progress be the result of calculating active time / iteration duration.
            overallProgress = secondsToWebAnimationsAPITime(*activeTime) / secondsToWebAnimationsAPITime(m_iterationDuration);
        }

        // 3. Return the result of calculating overall progress + iteration start.
        overallProgress += m_iterationStart;
        return std::abs(overallProgress);
    }();

    auto simpleIterationProgress = [this, overallProgress, phase, activeTime]() -> std::optional<double> {
        // 3.8.3.3. Calculating the simple iteration progress
        // https://drafts.csswg.org/web-animations-1/#calculating-the-simple-iteration-progress

        // The simple iteration progress is a fraction of the progress through the current iteration that
        // ignores transformations to the time introduced by the playback direction or timing functions
        // applied to the effect, and is calculated as follows:

        // 1. If the overall progress is unresolved, return unresolved.
        if (!overallProgress)
            return std::nullopt;

        // 2. If overall progress is infinity, let the simple iteration progress be iteration start % 1.0,
        // otherwise, let the simple iteration progress be overall progress % 1.0.
        double simpleIterationProgress = std::isinf(*overallProgress) ? fmod(m_iterationStart, 1) : fmod(*overallProgress, 1);

        // 3. If all of the following conditions are true,
        //
        // the simple iteration progress calculated above is zero, and
        // the animation effect is in the active phase or the after phase, and
        // the active time is equal to the active duration, and
        // the iteration count is not equal to zero.
        // let the simple iteration progress be 1.0.
        if (!simpleIterationProgress && (phase == AnimationEffectPhase::Active || phase == AnimationEffectPhase::After) && std::abs(activeTime->microseconds() - m_activeDuration.microseconds()) < timeEpsilon.microseconds() && m_iterations)
            return 1;

        return simpleIterationProgress;
    }();

    auto currentIteration = [this, activeTime, phase, simpleIterationProgress, overallProgress]() -> std::optional<double> {
        // 3.8.4. Calculating the current iteration
        // https://drafts.csswg.org/web-animations-1/#calculating-the-current-iteration

        // The current iteration can be calculated using the following steps:

        // 1. If the active time is unresolved, return unresolved.
        if (!activeTime)
            return std::nullopt;

        // 2. If the animation effect is in the after phase and the iteration count is infinity, return infinity.
        if (phase == AnimationEffectPhase::After && std::isinf(m_iterations))
            return std::numeric_limits<double>::infinity();

        // 3. If the simple iteration progress is 1.0, return floor(overall progress) - 1.
        if (*simpleIterationProgress == 1)
            return floor(*overallProgress) - 1;

        // 4. Otherwise, return floor(overall progress).
        return floor(*overallProgress);
    }();

    enum ComputedDirection : uint8_t { Forwards, Reverse };
    auto currentDirection = [this, currentIteration]() -> ComputedDirection {
        // 3.9.1. Calculating the directed progress
        // https://drafts.csswg.org/web-animations-1/#calculating-the-directed-progress

        // If playback direction is normal, let the current direction be forwards.
        if (m_direction == PlaybackDirection::Normal)
            return ComputedDirection::Forwards;

        // If playback direction is reverse, let the current direction be reverse.
        if (m_direction == PlaybackDirection::Reverse)
            return ComputedDirection::Reverse;

        if (!currentIteration)
            return ComputedDirection::Forwards;

        // Otherwise, let d be the current iteration.
        auto d = *currentIteration;
        // If playback direction is alternate-reverse increment d by 1.
        if (m_direction == PlaybackDirection::AlternateReverse)
            d++;
        // If d % 2 == 0, let the current direction be forwards, otherwise let the current direction be reverse.
        // If d is infinity, let the current direction be forwards.
        if (std::isinf(d) || !fmod(d, 2))
            return ComputedDirection::Forwards;
        return ComputedDirection::Reverse;
    }();

    auto directedProgress = [simpleIterationProgress, currentDirection]() -> std::optional<double> {
        // 3.9.1. Calculating the directed progress
        // https://drafts.csswg.org/web-animations-1/#calculating-the-directed-progress

        // The directed progress is calculated from the simple iteration progress using the following steps:

        // 1. If the simple iteration progress is unresolved, return unresolved.
        if (!simpleIterationProgress)
            return std::nullopt;

        // 2. Calculate the current direction (we implement this as a separate method).

        // 3. If the current direction is forwards then return the simple iteration progress.
        if (currentDirection == ComputedDirection::Forwards)
            return *simpleIterationProgress;

        // Otherwise, return 1.0 - simple iteration progress.
        return 1 - *simpleIterationProgress;
    }();

    return [this, directedProgress, currentDirection, phase]() -> std::optional<double> {
        // 3.10.1. Calculating the transformed progress
        // https://drafts.csswg.org/web-animations-1/#calculating-the-transformed-progress

        // The transformed progress is calculated from the directed progress using the following steps:
        //
        // 1. If the directed progress is unresolved, return unresolved.
        if (!directedProgress)
            return std::nullopt;

        if (auto iterationDuration = m_iterationDuration.seconds()) {
            ASSERT(m_timingFunction);

            bool before = false;
            // 2. Calculate the value of the before flag as follows:
            if (is<StepsTimingFunction>(m_timingFunction)) {
                // 1. Determine the current direction using the procedure defined in §3.9.1 Calculating the directed progress.
                // 2. If the current direction is forwards, let going forwards be true, otherwise it is false.
                bool goingForwards = currentDirection == ComputedDirection::Forwards;
                // 3. The before flag is set if the animation effect is in the before phase and going forwards is true;
                //    or if the animation effect is in the after phase and going forwards is false.
                before = (phase == AnimationEffectPhase::Before && goingForwards) || (phase == AnimationEffectPhase::After && !goingForwards);
            }

            // 3. Return the result of evaluating the animation effect’s timing function passing directed progress as the
            //    input progress value and before flag as the before flag.
            return m_timingFunction->transformProgress(*directedProgress, iterationDuration, before);
        }

        return *directedProgress;
    }();
}

static void blend(AcceleratedEffectProperty property, AcceleratedEffectValues& output, const AcceleratedEffectValues& from, const AcceleratedEffectValues& to, const BlendingContext& blendingContext, const FloatRect& bounds)
{
    switch (property) {
    case AcceleratedEffectProperty::Opacity:
        output.opacity = blend(from.opacity, to.opacity, blendingContext);
        break;
    case AcceleratedEffectProperty::Transform: {
        LayoutSize boxSize { bounds.size() };
        output.transform = to.transform.blend(from.transform, blendingContext, boxSize);
        break;
    }
    case AcceleratedEffectProperty::Translate:
        if (auto toTranslate = to.translate)
            output.translate = toTranslate->blend(from.translate.get(), blendingContext);
        break;
    case AcceleratedEffectProperty::Rotate:
        if (auto toRotate = to.rotate)
            output.rotate = toRotate->blend(from.rotate.get(), blendingContext);
        break;
    case AcceleratedEffectProperty::Scale:
        if (auto toScale = to.scale)
            output.scale = toScale->blend(from.scale.get(), blendingContext);
        break;
    case AcceleratedEffectProperty::OffsetPath:
        if (auto fromOffsetPath = from.offsetPath)
            output.offsetPath = fromOffsetPath->blend(to.offsetPath.get(), blendingContext);
        break;
    case AcceleratedEffectProperty::OffsetDistance:
        output.offsetDistance = blend(from.offsetDistance, to.offsetDistance, blendingContext);
        break;
    case AcceleratedEffectProperty::OffsetPosition:
        output.offsetPosition = blend(from.offsetPosition, to.offsetPosition, blendingContext);
        break;
    case AcceleratedEffectProperty::OffsetAnchor:
        output.offsetAnchor = blend(from.offsetAnchor, to.offsetAnchor, blendingContext);
        break;
    case AcceleratedEffectProperty::OffsetRotate:
        output.offsetRotate = from.offsetRotate.blend(to.offsetRotate, blendingContext);
        break;
    case AcceleratedEffectProperty::Filter:
        output.filter = to.filter.blend(from.filter, blendingContext);
        break;
#if ENABLE(FILTERS_LEVEL_2)
    case AcceleratedEffectProperty::BackdropFilter:
        output.backdropFilter = to.backdropFilter.blend(from.backdropFilter, blendingContext);
        break;
#endif
    case AcceleratedEffectProperty::Invalid:
        ASSERT_NOT_REACHED();
        break;
    }
}

std::optional<std::tuple<const AcceleratedEffectValues, const AcceleratedEffectValues, double>> AcceleratedEffect::keyframeValuesAndTransformedProgress(double iterationProgress, AcceleratedEffectProperty property, const AcceleratedEffectKeyframe& propertySpecificKeyframeWithZeroOffset, const AcceleratedEffectKeyframe& propertySpecificKeyframeWithOneOffset, const FloatRect& bounds) const
{
    // 1. If iteration progress is unresolved abort this procedure.
    // 2. Let target property be the longhand property for which the effect value is to be calculated.
    // 3. If animation type of the target property is not animatable abort this procedure since the effect cannot be applied.
    // 4. Define the neutral value for composition as a value which, when combined with an underlying value using the add composite operation,
    //    produces the underlying value.

    // 5. Let property-specific keyframes be the result of getting the set of computed keyframes for this keyframe effect.
    // 6. Remove any keyframes from property-specific keyframes that do not have a property value for target property.
    unsigned numberOfKeyframesWithZeroOffset = 0;
    unsigned numberOfKeyframesWithOneOffset = 0;
    Vector<const AcceleratedEffectKeyframe*> propertySpecificKeyframes;
    for (auto& keyframe : m_keyframes) {
        auto offset = keyframe.offset;
        if (!keyframe.animatedProperties.contains(property))
            continue;
        if (!offset)
            numberOfKeyframesWithZeroOffset++;
        if (offset == 1)
            numberOfKeyframesWithOneOffset++;
        propertySpecificKeyframes.append(&keyframe);
    }

    // 7. If property-specific keyframes is empty, return underlying value.
    if (propertySpecificKeyframes.isEmpty())
        return std::nullopt;

    auto hasImplicitZeroKeyframe = !numberOfKeyframesWithZeroOffset;
    auto hasImplicitOneKeyframe = !numberOfKeyframesWithOneOffset;

    // 8. If there is no keyframe in property-specific keyframes with a computed keyframe offset of 0, create a new keyframe with a computed keyframe
    //    offset of 0, a property value set to the neutral value for composition, and a composite operation of add, and prepend it to the beginning of
    //    property-specific keyframes.
    if (hasImplicitZeroKeyframe) {
        propertySpecificKeyframes.insert(0, &propertySpecificKeyframeWithZeroOffset);
        numberOfKeyframesWithZeroOffset = 1;
    }

    // 9. Similarly, if there is no keyframe in property-specific keyframes with a computed keyframe offset of 1, create a new keyframe with a computed
    //    keyframe offset of 1, a property value set to the neutral value for composition, and a composite operation of add, and append it to the end of
    //    property-specific keyframes.
    if (hasImplicitOneKeyframe) {
        propertySpecificKeyframes.append(&propertySpecificKeyframeWithOneOffset);
        numberOfKeyframesWithOneOffset = 1;
    }

    // 10. Let interval endpoints be an empty sequence of keyframes.
    Vector<const AcceleratedEffectKeyframe*> intervalEndpoints;

    // 11. Populate interval endpoints by following the steps from the first matching condition from below:
    if (iterationProgress < 0 && numberOfKeyframesWithZeroOffset > 1) {
        // If iteration progress < 0 and there is more than one keyframe in property-specific keyframes with a computed keyframe offset of 0,
        // Add the first keyframe in property-specific keyframes to interval endpoints.
        intervalEndpoints.append(propertySpecificKeyframes.first());
    } else if (iterationProgress >= 1 && numberOfKeyframesWithOneOffset > 1) {
        // If iteration progress ≥ 1 and there is more than one keyframe in property-specific keyframes with a computed keyframe offset of 1,
        // Add the last keyframe in property-specific keyframes to interval endpoints.
        intervalEndpoints.append(propertySpecificKeyframes.last());
    } else {
        // Otherwise,
        // 1. Append to interval endpoints the last keyframe in property-specific keyframes whose computed keyframe offset is less than or equal
        //    to iteration progress and less than 1. If there is no such keyframe (because, for example, the iteration progress is negative),
        //    add the last keyframe whose computed keyframe offset is 0.
        // 2. Append to interval endpoints the next keyframe in property-specific keyframes after the one added in the previous step.
        size_t indexOfLastKeyframeWithZeroOffset = 0;
        int indexOfFirstKeyframeToAddToIntervalEndpoints = -1;
        for (size_t i = 0; i < propertySpecificKeyframes.size(); ++i) {
            auto offset = propertySpecificKeyframes[i]->offset;
            if (!offset)
                indexOfLastKeyframeWithZeroOffset = i;
            if (offset <= iterationProgress && offset < 1)
                indexOfFirstKeyframeToAddToIntervalEndpoints = i;
            else
                break;
        }

        if (indexOfFirstKeyframeToAddToIntervalEndpoints >= 0) {
            intervalEndpoints.append(propertySpecificKeyframes[indexOfFirstKeyframeToAddToIntervalEndpoints]);
            intervalEndpoints.append(propertySpecificKeyframes[indexOfFirstKeyframeToAddToIntervalEndpoints + 1]);
        } else {
            ASSERT(indexOfLastKeyframeWithZeroOffset < propertySpecificKeyframes.size() - 1);
            intervalEndpoints.append(propertySpecificKeyframes[indexOfLastKeyframeWithZeroOffset]);
            intervalEndpoints.append(propertySpecificKeyframes[indexOfLastKeyframeWithZeroOffset + 1]);
        }
    }

    auto& startKeyframe = *intervalEndpoints.first();
    auto& endKeyframe = *intervalEndpoints.last();

    auto startKeyframeValues = startKeyframe.values;
    auto endKeyframeValues = endKeyframe.values;

    // 12. For each keyframe in interval endpoints:
    //     If keyframe has a composite operation that is not replace, or keyframe has no composite operation and the
    //     composite operation of this keyframe effect is not replace, then perform the following steps:
    //         1. Let composite operation to use be the composite operation of keyframe, or if it has none, the composite
    //            operation of this keyframe effect.
    //         2. Let value to combine be the property value of target property specified on keyframe.
    //         3. Replace the property value of target property on keyframe with the result of combining underlying value
    //            (Va) and value to combine (Vb) using the procedure for the composite operation to use corresponding to the
    //            target property’s animation type.
    if (CSSPropertyAnimation::isPropertyAdditiveOrCumulative(cssPropertyFromAcceleratedProperty(property))) {
        // Only do this for the 0 keyframe if it was provided explicitly, since otherwise we want to use the "neutral value
        // for composition" which really means we don't want to do anything but rather just use the underlying style which
        // is already set on startKeyframe.
        if (!startKeyframe.offset && !hasImplicitZeroKeyframe) {
            auto startKeyframeCompositeOperation = startKeyframe.compositeOperation.value_or(m_compositeOperation);
            if (startKeyframeCompositeOperation != CompositeOperation::Replace)
                blend(property, startKeyframeValues, propertySpecificKeyframeWithZeroOffset.values, startKeyframe.values, { 1, false, startKeyframeCompositeOperation }, bounds);
        }

        // Only do this for the 1 keyframe if it was provided explicitly, since otherwise we want to use the "neutral value
        // for composition" which really means we don't want to do anything but rather just use the underlying style which
        // is already set on endKeyframe.
        if (endKeyframe.offset == 1 && !hasImplicitOneKeyframe) {
            auto endKeyframeCompositeOperation = endKeyframe.compositeOperation.value_or(m_compositeOperation);
            if (endKeyframeCompositeOperation != CompositeOperation::Replace)
                blend(property, endKeyframeValues, propertySpecificKeyframeWithZeroOffset.values, endKeyframe.values, { 1, false, endKeyframeCompositeOperation }, bounds);
        }
    }

    // 13. If there is only one keyframe in interval endpoints return the property value of target property on that keyframe.
    if (intervalEndpoints.size() == 1)
        return std::make_optional(std::make_tuple(startKeyframeValues, startKeyframeValues, iterationProgress));

    // 14. Let start offset be the computed keyframe offset of the first keyframe in interval endpoints.
    auto startOffset = startKeyframe.offset;

    // 15. Let end offset be the computed keyframe offset of last keyframe in interval endpoints.
    auto endOffset = endKeyframe.offset;

    // 16. Let interval distance be the result of evaluating (iteration progress - start offset) / (end offset - start offset).
    auto intervalDistance = (iterationProgress - startOffset) / (endOffset - startOffset);

    // 17. Let transformed distance be the result of evaluating the timing function associated with the first keyframe in interval endpoints
    //     passing interval distance as the input progress.
    auto transformedDistance = intervalDistance;
    if (auto duration = m_iterationDuration) {
        auto rangeDuration = (endOffset - startOffset) * duration.seconds();
        if (auto* timingFunction = timingFunctionForKeyframe(startKeyframe))
            transformedDistance = timingFunction->transformProgress(intervalDistance, rangeDuration);
    }

    return std::make_optional(std::make_tuple(startKeyframeValues, endKeyframeValues, transformedDistance));
};

const TimingFunction* AcceleratedEffect::timingFunctionForKeyframe(const AcceleratedEffectKeyframe& keyframe) const
{
    if (m_animationType == WebAnimationType::CSSAnimation || m_animationType == WebAnimationType::CSSTransition) {
        // If we're dealing with a CSS Animation, the timing function may be specified on the keyframe.
        if (m_animationType == WebAnimationType::CSSAnimation) {
            if (auto& timingFunction = keyframe.timingFunction)
                return timingFunction.get();
        }

        // Failing that, or for a CSS Transition, the timing function is the default timing function.
        return m_defaultKeyframeTimingFunction.get();
    }

    return keyframe.timingFunction.get();
}

void AcceleratedEffect::apply(Seconds currentTime, AcceleratedEffectValues& values, const FloatRect& bounds)
{
    auto iterationProgress = computeIterationProgress(currentTime);
    if (!iterationProgress)
        return;

    // In the case of CSS Transitions we already know that there are only two keyframes, one where offset=0 and one where offset=1,
    // and only a single CSS property so we can simply blend based on the style available on those keyframes with the provided iteration
    // progress which already accounts for the transition's timing function.
    if (m_animationType == WebAnimationType::CSSTransition) {
        ASSERT(m_animatedProperties.hasExactlyOneBitSet());
        blend(*m_animatedProperties.begin(), values, m_keyframes.first().values, m_keyframes.last().values, { *iterationProgress, false, m_compositeOperation }, bounds);
        return;
    }

    auto propertySpecificKeyframeWithOffset = [&](double offset) {
        AcceleratedEffectKeyframe keyframe;
        keyframe.offset = offset;
        keyframe.values = values;
        return keyframe;
    };

    auto propertySpecificKeyframeWithZeroOffset = propertySpecificKeyframeWithOffset(0);
    auto propertySpecificKeyframeWithOneOffset = propertySpecificKeyframeWithOffset(1);

    for (auto animatedProperty : m_animatedProperties) {
        auto keyframeValuesAndTransformedProgress = this->keyframeValuesAndTransformedProgress(*iterationProgress, animatedProperty, propertySpecificKeyframeWithZeroOffset, propertySpecificKeyframeWithOneOffset, bounds);
        if (!keyframeValuesAndTransformedProgress)
            return;

        auto& [from, to, progress] = *keyframeValuesAndTransformedProgress;
        blend(animatedProperty, values, from, to, { progress }, bounds);
    }
}

bool AcceleratedEffect::animatesTransformRelatedProperty() const
{
    return m_animatedProperties.containsAny({
        AcceleratedEffectProperty::Transform,
        AcceleratedEffectProperty::Translate,
        AcceleratedEffectProperty::Rotate,
        AcceleratedEffectProperty::Scale,
        AcceleratedEffectProperty::OffsetPath,
        AcceleratedEffectProperty::OffsetDistance,
        AcceleratedEffectProperty::OffsetPosition,
        AcceleratedEffectProperty::OffsetAnchor,
        AcceleratedEffectProperty::OffsetRotate
    });
}

} // namespace WebCore

#endif // ENABLE(THREADED_ANIMATION_RESOLUTION)
