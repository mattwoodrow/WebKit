/*
 * Copyright (C) 2018-2023 Apple Inc. All rights reserved.
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
#import "RemoteLayerTreeNode.h"

#import "RemoteLayerTreeLayers.h"
#import <QuartzCore/CALayer.h>
#import <WebCore/WebActionDisablingCALayerDelegate.h>

#if PLATFORM(IOS_FAMILY)
#import <UIKit/UIView.h>
#endif

#if PLATFORM(VISION)
#import <pal/spi/cocoa/QuartzCoreSPI.h>
#endif

#if ENABLE(THREADED_ANIMATION_RESOLUTION)
#import <WebCore/AcceleratedEffectStack.h>
#import <WebCore/ScrollingThread.h>
#endif

namespace WebKit {

static NSString *const WKRemoteLayerTreeNodePropertyKey = @"WKRemoteLayerTreeNode";
#if ENABLE(INTERACTION_REGIONS_IN_EVENT_REGION)
static NSString *const WKInteractionRegionContainerKey = @"WKInteractionRegionContainer";
#endif

RemoteLayerTreeNode::RemoteLayerTreeNode(WebCore::PlatformLayerIdentifier layerID, Markable<WebCore::LayerHostingContextIdentifier> hostIdentifier, RetainPtr<CALayer> layer)
    : m_layerID(layerID)
    , m_remoteContextHostingIdentifier(hostIdentifier)
    , m_layer(WTFMove(layer))
{
    initializeLayer();
    [m_layer setDelegate:[WebActionDisablingCALayerDelegate shared]];
}

#if PLATFORM(IOS_FAMILY)
RemoteLayerTreeNode::RemoteLayerTreeNode(WebCore::PlatformLayerIdentifier layerID, Markable<WebCore::LayerHostingContextIdentifier> hostIdentifier, RetainPtr<UIView> uiView)
    : m_layerID(layerID)
    , m_remoteContextHostingIdentifier(hostIdentifier)
    , m_layer([uiView.get() layer])
    , m_uiView(WTFMove(uiView))
{
    initializeLayer();
}
#endif

RemoteLayerTreeNode::~RemoteLayerTreeNode()
{
#if ENABLE(THREADED_ANIMATION_RESOLUTION)
    if (m_effectStack)
        m_effectStack->clear(layer());
#endif
    [layer() setValue:nil forKey:WKRemoteLayerTreeNodePropertyKey];
#if ENABLE(INTERACTION_REGIONS_IN_EVENT_REGION)
    removeInteractionRegionsContainer();
#endif
}

std::unique_ptr<RemoteLayerTreeNode> RemoteLayerTreeNode::createWithPlainLayer(WebCore::PlatformLayerIdentifier layerID)
{
    RetainPtr<CALayer> layer = adoptNS([[WKCompositingLayer alloc] init]);
    return makeUnique<RemoteLayerTreeNode>(layerID, std::nullopt, WTFMove(layer));
}

void RemoteLayerTreeNode::detachFromParent()
{
#if ENABLE(INTERACTION_REGIONS_IN_EVENT_REGION)
    removeInteractionRegionsContainer();
#endif
#if PLATFORM(IOS_FAMILY)
    if (auto view = uiView()) {
        [view removeFromSuperview];
        return;
    }
#endif
    [layer() removeFromSuperlayer];
}

void RemoteLayerTreeNode::setEventRegion(const WebCore::EventRegion& eventRegion)
{
    m_eventRegion = eventRegion;
}

void RemoteLayerTreeNode::initializeLayer()
{
    [layer() setValue:[NSValue valueWithPointer:this] forKey:WKRemoteLayerTreeNodePropertyKey];
#if ENABLE(INTERACTION_REGIONS_IN_EVENT_REGION)
    if (![layer() isKindOfClass:[CATransformLayer class]])
        [layer() setHitTestsContentsAlphaChannel:YES];
#endif
}

#if ENABLE(INTERACTION_REGIONS_IN_EVENT_REGION)
CALayer* RemoteLayerTreeNode::ensureInteractionRegionsContainer()
{
    if (m_interactionRegionsContainer)
        return m_interactionRegionsContainer.get();

    m_interactionRegionsContainer = adoptNS([[CALayer alloc] init]);
    [m_interactionRegionsContainer setName:@"InteractionRegions Container"];
    [m_interactionRegionsContainer setValue:@(YES) forKey:WKInteractionRegionContainerKey];
    [m_interactionRegionsContainer setDelegate:[WebActionDisablingCALayerDelegate shared]];

    repositionInteractionRegionsContainerIfNeeded();
    propagateInteractionRegionsChangeInHierarchy(InteractionRegionsInSubtree::Yes);

    return m_interactionRegionsContainer.get();
}

void RemoteLayerTreeNode::removeInteractionRegionsContainer()
{
    if (!m_interactionRegionsContainer)
        return;

    [m_interactionRegionsContainer removeFromSuperlayer];
    m_interactionRegionsContainer = nullptr;

    propagateInteractionRegionsChangeInHierarchy(InteractionRegionsInSubtree::Unknown);
}

void RemoteLayerTreeNode::updateInteractionRegionAfterHierarchyChange()
{
    repositionInteractionRegionsContainerIfNeeded();

    bool hasInteractionRegionsDescendant = false;
    for (CALayer *sublayer in layer().sublayers) {
        if (auto *subnode = forCALayer(sublayer)) {
            if (subnode->hasInteractionRegions()) {
                hasInteractionRegionsDescendant = true;
                break;
            }
        }
    }

    if (m_hasInteractionRegionsDescendant == hasInteractionRegionsDescendant)
        return;

    setHasInteractionRegionsDescendant(hasInteractionRegionsDescendant);
    propagateInteractionRegionsChangeInHierarchy(hasInteractionRegionsDescendant ? InteractionRegionsInSubtree::Yes : InteractionRegionsInSubtree::Unknown);
}

bool RemoteLayerTreeNode::hasInteractionRegions() const
{
    return m_hasInteractionRegionsDescendant || m_interactionRegionsContainer;
}

void RemoteLayerTreeNode::repositionInteractionRegionsContainerIfNeeded()
{
    if (!m_interactionRegionsContainer)
        return;

    NSUInteger insertionPoint = 0;
    for (CALayer *sublayer in layer().sublayers) {
        if ([sublayer valueForKey:WKInteractionRegionContainerKey])
            continue;

        if (auto *subnode = forCALayer(sublayer)) {
            if (subnode->hasInteractionRegions())
                break;
        }

        insertionPoint++;
    }

    if ([layer().sublayers objectAtIndex:insertionPoint] == m_interactionRegionsContainer)
        return;

    [m_interactionRegionsContainer removeFromSuperlayer];
    [layer() insertSublayer:m_interactionRegionsContainer.get() atIndex:insertionPoint];
}

void RemoteLayerTreeNode::propagateInteractionRegionsChangeInHierarchy(InteractionRegionsInSubtree interactionRegionsInSubtree)
{
    for (auto* parentNode = forCALayer(layer().superlayer); parentNode; parentNode = forCALayer(parentNode->layer().superlayer)) {
        parentNode->repositionInteractionRegionsContainerIfNeeded();

        bool originalFlag = parentNode->hasInteractionRegionsDescendant();

        if (originalFlag && interactionRegionsInSubtree == InteractionRegionsInSubtree::Yes)
            break;

        if (interactionRegionsInSubtree == InteractionRegionsInSubtree::Yes) {
            parentNode->setHasInteractionRegionsDescendant(true);
            continue;
        }

        bool hasInteractionRegionsDescendant = false;
        for (CALayer *sublayer in parentNode->layer().sublayers) {
            if (auto *subnode = forCALayer(sublayer)) {
                if (subnode->hasInteractionRegions()) {
                    hasInteractionRegionsDescendant = true;
                    break;
                }
            }
        }

        if (originalFlag == hasInteractionRegionsDescendant)
            break;

        parentNode->setHasInteractionRegionsDescendant(hasInteractionRegionsDescendant);
        if (hasInteractionRegionsDescendant)
            interactionRegionsInSubtree = InteractionRegionsInSubtree::Yes;
    }
}
#endif

WebCore::PlatformLayerIdentifier RemoteLayerTreeNode::layerID(CALayer *layer)
{
    auto* node = forCALayer(layer);
    return node ? node->layerID() : WebCore::PlatformLayerIdentifier { };
}

RemoteLayerTreeNode* RemoteLayerTreeNode::forCALayer(CALayer *layer)
{
    return static_cast<RemoteLayerTreeNode*>([[layer valueForKey:WKRemoteLayerTreeNodePropertyKey] pointerValue]);
}

NSString *RemoteLayerTreeNode::appendLayerDescription(NSString *description, CALayer *layer)
{
    NSString *layerDescription = [NSString stringWithFormat:@" layerID = %llu \"%@\"", WebKit::RemoteLayerTreeNode::layerID(layer).object().toUInt64(), layer.name ? layer.name : @""];
    return [description stringByAppendingString:layerDescription];
}

#if ENABLE(THREADED_ANIMATION_RESOLUTION)
void RemoteLayerTreeNode::setAcceleratedEffectsAndBaseValues(const WebCore::AcceleratedEffects& effects, const WebCore::AcceleratedEffectValues& baseValues)
{
    ASSERT(isUIThread());

    WebCore::AcceleratedEffects clonedEffects;
    clonedEffects.reserveCapacity(effects.size());
    for (auto& effect : effects)
        clonedEffects.append(effect->clone());

    auto clonedBaseValues = baseValues.clone();

    // FIXME: need to keep the node alive.
    WebCore::ScrollingThread::dispatch([this, clonedEffects = WTFMove(clonedEffects), clonedBaseValues = WTFMove(clonedBaseValues)] {
        if (!m_effectStack)
            m_effectStack = makeUnique<WebCore::AcceleratedEffectStack>();

        m_effectStack->setEffects(WTFMove(clonedEffects));
        m_effectStack->setBaseValues(WTFMove(clonedBaseValues));
    });
}

bool RemoteLayerTreeNode::hasAnimationEffects() const
{
    ASSERT(!isUIThread());
    return m_effectStack && m_effectStack->hasEffects();
}

void RemoteLayerTreeNode::applyAnimatedEffectStack(Seconds currentTime)
{
    ASSERT(!isUIThread());

    if (!m_effectStack)
        return;

    // The effect stack will have either primary layer effects or
    // backdrop layer effects. We call both application methods.
    m_effectStack->applyPrimaryLayerEffects(layer(), currentTime);
    m_effectStack->applyBackdropLayerEffects(layer(), currentTime);

    // We can clear the effect stack if it's empty, but the previous
    // call to applyEffects() is important so that the base values
    // were re-applied.
    if (!m_effectStack->hasEffects())
        m_effectStack = nullptr;
}
#endif

}
