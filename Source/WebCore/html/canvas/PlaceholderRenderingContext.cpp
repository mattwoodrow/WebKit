/*
 * Copyright (C) 2017-2025 Apple Inc. All rights reserved.
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
#include "PlaceholderRenderingContext.h"

#if ENABLE(OFFSCREEN_CANVAS)

#include "GraphicsLayerContentsDisplayDelegate.h"
#include "HTMLCanvasElement.h"
#include "OffscreenCanvas.h"
#include <wtf/TZoneMallocInlines.h>

namespace WebCore {

WTF_MAKE_TZONE_ALLOCATED_IMPL(PlaceholderRenderingContextSource);

Ref<PlaceholderRenderingContextSource> PlaceholderRenderingContextSource::create(PlaceholderRenderingContext& context)
{
    return adoptRef(*new PlaceholderRenderingContextSource(context));
}

PlaceholderRenderingContextSource::PlaceholderRenderingContextSource(PlaceholderRenderingContext& placeholder)
    : m_placeholder(placeholder)
{
}

void PlaceholderRenderingContextSource::setPlaceholderBuffer(ImageBuffer& imageBuffer)
{
    bool delegateCopiedToLayer = false;
    {
        Locker locker { m_lock };
        if (m_delegate) {
            m_delegate->tryCopyToLayer(imageBuffer);
            delegateCopiedToLayer = true;
        }
    }

    RefPtr clone = imageBuffer.clone();
    if (!clone)
        return;
    std::unique_ptr serializedClone = ImageBuffer::sinkIntoSerializedImageBuffer(WTFMove(clone));
    if (!serializedClone)
        return;
    callOnMainThread([protectedThis = Ref { *this }, this, buffer = WTFMove(serializedClone), delegateCopiedToLayer] () mutable {
        assertIsMainThread();
        m_delegateHasCopiedToLayer |= delegateCopiedToLayer;
        if (!m_placeholder)
            return;
        RefPtr imageBuffer = SerializedImageBuffer::sinkIntoImageBuffer(WTFMove(buffer), m_placeholder->protectedCanvas()->scriptExecutionContext()->graphicsClient());
        if (!imageBuffer)
            return;

        if (!m_delegateHasCopiedToLayer) {
            Locker locker { m_lock };
            if (m_delegate) {
                m_delegate->tryCopyToLayer(*imageBuffer);
                m_delegateHasCopiedToLayer = true;
            } else
                m_imageBufferForDelegate = imageBuffer;
        }

        m_placeholder->setPlaceholderBuffer(imageBuffer.releaseNonNull());
    });
}

void PlaceholderRenderingContextSource::setContentsToLayer(GraphicsLayer& layer)
{
    assertIsMainThread();
    Locker locker { m_lock };
    m_delegate = layer.createAsyncContentsDisplayDelegate(m_delegate.get());
    if (m_imageBufferForDelegate) {
        ASSERT(!m_delegateHasCopiedToLayer);
        m_delegate->tryCopyToLayer(*std::exchange(m_imageBufferForDelegate, nullptr));
        m_delegateHasCopiedToLayer = true;
    }
}

WTF_MAKE_TZONE_OR_ISO_ALLOCATED_IMPL(PlaceholderRenderingContext);

std::unique_ptr<PlaceholderRenderingContext> PlaceholderRenderingContext::create(HTMLCanvasElement& element)
{
    return std::unique_ptr<PlaceholderRenderingContext> { new PlaceholderRenderingContext(element) };
}

PlaceholderRenderingContext::PlaceholderRenderingContext(HTMLCanvasElement& canvas)
    : CanvasRenderingContext(canvas, Type::Placeholder)
    , m_source(PlaceholderRenderingContextSource::create(*this))
{
}

HTMLCanvasElement& PlaceholderRenderingContext::canvas() const
{
    return downcast<HTMLCanvasElement>(canvasBase());
}

IntSize PlaceholderRenderingContext::size() const
{
    return canvas().size();
}

void PlaceholderRenderingContext::setContentsToLayer(GraphicsLayer& layer)
{
    m_source->setContentsToLayer(layer);
}

void PlaceholderRenderingContext::setPlaceholderBuffer(Ref<ImageBuffer>&& buffer)
{
    canvasBase().setImageBufferAndMarkDirty(WTFMove(buffer));
}

}

#endif
