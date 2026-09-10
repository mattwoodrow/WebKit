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

#include "ContextDestructionObserverInlines.h"
#include "GraphicsLayer.h"
#include "GraphicsLayerContentsDisplayDelegate.h"
#include "Document.h"
#include "HTMLCanvasElement.h"
#include "NativeImage.h"
#include "OffscreenCanvas.h"
#include <wtf/HashMap.h>
#include <wtf/NeverDestroyed.h>
#include <wtf/TZoneMallocInlines.h>

namespace WebCore {

WTF_MAKE_TZONE_ALLOCATED_IMPL(PlaceholderRenderingContextSource);

using PlaceholderRenderingContextSourceMap = HashMap<PlaceholderRenderingContextIdentifier, ThreadSafeWeakPtr<PlaceholderRenderingContextSource>>;

static Lock& sourceMapLock()
{
    static NeverDestroyed<Lock> lock;
    return lock.get();
}

static PlaceholderRenderingContextSourceMap& sourceMap() WTF_REQUIRES_LOCK(sourceMapLock())
{
    static NeverDestroyed<PlaceholderRenderingContextSourceMap> map;
    return map.get();
}

static PlaceholderRenderingContextSource::RemoteSourceFactory& remoteSourceFactory()
{
    static NeverDestroyed<PlaceholderRenderingContextSource::RemoteSourceFactory> factory;
    return factory.get();
}

void PlaceholderRenderingContextSource::setRemoteSourceFactory(RemoteSourceFactory&& factory)
{
    remoteSourceFactory() = WTF::move(factory);
}

static PlaceholderRenderingContextSource::DestructionHandler& destructionHandler()
{
    static NeverDestroyed<PlaceholderRenderingContextSource::DestructionHandler> handler;
    return handler.get();
}

void PlaceholderRenderingContextSource::setDestructionHandler(DestructionHandler&& handler)
{
    destructionHandler() = WTF::move(handler);
}

static PlaceholderRenderingContextSource::LayerChangeHandler& layerChangeHandler()
{
    static NeverDestroyed<PlaceholderRenderingContextSource::LayerChangeHandler> handler;
    return handler.get();
}

void PlaceholderRenderingContextSource::setLayerChangeHandler(LayerChangeHandler&& handler)
{
    layerChangeHandler() = WTF::move(handler);
}

RefPtr<PlaceholderRenderingContextSource> PlaceholderRenderingContextSource::createRemoteSource(PlaceholderRenderingContextIdentifier identifier)
{
    auto& factory = remoteSourceFactory();
    if (!factory)
        return nullptr;
    return factory(identifier);
}

RefPtr<PlaceholderRenderingContextSource> PlaceholderRenderingContextSource::sourceWithIdentifier(PlaceholderRenderingContextIdentifier identifier)
{
    Locker locker { sourceMapLock() };
    auto iterator = sourceMap().find(identifier);
    if (iterator == sourceMap().end())
        return nullptr;
    return iterator->value.get();
}

Ref<PlaceholderRenderingContextSource> PlaceholderRenderingContextSource::create(PlaceholderRenderingContext& context)
{
    return adoptRef(*new PlaceholderRenderingContextSource(context));
}

PlaceholderRenderingContextSource::PlaceholderRenderingContextSource(PlaceholderRenderingContextIdentifier identifier)
    : m_identifier(identifier)
{
}

PlaceholderRenderingContextSource::PlaceholderRenderingContextSource(PlaceholderRenderingContext& placeholder)
    : m_identifier(PlaceholderRenderingContextIdentifier::generate())
    , m_placeholder(placeholder)
{
    Locker locker { sourceMapLock() };
    sourceMap().add(m_identifier, ThreadSafeWeakPtr<PlaceholderRenderingContextSource> { *this });
}

PlaceholderRenderingContextSource::~PlaceholderRenderingContextSource()
{
    // Only sources that own a placeholder in this process are registered; remote sources are
    // identified by an identifier generated in the process that owns the placeholder.
    if (m_identifier.processIdentifier() != Process::identifier())
        return;
    {
        Locker locker { sourceMapLock() };
        sourceMap().remove(m_identifier);
    }
    // The last reference can be dropped on the thread holding the offscreen half, so hop to the
    // main thread where the WebKit layer's IPC connection lives.
    ensureOnMainThread([identifier = m_identifier] {
        if (auto& handler = destructionHandler())
            handler(identifier);
    });
}

void PlaceholderRenderingContextSource::setPlaceholderBuffer(ImageBuffer& imageBuffer, bool originClean, bool opaque, LayerUpdate layerUpdate)
{
    auto bufferVersion = ++m_bufferVersion;
    {
        Locker locker { m_lock };
        if (m_delegate) {
            if (layerUpdate == LayerUpdate::Needed)
                m_delegate->tryCopyToLayer(imageBuffer, opaque);
            m_delegateBufferVersion = bufferVersion;
        }
    }

    RefPtr clone = imageBuffer.clone();
    if (!clone)
        return;
    std::unique_ptr serializedClone = ImageBuffer::sinkIntoSerializedImageBuffer(WTF::move(clone));
    if (!serializedClone)
        return;
    callOnMainThread([weakPlaceholder = m_placeholder, buffer = WTF::move(serializedClone), bufferVersion, originClean, opaque] () mutable {
        assertIsMainThread();
        RefPtr placeholder = weakPlaceholder.get();
        if (!placeholder)
            return;
        RefPtr imageBuffer = SerializedImageBuffer::sinkIntoImageBuffer(WTF::move(buffer), protect(protect(placeholder->canvas())->scriptExecutionContext())->graphicsClient());
        if (!imageBuffer)
            return;
        Ref source = placeholder->source();
        {
            Locker locker { source->m_lock };
            if (source->m_delegate && source->m_delegateBufferVersion < bufferVersion) {
                // Compare the versions, so that possibly already historical buffer in this
                // main thread task does not override the newest buffer that the worker thread
                // already set.
                source->m_delegate->tryCopyToLayer(*imageBuffer, opaque);
                source->m_delegateBufferVersion = bufferVersion;
            }
        }

        placeholder->setPlaceholderBuffer(imageBuffer.releaseNonNull(), originClean, opaque);
        source->m_placeholderBufferVersion = bufferVersion;
    });
}

void PlaceholderRenderingContextSource::setPlaceholderBufferFromRemoteProcess(Ref<ImageBuffer>&& imageBuffer, bool originClean, bool opaque, LayerUpdate layerUpdate)
{
    assertIsMainThread();
    RefPtr placeholder = m_placeholder.get();
    if (!placeholder)
        return;

    auto bufferVersion = ++m_bufferVersion;
    {
        Locker locker { m_lock };
        if (m_delegate) {
            if (layerUpdate == LayerUpdate::Needed)
                m_delegate->tryCopyToLayer(imageBuffer, opaque);
            m_delegateBufferVersion = bufferVersion;
        }
    }

    placeholder->setPlaceholderBuffer(WTF::move(imageBuffer), originClean, opaque);
    m_placeholderBufferVersion = bufferVersion;
}

Document* PlaceholderRenderingContextSource::placeholderDocument() const
{
    assertIsMainThread();
    RefPtr placeholder = m_placeholder.get();
    if (!placeholder)
        return nullptr;
    return &placeholder->canvas().document();
}

void PlaceholderRenderingContextSource::setContentsToLayer(GraphicsLayer& layer, ImageBuffer* buffer, bool opaque)
{
    assertIsMainThread();
    std::optional<PlatformLayerIdentifier> layerID;
    {
        Locker locker { m_lock };
        if ((m_delegate = layer.createAsyncContentsDisplayDelegate(m_delegate.get()))) {
            if (buffer) {
                m_delegate->tryCopyToLayer(*buffer, opaque);
                m_delegateBufferVersion = m_placeholderBufferVersion;
            }
            layerID = m_delegate->destinationLayerID();
        }
    }
    reportLayerChange(layerID);
}

void PlaceholderRenderingContextSource::reportLayerChange(std::optional<PlatformLayerIdentifier> layerID)
{
    assertIsMainThread();
    if (m_identifier.processIdentifier() != Process::identifier())
        return;
    if (m_reportedLayerID.asOptional() == layerID)
        return;
    m_reportedLayerID = layerID;
    if (auto& handler = layerChangeHandler())
        handler(m_identifier, layerID);
}

WTF_MAKE_TZONE_ALLOCATED_IMPL(PlaceholderRenderingContext);

std::unique_ptr<PlaceholderRenderingContext> PlaceholderRenderingContext::create(HTMLCanvasElement& element)
{
    return std::unique_ptr<PlaceholderRenderingContext> { new PlaceholderRenderingContext(element) };
}

PlaceholderRenderingContext::PlaceholderRenderingContext(HTMLCanvasElement& canvas)
    : CanvasRenderingContext(canvas, Type::Placeholder)
    , m_source(PlaceholderRenderingContextSource::create(*this))
{
}

PlaceholderRenderingContext::~PlaceholderRenderingContext() = default;

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
    m_source->setContentsToLayer(layer, m_buffer.get(), m_opaque);
}

void PlaceholderRenderingContext::setPlaceholderBuffer(Ref<ImageBuffer>&& newBuffer, bool originClean, bool opaque)
{
    IntSize newSize = newBuffer->truncatedLogicalSize();
    Ref canvas = this->canvas();
    canvas->willUpdateContents(FloatRect { { }, newSize }, ShouldApplyPostProcessingToDirtyRect::No);
    m_opaque = opaque;
    updateMemoryCost(newBuffer->memoryCost());
    m_buffer = WTF::move(newBuffer);
    m_bufferNativeImage = nullptr;
    canvas->setSizeForControllingContext(newSize);
    if (originClean)
        canvas->setOriginClean();
    else
        canvas->setOriginTainted();
}

PixelFormat PlaceholderRenderingContext::pixelFormat() const
{
    if (auto* buffer = m_buffer.get())
        return buffer->pixelFormat();
    return CanvasRenderingContext::pixelFormat();
}

RefPtr<ImageBuffer> PlaceholderRenderingContext::surfaceBufferToImageBuffer(SurfaceBuffer)
{
    return m_buffer;
}

RefPtr<NativeImage> PlaceholderRenderingContext::surfaceBufferToNativeImage(SurfaceBuffer)
{
    if (m_bufferNativeImage)
        return m_bufferNativeImage;
    RefPtr buffer = m_buffer;
    if (!buffer)
        return nullptr;
    m_bufferNativeImage = buffer->copyNativeImage();
    return m_bufferNativeImage;
}

bool PlaceholderRenderingContext::isSurfaceBufferTransparentBlack(SurfaceBuffer) const
{
    return !m_buffer;
}

void PlaceholderRenderingContext::didUpdateCanvasSizeProperties(bool)
{
}

}

#endif
