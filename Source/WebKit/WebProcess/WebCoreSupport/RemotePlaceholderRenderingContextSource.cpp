/*
 * Copyright (C) 2026 Apple Inc. All rights reserved.
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
#include "RemotePlaceholderRenderingContextSource.h"

#if ENABLE(OFFSCREEN_CANVAS)

#include "ImageBufferBackendHandleSharing.h"
#include "RemoteImageBufferProxy.h"
#include "RemoteRenderingBackendProxy.h"
#include "WebProcess.h"
#include "WebProcessProxyMessages.h"
#include <WebCore/ImageBuffer.h>

namespace WebKit {
using namespace WebCore;

Ref<RemotePlaceholderRenderingContextSource> RemotePlaceholderRenderingContextSource::create(PlaceholderRenderingContextIdentifier identifier)
{
    return adoptRef(*new RemotePlaceholderRenderingContextSource(identifier));
}

RemotePlaceholderRenderingContextSource::RemotePlaceholderRenderingContextSource(PlaceholderRenderingContextIdentifier identifier)
    : PlaceholderRenderingContextSource(identifier)
{
}

void RemotePlaceholderRenderingContextSource::setPlaceholderBuffer(ImageBuffer& buffer, bool originClean, bool opaque, LayerUpdate)
{
    // Clone so that the offscreen side can keep drawing into its own buffer while this frame is
    // in flight, mirroring GraphicsLayerCARemoteAsyncContentsDisplayDelegate::tryCopyToLayer().
    RefPtr clone = buffer.clone();
    if (!clone)
        return;
    clone->flushDrawingContext();

    // The compositing half: an IOSurface send right the UI process can hand straight to the
    // placeholder's layer. No web process ever maps it, which matters because the WebContent
    // sandbox blocks IOKit whenever the GPU process is doing the rendering.
    std::optional<ImageBufferBackendHandle> backendHandle;
    if (auto* sharing = dynamicDowncast<ImageBufferBackendHandleSharing>(clone->toBackendSharing()))
        backendHandle = sharing->createBackendHandle(SharedMemory::Protection::ReadOnly);

    auto parameters = clone->parameters();
    auto info = clone->backendInfo();
    auto bufferIdentifier = clone->renderingResourceIdentifier();

    // The canvas element half: the placeholder's process needs pixels it can actually read, so
    // hand the buffer over inside the GPU process rather than shipping it through a web process.
    // (No web process may map the IOSurface above once the GPU process is doing the rendering.)
    std::optional<RemoteSerializedImageBufferIdentifier> transferIdentifier;
    if (auto serialized = ImageBuffer::sinkIntoSerializedImageBuffer(WTF::move(clone))) {
        if (auto* remoteSerialized = dynamicDowncast<RemoteSerializedImageBufferProxy>(serialized.get())) {
            if (remoteSerialized->transferToProcess(identifier().processIdentifier()))
                transferIdentifier = remoteSerialized->identifier();
        }
    }

    if (!backendHandle && !transferIdentifier)
        return;

    RefPtr connection = WebProcess::singleton().parentProcessConnection();
    if (!connection)
        return;

    connection->send(Messages::WebProcessProxy::CommitOffscreenCanvasPlaceholderFrame(identifier(), parameters, info, transferIdentifier, WTF::move(backendHandle), bufferIdentifier, originClean, opaque), 0);
}

} // namespace WebKit

#endif // ENABLE(OFFSCREEN_CANVAS)
