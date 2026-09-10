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

#pragma once

#if ENABLE(OFFSCREEN_CANVAS)

#include "CanvasRenderingContext.h"
#include "PlaceholderRenderingContextSource.h"
#include <wtf/TZoneMalloc.h>
#include <wtf/WeakPtr.h>

namespace WebCore {


class PlaceholderRenderingContext final : public CanvasRenderingContext {
    WTF_MAKE_TZONE_ALLOCATED(PlaceholderRenderingContext);
public:
    static std::unique_ptr<PlaceholderRenderingContext> create(HTMLCanvasElement&);

    ~PlaceholderRenderingContext();

    HTMLCanvasElement& NODELETE canvas() const;
    IntSize NODELETE size() const;
    void setPlaceholderBuffer(Ref<ImageBuffer>&&, bool originClean, bool opaque);

    PlaceholderRenderingContextSource& source() const { return m_source; }

    RefPtr<ImageBuffer> surfaceBufferToImageBuffer(SurfaceBuffer) final;
    RefPtr<NativeImage> surfaceBufferToNativeImage(SurfaceBuffer) final;
    bool isSurfaceBufferTransparentBlack(SurfaceBuffer) const final;
    void didUpdateCanvasSizeProperties(bool) final;

private:
    PlaceholderRenderingContext(HTMLCanvasElement&);
    void setContentsToLayer(GraphicsLayer&) final;
    PixelFormat pixelFormat() const final;
    bool isOpaque() const final { return m_opaque; }

    const Ref<PlaceholderRenderingContextSource> m_source;
    RefPtr<ImageBuffer> m_buffer; // Temporary until content is provided as NativeImage.
    RefPtr<NativeImage> m_bufferNativeImage;
    bool m_opaque { false };
};

}

SPECIALIZE_TYPE_TRAITS_CANVASRENDERINGCONTEXT(WebCore::PlaceholderRenderingContext, isPlaceholder())

#endif
