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

#if USE(CG)

#include <pal/spi/cg/CoreGraphicsSPI.h>
#include <wtf/HashMap.h>
#include <wtf/RetainPtr.h>
#include <wtf/TZoneMalloc.h>
#include <wtf/UniqueRef.h>
#include <wtf/Vector.h>

namespace WebCore {

class Font;
struct FontCustomPlatformData;
class GraphicsContext;
class GStateScope;
class NativeImage;

// A CGContextDelegate that forwards CoreGraphics drawing back into a WebCore GraphicsContext.
//
// This allows code that needs a CGContextRef (for example, PDFKit drawing a page) to render into
// a GraphicsContext that has no platform context, such as a DisplayList::Recorder.
//
// The CGContext created by this class starts with the same CTM as the target GraphicsContext.
// Every drawing callback maps the CoreGraphics gstate (CTM, clip, alpha, colors, shadow, ...) onto
// the target, wrapped in a save()/restore() pair so the target's state is unchanged afterwards.
class GraphicsContextCGDelegate {
    WTF_MAKE_TZONE_ALLOCATED(GraphicsContextCGDelegate);
    WTF_MAKE_NONCOPYABLE(GraphicsContextCGDelegate);
public:
    // Returns a CGContext whose drawing is forwarded to `target`. The CGContext owns the
    // delegate; `target` must outlive the returned CGContext.
    WEBCORE_EXPORT static RetainPtr<CGContextRef> createCGContext(GraphicsContext& target);

    // Returns a GraphicsContextCG wrapping a CGContext created by createCGContext().
    WEBCORE_EXPORT static UniqueRef<GraphicsContext> createGraphicsContext(GraphicsContext& target);

    ~GraphicsContextCGDelegate();

    GraphicsContext& target() const { return m_target; }

private:
    friend class GStateScope;

    explicit GraphicsContextCGDelegate(GraphicsContext&);

    static GraphicsContextCGDelegate& fromDelegate(CGContextDelegateRef);
    static void installCallbacks(CGContextDelegateRef);

    // Lifecycle.
    static void finalize(CGContextDelegateRef);

    // Queries.
    static CGColorTransformRef getColorTransform(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef);
    static CGAffineTransform getTransform(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef);
    static CGRect getBounds(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef);
    static CGColorSpaceRef getColorSpace(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef);
    static const char* getName(CGContextDelegateRef);

    // Drawing primitives.
    static void drawLines(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, const CGPoint[], size_t);
    static CGError drawRects(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGPathDrawingMode, const CGRect[], size_t);
    static CGError drawPath(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGPathDrawingMode, CGPathRef);
    static CGError drawPathDirect(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGPathDrawingMode, CGPathRef, const CGRect*);
    static CGError strokeArc(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGFloat x, CGFloat y, CGFloat radius, CGFloat startAngle, CGFloat endAngle, bool clockwise);
    static CGError drawImage(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGRect, CGImageRef);
    static CGError drawImages(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, const CGRect[], const CGImageRef[], const CGRect[], size_t);
    static CGError drawImageFromRect(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGRect dstRect, CGImageRef, CGRect srcRect);
    static CGError drawImageApplyingToneMapping(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGRect, CGImageRef, CGToneMapping, CFDictionaryRef);
    static CGError drawGlyphs(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, const CGAffineTransform*, const CGGlyph[], const CGPoint[], size_t);

    // Gradients and shadings.
    static CGError drawShading(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGShadingRef);
    static CGError drawLinearGradient(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGGradientRef, CGPoint, CGPoint, CGGradientDrawingOptions);
    static CGError drawRadialGradient(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGGradientRef, CGPoint, CGFloat, CGPoint, CGFloat, CGGradientDrawingOptions);
    static CGError drawConicGradient(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGGradientRef, CGPoint, CGFloat);

    // Display lists.
    static CGError drawDisplayList(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGDisplayListRef);

    // Operations (clear, erase, flush, ...). BeginPage / EndPage are intentionally not implemented
    // (the target isn't paginated), like bitmap contexts.
    static CGError operation(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CFStringRef, CFDictionaryRef);

    // Transparency layers. GetLayer / DrawLayer (CGLayer) are intentionally not implemented, see
    // installCallbacks().
    static CGContextDelegateRef beginLayer(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef, CGRect, CFDictionaryRef, CGContextDelegateRef);
    static CGContextDelegateRef endLayer(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef);

    void logUnimplementedCallback(const char* callbackName);

    // Returns a NativeImage that draws like `image` (see createDrawableImage()). Images that aren't
    // image masks are cached, so that the target sees the same NativeImage for repeated draws.
    RefPtr<NativeImage> nativeImageForDrawing(CGImageRef, CGColorRef maskColor);

    // Returns a horizontal Font of `size` for `font`. Fonts are cached, so that the target sees the
    // same Font for repeated draws.
    Ref<Font> fontForDrawing(CGFontRef, CGFloat size);

    GraphicsContext& m_target;
    RetainPtr<CGColorTransformRef> m_colorTransform;
    HashMap<RetainPtr<CGImageRef>, RefPtr<NativeImage>> m_nativeImages;
    HashMap<std::pair<RetainPtr<CGFontRef>, uint32_t>, Ref<Font>> m_fonts;
    HashMap<RetainPtr<CGFontRef>, RefPtr<FontCustomPlatformData>> m_customFontData;

    // One entry per open transparency layer: the gstate the layer is composited with (clip, alpha,
    // composite operation, shadow, soft mask) stays applied to the target until the layer ends.
    Vector<std::unique_ptr<GStateScope>> m_transparencyLayers;
};

} // namespace WebCore

#endif // USE(CG)
