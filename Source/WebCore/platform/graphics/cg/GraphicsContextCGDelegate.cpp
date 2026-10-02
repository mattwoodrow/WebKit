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

#include "config.h"
#include "GraphicsContextCGDelegate.h"

#if USE(CG)

#include "Color.h"
#include "ColorSpaceCG.h"
#include "Font.h"
#include "FontCustomPlatformData.h"
#include "FontPlatformData.h"
#include "Gradient.h"
#include "GraphicsContextCG.h"
#include "ImageBuffer.h"
#include "NativeImage.h"
#include "Path.h"
#include "PathCG.h"
#include "Pattern.h"
#include "SharedBuffer.h"
#include <pal/spi/cf/CoreTextSPI.h>
#include <wtf/MathExtras.h>
#include <wtf/TZoneMallocInlines.h>
#include <wtf/cf/TypeCastsCF.h>
#include <wtf/cf/VectorCF.h>

namespace WebCore {

WTF_MAKE_TZONE_ALLOCATED_IMPL(GraphicsContextCGDelegate);

// GStateScope maps a CoreGraphics gstate onto the target GraphicsContext for the duration of one
// drawing callback. Every drawing callback should use it:
//
//     GStateScope scope(delegate, rstate, gstate, GStateScope::paintForDrawingMode(mode));
//     if (!scope.shouldDraw())
//         return kCGErrorSuccess;
//     scope.target().fillPath(...); // Geometry in CG user space (the target's CTM is the gstate CTM).
//
// The constructor calls target.save(), applies the CG clip stack (which is in device space), sets
// the target's CTM to the gstate CTM (absolute, see createCGContext()), and maps alpha, composite
// operation / blend mode, style (shadow, blur, color matrix), antialiasing and image interpolation
// quality, and the soft mask (as an image clip). The fill and stroke brushes (colors or patterns) and the line parameters are applied
// only when requested with `Paint`. The destructor calls target.restore().
//
// shouldDraw() is false when nothing would be drawn (empty clip) or when the gstate can't be
// represented (for example, an unsupported pattern color).
class GStateScope {
    WTF_MAKE_TZONE_ALLOCATED(GStateScope);
    WTF_MAKE_NONCOPYABLE(GStateScope);
public:
    enum class Paint : uint8_t {
        Fill = 1 << 0, // Fill brush (color or pattern).
        Stroke = 1 << 1, // Stroke brush, line width, cap, join, miter limit and dash.
        // With Fill: the caller can fill with drawSpacedFillPattern(), so a spaced image pattern
        // fill brush is kept as such (see spacedFillPattern()) instead of being rasterized.
        SpacedFillPattern = 1 << 2,
    };

    // `userSpaceTransform` is concatenated to the gstate CTM to form the target's user space (for
    // example, the text matrix for glyphs). Line widths and dashes stay in the CG user space.
    GStateScope(GraphicsContextCGDelegate&, CGRenderingStateRef, CGGStateRef, OptionSet<Paint> = { }, const AffineTransform& userSpaceTransform = { });
    ~GStateScope();

    bool shouldDraw() const { return m_shouldDraw; }
    GraphicsContext& target() const { return m_delegate.target(); }
    const AffineTransform& ctm() const { return m_ctm; }
    float alpha() const { return m_alpha; }

    // A focus ring CGStyle. GraphicsContext can't express it as a style, only as
    // GraphicsContext::drawFocusRing() of a path, so fill callbacks draw it with drawFocusRing().
    struct FocusRing {
        Color color;
        float zoomFactor;
        CGFocusRingOrdering ordering;
    };
    const std::optional<FocusRing>& focusRing() const { return m_focusRing; }

    // An image pattern with spacing between the tiles (a CGPattern whose step is larger than its
    // bounds), which WebCore::Pattern can't express but GraphicsContext::drawPattern() can. Set
    // instead of the fill pattern when requested with Paint::SpacedFillPattern.
    struct SpacedImagePattern {
        Ref<NativeImage> image;
        // Concatenated to the CTM for drawing, so that the image space maps with a positive scale.
        AffineTransform userSpaceFlip;
        // In the flipped user space.
        AffineTransform patternTransform;
        FloatPoint phase;
        FloatSize spacing;
    };
    bool hasSpacedFillPattern() const { return !!m_spacedFillPattern; }
    // Fills `rect` (in the target's user space, clipped to the current clip) with the spaced fill pattern.
    void drawSpacedFillPattern(const FloatRect&);

    // Clips to `mask` (an image mask, or an image whose alpha or luminance is the coverage) placed
    // y-up in `rect`, in the current user space.
    void clipToImageMask(CGImageRef mask, const CGRect&);

    static OptionSet<Paint> paintForDrawingMode(CGPathDrawingMode);
    static WindRule windRuleForDrawingMode(CGPathDrawingMode);

private:
    void applyClipStack(CGGStateRef);
    bool clipToShape(CGPathRef);
    void applyClipMask(CGClipRef);
    void applySoftMask(CGSoftMaskRef);
    void applyCompositeOperation(CGGStateRef);
    void applyStyle(CGGStateRef);
    void applyLineParameters(CGGStateRef);
    enum class BrushType : bool { Fill, Stroke };
    bool applyBrush(CGGStateRef, CGColorRef, BrushType);
    RefPtr<Pattern> createPattern(CGGStateRef, CGColorRef);
    std::optional<SpacedImagePattern> createSpacedImagePattern(CGGStateRef, CGColorRef);

    GraphicsContextCGDelegate& m_delegate;
    AffineTransform m_ctm;
    AffineTransform m_baseCTM;
    AffineTransform m_userSpaceTransform;
    float m_alpha { 1 };
    std::optional<FocusRing> m_focusRing;
    std::optional<SpacedImagePattern> m_spacedFillPattern;
    OptionSet<Paint> m_paint;
    bool m_shouldDraw { true };
};

WTF_MAKE_TZONE_ALLOCATED_IMPL(GStateScope);

static InterpolationQuality interpolationQuality(CGInterpolationQuality quality)
{
    switch (quality) {
    case kCGInterpolationDefault:
        return InterpolationQuality::Default;
    case kCGInterpolationNone:
        return InterpolationQuality::DoNotInterpolate;
    case kCGInterpolationLow:
        return InterpolationQuality::Low;
    case kCGInterpolationMedium:
        return InterpolationQuality::Medium;
    case kCGInterpolationHigh:
        return InterpolationQuality::High;
    }
    return InterpolationQuality::Default;
}

static CompositeMode compositeMode(CGCompositeOperation operation)
{
    switch (operation) {
    case kCGCompositeClear:
        return { CompositeOperator::Clear, BlendMode::Normal };
    case kCGCompositeCopy:
        return { CompositeOperator::Copy, BlendMode::Normal };
    case kCGCompositeSover:
        return { CompositeOperator::SourceOver, BlendMode::Normal };
    case kCGCompositeSin:
        return { CompositeOperator::SourceIn, BlendMode::Normal };
    case kCGCompositeSout:
        return { CompositeOperator::SourceOut, BlendMode::Normal };
    case kCGCompositeSatop:
        return { CompositeOperator::SourceAtop, BlendMode::Normal };
    case kCGCompositeDover:
        return { CompositeOperator::DestinationOver, BlendMode::Normal };
    case kCGCompositeDin:
        return { CompositeOperator::DestinationIn, BlendMode::Normal };
    case kCGCompositeDout:
        return { CompositeOperator::DestinationOut, BlendMode::Normal };
    case kCGCompositeDatop:
        return { CompositeOperator::DestinationAtop, BlendMode::Normal };
    case kCGCompositeXor:
        return { CompositeOperator::XOR, BlendMode::Normal };
    case kCGCompositePlusd:
        return { CompositeOperator::PlusDarker, BlendMode::Normal };
    case kCGCompositePlusl:
        return { CompositeOperator::PlusLighter, BlendMode::Normal };
    case kCGCompositeMultiply:
        return { CompositeOperator::SourceOver, BlendMode::Multiply };
    case kCGCompositeScreen:
        return { CompositeOperator::SourceOver, BlendMode::Screen };
    case kCGCompositeOverlay:
        return { CompositeOperator::SourceOver, BlendMode::Overlay };
    case kCGCompositeDarken:
        return { CompositeOperator::SourceOver, BlendMode::Darken };
    case kCGCompositeLighten:
        return { CompositeOperator::SourceOver, BlendMode::Lighten };
    case kCGCompositeColorDodge:
        return { CompositeOperator::SourceOver, BlendMode::ColorDodge };
    case kCGCompositeColorBurn:
        return { CompositeOperator::SourceOver, BlendMode::ColorBurn };
    case kCGCompositeSoftLight:
        return { CompositeOperator::SourceOver, BlendMode::SoftLight };
    case kCGCompositeHardLight:
        return { CompositeOperator::SourceOver, BlendMode::HardLight };
    case kCGCompositeDifference:
        return { CompositeOperator::SourceOver, BlendMode::Difference };
    case kCGCompositeExclusion:
        return { CompositeOperator::SourceOver, BlendMode::Exclusion };
    case kCGCompositeHue:
        return { CompositeOperator::SourceOver, BlendMode::Hue };
    case kCGCompositeSaturation:
        return { CompositeOperator::SourceOver, BlendMode::Saturation };
    case kCGCompositeColor:
        return { CompositeOperator::SourceOver, BlendMode::Color };
    case kCGCompositeLuminosity:
        return { CompositeOperator::SourceOver, BlendMode::Luminosity };
    default:
        break;
    }
    return { CompositeOperator::SourceOver, BlendMode::Normal };
}

static LineCap lineCap(CGLineCap cap)
{
    switch (cap) {
    case kCGLineCapButt:
        return LineCap::Butt;
    case kCGLineCapRound:
        return LineCap::Round;
    case kCGLineCapSquare:
        return LineCap::Square;
    }
    return LineCap::Butt;
}

static LineJoin lineJoin(CGLineJoin join)
{
    switch (join) {
    case kCGLineJoinMiter:
        return LineJoin::Miter;
    case kCGLineJoinRound:
        return LineJoin::Round;
    case kCGLineJoinBevel:
        return LineJoin::Bevel;
    }
    return LineJoin::Miter;
}

enum class PreserveShapes : bool { No, Yes };

// True if `transform` maps axis-aligned rects to axis-aligned rects, up to floating point error
// (e.g. the inverse CTM composed with a shape transform that CG computed from the CTM).
static bool isNearlyRectilinear(const AffineTransform& transform)
{
    double scale = std::max({ std::abs(transform.a()), std::abs(transform.b()), std::abs(transform.c()), std::abs(transform.d()) });
    constexpr double tolerance = 1e-6;
    return std::abs(transform.b()) <= tolerance * scale && std::abs(transform.c()) <= tolerance * scale;
}

// CG reports a closed path of four lines forming a rect as a rect with an identity shape transform,
// and a CGPathAddRect() rect as a unit rect with a scale. Keep the four lines (with their start
// point and direction), snapped to the exact rect: accelerated CG only strokes rects natively when the
// shape scale is uniform, so the two forms render differently there, and Path::addRect() would
// turn the former into the latter.
static std::optional<Path> fourLineRectPath(CGPathRef path, const AffineTransform& transform, const FloatRect& rect)
{
    __block Vector<FloatPoint, 5> points;
    CGPathApplyWithBlock(path, ^(const CGPathElement* element) {
        if (element->type == kCGPathElementMoveToPoint || element->type == kCGPathElementAddLineToPoint)
            points.append(transform.mapPoint(FloatPoint(*element->points)));
    });
    if (points.size() < 4)
        return std::nullopt;

    auto snap = [](float value, float minimum, float maximum) {
        return std::abs(value - minimum) <= std::abs(value - maximum) ? minimum : maximum;
    };
    Path result;
    for (size_t i = 0; i < points.size(); ++i) {
        FloatPoint point { snap(points[i].x(), rect.x(), rect.maxX()), snap(points[i].y(), rect.y(), rect.maxY()) };
        if (!i)
            result.moveTo(point);
        else
            result.addLineTo(point);
    }
    result.closeSubpath();
    return result;
}

// CG paths remember when they were created as an ellipse or rounded rect, also after they were
// transformed (CG transforms drawPath() paths to device space). Preserve that so the target can use
// its native shape (accelerated CG rasterizes these differently from the equivalent beziers), as
// long as the shape is axis aligned in the target's user space (`transform` maps path space to it).
//
// Rects (CGPathAddRect() / CGContextAddRect() rects, which CG keeps as a unit rect plus a transform,
// and closed four-line rects, which stay four lines; see fourLineRectPath()) and single lines are
// re-created exactly too: drawPath() paths come in
// device space, and mapping their points back to user space adds float error that makes accelerated
// CG stop recognizing them as rects or lines, which it strokes
// differently from general paths (e.g. un-unioned rect corners).
static std::optional<Path> shapePathFromCGPath(CGPathRef path, const AffineTransform& transform)
{
    std::array<CGPoint, 2> line;
    if (CGPathIsLine(path, line.data())) {
        Path result;
        result.moveTo(transform.mapPoint(FloatPoint(line[0])));
        result.addLineTo(transform.mapPoint(FloatPoint(line[1])));
        return result;
    }

    CGRect rect;
    CGAffineTransform cgShapeTransform;
    if (CGPathIsRectWithTransform(path, &rect, &cgShapeTransform)) {
        auto shapeToTarget = transform * AffineTransform(cgShapeTransform);
        if (!isNearlyRectilinear(shapeToTarget))
            return std::nullopt;
        FloatRect targetRect = shapeToTarget.mapRect(FloatRect(rect));
        if (CGAffineTransformIsIdentity(cgShapeTransform))
            return fourLineRectPath(path, transform, targetRect);
        Path result;
        result.addRect(targetRect);
        return result;
    }

    if (CGPathIsEllipseWithTransform(path, &rect, &cgShapeTransform)) {
        auto shapeToTarget = transform * AffineTransform(cgShapeTransform);
        if (!isNearlyRectilinear(shapeToTarget))
            return std::nullopt;
        Path result;
        result.addEllipseInRect(shapeToTarget.mapRect(FloatRect(rect)));
        return result;
    }

    // Corners are ordered (minX, maxY), (maxX, maxY), (maxX, minY), (minX, minY) in shape space.
    std::array<CGSize, 4> corners;
    CGFloat cornerWidth;
    CGFloat cornerHeight;
    if (CGPathIsRoundedRectWithTransform(path, &rect, &cornerWidth, &cornerHeight, &cgShapeTransform))
        corners.fill(CGSizeMake(cornerWidth, cornerHeight));
    else if (!CGPathIsUnevenCornersRoundedRectWithTransform(path, &rect, corners.data(), &cgShapeTransform))
        return std::nullopt;

    auto shapeToTarget = transform * AffineTransform(cgShapeTransform);
    if (!isNearlyRectilinear(shapeToTarget))
        return std::nullopt;
    auto scaleCorner = [&](CGSize corner) {
        return FloatSize(std::abs(corner.width * shapeToTarget.a()), std::abs(corner.height * shapeToTarget.d()));
    };
    FloatSize minXMaxY = scaleCorner(corners[0]);
    FloatSize maxXMaxY = scaleCorner(corners[1]);
    FloatSize maxXMinY = scaleCorner(corners[2]);
    FloatSize minXMinY = scaleCorner(corners[3]);
    if (shapeToTarget.a() < 0) {
        std::swap(minXMaxY, maxXMaxY);
        std::swap(minXMinY, maxXMinY);
    }
    if (shapeToTarget.d() < 0) {
        std::swap(minXMaxY, minXMinY);
        std::swap(maxXMaxY, maxXMinY);
    }
    // In the target, minimum y is the top.
    Path result;
    result.addRoundedRect(FloatRoundedRect(shapeToTarget.mapRect(FloatRect(rect)), { minXMinY, maxXMinY, minXMaxY, maxXMaxY }));
    return result;
}

// GraphicsContextCG::clipOut() clips to CGRectInfinite plus the shape with the even-odd rule. Such
// coordinates don't survive conversion to float (or IPC), so clamp them to a large but finite range.
static RetainPtr<CGPathRef> clampPathCoordinates(CGPathRef path)
{
    static constexpr CGFloat maximumCoordinate = 1 << 24;
    CGRect bounds = CGPathGetBoundingBox(path);
    if (CGRectIsNull(bounds) || CGRectContainsRect(CGRectMake(-maximumCoordinate, -maximumCoordinate, 2 * maximumCoordinate, 2 * maximumCoordinate), bounds))
        return path;

    auto clamp = [](CGPoint point) {
        return CGPointMake(std::clamp(point.x, -maximumCoordinate, maximumCoordinate), std::clamp(point.y, -maximumCoordinate, maximumCoordinate));
    };

    RetainPtr clampedPath = adoptCF(CGPathCreateMutable());
    CGPathApplyWithBlock(path, ^(const CGPathElement* element) {
        auto points = unsafeMakeSpan(element->points, 3);
        switch (element->type) {
        case kCGPathElementMoveToPoint:
            CGPathMoveToPoint(clampedPath.get(), nullptr, clamp(points[0]).x, clamp(points[0]).y);
            break;
        case kCGPathElementAddLineToPoint:
            CGPathAddLineToPoint(clampedPath.get(), nullptr, clamp(points[0]).x, clamp(points[0]).y);
            break;
        case kCGPathElementAddQuadCurveToPoint:
            CGPathAddQuadCurveToPoint(clampedPath.get(), nullptr, clamp(points[0]).x, clamp(points[0]).y, clamp(points[1]).x, clamp(points[1]).y);
            break;
        case kCGPathElementAddCurveToPoint:
            CGPathAddCurveToPoint(clampedPath.get(), nullptr, clamp(points[0]).x, clamp(points[0]).y, clamp(points[1]).x, clamp(points[1]).y, clamp(points[2]).x, clamp(points[2]).y);
            break;
        case kCGPathElementCloseSubpath:
            CGPathCloseSubpath(clampedPath.get());
            break;
        }
    });
    return clampedPath;
}

static Path pathFromCGPath(CGPathRef cgPath, const AffineTransform& transform = { }, PreserveShapes preserveShapes = PreserveShapes::Yes)
{
    if (preserveShapes == PreserveShapes::Yes) {
        if (auto shapePath = shapePathFromCGPath(cgPath, transform))
            return WTF::move(*shapePath);
    }

    auto clampedPath = clampPathCoordinates(cgPath);
    CGPathRef path = clampedPath.get();

    if (transform.isIdentity())
        return Path { PathCG::create(adoptCF(CGPathCreateMutableCopy(path))) };

    CGAffineTransform cgTransform = transform;
    return Path { PathCG::create(adoptCF(CGPathCreateMutableCopyByTransformingPath(path, &cgTransform))) };
}

// The largest scale factor that `transform` applies along either axis.
static float maximumScale(const AffineTransform& transform)
{
    return std::max(std::hypot(transform.a(), transform.b()), std::hypot(transform.c(), transform.d()));
}

GStateScope::GStateScope(GraphicsContextCGDelegate& delegate, CGRenderingStateRef rstate, CGGStateRef gstate, OptionSet<Paint> paint, const AffineTransform& userSpaceTransform)
    : m_delegate(delegate)
    , m_ctm(AffineTransform(*CGGStateGetCTM(gstate)) * userSpaceTransform)
    , m_userSpaceTransform(userSpaceTransform)
    , m_paint(paint)
{
    // CG pattern matrices, pattern phases and style parameters are relative to the base CTM. It can
    // change while drawing (CGContextDrawTiledImage() resets it, for example).
    if (auto* baseCTM = CGRenderingStateGetBaseCTM(rstate))
        m_baseCTM = *baseCTM;

    auto& target = this->target();
    target.save();

    if (CGRectIsEmpty(CGGStateGetClipBoundingBox(gstate))) {
        m_shouldDraw = false;
        return;
    }

    applyClipStack(gstate);
    if (CGSoftMaskRef softMask = CGGStateGetSoftMask(gstate)) {
        applySoftMask(softMask);
        if (!m_shouldDraw)
            return;
    }
    target.setCTM(m_ctm);

    m_alpha = CGGStateGetAlpha(gstate);
    applyCompositeOperation(gstate);
    applyStyle(gstate);
    target.setShouldAntialias(CGGStateGetShouldAntialias(gstate) && CGRenderingStateGetAllowsAntialiasing(rstate));
    target.setImageInterpolationQuality(interpolationQuality(CGGStateGetInterpolationQuality(gstate)));

    if (paint.contains(Paint::Fill) && !applyBrush(gstate, CGGStateGetFillColor(gstate), BrushType::Fill))
        m_shouldDraw = false;
    if (paint.contains(Paint::Stroke)) {
        if (!applyBrush(gstate, CGGStateGetStrokeColor(gstate), BrushType::Stroke))
            m_shouldDraw = false;
        applyLineParameters(gstate);
    }
    // applyBrush() folds the alpha of pattern colors into m_alpha.
    target.setAlpha(m_alpha);
}

GStateScope::~GStateScope()
{
    target().restore();
}

OptionSet<GStateScope::Paint> GStateScope::paintForDrawingMode(CGPathDrawingMode mode)
{
    switch (mode) {
    case kCGPathFill:
    case kCGPathEOFill:
        return Paint::Fill;
    case kCGPathStroke:
        return Paint::Stroke;
    case kCGPathFillStroke:
    case kCGPathEOFillStroke:
        return { Paint::Fill, Paint::Stroke };
    }
    return { };
}

WindRule GStateScope::windRuleForDrawingMode(CGPathDrawingMode mode)
{
    return mode == kCGPathEOFill || mode == kCGPathEOFillStroke ? WindRule::EvenOdd : WindRule::NonZero;
}

using DeviceQuad = std::array<FloatPoint, 4>;

// The corners of the unit square under `transform`.
static DeviceQuad quadForUnitSquare(const AffineTransform& transform)
{
    return { transform.mapPoint(FloatPoint { 0, 0 }), transform.mapPoint(FloatPoint { 1, 0 }), transform.mapPoint(FloatPoint { 1, 1 }), transform.mapPoint(FloatPoint { 0, 1 }) };
}

static DeviceQuad quadForRect(const AffineTransform& transform, const CGRect& rect)
{
    CGRect standardRect = CGRectStandardize(rect);
    return quadForUnitSquare(transform * AffineTransform(standardRect.size.width, 0, 0, standardRect.size.height, standardRect.origin.x, standardRect.origin.y));
}

// Whether two quads have the same corners (in any order), up to float error.
static bool isSameQuad(const DeviceQuad& a, const DeviceQuad& b)
{
    static constexpr float tolerance = 1e-3;
    return std::ranges::all_of(a, [&](const FloatPoint& corner) {
        return std::ranges::any_of(b, [&](const FloatPoint& other) {
            return std::abs(corner.x() - other.x()) <= tolerance && std::abs(corner.y() - other.y()) <= tolerance;
        });
    });
}

// The device space quad of a rect clip (a rect clip entry, or a path clip that is a rect), if it is one.
static std::optional<DeviceQuad> rectClipQuad(CGClipRef clip)
{
    if (CGClipGetStroke(clip))
        return std::nullopt;
    if (CGClipGetType(clip) == kCGClipTypeRect)
        return quadForRect({ }, CGClipGetRect(clip));
    if (CGClipGetType(clip) != kCGClipTypePath || CGClipGetMode(clip) == kCGEOClip)
        return std::nullopt;
    RetainPtr path = adoptCF(CGClipCreateClipPath(clip));
    CGRect rect;
    CGAffineTransform transform;
    if (!path || !CGPathIsRectWithTransform(path.get(), &rect, &transform))
        return std::nullopt;
    return quadForRect(transform, rect);
}

void GStateScope::applyClipStack(CGGStateRef gstate)
{
    CGClipStackRef clipStack = CGGStateGetClipStack(gstate);
    if (!clipStack || CGClipStackIsInfinite(clipStack))
        return;

    // The clip stack is in device space.
    auto& target = this->target();
    target.setCTM({ });

    // Mask clips are replayed with GraphicsContext::clipToImageBuffer(), which clips to the mask
    // rect as well. GraphicsContextCG::clipToImageBuffer() itself clips to the rect before clipping
    // to the mask, so the stack usually has the same rect as a separate clip; applying both would
    // clip antialiased (e.g. rotated) mask edges twice. Skip rect clips that a mask clip repeats.
    size_t count = CGClipStackGetCount(clipStack);
    Vector<DeviceQuad, 1> maskQuads;
    for (size_t i = 0; i < count; ++i) {
        CGClipRef clip = CGClipStackGetClipAtIndex(clipStack, i);
        if (CGClipGetType(clip) != kCGClipTypeMask)
            continue;
        if (CGClipMask* mask = CGClipGetMask(clip))
            maskQuads.append(quadForRect(CGClipMaskGetMatrix(mask), CGClipMaskGetRect(mask)));
    }
    auto isRepeatedByMask = [&](const std::optional<DeviceQuad>& quad) {
        return quad && std::ranges::any_of(maskQuads, [&](auto& maskQuad) {
            return isSameQuad(*quad, maskQuad);
        });
    };

    // Rect clips are not kept as entries; CG intersects them all into the stack's rect.
    // Aliased rect clips have already been made integral.
    CGRect clipRect = CGClipStackGetRect(clipStack);
    if (!CGRectEqualToRect(clipRect, CGRectInfinite) && !isRepeatedByMask(quadForRect({ }, clipRect))) {
        target.setShouldAntialias(true);
        target.clip(clipRect);
    }

    for (size_t i = 0; i < count; ++i) {
        CGClipRef clip = CGClipStackGetClipAtIndex(clipStack, i);
        if (!maskQuads.isEmpty() && CGClipGetType(clip) != kCGClipTypeMask && isRepeatedByMask(rectClipQuad(clip)))
            continue;
        target.setShouldAntialias(CGClipGetShouldAntialias(clip));

        switch (CGClipGetType(clip)) {
        case kCGClipTypeRect:
            if (!CGClipGetStroke(clip)) {
                target.clip(CGClipGetRect(clip));
                break;
            }
            [[fallthrough]];
        case kCGClipTypePath:
        case kCGClipTypeTextClipping: {
            // Converts stroke clips and text clips to their outline paths.
            RetainPtr path = adoptCF(CGClipCreateClipPath(clip));
            if (!path) {
                m_delegate.logUnimplementedCallback("GStateScope (clip without path)");
                break;
            }
            if (clipToShape(path.get()))
                break;
            auto windRule = CGClipGetMode(clip) == kCGEOClip ? WindRule::EvenOdd : WindRule::NonZero;
            target.clipPath(pathFromCGPath(path.get()), windRule);
            break;
        }
        case kCGClipTypeMask:
            applyClipMask(clip);
            target.setCTM({ });
            break;
        default:
            m_delegate.logUnimplementedCallback("GStateScope (unknown clip type)");
            break;
        }
    }
}

// Clip paths are in device space, but CG remembers rects, ellipses and rounded rects clipped under
// any CTM, as a shape in a unit rect plus the transform to device space. Clip to the native shape
// under that transform, as the target does when such a shape is clipped directly (accelerated CG
// rasterizes these clips differently from general paths, e.g. a rect clip under a rotation keeps
// the antialiased edges of content tangent to it). Expects the target CTM to be the identity.
// Returns false if the path isn't such a shape, or if it is axis aligned in device space.
bool GStateScope::clipToShape(CGPathRef path)
{
    CGRect rect;
    CGAffineTransform cgShapeTransform;
    bool isRect = CGPathIsRectWithTransform(path, &rect, &cgShapeTransform);
    if (!isRect
        && !CGPathIsEllipseWithTransform(path, &rect, &cgShapeTransform)
        && !CGPathIsRoundedRectWithTransform(path, &rect, nullptr, nullptr, &cgShapeTransform)
        && !CGPathIsUnevenCornersRoundedRectWithTransform(path, &rect, nullptr, &cgShapeTransform))
        return false;

    // Axis-aligned shapes are re-created in device space by pathFromCGPath(), which matches the
    // target more closely than a unit shape under a scale.
    AffineTransform shapeTransform(cgShapeTransform);
    if (isNearlyRectilinear(shapeTransform))
        return false;
    auto inverseShapeTransform = shapeTransform.inverse();
    if (!inverseShapeTransform)
        return false;

    std::optional<Path> shapePath;
    if (!isRect) {
        shapePath = shapePathFromCGPath(path, *inverseShapeTransform);
        if (!shapePath)
            return false;
    }

    auto& target = this->target();
    target.setCTM(shapeTransform);
    if (isRect)
        target.clip(rect);
    else
        target.clipPath(*shapePath, WindRule::NonZero);
    target.setCTM({ });
    return true;
}

// Renders a CG image mask (CGImageMaskCreate(), which is how CG stores every clip mask) into an
// image whose alpha is the mask coverage.
static RetainPtr<CGImageRef> createAlphaImageFromImageMask(CGImageRef imageMask)
{
    size_t width = CGImageGetWidth(imageMask);
    size_t height = CGImageGetHeight(imageMask);
    RetainPtr colorSpace = adoptCF(CGColorSpaceCreateWithName(kCGColorSpaceSRGB));
    RetainPtr context = adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, colorSpace.get(), kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    if (!context)
        return nullptr;
    CGContextSetFillColorWithColor(context.get(), CGColorGetConstantColor(kCGColorBlack));
    CGContextDrawImage(context.get(), CGRectMake(0, 0, width, height), imageMask);
    return adoptCF(CGBitmapContextCreateImage(context.get()));
}

void GStateScope::applyClipMask(CGClipRef clip)
{
    CGClipMask* mask = CGClipGetMask(clip);
    CGImageRef image = mask ? CGClipMaskGetImage(mask) : nullptr;
    if (!image)
        return;

    // The mask matrix is the CTM at the time of the clip.
    target().setCTM(CGClipMaskGetMatrix(mask));
    clipToImageMask(image, CGClipMaskGetRect(mask));
}

void GStateScope::clipToImageMask(CGImageRef mask, const CGRect& rect)
{
    RetainPtr image = mask;

    // Masks without alpha use their luminance as coverage.
    bool useLuminance = false;
    if (CGImageIsMask(image.get()))
        image = createAlphaImageFromImageMask(image.get());
    else {
        auto alphaInfo = CGImageGetAlphaInfo(image.get());
        useLuminance = alphaInfo == kCGImageAlphaNone || alphaInfo == kCGImageAlphaNoneSkipFirst || alphaInfo == kCGImageAlphaNoneSkipLast;
    }

    auto& target = this->target();
    RefPtr nativeImage = image ? NativeImage::create(RetainPtr { image }) : nullptr;
    if (!nativeImage)
        return;
    FloatSize imageSize(CGImageGetWidth(image.get()), CGImageGetHeight(image.get()));
    RefPtr maskBuffer = target.createImageBuffer(imageSize, 1, ColorSpace::SRGB());
    if (!maskBuffer)
        return;

    FloatRect imageRect { { }, imageSize };
    maskBuffer->context().drawNativeImage(*nativeImage, imageRect, imageRect);
    if (useLuminance)
        maskBuffer->convertToLuminanceMask();

    // The image is placed in `rect` y-up, like CGContextDrawImage(). Flip it so it lands right side
    // up with clipToImageBuffer(), then restore the user space.
    auto ctm = target.getCTM();
    FloatRect clipRect = CGRectStandardize(rect);
    target.translate(0, clipRect.height() + 2 * clipRect.y());
    target.scale(FloatSize(1, -1));
    target.clipToImageBuffer(*maskBuffer, clipRect);
    target.setCTM(ctm);
}

// Renders the coverage of `softMask` over `deviceExtent` (device space) into an image placed y-up
// in the returned rect, using the same semantics as CG's own soft mask rendering:
// - With a background color, the mask is a luminosity mask: the mask bounds are filled with the
//   background and the luminance of the mask content is the coverage. The background luminance
//   is also the coverage outside the mask bounds.
// - Without a background color, the alpha of the mask content is the coverage (0 outside).
// - The transfer function, if any, maps the coverage.
// Returns a null image if the coverage is zero everywhere in `deviceExtent`, and std::nullopt if
// the mask can't be rendered.
static std::optional<std::pair<RetainPtr<CGImageRef>, CGRect>> createSoftMaskImage(CGSoftMaskRef softMask, CGRect deviceExtent)
{
    static constexpr float coverageEpsilon = 1.f / 255;
    static constexpr CGFloat maximumDimension = 8192;

    CGAffineTransform matrix = CGSoftMaskGetMatrix(softMask);
    CGRect maskBounds = CGSoftMaskGetBounds(softMask);
    CGColorRef background = CGSoftMaskGetBackground(softMask);
    CGFunctionRef transfer = CGSoftMaskGetTransfer(softMask);
    if (transfer && CGFunctionIsIdentity(transfer))
        transfer = nullptr;

    auto applyTransfer = [&](CGFloat coverage) -> CGFloat {
        if (!transfer)
            return coverage;
        CGFloat result = 0;
        CGFunctionEvaluate(transfer, &coverage, &result);
        return std::clamp<CGFloat>(result, 0, 1);
    };

    RetainPtr grayColorSpace = adoptCF(CGColorSpaceCreateDeviceGray());
    CGFloat backgroundCoverage = 0;
    if (background) {
        RetainPtr grayBackground = adoptCF(CGColorCreateCopyByMatchingToColorSpace(grayColorSpace.get(), kCGRenderingIntentDefault, background, nullptr));
        backgroundCoverage = grayBackground ? CGColorGetComponents(grayBackground.get())[0] : CGColorGetComponents(background)[0];
    }
    backgroundCoverage = applyTransfer(backgroundCoverage);

    // Outside the mask bounds the coverage is the background coverage, so the whole extent needs
    // to be rendered unless that is zero.
    CGRect rect = deviceExtent;
    if (backgroundCoverage < coverageEpsilon)
        rect = CGRectIntersection(rect, CGRectApplyAffineTransform(maskBounds, matrix));
    rect = CGRectIntegral(rect);
    if (CGRectIsEmpty(rect))
        return std::pair<RetainPtr<CGImageRef>, CGRect> { };
    if (CGRectIsInfinite(rect) || rect.size.width > maximumDimension || rect.size.height > maximumDimension)
        return std::nullopt;

    // Luminosity masks render into a gray bitmap (CG converts colors to luminance); alpha masks
    // into an ARGB bitmap of which only the alpha is used.
    size_t width = rect.size.width;
    size_t height = rect.size.height;
    RetainPtr<CGContextRef> context;
    if (background)
        context = adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, grayColorSpace.get(), kCGImageAlphaNone));
    else {
        RetainPtr colorSpace = adoptCF(CGColorSpaceCreateWithName(kCGColorSpaceSRGB));
        context = adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, colorSpace.get(), kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    }
    if (!context)
        return std::nullopt;

    CGContextTranslateCTM(context.get(), -rect.origin.x, -rect.origin.y);
    if (background) {
        CGContextSetFillColorWithColor(context.get(), background);
        CGContextFillRect(context.get(), rect);
    }
    CGContextConcatCTM(context.get(), matrix);
    CGContextClipToRect(context.get(), maskBounds);
    CGContextSetBaseCTM(context.get(), CGContextGetCTM(context.get()));

    // The mask content is drawn by CG (for example a PDF soft mask form) into the bitmap context.
    CGSoftMaskDelegateDrawSoftMask(CGContextGetDelegate(context.get()), CGContextGetRenderingState(context.get()), CGContextGetGState(context.get()), softMask);
    CGContextFlush(context.get());

    std::array<uint8_t, 256> table;
    for (size_t i = 0; i < table.size(); ++i)
        table[i] = transfer ? std::lround(applyTransfer(i / 255.) * 255) : i;

    auto* data = static_cast<const uint8_t*>(CGBitmapContextGetData(context.get()));
    size_t bytesPerRow = CGBitmapContextGetBytesPerRow(context.get());
    if (!data)
        return std::nullopt;

    // Produce an image whose alpha is the (transferred) coverage, as premultiplied black, so the
    // target doesn't need to convert luminance itself.
    RetainPtr colorSpace = adoptCF(CGColorSpaceCreateWithName(kCGColorSpaceSRGB));
    RetainPtr coverageContext = adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, colorSpace.get(), kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    auto* coverageData = coverageContext ? static_cast<uint8_t*>(CGBitmapContextGetData(coverageContext.get())) : nullptr;
    if (!coverageData)
        return std::nullopt;
    size_t coverageBytesPerRow = CGBitmapContextGetBytesPerRow(coverageContext.get());
    auto pixels = unsafeMakeSpan(data, bytesPerRow * height);
    auto coveragePixels = unsafeMakeSpan(coverageData, coverageBytesPerRow * height);
    for (size_t y = 0; y < height; ++y) {
        auto destination = spanReinterpretCast<uint32_t>(coveragePixels.subspan(y * coverageBytesPerRow, width * sizeof(uint32_t)));
        if (background) {
            // Luminosity: the gray value is the coverage.
            auto source = pixels.subspan(y * bytesPerRow, width);
            for (size_t x = 0; x < width; ++x)
                destination[x] = static_cast<uint32_t>(table[source[x]]) << 24;
            continue;
        }
        // Alpha: keep only the alpha.
        auto source = spanReinterpretCast<const uint32_t>(pixels.subspan(y * bytesPerRow, width * sizeof(uint32_t)));
        for (size_t x = 0; x < width; ++x)
            destination[x] = static_cast<uint32_t>(table[source[x] >> 24]) << 24;
    }

    RetainPtr image = adoptCF(CGBitmapContextCreateImage(coverageContext.get()));
    if (!image)
        return std::nullopt;
    return std::pair { WTF::move(image), rect };
}

void GStateScope::applySoftMask(CGSoftMaskRef softMask)
{
    // The soft mask matrix maps the soft mask space to device space. Masks the draw (including its
    // shadow) by clipping to the rendered coverage.
    auto& target = this->target();
    target.setCTM({ });
    CGRect deviceExtent = FloatRect(target.clipBounds());
    auto mask = createSoftMaskImage(softMask, deviceExtent);
    if (!mask) {
        // Draw unmasked rather than not at all.
        m_delegate.logUnimplementedCallback("GStateScope (soft mask too large)");
        return;
    }
    auto& [image, rect] = *mask;
    if (!image) {
        m_shouldDraw = false;
        return;
    }
    clipToImageMask(image.get(), rect);
}

void GStateScope::applyCompositeOperation(CGGStateRef gstate)
{
    auto mode = compositeMode(CGGStateGetCompositeOperation(gstate));
    target().setCompositeOperation(mode.operation, mode.blendMode);
}

void GStateScope::applyStyle(CGGStateRef gstate)
{
    auto& target = this->target();
    CGStyleRef style = CGGStateGetStyle(gstate);
    if (!style) {
        target.clearDropShadow();
        target.setStyle(std::nullopt);
        return;
    }

    // CG style offsets and radii are in the base space (unaffected by the CTM). Convert them to the
    // current user space, so that the target maps them through its own user space to base space
    // transform. If that's not possible, pass them through and hope the base spaces match.
    std::optional<AffineTransform> baseToUser;
    CGFloat userToBaseScale = 1;
    auto inverseCTM = m_ctm.inverse();
    auto inverseBaseCTM = m_baseCTM.inverse();
    if (inverseCTM && inverseBaseCTM) {
        userToBaseScale = singularValue(*inverseBaseCTM * m_ctm, SingularValueSelection::Smallest);
        if (userToBaseScale)
            baseToUser = *inverseCTM * m_baseCTM;
    }
    target.setShadowsIgnoreTransforms(!baseToUser);

    auto convertOffset = [&](FloatSize offset) {
        if (!baseToUser)
            return offset;
        return baseToUser->mapPoint(FloatPoint(offset)) - baseToUser->mapPoint(FloatPoint());
    };
    auto convertRadius = [&](CGFloat radius) -> float {
        return baseToUser ? radius / userToBaseScale : radius;
    };

    switch (CGStyleGetType(style)) {
    case kCGStyleShadow: {
        const auto& shadowStyle = *static_cast<const CGShadowStyle*>(CGStyleGetData(style));
        auto radians = deg2rad(shadowStyle.azimuth - 180);
        auto offset = FloatSize(std::cos(radians), std::sin(radians)) * shadowStyle.height;
        target.setStyle(std::nullopt);
        target.setDropShadow({ convertOffset(offset), convertRadius(shadowStyle.radius), Color::createAndPreserveColorSpace(CGStyleGetColor(style)) });
        return;
    }
#if HAVE(CGSTYLE_COLORMATRIX_BLUR)
    case kCGStyleGaussianBlur: {
        const auto& blurStyle = *static_cast<const CGGaussianBlurStyle*>(CGStyleGetData(style));
        float radius = convertRadius(blurStyle.radius);
        target.clearDropShadow();
        target.setStyle(GraphicsGaussianBlur { { radius, radius } });
        return;
    }
    case kCGStyleColorMatrix: {
        const auto& colorMatrixStyle = *static_cast<const CGColorMatrixStyle*>(CGStyleGetData(style));
        GraphicsColorMatrix colorMatrix;
        for (auto [destination, source] : zippedRange(colorMatrix.values, std::span { colorMatrixStyle.matrix }))
            destination = source;
        target.clearDropShadow();
        target.setStyle(colorMatrix);
        return;
    }
#endif
    case kCGStyleFocusRing: {
        // GraphicsContextCG::drawFocusRing() scales the default radius by the zoom factor and the
        // user to base space scale; recover the zoom factor so that the target does the same.
        const auto& focusRingStyle = *static_cast<const CGFocusRingStyle*>(CGStyleGetData(style));
        CGColorRef color = CGStyleGetColor(style);
        CGFloat defaultRadius = defaultFocusRingRadius();
        if (!color || defaultRadius <= 0)
            break;
        CGFloat scale = inverseBaseCTM ? singularValue(*inverseBaseCTM * m_ctm, SingularValueSelection::Largest) : 1;
        if (scale <= 0)
            scale = 1;
        m_focusRing = FocusRing { Color::createAndPreserveColorSpace(color), static_cast<float>(focusRingStyle.radius / (defaultRadius * scale)), focusRingStyle.ordering };
        target.clearDropShadow();
        target.setStyle(std::nullopt);
        return;
    }
    default:
        break;
    }

    // Focus rings without a color (tinted rings) and unknown styles are not representable.
    m_delegate.logUnimplementedCallback("GStateScope (unsupported CGStyle)");
    target.clearDropShadow();
    target.setStyle(std::nullopt);
}

void GStateScope::applyLineParameters(CGGStateRef gstate)
{
    auto& target = this->target();

    // Line widths are in the CG user space; convert them to the target's user space.
    AffineTransform gstateCTM = *CGGStateGetCTM(gstate);
    double userSpaceScale = std::sqrt(std::abs(m_userSpaceTransform.a() * m_userSpaceTransform.d() - m_userSpaceTransform.b() * m_userSpaceTransform.c()));
    if (!userSpaceScale)
        userSpaceScale = 1;

    CGFloat lineWidth = CGGStateGetLineWidth(gstate);
    if (lineWidth == kCGLineWidthHairline) {
        // A hairline is one device pixel wide regardless of the CTM.
        auto scale = maximumScale(gstateCTM);
        lineWidth = scale ? 1 / scale : 1;
    } else
        lineWidth = CGGStateGetAdjustedLineWidth(gstate, gstateCTM);

    target.setStrokeStyle(StrokeStyle::SolidStroke);
    target.setStrokeThickness(lineWidth / userSpaceScale);
    target.setLineCap(lineCap(CGGStateGetLineCap(gstate)));
    target.setLineJoin(lineJoin(CGGStateGetLineJoin(gstate)));
    target.setMiterLimit(CGGStateGetMiterLimit(gstate));

    CGFloat phase = 0;
    size_t count = 0;
    const CGFloat* pattern = nullptr;
    if (CGDashRef dash = CGGStateGetLineDash(gstate))
        pattern = CGDashGetPattern(dash, &phase, &count);
    if (!pattern)
        count = 0;
    DashArray dashArray(unsafeMakeSpan(pattern, count));
    if (userSpaceScale != 1) {
        for (auto& length : dashArray)
            length /= userSpaceScale;
    }
    target.setLineDash(WTF::move(dashArray), phase / userSpaceScale);
}

bool GStateScope::applyBrush(CGGStateRef gstate, CGColorRef color, BrushType type)
{
    auto& target = this->target();

    if (color && CGColorGetPattern(color)) {
        if (type == BrushType::Fill && m_paint.contains(Paint::SpacedFillPattern)) {
            if (auto spacedPattern = createSpacedImagePattern(gstate, color)) {
                m_alpha *= CGColorGetAlpha(color);
                m_spacedFillPattern = WTF::move(spacedPattern);
                return true;
            }
        }
        RefPtr pattern = createPattern(gstate, color);
        if (!pattern)
            return false;
        // The alpha of a pattern color modulates the whole pattern.
        // FIXME: This is lossy if both the fill and stroke are patterns with different alphas.
        m_alpha *= CGColorGetAlpha(color);
        if (type == BrushType::Fill)
            target.setFillPattern(pattern.releaseNonNull());
        else
            target.setStrokePattern(pattern.releaseNonNull());
        return true;
    }

    auto webColor = color ? Color::createAndPreserveColorSpace(color) : Color::black;
    if (type == BrushType::Fill)
        target.setFillColor(webColor);
    else
        target.setStrokeColor(webColor);
    return true;
}

// Uncolored (stencil) patterns, including image mask patterns, are painted with the components of
// the pattern color. Its alpha is applied as the global alpha instead (see applyBrush()).
static RetainPtr<CGColorRef> createUncoloredPatternTint(CGColorRef patternColor)
{
    RetainPtr baseColorSpace = CGColorSpaceGetBaseColorSpace(CGColorGetColorSpace(patternColor));
    if (!baseColorSpace)
        return nullptr;
    RetainPtr tint = adoptCF(CGColorCreate(baseColorSpace.get(), CGColorGetComponents(patternColor)));
    if (!tint)
        return nullptr;
    return adoptCF(CGColorCreateCopyWithAlpha(tint.get(), 1));
}

// Rasterizes one tile (a `step` sized cell starting at the pattern bounds origin) of a CGPattern
// that WebCore can't represent directly, at roughly the device resolution.
static RetainPtr<CGImageRef> rasterizePatternTile(CGPatternRef pattern, CGColorRef color, const FloatRect& tileRect, const FloatSize& tileStep, const FloatSize& pixelSize, CGColorSpaceRef colorSpace)
{
    RetainPtr context = adoptCF(CGBitmapContextCreate(nullptr, pixelSize.width(), pixelSize.height(), 8, 0, colorSpace, kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    if (!context)
        return nullptr;

    CGContextScaleCTM(context.get(), pixelSize.width() / tileRect.width(), pixelSize.height() / tileRect.height());
    CGContextTranslateCTM(context.get(), -tileRect.x(), -tileRect.y());

    if (!CGPatternIsColored(pattern)) {
        auto tint = createUncoloredPatternTint(color);
        if (!tint)
            return nullptr;
        CGContextSetFillColorWithColor(context.get(), tint.get());
        CGContextSetStrokeColorWithColor(context.get(), tint.get());
    }

    // Draw neighbouring cells too, since a cell may extend past its step.
    int repeatX = tileStep.width() ? 1 : 0;
    int repeatY = tileStep.height() ? 1 : 0;
    for (int y = -repeatY; y <= repeatY; ++y) {
        for (int x = -repeatX; x <= repeatX; ++x) {
            CGContextSaveGState(context.get());
            CGContextTranslateCTM(context.get(), x * tileStep.width(), y * tileStep.height());
            CGContextDrawPatternCell(context.get(), pattern);
            CGContextRestoreGState(context.get());
        }
    }

    return adoptCF(CGBitmapContextCreateImage(context.get()));
}

// Renders an image mask (CGImageIsMask()) filled with `color`, at the resolution of the mask.
static RetainPtr<CGImageRef> createImageFromImageMask(CGImageRef imageMask, CGColorRef color, CGColorSpaceRef colorSpace)
{
    size_t width = CGImageGetWidth(imageMask);
    size_t height = CGImageGetHeight(imageMask);
    RetainPtr context = adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, colorSpace, kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    if (!context)
        return nullptr;
    CGContextSetInterpolationQuality(context.get(), kCGInterpolationNone);
    CGContextSetFillColorWithColor(context.get(), color);
    CGContextDrawImage(context.get(), CGRectMake(0, 0, width, height), imageMask);
    return adoptCF(CGBitmapContextCreateImage(context.get()));
}

// Remote targets copy the raw pixels of an image (see ShareableBitmap::createFromImagePixels()),
// which drops anything CG applies while drawing it: a mask or soft mask (CGImageCreateWithMask()),
// masking colors (CGImageCreateWithMaskingColors()) and decode arrays. Renders such images into a
// plain bitmap, at the resolution of the image.
static RetainPtr<CGImageRef> createFlattenedImage(CGImageRef image, CGColorSpaceRef fallbackColorSpace)
{
    RetainPtr colorSpace = CGImageGetColorSpace(image);
    if (!colorSpace || CGColorSpaceGetModel(colorSpace.get()) != kCGColorSpaceModelRGB || !CGColorSpaceSupportsOutput(colorSpace.get()))
        colorSpace = fallbackColorSpace;

    size_t width = CGImageGetWidth(image);
    size_t height = CGImageGetHeight(image);
    bool useFloat = CGImageGetBitsPerComponent(image) > 8;
    auto context = useFloat
        ? adoptCF(CGBitmapContextCreate(nullptr, width, height, 16, 0, colorSpace.get(), static_cast<CGBitmapInfo>(kCGImageAlphaPremultipliedLast) | static_cast<CGBitmapInfo>(kCGBitmapFloatComponents) | static_cast<CGBitmapInfo>(kCGBitmapByteOrder16Host)))
        : adoptCF(CGBitmapContextCreate(nullptr, width, height, 8, 0, colorSpace.get(), kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    if (!context)
        return nullptr;
    CGContextSetInterpolationQuality(context.get(), kCGInterpolationNone);
    CGContextDrawImage(context.get(), CGRectMake(0, 0, width, height), image);
    RetainPtr flattenedImage = adoptCF(CGBitmapContextCreateImage(context.get()));

#if HAVE(SUPPORT_HDR_DISPLAY_APIS)
    float headroom = CGImageGetContentHeadroom(image);
    if (flattenedImage && useFloat && headroom > 1) {
        if (RetainPtr imageWithHeadroom = adoptCF(CGImageCreateCopyWithContentHeadroom(headroom, flattenedImage.get())))
            return imageWithHeadroom;
    }
#endif
    return flattenedImage;
}

static bool needsFlattening(CGImageRef image)
{
    return CGImageGetMask(image) || CGImageGetMaskingColors(image) || CGImageGetDecode(image);
}

// Returns an image that draws like `image` would in CG, without masks, masking colors or decode
// arrays. Image masks are filled with `maskColor`; returns null if that's not possible.
static RetainPtr<CGImageRef> createDrawableImage(CGImageRef image, CGColorRef maskColor, CGColorSpaceRef colorSpace)
{
    if (CGImageIsMask(image)) {
        if (!maskColor || CGColorGetPattern(maskColor))
            return nullptr;
        return createImageFromImageMask(image, maskColor, colorSpace);
    }
    if (needsFlattening(image))
        return createFlattenedImage(image, colorSpace);
    return image;
}

RefPtr<Pattern> GStateScope::createPattern(CGGStateRef gstate, CGColorRef color)
{
    CGPatternRef cgPattern = CGColorGetPattern(color);
    auto inverseCTM = m_ctm.inverse();
    if (!inverseCTM)
        return nullptr;

    // Steps this large mean the pattern doesn't repeat in that direction (see Pattern::createPlatformPattern()).
    static constexpr CGFloat nonRepeatingStep = 1 << 21;

    FloatRect bounds = CGPatternGetBounds(cgPattern);
    CGSize step = CGPatternGetStep(cgPattern);
    bool repeatX = std::abs(step.width) < nonRepeatingStep;
    bool repeatY = std::abs(step.height) < nonRepeatingStep;
    if (bounds.isEmpty())
        return nullptr;

    // Pattern space -> CG default user space (the base space) -> device space.
    CGSize phase = CGGStateGetPatternPhase(gstate);
    auto patternToDevice = m_baseCTM * AffineTransform::makeTranslation(FloatSize(phase)) * AffineTransform(CGPatternGetMatrix(cgPattern));

    RefPtr<NativeImage> nativeImage;
    FloatRect tileRect = bounds;
    bool tileMatchesStep = (!repeatX || step.width == bounds.width()) && (!repeatY || step.height == bounds.height());
    if (CGImageRef image = CGPatternGetImage(cgPattern); image && tileMatchesStep) {
        // CGContextDrawTiledImage() makes image mask patterns uncolored.
        RetainPtr<CGColorRef> maskColor;
        if (CGImageIsMask(image) && !CGPatternIsColored(cgPattern))
            maskColor = createUncoloredPatternTint(color);
        nativeImage = m_delegate.nativeImageForDrawing(image, maskColor.get());
    }

    if (!nativeImage) {
        // Rasterize the pattern cell. The tile is the step rect so that spacing is preserved.
        if (repeatX)
            tileRect.setWidth(std::abs(step.width));
        if (repeatY)
            tileRect.setHeight(std::abs(step.height));
        if (tileRect.isEmpty())
            return nullptr;

        static constexpr float maximumTileDimension = 4096;
        float scale = maximumScale(patternToDevice);
        FloatSize pixelSize = tileRect.size() * scale;
        pixelSize = pixelSize.shrunkTo({ maximumTileDimension, maximumTileDimension }).expandedTo({ 1, 1 });
        pixelSize = { std::ceil(pixelSize.width()), std::ceil(pixelSize.height()) };

        FloatSize tileStep { repeatX ? tileRect.width() : 0, repeatY ? tileRect.height() : 0 };
        auto image = rasterizePatternTile(cgPattern, color, tileRect, tileStep, pixelSize, target().colorSpace().platformColorSpace());
        if (!image)
            return nullptr;
        nativeImage = NativeImage::create(WTF::move(image));
        if (!nativeImage)
            return nullptr;
    }

    // Image pixels (y-down) -> pattern space (y-up, the image fills `tileRect`).
    FloatSize imageSize = nativeImage->size();
    AffineTransform imageToPattern;
    imageToPattern.translate(tileRect.location());
    imageToPattern.scale(tileRect.width() / imageSize.width(), tileRect.height() / imageSize.height());
    imageToPattern.translate(0, imageSize.height());
    imageToPattern.scale(1, -1);

    // A WebCore pattern transform maps image pixels (y-down) to the user space at the time of
    // drawing, which is the gstate CTM.
    auto patternSpaceTransform = *inverseCTM * patternToDevice * imageToPattern;

    return Pattern::create(SourceImage::ImageVariant { nativeImage.releaseNonNull() }, { repeatX, repeatY, patternSpaceTransform });
}

// GraphicsContextCG::drawPattern() (which CG-backed and remote targets use) tiles the image at
// `phase + patternTransform * pixel`, every image size + spacing (in user space), as a CGPattern
// with a step larger than its bounds; this inverts that. Only for image patterns whose image space
// maps to the target's user space with a scale and a translation (flipped as needed for a positive
// scale), without a CG style (filling through a clip would clip the shadow too).
auto GStateScope::createSpacedImagePattern(CGGStateRef gstate, CGColorRef color) -> std::optional<SpacedImagePattern>
{
    CGPatternRef cgPattern = CGColorGetPattern(color);
    CGImageRef image = CGPatternGetImage(cgPattern);
    if (!image || CGGStateGetStyle(gstate))
        return std::nullopt;

    // Steps this large mean the pattern doesn't repeat in that direction (see createPattern()).
    static constexpr CGFloat nonRepeatingStep = 1 << 21;
    FloatRect bounds = CGPatternGetBounds(cgPattern);
    CGSize step = CGPatternGetStep(cgPattern);
    if (bounds.isEmpty() || step.width < bounds.width() || step.height < bounds.height() || step.width >= nonRepeatingStep || step.height >= nonRepeatingStep)
        return std::nullopt;
    if (step.width == bounds.width() && step.height == bounds.height())
        return std::nullopt;

    auto inverseCTM = m_ctm.inverse();
    if (!inverseCTM)
        return std::nullopt;

    RetainPtr<CGColorRef> maskColor;
    if (CGImageIsMask(image) && !CGPatternIsColored(cgPattern))
        maskColor = createUncoloredPatternTint(color);
    RefPtr nativeImage = m_delegate.nativeImageForDrawing(image, maskColor.get());
    if (!nativeImage)
        return std::nullopt;

    // Pattern space -> CG default user space (the base space) -> device space -> the target's user space.
    CGSize phase = CGGStateGetPatternPhase(gstate);
    auto patternToUser = *inverseCTM * m_baseCTM * AffineTransform::makeTranslation(FloatSize(phase)) * AffineTransform(CGPatternGetMatrix(cgPattern));

    // Image pixels (y-down) -> pattern space (y-up, the image fills `bounds`).
    FloatSize imageSize = nativeImage->size();
    AffineTransform imageToPattern;
    imageToPattern.translate(bounds.location());
    imageToPattern.scale(bounds.width() / imageSize.width(), bounds.height() / imageSize.height());
    imageToPattern.translate(0, imageSize.height());
    imageToPattern.scale(1, -1);

    auto imageToUser = patternToUser * imageToPattern;
    if (!isNearlyRectilinear(imageToUser) || !imageToUser.a() || !imageToUser.d())
        return std::nullopt;

    // The flip is its own inverse: flipped user space = flip * user space.
    auto userSpaceFlip = AffineTransform::makeScale({ imageToUser.a() < 0 ? -1.f : 1.f, imageToUser.d() < 0 ? -1.f : 1.f });
    auto imageToFlippedUser = userSpaceFlip * imageToUser;
    FloatSize spacing { static_cast<float>((step.width - bounds.width()) * std::abs(patternToUser.a())), static_cast<float>((step.height - bounds.height()) * std::abs(patternToUser.d())) };
    return SpacedImagePattern { nativeImage.releaseNonNull(), userSpaceFlip, AffineTransform(imageToFlippedUser.a(), 0, 0, imageToFlippedUser.d(), 0, 0), FloatPoint(imageToFlippedUser.e(), imageToFlippedUser.f()), spacing };
}

void GStateScope::drawSpacedFillPattern(const FloatRect& rect)
{
    ASSERT(m_spacedFillPattern);
    auto& target = this->target();
    auto& pattern = *m_spacedFillPattern;
    auto ctm = target.getCTM();
    target.concatCTM(pattern.userSpaceFlip);
    target.drawPattern(pattern.image, pattern.userSpaceFlip.mapRect(rect), { { }, pattern.image->size() }, pattern.patternTransform, pattern.phase, pattern.spacing, { target.compositeOperation(), target.blendMode() });
    target.setCTM(ctm);
}

RetainPtr<CGContextRef> GraphicsContextCGDelegate::createCGContext(GraphicsContext& target)
{
    // The CGContextDelegate owns the GraphicsContextCGDelegate; it is deleted in finalize().
    auto* delegateInfo = new GraphicsContextCGDelegate(target);
    RetainPtr contextDelegate = adoptCF(CGContextDelegateCreate(delegateInfo));
    if (!contextDelegate) {
        delete delegateInfo;
        return nullptr;
    }
    installCallbacks(contextDelegate.get());

    RetainPtr context = adoptCF(CGContextCreateWithDelegate(contextDelegate.get(), kCGContextTypeUnknown, nullptr, nullptr));
    if (!context)
        return nullptr;

    // Start with the same user space as the target.
    CGAffineTransform targetCTM = target.getCTM();
    CGContextConcatCTM(context.get(), targetCTM);
    CGContextSetBaseCTM(context.get(), targetCTM);
    return context;
}

UniqueRef<GraphicsContext> GraphicsContextCGDelegate::createGraphicsContext(GraphicsContext& target)
{
    // A plain GraphicsContextCG: WebKit drawing reaches the target only through CG and the delegate
    // callbacks, like any other CG client's drawing (for example PDFKit's). It reports the target's
    // rendering mode, so that WebKit draws as it would into the target (e.g. blurred shadows as CG
    // shadow styles rather than its own software ShadowBlur images).
    return makeUniqueRef<GraphicsContextCG>(createCGContext(target).get(), GraphicsContextCG::CGContextSource::Unknown, target.renderingMode());
}

GraphicsContextCGDelegate::GraphicsContextCGDelegate(GraphicsContext& target)
    : m_target(target)
{
}

GraphicsContextCGDelegate::~GraphicsContextCGDelegate()
{
    // Close transparency layers that CG left open, so the target's state stays balanced.
    while (!m_transparencyLayers.isEmpty()) {
        m_target.endTransparencyLayer();
        m_transparencyLayers.removeLast();
    }
}

GraphicsContextCGDelegate& GraphicsContextCGDelegate::fromDelegate(CGContextDelegateRef delegate)
{
    return *static_cast<GraphicsContextCGDelegate*>(CGContextDelegateGetInfo(delegate));
}

void GraphicsContextCGDelegate::installCallbacks(CGContextDelegateRef delegate)
{
    auto set = [&](CGContextDelegateCallbackName name, auto callback) {
        CGContextDelegateSetCallback(delegate, name, reinterpret_cast<CGContextDelegateCallback>(callback));
    };

    set(deFinalize, &finalize);
    set(deGetColorTransform, &getColorTransform);
    set(deGetTransform, &getTransform);
    set(deGetBounds, &getBounds);
    set(deGetColorSpace, &getColorSpace);
    set(deGetDelegateName, &getName);
    set(deDrawLines, &drawLines);
    set(deDrawRects, &drawRects);
    set(deDrawPath, &drawPath);
    set(deDrawPathDirect, &drawPathDirect);
    set(deStrokeArc, &strokeArc);
    set(deDrawImage, &drawImage);
    set(deDrawImages, &drawImages);
    set(deDrawImageFromRect, &drawImageFromRect);
    set(deDrawImageApplyingToneMapping, &drawImageApplyingToneMapping);
    set(deDrawGlyphs, &drawGlyphs);
    set(deDrawShading, &drawShading);
    set(deDrawLinearGradient, &drawLinearGradient);
    set(deDrawRadialGradient, &drawRadialGradient);
    set(deDrawConicGradient, &drawConicGradient);
    set(deDrawDisplayList, &drawDisplayList);
    set(deOperation, &operation);
    set(deBeginLayer, &beginLayer);
    set(deEndLayer, &endLayer);
    // Intentionally not implemented:
    // - GetLayer / DrawLayer: without GetLayer, CGLayerCreateWithContext() records into a CG
    //   display list delegate because we implement DrawDisplayList; drawing the CGLayer replays
    //   the display list through drawDisplayList() (without DrawDisplayList, CG would use a
    //   bitmap delegate and draw it back as an image).
    // - BeginPage / EndPage: the target isn't paginated (bitmap contexts don't implement them).
    // - DrawWindowContents: window server content; can't be represented in a GraphicsContext.
    // - CreateImage / GetBitmapContextInfo: there is no backing store to read back (CG returns
    //   nullptr, as for other non-bitmap contexts).
    // - SerializeDisplayList: only meaningful for display list delegates.
}

void GraphicsContextCGDelegate::logUnimplementedCallback(const char* callbackName)
{
    LOG_ERROR("GraphicsContextCGDelegate: %s is not implemented", callbackName);
}

#pragma mark - Lifecycle

void GraphicsContextCGDelegate::finalize(CGContextDelegateRef delegate)
{
    delete &fromDelegate(delegate);
}

#pragma mark - Queries

CGColorTransformRef GraphicsContextCGDelegate::getColorTransform(CGContextDelegateRef delegate, CGRenderingStateRef, CGGStateRef)
{
    // CG doesn't convert colors for delegates (colors, images and gradients reach the callbacks in
    // their own color spaces and the target converts them). Like bitmap contexts, return a
    // transform to the destination color space, so that CGContextCopyDeviceColorSpace() reports
    // the target's color space. The target's color space doesn't change, so it's created once.
    auto& graphicsContextDelegate = fromDelegate(delegate);
    if (!graphicsContextDelegate.m_colorTransform)
        graphicsContextDelegate.m_colorTransform = adoptCF(CGColorTransformCreate(graphicsContextDelegate.target().colorSpace().platformColorSpace(), nullptr));
    return graphicsContextDelegate.m_colorTransform.get();
}

CGAffineTransform GraphicsContextCGDelegate::getTransform(CGContextDelegateRef, CGRenderingStateRef, CGGStateRef)
{
    // The default user space to device space transform. The gstate CTM is already the absolute
    // transform to the target's device space (see createCGContext()), so this is the identity.
    return CGAffineTransformIdentity;
}

CGRect GraphicsContextCGDelegate::getBounds(CGContextDelegateRef delegate, CGRenderingStateRef, CGGStateRef)
{
    // The device space bounds. CG intersects these with the gstate clip to answer
    // CGContextGetClipBoundingBox(), which GraphicsContextCG::clipBounds() uses.
    auto& target = fromDelegate(delegate).target();
    return target.getCTM().mapRect(FloatRect(target.clipBounds()));
}

CGColorSpaceRef GraphicsContextCGDelegate::getColorSpace(CGContextDelegateRef delegate, CGRenderingStateRef, CGGStateRef)
{
    return fromDelegate(delegate).target().colorSpace().platformColorSpace();
}

const char* GraphicsContextCGDelegate::getName(CGContextDelegateRef)
{
    return "WebCore::GraphicsContextCGDelegate";
}

#pragma mark - Drawing primitives

// Re-creating a shape can change where its outline starts and its direction, which matters for dashes.
static PreserveShapes preserveShapesForDrawingMode(CGGStateRef gstate, CGPathDrawingMode mode)
{
    if (!GStateScope::paintForDrawingMode(mode).contains(GStateScope::Paint::Stroke))
        return PreserveShapes::Yes;
    CGFloat phase = 0;
    size_t dashCount = 0;
    if (CGDashRef dash = CGGStateGetLineDash(gstate))
        CGDashGetPattern(dash, &phase, &dashCount);
    return dashCount ? PreserveShapes::No : PreserveShapes::Yes;
}

static void drawPathWithMode(GStateScope& scope, CGPathDrawingMode mode, const Path& path)
{
    auto& target = scope.target();
    if (scope.hasSpacedFillPattern() && !scope.focusRing() && GStateScope::paintForDrawingMode(mode).contains(GStateScope::Paint::Fill)) {
        {
            GraphicsContextStateSaver stateSaver(target);
            target.clipPath(path, GStateScope::windRuleForDrawingMode(mode));
            scope.drawSpacedFillPattern(path.fastBoundingRect());
        }
        if (GStateScope::paintForDrawingMode(mode).contains(GStateScope::Paint::Stroke))
            target.strokePath(path);
        return;
    }
    if (auto& focusRing = scope.focusRing(); focusRing && GStateScope::paintForDrawingMode(mode).contains(GStateScope::Paint::Fill)) {
        // A focus ring style replaces the fill (kCGFocusRingOrderingNone), or is drawn below or above it.
        // The outline width argument is ignored by GraphicsContextCG.
        if (focusRing->ordering == kCGFocusRingOrderingAbove)
            target.fillPath(path);
        target.drawFocusRing(path, 0, focusRing->color, focusRing->zoomFactor);
        if (focusRing->ordering == kCGFocusRingOrderingBelow)
            target.fillPath(path);
        return;
    }

    switch (mode) {
    case kCGPathFill:
    case kCGPathEOFill:
        target.setFillRule(GStateScope::windRuleForDrawingMode(mode));
        target.fillPath(path);
        return;
    case kCGPathStroke:
        target.strokePath(path);
        return;
    case kCGPathFillStroke:
    case kCGPathEOFillStroke:
        target.setFillRule(GStateScope::windRuleForDrawingMode(mode));
        target.drawPath(path);
        return;
    }
}

// Points are in user space. Each pair of points is a separate line segment; all are stroked together.
void GraphicsContextCGDelegate::drawLines(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, const CGPoint points[], size_t count)
{
    if (count < 2)
        return;

    GStateScope scope(fromDelegate(delegate), rstate, gstate, GStateScope::Paint::Stroke);
    if (!scope.shouldDraw())
        return;

    auto pointsSpan = unsafeMakeSpan(points, count);
    if (count < 4) {
        scope.target().strokeLine({ FloatPoint(pointsSpan[0]), FloatPoint(pointsSpan[1]) });
        return;
    }

    Path path;
    for (size_t i = 0; i + 1 < count; i += 2) {
        path.moveTo(pointsSpan[i]);
        path.addLineTo(pointsSpan[i + 1]);
    }
    scope.target().strokePath(path);
}

// Rects are in user space. Multiple rects are drawn as a single path, like CG's fallback.
CGError GraphicsContextCGDelegate::drawRects(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGPathDrawingMode mode, const CGRect rects[], size_t count)
{
    if (!count)
        return kCGErrorSuccess;

    GStateScope scope(fromDelegate(delegate), rstate, gstate, GStateScope::paintForDrawingMode(mode) | GStateScope::Paint::SpacedFillPattern);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    auto& target = scope.target();
    auto rectsSpan = unsafeMakeSpan(rects, count);
    if (count == 1 && !scope.focusRing()) {
        FloatRect rect = CGRectStandardize(rectsSpan[0]);
        switch (mode) {
        case kCGPathFill:
        case kCGPathEOFill:
            if (scope.hasSpacedFillPattern()) {
                scope.drawSpacedFillPattern(rect);
                return kCGErrorSuccess;
            }
            target.fillRect(rect);
            return kCGErrorSuccess;
        case kCGPathStroke:
            target.strokeRect(rect, target.strokeThickness());
            return kCGErrorSuccess;
        case kCGPathFillStroke:
        case kCGPathEOFillStroke:
            break;
        }
    }

    Path path;
    for (auto& rect : rectsSpan)
        path.addRect(CGRectStandardize(rect));
    drawPathWithMode(scope, mode, path);
    return kCGErrorSuccess;
}

// The path has already been transformed to device space by the CTM. Transform it back to user
// space so that the line width and dash pattern are applied in user space.
CGError GraphicsContextCGDelegate::drawPath(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGPathDrawingMode mode, CGPathRef path)
{
    GStateScope scope(fromDelegate(delegate), rstate, gstate, GStateScope::paintForDrawingMode(mode) | GStateScope::Paint::SpacedFillPattern);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    auto inverseCTM = scope.ctm().inverse();
    if (!inverseCTM)
        return kCGErrorSuccess;

    drawPathWithMode(scope, mode, pathFromCGPath(path, *inverseCTM, preserveShapesForDrawingMode(gstate, mode)));
    return kCGErrorSuccess;
}

// Unlike drawPath, the path given to drawPathDirect is in user space.
CGError GraphicsContextCGDelegate::drawPathDirect(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGPathDrawingMode mode, CGPathRef path, const CGRect*)
{
    GStateScope scope(fromDelegate(delegate), rstate, gstate, GStateScope::paintForDrawingMode(mode) | GStateScope::Paint::SpacedFillPattern);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    drawPathWithMode(scope, mode, pathFromCGPath(path, { }, preserveShapesForDrawingMode(gstate, mode)));
    return kCGErrorSuccess;
}

// The arc is in user space.
CGError GraphicsContextCGDelegate::strokeArc(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGFloat x, CGFloat y, CGFloat radius, CGFloat startAngle, CGFloat endAngle, bool clockwise)
{
    GStateScope scope(fromDelegate(delegate), rstate, gstate, GStateScope::Paint::Stroke);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    // CG's `clockwise` is relative to a y-up coordinate system; see addToCGPath(CGMutablePathRef, const PathArc&).
    auto direction = clockwise ? RotationDirection::Counterclockwise : RotationDirection::Clockwise;
    scope.target().strokeArc({ FloatPoint(x, y), static_cast<float>(radius), static_cast<float>(startAngle), static_cast<float>(endAngle), direction });
    return kCGErrorSuccess;
}

RefPtr<NativeImage> GraphicsContextCGDelegate::nativeImageForDrawing(CGImageRef image, CGColorRef maskColor)
{
    if (CGImageIsMask(image)) {
        auto drawableImage = createDrawableImage(image, maskColor, m_target.colorSpace().platformColorSpace());
        return drawableImage ? NativeImage::create(WTF::move(drawableImage)) : nullptr;
    }

    return m_nativeImages.ensure(image, [&] -> RefPtr<NativeImage> {
        auto drawableImage = createDrawableImage(image, nullptr, m_target.colorSpace().platformColorSpace());
        return drawableImage ? NativeImage::create(WTF::move(drawableImage)) : nullptr;
    }).iterator->value;
}

#if HAVE(SUPPORT_HDR_DISPLAY_APIS)
// GraphicsContextCG::drawNativeImage() sets these tone mapping options for PlatformDynamicRangeLimit::standard().
static bool isStandardDynamicRangeLimit(const CGContentToneMappingInfo& toneMappingInfo)
{
    if (!toneMappingInfo.options)
        return false;
    auto floatValue = [&](CFStringRef key) -> std::optional<float> {
        auto number = dynamic_cf_cast<CFNumberRef>(CFDictionaryGetValue(toneMappingInfo.options, key));
        float value;
        if (!number || !CFNumberGetValue(number, kCFNumberFloatType, &value))
            return std::nullopt;
        return value;
    };
    return floatValue(kCGContentEDRStrength) == 0.f && floatValue(kCGConstrainedDynamicRange).value_or(0) == 0.f;
}
#endif

// GraphicsContext::drawNativeImage() takes the compositing mode from the options, not from the
// context, so pass along the mode GStateScope applied. Interpolation quality comes from the context
// (the gstate quality). CGImageGetShouldInterpolate() is deliberately not mapped: the target sees the
// same CGImage, so a CG-backed target applies it exactly like CG would (it only matters when the
// gstate quality is kCGInterpolationDefault), and remote targets ignore it as they do for any image
// drawn directly (the GPU process re-creates images with ShouldInterpolate::Yes). Forcing
// DoNotInterpolate made e.g. ShareableBitmap images (created with ShouldInterpolate::No) render
// point-sampled on remote targets.
static ImagePaintingOptions imagePaintingOptions(const GraphicsContext& target, CGGStateRef gstate)
{
    ImagePaintingOptions options { target.compositeOperation(), target.blendMode(), InterpolationQuality::Default };
#if HAVE(SUPPORT_HDR_DISPLAY_APIS)
    // GraphicsContextCG::drawNativeImage() lowers the EDR target headroom for images with more headroom.
    if (float headroom = CGGStateGetEDRTargetHeadroom(gstate); headroom > 0)
        options = { options, Headroom(headroom) };
    if (isStandardDynamicRangeLimit(CGGStateGetContentToneMappingInfo(gstate)))
        options = { options, PlatformDynamicRangeLimit::standard(), DrawsHDRContent::Yes };
#else
    UNUSED_PARAM(gstate);
#endif
    return options;
}

// CG draws images y-up in `rect` (row 0 at the maximum y), while GraphicsContext::drawNativeImage()
// draws them y-down. Flip around the rect. `sourceRect` is in image pixels, with row 0 at the top.
static void drawNativeImageInRect(GraphicsContext& target, CGGStateRef gstate, NativeImage& nativeImage, const FloatRect& rect, const FloatRect& sourceRect)
{
    target.translate(0, 2 * rect.y() + rect.height());
    target.scale(FloatSize(1, -1));
    target.drawNativeImage(nativeImage, rect, sourceRect, imagePaintingOptions(target, gstate));
}

// `rect` is in user space.
CGError GraphicsContextCGDelegate::drawImage(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGRect rect, CGImageRef image)
{
    auto& graphicsContextDelegate = fromDelegate(delegate);

    // Image masks are painted with the fill color.
    RefPtr nativeImage = graphicsContextDelegate.nativeImageForDrawing(image, CGGStateGetFillColor(gstate));
    if (!nativeImage && !CGImageIsMask(image))
        return kCGErrorSuccess;

    GStateScope scope(graphicsContextDelegate, rstate, gstate, nativeImage ? OptionSet<GStateScope::Paint> { } : GStateScope::Paint::Fill);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    if (!nativeImage) {
        // An image mask with a pattern fill color: fill the image rect, clipped to the mask.
        scope.clipToImageMask(image, rect);
        scope.target().fillRect(CGRectStandardize(rect));
        return kCGErrorSuccess;
    }

    drawNativeImageInRect(scope.target(), gstate, *nativeImage, CGRectStandardize(rect), { { }, nativeImage->size() });
    return kCGErrorSuccess;
}

// For each image, CGContextDrawImages() tiles the image (placed at `imageRects[i]`) over `destinationRects[i]`,
// or over the whole clip if there are no destination rects. Rects are in user space.
CGError GraphicsContextCGDelegate::drawImages(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, const CGRect imageRects[], const CGImageRef images[], const CGRect destinationRects[], size_t count)
{
    if (!count || !imageRects || !images)
        return kCGErrorSuccess;

    auto imageRectsSpan = unsafeMakeSpan(imageRects, count);
    auto imagesSpan = unsafeMakeSpan(images, count);

    // Let CG fall back to drawing image masks one by one when the fill color is a pattern.
    CGColorRef fillColor = CGGStateGetFillColor(gstate);
    if (fillColor && CGColorGetPattern(fillColor) && std::ranges::any_of(imagesSpan, [](CGImageRef image) { return image && CGImageIsMask(image); }))
        return kCGErrorNotImplemented;

    auto& graphicsContextDelegate = fromDelegate(delegate);
    auto destinationRectsSpan = destinationRects ? unsafeMakeSpan(destinationRects, count) : std::span<const CGRect> { };

    for (size_t i = 0; i < count; ++i) {
        CGImageRef image = imagesSpan[i];
        FloatRect imageRect = CGRectStandardize(imageRectsSpan[i]);
        if (!image || imageRect.isEmpty())
            continue;

        GStateScope scope(graphicsContextDelegate, rstate, gstate);
        if (!scope.shouldDraw())
            continue;
        auto& target = scope.target();

        FloatRect destinationRect;
        if (destinationRects)
            destinationRect = CGRectStandardize(destinationRectsSpan[i]);
        else if (auto inverseCTM = scope.ctm().inverse())
            destinationRect = inverseCTM->mapRect(FloatRect(CGGStateGetClipBoundingBox(gstate)));
        if (destinationRect.isEmpty())
            continue;

        RefPtr nativeImage = graphicsContextDelegate.nativeImageForDrawing(image, fillColor);
        if (!nativeImage)
            continue;

        if (imageRect.contains(destinationRect)) {
            if (imageRect != destinationRect)
                target.clip(destinationRect);
            drawNativeImageInRect(target, gstate, *nativeImage, imageRect, { { }, nativeImage->size() });
            continue;
        }

        // Image pixels (y-down) -> user space (y-up, the image fills `imageRect`).
        FloatSize imageSize = nativeImage->size();
        AffineTransform patternSpaceTransform;
        patternSpaceTransform.translate(imageRect.x(), imageRect.maxY());
        patternSpaceTransform.scale(imageRect.width() / imageSize.width(), -imageRect.height() / imageSize.height());
        target.setFillPattern(Pattern::create(SourceImage::ImageVariant { nativeImage.releaseNonNull() }, { true, true, patternSpaceTransform }));
        target.fillRect(destinationRect);
    }
    return kCGErrorSuccess;
}

// `destinationRect` is in user space; `sourceRect` is in image pixels, with row 0 at the top (like CGImageCreateWithImageInRect()).
CGError GraphicsContextCGDelegate::drawImageFromRect(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGRect destinationRect, CGImageRef image, CGRect sourceRect)
{
    // CG's fallback draws CGImageCreateWithImageInRect(image, sourceRect), which uses the integral
    // source rect clipped to the image.
    FloatRect imageBounds { 0, 0, static_cast<float>(CGImageGetWidth(image)), static_cast<float>(CGImageGetHeight(image)) };
    FloatRect clippedSourceRect = intersection(CGRectIntegral(CGRectStandardize(sourceRect)), imageBounds);
    if (clippedSourceRect.isEmpty())
        return kCGErrorSuccess;

    // Let CG fall back to drawing a sub-image with drawImage() for image masks with a pattern fill color.
    auto& graphicsContextDelegate = fromDelegate(delegate);
    RefPtr nativeImage = graphicsContextDelegate.nativeImageForDrawing(image, CGGStateGetFillColor(gstate));
    if (!nativeImage)
        return CGImageIsMask(image) ? kCGErrorNotImplemented : kCGErrorSuccess;

    GStateScope scope(graphicsContextDelegate, rstate, gstate);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    drawNativeImageInRect(scope.target(), gstate, *nativeImage, CGRectStandardize(destinationRect), clippedSourceRect);
    return kCGErrorSuccess;
}

// Only called for non-default tone mapping (see CGContextDrawImageApplyingToneMapping()). GraphicsContext
// has no per-draw tone mapping method, so the image is drawn with the gstate's headroom.
CGError GraphicsContextCGDelegate::drawImageApplyingToneMapping(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGRect rect, CGImageRef image, CGToneMapping method, CFDictionaryRef options)
{
    if (method != kCGToneMappingDefault || options) {
        // FIXME: The tone mapping method and options are not representable.
        fromDelegate(delegate).logUnimplementedCallback("drawImageApplyingToneMapping (tone mapping method/options)");
    }
    return drawImage(delegate, rstate, gstate, rect, image);
}

// Rebuilds an sfnt (OpenType) file from the tables of `font`.
static RefPtr<SharedBuffer> createFontDataFromTables(CGFontRef font)
{
    RetainPtr tags = adoptCF(CGFontCopyTableTags(font));
    if (!tags)
        return nullptr;

    Vector<std::pair<uint32_t, RetainPtr<CFDataRef>>> tables;
    CFIndex tagCount = CFArrayGetCount(tags.get());
    for (CFIndex i = 0; i < tagCount; ++i) {
        // The array holds the tags themselves, not CF objects.
        auto tag = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(CFArrayGetValueAtIndex(tags.get(), i)));
        if (RetainPtr table = adoptCF(CGFontCopyTableForTag(font, tag)))
            tables.append({ tag, WTF::move(table) });
    }
    if (tables.isEmpty())
        return nullptr;
    std::ranges::sort(tables, { }, [](auto& table) {
        return table.first;
    });

    Vector<uint8_t> result;
    auto append16 = [&](uint16_t value) {
        result.append(value >> 8);
        result.append(value);
    };
    auto append32 = [&](uint32_t value) {
        append16(value >> 16);
        append16(value);
    };
    auto checksum = [](std::span<const uint8_t> bytes) {
        uint32_t sum = 0;
        for (size_t i = 0; i < bytes.size(); i += 4) {
            uint32_t word = 0;
            for (size_t j = 0; j < 4; ++j)
                word = (word << 8) | (i + j < bytes.size() ? bytes[i + j] : 0);
            sum += word;
        }
        return sum;
    };

    constexpr uint32_t cffTag = 0x43464620; // 'CFF '
    constexpr uint32_t cff2Tag = 0x43464632; // 'CFF2'
    bool isCFF = std::ranges::any_of(tables, [](auto& table) {
        return table.first == cffTag || table.first == cff2Tag;
    });
    uint16_t numTables = tables.size();
    uint16_t entrySelector = 0;
    while ((2u << entrySelector) <= numTables)
        ++entrySelector;
    uint16_t searchRange = (1u << entrySelector) * 16;

    append32(isCFF ? 0x4F54544F /* 'OTTO' */ : 0x00010000);
    append16(numTables);
    append16(searchRange);
    append16(entrySelector);
    append16(numTables * 16 - searchRange);

    uint32_t offset = 12 + 16 * numTables;
    for (auto& [tag, table] : tables) {
        auto bytes = span(table.get());
        append32(tag);
        append32(checksum(bytes));
        append32(offset);
        append32(bytes.size());
        offset += roundUpToMultipleOf<4>(bytes.size());
    }
    for (auto& [tag, table] : tables) {
        auto bytes = span(table.get());
        result.append(bytes);
        result.grow(roundUpToMultipleOf<4>(result.size()));
    }
    return SharedBuffer::create(WTF::move(result));
}

// Remote targets re-create installed fonts from their descriptor attributes, looking them up by
// name (see createCTFont()). System UI fonts (".SFNS-Regular", ".SFNSMono-Medium", ...) are hidden
// from name lookup, so CoreText resolves their instances to Times (and logs about it) unless the
// descriptor includes hidden fonts. Returns null if the font doesn't need a different descriptor or
// the new one doesn't resolve to the same font file and face.
static RetainPtr<CTFontRef> createInstalledFontForRemoteTargets(CTFontRef font, CFDictionaryRef variation)
{
    RetainPtr postScriptName = adoptCF(CTFontCopyPostScriptName(font));
    bool isHiddenFont = postScriptName && CFStringHasPrefix(postScriptName.get(), CFSTR("."));
    if (!isHiddenFont && !variation)
        return nullptr;

    RetainPtr descriptor = adoptCF(CTFontCopyFontDescriptor(font));
    RetainPtr attributes = adoptCF(CFDictionaryCreateMutableCopy(kCFAllocatorDefault, 0, adoptCF(CTFontDescriptorCopyAttributes(descriptor.get())).get()));
    if (variation)
        CFDictionarySetValue(attributes.get(), kCTFontVariationAttribute, variation);
    auto options = CTFontDescriptorGetOptions(descriptor.get());
    if (isHiddenFont)
        options |= kCTFontDescriptorOptionIncludeHiddenFonts;

    RetainPtr newDescriptor = adoptCF(CTFontDescriptorCreateWithAttributesAndOptions(attributes.get(), options));
    RetainPtr newFont = adoptCF(CTFontCreateWithFontDescriptor(newDescriptor.get(), CTFontGetSize(font), nullptr));
    if (!newFont || !safeCFEqual(adoptCF(CTFontCopyPostScriptName(newFont.get())).get(), postScriptName.get()))
        return nullptr;
    // Several font files can have faces with the same name (e.g. PingFang.ttc and the hidden
    // PingFangUI.ttc), with different glyph IDs.
    if (!safeCFEqual(adoptCF(CTFontCopyAttribute(newFont.get(), kCTFontURLAttribute)).get(), adoptCF(CTFontCopyAttribute(font, kCTFontURLAttribute)).get()))
        return nullptr;
    return newFont;
}

Ref<Font> GraphicsContextCGDelegate::fontForDrawing(CGFontRef cgFont, CGFloat size)
{
    float fontSize = size;
    return m_fonts.ensure({ cgFont, std::bit_cast<uint32_t>(fontSize) }, [&] {
        RetainPtr ctFont = adoptCF(CTFontCreateWithGraphicsFont(cgFont, fontSize, nullptr, nullptr));

        // For an instance of a variable font, CTFontCreateWithGraphicsFont() names the instance (e.g.
        // ".SFNS-Regular_wdth_opsz110000_GRAD_wght") but doesn't always put its variations in the font
        // descriptor. Remote targets re-create the font from the descriptor attributes, so add them.
        RetainPtr variation = adoptCF(CTFontCopyVariation(ctFont.get()));
        if (variation && !CFDictionaryGetCount(variation.get()))
            variation = nullptr;

        // Fonts that aren't backed by a file (web fonts) need their data so that remote targets can
        // re-create them. CG only exposes the tables, so rebuild the font file from those.
        RefPtr<FontCustomPlatformData> customPlatformData;
        if (!adoptCF(CTFontCopyAttribute(ctFont.get(), kCTFontURLAttribute))) {
            customPlatformData = m_customFontData.ensure(cgFont, [&] -> RefPtr<FontCustomPlatformData> {
                RefPtr fontData = createFontDataFromTables(cgFont);
                return fontData ? FontCustomPlatformData::create(*fontData, { }) : nullptr;
            }).iterator->value;
            // The rebuilt font file holds the default instance; create the font from it the way remote
            // targets will (FontPlatformData::create()), with the variations of this instance.
            if (customPlatformData && variation) {
                CFTypeRef keys[] = { kCTFontVariationAttribute };
                CFTypeRef values[] = { variation.get() };
                RetainPtr attributes = adoptCF(CFDictionaryCreate(kCFAllocatorDefault, keys, values, std::size(keys), &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks));
                RetainPtr descriptor = adoptCF(CTFontDescriptorCreateCopyWithAttributes(protect(customPlatformData->fontDescriptor).get(), attributes.get()));
                ctFont = adoptCF(CTFontCreateWithFontDescriptor(descriptor.get(), fontSize, nullptr));
            }
        } else if (auto font = createInstalledFontForRemoteTargets(ctFont.get(), variation.get()))
            ctFont = WTF::move(font);

        return Font::create(FontPlatformData(WTF::move(ctFont), fontSize, false, false, FontOrientation::Horizontal, FontWidthVariant::RegularWidth, TextRenderingMode::Auto, { }, customPlatformData.get()));
    }).iterator->value;
}

static std::optional<TextDrawingModeFlags> textDrawingMode(CGTextDrawingMode mode)
{
    // CG adds the glyph outlines of the clip modes to the gstate clip itself; GStateScope applies that
    // clip to later drawing. kCGTextClip never reaches the delegate.
    switch (mode) {
    case kCGTextFill:
    case kCGTextFillClip:
        return TextDrawingModeFlags { TextDrawingMode::Fill };
    case kCGTextStroke:
    case kCGTextStrokeClip:
        return TextDrawingModeFlags { TextDrawingMode::Stroke };
    case kCGTextFillStroke:
    case kCGTextFillStrokeClip:
        return TextDrawingModeFlags { TextDrawingMode::Fill, TextDrawingMode::Stroke };
    case kCGTextInvisible:
    case kCGTextClip:
        break;
    }
    return std::nullopt;
}

static FontSmoothingMode fontSmoothingMode(CGRenderingStateRef rstate, CGGStateRef gstate)
{
    // FontCascade::drawGlyphs() disables antialiasing (not font antialiasing) for FontSmoothingMode::None.
    if (!CGGStateGetShouldAntialias(gstate) || !CGRenderingStateGetAllowsAntialiasing(rstate))
        return FontSmoothingMode::None;
#if PLATFORM(MAC)
    // Before calling the delegate, CGContextDelegateDrawGlyphs() replaces the font smoothing and font
    // antialiasing state: smoothing becomes font antialiasing with a custom (luminance based) dilation.
    if ((CGGStateGetFontAntialiasingStyle(gstate) & kCGFontAntialiasingStyleUnfilteredCustomDilation) == kCGFontAntialiasingStyleUnfilteredCustomDilation)
        return FontSmoothingMode::SubpixelAntialiased;
#else
    if (CGGStateGetShouldSmoothFonts(gstate) && CGRenderingStateGetAllowsFontSmoothing(rstate))
        return FontSmoothingMode::SubpixelAntialiased;
#endif
    return FontSmoothingMode::Antialiased;
}

// Glyph `i` is drawn at `textMatrix * positions[i]` in user space, scaled by the font size. Character
// spacing and the advances are already applied to `positions`.
CGError GraphicsContextCGDelegate::drawGlyphs(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, const CGAffineTransform* textMatrix, const CGGlyph glyphs[], const CGPoint positions[], size_t count)
{
    if (!count || !glyphs || !positions)
        return kCGErrorSuccess;

    auto mode = textDrawingMode(CGGStateGetTextDrawingMode(gstate));
    if (!mode)
        return kCGErrorSuccess;

    CGFontRef cgFont = CGGStateGetFont(gstate);
    CGFloat fontSize = CGGStateGetFontSize(gstate);
    if (!cgFont || !fontSize)
        return kCGErrorSuccess;

    AffineTransform glyphSpaceTransform = textMatrix ? AffineTransform(*textMatrix) : AffineTransform();
    float positionScale = 1;
    if (fontSize < 0) {
        // Fold a negative font size into the text matrix: tm * translate(p) * scale(-s) == (tm * scale(-1)) * translate(-p) * scale(s).
        glyphSpaceTransform.scale(-1);
        positionScale = -1;
        fontSize = -fontSize;
    }

    // WebCore draws horizontal glyphs with a y-flipped text matrix (see computeTextMatrix()), and positions
    // them in user space. Make the target's user space CG's text space flipped in y, so that its text matrix
    // composes to CG's text matrix. Synthetic oblique and vertical text are already part of CG's text matrix
    // and positions, so the Font is horizontal without synthesis.
    glyphSpaceTransform.scale(1, -1);

    auto& graphicsContextDelegate = fromDelegate(delegate);
    OptionSet<GStateScope::Paint> paint;
    if (mode->contains(TextDrawingMode::Fill))
        paint.add(GStateScope::Paint::Fill);
    if (mode->contains(TextDrawingMode::Stroke))
        paint.add(GStateScope::Paint::Stroke);
    GStateScope scope(graphicsContextDelegate, rstate, gstate, paint, glyphSpaceTransform);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    auto& target = scope.target();
    target.setTextDrawingMode(*mode);
    // Text drawing doesn't apply pattern brushes by itself (see LegacyRenderSVGResourcePattern).
    if (mode->contains(TextDrawingMode::Fill) && target.fillPattern())
        target.applyFillPattern();
    if (mode->contains(TextDrawingMode::Stroke) && target.strokePattern())
        target.applyStrokePattern();
    target.setShouldSubpixelQuantizeFonts(CGGStateGetShouldSubpixelQuantizeFonts(gstate) && CGRenderingStateGetAllowsFontSubpixelQuantization(rstate));

    auto positionsSpan = unsafeMakeSpan(positions, count);
    auto toUserSpace = [&](CGPoint position) {
        return FloatPoint(position.x * positionScale, -position.y * positionScale);
    };
    Vector<GlyphBufferAdvance, 256> advances(count, [&](size_t i) -> GlyphBufferAdvance {
        if (i + 1 == count)
            return CGSizeZero;
        return CGSize(toUserSpace(positionsSpan[i + 1]) - toUserSpace(positionsSpan[i]));
    });

    Ref font = graphicsContextDelegate.fontForDrawing(cgFont, fontSize);
    target.drawGlyphs(font, unsafeMakeSpan(glyphs, count), advances.span(), toUserSpace(positionsSpan[0]), fontSmoothingMode(rstate, gstate));
    return kCGErrorSuccess;
}

#pragma mark - Gradients and shadings

static std::optional<ColorInterpolationMethod> interpolationMethodForColorSpace(ColorSpaceName colorSpace, AlphaPremultiplication alphaPremultiplication)
{
    auto method = [&](auto colorSpace) -> std::optional<ColorInterpolationMethod> {
        return ColorInterpolationMethod { colorSpace, alphaPremultiplication };
    };

    switch (colorSpace) {
    case ColorSpaceName::SRGB:
    case ColorSpaceName::ExtendedSRGB:
        return method(ColorInterpolationMethod::SRGB { });
    case ColorSpaceName::LinearSRGB:
    case ColorSpaceName::ExtendedLinearSRGB:
        return method(ColorInterpolationMethod::SRGBLinear { });
    case ColorSpaceName::DisplayP3:
    case ColorSpaceName::ExtendedDisplayP3:
        return method(ColorInterpolationMethod::DisplayP3 { });
    case ColorSpaceName::LinearDisplayP3:
    case ColorSpaceName::ExtendedLinearDisplayP3:
        return method(ColorInterpolationMethod::DisplayP3Linear { });
    case ColorSpaceName::A98RGB:
    case ColorSpaceName::ExtendedA98RGB:
        return method(ColorInterpolationMethod::A98RGB { });
    case ColorSpaceName::ProPhotoRGB:
    case ColorSpaceName::ExtendedProPhotoRGB:
        return method(ColorInterpolationMethod::ProPhotoRGB { });
    case ColorSpaceName::Rec2020:
    case ColorSpaceName::ExtendedRec2020:
        return method(ColorInterpolationMethod::Rec2020 { });
    case ColorSpaceName::XYZ_D50:
        return method(ColorInterpolationMethod::XYZD50 { });
    case ColorSpaceName::XYZ_D65:
        return method(ColorInterpolationMethod::XYZD65 { });
    case ColorSpaceName::HSL:
    case ColorSpaceName::HWB:
    case ColorSpaceName::LCH:
    case ColorSpaceName::Lab:
    case ColorSpaceName::OKLCH:
    case ColorSpaceName::OKLab:
        break;
    }
    return std::nullopt;
}

static bool isLinearColorSpace(ColorSpaceName colorSpace)
{
    switch (colorSpace) {
    case ColorSpaceName::LinearSRGB:
    case ColorSpaceName::ExtendedLinearSRGB:
    case ColorSpaceName::LinearDisplayP3:
    case ColorSpaceName::ExtendedLinearDisplayP3:
        return true;
    default:
        return false;
    }
}

// Returns the WebCore interpolation method that interpolates like CG does in `colorSpace`, or
// nullopt if there is none (gray, CMYK, ICC based, ... color spaces).
//
// GradientRendererCG interpolates sRGB gradients in the destination color space (if it isn't linear)
// and samples gradients with other interpolation methods. So when `colorSpace` is the target's color
// space (the common case, since GraphicsContextCG creates its CGGradients in the destination color
// space), the SRGB method reproduces the CGGradient exactly on the target.
static std::optional<ColorInterpolationMethod> interpolationMethodForCGColorSpace(CGColorSpaceRef colorSpace, const GraphicsContext& target, AlphaPremultiplication alphaPremultiplication)
{
    auto colorSpaceName = colorSpaceForCGColorSpace(colorSpace);
    if (CFEqual(colorSpace, target.colorSpace().platformColorSpace()) && !(colorSpaceName && isLinearColorSpace(*colorSpaceName)))
        return ColorInterpolationMethod { ColorInterpolationMethod::SRGB { }, alphaPremultiplication };

    if (!colorSpaceName)
        return std::nullopt;
    return interpolationMethodForColorSpace(*colorSpaceName, alphaPremultiplication);
}

static Color colorFromComponents(CGColorSpaceRef colorSpace, std::span<const CGFloat> components)
{
    RetainPtr color = adoptCF(CGColorCreate(colorSpace, components.data()));
    return Color::createAndPreserveColorSpace(color.get());
}

struct GradientStopsAndInterpolationMethod {
    GradientColorStops stops;
    ColorInterpolationMethod interpolationMethod;
};

// Samples a 1-in, N-out CG shading function (N = color components, optionally + alpha) over
// `domain` into gradient stops. Stops that linear interpolation reproduces within a small tolerance
// are dropped. This is lossy: discontinuities are smeared over one sampling interval.
static std::optional<GradientStopsAndInterpolationMethod> sampleShadingFunction(CGFunctionRef function, CGColorSpaceRef colorSpace, std::span<const CGFloat, 2> domain, const GraphicsContext& target)
{
    if (!function || !colorSpace)
        return std::nullopt;

    size_t colorComponentCount = CGColorSpaceGetNumberOfComponents(colorSpace);
    size_t rangeDimension = CGFunctionGetRangeDimension(function);
    if (CGFunctionGetDomainDimension(function) != 1 || (rangeDimension != colorComponentCount && rangeDimension != colorComponentCount + 1))
        return std::nullopt;

    static constexpr size_t sampleCount = 257;
    static constexpr CGFloat tolerance = 1. / 512;
    size_t stride = colorComponentCount + 1;
    Vector<CGFloat> samples(sampleCount * stride);
    Vector<CGFloat> output(std::max<size_t>(rangeDimension, 1));
    for (size_t i = 0; i < sampleCount; ++i) {
        CGFloat t = static_cast<CGFloat>(i) / (sampleCount - 1);
        CGFloat input = domain[0] + t * (domain[1] - domain[0]);
        CGFunctionEvaluate(function, &input, output.mutableSpan().data());
        auto sample = samples.mutableSpan().subspan(i * stride, stride);
        for (size_t component = 0; component < colorComponentCount; ++component)
            sample[component] = output[component];
        sample[colorComponentCount] = rangeDimension > colorComponentCount ? output[colorComponentCount] : 1;
    }

    auto sampleAt = [&](size_t index) {
        return samples.span().subspan(index * stride, stride);
    };

    // Whether linearly interpolating between samples `start` and `end` reproduces every sample in between.
    auto isLinearBetween = [&](size_t start, size_t end) {
        auto startSample = sampleAt(start);
        auto endSample = sampleAt(end);
        for (size_t i = start + 1; i < end; ++i) {
            CGFloat fraction = static_cast<CGFloat>(i - start) / (end - start);
            auto sample = sampleAt(i);
            for (size_t component = 0; component < stride; ++component) {
                if (std::abs(startSample[component] + fraction * (endSample[component] - startSample[component]) - sample[component]) > tolerance)
                    return false;
            }
        }
        return true;
    };

    GradientColorStops::StopVector stops;
    auto appendStop = [&](size_t index) {
        stops.append({ static_cast<float>(index) / (sampleCount - 1), colorFromComponents(colorSpace, sampleAt(index)) });
    };

    size_t lastStop = 0;
    appendStop(0);
    for (size_t i = 2; i < sampleCount; ++i) {
        if (!isLinearBetween(lastStop, i)) {
            lastStop = i - 1;
            appendStop(lastStop);
        }
    }
    appendStop(sampleCount - 1);

    // The samples were taken in `colorSpace`; interpolate there if WebCore can, otherwise the stops are
    // dense enough that interpolating in the target color space is close.
    auto interpolationMethod = interpolationMethodForCGColorSpace(colorSpace, target, AlphaPremultiplication::Unpremultiplied).value_or(ColorInterpolationMethod { ColorInterpolationMethod::SRGB { }, AlphaPremultiplication::Unpremultiplied });
    return GradientStopsAndInterpolationMethod { GradientColorStops::Sorted { WTF::move(stops) }, interpolationMethod };
}

static std::optional<GradientStopsAndInterpolationMethod> gradientStops(CGGradientRef gradient, const GraphicsContext& target)
{
    RetainPtr colorSpace = CGGradientGetColorSpace(gradient);
    auto alphaPremultiplication = CGGradientUsesPremultipliedInterpolation(gradient) ? AlphaPremultiplication::Premultiplied : AlphaPremultiplication::Unpremultiplied;

    auto interpolationMethod = interpolationMethodForCGColorSpace(colorSpace.get(), target, alphaPremultiplication);
    if (!interpolationMethod) {
        // WebCore can't interpolate in this color space: sample the gradient's function instead.
        static constexpr std::array<CGFloat, 2> domain { 0, 1 };
        return sampleShadingFunction(CGGradientGetFunction(gradient), colorSpace.get(), domain, target);
    }

    struct ApplierContext {
        CGColorSpaceRef colorSpace;
        size_t componentCount;
        GradientColorStops::StopVector stops;
    } context { colorSpace.get(), CGColorSpaceGetNumberOfComponents(colorSpace.get()) + 1, { } };

    CGGradientApply(gradient, &context, [](void* info, CGFloat location, const CGFloat* components) {
        auto& context = *static_cast<ApplierContext*>(info);
        context.stops.append({ static_cast<float>(location), colorFromComponents(context.colorSpace, unsafeMakeSpan(components, context.componentCount)) });
    });

    if (context.stops.isEmpty())
        return std::nullopt;

    // CG sorts the stops, and always has stops at 0 and 1.
    return GradientStopsAndInterpolationMethod { GradientColorStops::Sorted { WTF::move(context.stops) }, *interpolationMethod };
}

// WebCore gradients always extend (pad) their end colors. CG doesn't draw before the start (t < 0)
// or after the end (t > 1) of an axial or radial gradient unless asked to: represent that with
// transparent stops at 0 or 1, which the pad then extends.
static void addTransparentStopsForMissingExtends(GradientColorStops& stops, bool extendStart, bool extendEnd)
{
    if ((extendStart && extendEnd) || stops.isEmpty())
        return;

    GradientColorStops::StopVector extendedStops;
    extendedStops.reserveInitialCapacity(stops.size() + 2);
    if (!extendStart)
        extendedStops.append({ 0, stops.stops().first().color.colorWithAlpha(0.f) });
    extendedStops.appendVector(stops.stops());
    if (!extendEnd)
        extendedStops.append({ 1, stops.stops().last().color.colorWithAlpha(0.f) });
    stops = GradientColorStops::Sorted { WTF::move(extendedStops) };
}

// GraphicsContextCG (Gradient::paint()) draws conic gradients with the CTM rotated by -pi/2 around
// the center (CSS conic gradients start at 12 o'clock). Undo that, so the target issues the same CG call.
static float conicGradientAngle(CGFloat cgAngle)
{
    return cgAngle + piOverTwoDouble;
}

// Fills the clip with `gradient` (in user space).
static void fillClipWithGradient(GStateScope& scope, Ref<Gradient>&& gradient)
{
    auto& target = scope.target();
    if (!scope.ctm().isInvertible())
        return;
    FloatRect rect = target.clipBounds();
    if (rect.isEmpty())
        return;
    // CG gradients fill the whole clip, which GStateScope applied. Don't clip to `rect` too: it is
    // only an estimate on recording targets (Recorder::clipBounds() is enclosing in user space,
    // which under a scale can cut into the antialiased edge of the clip).
    target.fillRect(rect, gradient.get(), { }, RequiresClipToRect::No);
}

static bool isEmptyAxialGradient(CGPoint start, CGPoint end, bool extendStart, bool extendEnd)
{
    // Without both extends, CG draws nothing for a zero length axis.
    return CGPointEqualToPoint(start, end) && !(extendStart && extendEnd);
}

static bool isEmptyRadialGradient(CGPoint startCenter, CGFloat startRadius, CGPoint endCenter, CGFloat endRadius, bool extendStart, bool extendEnd)
{
    // Negative radii are passed through: WebCore relies on how CG draws them (normalized CSS stops < 0).
    return CGPointEqualToPoint(startCenter, endCenter) && startRadius == endRadius && !(extendStart && extendEnd);
}

static Ref<Gradient> createLinearGradient(GradientStopsAndInterpolationMethod&& stops, CGPoint start, CGPoint end, bool extendStart, bool extendEnd)
{
    addTransparentStopsForMissingExtends(stops.stops, extendStart, extendEnd);
    return Gradient::create(Gradient::LinearData { start, end }, stops.interpolationMethod, GradientSpreadMethod::Pad, WTF::move(stops.stops));
}

static Ref<Gradient> createRadialGradient(GradientStopsAndInterpolationMethod&& stops, CGPoint startCenter, CGFloat startRadius, CGPoint endCenter, CGFloat endRadius, bool extendStart, bool extendEnd)
{
    // FIXME: For cone shaped radial gradients, a point can lie on two circles. CG paints the larger t
    // within the extended range, so with transparent stops a point on circles with t in [0, 1] and
    // t > 1 is left unpainted while CG (without kCGGradientDrawsAfterEndLocation) paints it.
    addTransparentStopsForMissingExtends(stops.stops, extendStart, extendEnd);
    return Gradient::create(Gradient::RadialData { startCenter, endCenter, static_cast<float>(startRadius), static_cast<float>(endRadius), 1 }, stops.interpolationMethod, GradientSpreadMethod::Pad, WTF::move(stops.stops));
}

static Ref<Gradient> createConicGradient(GradientStopsAndInterpolationMethod&& stops, CGPoint center, CGFloat angle)
{
    return Gradient::create(Gradient::ConicData { center, conicGradientAngle(angle) }, stops.interpolationMethod, GradientSpreadMethod::Pad, WTF::move(stops.stops));
}

// Draws `shading` into a device space bitmap and draws that onto the target (for shadings that
// can't be represented as a WebCore::Gradient).
static void drawShadingByRasterizing(GStateScope& scope, CGGStateRef gstate, CGShadingRef shading)
{
    auto& target = scope.target();
    if (!scope.ctm().isInvertible())
        return;

    FloatRect userSpaceBounds = target.clipBounds();
    CGRect shadingBounds = CGShadingGetBounds(shading);
    if (!CGRectIsInfinite(shadingBounds) && !CGRectIsNull(shadingBounds))
        userSpaceBounds.intersect(shadingBounds);
    auto deviceRect = enclosingIntRect(scope.ctm().mapRect(userSpaceBounds));
    if (deviceRect.isEmpty())
        return;

    static constexpr int maximumDimension = 4096;
    float scale = std::min(1.f, static_cast<float>(maximumDimension) / std::max(deviceRect.width(), deviceRect.height()));
    IntSize pixelSize = expandedIntSize(FloatSize(deviceRect.size()).scaled(scale));

    RetainPtr colorSpace = target.colorSpace().platformColorSpace();
    if (CGColorSpaceGetModel(colorSpace.get()) != kCGColorSpaceModelRGB || !CGColorSpaceSupportsOutput(colorSpace.get()))
        colorSpace = cachedCGColorSpaceSingleton<ColorSpaceName::SRGB>();

    RetainPtr context = adoptCF(CGBitmapContextCreate(nullptr, pixelSize.width(), pixelSize.height(), 8, 0, colorSpace.get(), kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host));
    if (!context)
        return;

    // Row 0 of the bitmap is the top (minimum y) of `deviceRect`.
    CGContextTranslateCTM(context.get(), 0, pixelSize.height());
    CGContextScaleCTM(context.get(), scale, -scale);
    CGContextTranslateCTM(context.get(), -deviceRect.x(), -deviceRect.y());
    CGContextConcatCTM(context.get(), scope.ctm());
    CGContextSetShouldAntialias(context.get(), CGGStateGetShouldAntialias(gstate));
    CGContextDrawShading(context.get(), shading);

    RetainPtr image = adoptCF(CGBitmapContextCreateImage(context.get()));
    if (!image)
        return;
    RefPtr nativeImage = NativeImage::create(RetainPtr { image });
    if (!nativeImage)
        return;

    // FIXME: Shadow offsets were converted for the gstate CTM; they are off for non-identity CTMs here.
    target.setCTM({ });
    target.drawNativeImage(*nativeImage, deviceRect, { { }, pixelSize }, imagePaintingOptions(target, gstate));
}

CGError GraphicsContextCGDelegate::drawShading(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGShadingRef shading)
{
    auto& graphicsContextDelegate = fromDelegate(delegate);
    GStateScope scope(graphicsContextDelegate, rstate, gstate);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    auto& target = scope.target();
    CGColorSpaceRef colorSpace = CGShadingGetColorSpace(shading);
    auto* descriptor = CGShadingGetDescriptor(shading);

    // The shading's bounding box (if any) clips it.
    CGRect bounds = CGShadingGetBounds(shading);
    auto clipToBounds = [&] {
        if (!CGRectIsInfinite(bounds) && !CGRectIsNull(bounds))
            target.clip(bounds);
    };

    switch (CGShadingGetType(shading)) {
    case kCGShadingAxial: {
        const auto& axial = descriptor->axial;
        if (isEmptyAxialGradient(axial.start, axial.end, axial.extendStart, axial.extendEnd))
            return kCGErrorSuccess;
        auto stops = sampleShadingFunction(axial.function, colorSpace, std::span { axial.domain }, target);
        if (!stops)
            break;
        clipToBounds();
        fillClipWithGradient(scope, createLinearGradient(WTF::move(*stops), axial.start, axial.end, axial.extendStart, axial.extendEnd));
        return kCGErrorSuccess;
    }
    case kCGShadingRadial: {
        const auto& radial = descriptor->radial;
        if (isEmptyRadialGradient(radial.start, radial.startRadius, radial.end, radial.endRadius, radial.extendStart, radial.extendEnd))
            return kCGErrorSuccess;
        auto stops = sampleShadingFunction(radial.function, colorSpace, std::span { radial.domain }, target);
        if (!stops)
            break;
        clipToBounds();
        fillClipWithGradient(scope, createRadialGradient(WTF::move(*stops), radial.start, radial.startRadius, radial.end, radial.endRadius, radial.extendStart, radial.extendEnd));
        return kCGErrorSuccess;
    }
    case kCGShadingConic: {
        const auto& conic = descriptor->conic;
        auto stops = sampleShadingFunction(conic.function, colorSpace, std::span { conic.domain }, target);
        if (!stops)
            break;
        clipToBounds();
        fillClipWithGradient(scope, createConicGradient(WTF::move(*stops), conic.center, conic.angle));
        return kCGErrorSuccess;
    }
    case kCGShadingProcedural:
    case kCGShadingCustom:
        break;
    }

    // Function based (custom), mesh and other procedural shadings (PDF shading types 1, 4-7) have no
    // WebCore equivalent.
    graphicsContextDelegate.logUnimplementedCallback("drawShading (rasterized)");
    drawShadingByRasterizing(scope, gstate, shading);
    return kCGErrorSuccess;
}

// The gradient callbacks' geometry is in user space.
CGError GraphicsContextCGDelegate::drawLinearGradient(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGGradientRef gradient, CGPoint start, CGPoint end, CGGradientDrawingOptions options)
{
    bool extendStart = options & kCGGradientDrawsBeforeStartLocation;
    bool extendEnd = options & kCGGradientDrawsAfterEndLocation;
    if (isEmptyAxialGradient(start, end, extendStart, extendEnd))
        return kCGErrorSuccess;

    auto& graphicsContextDelegate = fromDelegate(delegate);
    auto stops = gradientStops(gradient, graphicsContextDelegate.target());
    if (!stops)
        return kCGErrorNotImplemented; // CG falls back to drawShading().

    GStateScope scope(graphicsContextDelegate, rstate, gstate);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    fillClipWithGradient(scope, createLinearGradient(WTF::move(*stops), start, end, extendStart, extendEnd));
    return kCGErrorSuccess;
}

CGError GraphicsContextCGDelegate::drawRadialGradient(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGGradientRef gradient, CGPoint startCenter, CGFloat startRadius, CGPoint endCenter, CGFloat endRadius, CGGradientDrawingOptions options)
{
    bool extendStart = options & kCGGradientDrawsBeforeStartLocation;
    bool extendEnd = options & kCGGradientDrawsAfterEndLocation;
    if (isEmptyRadialGradient(startCenter, startRadius, endCenter, endRadius, extendStart, extendEnd))
        return kCGErrorSuccess;

    auto& graphicsContextDelegate = fromDelegate(delegate);
    auto stops = gradientStops(gradient, graphicsContextDelegate.target());
    if (!stops)
        return kCGErrorNotImplemented; // CG falls back to drawShading().

    GStateScope scope(graphicsContextDelegate, rstate, gstate);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    fillClipWithGradient(scope, createRadialGradient(WTF::move(*stops), startCenter, startRadius, endCenter, endRadius, extendStart, extendEnd));
    return kCGErrorSuccess;
}

CGError GraphicsContextCGDelegate::drawConicGradient(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGGradientRef gradient, CGPoint center, CGFloat angle)
{
    auto& graphicsContextDelegate = fromDelegate(delegate);
    auto stops = gradientStops(gradient, graphicsContextDelegate.target());
    if (!stops)
        return kCGErrorNotImplemented; // CG falls back to drawShading().

    GStateScope scope(graphicsContextDelegate, rstate, gstate);
    if (!scope.shouldDraw())
        return kCGErrorSuccess;

    fillClipWithGradient(scope, createConicGradient(WTF::move(*stops), center, angle));
    return kCGErrorSuccess;
}

#pragma mark - Display lists

// CG replays display lists through this callback: CGLayers (see installCallbacks()), and display
// lists drawn with CGContextDrawDisplayList() (e.g. PDF patterns) or nested in other display lists.
// CGDisplayListDelegateDrawDisplayList() leaves grouping to delegates that implement it, so this
// mirrors what CG does when executing the display list itself: the display list's entries are
// executed through our own callbacks, inside a transparency layer if the gstate requires the
// display list to be composited as a single object. Nested display lists recurse through here.
CGError GraphicsContextCGDelegate::drawDisplayList(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGDisplayListRef displayList)
{
    bool needsGroup = CGGStateGetAlpha(gstate) != 1
        || CGGStateGetStyle(gstate)
        || CGGStateGetSoftMask(gstate)
        || CGGStateGetCompositeOperation(gstate) != kCGCompositeSover;
    if (!needsGroup) {
        CGDisplayListDrawInContextDelegate(displayList, delegate, rstate, gstate, nullptr);
        return kCGErrorSuccess;
    }

    // Group information in the display list's auxiliary info (knockout, group color space) is
    // ignored, like in beginLayer(); CG adds kCGContextGroup to every CGLayer display list, which
    // only matters for the cases above.
    RetainPtr context = adoptCF(CGContextCreateWithDelegate(delegate, kCGContextTypeUnknown, rstate, gstate));
    if (!context)
        return kCGErrorFailure;
    CGContextBeginTransparencyLayerWithRect(context.get(), CGDisplayListGetBoundingBox(displayList), nullptr);
    CGDisplayListDrawInContext(displayList, context.get());
    CGContextEndTransparencyLayer(context.get());
    return kCGErrorSuccess;
}

#pragma mark - Operations

CGError GraphicsContextCGDelegate::operation(CGContextDelegateRef delegate, CGRenderingStateRef, CGGStateRef, CFStringRef action, CFDictionaryRef)
{
    if (!action)
        return kCGErrorNotImplemented;

    // CGContextClear() / CGContextErase(): clear the whole destination to transparent / opaque
    // white, ignoring the gstate (clip, alpha, ...), like bitmap contexts. The target's own clip at
    // creation time still applies, so this only affects the area the CGContext can draw into.
    // Inside a transparency layer this clears the layer, as CG does.
    bool clear = CFEqual(action, kCGContextClear);
    if (clear || CFEqual(action, kCGContextErase)) {
        auto& target = fromDelegate(delegate).target();
        auto bounds = getBounds(delegate, nullptr, nullptr);
        GraphicsContextStateSaver stateSaver(target);
        target.setCTM({ });
        if (clear)
            target.clearRect(bounds);
        else
            target.fillRect(bounds, Color::white, CompositeOperator::Copy);
        return kCGErrorSuccess;
    }

    // Flush / synchronize / wait: the target has no pending CG work to flush.
    if (CFEqual(action, kCGContextFlush) || CFEqual(action, kCGContextSynchronize) || CFEqual(action, kCGContextSynchronizeAttributes) || CFEqual(action, kCGContextWait))
        return kCGErrorSuccess;

    // kCGContextDisplayList (page display lists from page-based display list delegates), kCGContextLog
    // and custom actions (display list "parameter" actions, PDF tags): not applicable. For
    // kCGContextDisplayList, kCGErrorNotImplemented makes CG execute the display list through
    // our drawing callbacks.
    return kCGErrorNotImplemented;
}

#pragma mark - Transparency layers

// CGContextBeginTransparencyLayer() calls this with the gstate the layer will be composited with
// (alpha, composite operation, style, soft mask, clip), then resets alpha, composite operation,
// style and soft mask in the gstate used inside the layer (CGGStateCreateCopyForLayer()), so the
// drawing callbacks inside the layer don't re-apply them. CGContextEndTransparencyLayer() calls
// endLayer() with the restored (same) gstate.
CGContextDelegateRef GraphicsContextCGDelegate::beginLayer(CGContextDelegateRef delegate, CGRenderingStateRef rstate, CGGStateRef gstate, CGRect rect, CFDictionaryRef, CGContextDelegateRef)
{
    auto& graphicsContextDelegate = fromDelegate(delegate);
    auto& target = graphicsContextDelegate.target();

    // The scope applies the composite state to the target and keeps it until endLayer(). The
    // target's transparency layer is composited with it (GraphicsContextCG passes the alpha,
    // composite operation, shadow and clip of the current state to CG).
    auto scope = makeUnique<GStateScope>(graphicsContextDelegate, rstate, gstate);

    // CG resets the clip in the layer's gstate, so the clip applied by the scope is the only thing
    // limiting the drawing in the layer. If the scope bailed out before applying it (empty clip,
    // fully transparent soft mask), nothing in the layer must be visible.
    if (!scope->shouldDraw())
        target.clip(FloatRect { });

    auto mode = compositeMode(CGGStateGetCompositeOperation(gstate));
    if (mode.operation == CompositeOperator::SourceOver && mode.blendMode == BlendMode::Normal)
        target.beginTransparencyLayer(scope->alpha());
    else
        target.beginTransparencyLayer(mode.operation, mode.blendMode);

    // The layer content is limited to `rect`, in user space (CGContextBeginTransparencyLayer() has
    // already taken it from the info bounding box, if any). Other info keys (group color space,
    // knockout) are ignored; a background color is filled by CG inside the layer.
    if (scope->shouldDraw() && !CGRectIsInfinite(rect))
        target.clip(rect);

    graphicsContextDelegate.m_transparencyLayers.append(WTF::move(scope));
    return delegate;
}

CGContextDelegateRef GraphicsContextCGDelegate::endLayer(CGContextDelegateRef delegate, CGRenderingStateRef, CGGStateRef)
{
    auto& graphicsContextDelegate = fromDelegate(delegate);
    if (graphicsContextDelegate.m_transparencyLayers.isEmpty())
        return nullptr; // Unbalanced; CG reports the error.

    graphicsContextDelegate.target().endTransparencyLayer();
    graphicsContextDelegate.m_transparencyLayers.removeLast();
    return delegate;
}

} // namespace WebCore

#endif // USE(CG)
