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

#include "FloatRect.h"
#include "FloatRoundedRect.h"
#include "PathElement.h"
#include "RotationDirection.h"
#include <wtf/Hasher.h>

namespace WTF {
class TextStream;
}

namespace WebCore {

struct PathMoveTo {
    FloatPoint point;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathMoveTo&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathMoveTo&);

inline void add(Hasher& hasher, const PathMoveTo& p)
{
    add(hasher, p.point);
}

struct PathLineTo {
    FloatPoint point;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathLineTo&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathLineTo&);

inline void add(Hasher& hasher, const PathLineTo& p)
{
    add(hasher, p.point);
}

struct PathQuadCurveTo {
    FloatPoint controlPoint;
    FloatPoint endPoint;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathQuadCurveTo&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathQuadCurveTo&);

inline void add(Hasher& hasher, const PathQuadCurveTo& p)
{
    add(hasher, p.controlPoint, p.endPoint);
}

struct PathBezierCurveTo {
    FloatPoint controlPoint1;
    FloatPoint controlPoint2;
    FloatPoint endPoint;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathBezierCurveTo&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathBezierCurveTo&);

inline void add(Hasher& hasher, const PathBezierCurveTo& p)
{
    add(hasher, p.controlPoint1, p.controlPoint2, p.endPoint);
}

struct PathArcTo {
    FloatPoint controlPoint1;
    FloatPoint controlPoint2;
    float radius;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathArcTo&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathArcTo&);

inline void add(Hasher& hasher, const PathArcTo& p)
{
    add(hasher, p.controlPoint1, p.controlPoint2, p.radius);
}

struct PathArc {
    FloatPoint center;
    float radius;
    float startAngle;
    float endAngle;
    RotationDirection direction;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathArc&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathArc&);

inline void add(Hasher& hasher, const PathArc& p)
{
    add(hasher, p.center, p.radius, p.startAngle, p.endAngle, p.direction);
}

struct PathClosedArc {
    PathArc arc;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathClosedArc&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathClosedArc&);

inline void add(Hasher& hasher, const PathClosedArc& p)
{
    add(hasher, p.arc);
}

struct PathEllipse {
    FloatPoint center;
    float radiusX;
    float radiusY;
    float rotation;
    float startAngle;
    float endAngle;
    RotationDirection direction;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathEllipse&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathEllipse&);

inline void add(Hasher& hasher, const PathEllipse& p)
{
    add(hasher, p.center, p.radiusX, p.radiusY, p.rotation, p.startAngle, p.endAngle, p.direction);
}

struct PathEllipseInRect {
    FloatRect rect;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathEllipseInRect&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathEllipseInRect&);

inline void add(Hasher& hasher, const PathEllipseInRect& p)
{
    add(hasher, p.rect);
}

struct PathRect {
    FloatRect rect;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathRect&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathRect&);


inline void add(Hasher& hasher, const PathRect& p)
{
    add(hasher, p.rect);
}

struct PathRoundedRect {
    enum class Strategy : uint8_t {
        PreferNative,
        PreferBezier
    };

    FloatRoundedRect roundedRect;
    Strategy strategy;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathRoundedRect&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathRoundedRect&);

inline void add(Hasher& hasher, const PathRoundedRect& p)
{
    add(hasher, p.roundedRect, p.strategy);
}

struct PathContinuousRoundedRect {
    FloatRect rect;
    float cornerWidth;
    float cornerHeight;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathContinuousRoundedRect&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathContinuousRoundedRect&);


inline void add(Hasher& hasher, const PathContinuousRoundedRect& p)
{
    add(hasher, p.rect, p.cornerWidth, p.cornerHeight);
}

struct PathDataLine {
    PathDataLine(FloatPoint start, FloatPoint end)
        : m_values { { start.x(), start.y(), end.x(), end.y() } }
    {
    }
    PathDataLine(std::span<const float, 4> values)
        : m_values { values[0], values[1], values[2], values[3] }
    {
    }

    FloatPoint start() const { return { m_values[0], m_values[1] }; }
    void setStart(FloatPoint p) { m_values[0] = p.x(); m_values[1] = p.y(); }
    FloatPoint end() const { return { m_values[2], m_values[3] }; }
    void setEnd(FloatPoint p) { m_values[2] = p.x(); m_values[3] = p.y(); }
    std::span<const float, 4> span() const LIFETIME_BOUND { return m_values; }
    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathDataLine&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
private:
    std::array<float, 4> m_values { };
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathDataLine&);

inline void add(Hasher& hasher, const PathDataLine& p)
{
    add(hasher, p.span()[0], p.span()[1], p.span()[2], p.span()[3]);
}

struct PathDataQuadCurve {
    FloatPoint start;
    FloatPoint controlPoint;
    FloatPoint endPoint;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathDataQuadCurve&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathDataQuadCurve&);

inline void add(Hasher& hasher, const PathDataQuadCurve& p)
{
    add(hasher, p.start, p.controlPoint, p.endPoint);
}

struct PathDataBezierCurve {
    FloatPoint start;
    FloatPoint controlPoint1;
    FloatPoint controlPoint2;
    FloatPoint endPoint;

    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathDataBezierCurve&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathDataBezierCurve&);

inline void add(Hasher& hasher, const PathDataBezierCurve& p)
{
    add(hasher, p.start, p.controlPoint1, p.controlPoint2, p.endPoint);
}

struct PathDataArc {
    FloatPoint start;
    FloatPoint controlPoint1;
    FloatPoint controlPoint2;
    float radius;

    static constexpr bool canApplyElements = false;
    static constexpr bool canTransform = false;

    bool operator==(const PathDataArc&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathDataArc&);

inline void add(Hasher& hasher, const PathDataArc& p)
{
    add(hasher, p.start, p.controlPoint1, p.controlPoint2, p.radius);
}

struct PathCloseSubpath {
    static constexpr bool canApplyElements = true;
    static constexpr bool canTransform = true;

    bool operator==(const PathCloseSubpath&) const = default;

    FloatPoint calculateEndPoint(const FloatPoint& currentPoint, FloatPoint& lastMoveToPoint) const;
    std::optional<FloatPoint> tryGetEndPointWithoutContext() const;

    void extendFastBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;
    void extendBoundingRect(const FloatPoint& currentPoint, const FloatPoint& lastMoveToPoint, FloatRect& boundingRect) const;

    void applyElements(const PathElementApplier&) const;

    void transform(const AffineTransform&);
};

WEBCORE_EXPORT WTF::TextStream& operator<<(WTF::TextStream&, const PathCloseSubpath&);

inline void add(Hasher&, const PathCloseSubpath&) { }

} // namespace WebCore
