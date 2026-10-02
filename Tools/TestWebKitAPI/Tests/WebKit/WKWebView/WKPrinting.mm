/*
 * Copyright (C) 2022 Apple Inc. All rights reserved.
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
#import "WKPrinting.h"

#import "Helpers/PlatformUtilities.h"
#import "Helpers/Test.h"
#import "Helpers/cocoa/CGImagePixelReader.h"
#import "Helpers/cocoa/PDFTestHelpers.h"
#import "Helpers/cocoa/SiteIsolationTestUtilities.h"
#import "Helpers/cocoa/TestNavigationDelegate.h"
#import "Helpers/cocoa/TestWKWebView.h"
#import "Helpers/Utilities.h"
#import <PDFKit/PDFKit.h>
#import <WebCore/Color.h>
#import <WebKit/WKData.h>
#import <WebKit/WKPagePrivate.h>
#import <WebKit/WKUIDelegate.h>
#import <WebKit/WKWebViewPrivate.h>
#import <WebKit/WKWebViewPrivateForTesting.h>
#import <WebKit/WKWebpagePreferences.h>
#import <WebKit/_WKFrameHandle.h>
#import <wtf/RetainPtr.h>
#import <wtf/cocoa/TypeCastsCocoa.h>
#import <wtf/darwin/DispatchExtras.h>

typedef void (^CallCompletionBlock)();

@interface PrintWithSimulatedPageComputationUIDelegate : NSObject <WKUIDelegate>

- (void)waitForPagination;

@end

@implementation PrintWithSimulatedPageComputationUIDelegate {
    bool _isDone;
}

- (void)callBlockAsync:(CallCompletionBlock)callCompletionBlock
{
    dispatch_async(mainDispatchQueueSingleton(), ^{
        callCompletionBlock();
    });
}

- (void)_webView:(WKWebView *)webView printFrame:(_WKFrameHandle *)frame pdfFirstPageSize:(CGSize)size completionHandler:(void (^)(void))completionHandler
{
    _isDone = false;
    CallCompletionBlock callCompletionBlock = ^{
        [webView _computePagesForPrinting:frame completionHandler:^{
            _isDone = true;
            completionHandler();
        }];
    };

    // Dispatch the completion handler asynchronously to ensure we don't block IPC in the web process in the unbounded sync IPC case.
    [self callBlockAsync:callCompletionBlock];
}

- (void)waitForPagination
{
    TestWebKitAPI::Util::run(&_isDone);
}

@end

TEST(Printing, PrintWithDelayedCompletion)
{
    RetainPtr configuration = adoptNS([WKWebViewConfiguration new]);
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600) configuration:configuration.get()]);
    RetainPtr delegate = adoptNS([PrintWithSimulatedPageComputationUIDelegate new]);
    [webView setUIDelegate:delegate.get()];

    NSURLRequest *request = [NSURLRequest requestWithURL:[NSBundle.test_resourcesBundle URLForResource:@"simple" withExtension:@"html"]];
    [webView loadRequest:request];
    [webView _test_waitForDidFinishNavigation];

    [webView evaluateJavaScript:@"window.print()" completionHandler:nil];
    [delegate waitForPagination];
}

#if PLATFORM(MAC)
@interface WKPrintPageBordersWebView : TestWKWebView
@end

@implementation WKPrintPageBordersWebView {
    bool _didDrawPageBorder;
}

- (void)drawPageBorderWithSize:(NSSize)borderSize
{
    _didDrawPageBorder = true;
}

- (void)_waitUntilPageBorderDrawn
{
    TestWebKitAPI::Util::run(&_didDrawPageBorder);
}

@end

@interface PrintShowingPrintPanelUIDelegate : NSObject <WKUIDelegate>

@end

@implementation PrintShowingPrintPanelUIDelegate

- (void)_webView:(WKWebView *)webView printFrame:(_WKFrameHandle *)frame pdfFirstPageSize:(CGSize)size completionHandler:(void (^)(void))completionHandler
{
    RetainPtr printInfo = adoptNS([[NSPrintInfo alloc] init]);

    NSPrintOperation *operation = [webView _printOperationWithPrintInfo:printInfo.get() forFrame:frame];

    operation.showsPrintPanel = YES;
    NSPrintPanel *printPanel = operation.printPanel;
    printPanel.options = printPanel.options | NSPrintPanelShowsPaperSize | NSPrintPanelShowsOrientation | NSPrintPanelShowsScaling | NSPrintPanelShowsPreview;

    [operation runOperationModalForWindow:webView.window delegate:nil didRunSelector:nil contextInfo:nil];

    if (completionHandler)
        completionHandler();
}

@end

TEST(Printing, PrintPageBorders)
{
    RetainPtr configuration = adoptNS([WKWebViewConfiguration new]);
    RetainPtr webView = adoptNS([[WKPrintPageBordersWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600) configuration:configuration.get()]);
    RetainPtr delegate = adoptNS([PrintShowingPrintPanelUIDelegate new]);
    [webView setUIDelegate:delegate.get()];

    NSURLRequest *request = [NSURLRequest requestWithURL:[NSBundle.test_resourcesBundle URLForResource:@"simple" withExtension:@"html"]];
    [webView loadRequest:request];
    [webView _test_waitForDidFinishNavigation];

    [webView evaluateJavaScript:@"window.print()" completionHandler:nil];
    [webView _waitUntilPageBorderDrawn];
}

@implementation TestPDFPrintDelegate {
    NSUInteger _printFrameCallCount;
}

- (void)_webView:(WKWebView *)webView printFrame:(_WKFrameHandle *)frame pdfFirstPageSize:(CGSize)size completionHandler:(void (^)(void))completionHandler
{
    completionHandler();
    ++_printFrameCallCount;
}

- (void)waitForPrintFrameCall
{
    while (!_printFrameCallCount)
        TestWebKitAPI::Util::spinRunLoop();
}

- (NSUInteger)printFrameCallCount
{
    return _printFrameCallCount;
}

@end

using namespace TestWebKitAPI;

NSURLRequest *PrintWithJSExecutionOptionTests::namedPDFRequest(NSString *resourceName)
{
    return [NSURLRequest requestWithURL:[NSBundle.test_resourcesBundle URLForResource:resourceName withExtension:@"pdf"]];
}

NSURLRequest *PrintWithJSExecutionOptionTests::pdfRequest()
{
    return namedPDFRequest(@"test_print");
}

NSURLRequest *PrintWithJSExecutionOptionTests::openActionPDFRequest()
{
    return namedPDFRequest(@"test_print_openaction");
}

std::string PrintWithJSExecutionOptionTests::testNameGenerator(testing::TestParamInfo<bool> info)
{
    return std::string { "allowsContentJavascript_is_" } + (info.param ? "true" : "false");
}

void PrintWithJSExecutionOptionTests::runTest(WKWebView *webView, NSURLRequest *request)
{
    RetainPtr delegate = adoptNS([TestPDFPrintDelegate new]);
    [webView setUIDelegate:delegate];

    RetainPtr preferences = adoptNS([[WKWebpagePreferences alloc] init]);
    [preferences setAllowsContentJavaScript:allowsContentJavascript()];

    [webView synchronouslyLoadRequest:request preferences:preferences];

    [delegate waitForPrintFrameCall];
}

void PrintWithJSExecutionOptionTests::runNonPrintingOpenActionTest(WKWebView *webView)
{
    RetainPtr delegate = adoptNS([TestPDFPrintDelegate new]);
    [webView setUIDelegate:delegate];

    RetainPtr preferences = adoptNS([[WKWebpagePreferences alloc] init]);
    [preferences setAllowsContentJavaScript:allowsContentJavascript()];

    for (NSString *resourceName in @[ @"test_openaction_destination", @"test_openaction_goto", @"test_openaction_nonprint" ])
        [webView synchronouslyLoadRequest:namedPDFRequest(resourceName) preferences:preferences];

    [webView synchronouslyLoadRequest:openActionPDFRequest() preferences:preferences];
    [delegate waitForPrintFrameCall];

    EXPECT_EQ([delegate printFrameCallCount], 1u);
}

TEST_P(PrintWithJSExecutionOptionTests, PDFWithWindowPrintEmbeddedJS)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 400, 400)]);
    runTest(webView, pdfRequest());
}

TEST_P(PrintWithJSExecutionOptionTests, PDFWithOpenActionPrintEmbeddedJS)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 400, 400)]);
    runTest(webView, openActionPDFRequest());
}

TEST_P(PrintWithJSExecutionOptionTests, PDFWithNonPrintingOpenActionDoesNotPrint)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 400, 400)]);
    runNonPrintingOpenActionTest(webView);
}

INSTANTIATE_TEST_SUITE_P(Printing, PrintWithJSExecutionOptionTests, testing::Bool(), &TestWebKitAPI::PrintWithJSExecutionOptionTests::testNameGenerator);

#if ENABLE(UNIFIED_PDF) && WK_HAVE_C_SPI

namespace TestWebKitAPI {

static constexpr WKPrintInfo letterPrintInfo { 1, 612, 792 };

static RetainPtr<WKWebViewConfiguration> configurationForPrintingUnifiedPDF(bool remoteSnapshottingEnabled)
{
    RetainPtr configuration = configurationForWebViewTestingUnifiedPDF();
    // With remote snapshotting, the printed pages are recorded into a RemoteSnapshotRecorderProxy
    // (a GraphicsContext without a CGContext) and drawn into the PDF by the GPU process.
    setFeatureEnabled(configuration.get(), @"RemoteSnapshottingEnabled", remoteSnapshottingEnabled);
    return configuration;
}

// Prints the main frame like WKPrintingView does (compute the page rects, then ask for the pages
// as PDF data) and returns the printed PDF data.
static RetainPtr<NSData> printMainFrameToPDF(WKWebView *webView)
{
    WKPageRef page = [webView _pageRefForTransitionToWKWebView];
    WKFrameRef mainFrame = WKPageGetMainFrame(page);

    struct PrintState {
        bool computedPages { false };
        uint32_t pageCount { 0 };
        bool done { false };
        RetainPtr<NSData> data;
    } state;

    WKPageComputePagesForPrinting(page, mainFrame, letterPrintInfo, [](WKRect*, uint32_t pageCount, double, WKErrorRef, void* context) {
        auto& state = *static_cast<PrintState*>(context);
        state.pageCount = pageCount;
        state.computedPages = true;
    }, &state);
    Util::run(&state.computedPages);
    EXPECT_GT(state.pageCount, 0u);

    WKPageDrawPagesToPDF(page, mainFrame, letterPrintInfo, 0, state.pageCount, [](WKDataRef data, WKErrorRef, void* context) {
        auto& state = *static_cast<PrintState*>(context);
        if (data)
            state.data = adoptNS([[NSData alloc] initWithBytes:WKDataGetBytes(data) length:WKDataGetSize(data)]);
        state.done = true;
    }, &state);
    Util::run(&state.done);

    WKPageEndPrinting(page);
    return state.data;
}

static RetainPtr<CGPDFDocumentRef> createPDFDocument(NSData *pdfData)
{
    RetainPtr provider = adoptCF(CGDataProviderCreateWithCFData(bridge_cast(pdfData)));
    return adoptCF(CGPDFDocumentCreateWithProvider(provider.get()));
}

static RetainPtr<CGImageRef> renderPrintedPage(NSData *pdfData, size_t pageNumber)
{
    RetainPtr document = createPDFDocument(pdfData);
    RetainPtr page = CGPDFDocumentGetPage(document.get(), pageNumber);
    if (!page)
        return nullptr;

    auto mediaBox = CGPDFPageGetBoxRect(page.get(), kCGPDFMediaBox);
    RetainPtr colorSpace = adoptCF(CGColorSpaceCreateWithName(kCGColorSpaceSRGB));
    RetainPtr context = adoptCF(CGBitmapContextCreate(nullptr, mediaBox.size.width, mediaBox.size.height, 8, 0, colorSpace.get(), static_cast<uint32_t>(kCGImageAlphaPremultipliedLast) | static_cast<uint32_t>(kCGBitmapByteOrder32Big)));
    CGContextSetRGBFillColor(context.get(), 1, 1, 1, 1);
    CGContextFillRect(context.get(), CGRectMake(0, 0, mediaBox.size.width, mediaBox.size.height));
    CGContextTranslateCTM(context.get(), -mediaBox.origin.x, -mediaBox.origin.y);
    CGContextDrawPDFPage(context.get(), page.get());
    return adoptCF(CGBitmapContextCreateImage(context.get()));
}

static bool colorsAreClose(const WebCore::Color& a, const WebCore::Color& b, int tolerance)
{
    auto [r1, g1, b1, a1] = a.toColorTypeLossy<WebCore::SRGBA<uint8_t>>().resolved();
    auto [r2, g2, b2, a2] = b.toColorTypeLossy<WebCore::SRGBA<uint8_t>>().resolved();
    return std::abs(r1 - r2) <= tolerance && std::abs(g1 - g2) <= tolerance && std::abs(b1 - b2) <= tolerance && std::abs(a1 - a2) <= tolerance;
}

static size_t countPixelsCloseTo(CGImageRef image, const WebCore::Color& color)
{
    CGImagePixelReader reader { image };
    size_t count = 0;
    for (unsigned y = 0; y < reader.height(); ++y) {
        for (unsigned x = 0; x < reader.width(); ++x) {
            if (colorsAreClose(reader.at(x, y), color, 24))
                ++count;
        }
    }
    return count;
}

static size_t countDifferentPixels(CGImageRef image, CGImageRef expectedImage)
{
    CGImagePixelReader reader { image };
    CGImagePixelReader expectedReader { expectedImage };
    EXPECT_EQ(reader.width(), expectedReader.width());
    EXPECT_EQ(reader.height(), expectedReader.height());
    if (reader.width() != expectedReader.width() || reader.height() != expectedReader.height())
        return std::numeric_limits<size_t>::max();

    size_t count = 0;
    for (unsigned y = 0; y < reader.height(); ++y) {
        for (unsigned x = 0; x < reader.width(); ++x) {
            if (!colorsAreClose(reader.at(x, y), expectedReader.at(x, y), 32))
                ++count;
        }
    }
    return count;
}

// The first two pages of multiple-pages-colored.pdf are filled with these colors.
static WebCore::Color firstPageColor()
{
    return WebCore::SRGBA<uint8_t> { 1, 113, 0 };
}

static WebCore::Color secondPageColor()
{
    return WebCore::SRGBA<uint8_t> { 238, 34, 12 };
}

static RetainPtr<NSData> printMainFramePDF(NSString *resourceName, bool remoteSnapshottingEnabled)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600) configuration:configurationForPrintingUnifiedPDF(remoteSnapshottingEnabled).get()]);
    [webView synchronouslyLoadRequest:[NSURLRequest requestWithURL:[NSBundle.test_resourcesBundle URLForResource:resourceName withExtension:@"pdf"]]];
    [webView waitForNextPresentationUpdate];
    return printMainFrameToPDF(webView.get());
}

static void testPrintingMainFramePDF(bool remoteSnapshottingEnabled)
{
    RetainPtr pdfData = printMainFramePDF(@"multiple-pages-colored", remoteSnapshottingEnabled);
    ASSERT_GT([pdfData length], 0u);

    RetainPtr document = createPDFDocument(pdfData.get());
    EXPECT_EQ(CGPDFDocumentGetNumberOfPages(document.get()), 3u);
    EXPECT_TRUE(CGRectEqualToRect(CGPDFPageGetBoxRect(CGPDFDocumentGetPage(document.get(), 1), kCGPDFMediaBox), CGRectMake(0, 0, 612, 792)));

    // Each page of the source document is filled with a solid color, so the printed pages must be too.
    RetainPtr firstPage = renderPrintedPage(pdfData.get(), 1);
    RetainPtr secondPage = renderPrintedPage(pdfData.get(), 2);
    ASSERT_TRUE(firstPage && secondPage);
    size_t pageArea = CGImageGetWidth(firstPage.get()) * CGImageGetHeight(firstPage.get());
    EXPECT_GT(countPixelsCloseTo(firstPage.get(), firstPageColor()), pageArea * 9 / 10);
    EXPECT_GT(countPixelsCloseTo(secondPage.get(), secondPageColor()), pageArea * 9 / 10);
}

UNIFIED_PDF_TEST(PrintMainFramePDF)
{
    testPrintingMainFramePDF(false);
}

UNIFIED_PDF_TEST(PrintMainFramePDFWithRemoteSnapshotting)
{
    testPrintingMainFramePDF(true);
}

UNIFIED_PDF_TEST(PrintMainFramePDFWithRemoteSnapshottingMatchesLocalPrinting)
{
    RetainPtr localPDFData = printMainFramePDF(@"test", false);
    RetainPtr remotePDFData = printMainFramePDF(@"test", true);
    ASSERT_GT([localPDFData length], 0u);
    ASSERT_GT([remotePDFData length], 0u);

    RetainPtr localDocument = adoptNS([[PDFDocument alloc] initWithData:localPDFData.get()]);
    RetainPtr remoteDocument = adoptNS([[PDFDocument alloc] initWithData:remotePDFData.get()]);
    EXPECT_EQ([localDocument pageCount], 1u);
    EXPECT_EQ([remoteDocument pageCount], [localDocument pageCount]);

    // The text of the PDF is still text in the printed document.
    EXPECT_TRUE([[[localDocument pageAtIndex:0] string] containsString:@"Test PDF Content"]);
    EXPECT_TRUE([[[remoteDocument pageAtIndex:0] string] containsString:@"Test PDF Content"]);

    RetainPtr localPage = renderPrintedPage(localPDFData.get(), 1);
    RetainPtr remotePage = renderPrintedPage(remotePDFData.get(), 1);
    ASSERT_TRUE(localPage && remotePage);
    EXPECT_GT(countPixelsCloseTo(localPage.get(), WebCore::Color::black), 100u);
    EXPECT_LT(countDifferentPixels(remotePage.get(), localPage.get()), 100u);
}

static RetainPtr<PDFAnnotation> printedLinkForMainFramePDFWithLink(bool remoteSnapshottingEnabled)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600) configuration:configurationForPrintingUnifiedPDF(remoteSnapshottingEnabled).get()]);
    [webView loadData:testPDFDataWithLink().get() MIMEType:@"application/pdf" characterEncodingName:@"" baseURL:[NSURL URLWithString:@"https://webkit.org/"]];
    [webView _test_waitForDidFinishNavigation];
    [webView waitForNextPresentationUpdate];

    RetainPtr pdfData = printMainFrameToPDF(webView.get());
    RetainPtr document = adoptNS([[PDFDocument alloc] initWithData:pdfData.get()]);
    EXPECT_EQ([document pageCount], 1u);
    for (PDFAnnotation *annotation in [[document pageAtIndex:0] annotations]) {
        if (annotation.URL)
            return annotation;
    }
    return nil;
}

UNIFIED_PDF_TEST(PrintMainFramePDFLinksWithRemoteSnapshotting)
{
    RetainPtr localLink = printedLinkForMainFramePDFWithLink(false);
    RetainPtr remoteLink = printedLinkForMainFramePDFWithLink(true);
    ASSERT_TRUE(localLink && remoteLink);
    EXPECT_WK_STREQ([localLink URL].absoluteString, "https://www.example.com/");
    EXPECT_WK_STREQ([remoteLink URL].absoluteString, [localLink URL].absoluteString);

    auto localBounds = [localLink bounds];
    auto remoteBounds = [remoteLink bounds];
    EXPECT_NEAR(remoteBounds.origin.x, localBounds.origin.x, 1);
    EXPECT_NEAR(remoteBounds.origin.y, localBounds.origin.y, 1);
    EXPECT_NEAR(remoteBounds.size.width, localBounds.size.width, 1);
    EXPECT_NEAR(remoteBounds.size.height, localBounds.size.height, 1);
}

static RetainPtr<NSData> printDocumentWithEmbeddedPDFs(bool remoteSnapshottingEnabled)
{
    RetainPtr webView = adoptNS([[TestWKWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600) configuration:configurationForPrintingUnifiedPDF(remoteSnapshottingEnabled).get()]);
    [webView synchronouslyLoadHTMLStringAndWaitUntilAllImmediateChildFramesPaint:@"<body style='margin: 0'>"
        "<iframe src='multiple-pages-colored.pdf' style='display: block; width: 300px; height: 300px; border: none'></iframe>"
        "<embed src='multiple-pages-colored.pdf' style='display: block; width: 300px; height: 300px; break-before: page'>"
        "</body>"];
    [webView waitForNextPresentationUpdate];
    return printMainFrameToPDF(webView.get());
}

UNIFIED_PDF_TEST(PrintEmbeddedPDFWithRemoteSnapshotting)
{
    RetainPtr localPDFData = printDocumentWithEmbeddedPDFs(false);
    RetainPtr remotePDFData = printDocumentWithEmbeddedPDFs(true);
    ASSERT_GT([localPDFData length], 0u);
    ASSERT_GT([remotePDFData length], 0u);

    RetainPtr localDocument = createPDFDocument(localPDFData.get());
    RetainPtr remoteDocument = createPDFDocument(remotePDFData.get());
    EXPECT_EQ(CGPDFDocumentGetNumberOfPages(localDocument.get()), 2u);
    EXPECT_EQ(CGPDFDocumentGetNumberOfPages(remoteDocument.get()), CGPDFDocumentGetNumberOfPages(localDocument.get()));

    // The iframe is on the first printed page and the embed on the second one (so the PDF is also
    // drawn after a page break). Both show the first (green) page of the PDF.
    for (size_t pageNumber = 1; pageNumber <= 2; ++pageNumber) {
        RetainPtr localPage = renderPrintedPage(localPDFData.get(), pageNumber);
        RetainPtr remotePage = renderPrintedPage(remotePDFData.get(), pageNumber);
        ASSERT_TRUE(localPage && remotePage);
        EXPECT_GT(countPixelsCloseTo(localPage.get(), firstPageColor()), 10000u);
        EXPECT_GT(countPixelsCloseTo(remotePage.get(), firstPageColor()), 10000u);
        size_t pageArea = CGImageGetWidth(localPage.get()) * CGImageGetHeight(localPage.get());
        EXPECT_LT(countDifferentPixels(remotePage.get(), localPage.get()), pageArea / 100);
    }
}

} // namespace TestWebKitAPI

#endif // ENABLE(UNIFIED_PDF) && WK_HAVE_C_SPI

#endif // PLATFORM(MAC)
