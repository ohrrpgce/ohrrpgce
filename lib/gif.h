//
// gif.h
// by Charlie Tangora
// Public domain.
// Email me : ctangora -at- gmail -dot- com
//
// This file offers a simple, very limited way to create animated GIFs directly in code.
//
// Those looking for particular cleverness are likely to be disappointed; it's pretty
// much a straight-ahead implementation of the GIF format with optional Floyd-Steinberg
// dithering. (It does at least use delta encoding - only the changed portions of each
// frame are saved.)
//
// So resulting files are often quite large. The hope is that it will be handy nonetheless
// as a quick and easily-integrated way for programs to spit out animations.
//
// There are two supported input formats, RGBA8 (the alpha is ignored), and
// 8-bit paletted (with a power-of-two palette size).
// (In the latter case you can save up to 768 bytes per frame by providing a global palette
// and reusing it for some frames.)
// Note that only 8-bit input frames can have transparent areas (producing a transparent
// GIF disables delta-coding).
// You can freely mix 32-bit and 8-bit input frames and even frames with differing sizes.
//
// USAGE:
// Allocate a GifWriter struct. Pass it to GifBegin() to initialize and write the header.
// Pass subsequent frames to GifWriteFrame() or GifWriteFrame8().
// If necessary, call GifOverwriteLastDelay() to set the correct delay after-the-fact.
// Finally, call GifEnd() to close the file handle and free memory.
//
// A frame is of the type GifRGBA[height][width], aka uint8_t[height][width][4], such that
//    frame[y][x] = [red, green, blue, alpha]

#ifndef gif_h
#define gif_h

#include <stdio.h>   // for FILE*
#include <string.h>  // for memcpy and bzero
#include <stdint.h>  // for integer typedefs
#include <stddef.h>  // for offsetof

// Define these macros to hook into a custom memory allocator.
// TEMP_MALLOC and TEMP_FREE will only be called in stack fashion - frees in the reverse order of mallocs
// and any temp memory allocated by a function will be freed before it exits.
// MALLOC and FREE are used only by GifBegin and GifEnd respectively (to allocate a buffer the size of the image, which
// is used to find changed pixels for delta-encoding.)
// REALLOC is only used if you mix different frame sizes.

#ifndef GIF_TEMP_MALLOC
#include <stdlib.h>
#define GIF_TEMP_MALLOC malloc
#endif

#ifndef GIF_TEMP_FREE
#include <stdlib.h>
#define GIF_TEMP_FREE free
#endif

#ifndef GIF_MALLOC
#include <stdlib.h>
#define GIF_MALLOC malloc
#endif

#ifndef GIF_REALLOC
#include <stdlib.h>
#define GIF_REALLOC realloc
#endif

#ifndef GIF_FREE
#include <stdlib.h>
#define GIF_FREE free
#endif

const int kGifTransIndex = 0;

// Maximum amount of accumulated diffused error in each color component, for F-S dithering.
// Can be set above 256, but don't go much higher.
// Set to a low value like 64 to minimise color bleeding, or a very
// low value like 16 to reduce the amount of dithering and noise.
int kGifMaxAccumError = 50;

// Define this to collect and print statistics about the quality of the palette
//#define GIF_STATS(x)  x
#define GIF_STATS(x)

#define GIF_ASSERT(x) (void)0
#ifndef GIF_ASSERT
#include <assert.h>
#define GIF_ASSERT assert
#endif

struct GifStats {
    int leaves, searches, totalDiff, nodes, totalLeafCost, maxLeafCost, maxLeafSize, maxLeafRange;
} GIF_STATS(stats);

// Layout of a pixel for GifWriteFrame. You can reorder this, but must be the same as GifRGBA32.
struct GifRGBA
{
    uint8_t b, g, r, a;

    uint8_t& comps(int comp)
    {
        return ((uint8_t*)this)[comp];
    }
};

struct GifRGBA32
{
    int32_t b, g, r, a;
};

struct GifPalette
{
    int bitDepth;  // log2 of the possible number of colors
    //int numColors; // The number of colors actually used (including kGifTransIndex)

    // alpha component is ignored; holds the original palette index when built from a palette.
    GifRGBA colors[256];
};

struct GifHeapQueue {
    int len;
    struct qitem {
        int cost, nodeIndex;
    } items[256];
};

struct GifKDNode {
    uint8_t splitComp : 7;     // Color component index (dimension) to split on (actually member offset)
    uint8_t isLeaf : 1;
    uint8_t palIndex;          // Leaf nodes only
    // The following are not used (and are uninitialised) in leaf nodes
    uint8_t splitVal;          // If the component color is >= this, it's in the right subtree.
    uint16_t left, right;      // Indices of children in GifKDTree.nodes[]
    // The following are used only when building the tree
    int firstPixel, lastPixel; // Range of pixels in the image contained in this box
    int cost;
    GIF_STATS(int maxRange;)
};

// k-d tree over RGB space
struct GifKDTree {
    int numNodes;
    GifKDNode nodes[512];
    GifPalette pal;
    GifHeapQueue queue;
};

// max, min, and abs functions
int GifIMax(int l, int r) { return l>r?l:r; }
int GifIMin(int l, int r) { return l<r?l:r; }
uint8_t GifUI8Max(uint8_t l, uint8_t r) { return l>r?l:r; }
uint8_t GifUI8Min(uint8_t l, uint8_t r) { return l<r?l:r; }
int GifIAbs(int i) { return i<0?-i:i; }

void GifHeapPop( GifHeapQueue* q )
{
    GIF_ASSERT(q->len);

    // We pop off the last element and look where to put it (don't resize yet)
    int hole = 1;
    --q->len;
    GifHeapQueue::qitem *to_insert = &q->items[1 + q->len];

    // Bubble up the hole from root until finding a spot to put to_insert
    while( hole*2 < 1 + q->len )
    {
        // Find the largest child
        int child = hole*2;  // left child
        if( child + 1 < 1 + q->len && q->items[child + 1].cost > q->items[child].cost )
            ++child;
        if( to_insert->cost >= q->items[child].cost )
            break;
        q->items[hole] = q->items[child];
        hole = child;
    }
    q->items[hole] = *to_insert;
}

void GifHeapPush( GifHeapQueue* q, int cost, int key )
{
    // Start from end and bubble down the value until its parent isn't larger
    int hole = 1 + q->len;  // where to place
    ++q->len;
    GIF_ASSERT(q->len < 511);

    while( hole > 1 && q->items[hole/2].cost < cost )
    {
        q->items[hole] = q->items[hole/2];
        hole /= 2;
    }
    q->items[hole].cost = cost;
    q->items[hole].nodeIndex = key;
}

bool GifRGBEqual( GifRGBA pixA, GifRGBA pixB )
{
    return pixA.r == pixB.r && pixA.g == pixB.g && pixA.b == pixB.b;
}

// Check if two palettes have the same colours (k-d tree stuff ignored)
bool GifPalettesEqual( const GifPalette* pPal1, const GifPalette* pPal2 )
{
    return pPal1->bitDepth == pPal2->bitDepth &&
           !memcmp(pPal1->colors, pPal2->colors, sizeof(GifRGBA) * (1 << pPal1->bitDepth));
}

// Update bestDiff and return true if color 'ind' is closer to r,g,b than bestDiff.
bool GifBetterColorMatch(const GifPalette* pPal, int ind, GifRGBA color, int& bestDiff)
{
    int r_err = color.r - (int)pPal->colors[ind].r;
    int g_err = color.g - (int)pPal->colors[ind].g;
    int b_err = color.b - (int)pPal->colors[ind].b;
    int diff = 2*r_err*r_err + 4*g_err*g_err + 3*b_err*b_err;
    if(diff >= bestDiff)
        return false;
    bestDiff = diff;
    return true;
}

// walks the k-d tree to pick the palette entry for a desired color.
// Takes as in/out parameters the current best color and its error -
// only changes them if it finds a better color in its subtree.
// this is the major hotspot in the code at the moment.
void GifGetClosestPaletteColor(GifKDTree* tree, GifRGBA color, int& bestInd, int& bestDiff, int nodeIndex = 0)
{
    GifKDNode &node = tree->nodes[nodeIndex];

    // base case, reached the bottom of the tree
    if(node.isLeaf)
    {
        GIF_STATS(++stats.leaves;)
        // check whether this color is better than the current winner
        if( GifBetterColorMatch(&tree->pal, node.palIndex, color, bestDiff) )
            bestInd = node.palIndex;
        return;
    }

    GIF_STATS(++stats.nodes;)

    // b g r -> 3 4 2
    int comp_mult = (0x020403 >> (node.splitComp << 8)) & 0xff;

    // Compare to the appropriate color component (r, g, or b) for this node of the k-d tree
    int comp = color.comps(node.splitComp);
    if(node.splitVal > comp)
    {
        // check the left subtree
        GifGetClosestPaletteColor(tree, color, bestInd, bestDiff, node.left);
        int cmpdiff = node.splitVal - comp;
        if( bestDiff > comp_mult * cmpdiff*cmpdiff )
        {
            // cannot prove there's not a better value in the right subtree, check that too
            GifGetClosestPaletteColor(tree, color, bestInd, bestDiff, node.right);
        }
    }
    else
    {
        GifGetClosestPaletteColor(tree, color, bestInd, bestDiff, node.right);
        // The left subtree has component values <= (node.splitVal - 1)
        int cmpdiff = comp - (node.splitVal - 1);
        if( bestDiff > comp_mult * cmpdiff*cmpdiff )
        {
            GifGetClosestPaletteColor(tree, color, bestInd, bestDiff, node.left);
        }
    }
}

void GifSwapPixels(GifRGBA* image, int pixA, int pixB)
{
    GifRGBA temp = image[pixA];
    image[pixA] = image[pixB];
    image[pixB] = temp;
}

// just the partition operation from quicksort 3-way
// Center element used as pivot. Afterwards, the pixels in [left, right) have
// 'com' component equal to the pivot; those before/after are lesser/greater.
uint8_t GifPartition(GifRGBA* image, int com, int &left, int &right)
{
    GifSwapPixels(image, left, left + (right - left) / 2);
    uint8_t comPivot = image[left].comps(com);
    for(int i1=left+1; i1<right; ++i1)
    {
        uint8_t comArray = image[i1].comps(com);
        if( comArray < comPivot )
        {
            GifSwapPixels(image, i1, left);
            ++left;
        }
        else if( comArray > comPivot )
        {
            --right;
            GifSwapPixels(image, i1, right);
            --i1;
        }
    }
    return comPivot;
}

// Perform an incomplete sort, finding all elements above and below the desired median
int GifPartitionByMedian(GifRGBA* image, int com, uint8_t& pivotVal, int left, int right)
{
    int neededCenter = left + (right - left) / 2;
    int initLeft = left, initRight = right;
    GIF_ASSERT(left < right-1);
    while(left < right-1)
    {
        int centerLeft = left, centerRight = right;
        pivotVal = GifPartition(image, com, centerLeft, centerRight);
        // Pixels with com equal to pivotVal are now in the interval [centerLeft, centerRight)

        if( neededCenter < centerLeft )
            right = centerLeft;
        else if( neededCenter >= centerRight )
            left = centerRight;
        else if( (centerLeft != initLeft && neededCenter - centerLeft <= centerRight - neededCenter) ||
                 centerRight == initRight )
            // Found the median, but have to decide whether to put it in left or right partition.
            // Never return initLeft or initRight: must carve off at least one pixel
            // (We can assume that there's at least one pixel not equal to pivotVal)
            return centerLeft;
        else
        {
            GIF_ASSERT(pivotVal < 255);
            ++pivotVal;
            return centerRight;
        }
    }
    // This happens when neededCenter == left == right - 1. Those two pixels may or may not be equal
    GIF_ASSERT(left > initLeft);
    GIF_ASSERT(neededCenter == left);
    pivotVal = image[left].comps(com);  // [left,initRight) is the right subtree
    return neededCenter;
}

// Create the palette, by taking the average of all colors in each subcube (k-d tree leaf)
void GifAverageColors(GifRGBA* image, GifKDTree* tree)
{
    tree->pal.colors[kGifTransIndex].r = 0;
    tree->pal.colors[kGifTransIndex].g = 0;
    tree->pal.colors[kGifTransIndex].b = 0;
    tree->pal.colors[kGifTransIndex].a = 0;

    // Fill the rest of the palette with the leaf nodes. Nodes still on the queue are leaves
    int palIndex = 0;
    for( int qIndex = 1; qIndex <= tree->queue.len; ++qIndex, ++palIndex )
    {
        if( palIndex == kGifTransIndex ) ++palIndex;
        GifHeapQueue::qitem& qitem = tree->queue.items[qIndex];
        GifKDNode& node = tree->nodes[qitem.nodeIndex];

        GIF_STATS(stats.totalLeafCost += qitem.cost;)
        GIF_STATS(stats.maxLeafCost = GifIMax(stats.maxLeafCost, qitem.cost);)
        GIF_STATS(stats.maxLeafRange = GifIMax(stats.maxLeafRange, node.maxRange);)
        GIF_STATS(stats.maxLeafSize = GifIMax(stats.maxLeafSize, node.lastPixel - node.firstPixel);)
        //GIF_STATS(printf("col %d q %d node %d cost %d range %d size %d\n", palIndex, qIndex, qitem.nodeIndex, qitem.cost, node.maxRange, node.lastPixel - node.firstPixel));

        uint64_t r=0, g=0, b=0;
        // If there are no changed pixels, then there is a single leaf node, with no pixels
        if( node.firstPixel != node.lastPixel )
        {
                // Possible optimisation: if node.cost == 0, just use image[node.firstPixel]
                for( int ii = node.firstPixel; ii < node.lastPixel; ++ii )
                {
                    r += image[ii].r;
                    g += image[ii].g;
                    b += image[ii].b;
                }

                uint32_t numPixels = node.lastPixel - node.firstPixel;
                r += (uint64_t)numPixels / 2;  // round to nearest
                g += (uint64_t)numPixels / 2;
                b += (uint64_t)numPixels / 2;

                r /= (uint32_t)numPixels;
                g /= (uint32_t)numPixels;
                b /= (uint32_t)numPixels;
        }
        GifRGBA& col = tree->pal.colors[palIndex];
        col.r = (uint8_t)r;
        col.g = (uint8_t)g;
        col.b = (uint8_t)b;
        // When building a k-d tree with a fixed palette col.a is the original palette index, otherwise it's garbage
        col.a = image[node.firstPixel].a;
        node.palIndex = palIndex;
    }
    //tree->pal.numColors = GifIMax(palIndex, kGifTransIndex + 1);
}

// Calculate cost of a node, and which way we should split it
void GifEvalNode( GifRGBA* image, GifKDNode& node )
{
    // (Note: node.firstPixel == node.lastPixel is possible)
    // Find the axis with the largest range
    int minR = 255, maxR = 0;
    int minG = 255, maxG = 0;
    int minB = 255, maxB = 0;
    for(int ii = node.firstPixel; ii < node.lastPixel; ++ii)
    {
        int r = image[ii].r;
        int g = image[ii].g;
        int b = image[ii].b;

        if(r > maxR) maxR = r;
        if(r < minR) minR = r;

        if(g > maxG) maxG = g;
        if(g < minG) minG = g;

        if(b > maxB) maxB = b;
        if(b < minB) minB = b;
    }

    int rRange = maxR - minR;
    int gRange = maxG - minG;
    int bRange = maxB - minB;
    int maxRange = GifIMax(GifIMax(rRange, gRange), bRange);
    GIF_STATS(node.maxRange = maxRange;)
    node.cost = maxRange * (node.lastPixel - node.firstPixel);

    node.splitComp = offsetof(struct GifRGBA, g);
    if(bRange > gRange) node.splitComp = offsetof(struct GifRGBA, b);
    if(rRange > bRange && rRange > gRange) node.splitComp = offsetof(struct GifRGBA, r);
}

int GifAddNode( GifKDTree* tree, GifRGBA* image, int firstPixel, int lastPixel )
{
    int nodeIndex = tree->numNodes++;
    GIF_ASSERT(nodeIndex < 512);
    GifKDNode& node = tree->nodes[nodeIndex];
    node.firstPixel = firstPixel;
    node.lastPixel = lastPixel;
    node.isLeaf = true;
    GifEvalNode(image, node);
    GifHeapPush(&tree->queue, node.cost, nodeIndex);
    return nodeIndex;
}

// Split a leaf node in two. It must not be entirely one color (splitting by splitComp must reduce cost).
void GifSplitNode( GifRGBA* image, GifKDTree* tree, GifKDNode& node )
{
    int medianPixel = GifPartitionByMedian(image, node.splitComp, node.splitVal, node.firstPixel, node.lastPixel);
    GIF_ASSERT(medianPixel > node.firstPixel);
    GIF_ASSERT(medianPixel < node.lastPixel);
    GIF_ASSERT(node.splitVal > 0);

    node.left = GifAddNode(tree, image, node.firstPixel, medianPixel);
    node.right = GifAddNode(tree, image, medianPixel, node.lastPixel);
    node.isLeaf = false;
}

// Builds a palette by creating a k-d tree of all pixels in the image
void GifSplitPalette( GifRGBA* image, GifKDTree* tree )
{
    // -1 for transparent color
    int maxLeaves = (1 << tree->pal.bitDepth) - 1;
    while( tree->queue.len < maxLeaves )
    {
        int nodeIndex = tree->queue.items[1].nodeIndex;  // Top of the heap
        GifKDNode& node = tree->nodes[nodeIndex];

        if( node.cost <= 0 )
            break;
        GIF_ASSERT(node.lastPixel > node.firstPixel + 1);  // At least two pixels

        GifHeapPop(&tree->queue);
        GifSplitNode(image, tree, node);
    }
}

// Finds all pixels that have changed from the previous image and
// moves them to the front of the buffer.
// This allows us to build a palette optimized for the colors of the
// changed pixels only.
int GifPickChangedPixels( const GifRGBA* lastFrame, GifRGBA* frame, int numPixels )
{
    int numChanged = 0;
    GifRGBA* writeIter = frame;

    for (int ii=0; ii<numPixels; ++ii)
    {
        if( !GifRGBEqual(*lastFrame, *frame) )
        {
            *writeIter++ = *frame;
            ++numChanged;
        }
        ++lastFrame;
        ++frame;
    }

    return numChanged;
}

// Creates a palette by placing all the image pixels in a k-d tree and then averaging the blocks at the bottom.
// This is known as the "modified median split" technique
void GifMakePalette( const GifRGBA* lastFrame, const GifRGBA* nextFrame, uint32_t width, uint32_t height, int bitDepth, bool buildForDither, GifKDTree* tree )
{
    (void)buildForDither;

    tree->pal.bitDepth = bitDepth;
    tree->numNodes = 0;
    tree->queue.len = 0;

    // SplitPalette is destructive (it sorts the pixels by color) so
    // we must create a copy of the image for it to destroy
    size_t imageSize = width * height * sizeof(GifRGBA);
    GifRGBA* destroyableImage = (GifRGBA*)GIF_TEMP_MALLOC(imageSize);
    memcpy(destroyableImage, nextFrame, imageSize);

    int numPixels = (int)(width * height);
    if(lastFrame)
        numPixels = GifPickChangedPixels(lastFrame, destroyableImage, numPixels);

    // initial node
    GifAddNode(tree, destroyableImage, 0, numPixels);

    GifSplitPalette(destroyableImage, tree);
    GifAverageColors(destroyableImage, tree);

    GIF_TEMP_FREE(destroyableImage);
}

// Implements Floyd-Steinberg dithering, writes palette index to alpha
void GifDitherImage( const GifRGBA* lastFrame, const GifRGBA* nextFrame, GifRGBA* outFrame, uint32_t width, uint32_t height, GifKDTree* tree )
{
    int numPixels = (int)(width * height);

    // errorPixels holds the accumulated error for each pixel; alpha channel ignored.
    // The extra 8 bits of precision allow for sub-single-color error values
    // to be propagated
    GifRGBA32* errorPixels = (GifRGBA32*)GIF_TEMP_MALLOC(sizeof(GifRGBA32) * (size_t)numPixels);
    memset(errorPixels, 0, sizeof(GifRGBA32) * (size_t)numPixels);

    for( uint32_t yy=0; yy<height; ++yy )
    {
        for( uint32_t xx=0; xx<width; ++xx )
        {
            GifRGBA nextPix = nextFrame[yy*width+xx];  // input
            GifRGBA32 errorPix = errorPixels[yy*width+xx];  // input
            GifRGBA& outPix = outFrame[yy*width+xx];  // output
            const GifRGBA* lastPix = lastFrame? &lastFrame[yy*width+xx] : NULL;

            // If this pixel didn't change in this frame then don't output anything here,
            // it creates flickering and bad compression. Though we could still propagate
            // the difference between searchColor and lastPix.
            if( lastFrame && GifRGBEqual(nextPix, *lastPix) )
            {
                outPix = nextPix;
                outPix.a = kGifTransIndex;
                continue;
            }

            // Cap the diffused error to prevent excessive bleeding.
            errorPix.r = GifIMin( kGifMaxAccumError * 256, GifIMax( -kGifMaxAccumError * 256, errorPix.r) );
            errorPix.g = GifIMin( kGifMaxAccumError * 256, GifIMax( -kGifMaxAccumError * 256, errorPix.g) );
            errorPix.b = GifIMin( kGifMaxAccumError * 256, GifIMax( -kGifMaxAccumError * 256, errorPix.b) );
            errorPix.r += (int32_t)nextPix.r * 256;
            errorPix.g += (int32_t)nextPix.g * 256;
            errorPix.b += (int32_t)nextPix.b * 256;

            // Compute the colors we want (rounding to nearest)
            GifRGBA searchColor;
            searchColor.r = (uint8_t)GifIMin(255, GifIMax(0, (errorPix.r + 127) / 256));
            searchColor.g = (uint8_t)GifIMin(255, GifIMax(0, (errorPix.g + 127) / 256));
            searchColor.b = (uint8_t)GifIMin(255, GifIMax(0, (errorPix.b + 127) / 256));
            searchColor.a = 0;

            int32_t bestDiff = 1000000;
            int32_t bestInd = kGifTransIndex;

            // Search the palette
            GifGetClosestPaletteColor(tree, searchColor, bestInd, bestDiff);
            GIF_STATS(stats.searches++;)
            GIF_STATS(stats.totalDiff += bestDiff;)

            GifRGBA selectedColor = tree->pal.colors[bestInd];

            // Write the result to the temp buffer
            // If it happens that we want the color from last frame, then just write out
            // a transparent pixel
            if( lastFrame && GifRGBEqual(selectedColor, *lastPix) )
            {
                outPix = nextPix;  // (lastPix might point to outPix, can't hoist out)
                outPix.a = kGifTransIndex;
            }
            else
            {
                //outPix = selectedColor;  // More correct but breaks delta coding
                outPix = nextPix;
                outPix.a = bestInd;
            }

            int32_t r_err = errorPix.r - selectedColor.r * 256;
            int32_t g_err = errorPix.g - selectedColor.g * 256;
            int32_t b_err = errorPix.b - selectedColor.b * 256;

            // Propagate the error to the four adjacent locations
            // that we haven't touched yet
            int quantloc_7 = (int)(yy*width+xx+1);
            int quantloc_3 = (int)(yy*width+width+xx-1);
            int quantloc_5 = (int)(yy*width+width+xx);
            int quantloc_1 = (int)(yy*width+width+xx+1);

            if(quantloc_7 < numPixels)
            {
                GifRGBA32& pix7 = errorPixels[quantloc_7];
                pix7.r += r_err * 6 / 16;
                pix7.g += g_err * 6 / 16;
                pix7.b += b_err * 6 / 16;
            }

            if(quantloc_3 < numPixels)
            {
                GifRGBA32& pix3 = errorPixels[quantloc_3];
                pix3.r += r_err * 3 / 16;
                pix3.g += g_err * 3 / 16;
                pix3.b += b_err * 3 / 16;
            }

            if(quantloc_5 < numPixels)
            {
                GifRGBA32& pix5 = errorPixels[quantloc_5];
                pix5.r += r_err * 4 / 16;
                pix5.g += g_err * 4 / 16;
                pix5.b += b_err * 4 / 16;
            }

            if(quantloc_1 < numPixels)
            {
                GifRGBA32& pix1 = errorPixels[quantloc_1];
                pix1.r += r_err / 16;
                pix1.g += g_err / 16;
                pix1.b += b_err / 16;
            }
        }
    }

    GIF_TEMP_FREE(errorPixels);
}

// Picks palette colors for the image using simple thresholding, no dithering. Writes palette index to alpha.
void GifThresholdImage( const GifRGBA* lastFrame, const GifRGBA* nextFrame, GifRGBA* outFrame, uint32_t width, uint32_t height, GifKDTree* tree )
{
    uint32_t numPixels = width*height;
    for( uint32_t ii=0; ii<numPixels; ++ii )
    {
        // if a previous color is available, and it matches the current color,
        // set the pixel to transparent
        if(lastFrame && GifRGBEqual(*lastFrame, *nextFrame))
        {
            *outFrame = *lastFrame;
            outFrame->a = kGifTransIndex;
        }
        else
        {
            // palettize the pixel
            int32_t bestDiff = 1000000;
            int32_t bestInd = 1;
            GifGetClosestPaletteColor(tree, *nextFrame, bestInd, bestDiff);
            GIF_STATS(stats.searches++;)
            GIF_STATS(stats.totalDiff += bestDiff;)

            // Write the resulting color to the output buffer
            *outFrame = tree->pal.colors[bestInd];
            outFrame->a = (uint8_t)bestInd;
        }

        if(lastFrame) ++lastFrame;
        ++outFrame;
        ++nextFrame;
    }
}

// Compare an already paletted frame to the previous one.
// nextFrame8 is 8-bit, lastFrame and outFrame are 32-bit.
void GifDeltaImage( const GifRGBA* lastFrame, const uint8_t* nextFrame8, GifRGBA* outFrame, uint32_t width, uint32_t height, bool deltaCoded, const GifPalette* pPal )
{
    uint32_t numPixels = width*height;
    int transReplacement = 0;
    if(deltaCoded)
    {
        // Not allowed to use kGifTransIndex, so remap it to nearest match
        int bestDiff = 1000000;
        GifRGBA col = pPal->colors[kGifTransIndex];
        for( int ind=0; ind<(1 << pPal->bitDepth); ++ind )
        {
            // check whether this color is better than the current winner
            if( ind != kGifTransIndex && GifBetterColorMatch(pPal, ind, col, bestDiff) )
                transReplacement = ind;
        }
    }

    for( uint32_t ii=0; ii<numPixels; ++ii )
    {
        int ind = nextFrame8[ii];
        if(ind == kGifTransIndex)
            ind = transReplacement;

        // if a previous color is available, and it matches the current color,
        // set the pixel to transparent
        if(lastFrame && GifRGBEqual(*lastFrame, pPal->colors[ind]))
        {
            *outFrame = *lastFrame;
            outFrame->a = kGifTransIndex;
        }
        else
        {
            *outFrame = pPal->colors[ind];
            outFrame->a = (uint8_t)ind;
        }

        if(lastFrame) ++lastFrame;
        ++outFrame;
    }
}

// Simple structure to write out the LZW-compressed portion of the image
// one bit at a time
struct GifBitStatus
{
    uint8_t bitIndex;  // how many bits in the partial byte written so far
    uint8_t byte;      // current partial byte

    uint32_t chunkIndex;
    uint8_t chunk[256];   // bytes are written in here until we have 256 of them, then written to the file
};

// insert a single bit
void GifWriteBit( GifBitStatus& stat, uint32_t bit )
{
    bit = bit & 1;
    bit = bit << stat.bitIndex;
    stat.byte |= bit;

    ++stat.bitIndex;
    if( stat.bitIndex > 7 )
    {
        // move the newly-finished byte to the chunk buffer
        stat.chunk[stat.chunkIndex++] = stat.byte;
        // and start a new byte
        stat.bitIndex = 0;
        stat.byte = 0;
    }
}

// write all bytes so far to the file
void GifWriteChunk( FILE* f, GifBitStatus& stat )
{
    fputc((int)stat.chunkIndex, f);
    fwrite(stat.chunk, 1, stat.chunkIndex, f);

    stat.bitIndex = 0;
    stat.byte = 0;
    stat.chunkIndex = 0;
}

void GifWriteCode( FILE* f, GifBitStatus& stat, uint32_t code, uint32_t length )
{
    for( uint32_t ii=0; ii<length; ++ii )
    {
        GifWriteBit(stat, code);
        code = code >> 1;

        if( stat.chunkIndex == 255 )
        {
            GifWriteChunk(f, stat);
        }
    }
}

// The LZW dictionary is a 256-ary tree constructed as the file is encoded,
// this is one node
struct GifLzwNode
{
    uint16_t m_next[256];
};

// write an image palette to the file
void GifWritePalette( const GifPalette* pPal, FILE* f )
{
    fputc(0, f);  // first color: transparency
    fputc(0, f);
    fputc(0, f);
    for(int ii=1; ii<(1 << pPal->bitDepth); ++ii)
    {
        const GifRGBA &col = pPal->colors[ii];
        fputc((int)col.r, f);
        fputc((int)col.g, f);
        fputc((int)col.b, f);
    }
}

// write the image header, LZW-compress and write out the image
// deltaCoded is true if transparency is used for delta coding, false if producing a transparent GIF
// localPalette is true to write out pPal as a local palette; otherwise it is the global palette.
void GifWriteLzwImage(FILE* f, GifRGBA* image, uint32_t left, uint32_t top,  uint32_t width, uint32_t height, uint32_t delay, const GifPalette* pPal, bool deltaCoded, bool localPalette)
{
    // graphics control extension
    fputc(0x21, f);
    fputc(0xf9, f);
    fputc(0x04, f);
    // disposal method
    if( deltaCoded )
        fputc(0x05, f); // leave this frame in place (next will draw on top)
    else
        fputc(0x09, f); // replace this frame with the background (so next can have transparent areas)
    fputc(delay & 0xff, f);
    fputc((delay >> 8) & 0xff, f);
    fputc(kGifTransIndex, f); // transparent color index
    fputc(0, f);

    fputc(0x2c, f); // image descriptor block

    fputc(left & 0xff, f);           // corner of image in canvas space
    fputc((left >> 8) & 0xff, f);
    fputc(top & 0xff, f);
    fputc((top >> 8) & 0xff, f);

    fputc(width & 0xff, f);          // width and height of image
    fputc((width >> 8) & 0xff, f);
    fputc(height & 0xff, f);
    fputc((height >> 8) & 0xff, f);

    if( localPalette )
    {
        fputc(0x80 + pPal->bitDepth-1, f); // local color table present, 2 ^ bitDepth entries
        GifWritePalette(pPal, f);
    }
    else
    {
        fputc(0, f); // no local color table
    }

    const int minCodeSize = pPal->bitDepth;
    const uint32_t clearCode = 1 << pPal->bitDepth;

    fputc(minCodeSize, f); // min code size 8 bits

    GifLzwNode* codetree = (GifLzwNode*)GIF_TEMP_MALLOC(sizeof(GifLzwNode)*4096);

    memset(codetree, 0, sizeof(GifLzwNode)*4096);
    int32_t curCode = -1;
    uint32_t codeSize = (uint32_t)minCodeSize + 1;
    uint32_t maxCode = clearCode+1;

    GifBitStatus stat;
    stat.byte = 0;
    stat.bitIndex = 0;
    stat.chunkIndex = 0;

    GifWriteCode(f, stat, clearCode, codeSize);  // start with a fresh LZW dictionary

    for(uint32_t yy=0; yy<height; ++yy)
    {
        for(uint32_t xx=0; xx<width; ++xx)
        {
            uint8_t nextValue = image[yy*width+xx].a;

            // "loser mode" - no compression, every single code is followed immediately by a clear
            //WriteCode( f, stat, nextValue, codeSize );
            //WriteCode( f, stat, 256, codeSize );

            if( curCode < 0 )
            {
                // the first value in the image
                curCode = nextValue;
            }
            else if( codetree[curCode].m_next[nextValue] )
            {
                // current run already in the dictionary
                curCode = codetree[curCode].m_next[nextValue];
            }
            else
            {
                // finish the current run, write a code
                GifWriteCode( f, stat, (uint32_t)curCode, codeSize );

                // insert the new run into the dictionary
                codetree[curCode].m_next[nextValue] = (uint16_t)++maxCode;

                if( maxCode >= (1ul << codeSize) )
                {
                    // dictionary entry count has broken a size barrier,
                    // we need more bits for codes
                    codeSize++;
                }
                if( maxCode == 4095 )
                {
                    // the dictionary is full, clear it out and begin anew
                    GifWriteCode(f, stat, clearCode, codeSize); // clear tree

                    memset(codetree, 0, sizeof(GifLzwNode)*4096);
                    codeSize = (uint32_t)minCodeSize + 1;
                    maxCode = clearCode+1;
                }

                curCode = nextValue;
            }
        }
    }

    // compression footer
    GifWriteCode( f, stat, (uint32_t)curCode, codeSize );
    GifWriteCode( f, stat, clearCode, codeSize );
    GifWriteCode( f, stat, clearCode+1, (uint32_t)minCodeSize+1 );

    // write out the last partial chunk
    while( stat.bitIndex ) GifWriteBit(stat, 0);
    if( stat.chunkIndex ) GifWriteChunk(f, stat);

    fputc(0, f); // image block terminator

    GIF_TEMP_FREE(codetree);
}

struct GifWriter
{
    FILE* f;
    GifRGBA* oldImage;
    bool firstFrame;
    bool deltaCoded;
    GifPalette* globalPal;
    int maxWidth;
    int maxHeight;
    int currentWidth;
    int currentHeight;
    bool sizeChanged;
    long int lastFramePos;
};

// Handle a call to GifWriteFrame[8] with a different image size to the previous
void GifHandleSizeChange( GifWriter* writer, int width, int height )
{
    if(writer->currentWidth != width || writer->currentHeight != height)
    {
        if(writer->currentWidth > width || writer->currentHeight > height)
        {
            // Change the disposal method for the previous frame, to erase the parts of
            // the image outside the new image
            long int pos = ftell(writer->f);
            fseek(writer->f, writer->lastFramePos + 3, SEEK_SET);
            fputc(0x09, writer->f); // replace this frame with the background
            fseek(writer->f, pos, SEEK_SET);
        }
        writer->maxWidth = GifIMax(writer->maxWidth, width);
        writer->maxHeight = GifIMax(writer->maxHeight, height);
        writer->currentWidth = width;
        writer->currentHeight = height;
        writer->oldImage = (GifRGBA*)GIF_REALLOC(writer->oldImage, (size_t)(width*height) * sizeof(GifRGBA));
        writer->firstFrame = true;  // Ignore the contents of oldImage
        writer->sizeChanged = true;
    }
}

// Creates a gif file.
// The input GIFWriter is assumed to be uninitialized.
// The delay value is the time between frames in hundredths of a second - note that not all viewers pay much attention to this value.
// transparent is whether to produce a transparent GIF. It only works if using GifWriteFrame8()
//     to provide images containing transparency, and it disables delta coding.
// globalPal is a default palette to use for GifWriteFrame8(). It is not used by GifWriteFrame().
bool GifBegin( GifWriter* writer, FILE *file, uint32_t width, uint32_t height, uint32_t delay, bool transparent = false, const GifPalette* globalPal = NULL )
{
    if(!file) return false;
    writer->f = file;

    writer->firstFrame = true;
    writer->deltaCoded = !transparent;
    writer->lastFramePos = -1;

    // allocate
    writer->oldImage = (GifRGBA*)GIF_MALLOC(width*height*sizeof(GifRGBA));

    fputs("GIF89a", writer->f);

    // screen descriptor
    fputc(width & 0xff, writer->f);
    fputc((width >> 8) & 0xff, writer->f);
    fputc(height & 0xff, writer->f);
    fputc((height >> 8) & 0xff, writer->f);
    writer->currentWidth = writer->maxWidth = (int)width;
    writer->currentHeight = writer->maxHeight = (int)height;
    writer->sizeChanged = false;

    if( globalPal )
        fputc(0xf0 + (globalPal->bitDepth - 1), writer->f);  // there is an unsorted global color table
    else
        fputc(0xf0, writer->f);  // there is an unsorted global color table of 2 entries
    fputc(0, writer->f);     // background color
    fputc(0, writer->f);     // pixels are square (we need to specify this because it's 1989)

    if( globalPal )
    {
        writer->globalPal = (GifPalette*)GIF_MALLOC(sizeof(GifPalette));
        memcpy(writer->globalPal, globalPal, sizeof(GifPalette));
        // write the global palette
        GifWritePalette(globalPal, writer->f);
    }
    else
    {
        writer->globalPal = NULL;
        // now the "global" palette (really just a dummy palette)
        // color 0: black
        fputc(0, writer->f);
        fputc(0, writer->f);
        fputc(0, writer->f);
        // color 1: also black
        fputc(0, writer->f);
        fputc(0, writer->f);
        fputc(0, writer->f);
    }

    if( delay != 0 )
    {
        // animation header
        fputc(0x21, writer->f); // extension
        fputc(0xff, writer->f); // application specific
        fputc(11, writer->f); // length 11
        fputs("NETSCAPE2.0", writer->f); // yes, really
        fputc(3, writer->f); // 3 bytes of NETSCAPE2.0 data

        fputc(1, writer->f); // JUST BECAUSE
        fputc(0, writer->f); // loop infinitely (byte 0)
        fputc(0, writer->f); // loop infinitely (byte 1)

        fputc(0, writer->f); // block terminator
    }

    return true;
}

// Writes out a new frame to a GIF in progress.
// The GIFWriter should have been created by GIFBegin.
// AFAIK, it is legal to use different bit depths for different frames of an image -
// this may be handy to save bits in animations that don't change much.
bool GifWriteFrame( GifWriter* writer, const GifRGBA* image, uint32_t width, uint32_t height, uint32_t delay, int bitDepth = 8, bool dither = false )
{
    if(!writer->f) return false;
    if(bitDepth <= 0 || bitDepth > 8) return false;
    GIF_STATS(memset(&stats, 0, sizeof stats);)

    GifHandleSizeChange(writer, (int)width, (int)height);
    const GifRGBA* oldImage = writer->firstFrame? NULL : writer->oldImage;
    // Only GifWriteFrame8 can produce transparent frames, but the frame before the current one
    // needs to be set to 'background' disposal to support that. So for simplicity, we disable
    // delta coding for all frames.
    if(!writer->deltaCoded)
        oldImage = NULL;
    writer->firstFrame = false;

    GifKDTree tree;
    GifMakePalette(oldImage, image, width, height, bitDepth, dither, &tree);

    if(dither)
        GifDitherImage(oldImage, image, writer->oldImage, width, height, &tree);
    else
        GifThresholdImage(oldImage, image, writer->oldImage, width, height, &tree);

    GIF_STATS(
        printf("GetClosestPaletteColor avg: internal nodes searched = %.3f leaves searched = %.3f match distance = %.4f\n",
               1. * stats.nodes / stats.searches, 1. * stats.leaves / stats.searches,
               1. * stats.totalDiff / stats.searches);
        printf("k-d tree leaves: num = %d, max size (pixels) = %d, max range = %d, max cost = %d, avg cost = %.3f\n",
               tree.queue.len, stats.maxLeafSize, stats.maxLeafRange, stats.maxLeafCost,
               1. * stats.totalLeafCost / tree.queue.len);
    )

    writer->lastFramePos = ftell(writer->f);
    GifWriteLzwImage(writer->f, writer->oldImage, 0, 0, width, height, delay, &tree.pal, writer->deltaCoded, true);

    return true;
}

// (See also GifWriteFrame.)
// If palette is NULL, or if it is identical to the global palette, then the global palette is used.
// If the GIF is transparent then index kGifTransIndex is transparency, otherwise
// all color indices may be used (however, kGifTransIndex is internally used for delta-coding so
// when it occurs in the input it will be replaced with the nearest match, so it's better to avoid it).
bool GifWriteFrame8( GifWriter* writer, const uint8_t* image, uint32_t width, uint32_t height, uint32_t delay, const GifPalette* pal = NULL )
{
    if(!writer->f) return false;
    if(!writer->globalPal && !pal) return false;

    GifHandleSizeChange(writer, (int)width, (int)height);
    const GifRGBA* oldImage = writer->firstFrame? NULL : writer->oldImage;
    if(!writer->deltaCoded)
        oldImage = NULL;
    writer->firstFrame = false;

    // Only write the local palette if it differs from global, or if there is no global palette
    if(pal && writer->globalPal && GifPalettesEqual(writer->globalPal, pal))
        pal = NULL;
    bool localPalette = (pal != NULL);
    if(!pal)
        pal = writer->globalPal;

    GifDeltaImage(oldImage, image, writer->oldImage, width, height, writer->deltaCoded, pal);

    writer->lastFramePos = ftell(writer->f);
    GifWriteLzwImage(writer->f, writer->oldImage, 0, 0, width, height, delay, pal, writer->deltaCoded, localPalette);

    return true;
}

// Change the delay for the last frame written
void GifOverwriteLastDelay( GifWriter* writer, uint32_t delay )
{
    FILE* f = writer->f;
    if(!f || writer->lastFramePos == -1) return;
    long int pos = ftell(f);
    fseek(f, writer->lastFramePos + 4, SEEK_SET);
    fputc(delay & 0xff, f);
    fputc((delay >> 8) & 0xff, f);
    fseek(f, pos, SEEK_SET);
}

// Writes the EOF code, closes the file handle, and frees temp memory used by a GIF.
// Many if not most viewers will still display a GIF properly if the EOF code is missing,
// but it's still a good idea to write it out.
bool GifEnd( GifWriter* writer )
{
    if(!writer->f) return false;

    fputc(0x3b, writer->f); // end of file
    if(writer->sizeChanged)
    {
        fseek(writer->f, 6, SEEK_SET);
        fputc(writer->maxWidth & 0xff, writer->f);
        fputc((writer->maxWidth >> 8) & 0xff, writer->f);
        fputc(writer->maxHeight & 0xff, writer->f);
        fputc((writer->maxHeight >> 8) & 0xff, writer->f);
    }
    fclose(writer->f);
    GIF_FREE(writer->oldImage);
    GIF_FREE(writer->globalPal);

    writer->f = NULL;
    writer->oldImage = NULL;
    writer->globalPal = NULL;

    return true;
}

#endif
