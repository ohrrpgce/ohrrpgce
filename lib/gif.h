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
// Create a GifWriter struct. Pass it to GifBegin() to initialize and write the header.
// Pass subsequent frames to GifWriteFrame() or GifWriteFrame8().
// Finally, call GifEnd() to close the file handle and free memory.
//
// A frame is of the type GifRGBA[width][height], aka uint8_t[width][height][4], such that
//    frame[x][y] = [red, green, blue, alpha]

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

// Layout of a pixel passed to GifWriteFrame. You can reorder this, but must be the same as GifRGBA32
union GifRGBA
{
    struct {
        uint8_t b, g, r, a;
    };
    uint8_t comps[4];
};

struct GifRGBA32
{
    uint32_t b, g, r, a;
};

struct GifPalette
{
    int bitDepth;

    uint8_t r[256];
    uint8_t g[256];
    uint8_t b[256];

    // (The following do not need to be initialised when passed to GifBegin() and GifWriteFrame8().)
    // k-d tree over RGB space, organized in heap fashion
    // i.e. left child of node i is node i*2, right child is node i*2+1
    // nodes 256-511 are implicitly the leaves, containing a color
    uint8_t treeSplitElt[256];
    uint8_t treeSplit[256];
};

// max, min, and abs functions
int GifIMax(int l, int r) { return l>r?l:r; }
int GifIMin(int l, int r) { return l<r?l:r; }
int GifIAbs(int i) { return i<0?-i:i; }

bool GifRGBEqual( GifRGBA pixA, GifRGBA pixB )
{
    return pixA.r == pixB.r && pixA.g == pixB.g && pixA.b == pixB.b;
}

// Check if two palettes are equal
bool GifPalettesEqual( const GifPalette* pPal1, const GifPalette* pPal2 )
{
    if( pPal1->bitDepth != pPal2->bitDepth ) return false;
    return !(memcmp(pPal1->r, pPal2->r, 1 << pPal1->bitDepth) ||
             memcmp(pPal1->g, pPal2->g, 1 << pPal1->bitDepth) ||
             memcmp(pPal1->b, pPal2->b, 1 << pPal1->bitDepth));
}

// Update bestDiff and return true if color 'ind' is closer to r,g,b than bestDiff (and not transparent).
bool GifBetterColorMatch(const GifPalette* pPal, int ind, int r, int g, int b, int& bestDiff)
{
    if(ind == kGifTransIndex) return false;

    int r_err = r - ((int32_t)pPal->r[ind]);
    int g_err = g - ((int32_t)pPal->g[ind]);
    int b_err = b - ((int32_t)pPal->b[ind]);
    int diff = GifIAbs(r_err)+GifIAbs(g_err)+GifIAbs(b_err);
    if(diff >= bestDiff)
        return false;
    bestDiff = diff;
    return true;
}

// walks the k-d tree to pick the palette entry for a desired color.
// Takes as in/out parameters the current best color and its error -
// only changes them if it finds a better color in its subtree.
// this is the major hotspot in the code at the moment.
void GifGetClosestPaletteColor(GifPalette* pPal, int r, int g, int b, int& bestInd, int& bestDiff, int treeRoot = 1)
{
    // base case, reached the bottom of the tree
    if(treeRoot > (1<<pPal->bitDepth)-1)
    {
        int ind = treeRoot-(1<<pPal->bitDepth);

        // check whether this color is better than the current winner
        if( GifBetterColorMatch(pPal, ind, r, g, b, bestDiff) )
            bestInd = ind;
        return;
    }

    // ignore unused nodes
    if(pPal->treeSplitElt[treeRoot] >= 3)
    {
        return;
    }

    // take the appropriate color (r, g, or b) for this node of the k-d tree
    int comps[3]; comps[0] = r; comps[1] = g; comps[2] = b;
    int splitComp = comps[pPal->treeSplitElt[treeRoot]];

    int splitPos = pPal->treeSplit[treeRoot];
    if(splitPos > splitComp)
    {
        // check the left subtree
        GifGetClosestPaletteColor(pPal, r, g, b, bestInd, bestDiff, treeRoot*2);
        if( bestDiff > splitPos - splitComp )
        {
            // cannot prove there's not a better value in the right subtree, check that too
            GifGetClosestPaletteColor(pPal, r, g, b, bestInd, bestDiff, treeRoot*2+1);
        }
    }
    else
    {
        GifGetClosestPaletteColor(pPal, r, g, b, bestInd, bestDiff, treeRoot*2+1);
        if( bestDiff > splitComp - splitPos )
        {
            GifGetClosestPaletteColor(pPal, r, g, b, bestInd, bestDiff, treeRoot*2);
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
void GifPartition(GifRGBA* image, int com, int &left, int &right)
{
    GifSwapPixels(image, left, left + (right - left) / 2);
    uint8_t comPivot = image[left].comps[com];
    GifRGBA rgbPivot = image[left];
    bool split = false;
    for(int i1=left; i1<right; ++i1)
    {
        uint8_t comArray = image[i1].comps[com];
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
        else
        {
            if( !GifRGBEqual(rgbPivot, image[i1]) )
            {
                if(split)
                {
                    GifSwapPixels(image, i1, left);
                    ++left;
                }
                else
                {
                    --right;
                    GifSwapPixels(image, i1, right);
                    --i1;
                }
                split = !split;
            }
        }
    }
}

// Perform an incomplete sort, finding all elements above and below the desired median
int GifPartitionByMedian(GifRGBA* image, int com, int left, int right, int neededCenter)
{
    while(left < right-1)
    {
        int centerLeft = left;
        int centerRight = right;
        GifPartition(image, com, centerLeft, centerRight);

        if( neededCenter <= centerLeft )
            right = centerLeft;
        else if( neededCenter >= centerRight )
            left = centerRight;
        else if( neededCenter - centerLeft < centerRight - neededCenter )
            return centerLeft;
        else
            return centerRight;
    }
    return neededCenter;
}

// Builds a palette by creating a balanced k-d tree of all pixels in the image
void GifSplitPalette(GifRGBA* image, int numPixels, int firstElt, int lastElt, int splitElt, int splitDist, int treeNode, bool buildForDither, GifPalette* pal)
{
    if(lastElt <= firstElt || numPixels == 0)
        return;

    // base case, bottom of the tree
    if(lastElt == firstElt+1)
    {
        if(buildForDither)
        {
            // Dithering needs at least one color as dark as anything
            // in the image and at least one brightest color -
            // otherwise it builds up error and produces strange artifacts
            if( firstElt == 1 )
            {
                // special case: the darkest color in the image
                uint32_t r=255, g=255, b=255;
                for(int ii=0; ii<numPixels; ++ii)
                {
                    r = GifIMin(r, image[ii].r);
                    g = GifIMin(g, image[ii].g);
                    b = GifIMin(b, image[ii].b);
                }

                pal->r[firstElt] = r;
                pal->g[firstElt] = g;
                pal->b[firstElt] = b;

                return;
            }

            if( firstElt == (1 << pal->bitDepth)-1 )
            {
                // special case: the lightest color in the image
                uint32_t r=0, g=0, b=0;
                for(int ii=0; ii<numPixels; ++ii)
                {
                    r = GifIMax(r, image[ii].r);
                    g = GifIMax(g, image[ii].g);
                    b = GifIMax(b, image[ii].b);
                }

                pal->r[firstElt] = r;
                pal->g[firstElt] = g;
                pal->b[firstElt] = b;

                return;
            }
        }

        // otherwise, take the average of all colors in this subcube
        uint64_t r=0, g=0, b=0;
        for(int ii=0; ii<numPixels; ++ii)
        {
            r += image[ii].r;
            g += image[ii].g;
            b += image[ii].b;
        }

        r += numPixels / 2;  // round to nearest
        g += numPixels / 2;
        b += numPixels / 2;

        r /= numPixels;
        g /= numPixels;
        b /= numPixels;

        pal->r[firstElt] = (uint8_t)r;
        pal->g[firstElt] = (uint8_t)g;
        pal->b[firstElt] = (uint8_t)b;

        return;
    }

    // Find the axis with the largest range
    int minR = 255, maxR = 0;
    int minG = 255, maxG = 0;
    int minB = 255, maxB = 0;
    for(int ii=0; ii<numPixels; ++ii)
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

    // and split along that axis. (incidentally, this means this isn't a "proper" k-d tree but I don't know what else to call it)
    int splitCom = offsetof(union GifRGBA, g);
    if(bRange > gRange) splitCom = offsetof(union GifRGBA, b);
    if(rRange > bRange && rRange > gRange) splitCom = offsetof(union GifRGBA, r);

    int subPixelsA = numPixels * (splitElt - firstElt) / (lastElt - firstElt);
    pal->treeSplitElt[treeNode] = splitCom;
    pal->treeSplit[treeNode] = image[subPixelsA].comps[splitCom];
    subPixelsA = GifPartitionByMedian(image, splitCom, 0, numPixels, subPixelsA);
    int subPixelsB = numPixels-subPixelsA;

    GifSplitPalette(image,            subPixelsA, firstElt, splitElt, splitElt-splitDist, splitDist/2, treeNode*2,   buildForDither, pal);
    GifSplitPalette(image+subPixelsA, subPixelsB, splitElt, lastElt,  splitElt+splitDist, splitDist/2, treeNode*2+1, buildForDither, pal);
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
void GifMakePalette( const GifRGBA* lastFrame, const GifRGBA* nextFrame, uint32_t width, uint32_t height, int bitDepth, bool buildForDither, GifPalette* pPal )
{
    pPal->bitDepth = bitDepth;

    // SplitPalette is destructive (it sorts the pixels by color) so
    // we must create a copy of the image for it to destroy
    int imageSize = width*height*sizeof(GifRGBA);
    GifRGBA* destroyableImage = (GifRGBA*)GIF_TEMP_MALLOC(imageSize);
    memcpy(destroyableImage, nextFrame, imageSize);

    int numPixels = width*height;
    if(lastFrame)
        numPixels = GifPickChangedPixels(lastFrame, destroyableImage, numPixels);

    const int lastElt = 1 << bitDepth;
    const int splitElt = lastElt/2;
    const int splitDist = splitElt/2;

    GifSplitPalette(destroyableImage, numPixels, 1, lastElt, splitElt, splitDist, 1, buildForDither, pPal);

    GIF_TEMP_FREE(destroyableImage);

    // add the bottom node for the transparency index
    pPal->treeSplit[1 << (bitDepth-1)] = 0;
    pPal->treeSplitElt[1 << (bitDepth-1)] = 0;

    pPal->r[0] = pPal->g[0] = pPal->b[0] = 0;
}

// Implements Floyd-Steinberg dithering, writes palette index to alpha
void GifDitherImage( const GifRGBA* lastFrame, const GifRGBA* nextFrame, GifRGBA* outFrame, uint32_t width, uint32_t height, GifPalette* pPal )
{
    int numPixels = width*height;

    // quantPixels initially holds color*256 for all pixels
    // The extra 8 bits of precision allow for sub-single-color error values
    // to be propagated
    GifRGBA32* quantPixels = (GifRGBA32*)GIF_TEMP_MALLOC(sizeof(GifRGBA32)*numPixels);

    for( int ii=0; ii<numPixels*4; ++ii )
    {
        uint8_t pix = ((uint8_t*)nextFrame)[ii];
        int32_t pix16 = int32_t(pix) * 256;
        ((uint32_t*)quantPixels)[ii] = pix16;
    }

    for( uint32_t yy=0; yy<height; ++yy )
    {
        for( uint32_t xx=0; xx<width; ++xx )
        {
            GifRGBA32& nextPix = quantPixels[yy*width+xx];
            const GifRGBA* lastPix = lastFrame? &lastFrame[yy*width+xx] : NULL;

            // Compute the colors we want (rounding to nearest)
            int32_t rr = (nextPix.r + 127) / 256;
            int32_t gg = (nextPix.g + 127) / 256;
            int32_t bb = (nextPix.b + 127) / 256;

            // if it happens that we want the color from last frame, then just write out
            // a transparent pixel
            if( lastFrame &&
               lastPix->r == rr &&
               lastPix->g == gg &&
               lastPix->b == bb )
            {
                nextPix.r = rr;
                nextPix.g = gg;
                nextPix.b = bb;
                nextPix.a = kGifTransIndex;
                continue;
            }

            int32_t bestDiff = 1000000;
            int32_t bestInd = kGifTransIndex;

            // Search the palete
            GifGetClosestPaletteColor(pPal, rr, gg, bb, bestInd, bestDiff);

            // Write the result to the temp buffer
            int32_t r_err = nextPix.r - int32_t(pPal->r[bestInd]) * 256;
            int32_t g_err = nextPix.g - int32_t(pPal->g[bestInd]) * 256;
            int32_t b_err = nextPix.b - int32_t(pPal->b[bestInd]) * 256;

            nextPix.r = pPal->r[bestInd];
            nextPix.g = pPal->g[bestInd];
            nextPix.b = pPal->b[bestInd];
            nextPix.a = bestInd;

            // Propagate the error to the four adjacent locations
            // that we haven't touched yet
            int quantloc_7 = (yy*width+xx+1);
            int quantloc_3 = (yy*width+width+xx-1);
            int quantloc_5 = (yy*width+width+xx);
            int quantloc_1 = (yy*width+width+xx+1);

            if(quantloc_7 < numPixels)
            {
                GifRGBA32& pix7 = quantPixels[quantloc_7];
                pix7.r += GifIMax( -pix7.r, r_err * 7 / 16 );
                pix7.g += GifIMax( -pix7.g, g_err * 7 / 16 );
                pix7.b += GifIMax( -pix7.b, b_err * 7 / 16 );
            }

            if(quantloc_3 < numPixels)
            {
                GifRGBA32& pix3 = quantPixels[quantloc_3];
                pix3.r += GifIMax( -pix3.r, r_err * 3 / 16 );
                pix3.g += GifIMax( -pix3.g, g_err * 3 / 16 );
                pix3.b += GifIMax( -pix3.b, b_err * 3 / 16 );
            }

            if(quantloc_5 < numPixels)
            {
                GifRGBA32& pix5 = quantPixels[quantloc_5];
                pix5.r += GifIMax( -pix5.r, r_err * 5 / 16 );
                pix5.g += GifIMax( -pix5.g, g_err * 5 / 16 );
                pix5.b += GifIMax( -pix5.b, b_err * 5 / 16 );
            }

            if(quantloc_1 < numPixels)
            {
                GifRGBA32& pix1 = quantPixels[quantloc_1];
                pix1.r += GifIMax( -pix1.r, r_err / 16 );
                pix1.g += GifIMax( -pix1.g, g_err / 16 );
                pix1.b += GifIMax( -pix1.b, b_err / 16 );
            }
        }
    }

    // Copy the palettized result to the output buffer
    for( int ii=0; ii<numPixels*4; ++ii )
    {
        ((uint8_t*)outFrame)[ii] = ((uint32_t*)quantPixels)[ii];
    }

    GIF_TEMP_FREE(quantPixels);
}

// Picks palette colors for the image using simple thresholding, no dithering. Writes palette index to alpha.
void GifThresholdImage( const GifRGBA* lastFrame, const GifRGBA* nextFrame, GifRGBA* outFrame, uint32_t width, uint32_t height, GifPalette* pPal )
{
    uint32_t numPixels = width*height;
    for( uint32_t ii=0; ii<numPixels; ++ii )
    {
        // if a previous color is available, and it matches the current color,
        // set the pixel to transparent
        if(lastFrame && GifRGBEqual(*lastFrame, *nextFrame))
        {
            outFrame->r = lastFrame->r;
            outFrame->g = lastFrame->g;
            outFrame->b = lastFrame->b;
            outFrame->a = kGifTransIndex;
        }
        else
        {
            // palettize the pixel
            int32_t bestDiff = 1000000;
            int32_t bestInd = 1;
            GifGetClosestPaletteColor(pPal, nextFrame->r, nextFrame->g, nextFrame->b, bestInd, bestDiff);

            // Write the resulting color to the output buffer
            outFrame->r = pPal->r[bestInd];
            outFrame->g = pPal->g[bestInd];
            outFrame->b = pPal->b[bestInd];
            outFrame->a = bestInd;
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
        int r = pPal->r[kGifTransIndex], g = pPal->g[kGifTransIndex], b = pPal->b[kGifTransIndex];
        for( int ind=0; ind<(1 << pPal->bitDepth); ++ind )
        {
            // check whether this color is better than the current winner
            if( GifBetterColorMatch(pPal, ind, r, g, b, bestDiff) )
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
        if(lastFrame &&
           lastFrame->r == pPal->r[ind] &&
           lastFrame->g == pPal->g[ind] &&
           lastFrame->b == pPal->b[ind])
        {
            outFrame->r = lastFrame->r;
            outFrame->g = lastFrame->g;
            outFrame->b = lastFrame->b;
            outFrame->a = kGifTransIndex;
        }
        else
        {
            outFrame->r = pPal->r[ind];
            outFrame->g = pPal->g[ind];
            outFrame->b = pPal->b[ind];
            outFrame->a = ind;
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
    fputc(stat.chunkIndex, f);
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
        uint32_t r = pPal->r[ii];
        uint32_t g = pPal->g[ii];
        uint32_t b = pPal->b[ii];

        fputc(r, f);
        fputc(g, f);
        fputc(b, f);
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
    uint32_t codeSize = minCodeSize+1;
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
                GifWriteCode( f, stat, curCode, codeSize );

                // insert the new run into the dictionary
                codetree[curCode].m_next[nextValue] = ++maxCode;

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
                    codeSize = minCodeSize+1;
                    maxCode = clearCode+1;
                }

                curCode = nextValue;
            }
        }
    }

    // compression footer
    GifWriteCode( f, stat, curCode, codeSize );
    GifWriteCode( f, stat, clearCode, codeSize );
    GifWriteCode( f, stat, clearCode+1, minCodeSize+1 );

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
};

// Handle a call to GifWriteFrame[8] with a different image size to the previous
void GifHandleSizeChange( GifWriter* writer, int width, int height )
{
    if(writer->currentWidth != width || writer->currentHeight != height)
    {
        writer->maxWidth = GifIMax(writer->maxWidth, width);
        writer->maxHeight = GifIMax(writer->maxHeight, height);
        writer->currentWidth = width;
        writer->currentHeight = height;
        writer->oldImage = (GifRGBA*)GIF_REALLOC(writer->oldImage, width*height*sizeof(GifRGBA));
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

    // allocate
    writer->oldImage = (GifRGBA*)GIF_MALLOC(width*height*sizeof(GifRGBA));

    fputs("GIF89a", writer->f);

    // screen descriptor
    fputc(width & 0xff, writer->f);
    fputc((width >> 8) & 0xff, writer->f);
    fputc(height & 0xff, writer->f);
    fputc((height >> 8) & 0xff, writer->f);
    writer->currentWidth = writer->maxWidth = width;
    writer->currentHeight = writer->maxHeight = height;
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

    GifHandleSizeChange(writer, width, height);
    const GifRGBA* oldImage = writer->firstFrame? NULL : writer->oldImage;
    // Only GifWriteFrame8 can produce transparent frames, but the frame before the current one
    // needs to be set to 'background' disposal to support that. So for simplicity, we disable
    // delta coding for all frames.
    if(!writer->deltaCoded)
        oldImage = NULL;
    writer->firstFrame = false;

    GifPalette pal;
    // mark all nodes unused
    memset(pal.treeSplitElt, 3, 256);
    GifMakePalette((dither? NULL : oldImage), image, width, height, bitDepth, dither, &pal);

    if(dither)
        GifDitherImage(oldImage, image, writer->oldImage, width, height, &pal);
    else
        GifThresholdImage(oldImage, image, writer->oldImage, width, height, &pal);

    GifWriteLzwImage(writer->f, writer->oldImage, 0, 0, width, height, delay, &pal, writer->deltaCoded, true);

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

    GifHandleSizeChange(writer, width, height);
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

    GifWriteLzwImage(writer->f, writer->oldImage, 0, 0, width, height, delay, pal, writer->deltaCoded, localPalette);

    return true;
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
