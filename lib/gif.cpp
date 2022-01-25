/* Dithering and KD-tree routines. Public Domain.
   This module contains routines that reuse parts of the gif.h library, and is the
   only module in the OHRRPGCE that should import gif.h, which is a header-only library.
 */
extern "C" {
#include "gif.h"


// Internal only
void indexed_tree_from_palette(GifKDTree& tree, const GifRGBA* palette, int bitDepth, int firstIndex) {
    // Copy 'palette', storing the original palette index into alpha, so
    // we can identify the colors after they're reordered by GifMakePalette
    GifRGBA* indexedPalette = (GifRGBA*)GIF_MALLOC((1 << bitDepth) * sizeof(GifRGBA));
    memcpy(indexedPalette, palette, (1 << bitDepth) * sizeof(GifRGBA));
    for (int idx = 0; idx < 1 << bitDepth; idx++) {
        indexedPalette[idx].a = idx;
    }

    // Remove disallowed colors
    for (int idx = 0; idx < firstIndex; idx++) {
        indexedPalette[idx] = indexedPalette[firstIndex];
    }

    // Create the KDTree for the palette by treating palette as an image
    GifMakePalette(NULL, indexedPalette, 1 << bitDepth, 1, bitDepth, true, &tree);

    GIF_FREE(indexedPalette);
}

// Quantize from 32-bit 'image' to 8-bit 'result', using Floyd-Steinberg dithering (or not).
// computePalette:  If true, 'palette' is an output, it is filled with a computed palette,
//             otherwise it is an input palette.
// bitDepth:   Gives the size of the palette, from 1 to 8
// firstIndex: Is the lowest allowed palette index in the output. A value of 0
//             allows the whole palette, a value of 1 excludes palette[0].
// maxerror:   Adjust max error propagation while dithering. Default 50, 0 disables dithering.
void dither_image(const GifRGBA* image, uint32_t width, uint32_t height, uint8_t* result, bool computePalette, GifRGBA* palette, int bitDepth, int firstIndex, int maxerror) {

    kGifMaxAccumError = maxerror;  // It's a global, yuck.

    uint32_t numPixels = width*height;
    GifKDTree tree;

    if (!computePalette) {
        // Compute a tree from palette, excluding firstIndex colors
        indexed_tree_from_palette(tree, palette, bitDepth, firstIndex);
    } else {
        // Compute a palette. firstIndex is ignored - It is always taken as 1.
        GifMakePalette(NULL, image, width, height, bitDepth, true, &tree);
    }

    GifRGBA* resultRGBA = (GifRGBA*)GIF_MALLOC(numPixels * sizeof(GifRGBA));
    GifDitherImage(NULL, image, resultRGBA, width, height, &tree);
    // The alpha component of each pixel in the result is the index into
    // tree.pal.colors, which is a reordered version of 'palette'.

    if (!computePalette) {
        for (uint32_t ii = 0; ii < numPixels; ++ii) {
            result[ii] = tree.pal.colors[resultRGBA[ii].a].a;  // Original palette index
        }
    } else {
        for (uint32_t ii = 0; ii < numPixels; ++ii) {
            result[ii] = resultRGBA[ii].a;  // Palette index
        }
        for (int idx = 0; idx < 1 << bitDepth; idx++) {
            palette[idx] = tree.pal.colors[idx];
            palette[idx].a = 255;
        }
    }

    GIF_FREE(resultRGBA);

    kGifMaxAccumError = 50;  // Reset to default
}

// Build a data structure (k-d tree) that allows fast querying of the
// nearest-match color in a palette. Delete the tree with delete_KDTree().
// The first 'firstIndex' colors in 'palette' will never be returned.
// (The k-d tree has at most 255 colors, since space is reserved for kGifTransIndex.
// But if firstIndex == 0, the color that will be omitted can be any, not just color 0.
// It will be the color that is most similar to another color in the palette.)
GifKDTree *make_KDTree_for_palette(const GifRGBA* palette, int bitDepth, int firstIndex) {
    GifKDTree *tree = (GifKDTree*)GIF_MALLOC(sizeof(GifKDTree));

    // Compute a tree from palette, excluding firstIndex colors
    indexed_tree_from_palette(*tree, palette, bitDepth, firstIndex);

    // GifMakePalette creates a tree->pal palette with reordered colors.
    // The original indices in *palette are stored in the alpha component
    // Reorder the colors so that their indices match 'palette', so
    // we avoid an extra indirection when querying the tree.

    // Fix the indices in the nodes
    for (int nodeIndex = 0; nodeIndex < tree->numNodes; nodeIndex++) {
        GifKDNode &node = tree->nodes[nodeIndex];
        if (node.isLeaf)
            node.palIndex = tree->pal.colors[node.palIndex].a;
    }

    // Instead of sorting the palette inplace, just overwrite it.
    memcpy(tree->pal.colors, palette, sizeof(GifRGBA) << bitDepth);

    return tree;
}

void delete_KDTree(GifKDTree *tree) {
    GIF_FREE(tree);
}

// Returns index of nearest-match color in the palette
int query_KDTree(GifKDTree* tree, GifRGBA color) {
    int bestDiff = 1000000;
    int bestInd = 0;
    GifGetClosestPaletteColor(tree, color, bestInd, bestDiff);
    return bestInd;
}

}
