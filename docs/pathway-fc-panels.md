# Movable popup panels in the pathway viewer

Open **Panel settings > Comparison / Panels**, select Group A, Group B, and the assay scale,
then click **Apply settings**. Enable **Popup panels** above the network.
Click a measured lipid-class node to open its species-level fold-change chart.
Panels open with a regular click. Right-click does not open panels. Ctrl + right-click
always deletes the targeted node or edge; no editing toggle is required. Panel content selects species FC,
enlarged projected plot, heatmap, or enrichment. Content switches immediately in
existing panels, preserving positions and projections; analytical settings still
require Apply and remapping. Plot and heatmap modes also support gene nodes.

Drag a panel header to move it, drag its bottom-right corner to resize it, and
use the close button to remove it. **Arrange panels** places open panels at the
left and right edges; **Clear panels** removes all of them. Connectors follow
both the cards and the network during pan/zoom or node movement. Cards retain
their positions when content switches. Applying analytical settings or switching
networks clears cards. Disabling the feature hides cards. Layouts are session-only
and are not saved in CYJS.

## Values and interpretation

- Uses the current `se_lipid_effective()` assay, including the acyl-chain filter.
- Non-negative abundance: `log2(mean(B) + 1e-8) - log2(mean(A) + 1e-8)`.
- Already log2 transformed: `mean(B) - mean(A)`.
- At least two finite values per group are needed for each feature. Negative
  abundance values are excluded, while negative log2 intensities are allowed.
- Shows the largest positive and most negative effects, up to the chosen number
  per direction. Zero effects are counted as valid but have no bars. These are
  effect-size plots: the bars do not imply statistical significance.
- Each card uses its own symmetric axis based on displayed finite species effects,
  rounded up to a readable tick. Small effects can use axes below 1; outliers are
  never clipped. Compare tick values across cards, rather than bar lengths alone.
  Labels report mapped, valid, shown, and excluded counts.
- Feature labels come from assay row names, consistent with existing molecule
  selection. Hover a truncated name or bar to read its full name and FC.

The scale selection must match the input data. Differences of z-scores or other
arbitrarily transformed values should not be interpreted as log2 fold changes.

## Export

**Save > Figure SVG (popup panels)** exports the current viewport and panel positions. FC text,
bars, and connector lines are vectors. The network is an embedded raster snapshot
at twice the viewport resolution. Plot/heatmap image sources are embedded; a failed
image download stops export with a message. Enrichment content uses SVG foreignObject.

The **Save > PDF with panels / popups** menu includes the FC overlay as a bitmap.
The **Save > Network PDF** menu continues to export the network alone.

## Verification

`tests/testthat/test-pathway-fc-panels.R` verifies effect direction, per-species
selection, missing/negative values, and log2 input. A local Shiny
module integration check with synthetic assay values verified adding, moving,
resizing, closing, contrast changes, SVG downloads, and network switching.
