# Pathway workspace controls

The former sidebar has been replaced by a full-width pathway canvas, a compact
toolbar, and a settings drawer.

- **Header:** current pathway name, node/connection counts, projection status,
  Save and conditions. Candidates are shown
  only in **Panel settings > Display**; select one and Apply to switch pathways.
- **Summary badges:** current applied group comparison, plot/aggregation, and
  acyl-chain filters. Click a badge to open its settings tab.
- **Canvas toolbar:** Popup panels toggle, **Highlight significant nodes**, Pathway Mapping, Save, Fit network, and a single **Panel settings**
  button, available even when FC is off. Arrange panels and Close all panels appear
  only when FC is on. The duplicate Settings button has been removed.
- **Settings:** Display, Comparison / FC, Filters, and Manage.

## Applying settings

Pathway selection, display, comparison, and filter inputs are drafts until **Apply settings** is
clicked. Closing the drawer preserves the drafts without changing analytical
results. The summary badges and FC panel data always use the applied settings.
Invalid group pairs or numeric ranges keep the drawer open and leave the previous
settings in effect. Clearing the chain selection is also committed by Apply.

Applying settings clears stored projection images and enrichment overlays across
all loaded pathways, closes old popups and FC cards, and clears the selected node.
Node/edge definitions and stored positions are retained. The header displays
**Not projected — run Pathway Mapping**. Use **Pathway Mapping** to regenerate
images with the new settings; successful mapping changes the status to Projected.
Mapping itself starts by clearing the selected pathway's old projection, so a
failed mapping does not leave stale images marked as current. Applying settings
unchecks **Highlight significant nodes**; enable it again when ready.

**Highlight significant nodes** runs immediately without opening the settings
drawer. It uses the applied comparison groups, test, alpha, and FDR setting, and
updates when those settings or the data change. Standard blue class outlines are
neutral gray; significant increases/decreases have thick red/blue outlines. Other
custom node colors remain intact. Turning highlighting off restores the base
style. The legend beside the toggle shows the active statistical threshold.

Enrichment CSV files are parsed on upload and overlaid after a successful projection.
**Clear enrichment** removes the overlay immediately and prevents it
from being re-applied by an unrelated settings change; select a file again to
enable it again.

## Immediate controls

File import, FC enable/disable, fit, panel arrangement/closing,
and export remain immediate. Manage contains imports and pathway deletion.
Node/edge deletion is always enabled: Ctrl + right-click deletes the targeted
element, while Delete/Backspace and Delete selected remove selected elements.
Regular right-click does not open a popup; panels open with a regular click.
Node dragging remains available. Deleting an entire pathway asks for confirmation.
Deleting nodes or edges saves the current client graph without rebuilding the
network, preserving zoom, pan, moved nodes, and open popup positions. Saved node
positions and deletions remain when switching pathways within the session.

The settings drawer supports Escape, keyboard tab traversal, arrow-key tab
navigation, and focus restoration. Its dynamic group/filter inputs are bound even
while closed, so opening a non-Bootstrap drawer does not leave empty selectors.

Local renderer scripts include content hashes in their URLs to prevent an old
cached style from being used with a new empty-projection representation. Renderer
initialization errors are displayed inside the canvas and sent to R as a warning
and notification. A ResizeObserver resizes and fits the network when a dashboard
tab first becomes visible, even if it was hidden during application startup.

## Verification

`tests/testthat/test-pathway-workspace.R` covers UI input IDs, four settings tabs,
draft isolation, applying values, invalid submissions, and filtered data. A local
Shiny/Cytoscape browser check covers summaries, FC updates, unchanged network
instances for unrelated settings, saving SVGs, deletion mode, network switching,
and a narrow viewport.
