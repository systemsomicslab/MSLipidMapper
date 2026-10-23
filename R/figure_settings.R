# Keep existing Shiny input IDs and bindings when presenting display controls
# in the per-analysis figure dialog.
.mslm_figure_registry <- function() {
  list(
    mod_pca_generic_ui = list(panel = "pca", fields = c("point_size", "show_scores_legend")),
    mod_oplsda_generic_ui = list(panel = "oplsda", fields = c("point_size", "show_scores_legend")),
    mod_feature_generic_ui = list(panel = "feat", fields = c("lipid-plot_type", "gene-plot_type", "lipid-show_bar_legend", "lipid-show_heatmap_legend")),
    mod_heatmap_ui = list(panel = "hm", fields = c("use_shared_palette_class", "show_sample_names_class", "use_shared_palette_mol", "show_sample_names_mol")),
    mod_plot_cor_ui = list(panel = "cor", fields = c("show_scatter_legend")),
    mod_volcano_ui = list(panel = "volcano", fields = c("point_size")),
    mod_lipid_enrich_ui = list(panel = "enrich", fields = c("bar_font_size", "bar_axis_label", "upset_text_scale", "upset_point_size")),
    mod_decoupled_chains_ui = list(panel = "decoupled", fields = c("point_size", "point_alpha")),
    mod_cyto_ui = list(panel = "net", fields = c("plot_type"))
  )
}

.mslm_collect_figure_controls <- function(ui, module_id, spec) {
  wanted <- paste0(module_id, "-", spec$fields)
  sections <- list()
  ids <- function(node) {
    if (inherits(node, "shiny.tag")) {
      c(node$attribs$id, unlist(lapply(node$children, ids), use.names = FALSE))
    } else if (is.list(node)) unlist(lapply(node, ids), use.names = FALSE)
    else character()
  }
  visit <- function(node, conditions = list()) {
    if (inherits(node, "shiny.tag")) {
      original <- intersect(ids(node), wanted)
      classes <- strsplit(node$attribs$class %||% "", " ", fixed = TRUE)[[1]]
      if ("shiny-panel-conditional" %in% classes) conditions <- c(conditions, list(node$attribs))
      if (length(original) && "form-group" %in% classes) {
        control <- node
        if (spec$panel == "hm") {
          context <- if (any(grepl("_class$", original))) "Class heatmap" else "Molecule heatmap"
          control <- shiny::tagList(shiny::tags$small(context), control)
        }
        for (attrs in rev(conditions)) {
          control <- shiny::div(control)
          control$attribs <- attrs
        }
        # Some legacy numeric inputs put their visible label in a sibling tag.
        if (paste0(module_id, "-point_size") %in% original && spec$panel == "pca")
          control <- shiny::tagList(shiny::tags$label("Score plot point size"), control)
        sections[[length(sections) + 1L]] <<- control
        return(NULL)
      }
      node$children <- Filter(Negate(is.null), lapply(node$children, visit, conditions = conditions))
      if (length(original) && !length(node$children)) return(NULL)
      if (length(original) && node$name == "details") {
        bodies <- Filter(function(x) inherits(x, "shiny.tag") && x$name != "summary", node$children)
        if (!length(bodies)) return(NULL)
      }
      if (node$name == "label" && identical(node$children, list("Score plot point size"))) return(NULL)
      node
    } else if (is.list(node)) {
      structure(Filter(Negate(is.null), lapply(node, visit, conditions = conditions)), class = class(node))
    } else node
  }
  body <- visit(ui)
  shiny::tagList(body, shiny::div(
    id = paste0("mslm-figure-stash-", spec$panel), hidden = "hidden",
    shiny::div(id = paste0("mslm-figure-controls-", spec$panel),
               class = "mslm-figure-local-controls", sections)
  ))
}

.mslm_figure_portal_ui <- function() {
  shiny::singleton(shiny::tagList(
    shiny::tags$style(shiny::HTML(".mslm-figure-local-controls { display:grid; grid-template-columns:repeat(auto-fit,minmax(220px,1fr)); gap:12px 24px; } .mslm-figure-local-controls .form-group { width:100% !important; margin:0 0 10px; } .mslm-figure-local-controls .selectize-dropdown { z-index:1200; }")),
    shiny::tags$script(shiny::HTML("(function(){
      var active = null;
      function restore() {
        if (!active) return;
        active.target.setAttribute('data-mslm-figure-closing', 'true');
        var stash = document.getElementById('mslm-figure-stash-' + active.panel);
        if (stash) stash.appendChild(active.controls);
        active = null;
      }
      function sync() {
        if (active && !active.target.isConnected) restore();
        var target = document.querySelector('[data-mslm-figure-target]:not([data-mslm-figure-closing])');
        if (!target || (active && active.target === target)) return;
        restore();
        var panel = target.getAttribute('data-mslm-figure-target');
        var controls = document.getElementById('mslm-figure-controls-' + panel);
        if (!controls) return;
        target.appendChild(controls);
        active = {panel:panel, controls:controls, target:target};
        if (window.jQuery) window.jQuery(window).trigger('resize');
      }
      function start() {
        new MutationObserver(sync).observe(document.body, {childList:true, subtree:true});
        if (window.jQuery) {
          window.jQuery(document).on('hide.bs.modal', '.modal', function(){
            if (active && this.contains(active.target)) restore();
          });
          // Return the bound inputs before Shiny unbinds/removes a modal.
          window.jQuery(document).on('shiny:unbound', '[data-mslm-figure-target]', restore);
        }
        sync();
      }
      if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
      else start();
    })();"))
  ))
}
